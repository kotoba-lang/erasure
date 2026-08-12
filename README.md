# erasure

Locally recoverable erasure codes over GF(2^8) — portable `.cljc` that RUNS a
shipped `.kotoba` decision core for its layout, roles and generator matrix.

Built for the `kura` storage network (ADR-2607299200), but it depends on
nothing from it: this is the field, the generator matrix, the repair planner
and a reference codec, and that is all.

## What it is

A systematic **LRC**: `k` data shards, one XOR parity per local group of `r`,
and `g` global Cauchy-Reed-Solomon parities.

```
index          role
------------   ------------------------------------------------
0 .. k-1       data; shard i is in group (quot i r)
k .. k+l-1     local parity; shard k+q is the xor of group q
k+l .. n-1     global parity; Cauchy rows over all k data shards
```

The point of the local parities is repair traffic. Losing one shard is ~99% of
all repair events, and it costs `r` reads and zero multiplies instead of `k`
reads and a linear solve.

## Measured, not asserted

The Gopalan et al. bound says an *optimal* LRC with these parameters could
reach minimum distance `d <= n - k - ceil(k/r) + 2`. A bound is not a
measurement, so `erasure.distance-test` searches erasure patterns:

| config | patterns at t | failures | conclusion |
|---|---|---|---|
| k=16 r=4 g=6 (n=26) | 657,800 at t=7 | **0** | tolerates 7 |
| k=16 r=4 g=6 (n=26) | 1,562,275 at t=8 | 1,464 | fails at 8 |
| k=8 r=4 g=3 (n=13) | exhaustive | — | d = 5, tolerates 4 |

So the production configuration's true minimum distance is exactly 8 — it
**meets** the bound rather than approaching it. The smallest failures wipe two
whole groups of data (`#{0..7}`), which is the shape the bound predicts.

The exhaustive t=7 sweep takes minutes and is not in the default suite; the
suite runs the small config exhaustively, a deterministic sample at t=7, and
the specific 8-shard witnesses that must fail. `erasure.distance-test/slow-distance-sweep`
reproduces the full numbers.

Storage multiplier at k=16/n=26 is **1.625x**. See ADR-2607299200 section 1
for why that number and not the 1.375x a naive LRC comparison suggests: an LRC
is *non-MDS*, so at equal rate it tolerates fewer arbitrary failures than
Reed-Solomon, and the overhead you can actually afford is set by node quality
and repair time, not by the code.

## Usage

```clojure
(require '[erasure.lrc :as lrc] '[erasure.codec :as codec])

(def lay (lrc/layout {:k 16 :r 4 :g 6}))
;; => {:k 16 :r 4 :g 6 :l 4 :n 26 ...}

(def shards (codec/encode lay data))        ; data: 16 equal-length vectors
(subvec shards 0 16)                        ; systematic — data passes through

;; Plan before fetching: the planner never touches shard bytes.
(lrc/recovery-plan lay #{7})
;; => {:recoverable? true
;;     :steps [{:op :local :target 7 :reads [4 5 6 17]}]
;;     :reads #{4 5 6 17}}

(codec/decode lay (assoc shards 7 nil))     ; nil marks a missing shard
```

`decode` returns `nil` for a pattern beyond the code rather than a plausible
wrong answer. `codec/decodable?` answers the same question without any bytes.

## The two halves

Per ADR-2607299200 section 5, the library is split along *decision vs
mechanism*, the same line `aiueos` draws between Kotoba objects and its C
kernel:

- **Decisions** — the field, the generator matrix, group assignment, the
  repair plan. Small values in, small values out. Stated in
  `kotoba/erasure_core.kotoba` (`kotoba/pure` profile, no capabilities).
- **Mechanism** — `erasure.gf/scale-add`, the multiply-accumulate over a whole
  shard. A host provider is expected to replace this with SIMD or table-driven
  code and prove byte-equality against the definition here. Nothing above it
  may know which one it got.

A shard is never a Kotoba value. Today's compile path caps a string leaf at
64 KiB, and even when that lifts, pushing 4 MiB through the decision layer
would be putting mechanism in the wrong place.

### Is the core actually running? Yes, for most of it

Per ADR-2608112100 a `.kotoba` core with a parity test is not migrated — a
parity test binds two implementations, and what runs is still the `.cljc`. So
the core is **compiled and shipped** as
`resources/erasure/oracle/erasure-core.kir.edn`, and `erasure.kotoba-oracle`
executes it. These now come from the artifact rather than from a second copy:

| `.cljc` | export it runs |
|---|---|
| `lrc/layout` | `layout-admissible?`, `group-count`, `shard-count` |
| `lrc/group-of`, `group-members`, `local-parity-of` | `group-of`, `group-start`, `group-end`, `local-parity-of` |
| `lrc/role`, `global-parity-index`, `local-repair-reads-for` | `role-of`, `role-index-of`, `global-parity-index`, `local-repair-reads` |
| `lrc/generator-row`, `matrix/cauchy-entry` | `generator-entry`, `cauchy-entry` |
| `lrc/max-tolerated-erasures` | `max-tolerated-erasures` |

Two things are deliberately NOT delegated, both measured on 2026-08-12:

- **The GF layer** (`gf/add`/`mul`/`pow`/`inv`/`div`). An interpreted `gf-mul`
  measured ~7,000x the host's table lookup and `gf-inv` ~250,000x, and these run
  once per byte per coefficient — ~16.7 million for one k=16 encode of a 1 MiB
  stripe. The core states the field; `erasure.kotoba-parity-test` holds the
  host's tables to it.
- **`lrc/locally-repairable`'s "exactly one of the cover is missing"**. Its
  guest counterparts take a `[:set :i64]`, and a `[:set :i64]` does not work
  under ClojureScript at this compiler/kir pin at all —
  `kotoba.kir.value/compare-typed-values` sends `:i64` to `cljs.core/compare`,
  which cannot compare two `js/BigInt`s, and even a one-element set traps
  inside the guest. Delegating on the JVM only would put the repair rule in two
  places with one unchecked. `erasure.set-boundary-test` pins the measurement
  on both runtimes and goes red when a pin fixes it.

Three gates keep this honest, and each has been seen to fail:
`erasure.kotoba-parity-test` (the core answers as the `.cljc` does),
`the-shipped-artifact-is-the-current-source-compiled` (the artifact IS that
source, compiled), and `the-host-reads-the-artifact-rather-than-keeping-a-copy`
(substitute a core that answers differently; the host follows).

Regenerate the artifact after editing the `.kotoba`:

```bash
clojure -M:test:gen
```

### Notes from porting

Things the current compile path enforces that are worth knowing before you
edit the `.kotoba`:

- `max-parameters` is **5**, and a `loop` counts its bindings *plus every
  outer variable its body captures* against that budget. The group scans are
  written as explicit tail-recursive functions with all state in the parameter
  list for exactly this reason.
- A comparison such as `(= a b)` works as an `if` condition but does not carry
  type `:bool` in a value position — `(defn p [...] :bool (= 1 x))` is an
  expression type mismatch. Fold it through `if` with literal `true`/`false`.
  Same wrinkle `or` has (it yields i64).
- `bit-xor` / `bit-and` / `bit-or` / `bit-not` are 64-bit; shifts are
  `i64-shift-left` / `i64-shift-right` / `u64-shift-right` with a **literal**
  count in [0,63]. That is enough for GF(2^8) without any table.
- An entryless library needs a non-empty `(:export [...])` in its `ns`.

## Tests

```bash
clojure -M:test                                    # JVM: parity, drift, delegation
clojure -Sdeps '{:paths ["src" "test"]}' -M:cljs \
  -m cljs.main --target node -m erasure.cljs-runner  # ClojureScript
clojure -M:lint
```

`kotoba-lang/compiler` is a **test-only** dependency — it produced the shipped
artifact and never reaches a consumer. `kotoba-lang/kotoba-kir` is the library's
one runtime dependency, pinned to the kir that compiler declares; they are a
matched pair and must move together.

**ClojureScript hosts must register the KIR.** There is no classpath to read
the artifact from, so a cljs host has to call
`erasure.kotoba-oracle/register-kir!` before building a layout; without it,
`erasure.lrc` throws. `test/erasure/cljs_kir.cljc` is what a node host does
(read the file with `fs`, register at load time); a browser host would inline
the artifact at build time or fetch it. This is a real narrowing of what this
library used to be — it used to load on any cljs host with nothing but its own
source — and it is the price of the rules having one home.

## Field

Reduction polynomial `0x11D` (x^8 + x^4 + x^3 + x^2 + 1), alpha = 2 — the
classic Reed-Solomon field, so shards interoperate with other implementations
over the same polynomial. Deliberately **not** the AES field `0x11B`, where 2
has order 51 and is not primitive.

## License

MIT.
