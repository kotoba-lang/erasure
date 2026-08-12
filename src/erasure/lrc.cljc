(ns erasure.lrc
  "Layout and repair planning for the locally recoverable code.

  **Shard layout.** For parameters `k` data shards, locality `r`, and `g`
  global parities, with `l = ceil(k/r)` groups:

      index          role
      ------------   ----------------------------------------------
      0 .. k-1       data; shard i belongs to group (quot i r)
      k .. k+l-1     local parity; shard k+q is the xor of group q
      k+l .. n-1     global parity; Cauchy rows over ALL k data shards

  Local parity is a plain xor, so repairing one lost shard inside a group
  costs `r` reads and zero multiplies. That is the whole point of the code
  and the reason ADR-2607299200 accepts a worse worst-case distance than a
  plain Reed-Solomon of the same rate: single-node loss is ~99% of repair
  events, and it drops from k reads to r.

  **This namespace holds decisions, not bytes.** Every function here maps
  small values to small values — indices, sets of indices, a plan. That is
  exactly the slice ADR-2607299200 section 5 assigns to the `.kotoba` port.

  **And it no longer holds most of them twice.** The admissible parameters,
  the group and shard counts, the group boundaries, the roles, the Cauchy
  construction, the whole generator matrix and the durability bound are stated
  once — in `kotoba/erasure_core.kotoba` — and this namespace RUNS that:
  compiled, shipped as `resources/erasure/oracle/erasure-core.kir.edn`,
  executed by `erasure.kotoba-oracle`. Per ADR-2608112100 the port is not
  \"done\" while the host keeps its own copy of a rule, and
  `erasure.kotoba-oracle-test` is what proves it does not.

  What stays here is what is not a decision: building a range, sorting a read
  list, intersecting two sets, naming an `AssertionError`, and the linear
  algebra of the global solve — plus one decision that is still stated twice,
  named below with the measurement that stopped it.

  **Planning is deterministic.** Where a choice exists — which k survivors to
  feed a global solve — the lowest indices win. Determinism is not cosmetic:
  it is what lets the `.kotoba` port be compared for equality rather than for
  plausibility.

  **The one decision that did NOT move, and the measurement that stopped it.**
  `locally-repairable` still counts here. Its guest counterparts
  (`erasure-core/cover-missing`, `locally-repairable?`, `local-repair-target`)
  take a `[:set :i64]`, and MEASURED 2026-08-12 at this compiler/kir pin, a
  `[:set :i64]` is unusable under ClojureScript — not merely as an argument but
  inside the guest, because `kotoba.kir.value/compare-typed-values` routes
  `:i64` through `cljs.core/compare`, which throws `Cannot compare 5 to 4` on
  the `js/BigInt` an `:i64` is on that runtime. A one-element set is enough:
  `typed-set-contains` compares. `erasure.set-boundary-test` pins this on both
  runtimes, so the day a pin fixes it, that test fails and names the delegation
  that can then move.

  Delegating it under `:clj` only was the alternative, and it is the worse one:
  it would put the repair rule in two places with exactly one of them checked,
  which is the state ADR-2608112100 exists to end. The scalar half of the
  decision — what a group's cover IS (`group-start`, `group-end`,
  `local-parity-of`) — does cross, and does, on both runtimes; what stays is
  \"exactly one of them\", and it is marked where it sits.

  **ClojureScript hosts must register the KIR** — there is no classpath to read
  the artifact from, so `erasure.kotoba-oracle/register-kir!` has to happen
  before a layout can be built at all. That is a real narrowing of what this
  library used to do on that runtime, and the price of the rules having one
  home. `test/erasure/cljs_runner.cljc` shows what a node host does about it."
  (:require [clojure.set :as set]
            [erasure.kotoba-oracle :as oracle]
            [erasure.matrix :as matrix]))

;; ── the seam ─────────────────────────────────────────────────────────
;; Every guest call in this namespace goes through one of these two. `decide`
;; is `call` with the answer remembered per core generation, which is safe
;; here because each question below has a small finite argument domain — see
;; `erasure.kotoba-oracle`.

(defn- ask [export & args]
  (oracle/i64-value (oracle/decide :erasure-core export (vec args))))

(defn- ask? [export & args]
  (oracle/decide :erasure-core export (vec args)))

(defn layout
  "Build a layout from `{:k :r :g}`.

  Throws on parameters the construction cannot honour: a Cauchy matrix over
  GF(2^8) needs `k + g <= 256`, and a group size below 2 makes local parity a
  duplicate rather than a code.

  Which parameters those are is `erasure-core/layout-admissible?`, and the
  group and shard counts are `group-count` / `shard-count`. What is left here
  is the signalling: the guest language has no `throw`, so naming an
  `AssertionError` is a host job — the same split the port's own header
  describes. One assert rather than the three this used to have, because the
  core states the admissible region as one predicate; the message enumerates
  what it covers."
  [{:keys [k r g]}]
  (assert (ask? 'layout-admissible? k r g)
          "erasure-core/layout-admissible?: k and g must be positive, locality r
           at least 2, and k + g at most 256 so Cauchy has distinct elements")
  (let [l (ask 'group-count k r)
        n (ask 'shard-count k r g)]
    {:k k :r r :g g :l l :n n
     :data-range [0 k]
     :local-range [k (+ k l)]
     :global-range [(+ k l) n]}))

(defn group-of
  "Which local group data shard `i` belongs to."
  [{:keys [r]} i]
  (ask 'group-of i r))

(defn group-members
  "Data shard indices in local group `q`. The last group is short when `r`
  does not divide `k` — which is `erasure-core/group-end`'s business, not
  this function's; the `range` is all that is left."
  [{:keys [k r]} q]
  (vec (range (ask 'group-start q r) (ask 'group-end k r q))))

(defn local-parity-of
  "The local-parity shard index covering local group `q`."
  [{:keys [k]} q]
  (ask 'local-parity-of k q))

(defn local-repair-reads-for
  "How many shards a single local repair inside group `q` reads.

  The cover is the group's data plus its local parity, so rebuilding one
  member reads the rest — `r` for a full group, fewer for a short trailing
  one. This is the number ADR-2607299200 quotes as the repair-traffic win, and
  it is now the number `erasure-core/local-repair-reads` returns rather than a
  second count of the same thing."
  [{:keys [k r]} q]
  (ask 'local-repair-reads k r q))

(defn global-parity-index
  "Which Cauchy row shard `i` is, or nil when `i` is not a global parity.

  The core answers -1 for \"not one\", because the guest has no nil; the
  translation is the whole of what happens here. Its unbounded top edge is
  deliberate and mirrored — see the note on `erasure-core/global-parity-index`."
  [{:keys [k r]} i]
  (let [row (ask 'global-parity-index k r i)]
    (when-not (neg? row) row)))

(def ^:private role-kinds
  "`erasure-core/role-of` answers with an i64 tag, because small integers
  compare across the two runtimes without marshalling. This is the only place
  that knows which keyword each tag means."
  {0 :data 1 :local 2 :global 3 :out-of-range})

(defn role
  "Classify a shard index.

  Both halves come from the core: `role-of` for the kind and `role-index-of`
  for the group or Cauchy row. The map is assembled here because a map with
  optional keys is not a guest value."
  [{:keys [k r g]} i]
  (let [kind (role-kinds (ask 'role-of k r g i))
        idx (ask 'role-index-of k r g i)]
    (case kind
      :out-of-range {:kind :out-of-range}
      :global {:kind :global :index idx}
      {:kind kind :group idx})))

(defn data-indices [{:keys [k]}] (set (range k)))

(defn global-indices [{:keys [k l n]}] (set (range (+ k l) n)))

(defn generator-row
  "The row of the generator matrix that shard `i` represents — i.e. the
  coefficients that produce shard `i` from the `k` data shards.

  Every shard has one, which is the point: a local parity is the sum of its
  group's identity rows, so it carries a real equation and belongs in any
  global solve alongside the data and Cauchy rows. The three cases used to be
  a `cond` here over `matrix/identity-row` and `matrix/cauchy-rows`; they are
  `erasure-core/generator-entry` now, one call per coefficient, and the `mapv`
  is what is left."
  [{:keys [k r g]} i]
  (mapv (fn [j] (ask 'generator-entry k r g i j)) (range k)))

(defn- local-cover
  "The shard set that reconstructs anything missing from group `q`: the
  group's data shards plus its local parity."
  [lay q]
  (conj (set (group-members lay q)) (local-parity-of lay q)))

(defn- locally-repairable
  "If exactly one member of group `q`'s cover is missing, that member can be
  rebuilt by xoring the rest. Returns `[target reads]` or nil.

  THE ONE RULE STILL STATED TWICE. `erasure-core/locally-repairable?` and
  `local-repair-target` say the same thing as the two lines below, and this
  namespace does not ask them: their `[:set :i64]` argument does not work under
  ClojureScript at this pin, and asking them only on the JVM would mean the
  repair rule lives in two places with one of them unchecked. See the
  namespace docstring for the measurement, `erasure.set-boundary-test` for the
  executable form of it, and `erasure.kotoba-parity-test` for what holds these
  two lines to the core meanwhile.

  What DOES come from the core here is what the cover is: `local-cover` is
  built out of `group-members` and `local-parity-of`, both of which are the
  guest's answers. So the substituted-core gate still reaches the planner
  through this function — it is the counting, not the geometry, that stayed."
  [lay q missing]
  (let [cover (local-cover lay q)
        gone (set/intersection cover missing)]
    (when (= 1 (count gone))
      [(first gone) (vec (sort (disj cover (first gone))))])))

(defn recovery-plan
  "Plan the reconstruction of `erased` (a set of shard indices).

  Returns `{:recoverable? :steps :reads}` where `:steps` is an ordered
  sequence of `{:op :local  :target i :reads [...]}` and
  `{:op :global :targets [...] :reads [...]}`, and `:reads` is the set of
  distinct shards that must actually be fetched.

  Strategy, in order:

  1. Drain every group that is missing exactly one of its cover. Repeat to a
     fixpoint, because rebuilding a group's data shard can make that group's
     local parity rebuildable in the next round.
  2. If data shards are still missing, solve globally — possible exactly when
     at least k shards survive among data and global parity, which the Cauchy
     construction guarantees is enough (see `erasure.matrix`).
  3. Recompute any parity shard still missing from the now-complete data.

  A pattern that leaves fewer than k survivors across data and global parity
  and cannot be finished locally is reported unrecoverable rather than
  attempted."
  [{:keys [k l n] :as lay} erased]
  (let [erased (set erased)]
    (loop [missing erased
           steps []
           guard 0]
      (cond
        (empty? missing)
        {:recoverable? true :steps steps
         :reads (into (sorted-set) (mapcat :reads steps))}

        ;; Defensive: each round must retire at least one shard, so this can
        ;; only trip if the loop below is ever changed to not make progress.
        (> guard n)
        {:recoverable? false :steps steps :reads #{} :reason :no-progress}

        :else
        (if-let [[target reads]
                 (some #(locally-repairable lay % missing)
                       (range l))]
          (recur (disj missing target)
                 (conj steps {:op :local :target target :reads reads})
                 (inc guard))
          ;; No group can finish on its own. Fall through to a global solve
          ;; over EVERY surviving shard — local parities included, since each
          ;; is a genuine equation in the data (see erasure.matrix).
          (let [survivors (vec (sort (set/difference (set (range n)) missing)))
                lost-data (vec (sort (set/intersection (data-indices lay) missing)))
                chosen (when (seq lost-data)
                         (matrix/independent-rows #(generator-row lay %) k survivors))]
            (if (and (seq lost-data) (nil? chosen))
              {:recoverable? false :steps steps :reads #{}
               :reason :rank-deficient
               :survivors (count survivors) :needed k}
              (let [;; Global solve rebuilds every lost data shard at once.
                    steps (cond-> steps
                            (seq lost-data)
                            (conj {:op :global :targets lost-data :reads chosen}))
                    missing (set/difference missing (set lost-data))
                    ;; Data is whole now, so any remaining parity is a
                    ;; recompute, not a solve.
                    parity-left (vec (sort missing))
                    steps (reduce
                           (fn [acc idx]
                             (let [{:keys [kind group]} (role lay idx)]
                               (conj acc
                                     (if (= kind :local)
                                       {:op :local :target idx
                                        :reads (group-members lay group)}
                                       {:op :recompute-global :target idx
                                        :reads (vec (range k))}))))
                           steps
                           parity-left)]
                {:recoverable? true :steps steps
                 :reads (into (sorted-set) (mapcat :reads steps))}))))))))

(defn max-tolerated-erasures
  "Largest t such that EVERY t-subset of shards can be erased and still
  recovered — i.e. the code's minimum distance minus one.

  This is measured, not asserted: `erasure.distance-test` searches erasure
  patterns against `recovery-plan`. The Gopalan et al. upper bound for an
  optimal LRC is `d <= n - k - ceil(k/r) + 2`; this construction is not
  claimed to meet it, and the test reports what it actually achieves. The
  inequality itself is `erasure-core/gopalan-distance-bound`, and this is its
  tolerated form from the same place."
  [{:keys [k r g]}]
  (ask 'max-tolerated-erasures k r g))
