(ns clinch.hooks
  (:require ["react" :as react]
            [goog.object :as gobj])
  (:require-macros [clinch.hooks]))

(deftype Atomified [react-ref deref-lens]
  IDeref
  (-deref [_]
    (deref-lens (first react-ref)))

  IReset
  (-reset! [_ v']
    ((second react-ref) v')
    v')

  ISwap
  (-swap! [o f]
    ((second react-ref) f))
  (-swap! [o f a]
    ((second react-ref) #(f % a)))
  (-swap! [o f a b]
    ((second react-ref) #(f % a b)))
  (-swap! [o f a b xs]
    ((second react-ref) #(apply f % a b xs))))

;; (defn updater
;;   ([x]
;;    )
;;   ([f & xs]
;;    ))

(defn useState
  "Like `React.useState`, but the update function returned can be used similar
  to `swap!`.

  Example:
  ```
  (let [[state set-state] (useState {:count 0})]
   ;; ...
   (set-state update :count inc))
  ```

  If `eq?` is passed in, will use that function to determine whether to update
  the React state. If it returns `true`, it will keep the old state, `false` it
  will render with new state."
  ([initial]
   (let [[v u] (react/useState initial)]
     [v (fn updater
          ([x] (u x))
          ([f & xs]
           (updater (fn spread-updater [x]
                      (apply f x xs)))))]))
  ([initial eq?]
   (let [[v u] (react/useState initial)]
     [v (fn updater
          ([x]
           ;; if x is not a fn, then it's likely not derived from previous state
           ;; so we don't bother checking equality
           (if-not (ifn? x)
             (u x)

             ;; When it is a function, new state will probably be derived from
             ;; previous. We can take advantage of structural sharing to do fast
             ;; equality here and avoid unnecessary re-renders
             (u (fn update [current-state]
                  (let [new-state (x current-state)]
                    (if (eq? current-state new-state)
                      ;; if equal, return the old one to preserve ref equality
                      ;; for React
                      current-state
                      new-state))))))
          ;; Support `(updater f a b c)`
          ([f & xs]
           (updater (fn spread-updater [x]
                      (apply f x xs)))))])))

(defn useIRef
  "Takes an initial value. Returns an atom that will _NOT_ re-render component
  on change."
  [initial]
  (let [react-ref (react/useRef initial)
        update-ref (fn updater
                     ([x]
                      (if-not (ifn? x)
                        (gobj/set react-ref "current" x)
                        (gobj/set react-ref "current"
                                  (x (gobj/get react-ref "current")))))
                     ([f & xs]
                      (updater (fn spread-updater [x]
                                 (apply f x xs)))))]
    (Atomified. [react-ref update-ref] #(.-current ^js %))))


(def useReducer
  "Just react/useReducer."
  react/useReducer)

;; React uses JS equality to check of the current deps are different than
;; previous deps values. This means that Clojure data (e.g. maps, sets, vecs)
;; equality is not respected and will trigger if you e.g. pass in a vec of
;; strings as props and need to depend on that inside of an effect.
;;
;; We can work around this by assigning the previous deps to a ref, and do
;; our own equality check to see if they have changed. If so, we update the
;; ref to equal the current value.
;;
;; We can then just pass this one value into e.g. `useEffect` and it will only
;; change if Clojure's equality detects a difference.
(defn useValue
  "Caches `x`. When a new `x` is passed in, returns new `x` only if it is
  not structurally equal to the previous `x`.

  Useful for optimizing `<-effect` et. al. when you have two values that might
  be structurally equal by referentially different."
  [x]
  (let [-x (react/useRef x)]
    ;; if they are equal, return the prev one to ensure ref equality
    (let [x' (if (= x (.-current -x))
               (.-current -x)
               x)]
      ;; Set the ref to be the last value that was succesfully used to render
      (react/useEffect (fn []
                         (set! (.-current -x) x)
                         js/undefined)
                       #js [x'])
      x')))

;; React `useEffect` expects either a function or undefined to be returned
(defn- wrap-fx [f]
  (fn wrap-fx-return []
    (let [x (f)]
      (if (fn? x)
        x
        js/undefined))))

(defn useEffect
  "Just react/useEffect"
  ([f]
   (react/useEffect (wrap-fx f)))
  ([f deps]
   (react/useEffect (wrap-fx f) (to-array deps))))

(def useContext
  "Just react/useContext"
  react/useContext)

(defn useMemo
  "Just react/useMemo"
  ([f] (react/useMemo f))
  ([f deps] (react/useMemo f (to-array deps))))

(defn useCallback
  "Just react/useCallback"
  ([f] (react/useCallback f))
  ([f deps] (react/useCallback f (to-array deps))))

(defn useImperativeHandle
  "Just react/useImperativeHandle"
  ([ref create-handle]
   (react/useImperativeHandle ref create-handle))
  ([ref create-handle deps]
   (react/useImperativeHandle ref create-handle
                              (to-array deps))))

(defn useLayoutEffect
  "Just react/useLayoutEffect"
  ([f] (react/useLayoutEffect f))
  ([f deps] (react/useLayoutEffect f (to-array deps))))


(def useDebugValue
  "Just react/useDebugValue"
  react/useDebugValue)

(defonce states (atom {}))

(defn useReloadable
  "A helper hook for constructing hooks that persist their state across
  hot-reloads.

  You must give it a hook-returning function and a globally unique key.
  useReloadable will then track the state in a global cache.

  When your app is re-mounted, it will first check the global cache for a value;
  if one exists, it will use that to initial the hook instead of the initial
  value.

  `initial` value is passed in as the value of the first mount. Default `nil`.
  `derive` value is used to derive the state to keep track in the global cache
  from the return value of the hook. Default is `identity`.

  Example:
  ```
  (defn useStateOnce
  [initial k]
  (useReloadable
   (fn useStateOnce* [state] (useState state))
   k
   :initial initial
   :derive first))
  ```
  "
  [use-hook k & {:keys [initial derive]
                 :or {initial nil
                      derive identity}}]
  (if js/goog.DEBUG
    ;; if a state already exists with name `k`, then pass that value in
    ;; otherwise, pass in the initial value.
    ;; capture the hook returned by the higher-order hook passed in
    (let [hook (use-hook (if (contains? @states k)
                           (@states k)
                           initial))]
      (let [has-mounted? (useIRef false)]
        (useEffect (fn []
                     (if @has-mounted?
                       (swap! states assoc k (derive hook))
                       (reset! has-mounted? true)))
                   [(derive hook)]))
      hook)
    ;; in release mode, just return <-state
    (use-hook initial)))

(defn useStateOnce
  "Like useState, but maintains your state across hot-reloads. `k` is a globally
  unique key to ensure you always get the same state back.

  Example: `(useStateOnce 0 ::counter)`"
  [initial k]
  (useReloadable
   (fn useStateOnce* [state] (useState state))
   k
   :initial initial
   :derive first))

(defn useReducerOnce
  "Like useReducer, but maintains your state across hot-reloads. `k` is a
  globally unique key to ensure you always get the same state back.

  Example: `(useReducerOnce reducer {:count 0} ::app-state)`
  "
  ([reducer initial k]
   (useReloadable
    (fn useReducerOnce* [state] (useReducer reducer state))
    k
    :initial initial
    :derive first))
  ([reducer initial init k]
   (useReloadable
    (fn useReducerOnce* [state] (useReducer reducer state init))
    k
    :initial initial
    :derive first)))
