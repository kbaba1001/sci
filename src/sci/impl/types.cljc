(ns sci.impl.types
  {:no-doc true})

(defprotocol IBox
  (setVal [_this _v])
  (getVal [_this]))

(defprotocol INode
  (eval-node [this ctx]))

(extend-protocol INode
  #?(:clj Object :cljs default)
  (eval-node [this ctx] this))

(extend-protocol INode
  nil
  (eval-node [this ctx] nil))

(deftype EvalVar [v]
  IBox
  (getVal [_this] v)
  INode
  (eval-node [_this _ctx] @v))

(defprotocol IReified
  (getInterfaces [_])
  (getMethods [_])
  (getProtocols [_]))

(deftype Reified [interfaces meths protocols]
  IReified
  (getInterfaces [_] interfaces)
  (getMethods [_] meths)
  (getProtocols [_] protocols))

(defn type-impl [x & _xs]
  (or (when (instance? #?(:clj sci.impl.types.IReified :cljs sci.impl.types/Reified) x)
        :sci.impl.protocols/reified)
      (some-> x meta :type)
      #?(:clj (class x) ;; no need to check for metadata anymore
         :cljs (type x))))

;; returned from analyzer when macroexpansion needs interleaved eval
(deftype EvalForm [form]
  IBox
  (getVal [_this] form))

(declare ->EvalFn)

(defprotocol Sexpr
  (sexpr [this]))

(extend-protocol Sexpr
  #?(:clj Object :cljs default) (sexpr [this] this))

(defprotocol Info
  (info [this]))

(deftype EvalFn [f info expr]
  ;; f = (fn [ctx] ...)
  ;; m = meta
  IBox
  (getVal [_this] f)
  #?(:clj clojure.lang.IMeta
     :cljs IMeta)
  (#?(:clj meta
      :cljs -meta) [_this] (meta expr))
  #?(:clj clojure.lang.IObj
     :cljs IWithMeta)
  (#?(:clj withMeta
      :cljs -with-meta) [_this m]
    (->EvalFn f info (with-meta expr m)))
  Info
  (info [_] info)
  Sexpr
  (sexpr [_] expr)
  Object
  (toString [_this]
    (str expr))
  INode
  (eval-node [_this ctx]
    (f ctx)))

(def kw-identical? #?(:clj identical? :cljs keyword-identical?))

(defn handle-meta [ctx m]
  ;; Sometimes metadata needs eval. In this case the metadata has metadata.
  (-> (if-let [mm (meta m)]
        (if (when mm (get mm :sci.impl/op))
          (eval-node m ctx)
          m)
        m)
      (dissoc :sci.impl/op)))

(defrecord MapNode []
  INode
  (eval-node [this ctx]
    (if-let [m (meta this)]
      (if (kw-identical? :eval (:sci.impl/op m))
        (with-meta (zipmap (map #(eval-node % ctx) (keys this))
                           (map #(eval-node % ctx) (vals this)))
          (handle-meta ctx m))
        this)
      this)))
