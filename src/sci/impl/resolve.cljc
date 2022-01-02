(ns sci.impl.resolve
  {:no-doc true}
  (:require [clojure.string :as str]
            [sci.impl.evaluator :as eval]
            [sci.impl.faster :as faster]
            [sci.impl.interop :as interop]
            [sci.impl.records :as records]
            [sci.impl.utils :as utils :refer [strip-core-ns
                                              ana-macros
                                              ctx-fn
                                              kw-identical?]]
            [sci.impl.vars :as vars]))

(defn throw-error-with-location [msg node]
  (utils/throw-error-with-location msg node {:phase "analysis"}))

(defn mark-resolve-sym
  [sym]
  (vary-meta
   sym
   (fn [m]
     (assoc m :sci.impl/op :resolve-sym))))

(defn check-permission! [ctx sym [check-sym  v]]
  (or (identical? utils/allowed-loop sym)
      (identical? utils/allowed-recur sym)
      (let [check-sym (strip-core-ns check-sym)
            allow (:allow ctx)]
        (when-not (if allow (or (and (vars/var? v) (not (:sci/built-in (meta v))))
                                (contains? allow check-sym))
                      true)
          (throw-error-with-location (str sym " is not allowed!") sym))
        (let [deny (:deny ctx)]
          (when (if deny (contains? deny check-sym)
                    false)
            (throw-error-with-location (str sym " is not allowed!") sym))))))

(defn lookup*
  ([ctx sym call?] (lookup* ctx sym call? false))
  ([ctx sym call? only-var?]
   (let [sym-ns (some-> (namespace sym) symbol)
         sym-name (symbol (name sym))
         env (faster/get-2 ctx :env)
         env @env
         cnn (vars/current-ns-name)
         the-current-ns (-> env :namespaces cnn)
         ;; resolve alias
         sym-ns (when sym-ns (or (get-in the-current-ns [:aliases sym-ns])
                                 sym-ns))]
     (if sym-ns
       (or
        (when (or (= sym-ns 'clojure.core) (= sym-ns 'cljs.core))
          (or (some-> env :namespaces (get 'clojure.core) (find sym-name))
              (when-let [v (when call? (get ana-macros sym-name))]
                [sym v])))
        (or (some-> env :namespaces (get sym-ns) (find sym-name))
            (when-not only-var?
                 (when-let [clazz (interop/resolve-class ctx sym-ns)]
                   [sym (if call?
                          (with-meta
                            [clazz sym-name]
                            {:sci.impl.analyzer/static-access true})
                          (ctx-fn
                           (fn [_ctx _bindings]
                             (interop/get-static-field [clazz sym-name]))
                           nil
                           sym
                           (assoc (meta sym)
                                  :file @vars/current-file
                                  :ns @vars/current-ns)))]))))
       ;; no sym-ns
       (or
        ;; prioritize refers over vars in the current namespace, see 527
        (when-let [refers (:refers the-current-ns)]
          (find refers sym-name))
        (find the-current-ns sym) ;; env can contain foo/bar symbols from bindings
        (let [kv (some-> env :namespaces (get 'clojure.core) (find sym-name))]
          ;; only valid when the symbol isn't excluded
          (when-not (some-> the-current-ns
                            :refer
                            (get 'clojure.core)
                            :exclude
                            (contains? sym-name))
            kv))
        (when (when call? (get ana-macros sym))
          [sym sym])
        (when-not only-var?
          (or
           (when-let [c (interop/resolve-class ctx sym)]
             [sym c])
           ;; resolves record or protocol referenced as class
           ;; e.g. clojure.lang.IDeref which is really a var in clojure.lang/IDeref
           (when-let [x (records/resolve-record-or-protocol-class ctx sym)]
             [sym x]))))))))

;; <<<<<<< HEAD
#_(defn lookup [ctx sym call?]
  (let [bindings (faster/get-2 ctx :bindings)]
    (or
     ;; bindings are not checked for permissions
     (when-let [[k v]
                (find bindings sym)]
       ;; never inline a binding at macro time!
       (let [;; pass along tag of expression!
             _ (when-let [cb (:closure-bindings ctx)]
                 (when-not (contains? (:param-map ctx) sym)
                   (vswap! cb conj sym)))
             v (if call? ;; resolve-symbol is already handled in the call case
                 (if (instance? clojure.lang.IDeref v)
                   v
                   (mark-resolve-sym k))
                 (if (instance? clojure.lang.IDeref v)
                   (ctx-fn
                    (fn [_ctx _bindings]
                      ;; (prn :direct k)
                      @v)
                    k)
                   (ctx-fn
                    (fn [_ctx bindings]
                      (eval/resolve-symbol bindings k))
                    k)))]
         [k v]))
     (when-let [kv (lookup* ctx sym call?)]
       (when (:check-permissions ctx)
         (check-permission! ctx sym kv))
       kv))))
;; =======
(defn lookup
  ([ctx sym call?] (lookup ctx sym call? nil))
  ([ctx sym call? tag]
   (let [bindings (faster/get-2 ctx :bindings)]
     (or
      ;; bindings are not checked for permissions
      (when-let [[k v]
                 (find bindings sym)]
        ;; never inline a binding at macro time, unless it's a function
        (if (kw-identical? :sci.impl.analyzer/self-ref v)
          (do
            (vreset! (:self-ref ctx) true)
            (when-let [cb (:closure-bindings ctx)]
              (vswap! cb conj sym))
            (if call?
              [k v]
              (if (instance? clojure.lang.IDeref v)
                [k (ctx-fn
                    (fn [_ctx _bindings]
                      (prn :direct k)
                      @v)
                    k)]
                [k (ctx-fn
                    (fn [_ctx bindings]
                      ;; TODO: optimize
                      @(eval/resolve-symbol bindings k))
                    nil
                    nil)])))
          (let [;; pass along tag of expression!
                _ (when-let [cb (:closure-bindings ctx)]
                    (when-not (contains? (:param-map ctx) sym)
                      (vswap! cb conj sym)))
                v (if call? ;; resolve-symbol is already handled in the call case
                    (if (instance? clojure.lang.IDeref v)
                      v
                      (mark-resolve-sym k))

                    (if (instance? clojure.lang.IDeref v)
                      (ctx-fn
                       (fn [_ctx _bindings]
                         ;; (prn :direct k)
                         @v)
                       k)
                      (ctx-fn
                       (fn [_ctx bindings]
                         (eval/resolve-symbol bindings k))
                       nil
                       (if tag
                         (vary-meta k assoc :tag tag)
                         k))))]
            [k v])))
      (when-let [kv (lookup* ctx sym call?)]
        (when (:check-permissions ctx)
          (check-permission! ctx sym kv))
        kv)))))
;; >>>>>>> master

;; workaround for evaluator also needing this function
(vreset! utils/lookup lookup)

(defn resolve-symbol
  ([ctx sym] (resolve-symbol ctx sym false nil))
  ([ctx sym call?] (resolve-symbol ctx sym call? nil))
  ([ctx sym call? tag]
   (let [res (second
              (or
               (lookup ctx sym call? tag)
               ;; TODO: check if symbol is in macros and then emit an error: cannot take
               ;; the value of a macro
               (let [n (name sym)]
                 (cond
                   (and call?
                        (str/starts-with? n ".")
                        (> (count n) 1))
                   [sym 'expand-dot*] ;; method invocation
                   (and call?
                        (str/ends-with? n ".")
                        (> (count n) 1))
                   [sym 'expand-constructor]
                   :else
                   (throw-error-with-location
                    (str "Could not resolve symbol: " (str sym))
                    sym)))))]
     ;; (prn 'resolve sym '-> res (meta res))
     res)))
