(ns net.timsg.gamma-tools.core
  (:refer-clojure :exclude [aget])
  (:require
   [clojure.clr.io :as io]
   [clojure.string :as string]
   [gamma.api :as g]
   [gamma.program :as p]
   [clojure.test :as t])
  (:import [System.IO
            Path File Directory FileInfo DirectoryInfo]))

;; ============================================================
;; utils
;; ============================================================

(defn- line-join [lines]
  (string/join "\n" lines))

(defn- block [head & innards]
  (str head " {" (apply str innards) "}"))

;; ============================================================
;; general prettifying
;; ============================================================

(defn- format-program [x]
  (let [lines (-> x
                (clojure.string/replace ";" ";\n")
                (clojure.string/replace "{" "{\n")
                (clojure.string/replace "}" "\n}\n")
                clojure.string/split-lines)
        indent-f (fn [indent s]
                   (-> (apply str (repeat indent "\t"))
                     (str s)))]
    (first
      (reduce
        (fn [[bldg, indent] line]
          (cond
            (re-matches #"^\s*}.*" line)
            [(str bldg (indent-f (dec indent) line) "\n")
             (dec indent)]
            
            (re-matches #".*{$" line)
            [(str bldg (indent-f indent line) "\n")
             (inc indent)]
            
            :else
            [(str bldg (indent-f indent line) "\n")
             indent]))
        ["" 0]
        lines))))

;; ============================================================
;; what are things
;; ============================================================
;; no validation here

(defn gamma-program? [x]
  (and (map? x) (= (:tag x) :program)))

;; ============================================================
;; conversion to unity shaders
;; ============================================================

(defn- input-decl [{:keys [inputs]}]
  (for [{:keys [name type]} inputs]
    (case type
      :vec2 (throw
              (Exception.
                (str "can't have vec2 input! name: " name)))
      :vec3 (throw
              (Exception.
                (str "can't have vec3 input! name: " name)))
      :vec4  (str name " (\"" name "\", Vector) = (0, 0, 0, 0)")
      :float(str name " (\"" name "\", Float) = 0.0")
      :else (throw
              (Exception.
                (str
                  "Currently incapable of dealing with inputs of type "
                  type "; name: " name))))))

(defn unity-shader [name, program]
  (let [{:keys [vertex-shader
                fragment-shader]
         :as program}            (if (gamma-program? program)
                                   program
                                   (p/program program))
         vertex-progo (if-let [vs (:glsl vertex-shader)]
                        (line-join
                          [""
                           "#ifdef VERTEX"
                           ""
                           vs
                           "#endif"
                           ""])
                        (throw (Exception. "Vertex shader not found")))
         fragment-progo (if-let [fs (:glsl fragment-shader)]
                          (line-join
                            [""
                             "#ifdef FRAGMENT"
                             ""
                             fs
                             "#endif"
                             ""])
                          (throw (Exception. "Fragment shader not found")))
         progo (line-join
                 [vertex-progo
                  fragment-progo])]
    (format-program
      (block (str "Shader \"" name "\"")
        (block "Properties"
          (line-join (input-decl program)))
        (block "SubShader"
          (block "Pass"
            (line-join
              ["GLSLPROGRAM"
               progo
               "ENDGLSL"])))))))

;; ============================================================
;; writing
;; ============================================================

(defn write-shader [name dir prog]
  (let [p (cond
            (string? prog) prog
            (map? prog) (unity-shader name prog)
            :else (throw
                    (Exception.
                      (str "prog must be either string,"
                        " gamma program map (eg, (= true (gamma-program? prog)))),"
                        " or map."))))
        path (Path/Combine
               (.FullName (io/as-file dir))
               (str name ".shader"))]
    (File/WriteAllText path p)
    (println p)
    (println path)))

;; ============================================================
;; gamma help
;; ============================================================
;; maybe this should be in a different namespace. who can know.

;; (defn op-type-dispatch [& args]
;;   (let [c (count args)]
;;     (cond
;;       (= 0 c) :0
;;       (= 1 c) :1
;;       (= 2 c) [:2 (map :type args)]
;;       (< 2 c) :3+
;;       )))

(defn g<
  ([_] true)
  ([a b] (g/< a b))
  ([a b & args]
   (->> (list* a b args)
     (partition 2 1)
     (map #(apply g/< %))
     (reduce g/and))))

(defn g+
  ([] 0)
  ([arg] arg)
  ([arg & args]
   (reduce g/+
     (cons arg args))))

;; this is pointless without more info about the shadertoy spec. does
;; the type of the larger vector win for the type of the return, for
;; example? that would make sense because it would preserve
;; commutativity for the commutative ops without breaking it for the
;; non-commutative ops, and is roughly consistent with adding scalars
;; to vectors to get a vector

(defn vector-type? [v]
  (#{:vec2 :vec3 :vec4}
   (:type (g/ensure-term v))))

(defn scalar-type? [v]
  (#{:float :int}
   (:type (g/ensure-term v))))

(defn vector-length [v]
  (case (:type (g/ensure-term v))
    :vec2 2
    :vec3 3
    :vec4 4))

(def constructor-log
  (atom []))

(defn constructor [node]
  (swap! constructor-log conj node)
  (case (:type (g/ensure-term node))
    :vec2 g/vec2
    :vec3 g/vec3
    :vec4 g/vec4))

;; until gamma.api/aget successfully coerces its index to int
(defn aget [x i]
  (g/aget x (g/int i)))

(defn dest [v]
  (let [n (vector-length v)]
    (map #(aget v %)
      (range n))))

(defn- vector-map-op [op a b]
  (let [[[short-len short]
         [long-len long]] (sort-by first
                            (map (juxt vector-length identity) [a b])),
         smap {short (apply (constructor long)
                       (take long-len
                         (concat
                           (take short-len (dest long))
                           (repeat 0))))}]
    (if (= short-len long-len)
      (op a b)
      (apply op
        (replace smap [a b])))))

(defn- vector-scalar-op [op a b]
  (let [[a b] (map g/ensure-term [a b])
        [[v] [s]] ((juxt filter remove) vector-type? [a b])
        smap {s (apply (constructor v) s)}]
    (apply op
      (replace smap [a b]))))

(defn- vector-scalar-buddy [op a b]
  (let [a (g/ensure-term a)
        b (g/ensure-term b)
        tinf (set (map #(cond
                          (vector-type? %) :vector
                          (scalar-type? %) :scalar)
                    [a b]))]
    (case tinf
      #{:vector} (vector-map-op op a b)
      #{:vector :scalar} (vector-scalar-op op a b)
      #{:scalar} (op a b))))

;; (require '[clojure.test :as t])

(defn- run-tests [& args]
  (binding [t/*test-out* *out*]
    (apply t/run-tests args)))

(defn- remove-id [x]
  (clojure.walk/prewalk
    (fn [y]
      (if (map? y)
        (dissoc y :id)
        y))
    x))

;; (defn- equiv? [x y]
;;   (= (remove-id x) (remove-id y)))

;; (t/deftest test-vector-scalar-buddy
;;   (let [f #(vector-scalar-buddy g/- %1 %2)]
;;     (t/is
;;       (equiv? (f 1 2) (g/- 1 2)))
;;     (t/is
;;       (equiv?
;;         (f
;;           (g/vec3 1 2 3)
;;           (g/vec4 5 6 7 8))
;;         (g/-
;;           (g/vec4 1 2 3 0)
;;           (g/vec4 5 6 7 8))))
;;     (t/is
;;       (equiv?
;;         (f
;;           (g/vec4 5 6 7 8)
;;           (g/vec3 1 2 3))
;;         (g/-
;;           (g/vec4 1 2 3 0)
;;           (g/vec4 5 6 7 8))))))

(defn g-
  ([a] (g/* a -1))
  ([a b]
   ;;(g/- a b)
   (vector-scalar-buddy g/- a b))
  ([a b & cs]
   (reduce g/-
     (list* a b cs))))

(defn g*
  ([] 1)
  ([arg] arg)
  ([arg & args]
   (reduce g/*
     (cons arg args))))

(defn gdiv
  ([arg] (g/div 1 arg))
  ([arg & args]
   (reduce g/div
     (cons arg args))))

(defn gx [v]
  (aget v 0))

(defn gy [v]
  (aget v 1))

(defn gz [v]
  (aget v 2))

(defn gw [v]
  (aget v 3))



