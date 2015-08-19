(ns gamma-tools.core
  (require
    [clojure.clr.io :as io]
    [clojure.string :as string]
    [gamma.api :as g]
    [gamma.program :as p])
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
      :vec4 (str name " (\"" name "\", Vector) = (0, 0, 0, 0)")
      :float (str name " (\"" name "\", Float) = 0.0")
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

(defn g-
  ([arg] (g/* arg -1))
  ([arg & args]
   (reduce g/-
     (cons arg args))))

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
  (g/aget v 0))

(defn gy [v]
  (g/aget v 1))

(defn gz [v]
  (g/aget v 2))

(defn gw [v]
  (g/aget v 3))

