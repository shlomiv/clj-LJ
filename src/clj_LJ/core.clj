(ns clj-LJ.core
  (import [LJava Constraint MathFormulas LJ]))

; helper functions for our puzzle
(defn group [& x]
  (LJava.LJ/group (object-array x)))

; define the world
(group 1 2 3 4 5 6 7 8)

(defn mk-constraint [vs a b]
  (Constraint. MathFormulas/abs (object-array [1 (nth vs a) (nth vs b)])))

(defn mk-comp [c vs a b]
  (Constraint. c LJ/OR (mk-constraint vs a b)))

(defn puzzle []
  (let [vss (LJ/varArray 8)
        c   (reduce (fn [c [a b]] (mk-comp c vss a b))
                    (mk-constraint vss 0 2)
                    [[1 2] [1 4] [2 3] [2 5] [3 6] [4 5] [5 6] [5 7]])
        lz  (lazy-seq  (LJ/lazy  (LJ/relation vss) LJ/DIFFER c ))]
    (doseq [l (take 10 lz)]
      (println (map #(.get % 0) (seq (.toArray l vss)))))))

(defn -main [& l]
  "Runs our puzzle"
  (puzzle))
