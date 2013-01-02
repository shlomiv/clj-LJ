(ns ljc.core)

; helper functions
(defn group [& x]
  (LJava.LJ/group (object-array x)))

(defn mk-constraint [vs a b]
  (LJava.Constraint. LJava.MathFormulas/abs (object-array [1 (nth vs a) (nth vs b)])))

(defn mk-comp [c vs a b]
  (LJava.Constraint. c LJava.LJ/OR (mk-constraint vs a b)))

; define the world
(group 1 2 3 4 5 6 7 8)

(defn testi []
  (let [vss (LJava.LJ/varArray 8)
        c   (reduce (fn [c [a b]] (mk-comp c vss a b))
                    (mk-constraint vss 0 2)
                    [[1 2] [1 4] [2 3] [2 5] [3 6] [4 5] [5 6] [5 7]])
       lz  (lazy-seq  (LJava.LJ/lazy  (LJava.LJ/relation vss) LJava.LJ/DIFFER c ))]
    (doseq [l (take 10 lz)]
      (println (map (memfn get) (seq (.toArray l vss)))))
    ))

(testi)


;;;; other attempts


(defn tes3t []
  (let [vss (LJava.LJ/varArray 8)
        c   (reduce (fn [c [a b]] (mk-comp c vss a b))
                    (mk-constraint vss 0 2)
                    [[1 2] [1 4] [2 3] [2 5] [3 6] [4 5] [5 6] [5 7]])
        lz  (LJava.LJ/a  (LJava.LJ/relation vss) LJava.LJ/DIFFER c)
       ]
    (println (alength (.getValues (aget vss 0))))))

;(tes3t)


(defn tes2t []
  (let [vss (LJava.LJ/varArray 8)
        c   (reduce (fn [c [a b]] (mk-comp c vss a b))
                    (mk-constraint vss 0 2)
                    [[1 2] [1 4] [2 3] [2 5] [3 6] [4 5] [5 6] [5 7]])
   ;    lz  (lazy-seq  (LJava.LJ/lazy  (LJava.LJ/relation vss) LJava.LJ/DIFFER c ))
        lz   (LJava.LJ/lazy  (LJava.LJ/relation vss) LJava.LJ/DIFFER c )
       ]
    (print (.next lz))
;    (doseq [l lz] (println l))
    ))





;(tes2t)

   #_(doseq [^Integer i (range 0 (count vss))]
      (doseq [^Integer j (range 0 (alength (.getValues (aget vss 0))))]
        (println  (aget (.getValues (aget vss i)) j ))))
