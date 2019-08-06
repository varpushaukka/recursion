(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max-apu [seq-1 seq-2]
  (if (empty? seq-1)
    2
    (if (empty? seq-2)
      1
      (seq-max-apu (rest seq-1) (rest seq-2)))))

(defn seq-max [seq-1 seq-2]
  (if (= 1 (seq-max-apu seq-1 seq-2))
    seq-1
    seq-2))

(defn longest-sequence [seq-seq]
  (if (empty? seq-seq)
    nil
    (if (singleton? seq-seq)
      (first seq-seq)
      (seq-max (first seq-seq) (longest-sequence (rest seq-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    []
    (if (not (pred? (first a-seq)))
      []
      (if (pred? (first a-seq))
        (cons (first a-seq)
              (my-take-while pred? (rest a-seq)))))))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) []
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false
    ))

(defn my-map [f seq-1 seq-2]
  (cond 
    (empty? seq-2) seq-2
    (empty? seq-1) seq-1
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
    ))

(defn power [n k]
  (if (zero? k) 
    1
    (* n (power n (dec k)))
    ))


(defn fib [n]
  (if (zero? n) 
    0
    (if (= 1 n)
      1
      (+ (fib (dec n)) (fib (- n 2))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (conj (my-repeat (dec how-many-times) what-to-repeat) what-to-repeat)))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (conj (my-range (dec up-to)) (dec up-to))
    ))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons [] a-seq)
    (cons a-seq (tails (rest a-seq)))))
  
(defn inits [a-seq]
  (cons [] 
        (map (fn init [x] 
               (if (zero? x)
                 []
                 (conj (init (dec x)) x)))
             a-seq)))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [[]]
    (loop [result []
           r (count a-seq)]
      (if (zero? r) 
        result
        (recur (conj result (concat (drop r a-seq) (take r a-seq))) (dec r))))))
  
(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq) 
    freqs
    (let [fr (first a-seq)]
      (if (get freqs fr)
        (my-frequencies-helper (assoc freqs fr (inc (get freqs fr))) (rest a-seq))
        (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (flatten (for [k a-map]
                  (repeat (second k) (first k)))))

(defn my-take [n coll]
  (cond
    (empty? coll) coll
    (zero? n) []
    :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  [:-])

(defn halve [a-seq]
  [:-])

(defn seq-merge [a-seq b-seq]
  [:-])

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

