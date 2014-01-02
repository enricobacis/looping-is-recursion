(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp) acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq)) (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (and  (empty? seq1) (empty? seq2)) true
   (or   (empty? seq1) (empty? seq2)) false
   (not= (first  seq1)  (first seq2))  false
   :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [index 0
         b-seq a-seq]
    (cond (empty? b-seq) nil
          (pred (first b-seq)) index
          :else (recur (inc index) (rest b-seq)))))

(defn avg [a-seq]
  (loop [n     0
         sum   0
         b-seq a-seq]
    (if (empty? b-seq) (/ sum n)
      (recur (inc n) (+ sum (first b-seq)) (rest b-seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [res   #{}
         b-seq a-seq]
    (if (empty? b-seq) res
      (recur (toggle res (first b-seq)) (rest b-seq)))))

(defn fast-fibo [n]
  (loop [to-zero n
         fibn-1  1
         fibn    0]
    (if (zero? to-zero) fibn
      (recur (dec to-zero) fibn (+ fibn-1 fibn)))))

(defn cut-at-repetition [a-seq]
  (loop [res   []
         b-seq a-seq]
    (if (or (empty? b-seq) (contains? (set res) (first b-seq))) res
      (recur (conj res (first b-seq)) (rest b-seq)))))
