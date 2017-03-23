(ns looping-is-recursion)

(defn power [base exp]
  (if (zero? exp)
    1
    (let [helper (fn [acc n]
                   (if (= n exp)
                     acc
                     (recur (* base acc) (inc n))))]
      (helper base 1))))

(defn last-element [a-seq]
  (let [rem-elem (fn [b-seq]
                   (if (empty? (rest b-seq))
                     (first b-seq)
                     (recur (rest b-seq))))]
    (rem-elem a-seq)))

(defn seq= [seq1 seq2]
  (let [compare-first-elem (fn [a-seq b-seq]
                             (cond
                               (and (empty? a-seq) (empty? b-seq))
                               true
                               (or (empty? a-seq) (empty? b-seq))
                               false
                               (not= (first a-seq) (first b-seq))
                               false
                               :else (recur (rest a-seq) (rest b-seq))))]
    (compare-first-elem seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         xs a-seq]
    (when (not (empty? xs))
      (if (pred (first xs))
        acc
        (recur (inc acc) (rest xs))))))

(defn avg [a-seq]
  (loop [sum 0
         div 0
         xs a-seq]
    (if (empty? xs)
      (/ sum div)
      (recur (+ sum (first xs)) (inc div) (rest xs)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [a-set #{}
         xs a-seq]
    (if (empty? xs)
      a-set
      (recur (toggle a-set (first xs)) (rest xs)))))

(defn fast-fibo [n]
  (loop [result 0
         next-num 1
         acc n]
    (if (zero? acc)
      result
      (recur (+ result next-num) result (dec acc)))))

(defn cut-at-repetition [a-seq]
  (loop [a-vec []
         b-seq a-seq]
    (if (empty? b-seq)
      a-vec
      (if (contains? (set a-vec) (first b-seq))
        a-vec
        (recur (conj a-vec (first b-seq)) (rest b-seq))))))

