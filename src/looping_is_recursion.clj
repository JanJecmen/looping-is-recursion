(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) (dec exp))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [a-seq]
                 (cond
                  (empty? a-seq) nil
                  (empty? (rest a-seq)) (first a-seq)
                  :else (recur (rest a-seq))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [seq1 seq2]
                 (cond
                  (and (empty? seq1) (empty? seq2)) true
                  (or (empty? seq1) (empty? seq2)) false
                  (not (= (first seq1) (first seq2))) false
                  :else (recur (rest seq1) (rest seq2))))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [i 0
         sq a-seq]
    (cond
     (empty? sq) nil
     (pred (first sq)) i
     :else (recur (inc i) (rest sq)))))

(defn avg [a-seq]
  (loop [n 0
         sum 0
         sq a-seq]
    (if (empty? sq)
      (/ sum n)
      (recur (inc n) (+ sum (first sq)) (rest sq)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (loop [result #{}
           sq a-seq]
      (if (empty? sq)
        result
        (recur (toggle result (first sq)) (rest sq))))))

(defn fast-fibo [n]
  (loop [f_n-1 0
         f_n 1
         i n]
    (if (zero? i)
      f_n-1
      (recur f_n (+ f_n-1 f_n) (dec i)))))

(defn cut-at-repetition [a-seq]
  (loop [elems #{}
         result []
         sq a-seq]
    (cond
     (empty? sq) result
     (contains? elems (first sq)) result
     :else (recur (conj elems (first sq))
                  (conj result (first sq))
                  (rest sq)))))
