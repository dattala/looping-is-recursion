(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc a-seq]
                 (if (empty? a-seq)
                   acc
                   (recur (first a-seq) (rest a-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [acc seq1 seq2]
                 (if (or (false? acc)
                         (and (empty? seq1)
                              (empty? seq2)))
                   acc
                   (recur (= (or (first seq1) 0) (first seq2)) (rest seq1) (rest seq2))))]
    (helper true seq1 seq2)))

(= nil '())

(defn find-first-index [pred a-seq]
  (loop [acc nil
         s a-seq]
    (cond
      (empty? s) nil
      (pred (first s)) acc
      :else (recur (inc (or acc 0)) (rest s)))))

(defn avg [a-seq]
  (loop [total 0
         nb 0
         s a-seq]
    (if (empty? s)
      (if (zero? nb)
        nil
        (/ total nb))
      (recur (+ total (first s)) (inc nb) (rest s)))))

(defn parity [a-seq]
  (loop [acc #{}
         s a-seq]
    (let [toggle (fn [acc x] (if (not (contains? acc x))
                           (conj acc x)
                           (set (filter (fn [y] (not= y x)) acc))))]
      (if (empty? s)
        acc
        (recur (toggle acc (first s)) (rest s))))))

(defn fast-fibo [n]
  (loop [i n
         Fn 1
         Fn-1 0]
    (cond
     (zero? i) Fn-1
     (== 1 i) Fn
     :else (recur (dec i) (+ Fn Fn-1) Fn))))

(defn cut-at-repetition [a-seq]
  (loop [acc []
         s a-seq]
    (let [f (first s)
          found (first (filter (fn [x] (= x f)) acc))]
      (if (or (empty? s) found)
        acc
        (recur (conj acc (first s)) (rest s))))))

