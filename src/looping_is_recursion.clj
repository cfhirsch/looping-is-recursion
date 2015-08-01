(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n k]
                 (if (zero? k)
                    acc
                   (recur (* acc n) n (dec k))))]
    (helper 1 base exp)))
;4

(defn last-element [a-seq]
  (cond
     (empty? a-seq)
       nil
     (== 1 (count a-seq))
       (first a-seq)
     :else
       (last-element (drop 1 a-seq))))
;7

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2))
      true
    (not (==  (count seq1) (count seq2)))
      false
    (not (== (first seq1) (first seq2)))
      false
    :else
      (seq= (drop 1 seq1) (drop 1 seq2))))
;13


(defn find-first-index [pred a-seq]
  (loop [i 0]
    (cond
      (== (count a-seq) i)
        nil
      (pred (get a-seq i))
        i
      :else
        (recur (inc i)))))
;17

(defn avg [a-seq]
  (loop [i 0
         sum 0]
    (if (== (count a-seq) i)
       (/ sum i)
       (recur (inc i) (+ sum (get a-seq i))))))
;20

(defn toggle [a-set elem]
   (if (contains? a-set elem)
       (disj a-set elem)
       (conj a-set elem)))

(defn parity [a-seq]
  (loop [i 0
         oddSet (set [])]
    (if (== (count a-seq) i)
      oddSet
      (recur (inc i) (toggle oddSet (get a-seq i))))))
;23


(defn fast-fibo [n]
  (loop [current 0
         previous 0
         i 0]
    (cond (== 1 n)
            1
          (== i n)
            (+ current previous)
          (== 1 i)
            (recur 1 0 (inc i))
          :else
            (recur (+ current previous) current (inc i)))))
;28


(defn cut-at-repetition [a-seq]
  (loop [i 0
         accum []
         encountered (set [])]
    (let [elem (get a-seq i)]
       (if (or (== i (count a-seq))
               (contains? encountered elem))
         accum
         (recur (inc i) (conj accum elem) (conj encountered elem))))))
;31
