(ns morthelper.core
  (:gen-class))

;pushing dummy numbers here
(def property-price 1111111111)
(def years-of-mortgage 11)
(def deposit 11111)
(def mortgage (- property-price deposit))
(def full-rate 11)
(def initial-period-length 1)
(def initial-period-rate 1)

(defn pmt [baseval r years]
  (* baseval
     (/ r
        (- 1
           (Math/pow (+ 1 r)
                     (* -1 (* 12 years))
                     )))))

(defn r-ify [rate]
  (/ rate 12 100))

(defn deal-breakdown [mortgage tot-years deal-period-years rate]
  (let [monthly-to-be-repaid (pmt mortgage (r-ify rate) tot-years)
        monthly-interest-fn (fn [outstanding-loan r] (* outstanding-loan (/ (/ r 100) 12)))]
    (loop [months-left (* 12 deal-period-years)
           month-interest (monthly-interest-fn mortgage rate)
           month-principal (- monthly-to-be-repaid month-interest)
           left-to-pay (- mortgage month-principal)
           total-interest-paid month-interest
           total-principal-paid month-principal]
      (if
        (<= months-left 1)
        {:monthly-payments   monthly-to-be-repaid
         :tot-principal-paid total-principal-paid
         :tot-interest-paid  total-interest-paid
         :tot-paid           (+ total-principal-paid total-interest-paid)
         :left-to-pay        left-to-pay}
        (do
          (let [next-month-interest (monthly-interest-fn left-to-pay rate)
                next-month-principal (- monthly-to-be-repaid next-month-interest)]
            (recur
              (dec months-left)
              next-month-interest
              next-month-principal
              (- left-to-pay next-month-principal)
              (+ total-interest-paid next-month-interest)
              (+ total-principal-paid next-month-principal))))))))

(defn full-breakdown [mortgage tot-years initial-period-years initial-period-rate full-rate]
  (let [initial-period-breakdown (deal-breakdown mortgage tot-years initial-period-years initial-period-rate)
        remaining-period-years (- tot-years initial-period-years)
        remaining-period-breakdown (deal-breakdown (:left-to-pay initial-period-breakdown)
                                                   remaining-period-years
                                                   remaining-period-years
                                                   full-rate)]
    {:initial-period-breakdown   initial-period-breakdown
     :remaining-period-breakdown remaining-period-breakdown
     :tot-principal-paid         (+ (:tot-principal-paid initial-period-breakdown) (:tot-principal-paid remaining-period-breakdown))
     :tot-interest-paid          (+ (:tot-interest-paid initial-period-breakdown) (:tot-interest-paid remaining-period-breakdown))
     :tot-paid                   (+ (:tot-paid initial-period-breakdown) (:tot-paid remaining-period-breakdown))
     }))

(full-breakdown mortgage years-of-mortgage initial-period-length initial-period-rate full-rate)
