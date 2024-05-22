; Визначення початкових фактів
(deffacts initial-data
    (number-series (create$ 0.9 20 30 40 50 60 70 80 10 100)) ; Список чисел
    (alphabet (create$ A B C D E F G)) ; Алфавіт
    (dirichlet-params (create$ 1.0 1.0 1.0 1.0 1.0 1.0)) ; Параметри Діріхле
)

; Функція для сортування числового ряду
(deffunction sort-numbers (?numbers)
    (bind ?sorted ?numbers)
    (bind ?changed TRUE)
    (while ?changed
        (bind ?changed FALSE)
        (bind ?i 1)
        (loop-for-count (?j 1 (- (length$ ?sorted) 1))
            (bind ?first (nth$ ?i ?sorted))
            (bind ?second (nth$ (+ ?i 1) ?sorted))
            (if (> ?first ?second)
                then
                    (bind ?sorted (replace$ ?sorted ?i ?i ?second))
                    (bind ?sorted (replace$ ?sorted (+ ?i 1) (+ ?i 1) ?first))
                    (bind ?changed TRUE)
            )
            (bind ?i (+ ?i 1))
        )
    )
    (return ?sorted)
)

; Функція для генерації інтервалів
(deffunction generate-intervals (?params)
    (bind ?total 0.0)
    (foreach ?p ?params
        (bind ?total (+ ?total (float ?p)))
    )
    (bind ?cumulative 0.0)
    (bind ?intervals (create$ 0))  ; Початковий елемент - 0
    (foreach ?p ?params
        (bind ?cumulative (+ ?cumulative (/ (float ?p) ?total)))
        (bind ?intervals (insert$ ?intervals (+ 1 (length$ ?intervals)) (* ?cumulative 100)))
    )
    (return ?intervals)
)

; Функція для визначення відповідного символу
(deffunction assign-symbol (?number ?intervals ?alphabet)
    (bind ?index 1)
    (loop-for-count (?i 2 (length$ ?intervals))
        (if (<= ?number (nth$ ?i ?intervals))
            then
                (return (nth$ (- ?i 1) ?alphabet))
        )
    )
    (return (nth$ (length$ ?alphabet) ?alphabet))  ; Обробка випадку останнього інтервалу
)

; Правило для обробки числового ряду
(defrule process-numbers
    ?f <- (number-series $?series)
    ?a <- (alphabet $?alpha)
    ?p <- (dirichlet-params $?params)
    =>
    (bind ?sorted-series (sort-numbers ?series))
    (bind ?intervals (generate-intervals ?params))
    (bind ?symbols (create$))
    (foreach ?num ?sorted-series
        (bind ?symbol (assign-symbol ?num ?intervals ?alpha))
        (bind ?symbols (insert$ ?symbols (+ 1 (length$ ?symbols)) ?symbol))
    )
    (assert (linguistic-series ?symbols))
    (retract ?f ?a ?p)
)

; Правило для виведення результатів
(defrule output
    ?ls <- (linguistic-series $?symbols)
    =>
    (printout t "Лінгвістичний ряд: ")
    (foreach ?symbol ?symbols
        (printout t ?symbol " ")
    )
    (printout t crlf)
    (retract ?ls)
)
