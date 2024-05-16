#lang racket

(require racket/math
         racket/list)

; Функція для сортування списку
(define (sort-list lst)
  (sort lst <))

; Функція для отримання індексу інтервалу для значення
(define (get-interval-index value intervals)
  (let loop ([i 0])
    (cond
      [(= i (length intervals)) (- i 1)]
      [(<= value (list-ref intervals i)) i]
      [else (loop (+ i 1))])))

; Функція для конвертації числового ряду в лінгвістичний ряд
(define (convert-to-linguistic-series numeric-series intervals alphabet-power)
  (for/list ([value numeric-series])
    (integer->char (+ 65 (get-interval-index value intervals)))))

; Функція для виведення лінгвістичного ряду
(define (print-linguistic-series linguistic-series)
  (printf "Лінгвістичний ряд: ~a\n" (string-join (map (lambda (ch) (string ch)) linguistic-series) " ")))

; Функція для створення та побудови матриці для лінгвістичного ряду
(define (build-linguistic-matrix linguistic-series)
  (define size 26) ; Розмір алфавіту
  (define matrix (make-vector size)) ; Створення вектора векторів для матриці

  ; Ініціалізація матриці нулями
  (for ([i (in-range size)])
    (vector-set! matrix i (make-vector size 0)))

  ; Функція для конвертації символу в індекс
  (define (char->index ch)
    (- (char->integer ch) 65))

  ; Оновлення матриці з входженнями
  (for ([i (in-range 1 (length linguistic-series))])
    (let* ([prev (char->index (list-ref linguistic-series (- i 1)))]
           [curr (char->index (list-ref linguistic-series i))]
           [current-value (vector-ref (vector-ref matrix prev) curr)])
      (vector-set! (vector-ref matrix prev) curr (+ current-value 1))))

  ; Конвертація матриці у список списків для виведення
  (for/list ([row (in-vector matrix)])
    (vector->list row)))

; Функція для виведення матриці
(define (print-linguistic-matrix matrix)
  (printf "Лінгвістична матриця:\n")
  (for ([row (in-list matrix)])
    (printf "~a\n" (string-join (map number->string row) " "))))

; Функція для генерації рівномірного розподілу інтервалів
(define (generate-equal-interval-distribution alphabet-power)
  (define interval-size (/ 1.0 alphabet-power))
  (define distribution (make-vector alphabet-power))
  (for ([i (in-range alphabet-power)])
    (vector-set! distribution i interval-size))
  distribution)

; Функція для визначення інтервалів
(define (define-intervals distribution)
  (define intervals (make-vector (+ (vector-length distribution) 1)))
  (vector-set! intervals 0 0.0)
  (for ([i (in-range 1 (+ (vector-length distribution) 1))])
    (vector-set! intervals i (+ (vector-ref intervals (- i 1)) (vector-ref distribution (- i 1)))))
  (vector->list intervals))

; Головна функція
(define (main)
  (random-seed (current-seconds))
  (define n 6) ; Кількість елементів у числовому ряді
  (define numeric-series (list 0.0 3.0 1.0 2.0 3.0 1.5)) ; Числовий ряд
  (define alphabet-power 2) ; Ступінь алфавіту
  (define distribution (generate-equal-interval-distribution alphabet-power)) ; Розподіл ймовірності
  (define sorted-numeric-series (sort-list numeric-series)) ; Відсортований числовий ряд
  (define intervals (define-intervals distribution)) ; Інтервали
  (define linguistic-series (convert-to-linguistic-series sorted-numeric-series intervals alphabet-power)) ; Лінгвістичний ряд
  (print-linguistic-series linguistic-series)
  (define linguistic-matrix (build-linguistic-matrix linguistic-series)) ; Побудова матриці
  (print-linguistic-matrix linguistic-matrix)) ; Виведення матриці

(main)
