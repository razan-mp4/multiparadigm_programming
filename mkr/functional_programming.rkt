#lang racket

(require racket/math
         racket/list
         racket/path
         racket/file
         racket/date)

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
(define (convert-to-linguistic-series numeric-series intervals)
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
  (printf "Матриця передування:\n")
  (for ([row (in-list matrix)])
    (printf "~a\n" (string-join (map number->string row) " "))))

; Функція для визначення інтервалів на основі діапазону даних
(define (define-intervals numeric-series alphabet-power)
  (define min-val (apply min numeric-series))
  (define max-val (apply max numeric-series))
  (define interval-size (/ (- max-val min-val) alphabet-power))
  (define intervals (for/list ([i (in-range 1 alphabet-power)])
                      (+ min-val (* i interval-size))))
  (append intervals (list max-val)))

; Функція для зчитування числового ряду з файлу
(define (read-numeric-series-from-file file-path)
  (define values (file->lines file-path))
  (filter real? (map string->number values)))

; Головна функція
(define (main)
  (random-seed (current-seconds))
  (define file-path "./column_D.csv") ; Шлях до файлу з числовим рядом
  (define numeric-series (read-numeric-series-from-file file-path)) ; Зчитування числового ряду з файлу
  (define alphabet-power 26) ; Ступінь алфавіту (A-Z)
  (define sorted-numeric-series (sort-list numeric-series)) ; Відсортований числовий ряд
  (define intervals (define-intervals sorted-numeric-series alphabet-power)) ; Інтервали
  
  ; Measure time for generating linguistic series
  (define start-series (current-inexact-milliseconds))
  (define linguistic-series (convert-to-linguistic-series sorted-numeric-series intervals)) ; Лінгвістичний ряд
  (define end-series (current-inexact-milliseconds))
  (define time-series (- end-series start-series))
  (print-linguistic-series linguistic-series)
  (printf "Time to generate linguistic series: ~a ms\n" time-series)
  
  ; Measure time for building transition matrix
  (define start-matrix (current-inexact-milliseconds))
  (define linguistic-matrix (build-linguistic-matrix linguistic-series)) ; Побудова матриці
  (define end-matrix (current-inexact-milliseconds))
  (define time-matrix (- end-matrix start-matrix))
  (print-linguistic-matrix linguistic-matrix)
  (printf "Time to build transition matrix: ~a ms\n" time-matrix))

(main)
