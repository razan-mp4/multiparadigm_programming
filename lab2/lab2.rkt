#lang racket

(require racket/math
         racket/list)



; Function to sort a list
(define (sort-list lst)
  (sort lst <))

; Function to get the interval index for a value
(define (get-interval-index value intervals)
  (let loop ([i 0])
    (cond
      [(= i (length intervals)) (- i 1)]
      [(<= value (list-ref intervals i)) i]
      [else (loop (+ i 1))])))

; Function to convert numeric series to linguistic series
(define (convert-to-linguistic-series numeric-series intervals alphabet-power)
  (for/list ([value numeric-series])
    (integer->char (+ 65 (get-interval-index value intervals)))))

; Function to print linguistic series
(define (print-linguistic-series linguistic-series)
  (printf "Лінгвістичний ряд: ~a\n" (string-join (map (lambda (ch) (string ch)) linguistic-series) " ")))

; Function to create and build matrix for linguistic series
(define (build-linguistic-matrix linguistic-series)
  (define size 26) ; Size of the alphabet
  (define matrix (make-vector size)) ; Create a vector of vectors for the matrix

  ; Initialize the matrix with zeros
  (for ([i (in-range size)])
    (vector-set! matrix i (make-vector size 0)))

  ; Function to convert char to index
  (define (char->index ch)
    (- (char->integer ch) 65))

  ; Update the matrix with occurrences
  (for ([i (in-range 1 (length linguistic-series))])
    (let* ([prev (char->index (list-ref linguistic-series (- i 1)))]
           [curr (char->index (list-ref linguistic-series i))]
           [current-value (vector-ref (vector-ref matrix prev) curr)])
      (vector-set! (vector-ref matrix prev) curr (+ current-value 1))))

  ; Convert the matrix to a list of lists for printing
  (for/list ([row (in-vector matrix)])
    (vector->list row)))

; Function to print the matrix
(define (print-linguistic-matrix matrix)
  (printf "Linguistic Matrix:\n")
  (for ([row (in-list matrix)])
    (printf "~a\n" (string-join (map number->string row) " "))))

; Function to generate equal interval distribution
(define (generate-equal-interval-distribution alphabet-power)
  (define interval-size (/ 1.0 alphabet-power))
  (define distribution (make-vector alphabet-power))
  (for ([i (in-range alphabet-power)])
    (vector-set! distribution i interval-size))
  distribution)

; Function to define intervals
(define (define-intervals distribution)
  (define intervals (make-vector (+ (vector-length distribution) 1)))
  (vector-set! intervals 0 0.0)
  (for ([i (in-range 1 (+ (vector-length distribution) 1))])
    (vector-set! intervals i (+ (vector-ref intervals (- i 1)) (vector-ref distribution (- i 1)))))
  (vector->list intervals))

; Main function
(define (main)
  (random-seed (current-seconds))
  (define n 6) ; Number of elements in numeric series
  (define numeric-series (list 0.0 3.0 1.0 2.0 3.0 1.5)) ; Numeric series
  (define alphabet-power 2) ; Alphabet power
  (define distribution (generate-equal-interval-distribution alphabet-power)) ; Probability distribution
  (define sorted-numeric-series (sort-list numeric-series)) ; Sorted numeric series
  (define intervals (define-intervals distribution)) ; Intervals
  (define linguistic-series (convert-to-linguistic-series sorted-numeric-series intervals alphabet-power)) ; Linguistic series
  (print-linguistic-series linguistic-series)
  (define linguistic-matrix (build-linguistic-matrix linguistic-series)) ; Build matrix
  (print-linguistic-matrix linguistic-matrix)) ; Print matrix

(main)
