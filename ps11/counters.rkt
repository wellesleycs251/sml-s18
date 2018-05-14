(define make-counter1
  (let ((count 0))
    (λ ()
      (λ ()
        (begin (set! count (+ count 1))
               count)))))

(define make-counter2
  (λ ()
    (let ((count 0))
      (λ ()
        (begin (set! count (+ count 1))
               count)))))

(define make-counter3
  (λ ()
    (λ ()
      (let ((count 0))
        (begin (set! count (+ count 1))
               count)))))

(define test-counter
  (λ (make-counter)
    (let ((a (make-counter))
          (b (make-counter)))
      (begin (println (a))
             (println (b))
             (println (a))))))