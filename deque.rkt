#lang racket
(define (make-deque) (mcons null null))

(define (front deque) (mcar deque))
(define (rear deque) (mcdr deque))

(define (set-front! deque item) (set-mcar! deque item))
(define (set-rear! deque item) (set-mcdr! deque item))

(define (empty? deque) (null? (front deque)))

(define (one? deque) (null? (mcdr (mcar (front deque)))))

(define (print deque)
  (define (collect deque result)
    (if (null? deque)
        result
        (collect (mcdr deque)
                 (cons (mcar (mcar deque)) result))))
  (collect (rear deque) null))

(define (insert-front! deque item)
  (let ([new-pair (mcons (mcons item null) null)])
    (cond ((empty? deque)
           (set-front! deque new-pair)
           (set-rear! deque new-pair))
          (else
           (set-mcdr! (mcar new-pair) (front deque))           
           (set-mcdr! (front deque) new-pair)
           (set-front! deque new-pair))))
  (print deque))

(define (insert-rear! deque item)
  (let ([new-pair (mcons (mcons item null) null)])
    (cond ((empty? deque)
           (set-front! deque new-pair)
           (set-rear! deque new-pair))
          (else
           (set-mcdr! (mcar (rear deque)) new-pair)
           (set-mcdr! new-pair (rear deque))
           (set-rear! deque new-pair))))
  (print deque))

(define (delete-front! deque)
  (cond ((empty? deque)
         (error "DELETE-FRONT called with an empty queue"))
        ((one? deque)
         (set-front! deque null)
         (set-rear! deque null))
        (else
         (set-mcdr! (mcdr (mcar (front deque))) null)
         (set-front! deque (mcdr (mcar (front deque))))))
  (print deque))

(define (delete-rear! deque)
  (cond ((empty? deque)
         (error "DELETE-REAR called with an empty queue"))
        ((one? deque)
         (set-front! deque null)
         (set-rear! deque null))
        (else
         (set-mcdr! (mcar (mcdr (rear deque))) null)
         (set-rear! deque (mcdr (rear deque)))))
  (print deque))
