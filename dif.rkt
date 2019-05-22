;;; Dif
;;; James Stevenson jssteven@uw.edu

#lang racket
(provide diff)
(provide simpl)

;; Basic accessors & helper procedures

(define get-op (lambda (expr)
                 (car expr)))

(define get-left (lambda (expr)
                   (cadr expr)))

(define get-remaining (lambda (expr)
                        (append (list (get-op expr)) (cddr expr))))

(define get-right (lambda (expr)
                    (caddr expr)))

(define opt? (lambda (char)
               (cond [(equal? '+ char) #t]
                     [(equal? '- char) #t]
                     [(equal? '* char) #t]
                     [(equal? '/ char) #t]
                     [(equal? 'expt char) #t]
                     [else #f])))

;; Core functions

(define (const? v E)
  (if (not (list? E))
    (or (number? E) (not (equal? v E)))
    (cond [(equal? (length E) 1) (const? v (car E))]
          [(equal? (length E) 2) (const? v (get-left E))]
          [(equal? (length E) 3) (and (const? v (get-left E)) (const? v (get-right E)))]
          [else (and (const? v (get-left E)) (const? v (get-remaining E)))])))

(define (get-func op table)
  (cadr (assoc op table)))

(define (diff v E)
  (cond [(const? v E) (diff-constant v E)]
        [(symbol? E) 1]
        [else ((get-func (get-op E) diff-dispatch) v E)]))

;;; Specific differentiation procedures

(define (diff-constant v E)
  0)

(define (simpl v E)
  (if (integer? E)
    E
    (let ([left (simpl v (get-left E))]
          [right (simpl v (get-right E))])
      (if (equal? (get-op E) '*)
        (cond [(or (equal? left 0) (equal? right 0)) 0]
              [(equal? left 1) right]
              [(equal? right 1) left])
        (if (and (integer? left) (integer? right))
          ((get-op E) left right)
          (list (get-op E) left right))))))

(define (diff-addit v E)
  (let ([ds-term (lambda (term) (diff v term))])
    (append (list (get-op E)) (map ds-term (cdr E)))))

(define (diff-mult v E)
  (list '+
        (list '* (diff v (get-left E)) (get-right E))
        (list '* (get-left E) (diff v (get-right E)))))

(define (diff-div v E)
  (let ([f (diff v (get-left E))]
        [g (diff v (get-right E))])
    (list '/
          (list '-
                (list '* (diff v f) g)
                (list '* f (diff v g)))
          (list expt g 2))))

(define (diff-expt v E)
  (if (symbol? (get-left E))
    (list '* (get-right E) (list 'expt (get-left E) (- (get-right E) 1)))
    (list '*
          (get-right E)
          (list '*
                (list 'expt (get-left E) (- (get-right E) 1))
                (diff v (get-left E))))))

;; Trigenometric derivatives
(define (diff-sin v E)
  (list '* (list 'cos (get-left E)) (diff v (get-left E))))

(define (diff-cos v E)
  (list '* (list 'sin (get-left E)) (list '* -1 (diff v (get-left E)))))

(define (diff-tan v E)
  (list '* (list 'expt (list 'sec (get-left E)) 2) (diff v (get-left E))))

(define (diff-csc v E)
  (list '*
        (list '*
              -1
              (list '*
                    (list 'csc (get-left E))
                    (list 'cot (get-left E))))
        (diff v (get-left E))))

(define (diff-sec v E)
  (list '*
        (list '*
              (list 'sec (get-left E))
              (list 'tan (get-left E)))
        (diff v (get-left E))))

(define (diff-cot v E)
  (list '*
        (list '*
              (list 'expt
                    (list 'csc (get-left E))
                    2)
              -1)
        (diff v (get-left E))))


;;; Dispatch table of supported operators
(define diff-dispatch
  (list (list '+ diff-addit)
        (list '- diff-addit)
        (list '* diff-mult)
        (list '/ diff-div)
        (list 'expt diff-expt)
        (list 'sin diff-sin)
        (list 'cos diff-cos)
        (list 'tan diff-tan)
        (list 'csc diff-csc)
        (list 'sec diff-sec)
        (list 'cot diff-cot)
        ))

;; Computation table of supported operators
(define simpl-dispatch
  (list (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list 'expt expt)))

