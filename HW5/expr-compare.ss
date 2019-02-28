#lang racket

; (expr-compare '(let ((x 1) (y 2)) (+ x y)) '(let ((a 1) (b 2)) (+ a b)))
; (expr-compare '(let ((x 1) (y 2)) (+ x y)) '(let ((a 1)) (+ a 1)))
; (expr-compare '((lambda (a) (f a)) 1) '((λ (a) (g a)) 2))
; (expr-compare '(+ #f ((λ (a b) (f a b)) 1 2)) '(+ #t ((lambda (a c) (f a c)) 1 2)))
; (expr-compare '((λ (a b) (f a b)) 1 2) '((λ (a c) (f c a)) 1 2))
; (expr-compare '((lambda (a) (eq? a ((λ (a b) ((λ (a b) (a b)) b a))
;                                    a (lambda (a) a))))
;               (lambda (b a) (b a)))
;              '((λ (a) (eqv? a ((lambda (b a) ((lambda (a b) (a b)) b a))
;                                a (λ (b) a))))
;                (lambda (a b) (a b))))

; returns true if x and y have >1 elements and are same length
(define (has-multiple-elements x y)
  (if (and (pair? x) (= (length x) (length y))) #t #f)
)

; returns true if x and y both start with 'let'
(define (both-have-let x y)
  (if (and (equal? 'let (car x)) (equal? 'let (car y))) #t #f)
)

; returns true if either x or y start with 'quote'
(define (one-has-quote x y)
  (if (or (equal? 'quote (car x)) (equal? 'quote (car y))
    (xor (equal? (car x) 'if) (equal? (car y) 'if))) #t #f)
)

; returns true if x and y both start with 'lambda'
(define (both-have-lambda x y)
  (if (and (equal? 'lambda (car x)) (equal? 'lambda (car y))) #t #f)
)

; returns either same variable y or bounded variable x ! y
(define (compare-bound-variables x y)
  ; if variable names are same, return either x or y
  (if (equal? x y)
      y
      ; otherwise, return x!y
      (string->symbol (string-append (symbol->string x) "!" (symbol->string y)))
  )
)

; returns list of (bounded) variables attached to associated value (i.e. (x!y 1))
(define (get-bound-variables x y x-vals y-vals)
  ; attach bounded variables to associated value in list
  (list (compare-bound-variables (car x) (car y))
        (expr-compare-helper (cadr x) (cadr y) x-vals y-vals)
  )
)

; binds single terms
(define (single-bind-kv keys vals)
  (if (pair? vals)
    (if (equal? keys (caar vals))
        (car (cdar vals))
        (single-bind-kv keys (cdr vals))
    )
    keys
  )
)

; binds multiple terms
(define (multiple-bind-kv keys vals)
        ; first element of keys == let
  (cond ((equal? 'let (car keys))
          (let* ((keys-variables  (map car (cadr keys)))
                 (keys-vals (map cadr (cadr keys)))
                 (bounded-keys-vals (map (lambda (kv) (bind-kv kv vals)) keys-vals))
                 (keys-body (caddr keys)))
              (list 'let (map list keys-variables bounded-keys-vals) (bind-kv keys-body vals))
          )
        )
        ; first element of keys == lambda
        ((equal? 'lambda (car keys))
          (let* ((formals (cadr keys))
                 (body (caddr keys)))
              (list 'lambda
                    formals
                    (bind-kv body vals)))
        )
        ; first element of keys == quote
        ((equal? 'quote (car keys)) keys)
        ; otherwise
        (else (cons (bind-kv (car keys) vals) (bind-kv (cdr keys) vals)))
    )
)

(define (bind-kv keys vals)
  (if (pair? keys)
    ; if more than one element in keys
    (multiple-bind-kv keys vals)
    ; only one element in keys
    (single-bind-kv keys vals)
  )
)
 
; returns y if x and y equal, otherwise returns
(define (compare-diff-length x y)
  (if (equal? x y)
    y
    ; if x and y different, either use %/(not %) or 'if % x y'
    (if (and (boolean? x) (equal? x (not y))) ; if x is boolean and x =/= y
      (if x '% '(not %))
      (list 'if '% x y)
    )
  )
)

; get diff summary of let statement
(define (get-diff-let x y x-keys y-keys x-body y-body x-vals y-vals)
  ; if x-keys and y-keys same length, run get-bound-variables
  (if (= (length x-keys) (length y-keys))
    (let* ((bound-variables (map (lambda (x y) (get-bound-variables x y x-vals y-vals)) x-keys y-keys))
      (diff-body (expr-compare-helper x-body y-body (append (map list (map car x-keys) (map car bound-variables)) x-vals) (append (map list (map car y-keys) (map car bound-variables)) y-vals))))
      (list 'let bound-variables diff-body)
    )
    ; otherwise compare terms using compare-diff-length
    (compare-diff-length (bind-kv x x-vals) (bind-kv y y-vals))
  )
)

; get diff summary of lambda function
(define (get-diff-lambda x y x-keys y-keys x-body y-body x-vals y-vals)
  ; if x-keys and y-keys are same length, run compare-bound-variables
  (if (= (length x-keys) (length y-keys))
    (let* ((bound-variables (map compare-bound-variables x-keys y-keys))
    (diff-body (expr-compare-helper x-body y-body (append (map list x-keys bound-variables) x-vals) (append (map list y-keys bound-variables) y-vals))))
      (list 'lambda bound-variables diff-body)
    )
    ; otherwise compare terms using compare-diff-length
    (compare-diff-length (bind-kv x x-vals) (bind-kv y y-vals))
  )
)

; else statement
(define (yeet x y x-vals y-vals)
  (cons (expr-compare-helper (car x) (car y) x-vals y-vals) (expr-compare-helper (cdr x) (cdr y) x-vals y-vals))
)

(define (expr-compare-helper x y x-vals y-vals)
  ; check if more than one element in x and y lists
  (if (has-multiple-elements x y)
          ; either x or y starts with quote
    (cond ((one-has-quote x y)
            (compare-diff-length (bind-kv x x-vals) (bind-kv y y-vals))
          )
          ; x and y start with let 
          ((both-have-let x y)
            ; x/y-keys are local variables
            ; x/y-body are body
            (let ((x-keys (cadr x)) (y-keys (cadr y)) (x-body (caddr x)) (y-body (caddr y)))
              ; get diff summary of let statement
              (get-diff-let x y x-keys y-keys x-body y-body x-vals y-vals)
            )
          )
          ; x and y start with lambda
          ((both-have-lambda x y)
            (let ((x-keys (cadr x)) (y-keys (cadr y)) (x-body (caddr x)) (y-body (caddr y)))
              (get-diff-lambda x y x-keys y-keys x-body y-body x-vals y-vals)
            )
          )
          ; otherwise
          (else
            (yeet x y x-vals y-vals)
          )
    )
    ; if last element in x and y lists
    (compare-diff-length (bind-kv x x-vals) (bind-kv y y-vals))
  )
)

(define (expr-compare x y)
  (expr-compare-helper x y '() '())
)