#lang racket

; EXPR-COMPARE

; (expr-compare '(lambda (a b) a b c) '(lambda (a b) c b a)) --> too many parenthesis
; (expr-compare '(lambda (a b) a b c) '(lambda (a b) a)) --> too many parenthesis around (a b c)

; definition of lambda symbol
(define lambda-symbol (string->symbol "\u03BB"))

; returns true if x and y have >1 elements and are same length
(define (multiple-elements-same-length x y)
  (if (and (and (pair? x) (pair? y)) (= (length x) (length y))) #t #f)
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
  (if (and (or (equal? 'lambda (car x)) (equal? lambda-symbol (car x)))
           (or (equal? 'lambda (car y)) (equal? lambda-symbol (car y))))
    #t
    #f
  )
)

; returns either same variable y or bounded variable x!y
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
  (list (compare-bound-variables (car x) (car y)) (expr-compare-helper (car (cdr x)) (car (cdr y)) x-vals y-vals))
)

; binds multiple terms
(define (multiple-bind-kv keys vals)
        ; first element of keys == let
  (cond ((equal? 'let (car keys))
          (let* ((keys-variables  (map car (car (cdr keys))))
                 (keys-vals (map cadr (car (cdr keys))))
                 (bounded-keys-vals (map (lambda (kv) (bind-kv kv vals)) keys-vals))
                 (keys-body (car (cdr (cdr keys)))))
              (list 'let (map list keys-variables bounded-keys-vals) (bind-kv keys-body vals))
          )
        )
        ; first element of keys == lambda
        ((equal? 'lambda (car keys))
          (let* ((formals (car (cdr keys)))
                 (length-body (length (cdr (cdr keys))))
                 (body (if (= length-body 1) (car (cdr (cdr keys))) (cdr (cdr keys)))))
                    (list 'lambda formals (bind-kv body vals))
          )
        )
        ; first element of keys == quote
        ((equal? 'quote (car keys)) keys)
        ; otherwise
        (else (cons (bind-kv (car keys) vals) (bind-kv (cdr keys) vals)))
  )
)

; binds single terms
(define (single-bind-kv keys vals)
  (if (pair? vals)
    (if (equal? keys (car (car vals)))
      (car (cdr (car vals)))
      (single-bind-kv keys (cdr vals))
    )
    keys
  )
)

; bind key and vals
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

; get diff summary of lambda function
(define (get-diff-lambda x y x-keys y-keys x-body y-body x-vals y-vals)
  ; check to see if arg has parenthesis or not
  (if (or (not(pair? x-keys)) (not(pair? y-keys)))
    ; if no parenthesis, just bind them
    (compare-diff-length (bind-kv x x-vals) (bind-kv y y-vals))

    ; otherwise compare arguments in parenthesis
    (let ((new-x-keys (if (list? x-keys) x-keys (list (car x-keys) '\. (cdr x-keys))))
          (new-y-keys (if (list? y-keys) y-keys (list (car y-keys) '\. (cdr y-keys))))
         )
      ; if x-keys and y-keys are same length, run compare-bound-variables to get bound-variables
      (if (= (length new-x-keys) (length new-y-keys))
        (let* ((bound-variables (map compare-bound-variables new-x-keys new-y-keys))
        (diff-body (expr-compare-helper x-body y-body (append (map list new-x-keys bound-variables) x-vals) (append (map list new-y-keys bound-variables) y-vals)))
        (lambda-or-symbol (if (and (equal? 'lambda (car x)) (equal? 'lambda (car y))) 'lambda lambda-symbol)))
          (list lambda-or-symbol bound-variables diff-body)
        )
        ; otherwise compare terms using compare-diff-length
        (compare-diff-length (bind-kv x x-vals) (bind-kv y y-vals))
      )
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

; else statement
(define (create-pair x y x-vals y-vals)
  (cons (expr-compare-helper (car x) (car y) x-vals y-vals) (expr-compare-helper (cdr x) (cdr y) x-vals y-vals))
)

; helper function for expr-compare
(define (expr-compare-helper x y x-vals y-vals)
  ; check if more than one element in x and y lists
  (if (multiple-elements-same-length x y)
          ; x and y start with lambda
    (cond ((both-have-lambda x y)
            (let* ((x-keys (car (cdr x))) (y-keys (car (cdr y)))
                   (length-x-body (length (cdr (cdr x))))
                   (length-y-body (length (cdr (cdr y))))
                   (x-body (if (= length-x-body 1) (car (cdr (cdr x))) (cdr (cdr x))))
                   (y-body (if (= length-y-body 1) (car (cdr (cdr y))) (cdr (cdr y)))))
                      (get-diff-lambda x y x-keys y-keys x-body y-body x-vals y-vals)
            )
          )
          ; x and y start with let 
          ((both-have-let x y)
            ; x/y-keys are local variables
            ; x/y-body are body
            (let ((x-keys (car (cdr x))) (y-keys (car (cdr y))) (x-body (car (cdr (cdr x)))) (y-body (car (cdr (cdr y)))))
              ; get diff summary of let statement
              (get-diff-let x y x-keys y-keys x-body y-body x-vals y-vals)
            )
          )         
          ; either x or y starts with quote
          ((one-has-quote x y)
            (compare-diff-length (bind-kv x x-vals) (bind-kv y y-vals))
          )
          ; otherwise
          (else
            (create-pair x y x-vals y-vals)
          )
    )
    ; if last element in x and y lists or x and y have different lengths
    (compare-diff-length (bind-kv x x-vals) (bind-kv y y-vals))
  )
)

; compare scheme expressions and produce diff summary
(define (expr-compare x y)
  (expr-compare-helper x y '() '())
)

; TEST-EXPR_COMPARE

; if #t then assign % as #t, otherwise assign % as #f
(define (assign-boolean bool expr)
  (if bool
    (list 'let '((% #t)) expr)
    (list 'let '((% #f)) expr)
  )
)

; evaluate expression returned by expr-compare with % bound to #t/f
; returns #t or #f if expressions equal each other
(define (test-expr-compare x y)
  (and 
    (equal? (eval (assign-boolean #t (expr-compare x y))) (eval x))
    (equal? (eval (assign-boolean #f (expr-compare x y))) (eval y))
  )
)

; TEST-EXPR-X/Y

; variables used to test expr-compare well by exercising all specifications
(define test-expr-x
  '(list 'a (cons
    '(let ((x 1))
      (* x 3)
    )
    (quote (lambda (b)
      (let ((c (+ 3 2) (d (- 7 5)))
        (if (and (> c d) (< b c))
          #t
          #f
        )
      )))
    )
  ))
)

(define test-expr-y
  '(list 'b (cons
    '(let ((y 1))
      (* y 2)
    )
    (quote (lambda (w)
      (let ((r (- 1 5) (d (+ 2 3)))
        (if (and (> r d) (< w r))
          #f
          #t
        )
      )))
    )
  ))
)




