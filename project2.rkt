#lang racket

;; given
(struct num (value grad)
    #:property prop:custom-write
    (lambda (num port write?)
        (fprintf port (if write? "(num ~s ~s)" "(num ~a ~a)")
            (num-value num) (num-grad num))))
;; given
(define relu (lambda (x) (if (> (num-value x) 0) x (num 0.0 0.0))))
;; given
(define mse (lambda (x y) (mul (sub x y) (sub x y))))

(define (third X ) (car (cdr (cdr X ))))

(define (second X) (car (cdr X)))

(define (get-value num-list) (if (list? num-list) (map second num-list) (num-value num-list)) )

(define (get-grad num-list) (if (list? num-list) (map third num-list) (num-grad num-list)) )

(define getv (lambda list (map (lambda (x) (num-value x)) list))) ;hocanın yazdığı

(define getg (lambda list (map (lambda (x) (num-grad x)) list)))


(define add (lambda list (num (apply +(apply getv list)) (apply + (apply getg list)))))

(define (sub num1 num2) (num (- (num-value num1) (num-value num2)) (- (num-grad num1) (num-grad num2))))



(define mul (lambda list (
                          cond [ (= (length list) 2 ) (num (apply * (apply getv list))
                                   (+ (*(num-grad (car list)) (num-value (cadr list)))(*(num-value (car list)) (num-grad (cadr list)))) )]
                               [else (mul (car list) (apply mul (cdr list)))  ])))

(define (vat name value var) ( if 
                                 (eq? name var) (cons name (num value 1.0)  )    
                                  (cons name  (num  value 0.0)  )  ))

;(foldr cons '() '((num 1 2)(num 3 4)) )


(define concat (lambda (keys values) (map (lambda (k v) `(,k ,v)) keys values)))
                              
(define (create-hash names values var) (make-hash (map vat names values (make-list (length names) var))) )

(define (parse hash expr) (cond
                            [(empty? expr) '()]
                            [(list? expr) (cons (parse hash (car expr)) (parse hash (cdr expr)) )    ]
                            [(eq? '+ expr) 'add]
                            [(eq? '- expr) 'sub]
                            [(eq? '* expr) 'mul]
                            [(eq? 'mse expr) 'mse]
                            [(eq? 'rlu expr) 'rlu]
                            [(number? expr) (num expr 0.0)]
                            [else (hash-ref hash expr)]
                            ))

(define (ismember name list) (if (list? (member name list)) #t #f))

(define (grad names values var expr) (num-grad (eval(parse (create-hash names values var)expr)))  )

(define (forpart isim names values var expr)(if (ismember isim var) (grad names values isim expr) '0.0))

(define (partial-grad names values var expr) (map forpart names (make-list (length names) names) (make-list (length values)values) (make-list (length names)var) (make-list (length names)expr))  ) 

(define (gradient-descent names values vars lr expr) (map - values (map * (make-list (length names) lr) (partial-grad names values vars expr)))   )

(define (optimize names values vars lr k expr) (cond
                                                 [(= k 1) (gradient-descent names values vars lr expr)]
                                                 [(gradient-descent names (optimize names values vars lr (- k 1) expr) vars lr expr)]))

  



                                         

  
                                            
                              

                              



