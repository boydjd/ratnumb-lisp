;;;;
;;;; A program that can add, subtract, multiple and divide rational numbers.
;;;; Additionally, an exponent can be applied to a rational number.
;;;;
;;;; Joshua Boyd <boyd_joshua@columbusstate.edu>
;;;; CPSC5135G
;;;;

(defun rnadd (x y)
  ;; takes two rational numbers as arguments and returns their sum
  (makern (+ (* (numer x) (denom y))
             (* (numer y) (denom x)))
          (* (denom x) (denom y))))

(defun rnsub (x y)
  ;; takes two rational numbers as arguments and returns the result of
  ;; subtracting the second argument from the first argument
  (makern (- (* (numer x) (denom y))
             (* (numer y) (denom x)))
          (* (denom x) (denom y))))

(defun rnprod (x y)
  ;; takes two rational numbers as arguments and returns their product
  (makern (* (numer x) (numer y))
          (* (denom x) (denom y))))

(defun rndiv (x y)
  ;; takes two rational numbers as arguments and returns the result of
  ;; dividing the first argument by the second argument
  (makern (* (numer x) (denom y))
          (* (denom x) (numer y))))

(defun rnexp (x y)
  ;; takes a rational number and an integer as arguments and returns
  ;; the rational number to the power of the second argument
  (cond ((> y -1) (/ (power (numer x) y) (power (denom x) y)))
        ((< y 0) (/ (power (denom x) (* y -1)) (power (numer x) (* y -1))))))

(defun power (x y)
  ;; takes two integers, multiplies x by x y times
  (cond ((= y 0) 1)
        (t (* x (power x (1- y))))))

(defun makern (n d)
  ;; takes a numerator and a denominator and returns a simplified fraction
  ;; as a list
  (list (/ n (gcd2 n d)) (/ d (gcd2 n d))))

(defun numer (x)
  ;; takes a rational number and returns the numerator
  (cond ((null x) 0)                                                                      ; empty list
        ((null (cdr x)) (car x))                                                          ; only number is present
        ((numberp (car (cdr x))) (car x))                                                 ; only fraction is present
        ((< (car x) 0) (- (- (car (car (cdr x))) (* (car x) (car (cdr (car (cdr x)))))))) ; number and fraction are present, number is negative
        (t (+ (car (car (cdr x))) (* (car x) (car (cdr (car (cdr x))))))))                 ; number and fraction are present

(defun denom (x)
  ;; takes a rational number and returns the denominator
  (cond ((null (car (cdr x))) 1)                                                          ; no fractional part, denominator is 1
        ((numberp (car (cdr x))) (car (cdr x)))                                           ; fractional part only, no number
        (t (car (cdr (car (cdr x)))))))                                                   ; number and fraction

(defun rnprint (x)
  ;; takes a single argument representing a rational number and prints it out
  ;; in human readable form
  (cond ((= (numer x) 0) 0)                                                               ; if numerator is zero, print 0.
       ((< (abs2 (numer x)) (denom x)) (format nil "~d/~d" (numer x) (denom x) ))         ; numerator is less than denominator, no number to print
       ((= (rem (numer x) (denom x)) 0) (format nil "~d" (truncate (numer x) (denom x)))) ; no remainder, print only the number
       (t (format nil "~d and ~d/~d" (truncate (numer x) (denom x)) (abs2 (rem (numer x) (denom x))) (denom x))))) ; print the full readable format

(defun gcd2 (a b)
  ;; takes two integers and returns the greatest common divisor
  (cond ((= b 0) a)
        (t (gcd2 b (abs2 (rem a b))))))

(defun abs2 (n)
  ;; takes an integer and returns the absolute value
  (cond ((< N 0) (- N))
        (t N)))
