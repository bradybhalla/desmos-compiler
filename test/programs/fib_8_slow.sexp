(def isOne (n) (
    (return (n == 1))))

(def isPositive (n) (
    (return (n > 0))))

(def fib (n) (
    (if (isOne n) (
        (return 1))
     elif (isPositive n) (
        (return ((fib (n - 1)) + (fib (n - 2)))))
     else (
        (return 0)))))

(decl n)
(set n (fib 8)) ; 21
