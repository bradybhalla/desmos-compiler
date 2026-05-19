
(def isEven (result) (
    (return
      (??
        (result == 0) true
        default (not (isEven (result - 1)))))
))

(def isOdd (result) (
    (return
      (??
        (result == 1) true
        default (not (isEven (result - 1)))))
))


(decl n)
(decl result)

(set result 0)
(set n 0)
(while (n < 4) (
    (if (isEven (n * n)) (
        (set result (result + 1))
    ))
    (set n (n + 1))))

; result = 2
