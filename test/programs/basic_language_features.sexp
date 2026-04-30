(def f (x y) (
    (set z (x + y))
    (set z (z * 2))
    (return z) ;; 2(x+y)
))

(set x (f 1 4)) ; 10
(set y (f x x)) ; 40
(set z 0)

(if (y > (200 * x)) (
    (set z 1))

 elif (y > x) (
    (set z 2)) ; this branch is taken

 else (
    (set z 3)))

(def isPositive (w) (
    (return (w > 0))))

(set w 0)
(while (isPositive z) (
    (set w (w + 1))
    (set z (z - 1))
))
;; w = 2

(set a (??
    (w == 1) 5
    (w == 2) 6
    (w == 3) 7
    default  8))
;; a = 6

