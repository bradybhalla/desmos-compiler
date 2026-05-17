
(def nextInSequence (n) (
    (return (??
        ((n % 2) == 0) (n / 2)
        default ((3 * n) + 1)))
))

(def max (a b) (
    (return (??
        (a < b) b
        default a))
))

(decl highest)
(decl cur)
(set cur 27)
(set highest cur)
(while (cur > 1) (
    (set cur (nextInSequence cur))
    (set highest (max highest cur))
))

; expected highest = 9232
