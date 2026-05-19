; check that variables are scoped correctly

(decl x)
(set x 2)

(def x (x) (
    (decl x)
    (decl y)
    (set x 0)
    (return x) ; return 0
))

(decl y)
(set y (x x)) ; y = 0
(set y (x + y)) ; y = 2
(x x)

(while (x > 0) (
    (set x (x - 1))
    (decl x)
    (set x 1)
))
(set y (y + x)) ; y = 2
(set x 2)

(if ((x x) == 0) (
    (decl x)
    (set x 3)
    (if (1 < 2) (
        (decl a)
        (set a x) ; a = 3
        (decl x)
        (set x (a + 1))
        (set y (y + x)) ; y = 6
    ))
))
(set y (y + x)) ; y = 8
