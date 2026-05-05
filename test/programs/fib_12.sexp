
(decl n)
(decl a)
(decl b)
(decl result)

(set n 12)
(set a 0)
(set b 1)

(while (n > 0) (
    (decl tmp)
    (set tmp a)
    (set a b)
    (set b (a + tmp))
    (set n (n - 1))
))

(set result a)
