; update count each time f runs
; if short circuiting works correctly this should have the expected final value
(decl count)
(set count 0)

(def f (n) (
    (set count (count + 1))
    (return n)
))

; +1
(if (((f 5) > 2) || ((f 5) > 3)) ()
 elif ((f 1) < 7) ())

; +3
(if (((f 5) < 2) || ((f 5) < 3)) ()
 elif ((f 1) > 7) ())

; +1
(if (((f 5) < 2) && ((f 5) < 3)) ())

; +2
(if (((f 5) > 2) && ((f 5) < 3)) ())

; +2
(if (5 < 2) ()
 elif (7 < 2) ()
 elif ((f 8) < 2) ()
 elif ((f 9) > 2) ()
 elif ((f 10) > 2) ()
 elif (10 > 2) ())

; +4
(decl x)
(set x (??
 ((f 2) < 1) (f 1)
 ((f 2) < 1) (f 2)
 ((f 2) > 1) (f 3)
 ((f 2) < 1) (f 4)
 ((f 2) < 1) (f 5)
 default     (f 6)
))

; +3
(if (((f 2) > 1) && ( ((f 0) > 1) || ((f 2) > 1) )) ())

; +1
(if (((f 2) < 1) && ( ((f 0) > 1) || ((f 2) > 1) )) ())

; count = 19
