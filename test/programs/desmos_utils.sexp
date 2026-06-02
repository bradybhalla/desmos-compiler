; # means it depends on user input
(#decl X 0)
(#decl Y 1)

; @ means we are drawing something
(@point X Y)
(@point ax ay)
(@point bx by)

(decl startx)
(decl starty)
(decl ax)
(decl ay)
(decl bx)
(decl by)
(set ax 0)
(set ay 0)
(set bx 5)
(set by 4)
(set startx X)
(set starty Y)

(while ((X == startx) && (Y == starty)) ())

(while true (
    (decl dx)
    (decl dy)

    (set dx (X - ax))
    (set dy (Y - ay))
    (set ax (ax + (dx * 0.1)))
    (set ay (ay + (dy * 0.1)))

    (set dx (X - bx))
    (set dy (Y - by))
    (set bx (bx + (dx * 0.1)))
    (set by (by + (dy * 0.1)))
))
