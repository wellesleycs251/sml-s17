datatype figure =
    Circ of real (* radius *)
  | Rect of real * real (* width, height *)
  | Tri of real * real * real (* side1, side2, side3 *)

val figs = [Circ 1.0, Rect (2.0,3.0),  Tri(4.0,5.0,6.0)] (* List of sample figures *)

val circs = map Circ [7.0, 8.0, 9.0] (* List of circles *)

fun perim (Circ r) = 2.0 * Math.pi * r
  | perim (Rect(w,h)) = 2.0 * (w + h)
  | perim (Tri(s1,s2,s3)) = s1 + s2 + s2

val perims = map perim figs

fun scale n (Circ r) = Circ (n * r) (* Scale figure by factor n *)
  | scale n (Rect(w,h)) = Rect (n*w, n*h)
  | scale n (Tri(s1,s2,s3)) = Tri (n*s1, n*s2, n*s3)

val scaledFigs = map (scale 3.0) figs

