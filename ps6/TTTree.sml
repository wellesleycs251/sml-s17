datatype TTTree = (* 2-3 tree of ints *)
	 L (* Leaf *)
	 | W of TTTree * int * TTTree (* tWo node *)
	 | H of TTTree * int * TTTree * int * TTTree (* tHree node *)

(* Valid trees *)

val t1 = W( W(W(L,1,L), 2, W(L,3,L)), 4, W(W(L,5,L), 6, W(L,7,L)))
val t2 = H( W(L,1,L), 2, H(L,3,L,4,L), 5, H(L,6,L,7,L))
val t3 = H( H(L,1,L,2,L), 3, W(L,4,L), 5, H(L,6,L,7,L))
val t4 = H( H(L,1,L,2,L), 3, H(L,4,L,5,L), 6, W(L,7,L))
						  
val vt0 = L

val vt2 = H (L,1,L,2,L)

val vt17 =
    H (W (W (L,1,L),2,H (L,3,L,4,L)),5,W (H (L,6,L,7,L),8,H (L,9,L,10,L)),11,
       H (H (L,12,L,13,L),14,W (L,15,L),16,W (L,17,L)))

val vt20 = 
  W
    (W (W (W (L,1,L),2,H (L,3,L,4,L)),5,W (H (L,6,L,7,L),8,H (L,9,L,10,L))),
     11,
     W
       (W (H (L,12,L,13,L),14,H (L,15,L,16,L)),17,
        W (W (L,18,L),19,W (L,20,L))))

val vt44 = 
  W
    (W
       (W (W (W (L,1,L),2,H (L,3,L,4,L)),5,W (H (L,6,L,7,L),8,H (L,9,L,10,L))),
        11,
        W
          (W (H (L,12,L,13,L),14,H (L,15,L,16,L)),17,
           W (H (L,18,L,19,L),20,H (L,21,L,22,L)))),23,
     W
       (W
          (W (H (L,24,L,25,L),26,H (L,27,L,28,L)),29,
           W (H (L,30,L,31,L),32,H (L,33,L,34,L))),35,
        W
          (W (H (L,36,L,37,L),38,H (L,39,L,40,L)),41,
           W (W (L,42,L),43,W (L,44,L)))))

val vt92 =     
  W
    (W
       (W
          (W
             (W (W (L,1,L),2,H (L,3,L,4,L)),5,
              W (H (L,6,L,7,L),8,H (L,9,L,10,L))),11,
           W
             (W (H (L,12,L,13,L),14,H (L,15,L,16,L)),17,
              W (H (L,18,L,19,L),20,H (L,21,L,22,L)))),23,
        W
          (W
             (W (H (L,24,L,25,L),26,H (L,27,L,28,L)),29,
              W (H (L,30,L,31,L),32,H (L,33,L,34,L))),35,
           W
             (W (H (L,36,L,37,L),38,H (L,39,L,40,L)),41,
              W (H (L,42,L,43,L),44,H (L,45,L,46,L))))),47,
     W
       (W
          (W
             (W (H (L,48,L,49,L),50,H (L,51,L,52,L)),53,
              W (H (L,54,L,55,L),56,H (L,57,L,58,L))),59,
           W
             (W (H (L,60,L,61,L),62,H (L,63,L,64,L)),65,
              W (H (L,66,L,67,L),68,H (L,69,L,70,L)))),71,
        W
          (W
             (W (H (L,72,L,73,L),74,H (L,75,L,76,L)),77,
              W (H (L,78,L,79,L),80,H (L,81,L,82,L))),83,
           W
             (W (H (L,84,L,85,L),86,H (L,87,L,88,L)),89,
              W (W (L,90,L),91,W (L,92,L))))))

(* Valid height but invalid sortedness *)
val io2 = H (L,2,L,1,L)

val io7 = H (W (L,1,L),2,H (L,3,L,5,L),4,H (L,6,L,7,L)) : TTTree

val io17 =
    H (W (W (L,1,L),2,H (L,3,L,4,L)),5,W (H (L,6,L,7,L),8,H (L,9,L,10,L)),11,
       H (H (L,12,L,13,L),14,W (L,15,L),17,W (L,16,L)))

val io20 = 
  W
    (W (W (W (L,1,L),2,H (L,3,L,4,L)),5,W (H (L,6,L,7,L),8,H (L,9,L,11,L))),
     10,
     W
       (W (H (L,12,L,13,L),14,H (L,15,L,16,L)),17,
        W (W (L,18,L),19,W (L,20,L))))

val io44 = 
  W
    (W
       (W (W (W (L,1,L),2,H (L,3,L,4,L)),5,W (H (L,6,L,7,L),8,H (L,9,L,10,L))),
        11,
        W
          (W (H (L,12,L,13,L),15,H (L,14,L,16,L)),17,
           W (H (L,18,L,19,L),20,H (L,21,L,22,L)))),23,
     W
       (W
          (W (H (L,24,L,25,L),26,H (L,27,L,28,L)),29,
           W (H (L,30,L,31,L),32,H (L,33,L,34,L))),35,
        W
          (W (H (L,36,L,37,L),38,H (L,39,L,40,L)),41,
           W (W (L,42,L),43,W (L,44,L)))))

val io92 =     
  W
    (W
       (W
          (W
             (W (W (L,1,L),2,H (L,3,L,4,L)),5,
              W (H (L,6,L,7,L),8,H (L,9,L,10,L))),11,
           W
             (W (H (L,12,L,13,L),14,H (L,15,L,16,L)),17,
              W (H (L,18,L,19,L),20,H (L,21,L,22,L)))),23,
        W
          (W
             (W (H (L,24,L,25,L),26,H (L,27,L,28,L)),29,
              W (H (L,30,L,31,L),32,H (L,33,L,34,L))),35,
           W
             (W (H (L,36,L,37,L),38,H (L,39,L,40,L)),41,
              W (H (L,43,L,42,L),44,H (L,45,L,46,L))))),47,
     W
       (W
          (W
             (W (H (L,48,L,49,L),50,H (L,51,L,52,L)),53,
              W (H (L,54,L,55,L),56,H (L,57,L,58,L))),59,
           W
             (W (H (L,60,L,61,L),62,H (L,63,L,64,L)),65,
              W (H (L,66,L,67,L),68,H (L,69,L,70,L)))),71,
        W
          (W
             (W (H (L,72,L,73,L),74,H (L,75,L,76,L)),77,
              W (H (L,78,L,79,L),80,H (L,81,L,82,L))),83,
           W
             (W (H (L,84,L,85,L),86,H (L,87,L,88,L)),89,
              W (W (L,90,L),91,W (L,92,L))))))

(* Valid sortedness but invalid height *)

val ih2 = W (L,1,W (L,2,L))

val ih9 = W (W (W (L,1,L),2, W(L,3,L)), 
	     4,
	     W (W (L,5,L), 6, W ( W(L,7,L),8, W(L,9,L))))
(* 7,8,9 at wrong height *)
								       
val ih17 =
    H (W (W (L,1,L),2,H (L,3,L,4,L)),5,W (H (L,6,L,7,L),8,H (L,9,L,10,L)),11,
       W (H (L,12,L,13,L),14, H (L,15,L,16,W (L,17,L))))
(* 15, 16, 17 at wrong height *)

val ih20 = 
  H (W (W (W (L,1,L),2,H (L,3,L,4,L)),5,W (H (L,6,L,7,L),8,H (L,9,L,10,L))),
     11,
     W (H (L,12,L,13,L),14,H (L,15,L,16,L)),
     17,
     W (W (L,18,L),19,W (L,20,L)))
(* changed W(..., 11, W(..., 17, ...)) to H(..., 11, ..., 17, ...) *)

val ih44 = 
 H (W (W (W (W (L,1,L),2,H (L,3,L,4,L)),5,W (H (L,6,L,7,L),8,H (L,9,L,10,L))),
        11,
        W
          (W (H (L,12,L,13,L),14,H (L,15,L,16,L)),17,
           W (H (L,18,L,19,L),20,H (L,21,L,22,L)))),
    23,
    W (W (H (L,24,L,25,L),26,H (L,27,L,28,L)),29,
       W (H (L,30,L,31,L),32,H (L,33,L,34,L))),
    35,
    W (W (H (L,36,L,37,L),38,H (L,39,L,40,L)),41,
          W (W (L,42,L),43,W (L,44,L))))
(* changed W(..., 23, W(..., 35, ...)) to H(..., 23, ..., 35, ...) *)    

val ih92 =     
  W
    (W
       (W
          (W
             (W (W (L,1,L),2,H (L,3,L,4,L)),5,
              W (H (L,6,L,7,L),8,H (L,9,L,10,L))),11,
           W
             (W (H (L,12,L,13,L),14,H (L,15,L,16,L)),17,
              W (H (L,18,L,19,L),20,H (L,21,L,22,L)))),23,
        W
          (W
             (W (H (L,24,L,25,L),26,H (L,27,L,28,L)),29,
              W (H (L,30,L,31,L),32,H (L,33,L,34,L))),35,
           W
             (W (H (L,36,L,37,L),38,H (L,39,L,40,L)),41,
              W (H (L,42,L,43,L),44,H (L,45,L,46,L))))),47,
     W
       (W
            (W
		 (W (W(L,48,L),49, W(L,50,L)), 
   	      51,
              W (W (W(L,51,L),52, W(L,53,L)),
                 55, 
                 W (W(L,56,L),57, W(L,58,L)))), 
	   59,
           W
             (W (H (L,60,L,61,L),62,H (L,63,L,64,L)),65,
              W (H (L,66,L,67,L),68,H (L,69,L,70,L)))),71,
        W
          (W
             (W (H (L,72,L,73,L),74,H (L,75,L,76,L)),77,
              W (H (L,78,L,79,L),80,H (L,81,L,82,L))),83,
           W
             (W (H (L,84,L,85,L),86,H (L,87,L,88,L)),89,
              W (W (L,90,L),91,W (L,92,L))))))
    
(* replaced 
   W (W (H (L,48,L,49,L),50,H (L,51,L,52,L)),53,
      W (H (L,54,L,55,L),56,H (L,57,L,58,L)))
   by 
   W (W (W(L,48,L),49 W(L,50,L))
      51,
      W (W (W(L,51,L),52 W(L,53,L)),
         55, 
         W (W(L,56,L),57 W(L,58,L))))
*)

val validTrees = [vt0, vt2, t2, t3, t4, t1, vt17, vt20, vt44, vt92]

val invalidTrees = [io2, io7, io17, io20, io44, io92,
		    ih2, ih9, ih17, ih20, ih44, ih92]
