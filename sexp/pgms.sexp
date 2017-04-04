; Contents of pgms.sexp

(postfix 2 1 nget mul) ; simple PostFix program

{ ; Curly braces are nestable block comments
  (postfix 1) ; silly program
  { (intex 0 17) ; Another silly program }
}

(intex 2 (/ (+ ($ 1) ($ 2)) 2)) ; Intex averaging program
