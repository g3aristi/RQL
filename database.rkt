#| Assignment 1 - Racket Query Language (due February 11, noon)

***Write the names and CDF accounts for each of your group members below.***
Gilberto Aristizabal, g3aristi
<Name>, <CDF>
|#
#lang racket

; Function versions for common syntactic forms.
; *Use these in your queries instead of the syntactic forms!!!*
(define (And x y) (and x y))
(define (Or x y) (or x y))
(define (If x y z) (if x y z))

; TODO: After you have defined your macro(s), make sure you add each one
; to the provide statement.
(provide attributes
         tuples
         size)

; Part 0: Semantic aliases

#|
(attributes table)
  table: a valid table (i.e., a list of lists as specified by assigment)

  Returns a list of the attributes in 'table', in the order they appear.
|#
(define (attributes table)
  (first table)
  )

#|
(tuples table)
  table: a valid table

  Returns a list of all tuples in 'table', in the order they appear.
  Note: it is possible for 'table' to contain no tuples.
|#
(define (tuples table)
  (rest table)
  )

#|
(size table)
  table: a valid table

  Returns the number of tuples in 'table'.
|#
(define (size table)
  (length (rest table))
  )

; Part I "WHERE" helpers; you may or may not wish to implement these.

#|
A function that takes: 
  - a list of attributes
  - a string (representing an attribute)
  - a tuple

  and returns the value of the tuple corresponding to that attribute.
|#
(define (getValue al a t)
  (if (equal? (first al) a)
        (first t)
        (getValue (rest al) a (rest t))
   )
  )


#|
A function that takes:
  - f: a unary function that takes a tuple and returns a boolean value
  - table: a valid table

  and returns a new table containing only the tuples in 'table'
  that satisfy 'f'.
|#
(define (satisfyCond f table)
  (satHelper f (rest table) (list(first table)) )
  )

(define (satHelper c t nt)
  (if (empty? t)
      nt
      (if (c (first t))
          (satHelper c (rest t)(append nt (list (first t))))
          (satHelper c (rest t) nt)
      )
  )
)

#|
A function 'replaceAttr' that takes:
  - x 
  - a list of attributes

  and returns a function 'f' which takes a tuple and does the following:
    - If 'x' is in the list of attributes, return the corrresponding value 
      in the tuple.
    - Otherwise, just ignore the tuple and return 'x'.
|#
(define (replaceAttr x al)
    (lambda (tuple)
        (if (contains x al #f)
            (let ([xLoc (attIndex x al 0)]) xLoc
                (list-ref tuple xLoc)
            )
            x
         )
     )
)

(define (contains target array init)
  (if (empty? array)
      init
      (if (equal? target (first array))
          #t
          (contains target (rest array) #f)
       )
   )
)

(define (attIndex target la ind)
    (if (equal? target (first la))
        ind
        (attIndex target (rest la) (+ 1 ind))
    )    
)

; Starter for Part 4; feel free to ignore!

; What should this macro do?
(define-syntax replace
  (syntax-rules ()
    ; The recursive step, when given a compound expression
    [(replace (expr ...) table)
     ; Change this!
     (void)]
    ; The base case, when given just an atom. This is easier!
    [(replace atom table)
     ; Change this!
     (void)]))

(define (sel-helper )
  )

;multiple attributes not just one
(define (select-from att table)
  (sel-helper att (tuples table) '())
  )

(define-syntax SQL
  (syntax-rules (SELECT FROM)
  ))

;---------------------------Our Own Testing-------------------------------------------------


;---------------------Prepared tables---------------------------
(define table1
  '(("Name" "Age" "City")
  ("David" 20 "Prague") 
  ("Jen" 30 "Toronto") 
  ("Paul" 80 "MarsCity")
  ("Carlo" 30 "Milan"))
  )

(define Person
  '(("Name" "Age" "LikesChocolate")
    ("David" 20 #t)
    ("Jen" 30 #t) 
    ("Paul" 100 #f)))

(define Teaching
    '(("Name" "Course")
    ("David" "CSC324")
    ("Jen" "CSC108")
    ("David" "CSC343")
    ))

;------------------------ Testing Attributes----------------------
(write "Testing attributes -----------")
(write "Table1")
(attributes table1) ;expect: '("Name" "Age" "City")
(write "Person table")
(attributes Person) ;expect: '("Name" "Age" "LikesChocolate")
(write "Teaching table")
(attributes Teaching) ;expect: '("Name" "Course")

;------------------------ Testing Tuples ------------------------
(write "Testing tuples -----------")
(write "Table1 ")
(tuples table1) ;expect: '(("David" 20 "Prague") ("Jen" 30 "Toronto") ("Paul" 80 "MarsCity") ("Carlo" 30 "Milan"))
(write "Person ")
(tuples Person) ;expect: '(("David" 20 #t) ("Jen" 30 #t) ("Paul" 100 #f))
(write "Teaching ")
(tuples Teaching) ;expect: '(("David" "CSC324") ("Jen" "CSC108") ("David" "CSC343"))

;------------------------ Testing Size ------------------------
(write "Testing size -----------")
(write "Table1 ")
(size table1) ;expect: 4
(write "Person ")
(size Person) ;expect: 3
(write "Teaching ")
(size Teaching) ;expect: 3

;------------------------ Testing getValue ------------------------
(write "Testing getValue -----------")
(write "Table1 ")
(getValue (attributes table1) "City" (fourth (tuples table1))) ;expect: "Milan"
(write "Person ")
(getValue (attributes Person) "Name" (first (tuples Person))) ;expect: "David"
(write "Teaching ")
(getValue (attributes Teaching) "Course" (first (tuples Teaching))) ;expect: "CSC324"

;------------------------ Testing satisfyCond ------------------------
(write "Testing satistfyCond -----------")
(write "Table1 ")
(define (f1 tuple)
    (if (equal? (second tuple) 30)
        #t
        #f
    )
)
(satisfyCond f1 table1) ;expect: '(("Name" "Age" "City") ("Jen" 30 "Toronto") ("Carlo" 30 "Milan"))
(write "Person ")
(define (f2 tuple)
    (if (equal? (third tuple) #f)
        #t
        #f
    )
)
(satisfyCond f2 Person) ;expect: '(("Name" "Age" "LikesChocolate") ("Paul" 100 #f))
(write "Teaching ")
(define (f3 tuple)
    (if (equal? (first tuple) "David")
        #t
        #f
    )
)
(satisfyCond f3 Teaching) ;expect: '(("Name" "Course") ("David" "CSC324") ("David" "CSC343"))


;------------------------ Testing replaceAttr ------------------------
(write "Testing replaceAttr -----------")
(write "Table1 ")
(define fun1 (replaceAttr "City" (attributes table1)))
(fun1 (fourth (tuples table1))) ;expect: "Milan"
(define fun2 (replaceAttr "None" (attributes table1)))
(fun2 (first (tuples table1))) ;expect: "None

(write "Person ")
(define fun3 (replaceAttr "Name" (attributes Person)))
(fun3 (second (tuples Person))) ;expect: "Jen"
(define fun4 (replaceAttr "Gender" (attributes Person)))
(fun4 (first (tuples Person))) ;expect: "Gender"


(write "Teaching ")
(define fun5 (replaceAttr "Course" (attributes Teaching)))
(fun5 (first (tuples Teaching))) ;expect: "CSC324"
(define fun6 (replaceAttr "Age" (attributes Teaching)))
(fun6 (first (tuples Teaching))) ;expect:"Age"


;------------------------------------------------------------------------------
(select-from '("Name" "LikesChocolate") Person)
