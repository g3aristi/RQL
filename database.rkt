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
  (if (empty? al)
      (error "Values does not exits, or specefy the table ")
      (if (equal? (first al) a)
        (first t)
        (getValue (rest al) a (rest t))
   )
      )
  )

;same as above but with multiple attributes
;So it returns a Table with only attributes in a list of attributes and a simple tuple
(define (getValues2 al lst-att t acc)
  (if (empty? lst-att)
      acc
      (getValues2 al (rest lst-att) t (append acc (list (getValue al (first lst-att) t ) )))
      )
  )

;Return Tables with attributes in list of attributes given and with all the original tuples
(define (getValues4 tables lst-att acc)
  (if (empty? tables)
      acc
      (getValues4 (rest tables) lst-att (append acc 
                                        (list (getValues3 (attributes (first tables)) lst-att
                                                    (tuples (first tables)) '() ))))
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

(define (rename-att name lst-att1 lst-att2 acc)
  (if (empty? lst-att1)
      acc
      (if (contains (first lst-att1) lst-att2 '())
          (rename-att name (rest lst-att1) lst-att2 
                      (append acc (list (string-append (string-append name ".")(first lst-att1)))))
          (rename-att name (rest lst-att1) lst-att2 (append acc (list (first lst-att1))))
      )
   ))

;return a list of all attributes in all the listed tables
(define (getAllAtts lst-tables acc)
  (if (empty? lst-tables)
      acc
      (getAllAtts (rest lst-tables)(append acc (attributes (first (first lst-tables)))))
      )
  )

;counts duplicates
(define (count-dup target lst)
  (count (λ (curr-a) (equal? curr-a target)) lst)
  )

(define (rename? att lst-atts)
  (if (> (count-dup att lst-atts) 1)
      #t
      #f
  ))

(define (rename2 pre att-name)
  (string-append (string-append pre ".") att-name)
  )

(define (rename-atts-table prefix table-atts lst-atts acc)
  (if (empty? table-atts)
      acc
      (if (rename? (first table-atts) lst-atts) 
          ;then rename
          (rename-atts-table prefix (rest table-atts) lst-atts 
                             (append acc (list (rename2 prefix (first table-atts)))))
          ;just add it to acc
          (rename-atts-table prefix (rest table-atts) lst-atts
                              (append acc (list (first table-atts))))
          )
      )
  )

(define (renameAll lst-tables all-atts acc)
  (if (empty? lst-tables)
      acc
      (renameAll (rest lst-tables) all-atts (append acc 
                                              (rename-atts-table 
                                              (second (first lst-tables)) 
                                              (attributes (first (first lst-tables)))
                                               all-atts
                                               '())
                                               ))
      )
  
  )

(define (cross-atts lst-tables)
  (list (renameAll lst-tables (getAllAtts lst-tables '()) '() ))
  )

(define (rename-atts t1 name1 t2 name2 acc)
  (list (append acc (rename-att name2 (attributes t2) (attributes t1)
          (append acc (rename-att name1 (attributes t1) (attributes t2) '()))))
  ))

(define (multiply-elems Table1-elem Table2) ;helper function to multiply individual elements from table1 with all the elements in table2
  (map (λ (Table2-elem) (append Table1-elem Table2-elem)) Table2)) ;for each element in table2 append that element to table1

(define (cartesian-product table1 table2)
  (append* (map(λ (table1-elem) (multiply-elems table1-elem table2))table1)));Run the helper on each element of table1 with table2

(define (n-cartesian-products lst-tables)
  (n-cart-products-helper (rest(rest lst-tables)) (cartesian-product (rest(first(first lstTables))) (rest(first(first(rest lstTables)))))))

; acc = (cartesian-product (rest(first(first lstTables))) (rest(first(first(rest lstTables)))))
; lst-tables = (rest(rest lst-tables))
(define (n-cart-products-helper lst-tables acc)
  (if (empty? lst-tables)
      acc
      (n-cart-products-helper (rest lst-tables) (cartesian-product acc (rest (first (first lst-tables)))))))
      

(define (crossed-table table1 n1 table2 n2)
  (append 
   (rename-atts table1 n1 table2 n2 '())
   (cartesian-product (rest table1) (rest table2))
          ))

;table consists of table table-name ex: '((table1 "t1") (table2 "t2"))
(define (crossed-tables lst-tables acc)
  (if (empty? lst-tables)
      acc
      (crossed-tables (rest (rest lst-tables))
                      (append acc (crossed-table (first (first lst-tables)) (second (first lst-tables)) 
                     (first (second lst-tables)) (second (second lst-tables))))
      )
  ))

(define (from lst-tables)
    (append (cross-atts lst-tables) (append '() (n-cartesian-products lst-tables)))
  )

;Returns table with multiple tuples and with attributes in the list of attributes given
(define (getValues3 al lst-att tuples acc)
  (if (empty? tuples)
      acc
      (getValues3 al lst-att (rest tuples) (append acc (list(getValues2 al lst-att (first tuples) '()))))
      )
  )

;multiple attributes not just one
(define (select-from lst-att table)
  (getValues3 (attributes table) lst-att (tuples table) (list lst-att))
  )

(define (select-from-where lst-att table pred)
  (select-from lst-att (satisfyCond pred table))
  )

;(SELECT '("t1.Name" "t3.Name") FROM (from lstTables))
(define-syntax SELECT
  (syntax-rules (SELECT * FROM WHERE ORDER-BY)
    [(SELECT * FROM table) table]
    [(SELECT <attrs> FROM <table>)
     (select-from <attrs> <table>)]
    [(SELECT <attrs> FROM <lst-table> WHERE <pred>)
     (select-from-where <attrs> <lst-table> <pred>)]
    ;[(SELECT <attrs> FROM [<table1> <name1>] [<table2> <name2>] ...))
     ;select-from <attrs> ]
  ))

(define(find-att-loc att table-atts i)
  (if(empty? table-atts)
     i
     (if (equal? (first table-atts) att)
         i
         (find-att-loc att (rest table-atts) (+ i 1))
         )
     )
  )

(define (order-by att table by)
  (let ([i (find-att-loc att (attributes table) 0)])
    (sort (tuples table)
          #:key (λ (x)(list-ref x i)) by)
  
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

(define lstTables 
  (list (list table1 "t1")
        (list Person "t2")
        (list Teaching "t3"))
  )
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
;(select-from '("Name" "City") table1)

(define (f8)
    (if (> "Age" 25)
        #t
        #f
    )
)
