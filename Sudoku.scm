#lang scheme

;Name: Lootii Kiri



;The Sudoku must be represented with a list of 4 lists, each list element representing one row of
;the matrix.

(define sudoku1 '((2 1 4 3) (4 3 2 1) (1 2 3 4) (3 4 1 2)))
(define sudoku2 '((2 1 4 3) (4 3 2 1) (1 2 3 3) (3 4 1 2)))

;function different: returns true if all numbers in a list are different.

(define (different L)
   (cond
     ;base case if we reached the end of the list and it is null, there were no duplicates, return true
     ((null? L) #t)
     ;return false if first element of the list is a member of the rest of the list
     ((member (car L) (cdr L)) #f)
     ;otherwise recursively run the different function on the rest of the list
     (else
      (different (cdr L)))))

;function extract4Columns: extracts the 4 columns of the 4x4 Sudoku.


(define (extract4Columns Sudoku)
  ;used map procedure to get the necessary elements, and store it in a list. columns 1,2,3 and 4.
  ;first element in each row is column 1 , second element in eachrow is column 2, etc.
  (let ((firstElements (map car Sudoku))
        (secondElements (map cadr Sudoku))
        (thirdElements (map caddr Sudoku))
        (fourthElements (map cadddr Sudoku)))
    ;combine each column list into a final list with all 4 columns
    (list firstElements secondElements thirdElements fourthElements)))

;function extract4Quadrants: extracts the 4 quadrants of the 4x4 Sudoku.


(define (extract4Quadrants lst)
  ;used map procedure to get the necessary elements, and store it in a list. columns 1,2,3 and 4. 
  (let* ((c1 (map car lst))
         (c2 (map cadr lst))
         (c3 (map caddr lst))
         (c4 (map cadddr lst))
         ;use car/cdr combinations to store the elements of each quadrant based on columns above
         (firstquad (list (car c1) (car c2) (cadr c1) (cadr c2)))
         (secondquad (list (car c3) (car c4) (cadr c3) (cadr c4)))
         (thirdquad (list (caddr c1) (caddr c2) (cadddr c1) (cadddr c2)))
         (fourthquad (list (caddr c3) (caddr c4) (cadddr c3) (cadddr c4))))
    ;finally create a list of all 4 lists (one for each quandrant)
    (list firstquad secondquad thirdquad fourthquad)))


;function merge3: merges three lists.

(define (merge3 l1 l2 l3)
  ;checks if either lists are null and returns empty list if so.
  (if (or (null? l1) (null? l2) (null? l3))
      '()
      ;otherwise use append procedure to merge three lists
      (append l1 l2 l3)))


;function checkSudoku: verifies if a sudoku is valid by proceeding as
;follows:
;
;merges together the list of rows of the sudoku with the list of its columns (from extract4Columns) and the lists of its quadrants (from extract4Quadrants)
;use map in order to call the function different on each element of the merged list
;use the result of map in order to determine if the sudoku is valid and return the result (true or false).



(define (checkSudoku sudoku)
  ;let* variables include rows of the sudoku,columns of the sudoku, quadrants of the sudoku
  ;the three merged together using merge3 and stored in variable merged
  ;applied the different function to each element of the merged list and stored it in variable differentResults
  (let* ((rows sudoku)
         (columns (extract4Columns sudoku))
         (quadrants (extract4Quadrants sudoku))
         (merged (merge3 rows columns quadrants))
         (differentResults (map different merged)))
    ; Check if any element of different-results is false
    ; If true, return false (sudoku is invalid)
    ; Otherwise, return true (sudoku is valid)
    (not (member #f differentResults))))



(different '(1 3 6 4 8 0))
(different '(1 3 6 4 1 8 0))
(extract4Columns sudoku1)
(extract4Quadrants sudoku1)
(merge3 '(1 3 6) '(5 4) '(1 2 3))
(checkSudoku sudoku1)
(checkSudoku sudoku2)

