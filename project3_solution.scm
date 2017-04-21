
;;; aakgul13@ku.edu.tr    Wed Nov 18 16:36:53 2015
;;;    	  	   	 
;;;	Comp200 Project 3 - Graphs
;;;    	  	   	 
;;; Before you start:    	  	   	 
;;; * Please read "Project Submission Instructions" carefully.
;;;    	  	   	 
;;; * Please read the "Project 3" handout available on the course
;;;   web site first to get a basic idea about the project and the
;;;   logic behind it, then to find out the details about what
;;;   your tasks are for the rest of the project.
;;;    	  	   	 
;;; * Plan your work with pencil and paper before starting to code.
;;;    	  	   	 
;;;    	  	   	 
;;; While you are working:    	  	   	 
;;; * Type all your work and notes in the appropriate sections of this file.
;;; * Please do not delete any of the existing lines.
;;; * Use the procedure names given in the instructions.
;;; * Remember to frequently save your file.
;;; * Use semicolon (;) to comment out text, tests and unused code.
;;; * Remember to document your code
;;; * Remember our collaboration policy: you can discuss with your friends but:
;;;    	  	   	 
;;;   *** NOTHING WRITTEN GETS EXCHANGED ***
;;;    	  	   	 
;;;    	  	   	 
;;;    	  	   	 
;;;    	  	   	 
;;;    	  	   	 
;;; Loading helper functions:    	  	   	 
(load "search.scm")    	  	   	 
    	  	   	 
;;; The following is defined so that the file loads without errors:
(define your-answer-here #f)    	  	   	 
    	  	   	 
;;;;;;;::;;;;:::;;;;:::;:;::::;;::::::;:;:::;::;;;::;;;:;::;;::;::;;;:;::;:::
;;; Exercise 0.1:    	  	   	 
;;;    	  	   	 
;;; Read and try to understand the codes within the search.scm
;;; There are some commented test case examples of pre-defined functions
;;; Uncomment and run the code see their results
;;; Copy and paste the results here (as comment lines)
;;;
;    Test cases and their results:
;
;   (find-graph-element test-graph 'b) ; Value: (graph-element b (c d e h) (more words))
;   (find-graph-element test-graph 'z) ; Value: ()
;   (find-node-children test-graph 'b) ; Value: (c d e h)
;   (find-node-children test-graph 'z) ; Value: ()
;   (find-node-contents test-graph 'b) ; Value: (more words)
;   (find-node-contents test-graph 'z) ; Value: ()  	  	   	 
;
;
;;;;;;;::;;;;:::;;;;:::;:;::::;;::::::;:;:::;::;;;::;;;:;::;;::;::;;;:;:::;;;
;;; Exercise 0.2:    	  	   	 
;;;    	  	   	 
;;; You have a DFS test case in the project instructions document.
;;; Run the test case of it and write the result down in comments.
;;; You should run it for an existing and non-existing node
;
; Example from the project instructions document:
;
;   (DFS-simple 'a
;        (lambda (node) (eq? node 'l))
;             test-graph)
; Values: (now-at a)(now-at b)(now-at c)(now-at d)(now-at e)(now-at f)(now-at g)(now-at h)(now-at i)(now-at j)(now-at k)(now-at l)#t    	  	   	 
;
; Test case for existing node:
;   (DFS-simple 'a
;        (lambda (node) (eq? node 'm))
;             test-graph)
; Values: (now-at a)(now-at b)(now-at c)(now-at d)(now-at e)(now-at f)(now-at g)(now-at h)(now-at i)(now-at j)(now-at k)(now-at l)(now-at m)#t

;
; Test case for non-existing node:
;
;   (DFS-simple 'a
;        (lambda (node) (eq? node 'y))
;             test-graph)
; Values: (now-at a)(now-at b)(now-at c)(now-at d)(now-at e)(now-at f)(now-at g)(now-at h)(now-at i)(now-at j)(now-at k)(now-at l)(now-at m)#f
;
;;;;;;;::;;;;:::;;;;:::;:;::::;;::::::;:;:::;::;;;::;;;:;::;;::;::;;;:;:::;;:
;;; Exercise 1: The Web as a General Graph
;;;    	  	   	 
;;;    	  	   	 
;;; Explain why our depth first strategy (using DFS-simple) will
;;; fail on the general graph.    	  	   	 
;;;    	  	   	 
    	  	   	 
;  ANSWER:  	  	   	 
;  If there is a loop like figure 1 in the project instructions documents, the strategy will be fail;
; beause the strategy looks the node and its one of the children, adn its one of the children. That means
; strategy follows the edges and goes deep and deep; but some nodes have direct and inverse edges and it
; causes some cycles. In the figure 1, there is a cycle between X, Y and W, and another cycle is between W and U,
; and V's edges goes back to itself. When we start with the V to search, it always looks V again, again, and again
; and it causes infinite loop. If it starts with X to search, it can follow the way to X to Y, Y to W, W to X and again
; there is an infinite loop. Figure 1 is an example as figure 2 but general graph may similar both figure 1 and/or
; figure 2. Because of this reason, the strategy will fail on the general graph. 
;    	  	   	 
;    	  	   	 
    	  	   	 
    	  	   	 
;;;;;;;::;;;;:::;;;;:::;:;::::;;::::::;:;:::;::;;;::;;;:;::;;::;::;;:;;::;;;;
;;; Exercise 2:  Breadth-first search
;;;    	  	   	 
;;; Follow the instructions in the Project 3 handout, do not forget
;;; the documentation and test cases.
    	  	   	 
(define (BFS-simple start goal? graph)
  (search start
          goal?
          find-node-children
          (lambda (new old) (append old new))
          graph))   	  	   	     	  	   	 
    	  	   	 
;;;    	  	   	 
;;; Test Cases    	  	   	 
;;;
;      Searchin Node m:
;      (BFS-simple 'a (lambda (node) (eq? node 'm)) test-graph)
; Values: (now-at a)(now-at b)(now-at i)(now-at m)#t
;
;      Searching a non-existing node:
;       (BFS-simple 'a (lambda (node) (eq? node 's)) test-graph)
; Values: (now-at a)(now-at b)(now-at i)(now-at m)(now-at c)(now-at d)(now-at e)(now-at h)(now-at j)(now-at k)(now-at l)(now-at f)(now-at g)#f
;
;      Searching a thing that is not a node:
;       (BFS-simple 'a (lambda (node) (eq? node 3)) test-graph)
; Values: (now-at a)(now-at b)(now-at i)(now-at m)(now-at c)(now-at d)(now-at e)(now-at h)(now-at j)(now-at k)(now-at l)(now-at f)(now-at g)#f
;
; ANSWER:
; BFS-simple strategy is similar with DFS but there is one difference. DFS looks the node and
; its children and then others. For example: in figure 2, we started the strategy from 'a.
; The strategy takes its children, looks first one ('b). While it looking, it takes first one's children
; and add the front of the list of looking ('c 'd 'e 'h). That means It looks the node, then one of its children,
; then one of its children if there is (ex. 'e has 2 children which are 'f and 'd), then next node ('h).
; But BFS-simple strategy looks first node('a), then takes its children('b 'i 'm) and looks all its children.
; When it controls a node, takes its own children and add the end of the list of looking(ex. while it looks 'b,
; it will add ('c 'd 'e 'h) end of the list ('b 'i 'm). That means, first strategy will look, 'b, 'i, 'm, then 'c 'd 'e 'h and so on.
; Only different that we must do while coding, chancing the merge procedure of the search input.
; DFS merges the children front of the list and BFS merges the children end of the list.
;
; Use at least 3 different test case with different nodes (one should be not a node)
; Test cases should be the same structure with the DFS test case which is given in the PDF file of project
;;;    	  	   	 
;;;    	  	   	 
    	  	   	 
    	  	   	 
;;;;;;;::;;;;:::;;;;:::;:;::::;;::::::;:;:::;::;;;::;;;:;::;;::;::;;:;;::;;;:
;;; Exercise 3: The Index Abstraction
;;;    	  	   	 
;;;    	  	   	 
;;; Follow the instructions in the Project 3 handout, do not forget
;;; the documentation and test cases.
    	  	   	 
; Adds the value under the given key in the index
(define (add-to-index! index key value) ; Index,Key,Val -> Index
  (let ((index-entry (find-entry-in-index index key)))
    (if (null? index-entry)    	  	   	 
	;; no entry -- create and insert a new one...
        (set! index-entry (list key (list value)))
	;; entry exists -- insert value if not already there...
	(begin (set! index (remove index-entry index))
               (set-cdr! index-entry
                         (list (cons value
                               (remove value
                                       (if (null? (cddr index-entry))
                                           (cadr index-entry)
                                           (cdr index-entry))))))))
    (set-cdr! index (cons index-entry (cdr index)))
index))

; ANSWER:
; add-to-index! takes 3 input. One of them is index, other one is key and last one is the value that we
; want to add. The procedure takes the index-entry of the index, then looks if its null or not.
; If it is null, then it creates a index-entry that contains (key (value)). If it is not null and
; has key and value(s), first it deletes from the index because if we add before deleting the existing index-entry,
; there will be 2 entry which has same key. After deleting the index-entry, the procedure sets the index-entry.
; New index-entry will have new value, existing value with the key. Before adding new value, it removes the new value if
; it's aldready been there. If it has one value, just takes the value, if it has more than one value, takes all of them.
; After setting the index-entry, add the index-entry to index and return the new index.
;;;;;;;;;;;;;;    	  	   	 
;;; Test Cases    	  	   	 
;;;;;;    	  	   	 
;    	  	   	 
 (define test-index (make-index))
 (add-to-index! test-index 'key1 'value1)
; Value: (index (key1 (value1)))
 (add-to-index! test-index 'key2 'value2)
; Value: (index (key2 (value2)) (key1 (value1)))
 (add-to-index! test-index 'key1 'another-value1)
; Value: (index (key1 (another-value1 value1)) (key2 (value2)))

    	  	   	 



;;;;;;;::;;;;:::;;;;:::;:;::::;;::::::;:;:::;::;;;::;;;:;::;;::;::;;:;;::;;:;
;;; Exercise 4:  search-with-cycles
;;;    	  	   	 
;;; Part (a): you will need to write a similar search procedure that
;;; handles cycles    	  	   	 
;;;    	  	   	 
;;;    	  	   	 
;;; Follow the instructions in the Project3 handout, do not forget
;;; the documentation and test cases.
;;;    	  	   	 
    	  	   	 
(define (search-with-cycles initial-state goal? successors merge graph)
  ;; initial-state is the start state of the search
  ;;    	  	   	 
  ;; goal? is the predicate that determines whether we have
  ;; reached the goal    	  	   	 
  ;;    	  	   	 
  ;; successors computes from the current state all successor states
  ;;    	  	   	 
  ;; merge combines new states with the set of states still to explore
 (let ((visited (make-index)))
   (define (search-inner still-to-do)
    (if (null? still-to-do) #f
        (let ((current (car still-to-do)))
          (if *search-debug*
              (write (list 'now-at current)))
          (if (goal? current) #t
              (begin (add-to-index! visited 'visited current)
                     (search-inner
               (remove* (find-in-index visited 'visited) (merge (successors graph current) (cdr still-to-do)))))))))
  (search-inner (list initial-state)))) 

; ANSWER:
; search-with-cycles procedure acts similar with search procedure. There are some difference between them.
; One of them, there is a index that takes the track of the visited nodes' name. First, it defines empty index.
; then there is an inner search procedure that takes the list of the next visiting nodes. If the list is empty, then
; it means there is no nodes that the procedure search. If it is not null, then first take the first element of the list
; and define as current. Since *search-debug* is true, writes which nodes that it visit currently. After that, if it finds
; node that it searches, returns #t. If not, first adds the node that we are in to visited index. Then, removes visited nodes
; from the list of the next visiting nodes to not to visit again same nodes and do all the things that inner search procedure
; do recursively until find the node or return #f. 
;
;
;;;;;;;::;;;;:::;;;;:::;:;::::;;::::::;:;:::;::;;;::;;;:;::;;::;::;;:;;::;;::
;;;    	  	   	 
;;; Part (b): Use search-with-cycles to define a new procedure called DFS that
;;;           implements full depth first search.
;;;    	  	   	 
;;;    	  	   	 
;;; Follow the instructions in the Project3 handout, do not forget
;;; the documentation and test cases.
    	  	   	 
(define (DFS_cycled start goal? graph)
  (search-with-cycles start
          goal?
          find-node-children
          (lambda (new old) (append new old))
          graph))   	  	   	 

; ANSWER:
; DFS_cycled is similar with DFS-simple strategy, only difference between them, DFS-simply uses
; search procudure and DFS_cycled uses search-with-cycles procedure. That means it looks first child, then its child and so on
; if it's not already visited, then the others as DFS-simple.
;
;;;    	  	   	 
;;; Test Cases    	  	   	 
;;;
    (DFS_cycled
               'c
               (lambda (x) (eq? x 'k))
               test-cycle)
; Value: (now-at c)(now-at a)(now-at b)#f
    (DFS_cycled
               'a
               (lambda (x) (eq? x 'c))
               test-cycle)
; Value: (now-at a)(now-at b)(now-at c)#t
    (DFS_cycled
               'b
               (lambda (x) (eq? x 'a))
               test-cycle)
;Value: (now-at b)(now-at c)(now-at a)#t
;
; I also defined a new cycles which is bigger than test-cycle; because test-cycle does not give
; enough data to analyses the strategies.
; Here is the new cycle:
(define test-cycle-2    	  	   	 
  (make-graph (list    	  	   	 
   (make-graph-element 'a '(b c) '(words for node a))
   (make-graph-element 'b '(c d) '(words for node b))
   (make-graph-element 'c '(a f) '(words for node c))
   (make-graph-element 'd '(a c e) '(words for node d))
   (make-graph-element 'e '(b d) '(words for node e))
   (make-graph-element 'f '(a d) '(words for node f)))))
; Test Cases:
       (DFS_cycled
               'a
               (lambda (x) (eq? x 'e))
               test-cycle-2)
; Value: (now-at a)(now-at b)(now-at c)(now-at f)(now-at d)(now-at e)#t
        (DFS_cycled
               'e
               (lambda (x) (eq? x 'f))
               test-cycle-2)
; Value: (now-at e)(now-at b)(now-at c)(now-at a)(now-at f)#t
        (DFS_cycled
               'c
               (lambda (x) (eq? x 'e))
               test-cycle-2)
; Value: (now-at c)(now-at a)(now-at b)(now-at d)(now-at e)#t
        (DFS_cycled
               'e
               (lambda (x) (eq? x 'k))
               test-cycle-2)
; Value: (now-at e)(now-at b)(now-at c)(now-at a)(now-at f)(now-at d)#f
;
; Use at least 2 different test case with different nodes
;    	  	   	 
;;;    	  	   	 
;;;    	  	   	 
    	  	   	 
    	  	   	 
    	  	   	 
    	  	   	 
;;;;;;;::;;;;:::;;;;:::;:;::::;;::::::;:;:::;::;;;::;;;:;::;;::;::;;:;;::;:;;
;;; Part (c): Use search-with-cycles to define a new procedure called BFS that
;;;           implements full breadth first search.
;;;    	  	   	 
;;;    	  	   	 
;;; Follow the instructions in the Project 3 handout, do not forget
;;; the documentation and test cases.
    	  	   	 
(define (BFS_cycled start goal? graph)
  (search-with-cycles start
          goal?
          find-node-children
          (lambda (new old) (append old new))
          graph))    	  	   	 
; ANSWER:
; BFS_cycled is similar with BFS-simple strategy, only difference between them, BFS-simply uses
; search procudure and BFS_cycled uses search-with-cycles procedure. That means it looks first the all children its own
; if it's not already visited, then children's children as BFS-simple.     	  	   	 
;;;    	  	   	 
;;; Test Cases    	  	   	 
;;;
      (BFS_cycled
               'c
               (lambda (x) (eq? x 'k))
               test-cycle)
; Value: (now-at c)(now-at a)(now-at b)#f
     (BFS_cycled
               'a
               (lambda (x) (eq? x 'c))
               test-cycle)
; Value: (now-at a)(now-at b)(now-at c)#t
    (BFS_cycled
               'b
               (lambda (x) (eq? x 'a))
               test-cycle)
; Value: (now-at b)(now-at c)(now-at a)#t
;
; ANSWER:
; There is no difference between DFS and BFS path when we try to search in test-cycle because 'c is both 'a's and 'b's children.
; Then, DFS follows the way that 'a then its children 'b and 'b's children 'c.
; BFS follows the way that 'a, then its children 'b and 'a's children 'c.
;
; This is why I defined a new cycle which name is test-cycle-2. It's defined above, under the DFS_cycled code.
; Test Cases for test-cycle-2:
        (BFS_cycled
               'a
               (lambda (x) (eq? x 'e))
               test-cycle-2)
; Value: (now-at a)(now-at b)(now-at c)(now-at d)(now-at f)(now-at e)#t
        (BFS_cycled
               'e
               (lambda (x) (eq? x 'f))
               test-cycle-2)
; Value: (now-at e)(now-at b)(now-at d)(now-at c)(now-at a)(now-at f)#t
(BFS_cycled
               'c
               (lambda (x) (eq? x 'e))
               test-cycle-2)
; Value: (now-at c)(now-at a)(now-at f)(now-at b)(now-at d)(now-at e)#t
        (BFS_cycled
               'e
               (lambda (x) (eq? x 'k))
               test-cycle-2)
; Value: (now-at e)(now-at b)(now-at d)(now-at c)(now-at a)(now-at f)#f
;

; Use at least 2 different test case with different nodes
;    	  	   	 
;;;    	  	   	 
;;;    	  	   	 
    	  	   	 
;;;    	  	   	 
;;; Part (d): Give the order in which the nodes are visited for DFS and BFS
;;;    	  	   	 
;;;    	  	   	 
;; ANSWER:
; I used same search in DFS and BFS codes becuase I wanted to see the path that they follow when they
; search the same node starting with the same node.
; Here is the test cases (and their graph trees) that I used above:
; DFS ('a 'e) : a-b-c-f-d-e
; BFS ('a 'e) : a-b-c-d-f-e
; Tree for DSF:
;        (1)a
;        (2)b
;        (3)c
;        (4)f
;        (5)d
;        (6)e
; Tree for BSF:
;        (1)a
;      (2)b  (3)c
;      (4)d  (5)f
;      (6)e
; DFS ('c 'e) : c-a-b-d-e
; BFS ('c 'e) : c-a-f-b-d-e
; Tree for DSF:
;         (1)c
;       (2)a (-)f (f is not visited)
;       (3)b
;       (4)d
;       (5)e
; Tree for BSF:
;        (1)c
;      (2)a (3)f
;      (4)b
;      (5)d
;      (6)e
; DFS ('e 'f) : e-b-c-a-f
; BFS ('e 'f) : e-b-d-c-a-f
; DSF ('e 'k) : e-b-c-a-f-d-#f
; BFS ('e 'k) : e-b-d-c-a-f-#f
; Tree for DSF:
;        (1)e
;     (2)b   (6)d
;     (3)c
;   (4)a (5)f
; Tree for BFS:
;        (1)e
;     (2)b   (3)d
;     (4)c   (5)a
;     (6)f