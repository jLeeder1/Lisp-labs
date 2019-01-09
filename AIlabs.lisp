;; A lisp file for the perceptron in lab 2 AI

(defun perc () (load "perceptron.lisp"))

;; A global variable for the house factors
(setf *house-factors '(cheap near-shops near-uni))

;; A function to make the perceptron with initialised biases as 1
;; line 1: define a function called "make-perceptron" with the paramater "factors"
;; line 2: create a local variable "perceptron" and let it be empty or "nil"
;; line 3: "(list 'bias 1)", make "bias 1" into a list with 2 elements: bias 1
;; line 3: "push (list 'bias 1) perceptron", put the list mentioned above into the variable perceptron
;; line 3: "reverse", reverse the order of the variable perceptron (so that the output order is the same as the input order)
;; line 3: dolist part "dolist (element factors ... perceptron", for each "element" in the given paramater "factors" do some function (line 4) and place the outcome into the variable "perceptron"
;; line 4: "(list element 1)", make a list containing the element from  the given paramater and the number 1
;; line 4: "push... perceptron". push the list mentioned above into the variable perceptron
;; line 4: "setf *perceptron...perceptron", set the variable perceptron equal to the functions mentied in the line 4 description. This means it will be saves as a global variable (*) and can be used elsewhere instead of as a ;ocal variable as suggested by "let" in line 2

(defun make-perceptron (factors)
  (let ((perceptron nil))
     (dolist (element factors (reverse (push (list 'bias 1) perceptron)))
       (setf *perceptron (push (list element 1) perceptron)))))



;; Creatingthe houses to classify

;; Lines 1, 2, 3 are a similar idea to the above
;; Line 4, "list element (first values))", create a list of the element from the paramater factors and the first element of list from paramater house: (cheap 1)
;; line 4, "push..house"", push the above line into the variable house
;; line 4, "setf *house", place the outcome of the above two line (in this dedcription) into a global variable called house
;; line 5, this is the clever bit, this uses the setf to set the value of the paramater "values" to the next memeber of the list. It says "set the list "valus" that currently contains (1 2 3) to equal the rest of the current line: (2 3). And does this for the amount of elements in "factors" so that yo are placing a differnt value in for each of the "factors". Really liked this

(defun make-house (factors values)
  (let ((house nil))
    (dolist (element factors (reverse house))
      (setf *house (push (list element (first values)) house)
	    values (rest values)))))



;; Task 3a

;; line 1, Define a function called "score" with 2 paramaters: "perceptron" and "house
;; line 2, Create a local variable called score which is initialised to 0
;; line 3, Open a do list where you iterate over each value "var" in the paramater "house"
;; line 4, Create a local variable called "var-val", make this variable equal to the second element of the list "var" (declared in the line above).
;; Line 5, Create a local variable for the weight from the perceptron associated with the current element of the paramater house
;; This works by: "first var" getting the first element of list "var" (cheap 1), e.g. "cheap
;;  "assoc...perceptron". Then get the association list the first var (e.g. "cheap") from the paramater "perceptron" (e.g. cheap 0)
;; "Second..perceptron", then get the second element of the list (cheap 0), so 0 and make "var-weight" equal this
;; line 6, increment score by (var val * var-weight (e.g. 1)
;; line 7, increment score by the value of the bias in the perceptron (the second value of (bias 1)

(defun score (perceptron house)
  (let ((score 0))
    (dolist (var house)
      (let ((var-val (second var))
	    (var-weight (second (assoc (first var) perceptron))))
	(incf score (* var-val var-weight))))
    ;; now add the bias to the score
    (incf score (second (assoc 'bias perceptron)))))

	
	
	
	;; Lab work for AI lab 3

(defun lab3 () (load "lab3.lisp"))

;; Global variable for the house, note the heirarchy
(setf *house
      '((type terrace)
	(rooms ((kitchen ((dimensions (20 12))
			  (appliances (cooker fridge dishwasher))))
		(bedroom ((dimensions (15 21))
			  (features ((double-bed washbasin)))))))
	(garden (pond lawn shed))))


;; These are just practice tasks for assoc

(setf *mylist '((a1 )(b 2)(c 3)))

;; Typeing (assoc c mylist) returns an error becasue it trys to search or symbol c, c has no value though (you need to put 'c so you don't evaluate c as a symbol)

;; Typing (asssoc 'd mylist) returns nil as there is no d in the list "mylist", not an erros just empty

;; A function to get the value from mylist when giving a key (need to put 'b not b)
;; Could be imporved where you give it a list as a paramater instead of hard coding
(defun get-property (prop)
  (second (assoc prop *mylist)))

;; Improved version as stated below
;; Could improve it to get a certain element number
(defun get-prop (prop thelist)
  (second (assoc prop thelist)))


;; A function to return a list of rooms and their features in a given house

(defun get-rooms (house)
  (assoc 'rooms *house))

;; A function to return a specific rooms features when given a room and a house

;; My version
;; Before this i was trying "list" in place of "second"
;; This returned "nil" becasue it was getting the list containing "room" instead of the
;; Room featrue list and then was trying to make an association list withe the paramater room
;; e.g. "kitchen" and the string "room" so was returning nil
;; I just read the desription on the lab sheet after writing this nad it says exactly the above
;; i have been stuck on this for ages, fuck my life

(defun get-a-room (room house)
  (assoc room (second (assoc 'rooms *house))))

;; His version
(defun get-room-type (room house)
  (assoc room (second (get-rooms house))))

;; Function to return garden features

(defun get-garden (house)
  (assoc 'garden house))

;; Get kitchen appliances from a given house
(defun get-appliances (house)
  (second (second (assoc 'kitchen (second (get-rooms house))))))

;; A function for getting the dimensions of a given room
;; "assoc...house" far right, get the lists associated with "rooms" in the house
;; second (middle), get the second eleemnt of the rooms list (it goes (rooms(list 1) (kitchen...)(bedroom...)
;; "assoc room", get the given froom from the above lists
;; get the second element from the list "(kitchen (dimensions appliances)"
;; get the first element from (dimenstions appliances)
(defun get-dimensions (room house)
  (first (second (assoc room (second (assoc 'rooms house))))))

;; There is an extension task here but it is long and i can't be assed, maybe look later




(defun lab4 () (load "lab4.lisp"))

;; Jalals stuff

(setf *rule-1 '(rule-conclusion ((adequate-savings false)(high-income false))))

;; returns (high-income false)
(assoc 'high-income (second *rule-1))

;; returns false (from high-income)
(second (assoc 'high-income (second *rule-1)))


;; Actual work for the lab

;; A function to return if x or y is the bigger number
;; So you define your function with paramaters in line 1
;; Then the condition if line 2, it reasds "IF X is GREATER than Y" do the next line
;; ELSE do line 4
;; Note that to do an else the word "progn" is nexessary, without this an error for too many arguements in the if is returned
;; The "~%" part seems to add a new line in

(defun bigger (x y)
  (if (> x y)
      (format t "~%first arguement is bigger")
    (progn (format t "~%second arguement is bigger"))))


;; Second method ofr bigger for testing the if statement
;; He talks about prog1 and progn stuff and i don't understand it
;; Prog1 apparently returns the first of a form and progn returns the last form
(defun bigger2 (x y)
  (if (> x y)
      (prog1 (format t "~%first arguement is bigger")
    (progn (format t "~%second arguement is bigger")))))


;; tasks

(setf students '((dave 81)(dee 26)(dozy 58)(beaky 52)(mick 45)(tich 64)))

;; This is returning the error "nil is not a real number"
;; have checked the "second (accoc..." park with "dee", it returns 26
;; Have take out line 2, has no effect
;; Is not reaching the else

(defun sort-students (student-list bottom-mark top-mark)
  (let ((students-marks nil))
    (dolist (element student-list students-marks)
      (let ((current-mark (second (assoc 'element student-list))))
      (if (> current-mark  bottom-mark)
	  (format t "~%This mark is lower than the boundary")))))
  (progn (format t "~%This is getting to here")))


  



	  
