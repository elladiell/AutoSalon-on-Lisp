(DEFSTRUCT NOTE ID NAME MASHINA)
(SETF (GET 'AUTOPARK' ID_COUNTER) 5)
(SETQ AUTOPARK
  (CONS (MAKE-NOTE :ID 1 :NAME 'VOLVO :MASHINA 3651)
    (CONS (MAKE-NOTE :ID 2 :NAME 'KAMAZ :MASHINA 1234)
      (CONS (MAKE-NOTE :ID 3 :NAME 'ZHIGUL :MASHINA 4732)
        (CONS (MAKE-NOTE :ID 4 :NAME 'ALFA-ROMEO :MASHINA 1345)
          (CONS (MAKE-NOTE :ID 5 :NAME 'BMW :MASHINA 7543) NIL))))))

(DEFUN MAIN ()
  (SETQ choice (PRINT_MENU))
  (COND
    ((EQ choice 0) T)
	((EQ choice 1)
	  (PRINT_AUTOPARK AUTOPARK)
	  (MAIN))
	((EQ choice 2)
	  (ADD_NOTE)
	  (MAIN))
	((EQ choice 3)
	  (FORMAT T "***** Edit note *****~%")
	  (FORMAT T "Input note ID: ")
	  (SETQ _id (READ))
	  (SETQ ed_note (SEARCH_NOTE AUTOPARK (LAMBDA (n) (IF (EQ (NOTE-ID n) _id) T NIL))))
	  (IF (NULL ed_note)
	    (FORMAT T "The note not found~%")
		(PROGN (EDIT_NOTE (CAR ed_note))
		 (FORMAT T "The note has been edited.~%")))
	  (MAIN))
	((EQ choice 4)
	  (FORMAT T "***** Delete note *****~%")
      (FORMAT T "Input note ID: ")
	  (SETQ _id (READ))
	  (SETQ del_note (SEARCH_NOTE AUTOPARK (LAMBDA (n) (IF (EQ (NOTE-ID n) _id) T NIL))))
	  (IF (NULL del_note)
	    (FORMAT T "The note not found~%")
		(PROGN (DELETE_NOTES del_note)
		 (FORMAT T "The note has been deleted.~%")))
	  (MAIN))
	((EQ choice 5)
	  (SETQ choice (PRINT_SEARCH_MENU))
	  (COND
	    ((EQ choice 1)
		  (FORMAT T "Input ID: ")
		  (SETQ _id (READ))
		  (PRINT_AUTOPARK 
		    (SEARCH_NOTE 
			  AUTOPARK 
			  (LAMBDA (n) (IF (EQ (NOTE-ID n) _id) T NIL)))))
		((EQ choice 2) 
		  (FORMAT T "Input mark: ")
		  (SETQ _name (READ))
		  (PRINT_AUTOPARK 
		    (SEARCH_NOTE 
			  AUTOPARK 
			  (LAMBDA (n) (IF (EQ (NOTE-NAME n) _name) T NIL)))))
		((EQ choice 3) 
		  (FORMAT T "Input number of car: ")
		  (SETQ _mashina (READ))
		  (PRINT_AUTOPARK 
		    (SEARCH_NOTE 
			  AUTOPARK 
			  (LAMBDA (n) (IF (EQ (NOTE-MASHINA n) _mashina) T NIL)))))
		((EQ choice 4)
		  (FORMAT T "Input car`s code: ")
		  (SETQ _code (READ))
		  (PRINT_AUTOPARK 
		    (SEARCH_NOTE 
			  AUTOPARK
			  (LAMBDA (n) (IF (EQ (MASHINA_CODE (NOTE-MASHINA n)) _code) T NIL))))))
		(MAIN))
	(T
	  (MAIN))))
	  
  

(DEFUN PRINT_MENU ()
  (FORMAT T "***** AUTOPARK *****~%")
  (FORMAT T "1. View autopark~%")
  (FORMAT T "2. Add note~%")
  (FORMAT T "3. Edit note~%")
  (FORMAT T "4. Delete note~%")
  (FORMAT T "5. Search note~%")
  (FORMAT T "0. Exit~%")
  (FORMAT T ">>")
  (READ))
  
(DEFUN PRINT_SEARCH_MENU ()
  (FORMAT T "***** Search note *****~%")
  (FORMAT T "1. Search by ID~%")
  (FORMAT T "2. Search by name~%")
  (FORMAT T "3. Search by car number~%")
  (FORMAT T "4. Search by car code~%")
  (FORMAT T ">>")
  (READ))
  
(DEFUN PRINT_NOTE (n)
  (COND
    ((NULL n) NIL)
    (T 
	  (FORMAT T "***** NOTE #~A *****~%" (NOTE-ID n))
      (FORMAT T "Name: ~A~%" (NOTE-NAME n))
      (FORMAT T "Car number: ~A~%" (NOTE-MASHINA n)))))
  
(DEFUN PRINT_AUTOPARK (SEQ)
  (COND
    ((NULL SEQ) T)
	(T
	  (PRINT_NOTE (CAR SEQ))
	  (TERPRI)
	  (PRINT_AUTOPARK (CDR SEQ)))))
  
(DEFUN ADD_NOTE ()
  (SETQ new_note (MAKE-NOTE))
  (FORMAT T "***** Add note *****~%")
  (FORMAT T "Input name: ")
  (SETF (NOTE-NAME new_note) (READ))
  (FORMAT T "Input Mashina`s number: ")
  (SETF (NOTE-MASHINA new_note) (READ))
  (SETF (NOTE-ID new_note) (INCF (GET 'AUTOPARK' ID_COUNTER)))
  (SETQ AUTOPARK (APPEND AUTOPARK (CONS new_note NIL)))
  (FORMAT T "The note has been added.~%"))
  
(DEFUN SEARCH_NOTE (SEQ CRITERIA)
  (COND
    ((NULL SEQ) NIL)
	(T
	  (IF (FUNCALL CRITERIA (CAR SEQ)) 
	    (CONS (CAR SEQ) (SEARCH_NOTE (CDR SEQ) CRITERIA))
		(SEARCH_NOTE (CDR SEQ) CRITERIA)))))
		
(DEFUN DELETE_NOTES (NOTES)
  (COND
    ((NULL NOTES) NIL)
	(T
	  (SETQ AUTOPARK (REMOVE (CAR NOTES) AUTOPARK))
	  (DELETE_NOTES (CDR NOTES)))))
	
(DEFUN EDIT_NOTE (n)
  (FORMAT T "Input name: ")
  (SETF (NOTE-NAME n) (READ))
  (FORMAT T "Input car number: ")
  (SETF (NOTE-MASHINA n) (READ)))
  
(DEFUN MASHINA_CODE (MASHINA)
  (FLOOR MASHINA 10000000))
	