(in-package :cl-user)
(defpackage set-game
  (:use :cl)
  (:export #:start-game-server))
(in-package :set-game)


(defun parse-card (str)
  (let ((r (parse-integer str :radix 3)))
    (unless (<= 0 r 80) (error "Invalid card ~a" str))
    r))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; we need these functions to define the constant below
  (defun write-card (num)
    (format nil "~3,4,'0r" num))
  (defun set-p (a b c)
    (etypecase a
      (integer (set-p (write-card a) b c))
      (string
       (etypecase b
         (integer (set-p a (write-card b) c))
         (string
          (etypecase c
            (integer (set-p a b (write-card c)))
            (string
             (flet ((char-to-num (c)
                      (- (char-code c) (char-code #\0))))
               (loop for digit from 0 below 4
                  with result = (make-array '(4) :element-type 'bit
                                            :initial-element 0)
                  with total-result = t
                  do (let ((da (char-to-num (aref a digit)))
                           (db (char-to-num (aref b digit)))
                           (dc (char-to-num (aref c digit))))
                       (setf (aref result digit)
                             (if (zerop (mod (+ da db dc) 3))
                                 (prog1 1
                                   (setf total-result (and total-result t)))
                                 (prog1 0
                                   (setf total-result nil)))))
                  finally (return (values total-result result))))))))))))

(defconstant +set-completion+
  (if (boundp '+set-completion+) (symbol-value '+set-completion+)
      (make-array '(81 81) :element-type '(unsigned-byte 8)
                  :initial-contents
                  (loop for i from 0 below 81
                     collect
                       (loop for j from 0 below 81
                          collect
                            (if (= i j) i
                                (loop for k from 0 below 81
                                   thereis (and (set-p i j k) k))))))))

(defun set-completion (a b)
  (aref +set-completion+ a b))

(defun make-random-deck ()
  (let ((deck (make-array '(81)
			  :element-type '(unsigned-byte 8)
			  :initial-contents
			  #.(map 'vector #'identity
				 (loop for i from 0 to 80 collect i)))))
    (loop for i from 0 to 79
       for j = (random (the (unsigned-byte 8) (- 81 i)))
       do (rotatef (aref deck i)
		   (aref deck (+ i j))))
    deck))

(defclass game (hunchensocket:websocket-resource)
  ((deck :initform (make-random-deck) :reader deck)
   (table :initform (make-array '(81) :element-type 'bit :initial-element 0)
	  :reader table)
   (drawn :initform 0 :accessor draw-count)
   (id :initarg :id :reader game-id)
   (players :initform 0 :accessor player-count)))
(defclass player (hunchensocket:websocket-client)
  ((score :initform 0 :accessor score)
   (name :initform nil :accessor name)
   (ready :initform nil :accessor ready)))

(defun game-started-p (game)
  (plusp (draw-count game)))
(defun draw-cards (game n)
  (let* ((start (draw-count game))
	 (end (min 81 (+ start n))))
    (setf (draw-count game) end)
    (loop for i from start below end
       for card = (aref (deck game) i)
       do (setf (aref (table game) card) 1)
       collect card)))
(defun withdraw-cards (game cards)
  (loop for card in cards
     if (zerop (aref (table game) card))
     do (error "Card ~a not in game ~a" card game)
     else do (setf (aref (table game) card) 0))
  nil)
(defun find-set (game)
  (let ((cards (loop for i from 0
		  for j across (table game)
		  when (plusp j) collect i)))
    (loop for (c1 . r) on cards
       do (loop for c2 in r
	     for c3 = (set-completion c1 c2)
	     when (plusp (aref (table game) c3))
	     do (return-from find-set (values t (list c1 c2 c3)))))
    nil))


(defvar *games* (make-hash-table :test 'eq))

(defun new-game ()
  (loop for id = (random #|most-positive-fixnum|# 1000)
     when (not (gethash id *games* nil))
     return (setf (gethash id *games*)
		  (make-instance 'game :client-class 'player
				 :id id))))
;;; games are at /g/id
(defun find-game (request)
  (let ((n (hunchentoot:script-name request)))
    (when (and
	   (> (length n) 3)
	   (string= n "/g/" :end1 3))
      (gethash (parse-integer n :start 3)
	       *games*))))
(defun maybe-new-game (request)
  (when (string= (hunchentoot:script-name request) "/new")
    (new-game)))
(pushnew 'find-game hunchensocket:*websocket-dispatch-table*)
(pushnew 'maybe-new-game hunchensocket:*websocket-dispatch-table*)

(defun broadcast (game message &rest args)
  (loop for player in (hunchensocket:clients game)
     do (hunchensocket:send-text-message player
					 (apply #'format nil message args)))
  nil)
;;; sent message formats:
;;; Pn   : there are n players in the room
;;; C... : cards (strings of four digits) to be added; concatenated
;;; W... : cards to be removed.
;;; S... : the scores and names of the players: score #\SPACE name #\newline
;;; O    : game over
;;; E... : an error
;;; N    : not a set
(defun broadcast-scores (game)
  (broadcast
   game
   "S~{~{~d ~a~}~%~}"
   (loop for player in (hunchensocket:clients game)
      when (name player)
      collect (list (score player) (name player)))))
(defun game-over (game)
  (broadcast-scores game)
  (broadcast game "O")
  (loop for player in (hunchensocket:clients game)
     do (hunchensocket:close-connection player))
  (remhash (game-id game) *games*))
(defun maybe-write-card (c)
  (etypecase c
    (integer (write-card c))
    (string c)))
(defun broadcast-cards (game cards)
  (if cards
      (broadcast game "C~{~A~}" (mapcar #'maybe-write-card cards))
      ;; no cards => could not draw any more so check if game has ended
      ;; all cards gone => game is efinitely over
      (when (< (count 1 (table game)) 3)
	(game-over game))))
(defun broadcast-withdraw (game cards)
  (broadcast game "W~{~A~}" (mapcar #'maybe-write-card cards)))


(defun start-game (game)
  (broadcast-scores game)
  (broadcast-cards game (draw-cards game 12)))
(defun maybe-start-game (game)
  (when (and
	 (not (game-started-p game))
	 (loop for player in (hunchensocket:clients game)
	    always (ready player)))
    (start-game game)))



(defmethod hunchensocket:client-connected ((game game) player)
  (unless (game-started-p game)
    (unless (cdr (hunchensocket:clients game))
      (broadcast game "~a" (game-id game)))
    (broadcast game "P~d" (incf (player-count game)))))
(defmethod hunchensocket:client-disconnected ((game game) player)
  (broadcast game "P~d" (decf (player-count game)))
  (if (hunchensocket:clients game)
      (maybe-start-game game)
      (remhash (game-id game) *games*)))
(defmethod hunchensocket:text-message-received ((game game) player message)
  (if (not (ready player))
      (if (find #\Newline message :test #'char=)
	  (hunchensocket:send-text-message player "ENewline")
	  (if (find message (hunchensocket:clients game)
		    :key #'name :test #'string=)
	      (hunchensocket:send-text-message player "EAlreay Used")
	      (progn
		(setf (name player) message
		      (ready player) t)
		(if (game-started-p game)
		    (progn
		      (broadcast-scores game)
		      (hunchensocket:send-text-message
		       player
		       (format nil "C~{~A~}"
			       (loop for i from 0
				  for j across (table game)
				  when (plusp j) collect (write-card i)))))
		    (maybe-start-game game))
		(unless (game-started-p game)
		  (broadcast-scores game)))))
      (if (= 12 (length message))
	  (let ((a (subseq message 0 4))
		(b (subseq message 4 8))
		(c (subseq message 8 12)))
	    (if (set-p a b c)
		(block outer
		  (broadcast-withdraw
		   game
		   (handler-case
		       (progn
			 (withdraw-cards game (mapcar #'parse-card
						      (list a b c)))
			 (list a b c))
		     (error (c)
		       (declare (ignore c))
		       (hunchensocket:send-text-message player "ENot on table")
		       (return-from outer))))
		  (broadcast-cards game (draw-cards game 3))
		  (incf (score player)))
		(progn
		  (decf (score player))
		  (hunchensocket:send-text-message player "N")))
	    (broadcast-scores game))
	  (hunchensocket:send-text-message player "EInvalid Message"))))


(defvar *server*)

(defun start-game-server (&key (port 8080))
  (setf *server*
        (make-instance 'hunchensocket:websocket-acceptor :port port))
  (setf (hunchentoot:acceptor-document-root *server*)
        (asdf:system-relative-pathname :set-game "www/"))
  (hunchentoot:start *server*))


