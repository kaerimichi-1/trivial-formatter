(in-package #:trivial-formatter/coalton)

(defvar *comment-at-line-start* nil)
(defvar *closing-toplevel-comment* nil)

(defun coalton-operator-p (form name)
  (and (consp form)
       (eq (car form) (find-symbol name '#:coalton))))

(defun definition-p (form)
  (and (consp form)
       (symbolp (car form))
       (eq (symbol-package (car form)) (find-package '#:coalton))
       (let ((name (symbol-name (car form))))
         (or (string= name "DEFINE")
             (uiop:string-prefix-p "DEFINE-" name)))))

(defun attribute-p (form)
  (some (lambda (name) (coalton-operator-p form name))
        '("DERIVE" "REPR" "MONOMORPHIZE" "INLINE" "NOINLINE" "OVERLAP")))

(defun declaration-matches-p (declaration definition)
  (and (coalton-operator-p definition "DEFINE")
       (let ((name (second definition)))
         (eq (second declaration) (if (consp name) (car name) name)))))

(defun inline-comment-p (comment)
  (case (trivial-formatter::line-comment-inline-p comment)
    ((t) t)
    ((nil) nil)
    (otherwise
     ;; Retain the original formatter's convention for non-seekable streams.
     (not (uiop:string-prefix-p
           ";" (trivial-formatter::comment-content comment))))))

(defun definition-groups (forms)
  "Group adjacent declarations, attributes and documentation with a definition.
The order of all source items is preserved, including unmatched declarations."
  (let (groups current kind declaration)
    (flet ((flush-group ()
             (when current
               (push (nreverse current) groups))
             (setf current nil kind nil declaration nil)))
      (dolist (form forms)
        (cond
          ((typep form 'trivial-formatter::line-comment)
           (unless (and current (inline-comment-p form))
             (when (member kind '(:definition :other)) (flush-group))
             (setf kind :prefix))
           (push form current))
          ((or (attribute-p form)
               (typep form 'trivial-formatter::block-comment))
           (when (member kind '(:definition :other)) (flush-group))
           (setf kind :prefix)
           (push form current))
          ((coalton-operator-p form "DECLARE")
           (when (or declaration (member kind '(:definition :other)))
             (flush-group))
           (setf declaration form kind :prefix)
           (push form current))
          ((definition-p form)
           (when (or (member kind '(:definition :other))
                     (and declaration
                          (not (declaration-matches-p declaration form))))
             (flush-group))
           (push form current)
           (setf kind :definition))
          (t
           (when current (flush-group))
           (push form current)
           (setf kind :other))))
      (flush-group))
    (nreverse groups)))

(defmethod print-object :around ((comment trivial-formatter::line-comment) stream)
  (if *coalton-layout*
      (progn
        (cond ((inline-comment-p comment) (write-char #\Space stream))
              ((not *comment-at-line-start*) (pprint-newline :mandatory stream)))
        (write-char #\; stream)
        (unless (uiop:string-prefix-p
                 ";" (trivial-formatter::comment-content comment))
          (write-char #\Space stream))
        (write-string (trivial-formatter::comment-content comment) stream)
        (when *closing-toplevel-comment* (pprint-indent :block -1 stream))
        (pprint-newline :mandatory stream))
      (call-next-method)))

(defun pprint-toplevel (stream form)
  (pprint-logical-block (stream nil :prefix "(" :suffix ")")
    (write (car form) :stream stream)
    (pprint-indent :block 1 stream)
    (let ((first-group t) (after-comment nil))
      (dolist (group (definition-groups (cdr form)))
        (unless (or after-comment
                    (and first-group
                         (typep (car group) 'trivial-formatter::line-comment)
                         (inline-comment-p (car group))))
          (pprint-newline :mandatory stream))
        (unless first-group (pprint-newline :mandatory stream))
        (let ((first-item t))
          (dolist (item group)
            (let ((comment-p (typep item 'trivial-formatter::line-comment)))
              (unless (or first-item after-comment
                          (and comment-p (inline-comment-p item)))
                (pprint-newline :mandatory stream))
              (let ((*comment-at-line-start*
                      (and comment-p (not (inline-comment-p item))))
                    (*closing-toplevel-comment*
                      (and comment-p (eq item (car (last form))))))
                (write item :stream stream))
              (setf first-item nil after-comment comment-p))))
        (setf first-group nil))
      (when after-comment (pprint-indent :block -1 stream)))))

(defun contains-toplevel-p (form)
  (and (consp form)
       (or (coalton-operator-p form "COALTON-TOPLEVEL")
           (and (not (member (car form) '(quote function)))
                (or (contains-toplevel-p (car form))
                    (contains-toplevel-p (cdr form)))))))

(defun pprint-definition (stream form)
  "Print a definition header and its body without moving inline comments."
  (pprint-logical-block (stream nil :prefix "(" :suffix ")")
    (write (car form) :stream stream)
    (when (cdr form)
      (write-char #\Space stream)
      (write (second form) :stream stream))
    (pprint-indent :block 1 stream)
    (let ((after-comment nil))
      (loop :for (item . rest) :on (cddr form)
            :for comment-p = (typep item 'trivial-formatter::line-comment)
            :do (unless (or after-comment
                            (and comment-p (inline-comment-p item)))
                  (pprint-newline :mandatory stream))
                (let ((*comment-at-line-start*
                        (and comment-p (not (inline-comment-p item))))
                      (*closing-toplevel-comment* (and comment-p (null rest))))
                  (write item :stream stream))
                (setf after-comment comment-p)))))

(defmethod trivial-formatter:write-code :around ((form t) stream)
  (if (or *coalton-layout* (contains-toplevel-p form))
      (let ((*coalton-layout* t))
        ;; The general postprocessor discards blank lines and joins comment
        ;; lines. This printer has already placed both using the source syntax.
        (let ((stream (trivial-formatter::output-stream stream)))
          (if (typep form 'trivial-formatter::line-comment)
              (progn
                (write-char #\; stream)
                (unless (uiop:string-prefix-p
                         ";" (trivial-formatter::comment-content form))
                  (write-char #\Space stream))
                (write-string (trivial-formatter::comment-content form) stream))
              (write-string (trivial-formatter::string-as-code form) stream)))
        nil)
      (call-next-method)))

(trivial-formatter-user::set-pprint-dispatch
 '(cons (eql coalton:coalton-toplevel)) #'pprint-toplevel)

(trivial-formatter-user::set-pprint-dispatch
 '(cons (member coalton:define coalton:define-type coalton:define-type-alias
                coalton:define-struct coalton:define-class coalton:define-instance
                coalton:define-exception))
 #'pprint-definition)
