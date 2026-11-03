(in-package #:trivial-formatter/coalton)

(defvar *coalton-layout* nil)

(defstruct bracket-form items)
(defstruct short-fn-form parameters trivia body)

(defun read-bracket-form (stream character)
  (declare (ignore character))
  (make-bracket-form
   :items (loop :for char = (peek-char t stream)
                :until (char= char #\])
                :collect (trivial-formatter::%read-as-code stream t nil t)
                :finally (read-char stream))))

(defun read-close-bracket (stream character)
  (declare (ignore stream character))
  (error "Unmatched close bracket in Coalton source."))

(defun read-short-fn-form (stream character)
  (declare (ignore character))
  (let ((seen (make-hash-table :test #'equal)))
    (let ((parameters
            (with-output-to-string (output)
              (loop :for char = (read-char stream)
                    :until (char= char #\.)
                    :do (unless (or (alpha-char-p char) (char= char #\_))
                          (error "Invalid short lambda parameter ~S." char))
                        (unless (char= char #\_)
                          (let ((name (string-upcase (string char))))
                            (when (gethash name seen)
                              (error "Duplicate short lambda parameter ~S." char))
                            (setf (gethash name seen) t)))
                        (write-char char output))))
          (trivia nil))
      (loop :for form = (trivial-formatter::%read-as-code stream t nil t)
            :while (typep form 'trivial-formatter::comment)
            :do (push form trivia)
            :finally
               (return (make-short-fn-form :parameters parameters
                                           :trivia (nreverse trivia)
                                           :body form))))))

(defun coalton-readtable ()
  "Build from the formatter's known syntax, independent of the caller's reader."
  (let ((table (named-readtables:copy-named-readtable 'trivial-formatter:as-code)))
    (set-macro-character #\[ #'read-bracket-form nil table)
    (set-macro-character #\] #'read-close-bracket nil table)
    (set-macro-character #\ƒ #'read-short-fn-form nil table)
    table))

(defmethod trivial-formatter:call-with-file-formatting-context :around
    ((pathname pathname) function)
  (if (equalp "ct" (pathname-type pathname))
      (let* ((*readtable* (coalton-readtable))
             ;; READ-AS-STRING otherwise reinstalls its own atom lexer, which
             ;; treats an adjacent closing bracket as part of a symbol.
             (read-as-string:*default-readtable* *readtable*)
             (*coalton-layout* t))
        (call-next-method))
      (call-next-method)))
