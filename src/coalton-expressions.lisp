(in-package #:trivial-formatter/coalton)

(defstruct role-form role value)

(defun trivia-p (item)
  (typep item 'trivial-formatter::comment))

(defun significant-items (items)
  (remove-if #'trivia-p items))

(defun token-name-p (item name)
  (and (symbolp item) (string= name (symbol-name item))))

(defun map-items (function items)
  (loop :with index = 0
        :for item :in items
        :collect (if (trivia-p item) item
                     (prog1 (funcall function index item) (incf index)))))

(defun split-groups (items boundaries)
  "Split at semantic item indices; retain comments on their original side."
  (let (groups current pending (index 0))
    (labels ((flush () (push (nreverse current) groups) (setf current nil))
             (drain ()
               (dolist (item (nreverse pending)) (push item current))
               (setf pending nil)))
      (dolist (item items)
        (cond ((and (trivia-p item)
                    (not (and (typep item 'trivial-formatter::line-comment)
                              (inline-comment-p item))))
               (push item pending))
              ((trivia-p item) (drain) (push item current))
              (t
               (when (member index boundaries) (flush))
               (drain)
               (push item current)
               (incf index))))
      (drain)
      (flush))
    (nreverse groups)))

(defun pprint-grouped (stream operator groups &key (prefix "(") (suffix ")")
                                                  (indent 1))
  "Print (separator items) groups. Break only at explicitly chosen boundaries."
  (pprint-logical-block (stream nil :prefix prefix :suffix suffix)
    (when operator (write operator :stream stream))
    (pprint-indent :block indent stream)
    (let ((after-comment nil) (line-start nil)
          (last-item (car (last (mapcan (lambda (group) (copy-list (second group)))
                                      groups)))))
      (dolist (group groups)
        (destructuring-bind (separator items) group
          (when items
            (unless (or after-comment
                        (and (typep (car items) 'trivial-formatter::line-comment)
                             (inline-comment-p (car items))))
              (case separator
                (:space (write-char #\Space stream))
                (:fill (write-char #\Space stream) (pprint-newline :fill stream))
                (:linear (write-char #\Space stream) (pprint-newline :linear stream))
                (:newline (pprint-newline :mandatory stream) (setf line-start t))))
            (let ((first t))
              (dolist (item items)
                (let ((comment-p (typep item 'trivial-formatter::line-comment)))
                  (unless (or first after-comment
                              (and comment-p (inline-comment-p item)))
                    (write-char #\Space stream))
                  (let ((*comment-at-line-start* (and comment-p line-start))
                        (*closing-toplevel-comment* (and comment-p (eq item last-item))))
                    (write item :stream stream))
                  (setf first nil after-comment comment-p line-start comment-p))))))))))

(defun pprint-expression (stream form header-count &key (body-separator :newline)
                                                        header-mapper body-mapper)
  (let* ((items (cdr form))
         (count (length (significant-items items)))
         (parts (split-groups items (loop :for index :from header-count :below count
                                         :collect index))))
    (pprint-grouped
     stream (car form)
     (cons (list (when (car form) :space)
                 (if header-mapper (map-items header-mapper (car parts))
                           (car parts)))
           (mapcar (lambda (part)
                     (list body-separator
                           (if body-mapper (map-items body-mapper part) part)))
                   (cdr parts))))))

(defun pprint-sequence (stream items separator)
  (pprint-grouped
   stream nil
   (loop :for group :in (split-groups items
                                     (loop :for index :from 1
                                           :below (length (significant-items items))
                                           :collect index))
         :for first = t :then nil
         :collect (list (unless first separator) group))
   :indent 0))

(defmethod print-object ((form role-form) stream)
  (case (role-form-role form)
    (:bindings (pprint-sequence stream (role-form-value form) :linear))
    (:parameters (pprint-sequence stream (role-form-value form) :fill))
    (:type (pprint-type stream (role-form-value form)))
    (:bind (pprint-binding stream (role-form-value form)))
    (:clause (pprint-clause stream (role-form-value form)))
    (otherwise (write (role-form-value form) :stream stream))))

(defun pprint-let (stream form)
  (let ((items (significant-items (cdr form))))
    (if (token-name-p (second items) "=")
        (pprint-expression stream form 2 :body-separator :fill)
        (pprint-expression stream form 1
                           :header-mapper (lambda (index item)
                                            (declare (ignore index))
                                            (make-role-form :role :bindings :value item))))))

(defun pprint-let-star (stream form)
  (pprint-expression stream form 1
                     :header-mapper (lambda (index item)
                                      (declare (ignore index))
                                      (make-role-form :role :bindings :value item))))

(defun pprint-binding (stream form)
  (let ((parts (split-groups form '(2))))
    (pprint-grouped stream nil
                    (list (list nil (car parts)) (list :fill (second parts))))))

(defun pprint-do (stream form)
  (pprint-expression stream form 0
                     :body-mapper
                     (lambda (index item)
                       (declare (ignore index))
                       (if (and (consp item)
                                (token-name-p (second (significant-items item)) "<-"))
                           (make-role-form :role :bind :value item)
                           item))))

(defun pprint-for (stream form)
  (let* ((raw (cdr form))
         (items (significant-items raw))
         (binding-index (if (keywordp (first items)) 1 0))
         (header-count (1+ binding-index))
         (index header-count)
         (boundaries (list header-count)))
    (when (eq (nth index items) :returns)
      (incf index 2) (push index boundaries))
    (when (member (nth index items) '(:while :until :repeat))
      (incf index 2) (push index boundaries))
    (loop :for position :from index :below (length items)
          :do (push position boundaries))
    (setf raw (map-items (lambda (position item)
                          (if (= position binding-index)
                              (make-role-form :role :bindings :value item) item)) raw))
    (let ((parts (split-groups raw (remove-duplicates boundaries))))
      (pprint-grouped stream (car form)
                      (cons (list :space (car parts))
                            (mapcar (lambda (part) (list :newline part)) (cdr parts)))))))

(defun comprehension-boundaries (items start)
  (loop :for position = start :then (+ position size)
        :while (< position (length items))
        :for marker = (nth position items)
        :for size = (case marker (:with 4) (:for 4) (:when 2)
                          (otherwise (error "Unknown Coalton builder clause ~S." marker)))
        :unless (<= (+ position size) (length items))
          :do (error "Incomplete Coalton builder clause ~S." marker)
        :collect position))

(defmethod print-object ((form bracket-form) stream)
  (let* ((raw (bracket-form-items form))
         (items (significant-items raw))
         (association-p (or (and (= (length items) 1) (token-name-p (first items) "=>"))
                            (token-name-p (second items) "=>")))
         (head-size (if association-p 3 1))
         (comprehension-p (member (nth head-size items) '(:with :for :when)))
         (boundaries
           (cond (comprehension-p (comprehension-boundaries items head-size))
                 (association-p (loop :for index :from 3 :below (length items) :by 3
                                      :collect index))
                 (t (loop :for index :from 1 :below (length items) :collect index)))))
    (pprint-grouped
     stream nil
     (loop :for part :in (split-groups raw boundaries)
           :for first = t :then nil
           :collect (list (unless first (if comprehension-p :linear :fill)) part))
     :prefix "[" :suffix "]" :indent 0)))

(defmethod print-object ((form short-fn-form) stream)
  (pprint-grouped stream nil
                  (list (list nil (append (short-fn-form-trivia form)
                                          (list (short-fn-form-body form)))))
                  :prefix (concatenate 'string "ƒ" (short-fn-form-parameters form) ".")
                  :suffix "" :indent 0))

(defun pprint-type (stream form)
  (if (atom form)
      (write form :stream stream)
      (let* ((items (significant-items form))
             (boundaries (loop :for item :in items :for index :from 1
                               :when (and (< index (length items))
                                          (some (lambda (name) (token-name-p item name))
                                                '("=>" "*" "->")))
                                 :collect index))
             (raw (map-items (lambda (index item)
                               (declare (ignore index))
                               (if (consp item) (make-role-form :role :type :value item) item))
                             form)))
        (pprint-grouped
         stream nil
         (loop :for part :in (split-groups raw boundaries)
               :for first = t :then nil
               :collect (list (unless first :linear) part))
         :indent 0))))

(defun pprint-declare (stream form)
  (pprint-expression stream form 1 :body-separator :fill
                     :body-mapper (lambda (index item)
                                    (declare (ignore index))
                                    (make-role-form :role :type :value item))))

(defun pprint-fn (stream form)
  (let* ((items (cdr form))
         (single-body-p (= 2 (length (significant-items items))))
         (compact-p (and single-body-p (notany #'trivia-p items))))
    (pprint-expression stream form 1
                       :body-separator (if compact-p :linear :newline)
                       :header-mapper (lambda (index item)
                                        (declare (ignore index))
                                        (make-role-form :role :parameters :value item)))))

(defun pprint-lisp (stream form)
  (pprint-expression stream form 2
                     :header-mapper (lambda (index item)
                                      (make-role-form :role (if (zerop index) :type :parameters)
                                                      :value item))))

(defun pprint-clause (stream form)
  (pprint-expression stream (cons nil form) 1
                     :body-separator (if (> (length (significant-items form)) 2)
                                         :newline :fill)))

(defun pprint-match (stream form)
  (pprint-expression stream form 1
                     :body-mapper (lambda (index item)
                                    (declare (ignore index))
                                    (make-role-form :role :clause :value item))))

(defun pprint-body (stream form) (pprint-expression stream form 0))
(defun pprint-when (stream form) (pprint-expression stream form 1))
(defun pprint-if (stream form)
  (pprint-expression stream form 1 :body-separator :linear))
(defun pprint-rec (stream form)
  (pprint-expression stream form 2
                     :header-mapper (lambda (index item)
                                      (if (= index 1)
                                          (make-role-form :role :bindings :value item) item))))

(macrolet ((rule (operators printer)
             `(trivial-formatter-user::set-pprint-dispatch
               '(cons (member ,@operators)) #',printer)))
  (rule (coalton:let) pprint-let)
  (rule (coalton:let* coalton:dynamic-bind) pprint-let-star)
  (rule (coalton:do) pprint-do)
  (rule (coalton:for coalton:for*) pprint-for)
  (rule (coalton:declare) pprint-declare)
  (rule (coalton:fn) pprint-fn)
  (rule (coalton:lisp) pprint-lisp)
  (rule (coalton:match) pprint-match)
  (rule (coalton:coalton coalton:progn) pprint-body)
  (rule (coalton:when coalton:unless) pprint-when)
  (rule (coalton:if) pprint-if)
  (rule (coalton:rec) pprint-rec))
