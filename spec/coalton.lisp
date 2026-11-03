(defpackage #:trivial-formatter/coalton/fixtures
  (:use #:coalton #:coalton-prelude))

(defpackage #:trivial-formatter/coalton/tests
  (:use #:cl)
  (:export #:run-tests))

(in-package #:trivial-formatter/coalton/tests)

(defvar *checks* 0)
(defvar *reader-evaluations* 0)

(defun check (condition message &rest arguments)
  (incf *checks*)
  (unless condition (error (apply #'format nil message arguments))))

(defun format-source (source &optional (width 80) (readtable :standard))
  (let ((*readtable* (named-readtables:copy-named-readtable readtable))
        (*package* (find-package '#:trivial-formatter/coalton/fixtures))
        (*print-right-margin* width))
    (trivial-formatter:call-with-file-formatting-context
     #P"input.ct"
     (lambda ()
       (with-input-from-string (input source)
         (unwind-protect
              (with-output-to-string (output)
                (trivial-formatter:print-as-code
                 (trivial-formatter:read-as-code input) output))
           (trivial-formatter::cleanup-brokens)))))))

(defun read-source (source)
  (let ((*readtable* (copy-readtable nil))
        (*package* (find-package '#:trivial-formatter/coalton/fixtures))
        (*read-eval* nil))
    ;; Use the compiler's native sugar readers, with ordinary parentheses, to
    ;; compare semantic forms independently of the formatter's syntax objects.
    (set-macro-character #\[ #'coalton-impl/reader::read-cl-bracket-form)
    (set-macro-character #\] #'coalton-impl/reader::read-cl-close-bracket)
    (set-macro-character #\ƒ #'coalton-impl/reader::read-cl-short-lambda-form)
    (labels ((canonical (value)
               (typecase value
                 (cons (cons (canonical (car value)) (canonical (cdr value))))
                 (symbol (if (symbol-package value) value
                             (list :uninterned (symbol-name value))))
                 (structure-object
                  ;; Native backquote readers use implementation-specific comma
                  ;; objects. Compare their readable spelling, not identity.
                  (let ((*print-pretty* nil) (*print-circle* t))
                    (prin1-to-string value)))
                 (t value))))
      (canonical (read-from-string source)))))

(defun fixture (name)
  (uiop:read-file-string
   (asdf:system-relative-pathname
    :trivial-formatter (format nil "spec/fixtures/~A.ct" name))))

(defun check-roundtrip (source)
  (dolist (width '(40 80 100))
    (let ((output (format-source source width)))
      (check (equal (read-source source) (read-source output))
             "Formatting changed source structure at width ~D:~%~A" width source)
      (check (equal output (format-source output width))
             "Formatting was not idempotent at width ~D:~%~A" width source)
      (check (equal output (format-source source width 'coalton:coalton))
             "Definition spacing changed with the ambient readtable:~%~A" source))))

(defun check-layouts ()
  (let ((input (fixture "coalton-spacing-input"))
        (expected (string-right-trim '(#\Newline #\Return)
                                     (fixture "coalton-spacing-expected"))))
    (check (equal expected (format-source input))
           "Definition spacing snapshot differs.~%Expected:~%~A~%Actual:~%~A"
           expected (format-source input))
    (check-roundtrip input))
  (dolist (source
           (list
            "(coalton-toplevel)"
            "(coalton-toplevel (define first 1) (define second 2))"
            "(coalton-toplevel (define-type-alias Count Integer) (define-struct Counter (value Integer)) (define-class (Bump :a) (bump (:a -> :a))) (define-instance (Bump Integer) (define (bump x) (+ x 1))) (define-exception Failure (Failed String)) (define-resumption Stop StopNow))"
            "(coalton-toplevel (repr :enum) (derive Show) (define-type Status Ready Busy) (monomorphize) (declare initial Integer) (define initial 0))"
            "(coalton-toplevel (declare first Integer) (declare second Integer) (define first 1) (define second 2))"
            "(eval-when (:load-toplevel) (coalton-toplevel (define first 1) (define second 2)))"
            (format nil "(coalton-toplevel (define first 1)~% ; leading single comment~% (declare second Integer) (define second 2))")
            (format nil "(coalton-toplevel (define first 1) ;; inline double comment~% (define second 2))")
            (format nil "(coalton-toplevel (define first 1)~% ;; trailing documentation~% )")
            (format nil "(coalton-toplevel ;; 한국어 설명~% (define first 1) ; 끝 주석~% ;; 다음 정의~% (define second 2))")
            (format nil "(coalton-toplevel (define first 1) #| second documentation |# (define second 2))")
            (format nil "(coalton-toplevel (define caption \"line one~%~%line three\") (define initial 0))")
            (format nil "(coalton-toplevel (define (next x) ;; function documentation~% (+ x 1)) (define initial 0))")))
    (check-roundtrip source))
  ;; These are separate requirements: following comments and declarations must
  ;; not accidentally remain attached to the preceding definition.
  (let ((output (format-source
                 (format nil "(coalton-toplevel (define first 1)~% ; next definition~% (declare second Integer) (define second 2))"))))
    (check (search (format nil "~%~%  ; next definition~%  (declare") output)
           "A leading single-semicolon comment moved onto the previous definition.")
    (check (search (format nil "(declare second integer)~%  (define second") output)
           "A declaration was separated from its matching definition."))
  (let ((output (format-source
                 (format nil "(coalton-toplevel (define first 1) ;; inline~% (define second 2))"))))
    (check (search (format nil "1) ;; inline~%~%  (define second") output)
           "An inline double-semicolon comment moved to the next definition."))
  (let ((output (format-source
                 "(coalton-toplevel (define-type A A1) (define-type-alias B Integer) (define-struct C (x Integer)) (define-class (D :a) (d (:a -> :a))) (define-instance (D Integer) (define (d x) x)))")))
    (check (= 4 (count "" (uiop:split-string output :separator '(#\Newline))
                       :test #'equal))
           "Definition families did not receive exactly one blank line between groups.")))

(defun check-fn-layouts ()
  (dolist (source '("(fn (x) x)" "(fn (x)             x)"))
    (check (equal "(fn (x) x)" (format-source source))
           "A short fn was not compacted: ~A" source))
  (check (equal "(fn (x) x)"
                (format-source (format nil "(fn (x)~%             x)")))
         "An existing multiline identity fn did not become a single line.")
  (check (equal "(fn () 1)" (format-source "(fn ()             1)"))
         "An empty fn parameter list or its whitespace was not preserved.")
  (let ((output (format-source
                 "(coalton [(fn (x)             x) (fn (y)             (+ y 1))])")))
    (check (equal output
                   (format nil "(coalton~%  [(fn (x) x) (fn (y) (+ y 1))])"))
           "Functions inside a collection did not normalize:~%~A" output))
  (let ((output (format-source
                 "(fn (x) (combine-very-long-symbol-name x x x x x x))" 40)))
    (check (uiop:string-prefix-p (format nil "(fn (x)~%  (combine-") output)
           "A long fn body was not indented two spaces:~%~A" output))
  (let* ((output (format-source
                  "(coalton [(fn (x) (combine-very-long-symbol-name x x x x x x))])" 40))
         (lines (uiop:split-string output :separator '(#\Newline)))
         (header (find-if (lambda (line) (search "(fn " line)) lines))
         (body (find-if (lambda (line) (search "(combine-" line)) lines)))
    (check (and header body
                (= (+ 2 (search "(fn " header)) (search "(combine-" body)))
           "A nested long fn body drifted from its opening parenthesis:~%~A" output))
  (check (equal (format nil "(fn (x)~%  (let y = (+ x 1))~%  (+ y 2))")
                (format-source "(fn (x) (let y = (+ x 1)) (+ y 2))"))
         "Multiple fn body expressions lost their multiline layout.")
  (dolist (source
           (list "(fn (x)             x)"
                 "(coalton [(fn (x)             x) (fn (y)             (+ y 1))])"
                 "(coalton [(fn (x)             x) (fn (x)             (+ x 1))])"
                 "(fn (x) (combine-very-long-symbol-name x x x x x x))"
                 "(coalton [(fn (x) (combine-very-long-symbol-name x x x x x x))])"
                 "(fn (x) (let y = (+ x 1)) (+ y 2))"
                 "(fn (x) \"keep             spaces\")"
                 (format nil "(fn (x) ;; inline~% x)")
                 (format nil "(fn (x)~% ;; documentation~% x)")
                 (format nil "(fn (x ;; parameter comment~% y) (+ x y))")))
    (check-roundtrip source)))

(defun check-file-api ()
  (uiop:with-temporary-file (:pathname target :stream out :type "ct")
    (write-line "(in-package #:trivial-formatter/coalton/fixtures)" out)
    (write-line ";; 파일 단위 포맷 검사" out)
    (write-string (fixture "coalton-spacing-input") out)
    (terpri out)
    (write-string (fixture "coalton-syntax-input") out)
    (close out)
    (uiop:with-temporary-file (:pathname sibling :stream sibling-out :type "ct")
      (write-string "UNRELATED FILE MUST STAY UNCHANGED" sibling-out)
      (close sibling-out)
      (let* ((original (uiop:read-file-string target))
             (*package* (find-package '#:cl-user))
             (*readtable* (copy-readtable nil))
             (*print-right-margin* 80)
             (preview (with-output-to-string (*standard-output*)
                        (trivial-formatter:fmt-file target))))
        (check (equal original (uiop:read-file-string target))
               "A preview changed the target file.")
        (check (eq *package* (find-package '#:cl-user))
               "Formatting leaked the source file's package into the caller.")
        (check (search ";; 파일 단위 포맷 검사" preview)
               "Formatting lost a Unicode comment from the selected file.")
        (trivial-formatter:fmt-file (namestring target) :supersede)
        (check (equal preview (uiop:read-file-string target))
               "Saved output differs from the preview.")
        (check (equal "UNRELATED FILE MUST STAY UNCHANGED"
                       (uiop:read-file-string sibling))
               "Formatting a selected file changed a sibling file.")
        (trivial-formatter:fmt-file target :supersede)
        (check (equal preview (uiop:read-file-string target))
               "Saving the selected file was not idempotent.")
        (with-open-file (stream target :direction :output :if-exists :supersede)
          (write-string "(cl:progn #.(cl:incf trivial-formatter/coalton/tests::*reader-evaluations*))"
                        stream))
        (let ((*reader-evaluations* 0))
          (with-output-to-string (*standard-output*)
            (trivial-formatter:fmt-file target))
          (check (zerop *reader-evaluations*)
                 "Formatting executed a read-time evaluation from the source file."))
        (let ((malformed "(coalton-toplevel (define broken 1)"))
          (with-open-file (stream target :direction :output :if-exists :supersede)
            (write-string malformed stream))
          (check (handler-case (progn (trivial-formatter:fmt-file target :supersede) nil)
                   (error () t))
                 "Malformed source unexpectedly formatted successfully.")
          (check (equal malformed (uiop:read-file-string target))
                 "A failed formatting operation changed the input file."))))))

(defun check-coalton-syntax ()
  (check (equal (string-right-trim '(#\Newline #\Return)
                                    (fixture "coalton-syntax-expected"))
                (format-source (fixture "coalton-syntax-input")))
         "Coalton syntax formatting snapshot differs.")
  (dolist (source
           (list
            (fixture "coalton-syntax-input")
            "(coalton (length []))"
            "(coalton (length [1 2 3]))"
            "(coalton (length [[1 2] [3 4]]))"
            "(coalton (length [=>]))"
            "(coalton (length [1 => 10 2 => 20]))"
            "(coalton (progn (let x = 1) (let (Tuple a b) = (Tuple x 2)) (+ a b)))"
            "(coalton (let ((declare x Integer) (x 1)) x))"
            "(coalton (let* ((x 1) (y (+ x 1))) y))"
            "(coalton (for* :scan ((declare i UFix) (i 0 (1+ i))) :returns i :repeat 10 (values)))"
            "(coalton (for :outer () :repeat 10 (for () :repeat 2 (break :outer))))"
            "(coalton (do ((Tuple first second) <- (fetch-two-long-results 100 200)) (pure (+ first second))))"
            "(coalton (map ƒx.(+ x 1) [1 2 3]))"
            "(coalton (map ƒx.x [1 2 3]))"
            "(coalton (map ƒx.ƒy.(+ x y) [1 2 3]))"
            "(coalton (map ƒX.x [1 2 3]))"
            "(coalton (map ƒα.α [1 2 3]))"
            "(coalton ƒ_x_.x)"
            "(coalton ƒ.1)"
            "(coalton (fn () 1))"
            "(coalton (fn (x &key (offset 1)) (+ x offset)))"
            "(coalton (match x ((Some y) (let z = (+ y 1)) z) ((None) 0)))"
            "(coalton (rec again ((counter 10) (total 0)) (if (== counter 0) total (again (1- counter) (+ total counter)))))"
            "(coalton-toplevel (declare combine ((Eq :a) => :a * :a * List :a -> Result String (List :a))))"
            (format nil "(coalton-toplevel (define evens [x :for ; binder comment~%x :below 10 :when (even? x)]))")
            (format nil "(coalton-toplevel (define squares [x => (* x x) :with limit = 10 :for x :below limit :when (even? x)]))")
            (format nil "(coalton (map ƒx.; body comment~%(+ x 1) [1 2 3]))")
            (format nil "(coalton (length [1 ; first item~% 2 3]))")
            (format nil "(coalton-toplevel (define pairs [1 ; key~% => 10 2 => 20]))")
            "(coalton (lisp (-> Integer) () (cl:length \"[] and ƒx.x\")))"
            "(coalton (lisp (-> Char) () #\\[))"
            "(define-expression-macro sum-range ((index from until) term) (cl:let ((start (cl:gensym \"start\"))) `(progn (let ,start = ,from) (for ((,index ,start (1+ ,index))) :until (>= ,index ,until) ,term))))"
            (format nil "(coalton (progn (let x ; name comment~%= 1) x))")))
    (check-roundtrip source))
  (let ((output (format-source "(coalton (length [1 2 3]))")))
    (check (search "[1 2 3]" output) "Collection notation was lost.")
    (check (not (search "%collection-" output)) "Reader expansion leaked into source."))
  (let ((output (format-source "(coalton (progn (let x = 1) x))")))
    (check (search "(let x = 1)" output) "A short let was split unnecessarily."))
  (let ((output (format-source "(coalton (map ƒx.(+ x 1) []))")))
    (check (search "ƒx.(+ x 1)" output) "Short lambda notation was lost."))
  (let ((output (format-source "(coalton (for :scan () :repeat 10 (values)))")))
    (check (search "(for :scan ()" output) "A loop label or empty binding list was lost.")
    (check (search (format nil "~%    :repeat 10~%") output)
           "Loop clauses were not separated from the body."))
  (dolist (source '("(coalton (length [1 2))" "(coalton (length ]))"
                    "(coalton ƒxx.x)" "(coalton ƒ1.x)"))
    (check (handler-case (progn (format-source source) nil) (error () t))
           "Malformed reader syntax unexpectedly formatted: ~A" source)))

(defun check-compilation ()
  (uiop:with-temporary-file (:pathname source :stream out :type "ct")
    (write-line "(in-package #:trivial-formatter/coalton/fixtures)" out)
    (write-line "(named-readtables:in-readtable coalton)" out)
    (write-string (format-source (fixture "coalton-spacing-input")) out)
    (terpri out)
    (write-string (format-source (fixture "coalton-syntax-input")) out)
    (close out)
    (uiop:with-temporary-file (:pathname fasl :stream fasl-stream
                              :type (pathname-type (compile-file-pathname source)))
      (close fasl-stream)
      (let ((*compile-verbose* nil) (*compile-print* nil))
        (multiple-value-bind (output warnings failure)
            (compile-file source :output-file fasl)
          (declare (ignore warnings))
          (check (and output (not failure)) "Formatted Coalton failed to compile.")
          (load output)
          (check (= 42 (funcall (fdefinition
                                (find-symbol "NEXT" '#:trivial-formatter/coalton/fixtures))
                               41))
                 "Formatted code returned the wrong result.")
          (dolist (example '(("ADD-SAMPLES" 1 9) ("SUM-EVEN-SQUARES" 10 125)
                             ("PAIR-TOTAL" 0 30) ("FOR-SUM" 5 10)
                             ("TUPLE-EXAMPLE" 40 42) ("CL-NEXT" 41 42)))
            (destructuring-bind (name input expected) example
              (check (= expected
                        (funcall (fdefinition
                                  (find-symbol name '#:trivial-formatter/coalton/fixtures))
                                 input))
                     "Formatted ~A returned the wrong result." name))))))))

(defun run-tests ()
  (let ((*checks* 0))
    (check-layouts)
    (check-fn-layouts)
    (check-coalton-syntax)
    (check-file-api)
    (check-compilation)
    (format t "~&PASS: ~D Coalton formatting checks.~%" *checks*)
    t))
