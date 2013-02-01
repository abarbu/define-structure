(module define-structure 
 (define-structure define-private-structure define-public-structure)
(import chicken scheme)
(require-extension nondeterminism)
(begin-for-syntax (require-extension srfi-1 nondeterminism))

(define-syntax define-structure-with-visibility
 (er-macro-transformer
  (lambda (exp rename compare)
   (unless (> (length exp) 2)
    (error "Bad define-structure, expects a type name and arguments"
     exp))
   (let* ((type-name (caddr exp))
          (fields (cdddr exp))
          (field-names (map (lambda (f)
                             (if (pair? f)
                                 (car f)
                                 f)) fields))
          (make (string->symbol (conc "make-" type-name)))
          (make-keywords (string->symbol (conc "make-keywords-" type-name)))
          (copy (string->symbol (conc "update-" type-name)))
          (copyp (string->symbol (conc "updatep-" type-name)))
          (set  (string->symbol (conc "set-" type-name "!")))
          (setp  (string->symbol (conc "setp-" type-name "!")))
          (predicate (string->symbol (conc type-name "?")))
          (to-alist (string->symbol (conc type-name "->alist")))
          (from-alist (string->symbol (conc "alist->" type-name)))
          (%begin (rename 'begin))
          (%define-record (rename 'define-record))
          (%define-reader-ctor (rename 'define-reader-ctor))
          (%define-record-printer (rename 'define-record-printer))
          (%define (rename 'define))
          (%lambda (rename 'lambda))
          (%list (rename 'list))
          (%cons (rename 'cons))
          (%if (rename 'if))
          (%not (rename 'not))
          (%eq? (rename 'eq?))
          (%let (rename 'let))
          (%uninitialized (rename 'uninitialized))
          (%case (rename 'case))
          (%loop (rename 'loop))
          (%obj (rename 'obj))
          (%val (rename 'val))
          (%lst (rename 'lst))
          (%car (rename 'car))
          (%cdr (rename 'cdr))
          (%proc (rename 'proc))
          (%upon-failure (rename 'upon-failure))
          (%fprintf (rename 'fprintf))
          (%export (rename 'export)))
    (receive (init-fields no-init-fields)
             (partition pair? fields)
             `(,%begin
               ,(if (cadr exp)
                    `(,%export
                      ;; define-record
                      ,make
                      ;; ours
                      ,make-keywords
                      ,set
                      ,setp
                      ,copy
                      ,copyp
                      ,to-alist
                      ,from-alist
                      ,predicate
                      ,@(join 
                         (map (lambda (f)
                               (list ;; define-record
                                (string->symbol (conc type-name "-" f))
                                (string->symbol (conc  type-name "-" f "-set!"))
                                ;; ours
                                (string->symbol (conc "set-" type-name "-" f "!"))
                                (string->symbol (conc type-name "-" f "-setp!"))
                                (string->symbol (conc "setp-" type-name "-" f "!"))
                                (string->symbol (conc "local-set-" type-name "-" f "!"))
                                (string->symbol (conc "local-setp-" type-name "-" f "!"))
                                (string->symbol (conc "update-" type-name "-" f))
                                (string->symbol (conc "updatep-" type-name "-" f))))
                              field-names)))
                    #f)
               (,%define-record ,type-name ,@no-init-fields ,@(map car init-fields))
               (,%define ,make-keywords
                         (,%lambda (#!key ,@fields)
                                   (,make ,@no-init-fields ,@(map car init-fields))))
               (,%define ,set
                         (,%let ((,%uninitialized (,%list 'uninitialized)))
                                (,%lambda (,%obj #!key 
                                                 ,@(map (lambda (f)
                                                         (list f %uninitialized))
                                                        field-names))
                                          ,@(map
                                             (lambda (f)
                                              `(,%if (,%not (,%eq? ,f ,%uninitialized))
                                                     (,(string->symbol (conc type-name "-" f "-set!"))
                                                      ,%obj ,f)))
                                             field-names)
                                          ,%obj)))
               (,%define ,setp
                         (,%let ((,%uninitialized (,%list 'uninitialized)))
                                (,%lambda (,%obj #!key 
                                                 ,@(map (lambda (f)
                                                         (list f %uninitialized))
                                                        field-names))
                                          ,@(map
                                             (lambda (f)
                                              `(,%if (,%not (,%eq? ,f ,%uninitialized))
                                                     (,(string->symbol (conc "setp-" type-name "-" f "!"))
                                                      ,%obj ,f)))
                                             field-names)
                                          ,%obj)))
               (,%define ,copy
                         (,%let ((,%uninitialized (,%list 'uninitialized)))
                                (,%lambda (old #!key ,@(map (lambda (f)
                                                             (list f %uninitialized))
                                                            field-names))
                                          (let ((new (,make-keywords ,@(fold (lambda (f rest)
                                                                              (cons (string->keyword
                                                                                     (symbol->string f))
                                                                                    (cons %uninitialized rest)))
                                                                             '() field-names))))
                                           ,@(map
                                              (lambda (f)
                                               `(,%if (,%eq? ,f ,%uninitialized)
                                                      (,(string->symbol (conc type-name "-" f "-set!"))
                                                       new
                                                       (,(string->symbol (conc type-name "-" f)) old))
                                                      (,(string->symbol (conc type-name "-" f "-set!"))
                                                       new ,f)))
                                              field-names)
                                           new))))
               (,%define ,copyp
                         (,%let ((,%uninitialized (,%list 'uninitialized)))
                                (,%lambda (old #!key ,@(map (lambda (f)
                                                             (list f %uninitialized))
                                                            field-names))
                                          (let ((new (,make-keywords ,@(fold (lambda (f rest)
                                                                              (cons (string->keyword
                                                                                     (symbol->string f))
                                                                                    (cons %uninitialized rest)))
                                                                             '() field-names))))
                                           ,@(map
                                              (lambda (f)
                                               `(,%if (,%eq? ,f ,%uninitialized)
                                                      (,(string->symbol (conc type-name "-" f "-set!"))
                                                       new
                                                       (,(string->symbol (conc type-name "-" f)) old))
                                                      (,(string->symbol (conc "set-" type-name "-" f "!"))
                                                       new (,f (,(string->symbol (conc type-name "-" f)) old)))))
                                              field-names)
                                           new))))
               (,%define ,to-alist
                         (,%lambda (,%obj)
                                   (,%list . ,(map
                                               (lambda (f)
                                                `(,%cons
                                                  ',f
                                                  (,(string->symbol (conc type-name "-" f)) ,%obj)))
                                               field-names))))
               (,%define ,from-alist
                         (,%lambda (alist)
                                   (,%let ,%loop ((,%lst alist)
                                                  (,%obj (,make-keywords)))
                                          (,%if (,%eq? ,%lst '())
                                                ,%obj
                                                (,%case (,%car (,%car ,%lst))
                                                        ,@(map (lambda (f)
                                                                `((,f) (,(string->symbol
                                                                          (conc type-name "-" f "-set!"))
                                                                        ,%obj (,%cdr (,%car ,%lst)))
                                                                  (,%loop (,%cdr ,%lst) ,%obj)))
                                                               field-names)
                                                        ;; Unknown fields are ignored, like in the constructor
                                                        (else (,%loop (,%cdr ,%lst) ,%obj)))))))
               ,@(map
                  (lambda (f)
                   `(,%define ,(string->symbol (conc "set-" type-name "-" f "!"))
                              ,(string->symbol (conc type-name "-" f "-set!"))))
                  field-names)
               ,@(map
                  (lambda (f)
                   `(,%define 
                     ,(string->symbol (conc type-name "-" f "-setp!")) 
                     (,%lambda (,%obj ,%proc)
                               (,(string->symbol (conc "set-" type-name "-" f "!"))
                                ,%obj
                                (,%proc (,(string->symbol (conc type-name "-" f)) ,%obj))))))
                  field-names)
               ,@(map
                  (lambda (f)
                   `(,%define ,(string->symbol (conc "setp-" type-name "-" f "!"))
                              ,(string->symbol (conc type-name "-" f "-setp!"))))
                  field-names)
               ,@(map
                  (lambda (f)
                   `(,%define
                     ,(string->symbol (conc "local-set-" type-name "-" f "!"))
                     (,%lambda (,%obj ,%val)
                               (let ((p (,(string->symbol (conc type-name "-" f)) ,%obj)))
                                (,%upon-failure (,(string->symbol (conc "set-" type-name "-" f "!")) ,%obj p))
                                (,(string->symbol (conc "set-" type-name "-" f "!")) ,%obj ,%val)))))
                  field-names)
               ,@(map
                  (lambda (f)
                   `(,%define
                     ,(string->symbol (conc "local-setp-" type-name "-" f "!"))
                     (,%lambda (,%obj ,%proc)
                               (let ((p (,(string->symbol (conc type-name "-" f)) ,%obj)))
                                (,%upon-failure (,(string->symbol (conc "set-" type-name "-" f "!")) ,%obj p))
                                (,(string->symbol (conc "setp-" type-name "-" f "!")) ,%obj ,%proc)))))
                  field-names)
               ,@(map
                  (lambda (f)
                   `(,%define ,(string->symbol (conc "update-" type-name "-" f))
                              (,%lambda (,%obj ,%val)
                                        (,copy ,%obj ,(string->keyword (conc f)) ,%val))))
                  field-names)
               ,@(map
                  (lambda (f)
                   `(,%define ,(string->symbol (conc "updatep-" type-name "-" f))
                              (,%lambda (,%obj ,%proc)
                                        (,copyp ,%obj ,(string->keyword (conc f)) ,%proc))))
                  field-names)
               (,%define-reader-ctor ',type-name ,make)
               (,%define-record-printer
                (,type-name ,%obj ,%val)
                (,%fprintf ,%val 
                           ,(conc "#,("
                                  type-name
                                  (apply conc (map (lambda (f) " ~S") field-names))
                                  ")")
                           ,@(map
                              (lambda (f)
                               `(,(string->symbol (conc type-name "-" f)) ,%obj))
                              field-names)))))))))

(define-syntax define-private-structure
 (syntax-rules ()
  ((_ body ...) (define-structure-with-visibility #f body ...))))

(define-syntax define-public-structure
 (syntax-rules ()
  ((_ body ...) (define-structure-with-visibility #t body ...))))

;; structures are public by default
(define-syntax define-structure
 (syntax-rules ()
  ((_ body ...) (define-structure-with-visibility #t body ...))))

)
