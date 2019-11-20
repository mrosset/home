(define-module (home generics)
  #:use-module (home path)
  #:use-module (home file)
  #:use-module (home doc-here)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:duplicates (merge-generics)
  #:export (copy ensure exists?))

(define-method (equal? (a <path>) (b <path>))
  (if (and (string= (path-name a) (path-name b))
           (eq? (paths a) (paths b))
           (eq? (mode a) (mode b))
           (eq? (type a) (type b)))
      #t
      #f))

(define-method (equal? (a <file>) (b <file>))
  (if (and (string= (file-hash a) (file-hash b))
           (next-method))
      #t
      #f))

(define-method (slots-equal? (a <file>) (b <file>))
  (let ((slots (class-slots (class-of a))))
    (for-each (lambda (slot)
                 (format #t "SLOT: ~a =? ~a\n" (slot-definition-name slot)
                        (equal? ((slot-definition-accessor slot) a)
                                ((slot-definition-accessor slot) b))))
               (filter (lambda (x) (and
                                    (not (equal? 'input (slot-definition-name x)))
                                    (not (equal? 'content (slot-definition-name x))))) slots))
    ))

(define-method (file= (a <file>) (b <file>))
  (if (and (string= (file-hash a) (file-hash b))
           (sum= a b)
           (string= (path-name a) (path-name b))
           (eq? (paths a) (paths b))
           (equal? (mode a) (mode b))
           (check-sum? a)
           (check-sum? b))
      #t
      #f))

(define-method (exists? (self <file>))
  (file-exists? (path-name self)))

(define-method (exists? (self <doc-here>))
  #t)

(define-method (copy (in <doc-here>) (out <path>))
  (let* ((tmp #f)
         (tmp-file (shallow-clone out)))
    (dynamic-wind
      (lambda _
        ;; FIXME: don't assume TMPDIR is /tmp
        (set! tmp (mkstemp! (string-copy "/tmp/home-XXXXXX")))
        (set! (path tmp-file) (string-copy (port-filename tmp))))
      (lambda _
        (display (content in) tmp)
        (close-port tmp)
        (chmod (path-name tmp-file) (mode tmp-file))
        ;; check the new file matches the expected output file
        (let ((disk (disk->file (path-name tmp-file))))
          (unless (file= disk tmp-file)
            (map describe (list disk tmp-file))
            (error "expect file: ~a got built file: ~a" (file-hash tmp-file) (sum tmp-file))))
        (rename-file (path-name tmp-file) (path-name out)))
      (lambda _
        (when (exists? tmp-file) (delete-file (path-name tmp-file)))))))

(define-method (ensure (self <path>) dry-run?)
  (let ((do-copy? (or (not (exists? self))
                      (not (file= (disk->file (path-name self)) self))
                      )))
    (if (and do-copy? (not dry-run?))
        (begin (copy (input self) self) #t)
        #f)))
