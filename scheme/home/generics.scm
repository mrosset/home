(define-module (home generics)
  #:use-module (home path)
  #:use-module (home dir)
  #:use-module (home file)
  #:use-module (home doc-here)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:duplicates (merge-generics))

(define-method (equal? (a <path>) (b <path>))
  (if (and (string= (path-name a) (path-name b))
           (eq? (mode a) (mode b))
           (eq? (type a) (type b)))
      #t
      #f))

(define-method (path= (a <string>) (b <path>))
  (string= a (path-name b)))

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

(define-method (file= (a <path>) (b <path>))
  (if (and (string= (file-hash a) (file-hash b))
           (sum= a b)
           (string= (path-name a) (path-name b))
           ;; (eq? (entries a) (entries b))
           (equal? (mode a) (mode b))
           (check-sum? a)
           (check-sum? b))
      #t
      #f))

(define-method (build (self <doc-here>))
  (if (exists? self)
      (begin
        (let ((disk (disk->file (path-name self))))
                  (unless (file= disk self)
            (remove self)
            (error "expect hash: ~a" (file-hash self)))
          )
        (path-name self))
   (let* ((tmp #f)
          (tmp-file (shallow-clone self)))
     (dynamic-wind
       (lambda _
         ;; FIXME: don't assume TMPDIR is /tmp
         (set! tmp (mkstemp! (string-copy "/tmp/home-XXXXXX")))
         (set! (path tmp-file) (string-copy (port-filename tmp))))
       (lambda _
         (display (content self) tmp)
         (close-port tmp)
         (chmod (path-name tmp-file) (mode tmp-file))
         ;; check the new file matches the expected output file
         (let ((disk (disk->file (path-name tmp-file))))
           (unless (file= disk tmp-file)
             (map describe (list disk tmp-file))
             (error "expect file: ~a got file: ~a" (file-hash tmp-file) (sum tmp-file))))
         (copy-file (path-name tmp-file) (path-name self))
         )
       (lambda _
         (when (exists? tmp-file)
           (delete-file (path-name tmp-file)))
         ))
     (path-name self))))

(define-method (build (self <file>))
  (let* ((in-file (build (input self)))
         (in (input self))
         (disk (disk->file in-file))
         )
    (unless (sum= self in)
      (map describe (list disk self))
      (error "expect file: ~a got file: ~a" (file-hash self) (sum in)))
    (copy-file (path-name in) (path-name self))
    (path-name self)))
