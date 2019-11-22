(define-module (home generics)
  #:use-module (home path)
  #:use-module (home dir)
  #:use-module (home file)
  #:use-module (home doc-here)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:duplicates (merge-generics)
  #:export (disk->file file= build ensure))

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


(define-method (disk->file (path <string>))
  (let* ((fi (stat path))
         (file (make (case (stat:type fi)
                       ('regular <file>)
                       ('directory <dir>)
                       (else (error "can't determine file class")))
                 #:path path
                 #:type (stat:type fi)
                 #:mode (stat:perms fi))))
    (when (is-a? file <file>)
      (set! (file-hash file) (sum file)))
    file))

(define-method (ensure (self <dir>) dry-run?)
  (let ((do-build?  (not (exists? self))))
    (if (and do-build? (not dry-run?))
        (begin (build self) #t)
        #f))
  (when (entries self)
    (for-each (lambda (x)
                (ensure x dry-run?))
              (entries self))))
(define-method (ensure (self <file>) dry-run?)
  (let ((do-build? (or (not (exists? self))
                      (not (file= (disk->file (path-name self)) self))
                      )))
    (if (and do-build? (not dry-run?))
        (begin (build (input self) self) #t)
        #f)))


(define-method (build (self <dir>))
  (mkdir (path-name self)) (mode self)
  (unless (path= (disk->file (path-name self)) self)
    (map describe (list (disk->file (path-name self)) self))
    (error "build: <dir> is not valid"))
  )

(define-method (build (in <doc-here>) (out <path>))
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
            (error "expect file: ~a got file: ~a" (file-hash tmp-file) (sum tmp-file))))
        (rename-file (path-name tmp-file) (path-name out)))
      (lambda _
        (when (exists? tmp-file) (delete-file (path-name tmp-file))))))
  (sum out))
