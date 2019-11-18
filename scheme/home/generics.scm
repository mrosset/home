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

(define-method (exists? (self <file>))
  (file-exists? (path-name self)))

(define-method (exists? (self <doc-here>))
  #t)

(define-method (copy (self <doc-here>) (output <path>))
  (call-with-output-file (path-name output)
     (lambda (port)
      (display (content self) port)
      (close-port port))))

(define-method (ensure (self <path>) dry-run?)
  (unless dry-run?
    (copy (input self) self)))
