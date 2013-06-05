(define modules '(kernel))

(define-task clean ()
  (sake:default-clean))

(define-task compile ()
  (for-each (lambda (m) (sake:compile-c-to-o (sake:compile-to-c m)))
            modules))

(define-task install ()
  (for-each sake:install-compiled-module modules)
  (sake:install-sphere-to-system))

(define-task uninstall ()
  (sake:uninstall-sphere-from-system))

(define-task all (compile install)
  'all)
