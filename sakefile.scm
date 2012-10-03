(include "~~spheres/prelude#.scm")
(%include sake: utils#)

(define modules '(kernel))

(define-task clean ()
  (sake:default-clean))

(define-task compile ()
  (for-each (lambda (m)
              (sake:compile-c-file (sake:generate-c-file m))
              (sake:compile-c-file (sake:generate-c-file
                                    m
                                    version: '(debug)
                                    compiler-options: '(debug))))
            modules))

(define-task install ()
  (for-each (lambda (m)
              (sake:install-compiled-module m)
              (sake:install-compiled-module m version: '(debug)))
            modules)
  (sake:install-system-sphere))

(define-task uninstall ()
  (sake:uninstall-system-sphere))

(define-task all (compile install)
  'all)
