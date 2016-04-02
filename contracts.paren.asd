
(asdf:defsystem :contracts.paren
  :name "contracts.paren"
  :description "Contracts for parenscript (javascript), inspired by Racket contracts."
  :version "0.0.3"
  :author "Crackbot <thecrackbot@gmail.com>"
  :maintainer "Crackbot <thecrackbot@gmail.com>"
  :license "The MIT License (MIT)"
  :components ((:static-file "contracts.paren.asd")
               (:module "src"
                        :components ((:file "package")
                                     (:file "combinators")
                                     (:file "runtime")
                                     (:file "api")
                                     (:file "contracts"))))
  :depends-on (:parenscript :iterate :serve.paren :mgl-pax :lisp-unit))
