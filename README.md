<!--dd -*- coding: utf-8 -*- -->  

CLPGKから実験的に分割移譲されたものです。
本リポジトリは凍結し、今後の開発は
https://lisp3dev/lisp3dev/
で行います。

# CL-JKit

SBCL. CCL, CMUCL

-- **roswell** --

$ ros install tradcrafts/cl-jkit

-- **git** --

$ cd quicklisp/local-projects

$ git clone https://github.com/tradcrafts/cl-jkit

CL> (ql:register-local-projects)


-- **load** --

CL> (ql:quickload :jkit)
