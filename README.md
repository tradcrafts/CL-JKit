<!--dd -*- coding: utf-8 -*- -->  

SBCL. CCL, CMUCL

-- *roswell* --

$ ros install tradcrafts/cl-jkit


-- *git* --

$ cd quicklisp/localprojects

$ git clone https://github.com/tradcrafts/cl-jkit

CL> (ql:register-local-projects)


-- *load* --

CL> (ql:quickload :jkit)
