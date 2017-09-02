;;; 
;;; Copyright © 2017 Didid Junaedi
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.
;;;

#+abcl
(error (format nil "Unable to run this program using ABCL. Try use SBCL." ))

#-abcl
(require "asdf")

(load "texutils.lisp")
(defvar *dbfields-table* (make-hash-table :test 'equalp))

#-abcl
(setf asdf:*central-registry* '(  #P"~/_lisp/asdf/clsql/"))
#-abcl
(require 'clsql)
#-abcl
(clsql:connect '("tpresume.db") :database-type :sqlite3)
#-abcl
(unless (clsql:list-tables)
    (format t "create new table~%")
    (clsql:create-table
                    'tppsonal '(
                            (name (string 30))
                            (nationality (string 30))
                            (address1 (string 30))
                            (address2 (string 30))))
    (clsql:insert-records 
                    :into 'tppsonal
                    :attributes '(name nationality address1 address2)
                    :values '(
                        "John Smith" 
                        "British" 
                        "123 Broadway,"
                        "City, 12345"))
)

(tplmac-class-0001 tex-cv-sql-query-base (block-begin block-end)
    (let* (
            (blk-cntnt (slot-value vtex-elm 'block-content))
            (rslt (apply #'clsql:select
                    (append 
                        (map 'list (lambda (x) (intern x)) (cdr blk-cntnt))
                        (list 
                            :from (car blk-cntnt) 
                            :limit 1 
                            :field-names t)))))
        (loop for nm in (cdr blk-cntnt) for val in (car rslt)
            do (setf (gethash nm *dbfields-table*) val))
    )
)

(tplmac-constru-0001 
    cv-el-sql-query sql-query tex-cv-sql-query-base 
    ((block-begin "\\sql-query{") (block-end "}")))

(tplmac-class-0001 tex-cv-dbfield-base (block-begin block-end)
    (dolist (blk-itm (slot-value vtex-elm 'block-content)) 
        (when blk-itm (format t "~a" (gethash blk-itm *dbfields-table*))))
)

(tplmac-constru-0001 
    cv-el-dbfield dbfield tex-cv-dbfield-base 
    ((block-begin "\\dbfield{") (block-end "}")))



(defclass tex-fun-wrapper ()
    (
        (fun-name  :initarg :fun-name)
        (fun-args  :initarg :fun-args)
    )
)

(defmethod write-elm ((fn-wrpper tex-fun-wrapper))
    (let (
            (fn-nm (slot-value fn-wrpper 'fun-name))
            (fn-args (slot-value fn-wrpper 'fun-args)))
        ;(format t "applying ~a~%" fn-nm)
        (write-elm (apply fn-nm fn-args))
    )
)

(defun codify (elm-lst)
    (let ((res nil))
        (dolist (elm-itm elm-lst)
            (cond
                ((or
                    (typep elm-itm 'string)
                    (typep elm-itm 'integer)
                    (null elm-itm)
                    )
                        (push elm-itm res))
                ((typep elm-itm 'list)
                    (cond
                        ((typep (car elm-itm) 'symbol)
                            (push (wrapfun elm-itm) res))
                        (t (push (codify elm-itm) res))
                        ))
                (t 
                    (format *error-output* "~a~%" (type-of elm-itm))
                    (error "invalid element type"))
            )
        )
        (reverse res)
    )
)


(let ((tag-fun-ht *tag-fun-table*))
    (loop for (key value) in
        '(
            (end-of-line cv-el-end-of-line)
            (section-styl-0001 cv-el-section-styl-0001)
            (itemize-styl-0001 cv-el-itemize-styl-0001)
            (paragraph-styl-0001 cv-el-paragraph-styl-0001)
            (paragraph-styl-0002 cv-el-paragraph-styl-0002)
            (format-ordinal-styl-0001 cv-format-ordinal-styl-0001)
            (href-http cv-el-href-http)
            (href-mail cv-el-href-mail)
            (section-periodic-item-styl-0001 cv-el-section-periodic-item-styl-0001)
            (section-periodic-item-styl-0002 cv-el-section-periodic-item-styl-0002)
            (section-group-item-styl-0001 cv-el-section-group-item-styl-0001)
            (section-styl-0003 cv-el-section-styl-0003)
            (section-group-item-styl-0002 cv-el-section-group-item-styl-0002)
            (section-styl-0004 cv-el-section-styl-0004)
            (parbox-styl-0001 cv-el-parbox-styl-0001)
            (section-group-item-styl-0003cv-el-section-group-item-styl-0003)
            (section-styl-0005 cv-el-section-styl-0005)
        )
        do (setf (gethash key tag-fun-ht) value)
    )
    (defun wrapfun (elm-lst)
        (let* (
                (tg (car elm-lst))
                (fn-args (codify (cdr elm-lst)))
                (fn-nm (gethash tg tag-fun-ht)))
            (cond
                (fn-nm
                    (make-instance 'tex-fun-wrapper
                                :fun-name fn-nm
                                :fun-args fn-args
                    ))
                (t (error (format nil "invalid tag: ~a." tg)))
            )
        )
    )
)


(let ((infl-arg nil) (otfl-arg nil) (lastky-arg nil)
            #+abcl (command-line-args *command-line-argument-list*)
            #+sbcl (command-line-args *posix-argv*)
        )
    (dolist (arg-itm command-line-args)
        (cond
            ((equalp (subseq arg-itm 0 1) "-")
                (setq lastky-arg arg-itm))
            ((equalp lastky-arg "-i")
                (setq infl-arg arg-itm)
                (setq lastky-arg nil))
            ((equalp lastky-arg "-o")
                (setq otfl-arg arg-itm)
                (setq lastky-arg nil))
            (t (setq lastky-arg nil))
        )
    )
    (let ((spc-ls nil) (tx-fl nil))
        (cond
            (infl-arg 
                (with-open-file (in infl-arg)
                    (setq spc-ls (read in)))
            )
            (t (setq spc-ls (read)))
        )
        (setq tx-fl (apply #'make-tex-file (codify spc-ls)))
        (cond
            (otfl-arg (with-open-file
                    (*standard-output* otfl-arg :direction :output)
                (write-elm tx-fl)))
            (t (write-elm tx-fl))
        )
    )
)

#-abcl
(clsql:disconnect)

(exit)
