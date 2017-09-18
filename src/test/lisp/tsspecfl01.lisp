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

(load "texutils.lisp")


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
                    ;(format *error-output* "~a: ~a~%" (type-of elm-itm))
                    (error (format *error-output* "invalid element type: ~a: ~a~%" (type-of elm-itm) elm-itm)))
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

(exit)
