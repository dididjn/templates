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


(tplmac-class-0001 html-string-base ()
    (dolist (blk-itm (slot-value vtex-elm 'block-content)) 
        (when blk-itm (write-elm blk-itm)))
)

(tplmac-constru-0001 
    html-string htm-string html-string-base ()
)

        
(tplmac-class-0001 cls-styl-concat-base (concat-strings)
    (do (
        (blk-cntn (slot-value vtex-elm 'block-content) (cdr blk-cntn))
        (cct-strs (slot-value vtex-elm 'concat-strings) (cdr cct-strs))
        )
        ((and (null blk-cntn) (null cct-strs)) "done")
        (when cct-strs (format t "~a" (car cct-strs)))
        (when blk-cntn (format t "~a" (car blk-cntn)))
        )
)

        
        
(defun fun-defstyl-concat (&rest vrst-args)
    (dolist (arg-itm vrst-args)
        (let* (
                (styl-nm (car arg-itm))
                (styl-cnc-strs (cdr arg-itm))
                )
            ;(format t "*evaluating*: ~a~%"
            ;    `(tplmac-constru-0001
            ;        ,(intern (string-upcase styl-nm))
            ;        ,(intern (string-upcase styl-nm))
            ;        html-styl-concat-base
            ;        ((concat-strings (list ,@styl-cnc-strs)))
            ;    ))
            (eval 
                `(tplmac-constru-0001
                    ,(intern (string-upcase styl-nm))
                    ,(intern (string-upcase styl-nm))
                    cls-styl-concat-base
                    ((concat-strings (list ,@styl-cnc-strs)))
                ))
        )
    )
    ;(loop for key being each hash-key of *tag-fun-table*
    ;        using (hash-value value)
    ;    do (format t "hash: ~a: ~a~%" key value))
)

(tplmac-class-0001 htm-list-base ()
    (list (slot-value vtex-elm 'block-content))
)

(tplmac-constru-0001 
    htm-list htm-list htm-list-base ()
)



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
                    (typep elm-itm 'symbol)
                    (null elm-itm)
                    )
                        (push elm-itm res))
                ((typep elm-itm 'list)
                    (cond
                        ((typep (car elm-itm) 'symbol)
                            ;(format t "*symbol*: ~a~%" (car elm-itm))
                            (cond
                                ((equalp (car elm-itm) 'DEFSTYL-CONCAT)
                                    (let* (
                                            (wrpedf (wrapfun elm-itm))
                                            (fn-nm (slot-value wrpedf 'fun-name))
                                            (fn-args (slot-value wrpedf 'fun-args)))
                                        (apply fn-nm fn-args)
                                        ;(format t "applied ~a~%" fn-nm)
                                        )
                                )
                                (t 
                                    ;(format t "~a~%" (car elm-itm)) 
                                    (push  (wrapfun elm-itm) res))
                            )
                        )
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
            (defstyl-concat fun-defstyl-concat)
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
        (setq tx-fl (apply #'make-base-file (codify spc-ls)))
        (cond
            (otfl-arg (with-open-file
                    (*standard-output* otfl-arg :direction :output)
                (write-elm tx-fl)))
            (t (write-elm tx-fl))
        )
    )
)
(exit)
