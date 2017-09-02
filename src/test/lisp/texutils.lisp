
;;;
;;; Copyright © 2017 Didid Junaedi
;;;
;;; This program is licensed under the Apache License, Version 2.0 (the
;;; "License"); you may not use this file except in compliance with the
;;; License. You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.
;;;   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; templatex.lisp version 0.1.0.1
;;; 14 May 2017
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Release Notes:
;;;
;;; For now I put release notes and documentation in the program
;;; file. I would later be looking for more appropriate place(s)
;;; where to store/host this project and related files.
;;;
;;; This is the first iteration of development of this application.
;;; Currently this program still uses a hard coded XeLaTeX template,
;;; which I downloaded from http://www.LaTeXTemplates.com . I have
;;; been able to produce valid PDF file from the LaTeX output, but
;;; you still need to download other files from the  site of the
;;; original file to do it.
;;;
;;; For this version I used Armed Bear Common Lisp (ABCL)
;;; version 1.0.1 to test and run the program.
;;;
;;; To run ABCL I invoked Java, in Linux, with argument
;;; "-Dfile.encoding=UTF8" to be able to write text like "Résumé".
;;;
;;; If invoked with argument(s) the program will use the first
;;; argument as the name of the output file, otherwise it will send
;;; the output to whatever in *standard-output* stream.
;;;
;;; Other than comments, extra blank lines and the spaces I
;;; introduce surrounding the dash ("-") in the periods, there are
;;; still extra "\\"s produced by the program. I would learn more
;;; about (Xe)LaTeX first before fixing this so that I can do it more
;;; consistently, not just for this particular template.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Application Description:
;;;
;;; This application, using Common Lisp, aims to make modifying
;;; template documents such as (Xe)LaTeX documents simpler and more
;;; flexible and able to do so with minimum program coding
;;; requirements. For example the programmer(s) would be looking for
;;; patterns that can be generalized, and for ways how to make users
;;; able to either swap templates easily or enhance/modify the one(s)
;;; already built-in or both and how to make it easy to input
;;; modifiable items/parts of the template(s), such as name, address,
;;; educations and skills.
;;;   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "cmnutils.lisp")


(defclass tex-file ()
    (
        (items :initform '())
    )
)

(defmethod write-elm ( ( tx-fl tex-file) )
    (format t "~a~%"      
"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This LaTeX script has been produced programmatically by Lisp
% program \"templatex.lisp\" version 0.101 (see
% https://technicalnotesblog.blogspot.com/ ), based on template
% downloaded from http://www.LaTeXTemplates.com (see details on
% Author(s) and License below).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Wilson Resume/CV
% XeLaTeX Template
% Version 1.0 (22/1/2015)
%
% This template has been downloaded from:
% http://www.LaTeXTemplates.com
%
% Original author:
% Howard Wilson (https://github.com/watsonbox/cv_template_2004) with
% extensive modifications by Vel (vel@latextemplates.com)
%
% License:
% CC BY-NC-SA 3.0 (http://creativecommons.org/licenses/by-nc-sa/3.0/)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%----------------------------------------------------------------------------------------
%    PACKAGES AND OTHER DOCUMENT CONFIGURATIONS
%----------------------------------------------------------------------------------------

\\documentclass[10pt]{article} % Default font size

\\input{structure.tex} % Include the file specifying document layout

%----------------------------------------------------------------------------------------

\\begin{document}

%----------------------------------------------------------------------------------------
%    NAME AND CONTACT INFORMATION
%----------------------------------------------------------------------------------------
"

    )
    (dolist (tx-itm (slot-value tx-fl 'items))
        (if (not (null tx-itm))
            (write-elm tx-itm)
        )
    )
    (format t "~a~%" "\\end{document}")

)

(defclass tex-element-block ()
    (
        (block-content :initarg :block-content)
    )
)

(defmethod write-elm ( ( vtx-blk tex-element-block) )
    (write-elm (slot-value vtx-blk 'block-content))
    (format t "~%")
)

(defmethod write-elm( ( str simple-base-string))
    (format t "~a" str)
)

(defmethod write-elm( ( str string))
    (format t "~a" str)
)


(defclass tex-cv-section-base( tex-element-block)
    (
        (section-title :initarg :title :initform "")
    )
)

(defmethod write-elm ( ( vcv-sect-bs tex-cv-section-base) )
    (princ (format nil "\\section{~a}~%"
        (slot-value vcv-sect-bs 'section-title)
    ))
    (dolist (sect-itm (slot-value vcv-sect-bs 'block-content))
        (when sect-itm
            (write-elm sect-itm)
            (format t "~%")
        )
    )

)

(defclass tex-cv-end-of-line () ()
)

(defmethod write-elm( (vcv-eol tex-cv-end-of-line) )
    (format t "\\\\")
)


(defun cv-el-end-of-line ()
    (make-instance 'tex-cv-end-of-line)
)

(tplmac-class-0001 tex-cv-block-base (block-begin block-end)
    (format t "~a" (slot-value vtex-elm 'block-begin) )
    (dolist (blk-itm (slot-value vtex-elm 'block-content)) 
        (when blk-itm (write-elm blk-itm)))
    (format t "~a" (slot-value vtex-elm 'block-end) )
)

(tplmac-constru-0001 
    cv-el-font-bf font-bf tex-cv-block-base 
    ((block-begin "\\textbf{") (block-end "}")))

(tplmac-constru-0001 
    cv-el-font-it font-it tex-cv-block-base 
    ((block-begin "\\textit{") (block-end "}")))

(tplmac-constru-0001 
    cv-el-title title tex-cv-block-base 
    ((block-begin "\\title{") (block-end (format nil " -- Résumé}~%"))))


(defclass tex-cv-section-styl-0001 (tex-cv-section-base)
    (
        (section-title :initarg :title :initform "")
    )
)

(defun cv-el-section-styl-0001 (vttl &rest vrst-args)
    (make-instance 'tex-cv-section-styl-0001
            :title vttl :block-content vrst-args)
)

(defclass tex-cv-itemize-styl-0001 (tex-element-block)
    (
        (indent :initarg :indent)
    )
)

(defmethod write-elm ( (vcv-itmz tex-cv-itemize-styl-0001) )
    (let (
            (itmz-kywrd
                (cond
                    ((slot-value vcv-itmz 'indent) "itemize")
                    (t "itemize-noindent")
                    )
            ))
        (format t "\\begin{~a}~%" itmz-kywrd)
        (dolist (itm (slot-value vcv-itmz 'block-content)) (when itm
            (format t "\\item{")
            (write-elm itm)
            (format t "}~%")
        ))
        (format t "\\end{~a}~%" itmz-kywrd)
    )
)

(defun cv-el-itemize-styl-0001 (vndnt &rest vrst-args)
    (make-instance 'tex-cv-itemize-styl-0001
                        :indent vndnt
                        :block-content vrst-args)
)


(defclass tex-cv-paragraph-styl-0001 (tex-element-block)
    (
        (end-w-newline :initarg :end-with-new-line)
    )
)

(defmethod write-elm ( (vcv-prgph tex-cv-paragraph-styl-0001) )
    (let ((sep-spc "") (need-new-ln nil))
        (dolist (itm (slot-value vcv-prgph 'block-content)) (when itm
            (format t "~a" sep-spc)
            (write-elm itm)
            (setq sep-spc " ")
            (setq need-new-ln t)
        ))
        ;(when need-new-ln (format t "\\\\~%"))
    )
)

(defun cv-el-paragraph-styl-0001 (&rest vrst-args)
    (make-instance 'tex-cv-paragraph-styl-0001
                        :block-content vrst-args)
)

(defclass tex-cv-paragraph-styl-0002 (tex-element-block) ()
)

(defmethod write-elm ( (vcv-prgph tex-cv-paragraph-styl-0002) )
    (let ((bl-first t)(sep-spc "") (need-new-ln nil))
        (dolist (itm (slot-value vcv-prgph 'block-content)) (when itm
            (when bl-first (format t "\\rule{0mm}{5mm}") (setq bl-first nil))
            (format t "~a" sep-spc)
            (write-elm itm)
            (setq sep-spc " ")
            (setq need-new-ln t)
        ))
        ;(when (and need-new-ln (slot-value vcv-prgph 'end-w-newline))
        ;        (format t "\\\\~%"))
    )
)

(defun cv-el-paragraph-styl-0002 (&rest vrst-args)
    (make-instance 'tex-cv-paragraph-styl-0002
                        :block-content vrst-args)
)



(defclass tex-cv-format-ordinal-styl-0001 ()
    (
        (ordinal-number :initarg :ordinal-number)
    )
)

(defmethod write-elm ( (vcv-fmt-ord tex-cv-format-ordinal-styl-0001) )
    (let* (
            (ord-num (slot-value vcv-fmt-ord 'ordinal-number))
            (mod10-ord-num (mod ord-num 10))
            (mod100-ord-num (mod ord-num 100))
            (sufx
                (cond
                    ((and (> mod100-ord-num 10) (< mod100-ord-num 20)) "th")
                    ( t
                        (case (mod ord-num 10)
                            (1 "st")
                            (2 "nd")
                            (3 "rd")
                            (otherwise "th"))
                    )
                )
            )
        )
        (format t "~d$^{~a}$" ord-num sufx)
    )
)

(defun cv-format-ordinal-styl-0001 (ord-num)
    (make-instance 'tex-cv-format-ordinal-styl-0001
                        :ordinal-number ord-num)
)


(defclass tex-cv-format-href-http ()
    (
        (website :initarg :website)
        (title :initarg :title :initform "")
    )
)

(defmethod write-elm ( (vcv-fmt-href-http tex-cv-format-href-http) )
    (format t "\\href{http://~a}{~a}"
        (slot-value vcv-fmt-href-http 'website)
        (slot-value vcv-fmt-href-http 'title)
    )
)
   

(defun cv-el-href-http (vwb-st vttl)
    (make-instance 'tex-cv-format-href-http
                        :website vwb-st :title vttl)
)

(defclass tex-cv-format-href-mail ()
    (
        (mail-address :initarg :mail-address)
    )
)

(defmethod write-elm ( (vcv-fmt-href-mail tex-cv-format-href-mail) )
    (format t "\\href{mailto:~a}{~:*~a}"
        (slot-value vcv-fmt-href-mail 'mail-address)
    )
)


(defun cv-el-href-mail (vml-addr)
    (make-instance 'tex-cv-format-href-mail
                        :mail-address vml-addr)
)

(defclass tex-cv-section-periodic-item-base (tex-element-block)
    (
        (period-from :initarg :period-from)
        (period-to :initarg :period-to)
    )
)

(defclass tex-cv-section-periodic-item-styl-0001
                (tex-cv-section-periodic-item-base)
    (
        (header-title :initarg :header-title)
        (header-entity :initarg :header-entity)
        (indented-list :initarg :indented-list)
    )
)

(defmethod write-elm ( ( vcv-sect-prd-itm
                            tex-cv-section-periodic-item-styl-0001) )
    (with-slots (
                    (pr-fr period-from)
                    (pr-to period-to)
                    (hd-ttl header-title)
                    (hd-entty header-entity)
                    (ndtd-ls indented-list)
                    (blk-cntnt block-content)
                )
                vcv-sect-prd-itm
        (format t "\\tabbedblock{~%\\bf{~a~a~a} \\> "
            (cond ((null pr-fr) "") (t pr-fr))
            (cond ((or (null pr-fr) (null pr-to)) "") (t " - "))
            (cond ((null pr-to) "") (t pr-to))
        )
        (write-elm hd-ttl)
        (format t " - ")
        (write-elm hd-entty)
        (format t "\\\\[5pt]~%")
        (dolist (ndtd-itm ndtd-ls) (when ndtd-itm
            (format t "\\>")
            (write-elm ndtd-itm)
            (format t "~%")
        ))
        (let ((frst-ln t)) (dolist (blk-itm blk-cntnt) (when blk-itm
            (when frst-ln (format t "\\>\\+~%") (setq frst-ln nil))                       
            (cond
                ((typep blk-itm 'cons) (let ((tab-pad "")) (dolist (itm-lv2 blk-itm)
                    (when itm-lv2
                        (format t "~a" tab-pad)
                        (write-elm itm-lv2)
                        (setq tab-pad "\\> ")))))
                (t (write-elm blk-itm))
            )
            ;(format t "~%")
            (format t "\\\\~%")
        )))
        (format t "}~%")
    )
)


(defun cv-el-section-periodic-item-styl-0001 (
            vprd-fr vprd-to vttl ventty vntd-ls &rest vrst-args)
    (make-instance 'tex-cv-section-periodic-item-styl-0001
            :period-from vprd-fr
            :period-to vprd-to
            :header-title vttl
            :header-entity ventty
            :indented-list vntd-ls
            :block-content vrst-args
    )
)

(defclass tex-cv-section-periodic-item-styl-0002
                (tex-cv-section-periodic-item-base)
    (
        (header-entity-website :initarg :header-entity-website)
        (header-entity-title :initarg :header-entity-title)
        (job-title :initarg :job-title)
    )
)

(defmethod write-elm ( ( vcv-sect-prd-itm
                            tex-cv-section-periodic-item-styl-0002) )
    (with-slots (
                    (pr-fr period-from)
                    (pr-to period-to)
                    (hd-entty-ws header-entity-website)
                    (hd-entty-ttl header-entity-title)
                    (jb-ttl job-title)
                    (blk-cntnt block-content)
                )
                vcv-sect-prd-itm
        (format t "\\job~%")
        (when (not (null pr-fr))
            (format t "{~a~a}"
                pr-fr
                (cond ((or (null pr-fr) (null pr-to)) "") (t " - "))
            )
        )
        (when (not (null pr-to)) (format t "{~a}" pr-to))
        (format t "~%{~a}~%{~a}~%{~a}~%"
                        hd-entty-ttl hd-entty-ws jb-ttl)
        ;;
        (format t "{")
        (let ((end-ln-pad "")) (dolist (blk-itm blk-cntnt)
            (when blk-itm
                (format t "~a" end-ln-pad)
                (write-elm blk-itm)
                (setq end-ln-pad (format nil "~%"))
            )
        ))
        (format t "}~%")
    )

)

(defun cv-el-section-periodic-item-styl-0002 (
            vprd-fr vprd-to vhd-entty-ws vhd-entty-ttl vjb-ttl
            &rest vrst-args)
    (make-instance 'tex-cv-section-periodic-item-styl-0002
            :period-from vprd-fr
            :period-to vprd-to
            :header-entity-website vhd-entty-ws
            :header-entity-title vhd-entty-ttl
            :job-title vjb-ttl
            :block-content vrst-args
    )
)

(defclass tex-cv-section-group-item-styl-0001( tex-element-block)
    ()
)

(defmethod write-elm((vcv-sect-grp-itm tex-cv-section-group-item-styl-0001))
    ;(format t "tex-cv-section-group-item-styl-0001~%")
    (dolist (sect-itm (car (slot-value vcv-sect-grp-itm 'block-content)))
        (format t "\\skillgroup{~a}~%{~%" (car sect-itm))
        (dolist (itm-lvl2 (cdr sect-itm))
            (format t "\\textit{~a}~a\\\\~%"
                (car itm-lvl2)
                (let ((itm-lvl2-2nd (cadr itm-lvl2)))
                    (cond
                        ((null itm-lvl2-2nd) "")
                        (t (format nil " - ~a" itm-lvl2-2nd)))
                )
            )
        )
        (format t "}~%")

    )
)


(defun cv-el-section-group-item-styl-0001 (&rest vrst-args)
    (make-instance 'tex-cv-section-group-item-styl-0001
            :block-content vrst-args
    )
)

(defclass tex-cv-section-styl-0003 (tex-cv-section-base)
    (
        (section-title :initarg :title :initform "")
    )
)

(defun cv-el-section-styl-0003 (vttl &rest vrst-args)
    (make-instance 'tex-cv-section-styl-0003
            :title vttl
            :block-content
                    (list (cv-el-section-group-item-styl-0001 vrst-args))
    )
)

(defclass tex-cv-section-group-item-styl-0002( tex-element-block)
    ()
)

(defmethod write-elm((vcv-sect-grp-itm tex-cv-section-group-item-styl-0002))
    ;(format t "tex-cv-section-group-item-styl-0002~%")
    (format t "\\interestsgroup{~%")
    (dolist (sect-itm (car (slot-value vcv-sect-grp-itm 'block-content)))
        (format t "\\interest{~a}~%" sect-itm)
    )
    (format t "}~%")
)


(defun cv-el-section-group-item-styl-0002 (&rest vrst-args)
    (make-instance 'tex-cv-section-group-item-styl-0002
            :block-content vrst-args
    )
)

(defclass tex-cv-section-styl-0004 (tex-cv-section-base)
    (
        (section-title :initarg :title :initform "")
    )
)



(defun cv-el-section-styl-0004 (vttl &rest vrst-args)
    (make-instance 'tex-cv-section-styl-0004
            :title vttl
            :block-content
                    (list (cv-el-section-group-item-styl-0002 vrst-args))
    )
)


(defclass tex-cv-parbox-styl-0001( tex-element-block) ()
)

(defmethod write-elm((vcv-tbd-blks tex-cv-parbox-styl-0001))
    (let ((itm-pos 1)) (dolist (tbd-blk
                        (slot-value vcv-tbd-blks 'block-content))
        (when (> itm-pos 1) (cond
            ((eq (mod itm-pos 2) 0) (format t "\\hfill~%"))
            (t (format t "\\\\~%"))
        ))
        (incf itm-pos)
        (format t "~a~%"
"\\parbox{0.5\\textwidth}{ % Block
\\begin{tabbing}
\\hspace{3cm} \\= \\hspace{4cm} \\= \\kill % Spacing within the block"
        )
        (let ((end-ln-pad "") (new-ln-str "")) (dolist (blk-ln tbd-blk)
                (format t "~a~a{\\bf ~a} \\> " end-ln-pad new-ln-str (car blk-ln))
                (let ((sub-end-ln-pad ""))
                (dolist (nx-itm (cdr blk-ln))
                    (format t "~a" sub-end-ln-pad)
                    (cond
                        ((typep nx-itm 'list)
                            (let ((spc-sep ""))
                            (dolist (sub-itm nx-itm)
                                (format t "~a" spc-sep)
                                (write-elm sub-itm)
                                (setq spc-sep " ")
                        )))
                        (t (write-elm nx-itm))
                    )
                    (setq sub-end-ln-pad (format nil " \\\\~%\\> "))
                ))
                (setq end-ln-pad " \\\\")
                (setq new-ln-str (format nil "~%"))
            )
            (format t "~a~a" end-ln-pad new-ln-str)
        )
        (format t "\\end{tabbing}}~%")
    ))
)


(defun cv-el-parbox-styl-0001 (&rest vrst-args)
    (make-instance 'tex-cv-parbox-styl-0001
            :block-content vrst-args
    )
)


(defclass tex-cv-section-group-item-styl-0003( tex-element-block) ()
)

(defmethod write-elm((vcv-sect-grp-itm tex-cv-section-group-item-styl-0003))
    ;(format t "tex-cv-section-group-item-styl-0003~%")
    (let ((itm-pos 1)) (dolist (sect-itm (car
                        (slot-value vcv-sect-grp-itm 'block-content)))
        (when (> itm-pos 1) (cond
            ((eq (mod itm-pos 2) 0) (format t "\\hfill~%"))
            (t (format t "\\\\~%"))
        ))
        (incf itm-pos)
        (format t "~a~%"
"\\parbox{0.5\\textwidth}{ % Block
\\begin{tabbing}
\\hspace{2.75cm} \\= \\hspace{4cm} \\= \\kill % Spacing within the block"
        )
        (let ((end-ln-pad "") (new-ln-str "")) (dolist (itm-lv2 sect-itm)
                (format t "~a~a{\\bf ~a} \\> " end-ln-pad new-ln-str (car itm-lv2))
                (write-elm (cadr itm-lv2))
                (setq end-ln-pad " \\\\")
                (setq new-ln-str (format nil "~%"))
            )
            (format t "~a" new-ln-str)
        )
        (format t "\\end{tabbing}}~%")
    ))
)




(defun cv-el-section-group-item-styl-0003 (&rest vrst-args)
    (make-instance 'tex-cv-section-group-item-styl-0003
            :block-content vrst-args
    )
)

;(defmethod write-tex((vcv-sect-grp-itm tex-cv-section-group-item-styl-0003))
;    (format t "tex-cv-section-group-item-styl-0003~%")
;)


(defclass tex-cv-section-styl-0005 (tex-cv-section-base)
    (
        (section-title :initarg :title :initform "")
    )
)

(defun cv-el-section-styl-0005 (vttl &rest vrst-args)
    (make-instance 'tex-cv-section-styl-0005
            :title vttl
            :block-content
                    (list (cv-el-section-group-item-styl-0003 vrst-args))
    )
)

(defun make-tex-file (&rest tx-vitm-lste)
    (let
        ((tx-fl (make-instance 'tex-file)))       
        (setf (slot-value tx-fl 'items)
            tx-vitm-lste
        )
        tx-fl
    )
)


