
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

(defvar *tag-fun-table* (make-hash-table :test 'equalp))

(defmacro tplmac-class-0001 (name fields &body body)
    `(progn
        (defclass ,name (tex-element-block) ,
            (let ((res nil)) 
                (dolist (f fields)
                    (let* (      
                            (fname (symbol-name f)) 
                            (kw (intern fname :keyword))) 
                        (push 
                            `(
                                ,f :initarg 
                                ,kw :initform nil)
                            res)))
                (nreverse res)))
        (defmethod write-elm ((vtex-elm ,name))
            ,@body)
    ))

(defmacro tplmac-constru-0001 (name tag-name tex-class inits &body body)
    `(progn
        (defun ,name (&rest vrst-args)
            (make-instance ',tex-class
                :block-content vrst-args
                ,@(let ((res nil)) 
                    (dolist (ini inits)
                        (let* (
                                (f (car ini))
                                (v (cadr ini))
                                (fname (symbol-name f)) 
                                (kw (intern fname :keyword))) 
                            (push kw res)
                            (push v res)))
                    (nreverse res)))
            ,@body)
        (setf (gethash ',tag-name *tag-fun-table*) ',name)
    )
)

(defmacro tplmac-class-0002 (name fields &body body)
    `(progn
        (tplmac-class-0001 ,name ,fields &body ,body)
    ))

