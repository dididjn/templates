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

(defun write-cv ()
  (write-tex (make-tex-file
    (cv-el-title "John Smith")
   
    (cv-el-parbox-styl-0001
        (list
            '( "Address" "123 Broadway," "City, 12345" )
            (list "Date of Birth"
                (list (cv-format-ordinal-styl-0001 7) "September" "1979")
            )
            '( "Nationality" "British")
        )
        (list
            '( "Home Phone" "+0 (000) 111 1111")
            '( "Mobile Phone" "+0 (000) 111 1112")
            (list "Email" (cv-el-href-mail "john@smith.com"))
        ))

    (cv-el-section-styl-0001 "Personal Profile"
        (cv-el-paragraph-styl-0001
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Duis elementum nec dolor sed sagittis. Cras justo lorem, volutpat mattis lacus vel, consequat aliquam quam. Interdum et malesuada fames ac ante ipsum primis in faucibus. Integer blandit, massa at tincidunt ornare, dolor magna interdum felis, ac blandit urna neque in turpis."
        )
    )

       
    (cv-el-section-styl-0001 "Education"
        (cv-el-section-periodic-item-styl-0001
            "2004" "2007" "BSc Hons in Computer Science"
            (cv-el-href-http
                "www.universityofcalifornia.edu"
                "The University of California, Berkeley" )
            (list
                (cv-el-paragraph-styl-0001
                    "First Class - 80\\% Average" (cv-el-end-of-line)))
            (cv-el-paragraph-styl-0001
                (cv-el-font-it
                    "Third Year Project - 89\\% awarded `Project of the Year 2007'"
                ))
        )
        (cv-el-section-periodic-item-styl-0001
            "2001" "2003" "Advanced Secondary Education"
            (cv-el-href-http
                "www.berkeleycollege.edu"
                "Berkeley College, California" )           
            '()
            (list (cv-el-font-it "Pure Mathematics") "A")
            (list (cv-el-font-it "Statistics (AS)") "A")
            (list (cv-el-font-it "Physics") "A")
            (list (cv-el-font-it "Economics") "B")
        )              
    )
    (cv-el-section-styl-0001 "Employment History"
        (cv-el-section-periodic-item-styl-0002
            "Sep 2011" "Present"
            "http://www.lehmanbrothers.com"
            "Lehman Brothers, 1234 Mario Park, San Francisco, CA, United States"
            "Senior Developer / Technical Team Lead"
            (cv-el-paragraph-styl-0001
                "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Duis elementum nec dolor sed sagittis. Cras justo lorem, volutpat mattis lacus vel, consequat aliquam quam. Interdum et malesuada fames ac ante ipsum primis in faucibus."
                (cv-el-end-of-line)
            )
            (cv-el-paragraph-styl-0002
                (cv-el-font-bf "Technologies:")
                "Ruby on Rails 2.3, Amazon EC2, NoSQL data stores, memcached, collaborative matching, Facebook Graph API."
            ))
        (cv-el-section-periodic-item-styl-0002
            "Oct 2009""Sep 2010"
            "http://www.initech.com"
            "Initech Inc., Otis St, CA 94025, United States"
            "Analyst"
            (cv-el-paragraph-styl-0001
                "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Duis elementum nec dolor:"
            )
            (cv-el-itemize-styl-0001 nil
                "Developed spreadsheets for risk analysis on exotic derivatives on a wide array of commodities (ags, oils, precious and base metals.)"
                "Managed blotter and secondary trades on structured notes, liaised with Middle Office, Sales and Structuring for bookkeeping."
            )
            (cv-el-paragraph-styl-0001
                "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Duis elementum nec dolor sed sagittis."
            )
        )
    )

    (cv-el-section-styl-0003 "Software Engineering Skills"
        (list "Programming Languages"
            (list "Ruby" "MRI 1.8.7, 1.9.2")
            (list "ASP.NET, C\\#, VB.NET")
            (list "PHP")
            (list "Java/Scala")
        )
        '("Web Development"
            ("HTML5, CSS3/SASS, JavaScript/CoffeeScript/jQuery")
            ("Ruby on Rails v3.1")
            ("Test:Unit, RSpec, Cucumber, Selenium" "automated testing frameworks")
            ("Apache/Nginx Web Servers")
        )
        '("Miscellaneous"
            ("Microsoft SQL Server 2000/2005" "database architecture and administration")
            ("Transact-SQL" "data definition and manipulation")
            ("SQL Profiler" "performance tuning and debugging")
            ("MySQL Server")
            ("CVS, DARCS, git" "source version control")
        ))
   
    (cv-el-section-styl-0004 "Interests"
        "Badminton, Tennis, Running, Cycling, Sailing"
        "Travelling"
        "Creative Writing"
        "Photography"
        "Car Mechanics"
    )
   
    (cv-el-section-styl-0005 "Referees"
        (list
            '("Name" "Bill Lumbergh")
            '("Company" "Initech Inc.")
            '("Position" "Vice President")
            (list "Contact" (cv-el-href-mail "bill@initech.com"))
        )
        (list
            '("Name" "Michael \"Big Mike\" Tucker")
            '("Company" "Burbank Buy More")
            '("Position" "Store Manager")
            (list "Contact" (cv-el-href-mail "mike@buymore.com"))
        )
    )
)))
               
(let ((outf  (car extensions:*command-line-argument-list*)))
        (cond
            (outf (with-open-file (*standard-output* outf :direction :output)
                (write-cv)))
            (t (write-cv))
        )
)
(exit)


(let ((otfl-arg nil) (lastky-arg nil))
    (dolist (arg-itm *command-line-argument-list*)
        (cond
            ((equalp (subseq arg-itm 0 1) "-")
                (setq lastky-arg arg-itm))
            ((equalp lastky-arg "-o")
                (setq otfl-arg arg-itm)
                (setq lastky-arg nil))
            (t setq lastky-arg nil)
        )
    )
    (cond
        (outfl-arg (with-open-file
                (*standard-output* outfl-arg :direction :output)
            (write-cv)))
        (t (write-cv))
    )
)

(exit)

