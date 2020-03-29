(use-package rust-mode :ensure t :defer t)

(cleopatra:configure)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))

(defun cleopatra-html-src-block (oldfun src-block contents info)
  (let*
      ((old-ret (funcall oldfun src-block contents info))
       (pars (org-babel-parse-header-arguments
              (org-element-property :parameters src-block)))
       (tangle (cdr (assoc :tangle pars)))
       (name (cdr (assoc :name pars))))
    (cond
     (name
      (concat
       "<div class=\"org-literate-programming\">"
       (format "<div class=\"org-src-name\">&lt;&lt%s&gt;&gt :=</div>" name)
       old-ret
       "</div>"))
     ((not (string= tangle "no"))
      (concat
       "<div class=\"org-literate-programming\">"
       old-ret
       (format "<div class=\"org-src-tangled-to\">%s</div>" tangle)
       "</div>"))
     (t old-ret))))

(advice-add 'org-html-src-block
            :around #'cleopatra-html-src-block)

(setq org-babel-exp-code-template
      (concat "#+BEGIN_SRC %lang%switches%flags "
              ":tangle %tangle :name %name\n"
              "%body\n"
              "#+END_SRC"))

(org-babel-lob-ingest "src/commons.org")

(setq org-publish-project-alist
      '(("og-html"
         :base-directory "src"
         :publishing-directory "book"
         :recursive t
         :publishing-function org-html-publish-to-html
         :auto-preamble t
         :html-link-home "/index.html"
         :html-head "<style>
           .org-src-name {
             font-weight : bold;
             font-family : monospace;
             font-size : smaller;
             margin-bottom : -1em;
           }
           
           .org-src-tangled-to::before {
             content : \"> \";
           }
           
           .org-src-tangled-to {
             font-weight : bold;
             font-family : monospace;
             font-size : smaller;
             margin-top : -1em;
             text-align : right;
           }
           
           #org-div-home-and-up {
             z-index : 1000;
             position : sticky;
             top : 0;
             background : white;
           }
         </style>")
        ("og-tangle"
         :base-directory "src"
         :exclude "commons.org"
         :publishing-directory "code"
         :recursive t
         :publishing-function cleopatra:tangle-publish)))

(org-publish-all)

(provide 'gen)
