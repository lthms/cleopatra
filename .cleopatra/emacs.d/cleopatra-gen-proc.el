;;; cleopatra-gen-proc.el --- The cleopatra Emacs Library
;;; Commentary:
;;; Code:
(defun cleopatra:gen-processes-tangle-publish (conf filename pub-dir)
  (let ((tangled (cleopatra:tangle-publish conf filename pub-dir))
        (proc (file-name-sans-extension (file-name-nondirectory  filename))))
    (insert
     (format "include %s.mk\n" proc))
    (insert
     (format "CONFIGURE += %s\n" (mapconcat 'identity tangled " ")))
    (insert
     (format "prebuild : %s-prebuild\nbuild : %s-build\npostbuild : %s-postbuild\n"
             proc proc proc))
    (insert
     (format "%s-build : %s-prebuild\n%s-postbuild : %s-build\n"
             proc proc proc proc))
    (insert
     (format ".PHONY : %s-prebuild %s-build %s-postbuild\n"
             proc proc proc proc))))

(cleopatra:configure)
(setq org-publish-use-timestamps-flag nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))

(setq org-publish-project-alist
      `(("cleopatra-gen-proc"
         :base-directory ,(getenv "CLEOPATRA_GENERATION_PROCESSES")
         :publishing-directory "."
         :publishing-function cleopatra:gen-processes-tangle-publish)))

(with-temp-buffer
  (org-publish-all)
  (write-file (concat (getenv "CLEOPATRA_DIRECTORY") "/deps.mk")))

(provide 'cleopatra-gen-proc)
;;; cleopatra-gen-proc.el ends here
