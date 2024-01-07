;;; cleopatra-gen-proc.el --- The cleopatra Emacs Library
;;; Commentary:
;;; Code:
(defun gen-processes-tangle-publish (conf filename pub-dir)
  (let ((tangled (cleopatra:tangle-publish conf filename pub-dir))
        (proc (file-name-sans-extension (file-name-nondirectory  filename))))
    (with-temp-buffer
      (insert
       (format "include %s.mk\n" proc)
       (format "CONFIGURE += %s\n" (mapconcat 'identity tangled " "))
       (format "prebuild : %s-prebuild\nbuild : %s-build\npostbuild : %s-postbuild\n"
               proc proc proc)
       (format "%s-build : %s-prebuild\n%s-postbuild : %s-build\n"
               proc proc proc proc)
       (format ".PHONY : %s-prebuild %s-build %s-postbuild\n"
               proc proc proc proc))
      (write-file (format "%s/%s.deps.mk" (getenv "CLEOPATRA_DIRECTORY") proc)))))

(cleopatra:configure)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))

(setq org-publish-project-alist
      `(("cleopatra-gen-proc"
         :base-directory ,(getenv "CLEOPATRA_GENERATION_PROCESSES")
         :publishing-directory "."
         :publishing-function gen-processes-tangle-publish)))

(org-publish-all)
;;; cleopatra-gen-proc.el ends here
