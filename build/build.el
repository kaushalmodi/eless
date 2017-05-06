;; Elisp to build eless and documentation
;; 1. Evaluate this buffer
;; 2. M-x eless-build

(defun my/org-texinfo-publish-to-info (proj-plist)
  (let ((pub-dir (plist-get proj-plist :publishing-directory))
        info-file)
    (dolist (texi-file (directory-files-recursively pub-dir "\\.texi\\'"))
      (message "Compiling `%s' .." texi-file)
      (setq info-file (org-texinfo-compile texi-file))
      ;; Create/update dir file
      (shell-command (concat "install-info " info-file " "
                             (expand-file-name "dir" pub-dir))))))

(defun eless/eval-commit-hash ()
  "Navigate to the source block for getting the git hash and eval it."
  (let ((file (buffer-file-name)))
    (when (and file
               (string= "eless.org" (file-name-nondirectory file)))
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (point-min))
          (re-search-forward "^git rev-parse HEAD")
          (org-babel-execute-src-block))))))
(add-hook 'org-babel-pre-tangle-hook #'eless/eval-commit-hash)

(defun eless-build ()
  "Use eless.org to tangle out the `eless' script and documentation."
  (interactive)
  (require 'ox-texinfo)                 ;For eless.info export
  (let* ((eless-root-dir (cdr (project-current))) ;Requires emacs 25.1
         (eless-org-file (expand-file-name "eless.org" eless-root-dir))
         (eless-doc-dir (concat eless-root-dir "doc/"))
         ;; cd doc/
         ;; git clone https://github.com/kaushalmodi/eless.wiki.git
         (eless-wiki-dir (concat eless-doc-dir "eless.wiki/"))
         (org-babel-post-tangle-hook nil) ;Initialize `org-babel-post-tangle-hook' to nil in this let-bound copy
         (org-publish-project-alist `(;; HTML
                                      ("eless-html"
                                       :base-directory ,eless-root-dir
                                       :with-tags nil
                                       :exclude "\\(CONTRIBUTING\\|README\\)\\.org"
                                       :exclude-tags ("noexport")
                                       :publishing-function org-html-publish-to-html
                                       :publishing-directory ,eless-doc-dir)
                                      ;; Info
                                      ("eless-info"
                                       :base-directory ,eless-root-dir
                                       :with-tags nil
                                       :exclude "\\(CONTRIBUTING\\|README\\)\\.org"
                                       :exclude-tags ("noexport")
                                       :publishing-function org-texinfo-publish-to-texinfo
                                       :publishing-directory ,eless-doc-dir
                                       :completion-function my/org-texinfo-publish-to-info)

                                      ("eless-all"
                                       :components ("eless-html" "eless-info")))))

    ;; Delete trailing whitespace in tangled buffer and save it
    (add-hook 'org-babel-post-tangle-hook #'delete-trailing-whitespace)
    (add-hook 'org-babel-post-tangle-hook #'save-buffer :append)

    (let ((eless-org-buf (get-buffer "eless.org")))
      (unless eless-org-buf             ;Open eless.org if it's not already
        (setq eless-org-buf (find-file-noselect eless-org-file)))
      (with-current-buffer eless-org-buf
        ;; Tangle the eless script from eless.org
        (org-babel-tangle-file eless-org-file)

        (if (file-exists-p eless-wiki-dir)
            (let ((subtree-tags-to-export '("readme" "contributing" "wiki"))
                  ;; If a subtree matches a tag, do not try to export further
                  ;; subtrees separately that could be under that.
                  (org-use-tag-inheritance nil)
                  (org-export-time-stamp-file nil) ;Do not print "Created <timestamp>" in exported files
                  (org-export-with-toc nil)  ;Do not export TOC
                  (org-export-with-tags nil)) ;Do not print tag names in exported files
              (dolist (tag subtree-tags-to-export)
                (let* ((exported-file-list (org-map-entries '(org-org-export-to-org nil :subtreep) tag)))
                  (when (string= "wiki" tag) ;Move the wiki files to the correct directory
                    (dolist (exported-file exported-file-list)
                      ;; Move the wiki files to the correct directory
                      (rename-file (expand-file-name exported-file eless-root-dir)
                                   (expand-file-name exported-file eless-wiki-dir)
                                   :ok-if-already-exists))))))
          (user-error "eless.wiki dir does not exist. You need to `cd doc/' and `git clone https://github.com/kaushalmodi/eless.wiki.git'"))))

    ;; Export to HTML and Info
    ;; The ":force" arguments ensures that the publishing always happens, even
    ;; if nothing has changed in the source org file. It's equivalent to
    ;; setting `org-publish-use-timestamps-flag' to nil.
    (org-publish-project "eless-all" :force)))
