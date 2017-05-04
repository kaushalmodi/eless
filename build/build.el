(require 'ox-texinfo)
(require 'ox-gfm)

(defconst eless-root-dir (cdr (project-current)) ;Requires emacs 25.1
  "Root directory of the eless project.")
(defconst eless-doc-dir (concat eless-root-dir "doc/")
  "Documentation directory of the eless project.")
(defconst eless-wiki-dir (concat eless-root-dir "wiki/")
  "Directory for Wiki repo for the eless project.")

(defun my/org-texinfo-publish-to-info (proj-plist)
  (let ((pub-dir (plist-get proj-plist :publishing-directory))
        info-file)
    (dolist (texi-file (directory-files-recursively pub-dir "\\.texi\\'"))
      (message "Compiling `%s' .." texi-file)
      (setq info-file (org-texinfo-compile texi-file))
      ;; Create/update dir file
      (shell-command (concat "install-info " info-file " "
                             (concat pub-dir "dir"))))))

(defun eless/readme-completion-fn (proj-plist)
  (let* ((pub-dir (plist-get proj-plist :publishing-directory))
         (before-name (concat pub-dir "eless.md"))
         (after-name (concat pub-dir "README.md")))
    (rename-file before-name after-name :ok-if-already-exists)))

(defun eless/contributing-completion-fn (proj-plist)
  (let* ((pub-dir (plist-get proj-plist :publishing-directory))
         (before-name (concat pub-dir "eless.md"))
         (after-name (concat pub-dir "CONTRIBUTING.md")))
    (rename-file before-name after-name :ok-if-already-exists)))

(defun eless/wiki-debug-completion-fn (proj-plist)
  (let* ((pub-dir (plist-get proj-plist :publishing-directory))
         (before-name (concat pub-dir "eless.md"))
         (after-name (concat pub-dir "How To Help Debug.md")))
    (rename-file before-name after-name :ok-if-already-exists)))

(defun eless/wiki-tcsh-completion-fn (proj-plist)
  (let* ((pub-dir (plist-get proj-plist :publishing-directory))
         (before-name (concat pub-dir "eless.md"))
         (after-name (concat pub-dir "Example eless Config in tcsh.md")))
    (rename-file before-name after-name :ok-if-already-exists)))

;; (org) Complex example
(setq org-publish-project-alist
      `(;; HTML
        ("eless-html"
         :base-directory ,eless-root-dir
         :exclude-tags ("noexport" "readme" "wiki")
         :publishing-function org-html-publish-to-html
         :publishing-directory ,eless-doc-dir)
        ;; Info
        ("eless-info"
         :base-directory ,eless-root-dir
         :exclude-tags ("noexport" "readme" "wiki")
         :publishing-function org-texinfo-publish-to-texinfo
         :publishing-directory ,eless-doc-dir
         :completion-function my/org-texinfo-publish-to-info)

        ;; eless script
        ("eless-tangle"
         :base-directory ,eless-root-dir
         :publishing-function org-babel-tangle-publish
         :publishing-directory ,eless-root-dir)

        ;; README.md
        ("eless-readme"
         :base-directory ,eless-root-dir
         :with-toc nil
         :with-tags nil
         :select-tags ("readme")     ;Cannot have hyphens in tags!
         :publishing-function org-gfm-publish-to-gfm
         :publishing-directory ,eless-root-dir
         :completion-function eless/readme-completion-fn)

        ;; CONTRIBUTING.md
        ("eless-contributing"
         :base-directory ,eless-root-dir
         :with-toc nil
         :with-tags nil
         :select-tags ("contributing")     ;Cannot have hyphens in tags!
         :publishing-function org-gfm-publish-to-gfm
         :publishing-directory ,eless-root-dir
         :completion-function eless/contributing-completion-fn)

        ;; Wiki Pages
        ("eless-wiki-debug"
         :base-directory ,eless-root-dir
         :with-toc nil
         :with-tags nil
         :select-tags ("wikidebug")     ;Cannot have hyphens in tags!
         :publishing-function org-gfm-publish-to-gfm
         :publishing-directory ,eless-wiki-dir
         :completion-function eless/wiki-debug-completion-fn)
        ("eless-wiki-tcsh"
         :base-directory ,eless-root-dir
         :with-toc nil
         :with-tags nil
         :select-tags ("wikitcsh")     ;Cannot have hyphens in tags!
         :publishing-function org-gfm-publish-to-gfm
         :publishing-directory ,eless-wiki-dir
         :completion-function eless/wiki-tcsh-completion-fn)
        ("eless-wiki"
         :components ("eless-wiki-debug" "eless-wiki-tcsh"))

        ("eless-all-docs"
         :components ("eless-html" "eless-info"
                      "eless-readme" "eless-contributing" "eless-wiki"))

        ("eless-all"
         :components ("eless-all-docs" "eless-tangle"))))
