;; Time-stamp: <2018-02-14 15:18:21 kmodi>

;; Setup to build eless and its documentation in an "emacs -Q" environment.

;; Contents:
;;
;;  Some sane settings
;;  Defvars
;;  Org and packages management
;;  Miscellaneous helper functions
;;    Info `dir' file generation
;;    Org HTML postamble
;;  Main functions
;;    Build eless script
;;    Build eless documentation
;;  Org defaults

;;; Some sane settings
(setq-default require-final-newline t)
(setq-default indent-tabs-mode nil)
;; Don't save trailing white space in the generated bash script and
;; documentation.
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;;; Defvars
(defvar eless-test-setup-verbose nil
  "When non-nil, enable printing more messages from setup-eless.el.")

(defvar eless-install-org-from-elpa (or (null (getenv "ELESS_DEFAULT_ORG"))
                                        (version< emacs-version "26.0"))
  "When non-nil, install Org from Org Elpa.

The default value of this variable is non-nil if emacs version is
older than 26 AND the environment variable ELESS_DEFAULT_ORG is
unset, else it is nil.

Emacs 26 onwards comes with at least Org 9.1.6.  So there is no
need to install Org from Elpa as that Org version meets the
minimum requirement for `eless'.  So set the environment
variable ELESS_DEFAULT_ORG to a value like 1 if using emacs 26
or newer.")

(defvar eless-elpa (let ((dir (getenv "ELESS_ELPA")))
                     (unless dir
                       (setq dir (concat (file-name-as-directory
                                          (concat temporary-file-directory (getenv "USER")))
                                         "eless-dev/")))
                     (setq dir (file-name-as-directory dir))
                     (make-directory dir :parents)
                     dir))
(when eless-test-setup-verbose
  (message "eless-elpa: %s" eless-elpa))

(defvar eless-packages '(htmlize rainbow-delimiters))
(when eless-install-org-from-elpa
  ;; `org' will always be detected as installed, so use
  ;; `org-plus-contrib'.
  ;; Fri Sep 22 18:24:19 EDT 2017 - kmodi
  ;; Install the packages in the specified order. We do not want
  ;; `toc-org' to be installed first. If that happens, `org' will be
  ;; required before the newer version of Org gets installed and we
  ;; will end up with mixed Org version.  So put `org-plus-contrib' at
  ;; the beginning of `eless-packages'.
  (add-to-list 'eless-packages 'org-plus-contrib))

(defvar eless-git-root (progn
                         (require 'vc-git)
                         (file-truename (vc-git-root default-directory)))
  "Absolute path of the git root of the current project.")
(when eless-test-setup-verbose
  (message "eless-git-root: %S" eless-git-root))

;;; Org and packages management
;; Below will prevent installation of `org' package as a dependency
;; when installing `eless' from Melpa.
(defun eless-package-dependency-check-ignore (orig-ret)
  "Remove the `black listed packages' from ORIG-RET.

Packages listed in the let-bound `pkg-black-list' will not be auto-installed
even if they are found as dependencies."
  (let ((pkg-black-list '(org))
        new-ret
        pkg-name)
    (dolist (pkg-struct orig-ret)
      (setq pkg-name (package-desc-name pkg-struct))
      (if (member pkg-name pkg-black-list)
          (message (concat "Package `%s' will not be installed. "
                           "See `eless-package-dependency-check-ignore'.")
                   pkg-name)
        ;; (message "Package to be installed: %s" pkg-name)
        (push pkg-struct new-ret)))
    (setq new-ret (reverse new-ret))
    ;; (message "after  %S" new-ret)
    new-ret))
(advice-add 'package-compute-transaction :filter-return #'eless-package-dependency-check-ignore)
;; (advice-remove 'package-compute-transaction #'eless-package-dependency-check-ignore)

(if (and (stringp eless-elpa)
         (file-exists-p eless-elpa))
    (progn
      ;; Load newer version of .el and .elc if both are available
      (setq load-prefer-newer t)

      (setq package-user-dir (format "%selpa_%s/" eless-elpa emacs-major-version))

      ;; Below require will auto-create `package-user-dir' it doesn't exist.
      (require 'package)

      (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                          (not (gnutls-available-p))))
             (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
        (add-to-list 'package-archives (cons "melpa" url))) ;For `htmlize', `rainbow-delimiters'

      ;; Even if we don't need to install Org from Elpa, we need to
      ;; add Org Elpa in `package-archives' to prevent the "Package
      ;; ‘org-9.0’ is unavailable" error.
      (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/")) ;For latest stable `org'

      ;; Load emacs packages and activate them.
      ;; Don't delete this line.
      (package-initialize)
      ;; `package-initialize' call is required before any of the below
      ;; can happen.
      (add-to-list 'load-path (concat eless-git-root "build/")) ;For setup-eless.el

      (defvar eless-missing-packages '()
        "List populated at each startup that contains the list of packages that need
to be installed.")

      (dolist (p eless-packages)
        ;; (message "Is %S installed? %s" p (package-installed-p p))
        (unless (package-installed-p p)
          (add-to-list 'eless-missing-packages p :append)))

      (when eless-missing-packages
        (message "Emacs is now refreshing its package database...")
        (package-refresh-contents)
        ;; Install the missing packages
        (dolist (p eless-missing-packages)
          (message "Installing `%s' .." p)
          (package-install p))
        (setq eless-missing-packages '())))
  (error "The environment variable ELESS_ELPA needs to be set"))

;; Remove Org that ships with Emacs from the `load-path' if installing
;; it from Elpa.
(when eless-install-org-from-elpa
  (let* ((bin-dir (when (and invocation-directory
                             (file-exists-p invocation-directory))
                    (file-truename invocation-directory)))
         (prefix-dir (when bin-dir
                       (replace-regexp-in-string "bin/\\'" "" bin-dir)))
         (share-dir (when prefix-dir
                      (concat prefix-dir "share/")))
         (lisp-dir-1 (when share-dir ;Possibility where the lisp dir is something like ../emacs/26.0.50/lisp/
                       (concat share-dir "emacs/"
                               ;; If `emacs-version' is x.y.z.w, remove the ".w" portion
                               ;; Though, this is not needed and also will do nothing in emacs 26+
                               ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=22b2207471807bda86534b4faf1a29b3a6447536
                               (replace-regexp-in-string "\\([0-9]+\\.[0-9]+\\.[0-9]+\\).*" "\\1" emacs-version)
                               "/lisp/")))
         (lisp-dir-2 (when share-dir ;Possibility where the lisp dir is something like ../emacs/25.2/lisp/
                       (concat share-dir "emacs/"
                               (replace-regexp-in-string "\\([0-9]+\\.[0-9]+\\).*" "\\1" emacs-version)
                               "/lisp/"))))
    (when eless-test-setup-verbose
      (message "emacs bin-dir: %s" bin-dir)
      (message "emacs prefix-dir: %s" prefix-dir)
      (message "emacs share-dir: %s" share-dir)
      (message "emacs lisp-dir-1: %s" lisp-dir-1)
      (message "emacs lisp-dir-2: %s" lisp-dir-2))
    (defvar eless-default-lisp-directory (cond
                                          ((file-exists-p lisp-dir-1)
                                           lisp-dir-1)
                                          ((file-exists-p lisp-dir-2)
                                           lisp-dir-2)
                                          (t
                                           nil))
      "Directory containing lisp files for the Emacs installation.

This value must match the path to the lisp/ directory of the
Emacs installation.  If Emacs is installed using
--prefix=\"${PREFIX_DIR}\" this value would typically be
\"${PREFIX_DIR}/share/emacs/<VERSION>/lisp/\"."))
  (when eless-test-setup-verbose
    (message "eless-default-lisp-directory: %S" eless-default-lisp-directory))

  (with-eval-after-load 'package
    ;; Remove Org that ships with Emacs from the `load-path'.
    (when (stringp eless-default-lisp-directory)
      (dolist (path load-path)
        (when (string-match-p (expand-file-name "org" eless-default-lisp-directory) path)
          (setq load-path (delete path load-path))))))

  ;; (message "`load-path': %S" load-path)
  ;; (message "`load-path' Shadows:")
  ;; (message (list-load-path-shadows :stringp))
  )

;;; Miscellaneous helper functions

;;;; Info `dir' file generation
(defun eless-org-texinfo-publish-to-info (proj-plist)
  (let ((pub-dir (plist-get proj-plist :publishing-directory))
        info-file)
    (dolist (texi-file (directory-files-recursively pub-dir "\\.texi\\'"))
      (message "Compiling `%s' .." texi-file)
      (setq info-file (org-texinfo-compile texi-file))
      ;; Create/update dir file
      (shell-command (concat "install-info " info-file " "
                             (expand-file-name "dir" pub-dir))))))

;;;; Org HTML postamble
(defun eless-org-html-postamble-fn (info)
  "Custom postamble for org to HTML exports.
INFO is the property list of export options."
  (let ((author (car (plist-get info :author)))
        (creator (plist-get info :creator))
        (date (car (org-export-get-date info)))
        (d1 "<div style=\"display: inline\" ")
        (d2 "</div>"))
    (concat "Exported using "
            d1 "class=\"creator\">" creator d2 ; emacs and org versions
            (when author
              (concat " by " author))
            (when date
              (concat " on " d1 "class=\"date\">" date d2))
            ".")))

;;; Main functions

;;;; Build eless script
(defun eless-build-script ()
  "Build the eless Bash script.
Tangle the eless script from eless.org."
  (require 'ob-tangle)
  (add-hook 'org-babel-pre-tangle-hook #'eless-eval-commit-hash)
  ;; Trailing whitespace management
  ;; Delete trailing whitespace in tangled buffer and save it.
  (add-hook 'org-babel-post-tangle-hook #'delete-trailing-whitespace)
  (add-hook 'org-babel-post-tangle-hook #'save-buffer :append)
  (org-babel-tangle-file (buffer-file-name)))

;;;; Build eless documentation
(defun eless-build-docs (&optional doctype)
  "Build eless documentation: HTML, Info, Org documents (for GitHub).
If the optional argument DOCTYPE is not provided (or nil), build
all the documents.  If non-nil, its valid values are `html',
`info', `org'."
  (let* ((eless-doc-dir (concat eless-git-root "docs/"))
         (org-publish-project-alist `(;; HTML
                                      ("eless-html"
                                       :base-directory ,eless-git-root
                                       :with-tags nil
                                       :exclude "\\(CONTRIBUTING\\|README\\)\\.org"
                                       :exclude-tags ("noexport")
                                       :publishing-function org-html-publish-to-html
                                       :publishing-directory ,eless-doc-dir)
                                      ;; Info
                                      ("eless-info"
                                       :base-directory ,eless-git-root
                                       :with-tags nil
                                       :exclude "\\(CONTRIBUTING\\|README\\)\\.org"
                                       :exclude-tags ("noexport")
                                       :publishing-function org-texinfo-publish-to-texinfo
                                       :publishing-directory ,eless-doc-dir
                                       :completion-function eless-org-texinfo-publish-to-info)

                                      ("eless-all"
                                       :components ("eless-html" "eless-info"))))
         (org-src-preserve-indentation t) ;Preserve the leading whitespace in src blocks
         (org-id-track-globally nil) ;Prevent "Could not read org-id-values .." error
         (org-export-with-sub-superscripts '{})
         (org-export-with-smart-quotes t)
         (org-export-headline-levels 4)
         (org-src-fontify-natively t)
         (org-html-table-default-attributes '(:border "2"
                                              :cellspacing "0"
                                              :cellpadding "6"
                                              :rules "groups"
                                              :frame "hsides"
                                              :align "center"
                                              ;; below class requires bootstrap.css ( http://getbootstrap.com )
                                              :class "table-striped"))
         (org-html-htmlize-output-type 'css)
         (org-html-htmlize-font-prefix "org-"))
    (setq org-html-postamble #'eless-org-html-postamble-fn)

    ;; Export to HTML.
    ;; The ":force" arguments ensures that the publishing always
    ;; happens, even if nothing has changed in the source org
    ;; file. It's equivalent to setting
    ;; `org-publish-use-timestamps-flag' to nil.
    (when (or (null doctype)
              (equal doctype 'html))
      (org-publish-project "eless-html" :force))

    ;; Export to Info.
    (when (or (null doctype)
              (equal doctype 'info))
      (require 'ox-texinfo)             ;For eless.info export
      (org-publish-project "eless-info" :force))

    ;; Export GitHub documents (README + CONTRIBUTING).
    (when (or (null doctype)
              (equal doctype 'org))
      (let ((subtree-tags-to-export '("readme" "contributing"))
            ;; If a subtree matches a tag, do not try to export
            ;; further subtrees separately that could be under that.
            (org-use-tag-inheritance nil)
            (org-export-time-stamp-file nil) ;Do not print "Created <timestamp>" in exported files
            (org-export-with-toc nil) ;Do not export TOC
            (org-export-with-tags nil)) ;Do not print tag names in exported files
        (dolist (tag subtree-tags-to-export)
          (let ((exported-file-list (org-map-entries
                                     (lambda ()
                                       (org-org-export-to-org nil :subtreep))
                                     tag)))))))

    ;; ;; Export GitHub documents (Wiki + README + CONTRIBUTING).
    ;; ;; cd docs/
    ;; ;; git clone https://github.com/kaushalmodi/eless.wiki.git
    ;; (when (or (null doctype)
    ;;           (equal doctype 'org))
    ;;   (let ((eless-wiki-dir (concat eless-doc-dir "eless.wiki/")))
    ;;     (if (file-exists-p eless-wiki-dir)
    ;;         (let ((subtree-tags-to-export '("readme" "contributing" "wiki"))
    ;;               ;; If a subtree matches a tag, do not try to export further
    ;;               ;; subtrees separately that could be under that.
    ;;               (org-use-tag-inheritance nil)
    ;;               (org-export-time-stamp-file nil) ;Do not print "Created <timestamp>" in exported files
    ;;               (org-export-with-toc nil) ;Do not export TOC
    ;;               (org-export-with-tags nil)) ;Do not print tag names in exported files
    ;;           (dolist (tag subtree-tags-to-export)
    ;;             (let ((exported-file-list (org-map-entries
    ;;                                        (lambda ()
    ;;                                          (org-org-export-to-org nil :subtreep))
    ;;                                        tag)))
    ;;               (when (string= "wiki" tag) ;Move the wiki files to the correct directory
    ;;                 (dolist (exported-file exported-file-list)
    ;;                   ;; Move the wiki files to the correct
    ;;                   ;; directory.
    ;;                   (rename-file (expand-file-name exported-file eless-git-root)
    ;;                                (expand-file-name exported-file eless-wiki-dir)
    ;;                                :ok-if-already-exists))))))
    ;;       (user-error (concat "eless.wiki dir does not exist. You need to `cd docs/' "
    ;;                           "and `git clone https://github.com/kaushalmodi/eless.wiki.git'")))))
    ))

(defun eless-build-html-docs ()
  (eless-build-docs 'html))

(defun eless-build-info-docs ()
  (eless-build-docs 'info))

(defun eless-build-org-docs ()
  (eless-build-docs 'org))

;;; Org defaults
(with-eval-after-load 'org
  ;; Allow multiple line Org emphasis markup
  ;; http://emacs.stackexchange.com/a/13828/115
  (setcar (nthcdr 4 org-emphasis-regexp-components) 20) ;Up to 20 lines, default is just 1
  ;; Below is needed to apply the modified `org-emphasis-regexp-components'
  ;; settings from above.
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

  ;; Prevent prompts like:
  ;;   Non-existent agenda file
  (defun org-check-agenda-file (file))

  (let (ob-lang-alist)
    (add-to-list 'ob-lang-alist '(emacs-lisp . t))
    (add-to-list 'ob-lang-alist '(org . t))
    (add-to-list 'ob-lang-alist '(shell . t))
    (org-babel-do-load-languages 'org-babel-load-languages ob-lang-alist))

  (with-eval-after-load 'ob-core
    (defun eless-eval-commit-hash ()
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
    (defun eless-org-confirm-babel-evaluate-fn (lang body)
      "Mark org source blocks and `git rev-parse' command as safe."
      (let ((unsafe t))
        (when (or (string= lang "org")
                  (string= body "git rev-parse HEAD | head -c 7"))
          (setq unsafe nil))
        unsafe))
    (setq org-confirm-babel-evaluate #'eless-org-confirm-babel-evaluate-fn))

  (with-eval-after-load 'ox
    (setq org-export-headline-levels 4) ;default is 3
    (setq org-export-allow-bind-keywords t) ;for #+bind: org-html-inline-image-rules (("https" . "svg\\?branch="))

    (with-eval-after-load 'ox-html
      (setq org-html-checkbox-type 'unicode))))
