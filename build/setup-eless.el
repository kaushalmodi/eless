;; Time-stamp: <2022-02-15 21:31:04 kmodi>

;; Setup to build eless and its documentation in an "emacs -Q" environment.

;; Contents:
;;
;;  Some sane settings
;;  Defvars
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
(setq-default make-backup-files nil)

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

(defvar eless-default-share-directory nil
  "Share directory for this Emacs installation.")

(defvar eless-default-lisp-directory nil
  "Directory containing lisp files for the Emacs installation.

This value must match the path to the lisp/ directory of the
Emacs installation.  If Emacs is installed using
--prefix=\"${PREFIX_DIR}\" this value would typically be
\"${PREFIX_DIR}/share/emacs/<VERSION>/lisp/\".")

;; Remove Org that ships with Emacs from the `load-path' if installing
;; it from Elpa.
(when eless-install-org-from-elpa
  (let* ((bin-dir (when (and invocation-directory
                             (file-exists-p invocation-directory))
                    (file-truename invocation-directory)))
         (prefix-dir (when bin-dir        ;Because bin-dir = prefix-dir + "bin/"
                       (file-name-directory (directory-file-name bin-dir))))
         (share-dir (when prefix-dir
                      (let ((share-dir-1 (file-name-as-directory (expand-file-name "share" prefix-dir))))
                        (when (file-exists-p share-dir-1)
                          (setq eless-default-share-directory share-dir-1))
                        share-dir-1)))
         (version-dir (when share-dir
                        (let* ((emacs-dir (file-name-as-directory (expand-file-name "emacs" share-dir)))
                               ;; Possibility where the lisp dir is something like
                               ;; ../emacs/26.0.50/lisp/.  If `emacs-version' is
                               ;; x.y.z.w, remove the ".w" portion.
                               ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=22b2207471807bda86534b4faf1a29b3a6447536
                               (version (replace-regexp-in-string "\\([0-9]+\\.[0-9]+\\.[0-9]+\\).*" "\\1" emacs-version))
                               (version-dir-1 (file-name-as-directory (expand-file-name version emacs-dir))))
                          (if (file-exists-p version-dir-1)
                              version-dir-1
                            ;; Possibility where the lisp dir is something like
                            ;; ../emacs/25.2/lisp/.  If `emacs-version' is x.y.z,
                            ;; remove the ".z" portion.
                            (setq version (replace-regexp-in-string "\\([0-9]+\\.[0-9]+\\).*" "\\1" emacs-version))
                            (setq version-dir-1 (file-name-as-directory (expand-file-name version emacs-dir)))
                            (when (file-exists-p version-dir-1)
                              version-dir-1))))))
    (when version-dir
      (let ((lisp-dir-1 (file-name-as-directory (expand-file-name "lisp" version-dir))))
        (when (file-exists-p lisp-dir-1)
          (setq eless-default-lisp-directory lisp-dir-1)))))
  (when eless-test-setup-verbose
    (message "eless-default-share-directory: %s" eless-default-share-directory)
    (message "eless-default-lisp-directory: %s" eless-default-lisp-directory))

  (with-eval-after-load 'package
    ;; Remove Org that ships with Emacs from the `load-path'.
    (let ((default-org-path (expand-file-name "org" eless-default-lisp-directory)))
      (setq load-path (delete default-org-path load-path)))

    ;; Remove Org installed in `site-lisp' directory from the `load-path'.
    (let ((site-lisp-org-path (expand-file-name "emacs/site-lisp/org" eless-default-share-directory)))
      (setq load-path (delete site-lisp-org-path load-path)))

    ;; Remove Org from `package--builtin-versions'.
    (setq package--builtin-versions (delete (assoc 'org package--builtin-versions) package--builtin-versions))
    ;; Remove Org from `package--builtins'.
    (require 'finder-inf nil t)         ;Populate `package--builtins'
    (setq package--builtins (delete (assoc 'org package--builtins) package--builtins))

    (when eless-test-setup-verbose
      (message "org detected as installed initially? %S" (package-installed-p 'org))
      (message "load-path: %s" load-path)
      ;; (message "`load-path' Shadows:")
      ;; (message (list-load-path-shadows :stringp))
      )))

(defvar eless-elpa (let ((dir (getenv "ELESS_ELPA")))
                     (unless dir
                       (setq dir
                             (let* ((dir-1 (file-name-as-directory (expand-file-name user-login-name temporary-file-directory)))
                                    (dir-2 (file-name-as-directory (expand-file-name "eless-dev" dir-1))))
                               dir-2)))
                     (setq dir (file-name-as-directory dir))
                     (make-directory dir :parents)
                     dir))
(when eless-test-setup-verbose
  (message "eless-elpa: %s" eless-elpa))

(defvar eless-packages '(htmlize rainbow-delimiters))
(when eless-install-org-from-elpa
  ;; Tue Feb 15 21:15:07 EST 2022 - kmodi
  ;; Put `org' at the beginning of `eless-packages' so that it
  ;; gets installed before any other package can `require' it.
  (add-to-list 'eless-packages 'org))

(defvar eless-git-root (progn
                         (require 'vc-git)
                         (file-truename (vc-git-root default-directory)))
  "Absolute path of the git root of the current project.")
(when eless-test-setup-verbose
  (message "eless-git-root: %S" eless-git-root))

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
        (when (equal system-type 'darwin)
          ;; macOS workaround for https://github.com/kaushalmodi/eless/issues/24.
          (setq network-security-level 'low))

        (message "Emacs is now refreshing its package database...")
        (package-refresh-contents)
        ;; Install the missing packages
        (dolist (p eless-missing-packages)
          (message "Installing `%s' .." p)
          (package-install p))
        (setq eless-missing-packages '())))
  (error "The environment variable ELESS_ELPA needs to be set"))

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
              (re-search-forward "^git describe --tags HEAD")
              (org-babel-execute-src-block))))))
    (defun eless-org-confirm-babel-evaluate-fn (lang body)
      "Mark org source blocks and `git describe' command as safe."
      (let ((unsafe t))
        (when (or (string= lang "org")
                  (string= body "git describe --tags HEAD"))
          (setq unsafe nil))
        unsafe))
    (setq org-confirm-babel-evaluate #'eless-org-confirm-babel-evaluate-fn))

  (with-eval-after-load 'ox
    (setq org-export-headline-levels 4) ;default is 3
    (setq org-export-allow-bind-keywords t) ;for #+bind: org-html-inline-image-rules (("https" . "svg\\?branch="))

    (with-eval-after-load 'ox-html
      (setq org-html-checkbox-type 'unicode))))
