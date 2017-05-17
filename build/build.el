;; Step 1: M-x eless-install-dependencies
;; Step 2: M-x eless-build

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

(defvar eless-package-user-dir (format "%selpa_%s/"
                                       (concat temporary-file-directory
                                               (getenv "USER") "/" "eless/")
                                       emacs-major-version)
  "Package base directory for installing the latest stable version of
 org for eless.")

(defun eless-install-dependencies ()
  "Install dependencies for building eless in `eless-package-user-dir'.

The dependencies, if not already installed, are installed in a
separate `eless-package-user-dir' which you can later safely delete.

Installed dependencies:

- Org version at least 9.x. If not found, `org-plus-contrib' is
  installed.
- `htmlize' for org html exports
- `rainbow-delimiters' for syntax highlighting of parentheses in
  elisp code in html exports"
  (interactive)
  ;; Remove the org that ships with emacs from the `load-path'.  That
  ;; org version is too old: 8.2.x (even on emacs 25.2).
  (dolist (path load-path)
    (when (string-match-p
           (concat "emacs/"
                   ;; If `emacs-version' is x.y.z.w, remove the ".w" portion
                   ;; Though, this will do nothing in emacs 26+
                   ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=22b2207471807bda86534b4faf1a29b3a6447536
                   (replace-regexp-in-string
                    "\\([0-9]+\\.[0-9]+\\.[0-9]+\\).*" "\\1" emacs-version)
                   "/lisp/org\\'")
           path)
      (setq load-path (delete path load-path))))

  ;; Let-bind `package-user-dir'
  (let ((package-user-dir eless-package-user-dir))
    (require 'package)
    ;; Install `org-plus-contrib'; `org' cannot be "installed", only upgraded.
    (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
    (package-initialize)
    (package-refresh-contents)
    (unless (package-installed-p 'org-plus-contrib)
      (message "Installing `org-plus-contrib' to %s .." package-user-dir)
      (package-install 'org-plus-contrib))
    (unless (and (package-installed-p 'htmlize)
                 (package-installed-p 'rainbow-delimiters)) ;For syntax highlighting of parentheses in elisp code
      (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
      (package-refresh-contents)
      (package-install 'htmlize)
      (package-install 'rainbow-delimiters))

    (when (called-interactively-p 'interactive)
      (message "Org version: %s.\nNow run `eless-build'." (org-version nil :full)))))

(defun eless-build ()
  "Use eless.org to generate the `eless' script and documentation."
  (interactive)
  ;; Remove org that ships with emacs from the `load-path'.  That org
  ;; version is too old: 8.2.x (even on emacs 25.2).
  (dolist (path load-path)
    (when (string-match-p
           (concat "emacs/"
                   ;; If `emacs-version' is x.y.z.w, remove the ".w" portion
                   ;; Though, this will do nothing in emacs 26+
                   ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=22b2207471807bda86534b4faf1a29b3a6447536
                   (replace-regexp-in-string "\\([0-9]+\\.[0-9]+\\.[0-9]+\\).*" "\\1" emacs-version)
                   "/lisp/org\\'")
           path)
      (setq load-path (delete path load-path))))

  ;; See if user has installed a newer version of org themselves
  (require 'org nil :noerror)
  (unless (ignore-errors
            (find-library-name "org"))
    ;; If not, check if `eless-package-user-dir' exists, assume that
    ;; the user has already installed the latest stable version of org
    ;; using `eless-install-dependencies'.
    (when (file-exists-p eless-package-user-dir)
      (eless-install-dependencies)     ;Still rerun it to update `load-path'
      (require 'org nil :noerror)))

  ;; Throw and error if even then org is not detected.
  (unless (ignore-errors
            (find-library-name "org"))
    (user-error (concat "Your org version is the one that ships with "
                        "emacs; it is too old (8.2.x). You will need "
                        "to run `eless-install-dependencies' to proceed.")))

  ;; `htmlize' is required for HTML exports.
  (unless (require 'htmlize nil :noerror)
    (user-error "You will need to run `eless-install-dependencies' to proceed."))

  ;; `rainbow-delimiters' is required for syntax highlighting the
  ;; parentheses in elisp code.
  (if (require 'rainbow-delimiters nil :noerror)
      (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
    (user-error "You will need to run `eless-install-dependencies' to proceed."))

  ;; Proceed if the org version is good.
  (message "Org version detected: %s" (org-version nil :full))
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
         org-html-postamble)
    ;; Customize the HTML postamble
    (defun eless/org-html-postamble-fn (info)
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
    (setq org-html-postamble #'eless/org-html-postamble-fn)

    (add-hook 'org-babel-pre-tangle-hook #'eless/eval-commit-hash)

    ;; Trailing whitespace management
    ;; Delete trailing whitespace in tangled buffer and save it.
    (add-hook 'org-babel-post-tangle-hook #'delete-trailing-whitespace)
    (add-hook 'org-babel-post-tangle-hook #'save-buffer :append)
    ;; Also, do not save trailing whitespace in org buffers.
    (defun eless/org-delete-trailing-whitespace ()
      (add-hook 'before-save-hook #'delete-trailing-whitespace :append :local))
    (add-hook 'org-mode-hook #'eless/org-delete-trailing-whitespace)

    (let ((eless-org-buf (get-buffer "eless.org"))
          (org-babel-load-languages org-babel-load-languages))
      (unless eless-org-buf             ;Open eless.org if it's not already
        (setq eless-org-buf (find-file-noselect eless-org-file)))
      (with-current-buffer eless-org-buf
        (defun eless/org-confirm-babel-evaluate-fn (lang body)
          "Mark org source blocks and `git rev-parse' command as safe."
          (let ((unsafe t))
            (when (or (string= lang "org")
                      (string= body "git rev-parse HEAD | head -c 7"))
              (setq unsafe nil))
            unsafe))
        (setq-local org-confirm-babel-evaluate #'eless/org-confirm-babel-evaluate-fn)

        (org-babel-do-load-languages 'org-babel-load-languages
                                     '((emacs-lisp . t) (shell . t) (org . t)))

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
    (let ((org-html-htmlize-output-type 'css)
          (org-html-htmlize-font-prefix "org-"))
      (require 'ox-texinfo)                 ;For eless.info export
      ;; The ":force" arguments ensures that the publishing
      ;; always happens, even if nothing has changed in
      ;; the source org file. It's equivalent to setting
      ;; `org-publish-use-timestamps-flag' to nil.
      (org-publish-project "eless-all" :force))

    ;; Remove the hooks added above
    (remove-hook 'org-babel-pre-tangle-hook #'eless/eval-commit-hash)
    (remove-hook 'org-babel-post-tangle-hook #'delete-trailing-whitespace)
    (remove-hook 'org-babel-post-tangle-hook #'save-buffer)
    (remove-hook 'org-mode-hook #'eless/org-delete-trailing-whitespace)))
