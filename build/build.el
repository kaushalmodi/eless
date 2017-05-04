;; Elisp to build eless and documentation
;; 1. Evaluate this buffer
;; 2. M-x eless/publish

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
          (re-search-forward "^\\*+ Git Commit Hash")
          (re-search-forward "^git rev-parse HEAD")
          (org-babel-execute-src-block))))))
(add-hook 'org-babel-pre-tangle-hook #'eless/eval-commit-hash)

(defun eless/post-tangle-delete-trailing-ws-and-save (orig-fun &rest args)
  "After tangling, delete trailing spaces from the target buffer and save it."
  (let* ((ret (apply orig-fun args))
         (source-file (buffer-file-name))
         (eless-buf (get-buffer "eless"))
         target-file)
    (when (and source-file
               (string= "eless.org" (file-name-nondirectory source-file)))
      ;; For some reason, the eless buffer if already opened needs to be closed
      ;; first.
      (when eless-buf
        (kill-buffer eless-buf))
      (setq target-file (expand-file-name "eless" (file-name-directory source-file)))
      (setq eless-buf (find-file-noselect target-file))
      (with-current-buffer eless-buf
        (delete-trailing-whitespace (point-min) nil)
        (save-buffer)
        (kill-buffer eless-buf))) ;No need to keep the eless buffer open
    ret))
(advice-add 'org-babel-tangle :around #'eless/post-tangle-delete-trailing-ws-and-save)
;; (advice-remove 'org-babel-tangle  #'eless/post-tangle-delete-trailing-ws-and-save)

(defun eless/publish ()
  "Use eless.org to tangle out the eless script and documentation."
  (interactive)
  ;; (org) Complex example
  (let* ((eless-root-dir (cdr (project-current))) ;Requires emacs 25.1
         (eless-org-file (expand-file-name "eless.org" eless-root-dir))
         (eless-doc-dir (concat eless-root-dir "doc/"))
         (eless-wiki-dir (concat eless-root-dir "wiki/"))
         (org-publish-project-alist `(;; HTML
                                      ("eless-html"
                                       :base-directory ,eless-root-dir
                                       :with-tags nil
                                       :exclude-tags ("noexport" "readme" "wiki")
                                       :publishing-function org-html-publish-to-html
                                       :publishing-directory ,eless-doc-dir)
                                      ;; Info
                                      ("eless-info"
                                       :base-directory ,eless-root-dir
                                       :with-tags nil
                                       :exclude-tags ("noexport" "readme" "wiki")
                                       :publishing-function org-texinfo-publish-to-texinfo
                                       :publishing-directory ,eless-doc-dir
                                       :completion-function my/org-texinfo-publish-to-info)

                                      ("eless-all"
                                       :components ("eless-html" "eless-info")))))

    (require 'ox-texinfo)               ;For eless.info export
    (require 'ox-gfm)                   ;For various .md exports

    (let ((eless-org-buf (get-buffer "eless.org")))
      (unless eless-org-buf             ;Open eless.org if it's not already
        (setq eless-org-buf (find-file-noselect eless-org-file)))
      (with-current-buffer eless-org-buf
        ;; Tangle the eless script from eless.org
        (org-babel-tangle-file eless-org-file)

        ;; Export to various .md files
        (let ((subtree-tags-to-export '("readme" "contributing"
                                        "wikitcsh"))
              ;; If a subtree matches a tag, do not try to export further
              ;; subtrees separately that could be under that.
              (org-use-tag-inheritance nil)
              (org-export-with-toc nil)  ;Do not export TOC
              (org-export-with-tags nil)) ;Do not print tag names in exported files
          (dolist (tag subtree-tags-to-export)
            (let* ((exported-file-list (org-map-entries '(org-gfm-export-to-markdown nil :subtreep) tag))
                   ;; The assumption is that for each tag, *only* one file is exported
                   (exported-file (when exported-file-list
                                    (car exported-file-list))))
              (when (string-match-p "\\`wiki.+" tag) ;Move the wiki files to the correct directory
                (rename-file (expand-file-name exported-file eless-root-dir)
                             (expand-file-name exported-file eless-wiki-dir)
                             :ok-if-already-exists)))))))

    ;; Export to HTML and Info
    (org-publish-project "eless-all" :force)))
