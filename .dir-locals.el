(nil
 . ((nil . ((indent-tabs-mode . nil)
            (fill-column . 70)
            (eval . (progn
                      (when (buffer-file-name)
                        (require 'vc-git) ;For `vc-git-root'
                        (autoload
                          'eless-build (expand-file-name "build/build.el" (vc-git-root (buffer-file-name)))
                          "Use eless.org to generate the `eless' script and documentation."
                          :interactive)
                        (autoload
                          'eless-install-dependencies (expand-file-name "build/build.el" (vc-git-root (buffer-file-name)))
                          "Install dependencies for building `eless', in `eless-package-user-dir'."
                          :interactive))))))
    (org-mode . ((mode . auto-fill)))))
