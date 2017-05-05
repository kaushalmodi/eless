# How to help debug

-   If you find `eless` not working as expected, file an [issue](https://github.com/kaushalmodi/eless/issues).
-   Include the following debug information:
    1.  `emacs --version`
    2.  `eless` debug info:
        -   Append the `-D` option to your `eless` use case. Examples:
            -   `eless foo -D`
            -   `info org | eless -D`
        -   If you are providing debug info for something like `man foo`, do
            -   `PAGER=​"eless -D" man foo`


# Development


## Dependencies

There are virtually no dependencies, except of course that you need org. *If you need to update the `CONTRIBUTE.md`, you will need the [`ox-gfm`](https://github.com/larstvei/ox-gfm) package (available on Melpa).*

I used the latest emacs and org-mode versions built from their master branches for this project. So if any of the below steps do not work for you, open an issue.


## Steps to build `eless` and documentation

-   Open `eless.org`.
    -   You will be prompted to mark certain *Local Variables* settings as safe:

        ```emacs-lisp
        (org-confirm-babel-evaluate lambda
                                    (lang body)
                                    (let
                                        ((unsafe t))
                                      (when
                                          (or
                                           (string= lang "org")
                                           (string= body "git rev-parse HEAD | head -c 7"))
                                        (setq unsafe nil))
                                      unsafe))
        (eval load
              (expand-file-name "build/build.el"))
        (org-html-htmlize-font-prefix . "org-")
        (org-html-htmlize-output-type . css)
        (org-src-preserve-indentation . t)
        ```
    -   Approving that permanently will exclamation mark will ensure that the `build.el` gets loaded each time you open `eless.org`. In addition it also enables auto-evaluating of the `git rev-parse HEAD` command and setting of few org variables.
-   Do `M-x eless-build`. *This command got defined when the `build/build.el` got auto-loaded in the above step.*
-   Run the tangled `eless` through [shellcheck](http://www.shellcheck.net/) to ensure that there are no errors.
-   Understand the changes made in `eless`, *plus* all the other files.
-   Provide a PR.
