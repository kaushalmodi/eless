# Contributing Guide


## How to help debug

-   If you find `eless` not working as expected, file an [issue](https://github.com/kaushalmodi/eless/issues).
-   Include the following debug information:
    1.  `emacs --version`
    2.  `eless` debug info:
        -   Append the `-D` option to your `eless` use case. Examples:
            -   `eless foo -D`
            -   `info org | eless -D`
        -   If you are providing debug info for something like `man foo`, do
            -   `PAGER=​"eless -D" man foo`


## Development


### Dependencies

1.  [`ox-extra`](http://orgmode.org/cgit.cgi/org-mode.git/tree/contrib/lisp/ox-extra.el) package to make `:ignore:` tag in `eless.org` work as intended.
    -   Installed as part of [`org-plus-contrib`](http://orgmode.org/elpa.html) package from Org ELPA.
    -   Or, if you build `org` from git, add `ox-extra` to the `ORG_ADD_CONTRIB` variable in `local.mk`. See [org build system](http://orgmode.org/worg/dev/org-build-system.html).
2.  [`ox-gfm`](https://github.com/larstvei/ox-gfm) package to generate the `.md` files.
    -   Can be installed via Melpa.

I use the latest emacs and org-mode versions built from their master branches. So if somehow the *Org Publish* step does not work for you, open an issue.


### How to generate the `eless` script

-   Evaluate `build/build.el`.
-   Open `eless.org`.
-   `C-c C-e P x`, select `eless-tangle` project.
    -   That will generate the `eless` script, delete trailing whitespaces from it, and save it.
-   Run the tangled `eless` through [shellcheck](http://www.shellcheck.net/) to ensure that there are no errors.
-   Do `C-x v =​` in the `eless` buffer and understand what changes you made.
-   Provide a PR.


### How to generate `eless` *plus* documentation

-   Evaluate `build/build.el`.
-   Open `eless.org`.
-   `C-c C-e P x`, select `eless-all` project.
    -   That will generate the `eless` script, delete trailing whitespaces from it, and save it.
    -   Update HTML and Info documentation.
    -   Update the README, Wiki, etc. pages.
-   Run the tangled `eless` through [shellcheck](http://www.shellcheck.net/) to ensure that there are no errors.
-   Understand the changes made in all the files.
-   Provide a PR.
