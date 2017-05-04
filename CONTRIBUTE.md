# Contributing Guide


## How to generate the `eless` script

-   Evaluate `build/build.el`.
-   Open `eless.org`.
-   `C-c C-e P x`, select `eless-tangle` project.
    -   That will generate the `eless` script and delete trailing whitespaces from it.
-   **Then you need to save `eless`**.
-   Do `C-x v =​` in the `eless` buffer and understand what changes you made.
-   Provide a PR.


## How to generate the documentation too

-   Evaluate `build/build.el`.
-   Open `eless.org`.
-   `C-c C-e P x`, select `eless-all` project.
    -   That will generate the `eless` script and delete trailing whitespaces from it.
        -   **Then you need to save `eless`**.
    -   Update HTML and Info documentation.
    -   Update the README, Wiki, etc. pages.
-   Understand the changes made in all the files.
-   Provide a PR.
