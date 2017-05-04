# Eless &#x2013; A Better Less

**ShellCheck status:**

[![ShellCheck Status](https://travis-ci.org/kaushalmodi/eless.svg?branch=master)](https://travis-ci.org/kaushalmodi/eless)

---

[**Full Documentation**](https://cdn.rawgit.com/kaushalmodi/eless/master/doc/eless.html)

---

`eless` is a combination of Bash script and a minimal emacs `view-mode` config.

This script is designed to:

-   Be portable &#x2013; Just one bash script to download to run
-   Be independent of a user&rsquo;s emacs config
-   Not require an emacs server to be already running

It was created out of a need to have something *like* `less` (in the sense of *launch quickly, do, and quit*, but better in these ways:

-   Do syntax highlighting
-   Render Org-mode files
-   A better navigable man page viewer
-   A better Info viewer
-   Dired, especially `wdired` (batch edit symbolic links, for example?)
-   Show colored diffs
-   Filter log files to only show or not show lines matching a regexp
-   Start auto-reverting log files when I want (like `tail -f`)
-   Quickly change frame and font sizes
-   .. and more; basically everything that emacs has to offer!

I call it `eless` and here&rsquo;s a little taste of what it looks like:

*Click the below image to see a GIF animation in larger size.*

[![eless GIF](https://raw.githubusercontent.com/kaushalmodi/eless/images/images/eless.png)](https://raw.githubusercontent.com/kaushalmodi/eless/images/images/eless.gif)

As a bonus:

-   This script passes [ShellCheck](http://www.shellcheck.net), and
-   Unofficial Bash [strict mode](http://redsymbol.net/articles/unofficial-bash-strict-mode) is enabled.


## Try it out

Here are some usage examples:

```shell
eless foo.txt               # Open foo.txt in eless in terminal (-nw) mode by default.
eless foo.txt --gui         # Open foo.txt in eless in GUI mode.
echo 'foo' | eless          #
echo 'foo' | eless -        # Same as above. The hyphen after eless does not matter; is anyways discarded.
grep 'bar' foo.txt | eless  #
PAGER=eless man grep        # Launches man pages in eless (terminal mode), if the environment variable PAGER is set to eless.
PAGER=eless man grep --gui  # Launches man pages in eless (GUI mode), if the environment variable PAGER is set to eless.
diff foo bar | eless        # Colored diff!
eless .                     # Open dired in the current directory (enhanced 'ls')
ls --color=always | eless   # Auto-detect ANSI color codes and convert those to colors
eless -h | eless            # See eless help ;-)
info emacs | eless          # Read emacs Info manual in eless
```


## Contributors

-   Thanks to [Iqbal Ansari](https://github.com/iqbalansari) for adding support to read piped data in `emacs -Q -nw`.
