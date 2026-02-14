# denote-merge: Denote extension to merge contents from one note into another

This is an optional extension to the `denote` package. It provides
commands and relevant user options to streamline the work of merging
contents from one Denote file to another. This is for users who
periodically review their notes to add, remove, or otherwise
consolidate their accumulated knowledge.

+ Git repository: <https://github.com/protesilaos/denote-merge>
+ Official manual: <https://protesilaos.com/emacs/denote-merge>
+ Backronym: Denote... Merging Eventually Reformats the Given Entries.

```elisp
(use-package denote-merge
  :ensure nil ; not in any package archive
  :init
  ;; Then upgrade it with the command `package-vc-upgrade' or `package-vc-upgrade-all'.
  (unless (package-installed-p 'institution-calendar)
    (package-vc-install "https://github.com/protesilaos/institution-calendar.git"))
  ;; You can bind these to keys.  They are here so you can learn about them.
  :commands
  ( denote-merge-file
    denote-merge-region
    denote-merge-region-plain
    denote-merge-region-plain-indented
    denote-merge-region-org-src
    denote-merge-region-org-quote
    denote-merge-region-org-example
    denote-merge-region-markdown-quote
    denote-merge-region-markdown-fenced-block))
```
