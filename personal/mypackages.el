(prelude-require-packages '(
                            all-the-icons
                            all-the-icons-dired
                            all-the-icons-gnus
                            all-the-icons-ibuffer
                            auctex auto-dictionary
                            autoinsert
                            autopair
                            bbdb
                            cdlatex
                            centaur-tabs
                            citeproc
                            citeproc-org
                            cl-lib
                            dash
                            dashboard
                            dash-functional
                            el-autoyas
                            el-get
                            eshell-bookmark
                            exec-path-from-shell
                            flyspell
                            fvwm-mode
                            gmail-message-mode
                            gnus-alias
                            gnus-summary-ext
                            gnus-x-gm-raw
                            interleave
                            leuven-theme
                            major-mode-icons
                            markdown-mode
                            neotree
                            org
                            org-bullets
                            org-context
                            org-mime
                            org-pdftools
                            org-ref
                            org-static-blog
                            outorg
                            ox-pandoc
                            ox-tufte
                            page-break-lines
                            paredit
                            popwin
                            proof-general
                            tangotango-theme
                            typo
                            use-package-el-get
                            vlf
                            w3m
                            which-key
                            xclip
                            xkcd
                            yasnippet
                            yasnippet-snippets
                            )
                          )

(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(use-package org
  :ensure org-plus-contrib)
