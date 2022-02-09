;;; config --- Main config file
;;; Commentary:
;;; I only have this header here so flycheck stops complaining
;;;
;;; Code:

(setq user-full-name "Roch D'Amour"
      user-mail-address "roch.damour@gmail.com")

(setq doom-theme 'base16-ia-dark)

(let ((font-family "Essential PragmataPro"))
  (setq doom-variable-pitch-font (font-spec :family "IBM Plex Sans Condensed" )
        doom-font (font-spec :family font-family)
        doom-big-font (font-spec :family font-family)))

(setq display-line-numbers-type t)

(setq fancy-splash-image (concat (getenv "DOOMDIR") "/notarock.png"))

(setq frame-resize-pixelwise t)

(setq scroll-margin 10)

(dimmer-configure-magit)
(dimmer-configure-org)
(dimmer-mode t)
(setq highlight-indent-guides-method 'column)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

(add-hook 'prog-mode-hook (lambda ()
                            (setq show-trailing-whitespace t)))
(add-hook 'org-mode-hook (lambda ()
                           (setq show-trailing-whitespace t)))
(add-hook 'markdown-mode-hook (lambda ()
                                (setq show-trailing-whitespace t)))
(add-hook 'markdown-mode-hook 'mixed-pitch-mode)
(add-hook 'latex-mode-hook 'mixed-pitch-mode)

(defun my/set-initial-frame ()
  "Set initial frame size and position"
  (let* ((base-factor 0.80)
         (a-width (* (display-pixel-width) (/ base-factor 2)))
         (a-height (* (display-pixel-height) base-factor))
         (a-left (truncate (/ (- (display-pixel-width) a-width) 2)))
         (a-top (truncate (/ (- (display-pixel-height) a-height) 2))))
    (set-frame-position (selected-frame) a-left a-top)
    (set-frame-size (selected-frame) (truncate a-width)  (truncate a-height) t)))

(my/set-initial-frame)

(defun indent-buffer ()
  "Indent the whole buffer"
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun insert-random-hash ()
  "Insert garbage retrived from /dev/urandom"
  (interactive)
  (insert (string-trim (shell-command-to-string "< /dev/urandom tr -dc _A-Z-a-z-0-9 | head -c${1:-64};echo;"))))

(evil-ex-define-cmd "q" 'kill-this-buffer)
(evil-ex-define-cmd "quit" 'evil-quit)

(setq lsp-python-ms-executable (executable-find "python-language-server"))

(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)

(map! :map web-mode-map
      :n "SPC m F" #'eslint-fix)

(map! :map typescript-mode-map
      :n "SPC m F" #'eslint-fix)

(use-package org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("■" "■" "■")))

(after! org
  (map! :map org-mode-map
        :n "m-j" #'org-metadown
        :n "m-k" #'org-metaup)
  (add-hook 'org-mode-hook 'org-appear-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'mixed-pitch-mode)
  (add-hook 'org-mode-hook (lambda ()
                             (setq show-trailing-whitespace t)))
  (setq  ispell-local-dictionary "fr-toutesvariantes"
         org-hide-emphasis-markers t
         org-directory "~/org/"
         org-journal-file-type 'daily
         org-journal-dir (concat org-directory "journals")
         org-todos-file (concat org-directory "todos.org")
         org-timesheet-dir (concat org-directory "timesheet.org")
         org-journal-file-format "%Y-%m-%d.org"
         org-journal-date-format "%e %b %Y (%A)"
         org-journal-time-format "%H:%M"
         org-extend-today-until 4
         org-capture-templates       (doct '(("Todos" :keys "t"
                                              :file org-todos-file
                                              :todo-state "TODO"
                                              :template ("* TODO: %^{description}"
                                                         ":properties:"
                                                         ":created: %u"
                                                         ":end:"))
                                             ("Timesheet" :keys "T"
                                              :file )))
         org-todo-keyword-faces (quote (("todo" :foreground "#ff6347" :weight bold)
                                        ("done" :foreground "#006400" :weight bold :strike-through t)))
         org-todo-keywords '((sequence "todo(t)" "done(d)"))
         org-log-done t))

(defun org-journal-file-header-func (time)
  "Custom function to create journal header."
  (concat
   (pcase org-journal-file-type
     (`daily "#+TITLE: Daily Journal\n#+STARTUP: showeverything")
     (`weekly "#+TITLE: Weekly Journal\n#+STARTUP: folded")
     (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: folded")
     (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: folded"))))

(after! markdown
  (setq ispell-local-dictionary "fr-toutesvariantes"))

(map! :ne "C-S-k" #'drag-stuff-up)
(map! :ne "C-S-j" #'drag-stuff-down)
(map! :ne "C-S-l" #'drag-stuff-right)
(map! :ne "C-S-h" #'drag-stuff-left)

(map! :ne "SPC =" #'indent-buffer)
(map! :ne "SPC #" #'comment-or-uncomment-region)

(map! :ne "SPC j g" #'dumb-jump-go)
(map! :ne "SPC j b" #'dumb-jump-back)

(map! :ne "SPC i h" #'insert-random-hash)

(map! :ne "SPC w V" (lambda () (interactive)(evil-window-vsplit) (other-window 1)))

(map! [remap org-capture] nil)

(global-git-gutter-mode +1)

(global-wakatime-mode +1)

(setq ispell-dictionary "en_CA")

(fset 'env-to-yaml
      (kmacro-lambda-form [?0 ?f ?= ?r ?: ?a ?  escape ?w ?y ?s ?i ?W ?\" ?0 ?j] 0 "%d"))

(setq projectile-project-search-path '("~/src/"))

(load-file (concat (getenv "DOOMDIR") "/extra.el"))
;;; config.el ends here
