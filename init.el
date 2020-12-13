;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,

(package-initialize)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.org/packages/")
	))

; list the packages you want
(setq package-list
      '(elpy cython-mode firestarter
	     gruvbox-theme autothemer ivy magit
	     projectile flycheck yasnippet
	     cuda-mode cmake-mode markdown-mode counsel
	     py-isort conda org org-bullets tramp
             flycheck lsp-ui lsp-pyright))


; activate all the packages
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(setq custom-file "~/.emacs.d/custom.el")
(ignore-errors (load custom-file)) ;; It may not yet exist.

;; Configure ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; Configure shell
(defun my-comint-init ()
  (setq comint-process-echoes t))
(add-hook 'comint-mode-hook 'my-comint-init)

;; Configure org mode
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c s") 'org-save-all-org-buffers)
(global-set-key (kbd "C-c g") 'org-revert-all-org-buffers)
(setq org-agenda-files '("~/Dropbox/org"))
(setq org-refile-targets '((nil :maxlevel . 9)
                                (org-agenda-files :maxlevel . 9)))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/org/notes.org" "Tasks")
             "* TODO %?\n %t %i %a")))

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
        ("IN-PROGRESS" . "yellow")
        ("WAITING" . (:foreground "blue" :weight bold))
        ("DONE" . org-done)))

;; enable export to markdown in org-mode
(eval-after-load "org"
  '(require 'ox-md nil t))

;; enable UTF-8 bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Make IPython prompt work correctly
(setq python-shell-interpreter "ipython"
    python-shell-interpreter-args "--simple-prompt -i")

;; Configure magit
(global-set-key (kbd "C-x g") 'magit-status)
(add-hook 'server-switch-hook 'magit-commit-diff)

;; Configure projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-completion-system 'ivy)
(setq projectile-mode-line "Projectile")

;; Configure tramp
(require 'tramp)
(setq vc-handled-backends '(SVN Git))
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(setq tramp-default-method "scp")
(customize-set-variable
 'tramp-ssh-controlmaster-options
 (concat
  "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
  "-o ControlMaster=auto -o ControlPersist=yes"))

;; flycheck
(global-flycheck-mode t)
;; note that these bindings are optional
(global-set-key (kbd "C-c n") 'flycheck-next-error)
;; this might override a default binding for running a python process,
;; see comments below this answer
(global-set-key (kbd "C-c p") 'flycheck-prev-error)

;; flycheck-pycheckers
;; Allows multiple syntax checkers to run in parallel on Python code
;; Ideal use-case: pyflakes for syntax combined with mypy for typing
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)
  )
(setq flycheck-pycheckers-checkers
  '(
    mypy3
    pyflakes
    )
  )

;; lsp-python-ms
(setq lsp-python-ms-auto-install-server t)
(add-hook 'hack-local-variables-hook
       (lambda ()
	 (when (derived-mode-p 'python-mode)
	   (require 'lsp-python-ms)
	   (lsp)))) ; or lsp-deferred
(setq lsp-keymap-prefix "C-c C-l")
(setq lsp-ui-doc-enable nil)
;; misc
;; (add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default indent-tabs-mode nil)
(setq global-auto-revert-mode t)
(setq auto-revert-remote-files t)

(provide 'init)
(put 'narrow-to-region 'disabled nil)
