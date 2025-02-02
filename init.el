;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,


;;; Code:
(package-initialize)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ))

; list the packages you want
(setq package-list
      '( cython-mode
             autothemer ivy magit
             projectile flycheck yasnippet
             cuda-mode cmake-mode markdown-mode counsel
             py-isort conda org org-bullets tramp
             flycheck flycheck-pycheckers lsp-pyright
             json-mode pyvenv company rust-mode
             dash s editorconfig xclip yaml-mode vterm multi-vterm ivy-posframe))


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

(require 'ivy-posframe)
;; display at `ivy-posframe-style'
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
(ivy-posframe-mode 1)

;; Configure shell
(defun my-comint-init ()
  (setq comint-process-echoes t))
(add-hook 'comint-mode-hook 'my-comint-init)

;; Configure magit
(global-set-key (kbd "C-x g") 'magit-status)
(add-hook 'server-switch-hook 'magit-commit-diff)

;; Configure projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-completion-system 'ivy)
(setq projectile-mode-line "Projectile")
(setq projectile-use-git-grep t)

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

;; show line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; lsp
(setq lsp-keymap-prefix "C-c l")  ;; needs to appear before `require' to work
(require 'lsp)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'cuda-mode-hook 'lsp)
(add-hook 'rust-mode 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

(add-to-list 'lsp-language-id-configuration '(cuda-mode . "cuda"))
(add-to-list 'auto-mode-alist '("\.cu$" . c++-mode))
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "clangd")
                  :major-modes '(cuda-mode)
                  :language-id "cuda"
                  :server-id 'lsp-cuda-mode))

(add-hook 'hack-local-variables-hook
       (lambda ()
         (when (derived-mode-p 'python-mode)
           (require 'lsp-pyright)
           (lsp-deferred)))) ; or lsp-deferred

(add-hook 'hack-local-variables-hook
       (lambda ()
         (when (derived-mode-p 'rust-mode)
           (lsp-deferred)))) ; or lsp-deferred

(setq lsp-enable-file-watchers nil)
(setq lsp-ui-doc-enable nil)
; (setq lsp-clangd-binary-path "/home/ashwin/miniconda3/envs/theseus_dev/bin/clangd")

;; GitHub copilot
(add-to-list 'load-path "/home/ashwin/workspace/copilot.el")
(require 'copilot)

(add-hook 'prog-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

;; xclip-mode
(require 'xclip)
(xclip-mode 1)

;; misc
;; (add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default indent-tabs-mode nil)
(setq global-auto-revert-mode t)
(setq auto-revert-remote-files t)

;; Trigger completion immediately.
(setq company-idle-delay 0)

;; Number the candidates (use M-1, M-2 etc to select completions).
(setq company-show-numbers t)

;; pinentry:
(setq epa-pinentry-mode 'loopback)

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; multi-vterm
(require 'multi-vterm)
(define-key global-map (kbd "C-c t") 'multi-vterm)
(define-key global-map (kbd "C-c n") 'multi-vterm-next)

;; Enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; fringe mode
(fringe-mode nil)

;; tramp
(setq tramp-histfile-override t)

(provide 'init)
;;; init.el ends here

