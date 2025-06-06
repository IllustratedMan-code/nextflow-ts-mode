
(defalias 'nextflow-parent-mode 'prog-mode)

;; See https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes/blob/master/groovy-mode.el
(setq nextflow-ts-font-lock-rules
  '(:language nextflow
    :override t
    :feature comment
    ([(comment) @font-lock-comment-face
      (groovy_doc) @font-lock-comment-face
      ])

    :language nextflow
    :override t
    :feature docs
    ([
      (groovy_doc_param "@param" @font-lock-keyword-face) 
      (groovy_doc_throws "@throws" @font-lock-keyword-face)
      ])

    :language nextflow
    :override t
    :feature keyword
    ([(groovy_package
       "package" @font-lock-keyword-face
       (identifier))
      (groovy_import "import" @font-lock-keyword-face)
      (declaration "def" @font-lock-keyword-face)
      ])

    :language nextflow
    :override t
    :feature type
    ([
      (declaration type: (identifier) @font-lock-type-face)
      (declaration type: (builtintype) @font-lock-type-face)
      ])

    :language nextflow
    :override t
    :feature literal
    ([
      (number_literal) @font-lock-number-face
      ])

    ))


(defun nextflow-ts-setup ()
  "Setup treesit for nextflow-ts-mode."
  ;; Our tree-sitter setup goes here.

  ;; This handles font locking -- more on that below.
  (setq-local treesit-font-lock-settings
               (apply #'treesit-font-lock-rules
                    nextflow-ts-font-lock-rules))

  (setq-local treesit-font-lock-feature-list
	      '((comment docs)
		(type keyword literal)
		  ))


  (treesit-major-mode-setup))


(define-derived-mode nextflow-ts-mode nextflow-parent-mode "Nextflow[ts]"
  "Major Mode for editing Nextflow code with tree-sitter"

  ;; look at syntax table in groovy-mode for a better one
  :syntax-table prog-mode-syntax-table

  (setq-local font-lock-defaults nil)
  (when (treesit-ready-p 'nextflow)
    (treesit-parser-create 'nextflow)
    (nextflow-ts-setup)
    ))

  
(defun nextflow-ts-reload-mode ()
  "Hard reset Tree-sitter font-lock and re-enable `nextflow-ts-mode`."
  (interactive)
  (kill-all-local-variables)
  (nextflow-ts-mode))
