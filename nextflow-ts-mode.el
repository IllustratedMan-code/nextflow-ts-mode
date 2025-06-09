
(defalias 'nextflow-parent-mode 'prog-mode)

;; See https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes/blob/master/groovy-mode.el
(setq nextflow-ts-font-lock-rules
  '(:language nextflow
    :override t
    :feature comment
    ([(comment) @font-lock-comment-face
      (groovy_doc) @font-lock-doc-face
      ])

    :language nextflow
    :override t
    :feature docs
    ([
      (groovy_doc_param "@param" @font-lock-doc-markup-face (identifier) @font-lock-variable-name-face) 
      (groovy_doc_throws "@throws" @font-lock-doc-markup-face (identifier) @font-lock-type-face)
      ])

    :language nextflow
    :override t
    :feature keyword
    ([(groovy_package
       "package" @font-lock-keyword-face
       (identifier))
      (groovy_import "import" @font-lock-keyword-face)
      (declaration "def" @font-lock-keyword-face)
      (function_definition type: "def" @font-lock-keyword-face)
      (return "return" @font-lock-keyword-face)
      (modifier) @font-lock-keyword-face
      (access_modifier) @font-lock-keyword-face
      ])

    :language nextflow
    :override t
    :feature type
    ([
      (declaration type: (identifier) @font-lock-type-face)
      (declaration type: (builtintype) @font-lock-type-face)
      (builtintype) @font-lock-builtin-face
      ])

    :language nextflow
    :override t
    :feature literal
    ([
      (number_literal) @font-lock-number-face
      ])

    :language nextflow
    :override t
    :feature variable
    ([
      (declaration name: (identifier) @font-lock-variable-name-face)
      (parameter_list (parameter name: (identifier) @font-lock-variable-name-face))
      ])

    :language nextflow
    :override t
    :feature function
    ([
      (function_definition
       function: (identifier) @font-lock-function-name-face
       )
      (assignment (identifier) @font-lock-function-name-face (closure "->" @font-lock-keyword-face))
      (closure "->" @font-lock-keyword-face)
      ])

    :language nextflow
    :override t
    :feature class
    ([
      (annotation) @font-lock-constant-face
      (class_definition "class" @font-lock-keyword-face name: (identifier) @font-lock-type-face)

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
		(type keyword literal variable function class)
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

(provide 'nextflow-ts-mode)
