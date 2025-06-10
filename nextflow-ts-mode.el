;;; nextflow-ts-mode.el --- Tree-sitter major mode for Nextflow -*- lexical-binding: t; -*-

;; Author: IllustratedMan-code
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages, tree-sitter
;; URL: https://github.com/IllustratedMan-code/nextflow-ts-mode

;;; Commentary:

;; Provides syntax highlighting and indentation for Nextflow using Tree-sitter.
;; Requires Emacs 29+ and a tree-sitter grammar for Nextflow installed.

;;; Code:
(defalias 'nextflow-parent-mode 'prog-mode)

;; See https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes/blob/master/groovy-mode.el
(defvar nextflow-ts-mode-indentation-rules
      '((nextflow
	 ((parent-is "source_file") column-0 0)
	 ((parent-is "pipeline") parent-bol 2)
	 ((parent-is "closure") parent-bol 2)
	 ((parent-is "process") parent-bol 2)
	 ((parent-is "workflow") parent-bol 2)
	 (no-node parent 0)
	 )))

(defvar nextflow-ts-font-lock-rules
  '(:language nextflow
    :feature comment
    ([(comment) @font-lock-comment-face
      (groovy_doc) @font-lock-doc-face
      ])

    :language nextflow
    :override t
    :feature docs
    ([
      (groovy_doc "/**" (first_line) @default)
      (groovy_doc_param "@param" @font-lock-doc-markup-face (identifier) @font-lock-variable-name-face) 
      (groovy_doc_throws "@throws" @font-lock-doc-markup-face (identifier) @font-lock-type-face)
      ])

    :language nextflow
    :override t
    :feature keyword
    ([(groovy_package
       "package" @font-lock-keyword-face
       )
      (groovy_import "import" @font-lock-keyword-face)
      (declaration "def" @font-lock-keyword-face)
      (function_definition type: "def" @font-lock-keyword-face)
      (return "return" @font-lock-keyword-face)
      (modifier) @font-lock-keyword-face
      (access_modifier) @font-lock-keyword-face
      (switch_statement "switch" @font-lock-keyword-face)
      (case "case" @font-lock-keyword-face)
      (case "default" @font-lock-keyword-face)
      (break) @font-lock-keyword-face
      (process "process" @font-lock-keyword-face)
      (workflow "workflow" @font-lock-keyword-face)
      (input_block "input:" @font-lock-keyword-face)
      (output_block "output:" @font-lock-keyword-face)
      (script "script:" @font-lock-keyword-face)
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
      (string) @font-lock-string-face
      (escape_sequence) @font-lock-escape-face
      (boolean_literal) @font-lock-constant-face
      ])

    :language nextflow
    :override t
    :feature variable
    ([
      (declaration name: (identifier) @font-lock-variable-name-face)
      (parameter_list (parameter name: (identifier) @font-lock-variable-name-face))
      (closure "->" @font-lock-keyword-face)
      (map_item key: (identifier) @font-lock-variable-use-face)
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
      (function_call function: (identifier) @font-lock-function-call-face)
      (juxt_function_call function: (identifier) @font-lock-function-call-face)
      ])

    :language nextflow
    :override t
    :feature class
    ([
      (annotation) @font-lock-constant-face
      (class_definition "class" @font-lock-keyword-face name: (identifier) @font-lock-type-face)

      ])
    :language nextflow
    :override t
    :feature pipeline
    ([
      (pipeline "pipeline" @font-lock-keyword-face)
      (juxt_function_call  function: (identifier) @font-lock-function-call-face)
      (interpolation
       "$" @font-lock-escape-face
       "{" @font-lock-bracket-face
       "}" @font-lock-bracket-face) @font-lock-variable-use-face
      ])
    
    :language nextflow
    :feature identifier
    ([
      (identifier) @font-lock-variable-use-face
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
	      '(
		(docs comment)
		(variable type keyword literal  function class pipeline)
		;; (identifier)
		))
  (setq-local treesit-simple-indent-rules nextflow-ts-mode-indentation-rules)


  (treesit-major-mode-setup))

;;;###autoload
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
