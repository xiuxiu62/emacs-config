;;; slang-mode.el --- Major mode for editing Slang shader files -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Claude Code
;; Keywords: languages, shaders, gpu, slang
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; URL: https://shader-slang.org/

;;; Commentary:

;; This package provides a major mode for editing Slang shader language files.
;; Slang is a shading language and compiler that extends HLSL with modern features
;; like generics, interfaces, modules, and automatic differentiation.
;;
;; Features:
;; - Syntax highlighting for Slang keywords, types, and constructs
;; - Support for preprocessor directives
;; - C-style comment handling
;; - Basic indentation support
;; - Integration with standard Emacs features
;;
;; Usage:
;; Add to your init file:
;;   (require 'slang-mode)
;;
;; Slang files (.slang) will automatically use this mode.

;;; Code:
(require 'cc-mode)

(defgroup slang nil
  "Major mode for editing Slang shader files."
  :group 'languages
  :prefix "slang-")

(defcustom slang-mode-hook nil
  "Hook run when entering Slang mode."
  :type 'hook
  :group 'slang)

(defcustom slang-indent-offset 4
  "Indentation offset for Slang code."
  :type 'integer
  :safe 'integerp
  :group 'slang)

;;; Syntax highlighting
(defconst slang-keywords
  '("break" "continue" "do" "else" "for" "goto" "if" "return" "while"
    "switch" "case" "default" "try" "throw" "catch" "defer" "discard"
    "typedef" "using" "func" "override" "public" "internal" "private"
    "import" "module" "implementing" "__include" "export" "__exported"
    "groupshared" "let" "var" "property" "extension" "in" "out" "inout"
    "ref" "namespace" "this" "cbuffer" "tbuffer" "uniform" "dynamic_uniform"
    "typealias" "new" "__extern_cpp" "__target_intrinsic" "__stage_intrinsic"
    "__intrinsic_asm" "spirv_asm" "__fwd_diff" "__bwd_diff" "fwd_diff" "bwd_diff"
    "__dispatch_kernel" "no_diff" "__constref" "expand" "each" "where"
    "typename" "constexpr" "dyn" "some" "implicit" "noncopyable"
    "__generic" "associatedtype" "throws")
  "Slang control flow and declaration keywords.")

(defconst slang-storage-modifiers
  '("const" "extern" "register" "restrict" "static" "volatile" "inline"
    "nointerpolation" "precise" "row_major" "column_major" "snorm" "unorm"
    "globallycoherent" "layout")
  "Slang storage modifiers.")

(defconst slang-types
  '("bool" "int" "uint" "half" "float" "double" "char" "void"
    "int8_t" "int16_t" "int32_t" "int64_t"
    "uint8_t" "uint16_t" "uint32_t" "uint64_t"
    "intptr_t" "uintptr_t" "string" "Ptr"
    "vector" "matrix")
  "Slang primitive types.")

(defconst slang-constants
  '("true" "false" "nullptr" "none" "NULL" "TRUE" "FALSE")
  "Slang constant values.")

(defconst slang-preprocessor-directives
  '("if" "ifdef" "ifndef" "elif" "else" "endif" "define" "undef"
    "include" "import" "module" "implementing" "__include"
    "pragma" "error" "warning" "line" "version" "lang" "language")
  "Slang preprocessor directives.")

(defconst slang-shader-stages
  '("vertex" "fragment" "compute" "hull" "domain" "geometry"
    "raygen" "intersection" "anyhit" "closesthit" "miss"
    "mesh" "amplification" "callable")
  "Slang shader stage names.")

(defconst slang-operators
  '("sizeof" "countof" "as" "is")
  "Slang keyword operators.")

;; Font lock keywords
(defvar slang-font-lock-keywords
  (let ((keyword-regexp (regexp-opt slang-keywords 'words))
        (storage-modifier-regexp (regexp-opt slang-storage-modifiers 'words))
        (type-regexp (regexp-opt slang-types 'words))
        (constant-regexp (regexp-opt slang-constants 'words))
        (operator-regexp (regexp-opt slang-operators 'words))
        (shader-stage-regexp (regexp-opt slang-shader-stages 'words)))
    `(
      ;; Preprocessor directives
      (,(concat "^[ \t]*#[ \t]*\\(" (regexp-opt slang-preprocessor-directives) "\\)\\b")
       (1 font-lock-preprocessor-face))

      ;; Preprocessor macro names
      ("^[ \t]*#[ \t]*define[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1 font-lock-variable-name-face)

      ;; Documentation comments (///, //!, /** ... */)
      ("\\(///.*$\\)" 1 font-lock-doc-face)
      ("\\(//!.*$\\)" 1 font-lock-doc-face)

      ;; Control flow keywords
      (,keyword-regexp . font-lock-keyword-face)

      ;; Storage modifiers
      (,storage-modifier-regexp . font-lock-type-face)

      ;; Built-in types
      (,type-regexp . font-lock-type-face)

      ;; Vector and matrix types (e.g., float3, int4x4)
      ("\\<\\(bool\\|char\\|half\\|float\\|int\\|double\\|uint\\)[1-4]\\(x[1-4]\\)?\\>"
       . font-lock-type-face)

      ;; HLSL/Slang buffer types
      ("\\<\\(RW\\)?\\(Structured\\|Byte\\|Constant\\|Parameter\\)?\\(Buffer\\|Block\\)\\>"
       . font-lock-type-face)
      ("\\<RWByteAddressBuffer\\>" . font-lock-type-face)
      ("\\<ByteAddressBuffer\\>" . font-lock-type-face)

      ;; Texture types
      ("\\<\\(RW\\)?Texture\\([1-3]D\\|Cube\\)\\(MS\\)?\\(Array\\)?\\>"
       . font-lock-type-face)

      ;; Sampler types
      ("\\<Sampler\\(Comparison\\)?State\\>" . font-lock-type-face)
      ("\\<RaytracingAccelerationStructure\\>" . font-lock-type-face)

      ;; Struct, class, interface, enum, namespace declarations
      ("\\<\\(struct\\|class\\|interface\\|enum\\|namespace\\)[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
       (1 font-lock-keyword-face)
       (2 font-lock-type-face))

      ;; Extension declarations
      ("\\<extension[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
       (1 font-lock-type-face))

      ;; Constants
      (,constant-regexp . font-lock-constant-face)

      ;; Shader stages
      (,shader-stage-regexp . font-lock-constant-face)

      ;; Keyword operators
      (,operator-regexp . font-lock-keyword-face)

      ;; Numeric literals (hexadecimal)
      ("\\<0[xX][0-9a-fA-F]+\\([uU]\\|[lL]\\|[uU][lL]\\|[lL][uU]\\|[fFhHzZ]\\)?\\>"
       . font-lock-constant-face)

      ;; Numeric literals (binary)
      ("\\<0[bB][01]+\\([uU]\\|[lL]\\|[uU][lL]\\|[lL][uU]\\)?\\>"
       . font-lock-constant-face)

      ;; Numeric literals (decimal)
      ("\\<[0-9]+\\(\\.[0-9]+\\)?\\([eE][+-]?[0-9]+\\)?\\([fFhHlLuUzZ]\\)?\\>"
       . font-lock-constant-face)

      ;; Function declarations and calls
      ("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \t]*(" 1 font-lock-function-name-face)

      ;; Attributes (e.g., [numthreads(8,8,1)])
      ("\\[\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1 font-lock-preprocessor-face)

      ;; HLSL semantics (e.g., : SV_Position)
      (":[ \t]*\\([A-Z_][A-Z0-9_]*\\)" 1 font-lock-builtin-face)

      ;; Member access highlighting
      ("\\.\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1 font-lock-variable-name-face)
      ))
  "Font lock keywords for Slang mode.")

;;; Syntax table

(defvar slang-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; C-style comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\"" table)

    ;; Operators
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?^ "." table)
    (modify-syntax-entry ?! "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?? "." table)
    (modify-syntax-entry ?: "." table)

    ;; Parentheses and brackets
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)

    ;; Underscore is part of word
    (modify-syntax-entry ?_ "w" table)

    ;; Preprocessor
    (modify-syntax-entry ?# "." table)

    table)
  "Syntax table for Slang mode.")

;;; Indentation

(defun slang-indent-line ()
  "Indent the current line as Slang code.
This is a simple indentation based on C-style brace matching."
  (interactive)
  (let ((indent-col 0)
        (cur-indent 0))
    (save-excursion
      (beginning-of-line)
      (if (bobp)
          (setq indent-col 0)
        (let ((not-indented t))
          ;; If current line closes a block, unindent
          (if (looking-at "^[ \t]*\\(}\\|)\\|]\\)")
              (progn
                (save-excursion
                  (forward-line -1)
                  (setq indent-col (max 0 (- (current-indentation) slang-indent-offset))))
                (setq not-indented nil))
            ;; Otherwise, check previous lines
            (while not-indented
              (forward-line -1)
              (if (bobp)
                  (setq not-indented nil)
                (if (looking-at "^[ \t]*\\(}\\|)\\|]\\)")
                    (setq indent-col (current-indentation)
                          not-indented nil)
                  (if (looking-at ".*\\({\\|(\\|\\[\\)[ \t]*\\(//.*\\)?$")
                      (setq indent-col (+ (current-indentation) slang-indent-offset)
                            not-indented nil)
                    (if (bobp)
                        (setq not-indented nil))))))))))

    (if (< indent-col 0)
        (setq indent-col 0))

    (save-excursion
      (beginning-of-line)
      (if (looking-at "^[ \t]+")
          (replace-match ""))
      (indent-to indent-col))

    ;; Move point to indentation if before it
    (if (< (current-column) indent-col)
        (move-to-column indent-col))))

;;; Imenu support

(defvar slang-imenu-generic-expression
  '(("Functions" "^[ \t]*\\(?:.*[ \t]+\\)?\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \t]*([^)]*)[^{]*{" 1)
    ("Structs" "^[ \t]*struct[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1)
    ("Classes" "^[ \t]*class[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1)
    ("Interfaces" "^[ \t]*interface[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1)
    ("Enums" "^[ \t]*enum[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1)
    ("Namespaces" "^[ \t]*namespace[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1))
  "Imenu generic expression for Slang mode.")

;;; Mode definition

(defvar slang-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'newline-and-indent)
    (define-key map (kbd "C-c C-c") 'comment-region)
    (define-key map (kbd "C-c C-u") 'uncomment-region)
    map)
  "Keymap for Slang mode.")

;;;###autoload
(define-derived-mode slang-mode prog-mode "Slang"
  "Major mode for editing Slang shader files.

Slang is a shading language that extends HLSL with modern programming
language features including generics, interfaces, and modules.

Key bindings:
\\{slang-mode-map}"
  :syntax-table slang-mode-syntax-table

  ;; Font lock
  (setq-local font-lock-defaults '(slang-font-lock-keywords nil nil))

  ;; Comments
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "\\(?://+\\|/\\*+\\)\\s *")
  (setq-local comment-multi-line t)

  ;; Indentation
  (setq-local indent-line-function 'slang-indent-line)
  (setq-local tab-width slang-indent-offset)
  (setq-local indent-tabs-mode nil)

  ;; Electric indentation
  (setq-local electric-indent-chars (append "{}():;" electric-indent-chars))

  ;; Imenu
  (setq-local imenu-generic-expression slang-imenu-generic-expression)
  (setq-local imenu-case-fold-search nil)

  ;; Which-function mode support
  (setq-local which-func-functions nil)

  ;; Syntax
  (setq-local parse-sexp-ignore-comments t)

  ;; Paragraph filling
  (setq-local comment-line-break-function 'c-indent-new-comment-line)
  (setq-local fill-paragraph-function 'c-fill-paragraph)

  ;; Movement
  (setq-local beginning-of-defun-function 'slang-beginning-of-defun)
  (setq-local end-of-defun-function 'slang-end-of-defun))

;;; Navigation functions

(defun slang-beginning-of-defun (&optional arg)
  "Move backward to the beginning of a function.
With ARG, do it that many times."
  (interactive "p")
  (unless arg (setq arg 1))
  (let ((case-fold-search nil))
    (if (< arg 0)
        (slang-end-of-defun (- arg))
      (dotimes (_ arg)
        (re-search-backward
         "^[ \t]*\\(?:[a-zA-Z_][a-zA-Z0-9_<>]*[ \t]+\\)*[a-zA-Z_][a-zA-Z0-9_]*[ \t]*([^)]*)[^{]*{"
         nil 'move)))))

(defun slang-end-of-defun (&optional arg)
  "Move forward to the end of a function.
With ARG, do it that many times."
  (interactive "p")
  (unless arg (setq arg 1))
  (let ((case-fold-search nil))
    (if (< arg 0)
        (slang-beginning-of-defun (- arg))
      (dotimes (_ arg)
        (re-search-forward
         "^[ \t]*\\(?:[a-zA-Z_][a-zA-Z0-9_<>]*[ \t]+\\)*[a-zA-Z_][a-zA-Z0-9_]*[ \t]*([^)]*)[^{]*{"
         nil 'move)
        (when (looking-at "")
          (forward-sexp))))))

;;; File association

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.slang\\'" . slang-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.slh\\'" . slang-mode))

;;; Additional utilities
(defun slang-insert-header-guard ()
  "Insert a header guard for the current file."
  (interactive)
  (let* ((filename (file-name-nondirectory (buffer-file-name)))
         (guard (upcase (replace-regexp-in-string "[^A-Z0-9]" "_" filename))))
    (save-excursion
      (goto-char (point-min))
      (insert (format "#ifndef %s\n#define %s\n\n" guard guard))
      (goto-char (point-max))
      (insert (format "\n#endif // %s\n" guard)))))

(defun slang-comment-region-with-doc (beg end)
  "Comment region from BEG to END with documentation-style comments."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (insert "/*!\n")
    (goto-char (+ end 4))
    (insert "\n*/")))

(provide 'slang-mode)
;;; slang-mode.el ends here
