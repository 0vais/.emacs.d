;;; jekyll-modes.el --- Major modes (markdown and HTML) for authoring Jekyll content

;; Copyright (C) 2014  Fredrik Appelberg

;; Author: Fredrik Appelberg <fredrik@milgrim.local>
;; Keywords: docs
;; Package-Version: 20141117.514
;; URL: https://github.com/fred-o/jekyll-modes
;; Version: 0.0.1
;; Package-Requires: ((polymode "0.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides major modes for authoring Jekyll Markdown and
;; HTML documents, with syntax support for YAML front matter, Liquid
;; tags and embedded code snippets.
;;
;; As this package depends on polymode, Emacs 24 is required.
;;
;; The package includes two modes, `jekyll-markdown-mode` and
;; `jekyll-html-mode`, which can be enabled as normal by adding the
;; following to you init file:
;;
;;     (add-to-list 'auto-mode-alist '("\\.md$" . jekyll-markdown-mode))
;;     (add-to-list 'auto-mode-alist '("\\.html" . jekyll-html-mode))
;;
 
;;; Code:

(require 'polymode)
(require 'poly-markdown)
(require 'liquid-mode)

;; This code is Jekyll mode taken from "https://github.com/fred-o/jekyll-modes"
;; Jekyll modes

(defcustom jekyll/yaml-frontmatter
  (pm-hbtchunkmode "yaml"
                   :mode 'yaml-mode
                   :head-reg "\\`---\n"
                   :tail-reg "[^\\`]---\n")
  "Yaml chunk"
  :group 'innermodes
  :type 'object)

(defcustom jekyll/liquid-tag
  (pm-hbtchunkmode "liquid"
                   :mode 'liquid-mode
                   :head-reg "{%"
                   :tail-reg "%}")
  "Liquid tag"
  :group 'innermodes
  :type 'object)

(defcustom jekyll/liquid-expression
  (pm-hbtchunkmode "liquid"
                   :mode 'liquid-mode
                   :head-reg "{{"
                   :tail-reg "}}")
  "Liquid expression"
  :group 'innermodes
  :type 'object)

(defcustom jekyll/math
  (pm-hbtchunkmode "tex"
                   :mode 'tex-mode
                   :head-reg "^\\$\\$\\\\,"
                   :tail-reg "\\\\,\\$\\$$")
  "Liquid expression"
  :group 'innermodes
  :type 'object)

(defcustom jekyll/pygments
  (pm-hbtchunkmode-auto "pygments"
                        :head-reg "{%\s*highlight\s*\\(\\w+\\)\s*\\w*\s*%}"
                        :tail-reg "{%\s*endhighlight\s*%}"
                        :head-mode 'liquid-mode
                        :tail-mode 'liquid-mode
                        :retriever-regexp "{%\s*highlight\s*\\(\\w+\\)\s*\\w*\s*%}"
                        :font-lock-narrow t)
  "Pygments chunk"
  :group 'innermodes
  :type 'object)

(defcustom jekyll/markdown 
  (pm-polymode-multi-auto "markdown"
                          :hostmode 'pm-host/markdown
                          :auto-innermode 'jekyll/pygments
                          :innermodes '(jekyll/yaml-frontmatter
                                        jekyll/liquid-expression
                                        jekyll/liquid-tag
										jekyll/math))
  "Markdown with YAML frontmatter and Liquid tags support."
  :group 'polymodes
  :type 'object)

(defcustom jekyll/html
  (pm-polymode-multi "html"
                     :hostmode 'pm-host/html
                     :innermodes '(jekyll/yaml-frontmatter
                                   jekyll/liquid-expression
                                   jekyll/liquid-tag))

  "HTML with YAML frontmatter and Liquid tags support."
  :group 'polymodes
  :type 'object)

;;;###autoload (autoload 'jekyll-markdown-mode "jekyll-modes")
(define-polymode jekyll-markdown-mode jekyll/markdown)

;;;###autoload (autoload 'jekyll-html-mode "jekyll-modes")
(define-polymode jekyll-html-mode jekyll/html)

(provide 'jekyll-modes)
;;; jekyll-modes.el ends here
