;;; email.macros.el --- Description -*- lexical-binding: t; -*-

(defmacro define-mu4e-search-fn (name docstring bookmark-string)
  "Generate a mu4e search function."
  `(defun ,name ()
     ,docstring
     (interactive)
     (unless (featurep 'mu4e)
       (require 'mu4e))
     (mu4e t)
     (mu4e-search-bookmark ,bookmark-string)))
