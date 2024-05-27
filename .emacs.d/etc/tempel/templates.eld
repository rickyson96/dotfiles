emacs-lisp-mode

(defadvice "(define-advice " (p "fun")
  " (" (p "how?") " (" (p "&rest _") ") " (p "name") ")" n>
  "\"" (p "docstring") "\"" n>
  p ")")

(adv "(define-advice " (p "fun")
  " (" (p "how?") " (" (p "&rest _") ") " (p "name") ")" n>
  "\"" (p "docstring") "\"" n>
  p ")")
