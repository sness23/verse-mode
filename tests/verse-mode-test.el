;;; verse-mode-test.el --- Tests for verse-mode -*- lexical-binding: t; -*-

(require 'ert)
(require 'verse-mode)

(ert-deftest verse-mode-font-lock-smoke ()
  "Ensure major mode enables without error."
  (with-temp-buffer
    (insert "using { /Verse.org/Random }\nvar X : int = 0\nif (X > 0):\n  X\n<# block #>\n")
    (goto-char (point-min))
    (verse-mode)
    (font-lock-ensure)
    (should (eq major-mode 'verse-mode))))

(provide 'verse-mode-test)
;;; verse-mode-test.el ends here
