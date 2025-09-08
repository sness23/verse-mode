;;; test-verse-mode.el --- Test script for verse-mode

;; Load verse-mode
(load-file "verse-mode.el")

;; Open the test file
(find-file "examples/button_puzzle.verse")

;; Check if verse-mode is active
(message "Current major mode: %s" major-mode)
(message "Font lock keywords: %s" (car font-lock-defaults))
(message "Font lock mode active: %s" font-lock-mode)

;; Force font-lock if needed
(font-lock-mode 1)
(font-lock-fontify-buffer)

;; Test some syntax highlighting
(save-excursion
  (goto-char (point-min))
  (when (search-forward "using" nil t)
    (message "'using' keyword highlighted: %s" 
             (get-text-property (- (point) 1) 'face)))
  
  (goto-char (point-min))
  (when (search-forward "class" nil t)
    (message "'class' keyword highlighted: %s" 
             (get-text-property (- (point) 1) 'face)))
  
  (goto-char (point-min))
  (when (search-forward "# Derive" nil t)
    (message "Comment highlighted: %s" 
             (get-text-property (point) 'face))))

(message "Test completed. Check the *Messages* buffer for results.")