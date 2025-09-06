;;; debug-syntax.el --- Debug syntax properties

;; Load verse-mode
(load-file "verse-mode.el")

;; Open the test file
(find-file "examples/button_puzzle.verse")

;; Go to line 8 where 'class' appears
(goto-line 8)
(beginning-of-line)

;; Print the line content
(message "Line 8 content: %s" (buffer-substring (line-beginning-position) (line-end-position)))

;; Check syntax properties around this area
(let ((start (line-beginning-position))
      (end (line-end-position)))
  (dotimes (i (- end start))
    (let ((pos (+ start i))
          (char (char-after (+ start i))))
      (when char
        (let ((syntax-prop (get-text-property pos 'syntax-table)))
          (when syntax-prop
            (message "Position %d (char '%c'): syntax-table = %s" pos char syntax-prop)))))))

;; Force syntax propertize and check again
(syntax-propertize (point-max))

;; Find the 'class' keyword
(beginning-of-line)
(when (search-forward "class" (line-end-position) t)
  (let ((class-pos (- (point) 1)))
    (message "'class' at position %d" class-pos)
    (message "Syntax class at 'class': %s" (syntax-class (syntax-after class-pos)))
    (message "In comment?: %s" (nth 4 (syntax-ppss class-pos)))
    (message "Face at 'class': %s" (get-text-property class-pos 'face))))

(message "Debug completed.")