;;; emacs-woodshed.el --- Helps practicing your musical instrument
;;; Commentary:
;; This package helps western style harmony based instrument players to practice
;; their chops.
;; Use `woodshed/start-practicing-arpeggios` to start practicing arpeggios.
;;; Code:

(require 'seq)

(define-derived-mode woodshed-mode special-mode "woodshed-mode"
  "Mode for showing outputted practice sheets.")

(defface woodshed-default
  '((t :family "Monospace"
       :weight medium))
  "The basic fixed-pitch face for woodshed buffers."
  :group 'basic-faces)

(defvar woodshed/practice-buffer-name "*arpeggio-practice*")

(defvar woodshed/notes '("C"
                         ("C♯" "D♭")
                         "D"
                         ("D♯" "E♭")
                         "E"
                         "F"
                         ("F♯" "G♭")
                         "G"
                         ("G♯" "A♭")
                         "A"
                         ("A♯" "B♭")
                         "B")
  "Notes and their enharmonic aliases in the western musical notation.")

(defvar woodshed/major-scale-intervals '(2 2 1 2 2 2))

(defun woodshed/note-equal (note1 note2)
  "Check if NOTE1 and NOTE2 are the same note."
  (if (listp note2)
      (or (string-equal note1 (car note2))
          (string-equal note1 (cadr note2)))
    (string-equal note1 note2)))

(defun woodshed/scale (root)
  "Output a major scale in the given ROOT."
  (let* ((position (seq-position woodshed/notes root))
         (extended-notes (seq-concatenate 'list woodshed/notes woodshed/notes))
         (positions (reverse
                     (seq-reduce
                      (lambda (scale interval)
                        (cons (+ (car scale) interval) scale))
                      woodshed/major-scale-intervals
                      (list position)))))
    (seq-map
     (apply-partially 'seq-elt extended-notes)
     positions)))

(defun woodshed/arpeggios (scale)
  "Output arpeggios in the given SCALE."
  (let ((extended-scale (seq-concatenate 'list scale scale)))
    (seq-map-indexed
     (lambda (note i)
       (list
        note
        (nth (+ i 2) extended-scale)
        (nth (+ i 4) extended-scale)))
     scale)))

(defun woodshed/pprint-note (note)
  "Stringify a NOTE."
  (if (listp note)
      (string-join note "/")
    note))

(defun woodshed/parse-note (note-string)
  "Parse NOTE-STRING into internal representation of the note."
  (if (string-match (rx "/") note-string)
      (split-string note-string "/")
    note-string))

(defun woodshed/pprint-chord (chord)
  "Stringify a CHORD."
  (concat
   "| "
   (string-join
    (seq-map
     (lambda (note)
       (let ((note-as-string (woodshed/pprint-note note)))
         (while (> 5 (length note-as-string))
           (set 'note-as-string (concat note-as-string " ")))
         note-as-string))
     chord)
    " ")
   " |"))

(defun woodshed/first-inversion (chord)
  "Return the first inversion of a CHORD."
  (seq-concatenate 'list (seq-rest chord) (seq-take chord 1)))

(defun woodshed/second-inversion (chord)
  "Return the second inversion of a CHORD."
  (woodshed/first-inversion
   (woodshed/first-inversion chord)))

(defun woodshed/third-inversion (chord)
  "Return the third inversion of a CHORD."
  (woodshed/first-inversion
   (woodshed/first-inversion
    (woodshed/first-inversion chord))))

(defun woodshed/render-practice-buffer (render-fn)
  "Rerender practice buffer.

Call RENDER-FN in the context of the practice buffer."
  (with-current-buffer (get-buffer-create woodshed/practice-buffer-name)
    (woodshed-mode)
    (buffer-face-set 'woodshed-default)
    (read-only-mode -1)
    (delete-region (point-min) (point-max))

    (funcall render-fn)

    (switch-to-buffer-other-window woodshed/practice-buffer-name)))

(defun woodshed/render-triads-and-inversions (root)
  "Render all triads and their inversions in a major scale starting at ROOT."
  (let ((chords (woodshed/arpeggios (woodshed/scale root))))
    (insert "Basic chords:\n")
    (dolist (chord chords)
      (insert (woodshed/pprint-chord chord)
              "\n"))

    (insert "\nFirst inversions:\n")
    (dolist (chord chords)
      (insert (woodshed/pprint-chord (woodshed/first-inversion chord))
              "\n"))

    (insert "\nSecond inversions:\n")
    (dolist (chord chords)
      (insert (woodshed/pprint-chord (woodshed/second-inversion chord))
              "\n"))))

(defun woodshed/arpeggio-practice (root)
  "Create a practice sheet to a buffer for chords in the given ROOT."
  (woodshed/render-practice-buffer
   (lambda ()
     (woodshed/render-triads-and-inversions root))))

(defun woodshed/start-practicing-arpeggios ()
  "Interactive version which asks which root you want to practice on."
  (interactive)
  (let ((root-note (completing-read
                    "Choose scale"
                    (mapcar 'woodshed/pprint-note woodshed/notes)
                    nil t)))
    (woodshed/arpeggio-practice
     (woodshed/parse-note root-note))))

(defun woodshed/circle-of-fifths ()
  "Generate circle of fifths starting from C."
  (seq-map (apply-partially 'seq-elt
                            woodshed/notes)
           (seq-map (lambda (note)
                      (% (* 7  note) 12))
                    (number-sequence 0 11))))

(defun woodshed/exercise-circle-of-fifths ()
  "Start practicing all arpeggios in circle of fifths."
  (interactive)
  (woodshed/render-practice-buffer
   (lambda ()
     (seq-do
      (lambda (root)
        (insert "======= " (woodshed/pprint-note root) " major =======\n")
        (woodshed/render-triads-and-inversions root)
        (insert "\n"))
      (woodshed/circle-of-fifths)))))



(provide 'emacs-woodshed)
;;; emacs-woodshed.el ends here
