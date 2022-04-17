;;; emacs-woodshed.el --- Helps practicing your musical instrument
;;; Commentary:
;; This package helps western style harmony based instrument players to practice
;; their chops.
;;; Code:

(require 'ivy)

(defvar woodshed/practice-buffer-name "*arpeggio-practice*")

(defvar woodshed/notes '("C"
                         ("C#" "Db")
                         "D"
                         ("D#" "Eb")
                         "E"
                         "F"
                         ("F#" "Gb")
                         "G"
                         ("G#" "Ab")
                         "A"
                         ("A#" "Bb")
                         "B")
  "Notes and their enharmonic aliases in the western musical notation.")

(defun woodshed/note-equal (note1 note2)
  "Check if NOTE1 and NOTE2 are the same note."
  (if (listp note2)
      (or (string-equal note1 (car note2))
          (string-equal note1 (cadr note2)))
    (string-equal note1 note2)))

(defun woodshed/scale (root)
  "Output a major scale in the given ROOT."
  (let ((notes '("C"
                 ("C#" "Db")
                 "D"
                 ("D#" "Eb")
                 "E"
                 "F"
                 ("F#" "Gb")
                 "G"
                 ("G#" "Ab")
                 "A"
                 ("A#" "Bb")
                 "B"))
        (major '(2 2 1 2 2 2)))
    (let ((position 0))
      (while (or (not (let* ((note (nth position notes)))
                        (woodshed/note-equal root note)))
                 (>= position (length notes)))
        (set 'position (+ 1 position)))

      (let ((scale (list (nth position notes))))
        (dolist (interval major)
          (set 'position (+ interval position))
          (when (>= position (length notes))
            (set 'position (- position (length notes))))
          (set 'scale (cons (nth position notes) scale)))
        (reverse scale)))))

(defun woodshed/arpeggios (scale)
  "Output arpeggios in the given SCALE."
  (let ((extended-scale (seq-concatenate 'list scale scale)))
    (seq-map-indexed
     (lambda (note i)
       (list
        (nth i extended-scale)
        (nth (+ i 2) extended-scale)
        (nth (+ i 4) extended-scale)))
     scale)))

(defun woodshed/pprint-note (note)
  "Stringify a NOTE."
  (if (listp note)
      (string-join note "/")
    note))

(defun woodshed/pprint-chord (chord)
  "Stringify a CHORD."
  (string-join
   (list
    (woodshed/pprint-note (car chord))
    (woodshed/pprint-note (cadr chord))
    (woodshed/pprint-note (caddr chord)))
   " "))

(defun woodshed/first-inversion (chord)
  "Return the first inversion of a CHORD.

Supports only triads at the moment."
  (list (cadr chord)
        (caddr chord)
        (car chord)))

(defun woodshed/second-inversion (chord)
  "Return the second inversion of a CHORD.

Supports only triads at the moment."
  (list (caddr chord)
        (car chord)
        (cadr chord)))

(defun woodshed/arpeggio-practice (root)
  "Create a practice sheet to a buffer for chords in the given ROOT."
  (with-current-buffer (get-buffer-create woodshed/practice-buffer-name)
    (delete-region (point-min) (point-max))
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
                "\n")))

    (switch-to-buffer-other-window woodshed/practice-buffer-name)))


(defun woodshed/start-practicing-arpeggios ()
  "Interactive version which asks which root you want to practice on."
  (interactive)
  (ivy-read "Choose scale"
            (mapcar 'woodshed/pprint-note woodshed/notes)
            :require-match t
            :action 'woodshed/arpeggio-practice))

(provide 'emacs-woodshed)
;;; emacs-woodshed.el ends here
