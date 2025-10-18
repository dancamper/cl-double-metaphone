;;;; double-metaphone.lisp

;;;; Pure Common Lisp implementation of Double Metaphone
;;;; based on the "slightly modified version" of the Double Metaphone algorithm
;;;; found at http://aspell.net/metaphone/

(in-package #:double-metaphone)

;;; ----------------------------------------------------------------------------
;;; Main algorithm

(defun double-metaphone (word)
  "Return two phonetic encodings (primary and secondary) for WORD using the
Double Metaphone algorithm.

WORD must be a string. The function returns two values:
1. The primary Double Metaphone key — the most likely phonetic representation.
2. The secondary Double Metaphone key — an alternate representation for
words with ambiguous or variant pronunciations.

Double Metaphone expects ASCII, so you may need to normalize your WORD before
submitting it.

Both keys are uppercase strings containing only ASCII letters. These encodings
are suitable for phonetic comparison, fuzzy matching, and indexing of English
words and names."
  (let* ((word (string-upcase word))
         (len (length word))
         (primary (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
         (secondary (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
         (index 0)
         (slavo-germanic-p (or (position #\W word :test #'char-equal)
                               (position #\K word :test #'char-equal)
                               (search "CZ" word)
                               (search "WITZ" word))))
    ;; Utility functions that reference variables defined above
    (labels ((word-char (pos)
               "Return character at absolute position POS from WORD or nil"
               (and (>= pos 0) (< pos len)
                    (char word pos)))
             (offset-char (&optional (offset 0))
               "Return character at relative position OFFSET (relative to position INDEX) within WORD or nil"
               (word-char (+ index offset)))
             (append-char (ch1 &optional (ch2 ch1))
               "Appends CH1 to primary result and CH2 to secondary result; CH2 defaults to CH1 if not provided"
               (vector-push-extend ch1 primary)
               (vector-push-extend ch2 secondary))
             (append-str (s1 &optional (s2 s1))
               "Appends S1 to primary result and S2 to secondary result; S2 defaults to S1 if not provided"
               (loop :for c :across s1
                     :do (vector-push-extend c primary))
               (loop :for c :across s2
                     :do (vector-push-extend c secondary)))
             (char-same-p (ch1 ch2)
               "True if CH1 is non-nil and same character as CH2"
               (and ch1 ch2 (char-equal ch1 ch2)))
             (char-member-p (ch char-bag)
               "True if CH is non-nil and a member of the list CHAR-BAG"
               (and ch (member ch char-bag :test #'char-equal)))
             (vowel-p (ch)
               "True if CH is non-nil and an English vowel ('Y' is excluded)"
               (and ch (member ch '(#\A #\E #\I #\O #\U) :test #'char-equal)))
             (starts-with-str (prefix &optional (offset 0))
               "True if PREFIX is found at relative position OFFSET (relative to position INDEX) within WORD"
               (let ((plen (length prefix))
                     (pos (+ index offset)))
                 (and (>= pos 0)
                      (<= (+ pos plen) len)
                      (string-equal (subseq word pos (+ pos plen)) prefix))))
             (starts-with-any (prefix-list &optional (offset 0))
               "True if any prefix within PREFIX-LIST is found at relative position OFFSET (relative to position INDEX) within WORD"
               (some (lambda (p) (starts-with-str p offset)) prefix-list)))
      (when (plusp len)
        ;; Initial beginning-of-string tests
        (cond ((starts-with-any '("GN" "KN" "PN" "WR" "PS"))
               ;; These are silent at the beginning of a word
               (incf index))
              ((member (word-char index) '(#\A #\E #\I #\O #\U #\Y) :test #'char-equal)
               ;; Vowels at the beginning of a word are all mapped to 'A'
               (append-char #\A)
               (incf index))
              ((char-same-p (word-char index) #\X)
               ;; 'X' at the beginning maps to 'S'
               (append-char #\S)
               (incf index)))

        (loop :while (< index len)
              :for ch = (word-char index)
              :for next-ch = (offset-char 1)
              :for prev-ch = (offset-char -1)
              :do (case ch
                    (#\B
                     (append-char #\P)
                     (if (char-same-p next-ch #\B)
                         (incf index 2)
                         (incf index)))
                    (#\C
                     (cond ((and (not (vowel-p (offset-char -2))) ; various germanic
                                 (starts-with-str "ACH" -1)
                                 (not (char-same-p (offset-char 2) #\I))
                                 (or (not (char-same-p (offset-char 2) #\E))
                                     (starts-with-any '("BACHER" "MACHER") -2)))
                            (append-char #\K)
                            (incf index 2))
                           ((and (zerop index) ; special case 'caesar'
                                 (starts-with-str "CAESAR"))
                            (append-char #\S)
                            (incf index 2))
                           ((starts-with-str "CHIA"); italian 'chianti'
                            (append-char #\K)
                            (incf index 2))
                           ((starts-with-str "CH")
                            (cond ((and (plusp index) ; find 'michael'
                                        (starts-with-str "CHAE"))
                                   (append-char #\K #\X)
                                   (incf index 2))
                                  ((and (zerop index)
                                        (starts-with-any '("HARAC" "HARIS" "HOR" "HYM" "HIA" "HEM") 1)
                                        (not (starts-with-str "CHORE" (- index)))) ; greek roots e.g. 'chemistry', 'chorus'
                                   (append-char #\K)
                                   (incf index 2))
                                  ((or (starts-with-any '("VAN " "VON " "SCH") (- index)) ; germanic, greek, or otherwise 'ch' for 'kh' sound
                                       (starts-with-any '("ORCHES" "ARCHIT" "ORCHID") -2) ; 'architect but not 'arch', 'orchestra', 'orchid'
                                       (and (or (char-member-p prev-ch '(#\A #\O #\U #\E))
                                                (zerop index))
                                            (char-member-p (offset-char 2) '(#\L #\R #\N #\M #\B #\H #\F #\V #\W)))) ; e.g., 'wachtler', 'wechsler', but not 'tichner'
                                   (append-char #\K)
                                   (incf index 2))
                                  ((plusp index)
                                   (if (starts-with-str "MC" (- index)) ; e.g., "McHugh"
                                       (append-char #\K)
                                       (append-char #\X #\K))
                                   (incf index 2))
                                  (t
                                   (append-char #\X)
                                   (incf index 2))))
                           ((and (starts-with-str "CZ") ; e.g, 'czerny'
                                 (not (starts-with-str "WICZ" -2)))
                            (append-char #\S #\X)
                            (incf index 2))
                           ((starts-with-str "CIA" 1) ; e.g., 'focaccia'
                            (append-char #\X)
                            (incf index 3))
                           ((and (starts-with-str "CC") ; double 'C', but not if e.g. 'McClellan'
                                 (not (char-same-p prev-ch #\M)))
                            (if (and (char-member-p (offset-char 2) '(#\I #\E #\H)) ; 'bellocchio' but not 'bacchus'
                                     (not (starts-with-str "HU" 2)))
                                (progn
                                  (if (or (char-same-p prev-ch #\A) ; 'accident', 'accede' 'succeed'
                                          (starts-with-any '("UCCEE" "UCCES") -1))
                                      (append-str "KS")
                                      (append-char #\X)) ; 'bacci', 'bertucci', other italian
                                  (incf index 3))
                                (progn
                                  ;; Pierce's rule
                                  (append-char #\K)
                                  (incf index 2))))
                           ((starts-with-any '("CK" "CG" "CQ"))
                            (append-char #\K)
                            (incf index 2))
                           ((starts-with-any '("CI" "CE" "CY")) ; italian vs. english
                            (if (starts-with-any '("CIO" "CIE" "CIA"))
                                (append-char #\S #\X)
                                (append-char #\S))
                            (incf index 2))
                           (t
                            (append-char #\K)
                            (cond ((starts-with-any '(" C" " Q" " G") 1) ; name sent in 'mac caffrey', 'mac gregor
                                   (incf index 3))
                                  ((and (char-member-p next-ch '(#\C #\K #\Q))
                                        (not (starts-with-any '("CE" "CI") 1)))
                                   (incf index 2))
                                  (t
                                   (incf index))))))
                    (#\D
                     (cond ((starts-with-str "DG")
                            (if (char-member-p (offset-char 2) '(#\E #\I #\Y))
                                (progn
                                  ;; e.g. 'edge'
                                  (append-char #\J)
                                  (incf index 3))
                                (progn
                                  ;; e.g. 'edgar'
                                  (append-str "TK")
                                  (incf index 2))))
                           ((or (starts-with-str "DT")
                                (starts-with-str "DD"))
                            (append-char #\T)
                            (incf index 2))
                           (t
                            (append-char #\T)
                            (incf index))))
                    (#\F
                     (append-char #\F)
                     (if (char-same-p next-ch #\F)
                         (incf index 2)
                         (incf index)))
                    (#\G
                     (cond ((char-same-p next-ch #\H)
                            (cond ((and (plusp index) (not (vowel-p prev-ch)))
                                   (append-char #\K)
                                   (incf index 2))
                                  ((zerop index) ; 'ghislane', ghiradelli
                                   (if (char-same-p (offset-char 2) #\I)
                                       (append-char #\J)
                                       (append-char #\K))
                                   (incf index 2))
                                  ((or (char-member-p (offset-char -2) '(#\B #\H #\D)) ; Parker's rule (with some further refinements) - e.g., 'hugh'
                                       (char-member-p (offset-char -3) '(#\B #\H #\D))
                                       (char-member-p (offset-char -4) '(#\B #\H)))
                                   (incf index 2))
                                  ((and (> index 2) ; e.g., 'laugh', 'McLaughlin', 'cough', 'gough', 'rough', 'tough'
                                        (char-same-p prev-ch #\U)
                                        (char-member-p (offset-char -3) '(#\C #\G #\L #\R #\T)))
                                   (append-char #\F)
                                   (incf index 2))
                                  ((and (plusp index)
                                        (not (char-same-p prev-ch #\I)))
                                   (append-char #\K)
                                   (incf index 2))
                                  (t
                                   (incf index 2))))
                           ((char-same-p next-ch #\N)
                            (cond ((and (= index 1)
                                        (vowel-p (char word 0))
                                        (not slavo-germanic-p))
                                   (append-str "KN" "N"))
                                  ((and (starts-with-str "EY") ; not e.g. 'cagney'
                                        (not (char-same-p next-ch #\Y))
                                        (not slavo-germanic-p))
                                   (append-str "N" "KN"))
                                  (t
                                   (append-str "KN")))
                            (incf index 2))
                           ((and (starts-with-str "LI" 1) ; 'tagliaro'
                                 (not slavo-germanic-p))
                            (append-str "KL" "L")
                            (incf index 2))
                           ((and (zerop index) ; -ges-,-gep-,-gel-, -gie- at beginning
                                 (or (char-same-p next-ch #\Y)
                                     (starts-with-any '("ES" "EP" "EB" "EL" "EY" "IB" "IL" "IN" "IE" "EI" "ER") 1)))
                            (append-char #\K #\J)
                            (incf index 2))
                           ((and (or (starts-with-str "ER" 1) ; -ger-,  -gy-
                                     (char-same-p next-ch #\Y))
                                 (not (starts-with-any '("DANGER" "RANGER" "MANGER") (- index)))
                                 (not (starts-with-any '("E" "I" "RGY" "OGY") -1)))
                            (append-char #\K #\J)
                            (incf index 2))
                           ((or (char-member-p next-ch '(#\E #\Y #\I)) ; italian e.g, 'biaggi'
                                (starts-with-any '("AGGI" "OGGI") -1))
                            (cond ((or (starts-with-any '("VAN " "VON " "SCH") (- index)) ; obvious germanic
                                       (starts-with-str "ET" 1))
                                   (append-char #\K))
                                  ((starts-with-str "IER" 1) ; always soft if french ending
                                   (append-char #\J))
                                  (t
                                   (append-char #\J #\K)))
                            (incf index 2))
                           ((char-same-p next-ch #\G)
                            (append-char #\K)
                            (incf index 2))
                           (t
                            (append-char #\K)
                            (incf index))))
                    (#\H
                     (if (and (or (zerop index) ; only keep if first & before vowel or btw. 2 vowels
                                  (vowel-p prev-ch))
                              (vowel-p next-ch))
                         (progn
                           (append-char #\H)
                           (incf index 2))
                         (incf index)))
                    (#\J
                     (cond ((or (starts-with-str "JOSE") ; obvious spanish, 'jose', 'san jacinto'
                                (starts-with-str "SAN " (- index)))
                            (if (or (and (zerop index)
                                         (char-same-p (offset-char 4) #\space))
                                    (starts-with-str "SAN " (- index)))
                                (append-char #\H)
                                (append-char #\J #\H))
                            (incf index))
                           ((and (zerop index) ; //Yankelovich/Jankelowicz
                                 (not (starts-with-str "JOSE")))
                            (append-char #\J #\A))
                           ((and (vowel-p prev-ch) ; spanish pron. of e.g. 'bajador'
                                 (not slavo-germanic-p)
                                 (char-member-p next-ch '(#\A #\O)))
                            (append-char #\J #\H))
                           ((= index (1- len))
                            (append-char #\J #\space))
                           ((and (char-member-p next-ch '(#\L #\T #\K #\S #\N #\M #\B #\Z))
                                 (not (char-member-p prev-ch '(#\S #\K #\L))))
                            (append-char #\J)))
                     (if (char-same-p next-ch #\J)
                         (incf index 2)
                         (incf index)))
                    (#\K
                     (append-char #\K)
                     (if (char-same-p next-ch #\K)
                         (incf index 2)
                         (incf index)))
                    (#\L
                     (append-char #\L)
                     (if (char-same-p next-ch #\L)
                         (progn
                           (when (or (and (= index (- len 3)) ; spanish e.g. 'cabrillo', 'gallegos'
                                          (starts-with-any '("ILLO" "ILLA" "ALLE") -1))
                                     (and (or (starts-with-any '("AS" "OS") (- len 2))
                                              (char-member-p (offset-char (1- len)) '(#\A #\O)))
                                          (starts-with-str "ALLE" -1)))
                             (append-str "L" "")
                             (incf index 2))
                           (incf index 2))
                         (incf index)))
                    (#\M
                     (append-char #\M)
                     (if (or (and (starts-with-str "UMB" -1)
                                  (or (= (1+ index) (1- len))
                                      (starts-with-str "ER" 2)))
                             (char-same-p next-ch #\M))
                         (incf index 2)
                         (incf index)))
                    (#\N
                     (append-char #\N)
                     (if (char-same-p next-ch #\N)
                         (incf index 2)
                         (incf index)))
                    (#\P
                     (cond ((char-same-p next-ch #\H)
                            (append-char #\F)
                            (incf index 2))
                           ((char-member-p next-ch '(#\P #\B)) ; also account for "campbell", "raspberry"
                            (incf index 2))
                           (t
                            (append-char #\P)
                            (incf index))))
                    (#\Q
                     (append-char #\K)
                     (if (char-same-p next-ch #\Q)
                         (incf index 2)
                         (incf index)))
                    (#\R
                     (if (and (= index (1- len)) ; french e.g. 'rogier', but exclude 'hochmeier'
                              (not slavo-germanic-p)
                              (starts-with-str "IE" -2)
                              (not (starts-with-any '("ME" "MA") -4)))
                         (append-str "" "R")
                         (append-char #\R))
                     (if (char-same-p next-ch #\R)
                         (incf index 2)
                         (incf index)))
                    (#\S
                     (cond ((starts-with-any '("ISL" "YSL") -1) ; special cases 'island', 'isle', 'carlisle', 'carlysle'
                            (incf index))
                           ((and (zerop index)
                                 (starts-with-str "SUGAR")) ; special case 'sugar-'
                            (append-char #\X #\S)
                            (incf index))
                           ((starts-with-str "SH")
                            (if (starts-with-any '("HEIM" "HOEK" "HOLM" "HOLZ") 1) ; germanic
                                (append-char #\S)
                                (append-char #\X))
                            (incf index 2))
                           ((starts-with-any '("SIO" "SIA" "SIAN")) ; italian & armenian
                            (if (not slavo-germanic-p)
                                (append-char #\S #\X)
                                (append-char #\S))
                            (incf index 3))
                           ((or (and (zerop index) ; german & anglicisations, e.g. 'smith' match 'schmidt', 'snider' match 'schneider'
                                     (char-member-p next-ch '(#\M #\N #\L #\W))) ; also, -sz- in slavic language altho in hungarian it is pronounced 's'
                                (char-same-p next-ch #\Z))
                            (append-char #\S #\X)
                            (if (char-same-p next-ch #\Z)
                                (incf index 2)
                                (incf index)))
                           ((starts-with-str "SC")
                            (cond ((char-same-p (offset-char 2) #\H) ; Schlesinger's rule
                                   (cond ((starts-with-any '("ER" "EN") 3) ; 'schermerhorn', 'schenker'
                                          (append-str "X" "SK"))
                                         ((starts-with-any '("OO" "UY" "ED" "EM") 3) ; dutch origin, e.g. 'school', 'schooner'
                                          (append-char #\X))
                                         ((and (zerop index)
                                               (not (vowel-p (word-char 3)))
                                               (not (char-same-p (word-char 3) #\W)))
                                          (append-char #\X #\S)))
                                   (incf index 3))
                                  ((char-member-p (offset-char 2) '(#\I #\E #\Y))
                                   (append-char #\S)
                                   (incf index 3))
                                  (t
                                   (append-str "SK")
                                   (incf index 3))))
                           (t
                            (if (and (= index (1- len)) ; french e.g. 'resnais', 'artois'
                                     (starts-with-any '("AI" "OI") -2))
                                (append-str "" "S")
                                (append-char #\S))
                            (if (char-member-p next-ch '(#\S #\Z))
                                (incf index 2)
                                (incf index)))))
                    (#\T
                     (cond ((starts-with-any '("TION" "TIA" "TCH"))
                            (append-char #\X)
                            (incf index 3))
                           ((or (starts-with-str "TH") ; special case 'thomas', 'thames' or germanic
                                (starts-with-str "TTH"))
                            (if (or (starts-with-any '("OM" "AM") 2)
                                    (starts-with-any '("VAN " "VON " "SCH") (- index)))
                                (append-char #\T)
                                (append-str "0" "T"))
                            (incf index 2))
                           (t
                            (append-char #\T)
                            (if (char-member-p next-ch '(#\T #\D))
                                (incf index 2)
                                (incf index)))))
                    (#\V
                     (append-char #\F)
                     (if (char-same-p next-ch #\V)
                         (incf index 2)
                         (incf index)))
                    (#\W
                     (if (starts-with-str "WR") ; can also be in middle of word
                         (progn
                           (append-char #\R)
                           (incf index 2))
                         (progn
                           (when (and (zerop index)
                                      (or (vowel-p next-ch)
                                          (starts-with-str "WH")))
                             (if (vowel-p next-ch) ; Wasserman should match Vasserman
                                 (append-char #\A #\F)
                                 (append-char #\A)))
                           (cond ((or (and (= index (1- len)) ; Arnow should match Arnoff
                                           (vowel-p prev-ch))
                                      (starts-with-any '("EWSKI" "EWSKY" "OWSKI" "OWSKY") -1)
                                      (starts-with-str "SCH"))
                                  (append-str "" "F")
                                  (incf index))
                                 ((starts-with-any '("WICZ" "WITZ")) ; polish e.g. 'filipowicz'
                                  (append-str "TS" "FX")
                                  (incf index 4))
                                 (t
                                  (incf index))))))
                    (#\X
                     (when (and (= index (1- len)) ; french e.g. breaux
                                (or (starts-with-any '("IAU" "EAU") -3)
                                    (starts-with-any '("AU" "OU") -2)))
                       (append-str "KS"))
                     (if (char-member-p next-ch '(#\C #\X))
                         (incf index 2)
                         (incf index)))
                    (#\Z
                     (if (char-same-p next-ch #\H) ; chinese pinyin e.g. 'zhao'
                         (progn
                           (append-char #\J)
                           (incf index 2))
                         (progn
                           (if (or (starts-with-any '("ZO" "ZI" "ZA") 1)
                                   (and slavo-germanic-p
                                        (not (char-same-p prev-ch #\T))))
                               (append-str "S" "TS")
                               (append-char #\S))
                           (if (char-same-p next-ch #\Z)
                               (incf index 2)
                               (incf index)))))
                    (otherwise (incf index))))))
    (values (coerce primary 'string)
            (coerce secondary 'string))))

;;; ----------------------------------------------------------------------------
;;; Simple accessors

(defun metaphone-primary (word)
  "Return the primary Double Metaphone encoding for WORD.

This is a convenience wrapper around `DOUBLE-METAPHONE` that returns only
the first (primary) phonetic key — the most likely representation of the
word's pronunciation."
  (nth-value 0 (double-metaphone word)))

(defun metaphone-alternate (word)
  "Return the secondary Double Metaphone encoding for WORD.

This is a convenience wrapper around `DOUBLE-METAPHONE` that returns only
the second (alternate) phonetic key, which captures less common or
ambiguous pronunciations."
  (nth-value 1 (double-metaphone word)))
