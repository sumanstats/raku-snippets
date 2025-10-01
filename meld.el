(yas-define-snippets 'markdown-mode
  '(
    ("meld-all"
     "`(let* ((notes '())   ;; collect truncation/cap messages
              entered-bil entered-inr entered-cr entered-na entered-alb entered-female entered-dialysis
              bil inr cr-meld cr-meld3 na alb)

         ;; -------- Bilirubin --------
         (setq entered-bil (string-to-number (read-string \"Bilirubin (mg/dL, ≥0): \")))
         (while (< entered-bil 0)
           (setq entered-bil (string-to-number (read-string \"Bilirubin (mg/dL, ≥0): \"))))
         (setq bil (max entered-bil 1.0))
         (when (< entered-bil 1.0)
           (push (format \"Bilirubin %.2f → set to 1.0 for MELD\" entered-bil) notes))

         ;; -------- INR --------
         (setq entered-inr (string-to-number (read-string \"INR (≥0): \")))
         (while (< entered-inr 0)
           (setq entered-inr (string-to-number (read-string \"INR (≥0): \"))))
         (setq inr (max entered-inr 1.0))
         (when (< entered-inr 1.0)
           (push (format \"INR %.2f → set to 1.0 for MELD\" entered-inr) notes))

         ;; -------- Dialysis --------
         (setq entered-dialysis
               (let ((ans (downcase (read-string \"Dialysis twice or more in last week? (y/n): \"))))
                 (member ans '(\"y\" \"yes\"))))
         
         ;; -------- Creatinine --------
         (setq entered-cr (string-to-number (read-string \"Creatinine (mg/dL, ≥0): \")))
         (while (< entered-cr 0)
           (setq entered-cr (string-to-number (read-string \"Creatinine (mg/dL, ≥0): \"))))
         
         (if entered-dialysis
             (progn
               (setq cr-meld 4.0
                     cr-meld3 3.0)
               (push \"Dialysis → creatinine set to 4.0 (MELD/MELD-Na) & 3.0 (MELD 3.0)\" notes)
               nil)  ;; ensure this branch doesn’t return a string
           (progn
             (setq cr-meld (min (max entered-cr 1.0) 4.0))
             (setq cr-meld3 (min (max entered-cr 1.0) 3.0))
             (cond
              ((< entered-cr 1.0)
               (push (format \"Creatinine %.2f → set to 1.0 (all formulas)\" entered-cr) notes))
              ((> entered-cr 4.0)
               (push (format \"Creatinine %.2f → truncated to 4.0 (MELD/MELD-Na) & 3.0 (MELD 3.0)\" entered-cr) notes))
              ((> entered-cr 3.0)
               (push (format \"Creatinine %.2f → truncated to 3.0 (MELD 3.0)\" entered-cr) notes)))
             nil))  ;; also return nil, not a string

         ;; -------- Sodium --------
         (setq entered-na (string-to-number (read-string \"Sodium (100–170 mmol/L): \")))
         (while (or (< entered-na 100) (> entered-na 170))
           (setq entered-na (string-to-number (read-string \"Sodium (100–170 mmol/L): \"))))
         (setq na (min (max entered-na 125.0) 137.0))
         (when (/= na entered-na)
           (push (format \"Sodium %.1f → adjusted to %.1f (MELD-Na/MELD 3.0)\" entered-na na) notes))

         ;; -------- Albumin --------
         (setq entered-alb (string-to-number (read-string \"Albumin (g/dL, ≥0): \")))
         (while (< entered-alb 0)
           (setq entered-alb (string-to-number (read-string \"Albumin (g/dL, ≥0): \"))))
         (setq alb (min (max entered-alb 1.5) 3.5))
         (when (/= alb entered-alb)
           (push (format \"Albumin %.2f → capped to %.2f (MELD 3.0)\" entered-alb alb) notes))

         ;; -------- Sex --------
         (setq entered-female
            (let ((ans (downcase (read-string \"Sex (M/F): \"))))
               (member ans '(\"f\" \"female\"))))

         ;; -------- Calculate scores --------
         (let* (;; MELD original
                (meld (+ (* 3.78 (log bil))
                         (* 11.2 (log inr))
                         (* 9.57 (log cr-meld))
                         6.43))
                ;; MELD-Na
                (meldna (min (max (+ meld
                                     (* 1.32 (- 137 na))
                                     (- (* 0.033 meld (- 137 na))))
                                  6.0)
                             40.0))
                ;; MELD 3.0
                (meld3 (min (max (+ (* (if entered-female 1.33 0) 1)
                                     (* 4.56 (log bil))
                                     (* 0.82 (- 137 na))
                                     (- (* 0.24 (- 137 na) (log bil)))
                                     (* 9.09 (log inr))
                                     (* 11.14 (log cr-meld3))
                                     (* 1.85 (- 3.5 alb))
                                     (- (* 1.83 (- 3.5 alb) (log cr-meld3)))
                                     6.0)
                                  6.0)
                             40.0)))
           ;; -------- Output --------
           (concat
            ;; Raw entered values
            \"Entered values:\n\"
            (format \"  Bilirubin = %.2f mg/dL\n\" entered-bil)
            (format \"  INR = %.2f\n\" entered-inr)
            (format \"  Creatinine = %.2f mg/dL\n\" entered-cr)
            (format \"  Sodium = %.1f mmol/L\n\" entered-na)
            (format \"  Albumin = %.2f g/dL\n\" entered-alb)
            (format \"  Dialysis = %s\n\" (if entered-dialysis \"Yes\" \"No\"))
            (format \"  Sex = %s\n\n\" (if entered-female \"Female\" \"Male\"))

            ;; Scores
            (format \"MELD = %.2f\nMELD-Na = %.2f\nMELD 3.0 = %.2f\n\n\" meld meldna meld3)

            ;; Creatinine summary line
            (format \"Creatinine actually used: MELD/MELD-Na = %.2f, MELD 3.0 = %.2f\n\n\" cr-meld cr-meld3)

            ;; Adjustment notes
            (if notes
                (concat \"Adjustments made:\n- \" (mapconcat 'identity (reverse notes) \"\n- \"))
              \"\"))))`"
     "Compute MELD, MELD-Na, and MELD 3.0 with live validation, dialysis handling, and adjustment summary")
    ))



(provide 'meld)
