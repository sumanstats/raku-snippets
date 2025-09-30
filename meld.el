
(yas-define-snippets 'markdown-mode
  '(
    ;; ========= Original MELD =========
    ("meld"
     "`(let* ((bil (string-to-number (read-string \"Bilirubin (mg/dL): \")))
              (inr (string-to-number (read-string \"INR: \")))
              (cr  (string-to-number (read-string \"Creatinine (mg/dL): \"))))
         ;; input validation
         (while (or (not (numberp bil)) (< bil 0))
           (setq bil (string-to-number (read-string \"Bilirubin must be ≥0. Re-enter: \"))))
         (while (or (not (numberp inr)) (< inr 0))
           (setq inr (string-to-number (read-string \"INR must be ≥0. Re-enter: \"))))
         (while (or (not (numberp cr)) (< cr 0))
           (setq cr (string-to-number (read-string \"Creatinine must be ≥0. Re-enter: \"))))
         ;; creatinine cap handling (MELD original caps creatinine at 4.0;
         ;; dialysis -> use 4.0 as per common policy)
         (when (> cr 4.0)
           (if (y-or-n-p (format \"Creatinine %.2f exceeds MELD cap (4.0). Accept capping to 4.0? \" cr))
               (setq cr 4.0)
             (setq cr (string-to-number (read-string \"Enter creatinine ≤ 4.0 mg/dL: \")))
             (while (> cr 4.0)
               (setq cr (string-to-number (read-string \"Still >4.0 — enter creatinine ≤ 4.0: \"))))))
         ;; minimum values for log (UNOS/OPTN: values < 1 set to 1.0)
         (setq bil (max bil 1.0))
         (setq inr (max inr 1.0))
         (setq cr  (max cr 1.0))
         ;; compute
         (let ((meld (+ (* 3.78 (log bil))
                        (* 11.2 (log inr))
                        (* 9.57 (log cr))
                        6.43)))
           (format \"MELD = %.0f\" meld)))`"
     "Original MELD score (interactive)")

    ;; ========= MELD-Na (UNOS style) =========
    ("meldna"
     "`(let* ((bil (string-to-number (read-string \"Bilirubin (mg/dL): \")))
              (inr (string-to-number (read-string \"INR: \")))
              (cr  (string-to-number (read-string \"Creatinine (mg/dL): \")))
              (na  (string-to-number (read-string \"Sodium (mmol/L): \"))))
         ;; validation
         (while (or (not (numberp bil)) (< bil 0))
           (setq bil (string-to-number (read-string \"Bilirubin must be ≥0. Re-enter: \"))))
         (while (or (not (numberp inr)) (< inr 0))
           (setq inr (string-to-number (read-string \"INR must be ≥0. Re-enter: \"))))
         (while (or (not (numberp cr)) (< cr 0))
           (setq cr (string-to-number (read-string \"Creatinine must be ≥0. Re-enter: \"))))
         (while (or (not (numberp na)))
           (setq na (string-to-number (read-string \"Sodium must be numeric. Re-enter: \"))))
         ;; sodium acceptable range prompt/cap (UNOS policy: sodium 125-137 used for MELD-Na)
         (when (or (< na 125) (> na 137))
           (if (y-or-n-p (format \"Sodium %.2f is outside 125-137 range and will be bounded; accept bounding to 125..137? \" na))
               (setq na (min (max na 125.0) 137.0))
             (setq na (string-to-number (read-string \"Enter sodium within 125-137 mmol/L: \")))
             (while (or (< na 125) (> na 137)
                        (not (numberp na)))
               (setq na (string-to-number (read-string \"Still out of bounds — sodium 125-137 mmol/L: \"))))))
         ;; creatinine cap handling (cap 4.0)
         (when (> cr 4.0)
           (if (y-or-n-p (format \"Creatinine %.2f exceeds MELD cap (4.0). Accept capping to 4.0? \" cr))
               (setq cr 4.0)
             (setq cr (string-to-number (read-string \"Enter creatinine ≤ 4.0 mg/dL: \")))
             (while (> cr 4.0) (setq cr (string-to-number (read-string \"Still >4.0 — enter creatinine ≤ 4.0: \"))))))
         ;; minima for logs
         (setq bil (max bil 1.0))
         (setq inr (max inr 1.0))
         (setq cr  (max cr 1.0))
         ;; base MELD
         (let* ((meld (+ (* 3.78 (log bil)) (* 11.2 (log inr)) (* 9.57 (log cr)) 6.43))
                (meldna (+ meld
                           (* 1.32 (- 137 na))
                           (- (* 0.033 meld (- 137 na))))))
           ;; policy bounds for reporting/listing
           (setq meldna (min (max meldna 6.0) 40.0))
           (format \"MELD-Na = %.1f\" meldna)))`"
     "MELD-Na score (interactive)")

    ;; ========= MELD 3.0 =========
    ;; Implements published MELD 3.0 formula (includes sex and albumin, and interaction terms).
    ;; Source: Kim et al. (MELD 3.0) / OPTN docs. See citations in conversation.
    ("meld3"
     "`(let* ((bil (string-to-number (read-string \"Bilirubin (mg/dL): \")))
              (inr (string-to-number (read-string \"INR: \")))
              (cr  (string-to-number (read-string \"Creatinine (mg/dL): \")))
              (na  (string-to-number (read-string \"Sodium (mmol/L): \")))
              (alb (string-to-number (read-string \"Albumin (g/dL): \")))
              (female (y-or-n-p \"Female? (y/n): \")))
         ;; basic validation
         (while (or (not (numberp bil)) (< bil 0))  (setq bil (string-to-number (read-string \"Bilirubin must be ≥0. Re-enter: \"))))
         (while (or (not (numberp inr)) (< inr 0))  (setq inr (string-to-number (read-string \"INR must be ≥0. Re-enter: \"))))
         (while (or (not (numberp cr)) (< cr 0))    (setq cr (string-to-number (read-string \"Creatinine must be ≥0. Re-enter: \"))))
         (while (or (not (numberp na)))              (setq na (string-to-number (read-string \"Sodium must be numeric. Re-enter: \"))))
         (while (or (not (numberp alb)) (< alb 0))  (setq alb (string-to-number (read-string \"Albumin must be ≥0. Re-enter: \"))))
         ;; creatinine cap for MELD 3.0 is 3.0 mg/dL (UNOS policy change)
         (when (> cr 3.0)
           (if (y-or-n-p (format \"Creatinine %.2f exceeds MELD 3.0 cap (3.0). Accept capping to 3.0? \" cr))
               (setq cr 3.0)
             (setq cr (string-to-number (read-string \"Enter creatinine ≤ 3.0 mg/dL: \")))
             (while (> cr 3.0) (setq cr (string-to-number (read-string \"Still >3.0 — enter creatinine ≤ 3.0: \"))))))
         ;; sodium bounding 125..137 for use in formula (policy)
         (when (or (< na 125) (> na 137))
           (if (y-or-n-p (format \"Sodium %.2f is outside 125-137 range and will be bounded; accept bounding to 125..137? \" na))
               (setq na (min (max na 125.0) 137.0))
             (setq na (string-to-number (read-string \"Enter sodium within 125-137 mmol/L: \")))
             (while (or (< na 125) (> na 137)) (setq na (string-to-number (read-string \"Still out of bounds — sodium 125-137 mmol/L: \"))))))
         ;; minima before log (UNOS/OPTN: values <1 set to 1.0; albumin bounded 1.5-3.5 in policy docs)
         (setq bil (max bil 1.0))
         (setq inr (max inr 1.0))
         (setq cr  (max cr 1.0))
         (setq alb (min (max alb 1.5) 3.5))
         ;; MELD 3.0 formula (published coefficients)
         ;; MELD 3.0 = 1.33*(female) + 4.56*ln(bilirubin) + 0.82*(137 - Na) - 0.24*(137 - Na)*ln(bilirubin)
         ;;           + 9.09*ln(INR) + 11.14*ln(creatinine) + 1.85*(3.5 - albumin) - 1.83*(3.5 - albumin)*ln(creatinine) + 6
         (let* ((fnum (if female 1 0))
                (val (+ (* 1.33 fnum)
                        (* 4.56 (log bil))
                        (* 0.82 (- 137 na))
                        (- (* 0.24 (- 137 na) (log bil)))
                        (* 9.09 (log inr))
                        (* 11.14 (log cr))
                        (* 1.85 (- 3.5 alb))
                        (- (* 1.83 (- 3.5 alb) (log cr)))
                        6.0)))
           ;; UNOS/OPTN policy: MELD 3.0 reported bounded to 6..40 and rounded for allocation
           (let ((val-bounded (min (max val 6.0) 40.0)))
             (format \"MELD 3.0 = %.2f (rounded %d)\" val-bounded (round val-bounded)))))`"
     "MELD 3.0 score (interactive)")

    ;; ========= PELD (Pediatric) =========
    ("peld"
     "`(let* ((bil (string-to-number (read-string \"Bilirubin (mg/dL): \")))
              (inr (string-to-number (read-string \"INR: \")))
              (alb (string-to-number (read-string \"Albumin (g/dL): \")))
              (age (string-to-number (read-string \"Age (years): \")))
              (growth (y-or-n-p \"Growth failure? (y/n): \")))
         ;; validation
         (while (or (not (numberp bil)) (< bil 0)) (setq bil (string-to-number (read-string \"Bilirubin must be ≥0. Re-enter: \"))))
         (while (or (not (numberp inr)) (< inr 0)) (setq inr (string-to-number (read-string \"INR must be ≥0. Re-enter: \"))))
         (while (or (not (numberp alb)) (< alb 0)) (setq alb (string-to-number (read-string \"Albumin must be ≥0. Re-enter: \"))))
         ;; policy: laboratory values <1.0 are set to 1.0 when calculating PELD
         (setq bil (max bil 1.0))
         (setq inr (max inr 1.0))
         (setq alb (max alb 1.0))
         ;; formula (original PELD)
         ;; PELD = 0.480*ln(bilirubin) + 1.857*ln(INR) - 0.687*ln(albumin)
         ;;       + 0.436 (if age < 1 yr) + 0.667 (if growth failure)
         (let* ((peld (+ (* 0.480 (log bil))
                         (* 1.857 (log inr))
                         (* -0.687 (log alb))
                         (if (< age 1) 0.436 0)
                         (if growth 0.667 0)))
                ;; UNOS/OPTN multiplication/offsets used in policy for allocation may apply;
                ;; here present the raw PELD value rounded to 2 decimals.
                )
           (format \"PELD = %.2f\" peld)))`"
     "PELD score (interactive)")

    ))

(provide 'meld)
