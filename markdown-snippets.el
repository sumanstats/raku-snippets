;; Define your snippets
(yas-define-snippets 'markdown-mode
  '(
    ("all" "Hb $1, TLC $2, N/L $3, platelet $4, INR $5, Ur/Cr $6\nNa/K $7, TB/DB $8, ALT/AST $9, ALP $10, prot/alb $11\n$0" "All investigations")
    ("inv" "Hb $1, TC $2, N/L $3, plt $4, Ur/Cr $5\nNa/K $6, prot/alb $7\n$0" "Partly investigations")
    ("lft" "TB/DB $1, ALT/AST $2, ALP $3, prot/alb $4\n$0" "liver function tests")
    ("vit" "vitals stable $0" "Vitals")
    ("bg" "blood group: $1 $0" "Blood group")
    ("upg" "ulceroproliferative growth $0" "ulceroproliferative growth")
    ("uri" "urine: $0" "urine")
    ("no" "no issues$0" "no issues")
    ("tub" "tube duodenostomy: $0" "tube duodenostomy")
    ("abg" "ABG: pH $1, pCO2 $2, pO2 $3, HCO3 $4, lactate $5 $0" "arterial blood gas analysis")
    ("br" "=============================================================== $0" "line separating time points")
    ("rft" "Ur/Cr $1, Na/K $2  $0" "rft")
    ("cbc" "Hb $1, TLC $2, N/L $3, platelet $4  $0" "blood count")
    ("bp" "Pulse $1, BP $2 $0" "blood pressure")
    ("samy" "ser/Amylase/Lipase $1 $0" "serum amylase lipase")
    ("pip" "Piperacillin + Tazobactam $0" "tazolin")
    ("pw" "Presented with $0" "presentation of illness")
    ("day" "DOA $0" "day of admission")
    ("sspd" "Stomach Sparing Pancreatoduodenectomy $0" "Stomach Sparing Pancreatoduodenectomy")
    ("tft" "T3 $1, T4 $2, TSH $3 $0" "Thyroid function test")
    ("dopp" "echogenicity $1, portal flow $2, hepatic vein flow $3\nhepatic artery flow: PSV $4, RI $5, acceleration time $6\nIVC $7, effusion $8, collection $9, miscellaneous $10 $0" "Recipient doppler")
    ("hba" "HbA1c $1 $0" "Glycated hemoglobin")
    ("bmi"
     "`(let (w height-cm h)
         ;; Get valid weight (kg): 3..150
         (while (not (and (numberp w) (>= w 3) (<= w 150)))
           (condition-case nil
               (progn
                 (setq w (float (read-number \"Present weight (kg): \")))
                 (unless (and (>= w 3) (<= w 150))
                   (message \"Weight must be between 3 and 150 kg\")
                   (setq w nil)))
             (wrong-type-argument
              (message \"Weight must be a number\")
              (setq w nil))))

         ;; Get valid height in cm: > 30
         (while (not (and (numberp height-cm) (> height-cm 30)))
           (condition-case nil
               (progn
                 (setq height-cm (float (read-number \"Height (cm): \")))
                 (unless (> height-cm 30)
                   (message \"Height must be greater than 30 cm\")
                   (setq height-cm nil)))
             (wrong-type-argument
              (message \"Height must be a number\")
              (setq height-cm nil))))

         ;; convert to metres for BMI calculation
         (setq h (/ height-cm 100.0))

         ;; calculate BMI and BSA
         (let* ((bmi (/ w (* h h)))
                (bsa (sqrt (/ (* height-cm w) 3600.0)))
                (bmi-category
                 (cond ((< bmi 18.5) \"Underweight\")
                       ((< bmi 23.0) \"Normal Weight\")
                       ((< bmi 25.0) \"Increased Risk/Overweight\")
                       (t \"High Risk/Obese\"))))
           (format \"Present weight: %.1f kg\nHeight: %.0f cm\n\nBMI: %.2f kg/m² (%s)\nBSA (Mosteller): %.2f m²\"
                   w height-cm bmi bmi-category bsa)))`"
     "BMI + BSA calculator with correct height validation")
    ("slv"
     "`(let (height weight bsa slv)
         ;; Prompt for valid weight 3-150 kg
         (while (not (and (numberp weight) (>= weight 3) (<= weight 150)))
           (condition-case nil
               (setq weight (float (read-number \"Weight (kg): \")))
             (wrong-type-argument
              (message \"Weight must be a number\")
              (setq weight nil))))
         
         ;; Prompt for valid height >30 cm
         (while (not (and (numberp height) (> height 30)))
           (condition-case nil
               (setq height (float (read-number \"Height (cm): \")))
             (wrong-type-argument
              (message \"Height must be a number\")
              (setq height nil))))

         ;; calculate BSA and SLV
         (setq bsa (sqrt (/ (* height weight) 3600.0)))
         (setq slv (- (* 1267 bsa) 794))

         ;; insert formatted result
         (format \"SLV: %.0f cc\" 
                 slv))`"
     "SLV calculator based on Mosteller formula")
     ("nri"
     "`(let (a w u h height-cm bsa)
         ;; Albumin validation
         (while (not (and (numberp a) (> a 0)))
           (condition-case nil
               (setq a (float (read-number \"Serum albumin (g/L): \")))
             (wrong-type-argument
              (message \"Albumin must be a number\")
              (setq a nil))))
         
         ;; Present weight validation: 3–150 kg
         (while (not (and (numberp w) (>= w 3) (<= w 150)))
           (condition-case nil
               (setq w (float (read-number \"Present weight (kg): \")))
             (wrong-type-argument
              (message \"Weight must be a number\")
              (setq w nil)))
           (unless (and (>= w 3) (<= w 150))
             (message \"Weight must be between 3–150 kg\")
             (setq w nil)))
         
         ;; Usual weight validation: 3–150 kg
         (while (not (and (numberp u) (>= u 3) (<= u 150)))
           (condition-case nil
               (setq u (float (read-number \"Usual weight (kg): \")))
             (wrong-type-argument
              (message \"Usual weight must be a number\")
              (setq u nil)))
           (unless (and (>= u 3) (<= u 150))
             (message \"Usual weight must be between 3–150 kg\")
             (setq u nil)))

         ;; Height validation: >30 cm
         (while (not (and (numberp height-cm) (> height-cm 30)))
           (condition-case nil
               (setq height-cm (float (read-number \"Height (cm): \")))
             (wrong-type-argument
              (message \"Height must be a number\")
              (setq height-cm nil)))
           (unless (> height-cm 30)
             (message \"Height must be greater than 30 cm\")
             (setq height-cm nil)))

         ;; convert height to meters for BMI
         (setq h (/ height-cm 100.0))

         ;; calculate BMI, NRI, BSA
         (let* ((bmi (/ w (* h h)))
                (nri (+ (* 1.519 a) (* 41.7 (/ w u))))
                (bsa (sqrt (/ (* height-cm w) 3600.0))))
           ;; BMI category
           (let ((bmi-category
                  (cond ((< bmi 18.5) \"Underweight\")
                        ((< bmi 23.0) \"Normal Weight\")
                        ((< bmi 25.0) \"Increased Risk/Overweight\")
                        (t \"High Risk/Obese\"))))
             ;; NRI category
             (let ((nri-category
                    (cond ((> nri 100.0) \"No\")
                          ((>= nri 97.6) \"Mild\")
                          ((>= nri 83.5) \"Moderate\")
                          (t \"Severe\"))))
               (format \"Present weight: %.1f kg\nUsual weight: %.1f kg\nHeight: %.0f cm\nAlbumin: %.0f g/L\n\nBSA: %.2f m²\nBMI: %.2f kg/m² (%s)\nNRI: %.2f (%s risk of malnutrition)\"
                       w u height-cm a bsa bmi bmi-category nri nri-category)))))`"
     "NRI + BMI + BSA calculator with validation")
    
    
   ))

(provide 'markdown-snippets)
