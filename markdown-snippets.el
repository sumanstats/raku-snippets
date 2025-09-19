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
 "`(let (h w)
     ;; Read height in cm and convert to meters
     (while (not (and (numberp h) (> h 0)))
       (setq h (/ (float (read-number \"Height in cm: \")) 100)))
     ;; Read weight in kg
     (while (not (and (numberp w) (> w 0)))
       (setq w (float (read-number \"Weight in kilograms: \"))))
     ;; Calculate BMI
     (let ((bmi (/ w (* h h))))
       (format \"Height: %.0f cm, Weight: %.0f kg\nBMI: %.2f\" (* h 100) w bmi)))`"
 "BMI calculator")

   ))

(provide 'markdown-snippets)



