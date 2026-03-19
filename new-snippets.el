(yas-define-snippets 'markdown-mode
  '(
    ("nam" "Name: $1, Surgery: $2 \n$0" "Name")
    ("otdate" "Date: $1 \n$0" "OT date")
    ("tumm" "CEA $1, CA19-9 $2 \n$0" "Tumor markers")
    ("baseline" "Wt $1, Ht $2, BMI $3 \n$0" "wt ht bmi")
    ("outcome" "OT duration: $1, Blood loss: $2, Transfusion: $3, Inotrope: $4
    Complication: $5 \n$0" "Outcome for surgery")
    ("halp" "Preop: Hb $1, TC $2, N/L $3, Alb $4, Plt $5\n$0" "HALP score")
    ("halps" "Hb $1, TC $2, N/L $3, Alb $4, Plt $5, ser amylase $6, Drain amylase Rt/Lt $7" "HALP for single day in Whipple")
    ("hepa" "Duration: $1, blood loss: $2, parenchymal dissection with: $3, 
    pringle: ${4:yes/no/duration if yes}, bile leak checked with: $5 \n$0" "hepatectomy details")
    ("intrasspd" "Intraop findings: ${1:mass details}, MPD: $2, texture: $3, thickness at neck: $4, duration: $5, blood loss: $6 \n$0" "SSPD details")
    ("recon" "CT recon: Artery => ${1:describe the course of artery, retroportal or other}, MHA => ${2: arising from LHA, RHA, both}, GDA origin => $3, Portal vein => ${4:type}, Hepatic vein: $5 $0" "Vascular anatomy")
    ("ino_transf" "Inotrope: $1, Transfusion: $2 $0" "Inotrope and transfusion details")
    ("colon" "Colonoscopy: $1 $0" "Colonoscopy findings")
    ("intraot" "Intraop findings: ${1:OT details}, duration: $5, blood loss: $6 \n$0" "Operative details")
    ("drainwhip" "Rt SHD: $1, Lt PJ: $2 $0" "Amount of drain in case of SSPD")
    ("ugi" "Upper GI endoscopy: $1 $0" "upper GI endoscopy findings")
    ("lai" "Name: $1, CT number: $2 \n Liver HU 1: $3 \n Spleen HU 1: $4 \n Liver HU 2: $5 \n Spleen HU 2: $6 \n Liver HU 3: $7 \n Spleen HU 3: $8 \n $0" "Liver LAI entry for thesis")
    ("gen" "Generated with $1. $0" "Generated with which AI?")        
  ))

(provide 'new-snippets)




