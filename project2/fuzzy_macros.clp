(import nrc.fuzzy.*)
(import nrc.fuzzy.jess.*)

(load-package nrc.fuzzy.jess.FuzzyFunctions)

;set these to the target values * 2 - did this to deal with under and over values
(defglobal ?*fatFvar* = (new FuzzyVariable "fat" 0.0 160 "grams"))
(defglobal ?*carbsFvar* = (new FuzzyVariable "carbohydrates" 0.0 520 "grams"))
(defglobal ?*proteinFvar* = (new FuzzyVariable "protein" 0.0 320.0 "grams")) 
(defglobal ?*bwFvar* = (new FuzzyVariable "bodyweight" 0.0 320.0 "lbs")) 

(defglobal ?*fchangeFvar* = (new FuzzyVariable "fChange" -1.0 1.0 ""))
(defglobal ?*cchangeFvar* = (new FuzzyVariable "cChange" -1.0 1.0 ""))
(defglobal ?*pchangeFvar* = (new FuzzyVariable "pChange" -1.0 1.0 ""))

(deftemplate target
  "users specified vals"
  (slot fats (type INTEGER))
  (slot carbs (type INTEGER))
  (slot protein (type INTEGER))
  (slot weight (type INTEGER))
  (slot user (type INTEGER)))

(deffacts initial-phase
   (phase set-goal))

;cannot enter anything less than 30 carbs and protein as resembles poor nutrition and this program will not support it does user error checking
(defrule protein-select
   (phase set-goal)
   ?targetMacros <- (target {(protein != nil || carbs != nil || fats != nil || weight != nil) && user == 0}) 
   =>
   (printout t "Target F: " ?targetMacros.fats  " C: "  ?targetMacros.carbs " P: " ?targetMacros.protein " BW: " ?targetMacros.weight crlf)
   (printout t "Ingested grams of protein: ")
   (bind ?inputP (read))
   (while (or(not(integerp ?inputP)) (> ?inputP (* ?targetMacros.protein 2)) (< ?inputP 30)) do
        (printout t "Please specify a valid integer between 30 and " (* ?targetMacros.protein 2) crlf)
        (bind ?inputP (read)))
    
   (printout t "Ingested grams of carbohydrates: ")
   (bind ?inputC (read))
   (while (or(not(integerp ?inputC)) (> ?inputC (* ?targetMacros.carbs 2)) (< ?inputC 30)) do
        (printout t "Please specify a valid integer between 30 and " (* ?targetMacros.carbs 2)  crlf)
        (bind ?inputC (read)))

   (printout t "Ingested grams of fat: ")
   (bind ?inputF (read))
   (while (or(not(integerp ?inputF)) (> ?inputF (* ?targetMacros.fats 2)) (< ?inputF 15)) do
        (printout t "Please specify a valid integer between 15 and " (* ?targetMacros.fats 2) crlf)
        (bind ?inputF (read)))

   (printout t "Current bodyweight: ")
   (bind ?inputW (read))
   (while (or(not(integerp ?inputW)) (> ?inputW (* ?targetMacros.weight 2)) (< ?inputW 80)) do
        (printout t "Please specify a valid integer between 80 and " (* ?targetMacros.weight 2) crlf)
        (bind ?inputW (read)))
        
   (assert (target(protein ?inputP) (carbs ?inputC) (fats ?inputF) (weight ?inputW) (user 1))))

;(defrule printcheck
;	?targetMacros <- (target {(protein != nil || carbs != nil || fats != nil || weight != nil) && user == 0})   
;    ?actualMacros <- (target {(protein != nil || carbs != nil || fats != nil || weight != nil) && user == 1})
;	=>
;    (bind ?out (- ?targetMacros.protein ?actualMacros.protein))
;    (printout t ?out  " "  ?problem.carbs  " "  ?problem.fats " " ?problem.weight crlf)
;)

(defrule init-FuzzyVariables
	?targetMacros <- (target {(protein != nil || carbs != nil || fats != nil || weight != nil) && user == 0})   
    ?actualMacros <- (target {(protein != nil || carbs != nil || fats != nil || weight != nil) && user == 1})
  =>
   ;terms for protein
   (bind ?xproteinHigh  (create$ (+ ?targetMacros.protein 10) (+ ?targetMacros.protein 30))) 
   (bind ?yproteinHigh  (create$ 0.0 1.0)) 
   (bind ?xproteinLow (create$ (- ?targetMacros.protein 30) (- ?targetMacros.protein 10))) 
   (bind ?yproteinLow (create$ 1.0 0.0)) 
   (?*proteinFvar* addTerm "high" ?xproteinHigh ?yproteinHigh 2) 
   (?*proteinFvar* addTerm "low" ?xproteinLow ?yproteinLow 2) 
   (?*proteinFvar* addTerm "veryHigh" "very high") 
   (?*proteinFvar* addTerm "ok" "not high and (not low)")
   ;; terms for carbs
   (bind ?xcarbsHigh  (create$ (+ ?targetMacros.carbs 10) (+ ?targetMacros.carbs 30)))
   (bind ?ycarbsHigh  (create$ 0.0 1.0)) 
   (bind ?xcarbsLow (create$ (- ?targetMacros.carbs 30) (- ?targetMacros.carbs 10)))
   (bind ?ycarbsLow (create$ 1.0 0.0)) 
   (?*carbsFvar* addTerm "high" ?xcarbsHigh ?ycarbsHigh 2) 
   (?*carbsFvar* addTerm "low" ?xcarbsLow ?ycarbsLow 2) 
   (?*carbsFvar* addTerm "veryHigh" "very high") 
   (?*carbsFvar* addTerm "ok" "not high and (not low)") 
   ;terms for fat
   (bind ?xfatHigh  (create$ ?targetMacros.fats (+ ?targetMacros.fats 15)))
   (bind ?yfatHigh  (create$ 0.0 1.0)) 
   (bind ?xfatLow (create$ (- ?targetMacros.fats 15) ?targetMacros.fats))
   (bind ?yfatLow (create$ 1.0 0.0)) 
   (?*fatFvar* addTerm "high" ?xfatHigh ?yfatHigh 2) 
   (?*fatFvar* addTerm "low" ?xfatLow ?yfatLow 2) 
   (?*fatFvar* addTerm "veryHigh" "very high") 
   (?*fatFvar* addTerm "ok" "not high and (not low)") 

    ;set fuzzy var for bodyweight
   (bind ?xbwHigh  (create$ ?targetMacros.weight (+ ?targetMacros.weight 5)))
   (bind ?ybwHigh  (create$ 0.0 1.0)) 
   (bind ?xbwLow (create$ (- ?targetMacros.weight 5) ?targetMacros.weight))
   (bind ?ybwLow (create$ 1.0 0.0)) 
   (?*bwFvar* addTerm "high" ?xbwHigh ?ybwHigh 2) 
   (?*bwFvar* addTerm "low" ?xbwLow ?ybwLow 2) 
   (?*bwFvar* addTerm "veryHigh" "very high") 
   (?*bwFvar* addTerm "ok" "not high and (not low)") 
    
   ;;our adjustments for each macro
   (?*fchangeFvar* addTerm "NB" (new ZFuzzySet -0.2 -.06))
   (?*fchangeFvar* addTerm "NM" (new TriangleFuzzySet -0.06 -.05 -.03))
   (?*fchangeFvar* addTerm "Z" (new TriangleFuzzySet -0.02 0 .02))
   (?*fchangeFvar* addTerm "PM" (new TriangleFuzzySet 0.03 .05 .07))
   (?*fchangeFvar* addTerm "PB" (new SFuzzySet 0.08 0.2))
    
   (?*cchangeFvar* addTerm "NB" (new ZFuzzySet -0.3 -.06))
   (?*cchangeFvar* addTerm "NM" (new TriangleFuzzySet -0.06 -.05 -.03))
   (?*cchangeFvar* addTerm "Z" (new TriangleFuzzySet -0.02 0 .02))
   (?*cchangeFvar* addTerm "PM" (new TriangleFuzzySet 0.03 .06 .1))
   (?*cchangeFvar* addTerm "PB" (new SFuzzySet 0.1 0.3))
    
   (?*pchangeFvar* addTerm "NB" (new ZFuzzySet -0.3 -.06))
   (?*pchangeFvar* addTerm "NM" (new TriangleFuzzySet -0.06 -.05 -.03))
   (?*pchangeFvar* addTerm "Z" (new TriangleFuzzySet -0.02 0 .02))
   (?*pchangeFvar* addTerm "PM" (new TriangleFuzzySet 0.03 .06 .1))
   (?*pchangeFvar* addTerm "PB" (new SFuzzySet 0.1 0.3))
   ;; add the fuzzy input based on user entries
   (assert (theProtein (new FuzzyValue ?*proteinFvar* (new TriangleFuzzySet ?actualMacros.protein ?actualMacros.protein ?actualMacros.protein))))
   (assert (theCarbs (new FuzzyValue ?*carbsFvar*(new TriangleFuzzySet ?actualMacros.carbs ?actualMacros.carbs ?actualMacros.carbs))))
   (assert (theFat (new FuzzyValue ?*fatFvar* (new TriangleFuzzySet ?actualMacros.fats ?actualMacros.fats ?actualMacros.fats))))
   (assert (theBW (new FuzzyValue ?*bwFvar* (new TriangleFuzzySet ?actualMacros.weight ?actualMacros.weight ?actualMacros.weight))))
)



;;;;;;;;;;;;;;;;;;;BW UP
;;protein high
(defrule p00
   (theBW ?t&:(fuzzy-match ?t "high"))
   (theBW ?t&:(not (fuzzy-match ?t "ok")))
   (theProtein ?p&:(fuzzy-match ?p "high"))
   (theProtein ?p&:(not (fuzzy-match ?p "ok")))
 => 
   (assert (proteinAdjust (new FuzzyValue ?*pchangeFvar* "NB")))
)

;;protein medium high
(defrule p10
   (theBW ?t&:(fuzzy-match ?t "high"))
   (theBW ?t&:(not (fuzzy-match ?t "ok")))
   (theProtein ?p&:(fuzzy-match ?p "high"))
   (theProtein ?p&:(fuzzy-match ?p "ok"))
 => 
   (assert (proteinAdjust (new FuzzyValue ?*pchangeFvar* "NB")))
)

;;protein ok
(defrule p20
   (theBW ?t&:(fuzzy-match ?t "high"))
   (theBW ?t&:(not (fuzzy-match ?t "ok")))
   (theProtein ?p&:(fuzzy-match ?p "ok")) 
   (theProtein ?p&:(not (fuzzy-match ?p "high")))
   (theProtein ?p&:(not (fuzzy-match ?p "low")))
 => 
   (assert (proteinAdjust (new FuzzyValue ?*pchangeFvar* "Z")))
)
;;protein medium low
(defrule p30
   (theBW ?t&:(fuzzy-match ?t "high"))
   (theBW ?t&:(not (fuzzy-match ?t "ok")))
   (theProtein ?p&:(fuzzy-match ?p "low")) 
   (theProtein ?p&:(fuzzy-match ?p "ok"))
 => 
   (assert (proteinAdjust (new FuzzyValue ?*pchangeFvar* "Z")))
)

;;protein low
(defrule p40
   (theBW ?t&:(fuzzy-match ?t "high"))
   (theBW ?t&:(not (fuzzy-match ?t "ok")))
   (theProtein ?p&:(fuzzy-match ?p "low")) 
   (theProtein ?p&:(not (fuzzy-match ?p "ok")))
 => 
   (assert (proteinAdjust (new FuzzyValue ?*pchangeFvar* "PM")))
)

;carbs high
(defrule c00
   (theBW ?t&:(fuzzy-match ?t "high"))
   (theBW ?t&:(not (fuzzy-match ?t "ok")))
   (theCarbs ?p&:(fuzzy-match ?p "high"))
   (theCarbs ?p&:(not (fuzzy-match ?p "ok")))
 => 
   (assert (carbsAdjust (new FuzzyValue ?*cchangeFvar* "NB")))
)

;carbs medium high
(defrule c10
   (theBW ?t&:(fuzzy-match ?t "high"))
   (theBW ?t&:(not (fuzzy-match ?t "ok")))
   (theCarbs ?p&:(fuzzy-match ?p "ok")) 
   (theCarbs ?p&:(fuzzy-match ?p "high"))
 => 
   (assert (carbsAdjust (new FuzzyValue ?*cchangeFvar* "NB")))
)

;carbs ok
(defrule c20
   (theBW ?t&:(fuzzy-match ?t "high"))
   (theBW ?t&:(not (fuzzy-match ?t "ok")))
   (theCarbs ?p&:(fuzzy-match ?p "ok")) 
   (theCarbs ?p&:(not (fuzzy-match ?p "high")))
   (theCarbs ?p&:(not (fuzzy-match ?p "low")))
 => 
   (assert (carbsAdjust (new FuzzyValue ?*cchangeFvar* "NM")))
)

;carbs medium low
(defrule c30
   (theBW ?t&:(fuzzy-match ?t "high"))
   (theBW ?t&:(not (fuzzy-match ?t "ok")))
   (theCarbs ?p&:(fuzzy-match ?p "ok")) 
   (theCarbs ?p&:(fuzzy-match ?p "low"))
 => 
   (assert (carbsAdjust (new FuzzyValue ?*cchangeFvar* "Z")))
)

;carbs low
(defrule c40
   (theBW ?t&:(fuzzy-match ?t "high"))
   (theBW ?t&:(not (fuzzy-match ?t "ok")))
   (theCarbs ?p&:(not (fuzzy-match ?p "ok")))
   (theCarbs ?p&:(fuzzy-match ?p "low"))
 => 
   (assert (carbsAdjust (new FuzzyValue ?*cchangeFvar* "PM")))
)

;fat high
(defrule f00
   (theBW ?t&:(fuzzy-match ?t "high"))
   (theBW ?t&:(not (fuzzy-match ?t "ok")))
   (theFat ?p&:(fuzzy-match ?p "high"))
   (theFat ?p&:(not (fuzzy-match ?p "ok")))
 => 
   (assert (FatAdjust (new FuzzyValue ?*fchangeFvar* "NB")))
)

;fat medium high
(defrule f10
   (theBW ?t&:(fuzzy-match ?t "high"))
   (theBW ?t&:(not (fuzzy-match ?t "ok")))
   (theFat ?p&:(fuzzy-match ?p "ok")) 
   (theFat ?p&:(fuzzy-match ?p "high"))
 => 
   (assert (FatAdjust (new FuzzyValue ?*fchangeFvar* "NB")))
)

;fat ok
(defrule f20
   (theBW ?t&:(fuzzy-match ?t "high"))
   (theBW ?t&:(not (fuzzy-match ?t "ok")))
   (theFat ?p&:(fuzzy-match ?p "ok")) 
   (theFat ?p&:(not (fuzzy-match ?p "high")))
   (theFat ?p&:(not (fuzzy-match ?p "low")))
 => 
   (assert (FatAdjust (new FuzzyValue ?*fchangeFvar* "NM")))
)

;fat medium low
(defrule f30
   (theBW ?t&:(fuzzy-match ?t "high"))
   (theBW ?t&:(not (fuzzy-match ?t "ok")))
   (theFat ?p&:(fuzzy-match ?p "ok")) 
   (theFat ?p&:(fuzzy-match ?p "low"))
 => 
   (assert (FatAdjust (new FuzzyValue ?*fchangeFvar* "Z")))
)

;fat low
(defrule f40
   (theBW ?t&:(fuzzy-match ?t "high"))
   (theBW ?t&:(not (fuzzy-match ?t "ok")))
   (theFat ?p&:(not (fuzzy-match ?p "ok")))
   (theFat ?p&:(fuzzy-match ?p "low"))
 => 
   (assert (FatAdjust (new FuzzyValue ?*fchangeFvar* "PM")))
)

;;;;;;;;;;BW medium high

;;protein high
(defrule p01
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(fuzzy-match ?t "high"))
   (theProtein ?p&:(fuzzy-match ?p "high"))
   (theProtein ?p&:(not (fuzzy-match ?p "ok")))
 => 
   (assert (proteinAdjust (new FuzzyValue ?*pchangeFvar* "NB")))
)

;;protein medium high
(defrule p11
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(fuzzy-match ?t "high"))
   (theProtein ?p&:(fuzzy-match ?p "high"))
   (theProtein ?p&:(fuzzy-match ?p "ok"))
 => 
   (assert (proteinAdjust (new FuzzyValue ?*pchangeFvar* "NM")))
)

;;protein ok
(defrule p21
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(fuzzy-match ?t "high"))
   (theProtein ?p&:(fuzzy-match ?p "ok")) 
   (theProtein ?p&:(not (fuzzy-match ?p "high")))
   (theProtein ?p&:(not (fuzzy-match ?p "low")))
 => 
   (assert (proteinAdjust (new FuzzyValue ?*pchangeFvar* "Z")))
)
;;protein medium low
(defrule p31
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(fuzzy-match ?t "high"))
   (theProtein ?p&:(fuzzy-match ?p "low")) 
   (theProtein ?p&:(fuzzy-match ?p "ok"))
 => 
   (assert (proteinAdjust (new FuzzyValue ?*pchangeFvar* "Z")))
)

;;protein low
(defrule p41
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(fuzzy-match ?t "high"))
   (theProtein ?p&:(fuzzy-match ?p "low")) 
   (theProtein ?p&:(not (fuzzy-match ?p "ok")))
 => 
   (assert (proteinAdjust (new FuzzyValue ?*pchangeFvar* "PM")))
)

;carbs high
(defrule c01
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(fuzzy-match ?t "high"))
   (theCarbs ?p&:(fuzzy-match ?p "high"))
   (theCarbs ?p&:(not (fuzzy-match ?p "ok")))
 => 
   (assert (carbsAdjust (new FuzzyValue ?*cchangeFvar* "NB")))
)

;carbs medium high
(defrule c11
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(fuzzy-match ?t "high"))
   (theCarbs ?p&:(fuzzy-match ?p "ok")) 
   (theCarbs ?p&:(fuzzy-match ?p "high"))
 => 
   (assert (carbsAdjust (new FuzzyValue ?*cchangeFvar* "NB")))
)

;carbs ok
(defrule c21
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(fuzzy-match ?t "high"))
   (theCarbs ?p&:(fuzzy-match ?p "ok")) 
   (theCarbs ?p&:(not (fuzzy-match ?p "high")))
   (theCarbs ?p&:(not (fuzzy-match ?p "low")))
 => 
   (assert (carbsAdjust (new FuzzyValue ?*cchangeFvar* "NM")))
)

;carbs medium low
(defrule c31
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(fuzzy-match ?t "high"))
   (theCarbs ?p&:(fuzzy-match ?p "ok")) 
   (theCarbs ?p&:(fuzzy-match ?p "low"))
 => 
   (assert (carbsAdjust (new FuzzyValue ?*cchangeFvar* "NM")))
)

;carbs low
(defrule c41
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(fuzzy-match ?t "high"))
   (theCarbs ?p&:(not (fuzzy-match ?p "ok")))
   (theCarbs ?p&:(fuzzy-match ?p "low"))
 => 
   (assert (carbsAdjust (new FuzzyValue ?*cchangeFvar* "Z")))
)

;fat high
(defrule f01
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(fuzzy-match ?t "high"))
   (theFat ?p&:(fuzzy-match ?p "high"))
   (theFat ?p&:(not (fuzzy-match ?p "ok")))
 => 
   (assert (FatAdjust (new FuzzyValue ?*fchangeFvar* "NB")))
)

;fat medium high
(defrule f11
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(fuzzy-match ?t "high"))
   (theFat ?p&:(fuzzy-match ?p "ok")) 
   (theFat ?p&:(fuzzy-match ?p "high"))
 => 
   (assert (FatAdjust (new FuzzyValue ?*fchangeFvar* "NB")))
)

;fat ok
(defrule f21
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(fuzzy-match ?t "high"))
   (theFat ?p&:(fuzzy-match ?p "ok")) 
   (theFat ?p&:(not (fuzzy-match ?p "high")))
   (theFat ?p&:(not (fuzzy-match ?p "low")))
 => 
   (assert (FatAdjust (new FuzzyValue ?*fchangeFvar* "NM")))
)

;fat medium low
(defrule f31
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(fuzzy-match ?t "high"))
   (theFat ?p&:(fuzzy-match ?p "ok")) 
   (theFat ?p&:(fuzzy-match ?p "low"))
 => 
   (assert (FatAdjust (new FuzzyValue ?*fchangeFvar* "Z")))
)

;fat low
(defrule f41
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(fuzzy-match ?t "high"))
   (theFat ?p&:(not (fuzzy-match ?p "ok")))
   (theFat ?p&:(fuzzy-match ?p "low"))
 => 
   (assert (FatAdjust (new FuzzyValue ?*fchangeFvar* "Z")))
)


;;;;;;;;;;;;;;;;;;;BW ok
;;protein high
(defrule p02
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(not (fuzzy-match ?t "high")))
   (theBW ?t&:(not (fuzzy-match ?t "low")))
   (theProtein ?p&:(fuzzy-match ?p "high"))
   (theProtein ?p&:(not (fuzzy-match ?p "ok")))
 => 
   (assert (proteinAdjust (new FuzzyValue ?*pchangeFvar* "NM")))
)

;;protein medium high
(defrule p12
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(not (fuzzy-match ?t "high")))
   (theBW ?t&:(not (fuzzy-match ?t "low")))
   (theProtein ?p&:(fuzzy-match ?p "high"))
   (theProtein ?p&:(fuzzy-match ?p "ok"))
 => 
   (assert (proteinAdjust (new FuzzyValue ?*pchangeFvar* "NM")))
)

;;protein ok
(defrule p22
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(not (fuzzy-match ?t "high")))
   (theBW ?t&:(not (fuzzy-match ?t "low")))
   (theProtein ?p&:(fuzzy-match ?p "ok")) 
   (theProtein ?p&:(not (fuzzy-match ?p "high")))
   (theProtein ?p&:(not (fuzzy-match ?p "low")))
 => 
   (assert (proteinAdjust (new FuzzyValue ?*pchangeFvar* "NM")))
)
;;protein medium low
(defrule p32
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(not (fuzzy-match ?t "high")))
   (theBW ?t&:(not (fuzzy-match ?t "low")))
   (theProtein ?p&:(fuzzy-match ?p "low")) 
   (theProtein ?p&:(fuzzy-match ?p "ok"))
 => 
   (assert (proteinAdjust (new FuzzyValue ?*pchangeFvar* "Z")))
)

;;protein low
(defrule p42
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(not (fuzzy-match ?t "high")))
   (theBW ?t&:(not (fuzzy-match ?t "low")))
   (theProtein ?p&:(fuzzy-match ?p "low")) 
   (theProtein ?p&:(not (fuzzy-match ?p "ok")))
 => 
   (assert (proteinAdjust (new FuzzyValue ?*pchangeFvar* "PM")))
)

;carbs high
(defrule c02
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(not (fuzzy-match ?t "high")))
   (theBW ?t&:(not (fuzzy-match ?t "low")))
   (theCarbs ?p&:(fuzzy-match ?p "high"))
   (theCarbs ?p&:(not (fuzzy-match ?p "ok")))
 => 
   (assert (carbsAdjust (new FuzzyValue ?*cchangeFvar* "NM")))
)

;carbs medium high
(defrule c12
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(not (fuzzy-match ?t "high")))
   (theBW ?t&:(not (fuzzy-match ?t "low")))
   (theCarbs ?p&:(fuzzy-match ?p "ok")) 
   (theCarbs ?p&:(fuzzy-match ?p "high"))
 => 
   (assert (carbsAdjust (new FuzzyValue ?*cchangeFvar* "NM")))
)

;carbs ok
(defrule c22
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(not (fuzzy-match ?t "high")))
   (theBW ?t&:(not (fuzzy-match ?t "low")))
   (theCarbs ?p&:(fuzzy-match ?p "ok")) 
   (theCarbs ?p&:(not (fuzzy-match ?p "high")))
   (theCarbs ?p&:(not (fuzzy-match ?p "low")))
 => 
   (assert (carbsAdjust (new FuzzyValue ?*cchangeFvar* "NM")))
)

;carbs medium low
(defrule c32
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(not (fuzzy-match ?t "high")))
   (theBW ?t&:(not (fuzzy-match ?t "low")))
   (theCarbs ?p&:(fuzzy-match ?p "ok")) 
   (theCarbs ?p&:(fuzzy-match ?p "low"))
 => 
   (assert (carbsAdjust (new FuzzyValue ?*cchangeFvar* "Z")))
)

;carbs low
(defrule c42
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(not (fuzzy-match ?t "high")))
   (theBW ?t&:(not (fuzzy-match ?t "low")))
   (theCarbs ?p&:(not (fuzzy-match ?p "ok")))
   (theCarbs ?p&:(fuzzy-match ?p "low"))
 => 
   (assert (carbsAdjust (new FuzzyValue ?*cchangeFvar* "Z")))
)

;fat high
(defrule f02
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(not (fuzzy-match ?t "high")))
   (theBW ?t&:(not (fuzzy-match ?t "low")))
   (theFat ?p&:(fuzzy-match ?p "high"))
   (theFat ?p&:(not (fuzzy-match ?p "ok")))
 => 
   (assert (FatAdjust (new FuzzyValue ?*fchangeFvar* "NM")))
)

;fat medium high
(defrule f12
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(not (fuzzy-match ?t "high")))
   (theBW ?t&:(not (fuzzy-match ?t "low")))
   (theFat ?p&:(fuzzy-match ?p "ok")) 
   (theFat ?p&:(fuzzy-match ?p "high"))
 => 
   (assert (FatAdjust (new FuzzyValue ?*fchangeFvar* "NM")))
)

;fat ok
(defrule f22
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(not (fuzzy-match ?t "high")))
   (theBW ?t&:(not (fuzzy-match ?t "low")))
   (theFat ?p&:(fuzzy-match ?p "ok")) 
   (theFat ?p&:(not (fuzzy-match ?p "high")))
   (theFat ?p&:(not (fuzzy-match ?p "low")))
 => 
   (assert (FatAdjust (new FuzzyValue ?*fchangeFvar* "Z")))
)

;fat medium low
(defrule f32
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(not (fuzzy-match ?t "high")))
   (theBW ?t&:(not (fuzzy-match ?t "low")))
   (theFat ?p&:(fuzzy-match ?p "ok")) 
   (theFat ?p&:(fuzzy-match ?p "low"))
 => 
   (assert (FatAdjust (new FuzzyValue ?*fchangeFvar* "Z")))
)

;fat low
(defrule f42
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(not (fuzzy-match ?t "high")))
   (theBW ?t&:(not (fuzzy-match ?t "low")))
   (theFat ?p&:(not (fuzzy-match ?p "ok")))
   (theFat ?p&:(fuzzy-match ?p "low"))
 => 
   (assert (FatAdjust (new FuzzyValue ?*fchangeFvar* "PM")))
)


;;;;;;;;;;;;;;;;;;;BW medium low
;;protein high
(defrule p03
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(fuzzy-match ?t "low"))
   (theProtein ?p&:(fuzzy-match ?p "high"))
   (theProtein ?p&:(not (fuzzy-match ?p "ok")))
 => 
   (assert (proteinAdjust (new FuzzyValue ?*pchangeFvar* "Z")))
)

;;protein medium high
(defrule p13
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(fuzzy-match ?t "low"))
   (theProtein ?p&:(fuzzy-match ?p "high"))
   (theProtein ?p&:(fuzzy-match ?p "ok"))
 => 
   (assert (proteinAdjust (new FuzzyValue ?*pchangeFvar* "Z")))
)

;;protein ok
(defrule p23
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(fuzzy-match ?t "low"))
   (theProtein ?p&:(fuzzy-match ?p "ok")) 
   (theProtein ?p&:(not (fuzzy-match ?p "high")))
   (theProtein ?p&:(not (fuzzy-match ?p "low")))
 => 
   (assert (proteinAdjust (new FuzzyValue ?*pchangeFvar* "Z")))
)
;;protein medium low
(defrule p33
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(fuzzy-match ?t "low"))
   (theProtein ?p&:(fuzzy-match ?p "low")) 
   (theProtein ?p&:(fuzzy-match ?p "ok"))
 => 
   (assert (proteinAdjust (new FuzzyValue ?*pchangeFvar* "Z")))
)

;;protein low
(defrule p43
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(fuzzy-match ?t "low"))
   (theProtein ?p&:(fuzzy-match ?p "low")) 
   (theProtein ?p&:(not (fuzzy-match ?p "ok")))
 => 
   (assert (proteinAdjust (new FuzzyValue ?*pchangeFvar* "Z")))
)

;carbs high
(defrule c03
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(fuzzy-match ?t "low"))
   (theCarbs ?p&:(fuzzy-match ?p "high"))
   (theCarbs ?p&:(not (fuzzy-match ?p "ok")))
 => 
   (assert (carbsAdjust (new FuzzyValue ?*cchangeFvar* "Z")))
)

;carbs medium high
(defrule c13
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(fuzzy-match ?t "low"))
   (theCarbs ?p&:(fuzzy-match ?p "ok")) 
   (theCarbs ?p&:(fuzzy-match ?p "high"))
 => 
   (assert (carbsAdjust (new FuzzyValue ?*cchangeFvar* "Z")))
)

;carbs ok
(defrule c23
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(fuzzy-match ?t "low"))
   (theCarbs ?p&:(fuzzy-match ?p "ok")) 
   (theCarbs ?p&:(not (fuzzy-match ?p "high")))
   (theCarbs ?p&:(not (fuzzy-match ?p "low")))
 => 
   (assert (carbsAdjust (new FuzzyValue ?*cchangeFvar* "Z")))
)

;carbs medium low
(defrule c33
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(fuzzy-match ?t "low"))
   (theCarbs ?p&:(fuzzy-match ?p "ok")) 
   (theCarbs ?p&:(fuzzy-match ?p "low"))
 => 
   (assert (carbsAdjust (new FuzzyValue ?*cchangeFvar* "Z")))
)

;carbs low
(defrule c43
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(fuzzy-match ?t "low"))
   (theCarbs ?p&:(not (fuzzy-match ?p "ok")))
   (theCarbs ?p&:(fuzzy-match ?p "low"))
 => 
   (assert (carbsAdjust (new FuzzyValue ?*cchangeFvar* "Z")))
)

;fat high
(defrule f03
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(fuzzy-match ?t "low"))
   (theFat ?p&:(fuzzy-match ?p "high"))
   (theFat ?p&:(not (fuzzy-match ?p "ok")))
 => 
   (assert (FatAdjust (new FuzzyValue ?*fchangeFvar* "Z")))
)

;fat medium high
(defrule f13
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(fuzzy-match ?t "low"))
   (theFat ?p&:(fuzzy-match ?p "ok")) 
   (theFat ?p&:(fuzzy-match ?p "high"))
 => 
   (assert (FatAdjust (new FuzzyValue ?*fchangeFvar* "Z")))
)

;fat ok
(defrule f23
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(fuzzy-match ?t "low"))
   (theFat ?p&:(fuzzy-match ?p "ok")) 
   (theFat ?p&:(not (fuzzy-match ?p "high")))
   (theFat ?p&:(not (fuzzy-match ?p "low")))
 => 
   (assert (FatAdjust (new FuzzyValue ?*fchangeFvar* "Z")))
)

;fat medium low
(defrule f33
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(fuzzy-match ?t "low"))
   (theFat ?p&:(fuzzy-match ?p "ok")) 
   (theFat ?p&:(fuzzy-match ?p "low"))
 => 
   (assert (FatAdjust (new FuzzyValue ?*fchangeFvar* "Z")))
)

;fat low
(defrule f43
   (theBW ?t&:(fuzzy-match ?t "ok")) 
   (theBW ?t&:(fuzzy-match ?t "low"))
   (theFat ?p&:(not (fuzzy-match ?p "ok")))
   (theFat ?p&:(fuzzy-match ?p "low"))
 => 
   (assert (FatAdjust (new FuzzyValue ?*fchangeFvar* "Z")))
)


;;;;;;;;;;;;;;;;;;;BW low
;;protein high
(defrule p04
   (theBW ?t&:(not (fuzzy-match ?t "ok")))
   (theBW ?t&:(fuzzy-match ?t "low"))
   (theProtein ?p&:(fuzzy-match ?p "high"))
   (theProtein ?p&:(not (fuzzy-match ?p "ok")))
 => 
   (assert (proteinAdjust (new FuzzyValue ?*pchangeFvar* "Z")))
)

;;protein medium high
(defrule p14
   (theBW ?t&:(not (fuzzy-match ?t "ok")))
   (theBW ?t&:(fuzzy-match ?t "low"))
   (theProtein ?p&:(fuzzy-match ?p "high"))
   (theProtein ?p&:(fuzzy-match ?p "ok"))
 => 
   (assert (proteinAdjust (new FuzzyValue ?*pchangeFvar* "Z")))
)

;;protein ok
(defrule p24
   (theBW ?t&:(not (fuzzy-match ?t "ok")))
   (theBW ?t&:(fuzzy-match ?t "low"))
   (theProtein ?p&:(fuzzy-match ?t "ok")) 
   (theProtein ?p&:(not (fuzzy-match ?p "high")))
   (theProtein ?p&:(not (fuzzy-match ?p "low")))
 => 
   (assert (proteinAdjust (new FuzzyValue ?*pchangeFvar* "PM")))
)
;;protein medium low
(defrule p34
   (theBW ?t&:(not (fuzzy-match ?t "ok")))
   (theBW ?t&:(fuzzy-match ?t "low"))
   (theProtein ?p&:(fuzzy-match ?p "low")) 
   (theProtein ?p&:(fuzzy-match ?p "ok"))
 => 
   (assert (proteinAdjust (new FuzzyValue ?*pchangeFvar* "PB")))
)

;;protein low
(defrule p44
   (theBW ?t&:(not (fuzzy-match ?t "ok")))
   (theBW ?t&:(fuzzy-match ?t "low"))
   (theProtein ?p&:(fuzzy-match ?p "low")) 
   (theProtein ?p&:(not (fuzzy-match ?p "ok")))
 => 
   (assert (proteinAdjust (new FuzzyValue ?*pchangeFvar* "PB")))
)

;carbs high
(defrule c04
   (theBW ?t&:(not (fuzzy-match ?t "ok")))
   (theBW ?t&:(fuzzy-match ?t "low"))
   (theCarbs ?p&:(fuzzy-match ?p "high"))
   (theCarbs ?p&:(not (fuzzy-match ?p "ok")))
 => 
   (assert (carbsAdjust (new FuzzyValue ?*cchangeFvar* "PM")))
)

;carbs medium high
(defrule c14
   (theBW ?t&:(not (fuzzy-match ?t "ok")))
   (theBW ?t&:(fuzzy-match ?t "low"))
   (theCarbs ?p&:(fuzzy-match ?p "ok")) 
   (theCarbs ?p&:(fuzzy-match ?p "high"))
 => 
   (assert (carbsAdjust (new FuzzyValue ?*cchangeFvar* "PM")))
)

;carbs ok
(defrule c24
   (theBW ?t&:(not (fuzzy-match ?t "ok")))
   (theBW ?t&:(fuzzy-match ?t "low"))
   (theCarbs ?p&:(fuzzy-match ?p "ok")) 
   (theCarbs ?p&:(not (fuzzy-match ?p "high")))
   (theCarbs ?p&:(not (fuzzy-match ?p "low")))
 => 
   (assert (carbsAdjust (new FuzzyValue ?*cchangeFvar* "PB")))
)

;carbs medium low
(defrule c34
   (theBW ?t&:(not (fuzzy-match ?t "ok")))
   (theBW ?t&:(fuzzy-match ?t "low"))
   (theCarbs ?p&:(fuzzy-match ?p "ok")) 
   (theCarbs ?p&:(fuzzy-match ?p "low"))
 => 
   (assert (carbsAdjust (new FuzzyValue ?*cchangeFvar* "PB")))
)

;carbs low
(defrule c44
   (theBW ?t&:(not (fuzzy-match ?t "ok")))
   (theBW ?t&:(fuzzy-match ?t "low"))
   (theCarbs ?p&:(not (fuzzy-match ?p "ok")))
   (theCarbs ?p&:(fuzzy-match ?p "low"))
 => 
   (assert (carbsAdjust (new FuzzyValue ?*cchangeFvar* "PB")))
)

;fat high
(defrule f04
   (theBW ?t&:(not (fuzzy-match ?t "ok")))
   (theBW ?t&:(fuzzy-match ?t "low"))
   (theFat ?p&:(fuzzy-match ?p "high"))
   (theFat ?p&:(not (fuzzy-match ?p "ok")))
 => 
   (assert (FatAdjust (new FuzzyValue ?*fchangeFvar* "Z")))
)

;fat medium high
(defrule f14
   (theBW ?t&:(not (fuzzy-match ?t "ok")))
   (theBW ?t&:(fuzzy-match ?t "low"))
   (theFat ?p&:(fuzzy-match ?p "ok")) 
   (theFat ?p&:(fuzzy-match ?p "high"))
 => 
   (assert (FatAdjust (new FuzzyValue ?*fchangeFvar* "Z")))
)

;fat ok
(defrule f24
   (theBW ?t&:(not (fuzzy-match ?t "ok")))
   (theBW ?t&:(fuzzy-match ?t "low"))
   (theFat ?p&:(fuzzy-match ?p "ok")) 
   (theFat ?p&:(not (fuzzy-match ?p "high")))
   (theFat ?p&:(not (fuzzy-match ?p "low")))
 => 
   (assert (FatAdjust (new FuzzyValue ?*fchangeFvar* "PM")))
)

;fat medium low
(defrule f34
   (theBW ?t&:(not (fuzzy-match ?t "ok")))
   (theBW ?t&:(fuzzy-match ?t "low"))
   (theFat ?p&:(fuzzy-match ?p "ok")) 
   (theFat ?p&:(fuzzy-match ?p "low"))
 => 
   (assert (FatAdjust (new FuzzyValue ?*fchangeFvar* "PB")))
)

;fat low
(defrule f44
   (theBW ?t&:(not (fuzzy-match ?t "ok")))
   (theBW ?t&:(fuzzy-match ?t "low"))
   (theFat ?p&:(not (fuzzy-match ?p "ok")))
   (theFat ?p&:(fuzzy-match ?p "low"))
 => 
   (assert (FatAdjust (new FuzzyValue ?*fchangeFvar* "PB")))
)

(defrule defuzz
   ?fa <- (FatAdjust ?af)
   ?ca <- (carbsAdjust ?ac)
   ?pa <- (proteinAdjust ?ap)
  =>
   (assert (crisp FatAdjust (?af momentDefuzzify)))
   (assert (crisp carbsAdjust (?ac momentDefuzzify)))
   (assert (crisp proteinAdjust (?ap momentDefuzzify)))
)

;get our adjustments
(defrule next_macros
   ?cf <- (crisp FatAdjust ?cfa)
   ?cc <- (crisp carbsAdjust ?cca)
   ?cp <- (crisp proteinAdjust ?cpa)
   ?actualMacros <- (target {(protein != nil || carbs != nil || fats != nil || weight != nil) && user == 1})
  =>
   (printout t "Adjust f: " (* ?actualMacros.fats ?cfa) " c: " (* ?actualMacros.carbs ?cca) " p: "(* ?actualMacros.protein ?cpa) crlf)
)    


(reset)
;set your target macronutrients here
(assert (target(protein 160) (carbs 260) (fats 80) (weight 160) (user 0)))
(run)
