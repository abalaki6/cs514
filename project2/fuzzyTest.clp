(defglobal ?*numSympt* = (new nrc.fuzzy.FuzzyVariable "number of symptoms" 0.0 10 "number"))

;template for the disease
(deftemplate Symptom (slot name) (slot dueDrugs (default FALSE)) (slot wasUsed (default FALSE) ) )

(deftemplate SymptomsCounter (slot name) (slot number))
(defrule init
    ;(declare (salience 100))
    =>
    (load-package nrc.fuzzy.jess.FuzzyFunctions)
    (bind ?xMDD (create$ 0.0 5.0))
    (bind ?yMDD (create$ 0.0 1.0))
    (?*numSympt* addTerm "MDD" ?xMDD ?yMDD 2)

    ;; symptoms
    (assert (Symptom (name "s1")))
    (assert (Symptom (name "s2")))

    (assert (SymptomsCounter (name "theMDD")
                       ( number
                            (new nrc.fuzzy.FuzzyValue ?*numSympt*
                                (new nrc.fuzzy.SingletonFuzzySet 1.0)
                            )
                       )
                   )

    )

    ;(assert theMDD(new nrc.fuzzy.FuzzyValue ?*numSympt* "MDD" (new nrc.fuzzy.SingletonFuzzySet 0.0)))
)

(defrule update
    ?s <-(Symptom (dueDrugs FALSE) (wasUsed FALSE))
    ?d <-(SymptomsCounter (name "theMDD"))
    =>
    (printout t "here" crlf)
    (printout t "before update " (?d.number maximumDefuzzify) crlf)

    (printout t (?d.number toString) crlf)
    (assert (SymptomsCounter (name "theMDD")
                  ( number (new nrc.fuzzy.FuzzyValue ?*numSympt*
                       (new nrc.fuzzy.SingletonFuzzySet (+ (?d.number maximumDefuzzify) 1)) ))
               )
    )
    (modify ?s(wasUsed TRUE))
    (retract ?d)
)

(defrule printS
    ?d <-(Symptom)
    =>
    (printout t ?d.name crlf)
)

(defrule printM
    ?m <-(SymptomsCounter)
    =>
    (printout t (?m.number maximumDefuzzify) crlf (?m.number toString) crlf)
)


;; a rule to print some interesting things
(defrule do-the-printing
   (theTemp ?t)
   (thePress ?p)
 =>
   (printout t "Temp is: " (?t toString) crlf "Press is: " (?p toString) crlf)
   (bind ?theFzVals
        (create$ (new nrc.fuzzy.FuzzyValue ?*tempFvar* "hot") ?t)
   )
   (printout t (call nrc.fuzzy.FuzzyValue plotFuzzyValues "*+" ?theFzVals) crlf)
   (printout t (call (new nrc.fuzzy.FuzzyValue ?*pressFvar* "low or medium")
                      plotFuzzyValue "*") crlf)
   (printout t (?p plotFuzzyValue "*") crlf)
)

