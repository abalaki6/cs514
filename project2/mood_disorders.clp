/**
 * CS514
 * Project 2
 * Mood disorders
 * @author Wolf
 */

;fuzzy logic variable for number of Symptoms of the disorders
(defglobal ?*numSymptoms* = (new nrc.fuzzy.FuzzyVariable "number of symptoms" 0.0 10 "number"))

;template for disorder
(deftemplate Disorder (declare (ordered TRUE)))

;template for any bipolar disorder
(deftemplate Bipolar (declare (ordered TRUE)))

;template for any fuzzy bipolar and other disorders
(deftemplate FuzzyDisorder (slot name) (slot fuzzyValue))

;fuzzy variable for the disorder
(deftemplate FuzzySymptom (slot name) (slot fuzzyValue))

;template for the disease
(deftemplate Symptom (slot name) (slot dueDrugs (default FALSE)) (slot wasUsed (default FALSE) ) )

;template for the period of the depression
;measures in weeks
(deftemplate Time (slot duration (type float) (default 0) ) )

;template for the age of the patient
(deftemplate Age (slot years (type integer) (default 0) ))

;template for counter of symptoms, if required number achieved has to fire fact about possibility of disease
(deftemplate TotalNumberDiseaseSymptoms (slot name) (slot count (type float) (default 0) ) (slot requires (type float) ) )

;define all TotalNumberDiseaseSymptoms facts
(deffacts TNDS
"List of all pre disease symptoms."

    (TotalNumberDiseaseSymptoms (name "_symptoms_major_depressive_disorder") (requires 5))
    (TotalNumberDiseaseSymptoms (name "_symptoms_manic_episode") (requires 3))
    (TotalNumberDiseaseSymptoms (name "_symptoms_hypomanic_episode") (requires 4))
    (TotalNumberDiseaseSymptoms (name "_symptoms_dysthymic_disorder") (requires 2))
)

(defrule init
    (declare (salience 100))
    =>
    (load-package nrc.fuzzy.jess.FuzzyFunctions)
    ;;add them to the fuzzy variable base
    (bind ?y (create$ 0.0 1.0))
    (bind ?mdd (create$ 0.0 5.0))
    (bind ?me (create$ 0.0 3.0))
    (bind ?he (create$ 0.0 4.0))
    (bind ?dd (create$ 0.0 2.0))

    (?*numSymptoms* addTerm "fuzzy_symptoms_major_depressive_disorder" ?mdd ?y 2)
    (?*numSymptoms* addTerm "fuzzy_symptoms_manic_episode" ?me ?y 2)
    (?*numSymptoms* addTerm "fuzzy_symptoms_hypomanic_episode" ?he ?y 2)
    (?*numSymptoms* addTerm "fuzzy_symptoms_dysthymic_disorder" ?dd ?y 2)

)

;;;;;;;;;
;evaluate disorders symptoms after all symptoms were counted
;;;;;;;;;

(defrule fuzzyNumberDiseaseSymptoms
    (not (Symptom (wasUsed FALSE)))
    ?s <- (TotalNumberDiseaseSymptoms)
    =>
    (?*numSymptoms* addTerm ?s.name (new nrc.fuzzy.SingletonFuzzySet ?s.count))
    ;(?*numSymptoms* addTerm "merge" "_symptoms_major_depressive_disorder and fuzzy_symptoms_major_depressive_disorder" )
    (assert (FuzzySymptom (name ?s.name) (fuzzyValue (new nrc.fuzzy.FuzzyValue ?*numSymptoms* (new nrc.fuzzy.SingletonFuzzySet ?s.count)))))
)

(defrule mddFuzzification
    (FuzzySymptom (name "_symptoms_major_depressive_disorder"))
    =>
    (?*numSymptoms* addTerm "mdd" "fuzzy_symptoms_major_depressive_disorder and _symptoms_major_depressive_disorder")
)

(defrule meFuzzification
    (FuzzySymptom (name "_symptoms_manic_episode"))
    =>
    (?*numSymptoms* addTerm "me" "fuzzy_symptoms_manic_episode and _symptoms_manic_episode")
)

(defrule heFuzzification
     (FuzzySymptom (name "_symptoms_hypomanic_episode"))
     =>
     (?*numSymptoms* addTerm "he" "fuzzy_symptoms_hypomanic_episode and _symptoms_hypomanic_episode")
 )

 (defrule ddFuzzification
      (FuzzySymptom (name "_symptoms_dysthymic_disorder"))
      =>
      (?*numSymptoms* addTerm "dd" "fuzzy_symptoms_dysthymic_disorder and _symptoms_dysthymic_disorder")
  )

;rule to add fact if symptoms are exist
(defrule hasSymptoms
    ?d <- ( TotalNumberDiseaseSymptoms (name ?name) { count >= requires})
    =>
    (assert (Symptom (name ?d.name) (wasUsed TRUE) ) )
)

(defrule isChild
"Add to KB Child if patient's age is below 20"
    (Age {years < 20})
    =>
    (assert (Child))
)

(defrule isAdult
"Add to KB Adult of patient's age is 20 and above"
    (Age {years >= 20})
    =>
    (assert (Adult))
)

;;;;;;;;;;;
;DISORDERS
;;;;;;;;;;;

(defrule isMajorDepressiveDisorder
"Determine if the patient has major depressive disorder"
    (not (Symptom(wasUsed FALSE)))
    ;(Symptom (name "_symptoms_major_depressive_disorder"))
    ;(not (Disorder "Mixed Episode"))
    (Symptom (name "_symptom_social_functioning") (dueDrugs FALSE))
    (not (Symptom (name "_symptom_suicidal")))
    =>
    (assert (Disorder "Major Depressive Disorder") )
    (?*numSymptoms* addTerm "Disorder_mdd" "mdd and (fuzzy_symptoms_major_depressive_disorder and  (not fuzzy_symptoms_manic_episode)")
    (bind ?x (new nrc.fuzzy.FuzzyValue ?*numSymptoms* "Disorder_mdd"))
    (assert (FuzzyDisorder (name "Major Depressive Disorder") (fuzzyValue (?x getMaxY)) ))
 )

(defrule isMixedEpisode
"Determine if the patient has mixed episode"
    (not (Symptom(wasUsed FALSE)))
    ;(Symptom (name "_symptoms_major_depressive_disorder"))
    ;(or
    ;    (Symptom (name "_symptoms_manic_episode"))
    ;    (Symptom (name "_symptoms_hypomanic_episode"))
    ;)
    (Symptom (name "_symptom_excessive_activities_painful") (dueDrugs FALSE))
    (Time {duration >= 1})
    =>
    (assert (Disorder "Mixed Episode") )
    (?*numSymptoms* addTerm "Disorder_mixe" "me and fuzzy_symptoms_manic_episode and (fuzzy_symptoms_manic_episode or fuzzy_symptoms_hypomanic_episode)")
    (bind ?x (new nrc.fuzzy.FuzzyValue ?*numSymptoms* "Disorder_mixe"))
    (assert (FuzzyDisorder (name "Mixed Episode") (fuzzyValue (?x getMaxY)) ))

)

(defrule isManicEpisode
"Determine if the patient has manic episode"
    (not (Symptom(wasUsed FALSE)))
    ;(Symptom (name "_symptoms_manic_episode") )
    ;(not (Disorder "Mixed Episode") )
    (Time {duration >= 1} )
    =>
    (assert (Disorder "Manic Episode") )
    (?*numSymptoms* addTerm "Disorder_me" "me and fuzzy_symptoms_manic_episode")
    (bind ?x (new nrc.fuzzy.FuzzyValue ?*numSymptoms* "Disorder_me"))
    (assert (FuzzyDisorder (name "Manic Episode") (fuzzyValue (?x getMaxY)) ))
)

(defrule isHypomanicEpisode
"Determine if the patient has hypomanic episode"
    (not (Symptom(wasUsed FALSE)))
    ;(Symptom (name "_symptoms_hypomanic_episode") )
    (Symptom (name "_symptom_social_functioning") (dueDrugs FALSE) )
    (Symptom (name "_symptom_changes_in_personality") (dueDrugs FALSE) )
    (Time {duration >= 0.5})
    =>
    (assert (Disorder "Hypomanic Episode") )
    (?*numSymptoms* addTerm "Disorder_he" "he and fuzzy_symptoms_hypomanic_episode")
    (bind ?x (new nrc.fuzzy.FuzzyValue ?*numSymptoms* "Disorder_he"))
    (assert (FuzzyDisorder (name "Hypomanic Episode") (fuzzyValue (?x getMaxY)) ))
)

(defrule isCyclothymicDisorder
"Determine if the patient has cyclothymic disorder"
    (not (Symptom(wasUsed FALSE)))
    (Symptom (name "_symptom_suicidal") (dueDrugs FALSE) )
    (Symptom (name "_symptoms_hypomanic_episode") )
    (Symptom (name "_symptom_depression_most_of_the_day") (dueDrugs FALSE) )
    ;(not (Symptom (name "_symptoms_major_depressive_disorder") ) )
    (or
        (and
            (Child)
            (Time {duration >= 52} ) ;1 year minimum for children
        )
        (and
            (Adult)
            (Time {duration >= 104} ) ;2 years minimum for adults
        )
    )
    ;(not (Disorder "Manic Episode") )
    ;(not (Disorder "Mixed Episode") )
    (not (Symptom (name "_symptom_schizophrenia") ) )
    =>
    (assert (Disorder "Cyclothymic Disorder") )
    (?*numSymptoms* addTerm "Disorder_ch" "not fuzzy_symptoms_major_depressive_disorder and not fuzzy_symptoms_manic_episode and me")
    (bind ?x (new nrc.fuzzy.FuzzyValue ?*numSymptoms* "Disorder_ch"))
    (assert (FuzzyDisorder (name "Cyclothymic Disorder") (fuzzyValue (?x getMaxY)) ))
)


(defrule isDysthymicDisorder
"Determine if the patient has dysthymic disorder"
    (not (Symptom(wasUsed FALSE)))
    (Symptom (name "_symptoms_dysthymic_disorder") )
    (not (Disorder "Major Depressive Disorder") )
    (not (Disorder "Manic Episode") )
    (not (Disorder "Mixed Episode") )
    (not (Disorder "Cyclothymic Disorder") )
    ;(or
    ;    (not (Symptom (name "_symptom_schizophrenia") (dueDrugs FALSE) ) )
    ;    (not (Disorder "Delusional Disorder") )
    ;)
    ;(not (Symptom (name "_symptom_social_functioning") (dueDrugs FALSE) ) )
    (or
        (and
            (Child)
            (Time {duration >= 52} ) ;1 year minimum for children
        )
        (and
            (Adult)
            (Time {duration >= 104} ) ;2 years minimum for adults
        )
    )
    =>
    (assert (Disorder "Dysthymic Disorder") )
)


;we can not have Manic and Hypomanic at the same time
 (defrule notManicAndHypomanic
    ?d <- (Disorder "Manic Episode")
    (Disorder "Hypomanic Episide")
    =>
    (retract ?d)
 )

;;;;
;Bipolar classification
;;;;

(defrule isBipolarIDisorderWithSingleManicEpisode
    (not (Symptom(wasUsed FALSE)))
    (Disorder "Manic Episode")
    (not (Disorder "Major Depressive Disorder") )
    (not (Symptom (name "_symptom_schizophrenia") (dueDrugs FALSE) ) )
    =>
    (assert (Bipolar "Bipolar I Disorder, single Manic Episode") )
    (?*numSymptoms* addTerm "Bipolar_I_ME" "Disorder_mdd and Disorder_me")
    (bind ?x (new nrc.fuzzy.FuzzyValue ?*numSymptoms* "Bipolar_I_ME"))
    (assert (FuzzyDisorder (name "Bipolar I Disorder, single Manic Episode") (fuzzyValue (?x getMaxY)) ))

)

(defrule isBipolarIDisorderRecentHypomanic
    (not (Symptom(wasUsed FALSE)))
    (Disorder "Hypomanic Episode")
    (Symptom (name "_symptom_social_functioning") (dueDrugs FALSE) )
    (not (Symptom (name "_symptom_schizophrenia") (dueDrugs FALSE) ) )
    =>
    (assert (Bipolar "Bipolar I Disorder, most recent Episode Hypomanic"))
    (?*numSymptoms* addTerm "Bipolar_I_EH" "Disorder_he")
    (bind ?x (new nrc.fuzzy.FuzzyValue ?*numSymptoms* "Bipolar_I_EH"))
    (assert (FuzzyDisorder (name "Bipolar I Disorder, most recent Episode Hypomanic") (fuzzyValue (?x getMaxY)) ))

)

(defrule isBipolarIDisorderRecentManic
    (not (Symptom(wasUsed FALSE)))
    (Disorder "Manic Episode")
    (not (Symptom( name "_symptom_schizophrenia") (dueDrugs FALSE) ) )
    =>
    (assert (Bipolar "Bipolar I Disorder, most recent Episode Manic") )
    (?*numSymptoms* addTerm "Bipolar_I_EM" "Disorder_me")
    (bind ?x (new nrc.fuzzy.FuzzyValue ?*numSymptoms* "Bipolar_I_EM"))
    (assert (FuzzyDisorder (name "Bipolar I Disorder, most recent Episode Manic") (fuzzyValue (?x getMaxY)) ))

)

(defrule isBipolarIDisorderRecentMixed
    (not (Symptom(wasUsed FALSE)))
    (Disorder "Mixed Episode")
    (not (Symptom (name "_symptom_schizophrenia") (dueDrugs FALSE) ) )
    =>
    (assert (Bipolar "Bipolar I Disorder, most recent Episode Mixed") )
    (?*numSymptoms* addTerm "Bipolar_I_MIXE" "Disorder_mixe")
    (bind ?x (new nrc.fuzzy.FuzzyValue ?*numSymptoms* "Bipolar_I_MIXE"))
    (assert (FuzzyDisorder (name "MBipolar I Disorder, most recent Episode Mixed") (fuzzyValue (?x getMaxY)) ))
)

(defrule isBipolarIDisorderRecentDepressed
    (not (Symptom(wasUsed FALSE)))
    (Disorder "Major Depressive Episode")
    (not (Symptom (name "_symptom_schizophrenia") (dueDrugs FALSE) ) )
    =>
    (assert (Bipolar "Bipolar I Disorder, most recent Episode Depressed") )
    (?*numSymptoms* addTerm "Bipolar_I_MDD" "Disorder_mdd")
    (bind ?x (new nrc.fuzzy.FuzzyValue ?*numSymptoms* "Bipolar_I_MDD"))
    (assert (FuzzyDisorder (name "Bipolar I Disorder, most recent Episode Depressed") (fuzzyValue (?x getMaxY)) ))
)

(defrule isBipolarIDisorderUnspecified
    (not (Symptom(wasUsed FALSE)))
    ;(or
        ;(Symptom (name "_symptoms_manic_episode") )
        ;(Symptom (name "_symptoms_hypomanic_episode") )
        ;(Symptom (name "_symptoms_major_depressive_disorder") )
        ;(and
             ;(Symptom (name "_symptoms_major_depressive_disorder"))
             ;(or
             ;    (Symptom (name "_symptoms_manic_episode"))
             ;    (Symptom (name "_symptoms_hypomanic_episode"))
             ;)
             ;(Symptom (name "_symptom_excessive_activities_painful") (dueDrugs FALSE))
        ;)
    ;)
    (Symptom (name "_symptom_social_functioning") (dueDrugs FALSE) )
    (Symptom (name "_symptom_schizophrenia") (dueDrugs FALSE) )
    =>
    (assert (Bipolar "Bipolar I Disorder, most recent Episode Unspecified") )
    (?*numSymptoms* addTerm "Bipolar_I_Unspecified" "mdd and fuzzy_symptoms_major_depressive_disorder or fuzzy_symptoms_manic_episode or fuzzy_symptoms_hypomanic_episode or (fuzzy_symptoms_major_depressive_disorder and (fuzzy_symptoms_hypomanic_episode or fuzzy_symptoms_manic_episode))")
    (bind ?x (new nrc.fuzzy.FuzzyValue ?*numSymptoms* "Bipolar_I_Unspecified"))
    (assert (FuzzyDisorder (name "Bipolar I Disorder, most recent Episode Unspecified") (fuzzyValue (?x getMaxY)) )))

(defrule isBipolarIIDisorder
    (not (Symptom(wasUsed FALSE)))
    ;(Symptom (name "_symptoms_major_depressive_disorder") )
    (not (Symptom (name "_symptom_schizophrenia") (dueDrugs FALSE) ) )
    (Symptom (name "_symptom_social_functioning") (dueDrugs FALSE) )
    =>
    (assert (Bipolar "Bipolar II Disorder") )
    (?*numSymptoms* addTerm "BipolarII" "mdd and fuzzy_symptoms_major_depressive_disorder")
    (bind ?x (new nrc.fuzzy.FuzzyValue ?*numSymptoms* "BipolarII"))
    (assert (FuzzyDisorder (name "Bipolar II Disorder") (fuzzyValue (?x getMaxY)) ))
)

(defrule NotBiPolarAndDisorder
    ?d <- (Disorder ?q)
    (Bipolar ?b)
    =>
    (retract ?d)
)


;;;;;;;;;;
;END OF DISORDERS
;;;;;;;;;;


;;;;;;;;;;
;SYMPTOMS
;;;;;;;;;;

;;some corrections
(defrule symptomsCorrection1
    ?s <- (Symptom (name "_symptom_changes_in_personality"))
    =>
    (modify ?s(wasUsed TRUE))
)
(defrule symptomsCorrection2
    ?s <- (Symptom (name "_symptom_social_functioning"))
    =>
    (modify ?s(wasUsed TRUE))
)
(defrule symptomsCorrection3
    ?s <- (Symptom (name "_symptom_schizophrenia"))
    =>
    (modify ?s(wasUsed TRUE))
)

;rule 1
(defrule has_symptom_depression_most_of_the_day
    ?s <-(Symptom (name "_symptom_depression_most_of_the_day") (dueDrugs FALSE) (wasUsed FALSE))
    ?d <-(TotalNumberDiseaseSymptoms (name "_symptoms_major_depressive_disorder"))
    =>
    (modify ?s(wasUsed TRUE))
    (modify ?d(count (+ ?d.count 1)))
)

;rule 2
(defrule has_symptom_markedly_diminished_interest
    ?s <-(Symptom (name "_symptom_markedly_diminished_interest") (dueDrugs FALSE) (wasUsed FALSE))
    ?d <-(TotalNumberDiseaseSymptoms (name "_symptoms_major_depressive_disorder"))
    =>
    (modify ?s(wasUsed TRUE))
    (modify ?d(count (+ ?d.count 1)))
)

;rule 3
(defrule has_symptom_weight_lost
    ?s <-(Symptom (name "_symptom_weight_lost") (dueDrugs FALSE) (wasUsed FALSE))
    ?d <-(TotalNumberDiseaseSymptoms (name "_symptoms_major_depressive_disorder"))
    =>
    (modify ?s(wasUsed TRUE))
    (modify ?d(count (+ ?d.count 1)))
)

;rule 4
(defrule has_symptom_insomnia_or_hypersomnia
    ?s <-(Symptom (name "_symptom_insomnia_or_hypersomnia") (dueDrugs FALSE) (wasUsed FALSE))
    ?d <-(TotalNumberDiseaseSymptoms (name "_symptoms_major_depressive_disorder"))
    ?d2 <-(TotalNumberDiseaseSymptoms (name "_symptoms_dysthymic_disorder"))
    =>
    (modify ?s(wasUsed TRUE))
    (modify ?d(count (+ ?d.count 1)))
    (modify ?d2(count (+ ?d2.count 1)))
)

;rule 5
(defrule has_symptom_retardation
    ?s <-(Symptom (name "_symptom_retardation") (dueDrugs FALSE) (wasUsed FALSE))
    ?d <-(TotalNumberDiseaseSymptoms (name "_symptoms_major_depressive_disorder"))
    =>
    (modify ?s(wasUsed TRUE))
    (modify ?d(count (+ ?d.count 1)))
)

;rule 6
(defrule has_symptom_loss_of_energy
    ?s <-(Symptom (name "_symptom_loss_of_energy") (dueDrugs FALSE) (wasUsed FALSE))
    ?d <-(TotalNumberDiseaseSymptoms (name "_symptoms_major_depressive_disorder"))
    ?d2 <-(TotalNumberDiseaseSymptoms (name "_symptoms_dysthymic_disorder"))
    =>
    (modify ?s(wasUsed TRUE))
    (modify ?d(count (+ ?d.count 1)))
    (modify ?d2(count (+ ?d2.count 1)))
)

;rule 7
(defrule has_symptom_worthlessness_or_guilt
    ?s <-(Symptom (name "_symptom_worthlessness_or_guilt") (dueDrugs FALSE) (wasUsed FALSE))
    ?d <-(TotalNumberDiseaseSymptoms (name "_symptoms_major_depressive_disorder"))
    =>
    (modify ?s(wasUsed TRUE))
    (modify ?d(count (+ ?d.count 1)))
)

;rule 8
(defrule has_symptom_diminished_to_think
    ?s <-(Symptom (name "_symptom_diminished_to_think") (dueDrugs FALSE) (wasUsed FALSE))
    ?d <-(TotalNumberDiseaseSymptoms (name "_symptoms_major_depressive_disorder"))
    =>
    (modify ?s(wasUsed TRUE))
    (modify ?d(count (+ ?d.count 1)))
)

;rule 9
(defrule has_symptom_suicidal
    ?s <-(Symptom (name "_symptom_suicidal") (dueDrugs FALSE) (wasUsed FALSE))
    ?d <-(TotalNumberDiseaseSymptoms (name "_symptoms_major_depressive_disorder"))
    =>
    (modify ?s(wasUsed TRUE))
    (modify ?d(count (+ ?d.count 1)))
)

;;;;;;;;
;rule 10
(defrule has_symptom_inflated_selfesteem
    ?s <-(Symptom (name "_symptom_inflated_selfesteem") (dueDrugs FALSE) (wasUsed FALSE))
    ?d <-(TotalNumberDiseaseSymptoms (name "_symptoms_manic_episode"))
    ?d2 <-(TotalNumberDiseaseSymptoms (name "_symptoms_hypomanic_episode"))
    =>
    (modify ?s(wasUsed TRUE))
    (modify ?d(count (+ ?d.count 1)))
    (modify ?d2(count (+ ?d2.count 1)))
)

;rule 12
(defrule has_symptom_decrease_need_of_sleep
    ?s <-(Symptom (name "_symptom_decrease_need_of_sleep") (dueDrugs FALSE) (wasUsed FALSE))
    ?d <-(TotalNumberDiseaseSymptoms (name "_symptoms_manic_episode"))
    ?d2 <-(TotalNumberDiseaseSymptoms (name "_symptoms_hypomanic_episode"))
    =>
    (modify ?s(wasUsed TRUE))
    (modify ?d(count (+ ?d.count 1)))
    (modify ?d2(count (+ ?d2.count 1)))
)

;rule 13
(defrule has_symptom_more_talkative
    ?s <-(Symptom (name "_symptom_more_talkative") (dueDrugs FALSE) (wasUsed FALSE))
    ?d <-(TotalNumberDiseaseSymptoms (name "_symptoms_manic_episode"))
    ?d2 <-(TotalNumberDiseaseSymptoms (name "_symptoms_hypomanic_episode"))
    =>
    (modify ?s(wasUsed TRUE))
    (modify ?d(count (+ ?d.count 1)))
    (modify ?d2(count (+ ?d2.count 1)))
)

;rule 14
(defrule has_symptom_flight_of_ideas
    ?s <-(Symptom (name "_symptom_flight_of_ideas") (dueDrugs FALSE) (wasUsed FALSE))
    ?d <-(TotalNumberDiseaseSymptoms (name "_symptoms_manic_episode"))
    ?d2 <-(TotalNumberDiseaseSymptoms (name "_symptoms_hypomanic_episode"))
    =>
    (modify ?s(wasUsed TRUE))
    (modify ?d(count (+ ?d.count 1)))
    (modify ?d2(count (+ ?d2.count 1)))
)

;rule 15
(defrule has_symptom_distractibility
    ?s <-(Symptom (name "_symptom_distractibility") (dueDrugs FALSE) (wasUsed FALSE))
    ?d <-(TotalNumberDiseaseSymptoms (name "_symptoms_manic_episode"))
    ?d2 <-(TotalNumberDiseaseSymptoms (name "_symptoms_hypomanic_episode"))
    ?d3 <-(TotalNumberDiseaseSymptoms (name "_symptoms_dysthymic_disorder"))
    =>
    (modify ?s(wasUsed TRUE))
    (modify ?d(count (+ ?d.count 1)))
    (modify ?d2(count (+ ?d2.count 1)))
    (modify ?d3(count (+ ?d3.count 1)))
)

;rule 16
(defrule has_symptom_increase_goal_activity
    ?s <-(Symptom (name "_symptom_increase_goal_activity") (dueDrugs FALSE) (wasUsed FALSE))
    ?d <-(TotalNumberDiseaseSymptoms (name "_symptoms_manic_episode"))
    ?d2 <-(TotalNumberDiseaseSymptoms (name "_symptoms_hypomanic_episode"))
    =>
    (modify ?s(wasUsed TRUE))
    (modify ?d(count (+ ?d.count 1)))
    (modify ?d2(count (+ ?d2.count 1)))
)

;rule 17
(defrule has_symptom_excessive_activities_painful
    ?s <-(Symptom (name "_symptom_excessive_activities_painful") (dueDrugs FALSE) (wasUsed FALSE))
    ?d <-(TotalNumberDiseaseSymptoms (name "_symptoms_manic_episode"))
    ?d2 <-(TotalNumberDiseaseSymptoms (name "_symptoms_hypomanic_episode"))
    =>
    (modify ?s(wasUsed TRUE))
    (modify ?d(count (+ ?d.count 1)))
    (modify ?d2(count (+ ?d2.count 1)))
)

;;;;;;;;
;rule 18
(defrule has_symptom_poor_appetite
    ?s <-(Symptom (name "_symptom_poor_appetite") (dueDrugs FALSE) (wasUsed FALSE))
    ?d <-(TotalNumberDiseaseSymptoms (name "_symptoms_dysthymic_disorder"))
    =>
    (modify ?s(wasUsed TRUE))
    (modify ?d(count (+ ?d.count 1)))
)

;rule 19
(defrule has_symptom_low_selfesteem
    ?s <-(Symptom (name "_symptom_low_selfesteem") (dueDrugs FALSE) (wasUsed FALSE))
    ?d <-(TotalNumberDiseaseSymptoms (name "_symptoms_dysthymic_disorder"))
    =>
    (modify ?s(wasUsed TRUE))
    (modify ?d(count (+ ?d.count 1)))
)