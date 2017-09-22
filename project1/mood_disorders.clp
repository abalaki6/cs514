/**
 * CS514
 * Project 1
 * Mood disorders
 * @author Wolf
 */

;template for disorder
(deftemplate Disorder (declare (ordered TRUE)))

;template for any bipolar disorder
(deftemplate Bipolar (declare (ordered TRUE)))

;template for the disease
(deftemplate Symptom (slot name) (slot dueDrugs (default FALSE)) (slot wasUsed (default FALSE) ) )

;template for the period of the depression
;measures in weeks
(deftemplate Time (slot duration (type float) (default 0) ) )

;template for the age of the patient
(deftemplate Age (slot years (type integer) (default 0) ))

;template for counter of symptoms, if required number achieved has to fire fact about possibility of disease
(deftemplate TotalNumberDiseaseSymptoms (slot name) (slot count (type integer) (default 0) ) (slot requires (type integer) ) )

;define all TotalNumberDiseaseSymptoms facts
(deffacts TNDS
"List of all pre disease symptoms."
    (TotalNumberDiseaseSymptoms (name "_symptoms_major_depressive_disorder") (requires 5))
    (TotalNumberDiseaseSymptoms (name "_symptoms_manic_episode") (requires 3))
    (TotalNumberDiseaseSymptoms (name "_symptoms_hypomanic_episode") (requires 4))
    (TotalNumberDiseaseSymptoms (name "_symptoms_dysthymic_disorder") (requires 2))
)

;rule to add fact if symptoms are exist
(defrule hasSymptoms
    ?d <- ( TotalNumberDiseaseSymptoms (name ?name) { count >= requires})
    =>
    (assert (Symptom (name ?d.name) ) )
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
    (Symptom (name "_symptoms_major_depressive_disorder"))
    (not (Disorder "Mixed Episode"))
    (Symptom (name "_symptom_social_functioning") (dueDrugs FALSE))
    (not (Symptom (name "_symptom_suicidal")))
    =>
    (assert (Disorder "Major Depressive Disorder") )
 )

(defrule isMixedEpisode
"Determine if the patient has mixed episode"
    (Symptom (name "_symptoms_major_depressive_disorder"))
    (or
        (Symptom (name "_symptoms_manic_episode"))
        (Symptom (name "_symptoms_hypomanic_episode"))
    )
    (Symptom (name "_symptom_excessive_activities_painful") (dueDrugs FALSE))
    (Time {duration >= 1})
    =>
    (assert (Disorder "Mixed Episode") )
)

(defrule isManicEpisode
"Determine if the patient has manic episode"
    (Symptom (name "_symptoms_manic_episode") )
    (not (Disorder "Mixed Episode") )
    (Time {duration >= 1} )
    =>
    (assert (Disorder "Manic Episode") )
)

(defrule isHypomanicEpisode
"Determine if the patient has hypomanic episode"
    (Symptom (name "_symptoms_hypomanic_episode") )
    (Symptom (name "_symptom_social_functioning") (dueDrugs FALSE) )
    (Symptom (name "_symptom_changes_in_personality") (dueDrugs FALSE) )
    (Time {duration >= 0.5})
    =>
    (assert (Disorder "Hypomanic Episode") )
)

(defrule isCyclothymicDisorder
"Determine if the patient has cyclothymic disorder"
    (Symptom (name "_symptom_suicidal") (dueDrugs FALSE) )
    (Symptom (name "_symptoms_hypomanic_episode") )
    (Symptom (name "_symptom_depression_most_of_the_day") (dueDrugs FALSE) )
    (not (Symptom (name "_symptoms_major_depressive_disorder") ) )
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
    (not (Disorder "Manic Episode") )
    (not (Disorder "Mixed Episode") )
    (not (Symptom (name "_symptom_schizophrenia") ) )
    =>
    (assert (Disorder "Cyclothymic Disorder") )
)


(defrule isDysthymicDisorder
"Determine if the patient has dysthymic disorder"
    (Symptom (name "_symptoms_dysthymic_disorder") )
    (not (Disorder "Major Depressive Disorder") )
    (not (Disorder "Manic Episode") )
    (not (Disorder "Mixed Episode") )
    (not (Disorder "Cyclothymic Disorder") )
    (or
        (not (Symptom (name "_symptom_schizophrenia") (dueDrugs FALSE) ) )
        (not (Disorder "Delusional Disorder") )
    )
    (not (Symptom (name "_symptom_social_functioning") (dueDrugs FALSE) ) )
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
    (Disorder "Manic Episode")
    (not (Disorder "Major Depressive Disorder") )
    (not (Symptom (name "_symptom_schizophrenia") (dueDrugs FALSE) ) )
    =>
    (assert (Bipolar "Bipolar I Disorder, single Manic Episode") )
)

(defrule isBipolarIDisorderRecentHypomanic
    (Disorder "Hypomanic Episode")
    (Symptom (name "_symptom_social_functioning") (dueDrugs FALSE) )
    (not (Symptom (name "_symptom_schizophrenia") (dueDrugs FALSE) ) )
    =>
    (assert (Bipolar "Bipolar I Disorder, most recent Episode Hypomanic"))
)

(defrule isBipolarIDisorderRecentManic
    (Disorder "Manic Episode")
    (not (Symptom( name "_symptom_schizophrenia") (dueDrugs FALSE) ) )
    =>
    (assert (Bipolar "Bipolar I Disorder, most recent Episode Manic") )
)

(defrule isBipolarIDisorderRecentMixed
    (Disorder "Mixed Episode")
    (not (Symptom (name "_symptom_schizophrenia") (dueDrugs FALSE) ) )
    =>
    (assert (Bipolar "Bipolar I Disorder, most recent Episode Mixed") )
)

(defrule isBipolarIDisorderRecentDepressed
    (Disorder "Major Depressive Episode")
    (not (Symptom (name "_symptom_schizophrenia") (dueDrugs FALSE) ) )
    =>
    (assert (Bipolar "Bipolar I Disorder, most recent Episode Depressed") )
)

(defrule isBipolarIDisorderUnspecified
    (or
        (Symptom (name "_symptoms_manic_episode") )
        (Symptom (name "_symptoms_hypomanic_episode") )
        (Symptom (name "_symptoms_major_depressive_disorder") )
        (and
             (Symptom (name "_symptoms_major_depressive_disorder"))
             (or
                 (Symptom (name "_symptoms_manic_episode"))
                 (Symptom (name "_symptoms_hypomanic_episode"))
             )
             (Symptom (name "_symptom_excessive_activities_painful") (dueDrugs FALSE))
        )
    )
    (Symptom (name "_symptom_social_functioning") (dueDrugs FALSE) )
    (Symptom (name "_symptom_schizophrenia") (dueDrugs FALSE) )
    =>
    (assert (Bipolar "Bipolar I Disorder, most recent Episode Unspecified") )
)

(defrule isBipolarIIDisorder
    (Symptom (name "_symptoms_major_depressive_disorder") )
    (not (Symptom (name "_symptom_schizophrenia") (dueDrugs FALSE) ) )
    (Symptom (name "_symptom_social_functioning") (dueDrugs FALSE) )
    =>
    (assert (Bipolar "Bipolar II Disorder") )
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