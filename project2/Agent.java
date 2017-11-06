import jess.*;
import nrc.fuzzy.*;
import nrc.fuzzy.jess.FuzzyRete;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Scanner;

/**
 * CS514
 * Project 1
 * Mood disorders
 *
 * @author Wolf
 */
public class Agent {
    private static Scanner input;
    private static boolean fromFile;
    //questions db source path
    private final static String dbSource = "questions";

//    public static void main1(String[] args) {
//        try {
//            double xHot[] = {25, 35, 50};
//            double yHot[] = {0, 1, 0};
//            double xCold[] = {5, 35};
//            double yCold[] = {1, 0};
//
//            FuzzyRete r = new FuzzyRete();
//            r.batch("fuzzyTest.clp");
//            r.reset();
//            r.run();
////            return;
//
////
//            FuzzyVariable fv = new FuzzyVariable("temperature", 0, 100, "C");
//            fv.addTerm("hot", xHot, yHot, 3);
//
//            Fuz.zyValue val = new FuzzyValue(fv,"hot");
//            val
////
//  fv.addTerm("cold", xCold, yCold, 2);
//            fv.addTerm("medium", "cold or ( hot)");
//
////            fv.addTerm("weird", new SingletonFuzzySet(20));
////
////            FuzzyValue val = new FuzzyValue(fv,"weird");
//////            System.out.println(val.toString());
//////            System.out.println(val.maximumDefuzzify());
////            val = new FuzzyValue(val.getFuzzyVariable(),new SingletonFuzzySet(val.maximumDefuzzify()+1));
//////            System.out.println(val.maximumDefuzzify());
////
////            FuzzyValue temps[] = new FuzzyValue[1];
////            temps[0] = new FuzzyValue(fv,"cold and weird");
////
//////            System.out.println("expecting to have something around .5 : " + temps[0].getMaxY());
////            FuzzyValue cold = new FuzzyValue(fv,"cold");
//////            temps[1] = new FuzzyValue(fv,"weird");
////
//////            System.out.println(FuzzyValue.plotFuzzyValues("*",temps));
////
//        } catch (FuzzyException | JessException ex) {
//            System.err.println(ex);
//        }
//    }

    public static void main(String[] args) throws JessException, FuzzyException {
        try {

            FuzzyRete r = new FuzzyRete();


            r.batch("mood_disorders.clp");

            // if the argument specified read from the file
            if (args.length == 1) {
                try {
                    input = new Scanner(new File(args[0]));
                } catch (Exception e) {
                    System.err.println(e);
                    System.exit(0);
                }
                fromFile = true;
            //otherwise use system.in stream
            }
            else {
                System.err.println("Expected you have a form file, please provide filled form");
                System.exit(-1);
                input = new Scanner(System.in);
                fromFile = false;
            }

            //reset all the information that we entered
            r.reset();
            //prompt or read the information about the patient
            fillTheForm(r);

            //run the engine
            r.eval("(run)");

            //output all the relevant conclusions from the KB
            printDisorders(r);

            //System.out.println(v.intValue(r.getGlobalContext()));
        } catch (JessException ex) {
            System.err.println(ex);
        }
    }

    /**
     * printout to the System.out all the disorders that patient potentially can have
     *
     * @param r current Rete
     */
    private static void printDisorders(FuzzyRete r) throws JessException {
        if (hasDisorder(r)) {
            Iterator facts = r.listFacts();
            Fact maxFact = null;
            System.out.println("That have been indicated that patient has a reasonable chance to have listed mood disorders:");
            while (facts.hasNext()) {
                Fact fact = (Fact) facts.next();

                if (isDisease(fact) && maxFact == null) {
                    maxFact = fact;
                } else if (isDisease(fact) && Double.parseDouble(fact.getSlotValue("fuzzyValue").toString()) > Double.parseDouble(maxFact.getSlotValue("fuzzyValue").toString())) {
                    maxFact = fact;
                }
            }

            if (maxFact != null && Double.parseDouble(maxFact.getSlotValue("fuzzyValue").toString()) > 0.5) {
                facts = r.listFacts();
                while (facts.hasNext()) {
                    Fact fact = (Fact) facts.next();
                    if (isDisease(fact) && Double.parseDouble(fact.getSlotValue("fuzzyValue").toString()) == Double.parseDouble(maxFact.getSlotValue("fuzzyValue").toString())) {
                        System.out.println(fact.getSlotValue("name").toString());
                    }
                }
            }

        } else {
            System.out.println("According to current knowledge base database, the patient most likely has no mood disorders!");
        }
    }

    /**
     * @param r current Rete
     * @return true if KB has any Disorder
     */
    private static boolean hasDisorder(Rete r) {
        Iterator facts = r.listFacts();
        while (facts.hasNext()) {
            Fact fact = (Fact) facts.next();
            if (isDisease(fact)) {
                return true;
            }
        }

        return false;
    }

    /**
     * @param fact current fact
     * @return string representation fact name in English, omits extra information
     */
    private static String onlyName(Fact fact) {
        String fullName = fact.toStringWithParens();
        return fullName.substring(fullName.indexOf("\""), fullName.lastIndexOf("\"") + 1);
    }

    /**
     * @param fact current fact
     * @return true of the given fact is below the Disease
     */
    private static boolean isDisease(Fact fact) {
        String name = fact.getName();
        return ("MAIN::FuzzyDisorder").equals(name);
    }

    /**
     * automatic system to fill the form for patient, if the file was specified as argument then reads from file
     * otherwise prompts from System.in answers
     *
     * @param r current Rete
     * @throws JessException for jess
     */
    private static void fillTheForm(Rete r) throws JessException {
        List<Question> questions = createQuestions();

        //ask the age of the patient
        if (fromFile) {
            input.nextLine();
        } else {
            System.out.println("How old is the patient?");
        }

        int age;
        try {
            age = Integer.parseInt(input.nextLine());
        } catch (Exception e) {
            System.err.println(e);
            age = 0;
        }

        age = age < 0 ? 0 : age;

        Fact fAge = new Fact("Age", r);
        fAge.setSlotValue("years", new Value(age, RU.INTEGER));
        r.assertFact(fAge);

        //ask for duration of mood or depression presents
        if (fromFile) {
            input.nextLine();
            input.nextLine();
        } else {
            System.out.println("For how long does patient have mood change or depression presents?");
            System.out.println("(print #weeks, if less than one then decimal value s.t. 4 days = 0.5. If more than 1 year 52, if 2 years 104)");
        }

        float duration;
        try {
            duration = Float.parseFloat(input.nextLine());
        } catch (Exception e) {
            System.err.println(e);
            duration = 0;
        }
        duration = duration < 0 ? 0 : duration;

        Fact time = new Fact("Time", r);
        time.setSlotValue("duration", new Value(duration, RU.FLOAT));
        r.assertFact(time);

        //prompt every single question form the db

        for (Question question : questions) {
            if (prompt(question.message)) {
                question.causedByDrugs = prompt("Could it be caused by medicine patient takes?");

                Fact fact = new Fact("Symptom", r);
                fact.setSlotValue("name", new Value(question.name, RU.STRING));
                fact.setSlotValue("dueDrugs", new Value(question.causedByDrugs));
                r.assertFact(fact);
            } else {
                if (fromFile) {
                    input.nextLine();
                    input.nextLine();
                }
            }
        }
    }

    /**
     * @param question current question
     * @return true if the answer to the question is true
     */
    private static boolean prompt(String question) {
        if (fromFile) {
            input.nextLine();
        } else {
            System.out.println(question + " (y/n)");
        }

        String s = input.nextLine();
        return "y".equals(s);
    }

    /**
     * parse the db of question to memory
     *
     * @return list of all question for symptoms
     */
    private static List<Question> createQuestions() {
        List<Question> questions = new ArrayList<Question>();

        try {
            FileReader file = new FileReader(dbSource);
            BufferedReader br = new BufferedReader(file);

            String question_name;
            String message;
            while ((question_name = br.readLine()) != null) {
                message = br.readLine();
                questions.add(new Question(question_name, message));
            }

        } catch (Exception e) {
            System.out.println(e);
            System.out.println(dbSource);
            System.exit(0);
        }

        return questions;
    }


    /**
     * class to represent single question about a symptom
     */
    private static class Question {
        String message;
        String name;
        boolean causedByDrugs;

        Question(String name, String message) {
            this.message = message;
            this.name = name;
        }
    }
}
