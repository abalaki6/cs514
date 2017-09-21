import jess.*;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

public class Main {
    static List<Question> questions;

    public static void main(String[] args) throws JessException {
        try {
            Rete r = new Rete();
            questions = createQuestions(System.getProperty("user.dir") + File.separator + "src" + File.separator + "questions");

            r.batch("mood_disorders.clp");
            r.reset();
            for(Question q : questions){
                Fact f = new Fact("Symptom", r);
                f.setSlotValue("name", new Value(q.name, RU.STRING));
                f.setSlotValue("dueDrugs", new Value(q.causedByDrugs));
                r.assertFact(f);
            }
            r.eval("(run)");
            r.eval("(facts)");
            // Prints '9'

            //System.out.println(v.intValue(r.getGlobalContext()));
        } catch (JessException ex) {
            System.err.println(ex);
        }
    }

    private static List<Question> createQuestions(String source) {
        List<Question> questions = new ArrayList<Question>();

        try{
            FileReader file = new FileReader(source);
            BufferedReader br = new BufferedReader(file);

            String question_name;
            String message;
            while( (question_name = br.readLine()) != null ){
                message = br.readLine();
                questions.add(new Question(question_name,message));
            }

        } catch (Exception e) {
            System.out.println(e);
            System.out.println(source);
            System.exit(0);
        }

        return questions;
    }


}
