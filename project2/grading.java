import jess.JessException;
import nrc.fuzzy.jess.FuzzyRete;

public class grading {
    public static void main(String[] args) throws JessException {
            FuzzyRete r = new FuzzyRete();
            r.batch("fuzzy_macros.clp");

            r.reset();
            r.run();
    }
}
