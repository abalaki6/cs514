/**
 *
 */
public class Question {
    public String message;
    public String name;
    public boolean isTrue;
    public boolean causedByDrugs;

    public Question(String name, String message){
        this.message = message;
        this.name = name;
    }
}
