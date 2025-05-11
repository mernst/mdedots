import java.text.*;

public class RetroweaverBug {

  public static void main(String[] args) {
    DecimalFormat df = new DecimalFormat("#.#");
    System.out.println(df.format(3.14));
  }

}
