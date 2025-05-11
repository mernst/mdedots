import java.text.*;

public class DateFormatBug {

  public static void main(String[] args) {
    // JDK 1.2beta4 throws java.lang.NullPointerException:
    //   (new java.text.DateFormat()).hashCode();
    // In JDK 1.5, DateFormat is abstract and the below is fine.
    (new java.text.SimpleDateFormat()).hashCode();
  }

}
