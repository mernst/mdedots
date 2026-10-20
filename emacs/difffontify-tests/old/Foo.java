/*
 * License line 1.
 * License line 2.
 * License line 3.
 * License line 4.
 * License line 5.
 * License line 6.
 * License line 7.
 * License line 8.
 * License line 9.
 * License line 10.
 * License line 11.
 * License line 12.
 * License line 13.
 * License line 14.
 */
package demo;

/** A demonstration class. */
public class Foo {
  private int count = 0;

  /** Returns the count. */
  public int count() {
    return count;
  }

  public void log() {
    System.out.println("This line is long enough that the printed diff must wrap it.");
  }
}
