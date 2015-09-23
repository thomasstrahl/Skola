package gui;

public class XLCounter {
    private int counter;

    public void increment() {
        counter++;
    }

    public String toString() {
        return Integer.toString(counter);
    }
}
