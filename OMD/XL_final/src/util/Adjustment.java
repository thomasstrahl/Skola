package util;

/**
 * Adjustment.java Created: Tue Oct 24 2005
 * @author Lennart Andersson
 * @version 0.1
 */
/**
 * Adjustment is a class for adjusting string representations of values within a
 * String.
 */
public class Adjustment {
    private int width;

    /**
     * Creates an adjustment.
     * 
     * @param width
     *            is the number of positions for the result. If the width is
     *            insufficient extra positions are added.
     */
    public Adjustment(int width) {
        this.width = width;
    }

    /**
     * Returns a centered String.
     * 
     * @param s
     *            is the string to adjust.
     */
    public String center(String s) {
        StringBuilder builder = new StringBuilder(width);
        int fill = width - s.length();
        for (int i = 0; i < fill / 2; i++) {
            builder.append(' ');
        }
        builder.append(s);
        for (int i = 0; i < fill - fill / 2; i++) {
            builder.append(' ');
        }
        return builder.toString();
    }

    /**
     * Returns a left adjusted String.
     * 
     * @param s
     *            is the string to adjust.
     */
    public String left(String s) {
        StringBuilder builder = new StringBuilder(width);
        builder.append(s);
        int fill = width - s.length();
        for (int i = 0; i < fill; i++) {
            builder.append(' ');
        }
        return builder.toString();
    }

    /**
     * Returns a right adjusted String.
     * 
     * @param b
     *            is the value to adjust.
     */
    public String right(boolean b) {
        return right(String.valueOf(b));
    }

    /**
     * Returns a right adjusted String.
     * 
     * @param c
     *            is the value to adjust.
     */
    public String right(char c) {
        return right(String.valueOf(c));
    }

    /**
     * Returns a right adjusted String.
     * 
     * @param number
     *            is the value to adjust.
     */
    public String right(double number) {
        return right(String.valueOf(number));
    }

    /**
     * Returns a right adjusted String.
     * 
     * @param number
     *            is the value to adjust.
     */
    public String right(float number) {
        return right(String.valueOf(number));
    }

    /**
     * Returns a right adjusted String.
     * 
     * @param number
     *            is the value to adjust.
     */
    public String right(int number) {
        return right(String.valueOf(number));
    }

    /**
     * Returns a right adjusted String.
     * 
     * @param number
     *            is the value to adjust.
     */
    public String right(long number) {
        return right(String.valueOf(number));
    }

    /**
     * Returns a right adjusted String.
     * 
     * @param item
     *            is the value to adjust.
     */
    public String right(Object item) {
        return right(String.valueOf(item));
    }

    /**
     * Returns a right adjusted String.
     * 
     * @param s
     *            is the string to adjust.
     */
    public String right(String s) {
        StringBuilder builder = new StringBuilder(width);
        int fill = width - s.length();
        for (int i = 0; i < fill; i++) {
            builder.append(' ');
        }
        builder.append(s);
        return builder.toString();
    }
}