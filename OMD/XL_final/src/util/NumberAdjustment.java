package util;

/**
 * NumberAdjustment.java Created: Mon OTue 24 2005
 * @author Lennart Andersson
 * @version 0.1
 */
/**
 * Adjustment is a class for adjusting string representations of numerical
 * values within a String.
 */
public class NumberAdjustment extends Adjustment {
    private final static double log10 = Math.log(10.0);
    private int decimals;
    private int exponent;

    /**
     * Creates an adjustment for numbers.
     * 
     * @param width
     *            is the number of positions for the result. If the width is
     *            insufficient extra positions are added.
     * @param decimals
     *            is the number of decimals in the result.
     */
    public NumberAdjustment(int width, int decimals) {
        super(width);
        this.decimals = decimals;
    }

    /**
     * Creates an adjustment for numbers with an exponent field.
     * 
     * @param width
     *            is the number of positions for the result. If the width is
     *            insufficient extra positions are added.
     * @param decimals
     *            is the number of positions for the decimals.
     * @param exponent
     *            is the number of positions for the exponent including the
     *            letter E. If the width is unsufficient extra positions are
     *            added.
     */
    public NumberAdjustment(int width, int decimals, int exponent) {
        this(width, decimals);
        this.exponent = exponent;
    }

    private StringBuilder fillZero(int exp) {
        StringBuilder builder = new StringBuilder();
        int length = exponent;
        if (exp < 0) {
            builder.append('-');
            exp = -exp;
            length = exponent - 1;
        }
        String s = String.valueOf(exp);
        for (int i = s.length(); i < length; i++) {
            builder.append('0');
        }
        return builder.append(s);
    }

    private StringBuilder format(double number) {
        StringBuilder builder = new StringBuilder();
        long intpart = (long) number;
        builder.append(intpart);
        double fraction = number - intpart;
        if (decimals > 0) {
            builder.append('.');
        }
        for (int i = 0; i < decimals; i++) {
            fraction *= 10;
            int d = (int) fraction;
            builder.append((char) (d + '0'));
            fraction = fraction - d;
        }
        return builder;
    }

    /**
     * Returns a right adjusted String.
     * 
     * @param number
     *            is the value to adjust.
     */
    public String right(double number) {
        StringBuilder builder = new StringBuilder();
        boolean negative = number < 0;
        if (negative) {
            number = -number;
            builder.append('-');
        }
        if (exponent > 0) {
            if (number == 0.0)
                return right(format(0.0) + "E" + fillZero(0));
            int exp = (int) Math.floor((Math.log(number) / log10));
            number /= Math.pow(10.0, exp);
            number += 0.5 * Math.pow(10.0, -decimals);
            if (number >= 10.0) {
                number /= 10.0;
                exp++;
            }
            builder.append(format(number)).append('E').append(fillZero(exp));
        } else {
            number += 0.5 * Math.pow(10.0, -decimals);
            builder.append(format(number));
        }
        return right(builder.toString());
    }

    /**
     * Returns a right adjusted String.
     * 
     * @param number
     *            is the value to adjust.
     */
    public String right(float number) {
        return right((double) number);
    }

    public static void main(String[] args) {
        Adjustment adjustment = new NumberAdjustment(10, 2, 2);
        System.out.println("0123456789");
        System.out.println(adjustment.right(-0.0000000000000000000000000));
        float value = (float) (1 / 3.0);
        System.out.println(adjustment.right(value));
        adjustment = new Adjustment(12);
        System.out.println(adjustment.right(value));
    }
}
