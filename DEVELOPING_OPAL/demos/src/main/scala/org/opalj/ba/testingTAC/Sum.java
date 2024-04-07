package org.opalj.ba.testingTAC;

public class Sum {

    /**
     * Calculates the sum of two integers.
     *
     * @param var1 The first integer.
     * @param var2 The second integer.
     * @return The sum of {@code var1} and {@code var2}.
     */
    public static int sum(int var1, int var2) {
        int result = var1 + var2;
        return result;
    }

    public static void main(String[] args) {
        int a = 5; // Example value for var1
        int b = 10; // Example value for var2
        int sumResult = sum(a, b);
        System.out.println("The sum of " + a + " and " + b + " is " + sumResult);
    }
}
