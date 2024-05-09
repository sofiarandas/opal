package org.opalj.ba.testingTAC;

public class HelloWorldToString {

    public static void main(String[] args) {
        int x = 0;
        int y = 2;
        boolean b = (x % y) == 0;
        boolean b2 = !b;
        if(b || b2)
        System.out.println("Hello World!");
    }
}

