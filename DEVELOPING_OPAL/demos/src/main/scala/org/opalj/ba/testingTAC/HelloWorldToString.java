package org.opalj.ba.testingTAC;

import java.util.Scanner;

public class HelloWorldToString {

    public static void main(String[] args) {
        String s1 = "Please enter a number.";
        System.out.println(s1);
        int a = s1.length();
        String s2 = "Please enter another number.";
        System.out.println(s2);
        int b = s2.length();
        int result = a + b;
        boolean bool = true;
        for(int i = 0; i < 10; i++){
            if(result < i){
                System.out.println("less than 5");
            }else if( result == i){
                System.out.println("zeroooo");
                foo(bool);
            }else{
                System.out.println("moreee");
            }
            foo(bool);
        }
    }

    public static boolean foo(boolean b){
        for(int i = 0; i < 2; i++){
            System.out.println("hello");
        }
        return !b;
    }
}

