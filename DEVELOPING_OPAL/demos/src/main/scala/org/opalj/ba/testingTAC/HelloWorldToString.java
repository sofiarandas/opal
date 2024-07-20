package org.opalj.ba.testingTAC;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class HelloWorldToString {

    public static void main(String[] args) {
        System.out.println("Holi Fiooooo");
        int i = 0;
        System.out.println(i);
        for(; i < 6; i++){
            dumbPrint();
        }
        dumbPrint();
    }

    public static void dumbPrint(){
        System.out.println("esto es de otro metodo");
        foo();
    }

    public static void foo(){
        System.out.println("y este tambien, osea mi StaticMethodCall funciona :D");
    }
}

