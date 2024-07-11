package org.opalj.ba.testingTAC;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class HelloWorldToString {

    public static void main(String[] args) {
        System.out.println("Holi Fiooooo");
        System.out.println("Holi Diosi");
        System.out.println("ya puedo printear cosas");
        for(int i = 0; i < 6; i++){
            if(i > 3){
                dumbPrint();
            }
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

