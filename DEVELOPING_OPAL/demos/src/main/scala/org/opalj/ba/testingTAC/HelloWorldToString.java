package org.opalj.ba.testingTAC;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class HelloWorldToString {

    public static void main(String[] args) {
        int x = 0;
        for(int i = 0; i < 6; i++)
            x++;
        if(x <= 0)
            System.out.println("Hello World!");
        if(x > 1)
            System.out.println("vos podes sofi");
        System.out.println("Hello World!");
        printDay("Monday");
        printDay("Sunday");
    }

    public static void printDay(String myDay) {

        switch (myDay) {
            case "Monday":
            case "Tuesday":
            case "Wednesday":
            case "Thursday":
            case "Friday":
                System.out.println(myDay + " is a weekday.");
                break;
            case "Saturday":
            case "Sunday":
                System.out.println(myDay + " is a weekend.");
                break;
            default:
                System.out.println(myDay + " is not a valid day.");
                break;
        }
    }
}

