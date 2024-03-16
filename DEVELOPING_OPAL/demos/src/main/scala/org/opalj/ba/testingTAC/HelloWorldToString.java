package org.opalj.ba.testingTAC;

public class HelloWorldToString {

    public static void main(String[] args) {
        MyClass obj = new MyClass("Hello, World!");
        for(int i = 0; i < 5; i++){
            if(i % 2 == 0) {
                System.out.println("Hola manuuuu");
            }
        }
        System.out.println(obj);
        boolean boolResult = obj.returnBool();
        boolResult = !boolResult;
        if(boolResult)
            System.out.println("bool was true");
    }

    private static class MyClass {
        private String message;

        public MyClass(String message) {
            this.message = message;
        }

        @Override
        public String toString() {
            // Custom toString method that returns the message
            return this.message;
        }

        public boolean returnBool() {
            return true;
        }
    }
}

