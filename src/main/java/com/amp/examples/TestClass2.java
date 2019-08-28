package com.amp.examples;

public class TestClass2 {
    public static void foo(int i, int j){
        if(j > 0){
            i += 1;
            j += 1;
            System.out.println(i);
        }
        if(j < 0){
            i += 1;
            j += 1;
            System.out.println(i);
        }
    }
}
