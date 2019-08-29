package com.amp.examples;

public class TestClass2 {
    public void foo(int i, int j){
        int k = 0;
        bar();
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
    protected void bar(){

    }
}
