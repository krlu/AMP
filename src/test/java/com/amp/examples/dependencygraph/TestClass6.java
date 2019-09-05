package com.amp.examples.dependencygraph;

public class TestClass6 {
    public void foo(int i, int j){
        int k = 0;
        if(j > 0){
            int m = 1;
            i += 1;
            System.out.println(k);
            j += 1;
            System.out.println(m);
        }
        else{
            int n = 1;
            i += 1;
            j += 1;
            System.out.println(n);
        }
    }
}

