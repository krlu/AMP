package com.amp.examples.refactor;


/**
 * Test code 2
 */
public class TestClass2 {
    public void foo(int i, int j){
        int k = 0;
        if(j > 0){
            int m = 1;
            i += 1;
            j += 1;
            System.out.println(m);
            System.out.println(k);
        }
        else{
            int n = 1;
            i += 1;
            j += 1;
            System.out.println(n);
            System.out.println(k);
        }
    }
}
