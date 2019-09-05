package com.amp.examples.refactor;


/**
 * Test code 3
 */
public class TestClass3 {
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
            if(i == 0){
                System.out.println("hi");
            }
            else {
                int n = 1;
                i += 1;
                j += 1;
                System.out.println(n);
                System.out.println(k);
            }
        }
    }
}
