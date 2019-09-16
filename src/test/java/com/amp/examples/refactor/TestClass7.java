package com.amp.examples.refactor;


/**
 * Test code 7
 */
public class TestClass7 {
    public void foo(int i, int j){
        int k = 0;
        if(j > 0){
            if(i == 0){
                int p = 1;
                i += 1;
                j += 1;
                System.out.println(p);
                System.out.println(k);
            }
            else {
                int m = 1;
                i += 1;
                j += 1;
                System.out.println(m);
                System.out.println(k);
            }
        }
        else if( j == 0){
            if(i == 0){
                int q = 1;
                i += 1;
                j += 1;
                System.out.println(q);
                System.out.println(k);
            }
            else {
                int n = 1;
                i += 1;
                j += 1;
                System.out.println(n);
                System.out.println(k);
            }
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
