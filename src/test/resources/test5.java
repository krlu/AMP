package com.amp.examples;


/**
 * Test code 5
 */
public class TestClass5 {
    public void foo(int i, int j) {
        int k = 0;
        if (j > 0) {
            fooHelper0(i, j, k);
        } else
            if (j == 0) {
                fooHelper0(i, j, k);
            } else {
                System.out.println("bye");
            }

    }

    private void fooHelper0(int i, int j, int k) {
        if (i == 0) {
            System.out.println("hi");
        }  else {
            int mhskjdhfkjs = 1;
            i += 1;
            j += 1;
            System.out.println(mhskjdhfkjs);
            System.out.println(k);
        }
    }
}