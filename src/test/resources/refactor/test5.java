package com.amp.examples.refactor;


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
            if (i == 0) {
                fooHelper1(i, k);
            } else {
                fooHelper0(i, j, k);
            }
        } else {
            fooHelper1(i, k);
        }

    }

    private void fooHelper0(int i, int j, int k) {
        int m = 1;
        i += 1;
        j += 1;
        System.out.println(m);
        System.out.println(k);
    }

    private void fooHelper1(int i, int k) {
        k += 1;
        i += 2;
        System.out.println("hi");
    }
}