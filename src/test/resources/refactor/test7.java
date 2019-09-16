package com.amp.examples.refactor;


/**
 * Test code 7
 */
public class TestClass7 {
    public void foo(int i, int j) {
        int k = 0;
        if (j > 0) {
            fooHelper0(i, j, k);
        } else
        if (j == 0) {
            fooHelper0(i, j, k);
        } else {
            fooHelper1(i, j, k);
        }

    }

    private void fooHelper0(int i, int j, int k) {
        if (i == 0) {
            fooHelper1(i, j, k);
        } else {
            fooHelper1(i, j, k);
        }
    }

    private void fooHelper1(int i, int j, int k) {
        int p = 1;
        i += 1;
        j += 1;
        System.out.println(p);
        System.out.println(k);
    }
}