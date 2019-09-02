package com.amp.examples;


/**
 * Test code 3
 */
public class TestClass3 {
    public void foo(int i, int j) {
        int k = 0;
        if (j > 0) {
            fooHelper0(i, j, k);
        } else {
            fooHelper0(i, j, k);
        }
    }

    private void fooHelper0(int i, int j, int k) {
        int m = 1;
        i += 1;
        j += 1;
        System.out.println(m);
        System.out.println(k);
    }
}