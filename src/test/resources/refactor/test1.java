package com.amp.examples.refactor;


/**
 * Test code 1
 */
public class TestClass1 {
    public void foo(int i, int j) {
        int k = 0;
        if (j > 0) {
            fooHelper0(i, j);
        }
        if (j < 0) {
            fooHelper0(i, j);
        }
    }

    private void fooHelper0(int i, int j) {
        i += 1;
        j += 1;
        System.out.println(i);
    }
}