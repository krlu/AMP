package com.amp.examples.refactor;


/**
 * Test code 6
 */
public class TestClass6 {
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
            fooHelper0Helper0(i, j, k);
        } else {
            fooHelper0Helper0(i, j, k);
        }
    }

    private void fooHelper0Helper0(int i, int j, int k) {
        int p = 1;
        i += 1;
        j += 1;
        System.out.println(p);
        System.out.println(k);
    }
}