package com.amp.examples.refactor;


/**
 * Test code 2
 */
public class TestClass2 {
    public void foo(int i, int j) {
        int k = 0;
        if (j > 0) {
            Object[] objects = fooHelper0(i, j, k);
            i = objects[0];
            j = objects[1];
        } else {
            Object[] objects = fooHelper0(i, j, k);
            i = objects[0];
            j = objects[1];
        }
    }

    private Object[] fooHelper0(int i, int j, int k) {
        int m = 1;
        i += 1;
        j += 1;
        System.out.println(m);
        System.out.println(k);
        return new Object[]{ i, j };
    }
}