package com.amp.examples.refactor;


/**
 * Test code 1
 */
public class TestClass1 {
    public void foo(int i, int j) {
        int k = 0;
        if (j > 0) {
            Object[] objects = fooHelper0(i, j);
            i = objects[0];
            j = objects[1];
        }
        if (j < 0) {
            Object[] objects = fooHelper0(i, j);
            i = objects[0];
            j = objects[1];
        }
    }

    private Object[] fooHelper0(int i, int j) {
        i += 1;
        j += 1;
        System.out.println(i);
        return new Object[]{ i, j };
    }
}