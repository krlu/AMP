package com.amp.examples.refactor;


/**
 * Test code 5
 */
public class TestClass5 {
    public void foo(int i, int j) {
        int k = 0;
        if (j > 0) {
            Object[] objects = fooHelper0(i, j, k);
            i = objects[0];
            j = objects[1];
        } else {
            if (j == 0) {
                if (i == 0) {
                    Object[] objects = fooHelper1(i, k);
                    k = objects[0];
                    i = objects[1];
                } else {
                    Object[] objects = fooHelper0(i, j, k);
                    i = objects[0];
                    j = objects[1];
                }
            } else {
                Object[] objects = fooHelper1(i, k);
                k = objects[0];
                i = objects[1];
            }
        }
    }

    private void bar() {
    }

    private Object[] fooHelper0(int i, int j, int k) {
        int m = 1;
        i += 1;
        j += 1;
        System.out.println(m);
        System.out.println(k);
        return new Object[]{ i, j };
    }

    private Object[] fooHelper1(int i, int k) {
        k += 1;
        i += 2;
        bar();
        return new Object[]{ k, i };
    }
}