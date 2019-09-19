package com.amp.examples.refactor;


/**
 * Test code 4
 */
public class TestClass4 {
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
        while (i <= 0) {
            System.out.println("bye");
            if (j < 1) {
                System.out.println("ok");
            }
            while (j <= 0) {
                Object[] objects = fooHelper0(i, j, k);
                i = objects[0];
                j = objects[1];
            }
        }
    }

    private Object[] fooHelper0(int i, int j, int k) {
        if (i == 0) {
            System.out.println("hi");
        } else {
            int m = 1;
            i += 1;
            j += 1;
            System.out.println(m);
            System.out.println(k);
        }
        return new Object[]{ i, j };
    }
}