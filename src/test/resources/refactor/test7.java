package com.amp.examples.refactor;


import java.util.ArrayList;


/**
 * Test code 7
 */
public class TestClass7 {
    public void foo(int i, int j) {
        ArrayList<String> list = new ArrayList<>();
        int k = 0;
        if (j > 0) {
            Object[] objects = fooHelper0(i, j, k);
            i = objects[0];
            j = objects[1];
        } else {
            if (j == 0) {
                Object[] objects = fooHelper0(i, j, k);
                i = objects[0];
                j = objects[1];
            } else {
                Object[] objects = fooHelper1(i, j, k);
                i = objects[0];
                j = objects[1];
            }
        }
    }

    private Object[] fooHelper0(int i, int j, int k) {
        if (i == 0) {
            Object[] objects = fooHelper1(i, j, k);
            i = objects[0];
            j = objects[1];
        } else {
            Object[] objects = fooHelper1(i, j, k);
            i = objects[0];
            j = objects[1];
        }
        return new Object[]{ i, j };
    }

    private Object[] fooHelper1(int i, int j, int k) {
        int p = 1;
        i += 1;
        j += 1;
        System.out.println(p);
        System.out.println(k);
        return new Object[]{ i, j };
    }
}