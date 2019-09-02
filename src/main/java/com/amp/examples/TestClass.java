package com.amp.examples;


/**
 * test code 1
 */
public class TestClass {
    public double testMethod() throws InterruptedException {
        double x = 1.0;
        x = 0.0;
        for (int j = 0; j < 4; j++) {
            Thread.sleep(1000);
            x += j;
            return 10.0;
        }
        return x;
    }

    public void foo() {
    }
}