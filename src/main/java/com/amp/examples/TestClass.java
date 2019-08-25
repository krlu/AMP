package com.amp.examples;

/**
 * test comment
 */
public class TestClass {
    public double testMethod() throws InterruptedException {
        double x = 1.0;
        x = 0.0;
        for(int j = 0; j < 100; j++){
            Thread.sleep(10);
            x += j;
        }
        return x;
    }
}