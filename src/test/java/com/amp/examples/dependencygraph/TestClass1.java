package com.amp.examples.dependencygraph;

public class TestClass1 {
    public void foo1(int i, int j){
        i += 1;
        j += 1;
    }

    public void foo2(int i, int j){
        j += 1;
        i += 1;
    }

    private void bar(int i){
    }
}

