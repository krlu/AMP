package com.amp.analysis;

import spoon.reflect.declaration.CtElement;

public class Cloner {
    public static CtElement createClone(CtElement model){
        return model.clone();
    }
}
