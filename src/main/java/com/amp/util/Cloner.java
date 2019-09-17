package com.amp.util;

import spoon.reflect.declaration.CtType;

public class Cloner {
    public static CtType createClone(CtType model){
        return model.clone();
    }
}
