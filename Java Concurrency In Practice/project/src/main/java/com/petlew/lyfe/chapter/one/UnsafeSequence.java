package com.petlew.lyfe.chapter.one;

import net.jcip.annotations.NotThreadSafe;

//This is not thread safe, as race condition may occur
@NotThreadSafe
public class UnsafeSequence {
    private int value;
    
    // Should return always unique, next value
    public int getNext() {
        return value++;
    }
}
