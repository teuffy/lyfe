package com.petlew.lyfe.chapter.two;

import java.io.IOException;

import javax.servlet.Servlet;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;

import net.jcip.annotations.GuardedBy;

public abstract class CachedFactorizer implements Servlet {
  @GuardedBy("this")
  private long hits;
  @GuardedBy("this")
  private long cacheHits;



  public void service(ServletRequest req, ServletResponse res)
      throws ServletException, IOException {
    synchronized (this) {
      // two atomic operations that need to be `atomic as a whole`, simplified example
      ++hits;
      ++cacheHits;
    }
  }
}
