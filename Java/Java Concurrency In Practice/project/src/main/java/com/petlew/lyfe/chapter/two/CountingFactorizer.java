package com.petlew.lyfe.chapter.two;

import java.io.IOException;
import java.util.concurrent.atomic.AtomicLong;

import javax.servlet.Servlet;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;

import net.jcip.annotations.ThreadSafe;

/*
 * exapmle is not the same as in the book, b/c I am to lazy to implement Factorizer
 */
@ThreadSafe
public abstract class CountingFactorizer implements Servlet {
  /*
   * this gives me a bit of monadic/functional feeling, 
   * as we have a container for our value,
   */
  private final AtomicLong count = new AtomicLong(0);

  public void service(ServletRequest req, ServletResponse res)
      throws ServletException, IOException {
      count.getAndIncrement()
  }
}
