package com.petlew.lyfe.chapter.two;

import java.io.IOException;

import javax.servlet.Servlet;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;

import net.jcip.annotations.ThreadSafe;

/**
 * This class is threadsafe b/c it does not have any state, so threads cannot access and change
 * state. Stateless objects are always thread‐safe.
 **/

@ThreadSafe
public class StatelessServlet implements Servlet {

  public void init(ServletConfig config) throws ServletException {
    // TODO Auto-generated method stub

  }

  public ServletConfig getServletConfig() {
    // TODO Auto-generated method stub
    return null;
  }

  public void service(ServletRequest req, ServletResponse res)
      throws ServletException, IOException {
    final String parameter = req.getParameter("name");
    res.getWriter().write("Hello " + parameter);

  }

  public String getServletInfo() {
    // TODO Auto-generated method stub
    return null;
  }

  public void destroy() {
    // TODO Auto-generated method stub

  }

}
