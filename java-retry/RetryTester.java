package io.gitlab.sj14.retry;

import org.junit.Test;

public class RetryTester {

  @Test
  public void testRetryExceptionSuccess() throws Exception {
    Retry.onException(3, new RetryOperation() {

      @Override
      public void doIt(int attempt) throws Exception {
          // First 2 tries fail,
          // third and last try succeeds.
          if (attempt < 2) {
            throw new java.lang.Exception();
        }
      }
    });
  }

  @Test(expected = java.lang.Exception.class)
  public void testRetryExceptionFail() throws Exception {
    Retry.onException(3, new RetryOperation() {

      @Override
      public void doIt(int attempt) throws Exception {
          throw new java.lang.Exception();
      }
    });
  }

  @Test(expected = java.lang.AssertionError.class)
  public void testRetryExceptionFailError() throws Exception {
    Retry.onException(3, new RetryOperation() {

      @Override
      public void doIt(int attempt) throws Exception {
        if (attempt < 2) {
          // Doesn't retry as it's an Error and we are using Retry.onException.
          // The first throw will already exit this test.
          throw new java.lang.AssertionError();
        }
      }
    });
  }

  @Test
  public void testRetryThrowableSuccess() throws Throwable {
    Retry.onThrowable(3, new RetryOperation() {

      @Override
      public void doIt(int attempt) throws Exception {
        // First 2 tries fail,
        // third and last try succeeds.
        if (attempt < 2) {
          throw new java.lang.AssertionError();
        }
      }
    });
  }

  @Test(expected = java.lang.AssertionError.class)
  public void testRetryThrowableFail() throws Throwable {
    Retry.onThrowable(3, new RetryOperation() {

      @Override
      public void doIt(int attempt) throws Exception {
          // throw Error instead of Exception
          throw new java.lang.AssertionError();
      }
    });
  }
}
