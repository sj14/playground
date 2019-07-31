package io.gitlab.sj14.retry;

import static org.junit.Assert.fail;

import org.junit.Test;

public class RetryTester {

  @Test
  public void testRetryExceptionSuccess() throws Exception {
    Retry.onException(3, attempt -> {
        // First 2 tries fail,
        // third and last try succeeds.
        if (attempt < 2) {
          throw new Exception();
      }
    });
  }

  @Test(expected = Exception.class)
  public void testRetryExceptionFail() throws Exception {
    Retry.onException(3, attempt -> {
        throw new Exception();
    });
  }

  @Test(expected = Error.class)
  public void testRetryExceptionOnThrowableFail() throws Exception {
    Retry.onException(3, attempt -> {
      // should rethrow the Error immediately, as we are only retrying on Exceptions
      if (attempt >= 1) {
        fail();
      }
      throw new AssertionError();
    });
  }

  @Test
  public void testRetryThrowableSuccess() throws Throwable {
    Retry.onThrowable(3, attempt -> {
      // First 2 tries fail,
      // third and last try succeeds.
      if (attempt < 2) {
        throw new AssertionError();
      }
    });
  }

  @Test
  public void testRetryThrowableOnExceptionSuccess() throws Throwable {
    Retry.onThrowable(3, attempt -> {
      // First 2 tries fail,
      // third and last try succeeds.
      if (attempt < 2) {
        throw new Exception();
      }
    });
  }

  @Test(expected = Error.class)
  public void testRetryThrowableFail() throws Throwable {
    Retry.onThrowable(3, attempt -> {
        // throw Error instead of Exception
        throw new Error();
    });
  }

  @Test(expected = Exception.class)
  public void testRetryThrowableOnExceptionFail() throws Throwable {
    Retry.onThrowable(3, attempt -> {
        throw new Exception();
    });
  }
}
