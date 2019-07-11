package io.gitlab.sj14.retry;

/**
 * based on https://stackoverflow.com/a/13240586
 */

public abstract class RetryOperation {

   /**
    * Retryable code to execute.
    * @param attempt counts the current attempt, starting at 0.
    * @throws Exception
    */
   abstract public void doIt(int attempt) throws Exception;
}
