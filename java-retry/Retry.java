package io.gitlab.sj14.retry;

public class Retry {

   private static final int LIMIT_ATTEMPTS = 7;

   /**
    * based on https://stackoverflow.com/a/13240586
    *
    * @param maxAttempts (max. value is 7)
    * @param retryOperation
    * @throws Exception
    */
   public static void onException(int maxAttempts, RetryOperation retryOperation) throws Exception {
      if (maxAttempts > LIMIT_ATTEMPTS) {
         maxAttempts = LIMIT_ATTEMPTS;
      }

      for (int attempt = 0; ; attempt++) {
         try {
            retryOperation.doIt(attempt);
            break; // call was successful
         } catch (Exception e) {
            if (attempt < maxAttempts) {
               // exponential backoff before trying again
               exponentialSleep(attempt+1);
               continue;
            }
            // reached max. attempts, don't retry, print stacktrace and throw the exception
            e.printStackTrace();
            throw e;
         }
      }
   }


   /**
    * Suitable for tests as junit's assert functions throws errors and not exceptions. Throwable will catch both, errors and exceptions
    * but shouldn't be used in "normal" code of the service.
    *
    * @param maxAttempts
    * @param retryOperation
    * @throws Exception
    */
   public static void onThrowable(int maxAttempts, RetryOperation retryOperation) throws Throwable {
      if (maxAttempts > LIMIT_ATTEMPTS) {
         maxAttempts = LIMIT_ATTEMPTS;
      }

      for (int attempt = 0; ; attempt++) {
         try {
            retryOperation.doIt(attempt);
            break; // call was successful
         } catch (Throwable t) {
            if (attempt < maxAttempts) {
               // exponential backoff before trying again
               exponentialSleep(attempt+1);
               continue;
            }
            // reached max. attempts, don't retry, print stacktrace and throw the throwable
            t.printStackTrace();
            throw t;
         }
      }
   }

   // using 6: limit max wait time (2^6 *100ms =  6,4 seconds in the last attempt)
   // using 7: limit max wait time (2^7 *100ms = 12,8 seconds in the last attempt)
   // based on https://docs.aws.amazon.com/en_us/general/latest/gr/api-retries.html
   private static void exponentialSleep(int count) throws InterruptedException {
      Thread.sleep(((long) Math.pow(2, count) * 100L));
   }
}
