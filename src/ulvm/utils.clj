(ns ^{:author "Adam Berger"} ulvm.utils
  "ULVM utilities")

(defmacro retrying
  [initial-backoff max-retries retryable-error body]
  `(loop [backoff#           ~initial-backoff
          remaining-retries# ~max-retries
          val#               ~body]
    (if (and (~retryable-error val#) (> remaining-retries# 0))
      (do (Thread/sleep backoff#)
          (recur (* backoff# 2)
                 (dec remaining-retries#)
                 ~body))
      val#)))
