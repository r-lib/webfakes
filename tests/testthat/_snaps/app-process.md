# error if cannot start

    Code
      new_app_process(app, process_timeout = 100, start = TRUE)
    Condition
      Error in `self$start()`:
      ! webfakes app subprocess did not start :(

---

    Code
      new_app_process(app, start = TRUE)
    Condition
      Error:
      ! failed to start webfakes app process: in callr subprocess.
      Caused by error:
      ! oops

---

    Code
      new_app_process(app, start = TRUE)
    Condition
      Error in `self$start()`:
      ! Unexpected message from webfakes app subprocess. Report a bug please.

