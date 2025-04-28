# invalid handler

    Code
      app$use("foobar")
    Condition
      Error in `parse_handlers()`:
      ! Invalid webfakes handler
    Code
      app$get("/foo", 1:100)
    Condition
      Error in `parse_handlers()`:
      ! Invalid webfakes handler

