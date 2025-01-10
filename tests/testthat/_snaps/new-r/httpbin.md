# /cookies/set/:name/:value

    Code
      curl::handle_cookies(handle)
    Output
           domain  flag path secure expiration name value
      1 127.0.0.1 FALSE    /  FALSE        Inf  foo   bar

# /cookies/set

    Code
      curl::handle_cookies(handle)
    Output
           domain  flag path secure expiration name value
      1 127.0.0.1 FALSE    /  FALSE        Inf  foo   bar
      2 127.0.0.1 FALSE    /  FALSE        Inf  bar   baz

# /cookies/delete

    Code
      curl::handle_cookies(handle)
    Output
           domain  flag path secure expiration name value
      1 127.0.0.1 FALSE    /  FALSE        Inf  bar   baz

