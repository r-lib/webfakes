# parse_range

    Code
      parse_range("foobar=1-100")
    Output
      NULL
    Code
      parse_range("bytes=0-100, 50-150")
    Output
      NULL
    Code
      parse_range("bytes=200-100")
    Output
      NULL
    Code
      parse_range("bytes=x-100")
    Output
      NULL
    Code
      parse_range("bytes=1-100")
    Output
           [,1] [,2]
      [1,]    1  100
    Code
      parse_range("bytes=1-")
    Output
           [,1] [,2]
      [1,]    1  Inf
    Code
      parse_range("bytes=-100")
    Output
           [,1] [,2]
      [1,]    0 -100
    Code
      parse_range("bytes=0-100, 200-")
    Output
           [,1] [,2]
      [1,]    0  100
      [2,]  200  Inf
    Code
      parse_range("bytes=200-300, 0-100")
    Output
           [,1] [,2]
      [1,]    0  100
      [2,]  200  300

