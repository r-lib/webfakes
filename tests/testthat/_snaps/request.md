# parse_query

    Code
      parse_query("foo")
    Output
      $foo
      [1] ""
      
    Code
      parse_query("?foo")
    Output
      $foo
      [1] ""
      
    Code
      parse_query("?foo&bar")
    Output
      $foo
      [1] ""
      
      $bar
      [1] ""
      
    Code
      parse_query("?foo&bar=baz")
    Output
      $foo
      [1] ""
      
      $bar
      [1] "baz"
      

