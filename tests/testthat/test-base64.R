
test_that("base64_decode", {

  decode_tests <- c(
    'YWE='   = 'aa',
    ' YWE='  =  'aa',
    'Y WE='  =  'aa',
    'YWE= '  =  'aa',
    "Y\nW\r\nE=" =  'aa',
    'YWE=====' =  'aa',    # extra padding
    'YWE'      =  'aa',    # missing padding
    'YWFh====' =  'aaa',
    'YQ'       =  'a',
    'Y'        = '',
    'x=='      = ''
  )

  for (i in seq_along(decode_tests)) {
    encoded <- names(decode_tests)[[i]]
    expected <- decode_tests[[i]]

    decoded <- base64_decode(encoded)
    expect_equal(decoded, expected)
  }
})
