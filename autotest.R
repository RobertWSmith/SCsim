library(testthat)

auto_test(
  code_path = file.path(getwd(), 'R'),
  test_path = file.path(getwd(), 'inst', 'tests')
  )