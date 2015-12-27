context("Utilities");

test_that("Milliseconds convert", {
  expect_match(MilliSecondsToDate(1), "1970-01-01 00:00:01");
});