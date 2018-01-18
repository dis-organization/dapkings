context("test-basic-access.R")

skip_if_not(capabilities("http/ftp"))
test_that("we can connect", {
   expect_true(have_dap())
})
