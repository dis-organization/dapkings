context("test-basic-access.R")

skip_if_not(capabilities("http/ftp"))
test_that("we can connect", {
   expect_true(have_dap())
})

url <- canon_dap_url()
test_that("we can connect", {
  open_dap(url) %>% expect_s3_class("open_dap")
})

