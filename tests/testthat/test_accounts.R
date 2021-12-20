source('init.R')

test_that("List Accounts", {
  skip_on_cran()
  skip_on_travis()
  accounts <- gtm_accounts_list()
  expect_equal(ncol(accounts), 3)
  expect_equal(nrow(accounts), 1)  
})


test_that("Get an account", {
  skip_on_cran()
  skip_on_travis()
  account <- gtm_accounts_get(account_id)
  expect_equal(length(account), 5)
})
