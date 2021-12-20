library(testthat)
library(googleTagManageR)

account_id <- 6005575728
container_1 <- 56391183
container_1_public <- "GTM-5L3NK3J"



test_that("Check Correct Setup", {
  skip_on_cran()
  gtm_auth(sa_json="../../ci-service-account.json")
  accounts <- gtm_accounts_list()
  expect_true(account_id %in% accounts$accountId)
  containers <- gtm_containers_list(account_id)
  expect_true(container_1 %in% containers$containerId)
  expect_true(container_1_public %in% containers$publicId)
  }
)