test_that("to: set from individual arguments", {
  msg <- envelope() %>% to("bob@gmail.com", "alice@yahoo.com")
  expect_equal(msg$header$To, c("bob@gmail.com", "alice@yahoo.com"))
})

test_that("to: set from vector", {
  msg <- envelope() %>% to(c("bob@gmail.com", "alice@yahoo.com"))
  expect_equal(msg$header$To, c("bob@gmail.com", "alice@yahoo.com"))
})

test_that("from: set", {
  msg <- envelope() %>% from("craig@gmail.com")
  expect_equal(msg$header$From, "craig@gmail.com")
})

test_that("cc: set from individual arguments", {
  msg <- envelope() %>% cc("bob@gmail.com", "alice@yahoo.com")
  expect_equal(msg$header$Cc, c("bob@gmail.com", "alice@yahoo.com"))
})

test_that("cc: set from vector", {
  msg <- envelope() %>% cc(c("bob@gmail.com", "alice@yahoo.com"))
  expect_equal(msg$header$Cc, c("bob@gmail.com", "alice@yahoo.com"))
})

test_that("bcc: set from individual arguments", {
  msg <- envelope() %>% bcc("bob@gmail.com", "alice@yahoo.com")
  expect_equal(msg$header$Bcc, c("bob@gmail.com", "alice@yahoo.com"))
})

test_that("bcc: set from vector", {
  msg <- envelope() %>% bcc(c("bob@gmail.com", "alice@yahoo.com"))
  expect_equal(msg$header$Bcc, c("bob@gmail.com", "alice@yahoo.com"))
})

test_that("reply: set from individual arguments", {
  msg <- envelope() %>% reply("craig@gmail.com")
  expect_equal(msg$header$Reply, "craig@gmail.com")
})
