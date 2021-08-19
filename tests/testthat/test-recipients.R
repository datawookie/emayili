test_that("to: set/get", {
  msg <- envelope() %>% to("bob@gmail.com")
  expect_equal(msg$header$To, address("bob@gmail.com"))
  expect_equal(to(msg), address("bob@gmail.com"))
})

test_that("to: set multiple recipients", {
  msg <- envelope() %>% to("bob@gmail.com", "alice@yahoo.com")
  expect_equal(msg$header$To, address(c("bob@gmail.com", "alice@yahoo.com")))
})

test_that("to: set from vector", {
  msg <- envelope() %>% to(c("bob@gmail.com", "alice@yahoo.com"))
  expect_equal(msg$header$To, address(c("bob@gmail.com", "alice@yahoo.com")))
})

test_that("from: set/get", {
  msg <- envelope() %>% from("craig@gmail.com")
  expect_equal(msg$header$From, address("craig@gmail.com"))
  expect_equal(from(msg), address("craig@gmail.com"))
})

test_that("cc: set/get", {
  msg <- envelope() %>% cc("bob@gmail.com")
  expect_equal(msg$header$Cc, address("bob@gmail.com"))
  expect_equal(cc(msg), address("bob@gmail.com"))
})

test_that("cc: set multiple recipients", {
  msg <- envelope() %>% cc("bob@gmail.com", "alice@yahoo.com")
  expect_equal(msg$header$Cc, address(c("bob@gmail.com", "alice@yahoo.com")))
})

test_that("cc: set from vector", {
  msg <- envelope() %>% cc(c("bob@gmail.com", "alice@yahoo.com"))
  expect_equal(msg$header$Cc, address(c("bob@gmail.com", "alice@yahoo.com")))
})

test_that("bcc: set/get", {
  msg <- envelope() %>% bcc("bob@gmail.com")
  expect_equal(msg$header$Bcc, address(c("bob@gmail.com")))
  expect_equal(bcc(msg), address("bob@gmail.com"))
})

test_that("bcc: set  multiple recipients", {
  msg <- envelope() %>% bcc("bob@gmail.com", "alice@yahoo.com")
  expect_equal(msg$header$Bcc, address(c("bob@gmail.com", "alice@yahoo.com")))
})

test_that("bcc: set from vector", {
  msg <- envelope() %>% bcc(c("bob@gmail.com", "alice@yahoo.com"))
  expect_equal(msg$header$Bcc, address(c("bob@gmail.com", "alice@yahoo.com")))
})

test_that("reply: set/get", {
  msg <- envelope() %>% reply("craig@gmail.com")
  expect_equal(msg$header$Reply, address("craig@gmail.com"))
  expect_equal(reply(msg), address("craig@gmail.com"))
})

test_that("sender: set/get", {
  msg <- envelope() %>% sender("craig@gmail.com")
  expect_equal(msg$header$Sender, address("craig@gmail.com"))
  expect_equal(sender(msg), address("craig@gmail.com"))
})
