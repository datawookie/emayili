test_that("to: set/get", {
  msg <- envelope() %>% to("bob@gmail.com")
  expect_equal(msg$header$To, c("bob@gmail.com"))
  expect_equal(to(msg), "bob@gmail.com")
})

test_that("to: set multiple recipients", {
  msg <- envelope() %>% to("bob@gmail.com", "alice@yahoo.com")
  expect_equal(msg$header$To, c("bob@gmail.com", "alice@yahoo.com"))
})

test_that("to: set from vector", {
  msg <- envelope() %>% to(c("bob@gmail.com", "alice@yahoo.com"))
  expect_equal(msg$header$To, c("bob@gmail.com", "alice@yahoo.com"))
})

test_that("from: set/get", {
  msg <- envelope() %>% from("craig@gmail.com")
  expect_equal(msg$header$From, "craig@gmail.com")
  expect_equal(from(msg), "craig@gmail.com")
})

test_that("cc: set/get", {
  msg <- envelope() %>% cc("bob@gmail.com")
  expect_equal(msg$header$Cc, c("bob@gmail.com"))
  expect_equal(cc(msg), "bob@gmail.com")
})

test_that("cc: set multiple recipients", {
  msg <- envelope() %>% cc("bob@gmail.com", "alice@yahoo.com")
  expect_equal(msg$header$Cc, c("bob@gmail.com", "alice@yahoo.com"))
})

test_that("cc: set from vector", {
  msg <- envelope() %>% cc(c("bob@gmail.com", "alice@yahoo.com"))
  expect_equal(msg$header$Cc, c("bob@gmail.com", "alice@yahoo.com"))
})

test_that("bcc: set/get", {
  msg <- envelope() %>% bcc("bob@gmail.com")
  expect_equal(msg$header$Bcc, c("bob@gmail.com"))
  expect_equal(bcc(msg), c("bob@gmail.com"))
})

test_that("bcc: set  multiple recipients", {
  msg <- envelope() %>% bcc("bob@gmail.com", "alice@yahoo.com")
  expect_equal(msg$header$Bcc, c("bob@gmail.com", "alice@yahoo.com"))
})

test_that("bcc: set from vector", {
  msg <- envelope() %>% bcc(c("bob@gmail.com", "alice@yahoo.com"))
  expect_equal(msg$header$Bcc, c("bob@gmail.com", "alice@yahoo.com"))
})

test_that("reply: set/get", {
  msg <- envelope() %>% reply("craig@gmail.com")
  expect_equal(msg$header$Reply, "craig@gmail.com")
  expect_equal(reply(msg), c("craig@gmail.com"))
})

test_that("sender: set/get", {
  msg <- envelope() %>% sender("craig@gmail.com")
  expect_equal(msg$header$Sender, "craig@gmail.com")
  expect_equal(sender(msg), c("craig@gmail.com"))
})
