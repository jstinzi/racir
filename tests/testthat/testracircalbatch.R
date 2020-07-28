library(testthat)
library(racir)
context("Batch racir analysis")

data <- vector("list", 2)

set.seed(5)
n <- 50
data[[1]] <- data.frame(obs   = c(runif(n, 1, 200)),
                  E     = c(runif(n, 10 / 10000, 20 / 1000)),
                  A     = c(runif(n, -1, 30)),
                  Ci    = c(runif(n, 50, 700)),
                  gtc   = c(runif(n, 0.02, 0.3)),
                  Ca    = c(runif(n, 300, 900)),
                  CO2_r = c(runif(n, 300, 900)),
                  CO2_s = c(runif(n, 300, 900)),
                  H2O_r = c(runif(n, 10, 20)),
                  H2O_s = c(runif(n, 10, 20)),
                  Tleaf = c(rep(25, n)),
                  Qin   = c(rep(1500, n))
)

set.seed(6)
data[[2]] <- data.frame(obs   = c(runif(n, 1, 200)),
                  E     = c(runif(n, 10 / 10000, 20 / 1000)),
                  A     = c(runif(n, -1, 30)),
                  Ci    = c(runif(n, 50, 700)),
                  gtc   = c(runif(n, 0.02, 0.3)),
                  Ca    = c(runif(n, 300, 900)),
                  CO2_r = c(runif(n, 300, 900)),
                  CO2_s = c(runif(n, 300, 900)),
                  H2O_r = c(runif(n, 10, 20)),
                  H2O_s = c(runif(n, 10, 20)),
                  Tleaf = c(rep(25, n)),
                  Qin   = c(rep(1500, n))
)

set.seed(7)
cal <- data.frame(obs   = c(runif(n, 1, 200)),
                        E     = c(runif(n, 10 / 10000, 20 / 1000)),
                        A     = c(runif(n, -1, 30)),
                        Ci    = c(runif(n, 50, 700)),
                        gtc   = c(runif(n, 0.02, 0.3)),
                        Ca    = c(runif(n, 300, 900)),
                        CO2_r = c(runif(n, 300, 900)),
                        CO2_s = c(runif(n, 300, 900)),
                        H2O_r = c(runif(n, 10, 20)),
                        H2O_s = c(runif(n, 10, 20)),
                        Tleaf = c(rep(25, n)),
                        Qin   = c(rep(1500, n))
)

output <- racircalbatch(caldata = cal,
              data = data,
              mincut = 300,
              maxcut = 900)

test_that("Output", {
  expect_is(object = output, class = "list")
  expect_is(object = output[[1]], class = "data.frame")
  expect_is(object = output[[2]], class = "data.frame")
  expect_length(object = output, 2)
  expect_equal(object = length(output[[1]]), length(data[[1]]) + 3)
  expect_equal(object = length(output[[2]]), length(data[[2]]) + 3)
})

output <- racircalbatch_advanced(caldata = cal,
                        data = data,
                        mincut = 300,
                        maxcut = 900,
                        digits = -3)

test_that("Output", {
  expect_is(object = output, class = "list")
  expect_is(object = output[[1]], class = "data.frame")
  expect_is(object = output[[2]], class = "data.frame")
  expect_length(object = output, 2)
  expect_equal(object = length(output[[1]]), length(data[[1]]) + 4)
  expect_equal(object = length(output[[2]]), length(data[[2]]) + 4)
})
