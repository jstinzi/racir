library(testthat)
library(racir)
context("Calibrating racir files")

set.seed(5)
n <- 50
df1 <- data.frame(obs   = c(runif(n, 1, 200)),
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
n <- 50
df2 <- data.frame(obs   = c(runif(n, 1, 200)),
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

data <- list(df1, df2)

testdata <- racircal(caldata = data[[1]], data = data[[2]],
         mincut = 300, maxcut = 900)

test_that("Outputs", {
  expect_equal(object = length(testdata), 14)
})
