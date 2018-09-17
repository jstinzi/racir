library(testthat)
library(racir)
context("Reading Files")

set.seed(5)
n <- 50
df <- data.frame(E     = c(rep(0, 52), "E", 0, runif(n, 10 / 10000, 20 / 1000)),
                 A     = c(rep(0, 52), "A", 0, runif(n, -1, 30)),
                 Ci    = c(rep(0, 52), "Ci", 0, runif(n, 50, 700)),
                 gtc   = c(rep(0, 52), "gtc", 0, runif(n, 0.02, 0.3)),
                 Ca    = c(rep(0, 52), "Ca", 0, runif(n, 300, 900)),
                 CO2_r = c(rep(0, 52), "CO2_r", 0, runif(n, 300, 900)),
                 CO2_s = c(rep(0, 52), "CO2_s", 0, runif(n, 300, 900)),
                 H2O_r = c(rep(0, 52), "H2O_r", 0, runif(n, 10, 20)),
                 H2O_s = c(rep(0, 52), "H2O_s", 0, runif(n, 10, 20)),
                 Tleaf = c(rep(0, 52), "Tleaf", 0, rep(25, n)),
                 Qin   = c(rep(0, 52), "Qin", 0, rep(1500, n))
)
write.table(df, "df", sep = "\t", row.names = FALSE)

data <- read_6800("df")

test_that("Columns", {
  expect_is(object = data, class = "data.frame")
  expect_length(object = data, length(df))
})

unlink("df", recursive = FALSE)
