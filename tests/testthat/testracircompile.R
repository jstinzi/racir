library(testthat)
library(racir)
context("Compiling racirs")

set.seed(5)
n <- 50
df <- data.frame(E     = runif(n, 10 / 10000, 20 / 1000),
                 A     = runif(n, -1, 30),
                 Ci    = runif(n, 50, 700),
                 gtc   = runif(n, 0.02, 0.3),
                 Ca    = runif(n, 300, 900),
                 CO2_r = runif(n, 300, 900),
                 CO2_s = runif(n, 300, 900),
                 H2O_r = runif(n, 10, 20),
                 H2O_s = runif(n, 10, 20),
                 Tleaf = rep(25, n),
                 Qin   = rep(1500, n)
)
write.csv(df, "df.csv")

set.seed(3)
n <- 50
df2 <- data.frame(E     = runif(n, 10 / 10000, 20 / 1000),
                  A     = runif(n, -1, 30),
                  Ci    = runif(n, 50, 700),
                  gtc   = runif(n, 0.02, 0.3),
                  Ca    = runif(n, 300, 900),
                  CO2_r = runif(n, 300, 900),
                  CO2_s = runif(n, 300, 900),
                  H2O_r = runif(n, 10, 20),
                  H2O_s = runif(n, 10, 20),
                  Tleaf = rep(25, n),
                  Qin   = rep(1500, n)
)
write.csv(df2, "df2.csv")

files_list_1 <- list.files(pattern = "*.csv")

compile_racir("output.csv")

files_list_2 <- list.files(pattern = "*.csv")

test_that("Compile", {
  expect_is(object = files_list_1, class = "character")
  expect_is(object = files_list_2, class = "character")
  expect_length(object = files_list_2, (length(files_list_1) + 1))
})

unlink("df.csv", recursive = FALSE)
unlink("df2.csv", recursive = FALSE)
unlink("output.csv", recursive = FALSE)
