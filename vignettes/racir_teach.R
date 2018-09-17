## ------------------------------------------------------------------------
library(racir)

## ------------------------------------------------------------------------
data <- read_6800(system.file("extdata", "cal", package = "racir"))

## ----fig.height = 4, fig.width = 6---------------------------------------
racircalcheck(calfile = system.file("extdata", "cal", package = "racir"))

## ----fig.height = 4, fig.width = 6---------------------------------------
racircalcheck(calfile = system.file("extdata", "cal", package = "racir"),
              mincut = 350, maxcut = 780)

## ----fig.height = 4, fig.width = 6---------------------------------------
racircal(calfile = system.file("extdata", "cal", package = "racir"),
         mincut = 350, maxcut = 780,
         datafile = system.file("extdata", "poplar_2", package = "racir"))

## ----fig.height = 4, fig.width = 6---------------------------------------
#Create a list of files
myfiles <- c(system.file("extdata", "poplar_1", package = "racir"),
             system.file("extdata", "poplar_2", package = "racir"))

#If all files in a folder pertain to one calibration file, you can use:
#myfiles = list.files()

racircalbatch(calfile = system.file("extdata", "cal", package = "racir"),
         mincut = 350, maxcut = 780,
         datafiles = myfiles)

## ----eval = FALSE--------------------------------------------------------
#  compile_racir(outputfile = "mydata.csv")

