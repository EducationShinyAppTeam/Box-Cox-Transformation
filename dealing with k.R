rm(list = ls())
library(data.table)

data <- fread("./South_America_Life_Expectancy.csv")



data$Income <- lapply(
  X = data$Income,
  FUN = function(x){
    if (grepl(pattern = "k$", x = x)) {
      temp0 <- sub(pattern = "k$", replacement = "", x = x)
      return(as.numeric(temp0) * 1000)
    } else {
      return(x)
    }
  }
)

data$Income <- as.numeric(data$Income)

fwrite(data, "./South_America_Life.csv")
