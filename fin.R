library(readxl)

# Read the Excel file
Data <- read_excel("DataQ2.xlsx", sheet = "Hoja1", range = "A30:AJ238")
Data2 <- read_excel("DataQ2.xlsx", sheet = "Hoja1", range = "A1:AJ238")



colnames(Data) <- Data2[1, ]
Data
Data2 <- Data[, -1]  # Exclude the first column which contains the time
Data2
Data2$Bubble <- Data2$`SP P/E`-Data2$`Shiller P/E`

str(Data2)


Y <- data.frame(
  Y1 = 100 * diff(log(Data2[[1]])),
  Y2 = 100 * diff(log(Data2[[2]])),
  Y3 = 100 * diff(log(Data2[[21]])),
  Y4 = 100 * diff(log(Data2[[10]])),
  Y5 = Data2[-1, 6],  # Take all rows from the second row onwards for column 6
  Y6 = 100 * diff(log(Data2[[8]]))
  
)
colnames(Y) <- c("GDP", "GDP Deflator","Adjusted Dividends","WB Commodity price index","Federal Fund Rates","adjusted S&P 500") 

Y <- data.frame(
  Y1 = 100 * diff(log(Data2[[1]])),
  Y2 = 100 * diff(log(Data2[[2]])),
  Y3 = 100 * diff(log(Data2[[21]])),
  Y4 = 100 * diff(log(Data2[[10]])),
  Y5 = Data2[-1, 6],  # Take all rows from the second row onwards for column 6
  Y6 = 100 * diff(log(Data2[[35])),
  Y7 = 100 * diff(log(Data2[[34]]))
  
)

Data2[[36]]
colnames(Y) <- c("GDP", "GDP Deflator","Adjusted Dividends","WB Commodity price index","Federal Fund Rates","Bubble","Fundamental") 






library(vars)

var1 <- VAR(Y, lag.max = 4, ic = "AIC")


amat <- diag(7)
amat[lower.tri(amat, diag = FALSE)] <- NA

svar1 <- SVAR(var1, estmethod="direct", Amat=amat)
irf.svar <- irf(svar1)

plot(irf.svar)

install.packages("sas7bdat")
library(sas7bdat)
data <- read.sas7bdat("x.sas7bdat")

