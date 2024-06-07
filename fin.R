library(readxl)
library(ggplot2)
library(dplyr)

# Read the Excel file
Data <- read_excel("DataQ2.xlsx", sheet = "Hoja1", range = "A30:AE238")

Data2 <- read_excel("DataQ2.xlsx", sheet = "Hoja1", range = "A1:AE238")



colnames(Data) <- Data2[1, ]
Data
Data2 <- Data[, -1]  # Exclude the first column which contains the time
Data2
library(TTR)
library(zoo) # for rolling calculations

Data2$EMA20 <- EMA(Data2$`Shiller P/E`, n = 40)


# Calculate the 10-year moving average of CAPE prices
X<- cumsum(Data2$`Shiller P/E`)[1:39] / 1:39
Data2$EMA20[1:39]<-X



n <- nrow(Data2)  # Get the number of rows

# Create an empty column for standard deviation
Data2$SD <- NA

# Loop through rows 40 to n (inclusive)
for (i in 40:n) {
  Data2$SD[i] <- sd(Data2$EMA20[(i - 39):i], na.rm = TRUE)
}


# Loop through rows 1 to n (inclusive)
for (i in 1:39) {
  # Calculate standard deviation of all CAPE values up to row i
  Data2$SD[i] <- sd(Data2$EMA20[1:i], na.rm = TRUE)
}
Data2$SD[1]<-0

Data2$threshold<-Data2$EMA20+1.5*Data2$SD



#Data2$ShillerEarning <- Data2$`S&P500`/Data2$`SP P/E`
#Data2$ShillerPrice <- Data2$`Shiller P/E`*Data2$ShillerEarning

Data2$Bubble <- Data2$`Shiller P/E`-Data2$threshold
Data2$Fundamental<-Data2$`Shiller P/E`-Data2$Bubble


Data2$Bubble<-Data2$Bubble +16

Data2$Time<- 
ggplot(Data2, aes(x = Data2$Time, y = Data2$`Federal Funds Rate`)) +
  
  # Add geometry - line in this case
  geom_line(color = "black") +
  
  # Labels and title
  labs(title = "Federal Funds Rate",
       x = "Time",
       y = "Value")

Data3 <- Data2 %>% select(1, 2, 10, 6,34,35)
colnames(Data3) <- c("GDP", "GDP Deflator","WBCPI","Federal Fund Rates","Bubble Component","Fundamental Component") 

summary(Data3)
correlation_matrix <- cor(Data3)

library(corrplot)

# Create the correlogram
corrplot(correlation_matrix, type = "lower", tl.cex = 0.7)

Y <- data.frame(
  Y1 = 100 * diff(log(Data2$GDPC1)),
  Y2 = 100 * diff(log(Data2$GDPDEF)),
  Y3 = 100 * diff(log(Data2$`WB Commodity price index-non-energy`)),
  Y4 = Data2[-1, 6], 
  Y5 = 100 * diff(log(Data2$Bubble)),
  Y6 = 100 * diff(log(Data2$Fundamental))
  
  
)
colnames(Y) <- c("GDP", "GDP Deflator","WBCPI","Federal Fund Rates","Bubble Component","Fundamental Component") 

print(summary(Y))


Y <- data.frame(
  Y5 = Data2[-1, 6],
  # Take all rows from the second row onwards for column 6
  Y1 = 100 * diff(log(Data2$fundamental2))

)

colnames(Y) <- c("FFR","fun") 






library(vars)

var1 <- VAR(Y, lag.max = 4, ic = "AIC")


amat <- diag(6)
amat[lower.tri(amat, diag = FALSE)] <- NA

svar1 <- SVAR(var1, estmethod="direct", Amat=amat)
irf.svar <- irf(svar1)
irf.svar
plot(irf.svar)


acf_plot <- acf(Data2$Fundamental, main = "ACF Plot")
pacf_plot <- pacf(Data2$Fundamental, main = "PACF Plot")


acf_plot <- acf(Data2$Bubble, main = "ACF Plot")
pacf_plot <- pacf(Data2$Bubble, main = "PACF Plot")

acf_plot <- acf(Data2$`Federal Funds Rate`, main = "ACF Plot")
pacf_plot <- pacf(Data2$`Federal Funds Rate`, main = "PACF Plot")


install.packages("ggplot2")
library(sas7bdat)
data <- read.sas7bdat("x.sas7bdat")
install.packages("dplyr")
install.packages("TTR")
install.packages("corrplot")


