library(gstar)
library(xts)
library(readxl)
library(starma)
library(tseries)

# Model 12 Kota
Location <- read_excel('boxcoxdata.xlsx')
View(Location)
#-----Use data with xts object----#
x = xts(Location[, -1], order.by = as.Date(Location$Tanggal,frac=1))
s <- round(nrow(x) * 0.8) ## split into training and testing (80:20)
x_train <- x[1:s, ]
x_test <- x[-c(1:s), ]

weight = read.csv("matriks invers jarak.csv",header=FALSE)
weight = as.matrix(weight)
weight = weight/(ncol(x) - 1) #the sum of weight is equal to 1 every row.
stacf(x_train,wlist = weight)
stpacf(x_train,wlist = weight)

# Stationary test
adf.test(x_train$jakarta_bc)
adf.test(x_train$bogor_bc)
adf.test(x_train$depok_bc)
adf.test(x_train$tangerang_bc)
adf.test(x_train$bekasi_bc)
adf.test(x_train$surabaya_bc)
adf.test(x_train$bandung_bc)
adf.test(x_train$medan_bc)
adf.test(x_train$makassar_bc)
adf.test(x_train$semarang_bc)
adf.test(x_train$palembang_bc)
adf.test(x_train$denpasar_bc)

# Ljung Box test
Box.test(x_train$jakarta_bc,type = "Ljung")
Box.test(x_train$bogor_bc,type = "Ljung")
Box.test(x_train$depok_bc,type = "Ljung")
Box.test(x_train$tangerang_bc,type = "Ljung")
Box.test(x_train$bekasi_bc,type = "Ljung")
Box.test(x_train$surabaya_bc,type = "Ljung")
Box.test(x_train$bandung_bc,type = "Ljung")
Box.test(x_train$medan_bc,type = "Ljung")
Box.test(x_train$makassar_bc,type = "Ljung")
Box.test(x_train$semarang_bc,type = "Ljung")
Box.test(x_train$palembang_bc,type = "Ljung")
Box.test(x_train$denpasar_bc,type = "Ljung")

# QQ plot
par(mfrow=c(3,1))
qqnorm(x_train$jakarta_bc, main="QQ Plot Jakarta", xlab="Theoretical Quantiles", ylab="Sample Quantiles", col="blue", pch=19)
qqline(x_train$jakarta_bc, col="red", lty=2)
qqnorm(x_train$bogor_bc, main="QQ Plot Bogor", xlab="Theoretical Quantiles", ylab="Sample Quantiles", col="blue", pch=19)
qqline(x_train$bogor_bc, col="red", lty=2)
qqnorm(x_train$depok_bc, main="QQ Plot Depok", xlab="Theoretical Quantiles", ylab="Sample Quantiles", col="blue", pch=19)
qqline(x_train$depok_bc, col="red", lty=2)
qqnorm(x_train$tangerang_bc, main="QQ Plot Tangerang", xlab="Theoretical Quantiles", ylab="Sample Quantiles", col="blue", pch=19)
qqline(x_train$tangerang_bc, col="red", lty=2)
qqnorm(x_train$bekasi_bc, main="QQ Plot Bekasi", xlab="Theoretical Quantiles", ylab="Sample Quantiles", col="blue", pch=19)
qqline(x_train$bekasi_bc, col="red", lty=2)
qqnorm(x_train$surabaya_bc, main="QQ Plot Surabaya", xlab="Theoretical Quantiles", ylab="Sample Quantiles", col="blue", pch=19)
qqline(x_train$surabaya_bc, col="red", lty=2)
qqnorm(x_train$bandung_bc, main="QQ Plot Bandung", xlab="Theoretical Quantiles", ylab="Sample Quantiles", col="blue", pch=19)
qqline(x_train$bandung_bc, col="red", lty=2)
qqnorm(x_train$medan_bc, main="QQ Plot Medan", xlab="Theoretical Quantiles", ylab="Sample Quantiles", col="blue", pch=19)
qqline(x_train$medan_bc, col="red", lty=2)
qqnorm(x_train$makassar_bc, main="QQ Plot Makassar", xlab="Theoretical Quantiles", ylab="Sample Quantiles", col="blue", pch=19)
qqline(x_train$makassar_bc, col="red", lty=2)
qqnorm(x_train$semarang_bc, main="QQ Plot Semarang", xlab="Theoretical Quantiles", ylab="Sample Quantiles", col="blue", pch=19)
qqline(x_train$semarang_bc, col="red", lty=2)
qqnorm(x_train$palembang_bc, main="QQ Plot Palembang", xlab="Theoretical Quantiles", ylab="Sample Quantiles", col="blue", pch=19)
qqline(x_train$palembang_bc, col="red", lty=2)
qqnorm(x_train$denpasar_bc, main="QQ Plot Denpasar", xlab="Theoretical Quantiles", ylab="Sample Quantiles", col="blue", pch=19)
qqline(x_train$denpasar_bc, col="red", lty=2)


fit <- gstar(x_train, weight = weight,
             p = 1, d = 0, est = "OLS")
fit$B
write.csv(fit$B,"Psi Full.csv")

summary(fit)
performance(fit)
performance(fit, x_test) ## to check the performance with testing data
pred <- predict(fit, n = 10) #forecast 10 data ahead
write.csv(pred,"Forecasting Full.csv")
par(mfrow=c(1,1))
plot(fit)
plot(fit, n_predict = 10) #plot with 10 forecasting data
plot(fit, testing = x_test)#---- Use dataframe or matrix---#
x2 <- Location
x2$Tanggal <- NULL # remove the date column
# Create a data frame
city_data <- data.frame(
  City = c("Jakarta", "Bogor", "Depok", "Tangerang", "Bekasi", "Surabaya", "Bandung", "Medan", "Makassar", "Semarang", "Palembang", "Denpasar"),
  Latitude = c(-6.2088, -6.5946, -6.4025, -6.1751, -6.2349, -7.2575, -6.9175, 3.5952, -5.1477, -6.9713, -2.9761, -8.6705),
  Longitude = c(106.8456, 106.8066, 106.7942, 106.8272, 106.9946, 112.7521, 107.6191, 98.6733, 119.4327, 110.4256, 104.7759, 115.2115)
)

dst <- as.matrix(dist(city_data[, -1], diag = TRUE, upper = TRUE))
dst1 <- matrix(0, nrow = nrow(dst), ncol = ncol(dst))
for(i in 1:nrow(dst)) {
  for(j in 1:ncol(dst)){
    if(j == i) next
    dst1[i, j] <- sum(dst[i, -j])/sum(dst[i,])
  }
}
weight_inverse_distance <- matrix(0, nrow =
                                    nrow(dst), ncol = ncol(dst))
for(i in 1:nrow(dst)) {
  for(j in 1:ncol(dst)){
    if(j == i) next
    weight_inverse_distance[i, j] <- sum(dst1[i, j])/sum(dst1[i,])
  }
}
fit_inverse_distance <- gstar(x2, weight =
                                weight_inverse_distance, p = 2, d = 1, est = "OLS")
write.csv(fit_inverse_distance$B,"Fit Inverse Distance Full.csv")
summary(fit_inverse_distance)
performance(fit_inverse_distance)
predict(fit_inverse_distance)
plot(fit_inverse_distance)

# Model Cluster 1 3 Kota
Location <- read_excel('boxcoxdata.xlsx',sheet = "Sheet2")
View(Location)
#-----Use data with xts object----#
x = xts(Location[, -1], order.by = as.Date(Location$Tanggal,frac=1))
s <- round(nrow(x) * 0.8) ## split into training and testing (80:20)
x_train <- x[1:s, ]
x_test <- x[-c(1:s), ]

weight = read.csv("matriks c1.csv",header=FALSE)
weight = as.matrix(weight)
weight = weight/(ncol(x) - 1) #the sum of weight is equal to 1 every row.
stacf(x_train,wlist = weight)
stpacf(x_train,wlist = weight)

# Stationary test
adf.test(x_train$jakarta_bc)
adf.test(x_train$surabaya_bc)
adf.test(x_train$bandung_bc)

# Ljung Box test
Box.test(x_train$jakarta_bc,type = "Ljung")
Box.test(x_train$surabaya_bc,type = "Ljung")
Box.test(x_train$bandung_bc,type = "Ljung")

# QQ plot
par(mfrow=c(3,1))
qqnorm(x_train$jakarta_bc, main="QQ Plot Jakarta", xlab="Theoretical Quantiles", ylab="Sample Quantiles", col="blue", pch=19)
qqline(x_train$jakarta_bc, col="red", lty=2)
qqnorm(x_train$surabaya_bc, main="QQ Plot Surabaya", xlab="Theoretical Quantiles", ylab="Sample Quantiles", col="blue", pch=19)
qqline(x_train$surabaya_bc, col="red", lty=2)
qqnorm(x_train$bandung_bc, main="QQ Plot Bandung", xlab="Theoretical Quantiles", ylab="Sample Quantiles", col="blue", pch=19)
qqline(x_train$bandung_bc, col="red", lty=2)

fit <- gstar(x_train, weight = weight,
             p = 1, d = 0, est = "OLS")
fit
write.csv(fit$B,"Psi Cluster 1.csv")
summary(fit)
performance(fit)
performance(fit, x_test) ## to check the performance with testing data
pred <- predict(fit, n = 10) #forecast 10 data ahead
write.csv(pred,"Forecast Cluster 1.csv")
par(mfrow=c(1,1))
plot(fit)
plot(fit, n_predict = 10) #plot with 10 forecasting data
plot(fit, testing = x_test)
#---- Use dataframe or matrix---#
x2 <- Location
x2$Tanggal <- NULL # remove the date column
# Create a data frame
city_data <- data.frame(
  City = c("Jakarta", "Surabaya", "Bandung"),
  Latitude = c(-6.2088, -7.2575, -6.9175),
  Longitude = c(106.8456, 112.7521, 107.6191)
)

dst <- as.matrix(dist(city_data[, -1], diag = TRUE, upper = TRUE))
dst1 <- matrix(0, nrow = nrow(dst), ncol = ncol(dst))
for(i in 1:nrow(dst)) {
  for(j in 1:ncol(dst)){
    if(j == i) next
    dst1[i, j] <- sum(dst[i, -j])/sum(dst[i,])
  }
}
weight_inverse_distance <- matrix(0, nrow =
                                    nrow(dst), ncol = ncol(dst))
for(i in 1:nrow(dst)) {
  for(j in 1:ncol(dst)){
    if(j == i) next
    weight_inverse_distance[i, j] <- sum(dst1[i, j])/sum(dst1[i,])
  }
}
fit_inverse_distance <- gstar(x2, weight =
                                weight_inverse_distance, p = 2, d = 1, est = "OLS")
write.csv(fit_inverse_distance$B,"Fit Inverse Distance Cluster 1.csv")
summary(fit_inverse_distance)
performance(fit_inverse_distance)
predict(fit_inverse_distance)
plot(fit_inverse_distance)

# Model cluster 2 6 Kota
Location <- read_excel('boxcoxdata.xlsx',sheet = "Sheet3")
View(Location)
#-----Use data with xts object----#
x = xts(Location[, -1], order.by = as.Date(Location$Tanggal,frac=1))
s <- round(nrow(x) * 0.8) ## split into training and testing (80:20)
x_train <- x[1:s, ]
x_test <- x[-c(1:s), ]

weight = read.csv("Matriks c2.csv",header=FALSE)
weight = as.matrix(weight)
weight = weight/(ncol(x) - 1) #the sum of weight is equal to 1 every row.
stacf(x_train,wlist = weight)
stpacf(x_train,wlist = weight)

# Stationary test
adf.test(x_train$bogor_bc)
adf.test(x_train$depok_bc)
adf.test(x_train$tangerang_bc)
adf.test(x_train$makassar_bc)
adf.test(x_train$semarang_bc)
adf.test(x_train$palembang_bc)

# Ljung Box test
Box.test(x_train$bogor_bc,type = "Ljung")
Box.test(x_train$depok_bc,type = "Ljung")
Box.test(x_train$tangerang_bc,type = "Ljung")
Box.test(x_train$makassar_bc,type = "Ljung")
Box.test(x_train$semarang_bc,type = "Ljung")
Box.test(x_train$palembang_bc,type = "Ljung")

# QQ plot
par(mfrow=c(3,1))
qqnorm(x_train$bogor_bc, main="QQ Plot Bogor", xlab="Theoretical Quantiles", ylab="Sample Quantiles", col="blue", pch=19)
qqline(x_train$bogor_bc, col="red", lty=2)
qqnorm(x_train$depok_bc, main="QQ Plot Depok", xlab="Theoretical Quantiles", ylab="Sample Quantiles", col="blue", pch=19)
qqline(x_train$depok_bc, col="red", lty=2)
qqnorm(x_train$tangerang_bc, main="QQ Plot Tangerang", xlab="Theoretical Quantiles", ylab="Sample Quantiles", col="blue", pch=19)
qqline(x_train$tangerang_bc, col="red", lty=2)
qqnorm(x_train$makassar_bc, main="QQ Plot Makassar", xlab="Theoretical Quantiles", ylab="Sample Quantiles", col="blue", pch=19)
qqline(x_train$makassar_bc, col="red", lty=2)
qqnorm(x_train$semarang_bc, main="QQ Plot Semarang", xlab="Theoretical Quantiles", ylab="Sample Quantiles", col="blue", pch=19)
qqline(x_train$semarang_bc, col="red", lty=2)
qqnorm(x_train$palembang_bc, main="QQ Plot Palembang", xlab="Theoretical Quantiles", ylab="Sample Quantiles", col="blue", pch=19)
qqline(x_train$palembang_bc, col="red", lty=2)

fit <- gstar(x_train, weight = weight,
             p = 1, d = 0, est = "OLS")
fit
write.csv(fit$B,"Psi Cluster 2.csv")
summary(fit)
performance(fit)
performance(fit, x_test) ## to check the performance with testing data
pred <- predict(fit, n = 10) #forecast 10 data ahead
write.csv(pred,"Forecast Cluster 2.csv")
par(mfrow=c(1,1))
plot(fit)
plot(fit, n_predict = 10) #plot with 10 forecasting data
plot(fit, testing = x_test)
#---- Use dataframe or matrix---#
x2 <- Location
x2$Tanggal <- NULL # remove the date column
# Create a data frame
city_data <- data.frame(
  City = c("Bogor", "Depok", "Tangerang","Makassar", "Semarang", "Palembang"),
  Latitude = c(-6.5946, -6.4025, -6.1751, -5.1477, -6.9713, -2.9761),
  Longitude = c( 106.8066, 106.7942, 106.8272,119.4327, 110.4256, 104.7759)
)

dst <- as.matrix(dist(city_data[, -1], diag = TRUE, upper = TRUE))
dst1 <- matrix(0, nrow = nrow(dst), ncol = ncol(dst))
for(i in 1:nrow(dst)) {
  for(j in 1:ncol(dst)){
    if(j == i) next
    dst1[i, j] <- sum(dst[i, -j])/sum(dst[i,])
  }
}
weight_inverse_distance <- matrix(0, nrow =
                                    nrow(dst), ncol = ncol(dst))
for(i in 1:nrow(dst)) {
  for(j in 1:ncol(dst)){
    if(j == i) next
    weight_inverse_distance[i, j] <- sum(dst1[i, j])/sum(dst1[i,])
  }
}
fit_inverse_distance <- gstar(x2, weight =
                                weight_inverse_distance, p = 2, d = 1, est = "OLS")
write.csv(fit_inverse_distance$B,"Fit Inverse Distance Cluster 2.csv")
summary(fit_inverse_distance)
performance(fit_inverse_distance)
predict(fit_inverse_distance)
plot(fit_inverse_distance)

# Model cluster 3 3 Kota
Location <- read_excel('boxcoxdata.xlsx',sheet = "Sheet4")
View(Location)
#-----Use data with xts object----#
x = xts(Location[, -1], order.by = as.Date(Location$Tanggal,frac=1))
s <- round(nrow(x) * 0.8) ## split into training and testing (80:20)
x_train <- x[1:s, ]
x_test <- x[-c(1:s), ]

weight = read.csv("matriks c3.csv",header=FALSE)
weight = as.matrix(weight)
weight = weight/(ncol(x) - 1) #the sum of weight is equal to 1 every row.
stacf(x_train,wlist = weight)
stpacf(x_train,wlist = weight)

# Stationary test
adf.test(x_train$bekasi_bc)
adf.test(x_train$medan_bc)
adf.test(x_train$denpasar_bc)

# Ljung Box test
Box.test(x_train$bekasi_bc,type = "Ljung")
Box.test(x_train$medan_bc,type = "Ljung")
Box.test(x_train$denpasar_bc,type = "Ljung")

# QQ plot
par(mfrow=c(3,1))
qqnorm(x_train$bekasi_bc, main="QQ Plot Bekasi", xlab="Theoretical Quantiles", ylab="Sample Quantiles", col="blue", pch=19)
qqline(x_train$bekasi_bc, col="red", lty=2)
qqnorm(x_train$medan_bc, main="QQ Plot Medan", xlab="Theoretical Quantiles", ylab="Sample Quantiles", col="blue", pch=19)
qqline(x_train$medan_bc, col="red", lty=2)
qqnorm(x_train$denpasar_bc, main="QQ Plot Denpasar", xlab="Theoretical Quantiles", ylab="Sample Quantiles", col="blue", pch=19)
qqline(x_train$denpasar_bc, col="red", lty=2)

fit <- gstar(x_train, weight = weight,
             p = 1, d = 0, est = "OLS")
fit
write.csv(fit$B,"Psi Cluster 3.csv")
summary(fit)
performance(fit)
performance(fit, x_test) ## to check the performance with testing data
pred <- predict(fit, n = 10) #forecast 10 data ahead
write.csv(pred,"Forecast Cluster 3.csv")
par(mfrow=c(1,1))
plot(fit)
plot(fit, n_predict = 10) #plot with 10 forecasting data
plot(fit, testing = x_test)
#---- Use dataframe or matrix---#
x2 <- Location
x2$Tanggal <- NULL # remove the date column
# Create a data frame
city_data <- data.frame(
  City = c("Bekasi","Medan", "Denpasar"),
  Latitude = c(-6.2349,3.5952, -8.6705),
  Longitude = c(106.9946,98.6733, 115.2115)
)

dst <- as.matrix(dist(city_data[, -1], diag = TRUE, upper = TRUE))
dst1 <- matrix(0, nrow = nrow(dst), ncol = ncol(dst))
for(i in 1:nrow(dst)) {
  for(j in 1:ncol(dst)){
    if(j == i) next
    dst1[i, j] <- sum(dst[i, -j])/sum(dst[i,])
  }
}
weight_inverse_distance <- matrix(0, nrow =
                                    nrow(dst), ncol = ncol(dst))
for(i in 1:nrow(dst)) {
  for(j in 1:ncol(dst)){
    if(j == i) next
    weight_inverse_distance[i, j] <- sum(dst1[i, j])/sum(dst1[i,])
  }
}
fit_inverse_distance <- gstar(x2, weight =
                                weight_inverse_distance, p = 2, d = 1, est = "OLS")
write.csv(fit_inverse_distance$B,"Fit Inverse Distance Cluster 3.csv")
summary(fit_inverse_distance)
performance(fit_inverse_distance)
predict(fit_inverse_distance)
plot(fit_inverse_distance)
