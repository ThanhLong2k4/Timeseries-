---
title: "Untitled"
author: "Nguyễn Thành Long"
date: "2025-04-01"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
```{r}

library(readxl)
library(tseries)
library(urca)
library(forecast)
```

```{r}
data=read_excel("C:/Users/DELL/Documents/Chuỗi thời gian/DataCpSHS.xlsx")
price=data$price
ln_price=data$log
ln_price=na.omit(ln_price)

adf_none <- ur.df(price, type = "none", selectlags = "AIC")
summary(adf_none)

adf_drift <- ur.df(price, type = "drift", selectlags = "AIC")
summary(adf_drift)

adf_trend <- ur.df(price, type = "trend", selectlags = "AIC")
summary(adf_trend)
```
```{r}
diff_price <- diff(price)  # Tính chuỗi sai phân bậc 1
adf_diff <- ur.df(diff_price, type = "drift", selectlags = "AIC")
summary(adf_diff)
```
```{r}
# Mô hình ARIMA(1,1,0)
arima_1 <- Arima(price, order = c(1,1,0))
AIC_1 <- arima_1$aic
print(paste("AIC của ARIMA(1,1,0):", AIC_1))

# Mô hình ARIMA(0,1,1)
arima_2 <- Arima(price, order = c(0,1,1))
AIC_2 <- arima_2$aic
print(paste("AIC của ARIMA(0,1,1):", AIC_2))
```
```{r}
forecast_result <- forecast(arima_1, h = 10)
print(forecast_result)
```
```{r}
adf_none1 <- ur.df(ln_price, type = "none", selectlags = "AIC")
summary(adf_none)

adf_drift1 <- ur.df(ln_price, type = "drift", selectlags = "AIC")
summary(adf_drift)

adf_trend1 <- ur.df(ln_price, type = "trend", selectlags = "AIC")
summary(adf_trend)
```
```{r}
# Mô hình ARIMA(1,0,1)
arima_3 <- Arima(ln_price, order = c(1,0,1))
AIC_3 <- arima_3$aic
print(paste("AIC của ARIMA(1,0,1):", AIC_3))

# Mô hình ARIMA(2,0,1)
arima_4 <- Arima(ln_price, order = c(2,0,1))
AIC_4 <- arima_4$aic
print(paste("AIC của ARIMA(2,0,1):", AIC_4))
```
```{r}
forecast_result2 <- forecast(arima_3, h = 10)
print(forecast_result2)
```

