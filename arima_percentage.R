library(tseries)
#https://otexts.com/fpp2/arima-r.html
# arima tutorial: https://otexts.com/fpp2/arima-r.html
# https://rpubs.com/ionaskel/VaR_Garch_market_risk
#install.packages("forecast")
library(forecast)
library(readxl)

table02_07_04 <- read_excel("Downloads/Complete_Data_Set_amansir/table02_07_23.xlsx")
table02_07_04 <- read_excel("table02_07_23.xlsx")
view(table02_07_04)
#table02_07_04 <- read_excel("table02_07_04.xlsx")
i= table02_07_04$real_interest_rate
g = table02_07_04$growth_rate_in_real_gdp
pb = table02_07_04$pb_as_percentage_of_gdp
dr = table02_07_04$debt_to_gdp_ratio
year = table02_07_04$year
drpnew = ((1+i)/(1+g))*dr - pb
#View(drpnew)   
head(drpnew) 
# plot the data
plot(year,drpnew, type ="l",xlab="year", ylab="drp")# Plot of the data for Deposit
#perform KPSS test
kpss.test(drpnew, null="Trend")
# adf test
adf.test(drpnew)
# acf plot of original data
acf(drpnew)

# plot the data
plot(year,dr, type ="l",xlab="Year", ylab="Debt-to-GDP Ratio")# Plot of the data for Deposit
#perform KPSS test
kpss.test(dr, null="Trend")
# adf test
adf.test(dr)
# pp test
pp.test(dr)
# acf plot of original data
acf(dr)
model.arima = auto.arima(dr, max.order = c(0 , 0 ,0) , stationary = TRUE , trace = T , ic = 'aicc')
acf(dr)     #auto-correlation function
pacf(dr) 
plot(forecast(model.arima))
title(main="",
      xlab="Date in year", ylab="dr in Millions")
pred1<-forecast(model.arima,7)
pred1
#######


dr_diff1 <- diff(dr)
#log_drp <- drpnew$logret
date <- year[-c(1)]
plot(date,dr_diff1 , type="l", xlab="Year", ylab="Debt-to-GDP Ratio")
#perform KPSS test
kpss.test(dr_diff1, null="Trend")
# adf test
adf.test(dr_diff1)
# pp test
pp.test(dr_diff1)
# acf plot of first difference of data
acf(dr_diff1)

model.arima = auto.arima(dr_diff1, max.order = c(0 , 2 ,1) , stationary = TRUE , trace = T , ic = 'aicc')
acf(dr_diff1)     #auto-correlation function
pacf(dr_diff1) 
model <- arima(x = dr, order = c(0, 2, 1))
summary(model)
plot(forecast(model)) # 
autoplot(forecast(model))
title(main="",
      xlab="Date in year", ylab="dr in Millions")
pred1<-forecast(model,7)
pred1

y <- c(2022, 2023, 2024, 2025, 2026, 2027,2028)
year1 = table02_07_04$year
year  = c(year1,y)# normality check for residuals 

p = c(dr,pred1$mean)
l = c(dr,pred1$lower[,1])
u = c(dr,pred1$upper[,1])

plot(year,l, type = "l", col='blue')
lines(year,u,type = "l", col = 'red')
lines(year,p, type ="l")
########3
checkresiduals(model)
install.packages("car")
library(car)
qqPlot(model$residuals)
shapiro.test(model$residuals)

# hypothesis test
ks.test(model$residuals, "pnorm")
# comparison plot


#######################################

#diff-2

dr_diff2 <- diff(diff(dr))
#log_drp <- drpnew$logret
date <- year[-c(1,2)]
plot(date,dr_diff2 , type="l", xlab="Year", ylab="Debt-to-GDP Ratio")
#perform KPSS test
kpss.test(dr_diff2, null="Trend")
# adf test
adf.test(dr_diff2)
# pp test
pp.test(dr_diff2)
# acf plot of first difference of data
acf(dr_diff2)

model.arima = auto.arima(dr_diff2, max.order = c(0 , 2 ,1) , stationary = TRUE , trace = T , ic = 'aicc')
acf(dr_diff2)     #auto-correlation function
pacf(dr_diff2) 
model <- arima(x = dr, order = c(0, 2, 1))
summary(model)
library(lmtest)
coeftest(model)
# 
plot(forecast(model)) # 
autoplot(forecast(model))
title(main="",
      type="l", xlab="Year", ylab="Debt-to-GDP Ratio")
pred1<-forecast(model,7)
pred1
# normality check for residuals 
checkresiduals(model)
install.packages("car")
library(car)
qqPlot(model$residuals)
shapiro.test(model$residuals)
# hypothesis test
ks.test(model$residuals, "pnorm")
# jarque.bera.test
jarque.bera.test(model$residuals)

# install.packages("fUnitRoots")
# library(fUnitRoots)
# library(forecast)
# install.packages("FitAR")
# library(FitAR)
# futurVal <- forecast.Arima(model,h=7, level=c(95.0))
# plot.forecast(futurVal)

# comparison plot
y <- c(2022, 2023, 2024, 2025, 2026, 2027,2028)
pred1<-forecast(model,7)
pred1
point_estimation = 100*pred1$mean
lower_ci = 100*(pred1$lower[,2])
upper_ci = 100*(pred1$upper[,2])
df = data.frame(y,point_estimation,lower_ci,upper_ci)
plot(df$y,df$point_estimation/100,type = "l",lty=2, lwd =3, ylim=c(1.10,2.00), xlab="Year", ylab="Debt-to-GDP Ratio")
par(new=TRUE)
plot(df$y,df$lower_ci/100,"l", col="blue", pch = 19, lwd =3, ylim=c(1.10,2.00), xlab="Year", ylab="Debt-to-GDP Ratio")
par(new=TRUE)
plot(df$y,df$upper_ci/100,"l",col="red", pch = 19, lwd =3,ylim=c(1.10,2.00), xlab="Year", ylab="Debt-to-GDP Ratio")

legend(2022, 1.98, legend=c("Pessimistic", "Neither optimistic nor pessimistic","Optimistic" ),
       col=c("red","black", "blue"), lty=1:2, cex=0.8)

#title(main="",
#      xlab="Year", ylab="Debt-to-GDP Ratio")
pred1<-forecast(model,7)
pred1

# normality check for residuals 
checkresiduals(model)
library(car)
qqPlot(model$residuals)
shapiro.test(model$residuals)
qq.plot(model$reresiduals)
# hypothesis test
ks.test(model$residuals, "pnorm")
# jarque.bera.test
jarque.bera.test(model$residuals)


#######################################

# comparison plot
# comparison plot
y <- c(2022, 2023, 2024, 2025, 2026, 2027,2028)
pred1<-forecast(model,7)
pred1
point_estimation = 100*pred1$mean
lower_ci = 100*(pred1$lower[,2])
upper_ci = 100*(pred1$upper[,2])
df = data.frame(y,point_estimation,lower_ci,upper_ci)
plot(df$y,df$point_estimation,type = "l",lty=2, lwd =3, ylim=c(110,200), xlab="Year", ylab="Debt-to-GDP Ratio")
par(new=TRUE)
plot(df$y,df$lower_ci,"l", col="blue", pch = 19, lwd =3, ylim=c(110,200), xlab="Year", ylab="Debt-to-GDP Ratio")
par(new=TRUE)
plot(df$y,df$upper_ci,"l",col="red", pch = 19, lwd =3,ylim=c(110,200), xlab="Year", ylab="Debt-to-GDP Ratio")

legend(2022, 198, legend=c("Pessimistic", "Neither optimistic nor pessimistic","Optimistic" ),
       col=c("red","black", "blue"), lty=1:2, cex=0.8)

#title(main="",
#      xlab="Year", ylab="Debt-to-GDP Ratio")




## table-03 

table03_07_23 <- read_excel("Downloads/Complete_Data_Set_amansir/table03_07_23.xlsx")
nominal_gdp=table03_07_23[,2]
n=62
nominal_gdp_ret = ((nominal_gdp[2:n, 1] - nominal_gdp[1:(n-1), 1])/nominal_gdp[1:(n-1), 1])
library("writexl")
write_xlsx(nominal_gdp_ret,"/Users/Fahad/Downloads/nominal_gdp_ret.xlsx")


####### part-2 #############################################
## arimax model
####  Forecasting base
gdp_rate <- read_excel("Downloads/Complete_Data_Set_amansir/gdp_rate.xlsx")
gdp_rate <- read_excel("gdp_rate.xlsx")
z = dr
model_base <- arima(z, xreg = gdp_rate$nominal_gdp, order = c(0, 2, 1))
summary(model_base)
pred = predict(model_base, newxreg = c(20953),n.ahead=7,ci=0.95)
p = pred$pred
predicted = p
lower_ci = p-pred$se
upper_ci = p+pred$se
table_baseline = data.frame(lower_ci,predicted,upper_ci)
table_baseline


dr = table02_07_04$debt_to_gdp_ratio*100

l = c(dr,lower_ci)
p = c(dr,predicted)
u = c(dr,upper_ci)

y <- c(2022, 2023, 2024, 2025, 2026, 2027,2028)
year1 = table02_07_04$year
year  = c(year1,y)

df1 = data.frame(year,l,p,u)
plot(df1$year,(df1$l)/100,'l',main ="Nominal GDP Baseline",lty=2, lwd =1, xlab="Year", ylab="Debt-to-GDP Ratio",col= "red")
lines(df1$year,(df1$p)/100,'l',lwd =1)
lines(df1$year,(df1$u)/100,'l',lty=2, lwd =1, xlab="Year", ylab="Debt-to-GDP Ratio",col= "red")
abline(v = 2021, col="blue", lwd=1, lty=2)

####  Forecasting avg
mean(gdp_rate$nominal_gdp_avg)
z = dr
model_avg <- arima(z, xreg = gdp_rate$nominal_gdp_avg, order = c(0, 2, 1))
summary(model_avg)
pred = predict(model_avg, newxreg = c(7680.413),n.ahead=7,ci=0.95)
p = pred$pred
predicted = p
lower_ci = p-pred$se
upper_ci = p+pred$se
table_average = data.frame(lower_ci,predicted,upper_ci)
table_average


dr = table02_07_04$debt_to_gdp_ratio*100

l = c(dr,lower_ci)
p = c(dr,predicted)
u = c(dr,upper_ci)

y <- c(2022, 2023, 2024, 2025, 2026, 2027,2028)
year1 = table02_07_04$year
year  = c(year1,y)

df2 = data.frame(year,l,p,u)
plot(df2$year,(df2$l)/100,'l',main ="Nominal GDP Average",lty=2, lwd =1, xlab="Year", ylab="Debt-to-GDP Ratio",col= "red")
lines(df2$year,(df2$p)/100,'l',lwd =1)
lines(df2$year,(df2$u)/100,'l',lty=2, lwd =1, xlab="Year", ylab="Debt-to-GDP Ratio",col= "red")
abline(v = 2021, col="blue", lwd=1, lty=2)


####  Forecasting optimistic gdp
max(gdp_rate$nominal_gdp)
z = dr
model_opt <- arima(z, xreg = gdp_rate$nominal_gdp_opt, order = c(0, 2, 1))
summary(model_opt)
pred = predict(model_opt, newxreg = c(21433.2),n.ahead=7,ci=0.95)
p = pred$pred
predicted = p
lower_ci = p-pred$se
upper_ci = p+pred$se
table_optimistic = data.frame(lower_ci,predicted,upper_ci)
table_optimistic


dr = table02_07_04$debt_to_gdp_ratio*100

l = c(dr,lower_ci)
p = c(dr,predicted)
u = c(dr,upper_ci)

y <- c(2022, 2023, 2024, 2025, 2026, 2027,2028)
year1 = table02_07_04$year
year  = c(year1,y)

df3 = data.frame(year,l,p,u)
plot(df3$year,(df3$l)/100,'l',main ="Nominal GDP Optimistic",lty=2, lwd =1, xlab="Year", ylab="Debt-to-GDP Ratio",col= "red")
lines(df3$year,(df3$p)/100,'l',lwd =1)
lines(df3$year,(df3$u)/100,'l',lty=2, lwd =1, xlab="Year", ylab="Debt-to-GDP Ratio",col= "red")
abline(v = 2021, col="blue", lwd=1, lty=2)


####  Forecasting passimistic gdp
min(gdp_rate$nominal_gdp)
z = dr
model_pass <- arima(z, xreg = gdp_rate$nominal_gdp_pass, order = c(0, 2, 1))
summary(model_pass)
pred = predict(model_pass, newxreg = c(544.3),n.ahead=7,ci=0.95)
p = pred$pred
predicted = p
lower_ci = p-pred$se
upper_ci = p+pred$se
table_passimistic = data.frame(lower_ci,predicted,upper_ci)
table_passimistic

dr = table02_07_04$debt_to_gdp_ratio*100

l = c(dr,lower_ci)
p = c(dr,predicted)
u = c(dr,upper_ci)

y <- c(2022, 2023, 2024, 2025, 2026, 2027,2028)
year1 = table02_07_04$year
year  = c(year1,y)

df4 = data.frame(year,l,p,u)
plot(df4$year,(df4$l)/100,'l',main ="Nominal GDP Passimistic",lty=2, lwd =1, xlab="Year", ylab="Debt-to-GDP Ratio",col= "red")
lines(df4$year,(df4$p)/100,'l',lwd =1)
lines(df4$year,(df4$u)/100,'l',lty=2, lwd =1, xlab="Year", ylab="Debt-to-GDP Ratio",col= "red")
abline(v = 2021, col="blue", lwd=1, lty=2)


### list of tables -------------------------
table_baseline
table_average
table_optimistic
table_passimistic

# combine plot


plot(df1$year,(df1$p)/100,'l', lwd =1, xlab="Year", ylab="Debt-to-GDP Ratio",col= "green")
lines(df2$year,df2$p/100,'l',lty=2, lwd =2, xlab="Year", ylab="Debt-to-GDP Ratio",col= "red")
lines(df3$year,df3$p/100,'l',lty=2, lwd =2, xlab="Year", ylab="Debt-to-GDP Ratio")
lines(df4$year,df4$p/100,'l', lty=1,lwd =2, xlab="Year", ylab="Debt-to-GDP Ratio",col= "black")
legend(1960, 140/100, legend=c("Baseline","Average", "Pessimistic","Optimistic" ),
       col=c("green","red","black","blue" ), lty=1:2, cex=0.8)
abline(v = 2021, col="blue", lwd=1, lty=2)
##### create data


library("writexl")
dr= data.frame(dr)
write_xlsx(dr,"/Users/Fahad/Downloads/debt_to_gdp_ratio.xlsx")
mean= mean(dr[1:61,]) # exclude 2021
min = min(dr[1:61,])
max = max(dr[1:61,])

## data with four senario

debt_to_gdp_ratio <- read_excel("Downloads/Complete_Data_Set_amansir/debt_to_gdp_ratio.xlsx")
debt_to_gdp_ratio
tail(debt_to_gdp_ratio)
dr = 100*debt_to_gdp_ratio$debt_to_gdp_ratio_avg
model <- arima(x = dr, order = c(0, 2, 1))
pred1<-forecast(model,7)
pred1$mean










