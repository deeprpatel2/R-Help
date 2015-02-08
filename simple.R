

library(forecast)
library(fpp)


# reading data 

stock = read.csv("./input/A.csv", sep=",", header=TRUE)
# demo<- read.csv(file.choose());

# "coverting to month" is not needed when you download monthly data

#  stock$date = as.date(stock$Date)
# stock_monthly =aggregate(stock$close , bylist(date= format(stock$date ,"%Y%m")),mean)

# head of data (sample)
head(stock)

#converting to time series

tsstock <- ts(stock$close, start=c(2001,1), frequency=12)


tsstock
plot(tsstock)


# generalize function as Polynomial trend

t1 = seq(2000, 2015 , length = length(tsstock))

t12 = t1^7

polystock = lm(tsstock ~ t1 + t12 )
tsstocktrend1 = ts (polystock$fit , start =c (2000,1), frequency = 12 )

plot (tsstock , lw = 2 , col="blue" , mlim=c(2000,2015) )

lines(tsstocktrend1,lw=2,col="red")
abline(v=2015.25,lty=3)



# decompose a time series in Seasonal , Trend and irregular  components 
# it will work as second genaralized (trend) function.

stlstock = stl (tsstock , s.window= "periodic")
plot (stlstock , lw = 2 , col="blue"  )
tsstocktrend2 = stlstock$time.series[,2]
plot(forecast(stlstock))
abline(v=2015.25,lty=3)


plot (tsstock , lw = 3  )
lines(tsstocktrend1,col="purple",lw=2)
lines(tsstocktrend1,col="red",lw=2)
abline(v=2015.25,lty=3)

legend("bottomleft",legend=c("Actual function", "stl trend " ,"polynomial trend"),
       col =  c("black, red, purple"))




# Let's Begin the Prediction 

# 1. based on  polynomial function 

# Holt Winter Filtering 
HWStock1_ng = HoltWinters(tsstocktrend1,gamma=FALSE)
HWStock1 = HoltWinters(tsstocktrend1)
# Neural Networks
NETfit1 <- nnetar(tsstocktrend1)
# ARIMA model (some time errors)
autofit1 = auto.arima(tsstocktrend1)
fit12 <- arima(tsstocktrend1, order=c(1,0,0),list(order=c(2,1,0), period=12))
#Linear Model
fitl1 <- tslm(tsStocktrend1 ~ trend + season, lambda=0)
#STL Model
stlStock1 = stl(tsStocktrend1,s.window="periodic")

# ----------- plot graphs ------------------

plot(forecast(autofit1,h=24), xlim=c(2000,2017.25),ylim=c(50,100 ),lw=2 ,col="red" ,
     xlab="time" , ylab = "stock price" , main = "Prediction of the polynomial trend" )

lines(forecast(stlstock1,h=24)$mean, col="red" , lw =2)
lines(tsstock, lw=2)
lines(forecast(fitl1,h=24)$mean, col="orange")
lines(forecast(Netfit1,h=24)$mean,lw=3 ,lty= "longdash" col="brown")
lines(predict(HWstock1_ng , n.ahead= 24), lw = 2  , col = "green")

lines(forecast(fit12,h=24)$mean, col="Purple" , lw =2)

lines(predict(HWstock1 , n.ahead= 24, prediction.interval = T , level = 0.95),[.1],lw=2 ,col="green" )
lines(predict(HWstock1 , n.ahead= 24, prediction.interval = T , level = 0.95),[.2],lw=2 ,col="green" )
lines(predict(HWstock1 , n.ahead= 24, prediction.interval = T , level = 0.95),[.3],lw=2 ,col="green" )

legend("bottomleft" ,  legend=c("Actual function" ,"polynomial trend" , "holt" ,"arima- auto", "arima fixed"
                                "Nueral network",  "linear model"), col = "black" , "red","green" ,  "blue" ,"purple"
                                "brown" ,"orange"  )

abline(v=2015.25 ,lty =3)




# repeat model and plot part for all genealization of data

# 2. based on actual function 
# 3. Based on STL trend


















