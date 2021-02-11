CEU<-read.csv("CEU2000000001.csv")
em<-CEU[,2]


plot(ts(em,start=c(1939,1),freq=12),
     ylab="",xlab="", main="US Employment of Construction")
#########
yy<-log(em)
TT<-length(yy)
t<-1:TT

t1<-1:TT
t2<-t1^2
reg<-lm(yy~t1+t2)

trend<-fitted.values(reg)

detrend<-residuals(reg)

mi<-min(yy,trend)
mx<-max(yy,trend)

plot(ts(detrend,start=c(1939,1),freq=12),
     ylab="",xlab="",main="Detrended US Employment of Construction")
 



#######################
dr<-diff(detrend,lag=1,differences=1)

plot(ts(dr,start=c(1939,2),freq=12),
     ylab="",xlab="", main="% Change in US Employment of Construction")

acf.ffr<-list()
par(mfrow=c(2,1))

acf.ffr$r <- list('acf' = acf(dr, lag.max =
                                72,main="ACF for CEU"),
                  'pacf' = pacf(dr, lag.max = 
                                  72,main="PACF for CEU"))

##################################
sde<-diff(dr,lag=12,differences=1)

plot(ts(sde,start=c(1939,2),freq=12),
     ylab="",xlab="", main="Seasonally Differenced Employment")

acf.ffs<-list()
par(mfrow=c(2,1))

acf.ffs$r <- list('acf' = acf(sde, lag.max =
                                72,main="Seasonally Differenced Employment ACF"),
                  'pacf' = pacf(sde, lag.max = 
                                  72,main="Seasonally Differenced Employment PACF"))
################################

sar1 <- arima(dr,order=c(1,0,1),seasonal=list(order=c(1,0,1),period=12))

res <- residuals(sar1)

plot(ts(res,start=c(1939,2),freq=12),
     ylab="",xlab="",main="Seasonally Differenced Percent Change in Employment")

par(mfrow=c(2,1))
acf(res,lag.max=72,main="ACF ARMA(1,1)-SARMA(1,1) Filtered Residuals")
pacf(res,lag.max=72,main="PACF ARMA(1,1)-SARMA(1,1) Filtered Residuals")
##################################
#### ARMA(0,0,0)-SARMA(1,0,1)

sar1 <- arima(dr,order=c(0,0,0),seasonal=list(order=c(1,0,1),period=12))





res <- residuals(sar1)
plot(ts(res,start=c(1939,2),freq=12),main="Seasonally adjusted Employment by SARMA(1,1)",ylab=" ",xlab="")

par(mfrow=c(2,1))
acf(res,lag.max=72,main=" ACF ARMA(0,0)-SARMA(1,1) Filtered Residuals")
pacf(res,lag.max=72,main=" PACF ARMA(0,0)-SARMA(1,1) Filtered Residuals")

##################################
y <- ts(dr,frequency=12,start=c(1939,2))

library(forecast)
DD <- seasonaldummy(y)
dimnames(DD)[[1]] <- CEU[-1,1]

#DUMMY <- cbind(DD, 1-rowSums(DD) )
#dimnames(DUMMY)[[2]][4] <- "Q4"

reg <- lm(y~DD)

yres <- residuals(reg)

par(mfrow=c(2,1))
acf(yres,lag.max=72,main="ACF Residuals Filtered by Dummy")
pacf(yres,lag.max=72, main="PACF Residuals Filtered by Dummy")


plot(ts(yres,start=c(1939,2),freq=12),main="Seasonally adjusted CEU Growth by Dummy")
summary(reg)

#################################

###Problem ##2

###########################
load("EXData.Rdata")
Mer<-EXData
head(Mer)
tail(Mer)
names(Mer)

TT<-243

plot(ts(Mer$USGBP,start=c(1999,1),freq=12),col="red",
     ylab="",xlab="",main="US Pound Exchange Rate")
plot(ts(Mer$USEURO,start=c(1999,1),freq=12),col="blue",
     ylab="",xlab="",main="US Euro Exchange Rate")
plot(ts(Mer$TWEXUS,start=c(1999,1),freq=12),col="purple",
     ylab="",xlab="",main="Trade Weighted US Dollar Index")

par(mfrow=c(2,1))
acf(Mer$USGBP,lag.max=36,main=
      "ACF USGBP")
pacf(Mer$USGBP,lag.max=36,main=
       "PACF USGBP")
acf(Mer$USEURO,lag.max=36,main=
      "ACF USEURO")
pacf(Mer$USEURO,lag.max=36,main=
       "PACF USEURO")
acf(Mer$TWEXUS,lag.max=36,main=
      "ACF TWEXUS")
pacf(Mer$TWEXUS,lag.max=36,main=
       "PACF TWEXUS")
################

library(fUnitRoots)

adfTest(Mer$USGBP,type="c",lags=2)

adfTest(Mer$USEURO,type="c",lags=2)

adfTest(Mer$TWEXUS,type="c",lags=2)
######################


Usgbp<-log(Mer$USGBP[2:TT]/Mer$USGBP[1:(TT-1)])*100
Useuro<-log(Mer$USEURO[2:TT]/Mer$USEURO[1:(TT-1)])*100
Twexus<-log(Mer$TWEXUS[2:TT]/Mer$TWEXUS[1:(TT-1)])*100


library(tseries)

adf.test(Usgbp,alternative="stationary")
adf.test(Useuro,alternative="stationary")
adf.test(Twexus,alternative="stationary")
###########################
#######Split the Sample
TT<-dim(Mer)[1]

y <- Mer[,c("USGBP","USEURO")] 
colMeans(y)

loc1 <- which(Mer[,1]=="12/1/2006")


USGBPest.period<-Usgbp[1:loc1]
USGBPfore.period<-Usgbp[(loc1+1): length(Usgbp)]

USEUROest.period<-Useuro[1:loc1]
USEUROfore.period<-Useuro[(loc1+1): length(Useuro)]

TWEXUSest.period<-Twexus[1:loc1]
TWEXUSfore.period<-Twexus[(loc1+1): length(Twexus)]

Bound<-cbind(USGBPest.period,USEUROest.period)



#####comment out
estp <- y[2:loc1,]
forep <- y[(loc1+1):TT,] 

usg <- matrix(Mer$TWEXUS,TT,1,dimnames=list(c(),"TWEXUS")) 
usp <- matrix(usg[1:(loc1-1)] ,loc1-1,1,dimnames=list(c(),"TWEXUS"))  

################VAR Order Selection
library(vars)


vse <- VARselect(Bound,lag.max=12,type="const",exogen=TWEXUSest.period)
vse
write.csv(t(vse$criteria),file="VAROrderSelection.csv")

va1 <- VAR(Bound,p=1,type="const",exogen=TWEXUSest.period )
Bcoef(va1)
summary(va1)


#############################Granger-Causality Tests
causality(va1,cause=c("USGBPest.period" ))
causality(va1,cause=c( "USEUROest.period" ))




va2 <- VAR(estp,p=2,type="const",exogen=usp  )
Bcoef(va2)
summary(va2)

causality(va2,cause=c("USGBPest.period" ))
causality(va2,cause=c( "USEUROest.period" ))


##################################################
#################### Rolling forecasts VAR(1)

y<-cbind(Usgbp,Useuro)



fp <- loc2-loc1 ## number of forecast periods


estp <- y[2:loc1,]
forep <- y[(loc1+1):TT,] 

usg <- matrix(Twexus,TT,1,dimnames=list(c(),"TWEXUS")) 
usp <- matrix(usg[1:(loc1-1)] ,loc1-1,1,dimnames=list(c(),"TWEXUS")) 




Postforecast <- list()

for (model in 1:1) {
  Postforecast[[model]] <-array(0,c(fp,4,2),
                                dimnames=list(c(),c("fcst","low","upper","CI"),c("USGBP","USEURO")))
  
  for (i in 1:fp) {
    ys <- y[(i+1):(loc1+i-1),]
    xs <- matrix(Twexus[i:(loc1-1+i-1)],,1,
                 dimnames=list(c(),"TWEXUS"))
    
    va1 <- VAR(ys,p=model,type="const",exogen=xs ) 
    pred <- predict(va1,n.ahead=1,ci=0.95,dumvar=as.matrix(Twexus[ loc1-1+i],,1))
    Postforecast[[model]][i,,1] <- pred$fcst$Usgbp
    Postforecast[[model]][i,,2] <- pred$fcst$Useuro
  }
}





#####################RMSFE and MAFE
PostR <- matrix(0,4,1,dimnames=list(c("RMSFE_USGBP","RMSFE_USEURO",
                                      "MAFE_USGBP","MAFE_USEURO"),
                                    c("VAR(1)")))
for (model in 1:2) {
  for (i in 1:2) {
    PostR[i,model] <- sqrt(mean(  (y[(loc1+1):(TT-1),1]-Postforecast[[model]][,1,i])^2) )
    PostR[i+2,model] <-  mean(  abs(y[(loc1+1):(TT-1),1]-Postforecast[[model]][,1,i]) )  
  }
}
##########################################Part D
AR_Postforecast <- list()

for (model in 1:1) {
  AR_Postforecast[[model]] <-array(0,c(fp,3,2),
                                   dimnames=list(c(),c("forecast","low","upper"),
                                                 c("Tsgbp","Useuro")))
  for (j in 1:2) { 
    print(paste(model,j))
    for (i in 1:fp) {
      ys <- y[(i+1):(loc1+i-1),j]
      xs <- matrix(Twexus[i:(loc1-1+i-1)],,1,
                   dimnames=list(c(),"twexus"))
      
      va1 <- arima(ys,order=c(1,0,0), 
                   xreg=xs ) 
      fc <- predict(va1,n.ahead=1, newxreg=as.matrix(Twexus[loc1-1+i],,1)) 
      pred <- fc$pred
      se <- fc$se
      
      AR_Postforecast[[model]][i,,j] <- c(pred, pred-cr*se,pred+cr*se)
      
    }
  }
}
########RMSFE and MAFE
AR_PostR <- matrix(0,4,1,dimnames=list(c("RMSFE_USGBP","RMSFE_USEURO",
                                         "MAFE_USGBP","MAFE_USEURO"),
                                       c("AR(1)")))
for (model in 1:1) {
  for (i in 1:2) {
    AR_PostR[i,model] <- sqrt(mean(  (y[(loc1+1):(loc2-1),1]-AR_Postforecast[[model]][,1,i])^2) )
    AR_PostR[i+2,model] <-  mean(  abs(y[(loc1+1):(loc2-1),1]-AR_Postforecast[[model]][,1,i]) )  
  }
}
PostResult <- cbind(PostR,AR_PostR)
round(PostResult,3)

write.csv(PostResult,file="PostResult.csv")




######################################## Part F
mi <- min(y[,1],Postforecast[[1]][,1:3,1])
mx <- max(y[,1],Postforecast[[1]][,1:3,1])
plot(ts(y[,1],start=c(1999,1),freq=12),type="n",ylab="",xlab="",ylim=c(mi,mx),
     main="Forecast for Europe Data AR(1)")
lines(ts(y[,1],start=c(1999,1),freq=12))
lines(ts(AR_Postforecast[[1]][,1,1],start=c(2007,1),freq=12),col="blue",lwd=1)
lines(ts(AR_Postforecast[[1]][,2,1],start=c(2007,1),freq=12),col="red",lty=2)
lines(ts(AR_Postforecast[[1]][,3,1],start=c(2007,1),freq=12),col="red",lty=2)

legend("topleft",legend=c(expression(y[t]),
                                      expression(hat(y)[t]^"post"),
                                      "95% Intervals"),
                           col=c("red","blue","black","black"),lty=c(2,2,3,3))



















