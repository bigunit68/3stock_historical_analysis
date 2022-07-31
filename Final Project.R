## Final Project
library(quantmod)

getSymbols(Symbols="V",from="2022-01-01",to="2022-03-31", periodicity = "daily")

vclose <- V$V.Close

##1

daily_ret_list <- list()
for (j in 1:length(vclose)){
  daily <- log(vclose[[j+1]]) - log(vclose[[j]])
  daily_ret_list <- append(daily_ret_list, daily)
}

counter = 0
for (ret in daily_ret_list){
  if (ret > 0){
    counter = counter + 1
  }
}
#pct_day_up_v = counter/60

##2
absrets = 0
for (j in 1:length(vclose)){
  absrets = absrets + abs(vclose[[j+1]] - vclose[[j]])
}
avg_absrets = absrets / 60

##3&4
upday = 0
updaycount = 0
downday = 0
for (j in 1:length(vclose)){
  daily <- vclose[[j+1]] - vclose[[j]]
  if(daily >= 0){
    upday = upday + daily
    updaycount = updaycount + 1
  }else{
    downday = downday + daily
  }
}

## 5
log_rets = 0
for (j in 1:length(vclose)){
  daily <- log(vclose[[j+1]]) - log(vclose[[j]])
  log_rets = log_rets + daily
}

## 6 Variance
rets_list <- list()
for (j in 1:length(vclose)){
  daily <- log(vclose[[j+1]]) - log(vclose[[j]])
  rets_list <- append(rets_list, daily)
}
rets_numeric = as.numeric(rets_list)
var(rets_numeric)
sd(rets_numeric)*sqrt(252)

## Continuous Model for V


Vsigma = 0.3500035

simV_returns <- cumsum(Vsigma*sqrt(1/252)*rnorm(20) + .0001886)
fixedV_returns = simV_returns-Vsigma^2*(1/252)/2
simV_price <- 223.95*exp(fixedV_returns) 

simV_returns <- cumsum(Vsigma*sqrt(1/252)*rnorm(20) + .0001886)
fixedV_returns = simV_returns-Vsigma^2*(1/252)/2
simV_price2 <- 223.95*exp(fixedV_returns) 

simV_returns <- cumsum(Vsigma*sqrt(1/252)*rnorm(20) + .0001886)
fixedV_returns = simV_returns-Vsigma^2*(1/252)/2
simV_price3 <- 223.95*exp(fixedV_returns) 

plot(simV_price, type="b", col="red", xlab="Day", ylab="Price", main="Visa Model", ylim=c(200,270))
lines(simV_price2, col="blue", type="b")
lines(simV_price3, col="green", type="b")
abline(h=230, col='yellow', lwd=3)





## Continuous Model for pfe

PFEsigma = 0.2834872

simPFE_returns <- cumsum(PFEsigma*sqrt(1/252)*rnorm(20) - .001287)
fixedPFE_returns = simPFE_returns-PFEsigma^2*(1/252)/2
simPFE_price <- 52.44*exp(fixedPFE_returns)

simPFE_returns <- cumsum(PFEsigma*sqrt(1/252)*rnorm(20) - .001287)
fixedPFE_returns = simPFE_returns-PFEsigma^2*(1/252)/2
simPFE2_price <- 52.44*exp(fixedPFE_returns)

simPFE_returns <- cumsum(PFEsigma*sqrt(1/252)*rnorm(20) - .001287)
fixedPFE_returns = simPFE_returns-PFEsigma^2*(1/252)/2
simPFE3_price <- 52.44*exp(fixedPFE_returns)

plot(simPFE_price, type="b", col="red", xlab="Day", ylab="Price", main="Pfizer Model", ylim=c(48,56))
lines(simPFE2_price, col="blue", type="b")
lines(simPFE3_price, col="green", type="b")
abline(h=53, col='yellow', lwd=3)


## Continuous Model for Mcdonalds

MCDsigma = 0.2080528

simMCD_returns <- cumsum(MCDsigma*sqrt(1/252)*rnorm(20) - .00126)
fixedMCD_returns = simMCD_returns-MCDsigma^2*(1/252)/2
simMCD_price <- 249.03*exp(fixedMCD_returns)

simMCD_returns <- cumsum(MCDsigma*sqrt(1/252)*rnorm(20) - .00126)
fixedMCD_returns = simMCD_returns-MCDsigma^2*(1/252)/2
simMCD_price2 <- 249.03*exp(fixedMCD_returns)

simMCD_returns <- cumsum(MCDsigma*sqrt(1/252)*rnorm(20) - .00126)
fixedMCD_returns = simMCD_returns-MCDsigma^2*(1/252)/2
simMCD_price3 <- 249.03*exp(fixedMCD_returns)

plot(simMCD_price, type="b", col="red", xlab="Day", ylab="Price", main="McDonald's Model", ylim=c(238,260))
lines(simMCD_price2, col="blue", type="b")
lines(simMCD_price3, col="green", type="b")
abline(h=255, col='yellow', lwd=3)



## Discrete Model Visa
x<-c(0:20)

vstrike <- 230
modelsV <- list()
model1priceV<- list(223.95)
counter = 223.95

for (i in 1:20){
  randy <- runif(1, min=0, max=1)
  if (randy <= .483){
    counter = counter + 3.4
  }else{
    counter = counter - 3.1
  }
  model1priceV <- append(model1priceV, counter)
}


model2priceV <- list(223.95)
counter2 = 223.95
for (i in 1:20){
  randy <- runif(1, min=0, max=1)
  if (randy <= .483){
    counter2 = counter2 + 3.4
  }else{
    counter2 = counter2 - 3.1
  }
  model2priceV <- append(model2priceV, counter2)
}

model3priceV <- list(223.95)
counter3 = 223.95
for (i in 1:20){
  randy <- runif(1, min=0, max=1)
  if (randy <= .483){
    counter3 = counter3 + 3.4
  }else{
    counter3 = counter3 - 3.1
  }
  model3priceV <- append(model3priceV, counter3)
}


plot(x,model1priceV, type='l', col="red", ylim = c(205,238), lwd=2, xlab="Day", ylab="Price", main="Visa Binomial")
lines(x, model2priceV, type='l', col="blue", lwd=2)
lines(x, model3priceV, type='l', col="green", lwd=2)
abline(h=230, col='yellow', lwd=2)



## Discrete model Pfizer

pfestrike <- 53
modelspfe <- list()
model1pricepfe<- list(52.44)
counter = 52.44

for (i in 1:20){
  randy <- runif(1, min=0, max=1)
  if (randy <= .45){
    counter = counter + .77
  }else{
    counter = counter - .75
  }
  model1pricepfe <- append(model1pricepfe, counter)
}

model2pricepfe <- list(52.44)
counter2 = 52.44
for (i in 1:20){
  randy <- runif(1, min=0, max=1)
  if (randy <= .45){
    counter2 = counter2 + .77
  }else{
    counter2 = counter2 - .75
  }
  model2pricepfe <- append(model2pricepfe, counter2)
}

model3pricepfe <- list(52.44)
counter3 = 52.44
for (i in 1:20){
  randy <- runif(1, min=0, max=1)
  if (randy <= .45){
    counter3 = counter3 + .77
  }else{
    counter3 = counter3 - .75
  }
  model3pricepfe <- append(model3pricepfe, counter3)
}



plot(x,model1pricepfe, type='l', col="red", ylim = c(46,56), lwd=2, xlab="Day", ylab="Price", main="Pfizer Binomial")
lines(x, model2pricepfe, type='l', col="blue", lwd=2)
lines(x, model3pricepfe, type='l', col="green", lwd=2)
abline(h=53, col='yellow', lwd=3)



## MCD Binomial

mcdstrike <- 255
modelsmcd <- list()
model1pricemcd<- list(249.03)
counter = 249.03

for (i in 1:20){
  randy <- runif(1, min=0, max=1)
  if (randy <= .35){
    counter = counter + 2.9
  }else{
    counter = counter - 2.06
  }
  model1pricemcd <- append(model1pricemcd, counter)
}

model2pricemcd <- list(249.03)
counter2 = 249.03
for (i in 1:20){
  randy <- runif(1, min=0, max=1)
  if (randy <= .35){
    counter2 = counter2 + 2.9
  }else{
    counter2 = counter2 - 2.06
  }
  model2pricemcd <- append(model2pricemcd, counter2)
}

model3pricemcd <- list(249.03)
counter3 = 249.03
for (i in 1:20){
  randy <- runif(1, min=0, max=1)
  if (randy <= .35){
    counter3 = counter3 + 2.9
  }else{
    counter3 = counter3 - 2.06
  }
  model3pricemcd <- append(model3pricemcd, counter3)
}



plot(x,model1pricemcd, type='l', col="red", ylim = c(235,264), lwd=2, xlab="Day", ylab="Price", main="Mcdonald's Binomial")
lines(x, model2pricemcd, type='l', col="blue", lwd=2)
lines(x, model3pricemcd, type='l', col="green", lwd=2)
abline(h=255, col='yellow', lwd=2)


##### Running each model 5000 times to find on 15th trading day,,, april 22

## Visa Cont
Vsigma = 0.3500035
 
day15V_prices <- list()
day15v_optvalue <- list()
for (return in 1:5000){
  simV_returns <- cumsum(Vsigma*sqrt(1/252)*rnorm(15) + .0001886)
  fixedV_returns = simV_returns-Vsigma^2*(1/252)/2
  simV_price <- 223.95*exp(fixedV_returns) 
  day15V_prices <- append(day15V_prices, simV_price[15])
  
  if (simV_price[15] > vstrike){
    day15v_optvalue <- append(day15v_optvalue, (simV_price[15]-vstrike))
  }else{
    day15v_optvalue <- append(day15v_optvalue, 0)
  }
  
}
day15_meanV <- mean(unlist(day15V_prices))
day15_meanvopt <- mean(unlist(day15v_optvalue))


## Pfizer Cont
PFEsigma = 0.2834872

day15pfe_optvalue <- list()
day15pfe_prices <- list()
for (returnP in 1:5000){
  simPFE_returns <- cumsum(PFEsigma*sqrt(1/252)*rnorm(15) - .001287)
  fixedPFE_returns = simPFE_returns-PFEsigma^2*(1/252)/2
  simPFE_price <- 52.44*exp(fixedPFE_returns)
  day15pfe_prices <- append(day15pfe_prices, simPFE_price[15])
  
  if (simPFE_price[15] > pfestrike){
    day15pfe_optvalue <- append(day15pfe_optvalue, (simPFE_price[15]-pfestrike))
  }else{
    day15pfe_optvalue <- append(day15pfe_optvalue, 0)
  }
}

day15_meanpfe <- mean(unlist(day15pfe_prices))
day15_meanpfeopt <- mean(unlist(day15pfe_optvalue))

## mcdonald's cont
MCDsigma = 0.2080528

day15mcd_prices <- list()
day15mcd_optvalue <- list()
for (returnm in 1:5000){
  simMCD_returns <- cumsum(MCDsigma*sqrt(1/252)*rnorm(15) - .00126)
  fixedMCD_returns = simMCD_returns-MCDsigma^2*(1/252)/2
  simMCD_price <- 249.03*exp(fixedMCD_returns)
  day15mcd_prices <- append(day15mcd_prices, simMCD_price[15])
  
  if (simMCD_price[15] > mcdstrike){
    day15mcd_optvalue <- append(day15mcd_optvalue, (simMCD_price[15]-mcdstrike))
  }else{
    day15mcd_optvalue <- append(day15mcd_optvalue, 0)
  }
}

day15_meanmcd <- mean(unlist(day15mcd_prices))
day15_meanmcdopt <- mean(unlist(day15mcd_optvalue))


## Visa BIN
vbin_prices <- list()
for (i in 1:5000){
  modelsimpriceV <- list()
  count = 223.95
  for (i in 1:15){
    randy <- runif(1, min=0, max=1)
    if (randy <= .483){
      count = count + 3.4
    }else{
      count = count - 3.1
    }
    modelsimpriceV <- append(modelsimpriceV, count)
  }
  vbin_prices <- append(vbin_prices, modelsimpriceV[15])
}

vbin_opt <- list()
for (price in vbin_prices){
  if (price > vstrike){
    vbin_opt <- append(vbin_opt, (price - vstrike))
  }else{
    vbin_opt <- append(vbin_opt, 0)
  }
}

vbin_mean <- mean(unlist(vbin_prices))
vbin_optsmean <- mean(unlist(vbin_opt))


## pfe BIN

pbin_prices <- list()
for (i in 1:5000){
  modelsimpricep <- list()
  count = 52.44
  for (i in 1:15){
    randy <- runif(1, min=0, max=1)
    if (randy <= .45){
      count = count + .77
    }else{
      count = count - .75
    }
    modelsimpricep <- append(modelsimpricep, count)
  }
  pbin_prices <- append(pbin_prices, modelsimpricep[15])
}

pbin_opt <- list()
for (price in pbin_prices){
  if (price > pfestrike){
    pbin_opt <- append(pbin_opt, (price - pfestrike))
  }else{
    pbin_opt <- append(pbin_opt, 0)
  }
}

pbin_mean <- mean(unlist(pbin_prices))
pbin_optsmean <- mean(unlist(pbin_opt))


## MCD BIN

mbin_prices <- list()
for (i in 1:5000){
  modelsimpricem <- list()
  count = 249.03
  for (i in 1:15){
    randy <- runif(1, min=0, max=1)
    if (randy <= .35){
      count = count + 2.9
    }else{
      count = count - 2.06
    }
    modelsimpricem <- append(modelsimpricem, count)
  }
  mbin_prices <- append(mbin_prices, modelsimpricem[15])
}

mbin_opt <- list()
for (price in mbin_prices){
  if (price > mcdstrike){
    mbin_opt <- append(mbin_opt, (price - mcdstrike))
  }else{
    mbin_opt <- append(mbin_opt, 0)
  }
}

mbin_mean <- mean(unlist(mbin_prices))
mbin_optsmean <- mean(unlist(mbin_opt))


#### Black Scholes

## Visa

inprice <- 223.95
k <- 230
T <- 15
r <- .026
vol <- Vsigma



w <- (log(inprice/k)+(r+vol^2/2)*(T/252))/sqrt((T/252)*(vol^2))

optionpriceV <- (inprice*pnorm(w))-k*exp(-r*T/252)*pnorm(w-vol*sqrt(T/252))
vblack <- 5.207979

## PFizer
inprice <- 52.44
k <- 53
T <- 15
r <- .026
vol <- PFEsigma



w <- (log(inprice/k)+(r+vol^2/2)*(T/252))/sqrt((T/252)*(vol^2))

optionpricepfe <- (inprice*pnorm(w))-k*exp(-r*T/252)*pnorm(w-vol*sqrt(T/252))
pblack <- 1.226715

## MCdonalds

inprice <- 249.03
k <- 255
T <- 15
r <- .026
vol <- MCDsigma



w <- (log(inprice/k)+(r+vol^2/2)*(T/252))/sqrt((T/252)*(vol^2))

optionpricemcd <- (inprice*pnorm(w))-k*exp(-r*T/252)*pnorm(w-vol*sqrt(T/252))
mblack <- 2.788431




