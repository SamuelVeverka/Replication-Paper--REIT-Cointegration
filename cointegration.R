
setwd("C:\\Users\\Samuel\\Documents\\Grad School\\835 - Time Series Econometrics\\Final Project") # Set my working directory

#libraries
library(urca)
###########Cointegration Tests


#load data sets
CPIAUCSL <- read.csv("CPIAUCSL.csv", header = TRUE)
DGS10 <- read.csv("DGS10.csv", header = TRUE)
REIT <- read.csv("reit_price_return.csv", header = TRUE, stringsAsFactors = FALSE)
QUSR <- read.csv("QUSR628BIS.csv", header = TRUE)
SP500 <- read.csv("S&P500.csv", header = TRUE)


###REITS and S&P 500

REIT_price_ts <- ts(REIT[2:301,3],start=c(1972,1),frequency=12)
REIT_return_ts <- ts(REIT[2:301,2],start=c(1972,1),frequency=12)
SP500_ts <- ts(SP500[1:300,6],start=c(1972,1),frequency=12)
EQT_REIT_ts <- ts(REIT[2:301,4],start=c(1972,1),frequency=12)
MOR_REIT_ts <- ts(REIT[2:301,5],start=c(1972,1),frequency=12)  
DGS10_ts <- ts(DGS10[1:204,2],start=c(1980,1),frequency=12)
QUSR_ts <- ts(QUSR[9:84,2],start=c(1977,4),frequency=4)

#Panel A
REIT_return_7296 <- log(REIT_return_ts)
REIT_return_7292 <- log(window(REIT_return_ts, 1972,c(1991,12)))
REIT_return_9296 <- log(window(REIT_return_ts, 1992,c(1996,12)))

REIT_price_7296 <- log(REIT_price_ts)
REIT_price_7292 <- log(window(REIT_price_ts, 1972,c(1991,12)))
REIT_price_9296 <- log(window(REIT_price_ts, 1992,c(1996,12)))

SP500_7296 <- log(SP500_ts)
SP500_7292 <- log(window(SP500_ts, 1972,c(1991,12)))
SP500_9296 <- log(window(SP500_ts, 1992,c(1996,12)))



trend <- seq_along(REIT_return_7292)
# test for cointegration with estimated cointegrating vector
coint.eq <- lm(REIT_return_7292~ SP500_7292+ trend )
coint.eq <- lm(SP500_7296~REIT_return_7296 + trend )
summary(coint.eq)
ut.est <- coint.eq$resid

# test for unit root in cointegrating residual 
ut.ur=ur.df(ut.est, type ="none", selectlags = "AIC")
summary(ut.ur)



#Panel B
EQT_REIT_7296 <- log(EQT_REIT_ts)
EQT_REIT_7292 <- log(window(EQT_REIT_ts, 1972,c(1991,12)))
EQT_REIT_9296 <- log(window(EQT_REIT_ts, 1992,c(1996,12)))

MOR_REIT_7296 <- log(MOR_REIT_ts)
MOR_REIT_7292 <- log(window(MOR_REIT_ts, 1972,c(1991,12)))
MOR_REIT_9296 <- log(window(MOR_REIT_ts, 1992,c(1996,12)))


trend <- seq_along(MOR_REIT_7292)
# test for cointegration with estimated cointegrating vector
coint.eq <- lm(MOR_REIT_7292~ SP500_7292+ trend )
summary(coint.eq)
ut.est <- coint.eq$resid

# test for unit root in cointegrating residual 
ut.ur=ur.df(ut.est, type ="none", selectlags = "AIC")
summary(ut.ur)

#Panel C
REIT_return_8096 <- log(window(REIT_return_ts, 1980,c(1996,12)))
REIT_return_8092 <- log(window(REIT_return_ts, 1980,c(1991,12)))
REIT_return_9296 <- log(window(REIT_return_ts, 1992,c(1996,12)))

EQT_REIT_8096 <- log(window(EQT_REIT_ts, 1980,c(1996,12)))
EQT_REIT_8092 <- log(window(EQT_REIT_ts, 1980,c(1991,12)))
EQT_REIT_9296 <- log(window(EQT_REIT_ts, 1992,c(1996,12)))

MOR_REIT_8096 <- log(window(MOR_REIT_ts, 1980,c(1996,12)))
MOR_REIT_8092 <- log(window(MOR_REIT_ts, 1980,c(1991,12)))
MOR_REIT_9296 <- log(window(MOR_REIT_ts, 1992,c(1996,12)))

DGS10_8096 <- log(DGS10_ts)
DGS10_8092 <- log(window(DGS10_ts, 1980,c(1991,12)))
DGS10_9296 <- log(window(DGS10_ts, 1992,c(1996,12)))


trend <- seq_along(MOR_REIT_8096)
# test for cointegration with estimated cointegrating vector
coint.eq <- lm(MOR_REIT_8096 ~ DGS10_8096+ trend )
summary(coint.eq)
ut.est <- coint.eq$resid

# test for unit root in cointegrating residual 
ut.ur=ur.df(ut.est, type ="none", selectlags = "AIC")
summary(ut.ur)




####Table 4 - Error Correction Models
REIT_return_7796 <- log(window(REIT_return_ts, c(1977,10),c(1996,9)))
EQT_REIT_7796 <- log(window(EQT_REIT_ts,  c(1977,10),c(1996,9)))
MOR_REIT_7796 <- log(window(MOR_REIT_ts,  c(1977,10),c(1996,9)))
QUSR_7796 <- log(QUSR_ts)

qtr_REIT_return_7796 <- aggregate(REIT_return_7796, nfrequency=4,mean)
qtr_EQT_REIT_7796 <- aggregate(EQT_REIT_7796, nfrequency=4,mean)
qtr_MOR_REIT_7796 <- aggregate(MOR_REIT_7796, nfrequency=4,mean)

length(QUSR_7796)


# test for cointegration with estimated cointegrating vector
trend12 <- seq_along(qtr_REIT_return_7796)

coint.eq12 <- lm(qtr_REIT_return_7796 ~ QUSR_7796 + trend12 )
summary(coint.eq12)
ut.est12 <- coint.eq12$resid

# test for unit root in cointegrating residual 
ut.ur12=ur.df(ut.est12, type ="none", selectlags = "AIC")
summary(ut.ur12)


# test for cointegration with estimated cointegrating vector
trend34 <- seq_along(qtr_EQT_REIT_7796)

coint.eq34 <- lm(qtr_EQT_REIT_7796 ~ QUSR_7796 + trend34 )
summary(coint.eq34)
ut.est34 <- coint.eq34$resid

# test for unit root in cointegrating residual 
ut.ur34=ur.df(ut.est34, type ="none", selectlags = "AIC")
summary(ut.ur34)


# test for cointegration with estimated cointegrating vector
trend56 <- seq_along(qtr_MOR_REIT_7796)

coint.eq56 <- lm(qtr_MOR_REIT_7796 ~ QUSR_7796 + trend56 )
summary(coint.eq56)
ut.est56 <- coint.eq56$resid

# test for unit root in cointegrating residual 
ut.ur56=ur.df(ut.est56, type ="none", selectlags = "AIC")
summary(ut.ur56)





###VECM All REIT QUSR
#library(dynlm)
vardata1 <- ts(cbind(qtr_REIT_return_7796,QUSR_7796),start=c(1977,4),frequency=4)
info.crit <- VARselect(vardata1,lag.max=4,type="const")
info.crit




#ECM models 1&2
data12 <- cbind(qtr_REIT_return_7796,QUSR_7796)
test12 <- ca.jo(data12,ecdet="trend",type="trace",K=2,spec="transitory")
summary(test12)
model12 <- cajorls(test12,r=1) 
summary(model12$rlm) 
 
#ECM models 3&4
data34 <- cbind(qtr_EQT_REIT_7796 ,QUSR_7796)
test34 <- ca.jo(data34,ecdet="trend",type="trace",K=2,spec="transitory")
summary(test34)
model34 <- cajorls(test34,r=1) 
summary(model34$rlm) 

#ECM models 5&6
data56 <- cbind(qtr_MOR_REIT_7796 ,QUSR_7796)
test56 <- ca.jo(data56,ecdet="trend",type="trace",K=2,spec="transitory")
summary(test56)
model56 <- cajorls(test56,r=1) 
summary(model56$rlm) 






####Table 5 - Error Correction Models - REITS and CPI
CPIAUCSL_ts <- ts(CPIAUCSL[301:600,2],start=c(1972,1),frequency=12)


REIT_return_7296 <- log(REIT_return_ts)
REIT_return_7292 <- log(window(REIT_return_ts, 1972,c(1991,12)))
REIT_return_9296 <- log(window(REIT_return_ts, 1992,c(1996,12)))

EQT_REIT_7296 <- log(EQT_REIT_ts)
EQT_REIT_7292 <- log(window(EQT_REIT_ts, 1972,c(1991,12)))
EQT_REIT_9296 <- log(window(EQT_REIT_ts, 1992,c(1996,12)))

MOR_REIT_7296 <- log(MOR_REIT_ts)
MOR_REIT_7292 <- log(window(MOR_REIT_ts, 1972,c(1991,12)))
MOR_REIT_9296 <- log(window(MOR_REIT_ts, 1992,c(1996,12)))

CPIAUCSL_7296 <- log(CPIAUCSL_ts)
CPIAUCSL_7292 <- log(window(CPIAUCSL_ts, 1972,c(1991,12)))
CPIAUCSL_9296 <- log(window(CPIAUCSL_ts, 1992,c(1996,12)))


# test for cointegration with estimated cointegrating vector
trend12 <- seq_along(REIT_return_9296)

coint.eq12 <- lm(REIT_return_9296 ~ CPIAUCSL_9296 + trend12 )
summary(coint.eq12)
ut.est12 <- coint.eq12$resid

# test for unit root in cointegrating residual 
ut.ur12=ur.df(ut.est12, type ="none", selectlags = "AIC")
summary(ut.ur12)


# test for cointegration with estimated cointegrating vector
trend34 <- seq_along(EQT_REIT_9296)

coint.eq34 <- lm(EQT_REIT_9296 ~ CPIAUCSL_9296 + trend34 )
summary(coint.eq34)
ut.est34 <- coint.eq34$resid

# test for unit root in cointegrating residual 
ut.ur34=ur.df(ut.est34, type ="none", selectlags = "AIC")
summary(ut.ur34)


# test for cointegration with estimated cointegrating vector
trend56 <- seq_along(MOR_REIT_9296)

coint.eq56 <- lm(MOR_REIT_9296 ~ CPIAUCSL_9296 + trend56 )
summary(coint.eq56)
ut.est56 <- coint.eq56$resid

# test for unit root in cointegrating residual 
ut.ur56=ur.df(ut.est56, type ="none", selectlags = "AIC")
summary(ut.ur56)



##Full Period 1/72-12/96
#ECM models 1&2
data12 <- cbind(REIT_return_7296,CPIAUCSL_7296)
test12 <- ca.jo(data12,ecdet="trend",type="trace",K=2,spec="transitory")
summary(test12)
model12 <- cajorls(test12,r=1) 
summary(model12$rlm) 

#ECM models 3&4
data34 <- cbind(EQT_REIT_7296 ,CPIAUCSL_7296)
test34 <- ca.jo(data34,ecdet="trend",type="trace",K=2,spec="transitory")
summary(test34)
model34 <- cajorls(test34,r=1) 
summary(model34$rlm) 

#ECM models 5&6
data56 <- cbind(MOR_REIT_7296 ,CPIAUCSL_7296)
test56 <- ca.jo(data56,ecdet="trend",type="trace",K=2,spec="transitory")
summary(test56)
model56 <- cajorls(test56,r=1) 
summary(model56$rlm) 



##First Period 1/72-12/92
#ECM models 1&2
data12 <- cbind(REIT_return_7292,CPIAUCSL_7292)
test12 <- ca.jo(data12,ecdet="trend",type="trace",K=2,spec="transitory")
summary(test12)
model12 <- cajorls(test12,r=1) 
summary(model12$rlm) 

#ECM models 3&4
data34 <- cbind(EQT_REIT_7292 ,CPIAUCSL_7292)
test34 <- ca.jo(data34,ecdet="trend",type="trace",K=2,spec="transitory")
summary(test34)
model34 <- cajorls(test34,r=1) 
summary(model34$rlm) 

#ECM models 5&6
data56 <- cbind(MOR_REIT_7292 ,CPIAUCSL_7292)
test56 <- ca.jo(data56,ecdet="trend",type="trace",K=2,spec="transitory")
summary(test56)
model56 <- cajorls(test56,r=1) 
summary(model56$rlm) 

##Second Period 1/92-12/96
#ECM models 1&2
data12 <- cbind(REIT_return_9296,CPIAUCSL_9296)
test12 <- ca.jo(data12,ecdet="trend",type="trace",K=2,spec="transitory")
summary(test12)
model12 <- cajorls(test12,r=1) 
summary(model12$rlm) 

#ECM models 3&4
data34 <- cbind(EQT_REIT_9296 ,CPIAUCSL_9296)
test34 <- ca.jo(data34,ecdet="trend",type="trace",K=4,spec="transitory")
summary(test34)
model34 <- cajorls(test34,r=1) 
summary(model34$rlm) 

#ECM models 5&6
data56 <- cbind(MOR_REIT_9296 ,CPIAUCSL_9296)
test56 <- ca.jo(data56,ecdet="trend",type="trace",K=2,spec="transitory")
summary(test56)
model56 <- cajorls(test56,r=1) 
summary(model56$rlm) 



####Table 6 - Cointegration Tests - Equity REIT Index with Mortgage REIT Index
EQT_REIT_price_ts <- ts(REIT[2:301,6],start=c(1972,1),frequency=12)
MOR_REIT_price_ts <- ts(REIT[2:301,7],start=c(1972,1),frequency=12)  


EQT_REIT_7296 <- log(EQT_REIT_ts)
EQT_REIT_7292 <- log(window(EQT_REIT_ts, 1972,c(1991,12)))
EQT_REIT_9296 <- log(window(EQT_REIT_ts, 1992,c(1996,12)))


MOR_REIT_7296 <- log(MOR_REIT_ts)
MOR_REIT_7292 <- log(window(MOR_REIT_ts, 1972,c(1991,12)))
MOR_REIT_9296 <- log(window(MOR_REIT_ts, 1992,c(1996,12)))

EQT_REIT_price_7296 <- log(EQT_REIT_price_ts)
EQT_REIT_price_7292 <- log(window(EQT_REIT_price_ts, 1972,c(1991,12)))
EQT_REIT_price_9296 <- log(window(EQT_REIT_price_ts, 1992,c(1996,12)))


MOR_REIT_price_7296 <- log(MOR_REIT_price_ts)
MOR_REIT_price_7292 <- log(window(MOR_REIT_price_ts, 1972,c(1991,12)))
MOR_REIT_price_9296 <- log(window(MOR_REIT_price_ts, 1992,c(1996,12)))



# test for cointegration with estimated cointegrating vector
trend12 <- seq_along(EQT_REIT_price_7296)

coint.eq12 <- lm(EQT_REIT_price_7296 ~ MOR_REIT_price_7296 + trend12 )
summary(coint.eq12)
ut.est12 <- coint.eq12$resid

# test for unit root in cointegrating residual 
ut.ur12=ur.df(ut.est12, type ="none", selectlags = "AIC")
summary(ut.ur12)
 

# test for cointegration with estimated cointegrating vector
trend34 <- seq_along(EQT_REIT_price_7292)

coint.eq34 <- lm(EQT_REIT_price_7292 ~ MOR_REIT_price_7292 + trend34 )
summary(coint.eq34)
ut.est34 <- coint.eq34$resid

# test for unit root in cointegrating residual 
ut.ur34=ur.df(ut.est34, type ="none", selectlags = "AIC")
summary(ut.ur34)


# test for cointegration with estimated cointegrating vector
trend56 <- seq_along(EQT_REIT_price_9296)

coint.eq56 <- lm(EQT_REIT_price_9296 ~ MOR_REIT_price_9296 + trend56 )
summary(coint.eq56)
ut.est56 <- coint.eq56$resid

# test for unit root in cointegrating residual 
ut.ur56=ur.df(ut.est56, type ="none", selectlags = "AIC")
summary(ut.ur56)



##Full Period 1/72-12/96
#ECM models 1&2
data12 <- cbind(EQT_REIT_7296,MOR_REIT_7296)
test12 <- ca.jo(data12,ecdet="trend",type="trace",K=2,spec="transitory")
summary(test12)
model12 <- cajorls(test12,r=1) 
summary(model12$rlm) 


data12 <- cbind(EQT_REIT_price_7296,EQT_REIT_price_7296)
test12 <- ca.jo(data12,ecdet="trend",type="trace",K=2,spec="transitory")
summary(test12)
model12 <- cajorls(test12,r=1) 
summary(model12$rlm) 



#ECM models 3&4
data34 <- cbind(EQT_REIT_7292 ,MOR_REIT_7292)
test34 <- ca.jo(data34,ecdet="trend",type="trace",K=2,spec="transitory")
summary(test34)
model34 <- cajorls(test34,r=1) 
summary(model34$rlm) 

data34 <- cbind(EQT_REIT_price_7292 ,MOR_REIT_price_7292)
test34 <- ca.jo(data34,ecdet="trend",type="trace",K=2,spec="transitory")
summary(test34)
model34 <- cajorls(test34,r=1) 
summary(model34$rlm) 

#ECM models 5&6
data56 <- cbind(EQT_REIT_9296 ,MOR_REIT_9296)
test56 <- ca.jo(data56,ecdet="trend",type="trace",K=2,spec="transitory")
summary(test56)
model56 <- cajorls(test56,r=1) 
summary(model56$rlm) 

data56 <- cbind(EQT_REIT_price_9296 ,MOR_REIT_price_9296)
test56 <- ca.jo(data56,ecdet="trend",type="trace",K=2,spec="transitory")
summary(test56)
model56 <- cajorls(test56,r=1) 
summary(model56$rlm) 

















 
#d_qtr_REIT_return_7796 <- na.omit(diff(qtr_REIT_return_7796))
#d_QUSR_7796 <- na.omit(diff(QUSR_7796))
#ect.1 <- ut.est[2:(length(ut.est)-1)]



#d_qtr_REIT_return_7796_t <- d_qtr_REIT_return_7796[2:(length(d_qtr_REIT_return_7796))]
#d_qtr_REIT_return_7796_l <- d_qtr_REIT_return_7796[1:(length(d_qtr_REIT_return_7796)-1)]

#d_QUSR_7796_t <- d_QUSR_7796[2:(length(d_QUSR_7796))]
#d_QUSR_7796_l <- d_QUSR_7796[1:(length(d_QUSR_7796)-1)]

#vecm.1=dynlm(formula = d_qtr_REIT_return_7796_t ~ L(d_qtr_REIT_return_7796_t,1) + L(d_qtr_REIT_return_7796_t,2)
#                                              + L(d_QUSR_7796_t,1) + L(d_QUSR_7796_t,2) + ect.1)
#summary(vecm.1)


#vecm.2 <- dynlm(formula = d_QUSR_7796_t ~ L(d_qtr_REIT_return_7796_t,1) + L(d_qtr_REIT_return_7796_t,2)
#            + L(d_QUSR_7796_t,1) + L(d_QUSR_7796_t,2) + ect.1)
#summary(vecm.2) 
 
# xtr <- as.data.frame(c(qtr_REIT_return_7796_df,QUSR_7796_df ))
# xeq <- as.data.frame(c(qtr_REIT_return_7796_df,QUSR_7796_df,ut.est_df ))
#library(ecm)
#summary(ecm(xeq[,"x"], xtr, xeq))
#names(xeq)
#qtr_REIT_return_7796_df <- as.data.frame(qtr_REIT_return_7796)
#QUSR_7796_df <- as.data.frame(QUSR_7796)
#ut.est_df <- as.data.frame(ut.est)
#str(ut.est_df)
#nrow(qtr_REIT_return_7796_df)


