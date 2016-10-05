check_packages = function(names)
{
  for(name in names)
  {
    if (!(name %in% installed.packages()))
      install.packages(name, repos="http://cran.us.r-project.org")
    
    library(name, character.only=TRUE)
  }
}

check_packages(c("stats","devtools","utils","lubridate","dplyr","tseries","neuralnet","nnet","forecast"))


data = read.csv('/Users/luohaosheng/Desktop/Portfolio/Electricity_Price/australia-victoria-energy.csv')

data = dplyr::select(data,SETTLEMENTDATE,TOTALDEMAND,RRP)%>%
  mutate(YEAR=year(SETTLEMENTDATE),MONTH=month(SETTLEMENTDATE),DAY=day(SETTLEMENTDATE),
         HOUR =hour(SETTLEMENTDATE),MINUTE = minute(SETTLEMENTDATE),
         TIME = paste(HOUR, MINUTE),WDAY=wday(SETTLEMENTDATE,label=T),WDAY2=0.1)
data[data$WDAY%in%c("Sat","Sun"),"WDAY2"]=0
year = transmute(data, YEAR=year(SETTLEMENTDATE))
n = nrow(data)
#(paste("Number of all the datapoints:", n))
#(paste("Number of the datapoint in 2015:", nrow(data[year$YEAR!=2014,])))
#(paste("Number of duplicated data:", nrow(data[duplicated(data$SETTLEMENTDATE),])))
#(paste("Number of datapints that contain missing values:", nrow(data[!complete.cases(data),])))


datetime = seq(from=as.POSIXct("2014-1-1 0:30"), to=as.POSIXct("2015-1-1 0:00"),
               by="30 mins") 
plot(datetime, data$RRP,type="l",xlab="Date", ylab="RRP ($/MWh)",xaxs='i')
plot(datetime, data$TOTALDEMAND,type="l",xlab="Date",ylab="Total Demand (MW)", xaxs='i')


datetime = seq(from=as.POSIXct("2014-4-1 0:00"), to=as.POSIXct("2014-4-30 23:30"),
               by="30 mins")  
plot(datetime,data[data$MONTH==4,"RRP"], type="l", xaxs='i', xlab="Date", 
     ylab="RRP ($/MWh)")
plot(datetime,data[data$MONTH==4,"TOTALDEMAND"], type="l", xaxs='i', xlab="Date", 
     ylab="Total Demand (MW)")

datetime = seq(from=as.POSIXct("2014-4-5 0:00"), to=as.POSIXct("2014-4-13 23:30"),
               by="30 mins")  
plot(datetime, data[(data$MONTH==4)&(data$DAY>4)&(data$DAY<14),"RRP"], type="l",  
     xaxs='i', xlab="Date",ylab="RRP ($/MWh)", 
     main="The Time-series of RRP during April 5th - 13th")
plot(datetime, data[(data$MONTH==4)&(data$DAY>4)&(data$DAY<14),"TOTALDEMAND"], 
     type="l", xaxs='i', xlab="Date",ylab="Total Demand (MW)", 
     main="The Time-series of Total Demand during April 5th - 13th")


# generate public holiday database
Holiday = as.data.frame(matrix(c(1,1,1,27,3,10,4,18,4,21,4,25,6,9,11,4,12,25,12,26), 
                               nrow = 10, ncol = 2, byrow=T))
colnames(Holiday) = c("MONTH","DAY")
Holiday$holiday = 0.1

#smooth Jan 14-17, truncate other spikes 
data2 = data
hot_index = rownames(data[data$MONTH==1&data$DAY%in%14:17,])
normal_wday = data[((data$MONTH!=1)|(!data$DAY%in%14:17))&data$WDAY2==0.1,]
normal_wday = group_by(normal_wday,HOUR,MINUTE)
normal_wday = as.data.frame(summarise(normal_wday,TOTALDEMAND=mean(TOTALDEMAND),RRP=mean(RRP)))
normal_wday = rbind(normal_wday,normal_wday,normal_wday,normal_wday)
data2[hot_index,c("TOTALDEMAND","RRP")]=normal_wday[c("TOTALDEMAND","RRP")]
data2[data2$RRP<0,"RRP"]=0.01
data2[data2$RRP>80,"RRP"]=80

#stationariality of RRP and Totaldemand
adf.test(data2$RRP, "explosive")
adf.test(data2$TOTALDEMAND, "explosive")


plot(data2$RRP, data2$TOTALDEMAND, type = "p", pch=20, xlab="RRP ($/MWh)",
     ylab="Total Demand (MW)") 

#,main="The Correlation Between RRP and Total Demand after Cleaning"


data2[2:n,"LCRRP"] = log(data2$RRP[2:n])-log(data2$RRP[1:(n-1)])
data2[2:n,"LCTOTD"] = log(data2$TOTALDEMAND[2:n])-log(data2$TOTALDEMAND[1:(n-1)])

#merge data2 with the holiday database
data2 = merge(x = data2, y = Holiday, by = c("MONTH","DAY"), all.x = TRUE) #create holiday
data2[is.na(data2$holiday),"holiday"] = 0
data2 = arrange(data2,YEAR,MONTH,DAY,HOUR,MINUTE)

data2[,c("L1LCTOTD","L2LCTOTD","L3LCTOTD","L4LCTOTD","L336LCTOTD","L337LCTOTD",
         "L338LCTOTD","L339LCTOTD","L340LCTOTD")]=NA 
col = which(colnames(data2)=="L1LCTOTD")
col_LCTOTD = which(colnames(data2)=="LCTOTD")
for(j in c(1:4,336:340)){
  i=j+1
  data2[i:n,col]=data2[1:(n-j),col_LCTOTD]
  col=col+1
}

data2[,c("L1LCRRP","L2LCRRP","L3LCRRP","L4LCRRP","L336LCRRP","L337LCRRP",
         "L338LCRRP","L339LCRRP","L340LCRRP")]=NA 
col = which(colnames(data2)=="L1LCRRP")
col_LCRRP = which(colnames(data2)=="LCRRP")
for(j in c(1:4,336:340)){
  i=j+1
  data2[i:n,col]=data2[1:(n-j),col_LCRRP]
  col=col+1
}

acf(data2$LCRRP[2:nrow(data2)],lag.max = 48*7, type = "correlation",main="")
acf(data2$LCRRP[2:nrow(data2)],lag.max = 48*7, type = "partial",main="")

# create dataset for RRP model training
data3 = dplyr::select(data2, LCRRP, LCTOTD,WDAY2, holiday)
data3 = data3[complete.cases(data3),]
n_data3 = nrow(data3)

#remove outliers
#data3[data3$LCRRP>1,"LCRRP"]=1
#data3[data3$LCRRP<(0-1),"LCRRP"]=-1


diffLCRRP1 = diff(data3$LCRRP,48)
#n_diff1 = length(diffLCRRP1)
#data3 = data3[(nrow(data3)-n_diff1+1):nrow(data3),]
acf(diffLCRRP1,lag.max = 48*8,type = "correlation",main = "")
acf(diffLCRRP1,lag.max = 48*8,type = "partial",main = "")


#ln = Arima(data3$LCRRP, order=c(0,0,4), seasonal=list(order=c(0,1,1),period=48),
#           xreg=data3$LCTOTD, include.mean=FALSE, include.drift=FALSE,
#           include.constant=FALSE)
#ln1 = Arima(data3$LCRRP, order=c(0,0,4), seasonal=list(order=c(0,1,1),period=48),include.mean=FALSE, include.drift=FALSE,
#           include.constant=FALSE)
#ln2 = Arima(data3$LCRRP, order=c(0,0,4), seasonal=list(order=c(0,1,1),period=48),
#            xreg=data3[,c("LCTOTD","WDAY2","holiday")], include.mean=FALSE, include.drift=FALSE,
#            include.constant=FALSE)


# create dataset for error analysis
LN = as.data.frame(matrix(NA,3,8))
colnames(LN) = c("M1","M2","M3","M4","M5","M6","M7","Naive")
rownames(LN) = c("RMSE","MAE","AIC")
coeff = vector("list", 7)

M = vector("list", 7)

M[[1]] = list(c(0,0,4),list(order=c(0,1,1),period=48))
M[[2]] = list(c(0,0,4),list(order=c(0,1,1),period=48),c("LCTOTD"))
M[[3]] = list(c(0,0,4),list(order=c(0,1,1),period=48),c("LCTOTD","WDAY2","holiday"))
M[[4]] = list(c(0,0,4),list(order=c(0,1,2),period=48),c("LCTOTD"))
M[[5]] = list(c(0,0,5),list(order=c(0,1,1),period=48),c("LCTOTD"))
M[[6]] = list(c(0,0,5),list(order=c(0,1,2),period=48),c("LCTOTD"))
M[[7]] = list(c(0,0,3),list(order=c(0,1,2),period=48),c("LCTOTD"))

#random cross-validation 10%
#t = 10 #cross-validation time
#step = round(n_data3/(t+1),0)
#for(i in 1:t){
#  Final = i*step
#  train = data3[1:Final,]
#  test = data3[(Final+1):(Final+48),]

j=1
ln = Arima(data3$LCRRP, order=M[[j]][[1]], seasonal=M[[j]][[2]],
          include.mean=FALSE, include.drift=FALSE,include.constant=FALSE)
LN["RMSE",j] = accuracy(ln)[colnames(accuracy(ln))=="RMSE"]
LN["MAE",j] = accuracy(ln)[colnames(accuracy(ln))=="MAE"]
LN["AIC",j] = ln$aic
coeff[[j]] = ln$coef

for(j in 2:7){    
  ln = Arima(data3$LCRRP, order=M[[j]][[1]], seasonal=M[[j]][[2]],
             xreg=data3[,M[[j]][[3]]], include.mean=FALSE, include.drift=FALSE,
             include.constant=FALSE)
  
  LN["RMSE",j] = accuracy(ln)[colnames(accuracy(ln))=="RMSE"]
  LN["MAE",j] = accuracy(ln)[colnames(accuracy(ln))=="MAE"]
  LN["AIC",j] = ln$aic
  coeff[[j]] = ln$coef
}
LN["RMSE","Naive"] = sqrt(sum((data3[1:(n_data3-1),"LCRRP"]- data3[2:n_data3,"LCRRP"])^2)/(n_data3-1))
LN["MAE","Naive"] = sum(abs(data3[1:(n_data3-1),"LCRRP"] - data3[2:n_data3,"LCRRP"]))/(n_data3-1)
#}

round(LN,4)

j=6
ln = Arima(data3$LCRRP, order=M[[j]][[1]], seasonal=M[[j]][[2]],
           xreg=data3[,M[[j]][[3]]], include.mean=FALSE, include.drift=FALSE,
           include.constant=FALSE)
ln6_backup = ln

data3$fitted = fitted(ln)
data3$res = residuals(ln)
LCRRP.ARIMA.pre = forecast(ln, h = 1,xreg = ln$xreg)
LCRRP.ARIMA.pre48 = LCRRP.ARIMA.pre$mean[1:48]

DLE1 = accuracy(ln)[,colnames(accuracy(ln))=="MAE"]*0.1
#DLE2 = DLE1*5
data3$trend = 0
for (i in 1:n_data3){
  #if(data3[i,"LCRRP"]-data3[i,"fitted"]>DLE2) {data3[i,"trend"] = 0.2}
  if(data3[i,"res"]>DLE1) {data3[i,"trend"] = 1}
  #else if(data3[i,"LCRRP"]-data3[i,"fitted"]<(0-DLE2)) {data3[i,"trend"] = -0.2}
  else if(data3[i,"res"]<(0-DLE1)) {data3[i,"trend"] = -1}
}

data_nnet = dplyr::select(data2, LCRRP, LCTOTD, L1LCRRP:L340LCRRP)

data_nnet$trend = 0
data_nnet[2:nrow(data_nnet),"ARIMA.fit"] = data3$fitted
data_nnet[2:nrow(data_nnet),"ARIMA.res"] = data3$res
n_fit = length(data3$fitted)
data_nnet[2:nrow(data_nnet),"trend"] = data3$trend

data_nnet[3:nrow(data_nnet),"ARIMA.fitL1"] = data3$fitted[1:(n_fit-1)]
data_nnet[338:nrow(data_nnet),"ARIMA.fitL336"] = data3$fitted[1:(n_fit-336)]
data_nnet[339:nrow(data_nnet),"ARIMA.fitL337"] = data3$fitted[1:(n_fit-337)]
data_nnet[50:nrow(data_nnet),"ARIMA.fitL48"] = data3$fitted[1:(n_fit-48)]
data_nnet[51:nrow(data_nnet),"ARIMA.fitL49"] = data3$fitted[1:(n_fit-49)]



#data_nnet[4:nrow(data_nnet),"ARIMA.fitL2"] = data3$fitted[1:(n_fit-2)]
#data_nnet[5:nrow(data_nnet),"ARIMA.fitL3"] = data3$fitted[1:(n_fit-3)]
data_nnet = data_nnet[complete.cases(data_nnet),] 
data_nnet = data_nnet[2000:nrow(data_nnet),]

trend = class.ind(data_nnet$trend)

nnet1 = nnet(data_nnet[,c("L1LCRRP","L2LCRRP","L3LCRRP","L4LCRRP","L336LCRRP","L337LCRRP",
                          "L338LCRRP","L339LCRRP","L340LCRRP","LCTOTD","ARIMA.fit","ARIMA.fitL1",
                          "ARIMA.fitL336","ARIMA.fitL337")],
             trend, size = 9, rang = 0.4,decay = 5e-4, maxit = 1000)
data_nnet$tar = max.col(nnet1$fitted.values)-2
nrow(data_nnet[data_nnet$trend==data_nnet$tar,])

data_opt = data_nnet
#data_opt[data_opt$ARIMA.res>1,"ARIMA.res"] =1
#data_opt[data_opt$ARIMA.res<(0-1),"ARIMA.res"] =-1
#data_opt$ARIMA.fit = data_opt$LCRRP-data_opt$ARIMA.res


#correct% = 13022/17179
#,"ARIMA.fitL48","ARIMA.fitL49"
abs_loss = function(OLS){
  OLS1 = OLS
  vec = class.ind(data_opt$tar)%*%c(-OLS1,0,OLS1)
  abs = sum(abs(data_opt$LCRRP-data_opt$ARIMA.fit-vec))
  return(abs)
}

opt = optim(par = 0.1,fn=abs_loss,method ="L-BFGS-B",lower = 0)
data_opt$est = data_opt$ARIMA.fit+class.ind(data_opt$tar)%*%c(-opt$par,0,opt$par)


(RMSE_fin = sqrt(sum((data_opt$est-data_opt$LCRRP)^2)/nrow(data_opt)))
(MAE_fin = sum(abs(data_opt$est-data_opt$LCRRP))/nrow(data_opt))

acf((data_opt$est-data_opt$LCRRP),lag.max = 48*8,type = "correlation",main = "")
acf((data_opt$est-data_opt$LCRRP),lag.max = 48*8,type = "partial",main = "")


#create dataset for demand model training
data4 = dplyr::select(data2,LCTOTD,L1LCTOTD:L340LCTOTD,WDAY2,holiday)
data4 = data4[complete.cases(data4),]
n_data4 = nrow(data4)
name_data4 = names(data4)

ANN.TOTD = as.data.frame(matrix(0,2,5))
colnames(ANN.TOTD) = c("M1","M2","M3","M4","Naive")
rownames(ANN.TOTD) = c("MSE","MAPE") 

M = vector("list", 5)
M[[1]] = as.formula(paste("LCTOTD ~", 
                          paste(name_data4[!name_data4 %in% c("LCTOTD","WDAY2","holiday")], collapse = " + ")))
M[[2]] = as.formula(paste("LCTOTD ~", 
                          paste(name_data4[!name_data4 %in% c("LCTOTD","WDAY2")], collapse = " + ")))
M[[3]] = as.formula(paste("LCTOTD ~", 
                          paste(name_data4[!name_data4 %in% c("LCTOTD","holiday")], collapse = " + ")))
M[[4]] = as.formula(paste("LCTOTD ~", 
                          paste(name_data4[!name_data4 %in% "LCTOTD"], collapse = " + ")))
hiddenstr = list(6, 6, 6, 6)

#random cross-validation 10%
t = 10  #cross-validation time
for(i in 1:t){
  index = sample(1:nrow(data4),round(1/t*nrow(data4)))
  index_error = index - 1
  test.cv = data4[index,]
  train.cv = data4[-index,]
  totd.cv = data2[index,"TOTALDEMAND"]
  for(j in 1:4){
    nn = neuralnet(M[[j]],data=train.cv,hidden=hiddenstr[[j]],linear.output=T)
    pr.nn = exp(compute(nn,test.cv[,all.vars(M[[j]][[3]])])$net.result)
    pr.nn.TOTD = totd.cv*pr.nn
    ANN.TOTD["MSE",j] = sum((pr.nn.TOTD-data2[index,"TOTALDEMAND"])^2)/
      length(index)/t + ANN.TOTD["MSE",j] 
    ANN.TOTD["MAPE",j] = sum(abs((pr.nn.TOTD-data2[index,"TOTALDEMAND"])/
                                   data2[index,"TOTALDEMAND"]))/length(index)/t + 
      ANN.TOTD["MAPE",j]
  }
}

ANN.TOTD["MSE","Naive"] = sum((data2[1:(nrow(data2)-1),"TOTALDEMAND"]-
                                 data2[2:nrow(data2),"TOTALDEMAND"])^2)/(nrow(data2)-1)
ANN.TOTD["MAPE","Naive"] = sum(abs((data2[1:(nrow(data2)-1),"TOTALDEMAND"]-
                                      data2[2:nrow(data2),"TOTALDEMAND"])/
                                     data2[2:nrow(data2),"TOTALDEMAND"]))/nrow(data4)

("The cross-validation error of the neural network models:")
round(ANN.TOTD,4)

#create dataset for demand prediction
data5 = dplyr::select(data2,LCTOTD,L1LCTOTD:L340LCTOTD)
data5 = data5[complete.cases(data5),]
n_data5 = nrow(data5)
name_data5 = names(data5)

#implementation
f = as.formula(paste("LCTOTD ~", paste(name_data5[!name_data5 %in% "LCTOTD"], 
                                       collapse = " + ")))
nn_LCTOTD = neuralnet(f,data=data5,hidden=5,linear.output=T)

lag_index = c(1:4,336:340)
for(j in 1:96){
  for(i in seq(2:ncol(data5))){
    data5[(j+n_data5),(i+1)]=data5[(j+n_data5-lag_index[i]),1]
  }
}
rownames(data5) = seq(1:nrow(data5))

for(i in 1:96){
  data5[(i+n_data5),1] = compute(nn_LCTOTD, data5[(i+n_data5),all.vars(f[[3]])])$net.result
  for(j in 1:4){
    data5[(i+n_data5+j),(1+j)]=data5[(i+n_data5),1]
  }
}
data5 = data5[complete.cases(data5),]

pr1 = as.data.frame(exp(data5[(n_data5+1):nrow(data5),"LCTOTD"]))
start = data2[nrow(data2),"TOTALDEMAND"]
for(i in 1:nrow(pr1)){
  pr1[i,"PRTOTD"]=start*prod(pr1[1:i,1]) #prediction of total demand
}
colnames(pr1)=c("datetime","PRTOTD")
pr1 = rbind(data.frame("datetime" = 0, "PRTOTD" = data2[nrow(data2),"TOTALDEMAND"]),pr1)
pr1=pr1[1:24,]

datetime = seq(from=as.POSIXct("2015/1/1 0:00"), to=as.POSIXct("2015/1/1 11:30"), 
               by="30 mins")  
plot(datetime,pr1$PRTOTD,type="l",xaxs='i', xlab="Date", ylab = "Total Demand (MW)",lty=2,
     xlim = as.POSIXct(c("2015/1/1 0:00","2015/1/1 11:30")),ylim=c(1000,5000))

true = read.csv('/Users/luohaosheng/Desktop/Question/Tios/Test/DATA201501_VIC1.csv')$TOTALDEMAND[1:23]
true = c(data2[nrow(data2),"TOTALDEMAND"],true)
lines(datetime,true)
legend("bottomright", c("True","Predicted"), cex=0.8,lty=1:3, lwd=2, bty="n")


#create dataset for price prediction

#ARIMA predict for the next 48: LCRRP.ARIMA.pre48

data6 = data2[(nrow(data2)-400):nrow(data2),]
n6 = nrow(data6)
data6[(n6+1):(n6+23),"TOTALDEMAND"]=pr1$PRTOTD[2:24]
data6$ARIMA.fit = c(data3$fitted[(nrow(data3)-400):nrow(data3)],LCRRP.ARIMA.pre48[1:23])
data6$tar=data6$ARIMA.fitL1=data6$ARIMA.fitL336=data6$ARIMA.fitL337=NA
for(i in 1:23){
  data6[(i+401),"LCTOTD"]=log(data6[(i+401),"TOTALDEMAND"]/data6[(i+400),"TOTALDEMAND"])
  data6[(i+401),"ARIMA.fitL1"]=data6[(i+400),"ARIMA.fit"]
  data6[(i+401),"ARIMA.fitL336"]=data6[(i+401-336),"ARIMA.fit"]
  data6[(i+401),"ARIMA.fitL337"]=data6[(i+401-337),"ARIMA.fit"]
  data6[(i+401),"L1LCRRP"]=data6[(i+401-1),"LCRRP"]
  data6[(i+401),"L2LCRRP"]=data6[(i+401-2),"LCRRP"]
  data6[(i+401),"L3LCRRP"]=data6[(i+401-3),"LCRRP"]
  data6[(i+401),"L4LCRRP"]=data6[(i+401-4),"LCRRP"]
  data6[(i+401),"L336LCRRP"]=data6[(i+401-336),"LCRRP"]
  data6[(i+401),"L337LCRRP"]=data6[(i+401-337),"LCRRP"]
  data6[(i+401),"L338LCRRP"]=data6[(i+401-338),"LCRRP"]
  data6[(i+401),"L339LCRRP"]=data6[(i+401-339),"LCRRP"]
  data6[(i+401),"L340LCRRP"]=data6[(i+401-340),"LCRRP"]
  
}

OSL = opt$par
for(i in 1:23){
  data6[(i+401),"tar"] =  max.col(predict(nnet1, data6[(401+i),c("L1LCRRP","L2LCRRP","L3LCRRP","L4LCRRP","L336LCRRP","L337LCRRP",
                 "L338LCRRP","L339LCRRP","L340LCRRP","LCTOTD","ARIMA.fit",
                 "ARIMA.fitL1","ARIMA.fitL336","ARIMA.fitL337")], type = "raw"))-2
  data6[(i+401),"LCRRP"] = data6[(i+401),"ARIMA.fit"]+data6[(i+401),"tar"]*OSL
  data6[(i+402),"L1LCRRP"]=data6[(i+402-1),"LCRRP"]
  data6[(i+402),"L2LCRRP"]=data6[(i+402-2),"LCRRP"]
  data6[(i+402),"L3LCRRP"]=data6[(i+402-3),"LCRRP"]
  data6[(i+402),"L4LCRRP"]=data6[(i+402-4),"LCRRP"]
}

#data6[402:(401+48),"SETTLEMENTDATE"] = seq(from=as.POSIXct("2015/1/1 0:30"), to=as.POSIXct("2015/1/2 0:00"), by="30 mins") 
#n = nrow(pr1)
#pr1[2:n,"LCTOTD"] = log(pr1$TOTALDEMAND[2:n])-log(pr1$TOTALDEMAND[1:(n-1)])
#pr1[2:n,"LCTOTD2"] = pr1[2:n,"LCTOTD"]^2
#pr1[2:n,"LCTOTD3"] = pr1[2:n,"LCTOTD"]^3
#pr1 = pr1[complete.cases(pr1),]

#data6 = dplyr::select(data2,LCRRP,LCTOTD,LCTOTD2,LCTOTD3)
#data6 = data6[complete.cases(data6),]

#implementation
#f = as.formula("LCRRP ~ LCTOTD + LCTOTD2 + LCTOTD3")
#ln = lm(f,data=data6)
#pr.RRP.log = exp(cbind(rep(1,nrow(pr1)),as.matrix(pr1[,all.vars(f[[3]])]))
#                 %*%as.matrix(ln$coefficients))
start = data6[n6,"RRP"]
pr.RRP = as.data.frame(matrix(0,23,1))
colnames(pr.RRP)="PRRRP"
for(i in 1:23){
  pr.RRP[i,1]=start*prod(exp(data6[(1+n6):(i+n6),"LCRRP"])) #prediction of the setlement price
}

pr.RRP = rbind(data.frame(PRRRP = data2[nrow(data2),"RRP"]),pr.RRP)
true = read.csv('/Users/luohaosheng/Desktop/Question/Tios/Test/DATA201501_VIC1.csv')$RRP[1:23]
true = c(data2[nrow(data2),"RRP"],true)

datetime = seq(from=as.POSIXct("2015/1/1 0:00"), to=as.POSIXct("2015/1/1 11:30"), 
               by="30 mins")  
plot(datetime,pr.RRP$PRRRP,type="l",xaxs='i', xlab="Time", ylab = "Price ($/MWh)",
     main="", ylim = c(-10,25), xlim = as.POSIXct(c("2015/1/1 0:00","2015/1/1 11:30")),lty=2)


lines(datetime,true)
legend("bottomright", c("True","Predicted"), cex=0.8,lty=1:3, lwd=2, bty="n")
