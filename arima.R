setwd("C:\\Users\\19406\\Desktop\\数据分析\\时间序列\\sarima")
library(readxl)
cpi<-read_xlsx("数据.xlsx")
head(cpi)
plot(cpi,type="l")#时序图


par(mfrow=c(2,1))
CPI<-as.xts(cpi$CPI,order.by = cpi$时间)
plot(CPI)
##初步判定为不平稳序列

#-----非随机性检验-----
for(i in 1:6) print(Box.test(CPI,lag=i))
#非白噪声序列

#-----对数、差分平稳处理----
#CPI序列adf检验结果
adf.test(CPI)
#显著性水平a=0.01下不平稳

#CPI对数差分后检验结果
DlogCPI<-diff(log(CPI),ndiffs(CPI))[-1]
plot(DlogCPI)
adf.test(DlogCPI)
#显著性水平a=0.01下平稳

#-----平稳性检验----
nfactor<-ncol(DlogCPI)
obs<-nrow(DlogCPI)
title1<-c("Date","DlogCPI")
title2<-c("DlogCPI")
df=data.frame(matrix(NA,nfactor,3))
rownames(df)<-title2
colnames(df)<-c("ADF检验","P值","是否平稳")

for(i in 1:nfactor){
  df[i,1]<-adf.test(DlogCPI[,i])$statistic
  df[i,2]<-adf.test(DlogCPI[,i])$p.value
  if(df[i,2]<=0.05)(df[i,3]<-"平稳")
  else{df[i,3]<-"不平稳"}
}
df



#-----模型识别-----
pacf(CPI)#拖尾
acf(CPI)#拖尾
###初步判定为不平稳序列，由自相关系数和偏自相关系数拖尾可以判定出该模型为ARMA模型
###由于原数据对数一阶后平稳，该模型识别为ARIMA(1,1,1)模型

auto.arima(CPI)##自动定阶函数识别为ARIMA(3,1,0)模型


#-----模型检验---
par(mfrow=c(1,1))
#1.ARIMA(1,1,1)
ARIMA111<-arima(CPI,order = c(1,1,1))
ARIMA111
#平稳性检验
plot(ARIMA111)
mse1<-sum(ARIMA111$residuals^2)/nrow(CPI);mse1#残差均方和
sum(mean(ARIMA111$residuals^2))
###特征方程根的倒数均在单位圆内，模型平稳

#残差检验
for(i in 1:6) print(Box.test(ARIMA111$residuals,lag=i))
#模型残差序列为白噪声序列，即模型已将原始时间序列数据的信息基本提取出来


#2.ARIMA(3,1,0)
ARIMA310<-arima(CPI,order = c(3,1,0))
ARIMA310
plot(ARIMA310)
###特征方程根的倒数均在单位圆内，模型平稳

#残差检验
for(i in 1:6) print(Box.test(ARIMA310$residuals,lag=i))
#模型残差序列为白噪声序列，即模型已将原始时间序列数据的信息基本提取出来
mse2<-sum(ARIMA310$residuals^2)/nrow(CPI);mse2
sum(mean(ARIMA310$residuals^2))

#-----模型预测对比-----
library(dplyr)
library(ggplot2)
library(lubridate)
theme_set(theme_bw())

pre1<-cpi$CPI+ARIMA111$residuals
pre2<-cpi$CPI+ARIMA310$residuals


#作图
type<-rep(c("原序列","ARIMA(1,1,1)","ARIMA(3,1,0)"),each=nrow(CPI))

dat<-data.frame(时间=cpi$时间,模型=type,CPI=c(cpi$CPI,pre1,pre2))
head(dat)
ggplot(dat, aes(x=时间,fill=模型,col=模型)) + 
  geom_line(aes(y=CPI))

