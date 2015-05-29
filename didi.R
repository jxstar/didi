library(xlsx)
library(leaps) #add for leap
library(ggplot2) #add for ggplot
library(Hmisc) #describe
library(psych) #describe
library(pastecs) #stat.desc
library(iplots)

Delete.na=function(xxdc,name) {
  col.tobe.delete=c();
  for (i in (1:ncol(xxdc))){
    if (all(is.na(xxdc[,i]))) {
      col.tobe.delete=c(col.tobe.delete,i)
    }
  }

  if (length(col.tobe.delete)>0){
    cat("Delete the ",col.tobe.delete," col @",name,"\n")
    xxdc=xxdc[,-col.tobe.delete]
  }
  xxdc=na.omit(xxdc)
  xxdc$city=rep(name,nrow(xxdc))
  names(xxdc)=c("no","times","rank","city")
  xxdc=xxdc[order(xxdc$rank),]
  if (!ncol(xxdc)==4)   print("There is a problem @: ",name)
  return(xxdc)
}

#-----------------------------------------------------------
#1# Read the Date
bjdc=read.xlsx("didi.order.xlsx",2)
bjzc=read.xlsx("didi.order.xlsx",3)
shdc=read.xlsx("didi.order.xlsx",4)
gzdc=read.xlsx("didi.order.xlsx",5)
szdc=read.xlsx("didi.order.xlsx",6)
tjdc=read.xlsx("didi.order.xlsx",7)
xmdc=read.xlsx("didi.order.xlsx",8)
dldc=read.xlsx("didi.order.xlsx",9)
qddc=read.xlsx("didi.order.xlsx",10)
zzdc=read.xlsx("didi.order.xlsx",11)
xadc=read.xlsx("didi.order.xlsx",12)
cddc=read.xlsx("didi.order.xlsx",13)
cqdc=read.xlsx("didi.order.xlsx",14)
hzdc=read.xlsx("didi.order.xlsx",15)
njdc=read.xlsx("didi.order.xlsx",16)
whdc=read.xlsx("didi.order.xlsx",17)

#-----------------------------------------------------------
#2# Clear the Date
#names(zc)=c("no","type","id","times","rank")
bjdc=Delete.na(bjdc,"Beijing")
shdc=Delete.na(shdc,"Shanghai")
gzdc=Delete.na(gzdc,"Guangzhou")
szdc=Delete.na(szdc,"Shenzhen")
tjdc=Delete.na(tjdc,"Tianjin")
xmdc=Delete.na(xmdc,"Xiamen")
dldc=Delete.na(dldc,"Dalian")
qddc=Delete.na(qddc,"Qingdao")
zzdc=Delete.na(zzdc,"Zhengzhou")
xadc=Delete.na(xadc,"Xian")
cddc=Delete.na(cddc,"Chengdu")
cqdc=Delete.na(cqdc,"Chongqing")
hzdc=Delete.na(hzdc,"Hangzhou")
njdc=Delete.na(njdc,"Nanjing")
whdc=Delete.na(whdc,"Wuhan")

#-----------------------------------------------------------
#3# Define the Macro variable
dc=list(bjdc,shdc,gzdc,szdc,tjdc,xmdc,dldc,qddc,zzdc,xadc,cddc,cqdc,hzdc,njdc,whdc)
cityinall=c("Beijing","Shanghai","Guangzhou","Shenzhen","Tianjin","Xiamen","Dalian",
            "Qingdao","Zhengzhou","Xian","Chengdu","Chongqing","Hangzhou","Nanjing","Wuhan")
if (!length(dc)==length(cityinall)) print("Error: length of city and data is not the same")

result=data.frame(city=cityinall,ndriver=rep(NA,length(cityinall)), maxorder=rep(NA,length(cityinall)),meanorder=rep(NA,length(cityinall)),totorder=rep(NA,length(cityinall)))
fitlist=list();
pdtlist=list();

seg.result=data.frame(city=cityinall,ndriver=rep(NA,length(cityinall)), maxorder=rep(NA,length(cityinall)),meanorder=rep(NA,length(cityinall)),totorder=rep(NA,length(cityinall)))

#-----------------------------------------------------------
#4# Fit with LM and predict result
Fitdc=function (x,city) {
  
  ##Have a look at the data
  par(mfrow=c(2,2))
  plot(x$rank,x$times,type="p",cex=0.2,xlab="rank",ylab="service",main=paste("Service vs Ranks of" , city))
  hist(x$times,breaks=500,xlab="service times",main=paste("Histogram of" , city))
  timesden=density(x$times,from=0,to=max(x$times))
  plot(timesden,main=paste("Service times Density of" , city))
  rankden=density(x$rank,from=0,to=max(x$rank))
  plot(rankden,main=paste("Rank Density of" , city))
  
  ##fit the data
  xfit=lm(times~rank,x)
  print(summary(xfit))
  
  ##draw the fit result
  par(mfrow=c(1,1))
  mycolor=rainbow(10)
  plot(x$rank,x$times,type="p",cex=0.2,col=mycolor[1],xlab="rank",ylab="service",main=paste(city,"Da Che"))
  legend("topright",legend=c("real", "fitted"),lty=c(1,1,1,1),col=mycolor[c(1,2,4,5)])
  lines(x$rank,fitted(xfit),type="l",col=mycolor[2])
  #identify(x$rank, x$times)

  return(xfit)
}

Predictdc=function (x,xfit,city) {
  
  #define the marcro var
  
  maxrank=80000
  if (any(x$times==0))  {
    zerodriver=min(x[which(x$times==0),"rank"])
    #maxrank=min(maxrank,zerodriver-1)
  }
  
  r=c(1:maxrank)
  p=data.frame(no=rep(NA,length(r)),times=rep(NA,length(r)),rank=r)
  p$times=predict(xfit,newdata=p)
  p$times=round(p$times)
  p=p[which(p$times>0),]
  
  ##draw the predict result
  par(mfrow=c(1,1))
  mycolor=rainbow(10)
  #plot(p$rank,p$times,type="p",cex=0.1,xlab="rank",ylab="service",main=paste(city,"Da Che Predict"))  
  return(p)
}

Resultdc=function (p,xxdc,city){
  if (any(as.integer(xxdc$rank)==1)) maxorder=max(xxdc$times)
  else maxorder=max(p[which(p$rank==1),"times"],max(xxdc$times))
  xres=data.frame(city=city,ndriver=nrow(p),
                     maxorder=maxorder,
                     meanorder=mean(p$times,na.rm=T),
                     totorder=sum(p$times,na.rm=T)) 
  return(xres)
}

for (i in 1:length(cityinall)) {
#for (i in 1:2) {
  icity=cityinall[i]
  cat(icity)
  for (j in 1:length(dc)) {
    xxdc=dc[[j]]
    if (xxdc[1,"city"]==icity) break
    else if (j==length(dc)) print("Can not find the city of ", icity)
  }

  xfit=Fitdc(xxdc,icity)
  xpdt=Predictdc(xxdc,xfit,icity)
  xres=Resultdc(xpdt,xxdc,icity)
  
  fitlist[[i]]=xfit
  pdtlist[[i]]=xpdt
  result[which(result$city==icity),]=xres
  
}

(result)

#-----------------------------------------------------------
#5# View the result
if (F){
  result
  table(bjdcpdt$rank)
  
}

#-----------------------------------------------------------
#6# make the prediction better

Segfit=function(xxdc,h,l,city){
  
  hseg=xxdc[h,"rank"]
  lseg=xxdc[l,"rank"]
  
  bjh=xxdc[1:h,]
  bjm=xxdc[h+1:l,]
  bjl=xxdc[l+1:nrow(xxdc),]
  
  bjhfit=lm(times~rank,bjh)
  bjmfit=lm(times~rank,bjm)
  print(summary(bjmfit))
  bjlfit=lm(times~rank,bjl)
  
  maxrank=80000

  r=c(1:maxrank)
  p=data.frame(no=rep(NA,length(r)),times=rep(NA,length(r)),rank=r)
  ph=p
  pm=p
  pl=p
  ph$times=predict(bjhfit,newdata=ph)
  pm$times=predict(bjmfit,newdata=pm)
  pl$times=predict(bjlfit,newdata=pl)
  
  hmbreak=which.min(abs(ph$times-pm$times))
  lmbreak=which.min(abs(pl$times-pm$times))
  
if (T){  
  # the upper break we choose hmbreak, the break of prediction
  # the lower break we choose lseg,    the break of training data
  p[1:hmbreak,"times"]=ph[1:hmbreak,"times"]
  p[hmbreak+1:lseg,"times"]=pm[hmbreak+1:lseg,"times"]
  p[lseg+1:nrow(p),"times"]=pl[lseg+1:nrow(p),"times"]
   
} else {
  p[1:hseg,"times"]=ph[1:hseg,"times"]
  p[hseg+1:lseg,"times"]=pm[hseg+1:lseg,"times"]
  p[lseg+1:nrow(p),"times"]=pl[lseg+1:nrow(p),"times"]
}
  
  p$times=round(p$times)
  p=p[which(p$times>0),]
  
  par(mfrow=c(1,1))
  mycolor=rainbow(10)
  plot(xxdc$rank,xxdc$times,type="p",cex=0.2,col=mycolor[1],xlab="rank",ylab="service",main=paste(city,"Da Che"))
  legend("topright",legend=c("real", "perdict"),lty=c(1,1,1,1),col=mycolor[c(1,2,4,5)])
  lines(p$rank,p$times,type="l",col=mycolor[2])
  
  #identify(xxdc$rank, xxdc$times)
  xxres=Resultdc(p,xxdc,city)
  cat("city, nrow, hseg, lseg, hmbreak, lmbreak\n")
  cat(city, xxres$ndriver, hseg, lseg, hmbreak, lmbreak, "\n")

  res.list=list();
  res.list[[1]]=bjhfit
  res.list[[2]]=bjmfit
  res.list[[3]]=bjlfit
  res.list[[4]]=p
  res.list[[5]]=xxres
  return(res.list)  
  
}

if (F) {
# mark the break point and the strange point
bjres=Segfit(bjdc,6,263,"Beijing")

shdcfit=Fitdc(shdc,"Shanghai")
identify(shdc$rank, shdc$times)  # 7,9,195
shres=Segfit(shdc,9,195,"Shanghai")

gzdcfit=Fitdc(gzdc,"Guangzhou")
identify(gzdc$rank, gzdc$times)
gzres=Segfit(gzdc,7,107,"Guangzhou")

szdcfit=Fitdc(szdc,"Shenzhen")
identify(szdc$rank, szdc$times)
szres=Segfit(szdc,3,98,"Shenzhen")

tjdcfit=Fitdc(tjdc,"Tianjin")
identify(tjdc$rank, tjdc$times)# 1,2,3,123
tjres=Segfit(tjdc,2,123,"Tianjin")

xmdcfit=Fitdc(xmdc,"Xiamen")
identify(xmdc$rank, xmdc$times)# 
xmres=Segfit(xmdc,5,115,"Xiamen")

dldcfit=Fitdc(dldc,"Dalian")
identify(dldc$rank, dldc$times)# 
dlres=Segfit(dldc,6,126,"Dalian")

qddcfit=Fitdc(qddc,"Qingdao")
identify(qddc$rank, qddc$times)# 
qdres=Segfit(qddc,4,122,"Qingdao")

zzdcfit=Fitdc(zzdc,"Zhengzhou")
identify(zzdc$rank, zzdc$times)# 
zzres=Segfit(zzdc,20,111,"Zhengzhou")

xadcfit=Fitdc(xadc,"Xian")
identify(xadc$rank, xadc$times)# 4,106,110strange point:126
xares=Segfit(xadc[-c(126),],8,110,"Xian")

cddcfit=Fitdc(cddc,"Chengdu")
identify(cddc$rank, cddc$times)# 4,106,110strange point:38 ,120
cdres=Segfit(cddc,15,80,"Chengdu")

cqdcfit=Fitdc(cqdc,"Chongqing")
identify(cqdc$rank, cqdc$times)# 4,106,110strange point:38 ,120
cqres=Segfit(cqdc,14,98,"Chongqing")

hzdcfit=Fitdc(hzdc,"Hangzhou")
identify(hzdc$rank, hzdc$times)# 6,112strange point:12，132
hzres=Segfit(hzdc[-c(12,132),],6,111,"Hangzhou")

njdcfit=Fitdc(njdc,"Nanjing")
identify(njdc$rank, njdc$times)# 6,112strange point:12，132
njres=Segfit(njdc,15,110,"Nanjing")
cityinall

whdcfit=Fitdc(whdc,"Wuhan")
identify(whdc$rank, whdc$times)# 4,110strange point:38,120
whres=Segfit(whdc[-c(38,120),],4,101,"Wuhan")

} else {
  
  # mark the break point and the strange point
  bjres=Segfit(bjdc,6,263,"Beijing")
  shres=Segfit(shdc,9,195,"Shanghai")
  gzres=Segfit(gzdc,7,107,"Guangzhou")
  szres=Segfit(szdc,3,98,"Shenzhen")
  tjres=Segfit(tjdc,2,123,"Tianjin")
  xmres=Segfit(xmdc,5,115,"Xiamen")
  dlres=Segfit(dldc,6,126,"Dalian")
  qdres=Segfit(qddc,4,122,"Qingdao")
  zzres=Segfit(zzdc,20,111,"Zhengzhou")
  xares=Segfit(xadc[-c(126),],8,110,"Xian")
  cdres=Segfit(cddc,15,80,"Chengdu")
  cqres=Segfit(cqdc,14,98,"Chongqing")
  hzres=Segfit(hzdc[-c(12,132),],6,111,"Hangzhou")
  njres=Segfit(njdc[-c(39,98),],15,110,"Nanjing")
  whres=Segfit(whdc[-c(38,120),],4,101,"Wuhan")
  

  seg.result[1,]=bjres[[5]]
  seg.result[2,]=shres[[5]]
  seg.result[3,]=gzres[[5]]
  seg.result[4,]=szres[[5]]
  seg.result[5,]=tjres[[5]]
  seg.result[6,]=xmres[[5]]
  seg.result[7,]=dlres[[5]]
  seg.result[8,]=qdres[[5]]
  seg.result[9,]=zzres[[5]]
  seg.result[10,]=xares[[5]]
  seg.result[11,]=cdres[[5]]
  seg.result[12,]=cqres[[5]]
  seg.result[13,]=hzres[[5]]
  seg.result[14,]=njres[[5]]
  seg.result[15,]=whres[[5]]
  
  
  
}


#3 stage - 1 stage
diff.res=data.frame(city=result$city, 
                    ndriver=seg.result$ndriver-result$ndriver,
                    maxorder=seg.result$maxorder-result$maxorder,
                    meanorder=seg.result$meanorder-result$meanorder,
                    totorder=seg.result$totorder-result$totorder)




(seg.result)
(diff.res)



















bjdcfit=Fitdc(bjdc,"Beijing")
range(bjdc$rank)
r=c(1:79000)
p=data.frame(no=rep(NA,length(r)),times=rep(NA,length(r)),rank=r)
p$times=predict(bjdcfit,newdata=p)
p$times=round(p$times)
p=p[which(p$times>0),]
head(p)
tail(p)
mycolor=rainbow(10)
plot(p$rank,p$times,type="p",cex=0.1,col=mycolor[1],xlab="rank",ylab="service",main=paste(city,"Da Che Predict"))

N=max(p$rank)
M=p[1,"times"]
M
N
xfit2=density(bjdc$times,from=1,to=M,n=N)
plot(xfit2)
xfit3=density(p$times,from=0,to=M)
plot(xfit3)

qplot(bjdc$rank,data=bjdc,geom=c("histogram"),binwidth=100,main ="bj density")
qplot(bjdc$rank,data=bjdc,geom="density")
plot(bjdc$rank,bjdc$times,type="p",cex=0.2,col=mycolor[1],xlab="rank",ylab="service",main=paste(city,"Da Che"))


rank.den=density(bjdc$rank,from=1,to=N,n=N)
plot(rank.den)
rank.den$x
rank.den$y

times.den=density(bjdc$times,from=1,to=M,n=N)
plot(times.den)
times.den$x
times.den$y
rank.den=data.frame(x=order(rank.den$x,decreasing=T),y=rank.den$y)
times.den=data.frame(x=times.den$x,y=times.den$y)
times.den
rank.den
division=rank.den$y/times.den$y
plot(c(1:length(division)),division)

rank.den=rank.den[order(rank.den$x,decreasing=T),]









xfit=density(xmdc$times)
plot(xfit)
plot(density(x$times),bw=1,main=paste(city,"density"))



bjdcfit=Fitdc(bjdc,"Beijing")
shdcfit=Fitdc(shdc,"Shanghai")
gzdcfit=Fitdc(gzdc,"Guangzhou")
szfit=Fitdc(szdc,"Shenzhen")
tjdcfit=Fitdc(tjdc,"Tianjin")
xmdcfit=Fitdc(xadc,"Xiamen")
dldcfit=Fitdc(dldc,"Dalian")
qddcfit=Fitdc(qddc,"Qingdao")
zzdcfit=Fitdc(zzdc,"Zhengzhou")
xadcfit=Fitdc(xadc,"Xian")
cddcfit=Fitdc(cddc,"Chengdu")
cqdcfit=Fitdc(cqdc,"Chongqing")
hzdcfit=Fitdc(hzdc,"Hangzhou")
njdcfit=Fitdc(njdc,"Nanjing")
whdcfit=Fitdc(whdc,"Wuhan")









plot(zc$rank,zc$times,type="p")
qplot(zc$times,data=zc,geom="histogram",binwidth=1)
qplot(zc$times,data=zc,geom="density")
barplot(zc$times)
zc$ds=(1/zc$rank)
zfit=lm(times~I(zc$ds^0.2)+I(zc$ds^0.3)+I(zc$ds^0.1)+I(zc$ds^0.4),zc)
summary(zfit)
mycolor=rainbow(10)
plot(zc$rank,zc$times,type="p",cex=0.2,col=mycolor[1],xlab="rank",ylab="service",main="Zhuan Che")
legend("topright",legend=c("real", "fitted"),lty=c(1,1,1,1),col=mycolor[c(1,8,4,5)])
lines(zc$rank,fitted(zfit),type="l",col=mycolor[8])






range(zc$times)
range(zc$rank)

# assume : N drivers:1,2,3,...N
#          M is the best driver service times

M=max(zc$times)+min
N=max(zc$rank)

N=4000
M=2000
step=M/N
zfit2=density(zc$times,from=0,to=M,n=N)
plot(zfit2)

zfit2$x
zfit2$y
sum(zfit2$y)*step



xx <- faithful$eruptions
fit <- density(xx,n=10000)
N <- 1e6
x.new <- rnorm(N, sample(xx, size = N, replace = TRUE), fit$bw)
yy=sample(xx, size = N, replace = TRUE)
plot(fit)
lines(density(x.new), col = "blue")



