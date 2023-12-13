# Surface Water Final Project

# Load in the data!
watertbl1 <- read.table("~/Documents/SWFP/complete/out1/water_veg.txt", skip=7, header=FALSE, sep="", dec=".")
watertbl2 <- read.table("~/Documents/SWFP/complete/out2/water_veg.txt", skip=7, header=FALSE, sep="", dec=".")
watertbl3 <- read.table("~/Documents/SWFP/complete/out3/water_veg.txt", skip=7, header=FALSE, sep="", dec=".")
watertbl4 <- read.table("~/Documents/SWFP/complete/out4/water_veg.txt", skip=7, header=FALSE, sep="", dec=".")

watertbl5 <- read.table("~/Documents/SWFP/complete/out5/water_veg.txt", skip=7, header=FALSE, sep="", dec=".")
watertbl6 <- read.table("~/Documents/SWFP/complete/out6/water_veg.txt", skip=7, header=FALSE, sep="", dec=".")
watertbl7 <- read.table("~/Documents/SWFP/complete/out7/water_veg.txt", skip=7, header=FALSE, sep="", dec=".")
watertbl8 <- read.table("~/Documents/SWFP/complete/out8/water_veg.txt", skip=7, header=FALSE, sep="", dec=".")

watertbl9 <- read.table("~/Documents/SWFP/complete/out9/water_veg.txt", skip=7, header=FALSE, sep="", dec=".")
watertbl10 <- read.table("~/Documents/SWFP/complete/out10/water_veg.txt", skip=7, header=FALSE, sep="", dec=".")
watertbl11 <- read.table("~/Documents/SWFP/complete/out11/water_veg.txt", skip=7, header=FALSE, sep="", dec=".")
watertbl12 <- read.table("~/Documents/SWFP/complete/out12/water_veg.txt", skip=7, header=FALSE, sep="", dec=".")

# Extract runoff
runoff1 <- watertbl1[,14]
runoff2 <- watertbl2[,14]
runoff3 <- watertbl3[,14]
runoff4 <- watertbl4[,14]

runoff5 <- watertbl5[,14]
runoff6 <- watertbl6[,14]
runoff7 <- watertbl7[,14]
runoff8 <- watertbl8[,14]

runoff9 <- watertbl9[,14]
runoff10 <- watertbl10[,14]
runoff11 <- watertbl11[,14]
runoff12 <- watertbl12[,14]

t <- seq(1,364)

#Input variables
vLAI <- c(0,2.6424,4.757,6) #Leaf area index (dimensionless)
vSLP <- c(0,7,35,99) #slope (%)
vK <- c(1e-8,1,1e2,1e3) #Saturated conductivity (cm/hr)

# How many days have runoff?
runofflist <- list(runoff1,runoff2,runoff3,runoff4,runoff5,runoff6,runoff7,
                   runoff8,runoff9,runoff10,runoff11,runoff12)
runoffdays <- sapply(runofflist,function(runoff) sum(runoff>0))

#What is average daily runoff?
runoffavdaily <- numeric(length(runofflist))
for(i in seq_along(runofflist)){
  runoffavdaily[i] <- mean(runofflist[[i]])
}

# Elasticity between input variables and average daily runoff
lmLAI <- lm(runoffavdaily[1:4]~vLAI)
lmSLP <- lm(runoffavdaily[5:8]~vSLP)
lmK <- lm(runoffavdaily[9:12]~vK)

#Elasticity between input variables and days of runoff
lmLAIday <- lm(runoffdays[1:4]~vLAI)
lmSLPday <- lm(runoffdays[5:8]~vSLP)
lmKday <- lm(runoffdays[9:12]~vK)

# Correlation coefficient between input variables and average daily runoff
corLAI <- cor(runoffavdaily[1:4],vLAI)
corSLP <- cor(runoffavdaily[5:8],vSLP)
corK <- cor(runoffavdaily[9:12],vK)

#Correlation coefficient between input variables and days of runoff
corLAIday <- cor(runoffdays[1:4],vLAI)
corSLPday <- cor(runoffdays[5:8],vSLP)
corKday <- cor(runoffdays[9:12],vK)

#Elasticity and correlation coefficient if K is on a logarithmic scale
lmKlog <- lm(runoffavdaily[9:12]~log(vK))
lmKdaylog <- lm(runoffdays[9:12]~log(vK))
corKlog <- cor(runoffavdaily[9:12],log(vK))
corKdaylog <- cor(runoffdays[9:12],log(vK))

# Plot runoff over time
colvec=c('cadetblue','yellowgreen','gold','orange')
par(mfrow=c(3,1),mar=c(4,4,2,1))
plot(x=t,y=runoff1,type='l',ylab='Runoff (mm)',xlab='Day',col=colvec[1],main='(a) LAI')
lines(x=t,y=runoff2,type='l',col=colvec[2])
lines(x=t,y=runoff3,type='l',col=colvec[3])
lines(x=t,y=runoff4,type='l',col=colvec[4])
legend('topright',legend=vLAI,col=colvec,lty=1:1:1:1,ncol=2)

plot(x=t,y=runoff5,type='l',ylab='Runoff (mm)',xlab='Day',col=colvec[1],main='(b) Slope')
lines(x=t,y=runoff6,type='l',col=colvec[2])
lines(x=t,y=runoff7,type='l',col=colvec[3])
lines(x=t,y=runoff8,type='l',col=colvec[4])
legend('topright',legend=vSLP,col=colvec,lty=1:1:1:1,ncol=2)

plot(x=t,y=runoff9,type='l',ylab='Runoff (mm)',xlab='Day',col=colvec[1],main='(c) Saturated Conductivity')
lines(x=t,y=runoff10,type='l',col=colvec[2])
lines(x=t,y=runoff11,type='l',col=colvec[3])
lines(x=t,y=runoff12,type='l',col=colvec[4])
legend('topright',legend=vK,col=colvec,lty=1:1:1:1,ncol=2)

par(mfrow=c(3,2),mar=c(4,4,2,1))
#plot variables vs average daily runoff and days of runoff
plot(x=vLAI,y=runoffavdaily[1:4],ylab='Av. Daily Runoff (mm)',xlab='LAI',main='(a)',type='b',col='cadetblue')
plot(x=vLAI,y=runoffdays[1:4],ylab='Days of Runoff',xlab='LAI',main='(b)',type='b',col='cadetblue')
plot(x=vSLP,y=runoffavdaily[5:8],ylab='Av. Daily Runoff (mm)',xlab='Slope (%)',main='(c)',type='b',col='cadetblue')
plot(x=vSLP,y=runoffdays[5:8],ylab='Days of Runoff',xlab='Slope (%)',main='(d)',type='b',col='cadetblue')
plot(x=vK,y=runoffavdaily[9:12],log='x',ylab='Av. Daily Runoff (mm)',xlab='Saturated Conductivity (cm/hr)',main='(e)',type='b',col='cadetblue')
plot(x=vK,y=runoffdays[9:12],log='x',ylab='Days of Runoff',xlab='Saturated Conductivity (cm/hr)',main='(f)',type='b',col='cadetblue')
