#############################################################
#####################Section 1 data and EDA##################
#############################################################

#Read the data
dat<-read.csv('Prostate.csv')

#Split data into a training and validation set
tdat<-dat[dat$train==TRUE,]
vdat<-dat[dat$train==FALSE,]

#center and scale all predictors - necessary for shrinkage methods
tdats<-data.frame(
  lcavol  =(tdat$lcavol  -mean(tdat$lcavol ))/sd(tdat$lcavol ),
  lweight =(tdat$lweight -mean(tdat$lweight))/sd(tdat$lweight),
  age     =(tdat$age     -mean(tdat$age    ))/sd(tdat$age    ),
  lbph    =(tdat$lbph    -mean(tdat$lbph   ))/sd(tdat$lbph   ),
  svi     =(tdat$svi     -mean(tdat$svi    ))/sd(tdat$svi    ),
  lcp     =(tdat$lcp     -mean(tdat$lcp    ))/sd(tdat$lcp    ),
  gleason =(tdat$gleason -mean(tdat$gleason))/sd(tdat$gleason),
  pgg45   =(tdat$pgg45   -mean(tdat$pgg45  ))/sd(tdat$pgg45  )
)
lpsa    =tdat$lpsa

#By default, refer to objects in tdat
attach(tdats)

#examine the correlation among the values in the data 
pairs(tdats)
cor(tdats)

#Some basic regressions
#regrssion review code
plot(lcavol,lpsa,pch=16,main='Describe the association')
lcavolfit<-summary(lm(lpsa~lcavol))

plot(lcavol,lpsa,pch=16,main='Data plus estimated regression line')
abline(lcavolfit$coeff[1,1],lcavolfit$coeff[2,1],col='red',lwd=3)

plot(lcavol,lpsa,pch=16,main='Data plus estimated regression line?')
abline(lcavolfit$coeff[1,1],lcavolfit$coeff[2,1],col='red',lwd=3)
abline(a=-2,b=3,col='darkgreen',lwd=3)

plot(lcavol,lpsa,pch=16,main='Data plus estimated regression line?')
abline(lcavolfit$coeff[1,1],lcavolfit$coeff[2,1],col='red',lwd=3)
abline(a=1.2,b=1,col='darkblue',lwd=3)

#Bivariate relationship between lpsa (the outcome) and each predictor
par(mfrow=c(2,4))
plot(age,lpsa,pch=16,cex.lab=1.5)
agefit<-summary(lm(lpsa~age))
abline(agefit$coeff[1,1],agefit$coeff[2,1],col='red',lwd=3)

plot(gleason,lpsa,pch=16,cex.lab=1.5)
gleasonfit<-summary(lm(lpsa~gleason))
abline(gleasonfit$coeff[1,1],gleasonfit$coeff[2,1],col='red',lwd=3)

plot(lbph,lpsa,pch=16,cex.lab=1.5)
lbphfit<-summary(lm(lpsa~lbph))
abline(lbphfit$coeff[1,1],lbphfit$coeff[2,1],col='red',lwd=3)

plot(lcavol,lpsa,pch=16,cex.lab=1.5)
lcavolfit<-summary(lm(lpsa~lcavol))
abline(lcavolfit$coeff[1,1],lcavolfit$coeff[2,1],col='red',lwd=3)

plot(lweight,lpsa,pch=16,cex.lab=1.5)
lweightfit<-summary(lm(lpsa~lweight))
abline(lweightfit$coeff[1,1],lweightfit$coeff[2,1],col='red',lwd=3)

plot(svi,lpsa,pch=16,cex.lab=1.5)
svifit<-summary(lm(lpsa~svi))
abline(svifit$coeff[1,1],svifit$coeff[2,1],col='red',lwd=3)

plot(lcp,lpsa,pch=16,cex.lab=1.5)
lcpfit<-summary(lm(lpsa~lcp))
abline(lcpfit$coeff[1,1],lcpfit$coeff[2,1],col='red',lwd=3)

plot(pgg45,lpsa,pch=16,cex.lab=1.5)
pgg45fit<-summary(lm(lpsa~pgg45))
abline(pgg45fit$coeff[1,1],pgg45fit$coeff[2,1],col='red',lwd=3)

#Examine the ordinary least squares fit of the full model using multiple regression
OLS<-summary(lm(lpsa~.,data=tdats))
OLS

#############################################################
#####################Section 2 implement lasso###############
#############################################################
#Load the lars packagem, which can fit the lasso
library(lars)
?lars  #What does lars do?

#define a lasso-object
lasso<-lars(x=as.matrix(tdats),y=lpsa,type='lasso',trace=FALSE,normalize=TRUE,intercept=TRUE)
par(mfrow=c(1,1));plot(lasso)
lasso
coef(lasso) #look at lasso coefficients at each step

predict.lars(object=lasso,s=.375,mode='fraction',type='coefficients')
predict.lars(object=lasso,newx=tdats,s=.375,mode='fraction',type='fit')

absum<-sum(abs(OLS$coeff[-1,1]))

#Build the lasso plot from the ground up 
t<-apply(abs(coef(lasso)),1,sum) #Sum of absolute value of OLS coefficients
s<-t/absum

plot( s,coef(lasso)[,1],ylim=c(-.3,0.7),type='l',lwd=2,xlab='Shrinkage factor s',
      main='Lasso path - coefficients  as a function of shrinkage factor s',
      xlim=c(0,1.2),axes=FALSE,ylab='Coefficient',cex.lab=1.5,cex.axis=1.4)
axis(1,at=seq(0,1,.2),cex.axis=1.4)
axis(2,at=seq(-.3,.7,.2),cex.axis=1.4)
lines(s,coef(lasso)[,2],lwd=2)
lines(s,coef(lasso)[,3],lwd=2)
lines(s,coef(lasso)[,4],lwd=2)
lines(s,coef(lasso)[,5],lwd=2)
lines(s,coef(lasso)[,6],lwd=2)
lines(s,coef(lasso)[,7],lwd=2)
lines(s,coef(lasso)[,8],lwd=2)

text(1.05,0.72,'lcavol')
text(1.03,0.34,'svi')
text(1.05,0.30,'lweight')
text(1.05,0.26,'ppg45')
text(1.04,0.20,'lbph')
text(1.06,-.02,'gleason')
text(1.03,-.15,'age')
text(1.03,-.29,'lcp')

abline(v=s,col='lightgray',lty=3)

#############################################################
#####Section 3 Cross validation and choosing s###############
#############################################################

##10 fold Cross validation to choose a value of s
set.seed(389173367) #sets 'starting point' for list of random numbers

#Genreate a vector of holdout labels
cvlab<-sample(1:10,67,replace=TRUE)

#How many of each label are there?
table(cvlab)

#Create a vector of candidate s values
#Try each s value on all cross validated sets
svec<-seq(0,1,.05)
J<-length(svec)

#Initialize a list to store lasso objects from k fold cross validation
lassolist<-list()

#Initialize a list to store predictions from each lasso set
predtrain<-list()

#Initialize a matrix to store MSE
#Rows correspond to the J values of s, columns correspond to the ten holdout sets
MSEstore<-matrix(NA,J,10)

#Use a for loop to get each lasso fit holding out the ith set
#Then predict the ith set using the holdout model
for(i in 1:10){
lassolist[[i]]<-lars(x=as.matrix(tdats)[cvlab!=i,],y=lpsa[cvlab!=i],type='lasso',trace=FALSE,normalize=TRUE,intercept=TRUE)
predtrain[[i]]<-fit<-predict.lars(object=lassolist[[i]],newx=tdats[cvlab==i,],s=svec,mode='fraction',type='fit')$fit
#Start a new loop to get MSE for each combination of ith holdout set and jth value of s
for(j in 1:J){
  MSEstore[j,i]<-mean((predtrain[[i]][,j]-lpsa[cvlab==i])^2) #This computes MSE
  }
}

#These apply statements compute mean and standard error  of the observed MSEs at J values of s across the 10 holdout sets
meanMSE<-apply(MSEstore,1,mean)
stdMSE<-apply(MSEstore,1,sd)/sqrt(10)

plot(svec,meanMSE,ylim=c(0.5,1.75),pch=16,col=colors()[258],axes=FALSE,cex=1.2,
     xlab='Shrinkage factor s',ylab='Mean square error',cex.lab=1.7,main='Average CV prediction error as a function of s')
axis(1,cex.axis=1.4,cex.axis=1.2)
axis(2,las=1,at=seq(0.5,1.75,.25),cex.axis=1.2)
lines(svec,meanMSE,lty=1,col=colors()[258])
for(i in 1:J)segments(svec[i],(meanMSE[i]-stdMSE[i]),svec[i],(meanMSE[i]+stdMSE[i]))
abline(h=(meanMSE+stdMSE)[18],lty=2)
points(svec[9],meanMSE[9],col='red',pch=15,cex=1.3)
legend(.35,1.5,legend=c('mean MSE','standard error (SE)','1 SE above lowest mean','chosen value of s'),
       pch=c(16,NA,NA,15),col=c(colors()[258],1,1,'red'),cex=1.1,lty=c(1,1,2,NA))

#These are the coefficients for the chosen model
predict.lars(lasso,s=.4,mode='fraction',type='coefficients')

#How well does the training model work on training and validation sets?

#center and scale all predictors - necessary for shrinkage methods
vdats<-data.frame(
  lcavol  =(vdat$lcavol  -mean(vdat$lcavol ))/sd(vdat$lcavol ),
  lweight =(vdat$lweight -mean(vdat$lweight))/sd(vdat$lweight),
  age     =(vdat$age     -mean(vdat$age    ))/sd(vdat$age    ),
  lbph    =(vdat$lbph    -mean(vdat$lbph   ))/sd(vdat$lbph   ),
  svi     =(vdat$svi     -mean(vdat$svi    ))/sd(vdat$svi    ),
  lcp     =(vdat$lcp     -mean(vdat$lcp    ))/sd(vdat$lcp    ),
  gleason =(vdat$gleason -mean(vdat$gleason))/sd(vdat$gleason),
  pgg45   =(vdat$pgg45   -mean(vdat$pgg45  ))/sd(vdat$pgg45  )
)

#Validation set - compute MSE
yhat_v<-predict.lars(lasso,newx=vdats,s=.4,mode='fraction',type='fit')$fit
lpsa_v<-vdat$lpsa
mean( (yhat_v-lpsa_v)^2   )

#What model is chosen by AIC BIC?
library(bestglm)
?bestglm
Xy<-data.frame(
  lcavol  =tdats$lcavol  ,
  lweight =tdats$lweight ,
  age     =tdats$age     ,
  lbph    =tdats$lbph    ,
  svi     =tdats$svi     ,
  lcp     =tdats$lcp     ,
  gleason =tdats$gleason ,
  pgg45   =tdats$pgg45 ,
  y       =lpsa)

bestglm(Xy,family=gaussian,IC='BIC',method='exhaustive')
bestglm(Xy,family=gaussian,IC='AIC',method='exhaustive')