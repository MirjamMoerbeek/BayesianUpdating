framingham <- read.delim("C:/Users/moerb101/surfdrive/Onderzoek - sequential Bayes/Example BF trajectory/framingham.txt") ### read data file

library(bain) ### load bain package

male=framingham[1:669,1] ### cholesterol values for males
female=framingham[670:1406,1] ### cholesterol values for females
set.seed(16352) ### set random seed
male=sample(male) ### put cholesterol values for males in random order
female=sample(female) ### put cholesterol values for females in random order


### analysis based on first 50 males and first 50 females
x=male[1:50] ###  first 50 males
y=female[1:50] ###  first 50 females
ttest=t_test(x,y,paired=FALSE,var.equal=FALSE) ### perform Welch's test
results=bain(ttest,"x=y;x<y",fraction=1) ### calculate Bayes Factor
results
results$BFmatrix ### Bayes Factor matrix
summary(results)


### Bayesian Updating

BFtarget=10 ### target Bayes value
Nmin=20 ### minimum sample size
x=male[1:Nmin] ### group size males is minimum group size
y=female[1:Nmin] ### group size mfeales is minimum group size

ttest=t_test(x,y,paired=FALSE,var.equal=FALSE) ### perform Welch's test
results=bain(ttest,"x=y;x<y") ### calculate Bayes Factor
BF=results$BFmatrix[2,1] ### Bayes Factor

data=c(Nmin,BF)
N=Nmin

while(BF<BFtarget&BF>1/BFtarget)
{
  N=N+1 ### increases sample size by 1
  x=male[1:N] ### sample males
  y=female[1:N] ### sample females
  ttest=t_test(x,y,paired=FALSE,var.equal=FALSE) ### perform Welch's test
  results=bain(ttest,"x=y;x<y",fraction=1) ### calculate Bayes Factor
  BF=results$BFmatrix[2,1] ### Bayes Factor
  print(c(length(x),BF))
  data=rbind(data,(c(length(x),BF)))
}

### plot trajectory of Bayes
plot(data[,1],log10(data[,2]),type="l",yaxt="n",xlim=c(0,200),ylim=c(log10(1/30),log10(30)),xlab="Number of subjects per gender",ylab="Bayes Factor",cex.lab=1.25,cex.axis=1.25)
axis(2, at= c(-log10(30), -log10(10), -log10(3), log10(1), log10(3), log10(10), log10(30)),
     labels=c("1/30", "1/10", "1/3", "1", "3", "10", "30"), col.axis="black", las=2,cex.axis=1.25)
abline(h=c(-1*log10(BFtarget),log10(BFtarget)),col="red",lty=2)


