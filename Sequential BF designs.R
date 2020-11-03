  library(bain)

  ################################################################################################################################################################################
  ### simulate trials
  ################################################################################################################################################################################
  
  
  nr.trials=5000  ### number of trials to generate
  
  ### specify scenario
  HypSet=1 # set of hypotheses. 
  means=c(0,0.2) #  means
  variances=c(1,1)
  BFtarget=5 # target BF
  fraction=3 # fraction to be used for prior
  Nmin=20 # minimal sample size
  Nmax=50 # maximum sample size
  
  set.seed(12345)
  
  ### create vectors to store output
  BF.out=rep(NA,nr.trials)
  N.out=rep(NA,nr.trials)
  
  for(ii in 1:nr.trials)
    {
    print(ii)
    # two populations to sample from
    pop.x=rnorm(100000,means[1],variances[1])
    pop.y=rnorm(100000,means[2],variances[2])


    ### make certain to enter the while loop by using BF=1 (even though this may not be the true value)
	  N.x=Nmin-1
	  BF=1
	    
    ### increase sample size if needed
    while(BF<BFtarget & BF>1/BFtarget&N.x<Nmax )
      {
      # add observations per group
      if(N.x<50000) N.step=50
      if(N.x<5000) N.step=20
      if(N.x<2500) N.step=10
      if(N.x<1000) N.step=5
      if(N.x<100) N.step=1
      N.x=N.x+N.step

      x=pop.x[1:N.x]	
      y=pop.y[1:N.x]	

      if(variances[1]==variances[2])
      ttest=t_test(x,y,paired=FALSE,var.equal=TRUE)
      else
      ttest=t_test(x,y,paired=FALSE,var.equal=FALSE)
      
      if(HypSet==1)
      {
        results=bain(ttest,"x=y;x<y",fraction=fraction)
        BF=results$BFmatrix[2,1] # Bayes Factor
      }
      if(HypSet==2)
      {
        results=bain(ttest,"x=y",fraction=fraction)
        BF=1/results$fit[1,7] # Bayes Factor
      }
      if(HypSet==3)
      {
        results=bain(ttest,"x>y;x<y",fraction=fraction)
        BF=results$BFmatrix[2,1] # Bayes Factor
      }
      
	  BF.out[ii]=BF
	  N.out[ii]=N.x

    }
  }

  
  ################################################################################################################################################################################
  ### results statistics
  ################################################################################################################################################################################

  ### note for interpretation 
  ### if data are generated under first hypothesis of each set, then the true hypothesis gets most support from the data if BF<1/BFtarget
  ### if data are generated under second hypothesis of each set, then the true hypothesis gets most support from the data if BF>BFtarget
  ### if BF in between BFtarget and 1/BFtarget (i.e. if maximum group size reached), then inconclusive
  
  
  100*length(BF.out[BF.out<1/BFtarget])/length(BF.out) #percentage BF < 1/targetBF
  100*length(BF.out[BF.out>BFtarget])/length(BF.out) # percentage BF > targetBF
  100*(1-length(BF.out[BF.out>BFtarget])/length(BF.out)-length(BF.out[BF.out<1/BFtarget])/length(BF.out)) # percentage inconclusive
  mean(BF.out) # mean BF
  median(BF.out) # median BF
  round(mean(N.out)) # mean N
  median(N.out) # median N
  max(N.out) # maximum N
  
  ################################################################################################################################################################################
  ### results plots
  ################################################################################################################################################################################

  ### three histograms of N (one for BF>BFtarget and one for BF<1/BFtarget and one for 1/BFtarget<BF<BFtarget)
  par(mfrow=c(1,3))
  Nmax=max(N.out)+50
  maxheight=max(c(hist(N.out[BF.out>BFtarget],breaks=seq(0,Nmax,by=50),plot=FALSE)$counts,hist(N.out[BF.out<1/BFtarget],breaks=seq(0,Nmax,by=50),plot=FALSE)$counts,hist(N.out[BF.out>1/BFtarget&BF.out<BFtarget],breaks=seq(0,Nmax,by=50),plot=FALSE)$counts))
  hist(N.out[BF.out<1/BFtarget],xlim=c(0,Nmax),ylim=c(0,maxheight),breaks=seq(0,Nmax,by=10),xlab="sample size",main="BF<1/BFtarget")
  hist(N.out[BF.out>1/BFtarget&BF.out<BFtarget],xlim=c(0,Nmax),ylim=c(0,maxheight),breaks=seq(0,Nmax,by=10),xlab="sample size",main="1/BFtarget<BF<BFtarget")
  hist(N.out[BF.out>BFtarget],xlim=c(0,Nmax),ylim=c(0,maxheight),breaks=seq(0,Nmax,by=10),xlab="sample size",main="BF>BFtarget")

  ### histogram of BF (vertical red lines are 1/BFtarget and BFtarget)
  par(mfrow=c(1,1))
  hist(log10(BF.out),col="grey",xaxt="n",xlab="",xlim=c(-3,3),main="",breaks=100)
  axis(1, at= c(-3,-2,-1,0,1,2,3),labels=c("1/1000","1/100","1/10","1","10","100","1000"),col.axis="black", las=2)
  abline(v=c(log10(1/BFtarget),log10(BFtarget)),lty=2,col="red")
  
