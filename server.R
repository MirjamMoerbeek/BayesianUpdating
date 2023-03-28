library(shiny)

function(input, output) {
  
  results=readRDS("results.rds")

  output$histN <- renderPlot({
  HypSet=as.integer(input$HypSet)
  if(HypSet==3)
  ES=as.integer(input$ES3)
  else(ES=as.integer(input$ES))
  BFtarget=as.integer(input$BFtarget)
  fraction=as.integer(input$fraction)
  type=as.integer(input$type)
  Nmin=as.integer(input$Nmin)
  Nmax=as.integer(input$Nmax)

  batch=100000*HypSet+10000*ES+1000*BFtarget+100*fraction+10*type+1*Nmin
    
  selection=results[results[,1]==batch,]
  N=selection[,Nmax+5]
  BF=selection[,Nmax+1]
  
  BFtarget=c(3,5,10,20)[BFtarget]
  Nmax=c(50,100,200,50000)[Nmax]
  if(Nmax==50) stepsize=1
  if(Nmax==100) stepsize=5
  if(Nmax==200) stepsize=5
  if(Nmax==50000) {Nmax=max(N)
  stepsize=Nmax/100}
  
  inconclusive=FALSE
  if(length(BF[BF>1/BFtarget&BF<BFtarget])>0)
  inconclusive=TRUE

  if(inconclusive==TRUE)
  maxheight=max(c(hist(N[BF>BFtarget],breaks=seq(0,Nmax,stepsize),plot=FALSE)$counts,hist(N[BF<BFtarget&BF>1/BFtarget],breaks=seq(0,Nmax,stepsize),plot=FALSE)$counts,hist(N[BF<1/BFtarget],breaks=seq(0,Nmax,stepsize),plot=FALSE)$counts))
  else  
    maxheight=max(c(hist(N[BF>BFtarget],breaks=seq(0,Nmax,stepsize),plot=FALSE)$counts,hist(N[BF<1/BFtarget],breaks=seq(0,Nmax,stepsize),plot=FALSE)$counts))
  
  par(mfrow=c(1,3))

  if(ES>1){
  
  hist(N[BF<1/BFtarget],xlim=c(0,Nmax),ylim=c(0,maxheight),breaks=seq(0,Nmax,stepsize),col=rgb(0.8,0.8,0.8,0.5),xlab="Group size",main="BF < 1/BFtarget (incorrect hypothesis favoured)",cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
  hist(N[BF>1/BFtarget&BF<BFtarget],xlim=c(0,Nmax),ylim=c(0,maxheight),breaks=seq(0,Nmax,stepsize),col=rgb(0.8,0.8,0.8,0.5),xlab="Group size",main="1/BFtarget < BF<BFtarget (neither hypothesis favoured)",cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
  hist(N[BF>BFtarget],xlim=c(0,Nmax),ylim=c(0,maxheight),breaks=seq(0,Nmax,stepsize), col=rgb(0.1,0.1,0.1,0.5),xlab="Group size",main="BF > BFtarget (correct hypothesis favoured)",cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
  }
  
  if(ES==1){
    hist(N[BF<1/BFtarget],xlim=c(0,Nmax),ylim=c(0,maxheight),breaks=seq(0,Nmax,stepsize),col=rgb(0.8,0.8,0.8,0.5),xlab="Group size",main="BF < 1/BFtarget (correct hypothesis favoured)",cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
    hist(N[BF>1/BFtarget&BF<BFtarget],xlim=c(0,Nmax),ylim=c(0,maxheight),breaks=seq(0,Nmax,stepsize),col=rgb(0.8,0.8,0.8,0.5),xlab="Group size",main="1/BFtarget < BF < BFtarget (neither hypothesis favoured)",cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
    hist(N[BF>BFtarget],xlim=c(0,Nmax),ylim=c(0,maxheight),breaks=seq(0,Nmax,stepsize), col=rgb(0.1,0.1,0.1,0.5),xlab="Group size",main="BF > BFtarget (incorrect hypothesis favoured)",cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
  }

  })
  
  
  
  output$histBF <- renderPlot({
    HypSet=as.integer(input$HypSet)
    if(HypSet==3)
      ES=as.integer(input$ES3)
    else(ES=as.integer(input$ES))
    BFtarget=as.integer(input$BFtarget)
    fraction=as.integer(input$fraction)
    type=as.integer(input$type)
    Nmin=as.integer(input$Nmin)
    Nmax=as.integer(input$Nmax)
    
    batch=100000*HypSet+10000*ES+1000*BFtarget+100*fraction+10*type+1*Nmin
    
    selection=results[results[,1]==batch,]
    N=selection[,Nmax+5]
    BF=selection[,Nmax+1]
    
    BFtarget=c(3,5,10,20)[BFtarget]
    
    
    hist(log10(BF),col="grey",xaxt="n",xlab="Bayes Factor",breaks=500,main="",xlim=c(-3.5,3.5))
    axis(1, at= c(-3,-2,-1,0,1,2,3),labels=c("1/1000","1/100","1/10","1","10","100","1000"), col.axis="black", las=2)
    abline(v=c(log10(1/BFtarget),log10(BFtarget)),lty=2,col="red")
    
  })
  
  
  output$meanN <- renderText({ 
    HypSet=as.integer(input$HypSet)
    if(HypSet==3)
      ES=as.integer(input$ES3)
    else(ES=as.integer(input$ES))
    BFtarget=as.integer(input$BFtarget)
    fraction=as.integer(input$fraction)
    type=as.integer(input$type)
    Nmin=as.integer(input$Nmin)
    Nmax=as.integer(input$Nmax)
    
    batch=100000*HypSet+10000*ES+1000*BFtarget+100*fraction+10*type+1*Nmin
    
    selection=results[results[,1]==batch,]
    N=selection[,Nmax+5]
    meanN=mean(N)
    
    paste("Mean group size: ", meanN)
  })
  
  
  output$medianN <- renderText({ 
    HypSet=as.integer(input$HypSet)
    if(HypSet==3)
      ES=as.integer(input$ES3)
    else(ES=as.integer(input$ES))
    BFtarget=as.integer(input$BFtarget)
    fraction=as.integer(input$fraction)
    type=as.integer(input$type)
    Nmin=as.integer(input$Nmin)
    Nmax=as.integer(input$Nmax)
    
    batch=100000*HypSet+10000*ES+1000*BFtarget+100*fraction+10*type+1*Nmin
    
    selection=results[results[,1]==batch,]
    N=selection[,Nmax+5]
    medianN=median(N)
    
    paste("Median group size: ", medianN)
  })
  
  
  output$maxN <- renderText({ 
    HypSet=as.integer(input$HypSet)
    if(HypSet==3)
      ES=as.integer(input$ES3)
    else(ES=as.integer(input$ES))
    BFtarget=as.integer(input$BFtarget)
    fraction=as.integer(input$fraction)
    type=as.integer(input$type)
    Nmin=as.integer(input$Nmin)
    Nmax=as.integer(input$Nmax)
    
    batch=100000*HypSet+10000*ES+1000*BFtarget+100*fraction+10*type+1*Nmin
    
    selection=results[results[,1]==batch,]
    N=selection[,Nmax+5]
    maxN=max(N)
    
    paste("Maximum group size: ", maxN)
  })
  
  
  output$percnonerror <- renderText({ 
    HypSet=as.integer(input$HypSet)
    if(HypSet==3)
      ES=as.integer(input$ES3)
    else(ES=as.integer(input$ES))
    BFtarget=as.integer(input$BFtarget)
    fraction=as.integer(input$fraction)
    type=as.integer(input$type)
    Nmin=as.integer(input$Nmin)
    Nmax=as.integer(input$Nmax)
    
    batch=100000*HypSet+10000*ES+1000*BFtarget+100*fraction+10*type+1*Nmin
    
    selection=results[results[,1]==batch,]
    N=selection[,Nmax+5]
    maxN=max(N)
    BF=selection[,Nmax+1]
    
    BFtarget=c(3,5,10,20)[BFtarget]
    
    
    if(ES==1)
      percnonerror=100*length(BF[BF<1/BFtarget])/5000
    else
      percnonerror=100*length(BF[BF>BFtarget])/5000
    percnonerror=round(percnonerror,2)
    paste("Percentage data sets for which correct hypothesis is favoured: ", percnonerror)
  })
  
  
  output$percerror <- renderText({ 
    HypSet=as.integer(input$HypSet)
    if(HypSet==3)
      ES=as.integer(input$ES3)
    else(ES=as.integer(input$ES))
    BFtarget=as.integer(input$BFtarget)
    fraction=as.integer(input$fraction)
    type=as.integer(input$type)
    Nmin=as.integer(input$Nmin)
    Nmax=as.integer(input$Nmax)
    
    batch=100000*HypSet+10000*ES+1000*BFtarget+100*fraction+10*type+1*Nmin
    
    selection=results[results[,1]==batch,]
    N=selection[,Nmax+5]
    maxN=max(N)
    BF=selection[,Nmax+1]
    
    BFtarget=c(3,5,10,20)[BFtarget]
    
    
    if(ES==1)
      percerror=100*length(BF[BF>BFtarget])/5000
    else
      percerror=100*length(BF[BF<1/BFtarget])/5000
    percerror=round(percerror,2)
    paste("Percentage data sets for which incorrect hypothesis is favoured: ", percerror)
  })
  
  
  output$percinconclusive <- renderText({ 
    HypSet=as.integer(input$HypSet)
    if(HypSet==3)
      ES=as.integer(input$ES3)
    else(ES=as.integer(input$ES))
    BFtarget=as.integer(input$BFtarget)
    fraction=as.integer(input$fraction)
    type=as.integer(input$type)
    Nmin=as.integer(input$Nmin)
    Nmax=as.integer(input$Nmax)
    
    batch=100000*HypSet+10000*ES+1000*BFtarget+100*fraction+10*type+1*Nmin
    
    selection=results[results[,1]==batch,]
    N=selection[,Nmax+5]
    maxN=max(N)
    BF=selection[,Nmax+1]
    
    BFtarget=c(3,5,10,20)[BFtarget]
    
    
   
      percinconclusive=100*(1-length(BF[BF>BFtarget])/5000-length(BF[BF<1/BFtarget])/5000)
      percinconclusive=round(percinconclusive,2)
   
    paste("Percentage data sets for which neither hypothesis is favoured: ", percinconclusive)
  })
  
  

}