############
# Minerva AL
#modification to add deblurring
############
library(lsa)

#Create inputs
Cue<-c(rep(1,20))   #cue present all ones
nCue<-c(rep(0,20))  #cue absent all zeros
AX<-c(Cue,nCue,nCue,nCue,Cue,Cue) #vector of inputs
BX<-c(nCue,Cue,nCue,nCue,Cue,Cue)
CX<-c(nCue,nCue,Cue,nCue,Cue,Cue)
inputMatrix<-matrix(c(AX,BX),ncol=120,byrow=TRUE)

#Model parameters
LRate<-.7


##run simulated subjects
SubData<-matrix()
for (subs in 1:5){
  #Initialize memory
  memory<-t(matrix(runif(240,-.1,.1)))
  memory<-rbind(memory,t(matrix(runif(240,-.1,.1))))
  Learning<-matrix(0,ncol=100,nrow=2)
  ALearning<-matrix(0,ncol=100,nrow=6)
  #run the model
  for(iter in 1:100){
    randChoice<-round(runif(1,1,2))
    inputs<-inputMatrix[randChoice,]
    echo<-getEcho(inputs,memory) #generate echo from memory
    discrepancy<-inputs-echo     #compute discrepancy between input and memory
    ##Testing for learning
    for (iM in 1:dim(inputMatrix)[1]){
      testEcho<-getEcho(inputMatrix[iM,],memory)
      Learning[iM,iter]<-(testEcho[101:120]%*%Cue)/length(Cue)
    }
    testEcho<-getEcho(inputMatrix[1,],memory)
    ALearning[1,iter]<-(testEcho[1:20]%*%Cue)/length(Cue)
    ALearning[2,iter]<-(testEcho[21:40]%*%Cue)/length(Cue)
    ALearning[3,iter]<-(testEcho[41:60]%*%Cue)/length(Cue)
    ALearning[4,iter]<-(testEcho[61:80]%*%Cue)/length(Cue)
    ALearning[5,iter]<-(testEcho[81:100]%*%Cue)/length(Cue)
    ALearning[6,iter]<-(testEcho[101:120]%*%Cue)/length(Cue)
    #getEchoDeblur(echo,memory)
    memory<-rbind(memory,c(discrepancy[1:120]*rbinom(120,1,LRate),getEchoDeblur(echo,memory)[121:240])) #Store in memory
  }
  if (subs==1){
    SubDataA<-Learning[1,]
    SubDataB<-Learning[2,]
    SubDataAA<-ALearning[1,]
    SubDataAB<-ALearning[2,]
    SubDataAC<-ALearning[3,]
    SubDataAD<-ALearning[4,]
    SubDataAS<-ALearning[5,]
    SubDataAX<-ALearning[6,]
  }else{
    SubDataA<-rbind(SubDataA,Learning[1,])
    SubDataB<-rbind(SubDataB,Learning[2,])
    SubDataAA<-rbind(SubDataAA,ALearning[1,])
    SubDataAB<-rbind(SubDataAB,ALearning[2,])
    SubDataAC<-rbind(SubDataAC,ALearning[3,])
    SubDataAD<-rbind(SubDataAD,ALearning[4,])
    SubDataAS<-rbind(SubDataAS,ALearning[5,])
    SubDataAX<-rbind(SubDataAX,ALearning[6,])
  }
}


AllData<-matrix(c(colMeans(SubDataA),colMeans(SubDataB),colMeans(SubDataAB)),ncol=100,byrow=TRUE)
matplot(t(AllData))

AllData<-matrix(c(colMeans(SubDataAA),colMeans(SubDataAB),colMeans(SubDataAC),colMeans(SubDataAD),colMeans(SubDataAS),colMeans(SubDataAX)),ncol=100,byrow=TRUE)
colorSet<-c("blue","red","green","magenta","grey","black")
matplot(t(AllData), type="p", pch=c(1,2,3,4,5,6),col=colorSet,ylab="Strength",xlab="trials")
legend("right",lty=1,legend=c("A","B","C","D","S","X"),cex=.7, pch=c(1,2,3,4,5,6),col=colorSet)

colorSet<-c("blue","red","green","black","magenta","grey")

plot(colMeans(SubDataAB), xlab="Trials",ylab="X given A",main="Acquisition")

getEcho(inputMatrix[2,],memory)[1:20]%*%Cue/length(Cue)



#####################
##Functions
#####################

#retrieve an echo from memory
getEcho<-function(probe,mem) {
  #simvals<-cosine(t(rbind(probe[1:100],mem[,1:100])))[1,][-1]
  simvals<-c()
for(j in 1:dim(mem)[1]){
  simvals[j]<-cosine(c(probe[1:100],mem[dim(mem)[1],121:240]),c(mem[j,1:100],mem[j,121:240]))
}
  echomat<-abs(mem)^simvals
  echomat[echomat==Inf]<-0
  echo<-colSums(echomat*sign(mem))+runif(240,-.01,.01)
  echo<-echo/max(abs(echo))
  return(echo)
}

#retrieve an echo from memory
getEchoDeblur<-function(probe,mem) {
  #simvals<-cosine(t(rbind(probe[1:100],mem[,1:100])))[1,][-1]
  simvals<-c()
for(j in 1:dim(mem)[1]){
  simvals[j]<-cosine(probe[1:240],mem[j,1:240])
}
  echomat<-abs(mem)^simvals
  echomat[echomat==Inf]<-0
  echo<-colSums(echomat*sign(mem))+runif(240,-.01,.01)
  echo<-echo/max(abs(echo))
  return(echo)
}


