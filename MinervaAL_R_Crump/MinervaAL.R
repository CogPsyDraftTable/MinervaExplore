############
# Minerva AL
############
library(lsa)

#Create inputs
Cue<-c(rep(1,20))   #cue present all ones
nCue<-c(rep(0,20))  #cue absent all zeros
inputs<-c(Cue,nCue,nCue,nCue,Cue,Cue) #vector of inputs

#Model parameters
LRate<-.3


##run simulated subjects
SubData<-matrix()
for (subs in 1:10){
  #Initialize memory
  memory<-t(matrix(runif(120,-.1,.1)))
  memory<-rbind(memory,t(matrix(runif(120,-.1,.1))))
  Learning<-c()
  #run the model
  for(iter in 1:100){
    echo<-getEcho(inputs,memory) #generate echo from memory
    discrepancy<-inputs-echo     #compute discrepancy between input and memory
    testEcho<-getEcho(inputs,memory)
    Learning[iter]<-(testEcho[101:120]%*%inputs[101:120])/length(inputs[101:120])
    memory<-rbind(memory,discrepancy*rbinom(120,1,LRate)) #Store in memory
  }
  if (subs==1){
    SubData<-Learning
  }else{
  SubData<-rbind(SubData,Learning)
  }
}
plot(colMeans(SubData), xlab="Trials",ylab="X given A",main="Acquisition")



#####################
##Functions
#####################

#retrieve an echo from memory
getEcho<-function(probe,mem) {
  #simvals<-cosine(t(rbind(probe[1:100],mem[,1:100])))[1,][-1]
  simvals<-c()
for(j in 1:dim(mem)[1]){
  simvals[j]<-cosine(probe[1:100],mem[j,1:100])
}
  echo<-colSums((abs(mem)^simvals)*sign(mem))+runif(120,-.01,.01)
  echo<-echo/max(abs(echo))
  return(echo)
}



tic <- function(gcFirst = TRUE, type=c("elapsed", "user.self", "sys.self"))
{
   type <- match.arg(type)
   assign(".type", type, envir=baseenv())
   if(gcFirst) gc(FALSE)
   tic <- proc.time()[type]         
   assign(".tic", tic, envir=baseenv())
   invisible(tic)
}

toc <- function()
{
   type <- get(".type", envir=baseenv())
   toc <- proc.time()[type]
   tic <- get(".tic", envir=baseenv())
   print(toc - tic)
   invisible(toc)
}