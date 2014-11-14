############
# Minerva AL 
# Massed vs. spaced practice
# Matthew Crump Sep.2, 2013
############

#Requires these packages
#And the Functions listed below the main code which must be loaded first
############################################
library(lsa)

############################################
# MAIN MODELING SCRIPT
############################################

# Create inputs
###############
Cue<-c(rep(1,20))   # cue present all ones
nCue<-c(rep(0,20))  # cue absent all zeros
A<-c(Cue,nCue,nCue,nCue,nCue,nCue) # vectors of input patterns
B<-c(nCue,Cue,nCue,nCue,nCue,nCue)
C<-c(nCue,nCue,Cue,nCue,Cue,nCue)
D<-c(nCue,nCue,nCue,Cue,Cue,nCue)
S<-c(nCue,nCue,nCue,nCue,Cue,nCue)
X<-c(nCue,nCue,nCue,nCue,Cue,Cue)
TestItems<-matrix(c("A",1,20,"B",21,40),nrow=2,ncol=3,byrow=TRUE)
TestLength<-dim(TestItems)[1]

# Define Model parameters
#########################
LRate<-.9             # Learning Rate parameter, controls sampling noise, ranges between 0  and 1
simSubs<-100          # Number of simulated subjects to run
Ntrials<-50

#Run simulated subjects
########################
SubData<-array(0,dim=c(TestLength,simSubs,Ntrials+2))     #? could be garbage
for (subs in 1:simSubs){
  
  # Define trial Sequence
  ########################
  TrialSequence<-c(rep("A",25),rep("B",25)) # Fixed order trial sequence
  #TrialSequence<-c(sample(c(rep("A",25),rep("B",25))))   #Random order trial sequence
  Ntrials<-length(TrialSequence)            # number of trials in TrialSequence
  
  # Initialize memory
  ##################
  memory<-t(matrix(runif(120,-.1,.1)))                #adds one noisy memory trace
  memory<-rbind(memory,t(matrix(runif(120,-.1,.1))))  #adds another noisy memory trace
  Learning<-matrix(0,ncol=Ntrials+2,nrow=TestLength)  #Initialize matrices for storing learning results
  
  #Run the model
  ##############
  for(iter in 1:Ntrials){
    inputs<-eval(as.name(TrialSequence[iter]))  #Select input Stimulus from Trial Sequence
    echo<-getEcho(inputs,memory,1)              #generate echo from memory, 1 is recP for recency weight
    discrepancy<-inputs-echo                    #compute discrepancy between input and memory
    
    #Test the model with each cue and record similarity value
    #Provides learning curves
    ##########################################################
    for (hh in 1:TestLength){
      testEcho<-getEcho(eval(as.name(TestItems[hh,1])),memory,1)
      Learning[hh,iter]<-(testEcho[as.numeric(TestItems[hh,2]):as.numeric(TestItems[hh,3])]%*%Cue)/length(Cue)
    }
    
    #Update Memory matrix
    #Store discrepancy between probe and echo into memory, add noise
    ################################################################
    memory<-rbind(memory,discrepancy*rbinom(120,1,LRate))
  }
  
  #Immediate Learning Test
  #recP in getEcho set to 0
  ############################
  for (hh in 1:TestLength){
    testEcho<-getEcho(eval(as.name(TestItems[hh,1])),memory,0)
    Learning[hh,iter+1]<-(testEcho[as.numeric(TestItems[hh,2]):as.numeric(TestItems[hh,3])]%*%Cue)/length(Cue)
  }
  #Forgetting
  ######################
  pRemember<-.2           #range between 0 and 1, 0 means forget everything
  for(jj in 1:dim(memory)[1]){
    memory[jj,]<-memory[jj,]*rbinom(120,1,pRemember)
  }
  
  #Delayed Learning Test
  #recP in getEcho set to 0
  ############################
  for (hh in 1:TestLength){
    testEcho<-getEcho(eval(as.name(TestItems[hh,1])),memory,0)
    Learning[hh,iter+2]<-(testEcho[as.numeric(TestItems[hh,2]):as.numeric(TestItems[hh,3])]%*%Cue)/length(Cue)
  }
  
  #Store simulated subject results
  #################################  
  for (hh in 1:TestLength){
    SubData[hh,subs,]<-Learning[hh,]
  }
}               

#Generate means for Immediate and delayed tests in fixed order condition
###############################################
FixedResults<-c(mean(SubData[1,,51]),mean(SubData[2,,51]),mean(SubData[1,,52]),mean(SubData[2,,52]))



#####################
#####################
#RUN RANDOM SEQUENCES
#####################
#####################


#Run simulated subjects
########################
SubData<-array(0,dim=c(TestLength,simSubs,Ntrials+2))     #? could be garbage
for (subs in 1:simSubs){
  
  # Define trial Sequence
  ########################
  TrialSequence<-c(rep("A",25),rep("B",25)) # Fixed order trial sequence
  #TrialSequence<-c(sample(c(rep("A",25),rep("B",25))))   #Random order trial sequence
  Ntrials<-length(TrialSequence)            # number of trials in TrialSequence
  
  # Initialize memory
  ##################
  memory<-t(matrix(runif(120,-.1,.1)))                #adds one noisy memory trace
  memory<-rbind(memory,t(matrix(runif(120,-.1,.1))))  #adds another noisy memory trace
  Learning<-matrix(0,ncol=Ntrials+2,nrow=TestLength)  #Initialize matrices for storing learning results
  
  #Run the model
  ##############
  for(iter in 1:Ntrials){
    inputs<-eval(as.name(TrialSequence[iter]))  #Select input Stimulus from Trial Sequence
    echo<-getEcho(inputs,memory,1)              #generate echo from memory, 1 is recP for recency weight
    discrepancy<-inputs-echo                    #compute discrepancy between input and memory
    
    #Test the model with each cue and record similarity value
    #Provides learning curves
    ##########################################################
    for (hh in 1:TestLength){
      testEcho<-getEcho(eval(as.name(TestItems[hh,1])),memory,0)
      Learning[hh,iter]<-(testEcho[as.numeric(TestItems[hh,2]):as.numeric(TestItems[hh,3])]%*%Cue)/length(Cue)
    }
    
    #Update Memory matrix
    #Store discrepancy between probe and echo into memory, add noise
    ################################################################
    memory<-rbind(memory,discrepancy*rbinom(120,1,LRate))
    
    
  }
  
  #Immediate Learning Test
  #recP in getEcho set to 0
  ############################
  for (hh in 1:TestLength){
    testEcho<-getEcho(eval(as.name(TestItems[hh,1])),memory,0)
    Learning[hh,iter+1]<-(testEcho[as.numeric(TestItems[hh,2]):as.numeric(TestItems[hh,3])]%*%Cue)/length(Cue)
  }
  #Forgetting
  ######################
  pRemember<-.2           #range between 0 and 1, 0 means forget everything
  for(jj in 1:dim(memory)[1]){
    memory[jj,]<-memory[jj,]*rbinom(120,1,pRemember)
  }
  
  #Delayed Learning Test
  #recP in getEcho set to 0
  ############################
  for (hh in 1:TestLength){
    testEcho<-getEcho(eval(as.name(TestItems[hh,1])),memory,0)
    Learning[hh,iter+2]<-(testEcho[as.numeric(TestItems[hh,2]):as.numeric(TestItems[hh,3])]%*%Cue)/length(Cue)
  }
  
  #Store simulated subject results
  #################################  
  for (hh in 1:TestLength){
    SubData[hh,subs,]<-Learning[hh,]
  }
}               

#Generate means for Immediate and delayed tests in fixed order condition
###############################################
RandomResults<-c(mean(SubData[1,,51]),mean(SubData[2,,51]),mean(SubData[1,,52]),mean(SubData[2,,52]))

print(FixedResults)
print(RandomResults)


AllData<-matrix(c(colMeans(SubDataAA),colMeans(SubDataAB),colMeans(SubDataAC),colMeans(SubDataAD),colMeans(SubDataAS),colMeans(SubDataAX)),ncol=Ntrials,byrow=TRUE)
colorSet<-c("blue","red","green","magenta","grey","black")
matplot(t(AllData), type="p", pch=c(1,2,3,4,5,6),col=colorSet,ylab="Strength",xlab="trials")
legend("right",lty=1,legend=c("A","B","C","D","S","X"),cex=.7, pch=c(1,2,3,4,5,6),col=colorSet)













AllData<-matrix(c(colMeans(SubDataA),colMeans(SubDataB),colMeans(SubDataAB)),ncol=100,byrow=TRUE)
matplot(t(AllData))



colorSet<-c("blue","red","green","black","magenta","grey")

plot(colMeans(SubDataAB), xlab="Trials",ylab="X given A",main="Acquisition")

getEcho(inputMatrix[2,],memory)[1:20]%*%Cue/length(Cue)



#####################
##FUNCTION  LIST
#####################

#############################################################
# FUNCTION getEcho
# Retrieves an echo from memory
#
# probe = vector representing cue to memory
# mem = matrix representing memory traces
# recP = parameter weighting recency contribtion, 0 means none
#############################################################
getEcho<-function(probe,mem,recP) {
  simvals<-c()                                              # vector of similarities between probe and memory traces
  for(j in 1:dim(mem)[1]){                                  # Compute Similarities
    simvals[j]<-cosine(probe[1:100],mem[j,1:100])
  }
  echomat<-abs(mem)*(simvals^3)                             # Multiply memory traces by activation value
  echomat[echomat==Inf]<-0                                  # Set any Inf numbers to zero
  echo<-colSums(echomat*sign(mem))+runif(120,-.01,.01)      # Sum memory traces, add noise
  echo<-echo/max(abs(echo))                                 # Normalize Echo
  memsize<-dim(mem)[1]                                      # get total number of memory traces
  recencyVec<-mem[memsize,]/max(abs(mem[memsize,]))         # Normalize most recent memory trace
  newecho<-echo+(recencyVec*recP)                           # Combine "Long-term" echo with most recent trace
  newecho<-newecho/max(abs(newecho))                        # Normalize resulting echo
  return(newecho)
}

############################################################
#Timing test functions
############################################################
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
