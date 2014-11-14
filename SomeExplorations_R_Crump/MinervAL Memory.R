############
# Minerva AL 
# Discrepancy encoding memory effecdts
# Matthew Crump Nov 18, 2013
############

#Requires these packages
#And the Functions listed below the main code which must be loaded first
############################################
library(lsa)

############################################
# MAIN MODELING SCRIPT
############################################

# Create background memory for 100 words
# one word is 50 units, sampled from a binomial distribution of 1s and -1s
###############
BackgroundMemory<-rbinom(50*100,1,.5)
BM<-matrix(BackgroundMemory,ncol=50)
BM[BM==0]<--1

#create words to be presented during encoding
#Encoding probability refers to the clarity of the presented cue
EncodingProbability<-.7
PresentedWords<-matrix(ncol=50,nrow=75)
for(i in 1:50){
  PresentedWords[i,]<-BM[i,]*rbinom(50,1,EncodingProbability)
}
for(i in 1:25){
  PresentedWords[i+50,]<-BM[i,]*rbinom(50,1,EncodingProbability)
}

#compute similarity scores for all words in the test phase
sims<-c()
for(i in 1:100){
  sims<-c(sims,getEcho2(BM[i,],PresentedWords)%*%BM[i,]/50)
}

Double<-mean(sims[1:25])
Single<-mean(sims[26:50])
New<-mean(sims[51:100])

Labels<-c("Double","Single","New")
AllMeans<-c(Double,Single,New)
DfAllMeans<-data.frame(Labels,AllMeans)
plot(DfAllMeans)





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
  for (pp in 1:2){
  memory<-rbind(memory,t(matrix(runif(120,-.1,.1))))  #adds another noisy memory trace
  }
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
  #TrialSequence<-c(rep("A",25),rep("B",25)) # Fixed order trial sequence
  TrialSequence<-c(sample(c(rep("A",25),rep("B",25))))   #Random order trial sequence
  Ntrials<-length(TrialSequence)            # number of trials in TrialSequence
  
  # Initialize memory
  ##################
  memory<-t(matrix(runif(120,-.1,.1)))                #adds one noisy memory trace
  for (pp in 1:2){
    memory<-rbind(memory,t(matrix(runif(120,-.1,.1))))  #adds another noisy memory trace
  }
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

#Generate means for Immediate and delayed tests in random order condition
###############################################
RandomResults<-c(mean(SubData[1,,51]),mean(SubData[2,,51]),mean(SubData[1,,52]),mean(SubData[2,,52]))

#Print all results to screen
print(FixedResults)
print(RandomResults)


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

#############################################################
# FUNCTION getEcho
# Retrieves an echo from memory
#
# probe = vector representing cue to memory
# mem = matrix representing memory traces
# recP = parameter weighting recency contribtion, 0 means none
#############################################################
getEcho2<-function(probe,mem) {
  simvals<-c()                                              # vector of similarities between probe and memory traces
  for(j in 1:dim(mem)[1]){                                  # Compute Similarities
    simvals[j]<-cosine(probe[1:50],mem[j,1:50])
  }
  echomat<-abs(mem)*(simvals^3)                             # Multiply memory traces by activation value
  echomat[echomat==Inf]<-0                                  # Set any Inf numbers to zero
  echo<-colSums(echomat*sign(mem))+runif(50,-.01,.01)      # Sum memory traces, add noise
  echo<-echo/max(abs(echo))                                 # Normalize Echo
  return(echo)
}

