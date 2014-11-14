############
# Minerva AL 
# PC effects
# Matthew Crump Aug.30, 2013
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

CueSize<-25

Rs<-sign(runif(CueSize,-1,1))
Gs<-sign(runif(CueSize,-1,1))
Bs<-sign(runif(CueSize,-1,1))
Ys<-sign(runif(CueSize,-1,1))

#Color,Word,Congruency,ProportionCongruency
trials<-array(0,dim=c(1,6))
trials<-trials[-1,]
trials<-rbind(trials,c("Rs","Rs","Rs","C","high",1))
trials<-rbind(trials,c("Gs","Gs","Gs","C","high",2))
trials<-rbind(trials,c("Bs","Bs","Bs","C","high",3))
trials<-rbind(trials,c("Ys","Ys","Ys","C","high",4))
trials<-rbind(trials,c("Rs","Rs","Rs","C","high",1))
trials<-rbind(trials,c("Gs","Gs","Gs","C","high",2))
trials<-rbind(trials,c("Bs","Bs","Bs","C","high",3))
trials<-rbind(trials,c("Ys","Ys","Ys","C","high",4))
trials<-rbind(trials,c("Rs","Rs","Rs","C","high",1))
trials<-rbind(trials,c("Gs","Gs","Gs","C","high",2))
trials<-rbind(trials,c("Bs","Bs","Bs","C","high",3))
trials<-rbind(trials,c("Ys","Ys","Ys","C","high",4))
trials<-rbind(trials,c("Rs","Rs","Rs","C","high",1))
trials<-rbind(trials,c("Gs","Gs","Gs","C","high",2))
trials<-rbind(trials,c("Bs","Bs","Bs","C","high",3))
trials<-rbind(trials,c("Ys","Ys","Ys","C","high",4))
trials<-rbind(trials,c("Rs","Rs","Rs","C","high",1))
trials<-rbind(trials,c("Gs","Gs","Gs","C","high",2))
trials<-rbind(trials,c("Bs","Bs","Bs","C","high",3))
trials<-rbind(trials,c("Ys","Ys","Ys","C","high",4))
trials<-rbind(trials,c("Rs","Rs","Rs","C","high",1))
trials<-rbind(trials,c("Gs","Gs","Gs","C","high",2))
trials<-rbind(trials,c("Bs","Bs","Bs","C","high",3))
trials<-rbind(trials,c("Ys","Ys","Ys","C","high",4))
trials<-rbind(trials,c("Rs","Rs","Rs","C","high",1))
trials<-rbind(trials,c("Gs","Gs","Gs","C","high",2))
trials<-rbind(trials,c("Bs","Bs","Bs","C","high",3))
trials<-rbind(trials,c("Ys","Ys","Ys","C","high",4))
trials<-rbind(trials,c("Rs","Rs","Rs","C","high",1))
trials<-rbind(trials,c("Gs","Gs","Gs","C","high",2))
trials<-rbind(trials,c("Bs","Bs","Bs","C","high",3))
trials<-rbind(trials,c("Ys","Ys","Ys","C","high",4))
trials<-rbind(trials,c("Rs","Rs","Rs","C","high",1))
trials<-rbind(trials,c("Gs","Gs","Gs","C","high",2))
trials<-rbind(trials,c("Bs","Bs","Bs","C","high",3))
trials<-rbind(trials,c("Ys","Ys","Ys","C","high",4))
trials<-rbind(trials,c("Rs","Gs","Rs","I","high",1))
trials<-rbind(trials,c("Rs","Bs","Rs","I","high",1))
trials<-rbind(trials,c("Rs","Ys","Rs","I","high",1))
trials<-rbind(trials,c("Bs","Gs","Bs","I","high",3))
trials<-rbind(trials,c("Bs","Rs","Bs","I","high",3))
trials<-rbind(trials,c("Bs","Ys","Bs","I","high",3))
trials<-rbind(trials,c("Gs","Bs","Gs","I","high",2))
trials<-rbind(trials,c("Gs","Rs","Gs","I","high",2))
trials<-rbind(trials,c("Gs","Ys","Gs","I","high",2))
trials<-rbind(trials,c("Ys","Bs","Ys","I","high",4))
trials<-rbind(trials,c("Ys","Rs","Ys","I","high",4))
trials<-rbind(trials,c("Ys","Gs","Ys","I","high",4))

#Color,Word,Congruency,ProportionCongruency
trials<-array(0,dim=c(1,6))
trials<-trials[-1,]
trials<-rbind(trials,c("Rs","Rs","Rs","C","Equal",1))
trials<-rbind(trials,c("Gs","Gs","Gs","C","Equal",2))
trials<-rbind(trials,c("Bs","Bs","Bs","C","Equal",3))
trials<-rbind(trials,c("Ys","Ys","Ys","C","Equal",4))
trials<-rbind(trials,c("Rs","Rs","Rs","C","Equal",1))
trials<-rbind(trials,c("Gs","Gs","Gs","C","Equal",2))
trials<-rbind(trials,c("Bs","Bs","Bs","C","Equal",3))
trials<-rbind(trials,c("Ys","Ys","Ys","C","Equal",4))
trials<-rbind(trials,c("Rs","Rs","Rs","C","Equal",1))
trials<-rbind(trials,c("Gs","Gs","Gs","C","Equal",2))
trials<-rbind(trials,c("Bs","Bs","Bs","C","Equal",3))
trials<-rbind(trials,c("Ys","Ys","Ys","C","Equal",4))
trials<-rbind(trials,c("Rs","Gs","Rs","I","Equal",1))
trials<-rbind(trials,c("Rs","Bs","Rs","I","Equal",1))
trials<-rbind(trials,c("Rs","Ys","Rs","I","Equal",1))
trials<-rbind(trials,c("Bs","Gs","Bs","I","Equal",3))
trials<-rbind(trials,c("Bs","Rs","Bs","I","Equal",3))
trials<-rbind(trials,c("Bs","Ys","Bs","I","Equal",3))
trials<-rbind(trials,c("Gs","Bs","Gs","I","Equal",2))
trials<-rbind(trials,c("Gs","Rs","Gs","I","Equal",2))
trials<-rbind(trials,c("Gs","Ys","Gs","I","Equal",2))
trials<-rbind(trials,c("Ys","Bs","Ys","I","Equal",4))
trials<-rbind(trials,c("Ys","Rs","Ys","I","Equal",4))
trials<-rbind(trials,c("Ys","Gs","Ys","I","Equal",4))

#Color,Word,Congruency,ProportionCongruency
trials<-array(0,dim=c(1,6))
trials<-trials[-1,]
trials<-rbind(trials,c("Rs","Rs","Rs","C","low",1))
trials<-rbind(trials,c("Gs","Gs","Gs","C","low",2))
trials<-rbind(trials,c("Bs","Bs","Bs","C","low",3))
trials<-rbind(trials,c("Ys","Ys","Ys","C","low",4))
trials<-rbind(trials,c("Rs","Gs","Rs","I","low",1))
trials<-rbind(trials,c("Rs","Bs","Rs","I","low",1))
trials<-rbind(trials,c("Rs","Ys","Rs","I","low",1))
trials<-rbind(trials,c("Bs","Gs","Bs","I","low",3))
trials<-rbind(trials,c("Bs","Rs","Bs","I","low",3))
trials<-rbind(trials,c("Bs","Ys","Bs","I","low",3))
trials<-rbind(trials,c("Gs","Bs","Gs","I","low",2))
trials<-rbind(trials,c("Gs","Rs","Gs","I","low",2))
trials<-rbind(trials,c("Gs","Ys","Gs","I","low",2))
trials<-rbind(trials,c("Ys","Bs","Ys","I","low",4))
trials<-rbind(trials,c("Ys","Rs","Ys","I","low",4))
trials<-rbind(trials,c("Ys","Gs","Ys","I","low",4))


testResponse<-c("Rs","Gs","Bs","Ys")
testLength<-length(testResponse)




#TestItems<-matrix(c("A",1,20,"B",21,40),nrow=2,ncol=3,byrow=TRUE)
#TestLength<-dim(TestItems)[1]

# Define Model parameters
#########################
LRate<-.9             # Learning Rate parameter, controls sampling noise, ranges between 0  and 1
simSubs<-100          # Number of simulated subjects to run
Ntrials<-dim(trials)[1]

#Run simulated subjects
########################
SubData<-array(0,dim=c(1,1))    #? could be garbage
SubData<-SubData[-1,]

for (subs in 1:simSubs){
	
	#Randomize cues
	Rs<-sign(runif(CueSize,-1,1))
	Gs<-sign(runif(CueSize,-1,1))
	Bs<-sign(runif(CueSize,-1,1))
	Ys<-sign(runif(CueSize,-1,1))
  
  # Define trial Sequence
  ########################
  TrialSequence<-sample(sequence(Ntrials)) # Fixed order trial sequence
  #TrialSequence<-c(sample(c(rep("A",25),rep("B",25))))   #Random order trial sequence
  Ntrials<-length(TrialSequence)            # number of trials in TrialSequence
  
  # Initialize memory
  ##################
  memory<-t(matrix(runif(75,-.1,.1)))                #adds one noisy memory trace
  for (pp in 1:2){
  memory<-rbind(memory,t(matrix(runif(75,-.1,.1))))  #adds another noisy memory trace
  }
  Learning<-matrix(0,ncol=8,nrow=Ntrials)  #Initialize matrices for storing learning results
  
  #Run the model
  ##############
  for(iter in 1:Ntrials){
    inputs<-c(eval(as.name(trials[TrialSequence[iter],1])),eval(as.name(trials[TrialSequence[iter],2])),eval(as.name(trials[TrialSequence[iter],3])))  #Select input Stimulus from Trial Sequence
    echo<-getEcho(inputs,memory,0)              #generate echo from memory, 1 is recP for recency weight
    discrepancy<-inputs-echo                    #compute discrepancy between input and memory
    
    #Measure similarity between echo response and each possible response
    ##########################################################
    Learning[iter,1:6]<-trials[TrialSequence[iter],]
    PossibleResp<-vector()
     for (hh in 1:testLength){
     	PossibleResp[hh]<-echo[51:75]%*%eval(as.name(testResponse[hh]))/length(CueSize)
    }
   	Learning[iter,7]<-max(PossibleResp)
    if(which.max(PossibleResp)==trials[TrialSequence[iter],6]){
    Learning[iter,8]<-"Cor"
    }else{
    Learning[iter,8]<-"Inc"
    	}
    
    #Update Memory matrix
    #Store discrepancy between probe and echo into memory, add noise
    ################################################################
    memory<-rbind(memory,discrepancy*rbinom(75,1,LRate))
  }
  
  tempDataFrame<-data.frame(Learning)
  tResult<-aggregate(as.numeric(tempDataFrame$X7),list(Congruency=tempDataFrame$X4),mean)
  
  #Store simulated subject results
  #################################  
    SubData<-rbind(SubData,tResult$x)
    
}               

#Generate means for Immediate and delayed tests in fixed order condition
###############################################
colMeans(SubData)




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
    simvals[j]<-cosine(probe[1:50],mem[j,1:50])
  }
  echomat<-abs(mem)*(simvals^3)                             # Multiply memory traces by activation value
  echomat[echomat==Inf]<-0                                  # Set any Inf numbers to zero
  echo<-colSums(echomat*sign(mem))+runif(75,-.01,.01)      # Sum memory traces, add noise
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

