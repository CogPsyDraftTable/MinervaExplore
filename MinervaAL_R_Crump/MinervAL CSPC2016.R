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
setwd("~/Dropbox/DraftTable/Projects/MinervaExplore/MinervaAL_R_Crump")
# Create inputs
###############

CueSize<-25


# 4 colors, 4 words, 4 responses, two locations, 1 word dimension, 1 color dimension
CueMatrix<-matrix(sign(runif(CueSize*16,-1,1)),ncol=CueSize,nrow=16)
#create Color vectors that are part feature general (color dimension) and specific (color value)
Cgeneral<-rbinom(25,1,.5)
Cspecific<-1-Cgeneral
CueMatrix[1:4,]<-sweep(CueMatrix[1:4,],MARGIN=2,Cspecific,'*')
CueMatrix[1:4,]<-sweep(CueMatrix[1:4,],MARGIN=2,CueMatrix[15,]*Cgeneral,'+')
#create Word vectors that are part feature general (Word dimension) and specific (Word value)
Wgeneral<-rbinom(25,1,.5)
Wspecific<-1-Wgeneral
CueMatrix[5:8,]<-sweep(CueMatrix[5:8,],MARGIN=2,Wspecific,'*')
CueMatrix[5:8,]<-sweep(CueMatrix[5:8,],MARGIN=2,CueMatrix[16,]*Wgeneral,'+')
CueMatrix<-rbind(CueMatrix,rep(0,CueSize))

#Create matrix for CSPC trials
CSPCtrials<-read.table(file="CSPCTrials.txt",header=TRUE,sep="\t")
CSPCmatrix<-matrix(rep(0,96*25*4),ncol=25*4,nrow=96)
for(i in 1:96){
  CSPCmatrix[i,]<-c(CueMatrix[CSPCtrials[i,]$MW,],
                CueMatrix[CSPCtrials[i,]$MC,],
                CueMatrix[CSPCtrials[i,]$ML,],
                CueMatrix[CSPCtrials[i,]$MR,])
}

#Create matrix to initialize word and color memory
InitTrials<-read.table(file="InitializeMemory.txt",header=TRUE,sep="\t")
#initialize Wword
WordInitmatrix<-matrix(rep(0,(16*6)*25*4),ncol=25*4,nrow=96)
cnt<-0
for(j in 1:6){
  for(i in 1:16){
    cnt<-cnt+1
    WordInitmatrix[cnt,]<-c(CueMatrix[InitTrials[i,]$MW,],
                        CueMatrix[InitTrials[i,]$MC,],
                        CueMatrix[InitTrials[i,]$ML,],
                        CueMatrix[InitTrials[i,]$MR,])
  }
}
#initialize Color
ColorInitmatrix<-matrix(rep(0,(16*1)*25*4),ncol=25*4,nrow=16)
cnt<-0
for(j in 1:1){
  for(i in 17:32){
    cnt<-cnt+1
    ColorInitmatrix[cnt,]<-c(CueMatrix[InitTrials[i,]$MW,],
                            CueMatrix[InitTrials[i,]$MC,],
                            CueMatrix[InitTrials[i,]$ML,],
                            CueMatrix[InitTrials[i,]$MR,])
  }
}

InitMem<-rbind(WordInitmatrix,ColorInitmatrix)



#TestItems<-matrix(c("A",1,20,"B",21,40),nrow=2,ncol=3,byrow=TRUE)
#TestLength<-dim(TestItems)[1]

# Define Model parameters
#########################
LRate<-.9             # Learning Rate parameter, controls sampling noise, ranges between 0  and 1
simSubs<-20          # Number of simulated subjects to run
Ntrials<-dim(trials)[1]

#Run simulated subjects
########################
AllSubData<-data.frame()

for (subs in 1:simSubs){
	
  # Define trial Sequence
  ########################
  TrialSequence<-sample(1:96,replace=FALSE) # Fixed order trial sequence
  #TrialSequence<-c(sample(c(rep("A",25),rep("B",25))))   #Random order trial sequence
  Ntrials<-length(TrialSequence)            # number of trials in TrialSequence
  
  # Initialize memory
  ##################
#   memory<-t(matrix(runif(75,-.1,.1)))                #adds one noisy memory trace
#   for (pp in 1:2){
#   memory<-rbind(memory,t(matrix(runif(75,-.1,.1))))  #adds another noisy memory trace
#   }
  
  memory<-InitMem
  randomStart<-matrix(runif(dim(memory)[1]*dim(memory)[2],-.01,.1),ncol=dim(memory)[2],nrow=dim(memory)[1])
  memory<-memory+randomStart
  SubData<-data.frame(stringsAsFactors=FALSE)
 # Learning<-matrix(0,ncol=8,nrow=Ntrials)  #Initialize matrices for storing learning results
  
  #Run the model
  ##############
  for(iter in 1:Ntrials){
   # inputs<-c(eval(as.name(trials[TrialSequence[iter],1])),eval(as.name(trials[TrialSequence[iter],2])),eval(as.name(trials[TrialSequence[iter],3])))  #Select input Stimulus from Trial Sequence
    inputs<-CSPCmatrix[TrialSequence[iter],]
    echo<-getEcho(inputs,memory,0)              #generate echo from memory, 1 is recP for recency weight
    discrepancy<-inputs-echo                    #compute discrepancy between input and memory
    
    #Measure similarity between echo response and each possible response
    ##########################################################
#     ResponseEval<-c(rep(0,4))
#     rcnt<-0
#     for(resp in 9:12){
#       rcnt<-rcnt+1
#       ResponseEval[rcnt]<-cosine(echo[76:100],CueMatrix[resp,])
#     }
    
    ResponseEval<- cosine(echo[76:100],t(CueMatrix[9:12,]))
ResponseEval<-data.frame(subject=subs,trial=iter,
                         red=ResponseEval[1],
           green=ResponseEval[2],
           blue=ResponseEval[3],
           yellow=ResponseEval[4])
  SubData<-rbind(SubData,data.frame(CSPCtrials[TrialSequence[iter],],ResponseEval))   
  
    
    #Update Memory matrix
    #Store discrepancy between probe and echo into memory, add noise
    ################################################################
    memory<-rbind(memory,discrepancy*rbinom(100,1,LRate))
  }
  
  #tempDataFrame<-data.frame(Learning)
 # tResult<-aggregate(as.numeric(tempDataFrame$X7),list(Congruency=tempDataFrame$X4),mean)
  
  #Store simulated subject results
  #################################  
    AllSubData<-rbind(AllSubData,SubData)
    
}               

#analyze model performance

dim(AllSubData)
MaxActivation<-apply(AllSubData[,15:18],1,max)
MaxChoice<-apply(AllSubData[,15:18],1,which.max)
Accuracy<-(AllSubData$MR-8)==MaxChoice
AllSubData<-cbind(AllSubData,MaxActivation,MaxChoice,Accuracy)

AllMeans<-aggregate(MaxActivation~trial*Congruency,AllSubData[AllSubData$Accuracy==TRUE,],mean)

ggplot(AllMeans,aes(x=trial, y=MaxActivation, group=Congruency))+
  geom_line()+
  geom_point(aes(shape=Congruency))+
  theme_classic(base_size=12)+theme(
    legend.position="top",
    legend.direction="horizontal",
    legend.title = element_blank()
  )

AllMeans<-aggregate(Accuracy~trial*Congruency,AllSubData,mean)

ggplot(AllMeans,aes(x=trial, y=Accuracy, group=Congruency))+
  geom_line()+
  geom_point(aes(shape=Congruency))+
  theme_classic(base_size=12)+theme(
    legend.position="top",
    legend.direction="horizontal",
    legend.title = element_blank()
  )

AllMeans<-aggregate(MaxActivation~PC*Congruency,AllSubData[AllSubData$Accuracy==TRUE,],mean)

ggplot(AllMeans,aes(x=PC, y=MaxActivation,group=Congruency))+
  geom_line()+
  geom_point(aes(shape=Congruency))+
  theme_classic(base_size=12)+theme(
    legend.position="top",
    legend.direction="horizontal",
    legend.title = element_blank()
  )

plot(AllMeans)
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
    simvals[j]<-cosine(probe[1:75],mem[j,1:75])
  }
  echomat<-abs(mem)*(simvals^3)                             # Multiply memory traces by activation value
  echomat[echomat==Inf]<-0                                  # Set any Inf numbers to zero
  echo<-colSums(echomat*sign(mem))+runif(100,-.01,.01)      # Sum memory traces, add noise
  echo<-echo/max(abs(echo))                                 # Normalize Echo
 # memsize<-dim(mem)[1]                                      # get total number of memory traces
  #recencyVec<-mem[memsize,]/max(abs(mem[memsize,]))         # Normalize most recent memory trace
  #newecho<-echo+(recencyVec*recP)                           # Combine "Long-term" echo with most recent trace
  #newecho<-newecho/max(abs(newecho))                        # Normalize resulting echo
  return(echo)
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

