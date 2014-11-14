MINERVA DISCREPANCY ENCODING
----------------------------

# Simple MINERVA Model without discrepancy encoding

This model creates 100 cues that could be presented to MINERVA. The model simulates a simple memory procedure where 50 words are encoded. 25 are encoded once, and 25 are encoded twice. MINERVA is tested with all of the 100 words. Simlilarity of the echo to the probe is computed for each of the probe condition (Double, New, Single). The following code runs the MINERVA model once.

The model requires this package and function

```r
library(lsa)
```

```
## Loading required package: Snowball
## Loading required package: RWeka
```

```r
library(ggplot2)
library(Crump)
getEcho2 <- function(probe, mem) {
    simvals <- c()  # vector of similarities between probe and memory traces
    for (j in 1:dim(mem)[1]) {
        # Compute Similarities
        simvals[j] <- cosine(probe[1:50], mem[j, 1:50])
    }
    echomat <- abs(mem) * (simvals^3)  # Multiply memory traces by activation value
    echomat[echomat == Inf] <- 0  # Set any Inf numbers to zero
    echo <- colSums(echomat * sign(mem)) + runif(50, -0.01, 0.01)  # Sum memory traces, add noise
    echo <- echo/max(abs(echo))  # Normalize Echo
    return(echo)
}
```

The MINERVA model runs as follows


```r
# Create background memory for 100 words one word is 50 units, sampled from
# a binomial distribution of 1s and -1s
BackgroundMemory <- rbinom(50 * 100, 1, 0.5)
BM <- matrix(BackgroundMemory, ncol = 50)
BM[BM == 0] <- -1

# present first 50 words during encoding phase Encoding probability refers
# to the clarity of the presented cue
EncodingProbability <- 0.5
PresentedWords <- matrix(ncol = 50, nrow = 50)
for (i in 1:50) PresentedWords[i, ] <- BM[i, ] * rbinom(50, 1, EncodingProbability)
for (i in 1:25) PresentedWords <- rbind(PresentedWords, BM[i, ] * rbinom(50, 
    1, EncodingProbability))

# compute similarity scores for all words in the test phase
sims <- c()
for (i in 1:100) sims <- c(sims, getEcho2(BM[i, ], PresentedWords) %*% BM[i, 
    ]/50)

# find mean similarity scores for each probe type
Double <- mean(sims[1:25])
Single <- mean(sims[26:50])
New <- mean(sims[51:100])

# create a graph
ItemType <- c("Double", "Single", "New")
Similarity <- c(Double, Single, New)
DfAllMeans <- data.frame(ItemType, Similarity)
qplot(ItemType, Similarity, data = DfAllMeans, stat = "identity")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


The figure shows that items presented twice during encoding have higher similarity values to the probe than items presented once, and both OLD items have higher similarity than the NEW items. 

# Simple MINERVA model with multiple simulated subjects


```r

Double <- c()
Single <- c()
New <- c()
NumSimulatedSubs <- 5
for (subs in 1:NumSimulatedSubs) {
    BackgroundMemory <- rbinom(50 * 100, 1, 0.5)
    BM <- matrix(BackgroundMemory, ncol = 50)
    BM[BM == 0] <- -1
    
    EncodingProbability <- 0.5
    PresentedWords <- matrix(ncol = 50, nrow = 50)
    for (i in 1:50) PresentedWords[i, ] <- BM[i, ] * rbinom(50, 1, EncodingProbability)
    for (i in 1:25) PresentedWords <- rbind(PresentedWords, BM[i, ] * rbinom(50, 
        1, EncodingProbability))
    
    sims <- c()
    for (i in 1:100) sims <- c(sims, getEcho2(BM[i, ], PresentedWords) %*% BM[i, 
        ]/50)
    
    Double <- c(Double, mean(sims[1:25]))
    Single <- c(Single, mean(sims[26:50]))
    New <- c(New, mean(sims[51:100]))
}

# Create dataframe
Similarity <- c(Double, Single, New)
Subjects <- rep(rep(1:NumSimulatedSubs), 3)
ItemType <- rep(c("Double", "Single", "New"), each = NumSimulatedSubs)
DfAllMeans <- data.frame(Subjects, ItemType, Similarity)

# create a graph
DfAllMeans2 <- aggregate(Similarity ~ ItemType, DfAllMeans, mean)
DfAllMeans3 <- aggregate(Similarity ~ ItemType, DfAllMeans, stde)
DfAllMeans2 <- data.frame(DfAllMeans2, SE = DfAllMeans3$Similarity)

limits <- aes(ymax = DfAllMeans2$Similarity + DfAllMeans2$SE, ymin = DfAllMeans2$Similarity - 
    DfAllMeans2$SE)
ggplot(data = DfAllMeans2, aes(y = Similarity, x = ItemType)) + geom_bar(stat = "identity", 
    position = position_dodge(), fill = "gray") + geom_errorbar(limits, width = 0.1, 
    size = 0.2, color = "black", position = position_dodge(0.9)) + theme_classic(base_size = 12) + 
    ylab("Similarity") + xlab("Memory Probe") + ggtitle("N=5, noise=.5")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


# Simple MINERVA model with multiple simulated subjects and varying the noise paramater


```r

Double <- c()
Single <- c()
New <- c()
NoiseP <- c(0.2, 0.4, 0.6, 0.8, 0.9, 0.99)
for (NP in NoiseP) {
    NumSimulatedSubs <- 10
    for (subs in 1:NumSimulatedSubs) {
        BackgroundMemory <- rbinom(50 * 100, 1, 0.5)
        BM <- matrix(BackgroundMemory, ncol = 50)
        BM[BM == 0] <- -1
        
        EncodingProbability <- NP
        PresentedWords <- matrix(ncol = 50, nrow = 50)
        for (i in 1:50) PresentedWords[i, ] <- BM[i, ] * rbinom(50, 1, EncodingProbability)
        for (i in 1:25) PresentedWords <- rbind(PresentedWords, BM[i, ] * rbinom(50, 
            1, EncodingProbability))
        
        sims <- c()
        for (i in 1:100) sims <- c(sims, getEcho2(BM[i, ], PresentedWords) %*% 
            BM[i, ]/50)
        
        Double <- c(Double, mean(sims[1:25]))
        Single <- c(Single, mean(sims[26:50]))
        New <- c(New, mean(sims[51:100]))
    }
}

# Create dataframe
Similarity <- c(Double, Single, New)
Subjects <- rep(rep(1:NumSimulatedSubs), 3 * 6)
ItemType <- rep(c("Double", "Single", "New"), each = NumSimulatedSubs * 6)
Noise <- rep(c(".2", ".4", ".6", ".8", ".9", ".99"), each = NumSimulatedSubs)
DfAllMeans <- data.frame(Subjects, ItemType, Similarity, Noise)

# create a graph
DfAllMeans2 <- aggregate(Similarity ~ ItemType * Noise, DfAllMeans, mean)
DfAllMeans3 <- aggregate(Similarity ~ ItemType * Noise, DfAllMeans, stde)
DfAllMeans2 <- data.frame(DfAllMeans2, SE = DfAllMeans3$Similarity)

limits <- aes(ymax = DfAllMeans2$Similarity + DfAllMeans2$SE, ymin = DfAllMeans2$Similarity - 
    DfAllMeans2$SE)
ggplot(data = DfAllMeans2, aes(y = Similarity, x = ItemType)) + geom_bar(stat = "identity", 
    position = position_dodge(), fill = "gray") + geom_errorbar(limits, width = 0.1, 
    size = 0.2, color = "black", position = position_dodge(0.9)) + theme_classic(base_size = 12) + 
    ylab("Similarity") + xlab("Memory Probe") + ggtitle("N=20,Noise=.2-.99") + 
    facet_wrap(~Noise)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


