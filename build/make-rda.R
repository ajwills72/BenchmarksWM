## Convert data sets to R package format (.rda)
## Author: Andy J. Wills
## Licence: GPL 2.0+
library(tidyverse)
library(readxl)
############### BM1.1 #####################
pth  <- "../BenchmarksWM.Data/BM1.1.SetsizeAccuracy/"

## Issues w/ original R script:
## - Duplicate data sets (.xls and .txt).
## - BM1.1.SetsizeAccuracy.R:
##     - requires a directory "functions" which is absent
##     - assumes case-insentive filenames 

### Unsworth & Engle (2006a) ###

## Summary of changes:
## - Converted to long format.
## - Test and length separated as variables.
## - Simple/complex variable added to easily reproduce results as reported.
## - Variable levels given meaningful names.
## - Non-benchmark tests excluded.
fnam  <- paste0(pth, "Unsworth.Engle.Listlength.txt")
rawd <- read_tsv(fnam)
un <- gather(rawd, worupc2:wmset5, key="comb", value="acc")
un$size <- as.numeric(substr(un$comb, nchar(un$comb), nchar(un$comb)))
un$test <- substr(un$comb, 1, nchar(un$comb)-1)
un <- un %>% select(subject, test, size, acc)
un$test <- recode(un$test, worupc = "word", letupc = "letter",
                  opupc = "operation", rspupc = "reading")
un <- un %>% filter(test != "wmset") %>% filter(test != "stmset")
un <- un %>% arrange(subject, test, size)
un$type <- recode(un$test, word = "simple", letter = "simple",
                  operation = "complex", reading = "complex")
un <- un %>% select(subject, type, test, size, acc)
unsworth06a <- un
save(unsworth06a, file = "../pkg/data/unsworth06a.rda")
rm(un)

### Bunting et al. (2006) ###

## Summary of changes:
## - Converted to long format
## - Speed, span, and position, separated as variables
## - Variables given meaningful names
## - Handedness removed as all participants were right handed.

fnam  <- paste0(pth, "Bunting.Cowan.Running.xls")
rawd <- read_excel(fnam, sheet=3)
bu <- gather(rawd, f7sp7_ac:s1sp1_ac, key="comb", value="acc")
bu$speed <- substr(bu$comb, 1, 1)
bu$span <- as.integer(substr(bu$comb, 2, 2))
bu$pos <- as.integer(substr(bu$comb, 5, 5))
bu <- bu %>% select(Subject, Gender, speed, span, pos, acc)
colnames(bu) <- c("subject", "gender", "speed", "span", "pos", "acc")
bu$speed <- recode(bu$speed, f = "fast", s = "slow")
bu$subject <- as.integer(bu$subject)
bunting06 <- bu
save(bunting06, file = "../pkg/data/bunting06.rda")
rm(bu)

### McElree & Dosher (1989) ###
fnam  <- paste0(pth, "/mcelree89.csv")
mcelree89 <- read_csv(fnam)
save(mcelree89, file = "../pkg/data/mcelree89.rda")

### Jonides et al. (1997) ###
NbackPE <- c(0.03, 0.05, 0.065, 0.115)
acc  <- 1-NbackPE
back  <- 0:3
jonides97  <- data.frame(back, acc)
save(jonides97, file = "../pkg/data/jonides97.rda")

### Verhaeghen & Basak (2005)
NbackVerhaeghenY <- c(0.97, 0.96, 0.945, 0.92, 0.86)
acc  <- NbackVerhaeghenY
back  <- 1:5
verhaeghen05  <- data.frame(back, acc)
save(verhaeghen05, file = "../pkg/data/verhaeghen05.rda")

### Oberauer & Kliegel (2001)
fnam  <- paste0(pth, "/Oberauer.Kliegl.MU1.DAT")

## Load Part 1
colnames1 <- c("id", "setsize", "trial", "pt0", "pt1", "ptcat", "crit",
               "corrval1", "resp1", "correct1", "rt1", "corrval2", "resp2",
               "correct2", "rt2", "corrval3", "resp3", "correct3", "rt3",
               "corrval4", "resp4", "correct4", "rt4")
mutaf1 <- read.table(fnam, header=F, fill=T, col.names=colnames1) 

## Load Part 2
colnames2 <- c("id", "setsize", "trial", "pt0", "pt1", "ptcat", "crit",
               "corrval1", "resp1", "correct1", "rt1", "corrval2", "resp2",
               "correct2", "rt2", "corrval3", "resp3", "correct3", "rt3",
               "corrval4", "resp4", "correct4", "rt4", "corrval5", "resp5",
               "correct5", "rt5", "corrval6", "resp6", "correct6", "rt6")
fnam  <- paste0(pth, "/Oberauer.Kliegl.MU2.DAT")
mutaf2 <- read.table(fnam, header=F, fill=T, col.names=colnames2) 

## Label parts
mutaf1$exp = 1
mutaf2$exp = 2

## Remove setsize zero trials - not sure what these are...
mutaf1 <- mutaf1[mutaf1$setsize>0,]

## Filter to trials with maximum presentation duration (6s)
## and to young adults (id < 30)
mutaf1 <- mutaf1 %>% filter(pt0 > 5999) %>% filter(id < 30)
mutaf2 <- mutaf2 %>% filter(pt0 > 5999) %>% filter(id < 30)

## Select needed columns
mutaf1 <- mutaf1 %>% select(id, exp, setsize, trial, correct1, correct2,
                            correct3, correct4)

## Convert to long format and arrange order
mutaf1l <- mutaf1 %>%
    gather(key = "subtrial", value = "correct", correct1:correct4) %>%
    arrange(id, setsize, trial, subtrial)

mutaf2 <- mutaf2 %>% select(id, exp, setsize, trial, correct1, correct2,
                            correct3, correct4, correct5, correct6)


#############################
## Column numbers for columns whose labels include 'correct'
## i.e. those that contain accuracy scores
pcidx1 <- which(grepl("correct", colnames(mutaf1)))
pcidx2 <- which(grepl("correct", colnames(mutaf2)))

## Identify column containing setsize
ssidx <- which(colnames(mutaf1)=="setsize")



## Custom function to compute percent correct
computePC <- function(x) {
  setsize <- as.numeric(x[1])
  pcvector <- as.numeric(x[2:(setsize+1)])
  return(mean(pcvector))}

## Really inefficient way to work out %correct per trialk
mutaf1$PC <- NULL
for (j in 1:dim(mutaf1)[1]) {
  mutaf1[j,"PC"] <- computePC(mutaf1[j,c(ssidx, pcidx1)])
}

mutaf2$PC <- NULL
for (j in 1:dim(mutaf2)[1]) {
  mutaf2[j,"PC"] <- computePC(mutaf2[j,c(ssidx, pcidx2)])
}

## Weird way to subset to needed columns
mt1 <- mutaf1[, which(colnames(mutaf1) %in% c("id", "exp", "setsize", "pt0", "PC"))]
mt2 <- mutaf2[, which(colnames(mutaf2) %in% c("id", "exp", "setsize", "pt0", "PC"))]

## Combine
mutaf <- rbind(mt1, mt2)

## Select subj# < 30 (young adults)
## ... and also for the maximal presentation time of 6 seconds.
mt1.y.long <- subset(mt1, id < 30 & pt0 > 5999)
mt2.y.long <- subset(mt2, id < 30 & pt0 > 5999)

## Really inefficient way of getting accuracy by set size by subject
nsubj <- length(unique(mt1.y.long$id))
MUarray1 <- array(NA,dim=c(4,1,nsubj))
for (ss in 1:4) {
    d <- subset(mt1.y.long, setsize==ss)
    aggdat <- aggregate(PC ~ id, data=d, FUN=mean)
    MUarray1[ss,1,] <- aggdat$PC
}

nsubj <- length(unique(mt2.y.long$id))
MUarray2 <- array(NA,dim=c(3,1,nsubj))
for (ss in 4:6) {
  d <- subset(mt2.y.long, setsize==ss)
  aggdat <- aggregate(PC ~ id, data=d, FUN=mean)
  MUarray2[ss-3,1,] <- aggdat$PC
}
