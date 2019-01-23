## Convert data sets to R package format (.rda)
## Author: Andy J. Wills
## Licence: GPL 2.0+
library(tidyverse)
library(readxl)
############### BM1.1 #####################

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

rawd <- read_tsv("../BenchmarksWM.Data/BM1.1.SetsizeAccuracy/Unsworth.Engle.Listlength.txt")
un <- gather(rawd, worupc2:wmset5, key="comb", value="acc")
un$size <- as.numeric(substr(un$comb, nchar(un$comb), nchar(un$comb)))
un$test <- substr(un$comb, 1, nchar(un$comb)-1)
un <- un %>% select(subject, test, size, acc)
un$test <- recode(un$test, worupc = "word", letupc = "letter", opupc = "operation", rspupc = "reading")
un <- un %>% filter(test != "wmset") %>% filter(test != "stmset")
un <- un %>% arrange(subject, test, size)
un$type <- recode(un$test, word = "simple", letter = "simple", operation = "complex", reading = "complex")
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

rawd <- read_excel("../BenchmarksWM.Data/BM1.1.SetsizeAccuracy/Bunting.Cowan.Running.xls",
                   sheet=3)
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
