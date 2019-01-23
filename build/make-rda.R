## Convert data sets to R package format (.rda)
## Author: Andy J. Wills
## Licence: GPL 2.0+
library(tidyverse)

## Unsworth & Engle (2006a)

## Summary: Converted to long format. Test and length separated as
## variables. Non-benchmark data excluded.

rawd <- read_tsv("../BenchmarksWM.Data/BM1.1.SetsizeAccuracy/Unsworth.Engle.Listlength.txt")
un <- gather(rawd, worupc2:wmset5, key="comb", value="acc")
un$size <- as.numeric(substr(un$comb, nchar(un$comb), nchar(un$comb)))
un$test <- substr(un$comb, 1, nchar(un$comb)-1)
un <- un %>% select(subject, test, size, acc)
un$test <- recode(un$test, worupc = "word", letupc = "letter", opupc = "operation", rspupc = "reading")
un <- un %>% filter(test != "wmset") %>% filter(test != "stmset")
un <- un %>% arrange(subject, test, size)
unsworth06a <- un
save(unsworth06a, file = "../pkg/data/unsworth06a.rda")
