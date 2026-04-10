# Clear your work space before starting a new analysis#
rm(list = ls()) 

# Clear console
cat("\f")

library(cna)
library(causalHyperGraph)
library(frscore)

# PRESENCE OF LOW LIFE EXPECTANCY
ZZ <- LIFE_EXPECTANCY_CNA_4.1.26_HAScholar
names(ZZ)

LE <- ZZ[, c(2:4,6)]
names(LE)

#RETURNS:  HIGH_LOW_BIRTH_WEIGHT*HIGH_UNEMPLOYMENT + HIGH_LOW_BIRTH_WEIGHT*HIGH_SHOOTINGS + HIGH_UNEMPLOYMENT*HIGH_SHOOTINGS <-> LOW_LIFE_EXPECTANCY   1   1          6

TK <- cna(LE, con = 1, cov = 1,  details = c("ex","fa", "PAcon", "PACcov"), outcome = "LOW_LIFE_EXPECTANCY",
          suff.only = F, strict = TRUE, maxstep=c(7,7,20))
TK

plot(TK)

frscored_cna(LE, type = "cs", fit.range = c(1, 0.75), granularity = 0.05, details = c("ex","fa", "PAcon", "PACcov"),
             normalize = "truemax", outcome = "LOW_LIFE_EXPECTANCY", strict = TRUE, print.all = TRUE)

#CAUSAL CHAIN

LE <- ZZ[, c(2:6)]
names(LE)
TK <- cna(LE, con = .96, cov = .96,  details = c("ex","fa", "PAcon", "PACcov"), ordering = "HIGH_RESIDENTIAL_SEGREGATION < HIGH_SHOOTINGS, HIGH_LOW_BIRTH_WEIGHT, HIGH_UNEMPLOYMENT < LOW_LIFE_EXPECTANCY", 
          outcome = c("HIGH_RESIDENTIAL_SEGREGATION","HIGH_LOW_BIRTH_WEIGHT", "HIGH_UNEMPLOYMENT", "HIGH_SHOOTINGS", "LOW_LIFE_EXPECTANCY"), suff.only = F, strict = TRUE, maxstep=c(7,7,20))
TK
plot(TK)

#RETURNS: (HIGH_RESIDENTIAL_SEGREGATION<->HIGH_LOW_BIRTH_WEIGHT)*(HIGH_LOW_BIRTH_WEIGHT*HIGH_UNEMPLOYMENT+HIGH_LOW_BIRTH_WEIGHT*HIGH_SHOOTINGS+HIGH_UNEMPLOYMENT*HIGH_SHOOTINGS<->LOW_LIFE_EXPECTANCY)
#AS SOLUTION #6

frscored_cna(LE, type = "cs", fit.range = c(1, 0.75), granularity = 0.01, details = c("ex","fa", "PAcon", "PACcov"), outcome = c("HIGH_LOW_BIRTH_WEIGHT", "HIGH_UNEMPLOYMENT", "HIGH_SHOOTINGS", "LOW_LIFE_EXPECTANCY"),
             normalize = "truemax", ordering = "HIGH_RESIDENTIAL_SEGREGATION < HIGH_SHOOTINGS, HIGH_LOW_BIRTH_WEIGHT, HIGH_UNEMPLOYMENT < LOW_LIFE_EXPECTANCY", strict = TRUE, print.all = TRUE)

frscored_cna(LE, type = "cs", fit.range = c(1.0, 0.75), granularity = 0.01, details = c("ex","fa", "PAcon", "PACcov"), 
             normalize = "truemax", ordering = "HIGH_RESIDENTIAL_SEGREGATION < HIGH_SHOOTINGS, HIGH_LOW_BIRTH_WEIGHT, HIGH_UNEMPLOYMENT < LOW_LIFE_EXPECTANCY", strict = FALSE, print.all = TRUE)

