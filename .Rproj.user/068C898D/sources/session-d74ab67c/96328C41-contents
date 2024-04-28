#Party Families
#Group N

#table(ALL$charfamily)
#SOC: 2940
#RADRT: 911
#RADLEFT: 891
#LIB: 1791
#GREEN:845
#CON:2213

#####
#Belief Network Estimation
#####

#Loading additional libraries
library(NetworkComparisonTest)
library(qgraph)
library(bootnet)
library(RSVGTipsDevice)
RADRT <- ICDATA %>% filter(family == "RR")
RADLEFT <- ICDATA %>% filter(family == "RLeft")
#GREEN <- ALL %>% filter(charfamily == "GREEN")

#CON <- ALL %>% filter(charfamily == "CON")
#LIB <- ALL %>% filter(charfamily == "LIB")
#SOC <- ALL %>% filter(charfamily == "SOC")
#CD <- ALL %>% filter(charfamily == "CD")


Network_RADRT <- estimateNetwork(RADRT[all_att], default = "EBICglasso", corMethod = "spearman")
Network_RADLEFT <- estimateNetwork(RADLEFT[all_att], default = "EBICglasso", corMethod = "spearman")
#Network_GREEN <- estimateNetwork(GREEN[all_att], default = "EBICglasso", corMethod = "spearman")
#Network_CON <- estimateNetwork(CON[all_att], default = "EBICglasso", corMethod = "spearman")
#Network_LIB <- estimateNetwork(LIB[all_att], default = "EBICglasso", corMethod = "spearman")
#Network_SOC <- estimateNetwork(SOC[all_att], default = "EBICglasso", corMethod = "spearman")
#Network_CD <- estimateNetwork(CD[all_att], default = "EBICglasso", corMethod = "spearman")

#####
#Radical Right COMPARISONS
#####
RADRT_CON <- NCT(Network_RADRT, Network_CON,
                 it = 1000,
                 progressbar = TRUE, abs = FALSE)
#RADRT_CON

#GLOBAL EXPECTED INFLUENCE INVARIANCE TEST 
#Global expected influence per group:  5.223707 6.221247 
#Test statistic S:  0.9975401 
#p-value 0.047 

#GLOBAL STRENGTH INVARIANCE TEST 
#Global strength per group:  5.597479 7.281316 
#Test statistic S:  1.683837 
#p-value 0.127 

RADRT_LIB <- NCT(Network_RADRT, Network_LIB,
                 it = 1000,
                 progressbar = TRUE, abs = FALSE)
#RADRT_LIB

#GLOBAL EXPECTED INFLUENCE INVARIANCE TEST 
#Global expected influence per group:  5.223707 6.762832 
#Test statistic S:  1.539126 
#p-value 0 

#GLOBAL STRENGTH INVARIANCE TEST 
#Global strength per group:  5.597479 8.258762 
#Test statistic S:  2.661283 
#p-value 0


RADRT_SOC <- NCT(Network_RADRT, Network_SOC,
                 it = 1000,
                 progressbar = TRUE, abs = FALSE)
#RADRT_SOC

#GLOBAL EXPECTED INFLUENCE INVARIANCE TEST 
#Global expected influence per group:  5.223707 7.180944 
#Test statistic S:  1.957238 
#p-value 0

#GLOBAL STRENGTH INVARIANCE TEST 
#Global strength per group:  5.597479 7.975672 
#Test statistic S:  2.378193 
#p-value 0.002 


RADRT_CD <- NCT(Network_RADRT, Network_CD,
                it = 1000,
                progressbar = TRUE, abs = FALSE)

#RADRT_CD

#GLOBAL STRENGTH INVARIANCE TEST 
#Global strength per group:  5.223707 5.702582 
#Test statistic S:  0.4788758 
#p-value 0.299 


#GLOBAL STRENGTH INVARIANCE TEST 
#Global strength per group:  5.597479 6.021887 
#Test statistic S:  0.4244081 
#p-value 0.61


#####
#Radical Left COMPARISONS
#####

RADLEFT_CON <- NCT(Network_RADLEFT, Network_CON,
                   it = 1000,
                   progressbar = TRUE, abs = FALSE)

#RADLEFT_CON

#GLOBAL EXPECTED INFLUENCE INVARIANCE TEST 
#Global strength per group:  7.688992 6.221247 
#Test statistic S:  1.467745 
#p-value 0  

#GLOBAL STRENGTH INVARIANCE TEST 
#Global strength per group:  7.932525 7.281316 
#Test statistic S:  0.6512093 
#p-value 0.253 

RADLEFT_LIB <- NCT(Network_RADLEFT, Network_LIB,
                   it = 1000,
                   progressbar = TRUE, abs = FALSE)

#RADLEFT_LIB

#GLOBAL EXPECTED INFLUENCE INVARIANCE TEST 
#Global expected influence per group:  7.688992 6.762832 
#Test statistic S:  0.9261597 
#p-value 0 

#GLOBAL STRENGTH INVARIANCE TEST 
#Global strength per group:  7.932525 8.258762 
#Test statistic S:  0.3262367 
#p-value 0.484 

RADLEFT_SOC <- NCT(Network_RADLEFT, Network_SOC,
                   it = 1000,
                   progressbar = TRUE, abs = FALSE)

#RADLEFT_SOC

#GLOBAL EXPECTED INFLUENCE INVARIANCE TEST 
#Global expected influence per group:  7.688992 7.180944 
#Test statistic S:  0.5080478 
#p-value 0.194 

#GLOBAL STRENGTH INVARIANCE TEST 
#Global strength per group:  7.932525 7.975672 
#Test statistic S:  0.04314638 
#p-value 0.98

RADLEFT_CD <- NCT(Network_RADLEFT, Network_CD,
                  it = 1000,
                  progressbar = TRUE, abs = FALSE)

#RADLEFT_CD

#GLOBAL EXPECTED INFLUENCE INVARIANCE TEST 
#Global expected influence per group:  7.688992 5.702582 
#Test statistic S:  1.98641 
#p-value 0 


#GLOBAL STRENGTH INVARIANCE TEST 
#Global strength per group:  7.932525 6.021887 
#Test statistic S:  1.910639 
#p-value 0 


#####
#PLOTS
#####
#RADRT, RADLEFT

max_value <- max(
  max(abs(Network_RADRT$graph)),
  max(abs(Network_RADLEFT$graph))
)

net_layout <- averageLayout(Network_RADRT,
                            Network_RADLEFT,
                            layout = "spring")



svg("Networks.svg", width = 9, height = 5)
par(mfrow=c(1,2))
plot(Network_RADRT, 
     layout = net_layout,
     title = "Radical Right",
     title.cex = 1.5,
     maximum = max_value,
     groups = groups,
     color = c("gray90", "gray50"),
     negDashed = F,
     edge.width = 2,
     font = 1,
     borders = F,
     vsize = c(6, 6),
     GLratio = 1.5,
     legend = F,
     posCol = "darkblue",
     negCol = "darkred",
     filename = "RADRT")

plot(Network_RADLEFT, 
     layout = net_layout,
     title = "Radical Left",
     title.cex = 1.5,
     maximum = max_value,
     groups = groups,
     color = c("gray90", "gray50"),
     negDashed = F,
     edge.width = 2,
     font = 1,
     borders = F,
     vsize = c(6, 6),
     GLratio = 1.5,
     legend = F,
     posCol = "darkblue",
     negCol = "darkred",
     filename = "RADLEFT")
dev.off()



#####
#MAINSTREAM PARTIES COMPARISONS
#####
#CON, LIB, SOC, CD, GREEN
Main1 <- NCT(Network_CON, Network_CD,
             it = 1000,
             progressbar = TRUE, abs = FALSE)

#Main1

Main2 <- NCT(Network_CON, Network_LIB,
             it = 1000,
             progressbar = TRUE, abs = FALSE)

#Main2

Main3 <- NCT(Network_CON, Network_SOC,
             it = 1000,
             progressbar = TRUE, abs = FALSE)

#Main3

Main4 <- NCT(Network_CD, Network_LIB,
             it = 1000,
             progressbar = TRUE, abs = FALSE)

#Main4

Main5 <- NCT(Network_CD, Network_SOC,
             it = 1000,
             progressbar = TRUE, abs = FALSE)

#Main5

Main6 <- NCT(Network_LIB, Network_SOC,
             it = 1000,
             progressbar = TRUE, abs = FALSE)

#Main6

Main7 <- NCT(Network_GREEN, Network_CON,
             it = 1000,
             progressbar = TRUE, abs = FALSE)

#Main7

Main8 <- NCT(Network_GREEN, Network_LIB,
             it = 1000,
             progressbar = TRUE, abs = FALSE)

#Main8

Main9 <- NCT(Network_GREEN, Network_SOC,
             it = 1000,
             progressbar = TRUE, abs = FALSE)

#Main9

Main10 <- NCT(Network_GREEN, Network_CD,
              it = 1000,
              progressbar = TRUE, abs = TRUE)

#Main10

