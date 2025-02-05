library(NetworkComparisonTest)
library(qgraph)
library(bootnet)
library(patchwork)

all_att <- c("mnrgtjb", "freehms", "hmsfmlsh", "hmsacld", "imueclt", "imwbcnt", "Immigration", "gincdif",
             "dfincac", "smdfslv", "gvslvol", "gvslvue", "gvcldcr", "sbstrec", "sbprvpv", "sbbsntx",
             "sblazy", "uentrjb", "lbenent", "bennent")
items <- c("Men should have more right to job than women when jobs are scarce",
           "Gays and lesbians free to live life as they wish",
           "Ashamed if close family member gay or lesbian",
           "Gay and lesbian couples right to adopt children",
           "Country's cultural life undermined or enriched by immigrants",
           "Immigrants make country worse or better place to live",
           "Allow many/few immigrants (composite)",
           "Government should reduce differences in income levels",
           "Large differences in income acceptable to reward talents and efforts",
           "For fair society, differences in standard of living should be small",
           "Standard of living for the old, governments' responsibility",
           "Standard of living for the unemployed, governments' responsibility",
           "Child care services for working parents, governments' responsibility",
           "Social benefits/services place too great strain on economy",
           "Social benefits/services prevent widespread poverty",
           "Social benefits/services cost businesses too much in taxes/charges",
           "Social benefits/services make people lazy",
           "Most unemployed people do not really try to find a job",
           "Many with very low incomes get less benefit than legally entitled to",
           "Many manage to obtain benefits/services not entitled to")

groups <- c("Sociocultural", "Sociocultural", "Sociocultural", "Sociocultural", "Sociocultural", "Sociocultural", "Sociocultural",
            "Economic", "Economic", "Economic", "Economic", "Economic", "Economic", "Economic", "Economic", "Economic", "Economic",
            "Economic", "Economic", "Economic")



#Party Families
#Group N

#table(DATA16$family)
#Soc: 2940
#RR: 911
#RLeft: 891
#Lib: 1791
#Green: 811
#Cons:2213
#ChristDem: 1230

#####
#Belief Network Estimation
#####



RADRT <- DATA16 %>% filter(family == "RR")
RADLEFT <- DATA16 %>% filter(family == "RLeft")
GREEN <- DATA16 %>% filter(family == "Green")
CON <- DATA16 %>% filter(family == "Cons")
LIB <- DATA16 %>% filter(family == "Lib")
SOC <- DATA16 %>% filter(family == "Soc")
CD <- DATA16 %>% filter(family == "ChristDem")

Network_RADRT <- estimateNetwork(RADRT[all_att], default = "EBICglasso", corMethod = "spearman")
Network_RADLEFT <- estimateNetwork(RADLEFT[all_att], default = "EBICglasso", corMethod = "spearman")
Network_GREEN <- estimateNetwork(GREEN[all_att], default = "EBICglasso", corMethod = "spearman")
Network_CON <- estimateNetwork(CON[all_att], default = "EBICglasso", corMethod = "spearman")
Network_LIB <- estimateNetwork(LIB[all_att], default = "EBICglasso", corMethod = "spearman")
Network_SOC <- estimateNetwork(SOC[all_att], default = "EBICglasso", corMethod = "spearman")
Network_CD <- estimateNetwork(CD[all_att], default = "EBICglasso", corMethod = "spearman")


#Network_RADRT
#Number of non-zero edges: 52 / 190 
#Mean weight: 0.02749319 

#Network_RADLEFT
#Number of non-zero edges: 97 / 190 
#Mean weight: 0.04046838 

#Network_GREEN
#Number of non-zero edges: 78 / 190 
#Mean weight: 0.03491548 

#Network_CON
#Number of non-zero edges: 84 / 190 
#Mean weight: 0.0327434 

#Network_LIB
#Number of non-zero edges: 110 / 190 
#Mean weight: 0.03559385

#Network_SOC
#Number of non-zero edges: 116 / 190 
#Mean weight: 0.03779444


#Network_CD
#Number of non-zero edges: 70 / 190 
#Mean weight: 0.03001359 


#Network Comparisons / Requires more computational time!


#####
#Green COMPARISONS
#####
GREEN_CON <- NCT(Network_CON,Network_GREEN,
                 it = 1000,
                 progressbar = TRUE, abs = FALSE)
#GREEN_CON

#GLOBAL EXPECTED INFLUENCE INVARIANCE TEST 
#Global EI per group:  6.221247 6.633941 
#Test statistic S:  0.4126941 
#p-value 0.2947053


GREEN_LIB <- NCT(Network_GREEN, Network_LIB,
                 it = 1000,
                 progressbar = TRUE, abs = FALSE)
GREEN_LIB

#GLOBAL EXPECTED INFLUENCE INVARIANCE TEST 
#Global EI per group:  6.633941 6.762832 
#Test statistic S:  0.1288915 
#p-value 0.6783217

GREEN_CD <- NCT(Network_GREEN, Network_CD,
                it = 1000,
                progressbar = TRUE, abs = FALSE)

#GREEN_CD

#GLOBAL EXPECTED INFLUENCE INVARIANCE TEST 
#Global EI per group:  6.633941 5.702582 
#Test statistic S:  0.9313584 
#p-value 0.01898102


GREEN_RADRT <- NCT(Network_GREEN, Network_RADRT,
                it = 1000,
                progressbar = TRUE, abs = FALSE)
#GREEN_RADRT

#GLOBAL EXPECTED INFLUENCE INVARIANCE TEST 
#Global EI per group:  6.633941 5.223707 
#Test statistic S:  1.410234 
#p-value 0.000999001

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

RADLEFT_LIB <- NCT(Network_RADLEFT, Network_LIB,
                   it = 1000,
                   progressbar = TRUE, abs = FALSE)

#RADLEFT_LIB

#GLOBAL EXPECTED INFLUENCE INVARIANCE TEST 
#Global expected influence per group:  7.688992 6.762832 
#Test statistic S:  0.9261597 
#p-value 0 


RADLEFT_SOC <- NCT(Network_RADLEFT, Network_SOC,
                   it = 1000,
                   progressbar = TRUE, abs = FALSE)

#RADLEFT_SOC

#GLOBAL EXPECTED INFLUENCE INVARIANCE TEST 
#Global expected influence per group:  7.688992 7.180944 
#Test statistic S:  0.5080478 
#p-value 0.194 


RADLEFT_CD <- NCT(Network_RADLEFT, Network_CD,
                  it = 1000,
                  progressbar = TRUE, abs = FALSE)

#RADLEFT_CD

#GLOBAL EXPECTED INFLUENCE INVARIANCE TEST 
#Global expected influence per group:  7.688992 5.702582 
#Test statistic S:  1.98641 
#p-value 0 


RADLEFT_RADRT <- NCT(Network_RADLEFT, Network_RADRT,
                  it = 1000,
                  progressbar = TRUE, abs = FALSE)

#RADLEFT_RADRT

#GLOBAL EXPECTED INFLUENCE INVARIANCE TEST 
#Global EI per group:  7.688992 5.223707 
#Test statistic S:  2.465285 
#p-value 0.000999001


#####
#PLOTS
#####

max_value <- max(
  max(abs(Network_RADRT$graph)),
  max(abs(Network_RADLEFT$graph)),
  max(abs(Network_CD$graph)),
  max(abs(Network_CON$graph)),
  max(abs(Network_GREEN$graph)),
  max(abs(Network_LIB$graph)),
  max(abs(Network_SOC$graph))
)

net_layout <- averageLayout(Network_RADRT,
                            Network_RADLEFT,
                            Network_CD,
                            Network_CON,
                            Network_GREEN,
                            Network_LIB,
                            Network_SOC,
                            layout = "spring")

posCol_adjusted <- rgb(0, 0, 0.5, 0.7)
negCol_adjusted <- rgb(0.5, 0, 0, 0.7)


#####
#BNS Plot
#####

svg("Figures/Networks.svg", width = 6, height = 6, family = "serif")
par(mfrow = c(3,3), mar = c(10, 10, 10, 10) + 5)

plot(Network_RADLEFT, 
     layout = net_layout,
     title = "Radical Left",
     title.cex = 1.5,
     maximum = max_value,
     groups = groups,
     color = c("gray90", "gray10"),
     negDashed = F,
     edge.width = 2,
     font = 1,
     borders = F,
     vsize = c(5, 5),
     GLratio = 1.5,
     legend = F,
     posCol = posCol_adjusted,
     negCol = negCol_adjusted,
     labels = F,
     curveAll = T,
     fade = F,
     curveDefault = .5,
     curveShape = -1)
box()

plot(Network_RADRT, 
     layout = net_layout,
     title = "Radical Right",
     title.cex = 1.5,
     maximum = max_value,
     groups = groups,
     color = c("gray90", "gray10"),
     negDashed = F,
     edge.width = 2,
     font = 1,
     borders = F,
     vsize = c(5, 5),
     GLratio = 1.5,
     legend = F,
     posCol = posCol_adjusted,
     negCol = negCol_adjusted,
     labels = F,
     curveAll = T,
     fade = F,
     curveDefault = .5,
     curveShape = -1)

plot(Network_CON, 
     layout = net_layout,
     title = "Conservative",
     title.cex = 1.5,
     maximum = max_value,
     groups = groups,
     color = c("gray90", "gray10"),
     negDashed = F,
     edge.width = 2,
     font = 1,
     borders = F,
     vsize = c(5, 5),
     GLratio = 1.5,
     legend = F,
     posCol = posCol_adjusted,
     negCol = negCol_adjusted,
     labels = F,
     curveAll = T,
     fade = F,
     curveDefault = .5,
     curveShape = -1)

plot(Network_SOC, 
     layout = net_layout,
     title = "Socialist",
     title.cex = 1.5,
     maximum = max_value,
     groups = groups,
     color = c("gray90", "gray10"),
     negDashed = F,
     edge.width = 2,
     font = 1,
     borders = F,
     vsize = c(5, 5),
     GLratio = 1.5,
     legend = F,
     posCol = posCol_adjusted,
     negCol = negCol_adjusted,
     labels = F,
     curveAll = T,
     fade = F,
     curveDefault = .5,
     curveShape = -1)
box()

plot(Network_CD, 
     layout = net_layout,
     title = "Christian Democracy",
     title.cex = 1.5,
     maximum = max_value,
     groups = groups,
     color = c("gray90", "gray10"),
     negDashed = F,
     edge.width = 2,
     font = 1,
     borders = F,
     vsize = c(5, 5),
     GLratio = 1.5,
     legend = F,
     posCol = posCol_adjusted,
     negCol = negCol_adjusted,
     labels = F,
     curveAll = T,
     fade = F,
     curveDefault = .5,
     curveShape = -1)

plot(Network_LIB, 
     layout = net_layout,
     title = "Liberal",
     title.cex = 1.5,
     maximum = max_value,
     groups = groups,
     color = c("gray90", "gray10"),
     negDashed = F,
     edge.width = 2,
     font = 1,
     borders = F,
     vsize = c(5, 5),
     GLratio = 1.5,
     legend = F,
     posCol = posCol_adjusted,
     negCol = negCol_adjusted,
     labels = F,
     curveAll = T,
     fade = F,
     curveDefault = .5,
     curveShape = -1)

plot(Network_GREEN, 
     layout = net_layout,
     title = "Green",
     title.cex = 1.5,
     maximum = max_value,
     groups = groups,
     color = c("gray90", "gray10"),
     negDashed = F,
     edge.width = 2,
     font = 1,
     borders = F,
     vsize = c(5, 5),
     GLratio = 1.5,
     legend = F,
     posCol = posCol_adjusted,
     negCol = negCol_adjusted,
     labels = F,
     curveAll = T,
     fade = F,
     curveDefault = .5,
     curveShape = -1)
box()

par(mar = c(1, 1, 1, 1))

plot.new()

plot.new()  # Starts a new plot
plot.window(xlim = c(0, 1), ylim = c(0, 1))
text(0.5, 0.9, "Legend", cex = 1.2, font = 2)
text(0.05, 0.8, "Grey Nodes:", cex = .9, adj = 0)
text(0.25, 0.7, "Economic", cex = .9, adj = 0, font = 3, col = "grey30")
text(0.05, 0.6, "Black Nodes:", cex = .9, adj = 0, font = 3)
text(0.25, 0.5, "Sociocultural", cex = .9, adj = 0, font = 3, col = "black")
text(0.05, 0.4, "Blue Edges:", cex = .9, adj = 0, font = 3)
text(0.25, 0.3, "Positive Association", cex = .9, adj = 0, font = 3, col = "darkblue")
text(0.05, 0.2, "Red Edges:", cex = .9, adj = 0, font = 3)
text(0.25, 0.1, "Negative Association", cex = .9, adj = 0, font = 3, col = "darkred")
rect(0, 0, 1, 1, border = "grey50")

par(mar = c(10, 10, 10, 10) + 5)

dev.off()

