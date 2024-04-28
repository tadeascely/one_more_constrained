#####
#Belief Network Systems
#####


#LRECON EXT
LRECON_EXT_H <- ICDATA %>% filter(lrecon_ext >= mean(lrecon_ext) + sd(lrecon_ext))
LRECON_EXT_L <- ICDATA %>% filter(lrecon_ext <= mean(lrecon_ext) - sd(lrecon_ext))

Network_LRECON_EXT_H <- estimateNetwork(LRECON_EXT_H[all_att], default = "EBICglasso", corMethod = "spearman")
Network_LRECON_EXT_L <- estimateNetwork(LRECON_EXT_L[all_att], default = "EBICglasso", corMethod = "spearman")

#Plot
max_value <- max(
  max(abs(Network_LRECON_EXT_H$graph)),
  max(abs(Network_LRECON_EXT_L$graph))
)

net_layout <- averageLayout(Network_LRECON_EXT_H,
                            Network_LRECON_EXT_L,
                            layout = "spring")

svg("Economic Extremity.svg", width = 9, height = 5)
par(mfrow=c(1,2))
plot(Network_LRECON_EXT_H, 
     layout = net_layout,
     title = "Extreme Economic Position",
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
     filename = "Network_LRECON_EXT_H")

plot(Network_LRECON_EXT_L, 
     layout = net_layout,
     title = "Moderate Economic Position",
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
     filename = "Network_LRECON_EXT_L")
dev.off()



#Network_LRECON_EXT_H
#Network_LRECON_EXT_L

#COMPARISON
LRECON_EXT <- NCT(Network_LRECON_EXT_H, Network_LRECON_EXT_L,
                 it = 1000,
                 progressbar = TRUE, abs = FALSE)

#LRECON_EXT

#GLOBAL EXPECTED INFLUENCE INVARIANCE TEST 
#Global expected influence per group:  7.933083 7.309059 
#Test statistic S:  0.6240244 
#p-value 0 

#GLOBAL STRENGTH INVARIANCE TEST 
#Global strength per group:  8.533489 7.588784 
#Test statistic S:  0.9447045 
#p-value 0.003 

#GALTAN EXT
GALTAN_EXT_H <- ICDATA %>% filter(galtan_ext >= mean(galtan_ext) + sd(galtan_ext))
GALTAN_EXT_L <- ICDATA %>% filter(galtan_ext <= mean(galtan_ext) - sd(galtan_ext))

Network_GALTAN_EXT_H <- estimateNetwork(GALTAN_EXT_H[all_att], default = "EBICglasso", corMethod = "spearman")
Network_GALTAN_EXT_L <- estimateNetwork(GALTAN_EXT_L[all_att], default = "EBICglasso", corMethod = "spearman")

#Plot
max_value <- max(
  max(abs(Network_GALTAN_EXT_H$graph)),
  max(abs(Network_GALTAN_EXT_L$graph))
)

net_layout <- averageLayout(Network_GALTAN_EXT_H,
                            Network_GALTAN_EXT_L,
                            layout = "spring")

svg("Sociocultural Extremity.svg", width = 9, height = 5)
par(mfrow=c(1,2))
plot(Network_GALTAN_EXT_H, 
     layout = net_layout,
     title = "Extreme Sociocultural Position",
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
     filename = "Network_GALTAN_EXT_H")

plot(Network_GALTAN_EXT_L, 
     layout = net_layout,
     title = "Moderate Sociocultural Position",
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
     filename = "Network_GALTAN_EXT_L")
dev.off()


#Network_GALTAN_EXT_H
#Network_GALTAN_EXT_L

#COMPARISON
GALTAN_EXT <- NCT(Network_GALTAN_EXT_H, Network_GALTAN_EXT_L,
                  it = 1000,
                  progressbar = TRUE, abs = FALSE)

#GALTAN_EXT

#GLOBAL EXPECTED INFLUENCE INVARIANCE TEST 
#Global expected influence per group:  7.706653 7.118725 
#Test statistic S:  0.5879284 
#p-value 0 


#GLOBAL STRENGTH INVARIANCE TEST 
#Global strength per group:  8.541221 7.732868 
#Test statistic S:  0.8083536 
#p-value 0.008 


#LRECON SD
LRECON_SD_H <- ICDATA %>% filter(lrecon_sd >= mean(lrecon_sd) + sd(lrecon_sd))
LRECON_SD_L <- ICDATA %>% filter(lrecon_sd <= mean(lrecon_sd) - sd(lrecon_sd))

Network_LRECON_SD_H <- estimateNetwork(LRECON_SD_H[all_att], default = "EBICglasso", corMethod = "spearman")
Network_LRECON_SD_L <- estimateNetwork(LRECON_SD_L[all_att], default = "EBICglasso", corMethod = "spearman")

#Plot
max_value <- max(
  max(abs(Network_LRECON_SD_H$graph)),
  max(abs(Network_LRECON_SD_L$graph))
)

net_layout <- averageLayout(Network_LRECON_SD_H,
                            Network_LRECON_SD_L,
                            layout = "spring")

svg("Economic Blurring.svg", width = 9, height = 5)
par(mfrow=c(1,2))
plot(Network_LRECON_SD_H, 
     layout = net_layout,
     title = "High Economic Blurring",
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
     filename = "Network_LRECON_SD_H")

plot(Network_LRECON_SD_L, 
     layout = net_layout,
     title = "Low Economic Blurring",
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
     filename = "Network_LRECON_SD_L")
dev.off()

#Network_LRECON_SD_H
#Network_LRECON_SD_L

#COMPARISON
LRECON_SD <- NCT(Network_LRECON_SD_H, Network_LRECON_SD_L,
                  it = 1000,
                  progressbar = TRUE, abs = FALSE)
#LRECON_SD

#GLOBAL EXPECTED INFLUENCE INVARIANCE TEST 
#Global expected influence per group:  7.706653 7.118725 
#Test statistic S:  0.5879284 
#p-value 0 

#GLOBAL STRENGTH INVARIANCE TEST 
#Global strength per group:  6.889546 8.153166 
#Test statistic S:  1.26362 
#p-value 0.011 

#GALTAN SD
GALTAN_SD_H <- ICDATA %>% filter(galtan_sd >= mean(galtan_sd) + sd(galtan_sd))
GALTAN_SD_L <- ICDATA %>% filter(galtan_sd <= mean(galtan_sd) - sd(galtan_sd))

Network_GALTAN_SD_H <- estimateNetwork(GALTAN_SD_H[all_att], default = "EBICglasso", corMethod = "spearman")
Network_GALTAN_SD_L <- estimateNetwork(GALTAN_SD_L[all_att], default = "EBICglasso", corMethod = "spearman")

#Plot
max_value <- max(
  max(abs(Network_GALTAN_SD_H$graph)),
  max(abs(Network_GALTAN_SD_L$graph))
)

net_layout <- averageLayout(Network_GALTAN_SD_H,
                            Network_GALTAN_SD_L,
                            layout = "spring")

svg("Sociocultural Blurring.svg", width = 9, height = 5)
par(mfrow=c(1,2))
plot(Network_GALTAN_SD_H, 
     layout = net_layout,
     title = "High Sociocultural Blurring",
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
     filename = "Network_GALTAN_SD_H")

plot(Network_GALTAN_SD_L, 
     layout = net_layout,
     title = "Low Sociocultural Blurring",
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
     filename = "Network_GALTAN_SD_L")
dev.off()

#Network_GALTAN_SD_H
#Network_GALTAN_SD_L

#COMPARISON
GALTAN_SD <- NCT(Network_GALTAN_SD_H, Network_GALTAN_SD_L,
                 it = 1000,
                 progressbar = TRUE, abs = FALSE)
#GALTAN_SD

#GLOBAL EXPECTED INFLUENCE INVARIANCE TEST 
#Global expected influence per group:  7.393001 7.763226 
#Test statistic S:  0.3702249 
#p-value 0.04 


#GLOBAL STRENGTH INVARIANCE TEST 
#Global strength per group:  8.220999 8.378565 
#Test statistic S:  0.157566 
#p-value 0.645 


#Nicheness
NICHENESS_H <- nichDATA2 %>% filter(nicheness >= mean(nicheness) + sd(nicheness))
NICHENESS_L <- nichDATA2 %>% filter(nicheness <= mean(nicheness) - sd(nicheness))

Network_NICHENESS_H <- estimateNetwork(NICHENESS_H[all_att], default = "EBICglasso", corMethod = "spearman")
Network_NICHENESS_L <- estimateNetwork(NICHENESS_L[all_att], default = "EBICglasso", corMethod = "spearman")

#Network_NICHENESS_H
#Network_NICHENESS_L

#COMPARISON
NICHENESS <- NCT(Network_NICHENESS_H, Network_NICHENESS_L,
                 it = 1000,
                 progressbar = TRUE, abs = FALSE)

#NICHENESS

#GLOBAL EXPECTED INFLUENCE INVARIANCE TEST 
#Global strength per group:  7.074813 6.850853 
#Test statistic S:  0.2239599 
#p-value 0.459 

#GLOBAL STRENGTH INVARIANCE TEST 
#Global strength per group:  7.773197 7.717256 
#Test statistic S:  0.05594111 
#p-value 0.924 



