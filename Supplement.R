#Dependence on the DATAFIN Dataset, which is rendered through PS.R... in the future, I should integrate
#these files into one

#####
#Supplementary Materials
#####
#Analysis of lost cases

MISSING <- DATAFIN %>% filter(!PARTISAN %in% nichDATA2$PARTISAN)
PARTMIS <- MISSING %>% group_by(PARTISAN) %>% dplyr::summarize() 
#length(unique(MISSING$PARTISAN))


MIS1 <- MISSING %>% dplyr::select(lrecon, galtan, family)
MIS2 <- DATAFIN %>% dplyr::select(lrecon, galtan, family)
MIS2 %<>% mutate(dataset = 1)
MIS1 %<>% mutate(dataset = 0)
comparison <- rbind(MIS1, MIS2)
comparison %<>% mutate(dataset = as.factor(dataset))

tab <- table(comparison$dataset, comparison$family)
prop.table(tab, margin = 1)

length(unique(DATAFIN$PARTISAN))
table(MISSING$PARTISAN, MISSING$family)

#Party Inconsistency Analysis
#Party inconsistency variable is from the main code (cummulative data file)
PIM3 <- lmer(st.isal_econ ~ r.polintr + gndr + eisced  + PI + (1 | cntry:PARTISAN), data=ICDATA)
PIM4 <- lmer(st.isal_soc ~ r.polintr + gndr + eisced + PI + (1 | cntry:PARTISAN), data=ICDATA)
PIM1 <- lmer(st.isal_econ ~ r.polintr + gndr + eisced + lrecon_sd + PI + (1 | cntry:PARTISAN), data=ICDATA)
PIM2 <- lmer(st.isal_soc ~ r.polintr + gndr + eisced + galtan_sd + PI + (1 | cntry:PARTISAN), data=ICDATA)

tab_model(PIM3, PIM4, PIM1, PIM2, p.style = "stars",
          show.ngroups = FALSE,
          show.icc = FALSE,
          show.obs = TRUE,
          string.pred = "Coefficient", collapse.ci = TRUE,
          dv.labels = c("Party Inconsistency Economic", "Party Inconsistency Sociocultural",
                        "With Uncertainty Economic", "With Uncertainty Sociocultural"),
          pred.labels = c("Intercept", "Interest in Politics", "Female", "Education",
                          "Party Inconsistency", "Blurring Economic", "Blurring Sociocultural"))
#Correlation of inconsistency with blurring

#Convergent Validity

#ICDATA %<>% mutate(id_extremity = abs(lrscale - 5))

C1.1 <- lmer(st.isal_econ ~ partisanship + (1 | cntry:PARTISAN), data=ICDATA)
#arm::display(C1.1)
C1.2 <- lmer(st.isal_soc ~ partisanship + (1 | cntry:PARTISAN), data=ICDATA)
#arm::display(C1.2)

C2.1 <- lmer(st.isal_econ ~ id_extremity + (1 | cntry:PARTISAN), data=ICDATA)
#arm::display(C2.1)
C2.2 <- lmer(st.isal_soc ~ id_extremity + (1 | cntry:PARTISAN), data=ICDATA)
#arm::display(C2.2)

tab_model(C1.1, C1.2, collapse.ci = FALSE, show.p = TRUE, show.icc = FALSE, show.ngroups = FALSE,
          dv.labels = c("Economic", "Sociocultural"),
          pred.labels = c("Intercept", "Strength of Partisanship"))

tab_model(C2.1, C2.2, collapse.ci = FALSE, show.p = TRUE, show.icc = FALSE, show.ngroups = FALSE,
          dv.labels = c("Economic", "Sociocultural"),
          pred.labels = c("Intercept", "Ideological Extremity"))


#####
#CFA & Robust models
#####
#ADDITIONAL REQUIRED LIBRARIES
library(lavaan)
library(semTools)
library(ccpsyc)
library(ltm)

#ECONOMIC ISSUES - DATAFIN ITEMS

cfa_model_econ <- "econ =~ gincdif + imbgeco + dfincac + smdfslv + gvslvue + sbstrec +
sbbsntx + sblazy + imsclbn + uentrjb + bennent + lbenent + sbprvpv + gvslvol + gvcldcr"
fit_cfa_econ <- cfa(cfa_model_econ, data = ICDATA)
summary(fit_cfa_econ, fit.measures = TRUE,
        standardized = TRUE, rsquare = TRUE)

#ECON - REDUCED
MODecon <- c("gincdif", "imbgeco", "dfincac", "smdfslv", "gvslvue", "sbstrec", "sbbsntx", "sblazy", "imsclbn", "uentrjb", "bennent")

MODcfa_model_econ <- "econ =~ dfincac + gvslvue + sbstrec +
sbbsntx + sblazy + uentrjb + bennent"
MODfit_cfa_econ <- cfa(MODcfa_model_econ, data = ICDATA)
summary(MODfit_cfa_econ, fit.measures = TRUE,
        standardized = TRUE, rsquare = TRUE)


#SOCIOCULTURAL-FULL
cfa_model_soccult <- "soccult=~ mnrgtjb + freehms  + hmsfmlsh +
  hmsacld + euftf +imsmetn +imdfetn +
  impcntr + imueclt + imwbcnt + impsafe +
  ipfrule + ipudrst + ipstrgv + ipbhprp +
  imptrad"

fit_cfa_soccult <- cfa(cfa_model_soccult, data = ICDATA)

summary(fit_cfa_soccult, fit.measures = TRUE,
        standardized = TRUE, rsquare = TRUE)

#SOCIOCULT: IMMIGRATION - REDUCED
MODcfa_model_soccult <- "soccult=~ Immigration + imueclt + imwbcnt"
MODfit_cfa_soccult <- cfa(MODcfa_model_soccult, data = ICDATA)
summary(MODfit_cfa_soccult, fit.measures = TRUE,
        standardized = TRUE, rsquare = TRUE)

#SOCIOCULT: MORAL - REDUCED
MOD2cfa_model_soccult <- "soccult=~ mnrgtjb + freehms  + hmsfmlsh +
  hmsacld"
MOD2fit_cfa_soccult <- cfa(MOD2cfa_model_soccult, data = ICDATA)
summary(MOD2fit_cfa_soccult, fit.measures = TRUE,
        standardized = TRUE, rsquare = TRUE)

#Robust Models

#Extremity
RME <- lmer(ROBisal_econ ~ r.polintr + gndr + eisced + lrecon + I(lrecon^2) + (1 | cntry:PARTISAN), data=ICDATA)
#arm::display(RME)
RMS <- lmer(ROB1isal_soc ~ r.polintr + gndr + eisced + galtan + I(galtan^2) + (1 | cntry:PARTISAN), data=ICDATA)
#arm::display(RMS)
RMS2 <- lmer(ROB2isal_soc ~ r.polintr + gndr + eisced + galtan + I(galtan^2) + (1 | cntry:PARTISAN), data=ICDATA)
#arm::display(RMS2)
tab_model(RME, RMS, RMS2, dv.labels = c("Robust Economic", "Robust Immigration", "Robust Moral"), p.style = "stars",
          show.ngroups = FALSE,
          show.icc = FALSE,
          show.obs = TRUE,
          string.pred = "Coefficient",
          string.ci = "Conf. Int (95%)",
          pred.labels = c("Intercept", "Interest in Politics", "Female", "Education", "Left-Right Extremity", "GAL-TAN Extremity"))


RMEP <- plot_model(RME, type = "pred", terms=c("lrecon[DATAFIN]"), axis.title = c("Left-Right Position", "Ideological Alignment"),
                   title = "Robust Economic", colors = c("darkblue","darkred"),
                   show.legend = FALSE) + theme_classic()
RMEP2 <- RMEP + scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10))  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

RMSP <- plot_model(RMS, type = "pred", terms=c("galtan[DATAFIN]"), axis.title = c("GALT-TAN Position", "Ideological Alignment"),
                   title = "Robust Immigration", colors = c("darkred"),
                   show.legend = FALSE) + theme_classic()
RMSP2 <- RMSP + scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10))  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

RMS2P <- plot_model(RMS2, type = "pred", terms=c("galtan[DATAFIN]"), axis.title = c("GALT-TAN Position", "Ideological Alignment"),
                    title = "Robust Moral", colors = c("darkred"),
                    show.legend = FALSE) + theme_classic()
RMS2P2 <- RMS2P + scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10))  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

svg("Figures/robust.svg", width = 12, height = 6, family = "serif")
RMEP2 + ylim(-1, 3.2) + 
  RMSP2 + ylim(-1, 3.2) +
  RMS2P2 + ylim(-1, 3.2)
dev.off()


#####
#Detailed plots for individual networks
#####

svg("Figures/CD.svg", width = 12, height = 6)
plot(Network_CD, 
     layout = net_layout,
     title = "Christian Democracy",
     title.cex = 1.5,
     maximum = max_value,
     groups = groups,
     color = c("gray90", "gray50"),
     negDashed = F,
     edge.width = 2,
     font = 1,
     borders = F,
     vsize = c(6, 6),
     GLratio = 1,
     legend = T,
     posCol = "darkblue",
     negCol = "darkred",
     nodeNames = items,
     legend.cex = 0.4)
dev.off()

svg("Figures/CON.svg", width = 12, height = 6)
plot(Network_CON, 
     layout = net_layout,
     title = "Conservative",
     title.cex = 1.5,
     maximum = max_value,
     groups = groups,
     color = c("gray90", "gray50"),
     negDashed = F,
     edge.width = 2,
     font = 1,
     borders = F,
     vsize = c(6, 6),
     GLratio = 1,
     legend = T,
     posCol = "darkblue",
     negCol = "darkred",
     nodeNames = items,
     legend.cex = 0.4)
dev.off()

svg("Figures/GREEN.svg", width = 12, height = 6)
plot(Network_GREEN, 
     layout = net_layout,
     title = "Green",
     title.cex = 1.5,
     maximum = max_value,
     groups = groups,
     color = c("gray90", "gray50"),
     negDashed = F,
     edge.width = 2,
     font = 1,
     borders = F,
     vsize = c(6, 6),
     GLratio = 1,
     legend = T,
     posCol = "darkblue",
     negCol = "darkred",
     nodeNames = items,
     legend.cex = 0.4)
dev.off()

svg("Figures/LIB.svg", width = 12, height = 6)
plot(Network_LIB, 
     layout = net_layout,
     title = "Liberal",
     title.cex = 1.5,
     maximum = max_value,
     groups = groups,
     color = c("gray90", "gray50"),
     negDashed = F,
     edge.width = 2,
     font = 1,
     borders = F,
     vsize = c(6, 6),
     GLratio = 1,
     legend = T,
     posCol = "darkblue",
     negCol = "darkred",
     nodeNames = items,
     legend.cex = 0.4)
dev.off()

svg("Figures/RADLEFT.svg", width = 12, height = 6)
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
     GLratio = 1,
     legend = T,
     posCol = "darkblue",
     negCol = "darkred",
     nodeNames = items,
     legend.cex = 0.4)
dev.off()

svg("Figures/RADRT.svg", width = 12, height = 6)
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
     GLratio = 1,
     legend = T,
     posCol = "darkblue",
     negCol = "darkred",
     nodeNames = items,
     legend.cex = 0.4)
dev.off()

svg("Figures/SOC.svg", width = 12, height = 6)
plot(Network_SOC, 
     layout = net_layout,
     title = "Socialist",
     title.cex = 1.5,
     maximum = max_value,
     groups = groups,
     color = c("gray90", "gray50"),
     negDashed = F,
     edge.width = 2,
     font = 1,
     borders = F,
     vsize = c(6, 6),
     GLratio = 1,
     legend = T,
     posCol = "darkblue",
     negCol = "darkred",
     nodeNames = items,
     legend.cex = 0.4)
dev.off()

