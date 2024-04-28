#####
#LIBRARIES
#####
library(kableExtra)
library(tidyverse)
library(haven)
library(Hmisc)
library(lme4)
library(svglite)
library(sjmisc)
library(sjPlot)
library(influence.ME)
library(patchwork)
library(magrittr)
#paste function
library(stringr)
library(manifestoR)


ESS4 <- read.csv("ESS4e04_6-subset.csv")
#defining missing values
ESS4[ESS4 == 66] <- NA
ESS4[ESS4 == 77] <- NA
ESS4[ESS4 == 88] <- NA
ESS4[ESS4 == 99] <- NA

CHES2006 <- read_dta("2006_CHES_dataset_means.dta")
countries <- c(1,21, 3, 22, 5, 14, 6, 11, 23, 7, 10, 26, 12, 16, 29)
CHES2006 <- CHES2006 %>% subset(country %in% countries)

CHES2006 <- CHES2006 %>% mutate(family = case_when(family == 1 ~ "RR",
                                                   family == 2 ~ "Cons",
                                                   family == 3 ~ "Lib", 
                                                   family == 4 ~ "ChristDem",
                                                   family == 5 ~ "Soc",
                                                   family == 6 ~ "RLeft",
                                                   family == 7 ~ "Green",
                                                   TRUE ~ NA_character_)) %>% mutate(family = as.factor(family))

#Renaming cross-country duplicities in party names
CHES2006$party[which(CHES2006$country == 12 & CHES2006$party == "PS")] <- "PS(pt)"
CHES2006$party[which(CHES2006$country == 6 & CHES2006$party == "PS")] <- "PS(fr)"
CHES2006$party[which(CHES2006$country == 1 & CHES2006$party == "PS")] <- "PS(be)"

CHES2006$party[which(CHES2006$country == 14 & CHES2006$party == "KD")] <- "KD(fi)"
CHES2006$party[which(CHES2006$country == 16 & CHES2006$party == "KD")] <- "KD(se)"

CHES2006$party[which(CHES2006$country == 11 & CHES2006$party == "Lab")] <- "Lab(uk)"
CHES2006$party[which(CHES2006$country == 7 & CHES2006$party == "Lab")] <- "Lab(irl)"

CHES2006$party[which(CHES2006$country == 3 & CHES2006$party == "CDU")] <- "CDU(de)"

CHES2006 <- CHES2006 %>% mutate(PARTISAN = party)

#Belgium
#PS(!), SPA, ECOLO, Groen, MR, VLD, CDH, CD&V, NVA, VB
ESS4 <- ESS4 %>% mutate(prtclbbe = case_when(prtclbbe == 1 ~ "Groen",
                                             prtclbbe == 2 ~ "CD&V",
                                             prtclbbe == 5 ~ "SPA",
                                             prtclbbe == 8 ~ "VB",
                                             prtclbbe == 9 ~ "VLD",
                                             prtclbbe == 10 ~ "CDH",
                                             prtclbbe == 11 ~ "ECOLO",
                                             prtclbbe == 13 ~ "MR",
                                             prtclbbe == 14 ~ "PS(be)",
                                            TRUE ~ NA_character_))
#table(ESS4$prtclbbe)

#Czechia
#CSSD, ODS, KSCM, KDU-CSL, SNK-ED, SZ, Nezavisli
ESS4 <- ESS4 %>% mutate(prtclacz = case_when(prtclacz == 1 ~ "KSCM",
                                             prtclacz == 2 ~ "CSSD",
                                             prtclacz == 4 ~ "KDU-CSL",
                                             prtclacz == 5 ~ "ODS",
                                             TRUE ~ NA_character_))
#table(ESS4$prtclacz)

#Germany
#CDU, SPD, FDP, Grünen, Linkspartei/PDS, CSU
ESS4 <- ESS4 %>% mutate(prtclbde = case_when(prtclbde == 1 ~ "SPD",
                                             prtclbde == 2 & regionde == 9 ~ "CSU",
                                             prtclbde == 2 & regionde != 9 ~ "CDU(de)",
                                             prtclbde == 3 ~ "Grünen",
                                             prtclbde == 4 ~ "FDP",
                                             prtclbde == 5 ~ "Linkspartei/ PDS",
                                             TRUE ~ NA_character_))
table(DATA08$prtclbde)
table(ESS4$regionde, ESS4$PARTISAN)
#Estonia
#IRL, EK, ER, SDE, KP, ERL
ESS4 <- ESS4 %>% mutate(prtclbee = case_when(prtclbee == 1 ~ "IRL",
                                             prtclbee == 2 ~ "EK",
                                             prtclbee == 3 ~ "ER",
                                             prtclbee == 5 ~ "SDE",
                                             TRUE ~ NA_character_))
#table(ESS4$prtclbee)

#Spain
#PSOE, PP, IU, CiU, PNV, EA, ERC, BNG, CC, CHA
ESS4 <- ESS4 %>% mutate(prtclbes = case_when(prtclbes == 1 ~ "PP",
                                             prtclbes == 2 ~ "PSOE",
                                             prtclbes == 3 ~ "IU",
                                             prtclbes == 4 ~ "CiU",
                                             prtclbes == 6 ~ "PNV",
                                             prtclbes == 7 ~ "BNG",
                                             TRUE ~ NA_character_))
#table(ESS4$prtclbes)

#Finland
#SDP, KOK, KESK, VAS, TrueFinns, RKP/SFP, VIHR, KD
ESS4 <- ESS4 %>% mutate(prtclafi = case_when(prtclafi == 1 ~ "KOK",
                                             prtclafi == 2 ~ "RKP/SFP",
                                             prtclafi == 5 ~ "True Finns",
                                             prtclafi == 6 ~ "KD(fi)",
                                             prtclafi == 7 ~ "VIHR",
                                             prtclafi == 8 ~ "SDP",
                                             prtclafi == 9 ~ "VAS",
                                             TRUE ~ NA_character_))
#table(ESS4$prtclafi)


#France
#PCF, PS, PRG, VERTS, UMP, FN, MPF, UDF
ESS4 <- ESS4 %>% mutate(prtclbfr = case_when(prtclbfr == 2 ~ "FN",
                                             prtclbfr == 8 ~ "PCF",
                                             prtclbfr == 10 ~ "PS(fr)",
                                             prtclbfr == 11 ~ "UMP",
                                             prtclbfr == 12 ~ "UDF",
                                             prtclbfr == 13 ~ "VERTS",
                                             TRUE ~ NA_character_))
#table(ESS4$prtclbfr)

#United Kingdom
#Cons, Lab, LibDems, SNP, Plaid, Greens, UKIP
ESS4 <- ESS4 %>% mutate(prtclgb = case_when(prtclgb == 1 ~ "Cons",
                                             prtclgb == 2 ~ "Lab(uk)",
                                             prtclgb == 3 ~ "LibDems",
                                             prtclgb == 4 ~ "SNP",
                                            prtclgb == 6 ~ "Greens", 
                                            TRUE ~ NA_character_))
#table(ESS4$prtclgb)

#Hungary
#MSzP, Fidesz-M, MDF, SzDSz, KDNP
ESS4 <- ESS4 %>% mutate(prtclbhu = case_when(prtclbhu == 1 ~ "Fidesz-M",
                                            prtclbhu == 4 ~ "MSzP", 
                                            TRUE ~ NA_character_))
#table(ESS4$prtclbhu)

#Ireland
#FF, FG, Lab, GP, PD, SF
ESS4 <- ESS4 %>% mutate(prtclbie = case_when(prtclbie == 1 ~ "FF",
                                             prtclbie == 2 ~ "FG",
                                             prtclbie == 3 ~ "Lab(irl)",
                                             prtclbie == 4 ~ "GP", 
                                             prtclbie == 5 ~ "SF", 
                                             TRUE ~ NA_character_))
#table(ESS4$prtclbie)

#Netherlands
#CDA, PvdA, VVD, D66, GL, SP, CU, PVV
ESS4 <- ESS4 %>% mutate(prtclbnl = case_when(prtclbnl == 1 ~ "CDA",
                                             prtclbnl == 2 ~ "PvdA",
                                             prtclbnl == 3 ~ "VVD", 
                                             prtclbnl == 5 ~ "D66", 
                                             prtclbnl == 6 ~ "GL",
                                             prtclbnl == 7 ~ "SP",
                                             prtclbnl == 8 ~ "CU",
                                             prtclbnl == 11 ~ "PVV",
                                             TRUE ~ NA_character_))
#table(ESS4$prtclbnl)

#Poland
#SLD, PO, S, PiS, PSL, LPR, PD, SDPL
ESS4 <- ESS4 %>% mutate(prtclcpl = case_when(prtclcpl == 2 ~ "PO", 
                                             prtclcpl == 4 ~ "PSL", 
                                             prtclcpl == 5 ~ "PiS",
                                             prtclcpl == 8 ~ "SLD",
                                             TRUE ~ NA_character_))
#table(ESS4$prtclcpl)

#Portugal
#CDU, CDS/PP, PS, PPD/PSD, BE
ESS4 <- ESS4 %>% mutate(prtclbpt = case_when(prtclbpt == 1 ~ "BE",
                                             prtclbpt == 2 ~ "CDS/PP", 
                                             prtclbpt == 10 ~ "PPD/PSD", 
                                             prtclbpt == 11 ~ "PS(pt)",
                                             TRUE ~ NA_character_))
#table(ESS4$prtclbpt)

#Sweden
#V, SAP, C, FP, M, KD, MP, JL
ESS4 <- ESS4 %>% mutate(prtclse = case_when(prtclse == 1 ~ "C",
                                             prtclse == 2 ~ "FP", 
                                             prtclse == 3 ~ "KD(se)",
                                             prtclse == 6 ~ "SAP",
                                             prtclse == 7 ~ "V",
                                             TRUE ~ NA_character_))
#table(ESS4$prtclse)

#Slovenia
#LDS, SDS, ZLSD, SLS, NSI, DeSUS, SNS, AS
ESS4 <- ESS4 %>% mutate(prtclcsi = case_when(prtclcsi == 1 ~ "DeSUS",
                                            prtclcsi == 2 ~ "LDS",
                                            prtclcsi == 5 ~ "SDS",
                                            prtclcsi == 7 ~ "SNS",
                                            TRUE ~ NA_character_))
#table(ESS4$prtclcsi)

ESS4 <- ESS4 %>% mutate(PARTISAN = str_c(str_replace_na(prtclbbe, replacement = ""), str_replace_na(prtclacz, replacement = ""), str_replace_na(prtclbde, replacement = ""),
                                  str_replace_na(prtclbee, replacement = ""), str_replace_na(prtclbes, replacement = ""), str_replace_na(prtclafi, replacement = ""),
                                  str_replace_na(prtclbfr, replacement = ""), str_replace_na(prtclgb, replacement = ""), str_replace_na(prtclbhu, replacement = ""),
                                  str_replace_na(prtclbie, replacement = ""), str_replace_na(prtclbnl, replacement = ""), str_replace_na(prtclcpl, replacement = ""),
                                  str_replace_na(prtclbpt, replacement = ""), str_replace_na(prtclse, replacement = ""), str_replace_na(prtclcsi, replacement = "")))
ESS4$PARTISAN[ESS4$PARTISAN == ""] <- NA


#Compiling harmonized datasets
DATA08 <- merge(ESS4, CHES2006, by = "PARTISAN")

#####
#DEFINING MISSING VALUES - ALL ATTITUDES
#####
allfour <- c("imsmetn",	"imdfetn",	"impcntr")
a4 <- DATA08 %>% dplyr::select(all_of(allfour))
a4[a4 > 4] <- NA
DATA08[allfour] <- a4

allfive <- c("gincdif",	"mnrgtjb",	"freehms",	"dfincac",	"smdfslv",	"sbstrec",	"sbprvpv",	"sbbsntx",	"sblazy",	"imsclbn",	"uentrjb",	"lbenent",	"bennent")
a5 <- DATA08 %>% dplyr::select(all_of(allfive))
a5[a5 > 5] <- NA
DATA08[allfive] <- a5  

allsix <- c("impsafe",	"ipfrule",	"ipudrst",	"ipstrgv",	"ipbhprp",	"imptrad")
a6 <- DATA08 %>% dplyr::select(all_of(allsix))
a6[a6 > 6] <- NA
DATA08[allsix] <- a6

allten <- c("euftf",	"imbgeco",	"imueclt",	"imwbcnt",	"gvslvol",	"gvslvue",	"gvcldcr")
a10 <- DATA08 %>% dplyr::select(all_of(allten))
a10[a10 > 10] <- NA
DATA08[allten] <- a10

rm(a4, a5, a6, a10)

######
#REVERSE CODE SELECTED VARIABLES
######
#LRECON..... right = higher
#GALTAN.....TAN = higher

#5 levels
fivelevels <- c("mnrgtjb", "dfincac", "sbstrec", "sbbsntx", "sblazy", "uentrjb", "bennent")
d5 <- DATA08 %>% dplyr::select(all_of(fivelevels))
d5 <- d5 %>% mutate(dplyr::across(fivelevels, function(x) -1*x+6))
DATA08[fivelevels] <- d5

#6 levels
sixlevels <- c("impsafe", "ipfrule", "ipstrgv", "ipbhprp", "imptrad")
d6 <- DATA08 %>% dplyr::select(all_of(sixlevels))
d6 <- d6 %>% mutate(dplyr::across(sixlevels, function(x) -1*x+7))
DATA08[sixlevels] <- d6

#10 levels
tenlevels <- c("euftf", "imbgeco", "imueclt", "imwbcnt", "gvslvol", "gvslvue", "gvcldcr")
d10 <- DATA08 %>% dplyr::select(all_of(tenlevels))
d10 <- d10 %>% mutate(dplyr::across(tenlevels, function(x) -1*x+10))
DATA08[tenlevels] <- d10

rm(d5, d6, d10)

#####
#OUTCOME VARIABLES
#####
DATA08 <- DATA08 %>% mutate(Blrecon = case_when(lrecon > 5 ~ 1,
                                                lrecon < 5 ~ -1,
                                                TRUE ~ NA_real_))

DATA08 <- DATA08 %>% mutate(Bgaltan = case_when(galtan > 5 ~ 1,
                                                galtan < 5 ~ -1,
                                                TRUE ~ NA_real_))
#Reliability
#immig <- c("imdfetn", "impcntr", "imsmetn")
#ltm::cronbach.alpha(ALL[immig], na.rm = TRUE)
#cronbach.alpha(ALL[immig], na.rm = TRUE) <- reliability checked

DATA08 %<>% mutate(Immigration = (imdfetn + impcntr + imsmetn)/3)

DATA08 %<>% mutate(b.gincdif = case_when(gincdif > mean(gincdif, na.rm = TRUE) ~ 1,
                                         gincdif < mean(gincdif, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA08 %<>% mutate(b.dfincac = case_when(dfincac > mean(dfincac, na.rm = TRUE) ~ 1,
                                         dfincac < mean(dfincac, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA08 %<>% mutate(b.smdfslv = case_when(smdfslv > mean(smdfslv, na.rm = TRUE) ~ 1,
                                         smdfslv < mean(smdfslv, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA08 %<>% mutate(b.gvslvue = case_when(gvslvue > mean(gvslvue, na.rm = TRUE) ~ 1,
                                         gvslvue < mean(gvslvue, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA08 %<>% mutate(b.sbstrec = case_when(sbstrec > mean(sbstrec, na.rm = TRUE) ~ 1,
                                         sbstrec < mean(sbstrec, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA08 %<>% mutate(b.sbbsntx = case_when(sbbsntx > mean(sbbsntx, na.rm = TRUE) ~ 1,
                                         sbbsntx < mean(sbbsntx, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA08 %<>% mutate(b.sblazy = case_when(sblazy > mean(sblazy, na.rm = TRUE) ~ 1,
                                        sblazy < mean(sblazy, na.rm = TRUE) ~ -1,
                                        TRUE ~ 0))

DATA08 %<>% mutate(b.uentrjb = case_when(uentrjb > mean(uentrjb, na.rm = TRUE) ~ 1,
                                         uentrjb < mean(uentrjb, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA08 %<>% mutate(b.bennent = case_when(bennent > mean(bennent, na.rm = TRUE) ~ 1,
                                         bennent < mean(bennent, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA08 %<>% mutate(b.lbenent = case_when(lbenent > mean(lbenent, na.rm = TRUE) ~ 1,
                                         lbenent < mean(lbenent, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA08 %<>% mutate(b.sbprvpv = case_when(sbprvpv > mean(sbprvpv, na.rm = TRUE) ~ 1,
                                         sbprvpv < mean(sbprvpv, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA08 %<>% mutate(b.gvslvol = case_when(gvslvol > mean(gvslvol, na.rm = TRUE) ~ 1,
                                         gvslvol < mean(gvslvol, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA08 %<>% mutate(b.gvcldcr = case_when(gvcldcr > mean(gvcldcr, na.rm = TRUE) ~ 1,
                                         gvcldcr < mean(gvcldcr, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA08 %<>% mutate(b.mnrgtjb = case_when(mnrgtjb > mean(mnrgtjb, na.rm = TRUE) ~ 1,
                                         mnrgtjb < mean(mnrgtjb, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA08 %<>% mutate(b.freehms = case_when(freehms > mean(freehms, na.rm = TRUE) ~ 1,
                                         freehms < mean(freehms, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA08 %<>% mutate(b.imueclt = case_when(imueclt > mean(imueclt, na.rm = TRUE) ~ 1,
                                         imueclt < mean(imueclt, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA08 %<>% mutate(b.imwbcnt = case_when(imwbcnt > mean(imwbcnt, na.rm = TRUE) ~ 1,
                                         imwbcnt < mean(imwbcnt, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA08 %<>% mutate(b.Immigration = case_when(Immigration > mean(Immigration, na.rm = TRUE) ~ 1,
                                             Immigration < mean(Immigration, na.rm = TRUE) ~ -1,
                                             TRUE ~ 0))

#Economic issue alignment
DATA08 <- DATA08 %>% mutate(isal_econ2 = Blrecon*b.gincdif + Blrecon*b.dfincac +
                              Blrecon*b.smdfslv + Blrecon*b.gvslvol +Blrecon*b.gvslvue +Blrecon*b.gvcldcr +
                              Blrecon*b.sbstrec + Blrecon*b.sbprvpv + Blrecon*b.sbbsntx + Blrecon*b.sblazy +
                              Blrecon*b.uentrjb + Blrecon*b.lbenent + Blrecon*b.bennent)

#Sociocultural issue alignment
DATA08 <- DATA08 %>% mutate(isal_soc2 = Bgaltan*b.mnrgtjb + Bgaltan*b.freehms + Bgaltan*b.imueclt + Bgaltan*b.imwbcnt + Bgaltan*b.Immigration)

#Robust versions
#DATA08 <- DATA08 %>% mutate(ROBisal_econ = Blrecon*b.gvslvue + Blrecon*b.sbstrec + Blrecon*b.sbbsntx + Blrecon*b.sblazy +
#                              Blrecon*b.uentrjb + Blrecon*b.bennent)
#DATA08 <- DATA08 %>% mutate(st.ROBisal_econ = scale(ROBisal_econ))
#DATA08 <- DATA08 %>% mutate(ROB1isal_soc = Bgaltan*b.Immigration + Bgaltan*b.imueclt + Bgaltan*b.imwbcnt)
#DATA08 <- DATA08 %>% mutate(st.ROB1isal_soc = scale(ROB1isal_soc))
#DATA08 <- DATA08 %>% mutate(ROB2isal_soc = Bgaltan*b.mnrgtjb + Bgaltan*b.freehms)
#DATA08 <- DATA08 %>% mutate(st.ROB2isal_soc = scale(ROB2isal_soc))

#Interest in Politics (proxy for sophistication)
#define NAs
int <- DATA08$polintr
int[int > 4] <- NA
DATA08$polintr <- int

#reverse code (interested higher)
DATA08 %<>% mutate(r.polintr = -1*polintr+5)

#education
ed <- DATA08$eisced
ed[ed > 54] <- NA
DATA08$eisced <- ed

#gender
gen <- DATA08$gndr
gen[gen > 2] <- NA
DATA08$gndr <- gen

DATA08 %<>% mutate(galtan_ext = abs(galtan - 5))
DATA08 %<>% mutate(lrecon_ext = abs(lrecon - 5))
DATA08 %<>% mutate(st.isal_econ = scale(isal_econ2))
DATA08 %<>% mutate(st.isal_soc = scale(isal_soc2))
DATA08 %<>% rename(galtan_sd = std_galtan, lrecon_sd = std_lrecon)

DATA08FIN <- DATA08 %>% dplyr::select(family, st.isal_econ, st.isal_soc, r.polintr, gndr, eisced,lrecon_ext, galtan_ext, galtan_sd, lrecon_sd, cntry, PARTISAN)
DATA08FIN %<>% mutate(wave = "2008")
DATA16FIN <- ICDATA %>% dplyr::select(family, st.isal_econ, st.isal_soc, r.polintr, gndr, eisced, lrecon_ext, galtan_ext, galtan_sd, lrecon_sd, cntry, PARTISAN)
DATA16FIN %<>% mutate(wave = "2016")
DATAFIN <- rbind(DATA08FIN, DATA16FIN)

#Extremity
mod1 <- lmer(st.isal_econ ~ r.polintr + gndr + eisced + lrecon_ext + wave + lrecon_ext*wave + (wave | cntry:PARTISAN), data=DATAFIN)
#arm::display(mod1)
mod1P <- plot_model(mod1, type = "int", axis.title = c("Left-Right Extremity", "Partisan Issue Alignment"),
           title = "Economic")

mod2 <- lmer(st.isal_soc ~ r.polintr + gndr + eisced + galtan_ext + wave + galtan_ext*wave + (wave | cntry:PARTISAN), data=DATAFIN)
#arm::display(mod2)
mod2P <-plot_model(mod2, type = "int", axis.title = c("GALT-TAN Extremity", "Partisan Issue Alignment"),
           title = "Sociocultural")

svg("Extremity08.svg", width = 8, height = 4)
mod1P + mod2P
dev.off()

#Blurring
mod3 <- lmer(st.isal_econ ~ r.polintr + gndr + eisced + lrecon_sd + wave + lrecon_sd*wave + (wave | cntry:PARTISAN), data=DATAFIN)
#arm::display(mod3)
mod3P <- plot_model(mod3, type = "int", axis.title = c("Left-Right Position Blurring", "Partisan Issue Alignment"),
           title = "Economic")

mod4 <- lmer(st.isal_soc ~ r.polintr + gndr + eisced + galtan_sd + wave + galtan_sd*wave + (wave | cntry:PARTISAN), data=DATAFIN)
#arm::display(mod4)
mod4P <-plot_model(mod4, type = "int", axis.title = c("GALT-TAN Position Blurring", "Partisan Issue Alignment"),
           title = "Sociocultural")

svg("Blurrring08.svg", width = 8, height = 4)
mod3P + mod4P
dev.off()

#Families
DATAFIN$family <- factor(DATAFIN$family, levels=c('RR', 'Cons', 'ChristDem', 'Lib', 'Soc', "Green", "RLeft"))


mod5 <- lmer(st.isal_econ ~ family + wave + family*wave + (1 | cntry:PARTISAN), data=DATAFIN)
arm::display(mod6)
mod5P <- plot_model(M, type = "int", title = "Economic", axis.title = c("", "Partisan Issue Alignment"), colors = c("darkblue", "darkred"), show.legend = FALSE) + coord_flip()

mod6 <- lmer(st.isal_soc ~ family + wave + family*wave + (1 | cntry:PARTISAN), data=DATAFIN)
mod6P <- plot_model(L, type = "int", title = "Sociocultural", axis.title = c("", "Partisan Issue Alignment"), colors = c("darkblue", "darkred"), legend.title = "") + coord_flip()

svg("Families08.svg", width = 8, height = 4)
mod5P + mod6P
dev.off()




table(CHES2006$family)


