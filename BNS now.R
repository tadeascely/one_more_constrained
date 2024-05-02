library(NetworkComparisonTest)
library(qgraph)
library(bootnet)
library(patchwork)
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

# Suppress all warnings
options(warn = -1)

#import CHES
CHES <- read.csv("C:/Users/tadec/OneDrive - MUNI/GitHub/no_conservative_left/data/1999-2019_CHES_dataset_means(v3).csv")
ches_countries <- c(1,21, 3, 22, 5, 14, 6, 11, 23, 7, 10, 26, 12, 16, 29)
CHES %<>% subset(country %in% ches_countries)
CHES2014 <- CHES %>% subset(year == 2014)


#renaming parties with the same abbreviation
CHES2014$party[which(CHES2014$country == 29 & CHES2014$party == "SD")] <- "SD(si)"
CHES2014$party[which(CHES2014$country == 16 & CHES2014$party == "SD")] <- "SD(se)"

CHES2014$party[which(CHES2014$country == 3 & CHES2014$party == "CDU")] <- "CDU(de)"
CHES2014$party[which(CHES2014$country == 12 & CHES2014$party == "CDU")] <- "CDU(pt)"

CHES2014$party[which(CHES2014$country == 12 & CHES2014$party == "PS")] <- "PS(pt)"
CHES2014$party[which(CHES2014$country == 6 & CHES2014$party == "PS")] <- "PS(fr)"
CHES2014$party[which(CHES2014$country == 14 & CHES2014$party == "PS")] <- "PS(fi)"
CHES2014$party[which(CHES2014$country == 1 & CHES2014$party == "PS")] <- "PS(be)"

CHES2014$party[which(CHES2014$country == 14 & CHES2014$party == "KD")] <- "KD(fi)"
CHES2014$party[which(CHES2014$country == 16 & CHES2014$party == "KD")] <- "KD(se)"

CHES2014$party[which(CHES2014$country == 5 & CHES2014$party == "PP")] <- "PP(es)"
CHES2014$party[which(CHES2014$country == 16 & CHES2014$party == "PP")] <- "PP(pt)"

CHES2014$party[which(CHES2014$country == 10 & CHES2014$party == "sP")] <- "SP(nl)"

CHES2014 %<>% mutate(PARTISAN = party)

#import ESS
ESS8 <- read.csv("C:/Users/tadec/OneDrive - MUNI/Resources/wd/ESS-Data-Wizard-subset-2023-01-09.csv")
ESS8[ESS8 == 66] <- NA
ESS8[ESS8 == 77] <- NA
ESS8[ESS8 == 88] <- NA
ESS8[ESS8 == 99] <- NA


#####
#ESS8: recoding parties and merging with CHES
#####
#Belgium
ESS8 %<>% mutate(prtclcbe = case_when(prtclcbe == 1 ~ "Groen",
                                      prtclcbe == 2 ~ "CD&V",
                                      prtclcbe == 3 ~ "NVA",
                                      prtclcbe == 5 ~ "SPA",
                                      prtclcbe == 6 ~ "PVDA",
                                      prtclcbe == 7 ~ "VB",
                                      prtclcbe == 8 ~ "VLD",
                                      prtclcbe == 10 ~ "ECOLO",
                                      prtclcbe == 12 ~ "MR",
                                      prtclcbe == 13 ~ "PS(be)",
                                      prtclcbe == 14 ~ "PVDA",
                                      TRUE ~ NA_character_))
#table(ESS8$prtclcbe)


#Czechia
ESS8 %<>% mutate(prtcldcz = case_when(prtcldcz == 1 ~ "KSCM",
                                      prtcldcz == 2 ~ "CSSD",
                                      prtcldcz == 3 ~ "TOP09",
                                      prtcldcz == 4 ~ "ANO2011",
                                      prtcldcz == 5 ~ "ODS",
                                      prtcldcz == 6 ~ "KDU-CSL",
                                      prtcldcz == 7 ~ "USVIT",
                                      TRUE ~ NA_character_))
#table(ESS8$prtcldcz)



#Germany
ESS8 %<>% mutate(prtclede = case_when(prtclede == 1 & region == "DE2" ~ "CSU",
                                      prtclede == 1 & region != "DE2" ~ "CDU(de)",
                                      prtclede == 2 ~ "SPD",
                                      prtclede == 3 ~ "LINKE",
                                      prtclede == 4 ~ "Grunen",
                                      prtclede == 5 ~ "FDP",
                                      prtclede == 6 ~ "AfD",
                                      prtclede == 7 ~ "Piraten",
                                      TRUE ~ NA_character_))
#table(ESS8$prtclede)

#Estonia
ESS8 %<>% mutate(prtclfee = case_when(prtclfee == 1 ~ "ER",
                                      prtclfee == 2 ~ "EK",
                                      prtclfee == 3 ~ "IRL",
                                      prtclfee == 4 ~ "SDE",
                                      prtclfee == 11 ~ "EVE",
                                      TRUE ~ NA_character_))
#table(ESS8$prtclfee)

#Spain
ESS8 %<>% mutate(prtclees = case_when(prtclees == 1 ~ "PP(es)",
                                      prtclees == 2 ~ "PSOE",
                                      prtclees == 4 ~ "Podemos",
                                      prtclees == 5 ~ "C's",
                                      prtclees == 6 ~ "IU",
                                      prtclees == 10 ~ "ERC",
                                      prtclees == 13 ~ "EAJ/PNV",
                                      prtclees == 16 ~ "CC",
                                      TRUE ~ NA_character_))
table(ESS8$prtclees)

#Finland
ESS8 %<>% mutate(prtcldfi = case_when(prtcldfi == 1 ~ "KOK",
                                      prtcldfi == 2 ~ "RKP/SFP",
                                      prtcldfi == 3 ~ "KESK",
                                      prtcldfi == 4 ~ "PS(fi)",
                                      prtcldfi == 5 ~ "KD(fi)",
                                      prtcldfi == 10 ~ "VIHR",
                                      prtcldfi == 11 ~ "SDP",
                                      prtcldfi == 12 ~ "VAS",
                                      TRUE ~ NA_character_))

#table(ESS8$prtcldfi)

#France
ESS8 %<>% mutate(prtclefr = case_when(prtclefr == 2 ~ "FN",
                                      prtclefr == 6 ~ "PG",
                                      prtclefr == 7 ~ "PCF",
                                      prtclefr == 8 ~ "PRG",
                                      prtclefr == 9 ~ "MPF",
                                      prtclefr == 11 ~ "PS(fr)",
                                      prtclefr == 12 ~ "UMP",
                                      prtclefr == 13 ~ "MODEM",
                                      prtclefr == 14 ~ "EELV",
                                      TRUE ~ NA_character_))
#table(ESS8$prtclefr)

#United Kingdom
ESS8 %<>% mutate(prtclbgb = case_when(prtclbgb == 1 ~ "CONS",
                                      prtclbgb == 2 ~ "LAB",
                                      prtclbgb == 3 ~ "LibDem",
                                      prtclbgb == 4 ~ "SNP",
                                      prtclbgb == 5 ~ "PLAID",
                                      prtclbgb == 6 ~ "GREEN",
                                      prtclbgb == 7 ~ "UKIP",
                                      TRUE ~ NA_character_))
#table(ESS8$prtclbgb)

#Hungary
ESS8 %<>% mutate(prtclfhu = case_when(prtclfhu == 1 ~ "DK",
                                      prtclfhu == 2 ~ "E14",
                                      prtclfhu == 3 ~ "Fidesz",
                                      prtclfhu == 4 ~ "JOBBIK",
                                      prtclfhu == 6 ~ "LMP",
                                      prtclfhu == 7 ~ "MSZP",
                                      TRUE ~ NA_character_))
#table(ESS8$prtclfhu)

#Ireland
ESS8 %<>% mutate(prtcldie = case_when(prtcldie == 1 ~ "PBPA",
                                      prtcldie == 2 ~ "FF",
                                      prtcldie == 3 ~ "FG",
                                      prtcldie == 4 ~ "GP",
                                      prtcldie == 6 ~ "Lab",
                                      prtcldie == 7 ~ "SF",
                                      TRUE ~ NA_character_))
#table(ESS8$prtcldie)

#Netherlands
ESS8 %<>% mutate(prtclenl = case_when(prtclenl == 1 ~ "VVD",
                                      prtclenl == 2 ~ "PvdA",
                                      prtclenl == 3 ~ "PVV",
                                      prtclenl == 4 ~ "SP(nl)",
                                      prtclenl == 5 ~ "CDA",
                                      prtclenl == 6 ~ "D66",
                                      prtclenl == 7 ~ "CU",
                                      prtclenl == 8 ~ "GL",
                                      prtclenl == 9 ~ "SGP",
                                      prtclenl == 10 ~ "PvdD",                                      
                                      prtclenl == 11 ~ "50PLUS",
                                      TRUE ~ NA_character_))
#table(ESS8$prtclenl)


#Poland
ESS8 %<>% mutate(prtclgpl = case_when(prtclgpl == 4 ~ "PO",
                                      prtclgpl == 5 ~ "PSL",
                                      prtclgpl == 6 ~ "PiS",
                                      prtclgpl == 8 ~ "SLD",
                                      TRUE ~ NA_character_))
#table(ESS8$prtclgpl)

#Portugal

ESS8 %<>% mutate(prtclept = case_when(prtclept == 2 ~ "BE",
                                      prtclept == 3 ~ "CDU(pt)",
                                      prtclept == 13 ~ "PS(pt)",
                                      prtclept == 16 ~ "PSD",
                                      prtclept == 17 ~ "PP(pt)",
                                      TRUE ~ NA_character_))
#table(ESS8$prtclept)

#Sweden
ESS8 %<>% mutate(prtclbse = case_when(prtclbse == 1 ~ "C",
                                      prtclbse == 2 ~ "FP",
                                      prtclbse == 3 ~ "KD(se)",
                                      prtclbse == 4 ~ "MP",
                                      prtclbse == 5 ~ "M",
                                      prtclbse == 6 ~ "SAP",
                                      prtclbse == 7 ~ "V",
                                      prtclbse == 8 ~ "FI",
                                      prtclbse == 10 ~ "SD(se)",
                                      TRUE ~ NA_character_))
#table(ESS8$prtclbse)

#Slovenia
ESS8 %<>% mutate(prtclesi = case_when(prtclesi == 1 ~ "DeSUS",
                                      prtclesi == 3 ~ "NSI",
                                      prtclesi == 5 ~ "SD(si)",
                                      prtclesi == 6 ~ "SDS",
                                      prtclesi == 7 ~ "SLS",
                                      prtclesi == 8 ~ "SMC",
                                      prtclesi == 10 ~ "ZaAB",
                                      prtclesi == 11 ~ "ZL",
                                      TRUE ~ NA_character_))
#table(ESS8$prtclesi)

ESS8 %<>% mutate(PARTISAN = str_c(str_replace_na(prtclcbe, replacement = ""), str_replace_na(prtcldcz, replacement = ""), str_replace_na(prtclede, replacement = ""),
                                  str_replace_na(prtclfee, replacement = ""), str_replace_na(prtclees, replacement = ""), str_replace_na(prtcldfi, replacement = ""),
                                  str_replace_na(prtclefr, replacement = ""), str_replace_na(prtclbgb, replacement = ""), str_replace_na(prtclfhu, replacement = ""),
                                  str_replace_na(prtcldie, replacement = ""), str_replace_na(prtclenl, replacement = ""), str_replace_na(prtclgpl, replacement = ""),
                                  str_replace_na(prtclept, replacement = ""), str_replace_na(prtclbse, replacement = ""), str_replace_na(prtclesi, replacement = "")))
ESS8$PARTISAN[ESS8$PARTISAN == ""] <- NA
DATA <- merge(ESS8, CHES2014, by = "PARTISAN")
rm(ESS8, CHES)

#####
#DEFINING MISSING VALUES - ALL ATTITUDES
#####

allfour <- c("imsmetn",	"imdfetn",	"impcntr")
a4 <- DATA %>% dplyr::select(all_of(allfour))
a4[a4 > 4] <- NA
DATA[allfour] <- a4


allfive <- c("gincdif",	"mnrgtjb",	"freehms",	"hmsfmlsh",	"hmsacld",	"dfincac",	"smdfslv",	"sbstrec",	"sbprvpv",	"sbbsntx",	"sblazy",	"imsclbn",	"uentrjb",	"lbenent",	"bennent")
a5 <- DATA %>% dplyr::select(all_of(allfive))
a5[a5 > 5] <- NA
DATA[allfive] <- a5  

allsix <- c("impsafe",	"ipfrule",	"ipudrst",	"ipstrgv",	"ipbhprp",	"imptrad")
a6 <- DATA %>% dplyr::select(all_of(allsix))
a6[a6 > 6] <- NA
DATA[allsix] <- a6


allten <- c("euftf",	"imbgeco",	"imueclt",	"imwbcnt",	"gvslvol",	"gvslvue",	"gvcldcr")
a10 <- DATA %>% dplyr::select(all_of(allten))
a10[a10 > 10] <- NA
DATA[allten] <- a10

rm(a4, a5, a6, a10)



######
#REVERSE CODE SELECTED VARIABLES
######
#LRECON..... right = higher
#GALTAN.....TAN = higher

#5 levels
fivelevels <- c("mnrgtjb", "hmsfmlsh", "dfincac", "sbstrec", "sbbsntx", "sblazy", "uentrjb", "bennent")
d5 <- DATA %>% dplyr::select(all_of(fivelevels))
d5 <- d5 %>% mutate(across(fivelevels, function(x) -1*x+6))
DATA[fivelevels] <- d5

#6 levels
sixlevels <- c("impsafe", "ipfrule", "ipstrgv", "ipbhprp", "imptrad")
d6 <- DATA %>% dplyr::select(all_of(sixlevels))
d6 <- d6 %>% mutate(across(sixlevels, function(x) -1*x+7))
DATA[sixlevels] <- d6

#10 levels (+ 0)
tenlevels <- c("euftf", "imbgeco", "imueclt", "imwbcnt", "gvslvol", "gvslvue", "gvcldcr")
d10 <- DATA %>% dplyr::select(all_of(tenlevels))
d10 <- d10 %>% mutate(across(tenlevels, function(x) -1*x+10))
DATA[tenlevels] <- d10

rm(d5, d6, d10)




#####
#First analysis (Ideological Congruence - IC)
#####
ICDATA2 <- DATA
#####
#OUTCOME VARIABLE
#####
ICDATA2 <- ICDATA2 %>% mutate(Blrecon = case_when(lrecon > 5 ~ 1,
                                                  lrecon < 5 ~ -1,
                                                  TRUE ~ NA_real_))

ICDATA2 <- ICDATA2 %>% mutate(Bgaltan = case_when(galtan > 5 ~ 1,
                                                  galtan < 5 ~ -1,
                                                  TRUE ~ NA_real_))

#cronbach.alpha(ALL[immig], na.rm = TRUE)
ICDATA2 %<>% mutate(Immigration = (imdfetn + impcntr + imsmetn)/3)



ICDATA2 %<>% mutate(b.gincdif = case_when(gincdif > mean(gincdif, na.rm = TRUE) ~ 1,
                                          gincdif < mean(gincdif, na.rm = TRUE) ~ -1,
                                          TRUE ~ 0))

ICDATA2 %<>% mutate(b.dfincac = case_when(dfincac > mean(dfincac, na.rm = TRUE) ~ 1,
                                          dfincac < mean(dfincac, na.rm = TRUE) ~ -1,
                                          TRUE ~ 0))

ICDATA2 %<>% mutate(b.smdfslv = case_when(smdfslv > mean(smdfslv, na.rm = TRUE) ~ 1,
                                          smdfslv < mean(smdfslv, na.rm = TRUE) ~ -1,
                                          TRUE ~ 0))

ICDATA2 %<>% mutate(b.gvslvue = case_when(gvslvue > mean(gvslvue, na.rm = TRUE) ~ 1,
                                          gvslvue < mean(gvslvue, na.rm = TRUE) ~ -1,
                                          TRUE ~ 0))

ICDATA2 %<>% mutate(b.sbstrec = case_when(sbstrec > mean(sbstrec, na.rm = TRUE) ~ 1,
                                          sbstrec < mean(sbstrec, na.rm = TRUE) ~ -1,
                                          TRUE ~ 0))

ICDATA2 %<>% mutate(b.sbbsntx = case_when(sbbsntx > mean(sbbsntx, na.rm = TRUE) ~ 1,
                                          sbbsntx < mean(sbbsntx, na.rm = TRUE) ~ -1,
                                          TRUE ~ 0))

ICDATA2 %<>% mutate(b.sblazy = case_when(sblazy > mean(sblazy, na.rm = TRUE) ~ 1,
                                         sblazy < mean(sblazy, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

ICDATA2 %<>% mutate(b.uentrjb = case_when(uentrjb > mean(uentrjb, na.rm = TRUE) ~ 1,
                                          uentrjb < mean(uentrjb, na.rm = TRUE) ~ -1,
                                          TRUE ~ 0))

ICDATA2 %<>% mutate(b.bennent = case_when(bennent > mean(bennent, na.rm = TRUE) ~ 1,
                                          bennent < mean(bennent, na.rm = TRUE) ~ -1,
                                          TRUE ~ 0))

ICDATA2 %<>% mutate(b.lbenent = case_when(lbenent > mean(lbenent, na.rm = TRUE) ~ 1,
                                          lbenent < mean(lbenent, na.rm = TRUE) ~ -1,
                                          TRUE ~ 0))

ICDATA2 %<>% mutate(b.sbprvpv = case_when(sbprvpv > mean(sbprvpv, na.rm = TRUE) ~ 1,
                                          sbprvpv < mean(sbprvpv, na.rm = TRUE) ~ -1,
                                          TRUE ~ 0))

ICDATA2 %<>% mutate(b.gvslvol = case_when(gvslvol > mean(gvslvol, na.rm = TRUE) ~ 1,
                                          gvslvol < mean(gvslvol, na.rm = TRUE) ~ -1,
                                          TRUE ~ 0))

ICDATA2 %<>% mutate(b.gvcldcr = case_when(gvcldcr > mean(gvcldcr, na.rm = TRUE) ~ 1,
                                          gvcldcr < mean(gvcldcr, na.rm = TRUE) ~ -1,
                                          TRUE ~ 0))

ICDATA2 %<>% mutate(b.mnrgtjb = case_when(mnrgtjb > mean(mnrgtjb, na.rm = TRUE) ~ 1,
                                          mnrgtjb < mean(mnrgtjb, na.rm = TRUE) ~ -1,
                                          TRUE ~ 0))

ICDATA2 %<>% mutate(b.freehms = case_when(freehms > mean(freehms, na.rm = TRUE) ~ 1,
                                          freehms < mean(freehms, na.rm = TRUE) ~ -1,
                                          TRUE ~ 0))

ICDATA2 %<>% mutate(b.hmsfmlsh = case_when(hmsfmlsh > mean(hmsfmlsh, na.rm = TRUE) ~ 1,
                                           hmsfmlsh < mean(hmsfmlsh, na.rm = TRUE) ~ -1,
                                           TRUE ~ 0))

ICDATA2 %<>% mutate(b.hmsacld = case_when(hmsacld > mean(hmsacld, na.rm = TRUE) ~ 1,
                                          hmsacld < mean(hmsacld, na.rm = TRUE) ~ -1,
                                          TRUE ~ 0))

ICDATA2 %<>% mutate(b.imueclt = case_when(imueclt > mean(imueclt, na.rm = TRUE) ~ 1,
                                          imueclt < mean(imueclt, na.rm = TRUE) ~ -1,
                                          TRUE ~ 0))

ICDATA2 %<>% mutate(b.imwbcnt = case_when(imwbcnt > mean(imwbcnt, na.rm = TRUE) ~ 1,
                                          imwbcnt < mean(imwbcnt, na.rm = TRUE) ~ -1,
                                          TRUE ~ 0))

ICDATA2 %<>% mutate(b.Immigration = case_when(Immigration > mean(Immigration, na.rm = TRUE) ~ 1,
                                              Immigration < mean(Immigration, na.rm = TRUE) ~ -1,
                                              TRUE ~ 0))


#Economic issue alignment
ICDATA2 <- ICDATA2 %>% mutate(isal_econ2 = Blrecon*b.gincdif + Blrecon*b.dfincac +
                                Blrecon*b.smdfslv + Blrecon*b.gvslvol +Blrecon*b.gvslvue +Blrecon*b.gvcldcr +
                                Blrecon*b.sbstrec + Blrecon*b.sbprvpv + Blrecon*b.sbbsntx + Blrecon*b.sblazy +
                                Blrecon*b.uentrjb + Blrecon*b.lbenent + Blrecon*b.bennent)


#Cultural issue alignment
ICDATA2 <- ICDATA2 %>% mutate(isal_soc2 = Bgaltan*b.mnrgtjb + Bgaltan*b.freehms + Bgaltan*b.hmsfmlsh +
                                Bgaltan*b.hmsacld + Bgaltan*b.imueclt + Bgaltan*b.imwbcnt + Bgaltan*b.Immigration)

#####
#Robust versions
#####

ICDATA2 <- ICDATA2 %>% mutate(ROBisal_econ = Blrecon*b.gvslvue + Blrecon*b.sbstrec + Blrecon*b.sbbsntx + Blrecon*b.sblazy +
                                Blrecon*b.uentrjb + Blrecon*b.bennent)
ICDATA2 <- ICDATA2 %>% mutate(st.ROBisal_econ = scale(ROBisal_econ))
ICDATA2 <- ICDATA2 %>% mutate(ROB1isal_soc = Bgaltan*b.Immigration + Bgaltan*b.imueclt + Bgaltan*b.imwbcnt)
ICDATA2 <- ICDATA2 %>% mutate(st.ROB1isal_soc = scale(ROB1isal_soc))
ICDATA2 <- ICDATA2 %>% mutate(ROB2isal_soc = Bgaltan*b.mnrgtjb + Bgaltan*b.freehms  + Bgaltan*b.hmsfmlsh +
                                Bgaltan*b.hmsacld)
ICDATA2 <- ICDATA2 %>% mutate(st.ROB2isal_soc = scale(ROB2isal_soc))


#####
#INDEPENDENT VARIABLES
#####
#COUNTRY-LEVEL
ICCHES <- CHES2014
ICCHES <- ICCHES %>% dplyr::select(vote, lrecon, galtan, country) %>% na.omit()

#ELITE POLARIZATION
ICCHES <- ICCHES %>% group_by(country) %>%  mutate(DaltonPPEcon = sqrt(sum((((lrecon - (sum((lrecon*vote)/sum(vote))))/5)^2)*vote)))
ICCHES <- ICCHES %>% group_by(country) %>%  mutate(DaltonPPSoc = sqrt(sum((((galtan - (sum((galtan*vote)/sum(vote))))/5)^2)*vote)))

#PARTY SYSTEM INCONSISTENCY (Dassonneville, Fournier, Somer-Topcu)
ICCHES <- ICCHES %>% group_by(country) %>%  mutate(PSI = sum((vote/100)*(abs(lrecon - galtan))))

ICCHES <- aggregate(ICCHES, list(ICCHES$country), mean) %>% dplyr::select(country, DaltonPPEcon, DaltonPPSoc, PSI)

#After making necessary changes, merge with ICDATA2 dataframe
ICDATA2 <- merge(ICDATA2, ICCHES, by = "country")

#PARTY IDEOLOGICAL CONSISTENCY ACROSS DIMENSIONS
ICDATA2 <- ICDATA2 %>% mutate(PI = abs(lrecon - galtan))

#Interest in Politics (proxy for sophistication)
#define NAs
int <- ICDATA2$polintr
int[int > 4] <- NA
ICDATA2$polintr <- int

#reverse code (interested higher)
ICDATA2 %<>% mutate(r.polintr = -1*polintr+5)


ALL <- ICDATA2
#education
ed <- ALL$eisced
ed[ed > 54] <- NA
ALL$eisced <- ed

#gender
gen <- ALL$gndr
gen[gen > 2] <- NA
ALL$gndr <- gen

ALL %<>% mutate(charfamily = case_when(family == 1 ~  "RADRT",
                                       family == 2 ~  "CON",
                                       family == 3 ~  "LIB",
                                       family == 4 ~  "CD",  
                                       family == 5 ~  "SOC", 
                                       family == 6 ~  "RADLEFT", 
                                       family == 7 ~  "GREEN", 
                                       TRUE ~ NA_character_))

ALL %<>% mutate(mainstream = case_when(family == 2 | family == 3 | family == 4 | family == 5  ~ "Mainstream",
                                       family == 1 | family == 6 | family == 7 ~ "Challenger",
                                       TRUE ~ "Other"))

ALL %<>% mutate(radical = case_when(family == 1 ~ "RADR",
                                    family == 6 ~  "RADLEFT",
                                    family == 7 ~ "GREEN",
                                    TRUE ~ "DIFFERENT"))

ALL %<>% mutate(st.isal_econ = scale(isal_econ2))
ALL %<>% mutate(st.isal_soc = scale(isal_soc2))


ALL$charfamily <- factor(ALL$charfamily, levels = c("RADRT", "CON", "CD", "LIB",
                                                    "SOC", "GREEN", "RADLEFT"))

source("PS Transformation.R")

RADRT <- ICDATA %>% filter(family == "RR")
RADLEFT <- ICDATA %>% filter(family == "RLeft")
GREEN <- ICDATA %>% filter(family == "Green")
CON <- ICDATA %>% filter(family == "Cons")
LIB <- ICDATA %>% filter(family == "Lib")
SOC <- ICDATA %>% filter(family == "Soc")
CD <- ICDATA %>% filter(family == "ChristDem")


Network_RADRT <- estimateNetwork(RADRT[all_att], default = "EBICglasso", corMethod = "spearman")
Network_RADLEFT <- estimateNetwork(RADLEFT[all_att], default = "EBICglasso", corMethod = "spearman")
Network_GREEN <- estimateNetwork(GREEN[all_att], default = "EBICglasso", corMethod = "spearman")
Network_CON <- estimateNetwork(CON[all_att], default = "EBICglasso", corMethod = "spearman")
Network_LIB <- estimateNetwork(LIB[all_att], default = "EBICglasso", corMethod = "spearman")
Network_SOC <- estimateNetwork(SOC[all_att], default = "EBICglasso", corMethod = "spearman")
Network_CD <- estimateNetwork(CD[all_att], default = "EBICglasso", corMethod = "spearman")


GREEN_CON <- NCT(Network_GREEN, Network_CON,
                 it = 1000,
                 progressbar = TRUE, abs = FALSE)
#RADRT_CON

#GLOBAL EXPECTED INFLUENCE INVARIANCE TEST 
#Global expected influence per group:  5.223707 6.221247 
#Test statistic S:  0.9975401 
#p-value 0.047 


GREEN_LIB <- NCT(Network_GREEN, Network_LIB,
                 it = 1000,
                 progressbar = TRUE, abs = FALSE)
#RADRT_LIB

#GLOBAL EXPECTED INFLUENCE INVARIANCE TEST 
#Global expected influence per group:  5.223707 6.762832 
#Test statistic S:  1.539126 
#p-value 0 



GREEN_SOC <- NCT(Network_GREEN, Network_SOC,
                 it = 1000,
                 progressbar = TRUE, abs = FALSE)
#RADRT_SOC

#GLOBAL EXPECTED INFLUENCE INVARIANCE TEST 
#Global expected influence per group:  5.223707 7.180944 
#Test statistic S:  1.957238 
#p-value 0


GREEN_CD <- NCT(Network_GREEN, Network_CD,
                it = 1000,
                progressbar = TRUE, abs = FALSE)

#RADRT_CD

#GLOBAL STRENGTH INVARIANCE TEST 
#Global strength per group:  5.223707 5.702582 
#Test statistic S:  0.4788758 
#p-value 0.299 


GREEN_RADRT <- NCT(Network_GREEN, Network_RADRT,
                   it = 1000,
                   progressbar = TRUE, abs = FALSE)


#OTHER PARTIES COMPARISONS
#####
#CON, LIB, SOC, CD, RADRT
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

Main7 <- NCT(Network_RADRT, Network_CON,
             it = 1000,
             progressbar = TRUE, abs = FALSE)

#Main7

Main8 <- NCT(Network_RADRT, Network_LIB,
             it = 1000,
             progressbar = TRUE, abs = FALSE)

#Main8

Main9 <- NCT(Network_RADRT, Network_SOC,
             it = 1000,
             progressbar = TRUE, abs = FALSE)

#Main9

Main10 <- NCT(Network_RADRT, Network_CD,
              it = 1000,
              progressbar = TRUE, abs = TRUE)

#Main10

# Reset warning to default (typically 0)
options(warn = 0)
