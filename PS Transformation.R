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


#Uploading CHES2014 data
#This dataset contains only this survey wave, it includes expert standard deviations, but not party families!
CHES2014 <- read.csv("C:/Users/tadec/OneDrive - MUNI/GitHub/no_conservative_left/data/2014_CHES_dataset_means.csv")
ches_countries <- c(1,21, 3, 22, 5, 14, 6, 11, 23, 7, 10, 26, 12, 16, 29)
CHES2014 %<>% subset(country %in% ches_countries)

#What about matching the party families from the cummulative data file? then, I would not need two codes and everything

#ESS8 upload
ESS8 <- read.csv("C:/Users/tadec/OneDrive - MUNI/Resources/wd/ESS-Data-Wizard-subset-2023-01-09.csv")
#defining missing values
ESS8[ESS8 == 66] <- NA
ESS8[ESS8 == 77] <- NA
ESS8[ESS8 == 88] <- NA
ESS8[ESS8 == 99] <- NA

#Renaming cross-country duplicities in party names
CHES2014$party_name[which(CHES2014$country == 29 & CHES2014$party_name == "SD")] <- "SD(si)"
CHES2014$party_name[which(CHES2014$country == 16 & CHES2014$party_name == "SD")] <- "SD(se)"

CHES2014$party_name[which(CHES2014$country == 3 & CHES2014$party_name == "CDU")] <- "CDU(de)"
CHES2014$party_name[which(CHES2014$country == 12 & CHES2014$party_name == "CDU")] <- "CDU(pt)"

CHES2014$party_name[which(CHES2014$country == 12 & CHES2014$party_name == "PS")] <- "PS(pt)"
CHES2014$party_name[which(CHES2014$country == 6 & CHES2014$party_name == "PS")] <- "PS(fr)"
CHES2014$party_name[which(CHES2014$country == 14 & CHES2014$party_name == "PS")] <- "PS(fi)"
CHES2014$party_name[which(CHES2014$country == 1 & CHES2014$party_name == "PS")] <- "PS(be)"

CHES2014$party_name[which(CHES2014$country == 14 & CHES2014$party_name == "KD")] <- "KD(fi)"
CHES2014$party_name[which(CHES2014$country == 16 & CHES2014$party_name == "KD")] <- "KD(se)"

CHES2014$party_name[which(CHES2014$country == 5 & CHES2014$party_name == "PP")] <- "PP(es)"
CHES2014$party_name[which(CHES2014$country == 16 & CHES2014$party_name == "PP")] <- "PP(pt)"

CHES2014$party_name[which(CHES2014$country == 10 & CHES2014$party_name == "sP")] <- "SP(nl)"

CHES2014 %<>% mutate(PARTISAN = party_name)


#####
#Changing party names for matching with CHES data
#####
#Belgium
ESS8 %<>% mutate(prtclcbe = case_when(prtclcbe == 1 ~ "Groen",
                                      prtclcbe == 2 ~ "CD&V",
                                      prtclcbe == 3 ~ "N-VA",
                                      prtclcbe == 5 ~ "SPA",
                                      prtclcbe == 6 ~ "PVDA",
                                      prtclcbe == 7 ~ "VB",
                                      prtclcbe == 8 ~ "VLD",
                                      prtclcbe == 10 ~ "ECOLO",
                                      prtclcbe == 12 ~ "MR",
                                      prtclcbe == 13 ~ "PS(be)",
                                      prtclcbe == 14 ~ "PVDA",
                                      TRUE ~ NA_character_))
#Czechia
ESS8 %<>% mutate(prtcldcz = case_when(prtcldcz == 1 ~ "KSCM",
                                      prtcldcz == 2 ~ "CSSD",
                                      prtcldcz == 3 ~ "TOP09",
                                      prtcldcz == 4 ~ "ANO2011",
                                      prtcldcz == 5 ~ "ODS",
                                      prtcldcz == 6 ~ "KDU-CSL",
                                      prtcldcz == 7 ~ "USVIT",
                                      TRUE ~ NA_character_))
#Germany
ESS8 %<>% mutate(prtclede = case_when(prtclede == 1 & region == "DE2" ~ "CSU",
                                      prtclede == 1 & region != "DE2" ~ "CDU(de)",
                                      prtclede == 2 ~ "SPD",
                                      prtclede == 3 ~ "Linke",
                                      prtclede == 4 ~ "Grunen",
                                      prtclede == 5 ~ "FDP",
                                      prtclede == 6 ~ "AfD",
                                      prtclede == 7 ~ "Piraten",
                                      TRUE ~ NA_character_))
#Estonia
ESS8 %<>% mutate(prtclfee = case_when(prtclfee == 1 ~ "ER",
                                      prtclfee == 2 ~ "EK",
                                      prtclfee == 3 ~ "IRL",
                                      prtclfee == 4 ~ "SDE",
                                      prtclfee == 11 ~ "EVE",
                                      TRUE ~ NA_character_))
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
#United Kingdom
ESS8 %<>% mutate(prtclbgb = case_when(prtclbgb == 1 ~ "CONS",
                                      prtclbgb == 2 ~ "LAB",
                                      prtclbgb == 3 ~ "LIBDEM",
                                      prtclbgb == 4 ~ "SNP",
                                      prtclbgb == 5 ~ "PLAID",
                                      prtclbgb == 6 ~ "GREEN",
                                      prtclbgb == 7 ~ "UKIP",
                                      TRUE ~ NA_character_))
#Hungary
ESS8 %<>% mutate(prtclfhu = case_when(prtclfhu == 1 ~ "DK",
                                      prtclfhu == 2 ~ "E14",
                                      prtclfhu == 3 ~ "Fidesz",
                                      prtclfhu == 4 ~ "JOBBIK",
                                      prtclfhu == 6 ~ "LMP",
                                      prtclfhu == 7 ~ "MSZP",
                                      TRUE ~ NA_character_))
#Ireland
ESS8 %<>% mutate(prtcldie = case_when(prtcldie == 1 ~ "PBPA",
                                      prtcldie == 2 ~ "FF",
                                      prtcldie == 3 ~ "FG",
                                      prtcldie == 4 ~ "GP",
                                      prtcldie == 6 ~ "Lab",
                                      prtcldie == 7 ~ "SF",
                                      TRUE ~ NA_character_))
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
                                      prtclenl == 11 ~ "50PLUS",
                                      TRUE ~ NA_character_))
#Poland
ESS8 %<>% mutate(prtclgpl = case_when(prtclgpl == 4 ~ "PO",
                                      prtclgpl == 5 ~ "PSL",
                                      prtclgpl == 6 ~ "PiS",
                                      prtclgpl == 8 ~ "SLD",
                                      TRUE ~ NA_character_))
#Portugal

ESS8 %<>% mutate(prtclept = case_when(prtclept == 2 ~ "BE",
                                      prtclept == 3 ~ "CDU(pt)",
                                      prtclept == 13 ~ "PS(pt)",
                                      prtclept == 16 ~ "PSD",
                                      prtclept == 17 ~ "PP(pt)",
                                      TRUE ~ NA_character_))
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

#Missing names into NAs
ESS8 %<>% mutate(PARTISAN = str_c(str_replace_na(prtclcbe, replacement = ""), str_replace_na(prtcldcz, replacement = ""), str_replace_na(prtclede, replacement = ""),
                                  str_replace_na(prtclfee, replacement = ""), str_replace_na(prtclees, replacement = ""), str_replace_na(prtcldfi, replacement = ""),
                                  str_replace_na(prtclefr, replacement = ""), str_replace_na(prtclbgb, replacement = ""), str_replace_na(prtclfhu, replacement = ""),
                                  str_replace_na(prtcldie, replacement = ""), str_replace_na(prtclenl, replacement = ""), str_replace_na(prtclgpl, replacement = ""),
                                  str_replace_na(prtclept, replacement = ""), str_replace_na(prtclbse, replacement = ""), str_replace_na(prtclesi, replacement = "")))
ESS8$PARTISAN[ESS8$PARTISAN == ""] <- NA

#Compiling harmonized datasets
DATA2 <- merge(ESS8, CHES2014, by = "PARTISAN")



#####
#DEFINING MISSING VALUES - ALL ATTITUDES
#####
allfour <- c("imsmetn",	"imdfetn",	"impcntr")
a4 <- DATA2 %>% dplyr::select(all_of(allfour))
a4[a4 > 4] <- NA
DATA2[allfour] <- a4

allfive <- c("gincdif",	"mnrgtjb",	"freehms",	"hmsfmlsh",	"hmsacld",	"dfincac",	"smdfslv",	"sbstrec",	"sbprvpv",	"sbbsntx",	"sblazy",	"imsclbn",	"uentrjb",	"lbenent",	"bennent")
a5 <- DATA2 %>% dplyr::select(all_of(allfive))
a5[a5 > 5] <- NA
DATA2[allfive] <- a5  

allsix <- c("impsafe",	"ipfrule",	"ipudrst",	"ipstrgv",	"ipbhprp",	"imptrad")
a6 <- DATA2 %>% dplyr::select(all_of(allsix))
a6[a6 > 6] <- NA
DATA2[allsix] <- a6

allten <- c("euftf",	"imbgeco",	"imueclt",	"imwbcnt",	"gvslvol",	"gvslvue",	"gvcldcr")
a10 <- DATA2 %>% dplyr::select(all_of(allten))
a10[a10 > 10] <- NA
DATA2[allten] <- a10

rm(a4, a5, a6, a10)

######
#REVERSE CODE SELECTED VARIABLES
######
#LRECON..... right = higher
#GALTAN.....TAN = higher

#5 levels
fivelevels <- c("mnrgtjb", "hmsfmlsh", "dfincac", "sbstrec", "sbbsntx", "sblazy", "uentrjb", "bennent")
d5 <- DATA2 %>% dplyr::select(all_of(fivelevels))
d5 <- d5 %>% mutate(across(fivelevels, function(x) -1*x+6))
DATA2[fivelevels] <- d5

#6 levels
sixlevels <- c("impsafe", "ipfrule", "ipstrgv", "ipbhprp", "imptrad")
d6 <- DATA2 %>% dplyr::select(all_of(sixlevels))
d6 <- d6 %>% mutate(across(sixlevels, function(x) -1*x+7))
DATA2[sixlevels] <- d6

#10 levels
tenlevels <- c("euftf", "imbgeco", "imueclt", "imwbcnt", "gvslvol", "gvslvue", "gvcldcr")
d10 <- DATA2 %>% dplyr::select(all_of(tenlevels))
d10 <- d10 %>% mutate(across(tenlevels, function(x) -1*x+10))
DATA2[tenlevels] <- d10

rm(d5, d6, d10)

#####
#First analysis (Ideological Congruence - IC)
#####
ICDATA <- DATA2
#####
#OUTCOME VARIABLE
#####
ICDATA <- ICDATA %>% mutate(Blrecon = case_when(lrecon > 5 ~ 1,
                                                lrecon < 5 ~ -1,
                                                TRUE ~ NA_real_))

ICDATA <- ICDATA %>% mutate(Bgaltan = case_when(galtan > 5 ~ 1,
                                                galtan < 5 ~ -1,
                                                TRUE ~ NA_real_))

#Reliability
#immig <- c("imdfetn", "impcntr", "imsmetn")
#ltm::cronbach.alpha(ALL[immig], na.rm = TRUE)
#cronbach.alpha(ALL[immig], na.rm = TRUE) <- reliability checked

ICDATA %<>% mutate(Immigration = (imdfetn + impcntr + imsmetn)/3)

ICDATA %<>% mutate(b.gincdif = case_when(gincdif > mean(gincdif, na.rm = TRUE) ~ 1,
                                         gincdif < mean(gincdif, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

ICDATA %<>% mutate(b.dfincac = case_when(dfincac > mean(dfincac, na.rm = TRUE) ~ 1,
                                         dfincac < mean(dfincac, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

ICDATA %<>% mutate(b.smdfslv = case_when(smdfslv > mean(smdfslv, na.rm = TRUE) ~ 1,
                                         smdfslv < mean(smdfslv, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

ICDATA %<>% mutate(b.gvslvue = case_when(gvslvue > mean(gvslvue, na.rm = TRUE) ~ 1,
                                         gvslvue < mean(gvslvue, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

ICDATA %<>% mutate(b.sbstrec = case_when(sbstrec > mean(sbstrec, na.rm = TRUE) ~ 1,
                                         sbstrec < mean(sbstrec, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

ICDATA %<>% mutate(b.sbbsntx = case_when(sbbsntx > mean(sbbsntx, na.rm = TRUE) ~ 1,
                                         sbbsntx < mean(sbbsntx, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

ICDATA %<>% mutate(b.sblazy = case_when(sblazy > mean(sblazy, na.rm = TRUE) ~ 1,
                                        sblazy < mean(sblazy, na.rm = TRUE) ~ -1,
                                        TRUE ~ 0))

ICDATA %<>% mutate(b.uentrjb = case_when(uentrjb > mean(uentrjb, na.rm = TRUE) ~ 1,
                                         uentrjb < mean(uentrjb, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

ICDATA %<>% mutate(b.bennent = case_when(bennent > mean(bennent, na.rm = TRUE) ~ 1,
                                         bennent < mean(bennent, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

ICDATA %<>% mutate(b.lbenent = case_when(lbenent > mean(lbenent, na.rm = TRUE) ~ 1,
                                         lbenent < mean(lbenent, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

ICDATA %<>% mutate(b.sbprvpv = case_when(sbprvpv > mean(sbprvpv, na.rm = TRUE) ~ 1,
                                         sbprvpv < mean(sbprvpv, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

ICDATA %<>% mutate(b.gvslvol = case_when(gvslvol > mean(gvslvol, na.rm = TRUE) ~ 1,
                                         gvslvol < mean(gvslvol, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

ICDATA %<>% mutate(b.gvcldcr = case_when(gvcldcr > mean(gvcldcr, na.rm = TRUE) ~ 1,
                                         gvcldcr < mean(gvcldcr, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

ICDATA %<>% mutate(b.mnrgtjb = case_when(mnrgtjb > mean(mnrgtjb, na.rm = TRUE) ~ 1,
                                         mnrgtjb < mean(mnrgtjb, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

ICDATA %<>% mutate(b.freehms = case_when(freehms > mean(freehms, na.rm = TRUE) ~ 1,
                                         freehms < mean(freehms, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

ICDATA %<>% mutate(b.hmsfmlsh = case_when(hmsfmlsh > mean(hmsfmlsh, na.rm = TRUE) ~ 1,
                                          hmsfmlsh < mean(hmsfmlsh, na.rm = TRUE) ~ -1,
                                          TRUE ~ 0))

ICDATA %<>% mutate(b.hmsacld = case_when(hmsacld > mean(hmsacld, na.rm = TRUE) ~ 1,
                                         hmsacld < mean(hmsacld, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

ICDATA %<>% mutate(b.imueclt = case_when(imueclt > mean(imueclt, na.rm = TRUE) ~ 1,
                                         imueclt < mean(imueclt, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

ICDATA %<>% mutate(b.imwbcnt = case_when(imwbcnt > mean(imwbcnt, na.rm = TRUE) ~ 1,
                                         imwbcnt < mean(imwbcnt, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

ICDATA %<>% mutate(b.Immigration = case_when(Immigration > mean(Immigration, na.rm = TRUE) ~ 1,
                                             Immigration < mean(Immigration, na.rm = TRUE) ~ -1,
                                             TRUE ~ 0))

#Economic issue alignment
ICDATA <- ICDATA %>% mutate(isal_econ2 = Blrecon*b.gincdif + Blrecon*b.dfincac +
                              Blrecon*b.smdfslv + Blrecon*b.gvslvol +Blrecon*b.gvslvue +Blrecon*b.gvcldcr +
                              Blrecon*b.sbstrec + Blrecon*b.sbprvpv + Blrecon*b.sbbsntx + Blrecon*b.sblazy +
                              Blrecon*b.uentrjb + Blrecon*b.lbenent + Blrecon*b.bennent)

#Sociocultural issue alignment
ICDATA <- ICDATA %>% mutate(isal_soc2 = Bgaltan*b.mnrgtjb + Bgaltan*b.freehms + Bgaltan*b.hmsfmlsh +
                              Bgaltan*b.hmsacld + Bgaltan*b.imueclt + Bgaltan*b.imwbcnt + Bgaltan*b.Immigration)

#Robust versions
ICDATA <- ICDATA %>% mutate(ROBisal_econ = Blrecon*b.gvslvue + Blrecon*b.sbstrec + Blrecon*b.sbbsntx + Blrecon*b.sblazy +
                              Blrecon*b.uentrjb + Blrecon*b.bennent)
ICDATA <- ICDATA %>% mutate(st.ROBisal_econ = scale(ROBisal_econ))
ICDATA <- ICDATA %>% mutate(ROB1isal_soc = Bgaltan*b.Immigration + Bgaltan*b.imueclt + Bgaltan*b.imwbcnt)
ICDATA <- ICDATA %>% mutate(st.ROB1isal_soc = scale(ROB1isal_soc))
ICDATA <- ICDATA %>% mutate(ROB2isal_soc = Bgaltan*b.mnrgtjb + Bgaltan*b.freehms  + Bgaltan*b.hmsfmlsh +
                              Bgaltan*b.hmsacld)
ICDATA <- ICDATA %>% mutate(st.ROB2isal_soc = scale(ROB2isal_soc))

#####
#INDEPENDENT VARIABLES
#####
#COUNTRY-LEVEL
ICCHES <- read.csv("C:/Users/tadec/OneDrive - MUNI/GitHub/no_conservative_left/data/1999-2019_CHES_dataset_means(v3).csv") %>% subset(country %in% ches_countries) %>% subset(year == 2014) %>% dplyr::select(family, party_id)

ICCHES <- ICCHES %>% mutate(family = case_when(family == 1 ~ "RR",
                                               family == 2 ~ "Cons",
                                               family == 3 ~ "Lib", 
                                               family == 4 ~ "ChristDem",
                                               family == 5 ~ "Soc",
                                               family == 6 ~ "RLeft",
                                               family == 7 ~ "Green",
                                               TRUE ~ NA_character_)) %>% mutate(family = as.factor(family))
#TEST <- read.csv("1999-2019_CHES_dataset_means(v3).csv") %>% subset(year == 2014)
#names(TEST)
#ICCHES <- ICCHES %>% dplyr::select(vote, lrecon, galtan, country) %>% na.omit()

#ELITE POLARIZATION
#ICCHES <- ICCHES %>% group_by(country) %>%  mutate(DaltonPPEcon = sqrt(sum((((lrecon - (sum((lrecon*vote)/sum(vote))))/5)^2)*vote)))
#ICCHES <- ICCHES %>% group_by(country) %>%  mutate(DaltonPPSoc = sqrt(sum((((galtan - (sum((galtan*vote)/sum(vote))))/5)^2)*vote)))

#PARTY SYSTEM INCONSISTENCY (Dassonneville, Fournier, Somer-Topcu)
#ICCHES <- ICCHES %>% group_by(country) %>%  mutate(PSI = sum((vote/100)*(abs(lrecon - galtan))))

#ICCHES <- aggregate(ICCHES, list(ICCHES$country), mean) %>% dplyr::select(country, DaltonPPEcon, DaltonPPSoc, PSI)

#After making necessary changes, merge with ICDATA dataframe
ICDATA <- merge(ICDATA, ICCHES, by = "party_id")

#Interest in Politics (proxy for sophistication)
#define NAs
int <- ICDATA$polintr
int[int > 4] <- NA
ICDATA$polintr <- int

#reverse code (interested higher)
ICDATA %<>% mutate(r.polintr = -1*polintr+5)

#education
ed <- ICDATA$eisced
ed[ed > 54] <- NA
ICDATA$eisced <- ed

#gender
gen <- ICDATA$gndr
gen[gen > 2] <- NA
ICDATA$gndr <- gen

#Party Polarization Centered
#ICDATA <- ICDATA %>% mutate(c.PPEcon = DaltonPPEcon - mean(DaltonPPEcon))
#ICDATA <- ICDATA %>% mutate(c.PPSoc = DaltonPPSoc - mean(DaltonPPSoc))

#Partisanship - reverse coded
part <- ICDATA$prtdgcl
part[part > 4] <- NA
ICDATA$prtdgcl <- part
ICDATA %<>% mutate(partisanship = -1*prtdgcl + 5)

#Standardize Partisan Issue Alignment
ICDATA %<>% mutate(st.isal_econ = scale(isal_econ2))
ICDATA %<>% mutate(st.isal_soc = scale(isal_soc2))

#ideological extremity
ICDATA %<>% mutate(galtan_ext = abs(galtan - 5))
ICDATA %<>% mutate(lrecon_ext = abs(lrecon - 5))

ICDATA %<>% mutate(PI = abs(lrecon-galtan))

#####
#DV: DENSITY
#####
DEN <- ALL %>% dplyr::select(st.isal_econ, st.isal_soc)
DEN2 <- reshape2::melt(HIST)

#Multiple density plots
density <- ggplot(DEN2,aes(x=value, fill=variable)) + geom_density(alpha=0.5, adjust = 1.5) + xlim(-3, 3) + 
  scale_fill_discrete(labels = c("Economic", "Sociocultural")) +  labs(fill = "Ideological Axis") +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 13, face = "bold"), axis.title = element_text(size = 12),
        axis.text = element_text(size = 11))

#plot output
#svg("density.svg", height = 5, width = 7)
density + scale_fill_brewer(palette="Greys", labels = c("Economic", "Sociocultural"))
#dev.off()

PartyLevel <- ICDATA %>% group_by(family, PARTISAN) %>% dplyr::summarize(AverageEconomy = mean(st.isal_econ), AverageCulture = mean(st.isal_soc),
                                                                         lrecon_ext = mean(lrecon_ext), galtan_ext = mean(galtan_ext)) %>% filter(family == "RLeft" | family == "Green")
PartyLevel2 <- ICDATA %>% group_by(family, PARTISAN) %>% dplyr::summarize(AverageEconomy = mean(st.isal_econ), AverageCulture = mean(st.isal_soc),
                                                                          lrecon_sd = mean(lrecon_sd), galtan_sd = mean(galtan_sd)) %>% filter(family == "RLeft" | family == "Green")

