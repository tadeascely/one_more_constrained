#Install Required Packages


packages <- c("haven", "tidyverse", "Hmisc", "lme4", "svglite", "sjmisc", 
              "sjPlot", "influence.ME", "patchwork", "magrittr", "stringr", "manifestoR",
              "effects", "kableExtra", "NetworkComparisonTest", "qgraph", "bootnet",
              "arm", "reshape2", "DiagrammeR")

# Check if each package is installed, install if missing, and load it
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    require(pkg, character.only = TRUE)
  }
}



#####
#2008
#####

ESS4 <- read.csv("C:/Users/tadec/OneDrive - MUNI/GitHub/no_conservative_left/data/ESS4e04_6-subset.csv")
#defining missing values
ESS4[ESS4 == 66] <- NA
ESS4[ESS4 == 77] <- NA
ESS4[ESS4 == 88] <- NA
ESS4[ESS4 == 99] <- NA

CHES2006 <- read_dta("C:/Users/tadec/OneDrive - MUNI/GitHub/no_conservative_left/data/2006_CHES_dataset_means.dta")
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
#OUTCOME VARIABLE
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

DATA08 %<>% mutate(st.isal_econ = scale(isal_econ2))
DATA08 %<>% mutate(st.isal_soc = scale(isal_soc2))
DATA08 %<>% rename(galtan_sd = std_galtan, lrecon_sd = std_lrecon, partisanship = prtdgcl)

part <- DATA08$partisanship
part[part > 4] <- NA
DATA08$partisanship <- part



#####
#2016
#####

#Uploading CHES2014 data
#This dataset contains only this survey wave, it includes expert standard deviations, but not party families!
CHES2014 <- read.csv("data/2014_CHES_dataset_means.csv")
ches_countries <- c(1,21, 3, 22, 5, 14, 6, 11, 23, 7, 10, 26, 12, 16, 29)
CHES2014 %<>% subset(country %in% ches_countries)

#What about matching the party families from the cummulative data file? then, I would not need two codes and everything

#ESS8 upload
ESS8 <- read.csv("data/ESS-Data-Wizard-subset-2023-01-09.csv")
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
DATA16 <- merge(ESS8, CHES2014, by = "PARTISAN")



#####
#DEFINING MISSING VALUES - ALL ATTITUDES
#####
allfour <- c("imsmetn",	"imdfetn",	"impcntr")
a4 <- DATA16 %>% dplyr::select(all_of(allfour))
a4[a4 > 4] <- NA
DATA16[allfour] <- a4

allfive <- c("gincdif",	"mnrgtjb",	"freehms",	"hmsfmlsh",	"hmsacld",	"dfincac",	"smdfslv",	"sbstrec",	"sbprvpv",	"sbbsntx",	"sblazy",	"imsclbn",	"uentrjb",	"lbenent",	"bennent")
a5 <- DATA16 %>% dplyr::select(all_of(allfive))
a5[a5 > 5] <- NA
DATA16[allfive] <- a5  

allsix <- c("impsafe",	"ipfrule",	"ipudrst",	"ipstrgv",	"ipbhprp",	"imptrad")
a6 <- DATA16 %>% dplyr::select(all_of(allsix))
a6[a6 > 6] <- NA
DATA16[allsix] <- a6

allten <- c("euftf",	"imbgeco",	"imueclt",	"imwbcnt",	"gvslvol",	"gvslvue",	"gvcldcr")
a10 <- DATA16 %>% dplyr::select(all_of(allten))
a10[a10 > 10] <- NA
DATA16[allten] <- a10

rm(a4, a5, a6, a10)

######
#REVERSE CODE SELECTED VARIABLES
######
#LRECON..... right = higher
#GALTAN.....TAN = higher

#5 levels
fivelevels <- c("mnrgtjb", "hmsfmlsh", "dfincac", "sbstrec", "sbbsntx", "sblazy", "uentrjb", "bennent")
d5 <- DATA16 %>% dplyr::select(all_of(fivelevels))
d5 <- d5 %>% mutate(across(fivelevels, function(x) -1*x+6))
DATA16[fivelevels] <- d5

#6 levels
sixlevels <- c("impsafe", "ipfrule", "ipstrgv", "ipbhprp", "imptrad")
d6 <- DATA16 %>% dplyr::select(all_of(sixlevels))
d6 <- d6 %>% mutate(across(sixlevels, function(x) -1*x+7))
DATA16[sixlevels] <- d6

#10 levels
tenlevels <- c("euftf", "imbgeco", "imueclt", "imwbcnt", "gvslvol", "gvslvue", "gvcldcr")
d10 <- DATA16 %>% dplyr::select(all_of(tenlevels))
d10 <- d10 %>% mutate(across(tenlevels, function(x) -1*x+10))
DATA16[tenlevels] <- d10

rm(d5, d6, d10)

#####
#OUTCOME VARIABLE
#####
DATA16 <- DATA16 %>% mutate(Blrecon = case_when(lrecon > 5 ~ 1,
                                                lrecon < 5 ~ -1,
                                                TRUE ~ NA_real_))

DATA16 <- DATA16 %>% mutate(Bgaltan = case_when(galtan > 5 ~ 1,
                                                galtan < 5 ~ -1,
                                                TRUE ~ NA_real_))

#Reliability
#immig <- c("imdfetn", "impcntr", "imsmetn")
#ltm::cronbach.alpha(ALL[immig], na.rm = TRUE)
#cronbach.alpha(ALL[immig], na.rm = TRUE) <- reliability checked

DATA16 %<>% mutate(Immigration = (imdfetn + impcntr + imsmetn)/3)

DATA16 %<>% mutate(b.gincdif = case_when(gincdif > mean(gincdif, na.rm = TRUE) ~ 1,
                                         gincdif < mean(gincdif, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA16 %<>% mutate(b.dfincac = case_when(dfincac > mean(dfincac, na.rm = TRUE) ~ 1,
                                         dfincac < mean(dfincac, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA16 %<>% mutate(b.smdfslv = case_when(smdfslv > mean(smdfslv, na.rm = TRUE) ~ 1,
                                         smdfslv < mean(smdfslv, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA16 %<>% mutate(b.gvslvue = case_when(gvslvue > mean(gvslvue, na.rm = TRUE) ~ 1,
                                         gvslvue < mean(gvslvue, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA16 %<>% mutate(b.sbstrec = case_when(sbstrec > mean(sbstrec, na.rm = TRUE) ~ 1,
                                         sbstrec < mean(sbstrec, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA16 %<>% mutate(b.sbbsntx = case_when(sbbsntx > mean(sbbsntx, na.rm = TRUE) ~ 1,
                                         sbbsntx < mean(sbbsntx, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA16 %<>% mutate(b.sblazy = case_when(sblazy > mean(sblazy, na.rm = TRUE) ~ 1,
                                        sblazy < mean(sblazy, na.rm = TRUE) ~ -1,
                                        TRUE ~ 0))

DATA16 %<>% mutate(b.uentrjb = case_when(uentrjb > mean(uentrjb, na.rm = TRUE) ~ 1,
                                         uentrjb < mean(uentrjb, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA16 %<>% mutate(b.bennent = case_when(bennent > mean(bennent, na.rm = TRUE) ~ 1,
                                         bennent < mean(bennent, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA16 %<>% mutate(b.lbenent = case_when(lbenent > mean(lbenent, na.rm = TRUE) ~ 1,
                                         lbenent < mean(lbenent, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA16 %<>% mutate(b.sbprvpv = case_when(sbprvpv > mean(sbprvpv, na.rm = TRUE) ~ 1,
                                         sbprvpv < mean(sbprvpv, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA16 %<>% mutate(b.gvslvol = case_when(gvslvol > mean(gvslvol, na.rm = TRUE) ~ 1,
                                         gvslvol < mean(gvslvol, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA16 %<>% mutate(b.gvcldcr = case_when(gvcldcr > mean(gvcldcr, na.rm = TRUE) ~ 1,
                                         gvcldcr < mean(gvcldcr, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA16 %<>% mutate(b.mnrgtjb = case_when(mnrgtjb > mean(mnrgtjb, na.rm = TRUE) ~ 1,
                                         mnrgtjb < mean(mnrgtjb, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA16 %<>% mutate(b.freehms = case_when(freehms > mean(freehms, na.rm = TRUE) ~ 1,
                                         freehms < mean(freehms, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA16 %<>% mutate(b.hmsfmlsh = case_when(hmsfmlsh > mean(hmsfmlsh, na.rm = TRUE) ~ 1,
                                          hmsfmlsh < mean(hmsfmlsh, na.rm = TRUE) ~ -1,
                                          TRUE ~ 0))

DATA16 %<>% mutate(b.hmsacld = case_when(hmsacld > mean(hmsacld, na.rm = TRUE) ~ 1,
                                         hmsacld < mean(hmsacld, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA16 %<>% mutate(b.imueclt = case_when(imueclt > mean(imueclt, na.rm = TRUE) ~ 1,
                                         imueclt < mean(imueclt, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA16 %<>% mutate(b.imwbcnt = case_when(imwbcnt > mean(imwbcnt, na.rm = TRUE) ~ 1,
                                         imwbcnt < mean(imwbcnt, na.rm = TRUE) ~ -1,
                                         TRUE ~ 0))

DATA16 %<>% mutate(b.Immigration = case_when(Immigration > mean(Immigration, na.rm = TRUE) ~ 1,
                                             Immigration < mean(Immigration, na.rm = TRUE) ~ -1,
                                             TRUE ~ 0))

#Economic issue alignment
DATA16 <- DATA16 %>% mutate(isal_econ2 = Blrecon*b.gincdif + Blrecon*b.dfincac +
                              Blrecon*b.smdfslv + Blrecon*b.gvslvol +Blrecon*b.gvslvue +Blrecon*b.gvcldcr +
                              Blrecon*b.sbstrec + Blrecon*b.sbprvpv + Blrecon*b.sbbsntx + Blrecon*b.sblazy +
                              Blrecon*b.uentrjb + Blrecon*b.lbenent + Blrecon*b.bennent)

#Sociocultural issue alignment
DATA16 <- DATA16 %>% mutate(isal_soc2 = Bgaltan*b.mnrgtjb + Bgaltan*b.freehms + Bgaltan*b.hmsfmlsh +
                              Bgaltan*b.hmsacld + Bgaltan*b.imueclt + Bgaltan*b.imwbcnt + Bgaltan*b.Immigration)

#Robust versions
DATA16 <- DATA16 %>% mutate(ROBisal_econ = Blrecon*b.gvslvue + Blrecon*b.sbstrec + Blrecon*b.sbbsntx + Blrecon*b.sblazy +
                              Blrecon*b.uentrjb + Blrecon*b.bennent)
DATA16 <- DATA16 %>% mutate(st.ROBisal_econ = scale(ROBisal_econ))
DATA16 <- DATA16 %>% mutate(ROB1isal_soc = Bgaltan*b.Immigration + Bgaltan*b.imueclt + Bgaltan*b.imwbcnt)
DATA16 <- DATA16 %>% mutate(st.ROB1isal_soc = scale(ROB1isal_soc))
DATA16 <- DATA16 %>% mutate(ROB2isal_soc = Bgaltan*b.mnrgtjb + Bgaltan*b.freehms  + Bgaltan*b.hmsfmlsh +
                              Bgaltan*b.hmsacld)
DATA16 <- DATA16 %>% mutate(st.ROB2isal_soc = scale(ROB2isal_soc))

#####
#INDEPENDENT VARIABLES
#####
#COUNTRY-LEVEL
#families are NOT identified in CHES2014, so I match them from the cummulative dataset, 
ICCHES <- read.csv("data/1999-2019_CHES_dataset_means(v3).csv") %>% subset(country %in% ches_countries) %>% subset(year == 2014) %>% dplyr::select(family, party_id)

ICCHES <- ICCHES %>% mutate(family = case_when(family == 1 ~ "RR",
                                               family == 2 ~ "Cons",
                                               family == 3 ~ "Lib", 
                                               family == 4 ~ "ChristDem",
                                               family == 5 ~ "Soc",
                                               family == 6 ~ "RLeft",
                                               family == 7 ~ "Green",
                                               TRUE ~ NA_character_)) %>% mutate(family = as.factor(family))

#After making necessary changes, merge with DATA16 dataframe
DATA16 <- merge(DATA16, ICCHES, by = "party_id")

#Interest in Politics (proxy for sophistication)
#define NAs
int <- DATA16$polintr
int[int > 4] <- NA
DATA16$polintr <- int

#reverse code (interested higher)
DATA16 %<>% mutate(r.polintr = -1*polintr+5)

#education
ed <- DATA16$eisced
ed[ed > 54] <- NA
DATA16$eisced <- ed

#gender
gen <- DATA16$gndr
gen[gen > 2] <- NA
DATA16$gndr <- gen

#Partisanship - reverse coded
part <- DATA16$prtdgcl
part[part > 4] <- NA
DATA16$prtdgcl <- part
DATA16 %<>% mutate(partisanship = -1*prtdgcl + 5)

#Standardize Partisan Issue Alignment
DATA16 %<>% mutate(st.isal_econ = scale(isal_econ2))
DATA16 %<>% mutate(st.isal_soc = scale(isal_soc2))


#####
#Merging Datasets
#####

#RENAME DATA16 AND ALIKE TO DATA16

DATA08FIN <- DATA08 %>% dplyr::select(family, st.isal_econ, isal_econ2, st.isal_soc, isal_soc2, r.polintr, gndr, eisced,lrecon, galtan, galtan_sd, lrecon_sd, cntry, PARTISAN, partisanship)
DATA08FIN %<>% mutate(wave = "2008")
DATA16FIN <- DATA16 %>% dplyr::select(family, st.isal_econ, isal_econ2, st.isal_soc, isal_soc2, r.polintr, gndr, eisced, lrecon, galtan, galtan_sd, lrecon_sd, cntry, PARTISAN, partisanship)
DATA16FIN %<>% mutate(wave = "2016")
DATAFIN <- rbind(DATA08FIN, DATA16FIN)

write.csv(DATAFIN, "data/Data_Transformed.csv")
