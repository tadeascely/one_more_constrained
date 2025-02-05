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

DATAFIN <- read.csv("data/Data_Transformed.csv")
DATAFIN = DATAFIN |> mutate(wave = as.character(wave))
#####
#DV: DENSITY
#####
DEN <- DATAFIN %>% dplyr::select(st.isal_econ, st.isal_soc, wave)
DEN2 <- reshape2::melt(DEN, id.vars = "wave")

#Multiple density plots
density <- ggplot(DEN2,aes(x=value, fill=variable)) + geom_density(alpha=0.5, adjust = 2) + xlim(-3, 3) + 
  scale_fill_discrete(labels = c("Economic", "Sociocultural")) +  labs(fill = "Ideological Axis") +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 13, face = "bold"), axis.title = element_text(size = 12),
        axis.text = element_text(size = 11)) + facet_wrap(~wave)


#plot output
svg("Figures/density.svg", height = 5, width = 10, family = "serif")
density + scale_fill_brewer(palette="Greys", labels = c("Economic", "Sociocultural")) + theme_classic()
dev.off()

#####
#FAMILIES
#####
DATAFIN$family <- factor(DATAFIN$family, levels=c('RR', 'Cons', 'ChristDem', 'Lib', 'Soc', "Green", "RLeft"))

#Economic
FE <- lmer(st.isal_econ ~ family + wave + family*wave + (1 | cntry:PARTISAN), data=DATAFIN)
#summary(FE)
fam1 <- sjPlot::plot_model(FE, type = "int", terms = c("family", "wave"), colors = c("black","grey50"),
                   axis.title = c("", "Partisan Issue Alignment"),
                   title = "Economic", show.legend = F) + 
  coord_flip() + theme_classic() + ylim(-.7, 1.1)

fam1P <- fam1 +
geom_vline(xintercept =4.5) + scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7), labels = c("Radical Right", "Conservative",
                                                          "Christian Democracy", "Liberal", "Socialist",
                                                          "Green", "Radical Left")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


#Sociocultural
FS <- lmer(st.isal_soc ~ family + wave + family*wave + (1 | cntry:PARTISAN), data=DATAFIN)
#summary(FS)
fam2 <- sjPlot::plot_model(FS, type = "int", terms = c("family", "wave"), colors = c("black", "grey50"),
                           axis.title = c("", "Partisan Issue Alignment"),
                           title = "Sociocultural", show.legend = F) + 
  coord_flip() + theme_classic() + ylim(-.7, 1.1)


fam2P <- fam2 +
  geom_vline(xintercept =4.5) + scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7), labels = c("Radical Right", "Conservative",
                                                                                               "Christian Democracy", "Liberal",
                                                                                               "Socialist", "Green", "Radical Left")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

dodge_width <- 0.5
for (i in seq_along(fam2$layers)) {
  fam2$layers[[i]]$position <- position_dodge(width = dodge_width)
}

for (i in seq_along(fam1$layers)) {
  fam1$layers[[i]]$position <- position_dodge(width = dodge_width)
}

svg("Figures/families.svg", height = 4, width = 8, family = "serif")
fam1P+fam2P
dev.off()

#####
#Position
#####

#Position

#Economic

ME22 <- lmer(st.isal_econ ~ r.polintr + gndr + eisced + lrecon + I(lrecon^2) + wave + lrecon*wave  + I(lrecon^2)*wave + (wave | cntry:PARTISAN), data=DATAFIN)
#arm::display(ME22)
ME22P <- plot_model(ME22, type = "pred", terms=c("lrecon[all]", "wave"), axis.title = c("Left-Right Position", "Ideological Alignment"),
                    title = "Economic", colors = c("darkblue","darkred"),
                    show.legend = FALSE) + theme_classic()

ME22P1 <- ME22P + geom_histogram(data = DATAFIN, inherit.aes = FALSE, aes(x = lrecon, y = ..count../1000),
                       fill = "gray20", alpha = 0.2, position = position_nudge(y = -1.0), binwidth = 0.2)
MEEP <- ME22P1 + scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10))  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#sociocultural

MS22 <- lmer(st.isal_soc ~ r.polintr + gndr + eisced + galtan + I(galtan^2) + wave + galtan*wave + I(galtan^2)*wave + (wave | cntry:PARTISAN), data=DATAFIN)
#arm::display(MS22)
MS22P <- plot_model(MS22, type = "pred", terms=c("galtan[all]", "wave"), axis.title = c("GAL-TAN Position", "Ideological Alignment"),
                    title = "Sociocultural", colors = c("darkblue","darkred"),
                    show.legend = FALSE) + theme_classic()

MS22P1 <- MS22P + geom_histogram(data = DATAFIN, inherit.aes = FALSE, aes(x = galtan, y = ..count../1000),
                                 fill = "gray20", alpha = 0.2, position = position_nudge(y = -1.0), binwidth = 0.2)
MSEP <- MS22P1 + scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10))  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


#annotation
MEEP<- MEEP + 
  ggplot2::annotate("text", x = 6.5, y = .9, label = "2008 data", color = "darkblue", hjust = 0, size = 5) +
  ggplot2::annotate("text", x = 6.5, y = 0.75, label = "2016 data", color = "darkred", hjust = 0, size = 5)

MSEP <- MSEP + 
  ggplot2::annotate("text", x = 6.5, y = .9, label = "2008 data", color = "darkblue", hjust = 0, size = 5) +
  ggplot2::annotate("text", x = 6.5, y = 0.75, label = "2016 data", color = "darkred", hjust = 0, size = 5)

#Plot Output
svg("Figures/positions.svg", width = 10, height = 6, family = "serif")
MEEP + ylim(-1, 1.35) + 
  MSEP + ylim(-1, 1.35)
dev.off()


#####
#Blurring
#####


#Blurring

ME23 <- lmer(st.isal_econ ~ r.polintr + gndr + eisced + lrecon_sd + I(lrecon_sd^2) + wave + lrecon_sd*wave + I(lrecon_sd^2)*wave  + (wave | cntry:PARTISAN), data=DATAFIN)
#arm::display(ME23)

ME23P <- plot_model(ME23, type = "pred", terms=c("lrecon_sd[all]", "wave"), axis.title = c("Left-Right Blurring", "Ideological Alignment"),
                    title = "Economic", colors = c("darkblue","darkred"),
                    show.legend = FALSE) + theme_classic()

ME23P1 <- ME23P + geom_histogram(data = DATAFIN, inherit.aes = FALSE, aes(x = lrecon_sd, y = ..count../1000),
                                 fill = "gray20", alpha = 0.2, position = position_nudge(y = -2.0), binwidth = 0.2)
MEP2 <- ME23P1 + scale_x_continuous(breaks = c(0, 1, 2, 3))  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


MS23 <- lmer(st.isal_soc ~ r.polintr + gndr + eisced + galtan_sd + I(galtan_sd^2) + wave + galtan_sd*wave + I(galtan_sd^2)*wave + (wave | cntry:PARTISAN), data=DATAFIN)
#arm::display(MS23)
MS23P <- plot_model(MS23, type = "pred", terms=c("galtan_sd[all]", "wave"), axis.title = c("GAL-TAN Blurring", "Ideological Alignment"),
                    title = "Sociocultural", colors = c("darkblue","darkred"),
                    show.legend = FALSE) + theme_classic()

MS23P1 <- MS23P + geom_histogram(data = DATAFIN, inherit.aes = FALSE, aes(x = galtan_sd, y = ..count../1000),
                                 fill = "gray20", alpha = 0.2, position = position_nudge(y = -2.0), binwidth = 0.2)
MSP2 <- MS23P1 + scale_x_continuous(breaks = c(0, 1, 2, 3))  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#Plot Output
svg("Figures/blurring.svg", width = 10, height = 6, family = "serif")
MEP2 + ylim(-2, 1.35) + 
  MSP2 + ylim(-2, 1.35)
dev.off()

#####
#Nicheness
#####
ches_countries <- c(1,21, 3, 22, 5, 14, 6, 11, 23, 7, 10, 26, 12, 16, 29)
ICCHES2 <- read.csv("data/1999-2019_CHES_dataset_means(v3).csv") %>% subset(country %in% ches_countries) %>% subset(year == 2014) %>% dplyr::select(cmp_id, party_id)
DATCHES2014_2 <- merge(CHES2014, ICCHES2, by = "party_id")

#Programmatic Nicheness
mp_setapikey("manifesto_apikey.txt")
MPcountry <- c("Belgium", "Czech Republic", "Germany", "Estonia", "Spain", "Finland",
               "France", "United Kingdom", "Hungary", "Ireland", "Netherlands",
               "Poland", "Portugal", "Sweden", "Slovenia")
MP2 <- mp_maindataset()
MP2 %<>% filter(date < 201400 & date >= 201000)
MP2 %<>% subset(countryname %in% MPcountry)
MP2 %<>% filter(edate != "2010-05-29", edate != "2010-06-09")

nicheness <- mp_nicheness(MP2, method = "meyermiller")
MP2 <- merge(MP2, nicheness, by = "party")

#FOLLOWING COMMAND is dependent on using cummulative CHES dataset (CHES2014), which is saved 
#in a different form in this file (I need PS.R... to solve in future)
nichPART2 <- merge(DATCHES2014_2, MP2, by.x = "cmp_id", by.y = "party", all.x = FALSE, all.y = FALSE) |> dplyr::select(!country)
nichDATA2 <- merge(nichPART2, DATA16, by = "PARTISAN")
nichDATA2 <- nichDATA2[!is.na(nichDATA2$nicheness),]
nichDATA2 <- nichDATA2[!duplicated(colnames(nichDATA2))]

PartyLevel3 <- nichDATA2 %>% group_by(family, PARTISAN) %>% dplyr::summarize(AverageEconomy = mean(st.isal_econ), AverageCulture = mean(st.isal_soc),
                                                                             nicheness = mean(nicheness))


#Economic
ME25 <- lmer(st.isal_econ ~ r.polintr + gndr + eisced + nicheness + I(nicheness^2) + (1 | cntry:PARTISAN), data=nichDATA2)

ME25P <- plot_model(ME25, type = "pred", terms=c("nicheness[all]"), axis.title = c("Nicheness", "Ideological Alignment"),
                    title = "Economic", colors = c("darkblue","darkred"),
                    show.legend = FALSE) + theme_classic()

ME25P1 <- ME25P + geom_histogram(data = nichDATA2, inherit.aes = FALSE, aes(x = nicheness, y = ..count../1000),
                                 fill = "gray20", alpha = 0.2, position = position_nudge(y = -1.0), binwidth = 0.5)

MEP3 <- ME25P1 + scale_x_continuous(breaks = c(-4, -2 , 0, 2, 4))  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


#Sociocultural
MS25 <- lmer(st.isal_soc ~ r.polintr + gndr + eisced + nicheness + I(nicheness^2) + (1 | cntry:PARTISAN), data=nichDATA2)
#arm::display(MS25)
MS25P <- plot_model(MS25, type = "pred", terms=c("nicheness[all]"), axis.title = c("Nicheness", "Ideological Alignment"),
                    title = "Sociocultural", colors = c("darkblue","darkred"),
                    show.legend = FALSE) + theme_classic()

MS25P1 <- MS25P + geom_histogram(data = nichDATA2, inherit.aes = FALSE, aes(x = nicheness, y = ..count../1000),
                                 fill = "gray20", alpha = 0.2, position = position_nudge(y = -1.0), binwidth = 0.5)

MSP3 <- MS25P1 + scale_x_continuous(breaks = c(-4, -2 , 0, 2, 4))  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


#Plot (Output)
svg("Figures/nicheness.svg", width = 10, height = 6, family = "serif")
MEP3 + ylim(-1.2, 1.35) + 
  MSP3 + ylim(-1.2, 1.35)
dev.off()






