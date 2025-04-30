#------------------------------------------------------------------------------
############################# Case-contol study ###############################
#------------------------------------------------------------------------------

## Artikel 2 - selectie sociale factoren 
## Gemaakt door Erik Giltay & Maxine Storm
## Augustus 2023
##

## clear global environment
rm(list = ls(all = TRUE))

# # detach all packages 
# lapply(names(sessionInfo()$otherPkgs), function(pkgs)
#   detach(paste0("package:", pkgs), 
#          character.only = T, unload = T, force = T))

## load packages
library(haven)
library(tidyverse)
library(data.table)
library(dtplyr)
library(tictoc)
library(forestplot)
library(nnet)
library(mice)
library(labelled)



decimaal <- function(x,k) trimws(format(round(x,k), nsmall=k))
maak_p <- function(p) {
  uitvoer_p <- ifelse(p<0.01, decimaal(p,3), paste0(decimaal(p,2), " "))
  uitvoer_p <- ifelse(decimaal(p,2)==0.05, decimaal (p,3), uitvoer_p)
  uitvoer_p <- ifelse(uitvoer_p=="0.000", "p<0.001", paste0("p=", uitvoer_p))
  return(uitvoer_p)
}
scale2 <- function(x, na.rm = TRUE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)


----------------------------------------------------------------------
############## Sociale factoren op RINpersoonsniveau ##################
----------------------------------------------------------------------

#### INLADEN GROTE DATAFILES ####

####### Kinderen koppelen aan ouders #######
#* verander working directory om CBS data te selecteren 
# setwd("G:/Bevolking/KINDOUDERTAB")
# 
# tic()
# KINDOUDER <- read_sav("KINDOUDER2022TABV1.sav") %>%
# lazy_dt() %>%
# glimpse()
# toc()

# opslaan als rds bestand (sneller inladen)
# saveRDS(KINDOUDER, "H:/Maxine/RDS files/KINDOUDER.rds")
KINDOUDER <- readRDS("H:/Maxine/RDS files/KINDOUDER.rds")

variable.names(KINDOUDER)
# relevante vars: KINDOUDER$RINPERSOON en KINDOUDER$RINPERSOONpa en KINDOUDER$RINPERSOONMa en KINDOUDER$XKOPPELNUMMER


####### Kinderen koppelen aan huishoudens #######

#* verander working directory om CBS data te selecteren 
# setwd("G:/InkomenBestedingen/INHATAB")


# tic()
# KOPPELPERSOONHUISHOUDEN <- read_sav("KOPPELPERSOONHUISHOUDEN2017.sav") %>% 
# lazy_dt() %>% 
# glimpse()
# toc()
 
# opslaan als rds bestand (sneller inladen)
# saveRDS(KOPPELPERSOONHUISHOUDEN, "H:/Maxine/RDS files/KOPPELPERSOONHUISHOUDEN.rds")
KOPPELPERSOONHUISHOUDEN <- readRDS("H:/Maxine/RDS files/KOPPELPERSOONHUISHOUDEN.rds")
variable.names(KOPPELPERSOONHUISHOUDEN)
# relevante vars: KOPPELPERSOONHUISHOUDEN$RINPERSOON en KOPPELPERSOONHUISHOUDEN$RINPERSOONHKW

### Inladen bronbestand (drie groepen, combinatie)
combi_driegroepen <- readRDS("H:/Maxine/RDS files/combi_driegroepen.rds")

# Maak extra kolommen met RINPERSOONMa, RINPERSOONpa, XKOPPELNUMMER, RINPERSOONHKW (en bepaal of dit nr. overeenkomt met RINPERSOONpa)
combi_covariaten <- combi_driegroepen %>% 
  left_join(KOPPELPERSOONHUISHOUDEN %>% select(Rinpersoon = RINPERSOON, RINPERSOONHKW), by = "Rinpersoon") %>%
  left_join(KINDOUDER %>% select(Rinpersoon = RINPERSOON, RINPERSOONpa, RINPERSOONMa, XKOPPELNUMMER), by = "Rinpersoon") %>% 
  glimpse()

# 6 dubbele personen, kies de personen die 
combi_covariaten %>% group_by(Rinpersoon) %>% filter(n() > 1) %>% view()

######
# voor iedereen die twee keer voorkomt in het bestand, pakken we de gegevens waarbij kostwinnaar een van de ouders is (maximale informatie)
# sla Pietje over (case_control = 2518, 2519), Pietje is 2 x gekozen als controlepersoon, om 5 controle personen te houden per case blijft Pietje erin
combi_covariaten2 <- combi_covariaten %>%
  filter(!(Rinpersoon == "000310790" &  RINPERSOONpa == "146168792")) %>% # selecteer rij die je eruit wilt halen
  filter(!(Rinpersoon == "000066899" &  RINPERSOONpa == "185047728")) %>% 
  glimpse()

# check: alleen Pietje als duplicaat (Rinpersoonnr: 850437899)
combi_covariaten2 %>% group_by(Rinpersoon) %>% filter(n() > 1) %>% view()

#################### Covariaten toevoegen #################### 
#* verander working directory om CBS data te selecteren 
# setwd("G:/InkomenBestedingen/INHATAB")

# tic()
# INKOMEN <- read_sav("INHA2017TABV2.sav") %>%
# lazy_dt() %>%
# glimpse()
# toc()

# opslaan als rds bestand (sneller inladen)
# saveRDS(INKOMEN, "H:/Maxine/RDS files/INKOMEN.rds")
INKOMEN <- readRDS("H:/Maxine/RDS files/INKOMEN.rds")

variable.names(INKOMEN)

# relevante vars:
# INKOMEN$INHAHL = aantal personen in huishouden
# INKOMEN$INHAHLMI = aantal huishoudensleden met persoonlijk inkomen
# INKOMEN$INHP100HGEST = percentielgroepen

####### Zorgkosten #######
#* verander working directory om CBS data te selecteren 
# setwd("G:/GezondheidWelzijn/ZVWZORGKOSTENTAB/2017")

# tic()
# Zorgkosten <- read_sav("ZVWZORGKOSTEN2017TABV2.sav") %>%
# lazy_dt() %>%
# glimpse()
# toc()

# opslaan als rds bestand (sneller inladen)
saveRDS(Zorgkosten, "H:/Maxine/RDS files/Zorgkosten.rds")
Zorgkosten <- readRDS("H:/Maxine/RDS files/Zorgkosten.rds")
variable.names(Zorgkosten)

saveRDS(combi_covariaten3, "H:/Maxine/RDS files/combi_covariaten3.rds")
combi_covariaten3 <- readRDS("H:/Maxine/RDS files/combi_covariaten3.rds")

####### CITO scores #######
#* verander working directory om CBS data te selecteren 
setwd("G:/Onderwijs/CITOTAB")

#laad CITO scores in per jaar (in 2012 zou kind 17 jaar oud zijn in 2017)
# CITO2012 <- read_sav("CITOTAB2012V2.SAV")
# CITO2013 <- read_sav("CITOTAB2013V2.SAV")
# CITO2014 <- read_sav("CITOTAB2014V2.SAV")
# CITO2015 <- read_sav("CITOTAB2015V3.SAV")
# CITO2016 <- read_sav("CITOTAB2016V3.SAV")
# CITO2017 <- read_sav("CITOTAB2017V4.SAV")

# CITO_combi <- bind_rows(CITO2012, CITO2013, CITO2014, CITO2015, CITO2016, CITO2017)

# opslaan als rds bestand (sneller inladen)
# saveRDS(CITO, "H:/Maxine/RDS files/CITO_combi.rds")
CITO <- readRDS("H:/Maxine/RDS files/CITO_combi.rds")

CITO_combi <- CITO %>% select(Rinpersoon = RINPersoon, CitoStandaardScore) %>% 
 left_join(combi_covariaten3, by = "Rinpersoon")
  
# check, persoon met Rinnr. 523670292 heeft citoscore

# opslaan als rds bestand (sneller inladen)
# saveRDS(CITO, "H:/Maxine/RDS files/CITO.rds")
CITO <- readRDS("H:/Maxine/RDS files/CITO.rds")
variable.names(CITO)

# Maak extra kolommen met Citoscore en Total_zorgkosten
combi_covariaten4 <- combi_covariaten3 %>% 
 left_join(CITO_combi %>% select(Rinpersoon, Citoscore = CitoStandaardScore), by = "Rinpersoon") %>%
 left_join(Zorgkosten %>% select(Rinpersoon = RINPERSOON, Totale_zorgkosten = ZVWKTOTAAL), by = "Rinpersoon") %>% 
  glimpse()

# saveRDS(combi_covariaten4, "H:/Maxine/RDS files/combi_covariaten4.rds")
combi_covariaten4 <- readRDS("H:/Maxine/RDS files/combi_covariaten4.rds")

# check hoeveel Banjaard en Youz kinderen daarbij zitten
combi_covariaten4 %>% mutate(cito = !is.na(Citoscore)) %>% 
  select(group, cito) %>% table()
                                            
combi_covariaten4 %>% select(group,leeftijd) %>% hist()
hist(combi_covariaten4$leeftijd)

# Descriptive statistics
ggplot(combi_covariaten4) + geom_bar(aes(x = leeftijd))

ggplot(combi_covariaten4) + geom_histogram(aes(x = leeftijd), binwidth = 1)
table(combi_covariaten4$Citoscore)
nrow(combi_covariaten4) - sum(is.na(combi_covariaten4$Citoscore)) # 2613 CITO score aanwezig


sum(is.na(combi_covariaten4$Totale_zorgkosten)) # 188 kinderen waarvoor GEEN zorgkosten beschikbaar is


# combi_covariaten3 <- combi_covariaten2 %>%
#   left_join(INKOMEN %>% select(RINPERSOONHKW, INHAHL, INHAHLMI, INHP100HGEST), by = "RINPERSOONHKW") %>%
#   mutate(Institutioneel_huishouden = INHP100HGEST == -2,# institutionele huishoudens opslaan
#          Institutioneel_huishouden = Institutioneel_huishouden %>% as.numeric(), # opslaan als dummy
#          INHP100HGEST = ifelse(INHP100HGEST < 0, NA, INHP100HGEST), # missings naar NA transformeren
#          INHAHL = ifelse(INHAHL == 99, NA, INHAHL),  # missings naar NA transformeren
#          INHAHLMI  = ifelse(INHAHLMI  == 99, NA, INHAHLMI )) %>%  # missings naar NA transformeren
#   glimpse()
# 
# combi_covariaten3$INHAHLMI %>% table()
# opslaan als rds bestand (sneller inladen)
saveRDS(combi_covariaten3, "H:/Maxine/RDS files/combi_covariaten3.rds")
combi_covariaten3 <- readRDS("H:/Maxine/RDS files/combi_covariaten3.rds")
table(combi_covariaten3$group)

# Leeftijd ouders + geboorteland kind en ouders, 2017 variant!

# RDS bestand inladen
gba_persoon_2017 <- readRDS("H:/Maxine/RDS files/gba_persoon_2017.rds")

# geboortejaar ouders + geboorteland kind en ouders extraheren
combi_covariaten4 <- combi_covariaten3 %>% 
  left_join(gba_persoon_2017 %>% select(Rinpersoon = RINPERSOON, geboorteland = GBAGEBOORTELAND), by = "Rinpersoon")  %>% # geboorteland kind
  left_join(gba_persoon_2017 %>% select(Rinpersoon = RINPERSOON, leeftijdma = GBAGEBOORTEJAARMOEDER, geboortelandma = GBAGEBOORTELANDMOEDER), by = "Rinpersoon") %>% # leeftijd, geboorteland moeder
  left_join(gba_persoon_2017 %>% select(Rinpersoon = RINPERSOON, leeftijdpa = GBAGEBOORTEJAARVADER, geboortelandpa = GBAGEBOORTELANDVADER), by = "Rinpersoon") %>% # leeftijd, geboorteland vader
  glimpse()

# Leeftijd ouders omrekenen 
combi_covariaten4 <- combi_covariaten4 %>% 
  mutate(leeftijdma = str_sub(leeftijdma, 1, 4) %>% as.numeric(),
         leeftijdma = 2017 - leeftijdma) %>% 
  mutate(leeftijdpa = str_sub(leeftijdpa, 1, 4) %>% as.numeric(),
         leeftijdpa = 2017 - leeftijdpa) %>% 
  glimpse()


# labels extraheren, zodat geboorteland een character variable wordt
combi_covariaten4$geboorteland <- to_factor(combi_covariaten4$geboorteland)
combi_covariaten4$geboortelandpa <- to_factor(combi_covariaten4$geboortelandpa)
combi_covariaten4$geboortelandma <- to_factor(combi_covariaten4$geboortelandma)
head(combi_covariaten4$geboorteland)

View(combi_covariaten4)

# geboortelanden KIND op volgorde van meest voorkomend naar minst voorkomend
table(combi_covariaten4$geboorteland)
sort(table(combi_covariaten4$geboorteland), decreasing = TRUE)[1:15]
sort(table(combi_covariaten4$geboorteland)/19632 * 100, decreasing = TRUE)[1:15] # percentages

# geboortelanden KIND op volgorde (enkel Banjaard groep)
Banjaard_sub <- subset(combi_covariaten4, group == "A. Banjaard") 
sort(table(Banjaard_sub$geboorteland), decreasing = TRUE)[1:15]
sort(table(Banjaard_sub$geboorteland)/505 * 100, decreasing = TRUE)[1:15] # percentages

OVer # geboorteland MOEDER op volgorde van meest voorkomend naar minst voorkomend
table(combi_covariaten4$geboortelandma)
sort(table(combi_covariaten4$geboortelandma), decreasing = TRUE)[1:15]
sort(table(combi_covariaten4$geboortelandma)/19632 * 100, decreasing = TRUE)[1:15] # percentages

# geboorteland MOEDER op volgorde (enkel Banjaard groep)
Banjaard_subma <- subset(combi_covariaten4, group == "A. Banjaard") 
sort(table(Banjaard_subma$geboortelandma), decreasing = TRUE)[1:15]
sort(table(Banjaard_sub$geboortelandma)/505 * 100, decreasing = TRUE)[1:15] # percentages

# geboorteland VADER op volgorde van meest voorkomend naar minst voorkomend
table(combi_covariaten4$geboortelandpa)
sort(table(combi_covariaten4$geboortelandpa), decreasing = TRUE)[1:15]
sort(table(combi_covariaten4$geboortelandpa)/19632 * 100, decreasing = TRUE)[1:15] # percentages

# geboorteland VADER op volgorde (enkel Banjaard groep)
Banjaard_subpa <- subset(combi_covariaten4, group == "A. Banjaard") 
sort(table(Banjaard_subpa$geboortelandpa), decreasing = TRUE)[1:15]
sort(table(Banjaard_sub$geboortelandpa)/505 * 100, decreasing = TRUE)[1:15] # percentages


# opslaan als rds bestand (sneller inladen)
# saveRDS(combi_covariaten4, "H:/Maxine/RDS files/combi_covariaten4.rds")
combi_covariaten4 <- readRDS("H:/Maxine/RDS files/combi_covariaten4.rds")


# rename variabelen
variable.names(combi_covariaten4)
combi_covariaten4 <- rename(combi_covariaten4, Family_size = INHAHL)
combi_covariaten4 <- rename(combi_covariaten4, Household_members_with_income = INHAHLMI)
combi_covariaten4 <- rename(combi_covariaten4, Percentiles_household_income = INHP100HGEST )
combi_covariaten4 <- rename(combi_covariaten4, Mother_age = leeftijdma)
combi_covariaten4 <- rename(combi_covariaten4, Father_age = leeftijdpa)
variable.names(combi_covariaten4)

## geboorteland kind en ouders, 2022 variant!
# * verander working directory om CBS data te selecteren 
setwd("G:/Bevolking/GBAPERSOONTAB/2022")

 tic()
  gba_persoon2022 <- read_sav("GBAPERSOON2022TABV1.sav") %>% 
   lazy_dt() %>% 
 glimpse()
 toc()

# opslaan als rds bestand (sneller inladen)
# saveRDS(gba_persoon2022, "H:/Maxine/RDS files/gba_persoon_2022.rds")
 gba_persoon2022 <- readRDS("H:/Maxine/RDS files/gba_persoon_2022.rds")
 
# geboortejaar ouders + geboorteland kind en ouders extraheren
 combi_covariaten5 <- combi_covariaten4 %>% 
   left_join(gba_persoon2022 %>% select(Rinpersoon = RINPERSOON, Birth_country = GBAGEBOORTELAND, Birth_country_NL = GBAGEBOORTELANDNL, herkomstland = GBAHERKOMSTLAND), by = "Rinpersoon")  %>% # geboorteland kind
   left_join(gba_persoon2022 %>% select(Rinpersoon = RINPERSOON, Birth_country_mother = GBAGEBOORTELANDMOEDER), by = "Rinpersoon") %>% # leeftijd, geboorteland moeder
   left_join(gba_persoon2022 %>% select(Rinpersoon = RINPERSOON, Birth_country_father = GBAGEBOORTELANDVADER), by = "Rinpersoon") %>% # leeftijd, geboorteland vader
   glimpse()
 
# labels extraheren, zodat geboorteland een character variable wordt
 combi_covariaten5$Birth_country <- to_factor(combi_covariaten5$Birth_country)
 combi_covariaten5$Birth_country_mother <- to_factor(combi_covariaten5$Birth_country_mother)
 combi_covariaten5$Birth_country_father <- to_factor(combi_covariaten5$Birth_country_father)
 combi_covariaten5$herkomstland <- to_factor(combi_covariaten5$herkomstland)
 head(combi_covariaten5$Birth_country)
 View(combi_covariaten5)
 
# verwijder geboortegegevens uit 2017
 combi_covariaten5 <- select(combi_covariaten5, -c(geboorteland, geboortelandpa, geboortelandma))
 variable.names(combi_covariaten5)

# opslaan als rds bestand (sneller inladen)
# saveRDS(combi_covariaten5, "H:/Maxine/RDS files/combi_covariaten5.rds")
combi_covariaten5 <- readRDS("H:/Maxine/RDS files/combi_covariaten5.rds")
View(combi_covariaten5$Birth_country)
combi_covariaten5$Birth_country

----------------------------------------------------------------------
########## Omgevingsfactoren op buurt/wijk/stadniveau ################
----------------------------------------------------------------------

## Bevolkingsdichtheid
# * verander working directory om CBS data te selecteren 
setwd("G:/BouwenWonen/PC4OADSTED/2017")

tic()
stedelijkheid_2017 <- read_sav("pc4_sted_oad_2017V1.sav") %>% 
  lazy_dt() %>% 
  glimpse()
toc()

# opslaan als rds bestand (sneller inladen)
# saveRDS(stedelijkheid_2017, "H:/Maxine/RDS files/stedelijkheid_2017.rds")
stedelijkheid_2017 <- readRDS("H:/Maxine/RDS files/stedelijkheid_2017.rds")
variable.names(stedelijkheid_2017)
View(stedelijkheid_2017)
View(combi_covariaten5)

# hier verder #

#3) gebruik functie left_join om te mergen, voeg sted toe
left_join()

combi_covariaten6 <- combi_covariaten5 %>% 
  left_join(stedelijkheid_2017 %>% select(postc = pc4, urbanization_class = sted), by = "postc")  %>%
  glimpse()


## inkomens per postcodegebied

library(readxl)
Inkomen_per_postcode_2017 <- read_excel("H:/Maxine/Gemeente_wijk data/Inkomen_per_postcode_2017.xlsx")

combi_covariaten7 <- combi_covariaten6 %>% 
  left_join(Inkomen_per_postcode_2017 %>% select(postc = Postcodegebied, 
                                                 median_income_postcode = mediaan_inkomen, 
                                                 income_Q1_postcode = kwantiel_1,
                                                 income_Q2_postcode = kwantiel_2, 
                                                 income_Q3_postcode = kwantiel_3, 
                                                 income_Q4_postcode = kwantiel_4, 
                                                 income_Q5_postcode = kwantiel_5), by = "postc")  %>%
  glimpse()
View(combi_covariaten7)

# RINOBJECT inladen
#- inladen duurt lang
PERSOON_OBJECT_POSTCODE_2017 <- readRDS("H:/Maxine/RDS files/koppelbestand_persoon_object_2017.rds")

#koppel combi_covariaten8 aan RINobject
combi_covariaten8 <- combi_covariaten7 %>% 
  left_join(PERSOON_OBJECT_POSTCODE_2017 %>% select(Rinpersoon = RINPERSOON, RINOBJECTNUMMER = RINOBJECTNUMMER), by = "Rinpersoon")  %>%
  glimpse()


# laad wijk/buurtcodes in 


setwd("G:/BouwenWonen/VSLGWBTAB")
tic()
gemeente_wijk_buurtcodes <- read_sav("VSLGWB2023TAB03V1.sav") %>%
lazy_dt() %>%
glimpse()
toc()

# leftjoin variabele: bc2017 (hierin zit ook de wijk- en gemeentecode)

combi_covariaten9 <- combi_covariaten8 %>% 
  left_join(gemeente_wijk_buurtcodes %>% select(RINOBJECTNUMMER = RINOBJECTNUMMER, bc2017 = bc2017), by = "RINOBJECTNUMMER")  %>%
  glimpse()

# opleidingsniveau per buurt
setwd("H:/Maxine/Gemeente_wijk data")

Hoogst_behaald_opleidingsniveau_gwb <- read_excel("Hoogst_behaald_opleidingsniveau_gwb.xlsx")

# merge gemeente-, buurt- en wijkcode (nieuwe var: gwb_code)
Hoogst_behaald_opleidingsniveau_gwb$gwb_code <- paste(Hoogst_behaald_opleidingsniveau_gwb$Gemeentecode, 
                                                      Hoogst_behaald_opleidingsniveau_gwb$Wijkcode, 
                                                      Hoogst_behaald_opleidingsniveau_gwb$Buurtcode, sep = "")

# voeg laag/midden/hoog opleidingsniveau toe aan combi_covariaten bestand (by bc2017)
combi_covariaten10 <- combi_covariaten9 %>% 
  left_join(Hoogst_behaald_opleidingsniveau_gwb %>% select(bc2017 = gwb_code, 
                                                           nh_edulevel_low = Laag, 
                                                           nh_edulevel_middle = Middelbaar, 
                                                           nh_edulevel_high = Hoog), by = "bc2017")  %>%
  glimpse()

# as.numeric maken
combi_covariaten10$nh_edulevel_low <- as.numeric(combi_covariaten10$nh_edulevel_low)
combi_covariaten10$nh_edulevel_middle <- as.numeric(combi_covariaten10$nh_edulevel_middle)
combi_covariaten10$nh_edulevel_high <- as.numeric(combi_covariaten10$nh_edulevel_high)

View(combi_covariaten10)

# armoede indicatoren

setwd("H:/Maxine/Gemeente_wijk data")
indicatoren_armoede_DH_postcodes <- read_excel("indicatoren_armoede_DH_postcodes.xlsx")
variable.names(indicatoren_armoede_DH_postcodes)

combi_covariaten11 <- combi_covariaten10 %>% 
  left_join(indicatoren_armoede_DH_postcodes %>% select(postc = Postcode, 
                                                        low_income_perc_postcode = Huishoudens_laag_inkomen, 
                                                        housing_allowance_perc_postcode = Huishoudens_huurtoeslag,
                                                        healthcare_allowance_perc_postcode = Huishoudens_zorgtoeslag,
                                                        child_allowance_perc_postcode = Huishoudens_kindgebonden_budget), by = "postc")  %>%
  glimpse()


# opslaan als rds bestand (sneller inladen)
# saveRDS(combi_covariaten11, "H:/Maxine/RDS files/combi_covariaten11.rds")
combi_covariaten11 <- readRDS("H:/Maxine/RDS files/combi_covariaten11.rds")


----------------------------------------------------------------------
  ########## gezinsfactoren ################
----------------------------------------------------------------------
## type huishouden
setwd("G:/Bevolking/GBAHUISHOUDENSBUS")

tic()
GBAHUISHOUDEN_2017 <- read_sav("GBAHUISHOUDENS2017BUSV1.sav") %>% 
  lazy_dt() %>% 
  glimpse()
toc()

# opslaan als rds bestand (sneller inladen)
# saveRDS(GBAHUISHOUDEN_2017, "H:/Maxine/RDS files/GBAHUISHOUDEN_2017.rds")
GBAHUISHOUDEN_2017 <- readRDS("H:/Maxine/RDS files/GBAHUISHOUDEN_2017.rds")

# koppel type huishouden aan kinderen

# 8 categorieen, misschien zelf samenvoegen: niet geinteresseerd in zonder kinderen (type 2,3)

variable.names(GBAHUISHOUDEN_2017)
head(GBAHUISHOUDEN_2017$TYPHH)

# huishoudtype 
# categorieen samenvoegen

# nieuwe dataframe
gezin_sam <- GBAHUISHOUDEN_2017 %>% select(RINPERSOON, DATUMAANVANGHH, TYPHH)

gezin_sam2 <- gezin_sam %>% 
  group_by(RINPERSOON)  %>% 
  arrange(desc(DATUMAANVANGHH)) %>% 
  filter(row_number()==1) %>% 
  glimpse()

# selecteer eerst Rinpersonen:  
gezin_sam4 <- gezin_sam2 %>% 
  left_join(gezin_sam3 %>% select(Rinpersoon = RINPERSOON, family_type), by = "Rinpersoon")  %>%
  glimpse() 

combi_covariaten12 <- combi_covariaten11 %>% 
  left_join(gezin_sam2 %>% select(Rinpersoon = RINPERSOON, family_type = TYPHH), by = "Rinpersoon")  %>%
  glimpse()


View(combi_covariaten12)

# check verdeling: stap 2
table(gezin_sam3)

#### Toevoegen aan dataset: stap 3
# koppeling aan RIN kind 
combi_covariaten12 <- combi_covariaten11 %>% 
  left_join(gezin_sam3 %>% select(Rinpersoon = RINPERSOON, family_type), by = "Rinpersoon")  %>%
  glimpse()

#uitkomst: meer rijen/personen?
View(GBAHUISHOUDEN_2017)
glimpse(GBAHUISHOUDEN_2017)

variable.names(combi_covariaten12)
summary(combi_covariaten12$family_type)

# naar as.numeric
combi_covariaten12$family_type <- remove_labels(combi_covariaten13$family_type)
combi_covariaten12$family_type <- as.numeric(combi_covariaten13$family_type)

#check data + missings family_type (0.07 %): 
table(combi_covariaten13$family_type)
sum(is.na(combi_covariaten12$family_type)) # 14 missings
sum(is.na(combi_covariaten12$family_type)) / sum(complete.cases(combi_covariaten12$family_type)) * 100

# maak categorieen

combi_covariaten12 <- combi_covariaten13 %>% 
  mutate(
    family_type2 = case_when(
      family_type %in% c(4,5) ~ '  - Dual parent household', 
      family_type == 6 ~ '  - Single parent household', 
      family_type == 8 ~ '  - institutional household', 
      family_type %in% c(1:3) ~ '  impossible',
      TRUE ~ 'other'
    )
  )
table(combi_covariaten12$family_type2) / sum(complete.cases(combi_covariaten12$family_type)) * 100


# zie code hierboven

## Opleidingsniveau ouders
setwd("G:/Onderwijs/HOOGSTEOPLTAB/2017")

tic()
HOOGSTEOPL_2017 <- read_sav("HOOGSTEOPL2017TABV3.sav") %>% 
  lazy_dt() %>% 
  glimpse()
toc()

variable.names(HOOGSTEOPL_2017)
head(HOOGSTEOPL_2017$OPLNIVSOI2016AGG4HBMETNIRWO) # Hoogst behaalde*
head(HOOGSTEOPL_2017$OPLNIVSOI2016AGG4HGMETNIRWO) # Hoogst genoten

# uitzoeken hoe er 18 categorieen van hoogst behaalde opleidingen zijn
# koppeling aan RIN moeder en vader
combi_covariaten13 <- combi_covariaten12 %>% 
  left_join(HOOGSTEOPL_2017 %>% select(RINPERSOONMa = RINPERSOON, edulevel_mother = OPLNIVSOI2016AGG4HBMETNIRWO), by = "RINPERSOONMa")  %>%
  left_join(HOOGSTEOPL_2017 %>% select(RINPERSOONpa = RINPERSOON, edulevel_father = OPLNIVSOI2016AGG4HBMETNIRWO), by = "RINPERSOONpa")  %>%
  glimpse()
View(combi_covariaten13$edulevel_mother)

# naar as.numeric
combi_covariaten13$edulevel_mother <- remove_labels(combi_covariaten13$edulevel_mother)
combi_covariaten13$edulevel_father <- remove_labels(combi_covariaten13$edulevel_father)
combi_covariaten13$edulevel_mother <- as.numeric(combi_covariaten13$edulevel_mother)
combi_covariaten13$edulevel_father <- as.numeric(combi_covariaten13$edulevel_father)

#check data + missings mother (41.6 %): 
table(combi_covariaten13$edulevel_mother)
sum(is.na(combi_covariaten13$edulevel_mother))
sum(complete.cases(combi_covariaten13$edulevel_mother))

(sum(is.na(combi_covariaten13$edulevel_mother)) / sum(complete.cases(combi_covariaten13$edulevel_mother))) *100

#check data + missings father (69.0%): 
table(combi_covariaten13$edulevel_father)
sum(is.na(combi_covariaten13$edulevel_father))
sum(complete.cases(combi_covariaten13$edulevel_father))

(sum(is.na(combi_covariaten13$edulevel_father)) / sum(complete.cases(combi_covariaten13$edulevel_father))) *100

# maak categorieen: laag, middelbaar, hoog (moeder)
combi_covariaten13 <- combi_covariaten13 %>% 
  mutate(
    edulevel_mother2 = case_when(
      edulevel_mother %in% c(1110, 1210, 1220, 1111, 1211, 1221, 1112, 1212, 1222, 1213) ~ "  - Low", 
      edulevel_mother %in% c(2110, 2120, 2130, 2111, 2121, 2131, 2112, 2123) ~ "  - Middle", 
      edulevel_mother %in% c(3110, 3210, 3111, 3211, 3112, 3212, 3113, 3213) ~ "  - High", 
      TRUE ~ "  - Other"
    )
  )

# maak categorieen: laag, middelbaar, hoog (vader)
combi_covariaten13 <- combi_covariaten13 %>% 
  mutate(
    edulevel_father2 = case_when(
      edulevel_father %in% c(1110, 1210, 1220, 1111, 1211, 1221, 1112, 1212, 1222, 1213) ~ "  - Low", 
      edulevel_father %in% c(2110, 2120, 2130, 2111, 2121, 2131, 2112, 2123) ~ "  - Middle", 
      edulevel_father %in% c(3110, 3210, 3111, 3211, 3112, 3212, 3113, 3213) ~ "  - High", 
      TRUE ~ "  - Other"
    )
  )

variable.names(combi_covariaten13)
table(combi_covariaten13$edulevel_mother2)
table(combi_covariaten13$edulevel_father2)

#check of moeders uit Banjaard groep inderdaad over het algemeen lager zijn opgeleid
combi_covariaten13 %>%  group_by(group, edulevel_mother2) %>%  count()

#check of vaders uit Banjaard groep inderdaad over het algemeen lager zijn opgeleid
combi_covariaten13 %>%  group_by(group, edulevel_father2) %>%  count()

# opslaan als rds bestand (sneller inladen)
# saveRDS(combi_covariaten13, "H:/Maxine/RDS files/combi_covariaten13.rds")
combi_covariaten13 <- readRDS("H:/Maxine/RDS files/combi_covariaten13.rds")
variable.names(combi_covariaten13)

#schrijf weg als SPSS bestand
write_csv(combi_covariaten13, "H:/Maxine/RDS files/combi_covariatenSPSS.sav")

----------------------------------------------------------------------
  ############## Geboorteland kind, ouders##################
----------------------------------------------------------------------
  combi_covariaten14 <- readRDS("H:/Maxine/RDS files/combi_covariaten14.rds")
variable.names(combi_covariaten14)

combi_covariaten14$Birth_country_mother[1]
setwd("H:/Maxine/Etniciteit")
LANDAKTUEELREFV13 <- read_sav("LANDAKTUEELREFV13.SAV")

# maak factor van LAND
land_dfr <- tibble(land = as_factor(LANDAKTUEELREFV13$LAND),
                   driedeling = as_factor(LANDAKTUEELREFV13$LANDDRIEDELING))


combi_covariaten15 <- 
  combi_covariaten14 |>
  mutate(Birth_country = fct_relevel(Birth_country, levels(land_dfr$land)),
         Birth_country_father = fct_relevel(Birth_country_father, levels(land_dfr$land)),
         Birth_country_mother = fct_relevel(Birth_country_mother, levels(land_dfr$land))) |>
  mutate(Birth_driedeling = land_dfr$driedeling[as.numeric(Birth_country)],
         .after = Birth_country) |>
  mutate(Birth_driedeling_father = land_dfr$driedeling[as.numeric(Birth_country_father)],
         .after = Birth_country_father) |>
  mutate(Birth_driedeling_mother = land_dfr$driedeling[as.numeric(Birth_country_mother)], 
         .after = Birth_country_mother)

table(combi_new$Birth_driedeling)
table(combi_new$Birth_driedeling_mother)
table(combi_new$Birth_driedeling_father)

saveRDS(combi_covariaten15, "H:/Maxine/RDS files/combi_covariaten15.rds")
variable.names(combi_covariaten15)


combi_covariaten15 %>% group_by(group) %>% summarize(mean_family_size = mean(Family_size, na.rm = T))

# Table of birth countries child
combi_covariaten15 %>% 
  group_by(group, Birth_driedeling) %>% 
  summarize(count = n()) %>% 
  ungroup()

# Table of birth countries mother
combi_covariaten15 %>% 
  group_by(group, Birth_driedeling_mother) %>% 
  summarize(count = n()) %>% 
  ungroup()

# Table of birth countries father
combi_covariaten15 %>% 
  group_by(group, Birth_driedeling_father) %>% 
  summarize(count = n()) %>% 
  ungroup()

#----------------------------------------------------------------------
  ############## Aantal thuiswonende kinderen ##################
# ----------------------------------------------------------------------
GBAHUISHOUDEN_2017 <- readRDS("H:/Maxine/RDS files/GBAHUISHOUDEN_2017.rds")
combi_covariaten15 <- readRDS("H:/Maxine/RDS files/combi_covariaten15.rds")

GBAHUISHOUDEN_2017$AANTALKINDHH
# koppeling aan RIN kind 

# binnen een jaar wisselt het aantal thuiswonende kinderen, daarom eerst uniek aantal selecteren
gezin_sam <- GBAHUISHOUDEN_2017 %>% select(RINPERSOON, DATUMAANVANGHH, AANTALKINDHH)

#selecteer enkel unieke aantallen (door de eerste pakken van degene op volgorde van begin 2017 tot eind 2017)
gezin_sam2 <- gezin_sam %>% 
  group_by(RINPERSOON)  %>% 
  arrange(desc(DATUMAANVANGHH)) %>% 
  filter(row_number()==1) %>% 
  glimpse()

# selecteer eerst Rinpersonen:  
gezin_sam3 <- gezin_sam2 %>% 
  left_join(gezin_sam2 %>% select(RINPERSOON, AANTALKINDHH), by = "RINPERSOON")  %>%
  glimpse() 

#### Toevoegen aan dataset
combi_covariaten15c <- combi_covariaten15 %>% 
  left_join(gezin_sam2 %>% select(Rinpersoon = RINPERSOON, number_children = AANTALKINDHH), by = "Rinpersoon")  %>%
  glimpse()

#check of het klopt
result <- combi_covariaten15c %>% 
  filter(number_children == 0) %>% 
  group_by(group) %>% 
  summarise(count = n())

result 

age <- combi_covariaten15c %>% 
  filter(number_children == 0) %>% 
  select(leeftijd, number_children, Family_size)

age

range(combi_covariaten15c$number_children, na.rm = T)

combi_covariaten16 <- combi_covariaten15c
saveRDS(combi_covariaten16, "H:/Maxine/RDS files/combi_covariaten16.rds")
