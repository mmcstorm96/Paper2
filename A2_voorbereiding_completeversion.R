#------------------------------------------------------------------------------
############################# Case-contol study ###############################
#------------------------------------------------------------------------------

## Artikel 2 - voorbereiding (complete versie)
## Gemaakt door Erik Giltay & Maxine Storm
## Juli/Augustus 2023
##
## Hierin zijn de volgende databestanden gecombineerd: 
# A2_voorbereiding_erik2.R
# A2_voorbereiding_erik.R
# Script - Maxine2.R
##
# Resultaten zijn gebaseerd op het jaar 2018, selectiecriteria: 
# Banjaard kinderen waren in 2018 in zorg (uitschrijfdatum >= 2019, inschrijfdatum <= 2018)
# Banjaard kinderen waren in 2018 17 jaar of jonger, maar wel geboren (geboortejaar <= 2017 & geboortejaar > 2000)
# ! Inschrijfjaar is niet langer dan 5 jaar geleden vanaf 2017

## clear global environment
rm(list = ls(all = TRUE))

## load packages
library(haven)
library(tidyverse)
library(data.table)
library(dtplyr)
library(tictoc)

# set working directory
setwd("H:/Maxine/Subselectie databestanden/PG")

# inladen Banjaard data
INS_B <- read_sav("PG_INS.sav")
length(unique(INS_B$Rinpersoon)) # 2126 unieke personen 

# welke variabelen bevat de dataset
variable.names(INS_B) # geboortejaar #inschrijfdatum

#### INLADEN GROTE DATAFILE ####
# * verander working directory om CBS data te selecteren 
# setwd("G:/Bevolking/GBAPERSOONTAB/2017")
 
# tic()
#  gba_persoon <- read_sav("GBAPERSOON2017TABV1.sav") %>% 
#   lazy_dt() %>% 
# glimpse()
# toc()

# opslaan als rds bestand (sneller inladen)
# saveRDS(gba_persoon, "H:/Maxine/RDS files/gba_persoon_2017.rds")

################################################################################
############################# Banjaard kinderen (A) ############################
################################################################################
banjaard1 <- INS_B %>% 
  mutate(uitschrijfjaar = str_sub(uitschrijfdatum, 1, 4) %>% as.numeric(),
         leeftijd = 2017 - geboortejaar) %>% # leeftijd in 2017
  filter(uitschrijfjaar >= 2018 & geboortejaar <= 2016 & geboortejaar > 1999) %>% # selectiecriteria
  mutate(inschrijfdatum = str_sub(inschrijfdatum, 1, 4) %>% as.numeric()) %>% 
  mutate(group = "A. Banjaard") %>% 
  glimpse()

banjaard1$Rinpersoon %>% unique() %>% length() # N = 1205

################################################################################
############################### Youz kinderen (B) ##############################
################################################################################
setwd("H:/Maxine/Subselectie databestanden/PG")

youz <- read_sav("PG_INS_YOUZ_WIP.sav") %>%
  mutate(uitschrijfjaar = str_sub(uitschrijfdatum, 1, 4) %>% as.numeric(), 
         leeftijd = 2017 - geboortejaar) %>% # leeftijd in 2017
  mutate(inschrijfdatum = str_sub(inschrijfdatum, 1, 4) %>% as.numeric()) %>% 
  filter(uitschrijfjaar >= 2018 & geboortejaar <= 2016 & geboortejaar > 1999) %>% # selectiecriteria
  left_join(banjaard1 %>% select(Rinpersoon) %>% mutate(Banjaard = 1)) %>% 
  filter(is.na(Banjaard)) %>% 
  mutate(group = "B. Youz") %>%
  glimpse()

range(youz$leeftijd) # leeftijd 4 tot 21 jaar in het jaar 2021

### Combi bestand, met Banjaard en Youz label (tot 20 jaar, ... Banjaard kinderen)
combi <- full_join(banjaard1, youz) %>% 
  filter(leeftijd <= 17, str_sub(inschrijfdatum, 1, 4) %>% as.numeric() <= 2017 & inschrijfdatum >= 2012) %>% # niet langer dan 6 jaar in zorg
  select(Rinpersoon, sex = geslacht, inschrijfdatum, leeftijd, postc, group) %>%
  na.omit() %>% 
  glimpse()

# Verdeling inschrijfdatum per groep
combi %>% select(group, inschrijfdatum) %>% table()

banjaard1$inschrijfdatum
table(combi$group)
# 505 Banjaard kinderen
# 2767 Youz kinderen


################################################################################
######################### Kinderen algemene bevolking (C) ######################
################################################################################
#inladen data
gba_persoon <- readRDS("H:/Maxine/RDS files/gba_persoon_2017.rds")

#### LEEFTIJD ####

# bereken leeftijd a.d.h.v. geboortejaar, selecteer groep
# gp <- gba_persoon %>% 
#    mutate(leeftijd = 2017 - as.numeric(GBAGEBOORTEJAAR)) %>% 
#    filter(leeftijd <= 20 & leeftijd >=4) %>% # leeftijd onder de 21 jaar
#    select(Rinpersoon = RINPERSOON, sex = GBAGESLACHT, leeftijd) %>% 
#    mutate(sex = ifelse(sex == "1", "Man", "Vrouw")) %>%  # documentatie zegt: 1 = man
#    glimpse()
# gp$Rinpersoon %>% unique() %>% length()

# opslaan als rds bestand (sneller inladen)
# saveRDS(gp, "H:/Maxine/RDS files/generalpopulation.rds")
##### Inladen subselectie GBApersoon
gp <- readRDS("H:/Maxine/RDS files/generalpopulation.rds")

#### POSTCODES ####

#Achterhalen van postcodes vanuit microdata CBS
# Inladen postcodes VSLGWB2022TAB03V2 (hierin staan rinobjectnrs, geen rinpersoonnummers)
# tic()
# postcodes_GP <- read_sav("G:/BouwenWonen/VSLPOSTCODEBUS/VSLPOSTCODEBUSV2023031.sav") %>% 
#  glimpse()
# toc()

#!!!!!!!! hier gaat iets mis!
# opslaan als rds bestand (sneller inladen)
##### Inladen bestand met postcodes obv RINOBJECTNUMMERS
# saveRDS(postcode, "H:/Maxine/RDS files/postcodes_GP.rds")
postcodes_GP <- readRDS("H:/Maxine/RDS files/postcodes_GP.rds")

# inladen GBAADRESOBJECTBUS bestand met RINnummers (hierin staan zowel rinpersoonnummers als rinobjectnrs)
# tic()
# koppel_persoon_object <- read_sav("G:/Bevolking/GBAADRESOBJECTBUS/GBAADRESOBJECT2022BUSV1.sav") %>%
#  glimpse()
# toc()
# View(koppel_persoon_object)

# opslaan als rds bestand (sneller inladen)
# saveRDS(koppel_persoon_object, "H:/Maxine/RDS files/koppelbestand_persoon_object.rds")
##### Inladen van koppelbestand
koppel_persoon_object <- readRDS("H:/Maxine/RDS files/koppelbestand_persoon_object.rds")


### Combineren RINPERSOON met RINOBJECT ###
# voeg RINPERSOON toe aan postcodebestand
PERSOON_OBJECT_POSTCODE <- left_join(koppel_persoon_object %>%
                                     select(RINPERSOON, RINOBJECTNUMMER, GBADATUMAANVANGADRESHOUDING, GBADATUMEINDEADRESHOUDING),
                                     postcode, by = "RINOBJECTNUMMER")
#! warning: many-to-many relationship


# selecteer enkel personen waarbij 2017 tussen de aanvang en eindpostcode datum ligt
PERSOON_OBJECT_POSTCODE_2017 <- PERSOON_OBJECT_POSTCODE %>%
 filter(str_sub(GBADATUMAANVANGADRESHOUDING, 1, 4) %>% as.numeric() < 2017
        & str_sub(GBADATUMEINDEADRESHOUDING, 1, 4) %>% as.numeric() > 2017) %>%
 mutate(Rinpersoon = RINPERSOON, postc = POSTCODENUM)

# opslaan als rds bestand (sneller inladen)
# saveRDS(PERSOON_OBJECT_POSTCODE_2017, "H:/Maxine/RDS files/koppelbestand_persoon_object_2017.rds")
##### Inladen van koppelbestand 
PERSOON_OBJECT_POSTCODE_2017 <- readRDS("koppelbestand_persoon_object_2017.rds")

#### Combineer bestanden ####

# combineer combi bestand met gba bestand, mogelijk bovenstaand combi databestand aanvullen met data uit GBAPERSOON
combined_cbs <- left_join(gba_persoon %>% 
                            select(RINPERSOON, GBAGEBOORTELAND, GBAGESLACHT, GBAGEBOORTEJAAR), 
                          PERSOON_OBJECT_POSTCODE_2017, by = "RINPERSOON") 

combined_cbs1 <- combined_cbs %>% 
  mutate(GBAGEBOORTEJAAR = GBAGEBOORTEJAAR %>%  as.numeric, leeftijd = 2017 - GBAGEBOORTEJAAR, Rinpersoon = RINPERSOON, sex = GBAGESLACHT, group = "C") %>%
  select(Rinpersoon, sex, leeftijd, postc, group)

# selecteer enkel mensen van onder de 18 jaar, verwijder iedereen met ontbrekende data
combined_cbs2 <- combined_cbs1 %>% filter(leeftijd <= 18) %>% na.omit() %>% 
  glimpse()


####### POSTCODESELECTIE ######

# Postcodes achterhalen (114 in totaal, gebieden: Den Haag, Leidschendam-Voorburg, Zoetermeer, Rijswijk, Wassenaar)
all_post <- c(
  2240:2245, 2261:2264, 2266:2267, 2272:2275, 2281:2289, 2490:2493, 2495:2498, 2500:2509,
  2511:2518, 2521:2526, 2531:2533, 2541:2548, 2551:2555, 2561:2566, 2571:2574, 2581:2587,
  2591:2597, 2711:2713, 2715:2719, 2721:2729
)

# postcodes buiten bovenstaande gebieden waar banjaard kinderen wonen
buiten_Banjaard_post <- c(2265, 2271, 2291, 2292, 2295, 2613, 2614, 2616, 2622, 2624, 2625, 2627, 
                          2628, 2631, 2632, 2635, 2636, 2641, 2642, 2643, 2645, 2671, 2672, 2673, 
                          2675, 2678, 2681, 2684, 2685, 2691, 2692, 2694, 3155)

# Volledig aantal postcodes uit: Leidschendam, Voorburg, Wateringen, Kwintsheul, Delft, Nootdorp, Den Hoorn, Schipluiden,
# Pijnacker, Delftgauw, Naaldwijk, Honselersdijk, De Lier, Monster, Ter Heijde, Poeldijk, 's Gravenzande, Maasland (59 in totaal)
overige_post <- c(2260, 2261, 2262, 2263, 2264, 2265, 2266, 2267, 2270, 2271, 2272, 2273, 2274, 2275, 2290, 2291, 2292, 2295, 
                  2600, 2601, 2611, 2612, 2613, 2614, 2616, 2622, 2623, 2624, 2625, 2626, 2627, 2628, 2629, 2630, 2631, 2632, 
                  2635, 2636, 2640, 2641, 2642, 2643, 2645, 2670, 2671, 2672, 2673, 2675, 2678, 2680, 2681, 2684, 2685, 2690, 
                  2691, 2692, 2693, 2694, 3155)

volledig_post <- c(all_post, buiten_Banjaard_post, overige_post)
length(unique(volledig_post)) # 163 unieke postcodes in totaal


##### Selectie subpopulatie #####
# selecteer alleen als CBS-kinderen uit bovenstaande postcodegebieden komen
# gp2 <- combined_cbs2 %>% 
#  filter(postc %in% as.character(volledig_post)) %>% 
#  glimpse()

saveRDS(gp2, "H:/Maxine/RDS files/generalpopulation.rds")
gp2 <- readRDS("generalpopulation.rds")

gp3 <- gp2 %>% 
  filter(!(Rinpersoon %in% combi$Rinpersoon)) %>% 
  mutate(sex = ifelse(sex == "1", "Man", "Vrouw"), 
         group = "C. General population") %>% 
  glimpse()

nrow(gp3) # 236501 personen

set.seed(1) # zorg dat we steeds dezelde controlegroep krijgen

#### Sample uit gehele subpopulatie ####
kiezen_uit <- gp3
combi_driegroepen <- combi %>% 
  cbind(case_control = 1:nrow(combi)) %>% 
  glimpse()

tic()
i = 1
for (i in 1:nrow(combi)){  
  leeftijd_kiezen <- slice(combi_driegroepen, i) %>% pull(leeftijd)
  sex_kiezen <- slice(combi_driegroepen, i) %>% pull(sex)
  kiezen_uit_selectie <- kiezen_uit %>% 
    filter(sex == sex_kiezen, leeftijd == leeftijd_kiezen) %>% 
    group_by(Rinpersoon) %>% slice_head(n=1) %>% ungroup()
  gekozen <- sample(kiezen_uit_selectie$Rinpersoon, size=5, replace = FALSE) # 5 personen kiezen, geen dubbele
  gekozen_rows <- kiezen_uit_selectie %>% 
    filter(Rinpersoon %in% gekozen) %>% 
    mutate(case_control = i, inschrijfdatum = NA) # zodat we de controles bij de case kunnen groeperen
  combi_driegroepen <- rbind(combi_driegroepen, gekozen_rows) #voeg 5 records toe # FOUT!
  kiezen_uit <- kiezen_uit %>% 
    filter(!(Rinpersoon %in% gekozen)) # zorg ervoor dat dezelfde persoon niet 2 maal gekozen kan worden, dus 5 verwijderen
}
toc()

gekozen_rows$inschrijfdatum

combi_driegroepen %>% group_by(Rinpersoon) %>% filter(n() > 1) %>% view()

nrow(combi_driegroepen)
length(unique(combi_driegroepen$Rinpersoon))

combi_driegroepen %>% group_by(Rinpersoon) %>% filter(n() > 1) %>% view()

saveRDS(combi_driegroepen, "H:/Maxine/RDS files/combi_driegroepen.rds")
combi_driegroepen <- readRDS("H:/Maxine/RDS files/combi_driegroepen.rds")
View(combi_driegroepen)
combi_driegroepen$Rinpersoon %>% unique() %>% length()

combi_driegroepen$group %>% table()