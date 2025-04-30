########### Baseline characteristics table ############

#### Results section

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

combi_covariaten13 <- readRDS("H:/Maxine/RDS files/combi_covariaten13.rds")

# hoe check je missings?
sum(is.na(combi_covariaten13$sex))
View(combi_covariaten13)


# nieuwe groepen maken (recode into different variable) --> ENKEL unieke personen meenemen
casenr_banjaard <- combi_covariaten13 %>% filter(group == "A. Banjaard") %>% pull(case_control) %>% unique()
length(casenr_banjaard)

# casenr_banjaard <- combi_covariaten13 %>% filter(group == "A. Banjaard") %>% pull(case_control) # 6 meer
# length(casenr_banjaard)
# dus 6 dubbele?

casenr_youz <- combi_covariaten13 %>% filter(group == "B. Youz") %>% pull(case_control) %>% unique()
length(casenr_youz)
# casenr_youz <- combi_covariaten13 %>% filter(group == "B. Youz") %>% pull(case_control) # 58 meer
# length(casenr_youz)
# dus 58 dubbele?

# N per groep berekenen
combi_covariaten13 |>
unique() |>
  mutate(group_tbl = case_when(group == "A. Banjaard" ~ "Banjaard",
                               group == "B. Youz" ~ "Youz",
                               group == "C. General population" & case_control %in% casenr_banjaard ~ "Control Banjaard",
                               group == "C. General population" & case_control %in% casenr_youz ~ "Control Youz")) |>
pull(group_tbl) |> table()

# 5 * Banjaard = N Case Banjaard en 5 * Youz = N Case Youz
View(combi_covariaten14)

# nieuwe dataset met nieuwe variabele group_tbl en ENKEL UNIEKE personen 
combi_covariaten14 <- 
  combi_covariaten13 |>
  unique() |>
  mutate(group_tbl = case_when(group == "A. Banjaard" ~ "Banjaard",
                               group == "B. Youz" ~ "Youz",
                               group == "C. General population" & case_control %in% casenr_banjaard ~ "Control Banjaard",
                               group == "C. General population" & case_control %in% casenr_youz ~ "Control Youz"))

nrow(combi_covariaten14) # 440 minder dan combi_covariaten13 (door dubbelen?)


# opslaan als rds bestand (sneller inladen)
# saveRDS(combi_covariaten14, "H:/Maxine/RDS files/combi_covariaten14.rds")
combi_covariaten14 <- readRDS("H:/Maxine/RDS files/combi_covariaten14.rds")

#####################################################################################
######### bereken gemiddelde en standaarddeviatie voor numerieke variabelen #########
#####################################################################################

#--Leeftijd--
sum(is.na(combi_covariaten14$leeftijd)) # geen missings


combi_covariaten14 |>
  group_by(group_tbl) |> # LET OP: eerst Control Youz weergegeven, daarna Youz
  summarise(avg_age = round(mean(leeftijd), 2), sd_age = round(sd(leeftijd),2))

#--Family size (old) --

#n 
valid <- nrow(combi_covariaten14) - sum(is.na(combi_covariaten14$Family_size))
valid
#percentage 
valid /nrow(combi_covariaten14) * 100

combi_covariaten14 |>
  group_by(group_tbl) |>
  summarise(avg_famsize = mean(Family_size, na.rm = T), sd_famsize = sd(Family_size, na.rm = T))

#--Number of children (new) --

#n 
valid <- nrow(combi_covariaten16) - sum(is.na(combi_covariaten16$number_children))
valid
#percentage 
valid /nrow(combi_covariaten16) * 100

combi_covariaten16 |>
  group_by(group_tbl) |>
  summarise(avg_nochild = mean(number_children, na.rm = T), sd_famsize = sd(number_children, na.rm = T))


#--Age mother--
#n 
valid <- nrow(combi_covariaten14) - sum(is.na(combi_covariaten14$Mother_age))
valid
#percentage 
valid /nrow(combi_covariaten14) * 100

combi_covariaten14 |>
  group_by(group_tbl) |>
  summarise(avg_famsize = mean(Mother_age, na.rm = T), sd_famsize = sd(Mother_age, na.rm = T))

#--Age father--
#n 
valid <- nrow(combi_covariaten14) - sum(is.na(combi_covariaten14$Father_age))
valid
#percentage 
valid /nrow(combi_covariaten14) * 100

combi_covariaten14 |>
  group_by(group_tbl) |>
  summarise(avg_famsize = mean(Father_age, na.rm = T), sd_famsize = sd(Father_age, na.rm = T))

#--Household income--
#n 
valid <- nrow(combi_covariaten14) - sum(is.na(combi_covariaten14$Percentiles_household_income))
valid
#percentage 
valid /nrow(combi_covariaten14) * 100

combi_covariaten14 |>
  group_by(group_tbl) |>
  summarise(avg_inc = mean(Percentiles_household_income, na.rm = T), sd_inc = sd(Percentiles_household_income, na.rm = T))

#--Low neighborhood education level--
#n 
valid <- nrow(combi_covariaten14) - sum(is.na(combi_covariaten14$nh_edulevel_low))
valid
#percentage 
valid /nrow(combi_covariaten14) * 100

combi_covariaten14 |>
  group_by(group_tbl) |>
  summarise(avg_nhedulow = mean(nh_edulevel_low, na.rm = T), sd_nhedulow = sd(nh_edulevel_low, na.rm = T))


#--Low neighborhood income--
#n 
valid <- nrow(combi_covariaten14) - sum(is.na(combi_covariaten14$low_income_perc_postcode))
valid
#percentage 
valid /nrow(combi_covariaten14) * 100

combi_covariaten14 |>
  group_by(group_tbl) |>
  summarise(avg_nhedulow = mean(low_income_perc_postcode, na.rm = T), sd_nhedulow = sd(low_income_perc_postcode, na.rm = T))

########################################################################
######### bereken frequentie en % voor categorische variabelen #########
########################################################################

#--sex--
#n 
valid <- nrow(combi_covariaten14) - sum(is.na(combi_covariaten14$sex))
valid
#percentage 
valid /nrow(combi_covariaten14) * 100

tib <- combi_covariaten14 |>
  group_by(group_tbl, sex) |>
  count()

Perc_B <- tib$n / 505 * 100
Perc_BC <- tib$n / 2525 * 100
Perc_YC <- tib$n / 13835 * 100
Perc_Y <- tib$n / 2767 * 100

new_Perc <- c(Perc_B[1:2], Perc_BC[3:4], Perc_YC[5:6], Perc_Y[7:8])
tib$Percentages <- new_Perc
tib

#--Birth country child-- (nieuw, april 2024)
#n 
valid <- nrow(combi_covariaten15) - sum(is.na(combi_covariaten15$Birth_driedeling))
valid
#percentage 
valid /nrow(combi_covariaten15) * 100

tib <- combi_covariaten15 |>
  group_by(group_tbl, Birth_driedeling) |>
  count()


Perc_B <- tib$n / 505 * 100
Perc_BC <- tib$n / 2525 * 100
Perc_YC <- tib$n / 13835 * 100
Perc_Y <- tib$n / 2767 * 100

new_Perc <- c(Perc_B[1:3], Perc_BC[4:6], Perc_YC[7:9], Perc_Y[10:12])
tib$Percentages <- new_Perc
tib

#--Birth country mother-- (nieuw, april 2024)
combi_covariaten15 <- readRDS("H:/Maxine/RDS files/combi_covariaten15.rds")

#n 
valid <- nrow(combi_covariaten15) - sum(is.na(combi_covariaten15$Birth_driedeling))
valid
#percentage 
valid /nrow(combi_covariaten15) * 100

tib <- combi_covariaten15 |>
  group_by(group_tbl, Birth_driedeling_mother) |>
  count()

Perc_B <- tib$n / 505 * 100
Perc_BC <- tib$n / 2525 * 100
Perc_YC <- tib$n / 13835 * 100
Perc_Y <- tib$n / 2767 * 100

new_Perc <- c(Perc_B[1:3], Perc_BC[4:6], Perc_YC[7:9], Perc_Y[10:12])
tib$Percentages <- new_Perc
tib

#--Birth country father-- (nieuw, april 2024)

#n 
valid <- nrow(combi_covariaten15) - sum(is.na(combi_covariaten15$Birth_driedeling_father))
valid
#percentage 
valid /nrow(combi_covariaten15) * 100

tib <- combi_covariaten15 |>
  group_by(group_tbl, Birth_driedeling_father) |>
  count()


Perc_B <- tib$n / 505 * 100
Perc_BC <- tib$n / 2525 * 100
Perc_YC <- tib$n / 13835 * 100
Perc_Y <- tib$n / 2767 * 100

new_Perc <- c(Perc_B[1:3], Perc_BC[4:6], Perc_YC[7:9], Perc_Y[10:12])
tib$Percentages <- new_Perc
tib

#--family type--
#n 
valid <- nrow(combi_covariaten14) - sum(is.na(combi_covariaten14$family_type))
valid

#percentage 
valid /nrow(combi_covariaten14) * 100

#hercodeer zodat er 3 categorieen ontstaan
combi_covariaten14c <- combi_covariaten14 %>% 
  mutate(
    family_type3 = case_when(
      family_type %in% c(4,5) ~ '  - Dual parent household', 
      family_type == 6 ~ '  - Single parent household', 
      TRUE ~ 'other'
    )
  )
table(combi_covariaten14c$family_type3)

tib <- combi_covariaten14c |>
  group_by(group_tbl, family_type3) |>
  count()
tib
Perc_B <- tib$n / 505 * 100
Perc_BC <- tib$n / 2525 * 100
Perc_YC <- tib$n / 13835 * 100
Perc_Y <- tib$n / 2767 * 100

new_Perc <- c(Perc_B[1:3], Perc_BC[4:6], Perc_YC[7:9], Perc_Y[10:12])
nrow(tib)
length(new_Perc)
tib$Percentages <- new_Perc
tib


#--education level mother--
# other category hier is missing
combi_covariaten14c <- combi_covariaten14 %>% 
  mutate(edulevel_mother3 = replace(
    edulevel_mother2, 
    edulevel_mother2 == "  - Other", NA))


tib <- combi_covariaten14c |>
  group_by(group_tbl, edulevel_mother3) |>
  count()
tib
Perc_B <- tib$n / sum(tib$n[1:3]) * 100 # valid %, dus zonder missings
Perc_BC <- tib$n / sum(tib$n[5:7]) * 100 # valid %
Perc_YC <- tib$n / sum(tib$n[9:11]) * 100 # valid %
Perc_Y <- tib$n / sum(tib$n[13:15]) * 100 # valid %

new_Perc <- c(Perc_B[1:4], Perc_BC[5:8], Perc_YC[9:12], Perc_Y[13:16])
nrow(tib)
length(new_Perc)
tib$Percentages <- new_Perc
tib # negeer hier de NA rijen, LET OP: volgorde categorieen (high, LOW, MIDDLE) --> lastig aflezen

#n 
valid <- nrow(combi_covariaten14c) - sum(is.na(combi_covariaten14c$edulevel_mother3))
valid

#percentage 
valid /nrow(combi_covariaten14c) * 100

#--education level father--
# other category hier is missing
combi_covariaten14c <- combi_covariaten14 %>% 
  mutate(edulevel_father3 = replace(
    edulevel_father2, 
    edulevel_father2 == "  - Other", NA))


tib <- combi_covariaten14c |>
  group_by(group_tbl, edulevel_father3) |>
  count()
tib
Perc_B <- tib$n / sum(tib$n[1:3]) * 100 # valid %, dus zonder missings
Perc_BC <- tib$n / sum(tib$n[5:7]) * 100 # valid %
Perc_YC <- tib$n / sum(tib$n[9:11]) * 100 # valid %
Perc_Y <- tib$n / sum(tib$n[13:15]) * 100 # valid %

new_Perc <- c(Perc_B[1:4], Perc_BC[5:8], Perc_YC[9:12], Perc_Y[13:16])
nrow(tib)
length(new_Perc)
tib$Percentages <- new_Perc
tib # negeer hier de NA rijen, LET OP: volgorde categorieen (high, LOW, MIDDLE) --> lastig aflezen

#n 
valid <- nrow(combi_covariaten14c) - sum(is.na(combi_covariaten14c$edulevel_father3))
valid

#percentage 
valid /nrow(combi_covariaten14c) * 100

#--urbanisation class--
#n 
valid <- nrow(combi_covariaten14) - sum(is.na(combi_covariaten14$urbanization_class))
valid

#percentage 
valid /nrow(combi_covariaten14) * 100

tib <- combi_covariaten14 |>
  group_by(group_tbl, urbanization_class) |>
  count()

table(combi_covariaten14$urbanization_class)


#hercodeer zodat er 2 categorieen ontstaan

combi_covariaten14c <- combi_covariaten14 %>% 
  mutate(urbanization_class2 = case_when(
    urbanization_class %in% c(1,2) ~ "- Densely populated (class 1,2)",
    urbanization_class %in% c(3:5) ~ "- Sparsely populated (class 3,4,5)"
         )
    )


table(combi_covariaten14c$urbanization_class2)

tib <- combi_covariaten14c |>
  group_by(group_tbl, urbanization_class2) |>
  count()
tib
Perc_B <- tib$n / 505 * 100
Perc_BC <- tib$n / 2525 * 100
Perc_YC <- tib$n / 13835 * 100
Perc_Y <- tib$n / 2767 * 100

new_Perc <- c(Perc_B[1:2], Perc_BC[3:4], Perc_YC[5:6], Perc_Y[7:8])
nrow(tib)
length(new_Perc)
tib$Percentages <- new_Perc
tib

