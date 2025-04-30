################################################################################
############################# Case-control study ###############################
################################################################################
## socio-economische factoren 
## Gemaakt door Erik Giltay, Maxine Storm
## Laatste bewerking: 05-04-2024
################################################################################

## clear global environment
rm(list = ls(all = TRUE))

## load packages
library(haven)
library(tidyverse)
library(data.table)
library(dtplyr)
library(tictoc)
library(forestplot)
library(nnet)
library(mice)
library(modelsummary)
library(labelled)

################################################################################
#### HANDIGE FUNCTIES EN INLADEN DATAFILE ####
################################################################################

decimaal <- function(x, k) {
  rounded = round (x, k)
  formatted = sprintf(paste0("%0.", k, "f"), rounded)
  return(formatted)
}

maak_p <- function(p) {
  uitvoer_p <- ifelse(p<0.01, decimaal(p,3), paste0(decimaal(p,2), " "))
  uitvoer_p <- ifelse(decimaal(p,2)==0.05, decimaal (p,3), uitvoer_p)
  uitvoer_p <- ifelse(uitvoer_p=="0.000", "p<0.001", paste0("p=", uitvoer_p))
  return(uitvoer_p)
}
scale2 <- function(x, na.rm = TRUE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)

# inladen dataset
combi_covariaten16 <- readRDS("H:/Maxine/RDS files/combi_covariaten16.rds")

################################################################################
#### LOGISTISCHE REGRESSIE: banjaard vs controle
################################################################################

#!!!!!!! meedoen_nummers !!!!!!!!!
meedoen_nummers <- combi_covariaten16 %>% filter(group == "A. Banjaard") %>% pull(case_control) %>% unique()
# meedoen_nummers <- combi_covariaten16 %>% filter(group == "B. Youz") %>% pull(case_control) %>% unique()
# meedoen_nummers <- combi_covariaten16 %>% filter(group != "C. General population ") %>% pull(case_control) %>% unique()

#----------------------- veranderen naar 20 ----------------------------#
no_imput = 20 # AANPASSEN!


banjaard <- combi_covariaten16 %>% filter(case_control %in% meedoen_nummers) %>% 
  mutate(binair = group == "A. Banjaard") %>% 
  # mutate(binair = group == "B. Youz") %>% 
  select(binair, sex, leeftijd, Mother_age, Father_age, nh_edulevel_low, 
         number_children, family_type2, edulevel_mother2, edulevel_father2,
         Percentiles_household_income, Birth_driedeling, Birth_driedeling_mother, 
         Birth_driedeling_father, urbanization_class = urbanization_class, 
         Low_income_neighbourhood = median_income_postcode) %>% 
  mutate(Percentiles_household_income = -Percentiles_household_income,
         number_children = -number_children,
         Low_income_neighbourhood = - Low_income_neighbourhood,
         Urbanization_class = ifelse(urbanization_class %in% c("3", "4", "5"), "1", "2"),
         family_type2 = ifelse(family_type2 %in% c("  - institutional household", "other"), NA, family_type2),
         family_type2 = ifelse(str_detect(family_type2, "impossible"), NA, family_type2),
         edulevel_mother2 = ifelse(edulevel_mother2 == "  - Other", NA, edulevel_mother2), 
         edulevel_mother2 = factor(edulevel_mother2, levels = c("  - High", "  - Middle", "  - Low")),
         edulevel_father2 = ifelse(edulevel_father2 == "  - Other", NA, edulevel_father2), 
         edulevel_father2 = factor(edulevel_father2, levels = c("  - High", "  - Middle", "  - Low")),
         sex = ifelse(sex == "Vrouw", "1","2"),
         Birth_driedeling = factor(Birth_driedeling, exclude = "Onbekend"),
         Birth_driedeling_father = factor(Birth_driedeling_father, exclude = "Onbekend"),
         Birth_driedeling_mother = factor(Birth_driedeling_mother, exclude = "Onbekend"),
         Birth_driedeling = recode(Birth_driedeling, "Europa (exclusief Nederland)" = "  - Europe", "Buiten Europa" = "  - Country outside Europe", .default = "  - The Netherlands"),
         Birth_driedeling_mother = recode(Birth_driedeling_mother, "Europa (exclusief Nederland)" = "  - Europe", "Buiten Europa" = "  - Country outside Europe", .default = "  - The Netherlands"),
         Birth_driedeling_father = recode(Birth_driedeling_father, "Europa (exclusief Nederland)" = "  - Europe", "Buiten Europa" = "  - Country outside Europe", .default = "  - The Netherlands"),
         )

calc_beta <- function(model1){
  model1 %>% 
    purrr::map(\(x) modelsummary(x,output = "modelsummary_list", standardize = "basic")$tidy %>% 
                 tail(-1) %>% select(term, beta = estimate, se = std.error, statistic, p = p.value)) %>% 
    melt() %>% 
    group_by(term, variable) %>% 
    summarise(value = mean(value)) %>% -
    ungroup() %>% 
    spread(variable, value) %>% 
    mutate(pvalue = maak_p(p))
}

set.seed(1)

add_reference_rows <- function (db1, rownumber, header, ref_group){
  if (!is.na(ref_group)) {
    db1 <- add_row(db1, .before = rownumber, term = ref_group, estimate = "1.00 (Ref.)", 
                   or = 1 , lower = 1, upper = 1, estimate_unadj = "1.00 (Ref.)")
  }
  add_row(db1, .before = rownumber, term = header)
}

# adjusted model (multivariate)
adjusted <- with(mice(banjaard, method = "pmm", m = no_imput), # verander m = 5/10/25
                 glm(binair ~ sex + leeftijd + Birth_driedeling + Birth_driedeling_mother + 
                       Birth_driedeling_father + number_children + family_type2 + Mother_age + Father_age + 
                       edulevel_mother2 + edulevel_father2 + Percentiles_household_income + 
                       Urbanization_class + nh_edulevel_low + Low_income_neighbourhood, 
                     family = binomial())) %>% 
  .$analyses %>% 
  calc_beta() %>% 
  mutate(lower = exp(beta - 1.96 * se), upper = exp(beta + 1.96 * se), 
         statistic = decimaal(statistic, 3), or = exp(beta),
         estimate = paste0(decimaal(or, 2), " (", decimaal(lower, 2), "; ", 
                           decimaal(upper, 2), ")")) %>% 
  select(term, estimate, or, lower, upper, statistic, pvalue) %>% 
  glimpse()

# unadjusted model (univariate)

imputatie_file <- mice(banjaard, m = no_imput)

unadjusted <- list(
  with(imputatie_file, glm(binair ~ sex, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ leeftijd, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ Birth_driedeling, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ Birth_driedeling_mother, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ Birth_driedeling_father, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ number_children, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ family_type2, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ Mother_age, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ Father_age, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ edulevel_mother2, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ edulevel_father2, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ Percentiles_household_income, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ Urbanization_class, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ nh_edulevel_low, family = binomial)) %>% .$analyses %>% calc_beta(), 
  with(imputatie_file, glm(binair ~ Low_income_neighbourhood, family = binomial)) %>% .$analyses %>% calc_beta()) %>% 
  purrr::map_df(rbind) %>% 
  mutate(lower = exp(beta - 1.96 * se), upper = exp(beta + 1.96 * se) , 
         statistic = decimaal(statistic, 3), or = exp(beta),
         estimate_unadj = paste0(decimaal(or, 2), " (", decimaal(lower, 2), "; ",                       
                                 decimaal(upper, 2), ")")) %>% 
  select(term, estimate_unadj, stat_unadj = statistic, p_unadj = pvalue)

draai_rij_om <- function (df1, rij1, rij2){
  df1 %>% 
    mutate(new_row_number = case_when(row_number() == rij1 ~ rij2, 
                                      row_number() == rij2 ~ rij1, 
                                      TRUE ~ row_number())) %>% 
    arrange(new_row_number) %>% 
    select(-new_row_number)
}

combined <- unadjusted %>% 
  left_join(adjusted, by = join_by(term)) %>% 
  add_reference_rows(., rownumber = 18, header = "Urbanization class:", ref_group = "  - Sparsely populated (class 3-5)") %>% 
  add_reference_rows(., rownumber = 18, header = "Neighborhood domain:", ref_group = NA) %>% 
  add_reference_rows(., rownumber = 17, header = "Economic domain:", ref_group = NA) %>% 
  add_reference_rows(., rownumber = 15, header = "Education level father:", ref_group = "  - High") %>% 
  add_reference_rows(., rownumber = 13, header = "Education level mother:", ref_group = "  - High") %>% 
  add_reference_rows(., rownumber = 10, header = "Family type:", ref_group = "  - Dual parent household") %>% 
  add_reference_rows(., rownumber = 9, header = "Social and cultural domain:", ref_group = NA) %>% 
  add_reference_rows(., rownumber = 7, header = "Father's birth country:", ref_group = "  - The Netherlands") %>%
  add_reference_rows(., rownumber = 5, header = "Mother's birth country:", ref_group = "  - The Netherlands") %>%
  add_reference_rows(., rownumber = 3, header = "Child's birth country:", ref_group = "  - The Netherlands") %>%
  add_reference_rows(., rownumber = 1, header = "Demographic domain:", ref_group = NA) %>% 
  mutate(term = ifelse(term == "sex2", "Male sex", term),
         term = ifelse(term == "leeftijd", "Age", term),
         term = ifelse(str_detect(term, "- Low"), "  - Low", term),
         term = ifelse(str_detect(term, "Middle"), "  - Moderate", term),
         term = ifelse(str_detect(term, "household_income"), "Low household income", term),
         term = ifelse(str_detect(term, "Family size"), "Small family size", term),
         term = ifelse(str_detect(term, "nh_edulevel"), "Low neighborhood education level", term),
         term = ifelse(str_detect(term, "income_neighbourhood"), "Low neighborhood income", term),
         term = ifelse(str_detect(term, "Single"), "  - Single parent household", term),
         term = ifelse(str_detect(term, "Mother_age"), "Age mother", term),                              # changed
         term = ifelse(str_detect(term, "Father_age"), "Age father", term),                              # changed
         term = ifelse(str_detect(term, "class2"), "  - Densely populated (class 1-2)", term),
         term = ifelse(term == "number_children", "Low number of children in household", term),          # changed       
         term = ifelse(str_detect(term, "Country"), "  - Non-European country", term),
         term = ifelse(str_detect(term, "- Europe"), "  - Other European country", term)) %>%
  draai_rij_om(6, 7) %>%
  draai_rij_om(10, 11) %>%
  draai_rij_om(14, 15) %>%
  draai_rij_om(25, 26) %>%
  draai_rij_om(29, 30)


#correlaties
cor(imputatie_file$data[c(3:7, 11, 16)], use = "pairwise.complete.obs")
variable.names(imputatie_file$data)
cor(imputatie_file$data[c(1:2, 8:10, 12:15)], use = "pairwise.complete.obs", method = "spearman")


# VIF achterhalen
library(car)
pred <- model.matrix(binair ~ sex + leeftijd + Birth_driedeling + Birth_driedeling_mother + 
      Birth_driedeling_father + number_children + family_type2 + Mother_age + Father_age + 
      edulevel_mother2 + edulevel_father2 + Percentiles_household_income + 
      Urbanization_class + nh_edulevel_low + Low_income_neighbourhood, data = imputatie_file$data)

vif_values <- vif(glm(pred ~ ., data = imputatie_file$data, use = "pairwise.complete.obs"))
################################################################################
#### FOREST PLOT: banjaard vs controle
################################################################################

setwd("H:/Maxine/Forest plots")
pdf("Banjaard1004_20imp.pdf", height = 9, width = 14) # VERANDER titel van figuur
forestplot(x = combined %>% select(c(term, estimate_unadj, stat_unadj, p_unadj, estimate, statistic, pvalue)) %>% rbind(NA, .) %>% 
             rbind(c("Predictor", "Univariate OR (95% CI)", "t-statistic", "p-value", "Multivariate OR (95% CI)",          #changed
                     "t-statistic", "p-value"), .), 
           mean = combined$or %>% c(NA, NA, .),   
           lower = combined$lower %>% c(NA, NA, .),   
           upper = combined$upper %>% c(NA, NA, .),   
           align = c("l", "l", "l", "l", "l", "l", "c"),  # changed
           graph.pos = 7, 
           graphwidth = unit(60, "mm"), 
           boxsize = 0.3, 
           is.summary = c(T,F,T,rep(F,14),T,rep(F,14),T,F,T,rep(F, 15)),
           zero = 1, 
           xlog = T,
           txt_gp = fpTxtGp(xlab=gpar(ces = 0.9), label=gpar(ces = 0.8),title=gpar(ces = 1.1)),
           col = fpColors(lines = "black", box = "steelblue"),
           lwd.ci = 1.5, civertices = TRUE, ci.vertices.height = 0.05,
           clip = c(0.67, 1.83),
           xticks = seq(-.4,.6,.2),  
           grid = structure (exp(seq(-.4,.6,.2)), 
                             gp = gpar(lty=3, col = "black")),
           colgap = unit(5, "mm"),
           xlab = expression(" " %<-% "Lower odds ratio                               Higher odds ratio" %->% ""))
dev.off()

################################################################################
################################################################################
########################### ANALYSE 2 ##########################################
################################################################################
################################################################################

## clear global environment
rm(list = ls(all = TRUE))


################################################################################
#### HANDIGE FUNCTIES EN INLADEN DATAFILE ####
################################################################################

decimaal <- function(x, k) {
  rounded = round (x, k)
  formatted = sprintf(paste0("%0.", k, "f"), rounded)
  return(formatted)
}

maak_p <- function(p) {
  uitvoer_p <- ifelse(p<0.01, decimaal(p,3), paste0(decimaal(p,2), " "))
  uitvoer_p <- ifelse(decimaal(p,2)==0.05, decimaal (p,3), uitvoer_p)
  uitvoer_p <- ifelse(uitvoer_p=="0.000", "p<0.001", paste0("p=", uitvoer_p))
  return(uitvoer_p)
}
scale2 <- function(x, na.rm = TRUE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)

# inladen dataset
combi_covariaten16 <- readRDS("H:/Maxine/RDS files/combi_covariaten16.rds")

################################################################################
#### LOGISTISCHE REGRESSIE: banjaard vs controle
################################################################################

#!!!!!!! meedoen_nummers !!!!!!!!!

# meedoen_nummers <- combi_covariaten16 %>% filter(group == "A. Banjaard") %>% pull(case_control) %>% unique()
meedoen_nummers <- combi_covariaten16 %>% filter(group == "B. Youz") %>% pull(case_control) %>% unique()
# meedoen_nummers <- combi_covariaten16 %>% filter(group != "C. General population ") %>% pull(case_control) %>% unique()

#----------------------- veranderen naar 20 ----------------------------#
no_imput = 20 # AANPASSEN!

banjaard <- combi_covariaten16 %>% filter(case_control %in% meedoen_nummers) %>% 
  # mutate(binair = group == "A. Banjaard") %>% 
  mutate(binair = group == "B. Youz") %>% 
  select(binair, sex, leeftijd, Mother_age, Father_age, nh_edulevel_low, 
         number_children, family_type2, edulevel_mother2, edulevel_father2,
         Percentiles_household_income, Birth_driedeling, Birth_driedeling_mother, 
         Birth_driedeling_father, urbanization_class = urbanization_class, 
         Low_income_neighbourhood = median_income_postcode) %>% 
  mutate(Percentiles_household_income = -Percentiles_household_income,
         number_children = -number_children,
         Low_income_neighbourhood = - Low_income_neighbourhood,
         Urbanization_class = ifelse(urbanization_class %in% c("3", "4", "5"), "1", "2"),
         family_type2 = ifelse(family_type2 %in% c("  - institutional household", "other"), NA, family_type2),
         family_type2 = ifelse(str_detect(family_type2, "impossible"), NA, family_type2),
         edulevel_mother2 = ifelse(edulevel_mother2 == "  - Other", NA, edulevel_mother2), 
         edulevel_mother2 = factor(edulevel_mother2, levels = c("  - High", "  - Middle", "  - Low")),
         edulevel_father2 = ifelse(edulevel_father2 == "  - Other", NA, edulevel_father2), 
         edulevel_father2 = factor(edulevel_father2, levels = c("  - High", "  - Middle", "  - Low")),
         sex = ifelse(sex == "Vrouw", "1","2"),
         Birth_driedeling = factor(Birth_driedeling, exclude = "Onbekend"),
         Birth_driedeling_father = factor(Birth_driedeling_father, exclude = "Onbekend"),
         Birth_driedeling_mother = factor(Birth_driedeling_mother, exclude = "Onbekend"),
         Birth_driedeling = recode(Birth_driedeling, "Europa (exclusief Nederland)" = "  - Europe", "Buiten Europa" = "  - Country outside Europe", .default = "  - The Netherlands"),
         Birth_driedeling_mother = recode(Birth_driedeling_mother, "Europa (exclusief Nederland)" = "  - Europe", "Buiten Europa" = "  - Country outside Europe", .default = "  - The Netherlands"),
         Birth_driedeling_father = recode(Birth_driedeling_father, "Europa (exclusief Nederland)" = "  - Europe", "Buiten Europa" = "  - Country outside Europe", .default = "  - The Netherlands"),
  )

calc_beta <- function(model1){
  model1 %>% 
    purrr::map(\(x) modelsummary(x,output = "modelsummary_list", standardize = "basic")$tidy %>% 
                 tail(-1) %>% select(term, beta = estimate, se = std.error, statistic, p = p.value)) %>% 
    melt() %>% 
    group_by(term, variable) %>% 
    summarise(value = mean(value)) %>% 
    ungroup() %>% 
    spread(variable, value) %>% 
    mutate(pvalue = maak_p(p))
}

set.seed(1)

add_reference_rows <- function (db1, rownumber, header, ref_group){
  if (!is.na(ref_group)) {
    db1 <- add_row(db1, .before = rownumber, term = ref_group, estimate = "1.00 (Ref.)", 
                   or = 1 , lower = 1, upper = 1, estimate_unadj = "1.00 (Ref.)")
  }
  add_row(db1, .before = rownumber, term = header)
}

# adjusted model (multivariate)

adjusted <- with(mice(banjaard, method = "pmm", m = no_imput), # verander m = 5/10/25
                 glm(binair ~ sex + leeftijd + Birth_driedeling + Birth_driedeling_mother + 
                       Birth_driedeling_father + number_children + family_type2 + Mother_age + Father_age + 
                       edulevel_mother2 + edulevel_father2 + Percentiles_household_income + 
                       Urbanization_class + nh_edulevel_low + Low_income_neighbourhood, 
                     family = binomial())) %>% 
  .$analyses %>% 
  calc_beta() %>% 
  mutate(lower = exp(beta - 1.96 * se), upper = exp(beta + 1.96 * se), 
         statistic = decimaal(statistic, 3), or = exp(beta),
         estimate = paste0(decimaal(or, 2), " (", decimaal(lower, 2), "; ",          #changed
                           decimaal(upper, 2), ")")) %>% 
  select(term, estimate, or, lower, upper, statistic, pvalue) %>% 
  glimpse()

# unadjusted model (univariate)

imputatie_file <- mice(banjaard, m = no_imput)

unadjusted <- list(
  with(imputatie_file, glm(binair ~ sex, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ leeftijd, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ Birth_driedeling, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ Birth_driedeling_mother, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ Birth_driedeling_father, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ number_children, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ family_type2, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ Mother_age, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ Father_age, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ edulevel_mother2, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ edulevel_father2, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ Percentiles_household_income, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ Urbanization_class, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ nh_edulevel_low, family = binomial)) %>% .$analyses %>% calc_beta(), 
  with(imputatie_file, glm(binair ~ Low_income_neighbourhood, family = binomial)) %>% .$analyses %>% calc_beta()) %>% 
  purrr::map_df(rbind) %>% 
  mutate(lower = exp(beta - 1.96 * se), upper = exp(beta + 1.96 * se) , 
         statistic = decimaal(statistic, 3), or = exp(beta),
         estimate_unadj = paste0(decimaal(or, 2), " (", decimaal(lower, 2), "; ", 
                                 decimaal(upper, 2), ")")) %>% 
  select(term, estimate_unadj, stat_unadj = statistic, p_unadj = pvalue)

draai_rij_om <- function (df1, rij1, rij2){
  df1 %>% 
    mutate(new_row_number = case_when(row_number() == rij1 ~ rij2, 
                                      row_number() == rij2 ~ rij1, 
                                      TRUE ~ row_number())) %>% 
    arrange(new_row_number) %>% 
    select(-new_row_number)
}

combined <- unadjusted %>% 
  left_join(adjusted, by = join_by(term)) %>% 
  add_reference_rows(., rownumber = 18, header = "Urbanization class:", ref_group = "  - Sparsely populated (class 3-5)") %>% 
  add_reference_rows(., rownumber = 18, header = "Neighborhood domain:", ref_group = NA) %>% 
  add_reference_rows(., rownumber = 17, header = "Economic domain:", ref_group = NA) %>% 
  add_reference_rows(., rownumber = 15, header = "Education level father:", ref_group = "  - High") %>% 
  add_reference_rows(., rownumber = 13, header = "Education level mother:", ref_group = "  - High") %>% 
  add_reference_rows(., rownumber = 10, header = "Family type:", ref_group = "  - Dual parent household") %>% 
  add_reference_rows(., rownumber = 9, header = "Social and cultural domain:", ref_group = NA) %>% 
  add_reference_rows(., rownumber = 7, header = "Father's birth country:", ref_group = "  - The Netherlands") %>%
  add_reference_rows(., rownumber = 5, header = "Mother's birth country:", ref_group = "  - The Netherlands") %>%
  add_reference_rows(., rownumber = 3, header = "Child's birth country:", ref_group = "  - The Netherlands") %>%
  add_reference_rows(., rownumber = 1, header = "Demographic domain:", ref_group = NA) %>% 
  mutate(term = ifelse(term == "sex2", "Male sex", term),
         term = ifelse(term == "leeftijd", "Age", term),
         term = ifelse(str_detect(term, "- Low"), "  - Low", term),
         term = ifelse(str_detect(term, "Middle"), "  - Moderate", term),
         term = ifelse(str_detect(term, "household_income"), "Low household income", term),
         term = ifelse(str_detect(term, "Family size"), "Small family size", term),
         term = ifelse(str_detect(term, "nh_edulevel"), "Low neighborhood education level", term),
         term = ifelse(str_detect(term, "income_neighbourhood"), "Low neighborhood income", term),
         term = ifelse(str_detect(term, "Single"), "  - Single parent household", term),
         term = ifelse(str_detect(term, "Mother_age"), "Age mother", term),                              # changed
         term = ifelse(str_detect(term, "Father_age"), "Age father", term),                              # changed
         term = ifelse(str_detect(term, "class2"), "  - Densely populated (class 1-2)", term),
         term = ifelse(term == "number_children", "Low number of children in household", term),          # changed
         term = ifelse(str_detect(term, "Country"), "  - Non-European country", term),
         term = ifelse(str_detect(term, "- Europe"), "  - Other European country", term)) %>%
  draai_rij_om(6, 7) %>%
  draai_rij_om(10, 11) %>%
  draai_rij_om(14, 15) %>%
  draai_rij_om(25, 26) %>%
  draai_rij_om(29, 30)

################################################################################
#### FOREST PLOT: Youz vs controle
################################################################################

setwd("H:/Maxine/Forest plots")
pdf("Youz1004_20imp.pdf", height = 9, width = 14) # VERANDER titel van figuur
forestplot(x = combined %>% select(c(term, estimate_unadj, stat_unadj, p_unadj, estimate, statistic, pvalue)) %>% rbind(NA, .) %>% 
             rbind(c("Predictor", "Univariate OR (95% CI)", "t-statistic", "p-value", "Multivariate OR (95% CI)",          #changed
                     "t-statistic", "p-value"), .), 
           mean = combined$or %>% c(NA, NA, .),   
           lower = combined$lower %>% c(NA, NA, .),   
           upper = combined$upper %>% c(NA, NA, .),   
           align = c("l", "l", "l", "l", "l", "l", "c"),  # changed
           graph.pos = 7, 
           graphwidth = unit(60, "mm"), 
           boxsize = 0.3, 
           is.summary = c(T,F,T,rep(F,14),T,rep(F,14),T,F,T,rep(F, 15)),
           zero = 1, 
           xlog = T,
           txt_gp = fpTxtGp(xlab=gpar(ces = 0.9), label=gpar(ces = 0.8),title=gpar(ces = 1.1)),
           col = fpColors(lines = "black", box = "steelblue"),
           lwd.ci = 1.5, civertices = TRUE, ci.vertices.height = 0.05,
           clip = c(0.67, 1.83),
           xticks = seq(-.4,.6,.2),  
           grid = structure (exp(seq(-.4,.6,.2)), 
                             gp = gpar(lty=3, col = "black")),
           colgap = unit(5, "mm"),
           xlab = expression(" " %<-% "Lower odds ratio                               Higher odds ratio" %->% ""))
dev.off()
?forestplot
################################################################################
################################################################################
########################### ANALYSE 3 ##########################################
################################################################################
################################################################################

rm(list = ls(all = TRUE))

################################################################################
#### HANDIGE FUNCTIES EN INLADEN DATAFILE ####
################################################################################

decimaal <- function(x, k) {
  rounded = round (x, k)
  formatted = sprintf(paste0("%0.", k, "f"), rounded)
  return(formatted)
}

maak_p <- function(p) {
  uitvoer_p <- ifelse(p<0.01, decimaal(p,3), paste0(decimaal(p,2), " "))
  uitvoer_p <- ifelse(decimaal(p,2)==0.05, decimaal (p,3), uitvoer_p)
  uitvoer_p <- ifelse(uitvoer_p=="0.000", "p<0.001", paste0("p=", uitvoer_p))
  return(uitvoer_p)
}
scale2 <- function(x, na.rm = TRUE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)

# inladen dataset
combi_covariaten16 <- readRDS("H:/Maxine/RDS files/combi_covariaten16.rds")

################################################################################
#### LOGISTISCHE REGRESSIE: banjaard vs Youz
################################################################################

#!!!!!!! meedoen_nummers !!!!!!!!!

# meedoen_nummers <- combi_covariaten16 %>% filter(group == "A. Banjaard") %>% pull(case_control) %>% unique()
# meedoen_nummers <- combi_covariaten16 %>% filter(group == "B. Youz") %>% pull(case_control) %>% unique()
meedoen_nummers <- combi_covariaten16 %>% filter(group != "C. General population ") %>% pull(case_control) %>% unique()

#----------------------- veranderen naar 20 ----------------------------#
no_imput = 20 # AANPASSEN!

banjaard <- combi_covariaten16 %>% filter(case_control %in% meedoen_nummers) %>% 
  mutate(binair = group == "A. Banjaard") %>% 
  # mutate(binair = group == "B. Youz") %>% 
  select(binair, sex, leeftijd, Mother_age, Father_age, nh_edulevel_low, 
         number_children, family_type2, edulevel_mother2, edulevel_father2,
         Percentiles_household_income, Birth_driedeling, Birth_driedeling_mother, 
         Birth_driedeling_father, urbanization_class = urbanization_class, 
         Low_income_neighbourhood = median_income_postcode) %>% 
  mutate(Percentiles_household_income = -Percentiles_household_income,
         number_children = -number_children,
         Low_income_neighbourhood = - Low_income_neighbourhood,
         Urbanization_class = ifelse(urbanization_class %in% c("3", "4", "5"), "1", "2"),
         family_type2 = ifelse(family_type2 %in% c("  - institutional household", "other"), NA, family_type2),
         family_type2 = ifelse(str_detect(family_type2, "impossible"), NA, family_type2),
         edulevel_mother2 = ifelse(edulevel_mother2 == "  - Other", NA, edulevel_mother2), 
         edulevel_mother2 = factor(edulevel_mother2, levels = c("  - High", "  - Middle", "  - Low")),
         edulevel_father2 = ifelse(edulevel_father2 == "  - Other", NA, edulevel_father2), 
         edulevel_father2 = factor(edulevel_father2, levels = c("  - High", "  - Middle", "  - Low")),
         sex = ifelse(sex == "Vrouw", "1","2"),
         Birth_driedeling = factor(Birth_driedeling, exclude = "Onbekend"),
         Birth_driedeling_father = factor(Birth_driedeling_father, exclude = "Onbekend"),
         Birth_driedeling_mother = factor(Birth_driedeling_mother, exclude = "Onbekend"),
         Birth_driedeling = recode(Birth_driedeling, "Europa (exclusief Nederland)" = "  - Europe", "Buiten Europa" = "  - Country outside Europe", .default = "  - The Netherlands"),
         Birth_driedeling_mother = recode(Birth_driedeling_mother, "Europa (exclusief Nederland)" = "  - Europe", "Buiten Europa" = "  - Country outside Europe", .default = "  - The Netherlands"),
         Birth_driedeling_father = recode(Birth_driedeling_father, "Europa (exclusief Nederland)" = "  - Europe", "Buiten Europa" = "  - Country outside Europe", .default = "  - The Netherlands"),
  )

calc_beta <- function(model1){
  model1 %>% 
    purrr::map(\(x) modelsummary(x,output = "modelsummary_list", standardize = "basic")$tidy %>% 
                 tail(-1) %>% select(term, beta = estimate, se = std.error, statistic, p = p.value)) %>% 
    melt() %>% 
    group_by(term, variable) %>% 
    summarise(value = mean(value)) %>% 
    ungroup() %>% 
    spread(variable, value) %>% 
    mutate(pvalue = maak_p(p))
}

set.seed(1)

add_reference_rows <- function (db1, rownumber, header, ref_group){
  if (!is.na(ref_group)) {
    db1 <- add_row(db1, .before = rownumber, term = ref_group, estimate = "1.00 (Ref.)", 
                   or = 1 , lower = 1, upper = 1, estimate_unadj = "1.00 (Ref.)")
  }
  add_row(db1, .before = rownumber, term = header)
}

# adjusted model (multivariate)

adjusted <- with(mice(banjaard, method = "pmm", m = no_imput), # verander m = 5/10/25
                 glm(binair ~ sex + leeftijd + Birth_driedeling + Birth_driedeling_mother + 
                       Birth_driedeling_father + number_children + family_type2 + Mother_age + Father_age + 
                       edulevel_mother2 + edulevel_father2 + Percentiles_household_income + 
                       Urbanization_class + nh_edulevel_low + Low_income_neighbourhood, 
                     family = binomial())) %>% 
  .$analyses %>% 
  calc_beta() %>% 
  mutate(lower = exp(beta - 1.96 * se), upper = exp(beta + 1.96 * se), 
         statistic = decimaal(statistic, 3), or = exp(beta),
         estimate = paste0(decimaal(or, 2), " (", decimaal(lower, 2), "; ",                    #changed 
                           decimaal(upper, 2), ")")) %>% 
  select(term, estimate, or, lower, upper, statistic, pvalue) %>% 
  glimpse()

# unadjusted model (univariate)

imputatie_file <- mice(banjaard, m = no_imput)

unadjusted <- list(
  with(imputatie_file, glm(binair ~ sex, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ leeftijd, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ Birth_driedeling, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ Birth_driedeling_mother, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ Birth_driedeling_father, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ number_children, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ family_type2, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ Mother_age, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ Father_age, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ edulevel_mother2, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ edulevel_father2, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ Percentiles_household_income, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ Urbanization_class, family = binomial)) %>% .$analyses %>% calc_beta(),
  with(imputatie_file, glm(binair ~ nh_edulevel_low, family = binomial)) %>% .$analyses %>% calc_beta(), 
  with(imputatie_file, glm(binair ~ Low_income_neighbourhood, family = binomial)) %>% .$analyses %>% calc_beta()) %>% 
  purrr::map_df(rbind) %>% 
  mutate(lower = exp(beta - 1.96 * se), upper = exp(beta + 1.96 * se) , 
         statistic = decimaal(statistic, 3), or = exp(beta),
         estimate_unadj = paste0(decimaal(or, 2), " (", decimaal(lower, 2), "; ", 
                                 decimaal(upper, 2), ")")) %>% 
  select(term, estimate_unadj, stat_unadj = statistic, p_unadj = pvalue)

draai_rij_om <- function (df1, rij1, rij2){
  df1 %>% 
    mutate(new_row_number = case_when(row_number() == rij1 ~ rij2, 
                                      row_number() == rij2 ~ rij1, 
                                      TRUE ~ row_number())) %>% 
    arrange(new_row_number) %>% 
    select(-new_row_number)
}

combined <- unadjusted %>% 
  left_join(adjusted, by = join_by(term)) %>% 
  add_reference_rows(., rownumber = 18, header = "Urbanization class:", ref_group = "  - Sparsely populated (class 3-5)") %>% 
  add_reference_rows(., rownumber = 18, header = "Neighborhood domain:", ref_group = NA) %>% 
  add_reference_rows(., rownumber = 17, header = "Economic domain:", ref_group = NA) %>% 
  add_reference_rows(., rownumber = 15, header = "Education level father:", ref_group = "  - High") %>% 
  add_reference_rows(., rownumber = 13, header = "Education level mother:", ref_group = "  - High") %>% 
  add_reference_rows(., rownumber = 10, header = "Family type:", ref_group = "  - Dual parent household") %>% 
  add_reference_rows(., rownumber = 9, header = "Social and cultural domain:", ref_group = NA) %>% 
  add_reference_rows(., rownumber = 7, header = "Father's birth country:", ref_group = "  - The Netherlands") %>%
  add_reference_rows(., rownumber = 5, header = "Mother's birth country:", ref_group = "  - The Netherlands") %>%
  add_reference_rows(., rownumber = 3, header = "Child's birth country:", ref_group = "  - The Netherlands") %>%
  add_reference_rows(., rownumber = 1, header = "Demographic domain:", ref_group = NA) %>% 
  mutate(term = ifelse(term == "sex2", "Male sex", term),
         term = ifelse(term == "leeftijd", "Age", term),
         term = ifelse(str_detect(term, "- Low"), "  - Low", term),
         term = ifelse(str_detect(term, "Middle"), "  - Moderate", term),
         term = ifelse(str_detect(term, "household_income"), "Low household income", term),
         term = ifelse(str_detect(term, "Family size"), "Small family size", term),
         term = ifelse(str_detect(term, "nh_edulevel"), "Low neighborhood education level", term),
         term = ifelse(str_detect(term, "income_neighbourhood"), "Low neighborhood income", term),
         term = ifelse(str_detect(term, "Single"), "  - Single parent household", term),
         term = ifelse(str_detect(term, "Mother_age"), "Age mother", term),                              # changed
         term = ifelse(str_detect(term, "Father_age"), "Age father", term),                              # changed
         term = ifelse(str_detect(term, "class2"), "  - Densely populated (class 1-2)", term),
         term = ifelse(term == "number_children", "Low number of children in household", term),          # changed
         term = ifelse(str_detect(term, "Country"), "  - Non-European country", term),
         term = ifelse(str_detect(term, "- Europe"), "  - Other European country", term)) %>%
  draai_rij_om(6, 7) %>%
  draai_rij_om(10, 11) %>%
  draai_rij_om(14, 15) %>%
  draai_rij_om(25, 26) %>%
  draai_rij_om(29, 30)

################################################################################
#### FOREST PLOT: banjaard vs controle
################################################################################

setwd("H:/Maxine/Forest plots")
pdf("BanjaardYouz1004_20imp.pdf", height = 9, width = 14) # VERANDER titel van figuur
forestplot(x = combined %>% select(c(term, estimate_unadj, stat_unadj, p_unadj, estimate, statistic, pvalue)) %>% rbind(NA, .) %>% 
             rbind(c("Predictor", "Univariate OR (95% CI)", "t-statistic", "p-value", "Multivariate OR (95% CI)",          #changed
                     "t-statistic", "p-value"), .), 
           mean = combined$or %>% c(NA, NA, .),   
           lower = combined$lower %>% c(NA, NA, .),   
           upper = combined$upper %>% c(NA, NA, .),   
           align = c("l", "l", "l", "l", "l", "l", "c"),  # changed
           graph.pos = 7, 
           graphwidth = unit(60, "mm"), 
           boxsize = 0.3, 
           is.summary = c(T,F,T,rep(F,14),T,rep(F,14),T,F,T,rep(F, 15)),
           zero = 1, 
           xlog = T,
           txt_gp = fpTxtGp(xlab=gpar(ces = 0.9), label=gpar(ces = 0.8),title=gpar(ces = 1.1)),
           col = fpColors(lines = "black", box = "steelblue"),
           lwd.ci = 1.5, civertices = TRUE, ci.vertices.height = 0.05,
           clip = c(0.67, 1.83),
           xticks = seq(-.4,.6,.2),  
           grid = structure (exp(seq(-.4,.6,.2)), 
                             gp = gpar(lty=3, col = "black")),
           colgap = unit(5, "mm"),
           xlab = expression(" " %<-% "Lower odds ratio                               Higher odds ratio" %->% ""))
dev.off()