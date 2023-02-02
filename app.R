library(shiny)
library(ggvis)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyverse)
library(DT)
library(plotly)
library(writexl)
library(shinydashboard)
library(ggforce)
library(data.table)
library(ids)
library(ineq)
library(gtExtras)
library(shinyFiles)
library(acid)
library(readr)
library(shinybusy)
library(hutils)
library(rhandsontable)


#setwd("/Users/sjifradeleeuw/Library/Mobile Documents/com~apple~CloudDocs/Sandbox3_v2")


#df = df[sample(1:nrow(df), nrow(df)/2, replace = F),]
#write_rds(df, "raming_data_test.rds")

#write_rds(df, "raming_data.rds")

#sub1 = subset(df, tot_rend_50 < 0)
#sub2 = subset(df, tot_rend_50 >= 0)

#sub1 = sub1 %>%
#  dplyr::select(.,-c("factor")) %>%
#  group_by(group) %>%
#  summarise_each(funs(mean)) %>%
#  full_join(., aggregate(factor ~ group, data = sub1, FUN = sum), by = "group")

#df = rbind(sub1, sub2)
#write_rds(df, "raming_data_short.rds")

#df = rbind(sub1, sub2)
#write_rds(df, "raming_data_final.rds")

mld = 1000000000
mln = 1000000

# SET BASE MULTIPLIERS

risico = 2
sd = 0.2
sd_rend = 0.5 * risico * sd

# FUNCTIONS

# percentage belasting tov aanwas
calc_percentage = function(deel, totaal) {
  if (totaal > 0) {
    perc = (deel / totaal) * 100
  } else {
    perc = 0
  }
  
  return(perc)
}

# vervang door nul
replace_by_zero = function(x) {
  x[x < 0] = 0
  return(x)
}

# negatie %in%
`%notin%` = Negate(`%in%`)

# function to draw value from normal distribution
gen_value = function(mean, sd = 0.2, n = 1) {
  x = rnorm(n = n, mean = mean, sd = sd)
  return(x)
}

# update value last year
update_value = function(current_value, perc_change) {
  new_value = current_value + (current_value * (perc_change / 100))
  return(new_value)
}

# function to generate history for given input
gen_history = function(row, sd_rend, crisis = "nee") {
  data_list = list()
  data_list_2 = list()
  
  if (row$risico == 0) {
    sd = 0
    sd_rend = 0
  }
  
  if (row$spaargeld < 0 | is.na(row$spaargeld)) {
    row$spaargeld = 0
  }
  if (row$finproduct < 0 |
      is.na(row$finproduct)) {
    row$finproduct = 0
  }
  if (row$restbezit < 0 | is.na(row$restbezit)) {
    row$restbezit = 0
  }
  if (row$schuld < 0 | is.na(row$schuld)) {
    row$schuld = 0
  }
  
  data_list[[1]] = row
  
  for (i in c(2:20)) {
    jaar = c(2026:2045)[i]
    vorig_jaar = data_list[[i - 1]]
    
    data_list[[i]] = data.frame(
      id = row$id,
      jaar = jaar,
      omschrijving = vorig_jaar$omschrijving,
      risico = vorig_jaar$risico,
      spaargeld = update_value(vorig_jaar$spaargeld, vorig_jaar$spaargeld_rendperc),
      finproduct = update_value(vorig_jaar$finproduct, vorig_jaar$finproduct_rendperc),
      restbezit = update_value(vorig_jaar$restbezit, vorig_jaar$restbezit_rendperc),
      schuld = vorig_jaar$schuld,
      spaargeld_rendperc = row$spaargeld_rendperc,
      finproduct_rendperc = gen_value(row$finproduct_rendperc, sd_rend, n = 1),
      restbezit_rendperc = gen_value(row$restbezit_rendperc, sd, n = 1),
      schuld_rendperc = row$schuld_rendperc
    )
    
  }
  
  data = do.call(rbind, data_list) %>%
    mutate(vermogen = spaargeld + finproduct + restbezit - schuld) %>%
    mutate(spaargeld_aanwas = spaargeld * (spaargeld_rendperc / 100)) %>%
    mutate(finproduct_aanwas = finproduct * (finproduct_rendperc / 100)) %>%
    mutate(restbezit_aanwas = restbezit * (restbezit_rendperc / 100)) %>%
    mutate(schuld_aanwas = schuld * (schuld_rendperc / 100))
  
  if (crisis == "ja") {
    startperc = data$finproduct_rendperc[which(data$jaar == 2035)] / 100
    startsaldo = data$finproduct_aanwas[which(data$jaar == 2035)]
    
    data$finproduct_aanwas[data$jaar == 2036] = startsaldo - 50 * startsaldo *
      startperc
    data$finproduct_aanwas[data$jaar == 2037] = startsaldo - 45 * startsaldo *
      startperc
    data$finproduct_aanwas[data$jaar == 2038] = startsaldo - 30 * startsaldo *
      startperc
    data$finproduct_aanwas[data$jaar == 2039] = startsaldo - 15 * startsaldo *
      startperc
  }
  
  # VOEG TOE AANTAL JAREN CRISIS
  
  data = data %>%
    mutate(aanwas = spaargeld_aanwas + finproduct_aanwas + restbezit_aanwas - schuld_aanwas) %>%
    mutate_at(vars(-id,-omschrijving), funs(round(., 1))) %>%
    arrange(id, jaar)
  
  data$aanwas = (data$spaargeld * (data$spaargeld_rendperc / 100)) + 
    (data$finproduct * (data$finproduct_rendperc / 100)) + 
    (data$restbezit * (data$restbezit_rendperc / 100)) - 
    (data$schuld * (data$schuld_rendperc / 100))
  
  return(data)
}

# function to calculate carry back/forward losses

verreken_verlies = function(data,
                            hvi,
                            cf = 0,
                            cb = 0,
                            drempel = 0) {
  start = min(data$jaar)
  end = max(data$jaar)
  
  drempel = -1 * drempel
  
  data$grondslag = case_when(
    data$aanwas < drempel ~ data$aanwas + abs(drempel),
    data$aanwas >= drempel & data$aanwas <= hvi ~ 0,
    data$aanwas > hvi ~ data$aanwas - hvi
  )
  
  data$grondslag_max = data$grondslag
  data$grondslag_voor_vv = data$grondslag
  
  temp = list()
  # voor elk individu
  for (i in 1:length(unique(data$id))) {
    # subset persoon
    dat = subset(data, id == unique(data$id)[i])
    # verlies verrekenings variabelen
    dat$verlies = dat$grondslag
    # als winst in verliesjaar, stel verrekenbaar verlies gelijk aan nul
    dat$verlies[dat$verlies > 0] = 0
    # absoluteer verlies
    dat$verlies = abs(dat$verlies)
    
    # voor elk belastingjaar
    for (y in c(1:length(c(start:end)))) {
      belastingjaar = c(start:end)[y]
      verlies_periode = c(belastingjaar - cf, belastingjaar + cb)
      
      if (verlies_periode[1] < 2026) {
        verlies_periode[1] = 2026
      }
      if (verlies_periode[2] > 2045) {
        verlies_periode[2] = 2045
      }
      
      
      
      # als positieve grondslag belastingjaar
      if (dat$grondslag[which(dat$jaar == belastingjaar)] > 0) {
        # loop over elk verliesjaar
        for (verliesjaar in verlies_periode[1]:verlies_periode[2]) {
          # als verlies verliesjaar groter is dan grondslag belastingjaar, stel vv gelijk aan grondslag
          if (dat$verlies[which(dat$jaar == verliesjaar)] > dat$grondslag[which(dat$jaar == belastingjaar)]) {
            verlies_temp = dat$grondslag[which(dat$jaar == belastingjaar)]
            
          } else {
            verlies_temp = dat$verlies[which(dat$jaar == verliesjaar)]
            
          }
          
          # trek verrekenbaar  verlies af van grondslag belastingjaar
          dat$grondslag[dat$jaar == belastingjaar] = dat$grondslag[which(dat$jaar == belastingjaar)] - verlies_temp
          
          # tel vermindering op bij verlies verliesjaar
          dat$grondslag[dat$jaar == verliesjaar] = dat$grondslag[which(dat$jaar == verliesjaar)] + verlies_temp
          
          # vereffen verliessaldo
          dat$verlies[dat$jaar == verliesjaar] = dat$verlies[dat$jaar == verliesjaar] - verlies_temp
          
        }
      }
      
      # als positieve grondslag belastingjaar
      if (dat$grondslag[which(dat$jaar == belastingjaar)] > 0) {
        # loop over elk verliesjaar
        for (verliesjaar in start:end) {
          # als verlies verliesjaar groter is dan grondslag belastingjaar, stel vv gelijk aan grondslag
          if (dat$verlies[which(dat$jaar == verliesjaar)] > dat$grondslag[which(dat$jaar == belastingjaar)]) {
            verlies_temp = dat$grondslag_max[which(dat$jaar == belastingjaar)]
            
          } else {
            verlies_temp = dat$verlies[which(dat$jaar == verliesjaar)]
            
          }
          
          # trek verrekenbaar  verlies af van grondslag belastingjaar
          dat$grondslag_max[dat$jaar == belastingjaar] = dat$grondslag_max[which(dat$jaar == belastingjaar)] - verlies_temp
          
          # tel vermindering op bij verlies verliesjaar
          dat$grondslag_max[dat$jaar == verliesjaar] = dat$grondslag_max[which(dat$jaar == verliesjaar)] + verlies_temp
          
          # vereffen verliessaldo
          dat$verlies[dat$jaar == verliesjaar] = dat$verlies[dat$jaar == verliesjaar] - verlies_temp
          
        }
      }
      
      
      
    }
    
    temp[[length(temp) + 1]] = dat
    
  }
  
  data = do.call(rbind, temp)
  data$grondslag[data$grondslag < 0] = 0
  data$grondslag_voor_vv[data$grondslag_voor_vv < 0] = 0
  data$grondslag_max[data$grondslag_max < 0] = 0
  
  return(data)
  
}

replace_pos_by_zero = function(x) {
  x[x > 0] = 0
  return(x)
}

replace_by_zero = function(x) {
  x[x < 0] = 0
  return(x)
}

# functie om belasting te bepalen voor een bepaalde grondslag
bepaal_belasting = function(grondslag,
                            schijf_2 = NA,
                            schijf_3 = NA,
                            tarief_1 = 34,
                            tarief_2 = NA,
                            tarief_3 = NA) {
  if (grondslag > 0) {
    # Schijf 3
    if (!is.na(schijf_3)) {
      schijf_3 = grondslag - schijf_3
    } else {
      schijf_3 = 0
    }
    if (schijf_3 < 0) {
      schijf_3 = 0
    }
    if (!is.na(tarief_3)) {
      belasting_3 = schijf_3 * (tarief_3 / 100)
    } else {
      belasting_3 = NA
    }
    
    # Schijf 2
    if (!is.na(schijf_2)) {
      schijf_2 = grondslag - schijf_3 - schijf_2
    } else {
      schijf_2 = 0
    }
    if (schijf_2 < 0) {
      schijf_2 = 0
    }
    if (!is.na(tarief_2)) {
      belasting_2 = schijf_2 * (tarief_2 / 100)
    } else {
      belasting_2 = NA
    }
    
    # Schijf 1
    schijf_1 = grondslag - schijf_3 - schijf_2
    if (schijf_1 < 0) {
      schijf_1 = 0
    }
    if (!is.na(tarief_1)) {
      belasting_1 = schijf_1 * (tarief_1 / 100)
    } else {
      belasting_1 = NA
    }
    
    out = data.frame(
      schijf = c(1:3),
      aanwas = c(schijf_1, schijf_2, schijf_3),
      belasting = c(belasting_1, belasting_2, belasting_3)
    )
  } else {
    out = data.frame(
      schijf = c(1:3),
      aanwas = c(0, 0, 0),
      belasting = c(0, 0, 0)
    )
  }
  
  return(out)
}

bepaal_belasting_totaal = function(grondslag,
                                   schijf_2 = NA,
                                   schijf_3 = NA,
                                   tarief_1 = 34,
                                   tarief_2 = NA,
                                   tarief_3 = NA) {
  out = sum(
    bepaal_belasting(
      grondslag,
      schijf_2 = NA,
      schijf_3 = NA,
      tarief_1 = 34,
      tarief_2 = NA,
      tarief_3 = NA
    )$belasting,
    na.rm = T
  )
  
  return(out)
  
}

# functie om getal om te zetten in bedrag in €
number_to_money = function(number) {
  options(scipen = 999)
  number = round(number, digits = 2)
  if (number >= 0) {
    number = paste0("€", number)
  } else {
    number = paste0("-€", abs(number))
  }
  return(number)
}

# functie om getal om te zetten naar percentage
percentify = function(number) {
  number = paste0(round(number, digits = 2), "%")
  return(number)
}

# functie om aanwas te berekenen gegeven vermogen en rendement
bereken_aanwas = function(vermogen, rendement) {
  if (rendement != 0) {
    aanwas =  vermogen * (rendement / 100)
  } else {
    aanwas = 0
  }
  return(aanwas)
}


# functie om variant door te rekenen voor casus
gen_combi = function(dat_variant, dat_case, jaar = "alle jaren") {
  #dat_variant = variant_data
  #dat_case = case_data
  # verlies verrekening
  dat_case_verlies = verreken_verlies(
    data = dat_case,
    hvi = dat_variant$hvi,
    cf = dat_variant$cf,
    cb = dat_variant$cb
  )
  
  if (jaar != "alle jaren") {
    dat_case_verlies = subset(dat_case_verlies, jaar == as.numeric(jaar))
  }
  
  bepaal_belasting_totaal_adjusted = function(x) {
    bepaal_belasting_totaal(
      x,
      schijf_2 = dat_variant$schijf_2,
      schijf_3 = dat_variant$schijf_3,
      tarief_1 = dat_variant$tarief_1,
      tarief_2 = dat_variant$tarief_2,
      tarief_3 = dat_variant$tarief_3
    )
  }
  
  belasting = sum(
    sapply(dat_case_verlies$grondslag, FUN = bepaal_belasting_totaal_adjusted),
    na.rm = T
  )
  
  # aanwas
  
  
  aanwas = dat_case_verlies %>%
    mutate(aanwas = replace_by_zero(.$aanwas)) %>%
    dplyr::select(., "aanwas") %>%
    sum() %>%
    round(., 0)
  
  # percentage belasting
  belasting_perc = calc_percentage(belasting, sum(dat_case$aanwas, na.rm = T))
  
  # percentage verrekend verlies tov totaal verlies
  verlies = dat_case_verlies %>%
    mutate(aanwas = case_when(aanwas > 0 ~ 0,
                              aanwas <= 0 ~ aanwas)) %>%
    dplyr::select(., "aanwas") %>%
    sum() %>%
    abs()
  
  vv = abs(sum((
    dat_case_verlies$grondslag_voor_vv - dat_case_verlies$grondslag
  )
  ))
  verrekend_verlies_perc = calc_percentage(vv, verlies)
  
  # percentage grondslag tov aanwas
  grondslag_perc = calc_percentage(sum(dat_case_verlies$grondslag), aanwas)
  
  
  
  # berekeningen
  temp_case = data.frame(
    case_name = dat_case$omschrijving[1],
    variant_name = dat_variant$variant,
    
    # belastingplichtige
    
    risico = round(mean(dat_case$risico, na.rm = T), 0),
    belasting = round(belasting, 0),
    belasting_perc = round(belasting_perc, 0),
    verlies = round(verlies, 0),
    verrekend_verlies = round(vv, 0),
    verrekend_verlies_perc = round(verrekend_verlies_perc, 0),
    vermogen = round(sum(dat_case$vermogen, na.rm = T), 0),
    aanwas = aanwas,
    grondslag = round(sum(dat_case_verlies$grondslag), 0),
    grondslag_perc = round(grondslag_perc, 2),
    spaargeld = round(sum(dat_case$spaargeld, na.rm = T), 0),
    finproduct = round(sum(dat_case$finproduct, na.rm = T), 0),
    restbezit = round(sum(dat_case$restbezit, na.rm = T), 0),
    schuld = round(sum(dat_case$schuld, na.rm = T), 0),
    spaargeld_rendperc = round(mean(dat_case$spaargeld_rendperc, na.rm = T), 0),
    finproduct_rendperc = round(mean(dat_case$finproduct_rendperc, na.rm = T), 0),
    restbezit_rendperc = round(mean(dat_case$restbezit_rendperc, na.rm = T), 0),
    schuld_rendperc = round(mean(dat_case$schuld_rendperc, na.rm = T), 0),
    
    # variant
    hvi = dat_variant$hvi,
    verlies_drempel = dat_variant$verlies_drempel,
    cf = dat_variant$cf,
    cb = dat_variant$cb,
    schijf_2 = dat_variant$schijf_2,
    schijf_3 = dat_variant$schijf_3,
    tarief_1 = dat_variant$tarief_1,
    tarief_2 = dat_variant$tarief_2,
    tarief_3 = dat_variant$tarief_3
  )
  
  return(temp_case)
  
}

# DATA

# case data
id = 1
jaar = 2026
omschrijving = "Jan Modaal"
risico = 2

spaargeld = gen_value(42300, sd)
finproduct = gen_value(7000, sd)

restbezit = 0
schuld = gen_value(12800, sd)

spaargeld_rendperc = gen_value(0.36, sd)

finproduct_rendperc = gen_value(6.17, sd_rend)

restbezit_rendperc = gen_value(6.17, sd)

schuld_rendperc = gen_value(2.57, sd)

case_data_colnames_original = c(
  "omschrijving",
  "risico",
  "jaar",
  "vermogen",
  "aanwas",
  "spaargeld",
  "finproduct",
  "restbezit",
  "schuld",
  "spaargeld_rendperc",
  "finproduct_rendperc",
  "restbezit_rendperc",
  "schuld_rendperc"
)

case_data_colnames_print = c(
  "omschrijving",
  "risico",
  "jaar",
  "vermogen",
  "aanwas",
  "spaargeld",
  "financiële producten",
  "overig bezit",
  "schuld",
  "rendement spaargeld (%)",
  "rendement financiële producten (%)",
  "rendement overig bezit (%)",
  "rendement schuld (%)"
)

case_data = gen_history(
  data.frame(
    id = id,
    jaar = jaar,
    omschrijving = omschrijving,
    risico = risico,
    spaargeld = spaargeld,
    finproduct = finproduct,
    restbezit = restbezit,
    schuld = schuld,
    spaargeld_rendperc = spaargeld_rendperc,
    finproduct_rendperc = finproduct_rendperc,
    restbezit_rendperc = restbezit_rendperc,
    schuld_rendperc = schuld_rendperc
  ),
  sd_rend = sd_rend
)

# varianten data
colnames_variant_original = c(
  "variant",
  "hvi",
  "verlies_drempel",
  "cf",
  "cb",
  "schijf_2",
  "schijf_3",
  "tarief_1",
  "tarief_2",
  "tarief_3"
)

colnames_variant_print = c("variant",
                           "hvi",
                           "verlies drempel",
                           "CF",
                           "CB",
                           "S2 €",
                           "S3 €",
                           "T1 %",
                           "T2 %",
                           "T3 %")

variant_data = data.frame(
  variant = "Variant",
  hvi = 1000,
  verlies_drempel = 1000,
  cf = 9,
  cb = 1,
  schijf_2 = as.numeric(NA),
  schijf_3 = as.numeric(NA),
  tarief_1 = 34,
  tarief_2 = as.numeric(NA),
  tarief_3 = as.numeric(NA)
)

variant_data_macro_input = data.frame(
  variant = "Variant",
  data = "Raming",
  hvi = 1000,
  verlies_drempel = 1000,
  cf = 9,
  cb = 1,
  schijf_2 = as.numeric(NA),
  schijf_3 = as.numeric(NA),
  tarief_1 = 34,
  tarief_2 = as.numeric(NA),
  tarief_3 = as.numeric(NA)
)



# variant effect data
variant_case_effects = gen_combi(dat_variant = variant_data, dat_case = case_data)

colnames_variant_population_original = c(
  "variant",
  "budget",
  "gini_budget",
  "gini_grondslag",
  "gini_belasting",
  "verlies_belasting",
  "hvi",
  "verlies_drempel",
  "cf",
  "cb",
  "schijf_2",
  "schijf_3",
  "tarief_1",
  "tarief_2",
  "tarief_3"
)

colnames_variant_population_print = c(
  "variant",
  "budgettaire opbrengst",
  "budget stabiliteit",
  "grondslag ongelijkheid",
  "belasting ongelijkheid",
  "opbrengst door onverrekend verlies (mld.)",
  "hvi",
  "verlies drempel",
  "CF",
  "CB",
  "S2 €",
  "S3 €",
  "T1 %",
  "T2 %",
  "T3 %"
)
# SET BASE MULTIPLIERS

risico = 2
sd = 0.2
sd_rend = 0.5 * risico * sd

# FUNCTIONS

# percentage belasting tov aanwas
calc_percentage = function(deel, totaal) {
  if (totaal > 0) {
    perc = (deel / totaal) * 100
  } else {
    perc = 0
  }
  
  return(perc)
}

# vervang door nul
replace_by_zero = function(x) {
  x[x < 0] = 0
  return(x)
}

# negatie %in%
`%notin%` = Negate(`%in%`)

# function to draw value from normal distribution
gen_value = function(mean, sd = 0.2, n = 1) {
  x = rnorm(n = n, mean = mean, sd = sd)
  return(x)
}

# update value last year
update_value = function(current_value, perc_change) {
  new_value = current_value + (current_value * (perc_change / 100))
  return(new_value)
}

# function to generate history for given input
gen_history = function(row, sd_rend, crisis = "nee") {
  data_list = list()
  data_list_2 = list()
  
  if (row$risico == 0) {
    sd = 0
    sd_rend = 0
  }
  
  if (row$spaargeld < 0 | is.na(row$spaargeld)) {
    row$spaargeld = 0
  }
  if (row$finproduct < 0 |
      is.na(row$finproduct)) {
    row$finproduct = 0
  }
  if (row$restbezit < 0 | is.na(row$restbezit)) {
    row$restbezit = 0
  }
  if (row$schuld < 0 | is.na(row$schuld)) {
    row$schuld = 0
  }
  
  data_list[[1]] = row
  
  for (i in c(2:20)) {
    jaar = c(2026:2045)[i]
    vorig_jaar = data_list[[i - 1]]
    
    data_list[[i]] = data.frame(
      id = row$id,
      jaar = jaar,
      omschrijving = vorig_jaar$omschrijving,
      risico = vorig_jaar$risico,
      spaargeld = update_value(vorig_jaar$spaargeld, vorig_jaar$spaargeld_rendperc),
      finproduct = update_value(vorig_jaar$finproduct, vorig_jaar$finproduct_rendperc),
      restbezit = update_value(vorig_jaar$restbezit, vorig_jaar$restbezit_rendperc),
      schuld = vorig_jaar$schuld,
      spaargeld_rendperc = row$spaargeld_rendperc,
      finproduct_rendperc = gen_value(row$finproduct_rendperc, sd_rend, n = 1),
      restbezit_rendperc = gen_value(row$restbezit_rendperc, sd, n = 1),
      schuld_rendperc = row$schuld_rendperc
    )
    
  }
  
  data = do.call(rbind, data_list) %>%
    mutate(vermogen = spaargeld + finproduct + restbezit - schuld) %>%
    mutate(spaargeld_aanwas = spaargeld * (spaargeld_rendperc / 100)) %>%
    mutate(finproduct_aanwas = finproduct * (finproduct_rendperc / 100)) %>%
    mutate(restbezit_aanwas = restbezit * (restbezit_rendperc / 100)) %>%
    mutate(schuld_aanwas = schuld * (schuld_rendperc / 100))
  
  if (crisis == "ja") {
    startperc = data$finproduct_rendperc[which(data$jaar == 2035)] / 100
    startsaldo = data$finproduct_aanwas[which(data$jaar == 2035)]
    
    data$finproduct_aanwas[data$jaar == 2036] = startsaldo - 50 * startsaldo *
      startperc
    data$finproduct_aanwas[data$jaar == 2037] = startsaldo - 45 * startsaldo *
      startperc
    data$finproduct_aanwas[data$jaar == 2038] = startsaldo - 30 * startsaldo *
      startperc
    data$finproduct_aanwas[data$jaar == 2039] = startsaldo - 15 * startsaldo *
      startperc
  }
  
  # VOEG TOE AANTAL JAREN CRISIS
  
  data = data %>%
    mutate(aanwas = spaargeld_aanwas + finproduct_aanwas + restbezit_aanwas - schuld_aanwas) %>%
    mutate_at(vars(-id,-omschrijving), funs(round(., 1))) %>%
    arrange(id, jaar)
  
  return(data)
}

# function to calculate carry back/forward losses

verreken_verlies = function(data,
                            hvi,
                            cf = 0,
                            cb = 0,
                            drempel = 0) {
  start = min(data$jaar)
  end = max(data$jaar)
  
  drempel = -1 * drempel
  
  data$grondslag = case_when(
    data$aanwas < drempel ~ data$aanwas + abs(drempel),
    data$aanwas >= drempel & data$aanwas <= hvi ~ 0,
    data$aanwas > hvi ~ data$aanwas - hvi
  )
  
  data$grondslag_max = data$grondslag
  data$grondslag_voor_vv = data$grondslag
  
  temp = list()
  # voor elk individu
  for (i in 1:length(unique(data$id))) {
    # subset persoon
    dat = subset(data, id == unique(data$id)[i])
    # verlies verrekenings variabelen
    dat$verlies = dat$grondslag
    # als winst in verliesjaar, stel verrekenbaar verlies gelijk aan nul
    dat$verlies[dat$verlies > 0] = 0
    # absoluteer verlies
    dat$verlies = abs(dat$verlies)
    
    # voor elk belastingjaar
    for (y in c(1:length(c(start:end)))) {
      belastingjaar = c(start:end)[y]
      verlies_periode = c(belastingjaar - cf, belastingjaar + cb)
      
      if (verlies_periode[1] < 2026) {
        verlies_periode[1] = 2026
      }
      if (verlies_periode[2] > 2045) {
        verlies_periode[2] = 2045
      }
      
      
      
      # als positieve grondslag belastingjaar
      if (dat$grondslag[which(dat$jaar == belastingjaar)] > 0) {
        # loop over elk verliesjaar
        for (verliesjaar in verlies_periode[1]:verlies_periode[2]) {
          # als verlies verliesjaar groter is dan grondslag belastingjaar, stel vv gelijk aan grondslag
          if (dat$verlies[which(dat$jaar == verliesjaar)] > dat$grondslag[which(dat$jaar == belastingjaar)]) {
            verlies_temp = dat$grondslag[which(dat$jaar == belastingjaar)]
            
          } else {
            verlies_temp = dat$verlies[which(dat$jaar == verliesjaar)]
            
          }
          
          # trek verrekenbaar  verlies af van grondslag belastingjaar
          dat$grondslag[dat$jaar == belastingjaar] = dat$grondslag[which(dat$jaar == belastingjaar)] - verlies_temp
          
          # tel vermindering op bij verlies verliesjaar
          dat$grondslag[dat$jaar == verliesjaar] = dat$grondslag[which(dat$jaar == verliesjaar)] + verlies_temp
          
          # vereffen verliessaldo
          dat$verlies[dat$jaar == verliesjaar] = dat$verlies[dat$jaar == verliesjaar] - verlies_temp
          
        }
      }
      
      # als positieve grondslag belastingjaar
      if (dat$grondslag[which(dat$jaar == belastingjaar)] > 0) {
        # loop over elk verliesjaar
        for (verliesjaar in start:end) {
          # als verlies verliesjaar groter is dan grondslag belastingjaar, stel vv gelijk aan grondslag
          if (dat$verlies[which(dat$jaar == verliesjaar)] > dat$grondslag[which(dat$jaar == belastingjaar)]) {
            verlies_temp = dat$grondslag_max[which(dat$jaar == belastingjaar)]
            
          } else {
            verlies_temp = dat$verlies[which(dat$jaar == verliesjaar)]
            
          }
          
          # trek verrekenbaar  verlies af van grondslag belastingjaar
          dat$grondslag_max[dat$jaar == belastingjaar] = dat$grondslag_max[which(dat$jaar == belastingjaar)] - verlies_temp
          
          # tel vermindering op bij verlies verliesjaar
          dat$grondslag_max[dat$jaar == verliesjaar] = dat$grondslag_max[which(dat$jaar == verliesjaar)] + verlies_temp
          
          # vereffen verliessaldo
          dat$verlies[dat$jaar == verliesjaar] = dat$verlies[dat$jaar == verliesjaar] - verlies_temp
          
        }
      }
      
      
      
    }
    
    temp[[length(temp) + 1]] = dat
    
  }
  
  data = do.call(rbind, temp)
  data$grondslag[data$grondslag < 0] = 0
  data$grondslag_voor_vv[data$grondslag_voor_vv < 0] = 0
  data$grondslag_max[data$grondslag_max < 0] = 0
  
  return(data)
  
}

replace_pos_by_zero = function(x) {
  x[x > 0] = 0
  return(x)
}

replace_by_zero = function(x) {
  x[x < 0] = 0
  return(x)
}

# functie om belasting te bepalen voor een bepaalde grondslag
bepaal_belasting = function(grondslag,
                            schijf_2 = NA,
                            schijf_3 = NA,
                            tarief_1 = 34,
                            tarief_2 = NA,
                            tarief_3 = NA) {
  if (grondslag > 0) {
    # Schijf 3
    if (!is.na(schijf_3)) {
      schijf_3 = grondslag - schijf_3
    } else {
      schijf_3 = 0
    }
    if (schijf_3 < 0) {
      schijf_3 = 0
    }
    if (!is.na(tarief_3)) {
      belasting_3 = schijf_3 * (tarief_3 / 100)
    } else {
      belasting_3 = NA
    }
    
    # Schijf 2
    if (!is.na(schijf_2)) {
      schijf_2 = grondslag - schijf_3 - schijf_2
    } else {
      schijf_2 = 0
    }
    if (schijf_2 < 0) {
      schijf_2 = 0
    }
    if (!is.na(tarief_2)) {
      belasting_2 = schijf_2 * (tarief_2 / 100)
    } else {
      belasting_2 = NA
    }
    
    # Schijf 1
    schijf_1 = grondslag - schijf_3 - schijf_2
    if (schijf_1 < 0) {
      schijf_1 = 0
    }
    if (!is.na(tarief_1)) {
      belasting_1 = schijf_1 * (tarief_1 / 100)
    } else {
      belasting_1 = NA
    }
    
    out = data.frame(
      schijf = c(1:3),
      aanwas = c(schijf_1, schijf_2, schijf_3),
      belasting = c(belasting_1, belasting_2, belasting_3)
    )
  } else {
    out = data.frame(
      schijf = c(1:3),
      aanwas = c(0, 0, 0),
      belasting = c(0, 0, 0)
    )
  }
  
  return(out)
}

bepaal_belasting_totaal = function(grondslag,
                                   schijf_2 = NA,
                                   schijf_3 = NA,
                                   tarief_1 = 34,
                                   tarief_2 = NA,
                                   tarief_3 = NA) {
  out = sum(
    bepaal_belasting(
      grondslag,
      schijf_2 = NA,
      schijf_3 = NA,
      tarief_1 = 34,
      tarief_2 = NA,
      tarief_3 = NA
    )$belasting,
    na.rm = T
  )
  
  return(out)
  
}

# functie om getal om te zetten in bedrag in €
number_to_money = function(number) {
  options(scipen = 999)
  number = round(number, digits = 2)
  if (number >= 0) {
    number = paste0("€", number)
  } else {
    number = paste0("-€", abs(number))
  }
  return(number)
}

# functie om getal om te zetten naar percentage
percentify = function(number) {
  number = paste0(round(number, digits = 2), "%")
  return(number)
}

# functie om aanwas te berekenen gegeven vermogen en rendement
bereken_aanwas = function(vermogen, rendement) {
  if (rendement != 0) {
    aanwas =  vermogen * (rendement / 100)
  } else {
    aanwas = 0
  }
  return(aanwas)
}


# functie om variant door te rekenen voor casus
gen_combi = function(dat_variant, dat_case, jaar = "alle jaren") {
  #dat_variant = variant_data
  #dat_case = case_data
  # verlies verrekening
  dat_case_verlies = verreken_verlies(
    data = dat_case,
    hvi = dat_variant$hvi,
    cf = dat_variant$cf,
    cb = dat_variant$cb
  )
  
  if (jaar != "alle jaren") {
    dat_case_verlies = subset(dat_case_verlies, jaar == as.numeric(jaar))
  }
  
  bepaal_belasting_totaal_adjusted = function(x) {
    bepaal_belasting_totaal(
      x,
      schijf_2 = dat_variant$schijf_2,
      schijf_3 = dat_variant$schijf_3,
      tarief_1 = dat_variant$tarief_1,
      tarief_2 = dat_variant$tarief_2,
      tarief_3 = dat_variant$tarief_3
    )
  }
  
  belasting = sum(
    sapply(dat_case_verlies$grondslag, FUN = bepaal_belasting_totaal_adjusted),
    na.rm = T
  )
  
  # aanwas
  
  
  aanwas = dat_case_verlies %>%
    mutate(aanwas = replace_by_zero(.$aanwas)) %>%
    dplyr::select(., "aanwas") %>%
    sum() %>%
    round(., 0)
  
  # percentage belasting
  belasting_perc = calc_percentage(belasting, sum(dat_case$aanwas, na.rm = T))
  
  # percentage verrekend verlies tov totaal verlies
  verlies = dat_case_verlies %>%
    mutate(aanwas = case_when(aanwas > 0 ~ 0,
                              aanwas <= 0 ~ aanwas)) %>%
    dplyr::select(., "aanwas") %>%
    sum() %>%
    abs()
  
  vv = abs(sum((
    dat_case_verlies$grondslag_voor_vv - dat_case_verlies$grondslag
  )
  ))
  verrekend_verlies_perc = calc_percentage(vv, verlies)
  
  # percentage grondslag tov aanwas
  grondslag_perc = calc_percentage(sum(dat_case_verlies$grondslag), aanwas)
  
  
  
  # berekeningen
  temp_case = data.frame(
    case_name = dat_case$omschrijving[1],
    variant_name = dat_variant$variant,
    
    # belastingplichtige
    
    risico = round(mean(dat_case$risico, na.rm = T), 0),
    belasting = round(belasting, 0),
    belasting_perc = round(belasting_perc, 0),
    verlies = round(verlies, 0),
    verrekend_verlies = round(vv, 0),
    verrekend_verlies_perc = round(verrekend_verlies_perc, 0),
    vermogen = round(sum(dat_case$vermogen, na.rm = T), 0),
    aanwas = aanwas,
    grondslag = round(sum(dat_case_verlies$grondslag), 0),
    grondslag_perc = round(grondslag_perc, 2),
    spaargeld = round(sum(dat_case$spaargeld, na.rm = T), 0),
    finproduct = round(sum(dat_case$finproduct, na.rm = T), 0),
    restbezit = round(sum(dat_case$restbezit, na.rm = T), 0),
    schuld = round(sum(dat_case$schuld, na.rm = T), 0),
    spaargeld_rendperc = round(mean(dat_case$spaargeld_rendperc, na.rm = T), 0),
    finproduct_rendperc = round(mean(dat_case$finproduct_rendperc, na.rm = T), 0),
    restbezit_rendperc = round(mean(dat_case$restbezit_rendperc, na.rm = T), 0),
    schuld_rendperc = round(mean(dat_case$schuld_rendperc, na.rm = T), 0),
    
    # variant
    hvi = dat_variant$hvi,
    verlies_drempel = dat_variant$verlies_drempel,
    cf = dat_variant$cf,
    cb = dat_variant$cb,
    schijf_2 = dat_variant$schijf_2,
    schijf_3 = dat_variant$schijf_3,
    tarief_1 = dat_variant$tarief_1,
    tarief_2 = dat_variant$tarief_2,
    tarief_3 = dat_variant$tarief_3
  )
  
  return(temp_case)
  
}

# DATA

# case data
id = 1
jaar = 2026
omschrijving = "Jan Modaal"
risico = 2

spaargeld = gen_value(42300, sd)
finproduct = gen_value(7000, sd)

restbezit = 0
schuld = gen_value(12800, sd)

spaargeld_rendperc = gen_value(0.36, sd)

finproduct_rendperc = gen_value(6.17, sd_rend)

restbezit_rendperc = gen_value(6.17, sd)

schuld_rendperc = gen_value(2.57, sd)

case_data_colnames_original = c(
  "omschrijving",
  "risico",
  "jaar",
  "vermogen",
  "aanwas",
  "spaargeld",
  "finproduct",
  "restbezit",
  "schuld",
  "spaargeld_rendperc",
  "finproduct_rendperc",
  "restbezit_rendperc",
  "schuld_rendperc"
)

case_data_colnames_print = c(
  "omschrijving",
  "risico",
  "jaar",
  "vermogen",
  "aanwas",
  "spaargeld",
  "financiële producten",
  "overig bezit",
  "schuld",
  "rendement spaargeld (%)",
  "rendement financiële producten (%)",
  "rendement overig bezit (%)",
  "rendement schuld (%)"
)

case_data = gen_history(
  data.frame(
    id = id,
    jaar = jaar,
    omschrijving = omschrijving,
    risico = risico,
    spaargeld = spaargeld,
    finproduct = finproduct,
    restbezit = restbezit,
    schuld = schuld,
    spaargeld_rendperc = spaargeld_rendperc,
    finproduct_rendperc = finproduct_rendperc,
    restbezit_rendperc = restbezit_rendperc,
    schuld_rendperc = schuld_rendperc
  ),
  sd_rend = sd_rend
)

# varianten data
colnames_variant_original = c(
  "variant",
  "hvi",
  "verlies_drempel",
  "cf",
  "cb",
  "schijf_2",
  "schijf_3",
  "tarief_1",
  "tarief_2",
  "tarief_3"
)

colnames_variant_print = c("variant",
                           "hvi",
                           "verlies drempel",
                           "CF",
                           "CB",
                           "S2 €",
                           "S3 €",
                           "T1 %",
                           "T2 %",
                           "T3 %")

variant_data = data.frame(
  variant = "Variant",
  hvi = 1000,
  verlies_drempel = 1000,
  cf = 9,
  cb = 1,
  schijf_2 = as.numeric(NA),
  schijf_3 = as.numeric(NA),
  tarief_1 = 34,
  tarief_2 = as.numeric(NA),
  tarief_3 = as.numeric(NA)
)

variant_data_macro_input = data.frame(
  variant = "Variant",
  data = "Raming",
  hvi = 1000,
  verlies_drempel = 1000,
  cf = 9,
  cb = 1,
  schijf_2 = as.numeric(NA),
  schijf_3 = as.numeric(NA),
  tarief_1 = 34,
  tarief_2 = as.numeric(NA),
  tarief_3 = as.numeric(NA)
)



# variant effect data
variant_case_effects = gen_combi(dat_variant = variant_data, dat_case = case_data)

colnames_variant_population_original = c(
  "variant",
  "budget",
  "gini_budget",
  "gini_grondslag",
  "gini_belasting",
  "verlies_belasting",
  "hvi",
  "verlies_drempel",
  "cf",
  "cb",
  "schijf_2",
  "schijf_3",
  "tarief_1",
  "tarief_2",
  "tarief_3"
)

colnames_variant_population_print = c(
  "variant",
  "budgettaire opbrengst",
  "budget stabiliteit",
  "grondslag ongelijkheid",
  "belasting ongelijkheid",
  "opbrengst door onverrekend verlies (mld.)",
  "hvi",
  "verlies drempel",
  "CF",
  "CB",
  "S2 €",
  "S3 €",
  "T1 %",
  "T2 %",
  "T3 %"
)

# APPLICATION SECURITY
library(shinymanager)

inactivity = "function idleTimer() {
var t = setTimeout(logout, 3600000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"


# data.frame with credentials info
credentials = data.frame(
  user = c("guest"),
  password = c("minfin"),
  stringsAsFactors = FALSE
)
# APPLICATION SECURITY
library(shinymanager)

inactivity = "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"


# data.frame with credentials info
credentials = data.frame(
  user = c("guest"),
  password = c("minfin"),
  stringsAsFactors = FALSE
)

keepalive = "
          <script>
          var socket_timeout_interval
          var n = 0
          $(document).on('shiny:connected', function(event) {
          socket_timeout_interval = setInterval(function(){
          Shiny.onInputChange('count', n++)
          }, 15000)
          });
          $(document).on('shiny:disconnected', function(event) {
          clearInterval(socket_timeout_interval)
          });
          </script>
          "

#

# USER INTERFACE

ui = secure_app(head_auth = tags$script(inactivity),
                fluidPage(tags$head(HTML(keepalive)), navbarPage(
                  "Sandbox 3",
                  
                  # MICRO ANALYSES
                  navbarMenu(
                    "Micro analyses",
                    
                    # STAP 1: WIE ZIJN DE BELASTINGPLICHTIGEN?
                    tabPanel(
                      "Stap 1: Wie zijn de belastingplichtigen?",
                      
                      div(
                        style = "font-size: 10px",
                        
                        # SIDEBAR PANEL
                        sidebarPanel(
                          textOutput("keepAlive"),
                          # Omschrijving
                          h5("Omschrijving"),
                          helpText(
                            "Wie is de belastingplichtige? Tip: kies een omschrijving die het makkelijk maakt de casus later terug te vinden."
                          ),
                          fluidRow(column(
                            12,
                            textInput(
                              inputId = "omschrijving",
                              label = "",
                              value = omschrijving
                            )
                          )),
                          
                          # Spaargeld
                          h5("Spaargeld (+)"),
                          helpText("Hoeveel spaargeld heeft hij en wat is de rente op spaargeld?"),
                          fluidRow(column(
                            6,
                            numericInput(
                              inputId = "spaargeld",
                              label = "Vermogen (€)",
                              value = round(spaargeld),
                              min = 0,
                              max = Inf
                            )
                          ), column(
                            6,
                            numericInput(
                              inputId = "spaargeld_rendperc",
                              label = "Rendement (%)",
                              value = round(spaargeld_rendperc),
                              min = 0,
                              max = Inf
                            )
                          )),
                          
                          # Financiele producten
                          h5("Financiële producten (+)"),
                          helpText(
                            "Wat is de waarde van zijn financiële producten, zoals verhandelbare aandelen, obligaties, of cryptovaluta en wat is het rendement op deze producten?"
                          ),
                          fluidRow(column(
                            6,
                            numericInput(
                              inputId = "finproduct",
                              label = "Vermogen (€)",
                              value = round(finproduct),
                              min = 0,
                              max = Inf
                            )
                          ), column(
                            6,
                            numericInput(
                              inputId = "finproduct_rendperc",
                              label = "Rendement (%)",
                              value = round(finproduct_rendperc),
                              min = 0,
                              max = Inf
                            )
                          )),
                          
                          # Onroerend goed
                          h5("Onroerend goed (+)"),
                          helpText(
                            "Wat is de waarde van zijn onroerende goederen, zoals een tweede huis en wat is het rendement op dit bezit?"
                          ),
                          fluidRow(column(
                            6,
                            numericInput(
                              inputId = "restbezit",
                              label = "Vermogen (€)",
                              value = round(restbezit),
                              min = 0,
                              max = Inf
                            )
                          ), column(
                            6,
                            numericInput(
                              inputId = "restbezit_rendperc",
                              label = "Rendement (%)",
                              value = round(restbezit_rendperc),
                              min = 0,
                              max = Inf
                            )
                          )),
                          
                          # Schulden
                          h5("Schulden (-)"),
                          helpText(
                            "Hoeveel schuld heeft belastingplichtige en wat is de rente op deze schuld?"
                          ),
                          fluidRow(column(
                            6,
                            numericInput(
                              inputId = "schuld",
                              label = "Vermogen (€)",
                              value = round(schuld),
                              min = 0,
                              max = Inf
                            )
                          ), column(
                            6,
                            numericInput(
                              inputId = "schuld_rendperc",
                              label = "Rendement (%)",
                              value = round(schuld_rendperc),
                              min = 0,
                              max = Inf
                            )
                          )),
                          
                          # Crisis
                          h5("Crisis"),
                          helpText(
                            "Heeft belastingplichtige een financiële crisis meegemaakt? Zo ja, dan simuleert de tool een vijfjarige crisis vanaf 2050."
                          ),
                          div(
                            style = "font-size: 14px;padding:0px;margin-top: -20px;margin-left: 20px",
                            radioButtons(
                              inputId = "crisis",
                              label = "",
                              choices = c("ja", "nee"),
                              selected = "nee",
                              inline = TRUE
                            )
                          ),
                          
                          # Risicoprofiel
                          h5("Risicoprofiel"),
                          helpText(
                            "Hoe risicovol zijn de financiële producten van belastingplichtige (0 = geen risico, 10 = hoog risico)? De tool neemt deze informatie mee om de fluctatie in aanwas over de tijd heen te berekenen."
                          ),
                          sliderInput(
                            inputId = "risico",
                            label = "",
                            value = round(risico),
                            min = 0,
                            max = 10
                          ),
                          
                          # Knoppen
                          fluidRow(column(
                            6,
                            actionButton(
                              inputId = "add_case",
                              label = "casus toevoegen",
                              width = '104%'
                            )
                          ),
                          column(
                            6,
                            actionButton(
                              inputId = "random_case",
                              label = "random casus",
                              width = '104%'
                            )
                          )),
                          
                          width = 3
                        )
                      ),
                      
                      
                      # MAIN PANEL
                      mainPanel(tabsetPanel(
                        # PANEL 1
                        tabPanel(title = "Bewerk dataset", fluidPage(HTML("<br>"),
                                                                     div(
                                                                       column(
                                                                         10,
                                                                         HTML(
                                                                           "<b>!!! Instructie !!!</b> Schets in de linker (grijze) kolom de situatie van de belastingplichtige voor het
                                jaar 2026. Druk vervolgens op de knop <em>toevoegen</em> om de casus toe te voegen. Eventueel kunt u ook een door de
                                computer gegenereerde casus toevoegen door op het knopje <em>random casus</em> te drukken. De vermogensaanwas ontwikkeling
                                wordt nadien op random wijze geexprapoleerd naar de jaren 2027 tot en met 2045. Bent u niet tevreden met de dataset?
                                In het onderstaande luik kunt u alle data te verwijderen middels de <em>reset</em> knop of een enkele <em>casus verwijderen</em>.
                                Wil u liever uw data bewerken in excel? Dat kan. Download het template via de <em>download template</em> knop en laadt
                                deze vervolgens op via de <em>data opladen</em> optie. Wil u de ingegeven data opslaan, druk dan op de <em>opslaan</em> knop rechtsboven.
                                U kunt vervolgens de grondslag berekening van een enkele belastingplichtige bekijken onder de tab <em>inspecteer casus</em>."
                                                                         ),
                                                                         
                                                                         HTML("<br>"),
                                                                         fluidRow(column(
                                                                           3,
                                                                           h5("download template"),
                                                                           downloadButton("download_template", label = "download template")
                                                                         ),
                                                                         column(
                                                                           9,
                                                                           h5("data opladen (.xlsx)"),
                                                                           fileInput(
                                                                             "upload_data",
                                                                             label = NULL,
                                                                             multiple = F,
                                                                             accept = ".xlsx",
                                                                             width = '100%',
                                                                             placeholder = NA
                                                                           )
                                                                         )),
                                                                         div(rHandsontableOutput('aanwas_data'), 
                                                                         style = "font-size:120%")
                                                                       ),
                                                                       column(
                                                                         2,
                                                                         actionButton(
                                                                           inputId = "reset_data",
                                                                           label = "reset dataset",
                                                                           width = '100%'
                                                                         ),
                                                                         h4(),
                                                                         actionButton(
                                                                           inputId = "delete_case",
                                                                           label = "verwijder casus",
                                                                           width = '100%'
                                                                         ),
                                                                         h4(),
                                                                         downloadButton("download_cases", label = "opslaan", style = "width:100%;")
                                                                       )
                                                                     )), style = "font-size:95%"),
                        
                        # PANEL 2
                        tabPanel(title = "Inspecteer casus",
                                 div(fluidPage(
                                   HTML("<br>"),
                                   fluidRow(
                                     h5("Welke casus wilt u bekijken?"),
                                     uiOutput("micro_1_select_case"),
                                     column(
                                       6,
                                       h5("Aanwas 2026"),
                                       htmlOutput("grondslag_tekst", align = "justify"),
                                       h5("Aanwas 2026-2045"),
                                       htmlOutput("grondslag_tekst_2", align = "justify"),
                                       HTML("<br>"),
                                       h5("Toelichting grafieken"),
                                       HTML(
                                         "<i>De bovenste grafiek visualiseert de aanwas berekening voor het jaar 2026. Het lichtgrijze gedeelte van elke staaf
                                toont de waarde (in €) van het vermogens bestanddeel en het donkergrijze gedeelte de aanwas in €. Beweeg met de cursor over de staven om de exacte waarden af te lezen.
                                De onderste grafiek toont de aanwas voor alle jaren. Beweeg met de cursor over de grafiek om de exacte waarden af te lezen. Deze statistieken vormen het startpunt voor
                                de grondslag berekening voor elk belastingjaar.</i>"
                                       )
                                     ),
                                     column(
                                       6,
                                       h5("Plot aanwas 2026"),
                                       plotlyOutput("plot_aanwas_2026"),
                                       h5("Plot aanwas 2026-2045"),
                                       plotlyOutput("plot_aanwas")
                                     ),
                                   )
                                 )), style = "font-size:100%")
                        
                      ))
                    ),
                    
                    # STAP 2: WELKE VARIANTEN WILT U DOORREKENEN?
                    tabPanel(
                      "Stap 2: Welke variant wilt u doorrekenen?",
                      
                      div(
                        style = "font-size: 10px",
                        
                        # SIDEBAR PANEL
                        sidebarPanel(
                          # DETAILS
                          h5("Variant"),
                          helpText("Wat is de naam van de variant?"),
                          textInput(
                            inputId = "naam_variant",
                            label = "Naam Variant",
                            value = "Variant"
                          ),
                          
                          # HEFFING VRIJ INKOMEN
                          h5("Heffingvrij Inkomen (€)"),
                          helpText(
                            "Welk bedrag van de aanwas dient vrijgesteld te worden van belasting?"
                          ),
                          sliderInput(
                            inputId = "hvi",
                            label = "",
                            value = 1000,
                            min = 0,
                            max = 5000,
                            step = 50
                          ),
                          
                          # VERLIES VERREKENING
                          h5("Verliesverrekening (Jaar)"),
                          helpText(
                            "Wat is de drempel voor verliesverrekening? Met hoeveel jaren mag belastingplichtige verlies verrekenen met winst in het huidig jaar
                               (voorwaarts / carry forward)? Met hoeveel jaren mag de belastingplichtige verlies in het huidig jaar verrekenen met winst in voorgaande jaren (achterwaarts / carry backward)?"
                          ),
                          sliderInput(
                            inputId = "verlies_drempel",
                            "Drempel",
                            min = 0,
                            max = 5000,
                            value = 1000,
                            step = 50
                          ),
                          sliderInput(
                            inputId = "verlies_voor",
                            "Voorwaarts (CF)",
                            min = 0,
                            max = 15,
                            value = 9
                          ),
                          sliderInput(
                            inputId = "verlies_achter",
                            "Achterwaarts (CB)",
                            min = 0,
                            max = 3,
                            value = 1
                          ),
                          
                          # SCHIJVEN
                          h5("Schijven"),
                          helpText(
                            "Hoeveel schijven (max. 3) heeft de variant? Wat zijn de schijfgrenzen en tarieven? N.B. Laat u de velden leeg, dan wordt er
                               automatisch verondersteld dat er slechts een schijf is (met een ongelimiteerde schijfgrens) en een tarief."
                          ),
                          
                          column(4, HTML("<b>S1: Ondergrens (€)</b>")),
                          column(
                            4,
                            numericInput(
                              inputId = "schijf_2",
                              label = "S2: Ondergrens (€)",
                              value = NA,
                              min = 0,
                              max = Inf
                            )
                          ),
                          column(
                            4,
                            numericInput(
                              inputId = "schijf_3",
                              label = "S3: Ondergrens (€)",
                              value = NA,
                              min = 0,
                              max = Inf
                            )
                          ),
                          
                          column(
                            4,
                            numericInput(
                              inputId = "tarief_1",
                              label = "S1: Tarief (%)",
                              value = 34,
                              min = 0,
                              max = Inf
                            )
                          ),
                          column(
                            4,
                            numericInput(
                              inputId = "tarief_2",
                              label = "S2: Tarief (%)",
                              value = NA,
                              min = 0,
                              max = Inf
                            )
                          ),
                          column(
                            4,
                            numericInput(
                              inputId = "tarief_3",
                              label = "S3: Tarief (%)",
                              value = NA,
                              min = 0,
                              max = Inf
                            )
                          ),
                          
                          # VOEG TOE
                          actionButton(
                            inputId = "add_variant",
                            label = "variant toevoegen",
                            width = '100%'
                          ),
                          
                          width = 3
                        )
                      ),
                      # eind div()
                      
                      # MAIN PANEL
                      mainPanel(tabsetPanel(
                        # TAB 1
                        tabPanel(title = "Bewerk dataset", fluidPage(
                          HTML("<br>"),
                          column(
                            10,
                            HTML(
                              "<b>!!! Instructie !!!</b> Specificeer in de linker (grijze) kolom de variant die u wil doorrekenen.
                              Druk vervolgens op de knop <em>variant toevoegen</em> om de variant toe te voegen. Bent u ontevreden met de dataset?
                              In het onderstaande luik kunt u alle data te verwijderen middels de <em>reset</em> knop of een enkele <em>variant verwijderen</em>.
                              Wil u liever uw data bewerken in excel? Dat kan. Download het template via de <em>download template</em> knop en laadt
                              deze vervolgens op via de <em>data opladen</em> optie. Wil u de ingegeven data opslaan, druk dan op de <em>opslaan</em> knop rechtsboven.
                              U kunt vervolgens de eigenschappen van een door u geselecteerde variant bekijken in de tab <em>inspecteer variant</em>.<br>"
                            ),
                            
                            HTML("<br>"),
                            fluidRow(column(
                              3,
                              h5("download template"),
                              downloadButton("download_template_variant", label = "download template")
                            ),
                            column(
                              9,
                              h5("data opladen (.xlsx)"),
                              fileInput(
                                "upload_data_variant",
                                label = NULL,
                                multiple = F,
                                accept = ".xlsx",
                                width = '100%',
                                placeholder = NA
                              )
                            )),
                            div(dataTableOutput('variant_data'), style = "font-size:90%")
                          ),
                          column(
                            2,
                            actionButton(
                              inputId = "reset_data_variant",
                              label = "reset dataset",
                              width = '100%'
                            ),
                            h4(),
                            actionButton(
                              inputId = "delete_variant",
                              label = "verwijder variant",
                              width = '100%'
                            ),
                            h4(),
                            downloadButton("download_variants", label = "opslaan", style = "width:100%;")
                          )
                        )),
                        
                        # TAB 2
                        tabPanel(title = "Inspecteer variant", fluidPage(
                          HTML("<br>"), fluidRow(
                            column(
                              5,
                              h5("Welke variant wilt u bekijken?"),
                              uiOutput("micro_1_select_variant")
                            ),
                            column(
                              7,
                              h5("Welk jaar wilt u bekijken?"),
                              selectInput(
                                "plot_variant_jaar",
                                label = NULL,
                                choices = 2026:2045,
                                selected = 2036,
                                width = '100%'
                              )
                            ),
                            column(
                              5,
                              h5("Toelichting variant"),
                              htmlOutput("variant_tekst", align = "justify")
                            ),
                            column(
                              7,
                              h5("Visualisatie variant"),
                              helpText(
                                "Beweeg met de cursor over de grafiek om nadere toelichting te krijgen over de specificaties van de variant."
                              ),
                              plotlyOutput("plot_variant")
                            )
                          )
                        ))
                      ))
                    ),
                    
                    # STAP 3: BEKIJK RESULTATEN
                    tabPanel(
                      "Stap 3: Bekijk resultaten",
                      
                      tabsetPanel(
                        # TAB 1
                        tabPanel(
                          "Dataset",
                          HTML("<br>"),
                          column(
                            10,
                            HTML(
                              "De onderstaande tabel bevat de doorrekening van elk van de door u gespecificeerde variant voor elk van de door u opgegeven belastingplichtigen.
                               Bent u ontevreden met de dataset? In het onderstaande luik kunt u alle data te verwijderen middels de <em>reset</em> knop of een enkele <em>rij verwijderen</em>.
                               Wil u de tabel opslaan, druk dan op de <em>download</em> knop rechtsboven. Onder de tab <em>inspecteer microvoorbeeld</em> kunt u vervolgens
                               de grondslag en belasting berekening voor een enkele casus en een enkele variant inspecteren. Onder de tab <em>inspecteer micro effecten</em>
                               worden de gevolgen van elk van de varianten voor de door u opgegeven casi in kaart gebracht. Specifiek wordt er naar een drietal statistieken gekeken:
                               <br><br>
                               <ul>
                               <li><b>grondslag ongelijkheid</b>, m.n. de mate waarin het percentage grondslag (t.o.v. aanwas) verschilt tussen de door u opgegeven belastingplichtigen  (0-1);</li>
                               <li><b>belasting ongelijkheid</b>, m.n. de mate waarin het percentage belasting (t.o.v. aanwas) verschilt tussen de door u opgegeven belastingplichtigen (0-1);</li>
                               <li><b>belasting door onverrekende verliezen</b>, m.n. extra belasting betaald ten gevolge van onverrekende verliezen. </li>
                               </ul> <br>"
                            ),
                            div(dataTableOutput('variant_case_effects'), style = "font-size:90%")
                          ),
                          column(
                            2,
                            actionButton(
                              inputId = "reset_variant_case_effects",
                              label = "reset dataset",
                              width = '100%'
                            ),
                            h4(),
                            actionButton(
                              inputId = "delete_variant_effects",
                              label = "verwijder variant",
                              width = '100%'
                            ),
                            h4(),
                            actionButton(
                              inputId = "delete_case_effects",
                              label = "verwijder casus",
                              width = '100%'
                            ),
                            h4(),
                            downloadButton(
                              "download_variants_case_effects",
                              label = "opslaan",
                              style = "width:100%;"
                            )
                          )
                        ),
                        
                        # TAB 2
                        tabPanel(
                          "Inspecteer microvoorbeeld",
                          HTML("<br>"),
                          
                          # SIDEBAR
                          div(
                            style = "font-size: 10px",
                            sidebarPanel(
                              h5("Jaar"),
                              helpText("Voor welke jaar wilt u een microvoorbeeld genereren?"),
                              selectInput(
                                "micro_3_select_year",
                                label = NULL,
                                choices = c("Alle jaren", 2026:2045)
                              ),
                              h5("Selecteer casus"),
                              helpText("Voor welke casus wilt u een microvoorbeeld genereren?"),
                              uiOutput("micro_3_select_case"),
                              h5("Selecteer variant"),
                              helpText("Voor welke variant wilt u een microvoorbeeld genereren?"),
                              uiOutput("micro_3_select_variant"),
                              downloadButton("download_micro", label = "voorbeeld opslaan", style = "width:100%;"),
                              width = 3
                            )
                          ),
                          
                          # MAINPANEL
                          mainPanel(
                            column(
                              6,
                              h5("Microvoorbeeld"),
                              div(dataTableOutput('tab_micro'), style = "font-size:80%"),
                              HTML("<br><br>"),
                              htmlOutput("micro_tekst", align = "justify")
                            ),
                            column(
                              6,
                              h5("Visualisatie microvoorbeeld"),
                              helpText(
                                "Beweeg met de cursor over de grafiek om nadere toelichting te krijgen over de aanwas (belastbaar en niet belastbaar) en verlies (verrekenbaar en niet verrekenbaar) van de belastingplichtige per belastingjaar."
                              ),
                              plotlyOutput("plot_micro")
                            )
                          )
                        ),
                        
                        # TAB 3
                        tabPanel(
                          "Inspecteer micro-effecten",
                          HTML("<br>"),
                          column(
                            3,
                            h5("Microeffecten per variant"),
                            helpText(
                              "De onderstaande tabel toont een drietal statistieken die samenvatten welk effect de door u opgegeven varianten hebben op de relatie tussen de door u opgegeven belastingplichtigen.
                               Grondslag ongelijkheid en belasting ongelijkheid vat de mate waarin belastingplichtigen een verschillend percentage grondslag hebben en belasting betale t.o.v. hun aanwas op een schaal
                               van 0 (perfect gelijk) tot 1 (perfect ongelijk). De statistiek belasting door onverrekende verliezen is het gemiddeld bedrag dat burgers extra betalen door restricties op verliesverrekening."
                            ),
                            
                            dataTableOutput('tab_microeffects'),
                            HTML("<br><br>"),
                            h5("Beste variant per categorie"),
                            helpText(
                              "Varianten met de minste ongelijkheid en de laagste belasting door onverrekende verliezen"
                            ),
                            dataTableOutput('tab_microwinners')
                          ),
                          column(
                            4,
                            h5("Selecteer variant"),
                            helpText(
                              "Beweeg met uw cursor over de staven om de nadere opsplitsing van de aanwas in belasting en grondslag per belastingplichtige te bekijken"
                            ),
                            uiOutput("plot_micro_select_variant_1_choices"),
                            plotlyOutput("plot_micro_variant_1")
                          ),
                          column(
                            4,
                            h5("Selecteer andere variant"),
                            helpText(
                              "Beweeg met uw cursor over de staven om de nadere opsplitsing van de aanwas in belasting en grondslag per belastingplichtige te bekijken"
                            ),
                            uiOutput("plot_micro_select_variant_2_choices"),
                            plotlyOutput("plot_micro_variant_2")
                          ),
                          
                        )
                      )
                      
                    )
                  ),
                  
                  
                  
                  # MACRO ANALYSES
                  tabPanel(
                    "Macro analyses",
                    
                    div(
                      style = "font-size: 10px",
                      sidebarPanel(
                        # DETAILS
                        h5("Variant"),
                        helpText("Wat is de naam van de variant?"),
                        textInput(
                          inputId = "naam_variant_macro",
                          label = "Naam Variant",
                          value = "Voorbeeld"
                        ),
                        
                        # HEFFING VRIJ INKOMEN
                        h5("Heffingvrij Inkomen (€)"),
                        helpText(
                          "Welk bedrag van de aanwas dient vrijgesteld te worden van belasting?"
                        ),
                        sliderInput(
                          inputId = "hvi_macro",
                          label = "",
                          value = 1000,
                          min = 0,
                          max = 5000,
                          step = 50
                        ),
                        
                        # VERLIES VERREKENING
                        h5("Verliesverrekening (Jaar)"),
                        helpText(
                          "Wat is de drempel voor verliesverrekening? Met hoeveel jaren mag belastingplichtige verlies verrekenen met winst in het huidig jaar (voorwaarts / carry forward)? Met hoeveel jaren mag de belastingplichtige verlies in het huidig jaar verrekenen met winst in voorgaande jaren (achterwaarts / carry backward)?"
                        ),
                        sliderInput(
                          inputId = "verlies_drempel_macro",
                          "Drempel",
                          min = 0,
                          max = 5000,
                          value = 1000,
                          step = 50
                        ),
                        sliderInput(
                          inputId = "verlies_voor_macro",
                          "Voorwaarts (CF)",
                          min = 0,
                          max = 10,
                          value = 9
                        ),
                        sliderInput(
                          inputId = "verlies_achter_macro",
                          "Achterwaarts (CB)",
                          min = 0,
                          max = 3,
                          value = 1
                        ),
                        
                        # SCHIJVEN
                        h5("Schijven"),
                        helpText(
                          "Hoeveel schijven (max. 3) heeft de variant? Wat zijn de schijfgrenzen en
                                 tarieven? N.B. Laat u de velden leeg, dan wordt er automatisch verondersteld dat er slechts een schijf is (met een ongelimiteerde schijfgrens) en een tarief."
                        ),
                        
                        column(4, HTML("<b>S1: Ondergrens (€)</b>")),
                        column(
                          4,
                          numericInput(
                            inputId = "schijf_2_macro",
                            label = "S2: Ondergrens (€)",
                            value = NA,
                            min = 0,
                            max = Inf
                          )
                        ),
                        column(
                          4,
                          numericInput(
                            inputId = "schijf_3_macro",
                            label = "S3: Ondergrens (€)",
                            value = NA,
                            min = 0,
                            max = Inf
                          )
                        ),
                        
                        column(
                          4,
                          numericInput(
                            inputId = "tarief_1_macro",
                            label = "S1: Tarief (%)",
                            value = 34,
                            min = 0,
                            max = Inf
                          )
                        ),
                        column(
                          4,
                          numericInput(
                            inputId = "tarief_2_macro",
                            label = "S2: Tarief (%)",
                            value = NA,
                            min = 0,
                            max = Inf
                          )
                        ),
                        column(
                          4,
                          numericInput(
                            inputId = "tarief_3_macro",
                            label = "S3: Tarief (%)",
                            value = NA,
                            min = 0,
                            max = Inf
                          )
                        ),
                        
                        
                        # STEEKPROEFGROOTTE
                        div(
                          style = "font-size: 12px",
                          h5("Type analyse"),
                          helpText(
                            "Voor hoeveel belastingplichtigen wilt u uw variant doorrekenne? Let op! De snelheid van de analyse is sterk afhankelijk van de keuze die u hier maakt.
              Wilt u snel een groot aantal varianten met elkaar vergelijken? Kies dan voor de optie 'Steekproef' (1 minuut). Wilt u een officiële raming opvragen voor een of twee varianten? Kies dan de optie 'Raming' (3 minuten)."
                          ),
                          HTML("<br>"),
                          
                          radioButtons(
                            inputId = "samp_size",
                            label = NULL,
                            choices = c("Raming",
                                        "Steekproef")
                          )
                        ),
                        
                        # VOEG TOE
                        actionButton(
                          inputId = "add_variant_macro",
                          label = "variant toevoegen",
                          width = '100%'
                        ),
                        
                        
                        
                        width = 3
                      )
                    ),
                    
                    mainPanel(tabsetPanel(
                      tabPanel(
                        "Resultaten",
                        HTML("<br>"),
                        column(
                          10,
                          HTML(
                            "De onderstaande tabel bevat de doorrekening van de door u gespecificeerde varianten voor de populatie.
             Bent u ontevreden met de dataset? In het onderstaande luik kunt u alle data te verwijderen middels de <em>reset</em> knop of een enkele <em>rij verwijderen</em>.
             Wil u de tabel opslaan, druk dan op de <em>download</em> knop rechtsboven. De variant wordt op basis van een viertal statistieken objectief in kaart gebracht:
             <br><br>
             <ul>
             <li><b>aantal belastingplichtigen (mln.)</b>, m.n. het aantal burgers dat een positieve box 3 grondslag heeft;</li>
             <li><b>opbrengst (mld.)</b>, m.n. de budgettaire opbrengst van de variant;</li>
             <li><b>Δ nieuw-oud (mld.)</b>, m.n. hoeveel meer of minder opbrengst de variant heeft t.o.v. het oude stelsel;</li>
             <li><b>gini grondslag (0-1)</b>, m.n. grondslag ongelijkheid of de mate waarin het percentage grondslag (t.o.v. aanwas) verschilt in de populatie  (0 'maximale gelijkheid'- 1 'maximale ongelijkheid');</li>
             <li><b>gini belasting (0-1)</b>, m.n. belasting ongelijkheid de mate waarin het percentage belasting (t.o.v. aanwas) verschilt in de populatie (0 'maximale gelijkheid'- 1 'maximale ongelijkheid');</li>
             <li><b>opbrengst door onverrekend verlies (mld.)</b>, m.n. extra budgettaire opbrengst door restricties op verliesverrekening; </li>
             <li><b>derving door verrekend verlies (mld.)</b>, m.n. budgettaire derving door de toepassing van verliesverrekening; </li>
             <li><b>derving door hvi (mld.)</b>, m.n. budgettaire derving veroorzaakt door de toepassing van een heffingvrij inkomen; </li>
             <li><b>opbrengst door vvd (mld.)</b>, m.n. budgettaire opbrengst veroorzaakt door de toepassing van een verliesverrekening drempel; </li>
             <li><b>aantal gedragseffect (mln.)</b>, m.n. het aantal burgers die minder belasting betalen ten gevolge van een gedragseffect; </li>
             <li><b>derving door gedragseffect (mld.)</b>, m.n. de derving voortvloeiend uit gedragseffecten onder de verliezers; </li>
             </ul> <br>
             <b> Waarschuwing: steekproefresultaten dienen enkel om varianten onderling te vergelijken. Ze geven geen precieze schattingen m.u.v. het aantal belastingplichtigen en de derving door het hvi </b>.
             <br><br>"
                          ),
                          
                          div(dataTableOutput('variant_population_effects'), style = "font-size:90%")
                        ),
                        column(
                          2,
                          actionButton(
                            inputId = "reset_variant_population_effects",
                            label = "reset dataset",
                            width = '100%'
                          ),
                          h4(),
                          actionButton(
                            inputId = "delete_variant_population_effects",
                            label = "verwijder variant",
                            width = '100%'
                          ),
                          
                          h4(),
                          downloadButton(
                            "download_variants_population_effects",
                            label = "opslaan",
                            style = "width:100%;"
                          )
                        )
                      ),
                      
                      tabPanel(title = "Toelichting variant", fluidPage(
                        HTML("<br>"), fluidRow(
                          column(
                            5,
                            h5("Welke variant wilt u bekijken?"),
                            uiOutput("micro_1_select_variant_macro")
                          ),
                          column(
                            7,
                            h5("Welk jaar wilt u bekijken?"),
                            selectInput(
                              "plot_variant_jaar_macro",
                              label = NULL,
                              choices = 2026:2045,
                              selected = 2036,
                              width = '100%'
                            )
                          ),
                          column(
                            5,
                            h5("Toelichting variant"),
                            htmlOutput("variant_tekst_macro", align = "justify")
                          ),
                          column(
                            7,
                            h5("Visualisatie variant"),
                            helpText(
                              "Beweeg met de cursor over de grafiek om nadere toelichting te krijgen over de specificaties van de variant."
                            ),
                            plotlyOutput("plot_variant_macro")
                          )
                        )
                      )),
                      
                      # TAB 2
                      tabPanel("Toelichting resultaten",
                               div(fluidPage(
                                 HTML("<br>"),
                                 fluidRow(
                                   column(
                                     4,
                                     h5("Welke variant wilt u bekijken?"),
                                     uiOutput("macro_budget_select_variant"),
                                     htmlOutput("macro_budget_tekst", align = "justify")
                                   ),
                                   column(
                                     8,
                                     h5("Cumulatieve opbrengst en derving (percentiel)"),
                                     HTML(
                                       "<i>De grafiek toont de cumulatieve opbrengst en derving in mld. € per rendementspercentiel.
                         Het eerste percentiel is dus de 1% van de populatie met de laagste aanwas, en het laatste percentiel
                         de 1% met de hoogste aanwas.</i>"
                                     ),
                                     plotlyOutput("macro_budget_fig"),
                                   ),
                                 )
                               )), style = "font-size:100%")
                      
                    )# end tabsetpanel
                    )# end mainpanel
                  ) # end tabpanel
                  
                  
                ))
) # eind FluidPage() + secureapp


# SERVER
server = function(input, output, session) {
  # security
  result_auth <-
    secure_server(check_credentials = check_credentials(credentials))
  
  output$res_auth <- renderPrint({
    reactiveValuesToList(result_auth)
  })
  
  
  # popups
  popup = function(title, text) {
    showModal(modalDialog(
      title = title,
      text,
      footer = actionButton(inputId = "close_window", label = "Sluiten")
    ))
    observeEvent(input$close_window, {
      removeModal()
    })
  }
  
  popup_adjusted = function(title, text, footer) {
    showModal(modalDialog(title = title, text, footer = footer))
  }
  
  
  ################################## 1. MICRO ANALYSES ##################################
  
  ############ 1.1. MICRO ANALYSES - STAP 1: WIE ZIJN DE BELASTINGPLICHTIGEN? ###########
  
  ############ SIDEBAR ###########
  
  
  # KNOP CASUS TOEVOEGEN
  observeEvent(input$add_case, {
    if (sum(
      c(
        input$spaargeld_rendperc,
        input$finproduct_rendperc,
        input$restbezit_rendperc,
        input$schuld_rendperc
      )
    ) > 0) {
      sd = 0.4
      sd_rend = 0.5 * isolate(input$risico) * sd
      
      if (is.null(input$upload_data)) {
        data = case_data()
      }
      if (!is.null(input$upload_data)) {
        data = upload_data$data
      }
      if (nrow(data) == 0) {
        id = 1
      } else {
        id = max(data$id) + 1
      }
      
      if (isolate(input$omschrijving) %in% data$omschrijving) {
        omschrijving_new = paste0(isolate(input$omschrijving), " ", max(data$id) + 1)
      } else {
        omschrijving_new = isolate(input$omschrijving)
      }
      
      if (is.na(isolate(input$spaargeld))) {
        spaargeld = 0
      } else {
        spaargeld = isolate(input$spaargeld)
      }
      if (is.na(isolate(input$finproduct))) {
        finproduct = 0
      } else {
        finproduct = isolate(input$finproduct)
      }
      if (is.na(isolate(input$restbezit))) {
        restbezit = 0
      } else {
        restbezit = isolate(input$restbezit)
      }
      if (is.na(isolate(input$schuld))) {
        schuld = 0
      } else {
        schuld = isolate(input$schuld)
      }
      
      if (is.na(isolate(input$spaargeld_rendperc))) {
        spaargeld_rendperc = 0
      } else {
        spaargeld_rendperc = isolate(input$spaargeld_rendperc)
      }
      if (is.na(isolate(input$finproduct_rendperc))) {
        finproduct_rendperc = 0
      } else {
        finproduct_rendperc = isolate(input$finproduct_rendperc)
      }
      if (is.na(isolate(input$restbezit_rendperc))) {
        restbezit_rendperc = 0
      } else {
        restbezit_rendperc = isolate(input$restbezit_rendperc)
      }
      if (is.na(isolate(input$schuld_rendperc))) {
        schuld_rendperc = 0
      } else {
        schuld_rendperc = isolate(input$schuld_rendperc)
      }
      
      dat = gen_history(
        data.frame(
          id = id,
          omschrijving = omschrijving_new,
          risico = gen_value(
            mean = isolate(input$risico),
            sd = sd,
            n = 1
          ),
          jaar = 2026,
          spaargeld = spaargeld,
          finproduct = finproduct,
          restbezit = restbezit,
          schuld = schuld,
          spaargeld_rendperc = spaargeld_rendperc,
          finproduct_rendperc = finproduct_rendperc,
          restbezit_rendperc = restbezit_rendperc,
          schuld_rendperc = schuld_rendperc
        ),
        sd_rend = sd_rend,
        crisis = input$crisis
      )
      
      # voeg data toe
      if (is.null(input$upload_data)) {
        case_data() %>% bind_rows(dat) %>% case_data()
      } else {
        upload_data$data = upload_data$data %>% bind_rows(dat)
      }
      
      # als rendement gelijk is aan nul
    } else {
      popup(
        "Casus niet toegevoegd",
        "Een burger zonder rendement zal onder geen enkele variant belasting betalen in Box 3."
      )
    }
    
  })
  
  # KNOP RANDOM CASUS
  observeEvent(input$random_case, {
    # switch data als opgeladen
    if (is.null(input$upload_data)) {
      data = case_data()
    }
    if (!is.null(input$upload_data)) {
      data = upload_data$data
    }
    
    # id
    if (nrow(data) == 0) {
      id = 1
    } else {
      id = max(data$id) + 1
    }
    
    # garandeer unieke omschrijving
    if (isolate(input$omschrijving) %in% data$omschrijving) {
      omschrijving_new = paste0(isolate(input$omschrijving), " ", max(data$id) + 1)
    } else {
      omschrijving_new = isolate(input$omschrijving)
    }
    
    # pas variantie aan
    risico = sample(0:10, 1, replace = T)
    sd = sd * 2
    sd_rend = risico * sd
    
    
    dat = gen_history(
      data.frame(
        id = id,
        omschrijving = omschrijving_new,
        risico = risico,
        jaar = 2026,
        spaargeld = max(c(
          gen_value(mean = 42300, sd = 42300 * sd), 0
        )),
        finproduct = max(c(gen_value(
          mean = 7000, sd = 7000 * sd
        ), 0)),
        restbezit = sample(c(
          rep(0, 100),
          rep(250000, 25),
          rep(500000, 12),
          rep(1000000, 6)
        ), 1, replace = T),
        schuld = max(c(
          gen_value(mean = 12800, sd = 12800 * sd), 0
        )),
        spaargeld_rendperc = max(c(gen_value(
          mean = 0.36, sd = 0.36 * sd
        ), 0)),
        finproduct_rendperc = max(c(
          gen_value(mean = 6.17, sd = 6.17 * sd_rend), 0
        )),
        restbezit_rendperc = max(c(
          gen_value(mean = 6.17, sd = 6.17 * sd_rend), 0
        )),
        schuld_rendperc = max(c(gen_value(
          mean = 2.57, sd = 2.57 * sd
        )), 0)
      ),
      sd_rend = sd_rend,
      crisis = input$crisis
    )
    
    # voeg data toe
    if (is.null(input$upload_data)) {
      case_data() %>% bind_rows(dat) %>% case_data()
    } else {
      upload_data$data = upload_data$data %>% bind_rows(dat)
    }
    
  })
  
  ############ TAB 1 ############
  
  # DOWNLOAD TEMPLATE
  output$download_template = downloadHandler(
    filename = function() {
      paste("sandbox3_template_casus.xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(
        data.frame(
          omschrijving = "",
          spaargeld = as.numeric(NA),
          spaargeld_rendementpercentage = as.numeric(NA),
          financieleproducten = as.numeric(NA),
          financieleproducten_rendementpercentage = as.numeric(NA),
          overigbezit = as.numeric(NA),
          overigbezit_rendementpercentage = as.numeric(NA),
          schuld = as.numeric(NA),
          schuld_rendementpercentage = as.numeric(NA),
          risicoprofiel = as.numeric(NA),
          crisis_janee = ""
        ),
        file
      )
    }
  )
  
  # OPLADEN DATA
  upload_data = reactiveValues(data = NULL)
  
  gen_empty_data = function() {
    data = gen_history(
      data.frame(
        id = 1,
        omschrijving = "1",
        risico = 1,
        jaar = 2026,
        spaargeld = 1,
        finproduct = 1,
        restbezit = 1,
        schuld = 1,
        spaargeld_rendperc = 1,
        finproduct_rendperc = 1,
        restbezit_rendperc = 1,
        schuld_rendperc = 1
      ),
      sd_rend = sd_rend,
      crisis = "nee"
    ) %>%
      filter(row_number() %in% -1)
    
    return(data)
    
  }
  
  gen_upload_data = function() {
    if (nrow(readxl::read_xlsx(input$upload_data$datapath)) > 0) {
      data = readxl::read_xlsx(input$upload_data$datapath) %>%
        setNames(
          c(
            "omschrijving",
            "spaargeld",
            "spaargeld_rendperc",
            "finproduct",
            "finproduct_rendperc",
            "restbezit",
            "restbezit_rendperc",
            "schuld",
            "schuld_rendperc",
            "risico",
            "crisis"
          )
        )
      
      datalist = list()
      
      # voor iedere belastingplichtige
      for (i in c(1:length(unique(data$omschrijving)))) {
        # selecteer belastingplichtige
        row = subset(data, omschrijving == unique(data$omschrijving)[i])[1, ]
        
        # genereer geschiedenis belastingplichtige
        datalist[[i]] = gen_history(
          data.frame(
            id = i,
            omschrijving = row$omschrijving,
            risico = row$risico,
            jaar = 2026,
            spaargeld = row$spaargeld,
            finproduct = row$finproduct,
            restbezit = row$restbezit,
            schuld = row$schuld,
            spaargeld_rendperc = row$spaargeld_rendperc,
            finproduct_rendperc = row$finproduct_rendperc,
            restbezit_rendperc = row$restbezit_rendperc,
            schuld_rendperc = row$schuld_rendperc
          ),
          sd_rend = sd_rend,
          crisis = row$crisis
        )
      }
      
      upload_data$data = do.call(rbind, datalist)
      
      # generate empty data frame if file does not contain any entries
    } else {
      upload_data$data = gen_empty_data()
      
    }
  }
  
  observeEvent(input$upload_data, {
    upload_data$data = gen_upload_data()
  })
  
  round_zero = function(x){x = round(x, 0)}
  
  # VOORGEPROGRAMEERDE DATA
  case_data = reactiveVal(
    case_data 
    )
  
  # OUTPUT TABEL
  output$aanwas_data = renderRHandsontable({
    if (is.null(input$upload_data)) {
      data = case_data()
    } else {
      data = upload_data$data
    }
    
    data = data %>%
      mutate(aanwas = (spaargeld * (spaargeld_rendperc / 100)) + 
               (finproduct * (finproduct_rendperc / 100)) + 
               (restbezit * (restbezit_rendperc / 100)) - 
               (schuld * (schuld_rendperc / 100))
      ) %>%
      mutate_at(c("vermogen", "aanwas", "finproduct", "restbezit", "schuld"), round_zero)
    
    sparkline = function(x){
      
      x = jsonlite::toJSON(list(
        values = c(0, x),
        options = list(type = "line")
      ))
      
     return(x)
      
    }
    
    sparkbar = function(x){
      
      x = jsonlite::toJSON(list(
        values = c(0, x),
        options = list(type = "bar")
      ))
      
      return(x)
      
    }
    
    data = data %>%
      group_by(omschrijving) %>%
      mutate(risico = mean(risico)) %>%
      mutate(vermogen = sparkbar(vermogen)) %>%
      mutate(aanwas = sparkbar(aanwas)) %>%
      mutate(spaargeld = sparkbar(spaargeld)) %>%
      mutate(finproduct = sparkbar(finproduct)) %>%
      mutate(restbezit = sparkbar(restbezit)) %>%
      mutate(schuld = sparkbar(schuld)) %>%
      mutate(spaargeld_rendperc = sparkbar(spaargeld_rendperc)) %>%
      mutate(finproduct_rendperc = sparkbar(finproduct_rendperc)) %>%
      mutate(restbezit_rendperc = sparkbar(restbezit_rendperc)) %>%
      mutate(schuld_rendperc = sparkbar(schuld_rendperc)) %>%
      slice(1)
    
    data$jaar = "2026-2045"
    
    library(rhandsontable)
    rhandsontable(
      dplyr::select(data, case_data_colnames_original), 
      rowHeaders = NULL, 
      width = 1100, 
      height = 700, 
      search = T, 
      colHeaders = case_data_colnames_print, 
      rowHeaderWidth = 200) %>%
      hot_col("vermogen", renderer = htmlwidgets::JS("renderSparkline"), valign = "htBottom") %>%
      hot_col("aanwas", renderer = htmlwidgets::JS("renderSparkline")) %>%
      hot_col("spaargeld", renderer = htmlwidgets::JS("renderSparkline")) %>%
      hot_col("financiële producten", renderer = htmlwidgets::JS("renderSparkline")) %>%
      hot_col("overig bezit", renderer = htmlwidgets::JS("renderSparkline")) %>%
      hot_col("schuld", renderer = htmlwidgets::JS("renderSparkline")) %>%
      hot_col("rendement spaargeld (%)", renderer = htmlwidgets::JS("renderSparkline")) %>%
      hot_col("rendement financiële producten (%)", renderer = htmlwidgets::JS("renderSparkline")) %>%
      hot_col("rendement overig bezit (%)", renderer = htmlwidgets::JS("renderSparkline")) %>%
      hot_col("rendement schuld (%)", renderer = htmlwidgets::JS("renderSparkline")) %>%
      #hot_rows(rowHeights = 40) %>%
      hot_cols(colWidths = c(120, 60, 120, rep(80, 6), rep(120,4) )) %>%
      hot_cols(fixedColumnsLeft = 1) %>%
      hot_rows(fixedRowsTop = 1) 
    
    
    #%>%  setNames(case_data_colnames_print)
    
    
    
    #out = rhandsontable(dplyr::select(data, c("omschrijving", "spaargeld")), rowHeaders = NULL, width = 550, height = 300) %>%
    #  hot_col("spaargeld", renderer = htmlwidgets::JS("renderSparkline"))
    
  }
  #, escape = F, server = F, rownames = F, selection = 'single', options = list(
  #  paging = T,
  #  pageLength = 16,
  #  scrollX = T
  #)
  )
  
  # KNOP RESET DATA
  observeEvent(input$reset_data, {
    if (is.null(input$upload_data)) {
      case_data() %>% filter(row_number() %in% -1) %>% case_data()
    } else {
      upload_data$data =  gen_empty_data()
    }
  })
  
  # KNOP VERWIJDER CASUS
  observeEvent(input$delete_case, {
    if (is.null(input$upload_data)) {
      remove = case_data()[input$aanwas_data_rows_selected, "omschrijving"]
      case_data() %>% subset(., omschrijving != remove) %>% case_data()
    } else {
      remove = upload_data$data[input$aanwas_data_rows_selected, "omschrijving"]
      upload_data$data = upload_data$data %>% subset(., omschrijving != remove)
    }
    
  })
  
  # KNOP DOWNLOAD
  selected_data = function(input = NULL) {
    if (is.null(input)) {
      data = case_data()
    } else {
      data = upload_data$data
    }
    return(data)
  }
  
  output$download_cases = downloadHandler(
    filename = function() {
      paste("sandbox3_casi.xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(selected_data(input$upload_data), file)
    }
  )
  
  ############ TAB 2 ############
  
  # DATA CASE NAMES
  output$micro_1_select_case = renderUI({
    if (is.null(input$upload_data)) {
      data = case_data()
    } else {
      data = upload_data$data
    }
    data = subset(data, jaar == 2026)
    selectInput(
      "micro_1_select_case_selection",
      label = NULL,
      choices = data$omschrijving
    )
  })
  
  # TEKSTUELE OMSCHRIJVING GRONDSLAG
  
  output$grondslag_tekst = renderText({
    # data
    if (is.null(input$upload_data)) {
      data = case_data()
    } else {
      data = upload_data$data
    }
    
    text = ""
    
    # als geen data beschikbaar
    if (nrow(data) < 1) {
      text = "Er is geen data beschikbaar. Voeg data toe met behulp van de tool. <br><br><br>"
      
      # als wel data beschikbaar
    } else {
      selection = input$micro_1_select_case_selection
      df = subset(data, jaar == 2026 & omschrijving == selection)
      naam = df$omschrijving
      
      
      # (a) Spaargeld
      if (df$spaargeld > 0) {
        text = paste0(
          text,
          "<b>Spaargeld.</b> Op de peildatum van 2026 heeft ",
          naam,
          " <i>",
          number_to_money(df$spaargeld),
          "</i> spaargeld op zijn rekening. Belastingplichtige ontvangt <i>",
          percentify(df$spaargeld_rendperc),
          " rente</i>. De aanwas is daarmee gelijk aan <i>",
          number_to_money(bereken_aanwas(
            df$spaargeld, df$spaargeld_rendperc
          )) ,
          "</i>. "
        )
      }
      else {
        text = paste0(
          text,
          "<b>Spaargeld.</b> Op de peildatum van 2026 heeft belastingplichtige geen spaargeld. "
        )
      }
      
      # (b) Financiële producten
      if (df$finproduct > 0) {
        text = paste0(
          text,
          "<b>Financiële producten </b> Daarnaast heeft belastingplichtige <i>",
          number_to_money(df$finproduct),
          "</i> financiële producten met een gemiddeld <i>rendement van ",
          percentify(df$finproduct_rendperc),
          "</i>. De aanwas is daarmee gelijk aan <i>",
          number_to_money(
            bereken_aanwas(df$finproduct, df$finproduct_rendperc)
          ),
          "</i>. "
        )
      } else {
        text =  paste0(
          text,
          "<b>Financiële producten </b>Belastingplichtige heeft geen financiële producten "
        )
      }
      
      # (c) Overig bezit
      if (df$restbezit > 0) {
        text = paste0(
          text,
          "<b>Onroerend goed.</b> Belastingplichtige beschikt ook over een tweede huis, t.w.v. <i>",
          number_to_money(df$restbezit),
          "</i>, met een gemiddeld rendement van <i>",
          percentify(df$restbezit_rendperc),
          "</i>. De aanwas is daarmee gelijk aan <i>",
          number_to_money(bereken_aanwas(
            df$restbezit, df$restbezit_rendperc
          )),
          "</i>. "
        )
      } else {
        text = paste0(
          text,
          "<b>Onroerend goed.</b> Belastingplichtige heeft geen onroerende goederen. "
        )
      }
      
      # (d) Schulden
      if (df$schuld > 0) {
        text = paste0(
          text,
          "<b>Schulden.</b> Naast positieve vermogensbestanddelen, heeft belastingplichtige ook <i>",
          number_to_money(df$schuld),
          " schulden</i>. De <i>rente bedraagt ",
          percentify(df$schuld_rendperc),
          "</i>. De aanwas op schulden is daarmee gelijk aan <i>",
          number_to_money(bereken_aanwas(df$schuld, df$schuld_rendperc)),
          "</i>. Dit bedrag wordt in mindering gebracht bij het rendement voortvloeiend uit de positieve vermogensbestanddelen. <br><br>"
        )
      } else {
        text = paste0(text,
                      "<b>Schulden.</b> Belastingplichtige heeft geen schulden. <br><br>")
      }
      
      df$aanwas = (df$spaargeld * (df$spaargeld_rendperc / 100)) + 
        (df$finproduct * (df$finproduct_rendperc / 100)) + 
        (df$restbezit * (df$restbezit_rendperc / 100)) - 
        (df$schuld * (df$schuld_rendperc / 100))
      
      text = paste0(
        text,
        "<b> Vóór het toepassen van het heffingvrij inkomen en verliesverrekening is de belastinggrondslag daardoor gelijk aan ",
        number_to_money(df$aanwas),
        "</b>. "
      )
      
    }
  })
  
  output$grondslag_tekst_2 = renderText({
    if (is.null(input$upload_data)) {
      data = case_data()
    } else {
      data = upload_data$data
    }
    selection = input$micro_1_select_case_selection
    df_long = subset(data, omschrijving == selection)
    naam = selection
    
    # text
    aanwas_min = min(df_long$aanwas)
    aanwas_max = max(df_long$aanwas)
    jaar_min = subset(df_long, aanwas == aanwas_min)$jaar[1]
    jaar_max = subset(df_long, aanwas == aanwas_max)$jaar[1]
    text = paste0(
      "De grondslag van ",
      naam,
      " verschilt over de jaren heen.
             U heeft enkel de data voor 2026 opgegeven. De aanwas voor resterende jaren wordt
             op basis van gerandomiseerde rendementen doorgerekend naar de jaren 2027 tot 2045.
             Het resultaat hiervan kunt u grafisch inspecteren in de onderstaande grafiek.
             Zo blijkt dat ",
      jaar_min,
      " het slechtste jaar voor ",
      naam,
      " was met een aanwas
             van ",
      number_to_money(aanwas_min),
      " en ",
      jaar_max,
      " het beste jaar met een aanwas
             van ",
      number_to_money(aanwas_max),
      ". Voor zover er sprake is van negatieve aanwas
             wordt deze in mindering gebracht bij de positieve grondslag in een ander jaar.
             Welk jaar dat is en hoeveel ",
      naam,
      " mag verrekenen is afhankelijk van de parameters
             van het door u gekozen stelsel in het volgende luik."
    )
    
    text
    
  })
  
  
  # PLOTS
  output$plot_aanwas_2026 = renderPlotly({
    inFile = input$upload_data
    if (is.null(inFile)) {
      data = case_data()
    } else {
      data = upload_data$data
    }
    
    df = subset(data,
                omschrijving == input$micro_1_select_case_selection &
                  jaar == 2026)
    
    df = data.frame(
      x = rep(c(
        rep("Spaargeld", nrow(df)),
        rep("Financiële\nproducten", nrow(df)),
        rep("Onroerend\ngoed", nrow(df)),
        rep("Schulden", nrow(df)),
        rep("Totaal", nrow(df))
      ), 2),
      y = c(
        # vermogen
        df$spaargeld,
        df$finproduct,
        df$restbezit,
        df$schuld,
        (df$spaargeld + df$finproduct + df$restbezit - df$schuld),
        
        #rendement
        df$spaargeld * (df$spaargeld_rendperc / 100),
        df$finproduct * (df$finproduct_rendperc / 100),
        df$restbezit * (df$restbezit_rendperc / 100),
        df$schuld * (df$schuld_rendperc / 100),
        
          (df$spaargeld * (df$spaargeld_rendperc / 100)) + 
          (df$finproduct * (df$finproduct_rendperc / 100)) + 
          (df$restbezit * (df$restbezit_rendperc / 100)) - 
            (df$schuld * (df$schuld_rendperc / 100))
      ),
      z = c(rep("Vermogen", nrow(df) * 5), rep("Aanwas", nrow(df) * 5))
    ) 
    
    df$text = 
      case_when(df$y >= 0 ~ paste0(df$z, ": €", round(df$y, 0)), 
                TRUE ~ paste0(df$z, ": -€", round(abs(df$y), 0)))
    
    df$x = factor(df$x, levels = unique(df$x), ordered = T)
    
    ggplotly(
      ggplot(df) +
        geom_bar(
          stat = "identity",
          aes(
            x = x,
            y = y,
            fill = z ,
            text = text
          ),
          alpha = 0.9,
          color = "black"
        ) +
        ylab("Vermogen en aanwas in €") +
        scale_fill_manual(values = c("grey30", "grey90")) +
        theme_minimal() +
        theme(
          legend.position = 'top',
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 0)
        ),
      tooltip = "text"
    ) %>%
      layout(
        yaxis = list(showticklabels = T),
        xaxis = list(showticklabels = T),
        hovermode = "x unified",
        showlegend = F
      )
    
  })
  
  output$plot_aanwas = renderPlotly({
    if (is.null(input$upload_data)) {
      data = case_data()
    } else {
      data = upload_data$data
    }
    
    df = subset(data, omschrijving == input$micro_1_select_case_selection)
    df$jaar = as.numeric(df$jaar)
    
    if (nrow(df) > 0) {
      plot_ly(
        x = df$jaar,
        y = df$aanwas,
        type = 'scatter',
        mode = 'lines+markers',
        color = I('black'),
        name = "Aanwas",
        hoverinfo = 'y',
        hovertemplate = '€%{y}',
        showlegend = F
      ) %>%
        layout(
          yaxis = list(showticklabels = T),
          xaxis = list(showticklabels = T),
          hovermode = "x unified",
          showlegend = T
        )
    } else {
      
    }
    
  })
  
  ############ 1.2. MICRO ANALYSES - STAP 2: Welke variant wilt u doorrekenen? ###########
  
  ############ SIDEBAR ###########
  
  # KNOP VARIANT TOEVOEGEN
  observeEvent(input$add_variant, {
    # 1. als schijf 2 lager ligt dan hvi
    if (!is.na(input$schijf_2) & input$schijf_2 <= input$hvi) {
      popup(
        "Casus niet toegevoegd",
        "De ondergrens van de tweede schijf kan niet lager zijn dan of gelijk zijn aan het heffing vrij inkomen."
      )
      
      # 2. als schijf 3 lager ligt dan schijf 2 of hvi
    } else if (!is.na(input$schijf_3) &
               input$schijf_3 <= input$hvi |
               !is.na(input$schijf_3) &
               !is.na(input$schijf_2) &
               input$schijf_3 <= input$schijf_2) {
      popup(
        "Casus niet toegevoegd",
        "De ondergrens van de derde schijf kan niet lager zijn dan of gelijk zijn aan de ondergrens van schijf 2 of het heffingvrij inkomen."
      )
      
      # 3. als geen tarief
    } else if (is.na(input$tarief_1)) {
      popup("Casus niet toegevoegd", "U heeft geen tarief bepaald.")
      
      # 4. als tarief incompleet
    } else if (!is.na(input$schijf_2) &
               is.na(input$tarief_2) |
               !is.na(input$schijf_3) & is.na(input$tarief_3)) {
      popup("Casus niet toegevoegd",
            "U bent één of meerdere tarieven vergeten in te voeren.")
      
      # Als correct ingevuld
    } else {
      if (is.null(input$upload_data_variant)) {
        data = variant_data_input()
      }
      if (!is.null(input$upload_data_variant)) {
        data = upload_data_variant$data
      }
      
      if (isolate(input$naam_variant) %in% data$variant) {
        naam_variant_new = paste0(isolate(input$naam_variant), " ", nrow(data) +
                                    1)
      } else {
        naam_variant_new = isolate(input$naam_variant)
      }
      
      dat = data.frame(
        variant = naam_variant_new,
        hvi = isolate(input$hvi),
        verlies_drempel = isolate(input$verlies_drempel),
        cf = isolate(input$verlies_voor),
        cb = isolate(input$verlies_achter),
        schijf_2 = isolate(input$schijf_2),
        schijf_3 = isolate(input$schijf_3),
        tarief_1 = isolate(input$tarief_1),
        tarief_2 = isolate(input$tarief_2),
        tarief_3 = isolate(input$tarief_3)
      )
      
      # variant toevoegen
      if (is.null(input$upload_data_variant)) {
        variant_data_input() %>%  bind_rows(dat) %>% variant_data_input()
      } else {
        upload_data_variant$data = upload_data_variant$data %>%  bind_rows(dat)
      }
      
      
      # doorrekening toevoegen
      
      
    }
    
  })
  
  ############ TAB 1 ############
  
  empty_dat_variant = data.frame(
    variant = "",
    hvi = NA,
    verlies_drempel = NA,
    cf = NA,
    cb = NA,
    schijf_2 = NA,
    schijf_3 = NA,
    tarief_1 = NA,
    tarief_2 = NA,
    tarief_3 = NA
  )
  
  # KNOP DOWNLOAD TEMPLATE
  output$download_template_variant = downloadHandler(
    filename = function() {
      paste("sandbox3_template_variant.xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(empty_dat_variant, file)
    }
  )
  
  # OPLADEN DATA
  upload_data_variant = reactiveValues(data = NULL)
  
  gen_empty_data_variant = function() {
    data = empty_dat_variant %>% filter(row_number() %in% -1) %>% setNames(colnames_variant_original)
    return(data)
  }
  
  gen_upload_data_variant = function() {
    if (nrow(readxl::read_xlsx(input$upload_data_variant$datapath)) > 0) {
      upload_data_variant$data = readxl::read_xlsx(input$upload_data_variant$datapath) %>%
        setNames(colnames_variant_original)
      # generate empty data frame if file does not contain any entries
    } else {
      upload_data_variant$data = gen_empty_data_variant()
    }
  }
  
  observeEvent(input$upload_data_variant, {
    upload_data_variant$data = gen_upload_data_variant()
  })
  
  # VOORGEPROGRAMEERDE DATA
  variant_data_input = reactiveVal(
    
    data.frame(
      variant = c(
        "standaard", # enkel gedragseffecten checken
        "zonder hvi",
        "zonder vvd",
        "zonder vv",
        "maximale vv",
        "vpb"
      ),
      hvi = c(1000, 0, 1000, 1000, 1000, 0),
      verlies_drempel = c(1000, 1000, 0, 0, 0, 0),
      cf = c(9, 9, 9, 0, Inf, Inf),
      cb = c(1, 1, 1, 0, 0, 0),
      schijf_2 = c(NA, NA, NA, NA, NA, 200001),
      schijf_3 = c(NA, NA, NA, NA, NA, NA),
      tarief_1 = c(34, 34, 34, 34, 34, 19),
      tarief_2 = c(NA, NA, NA, NA, NA, 25.8),
      tarief_3 = c(NA, NA, NA, NA, NA, NA)
    )
    
  )
  
  # OUTPUT TABEL
  output$variant_data = renderDataTable({
    if (is.null(input$upload_data_variant)) {
      data = variant_data_input()
    } else {
      data = upload_data_variant$data
    }
    data %>% setNames(colnames_variant_print)
  }, server = F, rownames = F, selection = 'single', options = list(
    paging = T,
    pageLength = 16,
    scrollX = T
  ))
  
  # KNOP RESET DATA
  observeEvent(input$reset_data_variant, {
    if (is.null(input$upload_data_variant)) {
      variant_data_input() %>% filter(row_number() %in% -1) %>% variant_data_input()
    } else {
      upload_data_variant$data =  data.frame(
        variant = as.character(),
        hvi = as.numeric(),
        verlies_drempel = as.numeric(),
        cf = as.numeric(),
        cb = as.numeric(),
        schijf_2 = as.numeric(),
        schijf_3 = as.numeric(),
        tarief_1 = as.numeric(),
        tarief_2 = as.numeric(),
        tarief_3 = as.numeric()
      )
    }
    
  })
  
  # KNOP VERWIJDER VARIANT
  observeEvent(input$delete_variant, {
    if (is.null(input$upload_data_variant)) {
      remove = variant_data_input()[input$variant_data_rows_selected, "variant"]
      variant_data_input() %>%  subset(., variant != remove) %>% variant_data_input()
    } else {
      upload_data_variant$data = upload_data_variant$data[-input$variant_data_rows_selected, ]
    }
  })
  
  
  # KNOP DOWNLOAD
  selected_data_variant = function(input = NULL) {
    if (is.null(input)) {
      data = variant_data_input()
    } else {
      data = upload_data_variant$data
    }
    return(data)
  }
  
  output$download_variants = downloadHandler(
    filename = function() {
      paste("sandbox3_variants.xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(selected_data_variant(input$upload_data_variant), file)
    }
  )
  
  ############ TAB 2 ############
  
  # DATA VARIANT NAMES
  output$micro_1_select_variant = renderUI({
    if (is.null(input$upload_data_variant)) {
      data = variant_data_input()
    } else {
      data = upload_data_variant$data
    }
    selectInput(
      "micro_1_select_variant_selection",
      label = NULL,
      choices = data$variant,
      width = '100%'
    )
  })
  
  # TEKST VARIANT
  output$variant_tekst = renderText({
    if (is.null(input$upload_data_variant)) {
      dat_variant = variant_data_input()
    } else {
      dat_variant = upload_data_variant$data
    }
    
    dat_variant = subset(dat_variant,
                         variant == input$micro_1_select_variant_selection)
    
    if (nrow(dat_variant) > 0) {
      # PARAMETERS
      naam_variant = input$micro_1_select_variant_selection
      
      # heffingvrij inkomen
      hvi = dat_variant$hvi
      hvi_verschil = hvi - 1000
      
      # verlies verrekeningsdrempel
      vv_drempel = dat_variant$verlies_drempel
      vv_drempel_verschil = vv_drempel - 1000
      
      vv_cf = dat_variant$cf
      vv_cb = dat_variant$cb
      
      jaar_nu = as.numeric(input$plot_variant_jaar)
      #vv_cf_verschil = vv_cf - 9
      #vv_cb_verschil = vv_cb - 1
      
      # tarieven en schijven
      s2 = dat_variant$schijf_2
      s3 = dat_variant$schijf_3
      t1 = dat_variant$tarief_1
      t2 = dat_variant$tarief_2
      t3 = dat_variant$tarief_3
      schijf_aantal = 1
      if (!is.na(t3) &
          !is.na(s3)) {
        schijf_aantal = 3
      }
      if (is.na(t3) &
          is.na(s3) & !is.na(t2) & !is.na(s2)) {
        schijf_aantal = 2
      }
      
      # text
      text = paste0(
        "Variant <i>",
        naam_variant,
        "</i> kent een heffingvrij inkomen <i>",
        number_to_money(hvi),
        "</i>.
                    Iedereen die een inkomen uit vermogen heeft onder deze grens, betaalt geen belasting in box 3.
                    De hoogte van het heffingvrij inkomen bepaalt eveneens het aantal burgers dat een beroep kan doen op verliesverrekening.
                    Variant voorziet een verliesverrekeningsdrempel van <i>",
        number_to_money(vv_drempel),
        "</i>; <i>",
        vv_cf,
        " jaar</i> voorwaartse
                    verliesverrekening en <i>",
        vv_cb,
        " jaar</i> achterwaartse verliesverrekening. Iedere burger met
                    (1) een belastbaar inkomen uit vermogen in belastingjaar ",
        jaar_nu,
        ", d.w.z. een inkomen
                    boven het heffingvrij inkomen en (2) onverrekende verliezen uit de jaren <i>",
        jaar_nu - vv_cf,
        " tot ",
        jaar_nu + vv_cb,
        "</i>
                    kan deze in mindering brengen bij de grondslag van het belastingjaar. "
      )
      
      if (schijf_aantal == 1) {
        text = paste0(
          text,
          "Variant kent een vlaktaks. Alle belastingplichtigen betalen <i>",
          percentify(t1),
          "</i> belasting over de grondslag van het belastingjaar."
        )
      }
      if (schijf_aantal == 2) {
        text = paste0(
          text,
          "Variant kent een progressief tarief met twee schijven. Belastingplichtigen betalen <i>",
          percentify(t1),
          "</i> belasting over de grondslag tot <i>",
          number_to_money(s2),
          "</i> en <i>",
          percentify(t2),
          "</i> over de rest."
        )
      }
      if (schijf_aantal == 3) {
        text = paste0(
          text,
          "Variant kent een progressief tarief met drie schijven.Belastingplichtigen betalen <i>",
          percentify(t1),
          "</i> belasting over de grondslag tot <i>",
          number_to_money(s2),
          "</i>; <i>",
          percentify(t2),
          "</i> tot <i>",
          number_to_money(s3),
          "</i> en <i>",
          percentify(t3),
          "</i> over de rest."
        )
      }
      
      # toelichting grafiek
      text = paste0(
        text,
        "<br><br><i>De grafiek verschaft een visualisatie van de door u gespecificeerde variant
                     voor een voorbeeld belastingplichtige. Het grijze gebied boven de nul is het heffingvrij inkomen en onder de
                     nul niet verrekenbare verliezen (daar deze onder de verliesverrekeningsdrempel zitten). Het groene gebied toont
                     de aanwas die in aanmerking komt voor achterwaartse verliesverrekening en het rode gebied de aanwas die in aanmerking
                     komt voor voorwaartse verliesverrekening. Beweeg met uw muis over de jaren om te inspecteren of de aanwas van dat
                     jaar onder het heffingvrij inkomen, belastbaar inkomen, onverrekenbaar verlies, of verrekenbaar verlies valt. U kunt het
                     belastingjaar veranderen door de slider te verschuiven. </i> <br><br>"
      )
      
    } else {
      text = "Er is geen data beschikbaar. Voeg data toe met behulp van de tool. <br><br><br>"
    }
    
    text
    
  })
  
  
  # PLOT VARIANT
  output$plot_variant = renderPlotly({
    input_jaar = as.numeric(input$plot_variant_jaar)
    
    if (is.null(input$upload_data_variant)) {
      dat_variant = variant_data_input()
    } else {
      dat_variant = upload_data_variant$data
    }
    dat_variant = subset(dat_variant,
                         variant == input$micro_1_select_variant_selection)
    
    if (nrow(dat_variant) > 0) {
      # PARAMETERS
      naam_variant = input$micro_1_select_variant_selection
      hvi = dat_variant$hvi
      vv_drempel = -dat_variant$verlies_drempel
      vv_cf = input_jaar - dat_variant$cf - 0.5
      if (vv_cf < 2026) {
        vv_cf = 2026 - 0.5
      }
      vv_cb = input_jaar + dat_variant$cb + 0.5
      if (vv_cb > 2045) {
        vv_cb = 2045 + 0.5
      }
      s2 = dat_variant$schijf_2
      s3 = dat_variant$schijf_3
      
      t1 = dat_variant$tarief_1
      t2 = dat_variant$tarief_2
      t3 = dat_variant$tarief_3
      
      schijf_aantal = 1
      if (!is.na(t3) &
          !is.na(s3)) {
        schijf_aantal = 3
      }
      if (is.na(t3) &
          is.na(s3) & !is.na(t2) & !is.na(s2)) {
        schijf_aantal = 2
      }
      
      # HIER AANPASSEN!!! randomly generate case knop
      if (hvi < 1000){hvi_c = 1000} else {hvi_c = hvi}
      if (vv_drempel > -1000){vv_drempel_c = -1000} else {vv_drempel_c = vv_drempel}
      
      s2 = dat_variant$schijf_2
      s3 = dat_variant$schijf_3
      
      t1 = dat_variant$tarief_1
      t2 = dat_variant$tarief_2
      t3 = dat_variant$tarief_3
      
      schijf_aantal = 1
      if (!is.na(t3) &
          !is.na(s3)) {
        schijf_aantal = 3
      }
      if (is.na(t3) &
          is.na(s3) & !is.na(t2) & !is.na(s2)) {
        schijf_aantal = 2
      }
      
      if (hvi < 1000){hvi_c = 1000} else {hvi_c = hvi}
      if (vv_drempel > -1000){vv_drempel_c = -1000} else {vv_drempel_c = vv_drempel}
      
      aanwas = c(
        0.5 * hvi_c,
        hvi_c + 0.5 * hvi_c,
        2 * hvi_c,
        3 * hvi_c,
        4 * hvi_c,
        3 * hvi_c,
        2 * hvi_c,
        2 * vv_drempel_c,
        0.7 * vv_drempel_c,
        3 * vv_drempel_c,
        4 * hvi_c,
        1.5 * vv_drempel_c,
        0.5 * vv_drempel_c,
        1.3 * hvi,
        2 * hvi_c,
        0.2 * hvi_c,
        0.5 * hvi_c,
        0.5 * vv_drempel_c,
        1.1 * vv_drempel_c,
        0.2 * vv_drempel_c
      )
      
      if (schijf_aantal == 2) {
        aanwas[3] = s2 + 400
      }
      if (schijf_aantal == 3) {
        aanwas[3] = s3 + s2 / 2
        aanwas[4] = s3 + 200
      }
      
      jaar = c(2026:(2025 + length(aanwas)))
      
      ymax = 1.75 * max(hvi_c, vv_drempel_c, aanwas)
      ymin = -ymax
      space = 0.2 * hvi_c
      
      
      s1_text = paste0("<b>Schijf 1:</b> ",
                       percentify(t1),
                       " belasting over aanwas boven het hvi.")
      
      # PLOT
      fig = plot_ly(showlegend = F, height = 500) %>%
        
        # hvi en vv drempel
        add_polygons(
          x = c(2025.5, 2025.5, 2045.5, 2045.5),
          y = c(space, hvi, hvi, space),
          color = I("grey70"),
          opacity = 0.3,
          name = "<b>Heffingvrij inkomen:</b> aanwas is niet belastbaar en \nkan ook niet verrekend worden met verliezen.",
          hoverinfo = 'text',
          hovertemplate = '%{text}'
        ) %>%
        add_polygons(
          x = c(2025.5, 2025.5, 2045.5, 2045.5),
          y = c(vv_drempel, -space, -space, vv_drempel),
          color = I("grey70"),
          opacity = 0.3,
          name = "<b>Verlies onder verliesverrekenings drempel:</b>\nverlies kan niet verrekend worden.",
          hoverinfo = 'text',
          hovertemplate = '%{text}'
        ) %>%
        
        # verliesverrekening
        add_polygons(
          x = c(vv_cf, vv_cf, input_jaar, input_jaar),
          y = c(vv_drempel - space, ymin, ymin, vv_drempel - space),
          color = I("red"),
          opacity = 0.3,
          name = "<b>Voorwaartse verliesverrekening:</b>\nverlies onder de drempel mag verrekend worden met grondslag belastingjaar.",
          hoverinfo = 'text',
          hovertemplate = '%{text}'
        ) %>%
        add_polygons(
          x = c(input_jaar, input_jaar, vv_cb, vv_cb),
          y = c(vv_drempel - space, ymin, ymin, vv_drempel - space),
          color = I("green"),
          opacity = 0.3,
          name = "<b>Achterwaartse verliesverrekening:</b>\n toekomstig verlies onder de drempel mag verrekend worden met grondslag belastingjaar.",
          hoverinfo = 'text',
          hovertemplate = '%{text}'
        ) %>%
        
        # huidig jaar
        add_trace(
          x = input_jaar,
          y = c(ymin - 10, ymax + 10),
          opacity = 0.7,
          color = I("grey20"),
          mode = 'lines',
          hovertemplate = ''
        )
      
      # schijven
      if (schijf_aantal == 1) {
        fig = fig %>% add_polygons(
          x = c(2025.5, 2025.5, 2045.5, 2045.5),
          y = c(hvi, ymax, ymax, hvi),
          color = I("grey70"),
          opacity = 0,
          name = s1_text,
          hoverinfo = 'text',
          hovertemplate = '%{text}'
        )
      }
      if (schijf_aantal == 2) {
        s1_text = paste0(
          "<b>Schijf 1:</b> ",
          percentify(t1),
          " belasting over aanwas boven het hvi tot ",
          number_to_money(s2),
          "."
        )
        s2_text = paste0(
          s1_text,
          "\n<b>Schijf 2:</b> ",
          percentify(t2),
          " belasting over aanwas boven ",
          number_to_money(s2),
          "."
        )
        fig = fig %>%
          add_polygons(
            x = c(2025.5, 2025.5, 2045.5, 2045.5),
            y = c(hvi, s2 - 1, s2 - 1, hvi),
            color = I("grey70"),
            opacity = 0,
            name = s1_text,
            hoverinfo = 'text',
            hovertemplate = '%{text}'
          ) %>%
          add_polygons(
            x = c(2025.5, 2025.5, 2045.5, 2045.5),
            y = c(s2, ymax, ymax, s2),
            color = I("grey70"),
            opacity = 0,
            name = s2_text,
            hoverinfo = 'text',
            hovertemplate = '%{text}'
          )
      }
      if (schijf_aantal == 3) {
        s1_text = paste0(
          "<b>Schijf 1:</b> ",
          percentify(t1),
          " belasting over aanwas boven het hvi tot ",
          number_to_money(s2),
          "."
        )
        s2_text = paste0(
          s1_text,
          "\n<b>Schijf 2:</b> ",
          percentify(t2),
          " belasting over aanwas tussen ",
          number_to_money(s2),
          " en ",
          number_to_money(s3),
          "."
        )
        s3_text = paste0(
          s2_text,
          "\n<b>Schijf 3:</b> ",
          percentify(t3),
          " belasting over aanwas boven ",
          number_to_money(s3),
          "."
        )
        
        fig = fig %>%
          add_polygons(
            x = c(2025.5, 2025.5, 2045.5, 2045.5),
            y = c(hvi, s2 - 1, s2 - 1, hvi),
            color = I("grey70"),
            opacity = 0,
            name = s1_text,
            hoverinfo = 'text',
            hovertemplate = '%{text}'
          ) %>%
          add_polygons(
            x = c(2025.5, 2025.5, 2045.5, 2045.5),
            y = c(s2, s3 - 1, s3 - 1, s2),
            color = I("grey70"),
            opacity = 0,
            name = s2_text,
            hoverinfo = 'text',
            hovertemplate = '%{text}'
          ) %>%
          add_polygons(
            x = c(2025.5, 2025.5, 2045.5, 2045.5),
            y = c(s3, ymax, ymax, s3),
            color = I("grey70"),
            opacity = 0,
            name = s3_text,
            hoverinfo = 'text',
            hovertemplate = '%{text}'
          )
      }
      
      fig = fig %>%
        add_trace(
          x =  ~ jaar,
          y =  ~ aanwas,
          opacity = 0.7,
          color = I("black"),
          mode = "lines+markers",
          name = "<b>Aanwas</b>",
          hovertemplate = '%{y}'
        ) %>%
        layout(
          yaxis = list(showticklabels = F),
          hovermode = "x unified",
          showlegend = T
        )
      
      fig
    } else {
      
    }
    
  })
  
  ############# 1.3. MICRO ANALYSES - STAP 3: RESULTATEN #############
  
  ############ TAB 1 ###########
  
  gen_case_effects = function() {
    if (is.null(input$upload_data_variant)) {
      variant_data = variant_data_input()
    } else {
      variant_data = upload_data_variant$data
    }
    if (is.null(input$upload_data)) {
      case_data = case_data()
    } else {
      case_data = upload_data$data
    }
    
    varnames = c(
      "belastingplichtige",
      "variant",
      "risico",
      "belasting €",
      "belasting (% aanwas)",
      "verlies",
      "verrekend verlies",
      "verrekend verlies (% verlies)",
      "vermogen",
      "aanwas",
      "grondslag",
      "grondslag (% aanwas)",
      "spaargeld",
      "financiële producten",
      "overig bezit",
      "schuld",
      "rendement spaargeld (%)",
      "rendement financiële producten (%)",
      "rendement overig bezit (%)",
      "rendement schuld (%)",
      "hvi",
      "verlies drempel",
      "CF",
      "CB",
      "S2 €",
      "S3 €",
      "T1 %",
      "T2 %",
      "T3 %"
    )
    
    if (nrow(variant_data) > 0 & nrow(case_data) > 0) {
      temp = list()
      # elke case
      for (i in c(1:length(unique(case_data$omschrijving)))) {
        # elke variant
        for (j in c(1:nrow(variant_data))) {
          temp[[length(temp) + 1]] = gen_combi(
            dat_variant = variant_data[j, ],
            dat_case = subset(
              case_data,
              omschrijving == unique(case_data$omschrijving)[i]
            )
          )
          
        }
      }
      
      temp = do.call(rbind, temp) %>% setNames(varnames)
    } else {
      temp = variant_case_effects %>% setNames(varnames) %>% filter(., row_number() %in% -1)
    }
    
    return(temp)
    
  }
  
  # OUTPUT TABEL
  output$variant_case_effects = renderDataTable({
    gen_case_effects()
    
  }, server = F, rownames = F, selection = 'single', options = list(
    paging = T,
    pageLength = 16,
    scrollX = T
  ))
  
  
  # KNOP RESET DATA
  
  observeEvent(input$reset_variant_case_effects, {
    popup_adjusted(
      title = "Waarschuwing",
      text = "U staat op het punt alle casus en variant data te verwijderen.
        Weet u zeker dat u deze actie wil voltooien?",
      footer = tagList(
        actionButton("annuleer_reset", "Annuleer"),
        actionButton("ok_reset", "Ga door")
      )
    )
    
    observeEvent(input$ok_reset, {
      if (is.null(input$upload_data_variant)) {
        variant_data_input() %>%
          filter(row_number() %in% -1) %>%
          variant_data_input()
      } else {
        upload_data_variant$data = upload_data_variant$data %>%
          filter(row_number() %in% -1)
      }
      
      if (is.null(input$upload_data)) {
        case_data() %>%
          filter(row_number() %in% -1) %>%
          case_data()
        
      } else {
        upload_data$data = upload_data$data %>%
          filter(row_number() %in% -1)
      }
      
      removeModal()
      
    })
    
    observeEvent(input$annuleer_reset, {
      removeModal()
    })
    
  })
  
  # KNOP VERWIJDER VARIANT
  observeEvent(input$delete_variant_effects, {
    temp = gen_case_effects()
    variant_id = as.character(temp$variant[input$variant_case_effects_rows_selected])
    
    if (is.null(input$upload_data_variant)) {
      variant_data_input() %>%
        subset(variant %notin% variant_id) %>%
        variant_data_input()
    } else {
      upload_data_variant$data = upload_data_variant$data %>%
        subset(variant %notin% variant_id)
    }
    
  })
  
  # KNOP VERWIJDER CASE
  observeEvent(input$delete_case_effects, {
    temp = gen_case_effects()
    case_id = as.character(temp$belastingplichtige[input$variant_case_effects_rows_selected])
    
    if (is.null(input$upload_data)) {
      case_data() %>%
        subset(omschrijving %notin% case_id) %>%
        case_data()
      
    } else {
      upload_data$data = upload_data$data %>%
        subset(omschrijving %notin% case_id)
    }
    
  })
  
  # KNOP DOWNLOAD
  output$download_variants_case_effects = downloadHandler(
    filename = function() {
      paste("sandbox3_resultaten.xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(gen_case_effects(), file)
    }
    
  )
  
  ############ TAB 2 ###########
  
  ###### SIDEBAR ######
  
  # DATA CASE NAMES
  output$micro_3_select_case = renderUI({
    if (is.null(input$upload_data)) {
      data = case_data()
    } else {
      data = upload_data$data
    }
    data = subset(data, jaar == 2026)
    selectInput(
      "micro_3_select_case_selection",
      label = NULL,
      choices = data$omschrijving
    )
  })
  
  # DATA VARIANT NAMES
  output$micro_3_select_variant = renderUI({
    if (is.null(input$upload_data_variant)) {
      data = variant_data_input()
    } else {
      data = upload_data_variant$data
    }
    selectInput(
      "micro_3_select_variant_selection",
      label = NULL,
      choices = data$variant
    )
  })
  
  
  # KNOP DOWNLOAD
  output$download_micro = downloadHandler(
    filename = function() {
      paste("sandbox3_microvoorbeeld.xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(rbind(gen_tab_micro(), c(gen_micro_text(), "")), file)
    }
  )
  
  ###### MAIN PANEL ######
  
  # MICRO VOORBEELD TABEL
  gen_tab_micro = function() {
    # variant data
    if (is.null(input$upload_data_variant)) {
      variant_dat = variant_data_input()
    } else {
      variant_dat = upload_data_variant$data
    }
    variant_id = input$micro_3_select_variant_selection
    variant_dat = subset(variant_dat, variant == variant_id)
    
    # case data
    if (is.null(input$upload_data)) {
      case_dat = case_data()
    } else {
      case_dat = upload_data$data
    }
    
    if (nrow(variant_dat) > 0 & nrow(case_dat) > 0) {
      case_id = input$micro_3_select_case_selection
      case_dat = subset(case_dat, omschrijving == case_id)
      case_dat = verreken_verlies(
        case_dat,
        hvi = variant_dat$hvi,
        cf = variant_dat$cf,
        cb = variant_dat$cb,
        drempel = variant_dat$verlies_drempel
      )
      
      if (input$micro_3_select_year == "Alle jaren") {
        case_dat = case_dat
      } else {
        case_dat = subset(case_dat,
                          jaar == as.numeric(input$micro_3_select_year))
      }
      
      # aanwas
      if (nrow(subset(case_dat, aanwas > 0)) > 0) {
        aanwas = round(sum(subset(case_dat, aanwas > 0)$aanwas , na.rm = T), 0)
      } else {
        aanwas = 0
      }
      
      # heffing vrij inkomen
      case_dat$hvi = case_dat$aanwas - case_dat$grondslag_voor_vv
      hvi = sum(case_dat$hvi, na.rm = T)
      
      # verrekend verlies
      vv = sum((case_dat$grondslag_voor_vv - case_dat$grondslag))
      
      # belasting
      grondslag = sum(case_dat$grondslag)
      belasting = bepaal_belasting(
        grondslag,
        schijf_2 = variant_dat$schijf_2,
        schijf_3 = variant_dat$schijf_3,
        tarief_1 = variant_dat$tarief_1,
        tarief_2 = variant_dat$tarief_2,
        tarief_3 = variant_dat$tarief_3
      )
      
      # tarieven
      if (!is.na(variant_dat$tarief_1)) {
        t1 = paste0(percentify(variant_dat$tarief_1), " tarief")
        aanwas_1 = round(belasting$aanwas[1], 0)
        belasting_1 = round(belasting$belasting[1], 0)
      } else {
        t1 = "n.v.t."
        aanwas_1 = "n.v.t."
        belasting_1 = "n.v.t."
      }
      
      if (!is.na(variant_dat$tarief_2)) {
        t2 = paste0(percentify(variant_dat$tarief_2), " tarief")
        aanwas_2 = round(belasting$aanwas[2], 0)
        belasting_2 = round(belasting$belasting[2], 0)
      } else {
        t2 = "n.v.t."
        aanwas_2 = "n.v.t."
        belasting_2 = "n.v.t."
      }
      
      if (!is.na(variant_dat$tarief_3)) {
        t3 = paste0(percentify(variant_dat$tarief_3), " tarief")
        aanwas_3 = round(belasting$aanwas[3], 0)
        belasting_3 = round(belasting$belasting[3], 0)
      } else {
        t3 = "n.v.t."
        aanwas_3 = "n.v.t."
        belasting_3 = "n.v.t."
      }
      
      if (variant_dat$hvi > 0) {
        hvi_tekst = round(hvi, 0)
      } else {
        hvi_tekst = "n.v.t."
      }
      if (variant_dat$cf > 0 |
          variant_dat$cb > 0) {
        vv_tekst = round(vv, 0)
      } else {
        vv_tekst = "n.v.t."
      }
      
      
      tab  = rbind(
        c("Aanwas ...", round(aanwas, 0)),
        c("... waarvan heffingvrij inkomen", hvi_tekst),
        c("... waarvan verrekend verlies", vv_tekst),
        c("Grondslag ...", round(grondslag, 0)),
        c("... waarvan in schijf 1", aanwas_1),
        c("... waarvan in schijf 2", aanwas_2),
        c("... waarvan in schijf 3", aanwas_3),
        c("Belasting ...", round(
          sum(belasting$belasting, na.rm = T), 0
        )),
        c(paste0("... waarvan in schijf 1 (", t1, ")") , belasting_1),
        c(paste0("... waarvan in schijf 2 (", t2, ")"), belasting_2),
        c(paste0("... waarvan in schijf 3 (", t3, ")"), belasting_3)
      ) %>% data.frame() %>%
        setNames(c("", "Bedrag in €"))
      
    } else {
      empty = "Geen data beschikbaar"
      
      tab = rbind(
        c("Aanwas ...", empty),
        c("... waarvan heffingvrij inkomen", empty),
        c("... waarvan verrekenbaar verlies", empty),
        c("Grondslag ...", empty),
        c("... waarvan in schijf 1", empty),
        c("... waarvan in schijf 2", empty),
        c("... waarvan in schijf 3", empty),
        c("Belasting ...", empty),
        c(paste0(
          "... waarvan in schijf 1 (", "n.v.t.", ")"
        ) , empty),
        c(paste0(
          "... waarvan in schijf 2 (", "n.v.t.", ")"
        ), empty),
        c(paste0(
          "... waarvan in schijf 3 (", "n.v.t.", ")"
        ), empty)
      ) %>% data.frame() %>%
        setNames(c("", "Bedrag in €"))
      
    }
    
    return(tab)
  }
  
  output$tab_micro = renderDataTable({
    gen_tab_micro()
    
  }, options = list(dom = 't', pageLength = 11), rownames = F)
  
  gen_micro_text = function() {
    # variant data
    if (is.null(input$upload_data_variant)) {
      variant_dat = variant_data_input()
    } else {
      variant_dat = upload_data_variant$data
    }
    variant_id = input$micro_3_select_variant_selection
    variant_dat = subset(variant_dat, variant == variant_id)
    
    # case data
    if (is.null(input$upload_data)) {
      case_dat = case_data()
    } else {
      case_dat = upload_data$data
    }
    case_id = input$micro_3_select_case_selection
    case_dat = subset(case_dat, omschrijving == case_id)
    
    if (nrow(variant_dat) > 0 & nrow(case_dat) > 0) {
      # tarieven en schijven
      s2 = variant_dat$schijf_2
      s3 = variant_dat$schijf_3
      t1 = variant_dat$tarief_1
      t2 = variant_dat$tarief_2
      t3 = variant_dat$tarief_3
      schijf_aantal = 1
      if (!is.na(t3) &
          !is.na(s3)) {
        schijf_aantal = 3
      }
      if (is.na(t3) &
          is.na(s3) & !is.na(t2) & !is.na(s2)) {
        schijf_aantal = 2
      }
      
      # data voor alle jaren
      if (input$micro_3_select_year == "Alle jaren") {
        temp = subset(gen_case_effects(),
                      variant == variant_id &
                        belastingplichtige == case_id)
        
        case_dat$aanwas_na_hvi = case_dat$aanwas - variant_dat$hvi
        case_dat$aanwas_na_hvi[case_dat$aanwas_na_hvi < 0] = 0
        aanwas_na_hvi = sum(case_dat$aanwas_na_hvi, na.rm = T)
        
        text = paste0(
          "<b>Aanwas.</b> ",
          temp$belastingplichtige,
          " heeft gedurende de periode van 2026 tot 2045 in totaal ",
          number_to_money(temp$aanwas),
          " aanwas genoten.
                    <b>Heffingvrij inkomen.</b> Ter berekening van de belasting grondslag is voor zover van toepassing elk jaar het heffingvrij inkomen van ",
          number_to_money(temp$hvi),
          "
                    in mindering gebracht bij de aanwas. Belastingplichtige kon in totaal " ,
          number_to_money(temp$aanwas - aanwas_na_hvi) , 
          " verrekenen. Na verrekening
                    van het heffingvrij inkomen resteerde er ",
          number_to_money(aanwas_na_hvi),
          " aanwas.

                    <b>Verliesverrekening.</b> Belastingplichtige mag in deze jaren verliezen groter dan ",
          number_to_money(variant_dat$verlies_drempel),
          " verrekenen van ",
          variant_dat$cf,
          " jaar voor het belastingjaar en ",
          variant_dat$cb,
          " jaar na het belastingjaar (teruggaand tot 2026).
                      Belastingplichtige heeft in totaal ",
          number_to_money(abs(temp$verlies)),
          " verlies geleden, waarvan hij ",
          number_to_money(temp[, "verrekend verlies"]),
          " (",
          percentify(temp[, "verrekend verlies (% verlies)"]),
          ") heeft mogen verrekenen.

                      <b>Grondslag.</b>  De grondslag na verliesverrekening is daarmee gelijk aan ",
          number_to_money(temp$grondslag),
          ". Dit is ",
          percentify(temp[, "grondslag (% aanwas)"]),
          " van de totale aanwas.<br><br>"
        )
        
        if (schijf_aantal == 1) {
          text = paste0(
            text,
            "<b>Belasting.</b> Belastingplichtige betaalt over deze grondslag ",
            percentify(t1),
            " belasting. "
          )
        } else if (schijf_aantal == 2) {
          text = paste0(
            text,
            "<b>Belasting.</b> Belastingplichtige betaalt ",
            percentify(t1),
            "belastig over grondslag tot ",
            number_to_money(s2),
            " en ",
            percentify(t2),
            " over het resterend bedrag. "
          )
        } else if (schijf_aantal == 3) {
          text = paste0(
            text,
            "<b>Belasting.</b> Belastingplichtige betaalt ",
            percentify(t1),
            "belastig over grondslag tot ",
            number_to_money(s2),
            ", ",
            percentify(t2),
            " over grondslag tot ",
            number_to_money(s3),
            " en ",
            percentify(t3),
            " over het resterend bedrag. "
          )
        }
        
        text = paste0(
          text,
          "In totaal heeft belastingplichtige ",
          number_to_money(temp[, "belasting €"]),
          " betaald. Dit is ",
          percentify(temp[, "belasting (% aanwas)"]),
          " van zijn aanwas."
        )
        
      } else {
        jaar_nu = as.numeric(input$micro_3_select_year)
        
        case_dat = verreken_verlies(
          case_dat,
          hvi = variant_dat$hvi,
          cf = variant_dat$cf,
          cb = variant_dat$cb,
          drempel = variant_dat$verlies_drempel
        )
        
        case_dat_jaar = subset(case_dat, jaar == jaar_nu)
        case_dat_jaar$aanwas_na_hvi = case_dat_jaar$aanwas - variant_dat$hvi
        case_dat_jaar$aanwas_na_hvi[case_dat_jaar$aanwas_na_hvi < 0] = 0
        
        vv = case_dat_jaar$aanwas_na_hvi - case_dat_jaar$grondslag
        
        vv_dat = subset(case_dat,
                        jaar >= jaar_nu - variant_dat$cf &
                          jaar <= jaar_nu + variant_dat$cb)
        if (nrow(subset(vv_dat, aanwas < 0)) > 0) {
          verlies = sum(subset(vv_dat, aanwas < 0)$aanwas, na.rm = T)
        } else {
          verlies = 0
        }
        belasting = sum(
          bepaal_belasting(
            grondslag = case_dat_jaar$grondslag,
            schijf_2 = variant_dat$schijf_2,
            schijf_3 = variant_dat$schijf_3,
            tarief_1 = variant_dat$tarief_1,
            tarief_2 = variant_dat$tarief_2,
            tarief_3 = variant_dat$tarief_2
          )$belasting,
          na.rm = T
        )
        
        if (case_dat_jaar$aanwas > 0) {
          vv_perc = (vv / case_dat_jaar$aanwas) * 100
          belasting_perc = (belasting / case_dat_jaar$aanwas) * 100
          grondslag_perc = (case_dat_jaar$grondslag / case_dat_jaar$aanwas) *
            100
        } else {
          vv_perc = 0
          belasting_perc = 0
          grondslag_perc = 0
        }
        
        text = paste0(
          "<b>Aanwas.</b> ",
          case_dat_jaar$omschrijving,
          " heeft in ",
          jaar_nu,
          " ",
          number_to_money(case_dat_jaar$aanwas),
          " aanwas genoten.
                    <b>Heffingvrij inkomen.</b> Ter berekening van de belasting grondslag is het heffingvrij inkomen van ",
          number_to_money(variant_dat$hvi),
          "
                    in mindering gebracht bij de aanwas. Na verrekening van het heffingvrij inkomen resteerde er ",
          number_to_money(case_dat_jaar$aanwas_na_hvi),
          " aanwas.

                    <b>Verliesverrekening.</b> Belastingplichtige mag in het belastingjaar verliezen groter dan ",
          number_to_money(variant_dat$verlies_drempel),
          " verrekenen van ",
          variant_dat$cf,
          " jaar voor het belastingjaar en ",
          variant_dat$cb,
          " jaar na het belastingjaar (teruggaand tot 2026).
                      Belastingplichtige heeft in totaal ",
          number_to_money(verlies),
          " verlies geleden, waarvan hij ",
          number_to_money(vv),
          " (",
          percentify(vv_perc),
          ") heeft mogen verrekenen.

                      <b>Grondslag.</b>  De grondslag na verliesverrekening is daarmee gelijk aan ",
          number_to_money(case_dat_jaar$grondslag),
          ". Dit is ",
          percentify(grondslag_perc),
          " van de totale aanwas.<br><br>"
        )
        
        if (schijf_aantal == 1) {
          text = paste0(
            text,
            "<b>Belasting.</b> Belastingplichtige betaalt over deze grondslag ",
            percentify(t1),
            " belasting. "
          )
        } else if (schijf_aantal == 2) {
          text = paste0(
            text,
            "<b>Belasting.</b> Belastingplichtige betaalt ",
            percentify(t1),
            "belastig over grondslag tot ",
            number_to_money(s2),
            " en ",
            percentify(t2),
            " over het resterend bedrag. "
          )
        } else if (schijf_aantal == 3) {
          text = paste0(
            text,
            "<b>Belasting.</b> Belastingplichtige betaalt ",
            percentify(t1),
            "belastig over grondslag tot ",
            number_to_money(s2),
            ", ",
            percentify(t2),
            " over grondslag tot ",
            number_to_money(s3),
            " en ",
            percentify(t3),
            " over het resterend bedrag. "
          )
        }
        
        text = paste0(
          text,
          "Belastingplichtige betaalt ",
          number_to_money(belasting),
          " belasting. Dit is ",
          percentify(belasting_perc),
          " van zijn aanwas."
        )
        
        
      }
    } else {
      text = "Geen data beschikbaar. Voeg data toe in Stap 1 en Stap 2."
    }
    
    return(text)
  }
  
  # MICRO VOORBEELD TEKST
  output$micro_tekst = renderText({
    gen_micro_text()
  })
  
  # MICRO VOORBEELD PLOT
  output$plot_micro = renderPlotly({
    if (input$micro_3_select_year == "Alle jaren") {
      input_jaar = 2042
    } else {
      input_jaar = as.numeric(input$micro_3_select_year)
    }
    
    # variant data
    if (is.null(input$upload_data_variant)) {
      variant_dat = variant_data_input()
    } else {
      variant_dat = upload_data_variant$data
    }
    variant_dat = subset(variant_dat,
                         variant == input$micro_3_select_variant_selection)
    
    # case data
    if (is.null(input$upload_data)) {
      case_dat = case_data()
    } else {
      case_dat = upload_data$data
    }
    case_dat = subset(case_dat,
                      omschrijving == input$micro_3_select_case_selection)
    
    
    if (nrow(variant_dat) > 0 & nrow(case_dat) > 0) {
      # parameters
      hvi = variant_dat$hvi
      vv_drempel = -variant_dat$verlies_drempel
      vv_cf = input_jaar - variant_dat$cf - 0.5
      if (vv_cf < 2026) {
        vv_cf = 2026 - 0.5
      }
      vv_cb = input_jaar + variant_dat$cb + 0.5
      if (vv_cb > 2045) {
        vv_cb = 2045 + 0.5
      }
      s2 = variant_dat$schijf_2
      s3 = variant_dat$schijf_3
      
      t1 = variant_dat$tarief_1
      t2 = variant_dat$tarief_2
      t3 = variant_dat$tarief_3
      
      schijf_aantal = 1
      if (!is.na(t3) &
          !is.na(s3)) {
        schijf_aantal = 3
      }
      if (is.na(t3) &
          is.na(s3) & !is.na(t2) & !is.na(s2)) {
        schijf_aantal = 2
      }
      
      aanwas = case_dat$aanwas
      jaar = case_dat$jaar
      
      ymax = 1.75 * max(hvi, vv_drempel, aanwas)
      ymin = -ymax
      space = 0.01 * hvi
      
      
      s1_text = paste0("<b>Schijf 1:</b> ",
                       percentify(t1),
                       " belasting over aanwas.")
      
      # PLOT
      fig = plot_ly(showlegend = F, height = 650) %>%
        
        # hvi en vv drempel
        add_polygons(
          x = c(2025.5, 2025.5, 2045.5, 2045.5),
          y = c(space, hvi, hvi, space),
          color = I("grey70"),
          opacity = 0.3,
          name = "<b>Heffingvrij inkomen:</b> aanwas is niet belastbaar en \nkan ook niet verrekend worden met verliezen.",
          hoverinfo = 'text',
          hovertemplate = '%{text}'
        ) %>%
        add_polygons(
          x = c(2025.5, 2025.5, 2045.5, 2045.5),
          y = c(vv_drempel, -space, -space, vv_drempel),
          color = I("grey70"),
          opacity = 0.3,
          name = "<b>Verlies onder verliesverrekenings drempel:</b>\nverlies kan niet verrekend worden.",
          hoverinfo = 'text',
          hovertemplate = '%{text}'
        ) %>%
        
        # verliesverrekening
        add_polygons(
          x = c(vv_cf, vv_cf, input_jaar, input_jaar),
          y = c(vv_drempel - space, ymin, ymin, vv_drempel - space),
          color = I("red"),
          opacity = 0.3,
          name = "<b>Voorwaartse verliesverrekening:</b>\nverlies mag verrekend worden met grondslag belastingjaar.",
          hoverinfo = 'text',
          hovertemplate = '%{text}'
        ) %>%
        add_polygons(
          x = c(input_jaar, input_jaar, vv_cb, vv_cb),
          y = c(vv_drempel - space, ymin, ymin, vv_drempel - space),
          color = I("green"),
          opacity = 0.3,
          name = "<b>Achterwaartse verliesverrekening:</b>\n toekomstig verlies mag verrekend worden met grondslag belastingjaar.",
          hoverinfo = 'text',
          hovertemplate = '%{text}'
        ) %>%
        
        # huidig jaar
        add_trace(
          x = input_jaar,
          y = c(ymin - 10, ymax + 10),
          opacity = 0.7,
          color = I("grey20"),
          mode = 'lines',
          hovertemplate = ''
        )
      
      # schijven
      if (schijf_aantal == 1) {
        fig = fig %>% add_polygons(
          x = c(2025.5, 2025.5, 2045.5, 2045.5),
          y = c(hvi, ymax, ymax, hvi),
          color = I("grey70"),
          opacity = 0,
          name = s1_text,
          hoverinfo = 'text',
          hovertemplate = '%{text}'
        )
      }
      if (schijf_aantal == 2) {
        s1_text = paste0(
          "<b>Schijf 1:</b> ",
          percentify(t1),
          " belasting over aanwas boven het hvi tot ",
          number_to_money(s2),
          "."
        )
        s2_text = paste0(
          s1_text,
          "\n<b>Schijf 2:</b> ",
          percentify(t2),
          " belasting over aanwas boven ",
          number_to_money(s2),
          "."
        )
        fig = fig %>%
          add_polygons(
            x = c(2025.5, 2025.5, 2045.5, 2045.5),
            y = c(hvi, s2 - 1, s2 - 1, hvi),
            color = I("grey70"),
            opacity = 0,
            name = s1_text,
            hoverinfo = 'text',
            hovertemplate = '%{text}'
          ) %>%
          add_polygons(
            x = c(2025.5, 2025.5, 2045.5, 2045.5),
            y = c(s2, ymax, ymax, s2),
            color = I("grey70"),
            opacity = 0,
            name = s2_text,
            hoverinfo = 'text',
            hovertemplate = '%{text}'
          )
      }
      if (schijf_aantal == 3) {
        s1_text = paste0(
          "<b>Schijf 1:</b> ",
          percentify(t1),
          " belasting over aanwas boven het hvi tot ",
          number_to_money(s2),
          "."
        )
        s2_text = paste0(
          s1_text,
          "\n<b>Schijf 2:</b> ",
          percentify(t2),
          " belasting over aanwas tot ",
          number_to_money(s3),
          "."
        )
        s3_text = paste0(
          s2_text,
          "\n<b>Schijf 3:</b> ",
          percentify(t3),
          " belasting over aanwas boven ",
          number_to_money(s3),
          "."
        )
        
        fig = fig %>%
          add_polygons(
            x = c(2025.5, 2025.5, 2045.5, 2045.5),
            y = c(hvi, s2 - 1, s2 - 1, hvi),
            color = I("grey70"),
            opacity = 0,
            name = s1_text,
            hoverinfo = 'text',
            hovertemplate = '%{text}'
          ) %>%
          add_polygons(
            x = c(2025.5, 2025.5, 2045.5, 2045.5),
            y = c(s2, s3 - 1, s3 - 1, s2),
            color = I("grey70"),
            opacity = 0,
            name = s2_text,
            hoverinfo = 'text',
            hovertemplate = '%{text}'
          ) %>%
          add_polygons(
            x = c(2025.5, 2025.5, 2045.5, 2045.5),
            y = c(s3, ymax, ymax, s3),
            color = I("grey70"),
            opacity = 0,
            name = s3_text,
            hoverinfo = 'text',
            hovertemplate = '%{text}'
          )
      }
      
      fig = fig %>%
        add_trace(
          x =  ~ jaar,
          y =  ~ aanwas,
          opacity = 0.7,
          color = I("black"),
          name = "<b>Aanwas</b>",
          hovertemplate = '%{y}',
          mode = "lines+markers"
        ) %>%
        layout(
          yaxis = list(showticklabels = T),
          hovermode = "x unified",
          showlegend = T
        )
      fig
    } else {
      
    }
    
  })
  
  ########## TAB 3 ###########
  
  dat_micro_effects = reactiveValues()
  
  output$tab_microeffects = renderDataTable({
    if (is.null(input$upload_data_variant)) {
      data = variant_data_input()
    } else {
      data = upload_data_variant$data
    }
    
    if (is.null(input$upload_data)) {
      case_dat = case_data()
    } else {
      case_dat = upload_data$data
    }
    
    out = list()
    
    for (i in c(1:nrow(data))) {
      variant = data[i,]
      N = length(unique(case_dat$omschrijving))
      
      bepaal_belasting_totaal_variant = function(grondslag) {
        out = bepaal_belasting_totaal(
          grondslag,
          schijf_2 = data$schijf_2,
          schijf_3 = data$schijf_3,
          tarief_1 = data$tarief_1,
          tarief_2 = data$tarief_2,
          tarief_3 = data$tarief_3
        )
        
        return(out)
        
      }
      
      dat_vv = verreken_verlies(case_dat,
                                hvi = variant$hvi,
                                variant$cf,
                                variant$cb,
                                variant$verlies_drempel)
      
      if (N >= 3) {
        # gini grondslag
        dat_vv$grondslag_perc = 0
        dat_vv$grondslag_perc[dat_vv$aanwas > 0] = (dat_vv$grondslag[which(dat_vv$aanwas > 0)] / dat_vv$aanwas[which(dat_vv$aanwas > 0)]) * 100
        
        gini_grondslag = dat_vv %>%
          aggregate(grondslag_perc ~ omschrijving,
                    data = .,
                    FUN = mean) %>%
          dplyr::select(., "grondslag_perc") %>%
          unlist() %>%
          gini()
        gini_grondslag = unlist(gini_grondslag$Gini[1])
        if (is.nan(gini_grondslag)) {
          gini_grondslag = 0
        }
        gini_grondslag = round(gini_grondslag, 2)
        
        # gini belasting
        dat_vv$belasting = sapply(dat_vv$grondslag, FUN = bepaal_belasting_totaal_variant)
        dat_vv$belasting_perc = 0
        dat_vv$belasting_perc[dat_vv$aanwas > 0] = (dat_vv$belasting[which(dat_vv$aanwas > 0)] / dat_vv$aanwas[which(dat_vv$aanwas > 0)]) * 100
        
        gini_belasting = dat_vv %>%
          aggregate(belasting_perc ~ omschrijving,
                    data = .,
                    FUN = mean) %>%
          dplyr::select(., "belasting_perc") %>%
          unlist() %>%
          gini()
        gini_belasting = unlist(gini_belasting$Gini[1])
        if (is.nan(gini_belasting)) {
          gini_belasting = 0
        }
        gini_belasting = round(gini_belasting, 2)
        
      } else {
        gini_grondslag = "onvoldoende observaties"
        gini_belasting = "onvoldoende observaties"
      }
      
      # overbelasting
      dat_vv$belasting_max = sapply(dat_vv$grondslag_max, FUN = bepaal_belasting_totaal_variant)
      
      belasting = sum(dat_vv$belasting)
      belasting_diff = belasting - sum(dat_vv$belasting_max)
      
      overbelasting = 0
      overbelasting[belasting_diff > 0] = (belasting_diff / belasting) *
        100
      
      out[[i]] = data.frame(
        N = N,
        gini_grondslag = gini_grondslag,
        gini_belasting = gini_belasting,
        overbelasting = round(overbelasting, 2)
      ) %>%
        setNames(
          c(
            "aantal observaties",
            "grondslag ongelijkheid (0-1)",
            "belasting ongelijkheid (0-1)",
            "% extra belasting door onverrekend verlies"
          )
        )
      
    }
    
    out = do.call(rbind, out)
    out = t(out)
    colnames(out) = data$variant
    dat_micro_effects$data = out
    
    out
    
  }, server = F, rownames = T, selection = 'none', options = list(dom = 't', scrollX = T))
  
  output$tab_microwinners = renderDataTable({
    out = dat_micro_effects$data
    
    out = t(out) %>%
      data.frame() %>%
      setNames(c(
        "N",
        "gini_grondslag",
        "gini_belasting",
        "verlies_belasting"
      )) %>%
      mutate(variant = row.names(.))
    
    out = data.frame(
      winnaar = c(
        subset(out, gini_grondslag == min(out$gini_grondslag))$variant[1],
        subset(out, gini_belasting == min(out$gini_belasting))$variant[1],
        subset(out, verlies_belasting == min(out$verlies_belasting))$variant[1]
      ),
      row.names = c(
        "grondslag ongelijkheid (0-1)",
        "belasting ongelijkheid (0-1)",
        "% extra belasting door onverrekend verlies"
      )
    )
    
    out
    
  }, server = F, rownames = T, selection = 'none', options = list(dom = 't', scrollX = T))
  
  # PLOTS
  
  output$plot_micro_select_variant_1_choices = renderUI({
    selectInput(
      "plot_micro_select_variant_1",
      label = NULL,
      choices = variant_data_input()$variant
    )
  })
  
  
  output$plot_micro_variant_1 = renderPlotly({
    # variant data
    if (is.null(input$upload_data_variant)) {
      variant_dat = variant_data_input()
    } else {
      variant_dat = upload_data_variant$data
    }
    variant_id = input$plot_micro_select_variant_1
    variant_dat = subset(variant_dat, variant == variant_id)
    
    # case data
    if (is.null(input$upload_data)) {
      case_dat = case_data()
    } else {
      case_dat = upload_data$data
    }
    
    temp = list()
    for (i in c(1:length(unique(case_dat$id)))) {
      dat = subset(case_dat, id == unique(case_dat$id)[i])
      naam = dat$omschrijving[1]
      aanwas = subset(dat, jaar == 2045)$aanwas
      grondslag = subset(
        verreken_verlies(
          data = dat,
          hvi = variant_dat$hvi,
          cf = variant_dat$cf,
          cb = variant_dat$cb,
          drempel = variant_dat$verlies_drempel
        ),
        jaar == 2045
      )$grondslag
      belasting = sum(
        bepaal_belasting(
          grondslag,
          schijf_2 = variant_dat$schijf_2,
          schijf_3 = variant_dat$schijf_3,
          tarief_1 = variant_dat$tarief_1,
          tarief_2 = variant_dat$tarief_2,
          tarief_3 = variant_dat$tarief_3
        )$belasting,
        na.rm = T
      )
      
      if (aanwas > 0) {
        grondslag_perc = (grondslag / aanwas) * 100
        belasting_perc = (belasting / aanwas) * 100
      } else {
        grondslag_perc = 0
        belasting_perc = 0
      }
      
      temp[[i]] = data.frame(
        casus = rep(naam, 3),
        percentage = round(
          c(
            100 - grondslag_perc,
            grondslag_perc - belasting_perc,
            belasting_perc
          ),
          1
        ),
        type = factor(
          c("resterende aanwas", "grondslag excl belasting", "belasting"),
          levels = c("resterende aanwas", "grondslag excl belasting", "belasting"),
          ordered = T
        ),
        text = c(paste0(
          naam, " (aanwas = ", number_to_money(aanwas), ")"
        ), "", "")
      )
      
    }
    
    temp = do.call(rbind, temp)
    h = 120 * length(unique(temp$casus))
    
    fig = ggplotly(
      ggplot(data = temp) +
        geom_col(
          stat = "identity",
          aes(x = casus, y = percentage, fill = type),
          position = "stack",
          width = 0.7,
          color = "black",
          alpha = 0.8
        ) + geom_text(
          aes(x = casus, y = 50, label = text),
          nudge_x = 0.5,
          size = 4
        ) +
        scale_fill_manual(values = c("grey90", "grey70", "grey30")) +  coord_flip() + theme_void() + theme(legend.position = 'none', panel.grid = element_blank()),
      height = h
    ) %>%
      layout(
        xaxis = list(
          title = "",
          zeroline = FALSE,
          showline = FALSE,
          showticklabels = FALSE,
          showgrid = FALSE
        ),
        yaxis = list(
          title = "",
          zeroline = FALSE,
          showline = FALSE,
          showticklabels = FALSE,
          showgrid = FALSE
        )
      )
    
    fig
    
  })
  
  output$plot_micro_select_variant_2_choices = renderUI({
    selectInput(
      "plot_micro_select_variant_2",
      label = NULL,
      choices = variant_data_input()$variant, selected = "zonder hvi"
    )
  })
  
  output$plot_micro_variant_2 = renderPlotly({
    # variant data
    if (is.null(input$upload_data_variant)) {
      variant_dat = variant_data_input()
    } else {
      variant_dat = upload_data_variant$data
    }
    variant_id = input$plot_micro_select_variant_2
    variant_dat = subset(variant_dat, variant == variant_id)
    
    # case data
    if (is.null(input$upload_data)) {
      case_dat = case_data()
    } else {
      case_dat = upload_data$data
    }
    
    temp = list()
    for (i in c(1:length(unique(case_dat$id)))) {
      dat = subset(case_dat, id == unique(case_dat$id)[i])
      naam = dat$omschrijving[1]
      aanwas = subset(dat, jaar == 2045)$aanwas
      grondslag = subset(
        verreken_verlies(
          data = dat,
          hvi = variant_dat$hvi,
          cf = variant_dat$cf,
          cb = variant_dat$cb,
          drempel = variant_dat$verlies_drempel
        ),
        jaar == 2045
      )$grondslag
      belasting = sum(
        bepaal_belasting(
          grondslag,
          schijf_2 = variant_dat$schijf_2,
          schijf_3 = variant_dat$schijf_3,
          tarief_1 = variant_dat$tarief_1,
          tarief_2 = variant_dat$tarief_2,
          tarief_3 = variant_dat$tarief_3
        )$belasting,
        na.rm = T
      )
      
      if (aanwas > 0) {
        grondslag_perc = (grondslag / aanwas) * 100
        belasting_perc = (belasting / aanwas) * 100
      } else {
        grondslag_perc = 0
        belasting_perc = 0
      }
      
      temp[[i]] = data.frame(
        casus = rep(naam, 3),
        percentage = round(
          c(
            100 - grondslag_perc,
            grondslag_perc - belasting_perc,
            belasting_perc
          ),
          1
        ),
        type = factor(
          c("resterende aanwas", "grondslag excl belasting", "belasting"),
          levels = c("resterende aanwas", "grondslag excl belasting", "belasting"),
          ordered = T
        ),
        text = c(paste0(
          naam, " (aanwas = ", number_to_money(aanwas), ")"
        ), "", "")
      )
      
    }
    
    temp = do.call(rbind, temp)
    h = 120 * length(unique(temp$casus))
    
    fig = ggplotly(
      ggplot(data = temp) +
        geom_col(
          stat = "identity",
          aes(x = casus, y = percentage, fill = type),
          position = "stack",
          width = 0.7,
          color = "black",
          alpha = 0.8
        ) +
        geom_text(
          aes(x = casus, y = 50, label = text),
          nudge_x = 0.5,
          size = 4
        ) +  scale_fill_manual(values = c("grey90", "grey70", "grey30")) +  coord_flip() +  theme_void() +
        theme(legend.position = 'none', panel.grid = element_blank()),
      height = h
    ) %>%
      layout(
        xaxis = list(
          title = "",
          zeroline = FALSE,
          showline = FALSE,
          showticklabels = FALSE,
          showgrid = FALSE
        ),
        yaxis = list(
          title = "",
          zeroline = FALSE,
          showline = FALSE,
          showticklabels = FALSE,
          showgrid = FALSE
        )
      )
    
    fig
    
    
  })
  
  ################################## 2. MACRO ANALYSES ##################################
  
  
  variant_population_effects = reactiveVal(
    data.frame(
      variant = c(
        "standaard", 
        "zonder hvi",
        "zonder vvd",
        "zonder vv",
        "maximale cf",
        "vpb"
      ),
      data = c("Raming", "Raming", "Raming", "Raming", "Raming", "Raming"),
      n_belastingplichtigen = c(2.13, 7.41, 2.13, 2.13, 2.13, 7.41),
      opbrengst =  c(5.61, 6.58, 5.49, 8.41, 5.30, 3.84),
      budget_neutraliteit =  c(0.61, 1.59, 0.49, 3.41, 0.30, -1.16),
      gini_grondslag =  c(0.90, 0.42, 0.91, 0.87, 0.91, 0.44),
      gini_belasting =  c(0.95, 0.77, 0.98, 0.98, 0.98, 0.77),
      verlies_belasting =  c(1.86, 2.09, 1.11, 4.89, 1.13, 0.71),
      verlies_derving = c(3.48, 3.54, 3.63, 0, 3.85, 2.34),
      hvi_derving =  c(1.24, 0, 1.24, 1.24, 1.24, 0),
      vvd_opbrengst = c(0.6, 0.12, 0, 0, 0, 0),
      effect_n =  c(2.40, 7.98, 2.35, 2.55, 2.33, 7.38),
      effect_eur =  c(0.44, 0.65, 0.41, 1.12, 0.38, 0.19),
      hvi = c(1000, 0, 1000, 1000, 1000, 0),
      verlies_drempel = c(1000, 1000, 0, 0, 0, 0),
      cf = c(9, 9, 9, 0, Inf, Inf),
      cb = c(1, 1, 1, 0, 0, 0),
      schijf_2 = c(NA, NA, NA, NA, NA, 200001),
      schijf_3 = c(NA, NA, NA, NA, NA, NA),
      tarief_1 = c(34, 34, 34, 34, 34, 19),
      tarief_2 = c(NA, NA, NA, NA, NA, 25.8),
      tarief_3 = c(NA, NA, NA, NA, NA, NA)
    )
  )
  
  output$variant_population_effects = renderDataTable({
    
    column_names = c(
      "variant",
      "data",
      "aantal belastingplichtigen (mln.)",
      "opbrengst (mld.)",
      "Δ nieuw-oud (mld.)",
      "gini grondslag (0-1)",
      "gini belasting (0-1)",
      "opbrengst door onverrekend verlies (mld.)",
      "derving door verrekend verlies (mld.)",
      "derving door hvi (mld.)",
      "opbrengst door vvd (mld.)",
      "aantal gedragseffect (mln.)",
      "derving door gedragseffect (mld.)",
      "hvi",
      "verlies drempel",
      "CF",
      "CB",
      "S2 €",
      "S3 €",
      "T1 %",
      "T2 %",
      "T3 %"
    )
    
    variant_population_effects() %>%
      setNames(column_names)
    
    
  }, escape = F, server = F, rownames = F, selection = 'single', options = list(
    autoWidth = TRUE,
    columnDefs = list(list(
      width = '100px', targets = c(0)
    )),
    paging = T,
    pageLength = 16,
    scrollX = T
  ))
  
  
  
  # KNOP RESET DATA
  observeEvent(input$reset_variant_population_effects, {
    variant_population_effects() %>%
      filter(., row_number() %in% -1) %>%
      variant_population_effects()
  })
  
  # KNOP DELETE
  observeEvent(input$delete_variant_population_effects, {
    remove = variant_population_effects()[input$variant_population_effects_rows_selected, "variant"]
    variant_population_effects() %>% subset(., variant != remove) %>% variant_population_effects()
  })
  
  # KNOP OPSLAAN
  output$download_variants_population_effects = downloadHandler(
    filename = function() {
      paste("sandbox3_macro_results.xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(variant_population_effects(), file)
    }
  )
  
  # DATA STORAGE
  macro_data_list = reactiveValues()
  macro_data_list$data = list('standaard' = readRDS("standaard_variant.rds"),
                              'zonder hvi' = readRDS("zonder_hvi_variant.rds"), 
                              'zonder vvd' = readRDS("zonder_vvd_variant.rds"), 
                              'zonder vv' = readRDS("zonder_vv_variant.rds"), 
                              'maximale cf' = readRDS("maximale_vv_variant.rds"), 
                              'vpb' = readRDS("vpb_variant.rds"))
  
  
  newfun = function(naam_variant = naam_variant, 
                    samp_size = samp_size, 
                    hvi = hvi, 
                    verlies_drempel = verlies_drempel, 
                    cf = cf, 
                    cb = cb, 
                    schijf_2 = schijf_2, 
                    schijf_3 = schijf_3, 
                    tarief_1 = tarief_1, 
                    tarief_2 = tarief_2, 
                    tarief_3 = tarief_3) {
    
    library(plyr)
    library(dplyr)
    library(tidyverse)
    library(DT)
    library(hutils)
    library(acid)
    
    
    mld = 1000000000
    mln = 1000000
    replace_pos_by_zero = function(x){x[x>0] = 0; return(x)}
    replace_by_zero = function(x){x[x<0] = 0;  return(x)}
    round_2 = function(x){
      x = round(x, 2)
      return(x)
    }
    
    min_hvi = function(x){
      x = case_when(
        x < hvi ~ 0, 
        TRUE ~ x - hvi
      )
      
      return(x)
    }
    
    verreken_verlies_macro = function(data,
                                      hvi = hvi,
                                      cf = cf,
                                      cb = cb,
                                      drempel = verlies_drempel) {
      vvd = -1 * drempel
      
      data$hvi_use = hvi
      data$hvi_use[data$tot_rend_50 < data$hvi_use] = data$tot_rend_50[which(data$tot_rend_50 < data$hvi_use)]
      data$hvi_use[data$tot_rend_50 < 0] = 0
      
      
      for (year in 26:61) {
        data[[paste0("grondslag_", year)]] = 0
        
        # als rendement groter is dan hvi, trek hvi af van rendement
        data[which(data[[paste0("tot_rend_", year)]] > hvi),
             paste0("grondslag_", year)] =
          data[which(data[[paste0("tot_rend_", year)]] > hvi),
               paste0("tot_rend_", year)] - hvi
        
        # als rendement kleiner is dan vvd, tel vvd op bij verlies
        data[which(data[[paste0("tot_rend_", year)]] < vvd),
             paste0("grondslag_", year)] =
          data[which(data[[paste0("tot_rend_", year)]] < vvd),
               paste0("tot_rend_", year)] + abs(vvd)
        
      }
      
      data$grondslag_na_hvi_tot = rowSums(dplyr::select(data, paste0("grondslag_", 26:61)) %>%
                                            mutate_at(names(.), replace_by_zero))
      
      data$vvd_use = 0
      
      no_loss = subset(data, tot_rend_50 <= hvi)
      loss = subset(data, tot_rend_50 > hvi)
      
      rm(list = c('data'))
      gc()
      
      # voor elk belastingjaar
      for (belastingjaar in 26:61) {
        verlies_periode = c(belastingjaar - cf, belastingjaar + cb)
        
        if (verlies_periode[1] < 26) {
          verlies_periode[1] = 26
        }
        if (verlies_periode[2] > 61) {
          verlies_periode[2] = 61
        }
        
        for (verliesjaar in verlies_periode[1]:verlies_periode[2]) {
          loss$verlies = 0
          loss$verlies[# als verlies in verliesjaar
            loss[[paste0("grondslag_", verliesjaar)]] < 0 &
              # en winst in belastingjaar
              loss[[paste0("grondslag_", belastingjaar)]] > 0 &
              # en verlies is kleiner of gelijk aan winst belastingjaar
              abs(loss[[paste0("grondslag_", verliesjaar)]]) <= loss[[paste0("grondslag_", belastingjaar)]]] =
            # stel gelijk aan verlies verliesjaar
            abs(loss[which(# als verlies in verliesjaar
              loss[[paste0("grondslag_", verliesjaar)]] < 0 &
                # en winst in belastingjaar
                loss[[paste0("grondslag_", belastingjaar)]] > 0 &
                # en verlies is kleiner of gelijk aan winst belastingjaar
                abs(loss[[paste0("grondslag_", verliesjaar)]]) <= loss[[paste0("grondslag_", belastingjaar)]]),
              paste0("grondslag_", verliesjaar)])
          
          loss$verlies[# als verlies in verliesjaar
            loss[[paste0("grondslag_", verliesjaar)]] < 0 &
              # en winst in belastingjaar
              loss[[paste0("grondslag_", belastingjaar)]] > 0 &
              # en verlies is groter dan winst belastingjaar
              abs(loss[[paste0("grondslag_", verliesjaar)]]) > loss[[paste0("grondslag_", belastingjaar)]]] =
            # stel gelijk aan winst belastingjaar
            abs(loss[which(# als verlies in verliesjaar
              loss[[paste0("grondslag_", verliesjaar)]] < 0 &
                # en winst in belastingjaar
                loss[[paste0("grondslag_", belastingjaar)]] > 0 &
                # en verlies is kleiner of gelijk aan winst belastingjaar
                abs(loss[[paste0("grondslag_", verliesjaar)]]) > loss[[paste0("grondslag_", belastingjaar)]]),
              paste0("grondslag_", belastingjaar)])
          
          
          # trek verrekenbaar verlies af van grondslag belastingjaar
          loss[[paste0("grondslag_", belastingjaar)]] = loss[[paste0("grondslag_", belastingjaar)]] - loss$verlies
          
          # tel vermindering op bij verlies verliesjaar
          loss[[paste0("grondslag_", verliesjaar)]] = loss[[paste0("grondslag_", verliesjaar)]] + loss$verlies
          
          if (belastingjaar == 50) {
            # als verrekend verlies, tel vvd op bij gebruikte vvd
            loss$vvd_use[loss$verlies > 0 &
                           loss[[paste0("grondslag_", belastingjaar)]] > verlies_drempel] =
              loss$vvd_use[which(loss$verlies > 0) &
                             loss[[paste0("grondslag_", belastingjaar)]] > verlies_drempel] +
              verlies_drempel

            loss$vvd_use[loss$verlies > 0 &
                           loss[[paste0("grondslag_", belastingjaar)]] > 0 &
                           loss[[paste0("grondslag_", belastingjaar)]] <= verlies_drempel] =
              loss$vvd_use[which(loss$verlies > 0) &
                             loss[[paste0("grondslag_", belastingjaar)]] > 0 &
                             loss[[paste0("grondslag_", belastingjaar)]] <= verlies_drempel] +
              loss[which(loss$verlies > 0) &
                     loss[[paste0("grondslag_", belastingjaar)]] > 0 &
                     loss[[paste0("grondslag_", belastingjaar)]] <= verlies_drempel,
                   paste0("grondslag_", belastingjaar)]
          }
          
        } # einde verlies periode
        
      }
    
      data = rbind.fill(loss, no_loss)
      rm(list = c('loss', 'no_loss'))
      gc()
      
      for (year in 26:61) {
        data[[paste0("grondslag_", year)]] = replace_by_zero(data[[paste0("grondslag_", year)]])
      }
      
      data$grondslag_na_vv_tot = rowSums(dplyr::select(data, paste0("grondslag_", 26:61)) %>%
                                           mutate_at(names(.), replace_by_zero))

      data = data %>%
        dplyr::select(
          .,
          c(
            "id_unique",
            "factor",
            "box3_hef_oud_50",
            "heff_oud",
            "tot_rend_50",
            paste0("grondslag_", 27:60),
            "hvi_use",
            "tot_loss",
            "grondslag_na_hvi_tot",
            "grondslag_na_vv_tot",
            "vvd_use"
          )
        )
      
      return(data)
      rm(data)
      gc()
    }
    
    bepaal_belasting = function(x){
      x = case_when(
        # vlak taks
        is.na(schijf_2) ~ (tarief_1 / 100) * x,
        
        # twee schijven
        
        is.na(schijf_3) &
          !is.na(schijf_2) & x < schijf_2 ~
          (tarief_1 / 100) * x,
        
        is.na(schijf_3) &
          !is.na(schijf_2) & x >= schijf_2 ~
          ((tarief_1 / 100) * schijf_2) +
          ((tarief_2 / 100) * (x - schijf_2)
          ),
        
        # drie schijven!is.na(schijf_3) &
        x < schijf_2 ~
          (tarief_1 / 100) * x,!is.na(schijf_3) &
          x >= schijf_2 &
          x < schijf_3 ~
          ((tarief_1 / 100) * schijf_2) +
          ((tarief_2 / 100) * (x - schijf_2)
          ),!is.na(schijf_3) &
          x >= schijf_3 ~
          ((tarief_1 / 100) * schijf_2) +
          ((tarief_2 / 100) * (schijf_3 - schijf_2)
          ) +
          ((tarief_3 / 100) * (x - schijf_3)
          )
      )
      
      return(x)
    }
    
    bepaal_belasting_top_tarief = function(x){
      
      if (!is.na(tarief_1) & tarief_1 > 40){tarief_1_n = 40} else {tarief_1_n = tarief_1}
      if (!is.na(tarief_2) & tarief_2 > 40){tarief_2_n = 40} else {tarief_2_n = tarief_2}
      if (!is.na(tarief_3) & tarief_3 > 40){tarief_3_n = 40} else {tarief_3_n = tarief_3}
      
      x = case_when(
        # vlak taks
        is.na(schijf_2) ~ (tarief_1_n / 100) * x,
        
        # twee schijven
        
        is.na(schijf_3) &
          !is.na(schijf_2) & x < schijf_2 ~
          (tarief_1_n / 100) * x,
        
        is.na(schijf_3) &
          !is.na(schijf_2) & x >= schijf_2 ~
          ((tarief_1_n / 100) * schijf_2) +
          ((tarief_2_n / 100) * (x - schijf_2)
          ),
        
        # drie schijven!is.na(schijf_3) &
        x < schijf_2 ~
          (tarief_1_n / 100) * x,!is.na(schijf_3) &
          x >= schijf_2 &
          x < schijf_3 ~
          ((tarief_1_n / 100) * schijf_2) +
          ((tarief_2_n / 100) * (x - schijf_2)
          ),!is.na(schijf_3) &
          x >= schijf_3 ~
          ((tarief_1_n / 100) * schijf_2) +
          ((tarief_2_n / 100) * (schijf_3 - schijf_2)
          ) +
          ((tarief_3_n / 100) * (x - schijf_3)
          )
      )
      
      return(x)
    }
    
      
      # VOOR STEEKPROEF
      if (samp_size == "Steekproef"){
        
        data = subset(readRDS("loss.rds"))
        
        # steekproef op basis van rendement
        data = data[sample(1:nrow(data), size = 5000, prob = (abs(data$tot_rend_50) / max(data$tot_rend_50))),]
        
        # reken de rest door
        data = verreken_verlies_macro(
          data,
          hvi = hvi,
          drempel =  verlies_drempel,
          cf = cf,
          cb = cb
        ) 
        
        
        
      }
    
     # naam_variant = "vpb"; samp_size = "Raming"; hvi = 0; verlies_drempel = 0; cf = 35; cb = 0; tarief_1 = 19; tarief_2 = 25.8; tarief_3 = NA; schijf_2 = 200001; schijf_3 = NA
    
      
     # BEREKENINGEN RAMING
      if (samp_size == "Raming"){
        
        data_1 = verreken_verlies_macro(
          readRDS("loss.rds")[1:800000,],
          hvi = hvi,
          cf = cf,
          cb = cb,
          drempel =  verlies_drempel
        ) 
        
        data_1$heff_nieuw = data_1 %>%
          mutate_at(paste0("grondslag_", 27:60), bepaal_belasting) %>%
          mutate(heff_nieuw = rowMeans(dplyr::select(., paste0("grondslag_", 27:60)))) %>%
          dplyr::select(., "heff_nieuw") %>%
          unlist()
        
        data_1 = dplyr::select(data_1, -c(paste0("grondslag_", c(27:49, 51:60) )))
        
        Sys.sleep(10)
        gc()
        
        data_2 = verreken_verlies_macro(
          readRDS("loss.rds")[800001:2398123,],
          hvi = hvi,
          cf = cf,
          cb = cb,
          drempel =  verlies_drempel
        ) 
        
        data_2$heff_nieuw = data_2 %>%
          mutate_at(paste0("grondslag_", 27:60), bepaal_belasting) %>%
          mutate(heff_nieuw = rowMeans(dplyr::select(., paste0("grondslag_", 27:60)))) %>%
          dplyr::select(., "heff_nieuw") %>%
          unlist()
        
        data_2 = dplyr::select(data_2, -c(paste0("grondslag_", c(27:49, 51:60) )))
        
        Sys.sleep(10)
        gc()
        
        min_hvi = function(x){
          x[x<=hvi] = 0
          x[x>hvi] = x[x>hvi] - hvi
          return(x)
        }
        
        data_3 = readRDS("no_loss.rds") %>%
          mutate(tot_rend_50_oud = tot_rend_50) %>%
          mutate_at(paste0("tot_rend_", 26:61), min_hvi) %>%
          mutate(grondslag_na_hvi_tot = rowSums(
            dplyr::select(., paste0("tot_rend_", 26:61))
          )) %>%
          mutate(grondslag_na_vv_tot = grondslag_na_hvi_tot) %>%
          mutate(vvd_use = 0) %>%
          dplyr::select(., c("id_unique", "factor", "box3_hef_oud_50", "heff_oud", "tot_rend_50_oud", 
                             paste0("tot_rend_", 27:60), "tot_loss", 
                             "grondslag_na_hvi_tot", "grondslag_na_vv_tot", "vvd_use")) %>%
          setNames(., c("id_unique", "factor", "box3_hef_oud_50", "heff_oud", "tot_rend_50", 
                        paste0("grondslag_", 27:60), "tot_loss", 
                        "grondslag_na_hvi_tot", "grondslag_na_vv_tot", "vvd_use"))
        
        
        data_3$heff_nieuw = data_3 %>%
          mutate_at(paste0("grondslag_", 27:60), bepaal_belasting) %>%
          mutate(heff_nieuw = rowMeans(dplyr::select(., paste0("grondslag_", 27:60)))) %>%
          dplyr::select(., "heff_nieuw") %>%
          unlist()
          
        data_3 = dplyr::select(data_3, -c(paste0("grondslag_", c(27:49, 51:60) )))
        
        data_3$hvi_use = 0
        data_3$hvi_use[data_3$tot_rend_50 > hvi] = hvi
        data_3$hvi_use[data_3$tot_rend_50 <= hvi] = data_3$tot_rend_50[which(data_3$tot_rend_50 < hvi)]
        data_3$hvi_use[data_3$tot_rend_50 < 0] = 0
       
        data = rbind(data_1, data_2, data_3)
        
        rm(list = c('data_1', 'data_2', 'data_3'))
        gc()
        
        } 
      
      # aantal belastingplichtigen
      n_belastingplichtigen = round_2(sum(data[which(data$tot_rend_50 > hvi), "factor"])/mln)

      # input hvi derving
      
      data$hvi_use = bepaal_belasting(data$hvi_use)
      data$hvi_derving = data$hvi_use * data$factor
      hvi_derving = round_2(sum(data$hvi_derving) / mld)

      # belasting
      data$belasting = bepaal_belasting(data$grondslag_50)

      # gedragseffecten

      # bepaal gemiddelde nieuwe heffing
      # data$heff_nieuw = data %>%
      #   mutate_at(paste0("grondslag_", 27:60), bepaal_belasting) %>%
      #   mutate(heff_nieuw = rowMeans(dplyr::select(., paste0("grondslag_", 27:60)))) %>%
      #   dplyr::select(., "heff_nieuw") %>%
      #   unlist()

      # gedragseffect = 1 als nieuwe heffing groter is dan oude heffing
      data$gedragseffect = 0
      data$gedragseffect[data$heff_nieuw > data$heff_oud] = 1

      top_tarief = max(c(tarief_1, tarief_2, tarief_3), na.rm = T)

      if (top_tarief < 40){

        # omvang gedragseffect = 0.2 * (nieuwe heffing in 2050 en oude heffing in 2050)
        data$gedragseffect_eur = 0
        data$gedragseffect_eur[data$gedragseffect == 1] = 0.2 * (data$belasting[which(data$gedragseffect == 1)] - data$box3_hef_oud_50[which(data$gedragseffect == 1)])


      } else {

        # verschil vab met toptarief 40% en oud
        data$belasting = bepaal_belasting(data$grondslag_50)
        data$belasting_top = bepaal_belasting_top_tarief(data$grondslag_50)

        data$gedragseffect_eur = 0
        data$gedragseffect_eur[data$gedragseffect == 1] =
          # alles onder toptarief effect 20%
          (0.2 * (data$belasting_top[which(data$gedragseffect == 1)] - data$box3_hef_oud_50[which(data$gedragseffect == 1)])) +
          # alles boven toptarief effect 50%
          (0.5 * (data$belasting[which(data$gedragseffect == 1)] - data$belasting_top[which(data$gedragseffect == 1)]))


      }

      data$gedragseffect_eur = data$gedragseffect_eur * data$factor
      effect_n = round_2(sum(data[which(data$gedragseffect == 1), "factor"])/mln)
      effect_eur = round_2(sum(data$gedragseffect_eur) / mld)

      # opbrengst
      data$opbrengst = ((data$belasting * data$factor) - data$gedragseffect_eur)
      opbrengst = round_2(sum(data$opbrengst) / mld)

      # budget neutraliteit
      budget_neutraliteit = round_2((sum(data$opbrengst) - 4995000000) / mld)

      # ongelijkheid
      data$grondslag_perc = 0
      data$grondslag_perc[data$tot_rend_50 > 0] = (data$grondslag_50[which(data$tot_rend_50 > 0)] / data$tot_rend_50[which(data$tot_rend_50 > 0)]) * 100
      data$belasting_perc = 0
      data$belasting_perc[data$tot_rend_50 > 0] = (data$opbrengst[which(data$tot_rend_50 > 0)] / data$tot_rend_50[which(data$tot_rend_50 > 0)]) * 100
      gini_grondslag = round_2(acid::weighted.gini(data$grondslag_perc, data$factor)$Gini)
      gini_belasting = round_2(acid::weighted.gini(data$belasting_perc, data$factor)$Gini)

      # opbrengst door onverrekende verliezen
      data$verrekend_verlies = data$grondslag_na_hvi_tot - data$grondslag_na_vv_tot
      data$verlies_saldo = abs(data$tot_loss) - abs(data$verrekend_verlies)

        # als verlies_saldo groter dan grondslag, stel gelijk aan grondslag
        data$verlies_saldo[data$verlies_saldo >= data$grondslag_50] = data$grondslag_50[which(data$verlies_saldo >= data$grondslag_50)]
        data$verlies_belasting = bepaal_belasting(data$verlies_saldo)
        data$verlies_belasting[data$verlies_belasting > data$opbrengst] = data$opbrengst[which(data$verlies_belasting > data$opbrengst)]
        data$verlies_belasting[data$opbrengst < 0] = data$opbrengst[which(data$opbrengst < 0)]

        data$verlies_belasting = data$verlies_belasting*data$factor
        verlies_belasting =  round_2(sum(
          data$verlies_belasting
        ) / mld)

      # opbrengst door toepassing van vvd
      data$vvd_opbrengst = bepaal_belasting(data$vvd_use) * data$factor
      vvd_opbrengst = round_2(sum(data$vvd_opbrengst) / mld)

      # derving door verliesverrekening
      data$na_hvi_50 = 0
      data$na_hvi_50[data$tot_rend_50 > hvi] = data$tot_rend_50[which(data$tot_rend_50 > hvi)] - hvi
      data$vv = data$na_hvi_50 - data$grondslag_50
      data$vv = bepaal_belasting(data$vv) * data$factor
      verlies_derving = round_2(sum(data$vv)/mld)
      
      result = data.frame(
        variant = naam_variant,
        data = samp_size,
        n_belastingplichtigen = n_belastingplichtigen,
        opbrengst = opbrengst,
        budget_neutraliteit = budget_neutraliteit,
        gini_grondslag = gini_grondslag,
        gini_belasting = gini_belasting,
        verlies_belasting = verlies_belasting,
        verlies_derving = verlies_derving,
        hvi_derving = hvi_derving,
        vvd_opbrengst = vvd_opbrengst,
        effect_n = effect_n,
        effect_eur = effect_eur,
        hvi = hvi,
        verlies_drempel = verlies_drempel,
        cf = cf,
        cb = cb,
        schijf_2 = schijf_2,
        schijf_3 = schijf_3,
        tarief_1 = tarief_1,
        tarief_2 = tarief_2,
        tarief_3 = tarief_3
      )
      
      # # STORE AGGREGATED DATA IN BACKGROUND
      # data = subset(data, tot_rend_50 > 0 & opbrengst >= 0)
      data$percentiel = weighted_ntile(data$tot_rend_50, weights = data$factor, 100)
      
      # OPBRENGST (+)
      
      # vvd
      data$vvd_opbrengst = data$vvd_opbrengst
      
      # onverrekende verliezen
      data$ov_opbrengst = data$verlies_belasting - data$vvd_opbrengst
      
      # resterende opbrengst
      data$opbrengst_rest = data$opbrengst - data$vvd_opbrengst - data$ov_opbrengst
      
      
      # DERVING (-)
      
      # hvi
      data$hvi_derving = -1 * data$hvi_derving
      
      # gedragseffect
      data$gedragseffect_eur = -1 * data$gedragseffect_eur
      
      # verrekende verliezen
      data$na_hvi_50 = 0
      data$na_hvi_50[data$tot_rend_50 > hvi] = data$tot_rend_50[which(data$tot_rend_50 > hvi)] - hvi
      data$vv = data$na_hvi_50 - data$grondslag_50
      data$vv = bepaal_belasting(data$vv) * data$factor
      data$vv = -1 * data$vv
      
      # aggregeer data
      data =
        rbind(
          # opbrengst uit onverrekend verlies
          data.frame(type = "opbrengst uit belasting op onverrekende verliezen (excl. vvd)",
                     aggregate(
                       ov_opbrengst ~ percentiel,
                       data = data,
                       FUN = sum
                     )) %>%
            setNames(c("type", "percentiel", "waarde")),
          
          # opbrengst uit verliesverrekenings drempel
          data.frame(
            type = "opbrengst uit verliesverrekeningsdrempel",
            aggregate(vvd_opbrengst ~ percentiel,
                      data = data,
                      FUN = sum)
          ) %>%
            setNames(c("type", "percentiel", "waarde")),
          
          # opbrengst uit resterde aanwas
          data.frame(
            type = "opbrengst uit resterende aanwas",
            aggregate(opbrengst_rest ~ percentiel,
                      data = data,
                      FUN = sum)
          ) %>%
            setNames(c("type", "percentiel", "waarde")),
          
          # derving door hvi
          data.frame(type = "derving door hvi", aggregate(
            hvi_derving ~ percentiel, data = data, FUN = sum
          )) %>%
            setNames(c("type", "percentiel", "waarde")),
          
          # derving door gedragseffect
          data.frame(
            type = "derving door gedragseffect",
            aggregate(
              gedragseffect_eur ~ percentiel,
              data = data,
              FUN = sum
            )
          ) %>%
            setNames(c("type", "percentiel", "waarde")),
          
          # derving door verrekend verlies
          data.frame(type = "derving door verrekend verlies", aggregate(
            vv ~ percentiel, data = data, FUN = sum
          )) %>%
            setNames(c("type", "percentiel", "waarde"))
        )
      
      data$waarde = data$waarde / mld
      # 
      # ggplot(data = agg, aes(x = percentiel, y = waarde, fill = type)) +
      #   geom_col(alpha = 0.7) +
      #   xlab("Percentiel populatie o.b.v. aanwas") + ylab("Cumulatief mld. €") +
      #   theme_minimal() +
      #   theme(legend.position = 'bottom',
      #          legend.title = element_blank())

    #write_rds(agg, "vpb_variant.rds")
    #removeModal()
     
    output = list(table = result, data = data)
    
    return(output)
    
  }
  
  observeEvent(input$add_variant_macro, {
    
    if (isolate(input$naam_variant_macro) %in% variant_population_effects()$variant) {
      naam_variant_new = paste0(isolate(input$naam_variant_macro), " ", nrow(variant_population_effects()) + 1)
    } else {
      naam_variant_new = isolate(input$naam_variant_macro)
    }

  library(callr)
  # run functie op achtergrond
    x = callr::r_bg(
      func = newfun,
      supervise = TRUE, 
      args = list(naam_variant = naam_variant_new, 
                  samp_size = input$samp_size, 
                  hvi = input$hvi_macro, 
                  verlies_drempel = input$verlies_drempel_macro, 
                  cf = input$verlies_voor_macro, 
                  cb = input$verlies_achter_macro, 
                  schijf_2 = input$schijf_2_macro, 
                  schijf_3 = input$schijf_3_macro, 
                  tarief_1 = input$tarief_1_macro, 
                  tarief_2 = input$tarief_2_macro, 
                  tarief_3 = input$tarief_3_macro)
    )
    
  # houd server in leven
    withProgress(message = 'Tel met mij mee!', value = 0, {
  
     if (input$samp_size == "Raming"){maxsec = 30}else{maxsec = 10}
      
     for (sec in seq(0,maxsec,1)){
       
       part = 1 / maxsec
       Sys.sleep(sec)
       incProgress(part, detail = paste0(sec, " belastingplichtige(n) ... Ha, ha, ha.' ~ Count von Count"))
  
       if (isFALSE(x$is_alive())){
  
         output = x$get_result()
         variant_population_effects() %>%
           bind_rows(output$table) %>%
           variant_population_effects()
         
        macro_data_list$data[[naam_variant_new]] = output$data
  
         break
  
       } else {
  
  
  
       }
       
     }
     })

   
 
  
  })
  
  # TOELICHTING RESULTATEN
  
  output$macro_budget_fig = renderPlotly({
    
    #names(macro_data_list$data) = variant_population_effects()$variant
    data = macro_data_list$data[[input$macro_budget_select_variant]]
    
    p = ggplot(data = data, aes(x = percentiel, y = waarde, fill = type)) +
      geom_col(alpha = 0.7) +
      xlab("Percentiel populatie o.b.v. aanwas") + ylab("Cumulatief mld. €") +
      theme_minimal() +
      theme(legend.position = 'bottom',
            legend.title = element_blank())
    
    # plot
    ggplotly(p, height = 950) %>%
      layout(legend = list(
        orientation = "h",
        # show entries horizontally
        xanchor = "center",
        # use center of legend as anchor
        x = 0.5,
        title = list(text = '')
      ))
    
  })
  

  # SELECT VARIANT
  output$macro_budget_select_variant = renderUI({
    data = variant_population_effects()
    selectInput(
      "macro_budget_select_variant",
      label = NULL,
      choices = data$variant,
      width = '30%'
    )
  })
  
  # DATA VARIANT NAMES
  output$micro_1_select_variant_macro = renderUI({
    data = variant_population_effects()
    
    selectInput(
      "micro_1_select_variant_selection_macro",
      label = NULL,
      choices = data$variant,
      width = '100%'
    )
  })
  
  # TEKST VARIANT
  output$variant_tekst_macro = renderText({
    dat_variant = variant_population_effects()
    dat_variant = subset(dat_variant,
                         variant == input$micro_1_select_variant_selection_macro)
    
    if (nrow(dat_variant) > 0) {
      # PARAMETERS
      naam_variant = input$micro_1_select_variant_selection
      
      # heffingvrij inkomen
      hvi = dat_variant$hvi
      hvi_verschil = hvi - 1000
      
      # verlies verrekeningsdrempel
      vv_drempel = dat_variant$verlies_drempel
      vv_drempel_verschil = vv_drempel - 1000
      
      vv_cf = dat_variant$cf
      vv_cb = dat_variant$cb
      
      jaar_nu = as.numeric(input$plot_variant_jaar)
      
      # tarieven en schijven
      s2 = dat_variant$schijf_2
      s3 = dat_variant$schijf_3
      t1 = dat_variant$tarief_1
      t2 = dat_variant$tarief_2
      t3 = dat_variant$tarief_3
      schijf_aantal = 1
      if (!is.na(t3) &
          !is.na(s3)) {
        schijf_aantal = 3
      }
      if (is.na(t3) &
          is.na(s3) & !is.na(t2) & !is.na(s2)) {
        schijf_aantal = 2
      }
      
      # text
      text = paste0(
        "Variant <i>",
        naam_variant,
        "</i> kent een heffingvrij inkomen <i>",
        number_to_money(hvi),
        "</i>.
                    Iedereen die een inkomen uit vermogen heeft onder deze grens, betaalt geen belasting in box 3.
                    De hoogte van het heffingvrij inkomen bepaalt eveneens het aantal burgers dat een beroep kan doen op verliesverrekening.
                    Variant voorziet een verliesverrekeningsdrempel van <i>",
        number_to_money(vv_drempel),
        "</i>; <i>",
        vv_cf,
        " jaar</i> voorwaartse
                    verliesverrekening en <i>",
        vv_cb,
        " jaar</i> achterwaartse verliesverrekening. Iedere burger met
                    (1) een belastbaar inkomen uit vermogen in belastingjaar ",
        jaar_nu,
        ", d.w.z. een inkomen
                    boven het heffingvrij inkomen en (2) onverrekende verliezen uit de jaren <i>",
        jaar_nu - vv_cf,
        " tot ",
        jaar_nu + vv_cb,
        "</i>
                    kan deze in mindering brengen bij de grondslag van het belastingjaar. "
      )
      
      if (schijf_aantal == 1) {
        text = paste0(
          text,
          "Variant kent een vlaktaks. Alle belastingplichtigen betalen <i>",
          percentify(t1),
          "</i> belasting over de grondslag van het belastingjaar."
        )
      }
      if (schijf_aantal == 2) {
        text = paste0(
          text,
          "Variant kent een progressief tarief met twee schijven. Belastingplichtigen betalen <i>",
          percentify(t1),
          "</i> belasting over de grondslag tot <i>",
          number_to_money(s2),
          "</i> en <i>",
          percentify(t2),
          "</i> over de rest."
        )
      }
      if (schijf_aantal == 3) {
        text = paste0(
          text,
          "Variant kent een progressief tarief met drie schijven.Belastingplichtigen betalen <i>",
          percentify(t1),
          "</i> belasting over de grondslag tot <i>",
          number_to_money(s2),
          "</i>; <i>",
          percentify(t2),
          "</i> tot <i>",
          number_to_money(s3),
          "</i> en <i>",
          percentify(t3),
          "</i> over de rest."
        )
      }
      
      # toelichting grafiek
      text = paste0(
        text,
        "<br><br><i>De grafiek verschaft een visualisatie van de door u gespecificeerde variant
                     voor een voorbeeld belastingplichtige. Het grijze gebied boven de nul is het heffingvrij inkomen en onder de
                     nul niet verrekenbare verliezen (daar deze onder de verliesverrekeningsdrempel zitten). Het groene gebied toont
                     de aanwas die in aanmerking komt voor achterwaartse verliesverrekening en het rode gebied de aanwas die in aanmerking
                     komt voor voorwaartse verliesverrekening. Beweeg met uw muis over de jaren om te inspecteren of de aanwas van dat
                     jaar onder het heffingvrij inkomen, belastbaar inkomen, onverrekenbaar verlies, of verrekenbaar verlies valt. U kunt het
                     belastingjaar veranderen door de slider te verschuiven. </i> <br><br>"
      )
      
    } else {
      text = "Er is geen data beschikbaar. Voeg data toe met behulp van de tool. <br><br><br>"
    }
    
    text
    
  })
  
  # PLOT VARIANT
  output$plot_variant_macro = renderPlotly({
    input_jaar = as.numeric(input$plot_variant_jaar_macro)
    
    dat_variant = variant_population_effects()
    dat_variant = subset(dat_variant,
                         variant == input$micro_1_select_variant_selection_macro)
    
    if (nrow(dat_variant) > 0) {
      # PARAMETERS
      naam_variant = input$micro_1_select_variant_selection_macro
      if (is.na(dat_variant$cf)){dat_variant$cf = 9}
      if (is.na(dat_variant$cb)){dat_variant$cb = 3}
      
      hvi = dat_variant$hvi
      vv_drempel = -dat_variant$verlies_drempel
      vv_cf = input_jaar - dat_variant$cf - 0.5
      if (vv_cf < 2026) {
        vv_cf = 2026 - 0.5
      }
      vv_cb = input_jaar + dat_variant$cb + 0.5
      if (vv_cb > 2045) {
        vv_cb = 2045 + 0.5
      }
      
      s2 = dat_variant$schijf_2
      s3 = dat_variant$schijf_3
      
      t1 = dat_variant$tarief_1
      t2 = dat_variant$tarief_2
      t3 = dat_variant$tarief_3
      
      schijf_aantal = 1
      if (!is.na(t3) &
          !is.na(s3)) {
        schijf_aantal = 3
      }
      if (is.na(t3) &
          is.na(s3) & !is.na(t2) & !is.na(s2)) {
        schijf_aantal = 2
      }
      
      if (hvi < 1000){hvi_c = 1000} else {hvi_c = hvi}
      if (vv_drempel > -1000){vv_drempel_c = -1000} else {vv_drempel_c = vv_drempel}
      
      aanwas = c(
        0.5 * hvi_c,
        hvi_c + 0.5 * hvi_c,
        2 * hvi_c,
        3 * hvi_c,
        4 * hvi_c,
        3 * hvi_c,
        2 * hvi_c,
        2 * vv_drempel_c,
        0.7 * vv_drempel_c,
        3 * vv_drempel_c,
        4 * hvi_c,
        1.5 * vv_drempel_c,
        0.5 * vv_drempel_c,
        1.3 * hvi,
        2 * hvi_c,
        0.2 * hvi_c,
        0.5 * hvi_c,
        0.5 * vv_drempel_c,
        1.1 * vv_drempel_c,
        0.2 * vv_drempel_c
      )
      
      if (schijf_aantal == 2) {
        aanwas[3] = s2 + 400
      }
      if (schijf_aantal == 3) {
        aanwas[3] = s3 + s2 / 2
        aanwas[4] = s3 + 200
      }
      
      jaar = c(2026:(2025 + length(aanwas)))
      
      ymax = 1.75 * max(hvi_c, vv_drempel_c, aanwas)
      ymin = -ymax
      space = 0.2 * hvi_c
      
      s1_text = paste0("<b>Schijf 1:</b> ",
                       percentify(t1),
                       " belasting over aanwas boven het hvi")
      
      # PLOT
      fig = plot_ly(showlegend = F, height = 500) %>%
        
        # hvi en vv drempel
        add_polygons(
          x = c(2025.5, 2025.5, 2045.5, 2045.5),
          y = c(space, hvi, hvi, space),
          color = I("grey70"),
          opacity = 0.3,
          name = "<b>Heffingvrij inkomen:</b> aanwas is niet belastbaar en \nkan ook niet verrekend worden met verliezen.",
          hoverinfo = 'text',
          hovertemplate = '%{text}'
        ) %>%
        add_polygons(
          x = c(2025.5, 2025.5, 2045.5, 2045.5),
          y = c(vv_drempel, -space, -space, vv_drempel),
          color = I("grey70"),
          opacity = 0.3,
          name = "<b>Verlies onder verliesverrekenings drempel:</b>\nverlies kan niet verrekend worden.",
          hoverinfo = 'text',
          hovertemplate = '%{text}'
        ) %>%
        
        # verliesverrekening
        add_polygons(
          x = c(vv_cf, vv_cf, input_jaar, input_jaar),
          y = c(vv_drempel - space, ymin, ymin, vv_drempel - space),
          color = I("red"),
          opacity = 0.3,
          name = "<b>Voorwaartse verliesverrekening:</b>\nverlies mag verrekend worden met grondslag belastingjaar.",
          hoverinfo = 'text',
          hovertemplate = '%{text}'
        ) %>%
        add_polygons(
          x = c(input_jaar, input_jaar, vv_cb, vv_cb),
          y = c(vv_drempel - space, ymin, ymin, vv_drempel - space),
          color = I("green"),
          opacity = 0.3,
          name = "<b>Achterwaartse verliesverrekening:</b>\n toekomstig verlies mag verrekend worden met grondslag belastingjaar.",
          hoverinfo = 'text',
          hovertemplate = '%{text}'
        ) %>%
        
        # huidig jaar
        add_trace(
          x = input_jaar,
          y = c(ymin - 10, ymax + 10),
          opacity = 0.7,
          color = I("grey20"),
          mode = 'lines',
          hovertemplate = ''
        )
      
      # schijven
      if (schijf_aantal == 1) {
        fig = fig %>% add_polygons(
          x = c(2025.5, 2025.5, 2045.5, 2045.5),
          y = c(hvi, ymax, ymax, hvi),
          color = I("grey70"),
          opacity = 0,
          name = s1_text,
          hoverinfo = 'text',
          hovertemplate = '%{text}'
        )
      }
      if (schijf_aantal == 2) {
        s1_text = paste0(
          "<b>Schijf 1:</b> ",
          percentify(t1),
          " belasting over aanwas boven het hvi tot ",
          number_to_money(s2),
          "."
        )
        s2_text = paste0(
          s1_text,
          "\n<b>Schijf 2:</b> ",
          percentify(t2),
          " belasting over aanwas boven ",
          number_to_money(s2),
          "."
        )
        fig = fig %>%
          add_polygons(
            x = c(2025.5, 2025.5, 2045.5, 2045.5),
            y = c(hvi, s2 - 1, s2 - 1, hvi),
            color = I("grey70"),
            opacity = 0,
            name = s1_text,
            hoverinfo = 'text',
            hovertemplate = '%{text}'
          ) %>%
          add_polygons(
            x = c(2025.5, 2025.5, 2045.5, 2045.5),
            y = c(s2, ymax, ymax, s2),
            color = I("grey70"),
            opacity = 0,
            name = s2_text,
            hoverinfo = 'text',
            hovertemplate = '%{text}'
          )
      }
      if (schijf_aantal == 3) {
        s1_text = paste0(
          "<b>Schijf 1:</b> ",
          percentify(t1),
          " belasting over aanwas tot ",
          number_to_money(s2),
          "."
        )
        s2_text = paste0(
          s1_text,
          "\n<b>Schijf 2:</b> ",
          percentify(t2),
          " belasting over aanwas tot ",
          number_to_money(s3),
          "."
        )
        s3_text = paste0(
          s2_text,
          "\n<b>Schijf 3:</b> ",
          percentify(t3),
          " belasting over aanwas boven ",
          number_to_money(s3),
          "."
        )
        
        fig = fig %>%
          add_polygons(
            x = c(2025.5, 2025.5, 2045.5, 2045.5),
            y = c(hvi, s2 - 1, s2 - 1, hvi),
            color = I("grey70"),
            opacity = 0,
            name = s1_text,
            hoverinfo = 'text',
            hovertemplate = '%{text}'
          ) %>%
          add_polygons(
            x = c(2025.5, 2025.5, 2045.5, 2045.5),
            y = c(s2, s3 - 1, s3 - 1, s2),
            color = I("grey70"),
            opacity = 0,
            name = s2_text,
            hoverinfo = 'text',
            hovertemplate = '%{text}'
          ) %>%
          add_polygons(
            x = c(2025.5, 2025.5, 2045.5, 2045.5),
            y = c(s3, ymax, ymax, s3),
            color = I("grey70"),
            opacity = 0,
            name = s3_text,
            hoverinfo = 'text',
            hovertemplate = '%{text}'
          )
      }
      
      fig = fig %>%
        add_trace(
          x =  ~ jaar,
          y =  ~ aanwas,
          opacity = 0.7,
          color = I("black"),
          line = list(dash = 'dash'),
          name = "<b>Aanwas</b>",
          hovertemplate = '%{y}'
        ) %>%
        layout(
          yaxis = list(showticklabels = F),
          hovermode = "x unified",
          showlegend = T
        )
      
      fig
    } else {
      
    }
    
  })
  
 
  
  # BUDGET TEKST
  output$macro_budget_tekst = renderText({
    case_data = macro_data_list$data[[input$macro_budget_select_variant]]
    variant_data = variant_data = subset(variant_population_effects(),
                                         variant == input$macro_budget_select_variant)
    
    schijf_aantal = 1
    if (!is.na(variant_data$tarief_3) &
        !is.na(variant_data$schijf_3)) {
      schijf_aantal = 3
    }
    if (is.na(variant_data$tarief_3) &
        is.na(variant_data$schijf_3) &
        !is.na(variant_data$tarief_2) &
        !is.na(variant_data$schijf_2)) {
      schijf_aantal = 2
    }
    
    if (variant_data$cf == Inf) {variant_data$cf = "onbeperkt aantal"}
    
    text = paste0(
      "

                  <b>Omschrijving</b>

                  <br><br>

                  Variant ",
      variant_data$variant,
      " kent een heffingvrij inkomen van ",
      number_to_money(variant_data$hvi),
      ",
                  een verliesverrekeningsdrempel van ",
      number_to_money(variant_data$verlies_drempel),
      ", ",
      variant_data$cf,
      " jaar voorwaartse verliesverrekening en ",
      variant_data$cb,
      " jaar achterwaartse verliesverrekening.
                  "
    )
    
    if (schijf_aantal == 1) {
      text = paste0(
        text,
        "Variant kent een vlaktaks. Alle belastingplichtigen betalen ",
        percentify(variant_data$tarief_1),
        " belasting over de grondslag van het belastingjaar."
      )
    }
    
    if (schijf_aantal == 2) {
      text = paste0(
        text,
        "Variant kent een progressief tarief met twee schijven. Belastingplichtigen betalen ",
        percentify(variant_data$tarief_1),
        " belasting over de grondslag tot ",
        number_to_money(variant_data$schijf_2),
        " en ",
        percentify(variant_data$tarief_2),
        " over de rest."
      )
    }
    
    if (schijf_aantal == 3) {
      text = paste0(
        text,
        "Variant kent een progressief tarief met drie schijven.Belastingplichtigen betalen ",
        percentify(variant_data$tarief_1),
        " belasting over de grondslag tot ",
        number_to_money(variant_data$schijf_2),
        "; ",
        percentify(variant_data$tarief_2),
        " tot ",
        number_to_money(variant_data$schijf_3),
        " en ",
        percentify(variant_data$tarief_3),
        " over de rest."
      )
    }
    
    text = paste0(text, "<br><br>")
    
    # OPBRENGST
    text = paste0(
      text,
      "<b>Opbrengst</b>
                  <br><br>
                  Onder opgegeven variant betalen ",
      variant_data$n_belastingplichtigen,
      " mln. burgers belasting.
                  In totaal betalen deze burgers naar schatting ",
      number_to_money(variant_data$opbrengst),
      " mld. belasting.
                  Circa ",
      number_to_money(abs(sum(
        subset(
          case_data,
          type == "opbrengst uit belasting op onverrekende verliezen (excl. vvd)" |
          type == "opbrengst uit verliesverrekeningsdrempel" 
        )$waarde
      ))),
      " mld. van deze opbrengst is het gevolg van
      restricties op de verliesverrekening (zie 'opbrengst uit belasting op onverrekende verliezen' in grafiek), 
      waarvan ", 
      number_to_money(abs(sum(
        subset(
          case_data,
          type == "opbrengst uit verliesverrekeningsdrempel" 
        )$waarde
      ))),
      " mld. door de toepassing van een verliesverrekeningsdrempel. 
      
      Onder opgegeven variant is de top 1% van de burgers met de hoogste aanwas verantwoordelijk voor ",
      percentify((
        sum(
          subset(
            case_data,
            percentiel == 100 &
              type == "opbrengst uit belasting op onverrekende verliezen (excl. vvd)" |
              percentiel == 100 &
              type == "opbrengst uit verliesverrekeningsdrempel" | 
              percentiel == 100 &
              type == "opbrengst uit resterende aanwas"
          )$waarde
        ) /
          variant_data$opbrengst
      ) * 100),
      "; de top 10% voor ",
      percentify((
        sum(
          subset(
            case_data,
            percentiel >= 90  &
              type == "opbrengst uit belasting op onverrekende verliezen (excl. vvd)" |
              percentiel >= 90 &
              type == "opbrengst uit verliesverrekeningsdrempel" | 
              percentiel >= 90  &
              type == "opbrengst uit resterende aanwas"
          )$waarde
        ) /
          variant_data$opbrengst
      ) * 100),
      "; en de top 25% voor ",
      percentify((
        sum(
          subset(
            case_data,
            percentiel >= 75  &
              type == "opbrengst uit belasting op onverrekende verliezen (excl. vvd)" |
              percentiel >= 75 &
              type == "opbrengst uit verliesverrekeningsdrempel" | 
              percentiel >= 75  &
              type == "opbrengst uit resterende aanwas"
          )$waarde
        ) /
          variant_data$opbrengst
      ) * 100),
      
      " van de opbrengst.
                  <br><br>"
    )
    
    # DERVING
    text = paste0(
      text,
      "<b>Derving</b>
                  <br><br>
                  De toepassing van het door u bepaald heffingvrij inkomen resulteert in een budgettaire derving van " ,
      number_to_money(variant_data$hvi_derving),
      " mld.
                  De toepassing van verliesverrekening resulteert daarnaast in een bijkomende ",
      number_to_money(variant_data$verlies_derving),
      " mld. derving. Hoe hoger de aanwas, hoe meer burgers profiteren van verliesverrekening.
                  Zo is de top 1% van de burgers met de hoogste aanwas verantwoordelijk voor ",
      percentify(abs((
        sum(
          subset(
            case_data,
            percentiel == 100 &
              type == "derving door verrekend verlies"
          )$waarde
        )
      ) /
        abs(
          sum(
            subset(case_data, type == "derving door verrekend verlies")$waarde
          )
        )) * 100),
      " van de derving; de top 10% voor ",
      percentify(abs((
        sum(
          subset(
            case_data,
            percentiel >= 90 &
              type == "derving door verrekend verlies"
          )$waarde
        )
      ) /
        abs(
          sum(
            subset(case_data, type == "derving door verrekend verlies")$waarde
          )
        )) * 100),
      "; en de top 25% voor ",
      percentify(abs((
        sum(
          subset(
            case_data,
            percentiel >= 75 &
              type == "derving door verrekend verlies"
          )$waarde
        )
      ) /
        abs(
          sum(
            subset(case_data, type == "derving door verrekend verlies")$waarde
          )
        )) * 100)
      ,
      " van de derving. Onder de opgegeven variant gaan ",
      variant_data$effect_n ,
      " mln.
                  burgers meer belasting betalen dan onder de voormalige wetgeving.
                  Deze burgers zullen hierdoor een stuk van hun vermogen anders benutten. Dit gedragseffect zal naar verwachting ",
      number_to_money(variant_data$effect_eur)
      ,
      " mld. derving opleveren.
                  <br><br>"
    )
    
    text
    
  })
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
