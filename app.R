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
library(sendmailR)
library(mailR)
library(shinyFiles)

# SET BASE MULTIPLIERS 

risico = 2
sd = 0.2
sd_rend = 0.5*risico*sd

# FUNCTIONS

# popups
warning = function(title, text){showModal(modalDialog(title = title, text, easyClose = TRUE))}

popup = function(title, text){
  showModal(modalDialog(title = title, text, footer = actionButton(inputId = "close_window", label = "Sluiten")))
  observeEvent(input$close_window, {removeModal()})
}

popup_adjusted = function(title, text, footer){showModal(modalDialog(title = title, text, footer = footer))}

# negatie %in%
`%notin%` <- Negate(`%in%`) 

# functie om aanwas in overbruggingswetgeving te bepalen
overbrug_me = function(spaargeld, finproduct, restbezit, schuld, type = "aanwas voor hvv"){
  
  schuld = schuld - 3400
  if (schuld < 0){schuld = 0}
  
  # voor toerekening van het hvv
  vermogen = spaargeld + finproduct + restbezit - schuld
  aanwas = 0.0036*spaargeld + 0.0617*finproduct + 0.0617*restbezit - 0.0257*schuld
  if (type == "aanwas voor hvv"){return(aanwas)}
  if (type == "vermogen voor hvv"){return(grondslag)}
  
  # na toerekening van het hvv
  hvv = 57000
  if (vermogen > 0) {prp = hvv / vermogen} else {prp = 1}
  if (prp > 1){prp = 1}
  vermogen_hvv = (spaargeld - (prp*spaargeld)) + (finproduct - (prp*finproduct)) + (restbezit - (prp*restbezit)) - (schuld - (prp*schuld))
  aanwas_hvv = 0.0036*(spaargeld - (prp*spaargeld)) + 0.0617*(finproduct - (prp*finproduct)) + 0.0617*(restbezit - (prp*restbezit)) - 0.0257*(schuld - (prp*schuld))
  
  if (type == "aanwas na hvv"){return(aanwas_hvv)}
  if (type == "vermogen na hvv"){return(vermogen_hvv)}
  
}

# function to draw value from normal distribution
gen_value = function(mean, sd = 0.2, n = 1){x = rnorm(n = n, mean = mean, sd = sd); return(x)}

# update value last year 
update_value = function(current_value, perc_change){new_value = current_value + (current_value * (perc_change / 100)); return(new_value)}

# function to generate history for given input 
gen_history = function(row, sd_rend, crisis = "nee"){
  
  aanwas_forfait = overbrug_me(row$spaargeld[1], row$findproduct[1], row$restbezit[1], row$schuld[1])
  
  data_list = list()
  data_list_2 = list()
  
  if (row$risico == 0){sd = 0; sd_rend = 0}
  
  if (row$spaargeld < 0 | is.na(row$spaargeld)){row$spaargeld = 0}
  if (row$finproduct < 0 | is.na(row$finproduct)){row$finproduct = 0}
  if (row$restbezit < 0 | is.na(row$restbezit)){row$restbezit = 0}
  if (row$schuld < 0 | is.na(row$schuld)){row$schuld = 0}
  
  data_list[[1]] = cbind(
    row, data.frame(
      spaargeld_forfait = 0.0036*row$spaargeld, finproduct_forfait = 0.0617*row$finproduct,
      restbezit_forfait = 0.0617*row$restbezit, schuld_forfait = 0.0257*row$schuld)) %>%
    mutate(aanwas_forfait = spaargeld_forfait + finproduct_forfait + restbezit_forfait - schuld_forfait)
  
  for (i in c(2:20)){
    
    jaar = c(2026:2045)[i]
    vorig_jaar = data_list[[i-1]] 
    
    dat1 = data.frame(
      id = row$id, jaar = jaar, omschrijving = vorig_jaar$omschrijving, risico = vorig_jaar$risico, 
      spaargeld = update_value(vorig_jaar$spaargeld, vorig_jaar$spaargeld_rendperc),
      finproduct = update_value(vorig_jaar$finproduct, vorig_jaar$finproduct_rendperc),
      restbezit = update_value(vorig_jaar$restbezit, vorig_jaar$restbezit_rendperc),
      schuld = vorig_jaar$schuld,
      spaargeld_rendperc = row$spaargeld_rendperc, 
      finproduct_rendperc = gen_value(row$finproduct_rendperc, sd_rend, n = 1),
      restbezit_rendperc = gen_value(row$restbezit_rendperc, sd, n = 1),
      schuld_rendperc = gen_value(row$schuld_rendperc, sd, n = 1)) 
    
    dat2 = data.frame(
      spaargeld_forfait = (0.0036*dat1$spaargeld), finproduct_forfait = (0.0617*dat1$finproduct),
      restbezit_forfait = (0.0617*dat1$restbezit), schuld_forfait = (0.0257*dat1$schuld)) %>%
      mutate(aanwas_forfait = spaargeld_forfait + finproduct_forfait + restbezit_forfait - schuld_forfait)
    
    data_list[[i]] = cbind(dat1, dat2)  
    
  }
  
  data = do.call(rbind, data_list) %>%
    mutate(vermogen = spaargeld + finproduct + restbezit - schuld) %>%
    mutate(spaargeld_aanwas = spaargeld * (spaargeld_rendperc/100)) %>% 
    mutate(finproduct_aanwas = finproduct * (finproduct_rendperc/100)) %>% 
    mutate(restbezit_aanwas = restbezit * (restbezit_rendperc/100)) %>% 
    mutate(schuld_aanwas = schuld * (schuld_rendperc/100)) 
  
  if (crisis == "ja"){
    startperc = data$finproduct_rendperc[which(data$jaar == 2035)]/100
    startsaldo = data$finproduct_aanwas[which(data$jaar == 2035)]
    
    data$finproduct_aanwas[data$jaar == 2036] = startsaldo - 50*startsaldo*startperc
    data$finproduct_aanwas[data$jaar == 2037] = startsaldo - 45*startsaldo*startperc
    data$finproduct_aanwas[data$jaar == 2038] = startsaldo - 30*startsaldo*startperc
    data$finproduct_aanwas[data$jaar == 2039] = startsaldo - 15*startsaldo*startperc}
  
  # VOEG TOE AANTAL JAREN CRISIS
  
  data = data %>% 
    mutate(aanwas = spaargeld_aanwas + finproduct_aanwas + restbezit_aanwas - schuld_aanwas) %>% 
    mutate_at(vars(-id, -omschrijving), funs(round(., 1))) %>%
    arrange(id, jaar)
  
  return(data)
}

# function to calculate carry back/forward losses

verreken_verlies = function(data, hvi, cf = 0, cb = 0, drempel = 0){
  
  drempel = -1*drempel
  
  data$grondslag = case_when(
    data$aanwas < drempel ~ data$aanwas,
    data$aanwas > drempel & data$aanwas <= hvi ~ 0,
    data$aanwas > hvi ~ data$aanwas - hvi       
  )
  
  data$cf = 0
  data$cb = 0
  
  for (y in c(1:19)){
    
    year = c(2027:2045)[y]
    
    # voorwaartse verliesverrekening
    if (data$grondslag[data$jaar == year] > 0 & cf > 0){
      
      cf_jaar = year - cf; if (cf_jaar < 2026){cf_jaar = 2026}
      cf_data = subset(data, jaar >= cf_jaar & jaar <= year - 1)
      
      for (i in c(1:nrow(cf_data))){
        cf_row = cf_data[i,]
        if (cf_row$grondslag < drempel){
          
          # als verlies kleiner is dan restant grondslag 
          if (abs(data$grondslag[data$jaar == cf_row$jaar]) <= data$grondslag[which(data$jaar == year)] & 
              data$grondslag[data$jaar == cf_row$jaar] != 0){
            
            # trek volledig verlies jaar af van grondslag huidig jaar
            data$grondslag[data$jaar == year] = data$grondslag[which(data$jaar == year)] - abs(cf_row$grondslag)
            
            # tel verlies op bij cf variabele
            data$cf[data$jaar == year] = data$cf[data$jaar == year] + abs(cf_row$grondslag)
            
            # zet resterende grondslag cf jaar gelijk aan nul zodat deze niet opnieuw meegerekend wordt
            data$grondslag[data$jaar == cf_row$jaar] = 0
            
            # als verlies groter is dan restant grondslag    
          } else if (abs(data$grondslag[data$jaar == cf_row$jaar]) >= data$grondslag[which(data$jaar == year)]) {
            
            # bepaal resterende grondslag 
            rest = data$grondslag[data$jaar == year]
            
            # zet grondslag huidig jaar gelijk aan nul 
            data$grondslag[data$jaar == year] = 0
            
            # tel rest bedrag op bij grondslag cf jaar
            if (data$grondslag[data$jaar == cf_row$jaar] < -rest){data$grondslag[data$jaar == cf_row$jaar] = data$grondslag[data$jaar == cf_row$jaar] + rest} else {data$grondslag[data$jaar == cf_row$jaar] = 0}
            
            # tel restbedrag op bij cf bedrag 
            data$cf[data$jaar == year] = data$cf[data$jaar == year] + rest   
          }}}
      
      # achterwaartse verliesverrekening
    } else if (data$grondslag[data$jaar == year] < drempel & cb > 0){
      
      cb_jaar = year - cb; if (cb_jaar < 2026){cb_jaar = 2026}
      cb_data = subset(data, jaar >= cb_jaar & jaar <= year - 1)
      
      for (i in c(1:nrow(cb_data))){
        
        cb_row = cb_data[i,]
        
        # als grondslag cb groter is dan nul 
        if (cb_row$grondslag > hvi){
          
          # als winst cb jaar kleiner is dan of gelijk is aan verlies huidig jaar 
          if (data$grondslag[data$jaar == cb_row$jaar] <= abs(data$grondslag[which(data$jaar == year)]) & 
              data$grondslag[data$jaar == cb_row$jaar] != 0){
            
            # tel grondslag cb jaar op bij restant verlies huidig jaar
            data$grondslag[data$jaar == year] = data$grondslag[data$jaar == year] + data$grondslag[data$jaar == cb_row$jaar]
            
            # tel grondslag cb op bij cb variabele huidig jaar
            data$cb[data$jaar == year] = data$cb[data$jaar == year] + data$grondslag[data$jaar == cb_row$jaar]
            
            # stel cb jaar grondslag gelijk aan nul
            data$grondslag[data$jaar == cb_row$jaar] = 0
            
            # winst cb jaar groter is dan restant verlies huidig jaar    
          } else if (data$grondslag[data$jaar == cb_row$jaar] > abs(data$grondslag[which(data$jaar == year)])) {
            
            # bepaal resterend verlies  
            rest = abs(data$grondslag[data$jaar == year])
            
            # trek resterend verlies af van winst cb jaar
            data$grondslag[data$jaar == cb_jaar] = data$grondslag[data$jaar == cb_jaar] - rest
            
            # zet grondslag huidig jaar gelijk aan nul 
            data$grondslag[data$jaar == year] = 0
            
            # tel verrekening op bij cb variabele 
            data$cb[data$jaar == year] = data$cb[data$jaar == year] + rest
            
          }}
        
      }}
    
  } # einde year loop
  
  return(data)
  
}

# functie om belasting te bepalen voor een bepaalde grondslag 
bepaal_belasting = function(grondslag, schijf_2 = NA, schijf_3 = NA, tarief_1 = 34, tarief_2 = NA, tarief_3 = NA){
  
  if (grondslag > 0){
    # Schijf 3
    if(!is.na(schijf_3)){schijf_3 = grondslag - schijf_3} else {schijf_3 = 0}
    if(schijf_3 < 0){schijf_3 = 0}
    if(!is.na(tarief_3)){belasting_3 = schijf_3 * (tarief_3/100)} else {belasting_3 = NA}
    
    # Schijf 2
    if(!is.na(schijf_2)){schijf_2 = grondslag - schijf_3 - schijf_2} else {schijf_2 = 0}
    if(schijf_2 < 0){schijf_2 = 0}
    if(!is.na(tarief_2)){belasting_2 = schijf_2 * (tarief_2/100)} else {belasting_2 = NA}
    
    # Schijf 1 
    schijf_1 = grondslag - schijf_3 - schijf_2
    if(schijf_1 < 0){schijf_1 = 0}
    if(!is.na(tarief_1)){belasting_1 = schijf_1 * (tarief_1/100)} else {belasting_1 = NA}
    
    out = data.frame(schijf = c(1:3), aanwas = c(schijf_1, schijf_2, schijf_3), belasting = c(belasting_1, belasting_2, belasting_3))
  } else {
    out = data.frame(schijf = c(1:3), aanwas = c(0,0,0), belasting = c(0,0,0))
  }
  
  return(out)
}

# functie om getal om te zetten in bedrag in €
number_to_money = function(number){
  options(scipen=999)
  number = round(number, digits = 2)
  if (number >= 0){number = paste0("€", number)} else {number = paste0("-€", abs(number))}
  return(number)
}

# functie om getal om te zetten naar percentage 
percentify = function(number){
  number = paste0(round(number, digits = 2), "%")
  return(number)  
}

# functie om aanwas te berekenen gegeven vermogen en rendement
bereken_aanwas = function(vermogen, rendement){
  if (rendement != 0){
    aanwas =  vermogen * (rendement / 100)
  } else {
    aanwas = 0
  }
  return(aanwas)
}

# functie om objectieve variant statistieken te genereren
test = select(readxl::read_xlsx("testdata.xlsx"),c("id", "jaar", "aanwas")) 

calculate_variant_stats_macro = function(data = test, hvi = 1000, vv_drempel = 1000, cf = 9, cb = 1, s2 = NA, s3 = NA, t1 = 34, t2 = NA, t3 = NA){
  
  # test data
  data_new = list()
  data_ideal = list()
  for (i in 1:length(unique(data$id))){
    
    data_id = unique(data$id)[i]
    temp = verreken_verlies(subset(data, id == data_id), hvi = hvi, cf = cf, cb = cb, drempel = vv_drempel)
    temp_ideal = verreken_verlies(subset(data, id == data_id), hvi = hvi, cf = 20, cb = 20, drempel = 0)
    
    temp$belasting = NA
    temp_ideal$belasting = NA
    for (j in 1:nrow(temp)){
      temp$belasting[j] = sum(bepaal_belasting(temp$grondslag[j], schijf_2 = s2, schijf_3 = s3, tarief_1 = t1, tarief_2 = t2, tarief_3 = t3)$belasting, na.rm = T)
      temp_ideal$belasting[j] = sum(bepaal_belasting(temp_ideal$grondslag[j], schijf_2 = s2, schijf_3 = s3, tarief_1 = t1, tarief_2 = t2, tarief_3 = t3)$belasting, na.rm = T)
    }
    
    data_new[[i]] = temp
    data_ideal[[i]] = temp_ideal
    
  }
  
  data_new = do.call(rbind, data_new)
  data_new_agg = aggregate(.~id, data_new, sum)
  data_ideal = do.call(rbind, data_ideal)
  
  # budgettaire raming 
  # HIER CORRIGEREN
  if (data_new_agg > 0){scale = 12000000 / nrow(data_new_agg)} else {scale = NA}
  budget_raming = round(sum(data_new$belasting, na.rm = T)*scale / 1000000000, 2)
  
  # grondslag (on)gelijkheid
  data_new$grondslag_perc = 0
  data_new$grondslag_perc[data_new$aanwas > 0] = (data_new$grondslag[which(data_new$aanwas > 0)] / data_new$aanwas[which(data_new$aanwas > 0)])*100
  
  gini_grondslag = round(ineq(data_new$grondslag_perc[which(data_new$jaar == 2045)],type="Gini"),2)
  
  # belasting (on)gelijkheid
  data_new$belasting_perc = 0
  data_new$belasting_perc[data_new$aanwas > 0] = (data_new$belasting[which(data_new$aanwas > 0)] / data_new$belasting[which(data_new$aanwas > 0)])*100
  
  gini_belasting = round(ineq(data_new$belasting_perc[which(data_new$jaar == 2045)],type="Gini"),2)
  
  # opbrengst instabiliteit 
  gini_opbrengst = round(ineq(aggregate(belasting~jaar, data = data_new, FUN = sum)$belasting,type="Gini"),2)
  
  # overbelasting
  overbelasting = round(budget_raming - (sum(data_ideal$belasting, na.rm = T)*scale/1000000000),2)
  
  return(list(budget_raming = budget_raming, gini_grondslag = gini_grondslag, gini_belasting = gini_belasting, gini_opbrengst = gini_opbrengst, overbelasting = overbelasting))
  
}

calculate_variant_stats_micro = function(data = test, hvi = 1000, vv_drempel = 1000, cf = 9, cb = 1, s2 = NA, s3 = NA, t1 = 34, t2 = NA, t3 = NA){
  
  # test data
  data_new = list()
  data_ideal = list()
  
  if (length(unique(data$id)) < 5){
    
    gini_grondslag = "onvoldoende observaties"
    gini_belasting = "onvoldoende observaties"
    overbelasting = "onvoldoende observaties"
    
  } else {
    
    for (i in 1:length(unique(data$id))){
      
      data_id = unique(data$id)[i]
      temp = verreken_verlies(subset(data, id == data_id), hvi = hvi, cf = cf, cb = cb, drempel = vv_drempel)
      temp_ideal = verreken_verlies(subset(data, id == data_id), hvi = hvi, cf = 20, cb = 20, drempel = 0)
      
      temp$belasting = NA
      temp_ideal$belasting = NA
      
      for (j in 1:nrow(temp)){
        temp$belasting[j] = sum(bepaal_belasting(temp$grondslag[j], schijf_2 = s2, schijf_3 = s3, tarief_1 = t1, tarief_2 = t2, tarief_3 = t3)$belasting, na.rm = T)
        temp_ideal$belasting[j] = sum(bepaal_belasting(temp_ideal$grondslag[j], schijf_2 = s2, schijf_3 = s3, tarief_1 = t1, tarief_2 = t2, tarief_3 = t3)$belasting, na.rm = T)
      }
      
      data_new[[i]] = temp
      data_ideal[[i]] = temp_ideal
      
    }
    
    data_new = do.call(rbind, data_new)
    data_ideal = do.call(rbind, data_ideal)
    
    # grondslag (on)gelijkheid
    data_new$grondslag_perc = 0
    data_new$grondslag_perc[data_new$aanwas > 0] = (data_new$grondslag[which(data_new$aanwas > 0)] / data_new$aanwas[which(data_new$aanwas > 0)])*100
    
    gini_grondslag = round(ineq(data_new$grondslag_perc[which(data_new$jaar == 2045)],type="Gini"),2)
    
    # belasting (on)gelijkheid
    data_new$belasting_perc = 0
    data_new$belasting_perc[data_new$grondslag > 0] = (data_new$belasting[which(data_new$grondslag > 0)] / data_new$aanwas[which(data_new$grondslag > 0)])*100
    
    gini_belasting = round(ineq(data_new$belasting_perc[which(data_new$jaar == 2045)],type="Gini"),2)
    
    # overbelasting
    verlies = sum(subset(data_new, aanwas < 0)$aanwas, na.rm = T)
    if (is.na(verlies)){verlies = 0}
    
    data_new$vv = data_new$cb + data_new$cf 
    vv = sum(data_new$vv, na.rm = T)
    if (is.na(vv)){vv = 0}
    
    if (verlies > 0) {overbelasting = 100 - ((vv / verlies)*100)} else {overbelasting = 0}
    overbelasting = round(overbelasting,2)
    
  }
  
  return(list(gini_grondslag = gini_grondslag, gini_belasting = gini_belasting, overbelasting = overbelasting))
  
}

# functie om variant door te rekenen voor casus
gen_combi = function(dat_variant, dat_case, jaar = "alle jaren"){
  
  # verlies verrekening
  dat_case_verlies = verreken_verlies(data = dat_case, hvi = dat_variant$hvi, cf = dat_variant$cf, cb = dat_variant$cb)
  if (jaar != "alle jaren"){dat_case_verlies = subset(dat_case_verlies, jaar == as.numeric(jaar))}
  
  belasting = list()
  for (row in c(1:nrow(dat_case_verlies))){
    temp = bepaal_belasting(grondslag = dat_case_verlies$grondslag[row], 
                            schijf_2 = dat_variant$schijf_2, schijf_3 = dat_variant$schijf_3, 
                            tarief_1 = dat_variant$tarief_1, tarief_2 = dat_variant$tarief_2, tarief_3 = dat_variant$tarief_3)
    belasting[[row]] = sum(temp$belasting, na.rm = T)
  }
  
  belasting = sum(do.call(rbind, belasting), na.rm = T)
  
  # percentage belasting tov aanwas
  if(sum(dat_case$aanwas, na.rm = T) > 0) {belasting_perc = round(((belasting / sum(dat_case$aanwas, na.rm = T))*100), 2)} else {belasting_perc = 0}
  # percentage verrekend verlies tov totaal verlies
  if (nrow(subset(dat_case, aanwas < 0) > 0)){verlies = sum(subset(dat_case, aanwas < 0)$aanwas, na.rm = T)} else {verlies = 0}
  if (verlies > 0){verrekend_verlies_perc = (sum(dat_case_verlies$cf, na.rm = T) + sum(dat_case_verlies$cb, na.rm = T))/verlies} else {verrekend_verlies_perc = 0}
  # percentage grondslag tov aanwas
  if (sum(dat_case_verlies$grondslag, na.rm = T) > 0){grondslag_perc = round((sum(dat_case_verlies$grondslag, na.rm = T)/sum(dat_case$aanwas, na.rm = T))*100, 2)} else {grondslag_perc = 0}
  
  # berekeningen
  temp_case = data.frame(
    
    case_name = dat_case$omschrijving[1], 
    variant_name = dat_variant$variant,
    
    # belastingplichtige
    
    risico = round(mean(dat_case$risico, na.rm = T), 2), 
    belasting = round(belasting, 2), 
    belasting_perc = belasting_perc, 
    verlies = abs(round(sum(subset(dat_case_verlies, aanwas < 0)$aanwas, na.rm = T), 2)),
    verrekend_verlies = round(sum(dat_case_verlies$cf, na.rm = T) +  sum(dat_case_verlies$cb, na.rm = T), 2), 
    verrekend_verlies_perc = verrekend_verlies_perc,
    vermogen = round(sum(dat_case$vermogen, na.rm = T), 2), 
    aanwas = round(sum(dat_case$aanwas, na.rm = T), 2), 
    grondslag = round(sum(subset(dat_case_verlies, grondslag > 0)$grondslag, na.rm = T), 2), 
    grondslag_perc = grondslag_perc, 
    spaargeld = round(sum(dat_case$spaargeld, na.rm = T), 2), 
    finproduct = round(sum(dat_case$finproduct, na.rm = T), 2), 
    restbezit = round(sum(dat_case$restbezit, na.rm = T), 2), 
    schuld = round(sum(dat_case$schuld, na.rm = T), 2), 
    spaargeld_rendperc = round(mean(dat_case$spaargeld_rendperc, na.rm = T), 2), 
    finproduct_rendperc = round(mean(dat_case$finproduct_rendperc, na.rm = T), 2), 
    restbezit_rendperc = round(mean(dat_case$restbezit_rendperc, na.rm = T), 2), 
    schuld_rendperc = round(mean(dat_case$schuld_rendperc, na.rm = T), 2),
    
    # variant
    hvi = dat_variant$hvi, 
    verlies_drempel = dat_variant$verlies_drempel, 
    cf = dat_variant$cf, 
    cb = dat_variant$cb, 
    schijf_2 = dat_variant$schijf_2, 
    schijf_3 = dat_variant$schijf_3, 
    tarief_1 = dat_variant$tarief_1, 
    tarief_2 = dat_variant$tarief_2, 
    tarief_3 = dat_variant$tarief_3)
  
  temp[is.na(temp)] = 0
  
  return(temp_case)
  
}

# DATA 

# case data 
id = 1; jaar = 2026; omschrijving = "Jan Modaal"; risico = 2; 
spaargeld = gen_value(42300, sd); finproduct = gen_value(7000, sd);
restbezit = 0; schuld = gen_value(12800, sd); 
spaargeld_rendperc = gen_value(0.36, sd); 
finproduct_rendperc = gen_value(6.17, sd_rend); 
restbezit_rendperc = gen_value(6.17, sd); 
schuld_rendperc = gen_value(2.57, sd)

case_data_colnames_original = c("omschrijving",  "risico", "jaar", "vermogen", "aanwas", "aanwas_forfait", "spaargeld", "finproduct", "restbezit", "schuld", "spaargeld_rendperc", "finproduct_rendperc", "restbezit_rendperc", "schuld_rendperc")
case_data_colnames_print = c("omschrijving", "risico", "jaar", "vermogen", "aanwas", "aanwas overbrugging", "spaargeld", "financiële producten", "overig bezit", "schuld", "rendement spaargeld (%)", "rendement financiële producten (%)", "rendement overig bezit (%)", "rendement schuld (%)")

case_data = gen_history(
  data.frame(id = id, jaar = jaar, omschrijving = omschrijving, risico = risico, 
             spaargeld = spaargeld, finproduct = finproduct, restbezit = restbezit, 
             schuld = schuld, spaargeld_rendperc = spaargeld_rendperc,
             finproduct_rendperc = finproduct_rendperc, restbezit_rendperc = restbezit_rendperc, 
             schuld_rendperc = schuld_rendperc), sd_rend = sd_rend)

# varianten data
colnames_variant_original = c("variant", "hvi", "verlies_drempel", "cf", "cb", "schijf_2", "schijf_3", "tarief_1", "tarief_2", "tarief_3")
colnames_variant_print = c("variant", "hvi", "verlies drempel", "CF", "CB", "S2 €", "S3 €", "T1 %", "T2 %", "T3 %")

variant_data = data.frame(
  variant = "Variant",  hvi = 1000, verlies_drempel = 1000, cf = 9, cb = 1, schijf_2 = as.numeric(NA), 
  schijf_3 = as.numeric(NA), tarief_1 = 34, tarief_2 = as.numeric(NA), tarief_3 = as.numeric(NA))

# variant effect data 
variant_case_effects = gen_combi(dat_variant = variant_data, dat_case = case_data)

# varianten data macro
variant_stats = calculate_variant_stats_macro(hvi = 1000, vv_drempel = 1000, cf = 9, cb = 1, s2 = NA, s3 = NA, t1 = 34, t2 = NA, t3 = NA)

variant_data_macro = data.frame(
  variant = "Variant", budget_raming = variant_stats$budget_raming, gini_grondslag = variant_stats$gini_grondslag, 
  gini_belasting = variant_stats$gini_belasting, gini_opbrengst = variant_stats$gini_opbrengst, 
  overbelasting = variant_stats$overbelasting, hvi = 1000, verlies_drempel = 1000, cf = 9, cb = 1, schijf_2 = as.numeric(NA), 
  schijf_3 = as.numeric(NA), tarief_1 = 34,  tarief_2 = as.numeric(NA), tarief_3 = as.numeric(NA))

# USER INTERFACE

ui = fluidPage(
  
  navbarPage("Sandbox 3",
             
             # MICRO ANALYSES
             navbarMenu("Micro analyses",
                        
                        # STAP 1: WIE ZIJN DE BELASTINGPLICHTIGEN?
                        tabPanel("Stap 1: Wie zijn de belastingplichtigen?",
                                 
                               div(style = "font-size: 10px", 
                               
                               # SIDEBAR PANEL
                               sidebarPanel(
                                 
                                 # Omschrijving
                                 h5("Omschrijving"), helpText("Wie is de belastingplichtige? Tip: kies een omschrijving die het makkelijk maakt de casus later terug te vinden."), fluidRow(column(12, textInput(inputId = "omschrijving", label = "", value = omschrijving))),
                                 
                                 # Spaargeld
                                 h5("Spaargeld (+)"), helpText("Hoeveel spaargeld heeft hij en wat is de rente op spaargeld?"),
                                 fluidRow(column(6, numericInput(inputId = "spaargeld", label = "Vermogen (€)", value = round(spaargeld), min = 0, max = Inf)), column(6, numericInput(inputId = "spaargeld_rendperc", label = "Rendement (%)", value = round(spaargeld_rendperc), min = 0, max = Inf))),
                                 
                                 # Financiele producten 
                                 h5("Financiële producten (+)"), helpText("Wat is de waarde van zijn financiële producten, zoals verhandelbare aandelen, obligaties, of cryptovaluta en wat is het rendement op deze producten?"),
                                 fluidRow(column(6, numericInput(inputId = "finproduct", label = "Vermogen (€)", value = round(finproduct), min = 0, max = Inf)), column(6, numericInput(inputId = "finproduct_rendperc", label = "Rendement (%)", value = round(finproduct_rendperc), min = 0, max = Inf))),
                                 
                                 # Onroerend goed
                                 h5("Onroerend goed (+)"), helpText("Wat is de waarde van zijn onroerende goederen, zoals een tweede huis en wat is het rendement op dit bezit?"),
                                 fluidRow(column(6, numericInput(inputId = "restbezit", label = "Vermogen (€)", value = round(restbezit), min = 0, max = Inf)), column(6, numericInput(inputId = "restbezit_rendperc", label = "Rendement (%)", value = round(restbezit_rendperc), min = 0, max = Inf))),
                                 
                                 # Schulden
                                 h5("Schulden (-)"), helpText("Hoeveel schuld heeft belastingplichtige en wat is de rente op deze schuld?"),
                                 fluidRow(column(6, numericInput(inputId = "schuld", label = "Vermogen (€)", value = round(schuld), min = 0, max = Inf)), column(6, numericInput(inputId = "schuld_rendperc", label = "Rendement (%)", value = round(schuld_rendperc), min = 0, max = Inf))),
                                 
                                 # Crisis
                                 h5("Crisis"), helpText("Heeft belastingplichtige een financiële crisis meegemaakt? Zo ja, dan simuleert de tool een vijfjarige crisis vanaf 2035."), 
                                 div(style = "font-size: 14px;padding:0px;margin-top: -20px;margin-left: 20px",  radioButtons(inputId = "crisis", label = "", choices = c("ja", "nee"), selected = "nee", inline = TRUE)),
                                 
                                 # Risicoprofiel
                                 h5("Risicoprofiel"), helpText("Hoe risicovol zijn de financiële producten van belastingplichtige (0 = geen risico, 10 = hoog risico)? De tool neemt deze informatie mee om de fluctatie in aanwas over de tijd heen te berekenen."),
                                 sliderInput(inputId = "risico", label = "", value = round(risico), min = 0, max = 10),
                                 
                                 # Knoppen
                                 fluidRow(
                                   column(6, actionButton(inputId = "add_case", label = "casus toevoegen", width = '104%')),
                                   column(6, actionButton(inputId = "random_case", label = "random casus", width = '104%'))), 
                                 
                                 width = 3)), 
                                 
                               
                              # MAIN PANEL
                              mainPanel(
                                tabsetPanel(
                                
                                # PANEL 1
                                tabPanel(title = "Bewerk dataset", fluidPage(HTML("<br>"),
                                div(column(10, HTML("<b>!!! Instructie !!!</b> Schets in de linker (grijze) kolom de situatie van de belastingplichtige voor het 
                                jaar 2026. Druk vervolgens op de knop <em>toevoegen</em> om de casus toe te voegen. Eventueel kunt u ook een door de 
                                computer gegenereerde casus toevoegen door op het knopje <em>random casus</em> te drukken. De vermogensaanwas ontwikkeling 
                                wordt nadien op random wijze geexprapoleerd naar de jaren 2027 tot en met 2045. Bent u niet tevreden met de dataset? 
                                In het onderstaande luik kunt u alle data te verwijderen middels de <em>reset</em> knop of een enkele <em>casus verwijderen</em>.
                                Wil u liever uw data bewerken in excel? Dat kan. Download het template via de <em>download template</em> knop en laadt
                                deze vervolgens op via de <em>data opladen</em> optie. Wil u de ingegeven data opslaan, druk dan op de <em>opslaan</em> knop rechtsboven.
                                U kunt vervolgens de grondslag berekening van een enkele belastingplichtige bekijken onder de tab <em>inspecteer casus</em>."), 
                            
                                HTML("<br>"), fluidRow(column(3, h5("download template"), downloadButton("download_template", label = "download template")),
                                column(9, h5("data opladen (.xlsx)"), fileInput("upload_data", label = NULL, multiple = F, accept = ".xlsx", width = '100%', placeholder = NA))),
                                div(dataTableOutput('aanwas_data'), style = "font-size:90%")), 
                                column(2, actionButton(inputId = "reset_data", label = "reset dataset", width = '100%'), h4(), actionButton(inputId = "delete_case", label = "verwijder casus", width = '100%'), h4(), downloadButton("download_cases", label = "opslaan", style = "width:100%;")))), style = "font-size:95%"), 
                                
                                # PANEL 2
                                tabPanel(title = "Inspecteer casus",
                                div(fluidPage(HTML("<br>"), 
                                fluidRow(h5("Welke casus wilt u bekijken?"), uiOutput("micro_1_select_case"),
                                column(6, h5("Aanwas 2026"), htmlOutput("grondslag_tekst", align = "justify"), h5("Aanwas 2026-2045"), htmlOutput("grondslag_tekst_2", align = "justify"), 
                                HTML("<br>"), h5("Toelichting grafieken"), HTML("<i>De bovenste grafiek visualiseert de aanwas berekening voor het jaar 2026. Het lichtgrijze gedeelte van elke staaf 
                                toont de waarde (in €) van het vermogens bestanddeel en het donkergrijze gedeelte de aanwas in €. Beweeg met de cursor over de staven om de exacte waarden af te lezen.
                                De onderste grafiek toont de aanwas voor alle jaren. Beweeg met de cursor over de grafiek om de exacte waarden af te lezen. Deze statistieken vormen het startpunt voor
                                de grondslag berekening voor elk belastingjaar.</i>")), 
                                column(6, h5("Plot aanwas 2026"), plotlyOutput("plot_aanwas_2026"), h5("Plot aanwas 2026-2045"), plotlyOutput("plot_aanwas")), 
                                ))), style = "font-size:100%")
                                
                             ))),
                        
                        # STAP 2: WELKE VARIANTEN WILT U DOORREKENEN?
                        tabPanel("Stap 2: Welke variant wilt u doorrekenen?", 
                                 
                            div(style = "font-size: 10px", 
                            
                            # SIDEBAR PANEL
                            sidebarPanel(
                                       
                               # DETAILS
                               h5("Variant"), helpText("Wat is de naam van de variant?"), textInput(inputId = "naam_variant", label = "Naam Variant", value = "Variant"),
                               
                               # HEFFING VRIJ INKOMEN
                               h5("Heffingvrij Inkomen (€)"), helpText("Welk bedrag van de aanwas dient vrijgesteld te worden van belasting?"),
                               sliderInput(inputId = "hvi", label = "", value = 1000, min = 0, max = 5000, step = 50),
                               
                               # VERLIES VERREKENING
                               h5("Verliesverrekening (Jaar)"),
                               helpText("Wat is de drempel voor verliesverrekening? Met hoeveel jaren mag belastingplichtige verlies verrekenen met winst in het huidig jaar 
                               (voorwaarts / carry forward)? Met hoeveel jaren mag de belastingplichtige verlies in het huidig jaar verrekenen met winst in voorgaande jaren (achterwaarts / carry backward)?"),
                               sliderInput(inputId = "verlies_drempel",  "Drempel", min = 0, max = 5000, value = 1000, step = 50), sliderInput(inputId = "verlies_voor",  "Voorwaarts (CF)", min = 0, max = 20, value = 9),
                               sliderInput(inputId = "verlies_achter",  "Achterwaarts (CB)", min = 0, max = 10, value = 1),
                               
                               # SCHIJVEN
                               h5("Schijven"),
                               helpText("Hoeveel schijven (max. 3) heeft de variant? Wat zijn de schijfgrenzen en tarieven? N.B. Laat u de velden leeg, dan wordt er
                               automatisch verondersteld dat er slechts een schijf is (met een ongelimiteerde schijfgrens) en een tarief."),
                               
                               column(4, HTML("<b>S1: Ondergrens (€)</b>")),
                               column(4, numericInput(inputId = "schijf_2", label = "S2: Ondergrens (€)", value = NA, min = 0, max = Inf)),
                               column(4, numericInput(inputId = "schijf_3", label = "S3: Ondergrens (€)", value = NA, min = 0, max = Inf)),
                               
                               column(4, numericInput(inputId = "tarief_1", label = "S1: Tarief (%)", value = 34, min = 0, max = Inf)),
                               column(4, numericInput(inputId = "tarief_2", label = "S2: Tarief (%)", value = NA, min = 0, max = Inf)),
                               column(4, numericInput(inputId = "tarief_3", label = "S3: Tarief (%)", value = NA, min = 0, max = Inf)),
                               
                               # VOEG TOE
                               actionButton(inputId = "add_variant", label = "variant toevoegen", width = '100%'),
                               
                               width = 3 )), # eind div()  
                            
                            # MAIN PANEL  
                            mainPanel(
                                   
                              tabsetPanel(
                              
                              # TAB 1       
                              tabPanel(title = "Bewerk dataset", fluidPage(HTML("<br>"),
                              column(10, HTML("<b>!!! Instructie !!!</b> Specificeer in de linker (grijze) kolom de variant die u wil doorrekenen. 
                              Druk vervolgens op de knop <em>variant toevoegen</em> om de variant toe te voegen. Bent u ontevreden met de dataset? 
                              In het onderstaande luik kunt u alle data te verwijderen middels de <em>reset</em> knop of een enkele <em>variant verwijderen</em>.
                              Wil u liever uw data bewerken in excel? Dat kan. Download het template via de <em>download template</em> knop en laadt
                              deze vervolgens op via de <em>data opladen</em> optie. Wil u de ingegeven data opslaan, druk dan op de <em>opslaan</em> knop rechtsboven.
                              U kunt vervolgens de eigenschappen van een door u geselecteerde variant bekijken in de tab <em>inspecteer variant</em>.<br>"),
                                                        
                              HTML("<br>"), fluidRow(
                              column(3, h5("download template"), downloadButton("download_template_variant", label = "download template")),
                              column(9, h5("data opladen (.xlsx)"), fileInput("upload_data_variant", label = NULL, multiple = F, accept = ".xlsx", width = '100%', placeholder = NA))),
                              div(dataTableOutput('variant_data'), style = "font-size:90%")), 
                              column(2, actionButton(inputId = "reset_data_variant", label = "reset dataset", width = '100%'), h4(), 
                              actionButton(inputId = "delete_variant", label = "verwijder variant", width = '100%'), h4(),
                              downloadButton("download_variants", label = "opslaan", style = "width:100%;")))),
                                     
                              # TAB 2
                              tabPanel(title = "Inspecteer variant", fluidPage(HTML("<br>"), fluidRow(
                              column(5, h5("Welke variant wilt u bekijken?"), uiOutput("micro_1_select_variant")),
                              column(7, h5("Welk jaar wilt u bekijken?"), selectInput("plot_variant_jaar", label = NULL, choices = 2026:2045, selected = 2036, width = '100%')),
                              column(5, h5(""), htmlOutput("variant_tekst", align = "justify")),
                              column(7, h5("Visualisatie variant"), helpText("Beweeg met de cursor over de grafiek om nadere toelichting te krijgen over de specificaties van de variant."), 
                                     plotlyOutput("plot_variant")))))
                              ))),
                        
                        # STAP 3: BEKIJK RESULTATEN
                        tabPanel("Stap 3: Bekijk resultaten",
                              
                               tabsetPanel(
                                 
                               # TAB 1
                               tabPanel("Dataset", HTML("<br>"),
                               column(10, HTML("De onderstaande tabel bevat de doorrekening van elk van de door u gespecificeerde variant voor elk van de door u opgegeven belastingplichtigen.
                               Bent u ontevreden met de dataset? In het onderstaande luik kunt u alle data te verwijderen middels de <em>reset</em> knop of een enkele <em>rij verwijderen</em>.
                               Wil u de tabel opslaan, druk dan op de <em>download</em> knop rechtsboven. Onder de tab <em>inspecteer microvoorbeeld</em> kunt u vervolgens 
                               de grondslag en belasting berekening voor een enkele casus en een enkele variant inspecteren. Onder de tab <em>inspecteer micro effecten</em> 
                               worden de gevolgen van elk van de varianten voor de door u opgegeven casi in kaart gebracht. Specifiek wordt er naar een drietal statistieken gekeken:
                               <br><br>
                               <ul>
                               <li><b>grondslag ongelijkheid</b>, m.n. de mate waarin het percentage grondslag (t.o.v. aanwas) verschilt tussen de door u opgegeven belastingplichtigen  (0-1);</li>
                               <li><b>belasting ongelijkheid</b>, m.n. de mate waarin het percentage belasting (t.o.v. aanwas) verschilt tussen de door u opgegeven belastingplichtigen (0-1);</li>
                               <li><b>overbelasting</b>, m.n. het percentage extra belasting dat de door u opgegeven belastingplichtigen betalen door restricties op verliesverrekening. </li>
                               </ul> <br>"),
                               div(dataTableOutput('variant_case_effects'), style = "font-size:90%")),
                               column(2, actionButton(inputId = "reset_variant_case_effects", label = "reset dataset", width = '100%'), h4(),
                               actionButton(inputId = "delete_variant_effects", label = "verwijder variant", width = '100%'), h4(),
                               actionButton(inputId = "delete_case_effects", label = "verwijder casus", width = '100%'), h4(),
                               downloadButton("download_variants_case_effects", label = "opslaan", style = "width:100%;"))),
                              
                               # TAB 2
                               tabPanel("Inspecteer microvoorbeeld", HTML("<br>"), 
                                  
                                  # SIDEBAR       
                                  div(style = "font-size: 10px", 
                                  sidebarPanel(h5("Jaar"), helpText("Voor welke jaar wilt u een microvoorbeeld genereren?"), 
                                  selectInput("micro_3_select_year", label = NULL, choices = c("Alle jaren", 2026:2045)),
                                  h5("Selecteer casus"),  helpText("Voor welke casus wilt u een microvoorbeeld genereren?"), uiOutput("micro_3_select_case"), 
                                  h5("Selecteer variant"), helpText("Voor welke variant wilt u een microvoorbeeld genereren?"), uiOutput("micro_3_select_variant"), 
                                  downloadButton("download_micro", label = "voorbeeld opslaan", style = "width:100%;"), width = 3)),
                                  
                                  # MAINPANEL 
                                  mainPanel(
                                  column(6, h5("Microvoorbeeld"), div(dataTableOutput('tab_micro'), style = "font-size:80%"), HTML("<br><br>"), htmlOutput("micro_tekst", align = "justify")),
                                  column(6, h5("Visualisatie microvoorbeeld"), helpText("Beweeg met de cursor over de grafiek om nadere toelichting te krijgen over de aanwas (belastbaar en niet belastbaar) en verlies (verrekenbaar en niet verrekenbaar) van de belastingplichtige per belastingjaar."), plotlyOutput("plot_micro")
                                  ))),
                                  
                               # TAB 3
                               tabPanel("Inspecteer micro-effecten", HTML("<br>"),
                               column(3, h5("Microeffecten per variant"), 
                               helpText("De onderstaande tabel toont een drietal statistieken die samenvatten welk effect de door u opgegeven varianten hebben op de relatie tussen de door u opgegeven belastingplichtigen.
                               Grondslag ongelijkheid en belasting ongelijkheid vat de mate waarin belastingplichtigen een verschillend percentage grondslag hebben en belasting betale t.o.v. hun aanwas op een schaal
                               van 0 (perfect gelijk) tot 1 (perfect ongelijk). De statistiek overbelasting vat het gemiddeld percentage belasting (t.o.v. totaal) dat belastingplichtigen terug zouden krijgen indien zij alle verliezen
                               zouden mogen verrekenen."), 
                                               
                               div(style = "font-size: 10px", dataTableOutput('tab_microeffects')), HTML("<br><br>"),
                               h5("Beste variant per categorie"), helpText("Varianten met de minste ongelijkheid en de laagste overbelasting."),
                               div(style = "font-size: 10px", dataTableOutput('tab_microwinners'))), 
                               column(4, h5("Selecteer variant"), 
                               helpText("Beweeg met uw cursor over de staven om de nadere opsplitsing van de aanwas in belasting en grondslag per belastingplichtige te bekijken"),
                               uiOutput("plot_micro_select_variant_1_choices"),
                               plotlyOutput("plot_micro_variant_1")),
                               column(4, h5("Selecteer andere variant"), 
                               helpText("Beweeg met uw cursor over de staven om de nadere opsplitsing van de aanwas in belasting en grondslag per belastingplichtige te bekijken"),
                               uiOutput("plot_micro_select_variant_2_choices"),
                               plotlyOutput("plot_micro_variant_2")),
                                        
                               ))
                               
                               )),
             
             
             
             # MACRO ANALYSES 
             navbarMenu("Macro analyses",
                        
                        # STAP 1: WELKE VARIANTEN WILT U VERGELIJKEN?
                        tabPanel("Stap 1: Welke varianten wilt u vergelijken?",
                                 
                                 # SIDEBAR
                                 div(style = "font-size: 10px", 
                                 sidebarPanel(
                                       
                                 # DETAILS
                                 h5("Variant"), helpText("Wat is de naam van de variant?"), textInput(inputId = "naam_variant_macro", label = "Naam Variant", value = "Voorbeeld"),
                                 
                                 # HEFFING VRIJ INKOMEN
                                 h5("Heffingvrij Inkomen (€)"), helpText("Welk bedrag van de aanwas dient vrijgesteld te worden van belasting?"),
                                 sliderInput(inputId = "hvi_macro", label = "", value = 1000, min = 0, max = 5000, step = 50),
                                 
                                 # VERLIES VERREKENING
                                 h5("Verliesverrekening (Jaar)"), helpText("Wat is de drempel voor verliesverrekening? Met hoeveel jaren mag belastingplichtige verlies verrekenen met winst in het huidig jaar (voorwaarts / carry forward)? Met hoeveel jaren mag de belastingplichtige verlies in het huidig jaar verrekenen met winst in voorgaande jaren (achterwaarts / carry backward)?"),
                                 sliderInput(inputId = "verlies_drempel_macro",  "Drempel", min = 0, max = 5000, value = 1000, step = 50),
                                 sliderInput(inputId = "verlies_voor_macro",  "Voorwaarts (CF)", min = 0, max = 20, value = 9),
                                 sliderInput(inputId = "verlies_achter_macro",  "Achterwaarts (CB)", min = 0, max = 10, value = 1),
                                 
                                 # SCHIJVEN
                                 h5("Schijven"), helpText("Hoeveel schijven (max. 3) heeft de variant? Wat zijn de schijfgrenzen en 
                                 tarieven? N.B. Laat u de velden leeg, dan wordt er automatisch verondersteld dat er slechts een schijf is (met een ongelimiteerde schijfgrens) en een tarief."),
                                 
                                 column(4, HTML("<b>S1: Ondergrens (€)</b>")),
                                 column(4, numericInput(inputId = "schijf_2_macro", label = "S2: Ondergrens (€)", value = NA, min = 0, max = Inf)),
                                 column(4, numericInput(inputId = "schijf_3_macro", label = "S3: Ondergrens (€)", value = NA, min = 0, max = Inf)),
                                 
                                 column(4, numericInput(inputId = "tarief_1_macro", label = "S1: Tarief (%)", value = 34, min = 0, max = Inf)),
                                 column(4, numericInput(inputId = "tarief_2_macro", label = "S2: Tarief (%)", value = NA, min = 0, max = Inf)),
                                 column(4, numericInput(inputId = "tarief_3_macro", label = "S3: Tarief (%)", value = NA, min = 0, max = Inf)),
                                 
                                 # VOEG TOE
                                 actionButton(inputId = "add_variant_macro", label = "variant toevoegen", width = '100%'),
                                 
                                 width = 3 )), # eind div()   
                                 
                                 
                                 # MAIN PANEL
                                 mainPanel(tabsetPanel(
                                     
                                   # TAB 1
                                   tabPanel(title = "Bewerk dataset", fluidPage(HTML("<br>"), column(10, HTML(
                                   "<b>!!! Instructie !!!</b> Specificeer in de linker (grijze) kolom de variant die u wil doorrekenen <b> voor de populatie </b>. 
                                   Druk vervolgens op de knop <em>variant toevoegen</em> om de variant toe te voegen. Bent u ontevreden met de dataset? 
                                   In het onderstaande luik kunt u alle data te verwijderen middels de <em>reset</em> knop of een enkele <em>variant verwijderen</em>.
                                   Wil u liever uw data bewerken in excel? Dat kan. Download het template via de <em>download template</em> knop en laadt
                                   deze vervolgens op via de <em>data opladen</em> optie. Wil u de ingegeven data opslaan, druk dan op de <em>opslaan</em> knop rechtsboven.
                                   U kunt vervolgens de eigenschappen van een door u geselecteerde variant bekijken in de tab <em>inspecteer variant</em>.<br>"),
                                   HTML("<br>"), 
                                                          
                                   fluidRow(
                                   column(3, h5("download template"), downloadButton("download_template_variant_macro", label = "download template")),
                                   column(9, h5("data opladen (.xlsx)"), fileInput("upload_data_variant_macro", label = NULL, multiple = F, accept = ".xlsx", width = '100%', placeholder = NA))),
                                   dataTableOutput('variant_data_macro')), 
                                   column(2, actionButton(inputId = "reset_data_variant_macro", label = "reset dataset", width = '100%'), h4(),
                                   actionButton(inputId = "delete_variant_macro", label = "verwijder variant", width = '100%'), h4(),
                                   downloadButton("download_variants_macro", label = "opslaan", style = "width:100%;")))),
                                     
                                   # TAB 2
                                   tabPanel(title = "Inspecteer variant", fluidPage(HTML("<br>"), fluidRow(
                                   column(5, h5("Welke variant wilt u bekijken?"), uiOutput("micro_1_select_variant_macro")),
                                   column(7, h5("Welk jaar wilt u bekijken?"), selectInput("plot_variant_jaar_macro", label = NULL, choices = 2026:2045, selected = 2036, width = '100%')),
                                   column(5, h5(""), htmlOutput("variant_tekst_macro", align = "justify")),
                                   column(7, h5("Visualisatie variant"), helpText("Beweeg met de cursor over de grafiek om nadere toelichting te krijgen over de specificaties van de variant."), 
                                          plotlyOutput("plot_variant_macro")))))
                                   
                        ))),
                        
                        tabPanel("Stap 2: Bekijk resultaten",
                                 tabsetPanel(
                                   
                                   # TAB 1
                                   tabPanel("Dataset", HTML("<br>"),
                                   column(10, HTML("De onderstaande tabel bevat <em>een schatting</em> van elk van de door u gespecificeerde variant voor de populatie.
                                   Bent u ontevreden met de dataset? In het onderstaande luik kunt u alle data te verwijderen middels de <em>reset</em> knop of een enkele <em>rij verwijderen</em>.
                                   Wil u de tabel opslaan, druk dan op de <em>download</em> knop rechtsboven. De variant wordt op basis van een viertal statistieken objectief in kaart gebracht:
                                   <br><br>
                                   <ul>
                                   <li><b>budgettaire opbrengst</b>, mn. de budgettaire opbrengst van de variant (in mld.);</li>
                                   <li><b>budget stabiliteit</b>, mn. de mate waarin de opbrengst gelijkmatig verdeeld is over de jaren heen (0-1);</li>
                                   <li><b>grondslag ongelijkheid</b>, m.n. de mate waarin het percentage grondslag (t.o.v. aanwas) verschilt in de populatie  (0-1);</li>
                                   <li><b>belasting ongelijkheid</b>, m.n. de mate waarin het percentage belasting (t.o.v. aanwas) verschilt in de populatie (0-1);</li>
                                   <li><b>overbelasting</b>, m.n. het percentage extra belasting dat burgers betalen door restricties op verliesverrekening. </li>
                                   </ul> <br>
                                   <em>Let op! De statistieken die hier worden gepresenteerd zijn <b>schattingen</b>, gebaseerd op een random steekproef (N = 2000) van onze populatie database. 
                                   Deze schattingen zijn voldoende om de verschillen tussen varianten in kaart te brengen (bijv. welke variant levert meer op, welke minder), maar zijn 
                                   minder precies dan een raming. Wilt u een raming opvragen? Sla de variant(en) op en stuur deze naar s.e.deleeuw@minfin.nl en u ontvangt de raming binnen een dag."),
                                   
                                   div(dataTableOutput('variant_population_effects'), style = "font-size:90%")),
                                   column(2, actionButton(inputId = "reset_variant_population_effects", label = "reset dataset", width = '100%'), h4(),
                                   actionButton(inputId = "delete_variant_population_effects", label = "verwijder variant", width = '100%'), h4(),
                                   actionButton(inputId = "delete__population_effects", label = "verwijder casus", width = '100%'), h4(),
                                   downloadButton("download_variants_population_effects", label = "opslaan", style = "width:100%;"))),
                                   
                                   # TAB 2
                                   tabPanel("Inspecteer budgettaire opbrengst en stabiliteit"),
                                   
                                   # TAB 3
                                   tabPanel("Inspecteer grondslag en belasting ongelijkheid"),
                                   
                                   # TAB 4
                                   tabPanel("Inspecteer overbelasting")
                                   
                                   ))),
             
             
             # GENEREER RAPORT 
             tabPanel("Genereer rapport")
             
             
  )) # eind FluidPage()


# SERVER
server = function(input, output) {
  
  #showModal(modalDialog(
  #  title = "Welkom bij SandBox 3!",
  #  "Deze app helpt u uw eigen microvoorbeelden te bouwen voor elke denkbare variant van het toekomstig Box 3 stelsel! 
  #   Heeft u opmerkingen, vragen, frustraties, of een nodeloos lange liefdesverklaring, neem vooral contact op met de datanerds on call:
  #   Dr. Sjifra de Leeuw (s.e.leeuw@minfin.nl) en Dr. Josha Box (j.m.h.box@minfin.nl).",
  #   easyClose = TRUE
  #))
  
  
  
  ################################## 1. MICRO ANALYSES ##################################
  
  
  ############ 1.1. MICRO ANALYSES - STAP 1: WIE ZIJN DE BELASTINGPLICHTIGEN? ###########
  
  ############ SIDEBAR ###########
  
  
  # KNOP CASUS TOEVOEGEN
  observeEvent(input$add_case, {
    
    set.seed(1993)
    
    # EASTER EGG
    easteregg = data.frame(
      omschrijving = c("Nick Jongerius", "Sjifra de Leeuw", "Josha Box", "Nienke Cornelissen", "Allard Smit", "Aschwin Moes", "Bart van den Hof", "Kees den Boogert", "Marjolein van den Berg", "Marjan Nienhuis", "Ruud Beenhakker", "Koen van Schie", "Rocus van Opstal"), 
      voornaam = c("Nick", "Sjifra", "Josha", "Nienke", "Allard", "Aschwin", "Bart", "Kees", "Marjolein", "Marjan", "Ruud", "Koen", "Rocus"), 
      geslacht = c("man", "vrouw", "man", "vrouw", "man", "man", "man", "man", "vrouw", "vrouw", "man", "man", "man")
    )
    
    easteregg$vnw = "hij"; easteregg$vnw[easteregg$geslacht == "vrouw"] = "zij"
    easteregg$vnw_bezit = "hem"; easteregg$vnw_bezit[easteregg$geslacht == "vrouw"] = "haar"
    
    if (input$omschrijving %in% easteregg$omschrijving){
      
      voornaam = subset(easteregg, omschrijving == input$omschrijving)$voornaam
      vnw = subset(easteregg, omschrijving == input$omschrijving)$vnw
      vnw_bezit = subset(easteregg, omschrijving == input$omschrijving)$vnw_bezit
      geslacht = subset(easteregg, omschrijving == input$omschrijving)$geslacht
      
      opmerkingen = c(
        paste0("Vrouwen vergelijken met een auto? Dat zit er voor ", voornaam, " niet in."),
        paste0("We gaan op sjiek, mensen. Dit is ", voornaam, ". Ouder van twee volwassen dochters en 
                en nee, ", vnw, " draagt niet het trouwpak van het eerste of tweede huwelijk, maar misschien
                wel van nummer drie. Ben jij die nummer drie? Bel ", vnw_bezit, " snel!" ),
        paste0(voornaam, " neemt nooit initiatief en doet nooit iets in het huishouden.
               Gun jezelf de tijd en met een beetje begeleiding heb je binnen 30 jaar een eigen ", voornaam, 
               " in huis die wel de ", geslacht, " van je dromen is. Even geduld dus!"),
        paste0(voornaam, " heeft helemaal geen behoefte aan een bakkie, ", vnw, " wil een moderne man
               die matcht en zelf zijn jasje dasje wast. Dus zie jij jezelf wel naast ", vnw_bezit, " in
               de spiegel staan, bel ", vnw_bezit, " dan snel."),
        paste0(voornaam, " heeft het al niet zo op dominante vrouwen en ", vnw, " is er nu helemaal klaar mee.
               Ben jij dol op het huishouden en zoek jij een ", geslacht, " die zich het liefst laat 
               verzorgen? Dan trakteert ", voornaam, " je graag op een bakkie -- of zelfs een achterbakkie 
               (wink wink, nudge nudge) als je mazzel hebt."),
        paste0(voornaam, " is heel erg gevoelig voor alcohol, maar kijkt erg nuchter naar de liefde. Wil jij
               een wijntje met ", vnw_bezit, " delen? Neem dan contact met ", vnw_bezit, " op!"), 
        paste0(voornaam, " vindt zichzelf te knap voor ", sample(easteregg$voornaam, size = 1), ". 
               Ben jij wel knap genoeg? Bel dan snel!"),
        paste0("Croissantjes lekker voorverwarmd op een inductieplaat, eitjes in de airfryer pan hup in de oven,
               versgeperste sinaasappelsap met meer pitjes dan sap. Heerlijk wakker worden want ", voornaam, 
               " heeft een geslaagd ontbijt voor je klaarstaan."), 
        paste0("Romantiek voor ", voornaam, " is lekker samenzitten met een goed boek onder een sterrenhemel. 
               Maar niet in de Provence. Je vult weer vanalles voor ", vnw_bezit, " in. Daar heeft ", vnw, 
               " veel moeite mee. Dus laat ", vnw_bezit, " in rust een boekje lezen.")
        
      )
      
      popup(paste0("Wat als ... ", input$omschrijving, " aan Lang Leve de Liefde (de meer genante versie van First Dates op SBS6) zou deelnemen?"), sample(opmerkingen, size = 1))
      
    }
    
    # AFSCHEID STEFAN
    if (input$omschrijving == "Stefan Smalbrugge"){
      
      popup_adjusted("Ga jij Stefan ook zo missen?", "Met ingang van volgend jaar zal Stefan ons bij het Ministerie verlaten ten gunste van zijn warme nestje bij de Belastingdienst. Ga jij Stefan missen?", 
                     footer = tagList(actionButton("stefan_nee", "Bwah"), actionButton("stefan_ja", "Jazeker")))
      
    
    # positief antwoord 
    observeEvent(input$stefan_ja, {
      
      popup_adjusted("Laat hem dat dan weten, kneus!", "Stuur hem hieronder een berichtje, een teken van waardering of een ongepast lange liefdesverklaring. P.S. Vergeet niet te vermelden van wie het berichtje afkomstig is!",
                     footer = tagList(
                       textInput("stefan_ja_naam", label = "Naam", width = '100%', placeholder = "Voornaam Achternaam"),
                       textInput("stefan_ja_email", label = "Email adres", width = '100%', placeholder = "voorbeeld@gmail.com"),
                       textInput("stefan_ja_text", label = "Bericht", width = '100%', placeholder = "Schrijf hier een lief berichtje."), 
                       actionButton("stefan_ja_verstuur", "Verstuur berichtje")))
    
      observeEvent(input$stefan_ja_verstuur, {
        
        #Stefan Smalbrugge
        send.mail(from = "sjifra.de.leeuw@outlook.com",
                  to = "sjifra.de.leeuw@gmail.com",
                  subject = paste0(input$stefan_ja_naam, " van het Ministerie van Financiën mist je ... "),
                  body = paste0(input$stefan_ja_text, "\n\nMail hem/haar terug via ", input$stefan_ja_email),
                  smtp = list(host.name="smtp.office365.com", port=587, tls=T, user.name="sjifra.de.leeuw@outlook.com", passwd="Winston3001_minfin"),
                  authenticate = TRUE, send = T)
        
        removeModal()
        
      })
      
    })
    
    # negatief antwoord
    observeEvent(input$stefan_nee, {
      popup_adjusted("Liar liar pants on fire", "Daar geloof ik niets van! Try again: Ga jij Stefan missen?", footer = tagList(actionButton("stefan_nee_2", "Ik ben een monster"), actionButton("stefan_ja_2", "Ja natuurlijk!")))
    })
    
    observeEvent(input$stefan_nee_2, {popup("Okay dan...", "Hij gaat jou ook niet missen.")})
    
    
    observeEvent(input$stefan_ja_2, {
      
      popup_adjusted("Laat hem dat dan weten, kneus!", "Stuur hem hieronder een berichtje, een teken van waardering of een ongepast lange liefdesverklaring. P.S. Vergeet niet te vermelden van wie het berichtje afkomstig is!",
                     footer = tagList(
                       textInput("stefan_ja_2_naam", label = "Naam", width = '100%', placeholder = "Voornaam Achternaam"),
                       textInput("stefan_ja_2_email", label = "Email adres", width = '100%', placeholder = "voorbeeld@gmail.com"),
                       textInput("stefan_ja_2_text", label = "Bericht", width = '100%', placeholder = "Schrijf hier een lief berichtje."), 
                       actionButton("stefan_ja_verstuur", "Verstuur berichtje")))
      
      observeEvent(input$stefan_ja_verstuur, {
        
        send.mail(from = "sjifra.de.leeuw@outlook.com",
                  to = "sjifra.de.leeuw@gmail.com",
                  subject = paste0(input$stefan_ja_2_naam, " van het Ministerie van Financiën mist je ... "),
                  body = paste0(input$stefan_ja_2_text, "\n Mail hem/haar terug via ", input$stefan_ja_2_email),
                  smtp = list(host.name="smtp.office365.com", port=587, tls=T, user.name="sjifra.de.leeuw@outlook.com", passwd="Winston3001_minfin"),
                  authenticate = TRUE, send = T)
        
        removeModal()
        
      })
      
    })
    
    } # einde afscheid stefan
    
    
    # ADD CASE 
    
    # als rendement groter dan nul
    if (sum(c(input$spaargeld_rendperc, input$finproduct_rendperc, input$restbezit_rendperc, input$schuld_rendperc)) > 0){
      
      sd = 0.4
      sd_rend = 0.5*isolate(input$risico)*sd
      
      if (is.null(input$upload_data)){data = case_data()} 
      if (!is.null(input$upload_data)){data = upload_data$data}
      if(nrow(data) == 0){id = 1} else {id = max(data$id) + 1}
      
      if(isolate(input$omschrijving) %in% data$omschrijving){omschrijving_new = paste0(isolate(input$omschrijving), " ", max(data$id) + 1)
      } else {omschrijving_new = isolate(input$omschrijving)}
      
      if (is.na(isolate(input$spaargeld))){spaargeld = 0} else {spaargeld = isolate(input$spaargeld)}
      if (is.na(isolate(input$finproduct))){finproduct = 0} else {finproduct = isolate(input$finproduct)}
      if (is.na(isolate(input$restbezit))){restbezit = 0} else {restbezit = isolate(input$restbezit)}
      if (is.na(isolate(input$schuld))){schuld = 0} else {schuld = isolate(input$schuld)}
      
      if (is.na(isolate(input$spaargeld_rendperc))){spaargeld_rendperc = 0} else {spaargeld_rendperc = isolate(input$spaargeld_rendperc)}
      if (is.na(isolate(input$finproduct_rendperc))){finproduct_rendperc = 0} else {finproduct_rendperc = isolate(input$finproduct_rendperc)}
      if (is.na(isolate(input$restbezit_rendperc))){restbezit_rendperc = 0} else {restbezit_rendperc = isolate(input$restbezit_rendperc)}
      if (is.na(isolate(input$schuld_rendperc))){schuld_rendperc = 0} else {schuld_rendperc = isolate(input$schuld_rendperc)}
      
      dat = gen_history(data.frame(
        id = id, omschrijving = omschrijving_new,  risico = gen_value(mean = isolate(input$risico), sd = sd, n = 1), jaar = 2026,
        spaargeld = spaargeld, finproduct = finproduct, restbezit = restbezit, schuld = schuld, spaargeld_rendperc = spaargeld_rendperc, 
        finproduct_rendperc = finproduct_rendperc, restbezit_rendperc = restbezit_rendperc, 
        schuld_rendperc = schuld_rendperc), sd_rend = sd_rend, crisis = input$crisis)
      
      # voeg data toe
      if (is.null(input$upload_data)){case_data() %>% bind_rows(dat) %>% case_data()
      } else {upload_data$data = upload_data$data %>% bind_rows(dat)}
      
    # als rendement gelijk is aan nul
    } else {warning("Casus niet toegevoegd", "Een burger zonder rendement zal onder geen enkele variant belasting betalen in Box 3.")}
    
  })
  
  # KNOP RANDOM CASUS 
  observeEvent(input$random_case, {
    
    # switch data als opgeladen 
    if (is.null(input$upload_data)){data = case_data()} 
    if (!is.null(input$upload_data)){data = upload_data$data}
    
    # id 
    if(nrow(data) == 0){id = 1} else {id = max(data$id) + 1}
    
    # garandeer unieke omschrijving
    if(isolate(input$omschrijving) %in% data$omschrijving){omschrijving_new = paste0(isolate(input$omschrijving), " ", max(data$id) + 1)
    } else {omschrijving_new = isolate(input$omschrijving)}
    
    # pas variantie aan
    risico = sample(0:10, 1, replace = T); sd = sd*2; sd_rend = risico*sd
    
    
    dat = gen_history(data.frame(id = id, omschrijving = omschrijving_new, risico = risico, jaar = 2026,
      spaargeld = max(c(gen_value(mean = 42300, sd = 42300*sd), 0)), 
      finproduct = max(c(gen_value(mean = 7000, sd = 7000*sd), 0)), 
      restbezit = sample(c(rep(0,100),rep(250000,25), rep(500000,12), rep(1000000,6)), 1, replace = T), 
      schuld = max(c(gen_value(mean = 12800, sd = 12800*sd), 0)), 
      spaargeld_rendperc = max(c(gen_value(mean = 0.36, sd = 0.36*sd), 0)), 
      finproduct_rendperc = max(c(gen_value(mean = 6.17, sd = 6.17*sd_rend), 0)), 
      restbezit_rendperc = max(c(gen_value(mean = 6.17, sd = 6.17*sd_rend), 0)), 
      schuld_rendperc = max(c(gen_value(mean = 2.57, sd = 2.57*sd)), 0)), 
      sd_rend = sd_rend, crisis = input$crisis)
    
    # voeg data toe
    if (is.null(input$upload_data)){case_data() %>% bind_rows(dat) %>% case_data()   
    } else { upload_data$data = upload_data$data %>% bind_rows(dat) }
    
  })
  
  ############ TAB 1 ############ 
  
  # DOWNLOAD TEMPLATE 
  output$download_template = downloadHandler(
    filename = function() { paste("sandbox3_template_casus.xlsx", sep="") },
    content = function(file) {
      write_xlsx(
        data.frame( omschrijving = "", spaargeld = as.numeric(NA), spaargeld_rendementpercentage = as.numeric(NA), 
          financieleproducten = as.numeric(NA), financieleproducten_rendementpercentage = as.numeric(NA), 
          overigbezit = as.numeric(NA), overigbezit_rendementpercentage = as.numeric(NA), 
          schuld = as.numeric(NA), schuld_rendementpercentage = as.numeric(NA), risicoprofiel = as.numeric(NA), 
          crisis_janee = ""),
    file)
    })
  
  # OPLADEN DATA 
  upload_data = reactiveValues(data = NULL)
  
  gen_empty_data = function(){
    
    data = gen_history(
      data.frame(id = 1, omschrijving = "1", 
                 risico = 1, jaar = 2026, spaargeld = 1, 
                 finproduct = 1, restbezit = 1, schuld = 1, 
                 spaargeld_rendperc = 1, finproduct_rendperc = 1, 
                 restbezit_rendperc = 1, schuld_rendperc = 1), 
      sd_rend = sd_rend, crisis = "nee") %>%
      filter(row_number() %in% -1)
    
    return(data)
    
  }
  
  gen_upload_data = function(){
    
    if (nrow(readxl::read_xlsx(input$upload_data$datapath)) > 0){
      
      data = readxl::read_xlsx(input$upload_data$datapath) %>%
        setNames(c("omschrijving", "spaargeld", "spaargeld_rendperc", "finproduct", "finproduct_rendperc", "restbezit", "restbezit_rendperc", "schuld", "schuld_rendperc", "risico", "crisis"))
      
      datalist = list() 
      
      # voor iedere belastingplichtige
      for (i in c(1:length(unique(data$omschrijving)))){
        
        # selecteer belastingplichtige
        row = subset(data, omschrijving == unique(data$omschrijving)[i])[1,]
        
        # genereer geschiedenis belastingplichtige
        datalist[[i]] = gen_history(data.frame(
          id = i, omschrijving = row$omschrijving, 
          risico = row$risico, jaar = 2026, spaargeld = row$spaargeld, 
          finproduct = row$finproduct, restbezit = row$restbezit, schuld = row$schuld, 
          spaargeld_rendperc = row$spaargeld_rendperc, finproduct_rendperc = row$finproduct_rendperc, 
          restbezit_rendperc = row$restbezit_rendperc, schuld_rendperc = row$schuld_rendperc), 
          sd_rend = sd_rend, crisis = row$crisis)  
      }
      
      upload_data$data = do.call(rbind, datalist) 
    
    # generate empty data frame if file does not contain any entries 
    } else {
      
      upload_data$data = gen_empty_data()
      
    }}
  
  observeEvent(input$upload_data, {upload_data$data = gen_upload_data()})
  
  # VOORGEPROGRAMEERDE DATA 
  case_data = reactiveVal(case_data)
  
  # OUTPUT TABEL
  output$aanwas_data = renderDataTable({
    if (is.null(input$upload_data)){data = case_data()} else {data = upload_data$data}
    select(data, case_data_colnames_original) %>%  setNames(case_data_colnames_print) 
  }, escape = F, server = F, rownames = F, selection = 'single', options = list(paging =T, pageLength = 16, scrollX = T))
  
  # KNOP RESET DATA
  observeEvent(input$reset_data, {
    if (is.null(input$upload_data)){case_data() %>% filter(row_number() %in% -1) %>% case_data() 
    } else {upload_data$data =  gen_empty_data()}
  })
  
  # KNOP VERWIJDER CASUS 
  observeEvent(input$delete_case, {
    
    if (is.null(input$upload_data)){    
      remove = case_data()[input$aanwas_data_rows_selected,"omschrijving"]
      case_data() %>% subset(., omschrijving != remove) %>% case_data()   
    } else { 
      remove = upload_data$data[input$aanwas_data_rows_selected, "omschrijving"]
      upload_data$data = upload_data$data %>% subset(., omschrijving != remove)
    }
    
  })
  
  # KNOP DOWNLOAD 
  selected_data = function(input = NULL){if (is.null(input)){data = case_data()} else {data = upload_data$data}; return(data)}
  
  output$download_cases = downloadHandler(
    filename = function() {paste("sandbox3_casi.xlsx", sep="")},
    content = function(file) {write_xlsx(selected_data(input$upload_data), file)}
  )
  
  ############ TAB 2 ############ 
  
  # DATA CASE NAMES
  output$micro_1_select_case = renderUI({
    if (is.null(input$upload_data)){data = case_data()} else {data = upload_data$data}
    data = subset(data, jaar == 2026)
    selectInput("micro_1_select_case_selection", label = NULL, choices = data$omschrijving)
  })
  
  # TEKSTUELE OMSCHRIJVING GRONDSLAG
  
  output$grondslag_tekst = renderText({ 
    
    # data 
    if (is.null(input$upload_data)){data = case_data()} else {data = upload_data$data}
    
    text = ""
    
    # als geen data beschikbaar
    if (nrow(data) < 1){text = "Er is geen data beschikbaar. Voeg data toe met behulp van de tool. <br><br><br>"
    
    # als wel data beschikbaar
    } else {
      
      selection = input$micro_1_select_case_selection
      df = subset(data, jaar == 2026 & omschrijving == selection)
      naam = df$omschrijving
      
      
      # (a) Spaargeld
      if (df$spaargeld > 0){text = paste0(text, "<b>Spaargeld.</b> Op de peildatum van 2026 heeft ", naam, " <i>", number_to_money(df$spaargeld), "</i> spaargeld op zijn rekening. Belastingplichtige ontvangt <i>", percentify(df$spaargeld_rendperc), " rente</i>. De aanwas is daarmee gelijk aan <i>", number_to_money(bereken_aanwas(df$spaargeld, df$spaargeld_rendperc)) , "</i>. ")} 
      else {text = paste0(text, "<b>Spaargeld.</b> Op de peildatum van 2026 heeft belastingplichtige geen spaargeld. ")}
      
      # (b) Financiële producten
      if (df$finproduct > 0){text = paste0(text, "<b>Financiële producten </b> Daarnaast heeft belastingplichtige <i>", number_to_money(df$finproduct), "</i> financiële producten met een gemiddeld <i>rendement van ", percentify(df$finproduct_rendperc), "</i>. De aanwas is daarmee gelijk aan <i>", number_to_money(bereken_aanwas(df$finproduct, df$finproduct_rendperc)), "</i>. ")
      } else {text =  paste0(text, "<b>Financiële producten </b>Belastingplichtige heeft geen financiële producten ")}
      
      # (c) Overig bezit
      if (df$restbezit > 0){text = paste0(text, "<b>Onroerend goed.</b> Belastingplichtige beschikt ook over een tweede huis, t.w.v. <i>", number_to_money(df$restbezit), "</i>, met een gemiddeld rendement van <i>", percentify(df$restbezit_rendperc), "</i>. De aanwas is daarmee gelijk aan <i>", number_to_money(bereken_aanwas(df$restbezit, df$restbezit_rendperc)), "</i>. ")
      } else {text = paste0(text, "<b>Onroerend goed.</b> Belastingplichtige heeft geen onroerende goederen. ")}
      
      # (d) Schulden
      if (df$schuld > 0){text = paste0(text, "<b>Schulden.</b> Naast positieve vermogensbestanddelen, heeft belastingplichtige ook <i>", number_to_money(df$schuld), " schulden</i>. De <i>rente bedraagt ", percentify(df$schuld_rendperc), "</i>. De aanwas op schulden is daarmee gelijk aan <i>", number_to_money(bereken_aanwas(df$schuld, df$schuld_rendperc)), "</i>. Dit bedrag wordt in mindering gebracht bij het rendement voortvloeiend uit de positieve vermogensbestanddelen. <br><br>")
      } else {text = paste0(text, "<b>Schulden.</b> Belastingplichtige heeft geen schulden. <br><br>")}
      
      text = paste0(text, "<b> Vóór het toepassen van het heffingvrij inkomen en verliesverrekening is de belastinggrondslag daardoor gelijk aan ", number_to_money(df$aanwas), "</b>. ")
      
      # Vergelijking met overbruggingswetgeving
      
      verschil = df$aanwas - df$aanwas_forfait
      if (verschil > 0){text = paste0(text, "<b>Dat is ", number_to_money(verschil), " meer dan de grondslag die belastingplichtige zou hebben vóór toerekening van het heffingvrij vermogen onder de overbrugging wetgeving.</b> <br><br>")} 
      else if (verschil < 0){text = paste0(text, "<b>Dat is ", number_to_money(abs(verschil)), " minder dan de grondslag die belastingplichtige vóór toerekening van het heffingvrij vermogen  zou hebben onder de overbrugging wetgeving.</b> <br><br>")}
      else {text = paste0(text, "<b> Dat is exact evenveel als de grondslag die belastingplichtige vóór toerekening van het heffingvrij vermogen zou hebben onder de overbruggings wetgeving.</b> <br><br>")}
      
    }})
  
  output$grondslag_tekst_2 = renderText({ 
    
    if (is.null(input$upload_data)){data = case_data()} else {data = upload_data$data}
    selection = input$micro_1_select_case_selection
    df_long = subset(data, omschrijving == selection)
    naam = selection
    
    # text 
    aanwas_min = min(df_long$aanwas)
    aanwas_max = max(df_long$aanwas)
    jaar_min = subset(df_long, aanwas == aanwas_min)$jaar[1]
    jaar_max = subset(df_long, aanwas == aanwas_max)$jaar[1]
    text = paste0("De grondslag van ", naam, " verschilt over de jaren heen.
             U heeft enkel de data voor 2026 opgegeven. De aanwas voor resterende jaren wordt 
             op basis van gerandomiseerde rendementen doorgerekend naar de jaren 2027 tot 2045.
             Het resultaat hiervan kunt u grafisch inspecteren in de onderstaande grafiek. 
             Zo blijkt dat ", jaar_min, " het slechtste jaar voor ", naam, " was met een aanwas
             van ", number_to_money(aanwas_min), " en ", jaar_max, " het beste jaar met een aanwas
             van ", number_to_money(aanwas_max), ". Voor zover er sprake is van negatieve aanwas
             wordt deze in mindering gebracht bij de positieve grondslag in een ander jaar.
             Welk jaar dat is en hoeveel ", naam, " mag verrekenen is afhankelijk van de parameters
             van het door u gekozen stelsel in het volgende luik.")
    
    text
    
  })
  
  
  # PLOTS
  output$plot_aanwas_2026 = renderPlotly({
    
    inFile = input$upload_data
    if (is.null(inFile)){data = case_data()} else {data = upload_data$data}
    
    df = subset(data, omschrijving == input$micro_1_select_case_selection & jaar == 2026)
    
    df = data.frame(
      x = rep(c(rep("Spaargeld", nrow(df)),  rep("Financiële\nproducten", nrow(df)), rep("Onroerend\ngoed", nrow(df)),  rep("Schulden", nrow(df)), rep("Totaal", nrow(df))), 2),
      y = c(df$spaargeld, df$finproduct, df$restbezit, df$schuld, (df$spaargeld + df$finproduct + df$restbezit - df$schuld), df$spaargeld*(df$spaargeld_rendperc/100), df$spaargeld*(df$finproduct_rendperc/100),
            df$restbezit*(df$restbezit_rendperc/100), df$schuld*(df$schuld_rendperc/100), (df$spaargeld*(df$spaargeld_rendperc/100) + df$spaargeld*(df$finproduct_rendperc/100) + df$restbezit*(df$restbezit_rendperc/100) - df$schuld*(df$schuld_rendperc/100))),
      z = c(rep("Vermogen", nrow(df)*5), rep("Aanwas", nrow(df)*5))
    ) %>%
      mutate(text = paste0(z, ": ", number_to_money(y)))
    
    df$x = factor(df$x, levels = unique(df$x), ordered = T)
    
    ggplotly(
      ggplot(df) +
       geom_bar(stat = "identity",  aes(x = x, y = y, fill = z, text = text), alpha = 0.9, color = "black") +
       ylab("Vermogen en aanwas in €") + 
       scale_fill_manual(values = c("grey30", "grey90")) + 
       theme_minimal() + 
       theme(legend.position = 'top', axis.title.x = element_blank(), axis.text.x = element_text(angle = 0)),
    tooltip="text"
    ) %>% 
      layout(yaxis = list(showticklabels = T), xaxis = list(showticklabels = T), hovermode = "x unified", showlegend = F)
    
  })
  
  output$plot_aanwas = renderPlotly({
    
    if (is.null(input$upload_data)){data = case_data()} else {data = upload_data$data}
    
    df = subset(data, omschrijving == input$micro_1_select_case_selection)
    df$text = paste0("Aanwas ", df$jaar, " = ", number_to_money(df$aanwas))
    df$jaar = as.numeric(df$jaar)
    
    if (nrow(df) > 0){
      plot_ly(x = df$jaar, y = df$aanwas, text = df$text, type = 'scatter', mode = 'lines+markers', color = I('black'), name = "Aanwas", hoverinfo = 'y', hovertemplate = '€%{y}', showlegend = F) %>% 
        layout(yaxis = list(showticklabels = T), xaxis = list(showticklabels = T), hovermode = "x unified", showlegend = T)
    } else {}
    
  })
  
  ############ 1.2. MICRO ANALYSES - STAP 2: Welke variant wilt u doorrekenen? ###########
  
  ############ SIDEBAR ###########
  
  # KNOP VARIANT TOEVOEGEN
  observeEvent(input$add_variant, {
    
    # 1. als schijf 2 lager ligt dan hvi 
    if (!is.na(input$schijf_2) & input$schijf_2 <= input$hvi){
      warning("Casus niet toegevoegd", "De ondergrens van de tweede schijf kan niet lager zijn dan of gelijk zijn aan het heffing vrij inkomen.")
      
    # 2. als schijf 3 lager ligt dan schijf 2 of hvi  
    } else if (!is.na(input$schijf_3) & input$schijf_3 <= input$hvi | !is.na(input$schijf_3) & !is.na(input$schijf_2) & input$schijf_3 <= input$schijf_2) {
      warning("Casus niet toegevoegd", "De ondergrens van de derde schijf kan niet lager zijn dan of gelijk zijn aan de ondergrens van schijf 2 of het heffingvrij inkomen.")
      
    # 3. als geen tarief
    } else if (is.na(input$tarief_1)){
      warning("Casus niet toegevoegd", "U heeft geen tarief bepaald.")
      
    # 4. als tarief incompleet
    } else if (!is.na(input$schijf_2) & is.na(input$tarief_2) | !is.na(input$schijf_3) & is.na(input$tarief_3)){
      warning("Casus niet toegevoegd", "U bent één of meerdere tarieven vergeten in te voeren.")
    
    # Als correct ingevuld  
    } else {
      
      if (is.null(input$upload_data_variant)){data = variant_data_input()} 
      if (!is.null(input$upload_data_variant)){data = upload_data_variant$data}
      
      if(isolate(input$naam_variant) %in% data$variant){naam_variant_new = paste0(isolate(input$naam_variant), " ", nrow(data)+1)
      } else {naam_variant_new = isolate(input$naam_variant)}
      
      dat = data.frame(
        variant = naam_variant_new, hvi = isolate(input$hvi), verlies_drempel = isolate(input$verlies_drempel),
        cf = isolate(input$verlies_voor), cb = isolate(input$verlies_achter), schijf_2 = isolate(input$schijf_2),
        schijf_3 = isolate(input$schijf_3), tarief_1 = isolate(input$tarief_1), tarief_2 = isolate(input$tarief_2),
        tarief_3 = isolate(input$tarief_3)) 
      
      # variant toevoegen
      if (is.null(input$upload_data_variant)){variant_data_input() %>%  bind_rows(dat) %>% variant_data_input()
      } else { upload_data_variant$data = upload_data_variant$data %>%  bind_rows(dat)}
      
    } 
    
  }) 
  
  ############ TAB 1 ############ 
  
  empty_dat_variant = data.frame( variant = "", hvi = NA, verlies_drempel = NA, cf = NA, cb = NA, schijf_2 = NA, schijf_3 = NA, tarief_1 = NA, tarief_2 = NA, tarief_3 = NA)
  
  # KNOP DOWNLOAD TEMPLATE
  output$download_template_variant = downloadHandler(
    filename = function() {paste("sandbox3_template_variant.xlsx", sep="")},
    content = function(file) {
      write_xlsx(empty_dat_variant, file)
  })
  
  # OPLADEN DATA 
  upload_data_variant = reactiveValues(data = NULL)
  
  gen_empty_data_variant = function(){
    data = empty_dat_variant %>% filter(row_number() %in% -1) %>% setNames(colnames_variant_original)
    return(data)
  }
  
  gen_upload_data_variant = function(){
    
    if (nrow(readxl::read_xlsx(input$upload_data_variant$datapath)) > 0){
      upload_data_variant$data = readxl::read_xlsx(input$upload_data_variant$datapath) %>%
        setNames(colnames_variant_original)
    # generate empty data frame if file does not contain any entries 
    } else {
      upload_data_variant$data = gen_empty_data_variant() 
    }}
  
  observeEvent(input$upload_data_variant, {upload_data_variant$data = gen_upload_data_variant()})
  
  # VOORGEPROGRAMEERDE DATA 
  variant_data_input = reactiveVal(variant_data)
  
  # OUTPUT TABEL
  output$variant_data = renderDataTable({
    if (is.null(input$upload_data_variant)){data = variant_data_input()} else {data = upload_data_variant$data}
    data %>% setNames(colnames_variant_print)
  }, server = F, rownames = F, selection = 'single', options = list(paging =T, pageLength = 16, scrollX = T))
  
  # KNOP RESET DATA
  observeEvent(input$reset_data_variant, {
    
    if (is.null(input$upload_data_variant)){variant_data_input() %>% filter(row_number() %in% -1) %>% variant_data_input()
    } else {upload_data_variant$data =  data.frame(variant = as.character(), 
                                                   hvi = as.numeric(), verlies_drempel = as.numeric(),
                                                   cf = as.numeric(), cb = as.numeric(), schijf_2 = as.numeric(), 
                                                   schijf_3 = as.numeric(), tarief_1 = as.numeric(), tarief_2 = as.numeric(),
                                                   tarief_3 = as.numeric())}
    
  })
  
  # KNOP VERWIJDER VARIANT 
  observeEvent(input$delete_variant, {
    if (is.null(input$upload_data_variant)){  
      remove = variant_data_input()[input$variant_data_rows_selected, "variant"]
      variant_data_input() %>%  subset(., variant != remove) %>% variant_data_input()
    } else { 
      upload_data_variant$data = upload_data_variant$data[-input$variant_data_rows_selected,]
    }
  })
  
  
  # KNOP DOWNLOAD 
  selected_data_variant = function(input = NULL){
    if (is.null(input)){data = variant_data_input()} else {data = upload_data_variant$data}
    return(data)
  }
  
  output$download_variants = downloadHandler(
    filename = function() {paste("sandbox3_variants.xlsx", sep="")},
    content = function(file) {write_xlsx(selected_data_variant(input$upload_data_variant), file)}
  )
  
  ############ TAB 2 ############ 
  
  # DATA VARIANT NAMES
  output$micro_1_select_variant = renderUI({
    if (is.null(input$upload_data_variant)){data = variant_data_input()} else {data = upload_data_variant$data}
    selectInput("micro_1_select_variant_selection", label = NULL, choices = data$variant, width = '100%')
  })
  
  # TEKST VARIANT 
  output$variant_tekst = renderText({
    
    if (is.null(input$upload_data_variant)){dat_variant = variant_data_input()} else {dat_variant = upload_data_variant$data}
    
    dat_variant = subset(dat_variant, variant == input$micro_1_select_variant_selection)
    
    if (nrow(dat_variant) > 0){
      
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
      s2 = dat_variant$schijf_2; s3 = dat_variant$schijf_3
      t1 = dat_variant$tarief_1; t2 = dat_variant$tarief_2; t3 = dat_variant$tarief_3
      schijf_aantal = 1; if (!is.na(t3) & !is.na(s3)){schijf_aantal = 3};  if (is.na(t3) & is.na(s3) & !is.na(t2) & !is.na(s2)){schijf_aantal = 2} 
      
      # text 
      text = paste0("Variant <i>", naam_variant, "</i> kent een heffingvrij inkomen <i>", number_to_money(hvi), "</i>. 
                    Iedereen die een inkomen uit vermogen heeft onder deze grens, betaalt geen belasting in box 3. 
                    De hoogte van het heffingvrij inkomen bepaalt eveneens het aantal burgers dat een beroep kan doen op verliesverrekening.
                    Variant voorziet een verliesverrekeningsdrempel van <i>", number_to_money(vv_drempel), "</i>; <i>",  vv_cf, " jaar</i> voorwaartse
                    verliesverrekening en <i>", vv_cb, " jaar</i> achterwaartse verliesverrekening. Iedere burger met 
                    (1) een belastbaar inkomen uit vermogen in belastingjaar ", jaar_nu, ", d.w.z. een inkomen
                    boven het heffingvrij inkomen en (2) onverrekende verliezen uit de jaren <i>", jaar_nu - vv_cf, " tot ", jaar_nu + vv_cb, "</i> 
                    kan deze in mindering brengen bij de grondslag van het belastingjaar. ")
      
      if (schijf_aantal == 1){text = paste0(text, "Variant kent een vlaktaks. Alle belastingplichtigen betalen <i>", percentify(t1), "</i> belasting over de grondslag van het belastingjaar.")}
      if (schijf_aantal == 2){text = paste0(text, "Variant kent een progressief tarief met twee schijven. Belastingplichtigen betalen <i>", percentify(t1), "</i> belasting over de grondslag tot <i>", number_to_money(s2), 
                                            "</i> en <i>", percentify(t2), "</i> over de rest."  )}
      if (schijf_aantal == 3){text = paste0(text, "Variant kent een progressief tarief met drie schijven.Belastingplichtigen betalen <i>", percentify(t1), "</i> belasting over de grondslag tot <i>", number_to_money(s2), 
                                            "</i>; <i>", percentify(t2), "</i> tot <i>", number_to_money(s3), "</i> en <i>", percentify(t3), "</i> over de rest." )}
      
      # toelichting grafiek
      text = paste0(text, "<br><br><i>De grafiek verschaft een visualisatie van de door u gespecificeerde variant 
                     voor een voorbeeld belastingplichtige. Het grijze gebied boven de nul is het heffingvrij inkomen en onder de 
                     nul niet verrekenbare verliezen (daar deze onder de verliesverrekeningsdrempel zitten). Het groene gebied toont 
                     de aanwas die in aanmerking komt voor achterwaartse verliesverrekening en het rode gebied de aanwas die in aanmerking
                     komt voor voorwaartse verliesverrekening. Beweeg met uw muis over de jaren om te inspecteren of de aanwas van dat
                     jaar onder het heffingvrij inkomen, belastbaar inkomen, onverrekenbaar verlies, of verrekenbaar verlies valt. U kunt het 
                     belastingjaar veranderen door de slider te verschuiven. </i> <br><br>")
      
    } else {
      text = "Er is geen data beschikbaar. Voeg data toe met behulp van de tool. <br><br><br>"
    }
    
    text
    
  })
  
  
  # PLOT VARIANT
  output$plot_variant = renderPlotly({
    
    
    input_jaar = as.numeric(input$plot_variant_jaar)
    
    if (is.null(input$upload_data_variant)){dat_variant = variant_data_input()} else {dat_variant = upload_data_variant$data}
    dat_variant = subset(dat_variant, variant == input$micro_1_select_variant_selection)
    
    if (nrow(dat_variant) > 0){
      
      # PARAMETERS
      naam_variant = input$micro_1_select_variant_selection
      hvi = dat_variant$hvi
      vv_drempel = -dat_variant$verlies_drempel
      vv_cf = input_jaar - dat_variant$cf - 0.5; if (vv_cf < 2026){vv_cf = 2026 - 0.5}
      vv_cb = input_jaar + dat_variant$cb + 0.5; if (vv_cb > 2045){vv_cb = 2045 + 0.5}
      s2 = dat_variant$schijf_2; s3 = dat_variant$schijf_3; 
      t1 = dat_variant$tarief_1; t2 = dat_variant$tarief_2; t3 = dat_variant$tarief_3
      
      schijf_aantal = 1; if (!is.na(t3) & !is.na(s3)){schijf_aantal = 3};  if (is.na(t3) & is.na(s3) & !is.na(t2) & !is.na(s2)){schijf_aantal = 2}  
      
      # HIER AANPASSEN!!! randomly generate case knop
      aanwas = c(0.5*hvi, hvi + 0.5*hvi, 2*hvi, 3*hvi, 4*hvi,
                 3*hvi, 2*hvi, 2*vv_drempel, 0.7*vv_drempel, 4*hvi, 3*vv_drempel, 
                 1.5*vv_drempel, 0.5*vv_drempel, 1.3*hvi, 2*hvi, 0.2*hvi,
                 0.5*hvi, 0.5*vv_drempel, 1.1*vv_drempel, 0.2*vv_drempel)
      jaar = c(2026:(2025+length(aanwas)))
      
      ymax = 1.75*max(hvi, vv_drempel, aanwas)
      ymin = -ymax
      space = 0.2*hvi
      
      
      s1_text = paste0("<b>Schijf 1:</b> ", percentify(t1), " belasting over aanwas.")
      
      # PLOT
      fig = plot_ly(showlegend = F, height = 500) %>%
        
        # hvi en vv drempel
        add_polygons(x=c(2025.5,2025.5,2045.5,2045.5),y=c(space,hvi,hvi,space), color=I("grey70"), opacity = 0.3,  name = "<b>Heffingvrij inkomen:</b> aanwas is niet belastbaar en \nkan ook niet verrekend worden met verliezen.", hoverinfo = 'text', hovertemplate = '%{text}') %>% 
        add_polygons(x=c(2025.5,2025.5,2045.5,2045.5),y=c(vv_drempel,-space,-space, vv_drempel), color=I("grey70"), opacity = 0.3,  name = "<b>Verlies onder verliesverrekenings drempel:</b>\nverlies kan niet verrekend worden.", hoverinfo = 'text', hovertemplate = '%{text}') %>% 
        
        # verliesverrekening 
        add_polygons(x=c(vv_cf,vv_cf,input_jaar,input_jaar),y=c(vv_drempel-space,ymin,ymin,vv_drempel-space), color=I("red"), opacity = 0.3, name = "<b>Voorwaartse verliesverrekening:</b>\nverlies mag verrekend worden met grondslag belastingjaar.",  hoverinfo = 'text', hovertemplate = '%{text}') %>%
        add_polygons(x=c(input_jaar, input_jaar, vv_cb, vv_cb), y=c(vv_drempel-space,ymin,ymin,vv_drempel-space), color=I("green"), opacity = 0.3, name = "<b>Achterwaartse verliesverrekening:</b>\n toekomstig verlies mag verrekend worden met grondslag belastingjaar.",  hoverinfo = 'text', hovertemplate = '%{text}') %>%
        
        # huidig jaar
        add_trace(x=input_jaar, y=c(ymin - 10, ymax + 10), opacity = 0.7, color=I("grey20"), mode = 'lines', hovertemplate = '') 
      
      # schijven
      if (schijf_aantal == 1){
        fig = fig %>% add_polygons(x=c(2025.5,2025.5,2045.5,2045.5),y=c(hvi, ymax, ymax, hvi), color=I("grey70"), opacity = 0,  name = s1_text, hoverinfo = 'text', hovertemplate = '%{text}')}
      if (schijf_aantal == 2){
        s1_text = paste0("<b>Schijf 1:</b> ", percentify(t1), " belasting over aanwas tot ", number_to_money(s2), ".")
        s2_text = paste0(s1_text, "\n<b>Schijf 2:</b> ", percentify(t2), " belasting over aanwas boven ", number_to_money(s2), ".")
        fig = fig %>% 
          add_polygons(x=c(2025.5,2025.5,2045.5,2045.5),y=c(hvi, s2-1, s2-1, hvi), color=I("grey70"), opacity = 0,  name = s1_text, hoverinfo = 'text', hovertemplate = '%{text}') %>%
          add_polygons(x=c(2025.5,2025.5,2045.5,2045.5),y=c(s2, ymax, ymax, s2), color=I("grey70"), opacity = 0,  name = s2_text, hoverinfo = 'text', hovertemplate = '%{text}') }
      if (schijf_aantal == 3){
        s1_text = paste0("<b>Schijf 1:</b> ", percentify(t1), " belasting over aanwas tot ", number_to_money(s2), ".")
        s2_text = paste0(s1_text, "\n<b>Schijf 2:</b> ", percentify(t2), " belasting over aanwas tot ", number_to_money(s3), ".")
        s3_text = paste0(s2_text, "\n<b>Schijf 3:</b> ", percentify(t3), " belasting over aanwas boven ", number_to_money(s3), ".")
        
        fig = fig %>% 
          add_polygons(x=c(2025.5,2025.5,2045.5,2045.5),y=c(hvi, s2-1, s2-1, hvi), color=I("grey70"), opacity = 0,  name = s1_text, hoverinfo = 'text', hovertemplate = '%{text}') %>%
          add_polygons(x=c(2025.5,2025.5,2045.5,2045.5),y=c(s2, s3-1, s3-1, s2), color=I("grey70"), opacity = 0,  name = s2_text, hoverinfo = 'text', hovertemplate = '%{text}') %>%
          add_polygons(x=c(2025.5,2025.5,2045.5,2045.5),y=c(s3, ymax, ymax, s3), color=I("grey70"), opacity = 0,  name = s3_text, hoverinfo = 'text', hovertemplate = '%{text}')
      }
      
      fig = fig %>%
        add_trace(x=~jaar, y=~aanwas, opacity = 0.7, color=I("black"), line = list(dash = 'dash'), name = "<b>Aanwas</b>", hovertemplate = '%{y}') %>%
        layout(yaxis = list(showticklabels = F), hovermode = "x unified", showlegend = T)
      
      fig
    } else {}
    
  })
  
  ############# 1.3. MICRO ANALYSES - STAP 3: RESULTATEN #############
  
  ############ TAB 1 ###########
  
  gen_case_effects = function(){
    
    if (is.null(input$upload_data_variant)){variant_data = variant_data_input()} else {variant_data = upload_data_variant$data}
    if (is.null(input$upload_data)){case_data = case_data()} else {case_data = upload_data$data}
    
    varnames = c("belastingplichtige", "variant", "risico", "belasting €", "belasting (% aanwas)", "verlies", "verrekend verlies", "verrekend verlies (% verlies)", 
                 "vermogen", "aanwas", "grondslag", "grondslag (% aanwas)", "spaargeld", "financiële producten", "overig bezit", "schuld", "rendement spaargeld (%)", "rendement financiële producten (%)", 
                 "rendement overig bezit (%)", "rendement schuld (%)", "hvi", "verlies drempel", "CF", "CB", "S2 €", "S3 €", "T1 %", "T2 %", "T3 %")
    
    if (nrow(variant_data) > 0 & nrow(case_data) > 0){
      
      temp = list()
      # elke case
      for (i in c(1:length(unique(case_data$omschrijving)))){
        # elke variant
        for (j in c(1:nrow(variant_data))){
          temp[[length(temp) + 1]] = gen_combi(dat_variant = variant_data[j,], dat_case = subset(case_data, omschrijving == unique(case_data$omschrijving)[i]))
          
        }}
      
      temp = do.call(rbind, temp) %>% setNames(varnames)
    } else {
      temp = variant_case_effects %>% setNames(varnames) %>% filter(., row_number() %in% -1)
    }
    
    return(temp)
    
  }
  
  # OUTPUT TABEL
  output$variant_case_effects = renderDataTable({
    
    gen_case_effects()
    
  }, server = F, rownames = F, selection = 'single', options = list(paging =T, pageLength = 16, scrollX = T))
  
  
  dataModal = function(failed = FALSE) {
    modalDialog(
      title = "Waarschuwing", 
      "U staat op het punt alle casus en variant data te verwijderen.
        Weet u zeker dat u deze actie wil voltooien?",
      footer = tagList(
        actionButton("annuleer_reset", "Annuleer"),
        actionButton("ok_reset", "Ga door")
      ))
  }
  
  
  # KNOP RESET DATA
  observeEvent(input$reset_variant_case_effects, {
    
    showModal(dataModal())
    
    observeEvent(input$ok_reset, {
      
      if (is.null(input$upload_data_variant)){
        variant_data_input() %>%
          filter(row_number() %in% -1) %>%
          variant_data_input()
      } else {
        upload_data_variant$data = upload_data_variant$data %>%
          filter(row_number() %in% -1)
      }
      
      if (is.null(input$upload_data)){
        case_data() %>%
          filter(row_number() %in% -1) %>%
          case_data()
        
      } else {
        upload_data$data = upload_data$data %>%
          filter(row_number() %in% -1)}
      
      removeModal()
      
    })
    
    observeEvent(input$annuleer_reset, {removeModal()})
    
  })
  
  # KNOP VERWIJDER VARIANT 
  observeEvent(input$delete_variant_effects, {
    
    temp = gen_case_effects()
    variant_id = as.character(temp$variant[input$variant_case_effects_rows_selected])
    
    if (is.null(input$upload_data_variant)){
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
    
    if (is.null(input$upload_data)){
      case_data() %>%
        subset(omschrijving %notin% case_id) %>%
        case_data()
      
    } else {
      upload_data$data = upload_data$data %>%
        subset(omschrijving %notin% case_id)}
    
  })
  
  # KNOP DOWNLOAD
  output$download_variants_case_effects = downloadHandler(
    
    filename = function() {paste("sandbox3_resultaten.xlsx", sep="")},
    content = function(file) {write_xlsx(gen_case_effects(), file)}
    
  )
  
  ############ TAB 2 ###########
  
  ###### SIDEBAR ######
  
  # DATA CASE NAMES
  output$micro_3_select_case = renderUI({
    if (is.null(input$upload_data)){data = case_data()} else {data = upload_data$data}
    data = subset(data, jaar == 2026)
    selectInput("micro_3_select_case_selection", label = NULL, choices = data$omschrijving)
  })
  
  # DATA VARIANT NAMES
  output$micro_3_select_variant = renderUI({
    if (is.null(input$upload_data_variant)){data = variant_data_input()} else {data = upload_data_variant$data}
    selectInput("micro_3_select_variant_selection", label = NULL, choices = data$variant)
  })
  
  
  # KNOP DOWNLOAD
  output$download_micro = downloadHandler(
    
    filename = function(){
      paste("sandbox3_microvoorbeeld.xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(rbind(gen_tab_micro(), c(gen_micro_text(), "")), file)
    }
  )
  
  ###### MAIN PANEL ######
  
  # MICRO VOORBEELD TABEL
  gen_tab_micro = function(){
    # variant data 
    if (is.null(input$upload_data_variant)){variant_dat = variant_data_input()} else {variant_dat = upload_data_variant$data}
    variant_id = input$micro_3_select_variant_selection
    variant_dat = subset(variant_dat, variant == variant_id)
    
    # case data
    if (is.null(input$upload_data)){case_dat = case_data()} else {case_dat = upload_data$data}
    case_id = input$micro_3_select_case_selection
    case_dat = subset(case_dat, omschrijving == case_id)
    case_dat = verreken_verlies(case_dat, hvi = variant_dat$hvi, cf = variant_dat$cf, cb = variant_dat$cb, drempel = variant_dat$verlies_drempel)
    
    if (input$micro_3_select_year == "Alle jaren"){case_dat = case_dat} else {case_dat = subset(case_dat, jaar == as.numeric(input$micro_3_select_year))}
    
    # aanwas
    if ( nrow( subset(case_dat, aanwas > 0) ) > 0 ){ aanwas = sum( subset(case_dat, aanwas > 0)$aanwas , na.rm = T )} else { aanwas = 0 }
    
    # heffing vrij inkomen 
    case_dat$aanwas_na_hvi = case_dat$aanwas - variant_dat$hvi
    case_dat$aanwas_na_hvi[case_dat$aanwas_na_hvi<0] = 0
    aanwas_na_hvi = sum( case_dat$aanwas_na_hvi, na.rm = T )
    
    # verrekenbaar verlies 
    case_dat$vv = case_dat$cb + case_dat$cf
    vv = sum(case_dat$vv, na.rm = T)
    
    # belasting
    grondslag = aanwas_na_hvi - vv
    belasting = bepaal_belasting(grondslag, schijf_2 = variant_dat$schijf_2,schijf_3 = variant_dat$schijf_3, tarief_1 = variant_dat$tarief_1, tarief_2 = variant_dat$tarief_2,tarief_3 = variant_dat$tarief_3)
    
    # tarieven 
    if (!is.na(variant_dat$tarief_1)){
      t1 = paste0(percentify(variant_dat$tarief_1), " tarief")
      aanwas_1 = belasting$aanwas[1]
      belasting_1 = belasting$belasting[1]
    } else {
      t1 = "n.v.t."
      aanwas_1 = "n.v.t."
      belasting_1 = "n.v.t."
    }
    
    if (!is.na(variant_dat$tarief_2)){
      t2 = paste0(percentify(variant_dat$tarief_2), " tarief")
      aanwas_2 = belasting$aanwas[2]
      belasting_2 = belasting$belasting[2]
    } else {
      t2 = "n.v.t."
      aanwas_2 = "n.v.t."
      belasting_2 = "n.v.t."
    }
    
    if (!is.na(variant_dat$tarief_3)){
      t3 = paste0(percentify(variant_dat$tarief_3), " tarief")
      aanwas_3 = belasting$aanwas[3]
      belasting_3 = belasting$belasting[3]
    } else {
      t3 = "n.v.t."
      aanwas_3 = "n.v.t."
      belasting_3 = "n.v.t."
    }
    
    if(variant_dat$hvi > 0){hvi_tekst = aanwas - aanwas_na_hvi} else {hvi_tekst = "n.v.t."}
    if(variant_dat$cf > 0 | variant_dat$cb > 0){vv_tekst = vv} else {vv_tekst = "n.v.t."}
    
    
    tab  = rbind(
      c("Aanwas ...", aanwas), 
      c("... waarvan heffingvrij inkomen", hvi_tekst), 
      c("... waarvan verrekenbaar verlies", vv_tekst),
      c("Grondslag ...", grondslag),
      c("... waarvan in schijf 1", aanwas_1),
      c("... waarvan in schijf 2", aanwas_2),
      c("... waarvan in schijf 3", aanwas_3),
      c("Belasting ...", sum(belasting$belasting, na.rm = T)),
      c(paste0("... waarvan in schijf 1 (", t1, ")") , belasting_1),
      c(paste0("... waarvan in schijf 2 (", t2, ")"), belasting_2),
      c(paste0("... waarvan in schijf 3 (", t3, ")"), belasting_3)
    ) %>% data.frame() %>%
      setNames(c("", "Bedrag in €"))
    
    return(tab)
  }
  
  output$tab_micro = renderDataTable({
    
    gen_tab_micro()
    
  }, options = list(dom = 't', pageLength = 11), rownames = F)
  
  gen_micro_text = function(){
    
    # variant data 
    if (is.null(input$upload_data_variant)){variant_dat = variant_data_input()} else {variant_dat = upload_data_variant$data}
    variant_id = input$micro_3_select_variant_selection
    variant_dat = subset(variant_dat, variant == variant_id)
    
    # case data
    if (is.null(input$upload_data)){case_dat = case_data()} else {case_dat = upload_data$data}
    case_id = input$micro_3_select_case_selection
    case_dat = subset(case_dat, omschrijving == case_id)
    
    # tarieven en schijven 
    s2 = variant_dat$schijf_2; s3 = variant_dat$schijf_3
    t1 = variant_dat$tarief_1; t2 = variant_dat$tarief_2; t3 = variant_dat$tarief_3
    schijf_aantal = 1; if (!is.na(t3) & !is.na(s3)){schijf_aantal = 3};  if (is.na(t3) & is.na(s3) & !is.na(t2) & !is.na(s2)){schijf_aantal = 2} 
    
    # data voor alle jaren
    if (input$micro_3_select_year == "Alle jaren"){
      
      temp = subset(gen_case_effects(), variant == variant_id & belastingplichtige == case_id)
      
      case_dat$aanwas_na_hvi = case_dat$aanwas - variant_dat$hvi
      case_dat$aanwas_na_hvi[case_dat$aanwas_na_hvi<0] = 0
      aanwas_na_hvi = sum( case_dat$aanwas_na_hvi, na.rm = T )
      
      text = paste0("<b>Aanwas.</b> ", temp$belastingplichtige, " heeft gedurende de periode van 2026 tot 2045 in totaal ", number_to_money(temp$aanwas), " aanwas genoten.
                    <b>Heffingvrij inkomen.</b> Ter berekening van de belasting grondslag is voor zover van toepassing elk jaar het heffingvrij inkomen van ", number_to_money(temp$hvi), "
                    in mindering gebracht bij de aanwas. Belastingplichtige kon in totaal " , number_to_money(temp$aanwas - aanwas_na_hvi) , " verrekenen. Na verrekening
                    van het heffingvrij inkomen resteerde er ", number_to_money(aanwas_na_hvi), " aanwas.
                    
                    <b>Verliesverrekening.</b> Belastingplichtige mag in deze jaren verliezen groter dan ", number_to_money(variant_dat$verlies_drempel), 
                    " verrekenen van ", variant_dat$cf, " jaar voor het belastingjaar en ", variant_dat$cb, " jaar na het belastingjaar (teruggaand tot 2026). 
                      Belastingplichtige heeft in totaal ", number_to_money(temp$verlies), " verlies geleden, waarvan hij ", number_to_money(temp[, "verrekend verlies"]), " (", 
                    percentify(temp[, "verrekend verlies (% verlies)"]), ") heeft mogen verrekenen. 
                      
                      <b>Grondslag.</b>  De grondslag na verliesverrekening is daarmee gelijk aan ", 
                    number_to_money(temp$grondslag), ". Dit is ", percentify(temp[,"grondslag (% aanwas)"]), " van de totale aanwas.<br><br>")
      
      if (schijf_aantal == 1) {text = paste0(text, "<b>Belasting.</b> Belastingplichtige betaalt over deze grondslag ", percentify(t1), " belasting. ")
      } else if (schijf_aantal == 2){text = paste0(text, "<b>Belasting.</b> Belastingplichtige betaalt ", percentify(t1), "belastig over grondslag tot ", number_to_money(s2),  " en ", percentify(t2), " over het resterend bedrag. ")
      } else if (schijf_aantal == 3){text = paste0(text, "<b>Belasting.</b> Belastingplichtige betaalt ", percentify(t1), "belastig over grondslag tot ", number_to_money(s2),  ", ", percentify(t2), " over grondslag tot ", number_to_money(s3), " en ", percentify(t3), " over het resterend bedrag. ")}
      
      text = paste0(text, "In totaal heeft belastingplichtige ", number_to_money(temp[, "belasting €"]), " betaald. Dit is ", percentify(temp[, "belasting (% aanwas)"]), " van zijn aanwas.")
      
    } else {
      
      jaar_nu = as.numeric(input$micro_3_select_year)
      
      case_dat = verreken_verlies(case_dat, hvi = variant_dat$hvi, cf = variant_dat$cf, cb = variant_dat$cb, drempel = variant_dat$verlies_drempel) 
      
      case_dat_jaar = subset(case_dat, jaar == jaar_nu)
      case_dat_jaar$aanwas_na_hvi = case_dat_jaar$aanwas - variant_dat$hvi
      case_dat_jaar$aanwas_na_hvi[case_dat_jaar$aanwas_na_hvi < 0] = 0
      
      vv = case_dat_jaar$cb + case_dat_jaar$cf
      
      vv_dat = subset(case_dat, jaar >= jaar_nu - variant_dat$cf & jaar <= jaar_nu + variant_dat$cb)
      if (nrow(subset(vv_dat, aanwas < 0)) > 0){ verlies = sum( subset(vv_dat, aanwas < 0)$aanwas, na.rm = T) } else { verlies = 0 }
      belasting = sum(bepaal_belasting(grondslag = case_dat_jaar$grondslag, schijf_2 = variant_dat$schijf_2, schijf_3 = variant_dat$schijf_3, tarief_1 = variant_dat$tarief_1, tarief_2 = variant_dat$tarief_2, tarief_3 = variant_dat$tarief_2)$belasting, na.rm = T)
      
      if(case_dat_jaar$aanwas > 0){
        vv_perc = (vv / case_dat_jaar$aanwas)*100
        belasting_perc = (belasting / case_dat_jaar$aanwas)*100
        grondslag_perc = (case_dat_jaar$grondslag / case_dat_jaar$aanwas)*100
      } else {
        vv_perc = 0
        belasting_perc = 0
        grondslag_perc = 0
      }
      
      text = paste0("<b>Aanwas.</b> ", case_dat_jaar$omschrijving, " heeft in ", jaar_nu, " ", number_to_money(case_dat_jaar$aanwas), " aanwas genoten.
                    <b>Heffingvrij inkomen.</b> Ter berekening van de belasting grondslag is het heffingvrij inkomen van ", number_to_money(variant_dat$hvi), "
                    in mindering gebracht bij de aanwas. Na verrekening van het heffingvrij inkomen resteerde er ", number_to_money(case_dat_jaar$aanwas_na_hvi), " aanwas.
                    
                    <b>Verliesverrekening.</b> Belastingplichtige mag in het belastingjaar verliezen groter dan ", number_to_money(variant_dat$verlies_drempel), 
                    " verrekenen van ", variant_dat$cf, " jaar voor het belastingjaar en ", variant_dat$cb, " jaar na het belastingjaar (teruggaand tot 2026). 
                      Belastingplichtige heeft in totaal ", number_to_money(verlies), " verlies geleden, waarvan hij ", number_to_money(vv), " (", 
                    percentify(vv_perc), ") heeft mogen verrekenen. 
                      
                      <b>Grondslag.</b>  De grondslag na verliesverrekening is daarmee gelijk aan ", 
                    number_to_money(case_dat_jaar$grondslag), ". Dit is ", percentify(grondslag_perc), " van de totale aanwas.<br><br>")
      
      if (schijf_aantal == 1) {text = paste0(text, "<b>Belasting.</b> Belastingplichtige betaalt over deze grondslag ", percentify(t1), " belasting. ")
      } else if (schijf_aantal == 2){text = paste0(text, "<b>Belasting.</b> Belastingplichtige betaalt ", percentify(t1), "belastig over grondslag tot ", number_to_money(s2),  " en ", percentify(t2), " over het resterend bedrag. ")
      } else if (schijf_aantal == 3){text = paste0(text, "<b>Belasting.</b> Belastingplichtige betaalt ", percentify(t1), "belastig over grondslag tot ", number_to_money(s2),  ", ", percentify(t2), " over grondslag tot ", number_to_money(s3), " en ", percentify(t3), " over het resterend bedrag. ")}
      
      text = paste0(text, "Belastingplichtige betaalt ", number_to_money(belasting), " belasting. Dit is ", percentify(belasting_perc), " van zijn aanwas.")
      
      
    }
    
    return(text)
  }
  
  # MICRO VOORBEELD TEKST 
  output$micro_tekst = renderText({gen_micro_text()})
  
  # MICRO VOORBEELD PLOT
  output$plot_micro = renderPlotly({
    
    if (input$micro_3_select_year == "Alle jaren"){input_jaar = 2035} else {input_jaar = as.numeric(input$micro_3_select_year)}
    
    # variant data 
    if (is.null(input$upload_data_variant)){variant_dat = variant_data_input()} else {variant_dat = upload_data_variant$data}
    variant_dat = subset(variant_dat, variant == input$micro_3_select_variant_selection)
    
    # case data
    if (is.null(input$upload_data)){case_dat = case_data()} else {case_dat = upload_data$data}
    case_dat = subset(case_dat, omschrijving == input$micro_3_select_case_selection)
    
    
    # parameters
    hvi = variant_dat$hvi
    vv_drempel = -variant_dat$verlies_drempel
    vv_cf = input_jaar - variant_dat$cf - 0.5; if (vv_cf < 2026){vv_cf = 2026 - 0.5}
    vv_cb = input_jaar + variant_dat$cb + 0.5; if (vv_cb > 2045){vv_cb = 2045 + 0.5}
    s2 = variant_dat$schijf_2; s3 = variant_dat$schijf_3; 
    t1 = variant_dat$tarief_1; t2 = variant_dat$tarief_2; t3 = variant_dat$tarief_3
    
    schijf_aantal = 1; if (!is.na(t3) & !is.na(s3)){schijf_aantal = 3};  if (is.na(t3) & is.na(s3) & !is.na(t2) & !is.na(s2)){schijf_aantal = 2}  
    
    aanwas = case_dat$aanwas
    jaar = case_dat$jaar
    
    ymax = 1.75*max(hvi, vv_drempel, aanwas)
    ymin = -ymax
    space = 0.01*hvi
    
    
    s1_text = paste0("<b>Schijf 1:</b> ", percentify(t1), " belasting over aanwas.")
    
    # PLOT
    fig = plot_ly(showlegend = F, height = 650) %>%
      
      # hvi en vv drempel
      add_polygons(x=c(2025.5,2025.5,2045.5,2045.5),y=c(space,hvi,hvi,space), color=I("grey70"), opacity = 0.3,  name = "<b>Heffingvrij inkomen:</b> aanwas is niet belastbaar en \nkan ook niet verrekend worden met verliezen.", hoverinfo = 'text', hovertemplate = '%{text}') %>% 
      add_polygons(x=c(2025.5,2025.5,2045.5,2045.5),y=c(vv_drempel,-space,-space, vv_drempel), color=I("grey70"), opacity = 0.3,  name = "<b>Verlies onder verliesverrekenings drempel:</b>\nverlies kan niet verrekend worden.", hoverinfo = 'text', hovertemplate = '%{text}') %>% 
      
      # verliesverrekening 
      add_polygons(x=c(vv_cf,vv_cf,input_jaar,input_jaar),y=c(vv_drempel-space,ymin,ymin,vv_drempel-space), color=I("red"), opacity = 0.3, name = "<b>Voorwaartse verliesverrekening:</b>\nverlies mag verrekend worden met grondslag belastingjaar.",  hoverinfo = 'text', hovertemplate = '%{text}') %>%
      add_polygons(x=c(input_jaar, input_jaar, vv_cb, vv_cb), y=c(vv_drempel-space,ymin,ymin,vv_drempel-space), color=I("green"), opacity = 0.3, name = "<b>Achterwaartse verliesverrekening:</b>\n toekomstig verlies mag verrekend worden met grondslag belastingjaar.",  hoverinfo = 'text', hovertemplate = '%{text}') %>%
      
      # huidig jaar
      add_trace(x=input_jaar, y=c(ymin - 10, ymax + 10), opacity = 0.7, color=I("grey20"), mode = 'lines', hovertemplate = '') 
    
    # schijven
    if (schijf_aantal == 1){
      fig = fig %>% add_polygons(x=c(2025.5,2025.5,2045.5,2045.5),y=c(hvi, ymax, ymax, hvi), color=I("grey70"), opacity = 0,  name = s1_text, hoverinfo = 'text', hovertemplate = '%{text}')}
    if (schijf_aantal == 2){
      s1_text = paste0("<b>Schijf 1:</b> ", percentify(t1), " belasting over aanwas tot ", number_to_money(s2), ".")
      s2_text = paste0(s1_text, "\n<b>Schijf 2:</b> ", percentify(t2), " belasting over aanwas boven ", number_to_money(s2), ".")
      fig = fig %>% 
        add_polygons(x=c(2025.5,2025.5,2045.5,2045.5),y=c(hvi, s2-1, s2-1, hvi), color=I("grey70"), opacity = 0,  name = s1_text, hoverinfo = 'text', hovertemplate = '%{text}') %>%
        add_polygons(x=c(2025.5,2025.5,2045.5,2045.5),y=c(s2, ymax, ymax, s2), color=I("grey70"), opacity = 0,  name = s2_text, hoverinfo = 'text', hovertemplate = '%{text}') }
    if (schijf_aantal == 3){
      s1_text = paste0("<b>Schijf 1:</b> ", percentify(t1), " belasting over aanwas tot ", number_to_money(s2), ".")
      s2_text = paste0(s1_text, "\n<b>Schijf 2:</b> ", percentify(t2), " belasting over aanwas tot ", number_to_money(s3), ".")
      s3_text = paste0(s2_text, "\n<b>Schijf 3:</b> ", percentify(t3), " belasting over aanwas boven ", number_to_money(s3), ".")
      
      fig = fig %>% 
        add_polygons(x=c(2025.5,2025.5,2045.5,2045.5),y=c(hvi, s2-1, s2-1, hvi), color=I("grey70"), opacity = 0,  name = s1_text, hoverinfo = 'text', hovertemplate = '%{text}') %>%
        add_polygons(x=c(2025.5,2025.5,2045.5,2045.5),y=c(s2, s3-1, s3-1, s2), color=I("grey70"), opacity = 0,  name = s2_text, hoverinfo = 'text', hovertemplate = '%{text}') %>%
        add_polygons(x=c(2025.5,2025.5,2045.5,2045.5),y=c(s3, ymax, ymax, s3), color=I("grey70"), opacity = 0,  name = s3_text, hoverinfo = 'text', hovertemplate = '%{text}')
    }
    
    fig = fig %>%
      add_trace(x=~jaar, y=~aanwas, opacity = 0.7, color=I("black"), name = "<b>Aanwas</b>", hovertemplate = '%{y}', mode = "lines+markers") %>%
      layout(yaxis = list(showticklabels = T), hovermode = "x unified", showlegend = T)
    
    fig
    
  })
  
  ########## TAB 3 ###########
  
  output$tab_microeffects = renderDataTable({
    
    if (is.null(input$upload_data_variant)){data = variant_data_input()} else {data = upload_data_variant$data}
    if (is.null(input$upload_data)){case_dat = case_data()} else {case_dat = upload_data$data}
    out = list()
    
    for (i in c(1:nrow(data))){
      out[[i]] = data.frame(
        t(select(cbind(N = length(unique(case_dat$omschrijving)), data.frame(calculate_variant_stats_micro(data = case_data(), hvi = data$hvi[i], vv_drempel = data$verlies_drempel[i], 
        cf = data$cf[i], cb = data$cb[i],  s2 = data$schijf_2[i], s3 = data$schijf_3[i], t1 = data$tarief_1[i], t2 = data$tarief_2[i], t3 = data$tarief_3[i]))),
        c("N", "gini_grondslag", "gini_belasting", "overbelasting"))), row.names = c("aantal observaties","grondslag ongelijkheid (0-1)", "belasting ongelijkheid (0-1)", "overbelasting (%)")) %>%
        setNames(c(data$variant[i])) 
    }
    
    out = do.call(cbind, out)
    out
    
  }, server = F, rownames = T, selection = 'none', options = list(dom = 't', scrollX = T))
  
  output$tab_microwinners = renderDataTable({
    
    if (is.null(input$upload_data_variant)){data = variant_data_input()} else {data = upload_data_variant$data}
    out = list()
    
    for (i in c(1:nrow(data))){
      out[[i]] = cbind(variant = data$variant[i], N = 2, data.frame(calculate_variant_stats_macro(hvi = data$hvi[i], vv_drempel = data$verlies_drempel[i], 
                 cf = data$cf[i], cb = data$cb[i],  s2 = data$schijf_2[i], s3 = data$schijf_3[i], t1 = data$tarief_1[i], t2 = data$tarief_2[i], t3 = data$tarief_3[i])))
    }
    
    out = do.call(rbind, out)
    
    out = data.frame(
      winnaar = c(subset(out, gini_grondslag == min(out$gini_grondslag))$variant[1], subset(out, gini_belasting == min(out$gini_belasting))$variant[1], subset(out, overbelasting == min(out$overbelasting))$variant[1]),
      row.names = c("grondslag ongelijkheid (0-1)", "belasting ongelijkheid (0-1)", "overbelasting (%)")
    )
    
    out
    
  }, server = F, rownames = T, selection = 'none', options = list(dom = 't', scrollX = T))
  
  # PLOTS  
  
  output$plot_micro_select_variant_1_choices = renderUI({
    selectInput("plot_micro_select_variant_1", label = NULL, choices = variant_data_input()$variant)
  })
  
  
  output$plot_micro_variant_1 = renderPlotly({
    
    # variant data 
    if (is.null(input$upload_data_variant)){variant_dat = variant_data_input()} else {variant_dat = upload_data_variant$data}
    variant_id = input$plot_micro_select_variant_1
    variant_dat = subset(variant_dat, variant == variant_id)
    
    # case data
    if (is.null(input$upload_data)){case_dat = case_data()} else {case_dat = upload_data$data}
    
    temp = list()
    for (i in c(1:length(unique(case_dat$id)))){
      
      dat = subset(case_dat, id == unique(case_dat$id)[i])
      naam = dat$omschrijving[1]
      aanwas = subset(dat, jaar == 2045)$aanwas
      grondslag = subset(verreken_verlies( data = dat, hvi = variant_dat$hvi, cf = variant_dat$cf, cb = variant_dat$cb, drempel = variant_dat$verlies_drempel), jaar == 2045)$grondslag
      belasting = sum(bepaal_belasting(grondslag, schijf_2 = variant_dat$schijf_2, schijf_3 = variant_dat$schijf_3, tarief_1 = variant_dat$tarief_1, tarief_2 = variant_dat$tarief_2, tarief_3 = variant_dat$tarief_3)$belasting, na.rm = T)
      
      if(aanwas > 0){grondslag_perc = (grondslag / aanwas) *100 ; belasting_perc = (belasting / aanwas) *100
      } else { grondslag_perc = 0 ; belasting_perc = 0 }

      temp[[i]] = data.frame(
        casus = rep(naam, 3), 
        percentage = round(c(100-grondslag_perc, grondslag_perc-belasting_perc, belasting_perc), 1), 
        type = factor(c("aanwas", "grondslag", "belasting"), levels = c("aanwas", "grondslag", "belasting"), ordered = T),
        text = c(paste0(naam, " (aanwas = ", number_to_money(aanwas), ")"), "", "")
      )  
      
    }
    
    temp = do.call(rbind, temp)
    h = 120*length(unique(temp$casus)) 
    
    fig = ggplotly(
      ggplot(data = temp) + 
        geom_col(stat = "identity", aes(x = casus, y = percentage, fill = type), position = "stack", width = 0.7, color = "black", alpha = 0.8) + geom_text(aes(x = casus, y = 50, label = text), nudge_x = 0.5, size = 4) + 
        scale_fill_manual(values = c("grey90", "grey70", "grey30")) +  coord_flip() + theme_void() + theme(legend.position = 'none', panel.grid = element_blank()),
      height = h
    ) %>% 
      layout(
        xaxis = list(title = "", zeroline = FALSE, showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
        yaxis = list(title = "", zeroline = FALSE, showline = FALSE, showticklabels = FALSE, showgrid = FALSE))
    
    fig
    
  })
  
  output$plot_micro_select_variant_2_choices = renderUI({
    selectInput("plot_micro_select_variant_2", label = NULL, choices = variant_data_input()$variant)
  })
  
  output$plot_micro_variant_2 = renderPlotly({
    
    # variant data 
    if (is.null(input$upload_data_variant)){variant_dat = variant_data_input()} else {variant_dat = upload_data_variant$data}
    variant_id = input$plot_micro_select_variant_2
    variant_dat = subset(variant_dat, variant == variant_id)
    
    # case data
    if (is.null(input$upload_data)){case_dat = case_data()} else {case_dat = upload_data$data}
    
    temp = list()
    for (i in c(1:length(unique(case_dat$id)))){
      
      dat = subset(case_dat, id == unique(case_dat$id)[i])
      naam = dat$omschrijving[1]
      aanwas = subset(dat, jaar == 2045)$aanwas
      grondslag = subset(verreken_verlies(data = dat, hvi = variant_dat$hvi, cf = variant_dat$cf, cb = variant_dat$cb, drempel = variant_dat$verlies_drempel), jaar == 2045)$grondslag
      belasting = sum(bepaal_belasting(grondslag, schijf_2 = variant_dat$schijf_2, schijf_3 = variant_dat$schijf_3, tarief_1 = variant_dat$tarief_1, tarief_2 = variant_dat$tarief_2, tarief_3 = variant_dat$tarief_3)$belasting, na.rm = T)
      
      if(aanwas > 0){ grondslag_perc = (grondslag / aanwas) *100 ; belasting_perc = (belasting / aanwas) *100
      } else { grondslag_perc = 0 ; belasting_perc = 0}
      
      temp[[i]] = data.frame(
        casus = rep(naam, 3), 
        percentage = round(c(100-grondslag_perc, grondslag_perc-belasting_perc, belasting_perc), 1), 
        type = factor(c("aanwas", "grondslag", "belasting"), levels = c("aanwas", "grondslag", "belasting"), ordered = T),
        text = c(paste0(naam, " (aanwas = ", number_to_money(aanwas), ")"), "", "")
      )  
      
    }
    
    temp = do.call(rbind, temp)
    h = 120*length(unique(temp$casus)) 
    
    fig = ggplotly(
      ggplot(data = temp) + 
        geom_col(stat = "identity", aes(x = casus, y = percentage, fill = type), position = "stack", width = 0.7, color = "black", alpha = 0.8) + 
        geom_text(aes(x = casus, y = 50, label = text), nudge_x = 0.5, size = 4) +  scale_fill_manual(values = c("grey90", "grey70", "grey30")) +  coord_flip() +  theme_void() + 
        theme(legend.position = 'none', panel.grid = element_blank()
        ),
      height = h
    ) %>% 
      layout(
        xaxis = list(title = "", zeroline = FALSE, showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
        yaxis = list(title = "", zeroline = FALSE, showline = FALSE, showticklabels = FALSE, showgrid = FALSE))
    
    fig
    
    
  })
  
  ################################## 2. MACRO ANALYSES ##################################
  
  ############ 2.1. MACRO ANALYSES - STAP 1: Welke variant wilt u doorrekenen? ##########
  
  ############ SIDEBAR ############ 
  
  # KNOP VARIANT TOEVOEGEN
  observeEvent(input$add_variant_macro, {
    
    # 1. als schijf 2 lager ligt dan hvi 
    if (!is.na(input$schijf_2_macro) & input$schijf_2_macro <= input$hvi_macro){
      warning("Casus niet toegevoegd", "De ondergrens van de tweede schijf kan niet lager zijn dan of gelijk zijn aan het heffing vrij inkomen.")
      
      # 2. als schijf 3 lager ligt dan schijf 2 of hvi  
    } else if (!is.na(input$schijf_3_macro) & input$schijf_3_macro <= input$hvi_macro | !is.na(input$schijf_3_macro) & !is.na(input$schijf_2_macro) & input$schijf_3_macro <= input$schijf_2_macro) {
      warning("Casus niet toegevoegd", "De ondergrens van de derde schijf kan niet lager zijn dan of gelijk zijn aan de ondergrens van schijf 2 of het heffingvrij inkomen.")
      
      # 3. als geen tarief
    } else if (is.na(input$tarief_1_macro)){
      warning("Casus niet toegevoegd", "U heeft geen tarief bepaald.")
      
      # 4. als tarief incompleet
    } else if (!is.na(input$schijf_2_macro) & is.na(input$tarief_2_macro) | !is.na(input$schijf_3_macro) & is.na(input$tarief_3_macro)){
      warning("Casus niet toegevoegd", "U bent één of meerdere tarieven vergeten in te voeren.")
      
      # Als correct ingevuld  
    } else {
      
      if (is.null(input$upload_data_variant_macro)){data = variant_data_input_macro()} 
      if (!is.null(input$upload_data_variant_macro)){data = upload_data_variant_macro$data}
      
      if(isolate(input$naam_variant_macro) %in% data$variant){naam_variant_new = paste0(isolate(input$naam_variant_macro), " ", nrow(data)+1)
      } else {naam_variant_new = isolate(input$naam_variant_macro)}
      
      dat = data.frame(
        variant = naam_variant_new, hvi = isolate(input$hvi_macro), verlies_drempel = isolate(input$verlies_drempel_macro),
        cf = isolate(input$verlies_voor_macro), cb = isolate(input$verlies_achter_macro), schijf_2 = isolate(input$schijf_2_macro),
        schijf_3 = isolate(input$schijf_3_macro), tarief_1 = isolate(input$tarief_1_macro), tarief_2 = isolate(input$tarief_2_macro),
        tarief_3 = isolate(input$tarief_3_macro)) 
      
      # variant toevoegen
      if (is.null(input$upload_data_variant_macro)){variant_data_input_macro() %>%  bind_rows(dat) %>% variant_data_input_macro()
      } else { upload_data_variant_macro$data = upload_data_variant_macro$data %>%  bind_rows(dat)}
      
    } 
    
  }) 
  
  
  ############ TAB 1 ############ 
  
  # KNOP DOWNLOAD TEMPLATE
  output$download_template_variant_macro = downloadHandler(
    filename = function() {paste("sandbox3_template_variant_macro.xlsx", sep="")},
    content = function(file) {
      write_xlsx(empty_dat_variant, file)
    })
  
  # OPLADEN DATA 
  upload_data_variant_macro = reactiveValues(data = NULL)
  
  gen_upload_data_variant_macro = function(){
    
    if (nrow(readxl::read_xlsx(input$upload_data_variant_macro$datapath)) > 0){
      upload_data_variant_macro$data = readxl::read_xlsx(input$upload_data_variant_macro$datapath) %>%
        setNames(colnames_variant_original)
      # generate empty data frame if file does not contain any entries 
    } else {
      upload_data_variant_macro$data = gen_empty_data_variant() 
    }}
  
  observeEvent(input$upload_data_variant_macro, {upload_data_variant_macro$data = gen_upload_data_variant_macro()})
  
  # VOORGEPROGRAMEERDE DATA 
  variant_data_input_macro = reactiveVal(variant_data)
  
  # OUTPUT TABEL
  output$variant_data_macro = renderDataTable({
    if (is.null(input$upload_data_variant_macro)){data = variant_data_input_macro()} else {data = upload_data_variant_macro$data}
    data %>% setNames(colnames_variant_print)
  }, server = F, rownames = F, selection = 'single', options = list(paging =T, pageLength = 16, scrollX = T))
  
  # KNOP RESET DATA
  observeEvent(input$reset_data_variant_macro, {
    
    if (is.null(input$upload_data_variant_macro)){variant_data_input_macro() %>% filter(row_number() %in% -1) %>% variant_data_input_macro()
    } else {upload_data_variant$data = gen_empty_data_variant()}
    
  })
  
  # KNOP VERWIJDER VARIANT 
  observeEvent(input$delete_variant_macro, {
    if (is.null(input$upload_data_variant_macro)){  
      remove = variant_data_input_macro()[input$variant_data_macro_rows_selected, "variant"]
      variant_data_input_macro() %>%  subset(., variant != remove) %>% variant_data_input_macro()
    } else { 
      upload_data_variant_macro$data = upload_data_variant_macro$data[-input$variant_data_macro_rows_selected,]
    }
  })
  
  
  # KNOP DOWNLOAD 
  selected_data_variant_macro = function(input = NULL){
    if (is.null(input)){data = variant_data_input_macro()} else {data = upload_data_variant_macro$data}
    return(data)
  }
  
  output$download_variants_macro = downloadHandler(
    filename = function() {paste("sandbox3_variants_macro.xlsx", sep="")},
    content = function(file) {write_xlsx(selected_data_variant_macro(input$upload_data_variant_macro), file)}
  )
  
  ############ TAB 2 ############ 
  
  # DATA VARIANT NAMES
  output$micro_1_select_variant_macro = renderUI({
    if (is.null(input$upload_data_variant_macro)){data = variant_data_input_macro()} else {data = upload_data_variant_macro$data}
    selectInput("micro_1_select_variant_selection_macro", label = NULL, choices = data$variant, width = '100%')
  })
  
  # TEKST VARIANT 
  output$variant_tekst_macro = renderText({
    
    if (is.null(input$upload_data_variant_macro)){dat_variant = variant_data_input_macro()} else {dat_variant = upload_data_variant_macro$data}
    
    dat_variant = subset(dat_variant, variant == input$micro_1_select_variant_selection_macro)
    
    if (nrow(dat_variant) > 0){
      
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
      s2 = dat_variant$schijf_2; s3 = dat_variant$schijf_3
      t1 = dat_variant$tarief_1; t2 = dat_variant$tarief_2; t3 = dat_variant$tarief_3
      schijf_aantal = 1; if (!is.na(t3) & !is.na(s3)){schijf_aantal = 3};  if (is.na(t3) & is.na(s3) & !is.na(t2) & !is.na(s2)){schijf_aantal = 2} 
      
      # text 
      text = paste0("Variant <i>", naam_variant, "</i> kent een heffingvrij inkomen <i>", number_to_money(hvi), "</i>. 
                    Iedereen die een inkomen uit vermogen heeft onder deze grens, betaalt geen belasting in box 3. 
                    De hoogte van het heffingvrij inkomen bepaalt eveneens het aantal burgers dat een beroep kan doen op verliesverrekening.
                    Variant voorziet een verliesverrekeningsdrempel van <i>", number_to_money(vv_drempel), "</i>; <i>",  vv_cf, " jaar</i> voorwaartse
                    verliesverrekening en <i>", vv_cb, " jaar</i> achterwaartse verliesverrekening. Iedere burger met 
                    (1) een belastbaar inkomen uit vermogen in belastingjaar ", jaar_nu, ", d.w.z. een inkomen
                    boven het heffingvrij inkomen en (2) onverrekende verliezen uit de jaren <i>", jaar_nu - vv_cf, " tot ", jaar_nu + vv_cb, "</i> 
                    kan deze in mindering brengen bij de grondslag van het belastingjaar. ")
      
      if (schijf_aantal == 1){text = paste0(text, "Variant kent een vlaktaks. Alle belastingplichtigen betalen <i>", percentify(t1), "</i> belasting over de grondslag van het belastingjaar.")}
      if (schijf_aantal == 2){text = paste0(text, "Variant kent een progressief tarief met twee schijven. Belastingplichtigen betalen <i>", percentify(t1), "</i> belasting over de grondslag tot <i>", number_to_money(s2), 
                                            "</i> en <i>", percentify(t2), "</i> over de rest."  )}
      if (schijf_aantal == 3){text = paste0(text, "Variant kent een progressief tarief met drie schijven.Belastingplichtigen betalen <i>", percentify(t1), "</i> belasting over de grondslag tot <i>", number_to_money(s2), 
                                            "</i>; <i>", percentify(t2), "</i> tot <i>", number_to_money(s3), "</i> en <i>", percentify(t3), "</i> over de rest." )}
      
      # toelichting grafiek
      text = paste0(text, "<br><br><i>De grafiek verschaft een visualisatie van de door u gespecificeerde variant 
                     voor een voorbeeld belastingplichtige. Het grijze gebied boven de nul is het heffingvrij inkomen en onder de 
                     nul niet verrekenbare verliezen (daar deze onder de verliesverrekeningsdrempel zitten). Het groene gebied toont 
                     de aanwas die in aanmerking komt voor achterwaartse verliesverrekening en het rode gebied de aanwas die in aanmerking
                     komt voor voorwaartse verliesverrekening. Beweeg met uw muis over de jaren om te inspecteren of de aanwas van dat
                     jaar onder het heffingvrij inkomen, belastbaar inkomen, onverrekenbaar verlies, of verrekenbaar verlies valt. U kunt het 
                     belastingjaar veranderen door de slider te verschuiven. </i> <br><br>")
      
    } else {
      text = "Er is geen data beschikbaar. Voeg data toe met behulp van de tool. <br><br><br>"
    }
    
    text
    
  })
  
  # PLOT VARIANT
  output$plot_variant_macro = renderPlotly({
    
    
    input_jaar = as.numeric(input$plot_variant_jaar_macro)
    
    if (is.null(input$upload_data_variant_macro)){dat_variant = variant_data_input_macro()} else {dat_variant = upload_data_variant_macro$data}
    dat_variant = subset(dat_variant, variant == input$micro_1_select_variant_selection_macro)
    
    if (nrow(dat_variant) > 0){
      
      # PARAMETERS
      naam_variant = input$micro_1_select_variant_selection_macro
      hvi = dat_variant$hvi
      vv_drempel = -dat_variant$verlies_drempel
      vv_cf = input_jaar - dat_variant$cf - 0.5; if (vv_cf < 2026){vv_cf = 2026 - 0.5}
      vv_cb = input_jaar + dat_variant$cb + 0.5; if (vv_cb > 2045){vv_cb = 2045 + 0.5}
      s2 = dat_variant$schijf_2; s3 = dat_variant$schijf_3; 
      t1 = dat_variant$tarief_1; t2 = dat_variant$tarief_2; t3 = dat_variant$tarief_3
      
      schijf_aantal = 1; if (!is.na(t3) & !is.na(s3)){schijf_aantal = 3};  if (is.na(t3) & is.na(s3) & !is.na(t2) & !is.na(s2)){schijf_aantal = 2}  
      
      # HIER AANPASSEN!!! randomly generate case knop
      aanwas = c(0.5*hvi, hvi + 0.5*hvi, 2*hvi, 3*hvi, 4*hvi, 3*hvi, 2*hvi, 2*vv_drempel, 0.7*vv_drempel, 4*hvi, 3*vv_drempel, 
                 1.5*vv_drempel, 0.5*vv_drempel, 1.3*hvi, 2*hvi, 0.2*hvi, 0.5*hvi, 0.5*vv_drempel, 1.1*vv_drempel, 0.2*vv_drempel)
      jaar = c(2026:(2025+length(aanwas)))
      
      ymax = 1.75*max(hvi, vv_drempel, aanwas)
      ymin = -ymax
      space = 0.2*hvi
      
      s1_text = paste0("<b>Schijf 1:</b> ", percentify(t1), " belasting over aanwas.")
      
      # PLOT
      fig = plot_ly(showlegend = F, height = 500) %>%
        
        # hvi en vv drempel
        add_polygons(x=c(2025.5,2025.5,2045.5,2045.5),y=c(space,hvi,hvi,space), color=I("grey70"), opacity = 0.3,  name = "<b>Heffingvrij inkomen:</b> aanwas is niet belastbaar en \nkan ook niet verrekend worden met verliezen.", hoverinfo = 'text', hovertemplate = '%{text}') %>% 
        add_polygons(x=c(2025.5,2025.5,2045.5,2045.5),y=c(vv_drempel,-space,-space, vv_drempel), color=I("grey70"), opacity = 0.3,  name = "<b>Verlies onder verliesverrekenings drempel:</b>\nverlies kan niet verrekend worden.", hoverinfo = 'text', hovertemplate = '%{text}') %>% 
        
        # verliesverrekening 
        add_polygons(x=c(vv_cf,vv_cf,input_jaar,input_jaar),y=c(vv_drempel-space,ymin,ymin,vv_drempel-space), color=I("red"), opacity = 0.3, name = "<b>Voorwaartse verliesverrekening:</b>\nverlies mag verrekend worden met grondslag belastingjaar.",  hoverinfo = 'text', hovertemplate = '%{text}') %>%
        add_polygons(x=c(input_jaar, input_jaar, vv_cb, vv_cb), y=c(vv_drempel-space,ymin,ymin,vv_drempel-space), color=I("green"), opacity = 0.3, name = "<b>Achterwaartse verliesverrekening:</b>\n toekomstig verlies mag verrekend worden met grondslag belastingjaar.",  hoverinfo = 'text', hovertemplate = '%{text}') %>%
        
        # huidig jaar
        add_trace(x=input_jaar, y=c(ymin - 10, ymax + 10), opacity = 0.7, color=I("grey20"), mode = 'lines', hovertemplate = '') 
      
      # schijven
      if (schijf_aantal == 1){
        fig = fig %>% add_polygons(x=c(2025.5,2025.5,2045.5,2045.5),y=c(hvi, ymax, ymax, hvi), color=I("grey70"), opacity = 0,  name = s1_text, hoverinfo = 'text', hovertemplate = '%{text}')}
      if (schijf_aantal == 2){
        s1_text = paste0("<b>Schijf 1:</b> ", percentify(t1), " belasting over aanwas tot ", number_to_money(s2), ".")
        s2_text = paste0(s1_text, "\n<b>Schijf 2:</b> ", percentify(t2), " belasting over aanwas boven ", number_to_money(s2), ".")
        fig = fig %>% 
          add_polygons(x=c(2025.5,2025.5,2045.5,2045.5),y=c(hvi, s2-1, s2-1, hvi), color=I("grey70"), opacity = 0,  name = s1_text, hoverinfo = 'text', hovertemplate = '%{text}') %>%
          add_polygons(x=c(2025.5,2025.5,2045.5,2045.5),y=c(s2, ymax, ymax, s2), color=I("grey70"), opacity = 0,  name = s2_text, hoverinfo = 'text', hovertemplate = '%{text}') }
      if (schijf_aantal == 3){
        s1_text = paste0("<b>Schijf 1:</b> ", percentify(t1), " belasting over aanwas tot ", number_to_money(s2), ".")
        s2_text = paste0(s1_text, "\n<b>Schijf 2:</b> ", percentify(t2), " belasting over aanwas tot ", number_to_money(s3), ".")
        s3_text = paste0(s2_text, "\n<b>Schijf 3:</b> ", percentify(t3), " belasting over aanwas boven ", number_to_money(s3), ".")
        
        fig = fig %>% 
          add_polygons(x=c(2025.5,2025.5,2045.5,2045.5),y=c(hvi, s2-1, s2-1, hvi), color=I("grey70"), opacity = 0,  name = s1_text, hoverinfo = 'text', hovertemplate = '%{text}') %>%
          add_polygons(x=c(2025.5,2025.5,2045.5,2045.5),y=c(s2, s3-1, s3-1, s2), color=I("grey70"), opacity = 0,  name = s2_text, hoverinfo = 'text', hovertemplate = '%{text}') %>%
          add_polygons(x=c(2025.5,2025.5,2045.5,2045.5),y=c(s3, ymax, ymax, s3), color=I("grey70"), opacity = 0,  name = s3_text, hoverinfo = 'text', hovertemplate = '%{text}')
      }
      
      fig = fig %>%
        add_trace(x=~jaar, y=~aanwas, opacity = 0.7, color=I("black"), line = list(dash = 'dash'), name = "<b>Aanwas</b>", hovertemplate = '%{y}') %>%
        layout(yaxis = list(showticklabels = F), hovermode = "x unified", showlegend = T)
      
      fig
    } else {}
    
  })
  
  ############ 2.2. MACRO ANALYSES - STAP 2: Bekijk resultaten ##########
  
  # NAAR BOVEN!
  colnames_variant_population_original = c("variant", "budget", "gini_budget", "gini_grondslag", "gini_belasting", "overbelasting", "hvi", "verlies_drempel", "cf", "cb", "schijf_2", "schijf_3", "tarief_1", "tarief_2", "tarief_3")
  colnames_variant_population_print = c("variant", "budgettaire opbrengst", "budget stabiliteit", "grondslag ongelijkheid", "belasting ongelijkheid", "overbelasting", "hvi", "verlies drempel", "CF", "CB", "S2 €", "S3 €", "T1 %", "T2 %", "T3 %")
  
  gen_population_effects = function(){
    
    if (is.null(input$upload_data_variant_macro)){variant_data = variant_data_input_macro()} else {variant_data = upload_data_variant_macro$data}
    #case_data = 10
    
    #if (nrow(variant_data) > 0 & nrow(case_data) > 0){
      
    #  temp = list()
      # elke case
    #  for (i in c(1:length(unique(case_data$omschrijving)))){
        # elke variant
    #    for (j in c(1:nrow(variant_data))){
    #      temp[[length(temp) + 1]] = gen_combi(dat_variant = variant_data[j,], dat_case = subset(case_data, omschrijving == unique(case_data$omschrijving)[i]))
    #      
    #    }}
      
    #  temp = do.call(rbind, temp) %>% setNames(varnames)
    #} else {
    #  temp = variant_case_effects %>% setNames(varnames) %>% filter(., row_number() %in% -1)
    #}
    
    #return(temp)
    
  }
  
  
  output$variant_population_effects = renderDataTable({})
  
  
  
  #column(2, actionButton(inputId = "reset_variant_population_effects", label = "reset dataset", width = '100%'), h4(),
  #actionButton(inputId = "delete_variant_population_effects", label = "verwijder variant", width = '100%'), h4(),
  #actionButton(inputId = "delete__population_effects", label = "verwijder casus", width = '100%'), h4(),
  #downloadButton("download_variants_population_effects", label = "opslaan", style = "width:100%;"))),
  
  ################ NIEUW VOOR MACRO ##################
  #text = "De door u geselecteerde variant wordt steeds vergeleken met de 'standaard variant'. 
  #Onder de standaard variant verstaan wij een variant met (1) hvi van €1000, (2) verlies verrekeningsdrempel van €1000,
  #(3) 9 jaar voorwaartse verliesverrekening, (4) één jaar achterwaartse verliesverrekening en (5) een uniform tarief van 34%. <br><br>"
  
  # HEFFING VRIJ INKOMEN
  #text = paste0(text, "Variant <i>", naam_variant, "</i> kent een heffinvrij inkomen van <i>", number_to_money(hvi), "</i>. ")
  #if (hvi_verschil > 0){text = paste0(text, "Dat is <i>", number_to_money(hvi_verschil), "</i> meer dan onder de standaard variant. ")
  #} else if (hvi_verschil < 0){text = paste0(text, "Dat is <i>", number_to_money(abs(hvi_verschil)), "</i> minder dan onder de standaard variant. ")
  #} else {text = paste0(text, "Dat is evenveel als het hvi onder de standaard variant. ")}
  
  # VERLIESVERREKENING 
  #text = paste0(text, "Variant voorziet een drempel van <i>", number_to_money(vv_drempel), "</i>. ")
  #if (vv_drempel_verschil > 0){text = paste0(text, "Dat is <i>", number_to_money(vv_drempel_verschil), "</i> meer dan onder de standaard variant. ")
  #} else if (vv_drempel_verschil < 0){text = paste0(text, "Dat is <i>", number_to_money(abs(vv_drempel_verschil)), "</i> minder dan onder de standaard variant. ")
  #} else {text = paste0(text, "Dat is evenveel als de drempel onder de standaard variant. ")}
  
  #text = paste0(text, "Variant faciliteert <i>", vv_cf, "</i> jaar voorwaartse verliesverrekening en <i>", vv_cb, "</i> achterwaartse verliesverrekening. Dat is respectievelijk ")
  #if (vv_cf_verschil > 0){
  #  text = paste0(text, "<i> ", vv_cf_verschil, " jaar</i> meer en ")
  #} else if (vv_cf_verschil < 0){
  #  text = paste0(text, "<i> ", abs(vv_cf_verschil), " jaar</i> minder en ")
  #} else {
  #  text = paste0(text, "<i> evenveel jaar</i> en ")
  #}
  
  #if (vv_cb_verschil > 0){
  #  text = paste0(text, "<i> ", vv_cb_verschil, " jaar</i> meer dan onder de standaard variant. ")
  #} else if (vv_cb_verschil < 0){
  #  text = paste0(text, "<i> ", abs(vv_cb_verschil), " jaar</i> minder dan onder de standaard variant. ")
  #} else {
  #  text = paste0(text, "<i> evenveel jaar</i> als onder de standaard variant. ")
  #}
  
  # TARIEF EN SCHIJFGRENZEN
  
  
  
  
  # PROGRESSIVITEIT VAN TARIEF
  #text = paste0(text, "<b>Progressiviteit tarief.</b> ")
  #if (schijf_aantal == 1){text = paste0(text, "Variant kent een vlaktaks. Belastingplichtigen betalen <i>", percentify(t1), "</i> belasting over de grondslag. ")}
  #if (schijf_aantal == 2){text = paste0(text, "Variant kent een progressief tarief met twee schijven. Belastingplichtigen betalen <i>", percentify(t1), "</i> belasting over de grondslag tot <i>", number_to_money(s2), 
  #                                      "</i> en <i>", percentify(t2), "</i> over de rest."  )}
  #if (schijf_aantal == 3){text = paste0(text, "Variant kent een progressief tarief met drie schijven.Belastingplichtigen betalen <i>", percentify(t1), "</i> belasting over de grondslag tot <i>", number_to_money(s2), 
  #                                      "</i>; <i>", percentify(t2), "</i> tot <i>", number_to_money(s3), "</i> en <i>", percentify(t3), "</i> over de rest." )}
  
  #text = paste0(text, "Het versterken van de progressiviteit heeft volgende gevolgen: <br><br>
  
  #                  <ul>
  #                  <li>burgers met middelgrote rendementen betalen minder en burgers met grote rendementen meer; </li>
  #                  <li>de staat ondervindt in beginsel een opbrengst; </li>
  #                  <li>in combinatie met verliesverrekening, is deze opbrengst instabieler. </li>
  #                  </ul>")
  #} else {
  #  text = "WAARSCHUWING: Er is geen data beschikbaar. Voeg data toe met behulp van de tool."
  
  
  
  
  # EIND SERVER     
}

# Run the application 
shinyApp(ui = ui, server = server)
