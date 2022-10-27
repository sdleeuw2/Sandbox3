library(shiny)
library(ggvis)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(DT)
library(plotly)
library(writexl)
library(shinydashboard)
library(ggforce)
library(data.table)
library(plyr)
library(ids)


# SET BASE MULTIPLIERS 
  
  risico = 2
  sd = 0.2
  sd_rend = 0.5*risico*sd

# FUNCTIONS
  
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
    prp = hvv / vermogen
    if (prp > 1){prp = 1}
    vermogen_hvv = (spaargeld - (prp*spaargeld)) + (finproduct - (prp*finproduct)) + (restbezit - (prp*restbezit)) - (schuld - (prp*schuld))
    aanwas_hvv = 0.0036*(spaargeld - (prp*spaargeld)) + 0.0617*(finproduct - (prp*finproduct)) + 0.0617*(restbezit - (prp*restbezit)) - 0.0257*(schuld - (prp*schuld))
    
    if (type == "aanwas na hvv"){return(aanwas_hvv)}
    if (type == "vermogen na hvv"){return(vermogen_hvv)}
    
  }

  # function to draw value from normal distribution
  gen_value = function(mean, sd, n = 1){x = rnorm(n = n, mean = mean, sd = mean*sd); return(x)}
  
  # update value last year 
  update_value = function(current_value, perc_change){new_value = current_value*(1+(perc_change/100))}
  
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
      spaargeld_forfait = 0.0036*row$spaargeld,
      finproduct_forfait = 0.0617*row$finproduct,
      restbezit_forfait = 0.0617*row$restbezit,
      schuld_forfait = 0.0257*row$schuld)) %>%
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
        spaargeld_forfait = (0.0036*dat1$spaargeld),
        finproduct_forfait = (0.0617*dat1$finproduct),
        restbezit_forfait = (0.0617*dat1$restbezit),
        schuld_forfait = (0.0257*dat1$schuld)) %>%
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
    
    return(out)
  }
  
  # functie om getal om te zetten in bedrag in €
  number_to_money = function(number){
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
  
  # functie om selectie rijen twee datasets om te zetten in een databestand
  process_selection = function(variant_selection, case_selection, variant_data, case_data, yvar, time){
    
    # VOEG TOE VERSCHIL T.O.V. OVERBRUGGINGSWETGEVING
    
    if (length(variant_selection) > 0 & length(case_selection) > 0){
      
      variant_data = variant_data[variant_selection,]
      case_ids = unique(subset(case_data, jaar == 2036)[case_selection,"id"])
      newdata = list()
      
      for (i in c(1:nrow(variant_data))){
        
        variant = variant_data[i,]
        
        for (j in c(1:length(case_ids))){  
          
          case = case_data[case_data$id == case_ids[j],]
          
          temp = data.frame(
            
            id_indiv = case_ids[j],
            # variant details 
            variant = variant$variant, hvi = variant$hvi, verlies_drempel = variant$verlies_drempel, cf = variant$cf, cb = variant$cb, 
            schijf_2 = variant$schijf_2, schijf_3 = variant$schijf_3, tarief_1 = variant$tarief_1, tarief_2 = variant$tarief_2, tarief_3 = variant$tarief_3,
            # grondslag berekening
            verreken_verlies(data = case, hvi = variant$hvi, cf = variant$cf, cb = variant$cb, drempel = variant$verlies_drempel)) %>%
            mutate(grondslag_perc = case_when(aanwas > 0 ~ ((grondslag / aanwas)*100), aanwas < 0 ~ 0)) %>%
            mutate(vv = cb + cf) 
          
          verlies = abs(sum(subset(case, aanwas < 0 & jaar < 2036)$aanwas))
          verlies_verrekend = (sum(temp$cb) + sum(temp$cf))
          if (verlies > 0){
            temp$vv_perc = (verlies_verrekend / verlies)*100; if (verlies_verrekend == 0){temp$vv_perc = 0}
          } else {
            temp$vv_perc = 0
          }
          
          # bepaal belasting
          for (i in c(1:nrow(temp))){
            belasting = bepaal_belasting(temp$grondslag[i], schijf_2 = variant$schijf_2, schijf_3 = variant$schijf_3, tarief_1 = variant$tarief_1, tarief_2 = variant$tarief_2, tarief_3 = variant$tarief_3)
            temp[i, "belasting"] = sum(belasting$belasting, na.rm = T)
            if (sum(belasting$aaanwas, na.rm = T) > 0){
              temp[i, "belasting_perc"] = (sum(belasting$belasting, na.rm = T) / sum(belasting$aanwas))*100
            } else {temp[i, "belasting_perc"] = 0}
          }
          
          newdata[[length(newdata) + 1]] = temp
          
        }}
      
      newdata = do.call(rbind, newdata)  
      newdata = data.frame(x = newdata$variant, y = newdata[, yvar], group = factor(newdata$id_indiv), jaar = newdata$jaar, 
                           hvi = newdata$hvi, verlies_drempel = newdata$verlies_drempel, cf = newdata$cf, cb = newdata$cb, 
                           s2 = newdata$schijf_2, s3 = newdata$schijf_3, t1 = newdata$tarief_1, t2 = newdata$tarief_2, 
                           t3 = newdata$tarief_3, person_id = newdata$id_indiv, vermogen = newdata$vermogen, aanwas = newdata$aanwas, 
                           vv = newdata$vv, vv_perc = newdata$vv_perc, grondslag = newdata$grondslag, 
                           grondslag_perc = newdata$grondslag_perc, belasting = newdata$belasting, belasting_perc = newdata$belasting_perc) %>%
                 mutate_at(vars(-x, -group, -person_id), funs(round(., 1)))
      
      
      if (time == "één jaar"){newdata = subset(newdata, jaar == variant$jaar)
      } else if (time == "alle jaren"){
        newdata = 
          merge(select(subset(newdata, jaar == variant$jaar), -c("y")),
                aggregate(y ~ x + group, data = newdata, FUN = mean), by = c("x", "group"))}
      
      
      return(newdata)
      
    }}

# DATA 

  # base values 
  id = 1; jaar = 2026; omschrijving = "Jan Modaal"; risico = 2; 
  spaargeld = gen_value(42300, sd); finproduct = gen_value(7000, sd);
  restbezit = 0; schuld = gen_value(12800, sd); 
  spaargeld_rendperc = gen_value(0.36, sd); 
  finproduct_rendperc = gen_value(6.17, sd_rend); 
  restbezit_rendperc = gen_value(6.17, sd); 
  schuld_rendperc = gen_value(2.57, sd)
  
  # case data 
  case_data = gen_history(
    data.frame(id = id, jaar = jaar, omschrijving = omschrijving, risico = risico, 
               spaargeld = spaargeld, finproduct = finproduct, restbezit = restbezit, 
               schuld = schuld, spaargeld_rendperc = spaargeld_rendperc,
               finproduct_rendperc = finproduct_rendperc, restbezit_rendperc = restbezit_rendperc, 
               schuld_rendperc = schuld_rendperc), sd_rend = sd_rend)
  
  # varianten data
  variant_data = data.frame(
    variant = "Voorbeeld", hvi = 1000, verlies_drempel = 1000,
    cf = 9, cb = 1, schijf_2 = as.numeric(NA), 
    schijf_3 = as.numeric(NA), tarief_1 = 34, tarief_2 = as.numeric(NA), tarief_3 = as.numeric(NA))
  
  # vergelijking data
  comparison_data = data.frame(naam_vergelijking = "Vergelijking", variant = "Voorbeeld", budget_raming = "t.b.a. in future!!!", gini_grondslag = "t.b.a. next update", 
                               gini_belasting = "t.b.a. update", opbrengst_stabiliteit = "t.b.a. next update", prox_winstbelasting = "t.b.a. next update",
                               hvi = 1000, verlies_drempel = 1000, cf = 9, cb = 1, schijf_2 = NA, schijf_3 = NA, tarief_1 = 34, tarief_2 = NA, tarief_3 = NA,
                               omschrijving = "Jan Modaal 1", risico = 2, belasting = 170, belasting_perc = 10.45, verlies = 1000, verrekend_verlies = 100, verrekend_verlies_perc = 10, 
                               vermogen = 36500, aanwas = 1625.74, grondslag = 500, grondslag_perc = 30.76, spaargeld = 42300, finproduct = 7000, restbezit = 0, schuld = 12800, 
                               spaargeld_rendperc = 0.36, finproduct_rendperc = 6.17, restbezit_rendperc = 6.17, schuld_rendperc = 2.57)
  

# USER INTERFACE

ui = fluidPage(
    
    tabsetPanel(
        
      type = "tabs",
        
      # TAB 1. HOE WORDT DE GRONDSLAG BEPAALD?
      tabPanel(
          
        div(style = "font-size: 14px","Dataset belastingplichtigen"),
        div(style = "font-size: 10px; padding: 0px 0px; margin-bottom:-20em", 
        HTML("<br>"),
            
          # INPUT 
          sidebarPanel(
            
            # Omschrijving
            h5("Omschrijving"), helpText("Wie is de belastingplichtige? Tip: kies een omschrijving die het makkelijk maakt de casus later terug te vinden."), fluidRow(column(12, textInput(inputId = "omschrijving", label = "", value = omschrijving))),
              
            # Spaargeld
            h5("Spaargeld (+)"), helpText("Hoeveel spaargeld heeft hij en wat is de rente op spaargeld?"),
              fluidRow(column(6, numericInput(inputId = "spaargeld", label = "Vermogen (€)", value = round(spaargeld), min = 0, max = Inf)), column(6, numericInput(inputId = "spaargeld_rendperc", label = "Aanwas (%)", value = round(spaargeld_rendperc), min = 0, max = Inf))),
              
            # Financiele producten 
            h5("Financiële producten (+)"), helpText("Wat is de waarde van zijn financiële producten, zoals verhandelbare aandelen, obligaties, of cryptovaluta en wat is het rendement op deze producten?"),
            fluidRow(column(6, numericInput(inputId = "finproduct", label = "Vermogen (€)", value = round(finproduct), min = 0, max = Inf)), column(6, numericInput(inputId = "finproduct_rendperc", label = "Aanwas (%)", value = round(finproduct_rendperc), min = 0, max = Inf))),
              
            # Onroerend goed
            h5("Onroerend goed (+)"), helpText("Wat is de waarde van zijn onroerende goederen, zoals een tweede huis en wat is het rendement op dit bezit?"),
            fluidRow(column(6, numericInput(inputId = "restbezit", label = "Vermogen (€)", value = round(restbezit), min = 0, max = Inf)), column(6, numericInput(inputId = "restbezit_rendperc", label = "Aanwas (%)", value = round(restbezit_rendperc), min = 0, max = Inf))),
              
            # Schulden
            h5("Schulden (-)"), helpText("Hoeveel schuld heeft belastingplichtige en wat is de rente op deze schuld?"),
            fluidRow(column(6, numericInput(inputId = "schuld", label = "Vermogen (€)", value = round(schuld), min = 0, max = Inf)), column(6, numericInput(inputId = "schuld_rendperc", label = "Aanwas (%)", value = round(schuld_rendperc), min = 0, max = Inf))),
              
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
              
            width = 3
              
            ) # eind sidebarPanel()
            ), # eind div()
            
          # OUTPUT 
          div(style = "font-size: 14px; margin-bottom:-20em; line-height: 25px", 
            
              # Tabel  
              mainPanel(
              tabsetPanel(
                
                tabPanel(title = "Bewerk dataset", 
                  fluidPage(
                    HTML("<br>"),
                    column(10, 
                      HTML("<b>!!! Instructie !!!</b> Schets in de linker (grijze) kolom de situatie van de belastingplichtige voor het 
                      jaar 2026. Druk vervolgens op de knop <em>toevoegen</em> om de casus toe te voegen. Eventueel kunt u ook een door de 
                      computer gegenereerde casus toevoegen door op het knopje <em>random casus</em> te drukken. De vermogensaanwas ontwikkeling 
                      wordt nadien op random wijze geexprapoleerd naar de jaren 2027 tot en met 2045. Bent u niet tevreden met de dataset? 
                      In het onderstaande luik kunt u alle data te verwijderen middels de <em>reset</em> knop of een enkele <em>casus verwijderen</em>.
                      Wil u liever uw data bewerken in excel? Dat kan. Download het template via de <em>download template</em> knop en laadt
                      deze vervolgens op via de <em>data opladen</em> optie. Wil u de ingegeven data opslaan, druk dan op de <em>download</em> knop rechtsboven.
                      U kunt vervolgens de grondslag berekening van een enkele belastingplichtige bekijken onder de tab <em>inspecteer casus</em>."),
                      HTML("<br>"), 
                    fluidRow(
                      column(3, h5("download template"), downloadButton("download_template", label = "download template")),
                      column(9, h5("data opladen (.xlsx)"), fileInput("upload_data", label = NULL, multiple = F, accept = ".xlsx", width = '100%', placeholder = NA))),
                      dataTableOutput('grondslag_data')), 
                    column(2, 
                      actionButton(inputId = "reset_data", label = "reset dataset", width = '100%'), h4(),
                      actionButton(inputId = "delete_case", label = "verwijder casus", width = '100%'), h4(),
                      downloadButton("download_cases", label = "download", style = "width:100%;")))), 
                
                tabPanel(title = "Inspecteer casus",
                  fluidPage(HTML("<br>"), 
                            fluidRow(
                              column(3, h5("Welke casus wilt u bekijken?"), dataTableOutput('case_names')),
                              column(9, h5("Grondslag berekening 2026"), 
                                     htmlOutput("grondslag_tekst", align = "justify"), 
                                     
                                     HTML("<br>"),
                                     h5("Grondslag 2026-2045"),
                                     htmlOutput("grondslag_grafieken", align = "justify"),
                                     plotlyOutput("plot_grondslag"))
                              
                            )
                   )))))), # !!!! eind tabPanel() belastingplichtige
        
        
        
        
        
        # TAB 2: HOE WORDT DE BELASTING BEPAALD?
        
        tabPanel(div(style = "font-size: 14px", "Dataset varianten"),
                 
            div(style = "font-size: 10px; padding: 0px 0px; margin-bottom:-20em", 
                
                HTML("<br>"),
                
                sidebarPanel(
                  
                  # DETAILS
                  h5("Variant"),
                  helpText("Wat is de naam van de variant?"),
                  textInput(inputId = "naam_variant", label = "Naam Variant", value = "Voorbeeld"),
                  
                  # HEFFING VRIJ INKOMEN
                  h5("Heffingvrij Inkomen (€)"),
                  helpText("Welk bedrag van de aanwas dient vrijgesteld te worden van belasting?"),
                  sliderInput(inputId = "hvi", label = "", value = 1000, min = 0, max = 5000, step = 50),
                  
                  # VERLIES VERREKENING
                  h5("Verliesverrekening (Jaar)"),
                  helpText("Wat is de drempel voor verliesverrekening? Met hoeveel jaren mag belastingplichtige verlies verrekenen met winst in het huidig jaar (voorwaarts / carry forward)? Met hoeveel jaren mag de belastingplichtige verlies in het huidig jaar verrekenen met winst in voorgaande jaren (achterwaarts / carry backward)?"),
                  sliderInput(inputId = "verlies_drempel",  "Drempel", min = 0, max = 5000, value = 1000, step = 50),
                  sliderInput(inputId = "verlies_voor",  "Voorwaarts (CF)", min = 0, max = 20, value = 9),
                  sliderInput(inputId = "verlies_achter",  "Achterwaarts (CB)", min = 0, max = 10, value = 1),
                  
                  # SCHIJVEN
                  h5("Schijven"),
                  helpText("Hoeveel schijven (max. 3) heeft de variant? Wat zijn de schijfgrenzen en tarieven? 
                           N.B. Laat u de velden leeg, dan wordt er automatisch verondersteld dat er slechts een schijf is (met een ongelimiteerde schijfgrens) en een tarief."),
                  
                  column(4, HTML("<b>S1: Ondergrens (€)</b>")),
                  column(4, numericInput(inputId = "schijf_2", label = "S2: Ondergrens (€)", value = NA, min = 0, max = Inf)),
                  column(4, numericInput(inputId = "schijf_3", label = "S3: Ondergrens (€)", value = NA, min = 0, max = Inf)),
                  
                  column(4, numericInput(inputId = "tarief_1", label = "S1: Tarief (%)", value = 34, min = 0, max = Inf)),
                  column(4, numericInput(inputId = "tarief_2", label = "S2: Tarief (%)", value = NA, min = 0, max = Inf)),
                  column(4, numericInput(inputId = "tarief_3", label = "S3: Tarief (%)", value = NA, min = 0, max = Inf)),
                  
                  # VOEG TOE
                  actionButton(inputId = "add_variant", label = "variant toevoegen", width = '100%'),
                  
                  width = 3
                  
                ) # eind sidebarPanel()
                ), # eind div()     
            
                mainPanel(tabsetPanel(
                    
                    tabPanel(title = "Bewerk dataset",
                      fluidPage(HTML("<br>"),
                        column(10, HTML("<b>!!! Instructie !!!</b> Specificeer in de linker (grijze) kolom de variant die u wil doorrekenen. 
                        Druk vervolgens op de knop <em>variant toevoegen</em> om de variant toe te voegen. Bent u niet tevreden met de dataset? 
                        In het onderstaande luik kunt u alle data te verwijderen middels de <em>reset</em> knop of een enkele <em>variant verwijderen</em>.
                        Wil u liever uw data bewerken in excel? Dat kan. Download het template via de <em>download template</em> knop en laadt
                        deze vervolgens op via de <em>data opladen</em> optie. Wil u de ingegeven data opslaan, druk dan op de <em>download</em> knop rechtsboven.
                        U kunt vervolgens bekijken welke effecten uw variant heeft op een enkele belastingplichtige onder de tab <em>inspecteer variant</em>. "),
                        HTML("<br>"), 
                        fluidRow(
                          column(3, h5("download template"), downloadButton("download_template_variant", label = "download template")),
                                 column(9, h5("data opladen (.xlsx)"), fileInput("upload_data_variant", label = NULL, multiple = F, accept = ".xlsx", width = '100%', placeholder = NA))),
                                 dataTableOutput('variant_data')), 
                          column(2, actionButton(inputId = "reset_data_variant", label = "reset dataset", width = '100%'), h4(),
                                actionButton(inputId = "delete_variant", label = "verwijder variant", width = '100%'), h4(),
                                downloadButton("download_variants", label = "download", style = "width:100%;")))
                       ),
                    
                    tabPanel(title = "Inspecteer variant",
                       fluidPage(HTML("<br>"), 
                       fluidRow(
                         column(3, h5("Welke variant wilt u bekijken?"), dataTableOutput('variant_names'), HTML("<br>")),
                         column(9, h5("Toelichting variant"), htmlOutput("variant_tekst", align = "justify"),
                         plotlyOutput("plot_variant"),
                         sliderInput(inputId = "plot_variant_jaar", label = NULL, min = 2026, max = 2045, value = 2035, step = 1, pre = "JAAR = ", width = '100%'),
                         ))))
                    
                ))), 
      
        tabPanel(div(style = "font-size: 14px","Dataset vergelijkingen"),
                 div(style = "font-size: 10px; padding: 0px 0px; margin-bottom:-20em",
                 HTML("<br>"),
                 sidebarPanel(
                   
                   # SIDEBAR
                   h5("Naam vergelijking"), helpText("Welke naam wilt u de vergelijking geven?"), 
                   textInput(inputId = "naam_vergelijking", label = NULL, value = "Vergelijking 1"),
                   h5("Selecteer casus"), helpText("Voor welke belastingplichtige(n) wilt u varianten vergelijken?"), HTML("<br>"), dataTableOutput('select_case'),
                   h5("Selecteer varianten"), helpText("Welke varianten wilt u vergelijken?"), HTML("<br>"), dataTableOutput('select_variant'),
                   actionButton(inputId = "add_comparison", label = "vergelijking toevoegen", width = '100%'),
                   
                   width = 3)),
                 
                 mainPanel(
                   tabsetPanel(
                     tabPanel(title = "Bewerk dataset", 
                            fluidPage(
                              HTML("<br>"),
                              column(10, 
                              HTML("<b>!!! Instructie !!!</b> Bepaal in de linker (grijze) kolom welke belastingplichtigen en varianten u wilt vergelijken.
                              De tool rekent dan elke combinatie van de selecties voor u door voor de variabelen grondslag, verrekend verlies en belasting.
                              De dataset presenteert vervolgens een gemiddelde van alle jaren (2026-2045) voor elke belastingplichtige voor elk van deze
                              variabelen. Dit betekent dat belastingplichtigen met dezelfde aanwas maar met verschillende fluctuaties in rendement een 
                              verschillende grondslag hebben of belasting betalen. De tabel voorziet daarnaast in een aantal statistieken op basis waarvan
                              u de varianten (objectief) kunt vergelijken, met name:
                              <br>
                              <ul>
                              <li>grondslag gelijkheid, m.n. de mate waarin burgers met dezelfde aanwas dezelfde grondslag hebben (0-100);</li>
                              <li>belasting gelijkheid, m.n. de mate waarin burgers met dezelfde aanwas dezelfde belasting betalen (0-100);</li>
                              <li>bugdettaire stabiliteit, m.n. de mate waarin opbrengst constant is over de jaren heen (0-100). </li>
                              <li>proximiteit winstbelasting, m.n. de mate waarin verliezen kunnen verrekend (0-100). </li>
                              </ul>
                              Bent u niet tevreden met de dataset? 
                              In het onderstaande luik kunt u alle data te verwijderen middels de <em>reset</em> knop of een enkele <em>rij verwijderen</em>.
                              Wil u de ingegeven data opslaan, druk dan op de <em>download</em> knop rechtsboven.
                              U kunt vervolgens een enkele vergelijking bekijken onder de tab <em>inspecteer vergelijking</em>.<br>"),
                              HTML("<br>"), 
                              dataTableOutput('comparison_data')), 
                              column(2, 
                                     actionButton(inputId = "reset_data_comparison", label = "reset dataset", width = '100%'), h4(),
                                     actionButton(inputId = "delete_comparison", label = "verwijder rij", width = '100%'), h4(),
                                     downloadButton("download_comparison", label = "download", style = "width:100%;")
                                     ))), 
                   
                   
                   
                   tabPanel("Inspecteer vergelijking casi"),
                   tabPanel("Inspecteer vergelijking varianten")
                   
                 ))) #,
            
            
                # VOOR WINDOW 3
                # htmlOutput("case_variant_tekst", align = "justify"), 
                # h5("Welke casus wilt u bekijken?"), dataTableOutput('case_names_variant'))
            
                #div(style = "font-size: 14px; margin-bottom:-20em; line-height: 25px", 
                #column(5, 
                #      
                #       absolutePanel(
                #         h4("Casus", align = "center"), 
                #         
                #         fluidRow(
                #           column(6, selectInput(inputId = "case_type_2", label = "Welke casus wilt u bekijken?", choices = c("meest recent", "mediaan", "hoogste aanwas", "laagste aanwas"))),
                #           column(6, selectInput(inputId = "jaar_nu", label = "Jaar", choices = 2026:2045, selected = 2036)),
                #         ),
                #         htmlOutput("belasting_text", align = "justify"), HTML("<br>"),
                #         actionButton(inputId = "new_case", label = "nieuwe casus", width = '100%'), HTML("<br><br>"),
                #       tabsetPanel(
                #         tabPanel("Overzicht", absolutePanel(HTML("<br>"), plotOutput("variant_plot")) , align = "center"), 
                #         tabPanel("Grondslag", absolutePanel(HTML("<br>"), plotOutput("belasting_plot_1")) , align = "center"), 
                #         tabPanel("Belasting", absolutePanel(HTML("<br>"), plotOutput("belasting_plot_2")) , align = "center")
                #         ), width = 550) # eind tabsetPanel()
                       
                #) # eind column()
                #), # eind div()
                
            #column(4, absolutePanel(h4("Dataset varianten", align = "center"), helpText("De onderstaande tabel bevat de door u opgeslagen varianten evenals de belasting voor de mediaan belastingplichtige uit uw dataset. Deze dataset wordt in de komende stappen gebruikt om varianten te vergelijken voor dezelfde belastingplichtige of om de situatie van de belastingplichtige in verschillende varianten te vergelijken."),
            #                        HTML("<br>"), dataTableOutput('variant_data'), HTML("<br><br>"), 
            #                        fluidRow(column(6,actionButton(inputId = "reset_variant_data", label = "reset", width = '100%')), column(6,downloadButton(outputId = "download_varianten", label = "download", width = '100%'))), 
            #                        width = 400))
                # eind tabPanel 
        
        
        # WELKE VARIANT IS HET MEEST GUNSTIG VOOR BELASTINGPLICHTIGE
        
        #tabPanel(div(style = "font-size: 14px","Welke variant is het meest gunstig?"),
        #         div(style = "font-size: 10px; padding: 0px 0px; margin-bottom:-20em", 
        #         HTML("<br>"),
        #             
        #         # INPUT 
        #         sidebarPanel(
        #           h5("Variabele"), helpText("Voor welke variabele wilt u de varianten vergelijken? (grondslag, verrekend verlies, belasting)"), selectInput(inputId = "yvar", label = "", choices = c("grondslag (€)", "grondslag (% aanwas)", "verrekend verlies (€)", "verrekend verlies (% verlies)", "belasting (€)", "belasting (% aanwas)"), selected = "belasting"),
        #           h5("Tijd"), helpText("Wilt u één jaar bekijken of de gemiddelden van alle jaren (2026-2045)?"), selectInput(inputId = "timevar", label = "", choices = c("één jaar", "alle jaren"), selected = "één jaar"),
        #           h5("Selecteer casus"), helpText("Voor welke belastingplichtige(n) wilt u varianten vergelijken?"), HTML("<br>"), dataTableOutput('case_selection'),
        #           h5("Selecteer varianten"), helpText("Welke varianten wilt u vergelijken?"), HTML("<br>"), dataTableOutput('variant_selection'),
        #           actionButton(inputId = "add_favorite", label = "favoriet toevoegen", width = '100%'),
        #           width = 3)), # eind sidebarPanel()
                 
        #         # OUTPUT 
        #         div(style = "font-size: 14px; margin-bottom:-20em; line-height: 25px", 
        #             column(5, 
        #                    
        #                    absolutePanel(
        #                      h4("Vergelijking", align = "center"), 
        #                      htmlOutput("variant_text", align = "justify"), HTML("<br>"),
        #                      tabsetPanel(
        #                        
        #                        tabPanel("Varianten", HTML("<br>"), plotOutput("compare_variants_plot", )),
        #                        tabPanel("Belastingplichtigen", HTML("<br>"), plotOutput("compare_individuals_plot"))
        #                      ), width = 550)# eind tabsetPanel()
        #                    
        #             ) # eind column()
        #         )    
        #             
        #         ) 
          
          
        ) # eind TabSetPanel()
      ) # eind FluidPage()


# SERVER
server = function(input, output) {
  
      #showModal(modalDialog(
      #  title = "Welkom bij SandBox 3!",
      #  "Deze app helpt u uw eigen microvoorbeelden te bouwen voor elke denkbare variant van het toekomstig Box 3 stelsel! 
      #   Heeft u opmerkingen, vragen, frustraties, of een nodeloos lange liefdesverklaring, neem vooral contact op met de datanerds on call:
      #   Dr. Sjifra de Leeuw (s.e.leeuw@minfin.nl) en Dr. Josha Box (j.m.h.box@minfin.nl).",
      #   easyClose = TRUE
      #))
  
  ################################## TABPANEL 1: WINDOW 1 ##################################
    
    # DOWNLOAD TEMPLATE 
    output$download_template = downloadHandler(
      filename = function() { 
        paste("sandbox3_template.xlsx", sep="")
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
            crisis_janee = ""),
          file)
      })
    
    # VOORGEPROGRAMEERDE STANDAARD DATA 
    case_data = reactiveVal(case_data)
    
    # OPLADEN DATA 
    upload_data = reactiveValues(data = NULL)
    
    gen_upload_data = function(){
      
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
    }
    
    observeEvent(input$upload_data, {upload_data$data = gen_upload_data()})
    
    gen_empty_data = function(){
      data = gen_upload_data() %>% filter(row_number() %in% -1)
      return(data)
    }
    
    # OUTPUT TABEL
    output$grondslag_data = renderDataTable({
      
      # UPLOAD DATA 
      inFile = input$upload_data
      
      if (is.null(inFile)){data = case_data()} else {data = upload_data$data}
      
      select(data, c("omschrijving",  "risico", "jaar", "vermogen", "aanwas", "aanwas_forfait", "spaargeld", "finproduct", "restbezit", "schuld", "spaargeld_rendperc", "finproduct_rendperc", "restbezit_rendperc", "schuld_rendperc")) %>% 
        setNames(c("omschrijving", "risico", "jaar", "vermogen", "aanwas", "aanwas overbrugging", "spaargeld", "financiële producten", "overig bezit", "schuld", "aanwas spaargeld (%)", "aanwas financiële producten (%)", "aanwas overig bezit (%)", "aanwas schuld (%)")) 
      
    }, server = F, rownames = F, selection = 'single', options = list(paging =T, pageLength = 12, scrollX = T))
    
    # KNOP RESET DATA
    observeEvent(input$reset_data, {
      
      if (is.null(input$upload_data)){case_data() %>% filter(row_number() %in% -1) %>% case_data() 
      } else {upload_data$data =  gen_empty_data()}
      
    })
    
    # KNOP DOWNLOAD 
    selected_data = function(input = NULL){
      
      if (is.null(input)){data = case_data()
      } else {data = upload_data$data}
      
      return(data)
    }
    
    output$download_cases = downloadHandler(
      
      filename = function() {paste("sandbox3_cases.xlsx", sep="")},
      content = function(file) {write_xlsx(selected_data(input$upload_data), file)}
      
      )
    
    # KNOP CASUS TOEVOEGEN
    observeEvent(input$add_case, {
      
      # Easter Egg
      easteregg = data.frame(
        omschrijving = c("Nick Jongerius", "Sjifra de Leeuw", "Josha Box", "Stefan Smalbrugge", "Nienke Cornelissen", "Allard Smit",
                         "Aschwin Moes", "Bart van den Hof", "Kees den Boogert", "Marjolein van den Berg", "Marjan Nienhuis", "Ruud Beenhakker", "Koen van Schie", 
                         "Rocus van Opstal"), 
        voornaam = c("Nick", "Sjifra", "Josha", "Stefan", "Nienke", "Allard", "Aschwin", "Bart", "Kees", "Marjolein", "Marjan", "Ruud", "Koen", "Rocus"), 
        geslacht = c("man", "vrouw", "man", "man", "vrouw", "man", "man", "man", "man", "vrouw", "vrouw", "man", "man", "man")
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
        paste0(voornaam, " vindt zichzelf te knap voor ", sample(easteregg$voornaam, size = 1), ". Ben jij wel knap genoeg? Bel dan snel!"),
        paste0("Croissantjes lekker voorverwarmd op een inductieplaat, eitjes in de airfryer pan hup in de oven,
               versgeperste sinaasappelsap met meer pitjes dan sap. Heerlijk wakker worden want ", voornaam, 
               " heeft een geslaagd ontbijt voor je klaarstaan."), 
        paste0("Romantiek voor ", voornaam, " is lekker samenzitten met een goed boek onder een sterrenhemel. 
                Maar niet in de Provence. Je vult weer vanalles voor ", vnw_bezit, " in. Daar heeft ", vnw, 
                " veel moeite mee. Dus laat ", vnw_bezit, " in rust een boekje lezen.")
        
      )
      
      
        showModal(modalDialog(
          title = sample(opmerkingen, size = 1),
          easyClose = TRUE
        ))
      }
      
      # het serieuze gedeelte 
      
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
          id = id, omschrijving = omschrijving_new, 
          risico = gen_value(mean = isolate(input$risico), sd = sd, n = 1), jaar = 2026,
          spaargeld = spaargeld, finproduct = finproduct, restbezit = restbezit, 
          schuld = schuld, spaargeld_rendperc = spaargeld_rendperc, finproduct_rendperc = finproduct_rendperc, restbezit_rendperc = restbezit_rendperc, 
          schuld_rendperc = schuld_rendperc), sd_rend = sd_rend, crisis = input$crisis)
      
      if (is.null(input$upload_data)){  
        case_data() %>% 
          bind_rows(dat) %>%
          case_data()   
      } else { 
        upload_data$data = upload_data$data %>% bind_rows(dat)   
        
      }
       
    })
    
    # KNOP RANDOM CASUS 
    observeEvent(input$random_case, {
      
      if (is.null(input$upload_data)){data = case_data()} 
      if (!is.null(input$upload_data)){data = upload_data$data}
      if(nrow(data) == 0){id = 1} else {id = max(data$id) + 1}
      
      if(isolate(input$omschrijving) %in% data$omschrijving){omschrijving_new = paste0(isolate(input$omschrijving), " ", max(data$id) + 1)
      } else {omschrijving_new = isolate(input$omschrijving)}
      
      risico = sample(0:10, 1, replace = T)
      #sd = sd
      sd_rend = risico*sd
      
      
     dat = gen_history(data.frame(
        id = id, omschrijving = omschrijving_new, 
        risico = risico, jaar = 2026,
        spaargeld = max(c(gen_value(mean = 42300, sd = sd), 0)), 
        finproduct = max(c(gen_value(mean = 7000, sd = sd), 0)), 
        restbezit = sample(seq(0,1000000,100000), 1, replace = T), 
        schuld = max(c(gen_value(mean = 12800, sd = sd), 0)), 
        spaargeld_rendperc = max(c(gen_value(mean = 0.36, sd = sd), 0)),
        finproduct_rendperc = max(c(gen_value(mean = 6.17, sd = sd_rend), 0)), 
        restbezit_rendperc = max(c(gen_value(mean = 6.17, sd = sd_rend), 0)), 
        schuld_rendperc = max(c(gen_value(mean = 2.57, sd = sd)), 0)), 
        sd_rend = sd_rend, crisis = input$crisis)
      
     if (is.null(input$upload_data)){  
       case_data() %>% 
         bind_rows(dat) %>%
         case_data()   
     } else { 
       upload_data$data = upload_data$data %>% bind_rows(dat)  
     }
      
    })
    
    # KNOP VERWIJDER CASUS 
    observeEvent(input$delete_case, {
      
      remove = input$grondslag_data_rows_selected
      
      if (is.null(input$upload_data)){    
        remove = case_data()[remove,"omschrijving"]
        case_data() %>% 
          subset(., omschrijving != remove) %>%
          case_data()   
      } else { 
        remove = upload_data$data[remove, "omschrijving"]
        upload_data$data = upload_data$data %>% subset(., omschrijving != remove)
      }
      
    })
    
    
    ################################## TABPANEL 1 -- WINDOW 2 ##################################
    
    # DATA CASE NAMES
    output$case_names = renderDataTable({
      
      inFile = input$upload_data
      if (is.null(inFile)){data = case_data()} else {data = upload_data$data}
      data = subset(data, jaar == 2026)
      select(data, c("omschrijving")) 
    
    }, server = F, rownames = F, selection = 'single', options = list(paging =T, pageLength = 50, scrollX = T))
    
    # TEKSTUELE OMSCHRIJVING GRONDSLAG
    
    output$grondslag_tekst = renderText({ 
      
      text = ""
      
      # als geen data beschikbaar
      if (nrow(case_data()) < 1){text = "WAARSCHUWING: Er is geen data beschikbaar. Voeg data toe met behulp van de tool."
      
      # als wel data beschikbaar
      } else {
        
        inFile = input$upload_data
        selection =  input$case_names_rows_selected
        selection = selection[length(selection)]
        if (is.null(selection)){selection =1}
        if (is.null(inFile)){data = case_data()} else {data = upload_data$data}
        df = subset(data, jaar == 2026)[selection,]
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
        
        text = paste0(text, "<b> Vóór het toepassen van het HVI en verliesverrekening is de belastinggrondslag daardoor gelijk aan ", number_to_money(df$aanwas), "</b>. ")
        
        # Vergelijking met overbruggingswetgeving
        
        verschil = df$aanwas - df$aanwas_forfait
        if (verschil > 0){text = paste0(text, "<b>Dat is ", number_to_money(verschil), " meer dan de grondslag die belastingplichtige zou hebben vóór toerekening van het heffingvrij vermogen onder de overbruggingswetgeving.</b> <br>")} 
        else if (verschil < 0){text = paste0(text, "<b>Dat is ", number_to_money(abs(verschil)), " minder dan de grondslag die belastingplichtige vóór toerekening van het heffingvrij vermogen  zou hebben onder de overbruggingswetgeving.</b> <br>")}
        else {text = paste0(text, "<b> Dat is exact evenveel als de grondslag die belastingplichtige vóór toerekening van het heffingvrij vermogen zou hebben onder de overbruggingswetgeving.</b> <br>")}
        
      }})
    
    # TEKSTUELE TOELICHTING GRONDSLAG 2026-2045
    output$grondslag_grafieken = renderText({ 
      
      inFile = input$upload_data
      if (is.null(inFile)){data = case_data()} else {data = upload_data$data}
      
      if (nrow(data) > 0){
      selection =  input$case_names_rows_selected
      selection = selection[length(selection)]
      if (is.null(selection)){selection =1}
      
      id_no = subset(data, jaar == 2026)[selection,]$omschrijving[1]
      df = subset(data, omschrijving == id_no)
      
      naam = id_no
      
      df_long = subset(data, omschrijving == id_no)
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
      } else {
        text = "WAARSCHUWING: Er is geen data beschikbaar. Voeg data toe met behulp van de tool"
      }
      
      text
      
    })
    
    # PLOT 
    output$plot_grondslag = renderPlotly({
      
      inFile = input$upload_data
      if (is.null(inFile)){data = case_data()} else {data = upload_data$data}
      
      selection =  input$case_names_rows_selected
      selection = selection[length(selection)]
      if (is.null(selection)){selection =1}
      
      id_no = subset(data, jaar == 2026)[selection,]$omschrijving[1]
      df = subset(data, omschrijving == id_no)
      
      if (nrow(df) > 0){
      plot_ly(df, y = ~aanwas_forfait, x = ~jaar, type = 'scatter', 
              line = list(width = 2, color = "#BEBEBE"),
              marker = list(color = '#BEBEBE'),
              inherit = FALSE, showlegend = TRUE,
              mode = 'lines+markers', 
              name = "Forfaitaire aanwas OBW", 
              hoverinfo = 'text') %>%
        add_lines(x = ~jaar, y = ~aanwas, mode = 'lines+markers', 
                  line = list(color = 'black', width = 2), 
                  marker = list(color = 'black'), hoverinfo = 'text',
                  text = ~number_to_money(aanwas), name = "Werkelijke aanwas") %>%
        add_lines(y = 0, x = range(df$jaar), line = list(color = "black", width = 2), inherit = FALSE, showlegend = FALSE) %>% 
        layout(xaxis = list(title = ''), yaxis = list(title = 'Aanwas in €', showticklabels = T), 
               legend = list(orientation = 'h'))
      } else {}
      
    })
    
   
            
    ################################## TABPANEL 2 -- WINDOW 1 #################################
   
    
    # VOORGEPROGRAMEERDE STANDAARD DATA 
    variant_data_input = reactiveVal(variant_data)
    
    # KNOP OPLADEN DATA 
    upload_variant_data = reactiveValues(data = NULL)
    
    gen_upload_variant_data = function(){
      upload_variant_data$data = readxl::read_xlsx(input$upload_data_variant$datapath) %>%
        setNames(c("variant", "hvi", "verlies_drempel", "cf", "cb", "schijf_2", "schijf_3", "tarief_1", "tarief_2", "tarief_3"))
    }
    
    observeEvent(input$upload_data_variant, {upload_variant_data$data = gen_upload_variant_data()})

    # OUTPUT TABEL
    output$variant_data = renderDataTable({
      
      inFile = input$upload_data_variant
      if (is.null(inFile)){data = variant_data_input()} else {data = upload_variant_data$data}
      
      data %>% setNames(c("variant", "hvi", "verlies drempel", "CF", "CB", "S2 €", "S3 €", "T1 %", "T2 %", "T3 %"))
      
    }, server = F, rownames = F, selection = 'single', options = list(paging =T, pageLength = 16, scrollX = T))
    
    # KNOP RESET DATA
    observeEvent(input$reset_data_variant, {
      
      if (is.null(input$upload_data_variant)){variant_data_input() %>% filter(row_number() %in% -1) %>% variant_data_input()
      } else {upload_variant_data$data =  data.frame(variant = as.character(), hvi = as.numeric(), verlies_drempel = as.numeric(),
                                                     cf = as.numeric(), cb = as.numeric(), schijf_2 = as.numeric(), 
                                                     schijf_3 = as.numeric(), tarief_1 = as.numeric(), tarief_2 = as.numeric(),
                                                     tarief_3 = as.numeric())}
      
    })
    
    # KNOP DOWNLOAD 
    selected_data_variant = function(input = NULL){
      
      if (is.null(input)){data = variant_data_input()
      } else {data = upload_variant_data$data}
      
      return(data)
    }
    
    output$download_variants = downloadHandler(
      
      filename = function() {paste("sandbox3_variants.xlsx", sep="")},
      content = function(file) {write_xlsx(selected_data_variant(input$upload_data_variant), file)}
      
    )
    
    # KNOP VERWIJDER VARIANT 
    observeEvent(input$delete_variant, {
      
      remove = input$variant_data_rows_selected
      remove = remove[length(remove)]
      
      if (is.null(input$upload_data_variant)){  
        
        remove = variant_data_input()[remove, "variant"]
        
        variant_data_input() %>% 
          subset(., variant != remove) %>%
          variant_data_input()
        
      } else { 
        upload_variant_data$data = upload_variant_data$data[-remove,]
      }
      
    })
    
    # KNOP VARIANT TOEVOEGEN
    observeEvent(input$add_variant, {
      
      if (is.null(input$upload_data_variant)){data = variant_data_input()} 
      if (!is.null(input$upload_data_variant)){data = upload_variant_data$data}
      
      if(isolate(input$naam_variant) %in% data$variant){naam_variant_new = paste0(isolate(input$naam_variant), " ", nrow(data)+1)
      } else {naam_variant_new = isolate(input$naam_variant)}
      
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
        tarief_3 = isolate(input$tarief_3)) 
      
      if (is.null(input$upload_data_variant)){  
        variant_data_input() %>% 
          bind_rows(dat) %>%
          variant_data_input()
      } else {
        
        dat = dat %>% setNames(c("variant", "hvi", "verlies drempel", "CF", "CB", "S2 €", "S3 €", "T1 %", "T2 %", "T3 %"))
        upload_variant_data$data = upload_variant_data$data %>% 
          bind_rows(dat)   
      } 
      
    })
    
    # KNOP DOWNLOAD TEMPLATE
    output$download_template_variant = downloadHandler(
      filename = function() { 
        paste("sandbox3_template_variant.xlsx", sep="")
      },
      content = function(file) {
        write_xlsx(
          data.frame(
            variant = as.character(),
            hvi = as.numeric(),
            verlies_drempel = as.numeric(),
            cf = as.numeric(),
            cb = as.numeric(),
            schijf_2 = as.numeric(),
            schijf_3 = as.numeric(),
            tarief_1 = as.numeric(),
            tarief_2 = as.numeric(),
            tarief_3 = as.numeric()),
          file)
      })
    
    ################################## TABPANEL 2 -- WINDOW 2 #################################
    
    # DATA VARIANT NAMES
    output$variant_names = renderDataTable({
      
      inFile = input$upload_data_variant
      if (is.null(inFile)){data = variant_data_input()} else {data = upload_variant_data$data}
      select(data, c("variant")) 
      
    }, server = F, rownames = F, selection = 'single', options = list(paging =T, pageLength = 18, scrollX = T))
    
    # TEKST VARIANT 
    output$variant_tekst = renderText({
      
      if (is.null(input$upload_data_variant)){dat_variant = variant_data_input()} else {dat_variant = upload_variant_data$data}
      
      if (!is.null(input$variant_names_rows_selected)){
        dat_variant_id = dat_variant %>%
          filter(row_number() %in% input$variant_names_rows_selected) %>%
          select("variant")
        dat_variant_id = dat_variant_id$variant[nrow(dat_variant_id)]
      } else {
        dat_variant_id = dat_variant$variant[1]
      }
      
      dat_variant = subset(dat_variant, variant == dat_variant_id)
      
      if (nrow(dat_variant) > 0){
      
      # PARAMETERS
      naam_variant = dat_variant_id
      hvi = dat_variant$hvi
      vv_drempel = dat_variant$verlies_drempel
      vv_cf = dat_variant$cf
      vv_cb = dat_variant$cb
      s2 = dat_variant$schijf_2; s3 = dat_variant$schijf_3
      t1 = dat_variant$tarief_1; t2 = dat_variant$tarief_2; t3 = dat_variant$tarief_3
      schijf_aantal = 1; if (!is.na(t3) & !is.na(s3)){schijf_aantal = 3};  if (is.na(t3) & is.na(s3) & !is.na(t2) & !is.na(s2)){schijf_aantal = 2} 
      
      # HEFFING VRIJ INKOMEN
      if (hvi > 0){
        text = paste0(
            "<b>Heffingvrij inkomen.</b> Variant <i>", naam_variant, "</i> kent een heffingvrij inkomen van <i>", number_to_money(hvi), "</i>. 
            Belastingplichtigen hoeven over dit bedrag geen belasting af te dragen. Het verhogen van het hvi heeft de volgende gevolgen: <br><br>
            
            <ul>
            <li>het aantal burgers dat een beroep kan doen op verliesverrekening neemt af; </li>
            <li>burgers met verliezen en belastbare winsten, hebben meer baat bij brede mogelijkheid tot verliesverrekening; </li>
            <li>de staat ondervindt budgettaire derving; </li>
            </ul> "
          ) 
       } else {
         text = paste0(
           "<b>Heffingvrij Inkomen.</b> Variant <i>", naam_variant, "</i> kent geen heffingvrij inkomen. 
           Iedereen betaalt dus belasting over zijn inkomen uit vermogen. Het gelijkstellen van het hvi aan nul 
           heeft de volgende gevolgen: <br><br>
            
            <ul>
            <li>het aantal burgers dat een beroep kan doen op verliesverrekening neemt toe; </li>
            <li>burgers hebben meer baat bij brede mogelijkheid tot verliesverrekening; </li>
            <li>de staat ondervindt een hogere budgettaire opbrengst; </li>
            </ul>")
       }
      
      # VERLIESVERREKENING 
      text = paste0(text, "<b>Verliesverrekening.</b> ")
        
        
        
        if (vv_cf == 0 & vv_cb == 0){
          text = paste0(text, "Er is geen mogelijkheid tot verliesverrekening. Belastingplichtigen kunnen hun verliezen dus niet in mindering
                        brengen bij de belastinggrondslag van andere jaren. Geen mogelijkheid tot verliesverrekening voorzien 
                        heeft volgende gevolgen: <br><br>
            
            <ul>
            <li>burgers met veel verlies betalen over de jaren heen te veel belasting; </li>
            <li>burgers hebben meer baat bij een hoger hvi; </li>
            <li>de staat ondervindt een hogere en meer stabiele budgettaire opbrengst. </li>
            </ul>")
          
        } else {
          
          if (vv_drempel > 0){
            text = paste0(text,
            "Er is een drempel voor verliesverrekening van <i>", number_to_money(vv_drempel), "</i>. Verliezen onder deze grens
            kunnen niet verrekend worden. "
            )} else {
              text = paste0(text, "Er is geen drempel voor verliesverrekening. Elk verlies mag verrekend worden. ")
            }
          
          if (vv_cf > 1){
            text = paste0(text, "Variant voorziet <i>", vv_cf, "</i> jaar voorwaartse verliesverrekening. Belastingplichtigen mogen verliezen uit
                          de voorgaande <i>", vv_cf, "</i> jaar in mindering brengen bij de belasting grondslag van het huidig jaar. ")
          } else if (vv_cf == 1) {
            text = paste0(text, "Variant voorziet <i>", vv_cf, "</i> jaar voorwaartse verliesverrekening. Belastingplichtigen mogen verliezen uit het 
                          afgelopen jaar in mindering brengen bij de belasting grondslag van het huidig jaar. ")
            
          } else {
            text = paste0(text, "Variant voorziet geen voorwaartse verliesverrekening. Belastingplichtigen mogen verliezen uit
                          de voorgaand jaren niet in mindering brengen bij de belasting grondslag van het huidig jaar. ")
          }
          
          if (vv_cb > 1){
            text = paste0(text, "Variant voorziet <i>", vv_cb, "</i> jaar achterwaartse verliesverrekening. Belastingplichtigen mogen verlies in het 
                          belastingjaar in mindering brengen bij de belasting grondslag van de voorgaande <i>", vv_cb, "</i> jaar in mindering 
                          brengen bij de belasting grondslag van het huidig jaar. ")
          } else if (vv_cb == 1){
            text = paste0(text, "Variant voorziet <i>", vv_cb, "</i> jaar achterwaartse verliesverrekening. Belastingplichtigen mogen verlies in het 
                          belastingjaar in mindering brengen bij de belasting grondslag van het afgelopen jaar in mindering brengen bij de 
                          belasting grondslag van het huidig jaar. ")
          } else {
            text = paste0(text, "Variant voorziet geen voorwaartse verliesverrekening. Belastingplichtigen mogen verliezen uit
                          de voorgaand jaren niet in mindering brengen bij de belasting grondslag van het huidig jaar. ")
          }
          
          text = paste0(text, "De mogelijkheid tot verliesverrekening verbreden (lagere drempel, langere periode) heeft volgende gevolgen: <br><br>
                          
                        <ul>
                        <li>meer burgers kunnen een beroep op verliesverrekening doen; </li>
                        <li>minder burgers betalen belasting in de hoogste schijf; </li>
                        <li>er onstaat een trade-off: hoe hoger de drempel, hoe meer baat burgers hebben bij een langere periode; </li>
                        <li>de staat ondervindt budgettaire derving en een lagere en minder stabiele opbrengsten. </li>
                        </ul>")
          
        }
          
      # PROGRESSIVITEIT VAN TARIEF
      text = paste0(text, "<b>Progressiviteit tarief.</b> ")
      if (schijf_aantal == 1){text = paste0(text, "Variant kent een vlaktaks. Belastingplichtigen betalen <i>", percentify(t1), "</i> belasting over de grondslag. ")}
      if (schijf_aantal == 2){text = paste0(text, "Variant kent een progressief tarief met twee schijven. Belastingplichtigen betalen <i>", percentify(t1), "</i> belasting over de grondslag tot <i>", number_to_money(s2), 
                                            "</i> en <i>", percentify(t2), "</i> over de rest."  )}
      if (schijf_aantal == 3){text = paste0(text, "Variant kent een progressief tarief met drie schijven.Belastingplichtigen betalen <i>", percentify(t1), "</i> belasting over de grondslag tot <i>", number_to_money(s2), 
                                            "</i>; <i>", percentify(t2), "</i> tot <i>", number_to_money(s3), "</i> en <i>", percentify(t3), "</i> over de rest." )}
      
      text = paste0(text, "Het versterken van de progressiviteit heeft volgende gevolgen: <br><br>
                          
                        <ul>
                        <li>burgers met middelgrote rendementen betalen minder en burgers met grote rendementen meer; </li>
                        <li>de staat ondervindt in beginsel een budgettaire opbrengst; </li>
                        <li>in combinatie met verliesverrekening, is deze opbrengst instabieler. </li>
                        </ul>")
      } else {
        text = "WAARSCHUWING: Er is geen data beschikbaar. Voeg data toe met behulp van de tool."
      }
      
      text
      
    })
    
    # PLOT VARIANT
    output$plot_variant = renderPlotly({
      
      
      ### HIER
      input_jaar = input$plot_variant_jaar
      
      if (is.null(input$upload_data_variant)){dat_variant = variant_data_input()} else {dat_variant = upload_variant_data$data}
      
      if (nrow(dat_variant) > 0){
      
      if (!is.null(input$variant_names_rows_selected)){
        dat_variant_id = dat_variant %>%
          filter(row_number() %in% input$variant_names_rows_selected) %>%
          select("variant")
        dat_variant_id = dat_variant_id$variant[nrow(dat_variant_id)]
      } else {
        dat_variant_id = dat_variant$variant[1]
      }
      
      dat_variant = subset(dat_variant, variant == dat_variant_id)
      
      # PARAMETERS
      naam_variant = dat_variant_id
      hvi = dat_variant$hvi
      vv_drempel = -dat_variant$verlies_drempel
      vv_cf = input_jaar - dat_variant$cf - 0.5; if (vv_cf < 2026){vv_cf = 2026 - 0.5}
      vv_cb = input_jaar + dat_variant$cb + 0.5; if (vv_cb > 2045){vv_cb = 2045 + 0.5}
      s2 = dat_variant$schijf_2; s3 = dat_variant$schijf_3; 
      t1 = dat_variant$tarief_1; t2 = dat_variant$tarief_2; t3 = dat_variant$tarief_3
      
      schijf_aantal = 1; if (!is.na(t3) & !is.na(s3)){schijf_aantal = 3};  if (is.na(t3) & is.na(s3) & !is.na(t2) & !is.na(s2)){schijf_aantal = 2}  
      
      # HIER AANPASSEN!!! randomly generate case knop
      aanwas = c(0.5*hvi, hvi + 0.5*hvi, 2*hvi, 3*hvi, 4*hvi,
                 3*hvi, 2*hvi, 2.5*hvi, 0.7*hvi, 4*vv_drempel, 3*vv_drempel, 
                 1.5*vv_drempel, 0.5*vv_drempel, 1.3*hvi, 2*hvi, 0.2*hvi,
                 0.5*hvi, 0.5*vv_drempel, 1.1*vv_drempel, 0.2*vv_drempel)
      jaar = c(2026:(2025+length(aanwas)))
      
      ymax = 1.75*max(hvi, vv_drempel, aanwas)
      ymin = -ymax
      space = 0.2*hvi
      
      
      s1_text = paste0("<b>Schijf 1:</b> ", percentify(t1), " belasting over aanwas.")
      
      # PLOT
      fig = plot_ly(showlegend = F) %>%
        
        # hvi en vv drempel
        add_polygons(x=c(2025.5,2025.5,2045.5,2045.5),y=c(space,hvi,hvi,space), color=I("grey70"), opacity = 0.3,  name = "<b>Heffingvrij inkomen:</b> Aanwas is niet belastbaar en \nkan ook niet verrekend worden met verliezen.", hoverinfo = 'text', hovertemplate = '%{text}') %>% 
        add_polygons(x=c(2025.5,2025.5,2045.5,2045.5),y=c(vv_drempel,-space,-space, vv_drempel), color=I("grey70"), opacity = 0.3,  name = "<b>Verlies onder verliesverrekenings drempel:</b>\nverlies kan niet verrekend worden.", hoverinfo = 'text', hovertemplate = '%{text}') %>% 
        
        # verliesverrekening 
        add_polygons(x=c(vv_cf,vv_cf,input_jaar,input_jaar),y=c(vv_drempel-space,ymin,ymin,vv_drempel-space), color=I("red"), opacity = 0.3, name = "<b>Voorwaartse verliesverrekening:</b>\nverlies mag verrekend worden met evt. winst belastingjaar.",  hoverinfo = 'text', hovertemplate = '%{text}') %>%
        add_polygons(x=c(vv_cb,vv_cb,input_jaar,input_jaar),y=c(hvi+space,ymax,ymax,hvi+space), color=I("green"), opacity = 0.3, name = "<b>Achterwaartse verliesverrekening:</b>\nwinst mag verrekend worden met evt. verlies belastingjaar.",  hoverinfo = 'text', hovertemplate = '%{text}') %>%
      
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
        add_trace(x=~jaar, y=~aanwas, opacity = 0.7, color=I("grey20"), line = list(dash = 'dash'), name = "<b>Aanwas</b>", hovertemplate = '%{y}') %>%
        layout(yaxis = list(showticklabels = F), hovermode = "x unified", showlegend = T)
            
      fig
      } else {}
      
    })
    
    ################################## TABPANEL 3: WINDOW 1 ##################################
    
    output$select_case = renderDataTable({
      
      inFile = input$upload_data
      if (is.null(inFile)){data = case_data()} else {data = upload_data$data}
      data = subset(data, jaar == 2026)
      select(data, c("omschrijving")) 
      
    }, server = F, rownames = F, options = list(paging =T, pageLength = 7, scrollX = T))
    
    output$select_variant = renderDataTable({
      
      inFile = input$upload_data_variant
      if (is.null(inFile)){data = variant_data_input()} else {data = upload_variant_data$data}
      select(data, c("variant")) 
      
    }, server = F, rownames = F, options = list(paging =T, pageLength = 7, scrollX = T))
    
    `%notin%` <- Negate(`%in%`) # NAAR BOVEN!!!!
    
    # VOORGEPROGRAMEERDE STANDAARD DATA 
    comparison_data = reactiveVal(comparison_data)
    
    output$comparison_data = renderDataTable({
      
      comparison_data() %>%
        setNames(c("vergelijking", "variant", "raming opbrengst", "grondslag gelijkheid", "belasting gelijkheid", "opbrengst stabiliteit", "proximiteit winstbelasting",
                   "hvi", "verlies drempel", "CF", "CB", "S2 €", "S3 €", "T1 %", "T2 %", "T3 %", "belastingplichtige", "risico", "belasting €", 
                   "belasting (% aanwas)", "verlies", "verrekend verlies", "verrekend verlies (% verlies)", "vermogen", "aanwas", "grondslag", "grondslag (% aanwas)",
                   "spaargeld", "financiële producten", "overig bezit", "schuld", "aanwas spaargeld (%)", "aanwas financiële producten (%)", 
                   "aanwas overig bezit (%)", "aanwas schuld (%)"))
      
    }, server = F, rownames = F, options = list(paging =T, pageLength = 8, scrollX = T))
    
    # KNOP VERWIJDER VERGELIJKING 
    
    observeEvent(input$delete_comparison, {
    
      remove = input$variant_data_rows_selected
      
      comparison_data() %>%
        filter(., row_number() %notin% input$comparison_data_rows_selected) %>%
        comparison_data() 
    
    })
    
    # KNOP RESET DATA
    observeEvent(input$reset_data_comparison, {
    
      comparison_data() %>%
        filter(row_number() %in% -1) %>%
        comparison_data()
      
      })
    
    # KNOP DOWNLOAD 
    output$download_comparison = downloadHandler(
      
      filename = function() {paste("sandbox3_vergelijkingen.xlsx", sep="")},
      content = function(file) {write_xlsx(comparison_data(), file)}
      
    )
    
    # VERGELIJKING TOEVOEGEN
    observeEvent(input$add_comparison, {
      
      
      if (length(input$select_variant_rows_selected) > 0){variant_selection = input$select_variant_rows_selected} else {variant_selection = 1}
      if (length(input$select_case_rows_selected) > 0){case_selection = input$select_case_rows_selected} else {case_selection = 1}
      
      # data 
      if (is.null(input$upload_data_variant)){dat_variant = variant_data_input()} else {dat_variant = upload_variant_data$data}
      if (is.null(input$upload_data)){dat_case = case_data()} else {dat_case = upload_data$data}
      
      # enkel als beide datasets observaties hebben 
      if (nrow(dat_variant) > 0 & nrow(dat_case) > 0){
        
          dat_variant = dat_variant[variant_selection,]
          
          # reken door voor elke variant en ....
          dat = list()
          
          for (j in c(1:length(variant_selection))){
          
              case_data_long = list()
              
              # ... voor elke case
              for (i in c(1:length(case_selection))){
                
                  case_id = subset(dat_case, jaar == 2026)$omschrijving[case_selection[i]] 
                  
                  # case data
                  dat_case_temp = subset(dat_case, omschrijving == case_id)
                  dat_case_verlies = verreken_verlies(data = dat_case_temp, hvi = dat_variant$hvi[j], cf = dat_variant$cf[j], cb = dat_variant$cb[j])
                  
                  belasting = list()
                  for (row in c(1:nrow(dat_case_verlies))){
                    temp = bepaal_belasting(grondslag = dat_case_verlies$grondslag[row], 
                                            schijf_2 = dat_variant$schijf_2[j], schijf_3 = dat_variant$schijf_3[j], 
                                            tarief_1 = dat_variant$tarief_1[j], tarief_2 = dat_variant$tarief_2[j], tarief_3 = dat_variant$tarief_3[j])
                    belasting[[row]] = sum(temp$belasting, na.rm = T)
                  }
                  
                  belasting = mean(do.call(rbind, belasting), na.rm = T)
                  
                  
                  # berekeningen
                  temp_case = data.frame(
                                   naam_vergelijking = input$naam_vergelijking, 
                    
                                   # variant
                                   variant = dat_variant$variant[j], 
                                   budget_raming = "t.b.a. in future!!!",
                                   gini_grondslag = "t.b.a. next update", 
                                   gini_belasting = "t.b.a. update", 
                                   opbrengst_stabiliteit = "t.b.a. next update",
                                   prox_winstbelasting = "t.b.a. next update",
                                   hvi = dat_variant$hvi[j], 
                                   verlies_drempel = dat_variant$verlies_drempel[j], 
                                   cf = dat_variant$cf[j], 
                                   cb = dat_variant$cb[j], 
                                   schijf_2 = dat_variant$schijf_2[j], 
                                   schijf_3 = dat_variant$schijf_3[j], 
                                   tarief_1 = dat_variant$tarief_1[j], 
                                   tarief_2 = dat_variant$tarief_2[j], 
                                   tarief_3 = dat_variant$tarief_3[j],
                                   
                                   # belastingplichtige
                                   omschrijving = dat_case_temp$omschrijving[1], 
                                   risico = round(mean(dat_case_temp$risico, na.rm = T), 2), 
                                   belasting = round(belasting, 2), 
                                   belasting_perc = round(((belasting / mean(dat_case_temp$aanwas, na.rm = T))*100), 2), 
                                   verlies = abs(round(mean(subset(dat_case_verlies, aanwas < 0)$aanwas, na.rm = T), 2)),
                                   verrekend_verlies = round(mean(dat_case_verlies$cf, na.rm = T) +  mean(dat_case_verlies$cb, na.rm = T), 2), 
                                   verrekend_verlies_perc = round((mean(dat_case_verlies$cf, na.rm = T) +  mean(dat_case_verlies$cb, na.rm = T)) / abs(mean(subset(dat_case_temp, aanwas < 0)$aanwas, na.rm = T))*100,2),
                                   vermogen = round(mean(dat_case_temp$vermogen, na.rm = T), 2), 
                                   aanwas = round(mean(dat_case_temp$aanwas, na.rm = T), 2), 
                                   grondslag = round(mean(subset(dat_case_verlies, grondslag > 0)$grondslag, na.rm = T), 2), 
                                   grondslag_perc = round((mean(dat_case_verlies$grondslag, na.rm = T)/mean(dat_case_temp$aanwas, na.rm = T))*100, 2), 
                                   spaargeld = round(mean(dat_case_temp$spaargeld, na.rm = T), 2), 
                                   finproduct = round(mean(dat_case_temp$finproduct, na.rm = T), 2), 
                                   restbezit = round(mean(dat_case_temp$restbezit, na.rm = T), 2), 
                                   schuld = round(mean(dat_case_temp$schuld, na.rm = T), 2), 
                                   spaargeld_rendperc = round(mean(dat_case_temp$spaargeld_rendperc, na.rm = T), 2), 
                                   finproduct_rendperc = round(mean(dat_case_temp$finproduct_rendperc, na.rm = T), 2), 
                                   restbezit_rendperc = round(mean(dat_case_temp$restbezit_rendperc, na.rm = T), 2), 
                                   schuld_rendperc = round(mean(dat_case_temp$schuld_rendperc, na.rm = T), 2))
                  
                  temp_case$verlies[is.na(temp_case$verlies)] = 0
                  temp_case$verrekend_verlies[is.na(temp_case$verrekend_verlies)] = 0
                  temp_case$verrekend_verlies_perc[is.na(temp_case$verrekend_verlies_perc)] = 0
                  temp_case$vermogen[is.na(temp_case$vermogen)] = 0
                  temp_case$aanwas[is.na(temp_case$aanwas)] = 0
                  temp_case$grondslag[is.na(temp_case$grondslag)] = 0
                  temp_case$grondslag_perc[is.na(temp_case$grondslag_perc)] = 0
                  temp_case$spaargeld[is.na(temp_case$spaargeld)] = 0
                  temp_case$finproduct[is.na(temp_case$finproduct)] = 0
                  temp_case$restbezit[is.na(temp_case$restbezit)] = 0
                  temp_case$schuld[is.na(temp_case$schuld)] = 0
                  temp_case$spaargeld_rendperc[is.na(temp_case$spaargeld_rendperc)] = 0
                  temp_case$finproduct_rendperc[is.na(temp_case$finproduct_rendperc)] = 0
                  temp_case$restbezit_rendperc[is.na(temp_case$restbezit_rendperc)] = 0
                  temp_case$schuld_rendperc[is.na(temp_case$schuld_rendperc)] = 0
                  
                  
                  case_data_long[[i]] = temp_case
              
              }
              
              dat[[j]] = do.call(rbind, case_data_long)
          
          }
          
          dat = do.call(rbind, dat)
          
          comparison_data() %>%
            bind_rows(dat) %>%
            comparison_data() 
      
      } else {
      
        comparison_data()  
        
      }
      
    })
    
    
    ################################## TABPANEL 3 -- WINDOW 2 #################################
    
    # DATA CASE NAMES
    output$case_names_variant = renderDataTable({
      
      inFile = input$upload_data
      if (is.null(inFile)){data = case_data()} else {data = upload_data$data}
      data = subset(data, jaar == 2026)
      select(data, c("omschrijving")) 
      
    }, server = F, rownames = F, selection = 'single', options = list(paging =T, pageLength = 5, scrollX = T))
    
    # TEKST GRONDSLAG EN BELASTING 2026
    
    output$case_variant_tekst = renderText({ 
      
      jaar_nu = as.numeric(2044)
      
      # overbruggingswetgeving
      hvv = 56000
      
      # variant data 
      if (is.null(input$upload_data_variant)){dat_variant = variant_data_input()} else {dat_variant = upload_variant_data$data}
      
      if (!is.null(input$variant_names_rows_selected)){
        dat_variant_id = dat_variant %>%
          filter(row_number() %in% input$variant_names_rows_selected) %>%
          select("variant")
        dat_variant_id = dat_variant_id$variant[nrow(dat_variant_id)]
      } else {
        dat_variant_id = dat_variant$variant[1]
      }
      
      dat_variant = subset(dat_variant, variant == dat_variant_id)
      naam_variant = dat_variant_id

      # case data
      if (is.null(input$upload_data)){dat_case = case_data()} else {dat_case = upload_data$data}
      
      if (!is.null(input$case_names_variant_rows_selected)){
        dat_case_id = subset(dat_case, jaar == jaar_nu) %>%
            filter(row_number() %in% input$case_names_variant_rows_selected) %>%
            select("omschrijving")
        dat_case_id = dat_case_id$omschrijving[nrow(dat_case_id)]
      } else {
        dat_case_id = as.character(subset(dat_case, jaar == jaar_nu)[1, "omschrijving"])
      }
      
      dat_case = subset(dat_case, omschrijving == dat_case_id)
      naam_case = dat_case_id
      
      # bereken verlies
      df = verreken_verlies(dat_case, hvi = dat_variant$hvi, cf = dat_variant$cf, cb = dat_variant$cb, drempel = dat_variant$verlies_drempel)
      df_jaar_nu = subset(df, jaar == jaar_nu)
      
      cf_jaar = jaar_nu - dat_variant$cf; if (cf_jaar < 2026){cf_jaar = 2026} # start jaar voorwaartse verliesverrekening
      cb_jaar = jaar_nu - dat_variant$cb; if (cb_jaar < 2026){cb_jaar = 2026} # start jaar achterwaartse verliesverrekening  
      
      verlies_totaal = abs(sum(subset(df, aanwas < 0)$aanwas, na.rm = T)) # verlies alle jaren
      verlies = abs(sum(subset(df, aanwas < 0 & jaar < jaar_nu)$aanwas, na.rm = T)) # verlies tot huidig jaar
      
      verlies_cf = abs(sum(subset(df, aanwas < 0 & jaar < jaar_nu & jaar >= cf_jaar)$aanwas, na.rm = T))
      verlies_cf_v = abs(sum(subset(df, aanwas < (-1*dat_variant$verlies_drempel) & jaar < jaar_nu & jaar >= cf_jaar)$aanwas, na.rm = T))
      
      # bereken aanwas
      aanwas = df_jaar_nu$aanwas
      aanwas_totaal = sum(subset(df, aanwas > 0)$aanwas, na.rm = T)
      aanwas_obw = dat_case$aanwas_forfait
      aanwas_obw_totaal = sum(subset(df, aanwas > 0)$aanwas_forfait, na.rm = T)
      
      # bereken aanwas minus hvi
      aanwas_hvi = df_jaar_nu$aanwas - dat_variant$hvi; if (aanwas_hvi < 0){aanwas_hvi = 0}
      aanwas_hvi_totaal = sum(subset(df, aanwas > dat_variant$hvi)$aanwas, na.rm = T); if (aanwas_hvi_totaal < 0){aanwas_hvi_totaal = 0}
      aanwas_obw_hvv = sum(subset(df, aanwas > hvv)$aanwas_forfait, na.rm = T); if (aanwas_obw_hvv < 0){aanwas_obw_hvv = 0}
      
      # bereken grondslag 
      grondslag = df_jaar_nu$grondslag; if (grondslag < 0){grondslag = 0}
      grondslag_obw = aanwas_obw_hvv
      grondslag_verschil = grondslag - grondslag_obw
      
      
      # CORRIGEER HIER!!!!
      #vermogen = df$vermogen
      
      #hvv = 56000
      #prp = hvv / vermogen
      #if (prp < 1){prp = 1}
      
      #grondslag_forfait = (df$spaargeld - (prp * df$spaargeld)) + (df$finproduct - (prp * df$finproduct)) + (df$restbezit - (prp * df$restbezit)) - (df$schuld - (prp * df$schuld)) 
      #if (grondslag_forfait < 0){grondslag_forfait = 0}
      
      #aanwas_forfait = 
      #  0.0036*(df$spaargeld - (prp * df$spaargeld)) + 
      #  0.0617*(df$finproduct - (prp * df$finproduct)) + 
      #  0.0617*(df$restbezit - (prp * df$restbezit)) - 
      #  0.0257*(df$schuld - (prp * df$schuld))
      
      # bereken belasting
      belasting_data = bepaal_belasting(grondslag, schijf_2 = dat_variant$schijf_2, schijf_3 = dat_variant$schijf_3, tarief_1 = dat_variant$tarief_1, tarief_2 = dat_variant$tarief_2, tarief_3 = dat_variant$tarief_3)
      belasting_s1 = belasting_data$belasting[1]; belasting_s2 = belasting_data$belasting[2]; belasting_s3 = belasting_data$belasting[3]
      belasting = sum(belasting_data$belasting, na.rm = T)
      
      belasting_obw = 0; if (aanwas_obw_hvv > 0){belasting_obw = 0.34*aanwas_obw_hvv}
      belasting_tov_aanwas = (belasting / aanwas) * 100
      
      belasting_verschil = belasting - belasting_obw
      
      
      ### TEKST ### 
      
      # als aanwas positief 
      if (aanwas >= 0){
      
      # heffingvrij inkomen
      text = paste0("<b>Heffingvrij Inkomen</b>. De grondslag staat gelijk aan de aanwas minus het Heffingvrij Inkomen (HVI) en het evt. bedrag voortvloeiend uit verliesverrekening. Belastingplichtige heeft in kalenderjaar <i>", number_to_money(aanwas), " aanwas</i>. ")
      text = paste0(text, "<i>Het HVI is €", dat_variant$hvi, "</i>. ")
      
      # als kleiner dan hvi
      if (aanwas <= dat_variant$hvi){
        text = paste0(text, "De grondslag na verrekening van het HVI is dus €0. ")
        
        # vergelijking overbrugging
        if (dat_case$vermogen > hvv){
              text = paste0(text, "Daar het vermogen van belastingplichtige meer dan €56000 bedraagt, zou na verrekening van het heffingvrij vermogen de <i>grondslag onder de overbruggingswetgeving gelijk zijn aan ", number_to_money(aanwas_obw_hvv), ".</i> ")
            } else {
              text = paste0(text, "Daar het vermogen van belastingplichtige minder dan €56000 bedraagt, zou de grondslag onder de overbruggingswetgeving na verrekening van het heffingvrij vermogen eveneens gelijk zijn aan €0. ")
            }
        
        # verliesverrekening
        text = paste0(text, "<b>Verliesverrekening.</b> Ook verliezen uit voorgaande jaren kunnen niet verrekend worden, net als onder de overbruggingswetgeving.<br><br>")
        
        # belasting en grondslag
        if (belasting_obw > 0){
          text = paste0(text, "<b>Daar de aanwas onder het HVI ligt, is de grondslag gelijk aan €0 en hoeft belastingplichtige geen belasting te betalen. Hij zou wél ", number_to_money(belasting_obw), " belasting betaald hebben onder de overbruggingswetgeving. </b>")
        } else {
          text = paste0(text, "<b>Daar de aanwas onder het HVI ligt, is de grondslag gelijk aan €0 en hoeft belastingplichtige geen belasting te betalen. Dit zou ook het geval zijn geweest onder de overbruggingswetgeving.</b>")
        }
        
      # als groter dan hvi  
      } else if (aanwas > dat_variant$hvi) {
        
        # verrekening hvi
        text = paste0(text, "De belastbare aanwas vóór verliesverrekening is daarbij gelijk aan <i>", number_to_money(aanwas_hvi), "</i>. ")
        
        if (dat_case$vermogen > hvv){
            text = paste0(text, "Daar het vermogen van belastingplichtige meer dan €56000 bedraagt, zou na verrekening van het heffingvrij vermogen de <i>grondslag onder de overbruggingswetgeving gelijk zijn aan ", number_to_money(aanwas_obw_hvv), ".</i> ")
        } else {
            text = paste0(text, "Daar het vermogen van belastingplichtige minder dan €56000 bedraagt, zou de grondslag onder de overbruggingswetgeving na verrekening van het heffingvrij vermogen echter gelijk zijn aan €0. ")
        }
        
        # voorwaartse verliesverrekening
        if (dat_variant$cf == 1){verlies_voor_tijd = "het afgelopen jaar"} else if (dat_variant$cf > 1){verlies_voor_tijd = paste0("de afgelopen ", dat_variant$cf, " jaren")}
        if (dat_variant$cf >0){text = paste0(text, "<b>Voorwaartse verliesverrekening.</b> Belastingplichtige mag evt. verlies uit <i>", verlies_voor_tijd, "</i> verrekenen met de belastbare winst in het huidig jaar, als dit verlies groter is dan <i>", number_to_money(dat_variant$verlies_drempel), "</i>. Let wel dat verliesverrekening slechts van toepassing is op de periode vanaf 2026. Verliezen van voor die tijd worden niet meegenomen. Onder de overbruggingswetgeving zou belastingplichtige geen verlies mogen verrekenen. ")
        } else {text = paste0(text, "<b>Voorwaartse verliesverrekening.</b> Belastingplichtige mag geen verlies verrekenen. Onder de overbruggingswetgeving zou belastingplichtige geen verlies mogen verrekenen. ")}
        
        if(df_jaar_nu$cf > 0){
          text = paste0(text, "In deze jaren heeft de belastingplichtige <i>", number_to_money(verlies_cf), "</i> verlies gemaakt, waarvan <i>", number_to_money(verlies_cf_v), "</i> in totaal verrekend mag worden.
                               In het huidig jaar mag belastingplichtige <i>", number_to_money(df_jaar_nu$cf), "</i> verlies verrekenen. ")}
        if (df_jaar_nu$cf == 0){text = paste0(text, "Belastingplichtige heeft al het verrekenbaar verlies reeds kunnen verrekenen met de belastbare winst uit voorgaande jaren of heeft geen verrekenbaar verlies gehad. ")}
        
        # grondslag
        if (grondslag_verschil > 0) {verschil_aanwas_text = paste0(" ", number_to_money(abs(grondslag_verschil)), " meer dan onder de overbruggingswetgeving. ")}
        else if (grondslag_verschil < 0) {verschil_aanwas_text = paste0(" ", number_to_money(abs(grondslag_verschil)), " minder dan onder de overbruggingswetgeving. ")} 
        else {verschil_aanwas_text = " evenveel als onder de overbruggingswetgeving. "}
        text = paste0(text, "<b>Grondslag.</b> De grondslag bedraagt ", number_to_money(grondslag), ". Dat is ",  verschil_aanwas_text, " ")
        
        # belasting
        schijf_aantal = nrow(subset(belasting_data, !is.na(belasting)))
          
        if (schijf_aantal == 1){text = paste0(text, "<b>Belasting.</b> Met slechts één schijf betaalt belastingplichtige <i>", percentify(dat_variant$tarief_1), "</i> belasting over deze grondslag, m.n. <i>", number_to_money(grondslag*(dat_variant$tarief_1/100)), "</i>. <br><br>")}
        if (schijf_aantal == 2){text = paste0(text, "<b>Belasting.</b> De grondslag wordt verdeeld over twee schijven. Belastingplichtige betaalt <i>", percentify(dat_variant$tarief_1), "</i> belasting over <i>", number_to_money(belasting_data$aanwas[1]), "</i> in Schijf 1 (<i>", number_to_money(belasting_s1), "</i>); en <i>", percentify(dat_variant$tarief_2), "</i> belasting over <i>", number_to_money(belasting_data$aanwas[2]), "</i> in Schijf 2, (<i>", number_to_money(belasting_s2), "</i>). <br><br>")}
        if (schijf_aantal == 3){text = paste0(text, "<b>Belasting.</b> De grondslag wordt verdeeld over drie schijven. Belastingplichtige betaalt <i>", percentify(dat_variant$tarief_1), "</i> belasting over <i>", number_to_money(belasting_data$aanwas[1]), "</i> in Schijf 1 (<i>", number_to_money(belasting_s1), "</i>); <i>", percentify(dat_variant$tarief_2), "</i> belasting over <i>", number_to_money(belasting_data$aanwas[2]), "</i> in Schijf 2, (<i>", number_to_money(belasting_s2), "</i>); en <i>", percentify(dat_variant$tarief_3), "</i> belasting over <i>", number_to_money(belasting_data$aanwas[3]), "</i> in Schijf 3 (<i>", number_to_money(belasting_s3), "</i>). <br><br>")}
      
        # overzicht 
        if (belasting_verschil > 0) {verschil_belasting_text = paste0(number_to_money(abs(belasting_verschil)), " meer dan onder de overbruggingswetgeving. ")}
        else if (belasting_verschil < 0) {verschil_belasting_text = paste0(number_to_money(abs(belasting_verschil)), " minder dan onder de overbruggingswetgeving. ")} 
        else {verschil_belasting_text = " evenveel als onder de overbruggingswetgeving. "}
        text = paste0(text, "<b> In totaal betaalt belastingplichtige ", number_to_money(belasting), " belasting. Dit is ", percentify(belasting_tov_aanwas), " van de totale aanwas in het belastingjaar. Dit is ", verschil_belasting_text)
        
      } 
      
      
      
      
      # als aanwas < 0    
      #} else {
      
      # grondslag
      #text = paste0("<b>Grondslag</b>. Belastingplichtige heeft in kalenderjaar <i>", number_to_money(aanwas), "</i> aanwas. Daar de aanwas negatief is, staat de belastinggrondslag gelijk aan <i>€0</i>. <br><br>")
      
      # achterwaartse verliesverrekening
      
      #if (input$verlies_achter == 1){verlies_achter_tijd = "het afgelopen jaar"} else if (input$verlies_achter > 1) {verlies_achter_tijd = paste0("de afgelopen ", input$verlies_achter, " jaren.")} 
      
      #if (input$verlies_achter > 0){text = paste0(text, "<b>Achterwaartse verliesverrekening</b>. Belastingplichtige mag zijn verlies uit het huidig jaar verrekenen met zijn eventuele belastbare aanwas uit <i>", verlies_achter_tijd, "</i> als dit verlies boven de <i> drempel van ", number_to_money(input$verlies_drempel),  "</i> ligt. Let wel dat verliesverrekening slechts van toepassing is op de periode vanaf 2026. Verliezen van voor die tijd worden niet meegenomen. ")}
      #else {text = paste0(text, "<b>Achterwaartse verliesverrekening</b>. Belastingplichtige mag zijn verlies uit het huidig jaar niet verrekenen. ")}
      
      #if(input$verlies_achter > 0){
      
      #  text = paste0(text, "In deze periode heeft belastingplichtige <i>", number_to_money(winst_vroeger_belast), "</i> belastbare aanwas gehad. ")
      #  if (verlies_data$cb == 0){text = paste0(text, "Voor zover belastingplichtige in het verleden belastbare aanwas heeft gehad, heeft hij deze reeds kunnen verrekenen met eerdere verliezen. ")}
      #  if (verlies_data$cb > 0){text = paste0(text, "Belastingplichtige mag <b>", number_to_money(verlies_data$cb), " belastbare aanwas verrekenen</b> met het verrekenbaar verlies in het kalenderjaar. ")}
      
      
      
      # }}}} # eind nrow(case_data()>0)
      
      text
      }
      })
    
    
    
    
    
    
    
   
    
    # 2.1. TEKST GRONDSLAG EN BELASTING
    
    # Switch dataset based on case type
    microdata_2 <- reactive({
      
      max_id = subset(aggregate(aanwas ~ id, data = subset(case_data(), jaar == isolate(as.numeric(input$jaar_nu))), FUN = sum), aanwas == max(aanwas))$id
      min_id = subset(aggregate(aanwas ~ id, data = subset(case_data(), jaar == isolate(as.numeric(input$jaar_nu))), FUN = sum), aanwas == min(aanwas))$id
      
      if (input$case_type_2 == "mediaan"){dataset = get_median(data = case_data(), year = isolate(as.numeric(input$jaar_nu)), period = c(2026:2045))}
      else if (input$case_type_2 == "meest recent"){dataset <- subset(case_data(), id == max(case_data()$id))}
      else if (input$case_type_2 == "hoogste aanwas"){dataset <- subset(case_data(), id == max_id)}
      else if (input$case_type_2 == "laagste aanwas"){dataset <- subset(case_data(), id == min_id)}
      return(dataset)
    })
    
    
    
    # 2.2. KNOP NIEUWE CASUS 
    
    
    
    
    # 2.4. PLOT GRONDSLAG
    output$belasting_plot_1 = renderPlot({
      
      
      df = subset(
        verreken_verlies(microdata_2(),hvi = input$hvi, cf = input$verlies_voor, cb = input$verlies_achter,drempel = input$verlies_drempel), 
        jaar == as.numeric(input$jaar_nu))
      
      
      # als aanwas groter dan hvi
      if (df$aanwas > input$hvi){
        
        data = data.frame(
          group = c( "(1) HVI", "(2) Verliesverrekening", "(3) Grondslag"),
          value = c(input$hvi, df$cf, df$grondslag))
        
        data$group = paste0(data$group, " (", number_to_money(data$value), ")")
        
        data <- data %>% 
          arrange(desc(group)) %>%
          mutate(prop = value / sum(data$value) *100) %>%
          mutate(ypos = cumsum(prop)- 0.5*prop )  
        
        ggplot(data, aes(x="", y=prop, fill=group)) +
          geom_bar(stat="identity", width=1, color="black", size = 1) +
          coord_polar("y", start=0) +
          theme_void() + 
          geom_text(aes(y = ypos, label = paste0(round(prop), "%")), color = "black", size=6, fontface = "bold") +
          scale_fill_manual(values = c("white", "grey90", "grey70", "grey40")) + 
          theme(axis.ticks=element_blank(),  
                axis.title=element_blank(),  
                axis.text.y=element_blank(), 
                legend.title = element_blank(),
                legend.key.size = unit(1.25, 'cm'), 
                legend.key.height = unit(1.25, 'cm'), 
                legend.key.width = unit(1.25, 'cm'), 
                legend.text = element_text(size=14)) 
        
      } else if (df$aanwas > 0 & df$aanwas < input$hvi){
        
        data = data.frame(
          group = c( "(1) HVI", "(2) Verliesverrekening", "(3) Grondslag"),
          value = c(df$aanwas, 0, 0))
        
        data$group = paste0(data$group, " (", number_to_money(data$value), ")")
        
        data <- data %>% 
          arrange(desc(group)) %>%
          mutate(prop = value / sum(data$value) *100) %>%
          mutate(ypos = cumsum(prop)- 0.5*prop )  
        
        
        ggplot(data, aes(x="", y=prop, fill=group)) +
          geom_bar(stat="identity", width=1, color="black", size = 1) +
          coord_polar("y", start=0) +
          theme_void() + 
          geom_text(aes(y = ypos, label = paste0(round(prop), "%")), color = "black", size=6, fontface = "bold") +
          scale_fill_manual(values = c("white", "grey90", "grey70", "grey40")) + 
          theme(axis.ticks=element_blank(),  
                axis.title=element_blank(),  
                axis.text.y=element_blank(), 
                legend.title = element_blank(),
                legend.key.size = unit(1.25, 'cm'), 
                legend.key.height = unit(1.25, 'cm'), 
                legend.key.width = unit(1.25, 'cm'), 
                legend.text = element_text(size=14)) 
        
      } else {
        
        ggplot() + 
          geom_text(aes(x = 0, y = 0, label = "Aanwas is negatief. Grondslag berekening is n.v.t."), fontface = "bold") + 
          theme_void()
        
      }
      
      
    }, bg="transparent", width = 500, height = 400, res = 72)
    
    # 2.5. PLOT BELASTING
    output$belasting_plot_2 = renderPlot({
      
      df = subset(
        verreken_verlies(microdata_2(), hvi = input$hvi, cf = input$verlies_voor, cb = input$verlies_achter, drempel = input$verlies_drempel), 
        jaar == as.numeric(input$jaar_nu))
      
      # als aanwas groter dan hvi
      if (df$aanwas > input$hvi){
        
        aanwas_belast = df$grondslag
        
        belasting = bepaal_belasting(aanwas_belast, schijf_2 = input$schijf_2, schijf_3 = input$schijf_3, tarief_1 = input$tarief_1, tarief_2 = input$tarief_2, tarief_3 = input$tarief_3)
        belasting = data.frame(
          schijf = paste0("Schijf ", rep(c(1:3),2)),
          grens = c(0, input$schijf_2, input$schijf_3),
          tarief = c(input$tarief_1, input$tarief_2, input$tarief_3),
          type = c(rep("Aanwas", 3), rep("Belasting", 3)), 
          waarde = c(belasting$aanwas, belasting$belasting)
        )
        
        if (!is.na(belasting$grens[3])){
          brks = c(0, belasting$grens[2], belasting$grens[3])
          labs = c(paste0("S1: ", belasting$tarief[1], "% v.a. €", belasting$grens[1]), paste0("S2: ", belasting$tarief[2], "% v.a. €", belasting$grens[2]), paste0("S3: ", belasting$tarief[3], "% v.a. €", belasting$grens[3]))
        } else if (is.na(belasting$grens[3]) & !is.na(belasting$grens[2])){
          brks = c(0, belasting$grens[2])
          labs = c(paste0("S1: ", belasting$tarief[1], "% v.a. €", belasting$grens[1]), paste0("S2: ", belasting$tarief[2], "% v.a. €", belasting$grens[2]))
        } else {
          brks = 0
          labs = c(paste0("S1: ", belasting$tarief[1], "% v.a. €", belasting$grens[1]))
        }
        
        
        if (aanwas_belast > 0){
        ggplot() +
          geom_bar(data = belasting, aes(x=type, y=waarde, fill=factor(schijf)), stat="identity", width=0.9, color="black", size = 1, position = position_stack(reverse = TRUE)) +
          geom_text(data = data.frame(x = c("Aanwas", "Belasting"), y = c(sum(belasting$waarde[1:3], na.rm = T), sum(belasting$waarde[4:6], na.rm = T))), aes(x = x, y = y, label = paste0("€", y, " (", round((y/y[1])*100), "%)")), vjust = -0.9, fontface = "bold") + 
          scale_fill_manual(values = c("white", "grey70", "grey40")) + 
          scale_y_continuous(breaks = brks, labels = labs, sec.axis = sec_axis( trans=~.*1, name="Aanwas en belasting in €")) +
          theme_minimal() + 
          theme(axis.ticks=element_blank(),  
                axis.title=element_blank(),  
                axis.text = element_text(face = "bold", size = 14),
                legend.title = element_blank(),
                legend.position = "bottom", 
                legend.key.size = unit(1.25, 'cm'), 
                legend.key.height = unit(1.25, 'cm'), 
                legend.key.width = unit(1.25, 'cm'), 
                legend.text = element_text(size=14),
                panel.grid = element_blank())
        }
        
        
      }}, bg="transparent", width = 500, height = 400, res = 72)
    
    
    # 2.6 TABEL VARIANTEN 
    
    # initialiseer dataset
    vergelijking = reactiveVal(variant_data)
      
    # toon lege dataset
    #output$variant_data = renderDataTable(
    #  datatable(vergelijking() %>%
    #              `colnames<-` (c("variant", "jaar", "hvi", "verlies drempel", "CF", "CB", "S1 (€)", "S2 (€)", "S3 (€)", "T1 (%)", "T2 (%)", "T3 (%)", "persoon id", "aanwas", "verrekend verlies (€)", "verrekend verlies (%)", "grondslag", "grondslag OBW", "belasting (€)", "belasting OBW (€)", "belasting (%)", "belasting OBW (%)")),
    #            options = list(pageLength = 14, scrollX = TRUE))
    #)
    
    # voeg nieuwe variant toe 
   observeEvent(input$add_variant2, {
      
      df = verreken_verlies(microdata_2(), hvi = input$hvi, cf = input$verlies_voor, cb = input$verlies_achter, drempel = input$verlies_drempel)
      jaar = as.numeric(input$jaar_nu)
      aanwas_belast = df$grondslag[df$jaar == jaar]
      belasting = bepaal_belasting(aanwas_belast, schijf_2 = input$schijf_2, schijf_3 = input$schijf_3, tarief_1 = input$tarief_1, tarief_2 = input$tarief_2, tarief_3 = input$tarief_3)
      naam_variant = isolate(input$naam_variant)
      if (naam_variant %in% vergelijking()$variant){naam_variant = as.character(random_id(n = 1, bytes = 4, use_openssl = TRUE))} 
      
      # OBW 
      grondslag_obw = round(overbrug_me(df$spaargeld[df$jaar == jaar], df$finproduct[df$jaar == jaar], df$restbezit[df$jaar == jaar], df$schuld[df$jaar == jaar], type = "aanwas na hvv"))
      aanwas_voor_hvv = round(overbrug_me(df$spaargeld[df$jaar == jaar], df$finproduct[df$jaar == jaar], df$restbezit[df$jaar == jaar], df$schuld[df$jaar == jaar], type = "aanwas voor hvv"))
      belasting_obw = round(sum(bepaal_belasting(grondslag_obw, schijf_2 = input$schijf_2, schijf_3 = input$schijf_3, tarief_1 = input$tarief_1, tarief_2 = input$tarief_2, tarief_3 = input$tarief_3)$belasting, na.rm = T))
      # HIER AANPASSEN!!!
      # HIER AANPASSEN!!!
      # HIER AANPASSEN!!!
      
      newrow = data.frame(
        variant = naam_variant,
        jaar = jaar,
        hvi = isolate(input$hvi),
        verlies_drempel = isolate(input$verlies_drempel),
        cf = isolate(input$verlies_voor),
        cb = isolate(input$verlies_achter),
        schijf_1 = 0,
        schijf_2 = isolate(input$schijf_2),
        schijf_3 = isolate(input$schijf_3),
        tarief_1 = isolate(input$tarief_1),
        tarief_2 = isolate(input$tarief_2),
        tarief_3 = isolate(input$tarief_3),
        id = df$id[1], 
        aanwas = df$aanwas[df$jaar == jaar],
        vv = sum(df$cb) + sum(df$cf),
        vv_perc = ((sum(df$cb) + sum(df$cf)) / abs(sum(subset(df, aanwas < 0)$aanwas)))*100,
        grondslag = df$grondslag[df$jaar == jaar],
        grondslag_forfait = grondslag_obw,
        belasting = round(sum(belasting$belasting, na.rm = T), 2),
        belasting_forfait = belasting_obw,
        belasting_perc = round((sum(belasting$belasting, na.rm = T) / sum(df$aanwas))*100,2),
        belasting_forfait_perc = round((belasting_obw / aanwas_voor_hvv)*100,2)
      )
      
      # update data
      vergelijking() %>%
        rbind(., newrow) %>%
        vergelijking()
    })
    
    
    output$vergelijking = renderDataTable(
      datatable(
        vergelijking(), 
        options = list(pageLength = 4))
    )
    
    
    # 2.7. KNOP RESET DATA
    observeEvent(input$reset_variant_data, {
      vergelijking() %>% 
        filter(row_number() %in% -100) %>% 
        vergelijking()
    })   
    
    # 2.8. DOWNLOAD DATA VARIANTEN
    output$download_varianten = downloadHandler(
      filename = function() { 
        paste("sandbox3_varianten_", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        write_xlsx(vergelijking(), file)
      })
    
     
  
    
    # 3. WELKE VARIANT IS HET MEEST VOORDELIG VOOR BELASTINGPLICHTIGE?
    
    # 3.1. INPUT TABELLEN
    output$case_selection = DT::renderDataTable(select(subset(case_data(), jaar == 2036), c("omschrijving", "vermogen", "aanwas")), selection = 'single', server = FALSE, rownames = F, options = list(paging =TRUE, pageLength = 7, scrollX = TRUE))
    output$variant_selection = DT::renderDataTable(select(vergelijking(), c("variant", "hvi", "verlies_drempel", "cf", "cb", "schijf_1", "schijf_2", "schijf_3", "tarief_1", "tarief_2", "tarief_3")) %>%
                                                   setNames(c("variant", "hvi", "verlies drempel", "CF", "CB", "S1", "S2", "S3", "T1", "T2", "T3")), selection = 'single', server = FALSE, rownames = F, options = list(paging =TRUE, pageLength = 7, scrollX = TRUE))
    
    # 3.2. TEKST 
    output$variant_text = renderText({
      
      "Of een variant al dan niet gunstig uitpakt voor belastingplichtige is afhankelijk van een aantal factoren en de interactie tussen deze factoren. Zo resulteert een hoog HVI in een lage belasting, maar zorgt het er eveneens voor dat belastingplichtigen veel minder vaak een beroep kunnen doen op verliesverrekening. 
      Ook verliesverrekening bijvoorbeeld zorgt voor een lagere afdracht, maar de omvang hiervan is dan weer afhankelijk van of er een progressief tarief is of niet. 
      Daarnaast dient er, naast de belastingplichtige, ook rekening te worden gehouden met de haalbaarheid van de variant voor de belastingdienst. Verliesverrekening zorgt voor een hogere last, terwijl een hoger HVI en verliesverrekenings drempel juist voor een lagere last zorgt. 
      De onderstaande plots bieden echter enkel informatie over de impact van de varianten op de belastingplichtigen.  
      "
      
    })
    
    
    # 3.3. PLOTS 
    
    # switch yvar based on input 
    yvar = reactive({
      
      if (input$yvar == "grondslag (€)"){yvar = "grondslag"}
      else if (input$yvar == "grondslag (% aanwas)"){yvar = "grondslag_perc"} 
      else if (input$yvar == "verrekend verlies (€)"){yvar = "vv"} 
      else if (input$yvar == "verrekend verlies (% verlies)"){yvar = "vv_perc"} 
      else if (input$yvar == "belasting (€)"){yvar = "belasting"} 
      else if (input$yvar == "belasting (% aanwas)"){yvar = "belasting_perc"} 
      
      return(yvar)   
    })
    
    
    
    # vergelijking varianten
    output$compare_variants_plot2 = output$compare_variants_plot = renderPlot({
      
      if (length(input$variant_selection_rows_selected) > 0 & length(input$case_selection_rows_selected) > 0){
        newdata = process_selection(
          variant_selection = input$variant_selection_rows_selected, 
          case_selection = input$case_selection_rows_selected, 
          variant_data =  vergelijking(),
          case_data = case_data(),
          yvar = yvar(), 
          time = input$timevar)
        
        if (grepl("\u20AC", input$yvar, fixed = TRUE)){newdata$unit = number_to_money(newdata$y)} else {newdata$unit = paste0(newdata$y, "%")}
        
        newdata$facet = paste0("belastingplichtige ", newdata$person_id, " (aanwas = ", number_to_money(newdata$aanwas), ")")
        newdata$x = paste0("variant ", newdata$x, "\n(hvi = ", number_to_money(newdata$hvi), ")")
        newdata$vjust = -0.9; if (newdata$y < 0){newdata$vjust = 1.5}
          
          ggplot(data = newdata, aes(x = x, y = y, label = unit, fill = x, vjust = vjust)) + 
            geom_hline(yintercept = max(newdata$y) + max(newdata$y)/10, color = "white", alpha = 0) + 
            geom_bar(stat="identity", width=0.75, color="black", size = 1) +
            geom_text(aes(x = x, y = y, label = unit, vjust = vjust), fontface = "bold") + 
            scale_fill_grey() + 
            theme_minimal() + 
            theme(legend.position = "None",
                  axis.ticks=element_blank(),  
                  axis.title=element_blank(),  
                  axis.text.y = element_blank(),
                  axis.text.x = element_text(face = "bold", size = 12, angle = 90, hjust = 1),
                  panel.grid = element_blank()) + 
            
            facet_wrap(~ facet, labeller = label_wrap_gen(multi_line = TRUE))
      } else {
        
        ggplot() + 
          geom_text(aes(x = 0, y = 10, label = "Selecteer belastingplichtigen en varianten om een plot te genereren."), fontface = "bold") + 
          ylim(0,10) + 
          theme_void()
        
      }
      
    }, bg="transparent", width = 500, height = 600, res = 72)
    
    # vergelijking belastingplichtigen
    output$compare_individuals_plot2 = renderPlot({
      
      if (length(input$variant_selection_rows_selected) > 0 & length(input$case_selection_rows_selected) > 0){
        newdata = process_selection(
          variant_selection = input$variant_selection_rows_selected, 
          case_selection = input$case_selection_rows_selected, 
          variant_data =  vergelijking(),
          case_data = case_data(),
          yvar = yvar(), 
          time = input$timevar)
        
        if (grepl("\u20AC", input$yvar, fixed = TRUE)){newdata$unit = number_to_money(newdata$y)} else {newdata$unit = paste0(newdata$y, "%")}
        
        newdata$facet = paste0("belastingplichtige ", newdata$person_id, "\n(aanwas = ", number_to_money(newdata$aanwas), ")")
        newdata$x = paste0("variant ", newdata$x, "\n(hvi = ", number_to_money(newdata$hvi), ")")
        newdata$vjust = -0.9; if (newdata$y < 0){newdata$vjust = 1.5}
        
        ggplot(data = newdata, aes(x = facet, y = y, label = unit, fill = facet, vjust = vjust)) + 
          geom_hline(yintercept = max(newdata$y) + max(newdata$y)/10, color = "white", alpha = 0) + 
          geom_bar(stat="identity", width=0.75, color="black", size = 1) +
          geom_text(aes(x = facet, y = y, label = unit, vjust = vjust), fontface = "bold") + 
          scale_fill_grey() + 
          theme_minimal() + 
          theme(legend.position = "None",
                axis.ticks=element_blank(),  
                axis.title=element_blank(),  
                axis.text.y = element_blank(),
                axis.text.x = element_text(face = "bold", size = 12, angle = 90, hjust = 1),
                panel.grid = element_blank()) + 
          
          facet_wrap(~ x, labeller = label_wrap_gen(multi_line = TRUE))
      } else {
        
        ggplot() + 
          geom_text(aes(x = 0, y = 10, label = "Selecteer belastingplichtigen en varianten om een plot te genereren."), fontface = "bold") + 
          ylim(0,10) + 
          theme_void()
        
      }
      
    }, bg="transparent", width = 500, height = 600, res = 72)
    
    
  

# EIND SERVER     
}

# Run the application 
shinyApp(ui = ui, server = server)
