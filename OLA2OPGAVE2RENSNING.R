##### Opgave 2
  ### Opdater DI's forbrugertillidsindikator med data til og med 2023. 
# Relevante libraries for at køre koden
library(devtools)
library(dkstat)
library(tidyverse)
##################
### Rens FORV1 ###
##################

# Henter fORV1 fra DKstat
# Indhenting af data
ftimeta = dst_meta(table = "FORV1", lang = "da")
queryfti = list(
  INDIKATOR = "*",
  Tid = "*")

ftidata = dst_get_data(table = "FORV1", query = queryfti, lang = "da")

# Pivot_wider for at få underspørgsmål opdelt i kolonner
ftidatawide = pivot_wider(ftidata, 
                          names_from = INDIKATOR,
                          values_from = value)

# Inden dataene subsettet, så rykkes rundt på rækkefølgen, da det hentes forkert.
ftidatawide <- ftidatawide[,c(1:6,8:10,7,11:14)]


# subsetter til relevant tidsramme

which(ftidatawide$TID == "1996-01-01") # SVAR: Række 304
ftisubset = ftidatawide[256:nrow(ftidatawide),]

# omdanner ftisubset til kvartalsvisbasis
class(ftisubset) # ftisubset er en tibble; loop virker ikke på tibble
ftisubset = as.data.frame(ftisubset) # fjerner tibble

ftisubset <- ftisubset[1:345,]

ftikvartal <- ftisubset[0,]


for (i in seq(3, nrow(ftisubset), by = 3)) {
  kvartal_måned = as.Date(ftisubset[i, 1]) #udtrækning af hver 3. kolonne for 'Tid'
  kvartal_total = kvartal_måned
  
  for (j in 2:ncol(ftisubset)) { # udregner gennemsnit for kolonnerne 2:ncol (tid frataget) i grupper af 3 (row 1,2,3 - row 4,5,6 osv.)
    meankolonne =  round(mean(as.numeric(ftisubset[(i-2):i, j]), na.rm = T), 2)
    kvartal_total = c(kvartal_total, meankolonne)
  }
  
  ftikvartal = rbind(ftikvartal, kvartal_total)
}
colnames(ftikvartal)= c("TID", "FTI", "Q1_Fam_nu", "Q2_Fam_etår", "Q3_Dan_nu", "Q4_Dan_etår", "Q5_Priser_nu", "Q6_Priser_etår", 
                        "Q7_Arbejdsløshed", "Q8_Forbrugsgoder_nu","Q9_Forbrugsgoder_etår", "Q10_Sparop_nu", 
                        "Q11_Sparop_etår", "Q12_Famøk_nu")
ftikvartal$TID <- as.Date(ftikvartal$TID+1)
head(ftikvartal)

#### Samme funktion laves til beregning af Q4 baseret på kun 2 værdier
ftisubset_q4_2024 <- ftidatawide[601:nrow(ftidatawide),]
ftisubset_q4_2024 <- as.data.frame(ftisubset_q4_2024)
ftikvartal_q4_2024 <- ftisubset_q4_2024[0,]

for (i in seq(2, nrow(ftisubset_q4_2024), by = 2)) {
  kvartal_måned = as.Date(ftisubset_q4_2024[i, 1]) #udtrækning af hver 3. kolonne for 'Tid'
  kvartal_total = kvartal_måned
  
  for (j in 2:ncol(ftisubset_q4_2024)) { # udregner gennemsnit for kolonnerne 2:ncol (tid frataget) i grupper af 3 (row 1,2,3 - row 4,5,6 osv.)
    meankolonne =  round(mean(as.numeric(ftisubset_q4_2024[(i-1):i, j]), na.rm = T), 2)
    kvartal_total = c(kvartal_total, meankolonne)
  }
  
  ftikvartal_q4_2024 = rbind(ftikvartal_q4_2024, kvartal_total)
}


colnames(ftikvartal_q4_2024)= c("TID", "FTI", "Q1_Fam_nu", "Q2_Fam_etår", "Q3_Dan_nu", "Q4_Dan_etår", "Q5_Priser_nu", "Q6_Priser_etår", 
                                "Q7_Arbejdsløshed", "Q8_Forbrugsgoder_nu","Q9_Forbrugsgoder_etår", "Q10_Sparop_nu", 
                                "Q11_Sparop_etår", "Q12_Famøk_nu")
ftikvartal_q4_2024$TID <- as.Date(ftikvartal_q4_2024$TID+1)
ftikvartal_q4_2024$TID <- as.Date("2024-12-01")

# Nu kombineres begge dataframes - SKAL BRUGES I OPG 2.2

ftikvartal_samlet <- rbind(ftikvartal,ftikvartal_q4_2024)


