##############################################
### OPGAVE 1 Boliger og Danmarks Statistik ###
##############################################

# Opgave 1.1 Det første skridt
# Skriv en kode, der viser hvordan du finder en tabel som kan give en liste over 
# byer med indbyggertal vha DST-pakken dkstat.

# indlæser DST-pakken dkstat;
library("devtools")
library(tidyverse)
library(dkstat)

options(scipen = 999)

  # Finder, henter og udplukker korrekt data;
indbyggersearch = dst_search(string = "befolkningen", field = "text")
indbyggermeta = dst_meta(table = "POSTNR1", lang = "da")
queryindbygger = list(PNR20 = "*",
                      tid = "2024")

#indhenter filteret data
indbyggerdf = dst_get_data(table = "POSTNR1", query = queryindbygger, lang = "da")

  # Filtrer "Hele landet" fra
indbyggerdf <- indbyggerdf[-1,]

  # Frasorter "(kommune-delen)"
indbyggerdf$PNR20 <- gsub("\\s*\\(.*$", "", indbyggerdf$PNR20)

  # Tilføjer "postnr" til ny kolonne
indbyggerdf$postnr <- substr(indbyggerdf$PNR20, 1, 4)

  # Frasorterer de første 4 cifre.
indbyggerdf$PNR20 <- sub("^[^a-zA-ZæøåÆØÅ]*", "", indbyggerdf$PNR20)

  #Dataen aggregeres
test_df <- indbyggerdf %>%
  mutate(by = case_when(
    as.numeric(postnr) >= 1000 & as.numeric(postnr) <= 1799 ~ "København",
    as.numeric(postnr) >= 1800 & as.numeric(postnr) <= 2000 ~ "Frederiksberg",
    as.numeric(postnr) >= 2100 & as.numeric(postnr) <= 2450 ~ "København",
    as.numeric(postnr) >= 5000 & as.numeric(postnr) <= 5270 ~ "Odense",
    as.numeric(postnr) >= 6700 & as.numeric(postnr) <= 6715 ~ "Esbjerg",
    as.numeric(postnr) >= 8000 & as.numeric(postnr) <= 8210 ~ "Aarhus",
    as.numeric(postnr) >= 8900 & as.numeric(postnr) <= 8960 ~ "Randers",
    as.numeric(postnr) >= 9000 & as.numeric(postnr) <= 9220 ~ "Aalborg",
    TRUE ~ NA_character_  # Rækker uden for intervaller får NA
  ))

# Aggregér kun rækker for København og Odense
aggregated_rows <- test_df %>%
  filter(!is.na(by)) %>%  # Vælg kun rækker med en by
  group_by(by) %>%        # Gruppér efter by
  summarize(
    PNR20 = first(by),                # Navn på byen
    TID = first(TID),                 # Beholder TID (eller summer, hvis relevant)
    value = sum(value, na.rm = TRUE)  # Summerer værdier
  )

# Behold de øvrige rækker, som ikke hører til København eller Odense
#remaining_rows <- test_df %>%
  #filter(is.na(by)) %>%  # Vælg rækker uden for de definerede byer
  #select(-by)            # Fjern den midlertidige kolonne "by"

# Kombiner den aggregerede række med de resterende rækker
#indbyggerdf_final <- bind_rows(aggregated_rows, remaining_rows)

cities <- c("København","Odense", "Esbjerg", "Frederiksberg", "Randers", "Aalborg", "Aarhus")

# Initialiser test_col med de originale værdier fra PNR20
indbyggerdf$test_col <- indbyggerdf$PNR20

# Iterér over hver by og opdater test_col
for (city in cities) {
  # Find rækker i indbyggerdf, der indeholder byen
  match_rows <- grepl(city, indbyggerdf$PNR20, ignore.case = TRUE)
  
  # Find værdien fra aggregated.rows for den præcise by
  value_to_insert <- aggregated_rows$value[aggregated_rows$PNR20 == city]
  
  # Sikr, at der kun findes én matchende værdi fra aggregated.rows
  if (length(value_to_insert) == 1) {
    indbyggerdf$value[match_rows] <- value_to_insert
  } else if (length(value_to_insert) > 1) {
    stop(paste("Flere værdier fundet i aggregated.rows for byen:", city))
  } else {
    # Hvis der ikke er match i aggregated.rows, behold værdien
    warning(paste("Ingen værdi fundet i aggregated.rows for byen:", city))
  }
}


indbyggerdf <- indbyggerdf[,-5]

colnames(indbyggerdf) <- c("by","dato","indbyggertal","postnr")

######################################
### opgave 1.2 - Kategori-variabel ###
######################################

# lav en kategorivariabel i R ud fra følgende kriterier: bycat = ...

# definerer kategori-variablen: 

bycat = (c("landsby"=250,
           "lille by"=1000,
           "almindelig by"=2500,
           "større by"=10000,
           "storby"=50000))

# opretter funktion med kategori variablen

# funktion er lavet med bycat[værdi] i stedet for bare en numerisk værdi, så funktionen er dynamisk og ikke hardcoded

bystørrelse <- function(indbyggertal) {
  if (indbyggertal < bycat["landsby"]) {
    return("landet")
  } else if (indbyggertal < bycat["lille by"]) {
    return("landsby")
  } else if (indbyggertal < bycat["almindelig by"]) {
    return("lille by")
  } else if (indbyggertal < bycat["større by"]) {
    return("almindelig by")
  } else if (indbyggertal < bycat["storby"]) {
    return("større by")
  } else {
    return("storby")
  }
}

# bruger bycat + function til at udregne og indsætte bystørrelse udfra bycat conditions
indbyggerdf$bystørrelse = sapply(indbyggerdf$indbyggertal, FUN = bystørrelse) 


#########################################
### Opgave 1.3 Merge de to dataframes ###
#########################################

# Indlæs filen med boliger og tilpas de to dataframes så du kan merge de to sammen via variablen "by"
# således at du får kategorien bycat med i dit bolig-datasæt

boliger = read.csv("data-kopi/boligsiden.csv")

indbyggerdf$postnr <- as.integer(indbyggerdf$postnr)

#merger dataframes og skaber en ny dataframe med rækkerne hvor begge unikke by-observationer er til stede i begge dataframes

indbygbolig = merge(indbyggerdf, boliger, by = "postnr")

#######################
### Opgave 1.4 Plot ###
#######################

# din merge skal producere en dataframe og et plot som vist nedenfor ....

# udplukker variablerne som skal bruges til plot

indbygbolig_clean = indbygbolig[ , c("by.x","postnr", "pris", "kvmpris", "bystørrelse")]

# plotter

## problem: grundet "." i kvmpris, gav plottet forkerte værdier; derfor: 
indbygbolig_clean$kvmpris = gsub("\\.", "", indbygbolig_clean$kvmpris)
indbygbolig_clean$kvmpris = as.numeric(indbygbolig_clean$kvmpris)
str(indbygbolig_clean)
# ændrer rækkefølge på bars i barplottet. 

indbygbolig_clean$bystørrelse = factor(indbygbolig_clean$bystørrelse, 
                                       level = c(
                                         "landet",
                                         "landsby",
                                         "lille by",
                                         "almindelig by", 
                                         "større by",
                                         "storby"))

indbygbolig_clean <- na.omit(indbygbolig_clean)

indbygbolig_clean_summary <- indbygbolig_clean %>%
  group_by(bystørrelse) %>%
  summarize(mean_kvmpris = mean(kvmpris, na.rm = TRUE))

ggplot(indbygbolig_clean_summary, aes(x = bystørrelse, y = mean_kvmpris, fill = bystørrelse))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(
    title = "Større byer har de højeste kvadratmeterpriser",
    subtitle = "En sammenligning af gennemsnitlige kvardratmeterpriser for forskelige typer byer",
    x = NULL,
    y = "kvmpris",
    caption = "Kilde: INDSÆT KILDE")+
  theme_classic()+
  theme(
    title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank(),  
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(vjust = 1, size = 12)
  )
boliger <- na.omit(boliger)
forskel <- setdiff(boliger$postnr,indbygbolig_clean$postnr)
print(forskel)

# Frekvenstabel
freq_bystørrelse_df <- data.frame(table(indbygbolig$bystørrelse))
freq_bystørrelse_df$Var1 <- factor(freq_bystørrelse_df$Var1, levels = c("landet",
                                                                        "landsby",
                                                                        "lille by",
                                                                        "almindelig by", 
                                                                        "større by",
                                                                        "storby"))
ggplot(freq_bystørrelse_df, aes(x = Var1, y = Freq))+
  geom_bar(stat = "identity")+
  scale_y_continuous(breaks = seq(0,max(freq_bystørrelse_df$Freq+100), by = 100))+
  theme_classic()

  