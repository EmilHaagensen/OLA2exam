# Opgave 4.1 - Illustration af forbrugertillid
# Her tages udgangspunkt i FORV1 datasættet

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

# Nu kombineres begge dataframes

ftikvartal <- rbind(ftikvartal,ftikvartal_q4_2024)

# Nu laves punkter, hvor det kan ses hvor forbrugerne er hhv. mest og mindst optimistiske
max_forbruger <- max(ftikvartal$FTI)
min_forbruger <- min(ftikvartal$FTI)

max_x <- ftikvartal$TID[which.max(ftikvartal$FTI)]
min_x <- ftikvartal$TID[which.min(ftikvartal$FTI)]

ggplot(ftikvartal, aes(x = TID, y = FTI))+
  geom_line(aes(color = "Forbrugertillidsindikator"), linewidth = 0.8)+
  geom_hline(aes(yintercept = 0, linetype = "Neutral (0)"), color = "red", linewidth = 0.5)+
  labs(
    title = "Forbrugertilliden faldt til det laveste niveau i slutningen af 2022",
    subtitle = "Grafen viser udviklingen i danskernes forbrugertillid fra 1996 til 2024, med markeringer for laveste og højeste punkter",
    x = NULL,
    y = "Nettotal",
    caption = "Kilde: DST")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("1996-03-01", "2024-09-01")),expand = c(0, 0))+ # Eksakte grænser
  scale_y_continuous(breaks = seq(-35, max(ftikvartal$FTI+10), by = 5))+
  scale_color_manual(values = c("Forbrugertillidsindikator" = "blue4")) +
  scale_linetype_manual(values = c("Neutral (0)" = "dashed")) +
  geom_point(aes(x = min_x, y = min_forbruger), color = "red", size = 3) + # Punkt for min
  geom_point(aes(x = max_x, y = max_forbruger), color = "darkgreen", size = 3) + # Punkt for max
  annotate("text", x = min_x, y = min_forbruger, label = paste0("Min: ", round(min_forbruger, 2)),
           vjust = -1, hjust = 1.2, color = "red", size = 3, fontface = "italic") +
  annotate("text", x = max_x, y = max_forbruger, label = paste0("Max: ", round(max_forbruger, 2)),
           vjust = -1, hjust = 1.2, color = "darkgreen", size = 3, fontface = "italic") +
  theme_bw()+
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10),
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank(),  
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(vjust = 1, size = 12),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1, color = "black"),
    legend.position = "bottom", 
    legend.title = element_blank()
  )

#### Opgave 4.2
# Beregn gennemsnittet af spørgsmål 9.

# Først findes 1. kvartal 2000
which(ftikvartal$TID == "2000-03-01") #17

# Ny dataframe oprettes til brug
ftikvartal_2 <- ftikvartal[17:nrow(ftikvartal),]

q5_mean <- mean(ftikvartal_2$Q8_Forbrugsgoder_nu)


ggplot(ftikvartal_2, aes(x = TID, y = Q8_Forbrugsgoder_nu))+
  geom_line(aes(color = "Q8 - Anskaffelse af større forbrugsgoder"), linewidth = 0.5)+
  geom_hline(aes(yintercept = q5_mean, linetype = "Gennemsnit"), color = "orange", linewidth = 0.4, alpha = 0.9) +
  geom_hline(aes(yintercept = 0, linetype = "Neutral (0)"), color = "red", linewidth = 0.5)+
  labs(
    title = "Forbrugertilliden til større forbrugsgoder forbliver negativ",
    subtitle = "Indikator for, om det anses som fordelagtigt at anskaffe større forbrugsgoder lige nu, viser konsekvent negativ tendens",
    x = NULL,
    y = NULL
  )+
  scale_color_manual(values = c("Q5 - Anskaffelse af større forbrugsgoder" = "blue4")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2000-03-01", "2024-09-01")),expand = c(0, 0))+
  scale_y_continuous(breaks = seq(-50, max(ftikvartal_2$Q8_Forbrugsgoder_nu+5), by = 5))+
  scale_linetype_manual(values = c("Gennemsnit" = "solid", "Neutral (0)" = "dashed")) +
  theme_bw()+
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, face = "italic"),
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank(),  
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(vjust = 1, size = 12),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1, color = "black"),
    legend.position = "bottom", 
    legend.title = element_blank(),  # Fjern legend-titlen
  )

# SVAR - Årsagen skyldes, at netop dette spørgsmål måles i 3 kategorier: 100,0,-100 hvor alle andre måles i 5 kategorier.

#### Opgave 4.3
forbrug_meta_sammenlign <- dst_meta(table = "NAHC021", lang = "da")
myquery_meta_sammenlign <- list(
  FORMAAAL = c("Fødevarer mv.",
               "Drikkevarer og tobak mv.",
               "Beklædning og fodtøj",
               "Boligbenyttelse",
               "Elektricitet, fjernvarme og andet brændsel",
               "Boligudstyr, husholdningstjenester mv.",
               "Medicin, lægeudgifter o.l.",
               "Køb af køretøjer",
               "Drift af køretøjer og transporttjenester",
               "Information og kommunikation",
               "Fritid, sport og kultur",
               "Undervisning",
               "Restauranter og hoteller",
               "Forsikring og finansielle tjenester",
               "Andre varer og tjenester"),
  PRISENHED = "2020-priser, kædede værdier",
  TID = "*"
)

forbrug_sammenlign <- dst_get_data(table = "NAHC021", query = myquery_meta_sammenlign, lang = "da")
forbrug_df_sammenlign <- pivot_wider(data = forbrug_sammenlign,
                                     names_from = FORMAAAL,
                                     values_from = value)

sammenlign_df <- forbrug_df_sammenlign[,-1]

which(sammenlign_df$TID == "2023-01-01") #58
sammenlign_2022_test <- sammenlign_df[58,]

sammenlign_df_clean <- pivot_longer(data = sammenlign_2022_test,
                                    cols = 2:16,
                                    names_to = "Variable",
                                    values_to = "Value")
sammenlign_df_clean <- sammenlign_df_clean[,-1] 

sammenlign_df_clean$Variable <- substr(sammenlign_df_clean$Variable, 5, nchar(sammenlign_df_clean$Variable))

# Omorganiserer faktorerne i forhold til V2
sammenlign_df_clean <- sammenlign_df_clean %>% arrange(desc(Value))  # Sorterer fra størst til lavest

# Faktorisere værdierne, så de plottet viser den korrekte rækkefølge
sammenlign_df_clean$Variable <- factor(sammenlign_df_clean$Variable, levels = sammenlign_df_clean$Variable)

options(scipen = 999)
# Opret barplot
ggplot(sammenlign_df_clean, aes(x = Variable, y = Value, fill = Value)) +
  geom_bar(stat = "identity") +
  labs(title = "Danskerne brugte flest penge på Boligbenyttelse i 2023",
       subtitle = "En rangering af danskernes forbrug på forskellige forbrugsgrupper, målt i mia. DKK",
       x = "Forbrugsgrupper",
       y = "Forbrug i mia. DKK") +
  theme_bw()+
  scale_y_continuous(breaks = seq(0, max(sammenlign_df_clean$Value, na.rm = TRUE), by = 50000))+
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank(),  
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(vjust = 1, size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "none"
  )

#### Find den kategori, som steg mest fra 2020 til 2023

stigning_2023 <- sammenlign_df[c(55,58),]

stigning_2023_df <- as.data.frame(t(stigning_2023))

colnames(stigning_2023_df) <-  c("2020","2023")

stigning_2023_df = stigning_2023_df[-1,]

stigning_2023_df$`2020` = as.numeric(stigning_2023_df$`2020`)
stigning_2023_df$`2023` = as.numeric(stigning_2023_df$`2023`)

stigning_2023_df$`Vækst i procent` = (stigning_2023_df$`2023` - stigning_2023_df$`2020`) / stigning_2023_df$`2020` *100

# Omorganiserer faktorerne i forhold til "Vækst i procent"
stigning_2023_df$forbrugsgrupper <- rownames(stigning_2023_df)
stigning_2023_df <- stigning_2023_df %>% 
  arrange(desc(`Vækst i procent`))  # Sorterer fra størst til lavest

stigning_2023_df$forbrugsgrupper <- substr(stigning_2023_df$forbrugsgrupper, 5, nchar(stigning_2023_df$forbrugsgrupper))
stigning_2023_df$forbrugsgrupper <- factor(stigning_2023_df$forbrugsgrupper, levels = stigning_2023_df$forbrugsgrupper)

ggplot(stigning_2023_df, aes(x = forbrugsgrupper, y = `Vækst i procent`, fill = `Vækst i procent`)) +
  geom_bar(stat = "identity") +
  labs(title = "Beklædning og fodtøj samt restauranter og hoteller har haft den største stigning af 2020 til 2023",
       subtitle = "En rangering af udviklingen i danskernes forbrug fra 2020 til 2023 på forskellige forbrugsgrupper, målt i procent",
       x = NULL,
       y = "Pct. (%)") +
  theme_bw()+
  scale_y_continuous(breaks = seq(-30, max(stigning_2023_df$`Vækst i procent`, na.rm = TRUE), by = 5))+
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10),
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank(),  
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(vjust = 1, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "none"
  )


#### Opgave 4.4

# opgave 4.4 - 22 simple lineære regressioner
# Vi henter forbrugsdataen på ny idet den anden var årligt
# Henter NKHC021 fra DKstat
forbrug_meta <- dst_meta(table = "NKHC021", lang = "da")
myquery_meta2 <- list(
  FORMAAAL = c("Fødevarer mv.",
               "Drikkevarer og tobak mv.",
               "Beklædning og fodtøj",
               "Boligbenyttelse",
               "Elektricitet, fjernvarme og andet brændsel",
               "Boligudstyr, husholdningstjenester mv.",
               "Medicin, lægeudgifter o.l.",
               "Køb af køretøjer",
               "Drift af køretøjer og transporttjenester",
               "Information og kommunikation",
               "Fritid, sport og kultur",
               "Undervisning",
               "Restauranter og hoteller",
               "Forsikring og finansielle tjenester",
               "Andre varer og tjenester"),
  PRISENHED = "Løbende priser",
  SÆSON = "Ikke sæsonkorrigeret",
  TID = "*"
)

forbrug_data <- dst_get_data(table = "NKHC021", query = myquery_meta2, lang = "da")
forbrug_df <- pivot_wider(data = forbrug_data,
                          names_from = FORMAAAL,
                          values_from = value)
forbrug_df <- forbrug_df[,-c(1,2)]

which(forbrug_df$TID == "2000-01-01") #41
forbrug_df <- forbrug_df[41:nrow(forbrug_df),]

# Nu oprettes DI's forbrugertillidsindikator bestående af:
# DK og forbrugernes øko. situation i dag samt købelyst nu.

di_forbrugertillid <- rowMeans(ftikvartal[,c(3,5,10,11)], na.rm = TRUE)

# Vi samler dataframes og fokuserer udelukkende på FTI og DIFTI
fti_samlet <- cbind(di_forbrugertillid, ftikvartal)
fti_samlet = fti_samlet[,c(1,2,3)]

which(fti_samlet$TID == "2000-03-1") #17
fti_samlet <- fti_samlet[17:nrow(fti_samlet),]

# Så fjerner vi kolonne "Tid"
fti_samlet <- fti_samlet[,-2]

# Nu samles begge dataframes
fti_samlet <- fti_samlet[-nrow(fti_samlet),]
fti_samlet_clean <- cbind(fti_samlet, forbrug_df)

# Dataen sorteres korrekt
fti_samlet_clean <- fti_samlet_clean[,c(3,1,2,4:18)]

colnames(fti_samlet_clean) <- c("År",
                                "DI_forbrugertillid",
                                "DST_Forbrugertillid",
                                "Fødevarer",
                                "drikkelse",
                                "Tøj",
                                "Boligbenyttelse",
                                "Aconto",
                                "Boligudstyr",
                                "Medicin",
                                "Køb_af_køretøjer",
                                "Drift_af_køretøjer",
                                "Information",
                                "Sport_og_kultur",
                                "Undervisning",
                                "Restauranter_og_hoteller",
                                "Forsikring",
                                "Andre_varer_og_tjenester")

# Alle regressioner:
# Afhængige variabler
afh_variable <- c("DI_forbrugertillid", "DST_Forbrugertillid")

# Uafhængige variabler
uafh_variable <- c("Fødevarer", "drikkelse", "Tøj", "Boligbenyttelse", 
                   "Aconto", "Boligudstyr", "Køb_af_køretøjer", "Medicin", 
                   "Drift_af_køretøjer", "Information", "Sport_og_kultur", 
                   "Undervisning", "Restauranter_og_hoteller", "Forsikring", "Andre_varer_og_tjenester")

regres_liste <- list()

for (afh in afh_variable) {
  regres_liste[[afh]] <- list()
  for (uafh in uafh_variable) {
    model <- lm(as.formula(paste(afh,"~",uafh)), data = fti_samlet_clean)
    regres_liste[[afh]][[uafh]] <- summary(model)
  }
}

View(regres_liste)
