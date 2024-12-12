####################
### OLA2 OPGAVE3 ###
####################
library(tidyverse)
library(dplyr)
library(corrplot)
library(pROC)
library(caret)
library(ggplot2)


# Opgave 3.1 - Feature Engineering, Dummy variable


forbrugmeta = dst_meta(table = "NKH1", lang = "da")

forbrugquery = list(
  TRANSAKT = "P.31 Husholdningernes forbrugsudgifter",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*")

forbrugdata = dst_get_data(table = "NKH1", query = forbrugquery, lang = "da")

forbrugdf = pivot_wider(forbrugdata, names_from = TRANSAKT, values_from = value)


# fjerner ligegyldige kolonner
forbrugdf = forbrugdf[,-1:-2]

# udregner årlig realvækst pr. kvartal
# grundet 4 NA-værdier i de første 4 rækker, gøres som følgende: 

realvækst = diff(log(as.numeric(forbrugdf$`P.31 Husholdningernes forbrugsudgifter`)), lag = 4) * 100
realvækstna = rep(NA, nrow(forbrugdf))
realvækstna[5:nrow(forbrugdf)] = realvækst
forbrugdf$realvækst = realvækstna


  # Der er i opgave 2 allerede indhentet forbruget gennem Dkstat API'en
  # gør rede for tabel + query 

View(forbrugdf)


which(forbrugdf$TID == "1998-01-01") # 33 
forbrugdf.log = forbrugdf[33:nrow(forbrugdf), ]

forbrugdf.log$logfactor <- ifelse(forbrugdf.log$realvækst >= 0, 1, 0)
forbrugdf.log$logfactor = as.factor(forbrugdf.log$logfactor)
levels(forbrugdf.log$logfactor)

freqtable = data.frame(table(forbrugdf.log$logfactor))
View(freqtable)
colnames(freqtable) = c("Dummyvariabel", "Frekvens")
rownames(freqtable) = c("Nedgang", "vækst")
# freqtable$Dummyvariabel <- as.factor(freqtable$Dummyvariabel)
# freqtable$Frekvens <- as.numeric(freqtable$Frekvens)



ggplot(freqtable, aes(x =Dummyvariabel, y = Frekvens, fill = Dummyvariabel)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(x = "Udvikling af den kvartalsvis årlige realvækst for husholdningernes forbrugsudgifter; Vækst = 1, Nedgang = 0",
       title = "Der er et stort overtal af vækstkvartaler i perioden 1998K1 - 2024K3",
       subtitle = "Fordelingen af dummyvariablen for den kvartalsvise årlige realvækst",
       caption = "Kilde: Statistikbanken, Table NKH1") +
  theme(plot.title = element_text(hjust = 0),
          legend.position = "right",
        legend.background = NULL) + 
  scale_fill_manual(values = c("0" = "red", "1" = "green"))

# Opgave 3.2 - Logistisk regression og forudsigelser 

# Indhenter de fire underspørgsmål som indgår i DI-FTI gennem DKStat API

DIFTI.meta = dst_meta(table = "FORV1", lang = "da") 

DIFTIquery = list(
  INDIKATOR = c(
                "Familiens økonomiske situation i dag, sammenlignet med for et år siden",
                "Danmarks økonomiske situation i dag, sammenlignet med for et år siden",
                "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket",
                "Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr."),
  Tid = "*")

DIFTIdf = dst_get_data(table = "FORV1", query = DIFTIquery, lang = "da")

DIFTIdf.wide = pivot_wider(DIFTIdf, names_from = INDIKATOR, values_from = value)
which(DIFTIdf.wide$TID == "1998-01-01") # 280
which(DIFTIdf.wide$TID == "2024-07-1") # 598

DIFTIdf.wide = DIFTIdf.wide[280:601, ]

DIFTIdf.wide = as.data.frame(DIFTIdf.wide)



difti.kvartal = DIFTIdf.wide[0 , ]

for (i in seq(1, nrow(DIFTIdf.wide), by = 3)) { 
  kvartal.måned = DIFTIdf.wide[i, 1] 
  kvartal.total = kvartal.måned
  
  for (j in 2:ncol(DIFTIdf.wide)) { 
    gnscol = mean(as.numeric(DIFTIdf.wide[i:(i+2), j]), na.rm = T)
    kvartal.total = c(kvartal.total, gnscol)
  }
  difti.kvartal = rbind(difti.kvartal, kvartal.total)
}

colnames(difti.kvartal) = colnames(DIFTIdf.wide)
difti.kvartal$TID = as.Date(difti.kvartal$TID)


difti.kvartal.uden3kvartal2024 = difti.kvartal[1:(nrow(difti.kvartal)-1), ]

logdf = merge(forbrugdf.log, difti.kvartal.uden3kvartal2024, by = "TID")

# Data er sorteret og klar til logistisk regression. 

    # Før vi kan lave regressionen er det vigtigt at bide mærke i, at vi har multi forklarende variabler
      # vi tester derfor først for multikolinaritet, for at se om dataen yderligere skal manipuleres. 



logvariabler = logdf[, c(5:8)]
logcorr = cor(logvariabler)
corrplot(logcorr, tl.cex = 0.5,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         number.cex = 1,
         diag = F)



    # Enorm høj multikolinaritet mellem variablerne

# Renser variablerne for multikolinaritet vha. PCR. 
  # for opgavens simplicitet medregnes kun PC1

logdf$difti = rowMeans(logdf[, c(5:8)])

logmodel = glm(logfactor ~ difti, data = logdf, family = "binomial")

summary(logmodel)

    # Forudsigelse af forbruget i 3. kvartal 2024
# options(scipen=999)

new.data.4kvartal2024 = data.frame(difti = rowMeans(difti.kvartal[108, c(2:5)]))

prediction.4kvartal = predict(logmodel, newdata = new.data.4kvartal2024, type = "response") # 0.6066

# der er derfor 60.66% chance for at forbruget stiger i 3. kvartal 2024
  # ved en standardgrænseværdi på 0.5 (50%) vil det altså sige, at vores model predicter forbruget til at være "op"/stigende

###############################################
### Opgave 3.3 - Simpel validering af model ###
###############################################

# For at validere modellen, benytter vi os af en konfusionsmatrice med en standardgrænseværdi på 0.5 (50%), for at se fordelingen af korrekte predictions.

prediction.df = data.frame(difti = logdf$difti)
prediction.values = predict(logmodel, prediction.df, type = "response")

# ny tom vektor. til stored binære predictions

prediction.values.vektor = c()

for (i in 1:length(prediction.values)) {
  if (is.na(prediction.values[i])) {
    prediction.values.vektor[i] = "NULL"
  } else if (prediction.values[i] >= 0.5) {
    prediction.values.vektor[i] = "1"
  } else {
    prediction.values.vektor[i] = "0"
  }
}
logdf$predictions50 = as.factor(prediction.values.vektor)

confusionmatrix.threshold50 = confusionMatrix(logdf$predictions50, logdf$logfactor)

cm.50 = as.data.frame(as.table(confusionmatrix.threshold50))
ggplot(cm.50, aes(Prediction, Reference)) +
  geom_tile(aes(fill = Freq), color = "white", size = 1.5) +  # Create the heatmap
  scale_fill_gradient(low = "white", high = "blue") +  # Color scale for the heatmap
  geom_text(aes(label = sprintf("%d", Freq)), vjust = 1) +  # Add text labels
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0)) +  # Rotate axis labels
  labs(x = "forudsagte værdier", y = "aktuelle værdier", title = "Utrolig optismistisk model: rigtig god til forudsigelse af positive værdier, dårlig til negative",
       subtitle = "Konfusionsmatrice med standardgrænseværdi: 0.5 (50%)")

print(confusionmatrix.threshold50)

# samme tilfælde som OLA3 opgave 3 (nærmest kopier tekst) 


######################################################
### Opgave 3.4 - Potentielle forbedringer af model ###
######################################################

# eftersom at modellen er alt for positiv (ignorer negatives) bla bla kopere præcist af fra ola3opgave3

    # søger en balanceret model: maksimum AUC 

logdf$logfactor <- ifelse(logdf$realvækst >= 0, "Op", "Ned")
forbrugdf.log$logfactor = as.factor(forbrugdf.log$logfactor)
levels(forbrugdf.log$logfactor)
logdf$logfactor <- factor(logdf$logfactor, levels = c("Ned", "Op"))



predicted.probs.ROC = predict(logmodel, newdata = logdf, type = "response")
ROC.kurve = roc(logdf$logfactor, predicted.probs.ROC, plot = TRUE)

optimal.grænse = coords(ROC.kurve, "best", best.method = "youden", ret = "threshold")


optimal.grænse.topleft = coords(ROC.kurve, "best", best.method = "closest.topleft", ret = "threshold")

    # tester med optimal.grænse
prediction.values.vektor.opti = c()

for (i in 1:length(prediction.values)) {
  if (is.na(prediction.values[i])) {
    prediction.values.vektor.opti[i] = "NULL"
  } else if (prediction.values[i] >= optimal.grænse) {
    prediction.values.vektor.opti[i] = "1"
  } else {
    prediction.values.vektor.opti[i] = "0"
  }
}
logdf$predictionsopti = as.factor(prediction.values.vektor.opti)

confusionmatrix.opti = confusionMatrix(logdf$predictionsopti, logdf$logfactor)

cm.opti = as.data.frame(as.table(confusionmatrix.opti))
ggplot(cm.opti, aes(Prediction, Reference)) +
  geom_tile(aes(fill = Freq), color = "white", size = 1.5) +  # Create the heatmap
  scale_fill_gradient(low = "white", high = "blue") +  # Color scale for the heatmap
  geom_text(aes(label = sprintf("%d", Freq)), vjust = 1) +  # Add text labels
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0)) +  # Rotate axis labels
  labs(x = "forudsagte værdier", y = "aktuelle værdier", title = "Balanceret model: modellen er nu hverken god til at forudsige positiver eller negativer",
       subtitle = "Konfusionsmatrice med optimalgrænseværdi for balance: 0.80937 (80,9%)")





