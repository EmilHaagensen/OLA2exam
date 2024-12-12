### Opgave 2_1 ####
  ### Til denne tages der udgangangspunkt i ftikvartal, da vi ikke skal forudsige.

### Subsetter til q1 2000
which(ftikvartal$TID == "2000-03-01")
ftikvartal_di_df <- ftikvartal[17:nrow(ftikvartal),]

### Fjerner det sidste kvartal da det ikke fremgår af NKN3
ftikvartal_di_df <- ftikvartal_di_df[-nrow(ftikvartal_di_df),]

#### DI's forbrugertillid udregnes

ftikvartal_di_df$DIFTI <- rowMeans(ftikvartal_di_df[,c(3,5,10,11)], na.rm = TRUE)
ftikvartal_di_df <- ftikvartal_di_df[,c(1,2,15)]

###### INDSÆT TABLLER AF FTI OG DIFTI I OPGAVEN #####

#### FTI OG DIFTI sammensæsttes nu med "NKN3"

# Indhenter data for forbrug
forbrugmeta = dst_meta(table = "NKN3", lang = "da")

queryforbrug = list(
  TRANSAKT = "P.31 Udgifter til individuelt forbrug",
  PRISENHED = "2020-priser, real værdi, (mia. kr.)",
  Tid="*")

forbrugdatasheet = dst_get_data(table = "NKN3", query = queryforbrug, lang = "da")

# omstrukturing af data
forbrugdf = pivot_wider(data = forbrugdatasheet,
                        names_from = TRANSAKT,
                        values_from = value)

# Fjerner kolonne 1 - prisenhed
forbrugdf <- forbrugdf[,-1]

# Realvækst beregnes
realvækst = diff(log(as.numeric(forbrugdf$`P31 P.31 Udgifter til individuelt forbrug`)), lag = 4)*100

realvækstna=rep(NA, nrow(forbrugdf))
realvækstna[5:nrow(forbrugdf)] = realvækst
forbrugdf$realvækst = realvækstna

### Subsetter dataen til q1 2000 og frem
which(forbrugdf$TID == "2000-01-01")
forbrugdf <- forbrugdf[5:nrow(forbrugdf),]

dstvsdi_forbrug <- data.frame(Tid = as.Date(forbrugdf$TID)+1,
                              dstFTI = ftikvartal_di_df$FTI,
                              diFTI = ftikvartal_di_df$DIFTI,
                              Realvækst = forbrugdf$realvækst)

### Værdierne plottes

scale_factor = 5 #Bruges til at skalere realvæksten.

ggplot(dstvsdi_forbrug, aes(x = Tid)) +
  # Barplot for Realvækst (højre y-akse)
  geom_bar(aes(y = Realvækst * scale_factor, fill = "Årlig realvækst pr. kvartal i privat forbruget (højre akse)"),
           stat = "identity", alpha = 0.8) +
  # Linjeplot for dstFTI (venstre y-akse)
  geom_line(aes(y = dstFTI, color = "Forbrugertillidsindikator for DI"), size = 0.8) +
  # Linjeplot for diFTI (venstre y-akse)
  geom_line(aes(y = diFTI, color = "Forbrugertillidsindikator for DST"), size = 0.8) +
  # Juster venstre y-akse
  scale_y_continuous(
    name = "Nettotal",
    limits = c(-50, 50), # Venstre y-akse går fra -50 til 50
    breaks = seq(-50, 50, by = 10),
    sec.axis = sec_axis(
      ~ . / scale_factor,               # Transformation for højre y-akse
      name = "Pct. (%)",                # Navn for højre y-akse
      breaks = seq(-10, 10, by = 2)     # Højre y-akse går fra -10 til 10
    )
  ) +
  # Juster x-aksen
  scale_x_date(
    breaks = seq(min(dstvsdi_forbrug$Tid), max(dstvsdi_forbrug$Tid), by = "1 years"),
    date_labels = "%Y",
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    values = c("Årlig realvækst pr. kvartal i privat forbruget (højre akse)" = "blue")  # Blå for barplot
  ) +
  scale_color_manual(
    values = c(
      "Forbrugertillidsindikator for DI" = "darkorange1",  # Grå for første linje
      "Forbrugertillidsindikator for DST" = "red4" # Sort for anden linje
    )
  ) +
  
  # Labels og tema
  labs(
    title = "Både DI's og DST's forbrugertillidsindikator følger i højere grad privatforbruget",
    subtitle = "Sammenligning af årlig realvækst i privarforbrug og forbrugertillidsindikatorer fra DI og DST (2000-2024)",
    x = "År",
    color = "Indikator"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10),
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank(),  
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", size = 0.5),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(vjust = 1, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.title.x = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  )

#### Vurdering af hvilken FTI er bedst.
### Multikolinariteten vurderes vha. korrelationsmatricer og laves på baggrund af
### variabler i begge korrelationsmatricerne

# Dst forbrugertillidsindikator: Spgs. 1, Spgs. 2, Spgs 3, spgs 4, spgs 9.
# DI forbrugertillidsindikator: Spgs. spgs. 2, spgs. 4, spgs 9, spgs. 10
### Vi retter datasættet til at fokuserer på ovenstående variable

forbrug_corr_dst <- ftikvartal_di_df[,c(3:6,10)]
forbrug_corr_di <- ftikvartal_di_df[,c(3,5,10,11)]

cor_dst <- cor(forbrug_corr_dst)
cor_di <- cor(forbrug_corr_di)

library(corrplot)


corrplot(cor_dst,
         method = "number",
         type = "lower",
         diag = F,
         number.cex = 1.5,tl.col = "black"
)

corrplot(cor_di,
         method = "number",
         type = "lower",
         diag = F,
         number.cex = 1.5,
         tl.col = "black"
)
cor(dstvsdi_forbrug$Realvækst, dstvsdi_forbrug$dstFTI)
cor(dstvsdi_forbrug$Realvækst, dstvsdi_forbrug$diFTI)

### Der er multikollinaritet i hele skidtet. 
### Dette kan vi komme udenom ved brug af en anden model, men det er for sent nu
### Sidst anvender vi en lienære regression til at se sammenhængen

lm_dst <- lm(realvækst ~ dstFTI,data = dstvsdi_forbrug)
lm_di <- lm(realvækst ~ diFTI,data = dstvsdi_forbrug)

summary(lm_dst)
summary(lm_di)

# Endeligt gemmes korrelation og forklaringsgraden i en tabel
korrelation_di = cor(dstvsdi_forbrug$diFTI, dstvsdi_forbrug$Realvækst)
korrelation_dst = cor(dstvsdi_forbrug$dstFTI, dstvsdi_forbrug$Realvækst)

r2_di <- summary(lm_di)$r.squared
r2_dst <- summary(lm_dst)$r.squared

vurdering_df <- data.frame(enhed = c("Forklaringsgrad", "Korrelation"),
                           di = round(c(r2_di, korrelation_di),2),
                           dst = round(c(r2_dst, korrelation_dst),2))
View(vurdering_df)
