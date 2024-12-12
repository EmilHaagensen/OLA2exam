### Opgave 2.2 #####
  ## Predict Q3 og Q4 2024 for realvæksten
### Denne gang tager vi udgangspunkt i ftikvartal_samlet, da vi både vil forudsige for Q3 2024 og Q4 2024

# Vi bruger dataframen fra opgave 4, hvor vi allerede har omdannet til kvartaler samt lavet Q4 2024
forbrug_forudsig_df <- ftikvartal_samlet

#Beregner diFTI
di_vl <-  rowMeans(forbrug_forudsig_df[,c(3,5,10,11)], na.rm = TRUE)
forbrug_forudsig_df$di_FTI <- di_vl

# Susbetter datasættet
which(forbrug_forudsig_df == "2000-03-01")
forbrug_forudsig_df <- forbrug_forudsig_df[17:nrow(forbrug_forudsig_df),]

#Sammensætter DI_FTI og DST_FTI i samme df
dstvsdi <- forbrug_forudsig_df[,c(2,15)]

# Finder værdierne vi predicter ud fra
  # Q3 værdierne
dstvsdi$FTI[99] #DST Q3 = -6,5
dstvsdi$di_FTI[99] #DI Q3 = -8,2

  #Q4 værdierne
dstvsdi$FTI[100] #DST Q4 = -9,1
dstvsdi$di_FTI[100] #DI Q4 = -10,275

new_data_dst_Q3 <- data.frame(dstFTI = -6.5)
new_data_di_Q3 <- data.frame(diFTI = -8.2)

new_data_dst_Q4 <- data.frame(dstFTI = -9.1)
new_data_di_Q4 <- data.frame(diFTI = -10.275)

predictdst_Q3 <- predict(lm_dst, newdata = new_data_dst_Q3) # 0.14
predictdi_Q3 <- predict(lm_di, newdata = new_data_di_Q3) # 0.66

predictdst_Q4 <- predict(lm_dst, newdata = new_data_dst_Q4) #-0.32
predictdi_Q4 <- predict(lm_di, newdata = new_data_di_Q4) # 0.28

dstvsdi_forbrug_forudsig <- dstvsdi_forbrug

### Plotter vores prediction for DST Q3 og Q4 #####
  ### Q3
ggplot(dstvsdi_forbrug_forudsig, aes(x = dstFTI, y = realvækst)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = T, level = 0.95, alpha = 0.5, fill = "grey") + 
  geom_point(aes(x = new_data_dst_Q3$dstFTI, y = predictdst_Q3), color = "red", size = 3) + 
  labs(title = "Ved en forbrugertillidsindikator på -6,5, vil forbrugsvækstkvotienten være 0.14", 
       y = " Årlig realvækst pr. kvartal",
       x = "DST forbrugertillidsindikator")
  ### Q4
ggplot(dstvsdi_forbrug_forudsig, aes(x = dstFTI, y = realvækst)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = T, level = 0.95, alpha = 0.5, fill = "grey") + 
  geom_point(aes(x = new_data_dst_Q4$dstFTI, y = predictdst_Q4), color = "red", size = 3) + 
  labs(title = "Ved en forbrugertillidsindikator på -9.1, vil forbrugsvækstkvotienten være 0.32", 
       y = " Årlig realvækst pr. kvartal",
       x = "DST forbrugertillidsindikator")

### Plotter vores prediction for DI Q3 og Q4 #####
  ### Q3
ggplot(dstvsdi_forbrug_forudsig, aes(x = diFTI, y = realvækst)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = T, level = 0.95, alpha = 0.5, fill = "grey") + 
  geom_point(aes(x = new_data_di_Q3$diFTI, y = predictdi_Q3), color = "red", size = 3) + 
  labs(title = "Ved en forbrugertillidsindikator på -8.2, vil forbrugsvækstkvotienten være 0.66", 
       y = " Årlig realvækst pr. kvartal",
       x = "DI forbrugertillidsindikator")

  ### Q4
ggplot(dstvsdi_forbrug_forudsig, aes(x = diFTI, y = realvækst)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = T, level = 0.95, alpha = 0.5, fill = "grey") + 
  geom_point(aes(x = new_data_di_Q4$diFTI, y = predictdi_Q4), color = "red", size = 3) + 
  labs(title = "Ved en forbrugertillidsindikator på -10.275, vil forbrugsvækstkvotienten være 0.28", 
       y = " Årlig realvækst pr. kvartal",
       x = "DI forbrugertillidsindikator")



### Nu vil vi gerne tilføje punkterne til det endelige plot. For at vi kan gøre det, så skal vi have tilføjet dem til vores dataframe.
dstvsdi_forbrug_forudsig[c(99,100),] = NA
dstvsdi_forbrug_forudsig$Realvækst2 = NA # Opretter en ekstra kolonne til at store realvækst for dst

# Opretter en ny række og gemmer Q3 2024 værdierne 
dstvsdi_forbrug_forudsig$Tid[99] = as.Date("2024-07-01")
dstvsdi_forbrug_forudsig$Realvækst[99] = predictdi_Q3
dstvsdi_forbrug_forudsig$Realvækst2[99] = predictdst_Q3
dstvsdi_forbrug_forudsig$diFTI[99] = new_data_di_Q3
dstvsdi_forbrug_forudsig$dstFTI[99] = new_data_dst_Q3

# Opretter en ny række og gemmer Q4 2024 værdierne 
dstvsdi_forbrug_forudsig$Tid[100] = as.Date("2024-10-01")
dstvsdi_forbrug_forudsig$Realvækst[100] = as.numeric(predictdi_Q4)
dstvsdi_forbrug_forudsig$Realvækst2[100] = as.numeric(predictdst_Q4)
dstvsdi_forbrug_forudsig$diFTI[100] = as.numeric(new_data_di_Q4)
dstvsdi_forbrug_forudsig$dstFTI[100] = as.numeric(new_data_dst_Q4)

dstvsdi_forbrug_forudsig$dstFTI <- unlist(dstvsdi_forbrug_forudsig$dstFTI)
dstvsdi_forbrug_forudsig$diFTI <- unlist(dstvsdi_forbrug_forudsig$diFTI)

### Her plottes Q3-værdierne for 2024

ggplot(dstvsdi_forbrug, aes(x = Tid)) +
  # Barplot for Realvækst (højre y-akse)
  geom_bar(aes(y = Realvækst * scale_factor, fill = "Årlig realvækst pr. kvartal i privat forbruget (højre akse)"),
           stat = "identity", alpha = 0.8) +
  # Linjeplot for dstFTI (venstre y-akse)
  geom_line(aes(y = dstFTI, color = "Forbrugertillidsindikator for DI"), size = 0.8) +
  # Linjeplot for diFTI (venstre y-akse)
  geom_line(aes(y = diFTI, color = "Forbrugertillidsindikator for DST"), size = 0.8) +
  # Punkter: Tilføj 4 geom_point() steder
  geom_point(aes(x = dstvsdi_forbrug_forudsig$Tid[99], y = dstvsdi_forbrug_forudsig$Realvækst[99]), color = "darkorange", size = 1) +  # DI Q3 2024
  #geom_point(aes(x = dstvsdi_forbrug_forudsig$Tid[100], y = dstvsdi_forbrug_forudsig$Realvækst[100]), color = "darkorange", size = 1) +   # DI Q4 2024
  geom_point(aes(x = dstvsdi_forbrug_forudsig$Tid[99], y = dstvsdi_forbrug_forudsig$Realvækst2[99]), color = "red4", size = 1) +    # DST Q3 2024
  #geom_point(aes(x = dstvsdi_forbrug_forudsig$Tid[100], y = dstvsdi_forbrug_forudsig$Realvækst2[100]), color = "red4", size = 1) + # DST Q4 2024
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
    breaks = seq(min(dstvsdi_forbrug_forudsig$Tid), as.Date("2025-01-01"), by = "1 years"),
    date_labels = "%Y",
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
    title = "Forbruget er positivt stigende med begge forbrugsindikatorer for Q3 2024",
    subtitle = "Sammenligning af forudsagte realvækst i privatforbrug og forbrugertillidsindikatorer fra DI og DST (2000-2024 Q3)",
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

### Her plottes Q3-værdierne 2024
ggplot(dstvsdi_forbrug, aes(x = Tid)) +
  # Barplot for Realvækst (højre y-akse)
  geom_bar(aes(y = Realvækst * scale_factor, fill = "Årlig realvækst pr. kvartal i privat forbruget (højre akse)"),
           stat = "identity", alpha = 0.8) +
  # Linjeplot for dstFTI (venstre y-akse)
  geom_line(aes(y = dstFTI, color = "Forbrugertillidsindikator for DI"), size = 0.8) +
  # Linjeplot for diFTI (venstre y-akse)
  geom_line(aes(y = diFTI, color = "Forbrugertillidsindikator for DST"), size = 0.8) +
  # Punkter: Tilføj 4 geom_point() steder
  #geom_point(aes(x = dstvsdi_forbrug_forudsig$Tid[99], y = dstvsdi_forbrug_forudsig$Realvækst[99]), color = "darkorange", size = 1) +  # DI Q3 2024
  geom_point(aes(x = dstvsdi_forbrug_forudsig$Tid[100], y = dstvsdi_forbrug_forudsig$Realvækst[100]), color = "darkorange", size = 2) +   # DI Q4 2024
  #geom_point(aes(x = dstvsdi_forbrug_forudsig$Tid[99], y = dstvsdi_forbrug_forudsig$Realvækst2[99]), color = "red4", size = 1) +    # DST Q3 2024
  geom_point(aes(x = dstvsdi_forbrug_forudsig$Tid[100], y = dstvsdi_forbrug_forudsig$Realvækst2[100]), color = "red4", size = 2) + # DST Q4 2024
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
    breaks = seq(min(dstvsdi_forbrug_forudsig$Tid), as.Date("2025-01-01"), by = "1 years"),
    date_labels = "%Y",
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
    title = "Begge forbrugsindikatorer indikerer lav til ingen vækst i Q4 2024",
    subtitle = "Sammenligning af forudsagte realvækst i privatforbrug og forbrugertillidsindikatorer fra DI og DST (2000-2024 Q4)",
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

# Endelig df, hvor koefficienterne samles
forudsig_df_2024 <- data.frame(Kvartal = c("Q3 2024", "Q4 2024"),
                               DI_FTI = round(c(new_data_di_Q3$diFTI, new_data_di_Q4$diFTI),2),
                               DST_FTI = round(c(new_data_dst_Q3$dstFTI, new_data_dst_Q4$dstFTI),2),
                               Realvækst_DI = round(c(predictdi_Q3,predictdi_Q4),2),
                               Realvækst_DST = round(c(predictdst_Q3, predictdst_Q4),2))

colnames(forudsig_df_2024) <- c("Tid", "DI Indikator", "DST Indikator", "Realvækst DI", "Realvækst DST")

View(forudsig_df_2024)

sum(dstvsdi_forbrug_forudsig[97:100,4])
