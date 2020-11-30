#Source 
source(Alle_afstande_alle_marker)


# Indlæs samlede_afstande_dens for alle marker
#samlede_afstande_1 <- read.csv("G:/Projekter/Afstande/Samlede_afstande_Bag_krap.csv")
#samlede_afstande_2 <- read.csv("G:/Projekter/Afstande/Samlede_afstande_Klim_SKR.csv")
#samlede_afstande_3 <- read.csv("G:/Projekter/Afstande/Samlede_afstande_Klimfjordholme.csv")
#samlede_afstande_4 <- read.csv("G:/Projekter/Afstande/Samlede_afstande_Krap_syd.csv")
#samlede_afstande_5 <- read.csv("G:/Projekter/Afstande/Samlede_afstande_Noerholm_E_N.csv")
#samlede_afstande_6 <- read.csv("G:/Projekter/Afstande/Samlede_afstande_Noerholm_E_S.csv")
#samlede_afstande_7 <- read.csv("G:/Projekter/Afstande/Samlede_afstande_Noerholm_M.csv")
#samlede_afstande_8 <- read.csv("G:/Projekter/Afstande/Samlede_afstande_Noerholm_MV.csv")
#samlede_afstande_9 <- read.csv("G:/Projekter/Afstande/Samlede_afstande_Noerholm_V.csv")
#samlede_afstande_10 <- read.csv("G:/Projekter/Afstande/Samlede_afstande_Pandrup_rya.csv")

# Lav en samlede_afstande
samlede_afstande <- rbind.data.frame(Samlede_afstande_dens_1,Samlede_afstande_dens_2,Samlede_afstande_dens_3,
                                     Samlede_afstande_dens_4,Samlede_afstande_dens_5,Samlede_afstande_dens_6,
                                     Samlede_afstande_dens_7,Samlede_afstande_dens_8,Samlede_afstande_dens_9,
                                     Samlede_afstande_dens_10)

str(samlede_afstande)


write.csv(samlede_afstande, file = "G:/Projekter/Samlede_afstande/Samlede_afstande.csv")
# Grupperet og indexerede afstande til vandløb 
# Udregn mean og std for alle bins


m_vand <- rep(NA,max(as.numeric(samlede_afstande$bins_vand), na.rm = TRUE))
avr_vand <- rep(NA,max(as.numeric(samlede_afstande$bins_vand), na.rm = TRUE))
s_vand <- rep(NA,max(as.numeric(samlede_afstande$bins_vand), na.rm = TRUE))
confd_vand <- rep(NA,max(as.numeric(samlede_afstande$bins_vand), na.rm = TRUE))
for(i in 1:max(as.numeric(samlede_afstande$bins_vand), na.rm = TRUE)){
  a <- subset(samlede_afstande, bins_vand == levels(bins_vand)[i])
  m_vand[i] <- sum(a$index)
  avr_vand[i] <- mean(a$index)
  s_vand[i] <- plotrix::std.error(a$index)
  confd_vand[i] <- (plotrix::std.error(a$index))*1.96
}

avr_vand
s_vand
confd_vand

bar <- barplot(avr_vand, ylim = c(0,0.5), main = "Distance to water ditches")
arrows(bar, avr_vand+confd_vand, bar, avr_vand-confd_vand, code = 3, angle = 90, length = 0.05)
axis(1, at = seq(0,36, by = 1), 
     labels = seq(0, to = 15+max(samlede_afstande$Afstande_veje), by = 15))




# Grupperet og indexerede afstande til hegn

m_hegn <- rep(NA,max(as.numeric(samlede_afstande$bins_hegn), na.rm = TRUE))
avr_hegn <- rep(NA,max(as.numeric(samlede_afstande$bins_hegn), na.rm = TRUE))
s_hegn <- rep(NA,max(as.numeric(samlede_afstande$bins_hegn), na.rm = TRUE))
confd_hegn <- rep(NA,max(as.numeric(samlede_afstande$bins_hegn), na.rm = TRUE))
for(i in 1:max(as.numeric(samlede_afstande$bins_hegn), na.rm = TRUE)){
  a <- subset(samlede_afstande, bins_hegn == levels(bins_hegn)[i])
  m_hegn[i] <- sum(a$index)
  avr_hegn[i] <- mean(a$index)
  s_hegn[i] <- plotrix::std.error(a$index)
  confd_hegn[i] <- (plotrix::std.error(a$index))*1.96
}

avr_hegn
s_hegn
confd_hegn

500/15

bar <- barplot(avr_hegn, ylim = c(0,0.4), main = "Distance to hedgerows")
arrows(bar, avr_hegn+confd_hegn, bar, avr_hegn-confd_hegn, code = 3, angle = 90, length = 0.05)
axis(1, at = seq(0,36, by = 1), 
     labels = seq(0, to = 15+max(samlede_afstande$Afstande_veje), by = 15))




# Grupperet og indexerede afstande til veje

m_veje <- rep(NA,max(as.numeric(samlede_afstande$bins_veje), na.rm = TRUE))
avr_veje <- rep(NA,max(as.numeric(samlede_afstande$bins_veje), na.rm = TRUE))
s_veje <- rep(NA,max(as.numeric(samlede_afstande$bins_veje), na.rm = TRUE))
confd_veje <- rep(NA,max(as.numeric(samlede_afstande$bins_veje), na.rm = TRUE))
for(i in 1:max(as.numeric(samlede_afstande$bins_veje), na.rm = TRUE)){
  a <- subset(samlede_afstande, bins_veje == levels(bins_veje)[i])
  m_veje[i] <- sum(a$index)
  avr_veje[i] <- mean(a$index)
  s_veje[i] <- plotrix::std.error(a$index)
  confd_veje[i] <- (plotrix::std.error(a$index))*1.96
}

avr_veje
s_veje
confd_veje

bar <- barplot(avr_veje, ylim = c(0,0.4), main = "Distance to roads", names.arg = levels(samlede_afstande$bins_veje),las=2)
arrows(bar, avr_veje+confd_veje, bar, avr_veje-confd_veje, code = 3, angle = 90, length = 0.05)


bar <- barplot(avr_veje, ylim = c(0,0.4), main = "Distance to roads", space = 0)
arrows(bar, avr_veje+confd_veje, bar, avr_veje-confd_veje, code = 3, angle = 90, length = 0.05)
axis(1, at = seq(0,36, by = 1), 
     labels = seq(0, to = 15+max(samlede_afstande$Afstande_veje), by = 15))


# Grupperet og indexerede afstande til vindmøller

m_vind <- rep(NA,max(as.numeric(samlede_afstande$bins_vind), na.rm = TRUE))
avr_vind <- rep(NA,max(as.numeric(samlede_afstande$bins_vind), na.rm = TRUE))
s_vind <- rep(NA,max(as.numeric(samlede_afstande$bins_vind), na.rm = TRUE))
confd_vind <- rep(NA,max(as.numeric(samlede_afstande$bins_vind), na.rm = TRUE))
for(i in 1:max(as.numeric(samlede_afstande$bins_vind), na.rm = TRUE)){
  a <- subset(samlede_afstande, bins_vind == levels(bins_vind)[i])
  m_vind[i] <- sum(a$index)
  avr_vind[i] <- mean(a$index)
  s_vind[i] <- plotrix::std.error(a$index)
  confd_vind[i] <- (plotrix::std.error(a$index))*1.96
}

avr_vind
s_vind
confd_vind

bar <- barplot(avr_vind, ylim = c(0,0.5), main = "Distance to vindmills", names.arg = levels(samlede_afstande$bins_vind),las=2)
arrows(bar, avr_vind+confd_vind, bar, avr_vind-confd_vind, code = 3, angle = 90, length = 0.05)
# axis(1,at = seq( from = 0, to = 500, by = 15))

#Alle plots samlet
par(mfrow = c(2,2))
par(oma=c(3.8,3,2.8,2))
par(mar=c(2.5,2.8,2.7,2))


bar <- barplot(avr_hegn, ylim = c(0,0.45), main = "Windbreaks", space = 0)
arrows(bar, avr_hegn+confd_hegn, bar, avr_hegn-confd_hegn, code = 3, angle = 90, length = 0.05)
axis(1, at = seq(0,22, by = 1), 
     labels = seq(0, to = 25+max(samlede_afstande$Afstande_hegn), by = 25))


bar <- barplot(avr_veje, ylim = c(0,0.45), main = "Roads", space = 0)
arrows(bar, avr_veje+confd_veje, bar, avr_veje-confd_veje, code = 3, angle = 90, length = 0.05)
axis(1, at = seq(0,22, by = 1), 
     labels = seq(0, to = 25+max(samlede_afstande$Afstande_veje), by = 25))

bar <- barplot(avr_vand, ylim = c(0,0.45), main = "Water ditches", space = 0)
arrows(bar, avr_vand+confd_vand, bar, avr_vand-confd_vand, code = 3, angle = 90, length = 0.05)
axis(1, at = seq(0,9, by = 1), 
     labels = seq(0, to = 25+max(samlede_afstande$Afstande_vand), by = 25))

bar <- barplot(avr_vind, ylim = c(0,0.45), main = "Wind turbines", space = 0)
arrows(bar, avr_vind+confd_vind, bar, avr_vind-confd_vind, code = 3, angle = 90, length = 0.05)
axis(1, at = seq(0,23, by = 1), 
     labels = seq(0, to = 100+max(samlede_afstande$Afstande_vind), by = 100))

mtext("Distance (m)",at=0.52, side=1, outer=T, cex=1.4, padj = 2)
mtext("Dropping density (index)", at=0.5, side=2, outer=T, cex=1.4, padj = 0)
mtext("Avoidance distance to landscape elements", at=0.5, side=3, outer=T, cex=1.8, padj = -0.5)

18*

par(mfrow = c(1,1))
par(oma=c(2,2,2,2))
par(mar=c(2,2,2,2))

