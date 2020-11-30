# Heatmap for indbyrdes afstande ift. vandløb Noerholm_V 
heat_N_V_vand <-read.table('G:/Projekter/Noerholm_V_UTM10m/Heatmap/Heatmap_vandlob.csv', sep = ',', dec = '.', header = TRUE)
heat_N_V_vand

# Find MAX indbyrdes distance
max <- max(heat_N_V_vand$distance)
max

is.na(heat_N_V_vand) <- sapply(heat_N_V_vand, is.infinite)

# tag den omvendte værdi max - indbyrdes distance
heat_N_V_vand$indbyrdes_reciprok <- 3 - heat_N_V_vand$distance

heat_N_V_vand <- heat_N_V_vand[is.finite(rowSums(heat_N_V_vand)),]
heat_N_V_vand

regr <- lm(heat_N_V_vand$indbyrdes_reciprok ~ heat_N_V_vand$distance_vandlob)
regr
summary(regr)
confint(regr)
resi <- residuals(regr)

#Sæt margin
par(mfrow = c(1,1))
par(oma=c(2,2,2,2))
par(mar=c(2,2,2,1))

# plot regression
plot(heat_N_V_vand$distance_vandlob,heat_N_V_vand$indbyrdes_reciprok, ylim = c(0,4),
     main = "Heatmap for distance to water ditch", xlab="Distance from ditch in meters", ylab="Reciprok distance")
abline(regr, col="red", lwd=1)

# Plot residuals
plot(heat_N_V_vand$distance_vandlob,resi, ylim = c(-10,100),
     main = "Heatmap for distance to water ditch", xlab="Distance from ditch in meters", ylab="Reciprok distance")
abline(regr, col="red", lwd=1)


# Heatmap for indbyrdes afstande ift. vandløb Klim_skr
heat_K_SKR_vand <-read.table('G:/Projekter/Klim_holme_SKR_UTM10m/Heatmap/Heatmap_vandlob.csv', sep = ',', dec = '.', header = TRUE)
heat_K_SKR_vand

# Find MAX indbyrdesdistance
max <- max(heat_K_SKR_vand$distance_heatmap)
max

heat_K_SKR_vand$indbyrdes_reciprok <- 2 - heat_K_SKR_vand$distance_heatmap


regr <- lm(heat_K_SKR_vand$indbyrdes_reciprok ~ heat_K_SKR_vand$distance)
regr
summary(regr)

plot(heat_K_SKR_vand$distance,heat_K_SKR_vand$indbyrdes_reciprok, ylim = c(0,3),
     main = "Heatmap for distance to water ditch", xlab="Distance from ditch in meters", ylab="Reciprok distance")

abline(regr, col="red", lwd=1)


# Heatmap for indbyrdes afstande ift. vandløb Klim_skr *****POlygon*****
heat_K_SKR_vand <-read.table('G:/Projekter/Klim_holme_SKR_UTM10m/Heatmap/Heatmap_polygon_vandlob.csv', sep = ',', dec = '.', header = TRUE)
heat_K_SKR_vand

# Regn om til antal lort pr m2 ***giver det samme resultat***
heat_K_SKR_vand$NUMPOINTS_m2 <- heat_K_SKR_vand$NUMPOINTS/2.3^2*pi
regr_buf <- lm(heat_K_SKR_vand$NUMPOINTS ~ heat_K_SKR_vand$distance)
regr_buf
summary(regr)

regr_m2 <- lm(heat_K_SKR_vand$NUMPOINTS_m2 ~ heat_K_SKR_vand$distance)
regr_m2
summary(regr)

#Lort pr buffer billede
plot(heat_K_SKR_vand$distance,heat_K_SKR_vand$NUMPOINTS, ylim = c(0,20),
     main = "Heatmap for distance to water ditch", xlab="Distance from ditch in meters", ylab="Number of droppings")

abline(regr_buf, col="red", lwd=1)

#Lort pr bufferet m2
plot(heat_K_SKR_vand$distance,heat_K_SKR_vand$NUMPOINTS_m2, ylim = c(0,40),
     main = "Heatmap for distance to water ditch", xlab="Distance from ditch in meters", ylab="Number of droppings")

abline(regr_m2, col="red", lwd=1)

1/0.5
1/2


# Heatmap for indbyrdes afstande ift. HEGN Klim_skr
heat_K_SKR_hegn <-read.table('G:/Projekter/Klim_holme_SKR_UTM10m/Heatmap/Heatmap_hegn.csv', sep = ',', dec = '.', header = TRUE)
heat_K_SKR_hegn

heat_K_SKR_hegn$indbyrdes_reciprok <- 1/heat_K_SKR_hegn$Distance_indbyrdes

regr <- lm(heat_K_SKR_hegn$indbyrdes_reciprok ~ heat_K_SKR_hegn$distance)
regr
summary(regr)

plot(heat_K_SKR_hegn$distance,heat_K_SKR_hegn$indbyrdes_reciprok, ylim = c(0,20),
     main = "Heatmap for distance to hedgerow", xlab="Distance from hedgerow in meters", ylab="Reciprok distance")

abline(regr, col="red", lwd=1)



# Heatmap for indbyrdes afstande ift. VEJE Klim_skr
heat_K_SKR_veje <-read.table('G:/Projekter/Klim_holme_SKR_UTM10m/Heatmap/Heatmap_veje.csv', sep = ',', dec = '.', header = TRUE)
heat_K_SKR_veje

heat_K_SKR_veje$indbyrdes_reciprok <- 1/heat_K_SKR_veje$Distance_indbyrdes

regr <- lm(heat_K_SKR_veje$indbyrdes_reciprok ~ heat_K_SKR_veje$distance_vej)
regr
summary(regr)

plot(heat_K_SKR_veje$distance_vej,heat_K_SKR_veje$indbyrdes_reciprok, ylim = c(0,20),
     main = "Heatmap for distance to roads", xlab="Distance from hedgerow in meters", ylab="Reciprok distance")

abline(regr, col="red", lwd=1)

     
     




