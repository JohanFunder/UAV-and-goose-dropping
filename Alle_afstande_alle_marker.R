#*******Klim_skr*********

#Indlæs covered m2 for marken
m2 <- read.table('G:/P8_projekt/Projekter/Klim_holme_SKR_UTM10m/5m_metadata_areal.csv', header = TRUE, sep = ',', dec ='.')
m2
# indlæs id til m2
id <- read.table('G:/P8_projekt/Projekter/Klim_holme_SKR_UTM10m/5m_udtynding/5m_name.csv', header = TRUE, sep = ',', dec ='.')
m2 <- data.frame(id$Name, m2$covered.m2)
colnames(m2) <- c("Name","m2")
m2

# Indlæs 10m names
id_10m <- read.table('G:/P8_projekt/Projekter/10m_afstande/Klim_SKR/Klim_skr_10m_id.csv', header = TRUE, sep = ',', dec ='.')
str(id_10m)
# Trim m2 efter 10m udtyndingen
m2_10m <- m2[match(id_10m$Name,m2$Name, nomatch=0),]

library('dplyr')
library('plyr')
#m2_10m <- m2 %>% semi_join(id_10m, by = "Name")
#m2_10m <- semi_join(m2, id_10m['Name'])
#m2_10m <- match_df(m2, id_10m, on="Name")

str(m2_10m)
head(m2_10m)
#Tilføj point pr billed 
points <- read.csv('G:/P8_projekt/Projekter/10m_afstande/Klim_SKR_10m_alle_afstande.csv', header = TRUE, sep = ',', dec ='.')
points

# Kombiner
m2_point <- data.frame(m2_10m,points$NUMPOINTS)

quantile(m2_point$m2,c(.05, .50, .95))
min(m2$m2)
max(m2$m2)
m2_point$m2[m2_point$m2 > 5] = 18

# Udregn densitet
m2_point$density <- m2_point$points.NUMPOINTS/m2_point$m2
head(m2_point)



sum(m2_point$points.NUMPOINTS)
sum(m2_point$m2, na.rm = TRUE)

med <-median(m2_point$density)
med
med_CI <- sd(m2_point$density)/sqrt(length(m2_point$density))*1.96
med_CI

lower <- med - med_CI
lower
upper <- med + med_CI
upper

avr <-mean(m2_point$density)
avr
avr_CI <- sd(m2_point$density)/sqrt(length(m2_point$density))*1.96

lower <- avr - avr_CI
lower
upper <- avr + avr_CI
upper

var <- var(m2_point$density)
var
var/avr
# FInd X2
index <- var/avr
index
n <- nrow(m2_point)
n
index * (n-1)

poin

Samlede_afstande_dens <- data.frame(points$distance_vand, points$distance_hegn, points$distance_veje, points$distance_vind,
                                    m2_point$density)
colnames(Samlede_afstande_dens) <- c("Afstande_vand", "Afstande_hegn", "Afstande_veje", "Afstande_vind", "dens")
Samlede_afstande_dens


#Find max dens
Samlede_afstande_dens

max <- max(Samlede_afstande_dens$dens)
max
Samlede_afstande_dens$index <- Samlede_afstande_dens$dens/max


# Tildel 15m afstand bins til hvert element 
binnedSamples_vand <- cut(Samlede_afstande_dens$Afstande_vand,
                          breaks = seq(from = 0, 
                                       to = 25 + max(Samlede_afstande_dens$Afstande_vand),
                                       by = 25))
binnedSamples_hegn <- cut(Samlede_afstande_dens$Afstande_hegn,
                          breaks = seq(from = 0, 
                                       to = 25 + max(Samlede_afstande_dens$Afstande_hegn),
                                       by = 25))
binnedSamples_veje <- cut(Samlede_afstande_dens$Afstande_veje,
                          breaks = seq(from = 0, 
                                       to = 25 + max(Samlede_afstande_dens$Afstande_veje),
                                       by = 25))
binnedSamples_vind <- cut(Samlede_afstande_dens$Afstande_vind,
                          breaks = seq(from = 0, 
                                       to = 200 + max(Samlede_afstande_dens$Afstande_vind),
                                       by = 200))

binnedSamples_vand

Samlede_afstande_dens$bins_vand <- binnedSamples_vand
Samlede_afstande_dens$bins_hegn <- binnedSamples_hegn
Samlede_afstande_dens$bins_veje <- binnedSamples_veje
Samlede_afstande_dens$bins_vind <- binnedSamples_vind


Samlede_afstande_dens_1 <- Samlede_afstande_dens

write.csv(Samlede_afstande_dens_1, file = "G:/P8_projekt/Projekter/Afstande/Samlede_afstande_Klim_SKR.csv")



#*******Klim_fjordholme*********

#Indlæs covered m2 for marken
m2 <- read.table('G:/P8_projekt/Projekter/Klim_fjordholme_UTM10m/metadata_areal.csv', header = TRUE, sep = ',', dec ='.')
m2


#Tilføj point pr billed 
points <- read.table('G:/P8_projekt/Projekter/10m_afstande/Klim_holme_10m_alle_afstande.csv', header = TRUE, sep = ',', dec ='.')
points

# Kombiner med alle afstande
m2_point <- data.frame(m2$covered.m2, points$NUMPOINTS)

# Udregn densitet
m2_point$density <- m2_point$points.NUMPOINTS/m2_point$m2.covered.m2
m2_point

med <-median(m2_point$density)
med
med_CI <- sd(m2_point$density)/sqrt(length(m2_point$density))*1.96
med_CI

lower <- med - med_CI
lower
upper <- med + med_CI
upper

avr <-mean(m2_point$density)
avr
avr_CI <- sd(m2_point$density)/sqrt(length(m2_point$density))*1.96

lower <- avr - avr_CI
lower
upper <- avr + avr_CI
upper

var <- var(m2_point$density)
var
var/avr
# FInd X2
index <- var/avr
n <- nrow(m2_point)
n
index * (n-1)



Samlede_afstande_dens <- data.frame(points$distance_vand, points$distance_hegn, points$distance_veje, points$distance_vind,
                                    m2_point$density)
colnames(Samlede_afstande_dens) <- c("Afstande_vand", "Afstande_hegn", "Afstande_veje", "Afstande_vind", "dens")
Samlede_afstande_dens

#Find max dens

max <- max(Samlede_afstande_dens$dens)
max
Samlede_afstande_dens$index <- Samlede_afstande_dens$dens/max
Samlede_afstande_dens

# Tildel 15m afstand bins til hvert element 
binnedSamples_vand <- cut(Samlede_afstande_dens$Afstande_vand,
                          breaks = seq(from = 0, 
                                       to = 25 + max(Samlede_afstande_dens$Afstande_vand),
                                       by = 25))
binnedSamples_hegn <- cut(Samlede_afstande_dens$Afstande_hegn,
                          breaks = seq(from = 0, 
                                       to = 25 + max(Samlede_afstande_dens$Afstande_hegn),
                                       by = 25))
binnedSamples_veje <- cut(Samlede_afstande_dens$Afstande_veje,
                          breaks = seq(from = 0, 
                                       to = 25 + max(Samlede_afstande_dens$Afstande_veje),
                                       by = 25))

binnedSamples_vind <- cut(Samlede_afstande_dens$Afstande_vind,
                          breaks = seq(from = 0, 
                                       to = 200 + max(Samlede_afstande_dens$Afstande_vind),
                                       by = 200))

binnedSamples_vand

Samlede_afstande_dens$bins_vand <- binnedSamples_vand
Samlede_afstande_dens$bins_hegn <- binnedSamples_hegn
Samlede_afstande_dens$bins_veje <- binnedSamples_veje
Samlede_afstande_dens$bins_vind <- binnedSamples_vind


Samlede_afstande_dens_2 <- Samlede_afstande_dens


write.csv(Samlede_afstande_dens_2, file = "G:/P8_projekt/Projekter/Afstande/Samlede_afstande_klimfjordholme.csv")




#*******Krap_Syd*********

#Indlæs covered m2 for marken
m2 <- read.table('G:/P8_projekt/Projekter/Krap_syd_UTM10m/metadata_areal.csv', header = TRUE, sep = ',', dec ='.')
m2


#Tilføj point pr billed 
points <- read.table('G:/P8_projekt/Projekter/10m_afstande/Krap_syd_10m_alle_afstande.csv', header = TRUE, sep = ',', dec ='.')
points

# Kombiner med alle afstande
m2_point <- data.frame(m2$covered.m2, points$NUMPOINTS)

# Udregn densitet
m2_point$density <- m2_point$points.NUMPOINTS/m2_point$m2.covered.m2
m2_point

med <-median(m2_point$density)
med
med_CI <- sd(m2_point$density)/sqrt(length(m2_point$density))*1.96
med_CI

lower <- med - med_CI
lower
upper <- med + med_CI
upper

avr <-mean(m2_point$density)
avr
avr_CI <- sd(m2_point$density)/sqrt(length(m2_point$density))*1.96

lower <- avr - avr_CI
lower
upper <- avr + avr_CI
upper

var <- var(m2_point$density)
var
var/avr

# FInd X2
index <- var/avr
n <- nrow(m2_point)
n
index * (n-1)


Samlede_afstande_dens <- data.frame(points$distance_vand, points$distance_hegn, points$distance_veje, points$distance_vind,
                                    m2_point$density)
colnames(Samlede_afstande_dens) <- c("Afstande_vand", "Afstande_hegn", "Afstande_veje", "Afstande_vind", "dens")
Samlede_afstande_dens

#Find max dens

max <- max(Samlede_afstande_dens$dens)
max
Samlede_afstande_dens$index <- Samlede_afstande_dens$dens/max
Samlede_afstande_dens

# Tildel 15m afstand bins til hvert element 
binnedSamples_vand <- cut(Samlede_afstande_dens$Afstande_vand,
                          breaks = seq(from = 0, 
                                       to = 25 + max(Samlede_afstande_dens$Afstande_vand),
                                       by = 25))
binnedSamples_hegn <- cut(Samlede_afstande_dens$Afstande_hegn,
                          breaks = seq(from = 0, 
                                       to = 25 + max(Samlede_afstande_dens$Afstande_hegn),
                                       by = 25))
binnedSamples_veje <- cut(Samlede_afstande_dens$Afstande_veje,
                          breaks = seq(from = 0, 
                                       to = 25 + max(Samlede_afstande_dens$Afstande_veje),
                                       by = 25))

binnedSamples_vind <- cut(Samlede_afstande_dens$Afstande_vind,
                          breaks = seq(from = 0, 
                                       to = 200 + max(Samlede_afstande_dens$Afstande_vind),
                                       by = 200))
binnedSamples_vind

Samlede_afstande_dens$bins_vand <- binnedSamples_vand
Samlede_afstande_dens$bins_hegn <- binnedSamples_hegn
Samlede_afstande_dens$bins_veje <- binnedSamples_veje
Samlede_afstande_dens$bins_vind <- binnedSamples_vind


Samlede_afstande_dens_3 <- Samlede_afstande_dens

write.csv(Samlede_afstande_dens_3, file = "G:/P8_projekt/Projekter/Afstande/Samlede_afstande_Krap_syd.csv")


#*******Bag_krap*********

#Indlæs covered m2 for marken
m2 <- read.table('G:/P8_projekt/Projekter/Klim_holme_bag_krap_UTM10m/metadata_areal.csv', header = TRUE, sep = ',', dec ='.')
m2


#Tilføj point pr billed 
points <- read.table('G:/P8_projekt/Projekter/10m_afstande/Bag_krap_10m_alle_afstande.csv', header = TRUE, sep = ',', dec ='.')
points

# Kombiner med alle afstande
m2_point <- data.frame(m2$covered.m2, points$NUMPOINTS)

# Udregn densitet
m2_point$density <- m2_point$points.NUMPOINTS/m2_point$m2.covered.m2
m2_point

med <-median(m2_point$density)
med
med_CI <- sd(m2_point$density)/sqrt(length(m2_point$density))*1.96
med_CI

lower <- med - med_CI
lower
upper <- med + med_CI
upper

avr <-mean(m2_point$density)
avr
avr_CI <- sd(m2_point$density)/sqrt(length(m2_point$density))*1.96

lower <- avr - avr_CI
lower
upper <- avr + avr_CI
upper

var <- var(m2_point$density)
var
var/avr

# FInd X2
index <- var/avr
n <- nrow(m2_point)
n
index * (n-1)

Samlede_afstande_dens <- data.frame(points$distance_vand, points$distance_hegn, points$distance_veje, points$distance_vind,
                                    m2_point$density)
colnames(Samlede_afstande_dens) <- c("Afstande_vand", "Afstande_hegn", "Afstande_veje", "Afstande_vind", "dens")
Samlede_afstande_dens

#Find max dens

max <- max(Samlede_afstande_dens$dens)
max
Samlede_afstande_dens$index <- Samlede_afstande_dens$dens/max
Samlede_afstande_dens

# Tildel 15m afstand bins til hvert element 
binnedSamples_vand <- cut(Samlede_afstande_dens$Afstande_vand,
                          breaks = seq(from = 0, 
                                       to = 25 + max(Samlede_afstande_dens$Afstande_vand),
                                       by = 25))
binnedSamples_hegn <- cut(Samlede_afstande_dens$Afstande_hegn,
                          breaks = seq(from = 0, 
                                       to = 25 + max(Samlede_afstande_dens$Afstande_hegn),
                                       by = 25))
binnedSamples_veje <- cut(Samlede_afstande_dens$Afstande_veje,
                          breaks = seq(from = 0, 
                                       to = 25 + max(Samlede_afstande_dens$Afstande_veje),
                                       by = 25))

binnedSamples_vind <- cut(Samlede_afstande_dens$Afstande_vind,
                          breaks = seq(from = 0, 
                                       to = 200 + max(Samlede_afstande_dens$Afstande_vind),
                                       by = 200))

binnedSamples_vand

Samlede_afstande_dens$bins_vand <- binnedSamples_vand
Samlede_afstande_dens$bins_hegn <- binnedSamples_hegn
Samlede_afstande_dens$bins_veje <- binnedSamples_veje
Samlede_afstande_dens$bins_vind <- binnedSamples_vind


Samlede_afstande_dens_4 <- Samlede_afstande_dens

write.csv(Samlede_afstande_dens_4, file = "G:/P8_projekt/Projekter/Afstande/Samlede_afstande_Bag_krap.csv")


#*******Noerholm_E_N_10m_alle_afstande*********

#Indlæs covered m2 for marken
m2 <- read.table('G:/P8_projekt/Projekter/Noerholm_E_N_UTM10m/metadata_areal.csv', header = TRUE, sep = ',', dec ='.')
m2


#Tilføj point pr billed 
points <- read.table('G:/P8_projekt/Projekter/10m_afstande/Noerholm_E_N_10m_alle_afstande.csv', header = TRUE, sep = ',', dec ='.')
points

# Kombiner med alle afstande
m2_point <- data.frame(m2$covered.m2, points$NUMPOINTS)

# Udregn densitet
m2_point$density <- m2_point$points.NUMPOINTS/m2_point$m2.covered.m2
m2_point

med <-median(m2_point$density)
med
med_CI <- sd(m2_point$density)/sqrt(length(m2_point$density))*1.96
med_CI

lower <- med - med_CI
lower
upper <- med + med_CI
upper

avr <-mean(m2_point$density)
avr
avr_CI <- sd(m2_point$density)/sqrt(length(m2_point$density))*1.96

lower <- avr - avr_CI
lower
upper <- avr + avr_CI
upper

var <- var(m2_point$density)
var
var/avr

# FInd X2
index <- var/avr
n <- nrow(m2_point)
n
index * (n-1)

Samlede_afstande_dens <- data.frame(points$distance_vand, points$distance_hegn, points$distance_veje, points$distance_vind,
                                    m2_point$density)
colnames(Samlede_afstande_dens) <- c("Afstande_vand", "Afstande_hegn", "Afstande_veje", "Afstande_vind", "dens")
Samlede_afstande_dens

#Find max dens

max <- max(Samlede_afstande_dens$dens)
max
Samlede_afstande_dens$index <- Samlede_afstande_dens$dens/max
Samlede_afstande_dens

# Tildel 15m afstand bins til hvert element 
binnedSamples_vand <- cut(Samlede_afstande_dens$Afstande_vand,
                          breaks = seq(from = 0, 
                                       to = 25 + max(Samlede_afstande_dens$Afstande_vand),
                                       by = 25))
binnedSamples_hegn <- cut(Samlede_afstande_dens$Afstande_hegn,
                          breaks = seq(from = 0, 
                                       to = 25 + max(Samlede_afstande_dens$Afstande_hegn),
                                       by = 25))
binnedSamples_veje <- cut(Samlede_afstande_dens$Afstande_veje,
                          breaks = seq(from = 0, 
                                       to = 25 + max(Samlede_afstande_dens$Afstande_veje),
                                       by = 25))

binnedSamples_vind <- cut(Samlede_afstande_dens$Afstande_vind,
                          breaks = seq(from = 0, 
                                       to = 200 + max(Samlede_afstande_dens$Afstande_vind),
                                       by = 200))


Samlede_afstande_dens$bins_vand <- binnedSamples_vand
Samlede_afstande_dens$bins_hegn <- binnedSamples_hegn
Samlede_afstande_dens$bins_veje <- binnedSamples_veje
Samlede_afstande_dens$bins_vind <- binnedSamples_vind


Samlede_afstande_dens_5 <- Samlede_afstande_dens

write.csv(Samlede_afstande_dens_5, file = "G:/P8_projekt/Projekter/Afstande/Samlede_afstande_Noerholm_E_N.csv")




#*******Noerholm_E_S_10m_alle_afstande*********

#Indlæs covered m2 for marken
m2 <- read.table('G:/P8_projekt/Projekter/Noerholm_E_S_UTM10m/metadata_areal.csv', header = TRUE, sep = ',', dec ='.')
m2


#Tilføj point pr billed 
points <- read.table('G:/P8_projekt/Projekter/10m_afstande/Noerholm_E_S_10m_alle_afstande.csv', header = TRUE, sep = ',', dec ='.')
points

# Kombiner med alle afstande
m2_point <- data.frame(m2$covered.m2, points$NUMPOINTS)

# Udregn densitet
m2_point$density <- m2_point$points.NUMPOINTS/m2_point$m2.covered.m2
m2_point

med <-median(m2_point$density)
med
med_CI <- sd(m2_point$density)/sqrt(length(m2_point$density))*1.96
med_CI

lower <- med - med_CI
lower
upper <- med + med_CI
upper

avr <-mean(m2_point$density)
avr
avr_CI <- sd(m2_point$density)/sqrt(length(m2_point$density))*1.96

lower <- avr - avr_CI
lower
upper <- avr + avr_CI
upper

var <- var(m2_point$density)
var
var/avr

# FInd X2
index <- var/avr
n <- nrow(m2_point)
n
index * (n-1)

Samlede_afstande_dens <- data.frame(points$distance_vand, points$distance_hegn, points$distance_veje, points$distance_vind,
                                    m2_point$density)
colnames(Samlede_afstande_dens) <- c("Afstande_vand", "Afstande_hegn", "Afstande_veje", "Afstande_vind", "dens")
Samlede_afstande_dens

#Find max dens

max <- max(Samlede_afstande_dens$dens)
max
Samlede_afstande_dens$index <- Samlede_afstande_dens$dens/max
Samlede_afstande_dens

# Tildel 15m afstand bins til hvert element 
binnedSamples_vand <- cut(Samlede_afstande_dens$Afstande_vand,
                          breaks = seq(from = 0, 
                                       to = 25 + max(Samlede_afstande_dens$Afstande_vand),
                                       by = 25))
binnedSamples_hegn <- cut(Samlede_afstande_dens$Afstande_hegn,
                          breaks = seq(from = 0, 
                                       to = 25 + max(Samlede_afstande_dens$Afstande_hegn),
                                       by = 25))
binnedSamples_veje <- cut(Samlede_afstande_dens$Afstande_veje,
                          breaks = seq(from = 0, 
                                       to = 25 + max(Samlede_afstande_dens$Afstande_veje),
                                       by = 25))

binnedSamples_vind <- cut(Samlede_afstande_dens$Afstande_vind,
                          breaks = seq(from = 0, 
                                       to = 200 + max(Samlede_afstande_dens$Afstande_vind),
                                       by = 200))

binnedSamples_vand

Samlede_afstande_dens$bins_vand <- binnedSamples_vand
Samlede_afstande_dens$bins_hegn <- binnedSamples_hegn
Samlede_afstande_dens$bins_veje <- binnedSamples_veje
Samlede_afstande_dens$bins_vind <- binnedSamples_vind


Samlede_afstande_dens_6 <- Samlede_afstande_dens

write.csv(Samlede_afstande_dens_6, file = "G:/P8_projekt/Projekter/Afstande/Samlede_afstande_Noerholm_E_S.csv")



#*******Noerholm_M_10m_alle_afstande*********

#Indlæs covered m2 for marken
m2 <- read.table('G:/P8_projekt/Projekter/Noerholm_M_UTM10m/metadata_areal.csv', header = TRUE, sep = ',', dec ='.')
m2


#Tilføj point pr billed 
points <- read.table('G:/P8_projekt/Projekter/10m_afstande/Noerholm_M_10m_alle_afstande.csv', header = TRUE, sep = ',', dec ='.')
points

# Kombiner med alle afstande
m2_point <- data.frame(m2$covered.m2, points$NUMPOINTS)

# Udregn densitet
m2_point$density <- m2_point$points.NUMPOINTS/m2_point$m2.covered.m2
m2_point

med <-median(m2_point$density)
med
med_CI <- sd(m2_point$density)/sqrt(length(m2_point$density))*1.96
med_CI

lower <- med - med_CI
lower
upper <- med + med_CI
upper

avr <-mean(m2_point$density)
avr
avr_CI <- sd(m2_point$density)/sqrt(length(m2_point$density))*1.96

lower <- avr - avr_CI
lower
upper <- avr + avr_CI
upper

var <- var(m2_point$density)
var
var/avr

# FInd X2
index <- var/avr
n <- nrow(m2_point)
n
index * (n-1)

Samlede_afstande_dens <- data.frame(points$distance_vand, points$distance_hegn, points$distance_veje, points$distance_vind,
                                    m2_point$density)
colnames(Samlede_afstande_dens) <- c("Afstande_vand", "Afstande_hegn", "Afstande_veje", "Afstande_vind", "dens")
Samlede_afstande_dens

#Find max dens

max <- max(Samlede_afstande_dens$dens)
max
Samlede_afstande_dens$index <- Samlede_afstande_dens$dens/max
Samlede_afstande_dens

# Tildel 15m afstand bins til hvert element 
binnedSamples_vand <- cut(Samlede_afstande_dens$Afstande_vand,
                          breaks = seq(from = 0, 
                                       to = 25 + max(Samlede_afstande_dens$Afstande_vand),
                                       by = 25))
binnedSamples_hegn <- cut(Samlede_afstande_dens$Afstande_hegn,
                          breaks = seq(from = 0, 
                                       to = 25 + max(Samlede_afstande_dens$Afstande_hegn),
                                       by = 25))
binnedSamples_veje <- cut(Samlede_afstande_dens$Afstande_veje,
                          breaks = seq(from = 0, 
                                       to = 25 + max(Samlede_afstande_dens$Afstande_veje),
                                       by = 25))

binnedSamples_vind <- cut(Samlede_afstande_dens$Afstande_vind,
                          breaks = seq(from = 0, 
                                       to = 200 + max(Samlede_afstande_dens$Afstande_vind),
                                       by = 200))

binnedSamples_vand

Samlede_afstande_dens$bins_vand <- binnedSamples_vand
Samlede_afstande_dens$bins_hegn <- binnedSamples_hegn
Samlede_afstande_dens$bins_veje <- binnedSamples_veje
Samlede_afstande_dens$bins_vind <- binnedSamples_vind


Samlede_afstande_dens_7 <- Samlede_afstande_dens

write.csv(Samlede_afstande_dens_7, file = "G:/P8_projekt/Projekter/Afstande/Samlede_afstande_Noerholm_M.csv")




#*******Noerholm_MV_10m_alle_afstande*********

#Indlæs covered m2 for marken
m2 <- read.table('G:/P8_projekt/Projekter/Noerholm_MV_UTM10m/metadata_areal.csv', header = TRUE, sep = ',', dec ='.')
m2


#Tilføj point pr billed 
points <- read.table('G:/P8_projekt/Projekter/10m_afstande/Noerholm_MV_10m_alle_afstande.csv', header = TRUE, sep = ',', dec ='.')
points

# Kombiner med alle afstande
m2_point <- data.frame(m2$covered.m2, points$NUMPOINTS)

# Udregn densitet
m2_point$density <- m2_point$points.NUMPOINTS/m2_point$m2.covered.m2
m2_point

med <-median(m2_point$density)
med
med_CI <- sd(m2_point$density)/sqrt(length(m2_point$density))*1.96
med_CI

lower <- med - med_CI
lower
upper <- med + med_CI
upper

avr <-mean(m2_point$density)
avr
avr_CI <- sd(m2_point$density)/sqrt(length(m2_point$density))*1.96

lower <- avr - avr_CI
lower
upper <- avr + avr_CI
upper

var <- var(m2_point$density)
var
var/avr

# Find X2
index <- var/avr
n <- nrow(m2_point)
n
index * (n-1)

Samlede_afstande_dens <- data.frame(points$distance_vand, points$distance_hegn, points$distance_veje, points$distance_vind,
                                    m2_point$density)
colnames(Samlede_afstande_dens) <- c("Afstande_vand", "Afstande_hegn", "Afstande_veje", "Afstande_vind", "dens")
Samlede_afstande_dens

#Find max dens

max <- max(Samlede_afstande_dens$dens)
max
Samlede_afstande_dens$index <- Samlede_afstande_dens$dens/max
Samlede_afstande_dens

# Tildel 15m afstand bins til hvert element 
binnedSamples_vand <- cut(Samlede_afstande_dens$Afstande_vand,
                          breaks = seq(from = 0, 
                                       to = 25 + max(Samlede_afstande_dens$Afstande_vand),
                                       by = 25))
binnedSamples_hegn <- cut(Samlede_afstande_dens$Afstande_hegn,
                          breaks = seq(from = 0, 
                                       to = 25 + max(Samlede_afstande_dens$Afstande_hegn),
                                       by = 25))
binnedSamples_veje <- cut(Samlede_afstande_dens$Afstande_veje,
                          breaks = seq(from = 0, 
                                       to = 25 + max(Samlede_afstande_dens$Afstande_veje),
                                       by = 25))

binnedSamples_vind <- cut(Samlede_afstande_dens$Afstande_vind,
                          breaks = seq(from = 0, 
                                       to = 200 + max(Samlede_afstande_dens$Afstande_vind),
                                       by = 200))

binnedSamples_vand

Samlede_afstande_dens$bins_vand <- binnedSamples_vand
Samlede_afstande_dens$bins_hegn <- binnedSamples_hegn
Samlede_afstande_dens$bins_veje <- binnedSamples_veje
Samlede_afstande_dens$bins_vind <- binnedSamples_vind


Samlede_afstande_dens_8 <- Samlede_afstande_dens

write.csv(Samlede_afstande_dens_8, file = "G:/P8_projekt/Projekter/Afstande/Samlede_afstande_Noerholm_MV.csv")




#*******Noerholm_V_10m_alle_afstande*********

#Indlæs covered m2 for marken
m2 <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/metadata_areal_5m.csv', header = TRUE, sep = ',', dec ='.')
head(m2)

# indlæs id til m2
id <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/5m_udtynding/5m_name.csv', header = TRUE, sep = ',', dec ='.')
m2 <- data.frame(id$Name, m2$covered.m2)

#View(m2)
colnames(m2) <- c("Name","m2")
str(m2)


# Indlæs 10m names
id_10m <- read.table('G:/P8_projekt/Projekter/10m_afstande/Noerholm_V_10m_alle_afstande.csv', header = TRUE, sep = ',', dec ='.')
id_9m <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/9m_udtynding/Names_9m.csv', header = TRUE, sep = ',', dec ='.')
id_12m <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/12m_udtynding/Names_12m.csv', header = TRUE, sep = ',', dec ='.')
id_6m <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/6m_udtynding/Names_6m.csv', header = TRUE, sep = ',', dec ='.')
id_5m <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/5m_udtynding/5m_name.csv', header = TRUE, sep = ',', dec ='.')
id_20m <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/20m_udtynding/Names_20m.csv', header = TRUE, sep = ',', dec ='.')


m2_10m <- m2[match(m2$Name,id_10m$Name, nomatch=0),]
#m2_10m <- field_2[match(id_12m$Name,field_2$name, nomatch=0),]
field_2
library('dplyr')
#m2_10m <- m2 %>% semi_join(id_10m, by = "Name")

quantile(field_2$m2,c(.05, .50, .95))
plot(field_2$m2)
trim_field_2 <- field_2
trim_field_2$m2[trim_field_2$m2 > 12] = 6.68
trim_field_2$trim_density <- trim_field_2$droppings/trim_field_2$m2
quantile(trim_field_2$m2,c(.05, .50, .95))
mean(field_2$m2)
mean(trim_field_2$trim_density)
mean(trim_field_2$density)
median(trim_field_2$trim_density)

m2_10m
head(trim_field_2)
str(m2_10m)

#Tilføj point pr billed 
points <- read.table('G:/P8_projekt/Projekter/10m_afstande/Noerholm_V_10m_alle_afstande.csv', header = TRUE, sep = ',', dec ='.')
points <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/points.csv', header = TRUE, sep = ',', dec ='.')

str(points)
#View(points)

# kombiner på den rigtige måde mellem m2 og points!!!!!
name <- cbind.data.frame(m2_10m$Name,points$Name)
#View(points)
#View(name)

# Kombiner med alle afstande
m2_point <- data.frame(m2_10m$m2, points$NUMPOINTS, points$Name,m2_10m$Name)
head(m2_point)
median(m2_point$m2_10m.m2)
quantile(m2_point$m2_10m.m2,c(.05, .50, .95))
#m2_point$m2_10m.m2[m2_point$m2_10m.m2 > 10] = 6.68

# Udregn densitet
m2_point$density <- m2_point$points.NUMPOINTS/m2_point$m2_10m.m2
head(m2_point)

sum(m2_point$points.NUMPOINTS)

med <-median(trim_field_2$density)
med
med_CI <- sd(trim_field_2$density)/sqrt(length(trim_field_2$density))*1.96
med_CI

lower <- med - med_CI
lower
upper <- med + med_CI
upper

avr <-mean(trim_field_2$density)
avr
avr_CI <- sd(trim_field_2$density)/sqrt(length(trim_field_2$density))*1.96

lower <- avr - avr_CI
lower
upper <- avr + avr_CI
upper

var <- var(trim_field_2$density)
var
var/avr

# FInd X2
index <- var/avr
index
n <- nrow(m2_point)
n
index * (n-1)


Samlede_afstande_dens <- data.frame(points$distance_vand, points$distance_hegn, points$distance_veje, points$distance_vind,
                                    m2_point$density)
colnames(Samlede_afstande_dens) <- c("Afstande_vand", "Afstande_hegn", "Afstande_veje", "Afstande_vind", "dens")
Samlede_afstande_dens

#Find max dens

max <- max(Samlede_afstande_dens$dens)
max
Samlede_afstande_dens$index <- Samlede_afstande_dens$dens/max
Samlede_afstande_dens

# Tildel 15m afstand bins til hvert element 
binnedSamples_vand <- cut(Samlede_afstande_dens$Afstande_vand,
                          breaks = seq(from = 0, 
                                       to = 25 + max(Samlede_afstande_dens$Afstande_vand),
                                       by = 25))
binnedSamples_hegn <- cut(Samlede_afstande_dens$Afstande_hegn,
                          breaks = seq(from = 0, 
                                       to = 25 + max(Samlede_afstande_dens$Afstande_hegn),
                                       by = 25))
binnedSamples_veje <- cut(Samlede_afstande_dens$Afstande_veje,
                          breaks = seq(from = 0, 
                                       to = 25 + max(Samlede_afstande_dens$Afstande_veje),
                                       by = 25))

binnedSamples_vind <- cut(Samlede_afstande_dens$Afstande_vind,
                          breaks = seq(from = 0, 
                                       to = 200 + max(Samlede_afstande_dens$Afstande_vind),
                                       by = 200))

binnedSamples_vand

Samlede_afstande_dens$bins_vand <- binnedSamples_vand
Samlede_afstande_dens$bins_hegn <- binnedSamples_hegn
Samlede_afstande_dens$bins_veje <- binnedSamples_veje
Samlede_afstande_dens$bins_vind <- binnedSamples_vind


Samlede_afstande_dens_9 <- Samlede_afstande_dens

write.csv(Samlede_afstande_dens_9, file = "G:/P8_projekt/Projekter/Afstande/Samlede_afstande_Noerholm_V.csv")




#*******Pandrup_10m_alle_afstande*********

#Indlæs covered m2 for marken
m2 <- read.table('G:/P8_projekt/Projekter/Pandrup_RYAE_UTM10m/metadata_areal.csv', header = TRUE, sep = ',', dec ='.')
m2


#Tilføj point pr billed 
points <- read.table('G:/P8_projekt/Projekter/10m_afstande/Pandrup_10m_alle_afstande.csv', header = TRUE, sep = ',', dec ='.')
points

# Kombiner med alle afstande
m2_point <- data.frame(m2$covered.m2, points$NUMPOINTS)

# Udregn densitet
m2_point$density <- m2_point$points.NUMPOINTS/m2_point$m2.covered.m2
m2_point

med <-median(m2_point$density)
med
med_CI <- sd(m2_point$density)/sqrt(length(m2_point$density))*1.96
med_CI

lower <- med - med_CI
lower
upper <- med + med_CI
upper

avr <-mean(m2_point$density)
avr
avr_CI <- sd(m2_point$density)/sqrt(length(m2_point$density))*1.96

lower <- avr - avr_CI
lower
upper <- avr + avr_CI
upper

var <- var(m2_point$density)
var
var/avr

# FInd X2
index <- var/avr
n <- nrow(m2_point)
n
index * (n-1)

Samlede_afstande_dens <- data.frame(points$distance_vand, points$distance_hegn, points$distance_veje, points$distance_vind,
                                    m2_point$density)
colnames(Samlede_afstande_dens) <- c("Afstande_vand", "Afstande_hegn", "Afstande_veje", "Afstande_vind", "dens")
Samlede_afstande_dens

#Find max dens

max <- max(Samlede_afstande_dens$dens)
max
Samlede_afstande_dens$index <- Samlede_afstande_dens$dens/max
Samlede_afstande_dens

# Tildel 15m afstand bins til hvert element 
binnedSamples_vand <- cut(Samlede_afstande_dens$Afstande_vand,
                          breaks = seq(from = 0, 
                                       to = 25 + max(Samlede_afstande_dens$Afstande_vand),
                                       by = 25))
binnedSamples_hegn <- cut(Samlede_afstande_dens$Afstande_hegn,
                          breaks = seq(from = 0, 
                                       to = 25 + max(Samlede_afstande_dens$Afstande_hegn),
                                       by = 25))
binnedSamples_veje <- cut(Samlede_afstande_dens$Afstande_veje,
                          breaks = seq(from = 0, 
                                       to = 25 + max(Samlede_afstande_dens$Afstande_veje),
                                       by = 25))

binnedSamples_vind <- cut(Samlede_afstande_dens$Afstande_vind,
                          breaks = seq(from = 0, 
                                       to = 200 + max(Samlede_afstande_dens$Afstande_vind),
                                       by = 200))

binnedSamples_vand

Samlede_afstande_dens$bins_vand <- binnedSamples_vand
Samlede_afstande_dens$bins_hegn <- binnedSamples_hegn
Samlede_afstande_dens$bins_veje <- binnedSamples_veje
Samlede_afstande_dens$bins_vind <- binnedSamples_vind


Samlede_afstande_dens_10 <- Samlede_afstande_dens

write.csv(Samlede_afstande_dens_10, file = "G:/P8_projekt/Projekter/Afstande/Samlede_afstande_Pandrup_rya.csv")




