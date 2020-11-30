#-------------------------------------------------------------------

# Indlæsninging vandløb_lort og vandløb_billeder Nørholm_V 5m
dir2 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/Afstande_vandlob"
dir3 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/Afstande_vandlob"

Afstande_vandlob_5m <- file.path(dir2,"Afstande_vandlob_5m_lort.csv")
Afstande_vandlob_bil <- file.path(dir3,"Afstande_vandlob_5m_billeder.csv")

Afstande_vandlob_5m <- read.csv(file=Afstande_vandlob_5m, header= TRUE)
Afstande_vandlob_bil <- read.csv(file=Afstande_vandlob_bil, header= TRUE)

# opdel i intervaller til vandløb
binnedSamples_lort <- cut( Afstande_vandlob_5m$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) )
binnedSamples_bil <- cut(Afstande_vandlob_bil$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) )
binnedSamples_lort
Afstande_vandlob_bil$bins <- binnedSamples_bil
Afstande_vandlob_bil$bins
Afstande_vandlob_bil

#indlæs m2
bil_id <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m//metadata_areal_5m.csv', header = TRUE, sep = ',', dec ='.')
head(bil_id)
m2 <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/metadata_areal_5m.csv', header = TRUE, sep = ',', dec ='.')

# Fjern uønskede characterer
library(tidyverse)
bil_id$Name <- str_remove(bil_id$SourceFile, "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/Thin_5m/")

m2
m2 <- data.frame(bil_id$Name, m2$covered.m2)
m2

# Indlæs m2 for R&M udtynding
bil_id_RogM <- read.table('G:/P8_projekt/Projekter/R&M/Prøvemark_2/Billed_position/Billed_position.csv', header = TRUE, sep = ',', dec ='.')

# Trim m2 efter R&M udtyndingen
m2_RogM <- m2[match(m2$bil_id.Name,bil_id_RogM$Name, nomatch=0),]
m2_RogM
sum(m2_RogM$m2.covered.m2)


# Indlæs antal POINTS for 5m udtynding
poly_points <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/5m_udtynding/Points_polygon/Points_polygon1.csv', header = TRUE, sep = ',', dec ='.')
str(poly_points)

polypoints_m2 <- data.frame(m2,poly_points)
head(polypoints_m2)
polypoints_m2 <- data.frame(polypoints_m2$Name,polypoints_m2$m2.covered.m2,polypoints_m2$NUMPOINTS)
polypoints_m2
colnames(polypoints_m2)<-c("name", "m2","droppings")
head(polypoints_m2)
# skriv .csv
write.csv(polypoints_m2, file = "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/Bootstrap/polypoints_m2.csv")


# Trim m2 efter udtyndingen
m2 <- m2[match(m2$bil_id.Name, Afstande_vandlob_bil$Name, nomatch=0),]
m2

af_bin_m2 <- data.frame(Afstande_vandlob_bil,m2$m2.covered.m2)
af_bin_m2

mean(af_bin_m2$m2.m2.covered.m2)

41191/sum(af_bin_m2$m2.m2.covered.m2,rm.na=TRUE)


# Find summen af m2 + mean + stadard error

std.err <- function(x) sd(x)/sqrt(length(x))

a <- subset(af_bin_m2, bins == '(0,5]')
m1 <- sum(a$m2.m2.covered.m2)
avr1 <- mean(a$m2.m2.covered.m2)
s1 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(5,10]')
m2 <- sum(a$m2.m2.covered.m2)
avr2 <- mean(a$m2.m2.covered.m2)
s2 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(10,15]')
m3 <- sum(a$m2.m2.covered.m2)
avr3 <- mean(a$m2.m2.covered.m2)
s3 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(15,20]')
m4 <- sum(a$m2.m2.covered.m2)
avr4 <- mean(a$m2.m2.covered.m2)
s4 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(20,25]')
m5 <- sum(a$m2.m2.covered.m2)
avr5 <- mean(a$m2.m2.covered.m2)
s5 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(25,30]')
m6 <- sum(a$m2.m2.covered.m2)
avr6 <- mean(a$m2.m2.covered.m2)
s6 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(30,35]')
m7 <- sum(a$m2.m2.covered.m2)
avr7 <- mean(a$m2.m2.covered.m2)
s7 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(35,40]')
m8 <- sum(a$m2.m2.covered.m2)
avr8 <- mean(a$m2.m2.covered.m2)
s8 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(40,45]')
m9 <- sum(a$m2.m2.covered.m2)
avr9 <- mean(a$m2.m2.covered.m2)
s9 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(45,50]')
m10 <- sum(a$m2.m2.covered.m2)
avr10 <- mean(a$m2.m2.covered.m2)
s10 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(50,55]')
m11 <- sum(a$m2.m2.covered.m2)
avr11 <- mean(a$m2.m2.covered.m2)
s11 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(55,60]')
m12 <- sum(a$m2.m2.covered.m2)
avr12 <- mean(a$m2.m2.covered.m2)
s12 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(60,65]')
m13 <- sum(a$m2.m2.covered.m2)
avr13 <- mean(a$m2.m2.covered.m2)
s13 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(65,70]')
m14 <- sum(a$m2.m2.covered.m2)
avr14 <- mean(a$m2.m2.covered.m2)
s14 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(70,75]')
m15 <- sum(a$m2.m2.covered.m2)
avr15 <- mean(a$m2.m2.covered.m2)
s15 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(75,80]')
m16 <- sum(a$m2.m2.covered.m2)
avr16 <- mean(a$m2.m2.covered.m2)
s16 <- std.err(a$m2.m2.covered.m2)

s1
s2
s3
# Samlet sum og std
samlet_sum <- cbind(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16)
samlet_avr <- cbind(avr1,avr2,avr3,avr4,avr5,avr6,avr7,avr8,avr9,avr10,avr11,avr12,avr13,avr14,avr15,avr16)
samlet_std <- cbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16)

stdfejl <- samlet_std / samlet_avr

# Lav tables
lort <- table(binnedSamples_lort)
bil <- table(binnedSamples_bil)
lort

df_samlet <- data.frame(samlet_sum)
df_samlet


x <- (lort / df_samlet)
x <- unlist(x)
x

lowerbound <- c(x  - stdfejl * 1.96)
lowerbound
upperbound <- c(x  + stdfejl * 1.96)
upperbound

par(mar=c(8,5,4,4))
bar <- barplot(x, ylab=expression(Droppings ~ m^{-2}),
               main="Dropping density for photo spacing of 5 m",
               names.arg=c("0m-5m","5m-10m","10m-15m","15m-20m",
                           "20m-25m","25m-30m","30m-35m","35m-40m","40m-45m","45m-50m","50m-55m","55m-60m",
                           "60m-65m","65m-70m","70m-75m","75m-80m"),las=2,
               cex.lab = 1.4, cex.main = 1.6)

mtext('Distance to fix point',at=9.3,side=1,outer=F,cex=1.4, padj = 7)  

arrows(bar, x+stdfejl, bar, x-stdfejl, code = 3, angle = 90, length = 0.05)

tynd_5 <- x


#-------------------------------------------------------------------

# Indlæsninging vandløb_lort og vandløb_billeder Nørholm_V 6m
dir2 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/Afstande_vandlob"
dir3 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/Afstande_vandlob"

Afstande_vandlob_5m <- file.path(dir2,"Afstande_vandlob_6m_lort.csv")
Afstande_vandlob_bil <- file.path(dir3,"Afstande_vandlob_6m_billeder.csv")

Afstande_vandlob_5m <- read.csv(file=Afstande_vandlob_5m, header= TRUE)
Afstande_vandlob_bil <- read.csv(file=Afstande_vandlob_bil, header= TRUE)

# opdel i intervaller til vandløb
binnedSamples_lort <- cut( Afstande_vandlob_5m$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) )
binnedSamples_bil <- cut(Afstande_vandlob_bil$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) )

Afstande_vandlob_bil$bins <- binnedSamples_bil
Afstande_vandlob_bil$bins
Afstande_vandlob_bil

#indlæs m2
bil_id <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/Noerholm_V_5m/Noerholm_V_5m.csv', header = TRUE, sep = ',', dec ='.')
bil_id
m2 <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/metadata_areal_5m.csv', header = TRUE, sep = ',', dec ='.')
m2
m2 <- data.frame(bil_id$Name, m2$covered.m2)
m2

# Trim m2 efter udtyndingen
m2 <- m2[match(m2$bil_id.Name, Afstande_vandlob_bil$Name, nomatch=0),]
m2

af_bin_m2 <- data.frame(Afstande_vandlob_bil,m2$m2.covered.m2)
af_bin_m2



# Find summen af m2 + mean + stadard error

std.err <- function(x) sd(x)/sqrt(length(x))

a <- subset(af_bin_m2, bins == '(0,5]')
m1 <- sum(a$m2.m2.covered.m2)
avr1 <- mean(a$m2.m2.covered.m2)
s1 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(5,10]')
m2 <- sum(a$m2.m2.covered.m2)
avr2 <- mean(a$m2.m2.covered.m2)
s2 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(10,15]')
m3 <- sum(a$m2.m2.covered.m2)
avr3 <- mean(a$m2.m2.covered.m2)
s3 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(15,20]')
m4 <- sum(a$m2.m2.covered.m2)
avr4 <- mean(a$m2.m2.covered.m2)
s4 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(20,25]')
m5 <- sum(a$m2.m2.covered.m2)
avr5 <- mean(a$m2.m2.covered.m2)
s5 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(25,30]')
m6 <- sum(a$m2.m2.covered.m2)
avr6 <- mean(a$m2.m2.covered.m2)
s6 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(30,35]')
m7 <- sum(a$m2.m2.covered.m2)
avr7 <- mean(a$m2.m2.covered.m2)
s7 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(35,40]')
m8 <- sum(a$m2.m2.covered.m2)
avr8 <- mean(a$m2.m2.covered.m2)
s8 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(40,45]')
m9 <- sum(a$m2.m2.covered.m2)
avr9 <- mean(a$m2.m2.covered.m2)
s9 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(45,50]')
m10 <- sum(a$m2.m2.covered.m2)
avr10 <- mean(a$m2.m2.covered.m2)
s10 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(50,55]')
m11 <- sum(a$m2.m2.covered.m2)
avr11 <- mean(a$m2.m2.covered.m2)
s11 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(55,60]')
m12 <- sum(a$m2.m2.covered.m2)
avr12 <- mean(a$m2.m2.covered.m2)
s12 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(60,65]')
m13 <- sum(a$m2.m2.covered.m2)
avr13 <- mean(a$m2.m2.covered.m2)
s13 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(65,70]')
m14 <- sum(a$m2.m2.covered.m2)
avr14 <- mean(a$m2.m2.covered.m2)
s14 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(70,75]')
m15 <- sum(a$m2.m2.covered.m2)
avr15 <- mean(a$m2.m2.covered.m2)
s15 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(75,80]')
m16 <- sum(a$m2.m2.covered.m2)
avr16 <- mean(a$m2.m2.covered.m2)
s16 <- std.err(a$m2.m2.covered.m2)

s1
s2
s3
# Samlet sum og std
samlet_sum <- cbind(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16)
samlet_avr <- cbind(avr1,avr2,avr3,avr4,avr5,avr6,avr7,avr8,avr9,avr10,avr11,avr12,avr13,avr14,avr15,avr16)
samlet_std <- cbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16)

stdfejl <- samlet_std / samlet_avr

# Lav tables
lort <- table(binnedSamples_lort)
bil <- table(binnedSamples_bil)
lort

df_samlet <- data.frame(samlet_sum)
df_samlet


x <- (lort / df_samlet)
x <- unlist(x)
x
par(mar=c(8,5,4,4))
bar <- barplot(x, ylab="Droppings per m2",
               main="Dropping density per m2",
               names.arg=c("0m-5m","5m-10m2","10m-15m","15m-20m",
                           "20m-25m","25m-30m","30m-35m","35m-40m","40m-45m","45m-50m","50m-55m","55m-60m",
                           "60m-65m","65m-70m","70m-75m","75m-80m"),las=2,
               cex.lab = 1.4, cex.main = 1.6)

mtext('Distance from ditch',at=9.3,side=1,outer=F,cex=1.4, padj = 7)  

arrows(bar, x+stdfejl, bar, x-stdfejl, code = 3, angle = 90, length = 0.05)

tynd_6 <- x

#-------------------------------------------------------------------

# Indlæsninging vandløb_lort og vandløb_billeder Nørholm_V 7m
dir2 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/Afstande_vandlob"
dir3 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/Afstande_vandlob"

Afstande_vandlob_5m <- file.path(dir2,"Afstande_vandlob_7m_lort.csv")
Afstande_vandlob_bil <- file.path(dir3,"Afstande_vandlob_7m_billeder.csv")

Afstande_vandlob_5m <- read.csv(file=Afstande_vandlob_5m, header= TRUE)
Afstande_vandlob_bil <- read.csv(file=Afstande_vandlob_bil, header= TRUE)

# opdel i intervaller til vandløb
binnedSamples_lort <- cut( Afstande_vandlob_5m$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) )
binnedSamples_bil <- cut(Afstande_vandlob_bil$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) )

Afstande_vandlob_bil$bins <- binnedSamples_bil
Afstande_vandlob_bil$bins
Afstande_vandlob_bil

#indlæs m2
bil_id <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/Noerholm_V_5m/Noerholm_V_5m.csv', header = TRUE, sep = ',', dec ='.')
bil_id
m2 <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/metadata_areal_5m.csv', header = TRUE, sep = ',', dec ='.')
m2
m2 <- data.frame(bil_id$Name, m2$covered.m2)
m2

# Trim m2 efter udtyndingen
m2 <- m2[match(m2$bil_id.Name, Afstande_vandlob_bil$Name, nomatch=0),]
m2

af_bin_m2 <- data.frame(Afstande_vandlob_bil,m2$m2.covered.m2)
af_bin_m2



# Find summen af m2 + mean + stadard error

std.err <- function(x) sd(x)/sqrt(length(x))

a <- subset(af_bin_m2, bins == '(0,5]')
m1 <- sum(a$m2.m2.covered.m2)
avr1 <- mean(a$m2.m2.covered.m2)
s1 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(5,10]')
m2 <- sum(a$m2.m2.covered.m2)
avr2 <- mean(a$m2.m2.covered.m2)
s2 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(10,15]')
m3 <- sum(a$m2.m2.covered.m2)
avr3 <- mean(a$m2.m2.covered.m2)
s3 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(15,20]')
m4 <- sum(a$m2.m2.covered.m2)
avr4 <- mean(a$m2.m2.covered.m2)
s4 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(20,25]')
m5 <- sum(a$m2.m2.covered.m2)
avr5 <- mean(a$m2.m2.covered.m2)
s5 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(25,30]')
m6 <- sum(a$m2.m2.covered.m2)
avr6 <- mean(a$m2.m2.covered.m2)
s6 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(30,35]')
m7 <- sum(a$m2.m2.covered.m2)
avr7 <- mean(a$m2.m2.covered.m2)
s7 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(35,40]')
m8 <- sum(a$m2.m2.covered.m2)
avr8 <- mean(a$m2.m2.covered.m2)
s8 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(40,45]')
m9 <- sum(a$m2.m2.covered.m2)
avr9 <- mean(a$m2.m2.covered.m2)
s9 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(45,50]')
m10 <- sum(a$m2.m2.covered.m2)
avr10 <- mean(a$m2.m2.covered.m2)
s10 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(50,55]')
m11 <- sum(a$m2.m2.covered.m2)
avr11 <- mean(a$m2.m2.covered.m2)
s11 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(55,60]')
m12 <- sum(a$m2.m2.covered.m2)
avr12 <- mean(a$m2.m2.covered.m2)
s12 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(60,65]')
m13 <- sum(a$m2.m2.covered.m2)
avr13 <- mean(a$m2.m2.covered.m2)
s13 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(65,70]')
m14 <- sum(a$m2.m2.covered.m2)
avr14 <- mean(a$m2.m2.covered.m2)
s14 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(70,75]')
m15 <- sum(a$m2.m2.covered.m2)
avr15 <- mean(a$m2.m2.covered.m2)
s15 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(75,80]')
m16 <- sum(a$m2.m2.covered.m2)
avr16 <- mean(a$m2.m2.covered.m2)
s16 <- std.err(a$m2.m2.covered.m2)

s1
s2
s3
# Samlet sum og std
samlet_sum <- cbind(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16)
samlet_avr <- cbind(avr1,avr2,avr3,avr4,avr5,avr6,avr7,avr8,avr9,avr10,avr11,avr12,avr13,avr14,avr15,avr16)
samlet_std <- cbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16)

stdfejl <- samlet_std / samlet_avr

# Lav tables
lort <- table(binnedSamples_lort)
bil <- table(binnedSamples_bil)
lort

df_samlet <- data.frame(samlet_sum)
df_samlet


x <- (lort / df_samlet)
x <- unlist(x)
x
par(mar=c(8,5,4,4))
bar <- barplot(x, ylab="Droppings per m2",
               main="Dropping density per m2",
               names.arg=c("0m-5m","5m-10m2","10m-15m","15m-20m",
                           "20m-25m","25m-30m","30m-35m","35m-40m","40m-45m","45m-50m","50m-55m","55m-60m",
                           "60m-65m","65m-70m","70m-75m","75m-80m"),las=2,
               cex.lab = 1.4, cex.main = 1.6)

mtext('Distance from ditch',at=9.3,side=1,outer=F,cex=1.4, padj = 7)  

arrows(bar, x+stdfejl, bar, x-stdfejl, code = 3, angle = 90, length = 0.05)

tynd_7 <- x

#-------------------------------------------------------------------

# Indlæsninging vandløb_lort og vandløb_billeder Nørholm_V 8m
dir2 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/Afstande_vandlob"
dir3 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/Afstande_vandlob"

Afstande_vandlob_5m <- file.path(dir2,"Afstande_vandlob_8m_lort.csv")
Afstande_vandlob_bil <- file.path(dir3,"Afstande_vandlob_8m_billeder.csv")

Afstande_vandlob_5m <- read.csv(file=Afstande_vandlob_5m, header= TRUE)
Afstande_vandlob_bil <- read.csv(file=Afstande_vandlob_bil, header= TRUE)

# opdel i intervaller til vandløb
binnedSamples_lort <- cut( Afstande_vandlob_5m$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) )
binnedSamples_bil <- cut(Afstande_vandlob_bil$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) )

Afstande_vandlob_bil$bins <- binnedSamples_bil
Afstande_vandlob_bil$bins
Afstande_vandlob_bil

#indlæs m2
bil_id <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/Noerholm_V_5m/Noerholm_V_5m.csv', header = TRUE, sep = ',', dec ='.')
bil_id
m2 <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/metadata_areal_5m.csv', header = TRUE, sep = ',', dec ='.')
m2
m2 <- data.frame(bil_id$Name, m2$covered.m2)
m2

# Trim m2 efter udtyndingen
m2 <- m2[match(m2$bil_id.Name, Afstande_vandlob_bil$Name, nomatch=0),]
m2

af_bin_m2 <- data.frame(Afstande_vandlob_bil,m2$m2.covered.m2)
af_bin_m2



# Find summen af m2 + mean + stadard error

std.err <- function(x) sd(x)/sqrt(length(x))

a <- subset(af_bin_m2, bins == '(0,5]')
m1 <- sum(a$m2.m2.covered.m2)
avr1 <- mean(a$m2.m2.covered.m2)
s1 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(5,10]')
m2 <- sum(a$m2.m2.covered.m2)
avr2 <- mean(a$m2.m2.covered.m2)
s2 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(10,15]')
m3 <- sum(a$m2.m2.covered.m2)
avr3 <- mean(a$m2.m2.covered.m2)
s3 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(15,20]')
m4 <- sum(a$m2.m2.covered.m2)
avr4 <- mean(a$m2.m2.covered.m2)
s4 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(20,25]')
m5 <- sum(a$m2.m2.covered.m2)
avr5 <- mean(a$m2.m2.covered.m2)
s5 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(25,30]')
m6 <- sum(a$m2.m2.covered.m2)
avr6 <- mean(a$m2.m2.covered.m2)
s6 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(30,35]')
m7 <- sum(a$m2.m2.covered.m2)
avr7 <- mean(a$m2.m2.covered.m2)
s7 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(35,40]')
m8 <- sum(a$m2.m2.covered.m2)
avr8 <- mean(a$m2.m2.covered.m2)
s8 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(40,45]')
m9 <- sum(a$m2.m2.covered.m2)
avr9 <- mean(a$m2.m2.covered.m2)
s9 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(45,50]')
m10 <- sum(a$m2.m2.covered.m2)
avr10 <- mean(a$m2.m2.covered.m2)
s10 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(50,55]')
m11 <- sum(a$m2.m2.covered.m2)
avr11 <- mean(a$m2.m2.covered.m2)
s11 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(55,60]')
m12 <- sum(a$m2.m2.covered.m2)
avr12 <- mean(a$m2.m2.covered.m2)
s12 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(60,65]')
m13 <- sum(a$m2.m2.covered.m2)
avr13 <- mean(a$m2.m2.covered.m2)
s13 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(65,70]')
m14 <- sum(a$m2.m2.covered.m2)
avr14 <- mean(a$m2.m2.covered.m2)
s14 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(70,75]')
m15 <- sum(a$m2.m2.covered.m2)
avr15 <- mean(a$m2.m2.covered.m2)
s15 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(75,80]')
m16 <- sum(a$m2.m2.covered.m2)
avr16 <- mean(a$m2.m2.covered.m2)
s16 <- std.err(a$m2.m2.covered.m2)

s1
s2
s3
# Samlet sum og std
samlet_sum <- cbind(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16)
samlet_avr <- cbind(avr1,avr2,avr3,avr4,avr5,avr6,avr7,avr8,avr9,avr10,avr11,avr12,avr13,avr14,avr15,avr16)
samlet_std <- cbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16)

stdfejl <- samlet_std / samlet_avr

# Lav tables
lort <- table(binnedSamples_lort)
bil <- table(binnedSamples_bil)
lort

df_samlet <- data.frame(samlet_sum)
df_samlet


x <- (lort / df_samlet)
x <- unlist(x)
x
par(mar=c(8,5,4,4))
bar <- barplot(x, ylab="Droppings per m2",
               main="Dropping density per m2",
               names.arg=c("0m-5m","5m-10m2","10m-15m","15m-20m",
                           "20m-25m","25m-30m","30m-35m","35m-40m","40m-45m","45m-50m","50m-55m","55m-60m",
                           "60m-65m","65m-70m","70m-75m","75m-80m"),las=2,
               cex.lab = 1.4, cex.main = 1.6)

mtext('Distance from ditch',at=9.3,side=1,outer=F,cex=1.4, padj = 7)  

arrows(bar, x+stdfejl, bar, x-stdfejl, code = 3, angle = 90, length = 0.05)

tynd_8 <- x


#-------------------------------------------------------------------

# Indlæsninging vandløb_lort og vandløb_billeder Nørholm_V 9m
dir2 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/Afstande_vandlob"
dir3 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/Afstande_vandlob"

Afstande_vandlob_5m <- file.path(dir2,"Afstande_vandlob_9m_lort.csv")
Afstande_vandlob_bil <- file.path(dir3,"Afstande_vandlob_9m_billeder.csv")

Afstande_vandlob_5m <- read.csv(file=Afstande_vandlob_5m, header= TRUE)
Afstande_vandlob_bil <- read.csv(file=Afstande_vandlob_bil, header= TRUE)

# opdel i intervaller til vandløb
binnedSamples_lort <- cut( Afstande_vandlob_5m$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) )
binnedSamples_bil <- cut(Afstande_vandlob_bil$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) )

Afstande_vandlob_bil$bins <- binnedSamples_bil
Afstande_vandlob_bil$bins
Afstande_vandlob_bil

#indlæs m2
bil_id <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/Noerholm_V_5m/Noerholm_V_5m.csv', header = TRUE, sep = ',', dec ='.')
bil_id
m2 <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/metadata_areal_5m.csv', header = TRUE, sep = ',', dec ='.')
m2
m2 <- data.frame(bil_id$Name, m2$covered.m2)
m2

# Trim m2 efter udtyndingen
m2 <- m2[match(m2$bil_id.Name, Afstande_vandlob_bil$Name, nomatch=0),]
m2

af_bin_m2 <- data.frame(Afstande_vandlob_bil,m2$m2.covered.m2)
af_bin_m2



# Find summen af m2 + mean + stadard error

std.err <- function(x) sd(x)/sqrt(length(x))

a <- subset(af_bin_m2, bins == '(0,5]')
m1 <- sum(a$m2.m2.covered.m2)
avr1 <- mean(a$m2.m2.covered.m2)
s1 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(5,10]')
m2 <- sum(a$m2.m2.covered.m2)
avr2 <- mean(a$m2.m2.covered.m2)
s2 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(10,15]')
m3 <- sum(a$m2.m2.covered.m2)
avr3 <- mean(a$m2.m2.covered.m2)
s3 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(15,20]')
m4 <- sum(a$m2.m2.covered.m2)
avr4 <- mean(a$m2.m2.covered.m2)
s4 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(20,25]')
m5 <- sum(a$m2.m2.covered.m2)
avr5 <- mean(a$m2.m2.covered.m2)
s5 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(25,30]')
m6 <- sum(a$m2.m2.covered.m2)
avr6 <- mean(a$m2.m2.covered.m2)
s6 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(30,35]')
m7 <- sum(a$m2.m2.covered.m2)
avr7 <- mean(a$m2.m2.covered.m2)
s7 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(35,40]')
m8 <- sum(a$m2.m2.covered.m2)
avr8 <- mean(a$m2.m2.covered.m2)
s8 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(40,45]')
m9 <- sum(a$m2.m2.covered.m2)
avr9 <- mean(a$m2.m2.covered.m2)
s9 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(45,50]')
m10 <- sum(a$m2.m2.covered.m2)
avr10 <- mean(a$m2.m2.covered.m2)
s10 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(50,55]')
m11 <- sum(a$m2.m2.covered.m2)
avr11 <- mean(a$m2.m2.covered.m2)
s11 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(55,60]')
m12 <- sum(a$m2.m2.covered.m2)
avr12 <- mean(a$m2.m2.covered.m2)
s12 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(60,65]')
m13 <- sum(a$m2.m2.covered.m2)
avr13 <- mean(a$m2.m2.covered.m2)
s13 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(65,70]')
m14 <- sum(a$m2.m2.covered.m2)
avr14 <- mean(a$m2.m2.covered.m2)
s14 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(70,75]')
m15 <- sum(a$m2.m2.covered.m2)
avr15 <- mean(a$m2.m2.covered.m2)
s15 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(75,80]')
m16 <- sum(a$m2.m2.covered.m2)
avr16 <- mean(a$m2.m2.covered.m2)
s16 <- std.err(a$m2.m2.covered.m2)

s1
s2
s3
# Samlet sum og std
samlet_sum <- cbind(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16)
samlet_avr <- cbind(avr1,avr2,avr3,avr4,avr5,avr6,avr7,avr8,avr9,avr10,avr11,avr12,avr13,avr14,avr15,avr16)
samlet_std <- cbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16)

stdfejl <- samlet_std / samlet_avr

# Lav tables
lort <- table(binnedSamples_lort)
bil <- table(binnedSamples_bil)
lort

df_samlet <- data.frame(samlet_sum)
df_samlet


x <- (lort / df_samlet)
x <- unlist(x)
x
par(mar=c(8,5,4,4))
bar <- barplot(x, ylab="Droppings per m2",
               main="Dropping density per m2",
               names.arg=c("0m-5m","5m-10m2","10m-15m","15m-20m",
                           "20m-25m","25m-30m","30m-35m","35m-40m","40m-45m","45m-50m","50m-55m","55m-60m",
                           "60m-65m","65m-70m","70m-75m","75m-80m"),las=2,
               cex.lab = 1.4, cex.main = 1.6)

mtext('Distance from ditch',at=9.3,side=1,outer=F,cex=1.4, padj = 7)  

arrows(bar, x+stdfejl, bar, x-stdfejl, code = 3, angle = 90, length = 0.05)

tynd_9 <- x

#-------------------------------------------------------------------

# Indlæsninging vandløb_lort og vandløb_billeder Nørholm_V 10m
dir2 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/Afstande_vandlob"
dir3 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/Afstande_vandlob"

Afstande_vandlob_5m <- file.path(dir2,"Afstande_vandlob_10m_lort.csv")
Afstande_vandlob_bil <- file.path(dir3,"Afstande_vandlob_10m_billeder.csv")

Afstande_vandlob_5m <- read.csv(file=Afstande_vandlob_5m, header= TRUE)
Afstande_vandlob_bil <- read.csv(file=Afstande_vandlob_bil, header= TRUE)

# opdel i intervaller til vandløb
binnedSamples_lort <- cut( Afstande_vandlob_5m$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) )
binnedSamples_bil <- cut(Afstande_vandlob_bil$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) )

Afstande_vandlob_bil$bins <- binnedSamples_bil
Afstande_vandlob_bil$bins
Afstande_vandlob_bil

#indlæs m2
bil_id <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/Noerholm_V_5m/Noerholm_V_5m.csv', header = TRUE, sep = ',', dec ='.')
bil_id
m2 <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/metadata_areal_5m.csv', header = TRUE, sep = ',', dec ='.')
m2
m2 <- data.frame(bil_id$Name, m2$covered.m2)
m2

# Trim m2 efter udtyndingen
m2 <- m2[match(m2$bil_id.Name, Afstande_vandlob_bil$Name, nomatch=0),]
m2

af_bin_m2 <- data.frame(Afstande_vandlob_bil,m2$m2.covered.m2)
af_bin_m2



# Find summen af m2 + mean + stadard error

std.err <- function(x) sd(x)/sqrt(length(x))

a <- subset(af_bin_m2, bins == '(0,5]')
m1 <- sum(a$m2.m2.covered.m2)
avr1 <- mean(a$m2.m2.covered.m2)
s1 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(5,10]')
m2 <- sum(a$m2.m2.covered.m2)
avr2 <- mean(a$m2.m2.covered.m2)
s2 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(10,15]')
m3 <- sum(a$m2.m2.covered.m2)
avr3 <- mean(a$m2.m2.covered.m2)
s3 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(15,20]')
m4 <- sum(a$m2.m2.covered.m2)
avr4 <- mean(a$m2.m2.covered.m2)
s4 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(20,25]')
m5 <- sum(a$m2.m2.covered.m2)
avr5 <- mean(a$m2.m2.covered.m2)
s5 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(25,30]')
m6 <- sum(a$m2.m2.covered.m2)
avr6 <- mean(a$m2.m2.covered.m2)
s6 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(30,35]')
m7 <- sum(a$m2.m2.covered.m2)
avr7 <- mean(a$m2.m2.covered.m2)
s7 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(35,40]')
m8 <- sum(a$m2.m2.covered.m2)
avr8 <- mean(a$m2.m2.covered.m2)
s8 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(40,45]')
m9 <- sum(a$m2.m2.covered.m2)
avr9 <- mean(a$m2.m2.covered.m2)
s9 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(45,50]')
m10 <- sum(a$m2.m2.covered.m2)
avr10 <- mean(a$m2.m2.covered.m2)
s10 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(50,55]')
m11 <- sum(a$m2.m2.covered.m2)
avr11 <- mean(a$m2.m2.covered.m2)
s11 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(55,60]')
m12 <- sum(a$m2.m2.covered.m2)
avr12 <- mean(a$m2.m2.covered.m2)
s12 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(60,65]')
m13 <- sum(a$m2.m2.covered.m2)
avr13 <- mean(a$m2.m2.covered.m2)
s13 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(65,70]')
m14 <- sum(a$m2.m2.covered.m2)
avr14 <- mean(a$m2.m2.covered.m2)
s14 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(70,75]')
m15 <- sum(a$m2.m2.covered.m2)
avr15 <- mean(a$m2.m2.covered.m2)
s15 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(75,80]')
m16 <- sum(a$m2.m2.covered.m2)
avr16 <- mean(a$m2.m2.covered.m2)
s16 <- std.err(a$m2.m2.covered.m2)

s1
s2
s3
# Samlet sum og std
samlet_sum <- cbind(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16)
samlet_avr <- cbind(avr1,avr2,avr3,avr4,avr5,avr6,avr7,avr8,avr9,avr10,avr11,avr12,avr13,avr14,avr15,avr16)
samlet_std <- cbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16)

stdfejl <- samlet_std / samlet_avr

# Lav tables
lort <- table(binnedSamples_lort)
bil <- table(binnedSamples_bil)
lort

df_samlet <- data.frame(samlet_sum)
df_samlet


x <- (lort / df_samlet)
x <- unlist(x)
x
lowerbound <- c(x  - stdfejl * 1.96)
lowerbound
upperbound <- c(x  + stdfejl * 1.96)
upperbound

par(mar=c(8,5,4,4))
bar <- barplot(x, ylab=expression(Droppings ~ m^{-2}),
               main="Dropping density for photo spacing of 10 m",
               names.arg=c("0m-5m","5m-10m","10m-15m","15m-20m",
                           "20m-25m","25m-30m","30m-35m","35m-40m","40m-45m","45m-50m","50m-55m","55m-60m",
                           "60m-65m","65m-70m","70m-75m","75m-80m"),las=2,
               cex.lab = 1.4, cex.main = 1.6)

mtext('Distance to fix point',at=9.3,side=1,outer=F,cex=1.4, padj = 7)  

arrows(bar, x+stdfejl, bar, x-stdfejl, code = 3, angle = 90, length = 0.05)

tynd_10 <- x


#-------------------------------------------------------------------

# Indlæsninging vandløb_lort og vandløb_billeder Nørholm_V 11m
dir2 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/Afstande_vandlob"
dir3 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/Afstande_vandlob"

Afstande_vandlob_5m <- file.path(dir2,"Afstande_vandlob_11m_lort.csv")
Afstande_vandlob_bil <- file.path(dir3,"Afstande_vandlob_11m_billeder.csv")

Afstande_vandlob_5m <- read.csv(file=Afstande_vandlob_5m, header= TRUE)
Afstande_vandlob_bil <- read.csv(file=Afstande_vandlob_bil, header= TRUE)

# opdel i intervaller til vandløb
binnedSamples_lort <- cut( Afstande_vandlob_5m$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) )
binnedSamples_bil <- cut(Afstande_vandlob_bil$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) )

Afstande_vandlob_bil$bins <- binnedSamples_bil
Afstande_vandlob_bil$bins
Afstande_vandlob_bil

#indlæs m2
bil_id <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/Noerholm_V_5m/Noerholm_V_5m.csv', header = TRUE, sep = ',', dec ='.')
bil_id
m2 <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/metadata_areal_5m.csv', header = TRUE, sep = ',', dec ='.')
m2
m2 <- data.frame(bil_id$Name, m2$covered.m2)
m2

# Trim m2 efter udtyndingen
m2 <- m2[match(m2$bil_id.Name, Afstande_vandlob_bil$Name, nomatch=0),]
m2

af_bin_m2 <- data.frame(Afstande_vandlob_bil,m2$m2.covered.m2)
af_bin_m2



# Find summen af m2 + mean + stadard error

std.err <- function(x) sd(x)/sqrt(length(x))

a <- subset(af_bin_m2, bins == '(0,5]')
m1 <- sum(a$m2.m2.covered.m2)
avr1 <- mean(a$m2.m2.covered.m2)
s1 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(5,10]')
m2 <- sum(a$m2.m2.covered.m2)
avr2 <- mean(a$m2.m2.covered.m2)
s2 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(10,15]')
m3 <- sum(a$m2.m2.covered.m2)
avr3 <- mean(a$m2.m2.covered.m2)
s3 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(15,20]')
m4 <- sum(a$m2.m2.covered.m2)
avr4 <- mean(a$m2.m2.covered.m2)
s4 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(20,25]')
m5 <- sum(a$m2.m2.covered.m2)
avr5 <- mean(a$m2.m2.covered.m2)
s5 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(25,30]')
m6 <- sum(a$m2.m2.covered.m2)
avr6 <- mean(a$m2.m2.covered.m2)
s6 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(30,35]')
m7 <- sum(a$m2.m2.covered.m2)
avr7 <- mean(a$m2.m2.covered.m2)
s7 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(35,40]')
m8 <- sum(a$m2.m2.covered.m2)
avr8 <- mean(a$m2.m2.covered.m2)
s8 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(40,45]')
m9 <- sum(a$m2.m2.covered.m2)
avr9 <- mean(a$m2.m2.covered.m2)
s9 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(45,50]')
m10 <- sum(a$m2.m2.covered.m2)
avr10 <- mean(a$m2.m2.covered.m2)
s10 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(50,55]')
m11 <- sum(a$m2.m2.covered.m2)
avr11 <- mean(a$m2.m2.covered.m2)
s11 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(55,60]')
m12 <- sum(a$m2.m2.covered.m2)
avr12 <- mean(a$m2.m2.covered.m2)
s12 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(60,65]')
m13 <- sum(a$m2.m2.covered.m2)
avr13 <- mean(a$m2.m2.covered.m2)
s13 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(65,70]')
m14 <- sum(a$m2.m2.covered.m2)
avr14 <- mean(a$m2.m2.covered.m2)
s14 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(70,75]')
m15 <- sum(a$m2.m2.covered.m2)
avr15 <- mean(a$m2.m2.covered.m2)
s15 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(75,80]')
m16 <- sum(a$m2.m2.covered.m2)
avr16 <- mean(a$m2.m2.covered.m2)
s16 <- std.err(a$m2.m2.covered.m2)

s1
s2
s3
# Samlet sum og std
samlet_sum <- cbind(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16)
samlet_avr <- cbind(avr1,avr2,avr3,avr4,avr5,avr6,avr7,avr8,avr9,avr10,avr11,avr12,avr13,avr14,avr15,avr16)
samlet_std <- cbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16)

stdfejl <- samlet_std / samlet_avr

# Lav tables
lort <- table(binnedSamples_lort)
bil <- table(binnedSamples_bil)
lort

df_samlet <- data.frame(samlet_sum)
df_samlet


x <- (lort / df_samlet)
x <- unlist(x)
x
par(mar=c(8,5,4,4))
bar <- barplot(x, ylab="Droppings per m2",
               main="Dropping density per m2",
               names.arg=c("0m-5m","5m-10m2","10m-15m","15m-20m",
                           "20m-25m","25m-30m","30m-35m","35m-40m","40m-45m","45m-50m","50m-55m","55m-60m",
                           "60m-65m","65m-70m","70m-75m","75m-80m"),las=2,
               cex.lab = 1.4, cex.main = 1.6)

mtext('Distance from ditch',at=9.3,side=1,outer=F,cex=1.4, padj = 7)  

arrows(bar, x+stdfejl, bar, x-stdfejl, code = 3, angle = 90, length = 0.05)

tynd_11 <- x

#-------------------------------------------------------------------

# Indlæsninging vandløb_lort og vandløb_billeder Nørholm_V 12m
dir2 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/Afstande_vandlob"
dir3 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/Afstande_vandlob"

Afstande_vandlob_5m <- file.path(dir2,"Afstande_vandlob_12m_lort.csv")
Afstande_vandlob_bil <- file.path(dir3,"Afstande_vandlob_12m_billeder.csv")

Afstande_vandlob_5m <- read.csv(file=Afstande_vandlob_5m, header= TRUE)
Afstande_vandlob_bil <- read.csv(file=Afstande_vandlob_bil, header= TRUE)

# opdel i intervaller til vandløb
binnedSamples_lort <- cut( Afstande_vandlob_5m$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) )
binnedSamples_bil <- cut(Afstande_vandlob_bil$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) )

Afstande_vandlob_bil$bins <- binnedSamples_bil
Afstande_vandlob_bil$bins
Afstande_vandlob_bil

#indlæs m2
bil_id <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/Noerholm_V_5m/Noerholm_V_5m.csv', header = TRUE, sep = ',', dec ='.')
bil_id
m2 <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/metadata_areal_5m.csv', header = TRUE, sep = ',', dec ='.')
m2
m2 <- data.frame(bil_id$Name, m2$covered.m2)
m2

# Trim m2 efter udtyndingen
m2 <- m2[match(m2$bil_id.Name, Afstande_vandlob_bil$Name, nomatch=0),]
m2

af_bin_m2 <- data.frame(Afstande_vandlob_bil,m2$m2.covered.m2)
af_bin_m2



# Find summen af m2 + mean + stadard error

std.err <- function(x) sd(x)/sqrt(length(x))

a <- subset(af_bin_m2, bins == '(0,5]')
m1 <- sum(a$m2.m2.covered.m2)
avr1 <- mean(a$m2.m2.covered.m2)
s1 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(5,10]')
m2 <- sum(a$m2.m2.covered.m2)
avr2 <- mean(a$m2.m2.covered.m2)
s2 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(10,15]')
m3 <- sum(a$m2.m2.covered.m2)
avr3 <- mean(a$m2.m2.covered.m2)
s3 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(15,20]')
m4 <- sum(a$m2.m2.covered.m2)
avr4 <- mean(a$m2.m2.covered.m2)
s4 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(20,25]')
m5 <- sum(a$m2.m2.covered.m2)
avr5 <- mean(a$m2.m2.covered.m2)
s5 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(25,30]')
m6 <- sum(a$m2.m2.covered.m2)
avr6 <- mean(a$m2.m2.covered.m2)
s6 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(30,35]')
m7 <- sum(a$m2.m2.covered.m2)
avr7 <- mean(a$m2.m2.covered.m2)
s7 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(35,40]')
m8 <- sum(a$m2.m2.covered.m2)
avr8 <- mean(a$m2.m2.covered.m2)
s8 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(40,45]')
m9 <- sum(a$m2.m2.covered.m2)
avr9 <- mean(a$m2.m2.covered.m2)
s9 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(45,50]')
m10 <- sum(a$m2.m2.covered.m2)
avr10 <- mean(a$m2.m2.covered.m2)
s10 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(50,55]')
m11 <- sum(a$m2.m2.covered.m2)
avr11 <- mean(a$m2.m2.covered.m2)
s11 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(55,60]')
m12 <- sum(a$m2.m2.covered.m2)
avr12 <- mean(a$m2.m2.covered.m2)
s12 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(60,65]')
m13 <- sum(a$m2.m2.covered.m2)
avr13 <- mean(a$m2.m2.covered.m2)
s13 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(65,70]')
m14 <- sum(a$m2.m2.covered.m2)
avr14 <- mean(a$m2.m2.covered.m2)
s14 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(70,75]')
m15 <- sum(a$m2.m2.covered.m2)
avr15 <- mean(a$m2.m2.covered.m2)
s15 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(75,80]')
m16 <- sum(a$m2.m2.covered.m2)
avr16 <- mean(a$m2.m2.covered.m2)
s16 <- std.err(a$m2.m2.covered.m2)

s1
s2
s3
# Samlet sum og std
samlet_sum <- cbind(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16)
samlet_avr <- cbind(avr1,avr2,avr3,avr4,avr5,avr6,avr7,avr8,avr9,avr10,avr11,avr12,avr13,avr14,avr15,avr16)
samlet_std <- cbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16)

stdfejl <- samlet_std / samlet_avr

# Lav tables
lort <- table(binnedSamples_lort)
bil <- table(binnedSamples_bil)
lort

df_samlet <- data.frame(samlet_sum)
df_samlet


x <- (lort / df_samlet)
x <- unlist(x)
x
par(mar=c(8,5,4,4))
bar <- barplot(x, ylab="Droppings per m2",
               main="Dropping density per m2",
               names.arg=c("0m-5m","5m-10m2","10m-15m","15m-20m",
                           "20m-25m","25m-30m","30m-35m","35m-40m","40m-45m","45m-50m","50m-55m","55m-60m",
                           "60m-65m","65m-70m","70m-75m","75m-80m"),las=2,
               cex.lab = 1.4, cex.main = 1.6)

mtext('Distance from ditch',at=9.3,side=1,outer=F,cex=1.4, padj = 7)  

arrows(bar, x+stdfejl, bar, x-stdfejl, code = 3, angle = 90, length = 0.05)

tynd_12 <- x

#-------------------------------------------------------------------

# Indlæsninging vandløb_lort og vandløb_billeder Nørholm_V 13m
dir2 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/Afstande_vandlob"
dir3 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/Afstande_vandlob"

Afstande_vandlob_5m <- file.path(dir2,"Afstande_vandlob_13m_lort.csv")
Afstande_vandlob_bil <- file.path(dir3,"Afstande_vandlob_13m_billeder.csv")

Afstande_vandlob_5m <- read.csv(file=Afstande_vandlob_5m, header= TRUE)
Afstande_vandlob_bil <- read.csv(file=Afstande_vandlob_bil, header= TRUE)

# opdel i intervaller til vandløb
binnedSamples_lort <- cut( Afstande_vandlob_5m$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) )
binnedSamples_bil <- cut(Afstande_vandlob_bil$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) )

Afstande_vandlob_bil$bins <- binnedSamples_bil
Afstande_vandlob_bil$bins
Afstande_vandlob_bil

#indlæs m2
bil_id <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/Noerholm_V_5m/Noerholm_V_5m.csv', header = TRUE, sep = ',', dec ='.')
bil_id
m2 <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/metadata_areal_5m.csv', header = TRUE, sep = ',', dec ='.')
m2
m2 <- data.frame(bil_id$Name, m2$covered.m2)
m2

# Trim m2 efter udtyndingen
m2 <- m2[match(m2$bil_id.Name, Afstande_vandlob_bil$Name, nomatch=0),]
m2

af_bin_m2 <- data.frame(Afstande_vandlob_bil,m2$m2.covered.m2)
af_bin_m2



# Find summen af m2 + mean + stadard error

std.err <- function(x) sd(x)/sqrt(length(x))

a <- subset(af_bin_m2, bins == '(0,5]')
m1 <- sum(a$m2.m2.covered.m2)
avr1 <- mean(a$m2.m2.covered.m2)
s1 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(5,10]')
m2 <- sum(a$m2.m2.covered.m2)
avr2 <- mean(a$m2.m2.covered.m2)
s2 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(10,15]')
m3 <- sum(a$m2.m2.covered.m2)
avr3 <- mean(a$m2.m2.covered.m2)
s3 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(15,20]')
m4 <- sum(a$m2.m2.covered.m2)
avr4 <- mean(a$m2.m2.covered.m2)
s4 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(20,25]')
m5 <- sum(a$m2.m2.covered.m2)
avr5 <- mean(a$m2.m2.covered.m2)
s5 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(25,30]')
m6 <- sum(a$m2.m2.covered.m2)
avr6 <- mean(a$m2.m2.covered.m2)
s6 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(30,35]')
m7 <- sum(a$m2.m2.covered.m2)
avr7 <- mean(a$m2.m2.covered.m2)
s7 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(35,40]')
m8 <- sum(a$m2.m2.covered.m2)
avr8 <- mean(a$m2.m2.covered.m2)
s8 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(40,45]')
m9 <- sum(a$m2.m2.covered.m2)
avr9 <- mean(a$m2.m2.covered.m2)
s9 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(45,50]')
m10 <- sum(a$m2.m2.covered.m2)
avr10 <- mean(a$m2.m2.covered.m2)
s10 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(50,55]')
m11 <- sum(a$m2.m2.covered.m2)
avr11 <- mean(a$m2.m2.covered.m2)
s11 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(55,60]')
m12 <- sum(a$m2.m2.covered.m2)
avr12 <- mean(a$m2.m2.covered.m2)
s12 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(60,65]')
m13 <- sum(a$m2.m2.covered.m2)
avr13 <- mean(a$m2.m2.covered.m2)
s13 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(65,70]')
m14 <- sum(a$m2.m2.covered.m2)
avr14 <- mean(a$m2.m2.covered.m2)
s14 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(70,75]')
m15 <- sum(a$m2.m2.covered.m2)
avr15 <- mean(a$m2.m2.covered.m2)
s15 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(75,80]')
m16 <- sum(a$m2.m2.covered.m2)
avr16 <- mean(a$m2.m2.covered.m2)
s16 <- std.err(a$m2.m2.covered.m2)

s1
s2
s3
# Samlet sum og std
samlet_sum <- cbind(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16)
samlet_avr <- cbind(avr1,avr2,avr3,avr4,avr5,avr6,avr7,avr8,avr9,avr10,avr11,avr12,avr13,avr14,avr15,avr16)
samlet_std <- cbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16)

stdfejl <- samlet_std / samlet_avr

# Lav tables
lort <- table(binnedSamples_lort)
bil <- table(binnedSamples_bil)
lort

df_samlet <- data.frame(samlet_sum)
df_samlet


x <- (lort / df_samlet)
x <- unlist(x)
x
par(mar=c(8,5,4,4))
bar <- barplot(x, ylab="Droppings per m2",
               main="Dropping density per m2",
               names.arg=c("0m-5m","5m-10m2","10m-15m","15m-20m",
                           "20m-25m","25m-30m","30m-35m","35m-40m","40m-45m","45m-50m","50m-55m","55m-60m",
                           "60m-65m","65m-70m","70m-75m","75m-80m"),las=2,
               cex.lab = 1.4, cex.main = 1.6)

mtext('Distance from ditch',at=9.3,side=1,outer=F,cex=1.4, padj = 7)  

arrows(bar, x+stdfejl, bar, x-stdfejl, code = 3, angle = 90, length = 0.05)

tynd_13 <- x

#-------------------------------------------------------------------

# Indlæsninging vandløb_lort og vandløb_billeder Nørholm_V 14m
dir2 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/Afstande_vandlob"
dir3 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/Afstande_vandlob"

Afstande_vandlob_5m <- file.path(dir2,"Afstande_vandlob_14m_lort.csv")
Afstande_vandlob_bil <- file.path(dir3,"Afstande_vandlob_14m_billeder.csv")

Afstande_vandlob_5m <- read.csv(file=Afstande_vandlob_5m, header= TRUE)
Afstande_vandlob_bil <- read.csv(file=Afstande_vandlob_bil, header= TRUE)

# opdel i intervaller til vandløb
binnedSamples_lort <- cut( Afstande_vandlob_5m$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) )
binnedSamples_bil <- cut(Afstande_vandlob_bil$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) )

Afstande_vandlob_bil$bins <- binnedSamples_bil
Afstande_vandlob_bil$bins
Afstande_vandlob_bil

#indlæs m2
bil_id <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/Noerholm_V_5m/Noerholm_V_5m.csv', header = TRUE, sep = ',', dec ='.')
bil_id
m2 <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/metadata_areal_5m.csv', header = TRUE, sep = ',', dec ='.')
m2
m2 <- data.frame(bil_id$Name, m2$covered.m2)
m2

# Trim m2 efter udtyndingen
m2 <- m2[match(m2$bil_id.Name, Afstande_vandlob_bil$Name, nomatch=0),]
m2

af_bin_m2 <- data.frame(Afstande_vandlob_bil,m2$m2.covered.m2)
af_bin_m2



# Find summen af m2 + mean + stadard error

std.err <- function(x) sd(x)/sqrt(length(x))

a <- subset(af_bin_m2, bins == '(0,5]')
m1 <- sum(a$m2.m2.covered.m2)
avr1 <- mean(a$m2.m2.covered.m2)
s1 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(5,10]')
m2 <- sum(a$m2.m2.covered.m2)
avr2 <- mean(a$m2.m2.covered.m2)
s2 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(10,15]')
m3 <- sum(a$m2.m2.covered.m2)
avr3 <- mean(a$m2.m2.covered.m2)
s3 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(15,20]')
m4 <- sum(a$m2.m2.covered.m2)
avr4 <- mean(a$m2.m2.covered.m2)
s4 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(20,25]')
m5 <- sum(a$m2.m2.covered.m2)
avr5 <- mean(a$m2.m2.covered.m2)
s5 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(25,30]')
m6 <- sum(a$m2.m2.covered.m2)
avr6 <- mean(a$m2.m2.covered.m2)
s6 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(30,35]')
m7 <- sum(a$m2.m2.covered.m2)
avr7 <- mean(a$m2.m2.covered.m2)
s7 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(35,40]')
m8 <- sum(a$m2.m2.covered.m2)
avr8 <- mean(a$m2.m2.covered.m2)
s8 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(40,45]')
m9 <- sum(a$m2.m2.covered.m2)
avr9 <- mean(a$m2.m2.covered.m2)
s9 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(45,50]')
m10 <- sum(a$m2.m2.covered.m2)
avr10 <- mean(a$m2.m2.covered.m2)
s10 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(50,55]')
m11 <- sum(a$m2.m2.covered.m2)
avr11 <- mean(a$m2.m2.covered.m2)
s11 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(55,60]')
m12 <- sum(a$m2.m2.covered.m2)
avr12 <- mean(a$m2.m2.covered.m2)
s12 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(60,65]')
m13 <- sum(a$m2.m2.covered.m2)
avr13 <- mean(a$m2.m2.covered.m2)
s13 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(65,70]')
m14 <- sum(a$m2.m2.covered.m2)
avr14 <- mean(a$m2.m2.covered.m2)
s14 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(70,75]')
m15 <- sum(a$m2.m2.covered.m2)
avr15 <- mean(a$m2.m2.covered.m2)
s15 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(75,80]')
m16 <- sum(a$m2.m2.covered.m2)
avr16 <- mean(a$m2.m2.covered.m2)
s16 <- std.err(a$m2.m2.covered.m2)

s1
s2
s3
# Samlet sum og std
samlet_sum <- cbind(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16)
samlet_avr <- cbind(avr1,avr2,avr3,avr4,avr5,avr6,avr7,avr8,avr9,avr10,avr11,avr12,avr13,avr14,avr15,avr16)
samlet_std <- cbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16)

stdfejl <- samlet_std / samlet_avr

# Lav tables
lort <- table(binnedSamples_lort)
bil <- table(binnedSamples_bil)
lort

df_samlet <- data.frame(samlet_sum)
df_samlet


x <- (lort / df_samlet)
x <- unlist(x)
x
par(mar=c(8,5,4,4))
bar <- barplot(x, ylab="Droppings per m2",
               main="Dropping density per m2",
               names.arg=c("0m-5m","5m-10m2","10m-15m","15m-20m",
                           "20m-25m","25m-30m","30m-35m","35m-40m","40m-45m","45m-50m","50m-55m","55m-60m",
                           "60m-65m","65m-70m","70m-75m","75m-80m"),las=2,
               cex.lab = 1.4, cex.main = 1.6)

mtext('Distance from ditch',at=9.3,side=1,outer=F,cex=1.4, padj = 7)  

arrows(bar, x+stdfejl, bar, x-stdfejl, code = 3, angle = 90, length = 0.05)

tynd_14 <- x

#-------------------------------------------------------------------

# Indlæsninging vandløb_lort og vandløb_billeder Nørholm_V 15m
dir2 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/Afstande_vandlob"
dir3 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/Afstande_vandlob"

Afstande_vandlob_5m <- file.path(dir2,"Afstande_vandlob_15m_lort.csv")
Afstande_vandlob_bil <- file.path(dir3,"Afstande_vandlob_15m_billeder.csv")

Afstande_vandlob_5m <- read.csv(file=Afstande_vandlob_5m, header= TRUE)
Afstande_vandlob_bil <- read.csv(file=Afstande_vandlob_bil, header= TRUE)

# opdel i intervaller til vandløb
binnedSamples_lort <- cut( Afstande_vandlob_5m$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) )
binnedSamples_bil <- cut(Afstande_vandlob_bil$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) )

Afstande_vandlob_bil$bins <- binnedSamples_bil
Afstande_vandlob_bil$bins
Afstande_vandlob_bil

#indlæs m2
bil_id <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/Noerholm_V_5m/Noerholm_V_5m.csv', header = TRUE, sep = ',', dec ='.')
bil_id
m2 <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/metadata_areal_5m.csv', header = TRUE, sep = ',', dec ='.')
m2
m2 <- data.frame(bil_id$Name, m2$covered.m2)
m2

# Trim m2 efter udtyndingen
m2 <- m2[match(m2$bil_id.Name, Afstande_vandlob_bil$Name, nomatch=0),]
m2

af_bin_m2 <- data.frame(Afstande_vandlob_bil,m2$m2.covered.m2)
af_bin_m2



# Find summen af m2 + mean + stadard error

std.err <- function(x) sd(x)/sqrt(length(x))

a <- subset(af_bin_m2, bins == '(0,5]')
m1 <- sum(a$m2.m2.covered.m2)
avr1 <- mean(a$m2.m2.covered.m2)
s1 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(5,10]')
m2 <- sum(a$m2.m2.covered.m2)
avr2 <- mean(a$m2.m2.covered.m2)
s2 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(10,15]')
m3 <- sum(a$m2.m2.covered.m2)
avr3 <- mean(a$m2.m2.covered.m2)
s3 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(15,20]')
m4 <- sum(a$m2.m2.covered.m2)
avr4 <- mean(a$m2.m2.covered.m2)
s4 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(20,25]')
m5 <- sum(a$m2.m2.covered.m2)
avr5 <- mean(a$m2.m2.covered.m2)
s5 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(25,30]')
m6 <- sum(a$m2.m2.covered.m2)
avr6 <- mean(a$m2.m2.covered.m2)
s6 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(30,35]')
m7 <- sum(a$m2.m2.covered.m2)
avr7 <- mean(a$m2.m2.covered.m2)
s7 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(35,40]')
m8 <- sum(a$m2.m2.covered.m2)
avr8 <- mean(a$m2.m2.covered.m2)
s8 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(40,45]')
m9 <- sum(a$m2.m2.covered.m2)
avr9 <- mean(a$m2.m2.covered.m2)
s9 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(45,50]')
m10 <- sum(a$m2.m2.covered.m2)
avr10 <- mean(a$m2.m2.covered.m2)
s10 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(50,55]')
m11 <- sum(a$m2.m2.covered.m2)
avr11 <- mean(a$m2.m2.covered.m2)
s11 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(55,60]')
m12 <- sum(a$m2.m2.covered.m2)
avr12 <- mean(a$m2.m2.covered.m2)
s12 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(60,65]')
m13 <- sum(a$m2.m2.covered.m2)
avr13 <- mean(a$m2.m2.covered.m2)
s13 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(65,70]')
m14 <- sum(a$m2.m2.covered.m2)
avr14 <- mean(a$m2.m2.covered.m2)
s14 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(70,75]')
m15 <- sum(a$m2.m2.covered.m2)
avr15 <- mean(a$m2.m2.covered.m2)
s15 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(75,80]')
m16 <- sum(a$m2.m2.covered.m2)
avr16 <- mean(a$m2.m2.covered.m2)
s16 <- std.err(a$m2.m2.covered.m2)

s1
s2
s3
# Samlet sum og std
samlet_sum <- cbind(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16)
samlet_avr <- cbind(avr1,avr2,avr3,avr4,avr5,avr6,avr7,avr8,avr9,avr10,avr11,avr12,avr13,avr14,avr15,avr16)
samlet_std <- cbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16)

stdfejl <- samlet_std / samlet_avr

# Lav tables
lort <- table(binnedSamples_lort)
bil <- table(binnedSamples_bil)
lort

df_samlet <- data.frame(samlet_sum)
df_samlet


x <- (lort / df_samlet)
x <- unlist(x)
x
is.na(x) <- sapply(x, is.infinite)

par(mar=c(8,5,4,4))
bar <- barplot(x, ylab="Droppings per m2",
               main="Dropping density per m2",
               names.arg=c("0m-5m","5m-10m2","10m-15m","15m-20m",
                           "20m-25m","25m-30m","30m-35m","35m-40m","40m-45m","45m-50m","50m-55m","55m-60m",
                           "60m-65m","65m-70m","70m-75m","75m-80m"),las=2,
               cex.lab = 1.4, cex.main = 1.6)

mtext('Distance from ditch',at=9.3,side=1,outer=F,cex=1.4, padj = 7)  

arrows(bar, x+stdfejl, bar, x-stdfejl, code = 3, angle = 90, length = 0.05)

tynd_15 <- x

#-------------------------------------------------------------------

# Indlæsninging vandløb_lort og vandløb_billeder Nørholm_V 16m
dir2 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/Afstande_vandlob"
dir3 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/Afstande_vandlob"

Afstande_vandlob_5m <- file.path(dir2,"Afstande_vandlob_16m_lort.csv")
Afstande_vandlob_bil <- file.path(dir3,"Afstande_vandlob_16m_billeder.csv")

Afstande_vandlob_5m <- read.csv(file=Afstande_vandlob_5m, header= TRUE)
Afstande_vandlob_bil <- read.csv(file=Afstande_vandlob_bil, header= TRUE)

# opdel i intervaller til vandløb
binnedSamples_lort <- cut( Afstande_vandlob_5m$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) )
binnedSamples_bil <- cut(Afstande_vandlob_bil$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) )

Afstande_vandlob_bil$bins <- binnedSamples_bil
Afstande_vandlob_bil$bins
Afstande_vandlob_bil

#indlæs m2
bil_id <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/Noerholm_V_5m/Noerholm_V_5m.csv', header = TRUE, sep = ',', dec ='.')
bil_id
m2 <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/metadata_areal_5m.csv', header = TRUE, sep = ',', dec ='.')
m2
m2 <- data.frame(bil_id$Name, m2$covered.m2)
m2

# Trim m2 efter udtyndingen
m2 <- m2[match(m2$bil_id.Name, Afstande_vandlob_bil$Name, nomatch=0),]
m2

af_bin_m2 <- data.frame(Afstande_vandlob_bil,m2$m2.covered.m2)
af_bin_m2



# Find summen af m2 + mean + stadard error

std.err <- function(x) sd(x)/sqrt(length(x))

a <- subset(af_bin_m2, bins == '(0,5]')
m1 <- sum(a$m2.m2.covered.m2)
avr1 <- mean(a$m2.m2.covered.m2)
s1 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(5,10]')
m2 <- sum(a$m2.m2.covered.m2)
avr2 <- mean(a$m2.m2.covered.m2)
s2 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(10,15]')
m3 <- sum(a$m2.m2.covered.m2)
avr3 <- mean(a$m2.m2.covered.m2)
s3 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(15,20]')
m4 <- sum(a$m2.m2.covered.m2)
avr4 <- mean(a$m2.m2.covered.m2)
s4 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(20,25]')
m5 <- sum(a$m2.m2.covered.m2)
avr5 <- mean(a$m2.m2.covered.m2)
s5 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(25,30]')
m6 <- sum(a$m2.m2.covered.m2)
avr6 <- mean(a$m2.m2.covered.m2)
s6 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(30,35]')
m7 <- sum(a$m2.m2.covered.m2)
avr7 <- mean(a$m2.m2.covered.m2)
s7 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(35,40]')
m8 <- sum(a$m2.m2.covered.m2)
avr8 <- mean(a$m2.m2.covered.m2)
s8 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(40,45]')
m9 <- sum(a$m2.m2.covered.m2)
avr9 <- mean(a$m2.m2.covered.m2)
s9 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(45,50]')
m10 <- sum(a$m2.m2.covered.m2)
avr10 <- mean(a$m2.m2.covered.m2)
s10 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(50,55]')
m11 <- sum(a$m2.m2.covered.m2)
avr11 <- mean(a$m2.m2.covered.m2)
s11 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(55,60]')
m12 <- sum(a$m2.m2.covered.m2)
avr12 <- mean(a$m2.m2.covered.m2)
s12 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(60,65]')
m13 <- sum(a$m2.m2.covered.m2)
avr13 <- mean(a$m2.m2.covered.m2)
s13 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(65,70]')
m14 <- sum(a$m2.m2.covered.m2)
avr14 <- mean(a$m2.m2.covered.m2)
s14 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(70,75]')
m15 <- sum(a$m2.m2.covered.m2)
avr15 <- mean(a$m2.m2.covered.m2)
s15 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(75,80]')
m16 <- sum(a$m2.m2.covered.m2)
avr16 <- mean(a$m2.m2.covered.m2)
s16 <- std.err(a$m2.m2.covered.m2)

s1
s2
s3
# Samlet sum og std
samlet_sum <- cbind(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16)
samlet_avr <- cbind(avr1,avr2,avr3,avr4,avr5,avr6,avr7,avr8,avr9,avr10,avr11,avr12,avr13,avr14,avr15,avr16)
samlet_std <- cbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16)

stdfejl <- samlet_std / samlet_avr

# Lav tables
lort <- table(binnedSamples_lort)
bil <- table(binnedSamples_bil)
lort

df_samlet <- data.frame(samlet_sum)
df_samlet


x <- (lort / df_samlet)
x <- unlist(x)
x
is.na(x) <- sapply(x, is.infinite)

par(mar=c(8,5,4,4))
bar <- barplot(x, ylab="Droppings per m2",
               main="Dropping density per m2",
               names.arg=c("0m-5m","5m-10m2","10m-15m","15m-20m",
                           "20m-25m","25m-30m","30m-35m","35m-40m","40m-45m","45m-50m","50m-55m","55m-60m",
                           "60m-65m","65m-70m","70m-75m","75m-80m"),las=2,
               cex.lab = 1.4, cex.main = 1.6)

mtext('Distance from ditch',at=9.3,side=1,outer=F,cex=1.4, padj = 7)  

arrows(bar, x+stdfejl, bar, x-stdfejl, code = 3, angle = 90, length = 0.05)

tynd_16 <- x

#-------------------------------------------------------------------

# Indlæsninging vandløb_lort og vandløb_billeder Nørholm_V 17m
dir2 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/Afstande_vandlob"
dir3 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/Afstande_vandlob"

Afstande_vandlob_5m <- file.path(dir2,"Afstande_vandlob_17m_lort.csv")
Afstande_vandlob_bil <- file.path(dir3,"Afstande_vandlob_17m_billeder.csv")

Afstande_vandlob_5m <- read.csv(file=Afstande_vandlob_5m, header= TRUE)
Afstande_vandlob_bil <- read.csv(file=Afstande_vandlob_bil, header= TRUE)

# opdel i intervaller til vandløb
binnedSamples_lort <- cut( Afstande_vandlob_5m$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) )
binnedSamples_bil <- cut(Afstande_vandlob_bil$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) )

Afstande_vandlob_bil$bins <- binnedSamples_bil
Afstande_vandlob_bil$bins
Afstande_vandlob_bil

#indlæs m2
bil_id <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/Noerholm_V_5m/Noerholm_V_5m.csv', header = TRUE, sep = ',', dec ='.')
bil_id
m2 <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/metadata_areal_5m.csv', header = TRUE, sep = ',', dec ='.')
m2
m2 <- data.frame(bil_id$Name, m2$covered.m2)
m2

# Trim m2 efter udtyndingen
m2 <- m2[match(m2$bil_id.Name, Afstande_vandlob_bil$Name, nomatch=0),]
m2

af_bin_m2 <- data.frame(Afstande_vandlob_bil,m2$m2.covered.m2)
af_bin_m2



# Find summen af m2 + mean + stadard error

std.err <- function(x) sd(x)/sqrt(length(x))

a <- subset(af_bin_m2, bins == '(0,5]')
m1 <- sum(a$m2.m2.covered.m2)
avr1 <- mean(a$m2.m2.covered.m2)
s1 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(5,10]')
m2 <- sum(a$m2.m2.covered.m2)
avr2 <- mean(a$m2.m2.covered.m2)
s2 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(10,15]')
m3 <- sum(a$m2.m2.covered.m2)
avr3 <- mean(a$m2.m2.covered.m2)
s3 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(15,20]')
m4 <- sum(a$m2.m2.covered.m2)
avr4 <- mean(a$m2.m2.covered.m2)
s4 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(20,25]')
m5 <- sum(a$m2.m2.covered.m2)
avr5 <- mean(a$m2.m2.covered.m2)
s5 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(25,30]')
m6 <- sum(a$m2.m2.covered.m2)
avr6 <- mean(a$m2.m2.covered.m2)
s6 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(30,35]')
m7 <- sum(a$m2.m2.covered.m2)
avr7 <- mean(a$m2.m2.covered.m2)
s7 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(35,40]')
m8 <- sum(a$m2.m2.covered.m2)
avr8 <- mean(a$m2.m2.covered.m2)
s8 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(40,45]')
m9 <- sum(a$m2.m2.covered.m2)
avr9 <- mean(a$m2.m2.covered.m2)
s9 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(45,50]')
m10 <- sum(a$m2.m2.covered.m2)
avr10 <- mean(a$m2.m2.covered.m2)
s10 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(50,55]')
m11 <- sum(a$m2.m2.covered.m2)
avr11 <- mean(a$m2.m2.covered.m2)
s11 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(55,60]')
m12 <- sum(a$m2.m2.covered.m2)
avr12 <- mean(a$m2.m2.covered.m2)
s12 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(60,65]')
m13 <- sum(a$m2.m2.covered.m2)
avr13 <- mean(a$m2.m2.covered.m2)
s13 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(65,70]')
m14 <- sum(a$m2.m2.covered.m2)
avr14 <- mean(a$m2.m2.covered.m2)
s14 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(70,75]')
m15 <- sum(a$m2.m2.covered.m2)
avr15 <- mean(a$m2.m2.covered.m2)
s15 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(75,80]')
m16 <- sum(a$m2.m2.covered.m2)
avr16 <- mean(a$m2.m2.covered.m2)
s16 <- std.err(a$m2.m2.covered.m2)

s1
s2
s3
# Samlet sum og std
samlet_sum <- cbind(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16)
samlet_avr <- cbind(avr1,avr2,avr3,avr4,avr5,avr6,avr7,avr8,avr9,avr10,avr11,avr12,avr13,avr14,avr15,avr16)
samlet_std <- cbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16)

stdfejl <- samlet_std / samlet_avr

# Lav tables
lort <- table(binnedSamples_lort)
bil <- table(binnedSamples_bil)
lort

df_samlet <- data.frame(samlet_sum)
df_samlet


x <- (lort / df_samlet)
x <- unlist(x)
x
is.na(x) <- sapply(x, is.infinite)

par(mar=c(8,5,4,4))
bar <- barplot(x, ylab="Droppings per m2",
               main="Dropping density per m2",
               names.arg=c("0m-5m","5m-10m2","10m-15m","15m-20m",
                           "20m-25m","25m-30m","30m-35m","35m-40m","40m-45m","45m-50m","50m-55m","55m-60m",
                           "60m-65m","65m-70m","70m-75m","75m-80m"),las=2,
               cex.lab = 1.4, cex.main = 1.6)

mtext('Distance from ditch',at=9.3,side=1,outer=F,cex=1.4, padj = 7)  

arrows(bar, x+stdfejl, bar, x-stdfejl, code = 3, angle = 90, length = 0.05)

tynd_17 <- x

#-------------------------------------------------------------------

# Indlæsninging vandløb_lort og vandløb_billeder Nørholm_V 18m
dir2 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/Afstande_vandlob"
dir3 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/Afstande_vandlob"

Afstande_vandlob_5m <- file.path(dir2,"Afstande_vandlob_18m_lort.csv")
Afstande_vandlob_bil <- file.path(dir3,"Afstande_vandlob_18m_billeder.csv")

Afstande_vandlob_5m <- read.csv(file=Afstande_vandlob_5m, header= TRUE)
Afstande_vandlob_bil <- read.csv(file=Afstande_vandlob_bil, header= TRUE)

# opdel i intervaller til vandløb
binnedSamples_lort <- cut( Afstande_vandlob_5m$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) )
binnedSamples_bil <- cut(Afstande_vandlob_bil$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) )

Afstande_vandlob_bil$bins <- binnedSamples_bil
Afstande_vandlob_bil$bins
Afstande_vandlob_bil

#indlæs m2
bil_id <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/Noerholm_V_5m/Noerholm_V_5m.csv', header = TRUE, sep = ',', dec ='.')
bil_id
m2 <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/metadata_areal_5m.csv', header = TRUE, sep = ',', dec ='.')
m2
m2 <- data.frame(bil_id$Name, m2$covered.m2)
m2

# Trim m2 efter udtyndingen
m2 <- m2[match(m2$bil_id.Name, Afstande_vandlob_bil$Name, nomatch=0),]
m2

af_bin_m2 <- data.frame(Afstande_vandlob_bil,m2$m2.covered.m2)
af_bin_m2



# Find summen af m2 + mean + stadard error

std.err <- function(x) sd(x)/sqrt(length(x))

a <- subset(af_bin_m2, bins == '(0,5]')
m1 <- sum(a$m2.m2.covered.m2)
avr1 <- mean(a$m2.m2.covered.m2)
s1 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(5,10]')
m2 <- sum(a$m2.m2.covered.m2)
avr2 <- mean(a$m2.m2.covered.m2)
s2 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(10,15]')
m3 <- sum(a$m2.m2.covered.m2)
avr3 <- mean(a$m2.m2.covered.m2)
s3 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(15,20]')
m4 <- sum(a$m2.m2.covered.m2)
avr4 <- mean(a$m2.m2.covered.m2)
s4 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(20,25]')
m5 <- sum(a$m2.m2.covered.m2)
avr5 <- mean(a$m2.m2.covered.m2)
s5 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(25,30]')
m6 <- sum(a$m2.m2.covered.m2)
avr6 <- mean(a$m2.m2.covered.m2)
s6 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(30,35]')
m7 <- sum(a$m2.m2.covered.m2)
avr7 <- mean(a$m2.m2.covered.m2)
s7 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(35,40]')
m8 <- sum(a$m2.m2.covered.m2)
avr8 <- mean(a$m2.m2.covered.m2)
s8 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(40,45]')
m9 <- sum(a$m2.m2.covered.m2)
avr9 <- mean(a$m2.m2.covered.m2)
s9 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(45,50]')
m10 <- sum(a$m2.m2.covered.m2)
avr10 <- mean(a$m2.m2.covered.m2)
s10 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(50,55]')
m11 <- sum(a$m2.m2.covered.m2)
avr11 <- mean(a$m2.m2.covered.m2)
s11 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(55,60]')
m12 <- sum(a$m2.m2.covered.m2)
avr12 <- mean(a$m2.m2.covered.m2)
s12 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(60,65]')
m13 <- sum(a$m2.m2.covered.m2)
avr13 <- mean(a$m2.m2.covered.m2)
s13 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(65,70]')
m14 <- sum(a$m2.m2.covered.m2)
avr14 <- mean(a$m2.m2.covered.m2)
s14 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(70,75]')
m15 <- sum(a$m2.m2.covered.m2)
avr15 <- mean(a$m2.m2.covered.m2)
s15 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(75,80]')
m16 <- sum(a$m2.m2.covered.m2)
avr16 <- mean(a$m2.m2.covered.m2)
s16 <- std.err(a$m2.m2.covered.m2)

s1
s2
s3
# Samlet sum og std
samlet_sum <- cbind(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16)
samlet_avr <- cbind(avr1,avr2,avr3,avr4,avr5,avr6,avr7,avr8,avr9,avr10,avr11,avr12,avr13,avr14,avr15,avr16)
samlet_std <- cbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16)

stdfejl <- samlet_std / samlet_avr

# Lav tables
lort <- table(binnedSamples_lort)
bil <- table(binnedSamples_bil)
lort

df_samlet <- data.frame(samlet_sum)
df_samlet


x <- (lort / df_samlet)
x <- unlist(x)
x
is.na(x) <- sapply(x, is.infinite)

par(mar=c(8,5,4,4))
bar <- barplot(x, ylab="Droppings per m2",
               main="Dropping density per m2",
               names.arg=c("0m-5m","5m-10m2","10m-15m","15m-20m",
                           "20m-25m","25m-30m","30m-35m","35m-40m","40m-45m","45m-50m","50m-55m","55m-60m",
                           "60m-65m","65m-70m","70m-75m","75m-80m"),las=2,
               cex.lab = 1.4, cex.main = 1.6)

mtext('Distance from ditch',at=9.3,side=1,outer=F,cex=1.4, padj = 7)  

arrows(bar, x+stdfejl, bar, x-stdfejl, code = 3, angle = 90, length = 0.05)

tynd_18 <- x


#-------------------------------------------------------------------

# Indlæsninging vandløb_lort og vandløb_billeder Nørholm_V 19m
dir2 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/Afstande_vandlob"
dir3 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/Afstande_vandlob"

Afstande_vandlob_5m <- file.path(dir2,"Afstande_vandlob_19m_lort.csv")
Afstande_vandlob_bil <- file.path(dir3,"Afstande_vandlob_19m_billeder.csv")

Afstande_vandlob_5m <- read.csv(file=Afstande_vandlob_5m, header= TRUE)
Afstande_vandlob_bil <- read.csv(file=Afstande_vandlob_bil, header= TRUE)

# opdel i intervaller til vandløb
binnedSamples_lort <- cut( Afstande_vandlob_5m$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) )
binnedSamples_bil <- cut(Afstande_vandlob_bil$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) )

Afstande_vandlob_bil$bins <- binnedSamples_bil
Afstande_vandlob_bil$bins
Afstande_vandlob_bil

#indlæs m2
bil_id <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/Noerholm_V_5m/Noerholm_V_5m.csv', header = TRUE, sep = ',', dec ='.')
bil_id
m2 <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/metadata_areal_5m.csv', header = TRUE, sep = ',', dec ='.')
m2
m2 <- data.frame(bil_id$Name, m2$covered.m2)
m2

# Trim m2 efter udtyndingen
m2 <- m2[match(m2$bil_id.Name, Afstande_vandlob_bil$Name, nomatch=0),]
m2

af_bin_m2 <- data.frame(Afstande_vandlob_bil,m2$m2.covered.m2)
af_bin_m2



# Find summen af m2 + mean + stadard error

std.err <- function(x) sd(x)/sqrt(length(x))

a <- subset(af_bin_m2, bins == '(0,5]')
m1 <- sum(a$m2.m2.covered.m2)
avr1 <- mean(a$m2.m2.covered.m2)
s1 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(5,10]')
m2 <- sum(a$m2.m2.covered.m2)
avr2 <- mean(a$m2.m2.covered.m2)
s2 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(10,15]')
m3 <- sum(a$m2.m2.covered.m2)
avr3 <- mean(a$m2.m2.covered.m2)
s3 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(15,20]')
m4 <- sum(a$m2.m2.covered.m2)
avr4 <- mean(a$m2.m2.covered.m2)
s4 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(20,25]')
m5 <- sum(a$m2.m2.covered.m2)
avr5 <- mean(a$m2.m2.covered.m2)
s5 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(25,30]')
m6 <- sum(a$m2.m2.covered.m2)
avr6 <- mean(a$m2.m2.covered.m2)
s6 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(30,35]')
m7 <- sum(a$m2.m2.covered.m2)
avr7 <- mean(a$m2.m2.covered.m2)
s7 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(35,40]')
m8 <- sum(a$m2.m2.covered.m2)
avr8 <- mean(a$m2.m2.covered.m2)
s8 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(40,45]')
m9 <- sum(a$m2.m2.covered.m2)
avr9 <- mean(a$m2.m2.covered.m2)
s9 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(45,50]')
m10 <- sum(a$m2.m2.covered.m2)
avr10 <- mean(a$m2.m2.covered.m2)
s10 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(50,55]')
m11 <- sum(a$m2.m2.covered.m2)
avr11 <- mean(a$m2.m2.covered.m2)
s11 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(55,60]')
m12 <- sum(a$m2.m2.covered.m2)
avr12 <- mean(a$m2.m2.covered.m2)
s12 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(60,65]')
m13 <- sum(a$m2.m2.covered.m2)
avr13 <- mean(a$m2.m2.covered.m2)
s13 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(65,70]')
m14 <- sum(a$m2.m2.covered.m2)
avr14 <- mean(a$m2.m2.covered.m2)
s14 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(70,75]')
m15 <- sum(a$m2.m2.covered.m2)
avr15 <- mean(a$m2.m2.covered.m2)
s15 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(75,80]')
m16 <- sum(a$m2.m2.covered.m2)
avr16 <- mean(a$m2.m2.covered.m2)
s16 <- std.err(a$m2.m2.covered.m2)

s1
s2
s3
# Samlet sum og std
samlet_sum <- cbind(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16)
samlet_avr <- cbind(avr1,avr2,avr3,avr4,avr5,avr6,avr7,avr8,avr9,avr10,avr11,avr12,avr13,avr14,avr15,avr16)
samlet_std <- cbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16)

stdfejl <- samlet_std / samlet_avr

# Lav tables
lort <- table(binnedSamples_lort)
bil <- table(binnedSamples_bil)
lort

df_samlet <- data.frame(samlet_sum)
df_samlet


x <- (lort / df_samlet)
x <- unlist(x)
x
is.na(x) <- sapply(x, is.infinite)

par(mar=c(8,5,4,4))
bar <- barplot(x, ylab="Droppings per m2",
               main="Dropping density per m2",
               names.arg=c("0m-5m","5m-10m2","10m-15m","15m-20m",
                           "20m-25m","25m-30m","30m-35m","35m-40m","40m-45m","45m-50m","50m-55m","55m-60m",
                           "60m-65m","65m-70m","70m-75m","75m-80m"),las=2,
               cex.lab = 1.4, cex.main = 1.6)

mtext('Distance from ditch',at=9.3,side=1,outer=F,cex=1.4, padj = 7)  

arrows(bar, x+stdfejl, bar, x-stdfejl, code = 3, angle = 90, length = 0.05)

tynd_19 <- x

#-------------------------------------------------------------------

# Indlæsninging vandløb_lort og vandløb_billeder Nørholm_V 20m
dir2 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/Afstande_vandlob"
dir3 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/Afstande_vandlob"

Afstande_vandlob_5m <- file.path(dir2,"Afstande_vandlob_20m_lort.csv")
Afstande_vandlob_bil <- file.path(dir3,"Afstande_vandlob_20m_billeder.csv")

Afstande_vandlob_5m <- read.csv(file=Afstande_vandlob_5m, header= TRUE)
Afstande_vandlob_bil <- read.csv(file=Afstande_vandlob_bil, header= TRUE)

# opdel i intervaller til vandløb
binnedSamples_lort <- cut( Afstande_vandlob_5m$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) )
binnedSamples_bil <- cut(Afstande_vandlob_bil$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80) )

Afstande_vandlob_bil$bins <- binnedSamples_bil
Afstande_vandlob_bil$bins
Afstande_vandlob_bil

#indlæs m2
bil_id <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/Noerholm_V_5m/Noerholm_V_5m.csv', header = TRUE, sep = ',', dec ='.')
bil_id
m2 <- read.table('G:/P8_projekt/Projekter/Noerholm_V_UTM10m/metadata_areal_5m.csv', header = TRUE, sep = ',', dec ='.')
m2
m2 <- data.frame(bil_id$Name, m2$covered.m2)
m2

# Trim m2 efter udtyndingen
m2 <- m2[match(m2$bil_id.Name, Afstande_vandlob_bil$Name, nomatch=0),]
m2

af_bin_m2 <- data.frame(Afstande_vandlob_bil,m2$m2.covered.m2)
af_bin_m2



# Find summen af m2 + mean + stadard error

std.err <- function(x) sd(x)/sqrt(length(x))

a <- subset(af_bin_m2, bins == '(0,5]')
m1 <- sum(a$m2.m2.covered.m2)
avr1 <- mean(a$m2.m2.covered.m2)
s1 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(5,10]')
m2 <- sum(a$m2.m2.covered.m2)
avr2 <- mean(a$m2.m2.covered.m2)
s2 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(10,15]')
m3 <- sum(a$m2.m2.covered.m2)
avr3 <- mean(a$m2.m2.covered.m2)
s3 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(15,20]')
m4 <- sum(a$m2.m2.covered.m2)
avr4 <- mean(a$m2.m2.covered.m2)
s4 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(20,25]')
m5 <- sum(a$m2.m2.covered.m2)
avr5 <- mean(a$m2.m2.covered.m2)
s5 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(25,30]')
m6 <- sum(a$m2.m2.covered.m2)
avr6 <- mean(a$m2.m2.covered.m2)
s6 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(30,35]')
m7 <- sum(a$m2.m2.covered.m2)
avr7 <- mean(a$m2.m2.covered.m2)
s7 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(35,40]')
m8 <- sum(a$m2.m2.covered.m2)
avr8 <- mean(a$m2.m2.covered.m2)
s8 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(40,45]')
m9 <- sum(a$m2.m2.covered.m2)
avr9 <- mean(a$m2.m2.covered.m2)
s9 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(45,50]')
m10 <- sum(a$m2.m2.covered.m2)
avr10 <- mean(a$m2.m2.covered.m2)
s10 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(50,55]')
m11 <- sum(a$m2.m2.covered.m2)
avr11 <- mean(a$m2.m2.covered.m2)
s11 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(55,60]')
m12 <- sum(a$m2.m2.covered.m2)
avr12 <- mean(a$m2.m2.covered.m2)
s12 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(60,65]')
m13 <- sum(a$m2.m2.covered.m2)
avr13 <- mean(a$m2.m2.covered.m2)
s13 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(65,70]')
m14 <- sum(a$m2.m2.covered.m2)
avr14 <- mean(a$m2.m2.covered.m2)
s14 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(70,75]')
m15 <- sum(a$m2.m2.covered.m2)
avr15 <- mean(a$m2.m2.covered.m2)
s15 <- std.err(a$m2.m2.covered.m2)
a <- subset(af_bin_m2, bins == '(75,80]')
m16 <- sum(a$m2.m2.covered.m2)
avr16 <- mean(a$m2.m2.covered.m2)
s16 <- std.err(a$m2.m2.covered.m2)

s1
s2
s3
# Samlet sum og std
samlet_sum <- cbind(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16)
samlet_avr <- cbind(avr1,avr2,avr3,avr4,avr5,avr6,avr7,avr8,avr9,avr10,avr11,avr12,avr13,avr14,avr15,avr16)
samlet_std <- cbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16)

stdfejl <- samlet_std / samlet_avr

# Lav tables
lort <- table(binnedSamples_lort)
bil <- table(binnedSamples_bil)
lort

df_samlet <- data.frame(samlet_sum)
df_samlet


x <- (lort / df_samlet)
x <- unlist(x)
x
is.na(x) <- sapply(x, is.infinite)

par(mar=c(8,5,4,4))
bar <- barplot(x, ylab="Droppings per m2",
               main="Dropping density per m2",
               names.arg=c("0m-5m","5m-10m2","10m-15m","15m-20m",
                           "20m-25m","25m-30m","30m-35m","35m-40m","40m-45m","45m-50m","50m-55m","55m-60m",
                           "60m-65m","65m-70m","70m-75m","75m-80m"),las=2,
               cex.lab = 1.4, cex.main = 1.6)

mtext('Distance from ditch',at=9.3,side=1,outer=F,cex=1.4, padj = 7)  

arrows(bar, x+stdfejl, bar, x-stdfejl, code = 3, angle = 90, length = 0.05)

tynd_20 <- x


#______________________________________________________________________________________

# Relative forskel imellem tyndinger
std.err <- function(x) sd(x,na.rm=TRUE)/sqrt(length(x))

is.na(procent_forskel) <- sapply(procent_forskel, is.infinite)

densitet <- data.frame(tynd_5,tynd_6,tynd_7,tynd_8,tynd_9,tynd_10,tynd_11,tynd_12,tynd_13,tynd_14,tynd_14,tynd_15,
                       tynd_16,tynd_17,tynd_18,tynd_19,tynd_20)

# avr forskel 5m-6m
forskel <- densitet$tynd_5 - densitet$tynd_6
forskel
dens <- densitet$tynd_5
procent_forskel <- (forskel/dens)*100
procent_forskel <- abs(procent_forskel)
procent_forskel
avr_diff_6m <- mean(procent_forskel,na.rm=TRUE)
avr_diff_6m
stdfejl_6m <- std.err(procent_forskel)
stdfejl_6m


# avr forskel 6m-7m
forskel <- densitet$tynd_5 - densitet$tynd_7
forskel
dens <- densitet$tynd_5
procent_forskel <- (forskel/dens)*100
procent_forskel <- abs(procent_forskel)
procent_forskel
avr_diff_7m <- mean(procent_forskel,na.rm=TRUE)
avr_diff_7m/15
stdfejl_7m <- std.err(procent_forskel)


# avr forskel 7m-8m
forskel <- densitet$tynd_5 - densitet$tynd_8
forskel
dens <- densitet$tynd_5
procent_forskel <- (forskel/dens)*100
procent_forskel <- abs(procent_forskel)
procent_forskel
avr_diff_8m <- mean(procent_forskel,na.rm=TRUE)
stdfejl_8m <- std.err(procent_forskel)

# avr forskel 8m-9m
forskel <- densitet$tynd_5 - densitet$tynd_9
forskel
dens <- densitet$tynd_5
procent_forskel <- (forskel/dens)*100
procent_forskel <- abs(procent_forskel)
procent_forskel
avr_diff_9m <- mean(procent_forskel,na.rm=TRUE)
stdfejl_9m <- std.err(procent_forskel)

# avr forskel 9m-10m
forskel <- densitet$tynd_5 - densitet$tynd_10
forskel
dens <- densitet$tynd_5
procent_forskel <- (forskel/dens)*100
procent_forskel <- abs(procent_forskel)
procent_forskel
avr_diff_10m <- mean(procent_forskel,na.rm=TRUE)
stdfejl_10m <- std.err(procent_forskel)

# avr forskel 10m-11m
forskel <- densitet$tynd_5 - densitet$tynd_11
forskel
dens <- densitet$tynd_5
procent_forskel <- (forskel/dens)*100
procent_forskel <- abs(procent_forskel)
procent_forskel
avr_diff_11m <- mean(procent_forskel,na.rm=TRUE)
stdfejl_11m <- std.err(procent_forskel)

# avr forskel 11m-12m
forskel <- densitet$tynd_5 - densitet$tynd_12
forskel
dens <- densitet$tynd_5
procent_forskel <- (forskel/dens)*100
procent_forskel <- abs(procent_forskel)
procent_forskel
avr_diff_12m <- mean(procent_forskel,na.rm=TRUE)
stdfejl_12m <- std.err(procent_forskel)

# avr forskel 12m-13m
forskel <- densitet$tynd_5 - densitet$tynd_13
forskel
dens <- densitet$tynd_5
procent_forskel <- (forskel/dens)*100
procent_forskel <- abs(procent_forskel)
procent_forskel
avr_diff_13m <- mean(procent_forskel,na.rm=TRUE)
stdfejl_13m <- std.err(procent_forskel)

# avr forskel 13m-14m
forskel <- densitet$tynd_5 - densitet$tynd_14
forskel
dens <- densitet$tynd_5
procent_forskel <- (forskel/dens)*100
procent_forskel <- abs(procent_forskel)
procent_forskel
avr_diff_14m <- mean(procent_forskel,na.rm=TRUE)
stdfejl_14m <- std.err(procent_forskel)

# avr forskel 14m-15m
forskel <- densitet$tynd_5 - densitet$tynd_15
forskel
dens <- densitet$tynd_5
procent_forskel <- (forskel/dens)*100
procent_forskel <- abs(procent_forskel)
procent_forskel
avr_diff_15m <- mean(procent_forskel,na.rm=TRUE)
stdfejl_15m <- std.err(procent_forskel)

# avr forskel 15m-16m
forskel <- densitet$tynd_5 - densitet$tynd_16
forskel
dens <- densitet$tynd_5
procent_forskel <- (forskel/dens)*100
procent_forskel <- abs(procent_forskel)
procent_forskel
avr_diff_16m <- mean(procent_forskel,na.rm=TRUE)
stdfejl_16m <- std.err(procent_forskel)

# avr forskel 16m-17m
forskel <- densitet$tynd_5 - densitet$tynd_17
forskel
dens <- densitet$tynd_5
procent_forskel <- (forskel/dens)*100
procent_forskel <- abs(procent_forskel)
procent_forskel
avr_diff_17m <- mean(procent_forskel,na.rm=TRUE)
stdfejl_17m <- std.err(procent_forskel)

# avr forskel 17m-18m
forskel <- densitet$tynd_5 - densitet$tynd_18
forskel
dens <- densitet$tynd_5
procent_forskel <- (forskel/dens)*100
procent_forskel <- abs(procent_forskel)
procent_forskel
avr_diff_18m <- mean(procent_forskel,na.rm=TRUE)
stdfejl_18m <- std.err(procent_forskel)

# avr forskel 18m-19m
forskel <- densitet$tynd_5 - densitet$tynd_19
forskel
dens <- densitet$tynd_5
procent_forskel <- (forskel/dens)*100
procent_forskel <- abs(procent_forskel)
procent_forskel

is.na(procent_forskel) <- sapply(procent_forskel, is.infinite)

avr_diff_19m <- mean(procent_forskel,na.rm=TRUE)

avr_diff_19m
stdfejl_19m <- std.err(procent_forskel)

# avr forskel 19m-20m
forskel <- densitet$tynd_5 - densitet$tynd_20
forskel
dens <- densitet$tynd_5
procent_forskel <- (forskel/dens)*100
procent_forskel <- abs(procent_forskel)
procent_forskel
avr_diff_20m <- mean(procent_forskel,na.rm=TRUE)
avr_diff_20m/15
stdfejl_20m <- std.err(procent_forskel)
stdfejl_20m
avr_diff_20m


#______________________________________________________________________________________

# Relative forskel imellem tyndinger 5m-10m og 10m-15m
std.err <- function(x) sd(x,na.rm=TRUE)/sqrt(length(x))

is.na(procent_forskel) <- sapply(procent_forskel, is.infinite)

densitet <- data.frame(tynd_5,tynd_6,tynd_7,tynd_8,tynd_9,tynd_10,tynd_11,tynd_12,tynd_13,tynd_14,tynd_14,tynd_15,
                       tynd_16,tynd_17,tynd_18,tynd_19,tynd_20)
densitet

# avr forskel 5m-10m minus 10m-15m
dens_5m_10m <- abs(cbind(densitet$tynd_5, densitet$tynd_6, densitet$tynd_7, densitet$tynd_8, densitet$tynd_9,
                         densitet$tynd_10))
dens_5m_10m

avr_dens_5m_10m <- (dens_5m_10m[,1] + dens_5m_10m[,2] + dens_5m_10m[,2] + dens_5m_10m[,2] + dens_5m_10m[,2] + 
                      dens_5m_10m[,2])/6

avr_dens_5m_10m

dens_10m_15m <- abs(cbind(densitet$tynd_10, densitet$tynd_11, densitet$tynd_12, densitet$tynd_13, densitet$tynd_14,
                          densitet$tynd_15))

avr_dens_10m_15m <- (dens_10m_15m[,1] + dens_10m_15m[,2] + dens_10m_15m[,2] + dens_10m_15m[,2] + dens_10m_15m[,2] + 
                       dens_10m_15m[,2])/6
avr_dens_10m_15m


forskel <- abs(avr_dens_5m_10m - avr_dens_10m_15m)
forskel

dens <- cbind(mean(dens_5m_10m[1,]), mean(dens_5m_10m[2,]), mean(dens_5m_10m[3,]), mean(dens_5m_10m[4,]),
              mean(dens_5m_10m[5,]), mean(dens_5m_10m[6,]), mean(dens_5m_10m[7,]), mean(dens_5m_10m[8,]),
              mean(dens_5m_10m[9,]), mean(dens_5m_10m[10,]), mean(dens_5m_10m[11,]), mean(dens_5m_10m[12,]), 
              mean(dens_5m_10m[13,]), mean(dens_5m_10m[14,]), mean(dens_5m_10m[15,]), mean(dens_5m_10m[16,]))

forskel
dens

procent_forskel <- forskel/dens

procent_forskel

procent_forskel <- abs(procent_forskel)
procent_forskel
avr_diff_5_10og10_15m <- mean(procent_forskel,na.rm=TRUE)
avr_diff_5_10og10_15m
stdfejl_5_10og10_15m <- std.err(procent_forskel)
stdfejl_5_10og10_15m

lowerbound <- c(avr_diff_5_10og10_15m - stdfejl_5_10og10_15m * sqrt(6)/1.96)
lowerbound
upperbound <- c(avr_diff_5_10og10_15m + stdfejl_5_10og10_15m * sqrt(6)/1.96)
upperbound

barplot(avr_diff_5_10og10_15m, ylim = c(0,1))
arrows(avr_diff_5_10og10_15m, avr_diff_5_10og10_15m + stdfejl_5_10og10_15m, 
       avr_diff_5_10og10_15m, avr_diff_5_10og10_15m - stdfejl_5_10og10_15m, code = 3, angle = 90, length = 0.05) 

#______________________________________________________________________________________

# Relative forskel imellem tyndinger 10m-15m minus 15m-20m
std.err <- function(x) sd(x,na.rm=TRUE)/sqrt(length(x))

is.na(procent_forskel) <- sapply(procent_forskel, is.infinite)


densitet <- data.frame(tynd_5,tynd_6,tynd_7,tynd_8,tynd_9,tynd_10,tynd_11,tynd_12,tynd_13,tynd_14,tynd_14,tynd_15,
                       tynd_16,tynd_17,tynd_18,tynd_19,tynd_20)
densitet


# avr forskel 10m-15m minus 15m-20m

dens_10m_15m <- abs(cbind(densitet$tynd_10, densitet$tynd_11, densitet$tynd_12, densitet$tynd_13, densitet$tynd_14,
                          densitet$tynd_15))

avr_dens_10m_15m <- (dens_10m_15m[,1] + dens_10m_15m[,2] + dens_10m_15m[,2] + dens_10m_15m[,2] + dens_10m_15m[,2] + 
                       dens_10m_15m[,2])/6


dens_15m_20m <- abs(cbind(densitet$tynd_15, densitet$tynd_16, densitet$tynd_17, densitet$tynd_18, densitet$tynd_19,
                          densitet$tynd_20))

avr_dens_15m_20m <- (dens_15m_20m[,1] + dens_15m_20m[,2] + dens_15m_20m[,2] + dens_15m_20m[,2] + dens_15m_20m[,2] + 
                       dens_15m_20m[,2])/6



forskel <- abs(avr_dens_10m_15m - avr_dens_15m_20m)
forskel

dens <- cbind(mean(dens_10m_15m[1,]), mean(dens_10m_15m[2,]), mean(dens_10m_15m[3,]), mean(dens_10m_15m[4,]),
              mean(dens_10m_15m[5,]), mean(dens_10m_15m[6,]), mean(dens_10m_15m[7,]), mean(dens_10m_15m[8,]),
              mean(dens_10m_15m[9,]), mean(dens_10m_15m[10,]), mean(dens_10m_15m[11,]), mean(dens_10m_15m[12,]),
              mean(dens_10m_15m[13,]), mean(dens_10m_15m[14,]), mean(dens_10m_15m[15,]), mean(dens_10m_15m[16,]))

forskel
dens

procent_forskel <- forskel/dens

procent_forskel

procent_forskel <- abs(procent_forskel)
procent_forskel
avr_diff_10_15og15_20m <- mean(procent_forskel,na.rm=TRUE)
avr_diff_10_15og15_20m
stdfejl_10_15og15_20m <- std.err(procent_forskel)
stdfejl_10_15og15_20m

lowerbound <- c(avr_diff_10_15og15_20m - stdfejl_10_15og15_20m * sqrt(6)/1.96)
lowerbound
upperbound <- c(avr_diff_10_15og15_20m + stdfejl_10_15og15_20m * sqrt(6)/1.96)
upperbound

barplot(avr_diff_10_15og15_20m, ylim = c(0,1))
arrows(avr_diff_10_15og15_20m, avr_diff_10_15og15_20m + stdfejl_10_15og15_20m, 
       avr_diff_10_15og15_20m, avr_diff_10_15og15_20m - stdfejl_10_15og15_20m, code = 3, angle = 90, length = 0.05) 


#----------------------------------* for begge store sammenligningsintervaller*-------------------------

avr_big_intervals <- cbind(avr_diff_5_10og10_15m, avr_diff_10_15og15_20m)
stdfejl_big_intervals <- cbind(stdfejl_5_10og10_15m, stdfejl_10_15og15_20m)

big_intervals <- barplot(avr_big_intervals, ylim = c(0,0.7))

arrows(big_intervals, avr_big_intervals + stdfejl_big_intervals, 
       big_intervals, avr_big_intervals - stdfejl_big_intervals, code = 3, angle = 90, length = 0.05) 




#______________________________________________________________________________________

# Relative forskel imellem tyndinger 5m-8m, 9m-12m, 13m-16m, 17m-20m
std.err <- function(x) sd(x,na.rm=TRUE)/sqrt(length(x))

is.na(procent_forskel) <- sapply(procent_forskel, is.infinite)


densitet <- data.frame(tynd_5,tynd_6,tynd_7,tynd_8,tynd_9,tynd_10,tynd_11,tynd_12,tynd_13,tynd_14,tynd_14,tynd_15,
                       tynd_16,tynd_17,tynd_18,tynd_19,tynd_20)
densitet


# avr forskel 5m-8m minus 9m-12m

dens_5m_8m <- abs(cbind(densitet$tynd_5, densitet$tynd_6, densitet$tynd_7, densitet$tynd_8))

avr_dens_5m_8m <- (dens_5m_8m[,1] + dens_5m_8m[,2] + dens_5m_8m[,3] + dens_10m_15m[,4])/4
avr_dens_5m_8m

dens_9m_12m <- abs(cbind(densitet$tynd_9, densitet$tynd_10, densitet$tynd_11, densitet$tynd_12))

avr_dens_9m_12m <- (dens_9m_12m[,1] + dens_9m_12m[,2] + dens_9m_12m[,3] + dens_9m_12m[,4])/4
avr_dens_9m_12m


forskel <- abs(avr_dens_5m_8m - avr_dens_9m_12m)
forskel

dens <- cbind(mean(dens_5m_8m[1,]), mean(dens_5m_8m[2,]), mean(dens_5m_8m[3,]), mean(dens_5m_8m[4,]),
              mean(dens_5m_8m[5,]), mean(dens_5m_8m[6,]), mean(dens_5m_8m[7,]), mean(dens_5m_8m[8,]),
              mean(dens_5m_8m[9,]), mean(dens_5m_8m[10,]), mean(dens_5m_8m[11,]), mean(dens_5m_8m[11,]),
              mean(dens_5m_8m[12,]), mean(dens_5m_8m[13,]), mean(dens_5m_8m[14,]), mean(dens_5m_8m[15,]))

forskel
dens

procent_forskel <- (forskel/dens)*100


is.na(procent_forskel) <- sapply(procent_forskel, is.infinite)
procent_forskel
procent_forskel <- abs(procent_forskel)
procent_forskel
avr_diff_5_8og9_12m <- mean(procent_forskel,na.rm=TRUE)
avr_diff_5_8og9_12m
stdfejl <- std.err(procent_forskel)
stdfejl

lowerbound_5_8og9_12m <- c(avr_diff_5_8og9_12m - stdfejl * sqrt(6)/1.96)
lowerbound_5_8og9_12m
upperbound_5_8og9_12m <- c(avr_diff_5_8og9_12m + stdfejl * sqrt(6)/1.96)
upperbound_5_8og9_12m

barplot(avr_diff_5_8og9_12m, ylim = c(0,1.5))
arrows(avr_diff_5_8og9_12m, upperbound_5_8og9_12m, 
       avr_diff_5_8og9_12m, lowerbound_5_8og9_12m, code = 3, angle = 90, length = 0.05) 


# avr forskel 9m-12m minus 13m-16m

dens_13m_16m <- abs(cbind(densitet$tynd_13, densitet$tynd_14, densitet$tynd_15, densitet$tynd_16))

avr_dens_13m_16m <- (dens_13m_16m[,1] + dens_13m_16m[,2] + dens_13m_16m[,3] + dens_13m_16m[,4])/4


dens_9m_12m <- abs(cbind(densitet$tynd_9, densitet$tynd_10, densitet$tynd_11, densitet$tynd_12))

avr_dens_9m_12m <- (dens_9m_12m[,1] + dens_9m_12m[,2] + dens_9m_12m[,3] + dens_9m_12m[,4])/4
avr_dens_9m_12m


# forskel <- abs(avr_dens_13m_16m - avr_dens_9m_12m)
forskel <- abs(avr_dens_13m_16m - avr_dens_5m_8m)
forskel

# dens <- cbind(mean(dens_9m_12m[1,]), mean(dens_9m_12m[2,]), mean(dens_9m_12m[3,]), mean(dens_9m_12m[4,]),
#              mean(dens_9m_12m[5,]), mean(dens_9m_12m[6,]), mean(dens_9m_12m[7,]), mean(dens_9m_12m[8,]),
#              mean(dens_9m_12m[9,]), mean(dens_9m_12m[10,]), mean(dens_9m_12m[11,]))

forskel
dens

procent_forskel <- (forskel/dens)*100
is.na(procent_forskel) <- sapply(procent_forskel, is.infinite)
procent_forskel

procent_forskel <- abs(procent_forskel)
procent_forskel
avr_diff_9_12og13_16m <- mean(procent_forskel,na.rm=TRUE)
avr_diff_9_12og13_16m
stdfejl <- std.err(procent_forskel)
stdfejl

lowerbound_9_12og13_16m <- c(avr_diff_9_12og13_16m - stdfejl * sqrt(6)/1.96)
lowerbound_9_12og13_16m
upperbound_9_12og13_16m <- c(avr_diff_9_12og13_16m + stdfejl * sqrt(6)/1.96)
upperbound_9_12og13_16m

barplot(avr_diff_9_12og13_16m, ylim = c(0,1.5))
arrows(avr_diff_9_12og13_16m, upperbound_9_12og13_16m, 
       avr_diff_9_12og13_16m, lowerbound_9_12og13_16m, code = 3, angle = 90, length = 0.05) 


# avr forskel 13m-16m minus 17m-20m

dens_13m_16m <- abs(cbind(densitet$tynd_13, densitet$tynd_14, densitet$tynd_15, densitet$tynd_16))

avr_dens_13m_16m <- (dens_13m_16m[,1] + dens_13m_16m[,2] + dens_13m_16m[,3] + dens_13m_16m[,4])/4
avr_dens_13m_16m

dens_17m_20m <- abs(cbind(densitet$tynd_17, densitet$tynd_18, densitet$tynd_19, densitet$tynd_20))

avr_dens_17m_20m <- (dens_17m_20m[,1] + dens_17m_20m[,2] + dens_17m_20m[,3] + dens_17m_20m[,4])/4
avr_dens_17m_20m


#forskel <- abs(avr_dens_17m_20m - avr_dens_13m_16m)
forskel <- abs(avr_dens_17m_20m - avr_dens_5m_8m)
forskel

# dens <- cbind(mean(dens_13m_16m[1,]), mean(dens_13m_16m[2,]), mean(dens_13m_16m[3,]), mean(dens_13m_16m[4,]),
#              mean(dens_13m_16m[5,]), mean(dens_13m_16m[6,]), mean(dens_13m_16m[7,]), mean(dens_13m_16m[8,]),
#              mean(dens_13m_16m[9,]), mean(dens_13m_16m[10,]), mean(dens_13m_16m[11,]))

forskel
dens

procent_forskel <- (forskel/dens)*100
is.na(procent_forskel) <- sapply(procent_forskel, is.infinite)
procent_forskel

procent_forskel <- abs(procent_forskel)
procent_forskel
avr_diff_13_16og17_20m <- mean(procent_forskel,na.rm=TRUE)
avr_diff_13_16og17_20m
stdfejl <- std.err(procent_forskel)
stdfejl

lowerbound_13_16og17_20m <- c(avr_diff_13_16og17_20m - stdfejl * 1.96)
lowerbound_13_16og17_20m
upperbound_13_16og17_20m <- c(avr_diff_13_16og17_20m + stdfejl * 1.96)
upperbound_13_16og17_20m

barplot(avr_diff_13_16og17_20m, ylim = c(0,150))
arrows(avr_diff_13_16og17_20m, upperbound_13_16og17_20m, 
       avr_diff_13_16og17_20m, lowerbound_13_16og17_20m, code = 3, angle = 90, length = 0.05) 




#----------------------------------* For de 3 store sammenligningsintervaller*-------------------------

avr_3_intervals_2 <- cbind(avr_diff_5_8og9_12m, avr_diff_9_12og13_16m, avr_diff_13_16og17_20m)
upperbound_intervals_2 <- cbind(upperbound_5_8og9_12m, upperbound_9_12og13_16m,upperbound_13_16og17_20m)
lowerbound_intervals_2 <- cbind(lowerbound_5_8og9_12m, lowerbound_9_12og13_16m,lowerbound_13_16og17_20m)



par(mar=c(6,6,4,4))
big_3_intervals_2 <- barplot(avr_3_intervals_2, ylim = c(0,150), main="Field 2 - Difference in relative density",
                           names.arg=c("5m-8m to 9m-12m","5m-8m to 13m-16m","5m-8m to 17m-20m"),las=1,
                           cex.lab = 1.8, cex.main = 1.8, cex.axis = 1.4, cex.names = 1.4,  col = "grey")
mtext("Thinning comparisons",at=1.9,side=1,outer=F,cex=1.6, padj = 4)
mtext("Difference in density (%)",at=70,side=2,outer=F,cex=1.6, padj = -3.5)

arrows(big_3_intervals_2, upperbound_intervals_2, 
       big_3_intervals_2, lowerbound_intervals_2, code = 3, angle = 90, length = 0.05, lwd = 3) 

avr_3_intervals_1


par(mar=c(6,6,4,4))
big_3_intervals_1 <- barplot(avr_3_intervals_1, ylim = c(0,150), main="Field 1 - Difference in relative density",
                             names.arg=c("5m-8m to 9m-12m","5m-8m to 13m-16m","5m-8m to 17m-20m"),las=1,
                             cex.lab = 1.8, cex.main = 1.8, cex.axis = 1.4, cex.names = 1.2,  col = "grey")
mtext("Thinning comparisons",at=1.9,side=1,outer=F,cex=1.6, padj = 4)
mtext("Difference in density (%)",at=70,side=2,outer=F,cex=1.6, padj = -3.5)

arrows(big_3_intervals_1, upperbound_intervals_1, 
       big_3_intervals_1, lowerbound_intervals_1, code = 3, angle = 90, length = 0.05, lwd = 3)



#####------------- Samlet for både Noerholm V og Klim SKR -------########
# Transfrom dataframe
pro <- cbind(avr_3_intervals_1,avr_3_intervals_2)
pro <- t(pro)
pro_begge <- cbind.data.frame(pro,c("5-8m vs.(9-12m","5-8m vs.(13-16m","5-8m vs.(17-20m"),
                   c('field_1','field_1','field_1','field_2','field_2','field_2'))


rownames(pro_begge) <- c()
colnames(pro_begge) <- c('pro','diff','field')

# rigtige rækkeføgle af factorer
pro_begge$diff <- factor(pro_begge$diff, 
                         levels=c("5-8m vs.(9-12m","5-8m vs.(13-16m","5-8m vs.(17-20m"))

pro_begge
# Tilføj conf intervaller
#Upper
upperbound <- cbind(upperbound_intervals_1,upperbound_intervals_2)
upperbound_begge <- t(upperbound)
rownames(upperbound_begge) <- c()
colnames(upperbound_begge) <- c('upperbound')
#Lower
lowerbound <- cbind(lowerbound_intervals_1,lowerbound_intervals_2)
lowerbound_begge <- t(lowerbound)
rownames(lowerbound_begge) <- c()
colnames(lowerbound_begge) <- c('lowerbound')

samlet_begge <- cbind.data.frame(pro_begge,lowerbound_begge, upperbound_begge)
samlet_begge
samlet_begge$pro <- as.numeric(as.character(samlet_begge$pro))
sapply(samlet_begge, class)

# Lav break i xlabs
addline_format <- function(x,...){
  gsub('\\(','\n',x)
}  


library('ggplot2')

ggplot(samlet_begge, aes(x=diff, y=pro, fill=field)) + 
  geom_bar(position=position_dodge(width = .7), stat="identity",
           colour="black", # Use black outlines,
           size=.5, width = .5) +      # Thinner lines) +
  geom_errorbar(aes(ymin=lowerbound, ymax=upperbound),
                width=.2, size=.5,      # Width of the error bars
                position=position_dodge(.7)) +
  xlab("Thinning comparisons") + 
  ylab('Difference in density (%)') +
  scale_fill_manual(values = c("grey100",'lightgrey'),
                    name="Test field", # Legend label, use darker colors
                    breaks=c("field_1","field_2"),
                    labels=c("Field 1","Field 2")) +
  
  scale_x_discrete(breaks=unique(samlet_begge$diff),
                   labels=addline_format(unique(samlet_begge$diff))) +
  theme_bw() +
  theme(legend.text=element_text(size=13),legend.title=element_text(size=15),
        axis.text = element_text(size=15),
        axis.title = element_text(size = 17),
        axis.ticks = element_line(size = 1),
        axis.title.x = element_text(vjust=0.8),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = c(0.15, .85)) +
  scale_y_continuous(limits=c(0,140),breaks=seq(0,140,20)) 












#KLIM_SKR***OBS****
# FOr de 3 sammenligninger
avr_3_intervals <- cbind(avr_diff_5_8og9_12m, avr_diff_9_12og13_16m, avr_diff_13_16og17_20m)
upperbound_intervals <- cbind(upperbound_5_8og9_12m, upperbound_9_12og13_16m,upperbound_13_16og17_20m)
lowerbound_intervals <- cbind(lowerbound_5_8og9_12m, lowerbound_9_12og13_16m,lowerbound_13_16og17_20m)


par(mar=c(8,5,4,4))
big_3_intervals <- barplot(avr_3_intervals, ylim = c(0,150), main="Field 1 - Difference in relative density",
                           names.arg=c("5m-8m to 9m-12m","5m-8m to 13m-16m","5m-8m to 17m-20m"),las=1,
                           cex.lab = 1.8, cex.main = 1.8, cex.axis = 1.4, cex.names = 1.2)
mtext("Thinning comparisons",at=1.9,side=1,outer=F,cex=1.6, padj = 5)
mtext("Difference in %",at=70,side=2,outer=F,cex=1.6, padj = -3.5)

arrows(big_3_intervals, upperbound_intervals, 
       big_3_intervals, lowerbound_intervals, code = 3, angle = 90, length = 0.05, lwd = 3) 

big_3_intervals








#----------------------- * Gennemsnit for alle bins for alle udtyndinger med 5m

rela_forskel <- cbind(avr_diff_6m,avr_diff_7m,avr_diff_8m,avr_diff_9m,avr_diff_10m,avr_diff_11m,avr_diff_12m,
                      avr_diff_13m,avr_diff_14m,avr_diff_15m,avr_diff_16m,avr_diff_17m,avr_diff_18m,
                      avr_diff_19m,avr_diff_20m)
rela_forskel

rela_5m_afstand <- cbind(mean(rela_forskel[1:4]),mean(rela_forskel[4:9]),
                         mean(rela_forskel[9:14]),mean(rela_forskel[14:19]))
rela_5m_afstand

rela_5_10_mean

rela_5_10_std <- std.err(rela_5_10)


rela_stdfejl <- cbind(stdfejl_6m,stdfejl_7m,stdfejl_8m,stdfejl_9m,stdfejl_10m,stdfejl_11m,stdfejl_12m,
                      stdfejl_13m,stdfejl_14m,stdfejl_15m,stdfejl_16m,stdfejl_17m,stdfejl_18m,
                      stdfejl_19m,stdfejl_20m)

lowerbound <- c(rela_forskel  - rela_stdfejl * 1.96)
lowerbound
upperbound <- c(rela_forskel  + rela_stdfejl * 1.96)
upperbound



par(mar=c(8,5,4,4))
rela_plot <- barplot(rela_forskel, ylab="Difference in density (%)",  col = "grey",
                     ylim = c(0,200), main="Field 2 - Difference in relative density",names.arg=c("5m-6m","5m-7m","5m-8m","5m-9m",
                                                                                                  "5m-10m","5m-11m","5m-12m","5m-13m","5m-14m","5m-15m","5m-16m","5m-17m",
                                                                                                  "5m-18m","5m-19m","5m-20m"),las=2,
                     cex.lab = 1.4, cex.main = 1.6)
mtext("Thinning comparisons",at=9,side=1,outer=F,cex=1.4, padj = 6)  

arrows(rela_plot, upperbound, rela_plot, lowerbound, code = 3, angle = 90, length = 0.05) 

