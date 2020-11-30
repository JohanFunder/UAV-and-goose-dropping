## Areal for 10m skråmark

dir <- "G:/Projekter/Klim_holme_SKR_UTM10m"

areal <- file.path(dir,"metadata_areal.csv") 

K_SKR <- read.csv(file=areal, header= TRUE)
str(K_SKR)

m2 <- K_SKR$covered.m2
m2

mean <- mean(m2)
mean
median(m2)
sum <- sum(m2)
sum

covered_area <- mean/(11.430447477104765^2)
covered_area

sqrt(27564.641)
27564.641/(100*100)

Field_area <- 27564.641

sum/Field_area

## Areal for 5m skråmark
dir <- "G:/Projekter/Klim_holme_SKR_UTM10m"

areal <- file.path(dir,"5m_metadata_areal.csv")  

K_SKR_5m <- read.csv(file=areal, header= TRUE)
str(K_SKR_5m)

m2 <- K_SKR_5m$covered.m2
m2
mean <- mean(m2)
mean
median <- median(m2)
median
sqrt(median)
sum <- sum(m2)
sum

sqrt(27564.641)
27564.641/(100*100)

Field_area <- 27564.641

sum/Field_area

#_____________________________________

## Areal for 5m Noerholm testmark
dir <- "G:/Projekter/Noerholm_V_UTM10m"

areal <- file.path(dir,"metadata_areal_5m.csv")  

K_SKR_5m <- read.csv(file=areal, header= TRUE)
str(K_SKR_5m)

m2 <- K_SKR_5m$covered.m2

mean <- mean(m2)
mean
median <- median(m2)
median
sqrt(median)
sum <- sum(m2)
sum

#Radius
sqrt(mean)

# Areal af hele mark (obs ikke gjort for denne mark endnu)
sqrt(80814)
80814/(100*100)

Field_area <- 80814

sum/Field_area



#_____________________________________

## Areal for 10m Noerholm testmark
dir <- "G:/Projekter/Noerholm_V_UTM10m"

areal <- file.path(dir,"metadata_areal.csv")  

K_SKR_5m <- read.csv(file=areal, header= TRUE)
str(K_SKR_5m)

m2 <- K_SKR_5m$covered.m2

mean <- mean(m2)
mean
median <- median(m2)
median
sqrt(median)
sum <- sum(m2)
sum

#Radius
sqrt(mean)

# Areal af hele mark (obs ikke gjort for denne mark endnu)
sqrt(80814)
80814/(100*100)

Field_area <- 80814

sum/Field_area

5/.40

