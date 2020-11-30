# Indlæs csv NNjoin afstande (Veje)
directory <- "G:/Projekter/Klim_holme_SKR_UTM10m/5m-20m_udtynding/Afstande_veje"

Afstande_vej_5 <- file.path(directory,"Afstande_vej_5m.csv")
Afstande_vej_6 <- file.path(directory,"Afstande_vej_6m.csv")
Afstande_vej_7 <- file.path(directory,"Afstande_vej_7m.csv")
Afstande_vej_8 <- file.path(directory,"Afstande_vej_8m.csv")
Afstande_vej_9 <- file.path(directory,"Afstande_vej_9m.csv")
Afstande_vej_10 <- file.path(directory,"Afstande_vej_10m.csv")
Afstande_vej_11 <- file.path(directory,"Afstande_vej_11m.csv")
Afstande_vej_12 <- file.path(directory,"Afstande_vej_12m.csv")
Afstande_vej_13 <- file.path(directory,"Afstande_vej_13m.csv")
Afstande_vej_14 <- file.path(directory,"Afstande_vej_14m.csv")
Afstande_vej_15 <- file.path(directory,"Afstande_vej_15m.csv")
Afstande_vej_16 <- file.path(directory,"Afstande_vej_16m.csv")
Afstande_vej_17 <- file.path(directory,"Afstande_vej_17m.csv")
Afstande_vej_18 <- file.path(directory,"Afstande_vej_18m.csv")
Afstande_vej_19 <- file.path(directory,"Afstande_vej_19m.csv")
Afstande_vej_20 <- file.path(directory,"Afstande_vej_20m.csv")


Afstande_vej_5m <- read.csv(file=Afstande_vej_5, header= TRUE)
Afstande_vej_6m <- read.csv(file=Afstande_vej_6, header= TRUE)
Afstande_vej_7m <- read.csv(file=Afstande_vej_7, header= TRUE)
Afstande_vej_8m <- read.csv(file=Afstande_vej_8, header= TRUE)
Afstande_vej_9m <- read.csv(file=Afstande_vej_9, header= TRUE)
Afstande_vej_10m <- read.csv(file=Afstande_vej_10, header= TRUE)
Afstande_vej_11m <- read.csv(file=Afstande_vej_11, header= TRUE)
Afstande_vej_12m <- read.csv(file=Afstande_vej_12, header= TRUE)
Afstande_vej_13m <- read.csv(file=Afstande_vej_13, header= TRUE)
Afstande_vej_14m <- read.csv(file=Afstande_vej_14, header= TRUE)
Afstande_vej_15m <- read.csv(file=Afstande_vej_15, header= TRUE)
Afstande_vej_16m <- read.csv(file=Afstande_vej_16, header= TRUE)
Afstande_vej_17m <- read.csv(file=Afstande_vej_17, header= TRUE)
Afstande_vej_18m <- read.csv(file=Afstande_vej_18, header= TRUE)
Afstande_vej_19m <- read.csv(file=Afstande_vej_19, header= TRUE)
Afstande_vej_20m <- read.csv(file=Afstande_vej_20, header= TRUE)



# Select colomn vith id 
Afstande_5m <- Afstande_vej_5m$distance
Afstande_6m <- Afstande_vej_6m$distance
Afstande_7m <- Afstande_vej_7m$distance
Afstande_8m <- Afstande_vej_8m$distance
Afstande_9m <- Afstande_vej_9m$distance
Afstande_10m <- Afstande_vej_10m$distance
Afstande_11m <- Afstande_vej_11m$distance
Afstande_12m <- Afstande_vej_12m$distance
Afstande_13m <- Afstande_vej_13m$distance
Afstande_14m <- Afstande_vej_14m$distance
Afstande_15m <- Afstande_vej_15m$distance
Afstande_16m <- Afstande_vej_16m$distance
Afstande_17m <- Afstande_vej_17m$distance
Afstande_18m <- Afstande_vej_18m$distance
Afstande_19m <- Afstande_vej_19m$distance
Afstande_20m <- Afstande_vej_20m$distance

Afstande_5m

All_afstande_mean <- c(mean(Afstande_5m),mean(Afstande_6m),mean(Afstande_7m),mean(Afstande_8m),mean(Afstande_9m),
                      mean(Afstande_10m),mean(Afstande_11m),mean(Afstande_12m),mean(Afstande_13m),mean(Afstande_14m),
                      mean(Afstande_15m),mean(Afstande_16m),mean(Afstande_17m),mean(Afstande_18m),mean(Afstande_19m),
                      mean(Afstande_20m))

plot(All_afstande_mean)

All_afstande_var <- c(var(Afstande_5m),var(Afstande_6m),var(Afstande_7m),var(Afstande_8m),var(Afstande_9m),
                  var(Afstande_10m),var(Afstande_11m),var(Afstande_12m),var(Afstande_13m),var(Afstande_14m),
                  var(Afstande_15m),var(Afstande_16m),var(Afstande_17m),var(Afstande_18m),var(Afstande_19m),
                  var(Afstande_20m))

plot(All_afstande_var)

med <- median(Afstande_5m)
gen <- mean(Afstande_5m)

var(Afstande_20m)
sd(Afstande_10m)
sd(Afstande_20m)
var(Afstande_6m)


stderr(gen)

# Indlæs csv NNjoin afstande (Vandlob)
directory <- "G:/Projekter/Klim_holme_SKR_UTM10m/5m-20m_udtynding/Afstande_vandlob"

Afstande_vandlob_5 <- file.path(directory,"Afstande_vandlob_5m.csv")
Afstande_vandlob_6 <- file.path(directory,"Afstande_vandlob_6m.csv")
Afstande_vandlob_7 <- file.path(directory,"Afstande_vandlob_7m.csv")
Afstande_vandlob_8 <- file.path(directory,"Afstande_vandlob_8m.csv")
Afstande_vandlob_9 <- file.path(directory,"Afstande_vandlob_9m.csv")
Afstande_vandlob_10 <- file.path(directory,"Afstande_vandlob_10m.csv")
Afstande_vandlob_11 <- file.path(directory,"Afstande_vandlob_11m.csv")
Afstande_vandlob_12 <- file.path(directory,"Afstande_vandlob_12m.csv")
Afstande_vandlob_13 <- file.path(directory,"Afstande_vandlob_13m.csv")
Afstande_vandlob_14 <- file.path(directory,"Afstande_vandlob_14m.csv")
Afstande_vandlob_15 <- file.path(directory,"Afstande_vandlob_15m.csv")
Afstande_vandlob_16 <- file.path(directory,"Afstande_vandlob_16m.csv")
Afstande_vandlob_17 <- file.path(directory,"Afstande_vandlob_17m.csv")
Afstande_vandlob_18 <- file.path(directory,"Afstande_vandlob_18m.csv")
Afstande_vandlob_19 <- file.path(directory,"Afstande_vandlob_19m.csv")
Afstande_vandlob_20 <- file.path(directory,"Afstande_vandlob_20m.csv")


Afstande_vandlob_5m <- read.csv(file=Afstande_vandlob_5, header= TRUE)
Afstande_vandlob_6m <- read.csv(file=Afstande_vandlob_6, header= TRUE)
Afstande_vandlob_7m <- read.csv(file=Afstande_vandlob_7, header= TRUE)
Afstande_vandlob_8m <- read.csv(file=Afstande_vandlob_8, header= TRUE)
Afstande_vandlob_9m <- read.csv(file=Afstande_vandlob_9, header= TRUE)
Afstande_vandlob_10m <- read.csv(file=Afstande_vandlob_10, header= TRUE)
Afstande_vandlob_11m <- read.csv(file=Afstande_vandlob_11, header= TRUE)
Afstande_vandlob_12m <- read.csv(file=Afstande_vandlob_12, header= TRUE)
Afstande_vandlob_13m <- read.csv(file=Afstande_vandlob_13, header= TRUE)
Afstande_vandlob_14m <- read.csv(file=Afstande_vandlob_14, header= TRUE)
Afstande_vandlob_15m <- read.csv(file=Afstande_vandlob_15, header= TRUE)
Afstande_vandlob_16m <- read.csv(file=Afstande_vandlob_16, header= TRUE)
Afstande_vandlob_17m <- read.csv(file=Afstande_vandlob_17, header= TRUE)
Afstande_vandlob_18m <- read.csv(file=Afstande_vandlob_18, header= TRUE)
Afstande_vandlob_19m <- read.csv(file=Afstande_vandlob_19, header= TRUE)
Afstande_vandlob_20m <- read.csv(file=Afstande_vandlob_20, header= TRUE)



# Select colomn vith id 
Afstande_Va_5m <- Afstande_vandlob_5m$distance
Afstande_Va_6m <- Afstande_vandlob_6m$distance
Afstande_Va_7m <- Afstande_vandlob_7m$distance
Afstande_Va_8m <- Afstande_vandlob_8m$distance
Afstande_Va_9m <- Afstande_vandlob_9m$distance
Afstande_Va_10m <- Afstande_vandlob_10m$distance
Afstande_Va_11m <- Afstande_vandlob_11m$distance
Afstande_Va_12m <- Afstande_vandlob_12m$distance
Afstande_Va_13m <- Afstande_vandlob_13m$distance
Afstande_Va_14m <- Afstande_vandlob_14m$distance
Afstande_Va_15m <- Afstande_vandlob_15m$distance
Afstande_Va_16m <- Afstande_vandlob_16m$distance
Afstande_Va_17m <- Afstande_vandlob_17m$distance
Afstande_Va_18m <- Afstande_vandlob_18m$distance
Afstande_Va_19m <- Afstande_vandlob_19m$distance
Afstande_Va_20m <- Afstande_vandlob_20m$distance



All_afstande_mean_va <- c(mean(Afstande_Va_5m),mean(Afstande_Va_6m),mean(Afstande_Va_7m),mean(Afstande_Va_8m),
                       mean(Afstande_Va_9m),mean(Afstande_Va_10m),mean(Afstande_Va_11m),mean(Afstande_Va_12m),
                       mean(Afstande_Va_13m),mean(Afstande_Va_14m),mean(Afstande_Va_15m),mean(Afstande_Va_16m),
                       mean(Afstande_Va_17m),mean(Afstande_Va_18m),mean(Afstande_Va_19m),mean(Afstande_Va_20m))

plot(All_afstande_mean_va)

All_afstande_var_va <- c(var(Afstande_Va_5m),var(Afstande_Va_6m),var(Afstande_Va_7m),var(Afstande_Va_8m),var(Afstande_Va_9m),
                      var(Afstande_Va_10m),var(Afstande_Va_11m),var(Afstande_Va_12m),var(Afstande_Va_13m),var(Afstande_Va_14m),
                      var(Afstande_Va_15m),var(Afstande_Va_16m),var(Afstande_Va_17m),var(Afstande_Va_18m),var(Afstande_Va_19m),
                      var(Afstande_Va_20m))

plot(All_afstande_var_va)

