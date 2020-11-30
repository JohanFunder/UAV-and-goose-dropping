# Indlæs udtyndet csv
directory <- "G:/Projekter/Klim_holme_SKR_UTM10m/R&M_udklipning"

RogM_udklipning <- file.path(directory,"RogM_udklipning.csv")
RogM_udklipning
RogM <- read.csv(file=RogM_udklipning, header= TRUE)
str(RogM)


# Read udvalgte
udvalgte <- RogM

# Select colomn vith id 
thin_vec <- udvalgte$Name

thin_vec


# identify the folders
current.folder <- "G:/Projekter/Klim_holme_SKR_UTM10m/Thin_5m"

New <- dir.create("G:/Projekter/Klim_holme_SKR_UTM10m/Thin_RogM")
new.folder <- ("New")

# find the files that you want => først jpg
list.of.files <- thin_vec
list.of.files

# copy the files to the new folder
file.copy(file.path("G:/Projekter/Klim_holme_SKR_UTM10m/Thin_5m",
                    list.of.files), "G:/Projekter/Klim_holme_SKR_UTM10m/Thin_RogM", copy.mode = TRUE)


## Gør det samme for udvalgte jgw filer
# Først udvælg jgw filer
find.jgw <- sub("\\.J.*", "", thin_vec)
find.jgw


jgw.ext <- paste(find.jgw,"jgw", sep ='.')
jgw.ext

# copy the files to the new folder
file.copy(file.path("G:/Projekter/Klim_holme_SKR_UTM10m/Thin_5m", jgw.ext),
          "G:/Projekter/Klim_holme_SKR_UTM10m/Thin_RogM", copy.mode = TRUE)


