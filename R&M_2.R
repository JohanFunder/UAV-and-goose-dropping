# Indlæs udtyndet csv
directory <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/RogM/Billed_position"

RogM_udklipning <- file.path(directory,"Billed_position.csv")
RogM_udklipning
RogM <- read.csv(file=RogM_udklipning, header= TRUE)
str(RogM)


# Read udvalgte
udvalgte <- RogM

# Select colomn vith id 
thin_vec <- udvalgte$Name

thin_vec


# identify the folders
current.folder <- "G:/Projekter/Noerholm_V_UTM10m/Thin"

New <- dir.create("G:/P8_projekt/Projekter/Noerholm_V_UTM10m/Thin_RogM")
new.folder <- ("New")

# find the files that you want => først jpg
list.of.files <- thin_vec
list.of.files

# copy the files to the new folder
file.copy(file.path("G:/P8_projekt/Projekter/Noerholm_V_UTM10m/Thin",
                    list.of.files), "G:/Projekter/Noerholm_V_UTM10m/Thin_RogM", copy.mode = TRUE)


## Gør det samme for udvalgte jgw filer
# Først udvælg jgw filer
find.jgw <- sub("\\.J.*", "", thin_vec)
find.jgw


jgw.ext <- paste(find.jgw,"jgw", sep ='.')
jgw.ext

# copy the files to the new folder
file.copy(file.path("G:/P8_projekt/Projekter/Noerholm_V_UTM10m/Thin", jgw.ext),
          "G:/Projekter/Noerholm_V_UTM10m/Thin_RogM", copy.mode = TRUE)

