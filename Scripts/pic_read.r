# Read udvalgte
udvalgte <- read.table('UTM_10m.csv', header= TRUE, sep =';',dec = ',')

# Select colomn vith id 
thin_vec <- udvalgte$x

thin_vec


# identify the folders
current.folder <- "100MEDIA"
dir.create("Thin_pic")
new.folder <- ("Thin_pic")

# find the files that you want først jpg
list.of.files <- thin_vec
list.of.files

# copy the files to the new folder
file.copy(file.path(current.folder, list.of.files), new.folder, copy.mode = TRUE)

## Gør det samme for udvalgte jgw filer
# Først udvælg jgw filer
find.jgw <- substring(thin_vec,1,8)
find.jgw
jgw.ext <- paste(find.jgw,"jgw", sep ='.')
jgw.ext

# copy the files to the new folder
file.copy(file.path(current.folder, jgw.ext), new.folder, copy.mode = TRUE)


