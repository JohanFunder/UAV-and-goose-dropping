# Indlæs udtyndet csv
directory <- "G:/Projekter/Klim_holme_bag_krap_UTM10m/Thin_photos"

Klim_holme_B_K_10m <- file.path(directory,"K_F_B_K_10m.csv")
Klim_holme_B_K_10m
Klim_B_K_10m <- read.csv(file=Klim_holme_B_K_10m, header= TRUE)
str(Klim_B_K_10m)


# Read udvalgte
udvalgte <- Klim_B_K_10m

# Select colomn vith id 
thin_vec <- udvalgte$Name

thin_vec


# identify the folders
current.folder <- "G:/Overflyvninger_lave/april_18_Klim_holme_bag_krap"

New <- dir.create("G:/Projekter/Klim_holme_bag_krap_UTM10m/Thin")
new.folder <- ("New")

# find the files that you want => først jpg
list.of.files <- thin_vec
list.of.files

# copy the files to the new folder
file.copy(file.path("G:/Overflyvninger_lave/april_18_Klim_holme_bag_krap",
                    list.of.files), "G:/Projekter/Klim_holme_bag_krap_UTM10m/Thin", copy.mode = TRUE)




###########################################################################################



###### Metadata indlæsning 

# Indlæs relevant metadata og export til .csv
system2("exiftool", args="-RelativeAltitude -GPSlatitude -GPSlongitude -GimbalYawDegree -ImageHeight -ImageWidth 
        -FocalLength -SensorWidth -csv -t -r 
        G:/Projekter/Klim_holme_bag_krap_UTM10m/Thin",
        stdout="G:/Projekter/Klim_holme_bag_krap_UTM10m/K_F_B_K_photodata.csv",stderr = TRUE)

# Indlæs .csv
data1 <- read.csv("G:/Projekter/Klim_holme_bag_krap_UTM10m/K_F_B_K_photodata.csv")
View(data1)

# Indsæt GPS koord dec og UTM.
GPS_UTM <- read.csv("G:/Projekter/Klim_holme_bag_krap_UTM10m/Thin_photos/K_F_B_K_10m.csv")[ ,c('xcoord', 'ycoord')]

data <- data.frame(data1,GPS_UTM)
str(data)



# tilføj col vectorer med "sensor width of the camera (mm)" and "focal length of the camera (mm)"
##OBS: PHANTOM 4 PRO
a <- c(4.62)
b <- c(6.16)

a1 <- rep(a,282)
b1 <- rep(b,282)


require(reshape2)
df <- (data.frame(a1,b1))
colnames(df) <- c("focal.length.of.the.camera.mm",
                  "sensor.width.of.the.camera.mm")

# Kombiner de 2 vektorer til metadata
metadata <- data.frame(data,df)
str(metadata)

#tilføj vektor med funktion for pixel pr cm (find først d = \sqrt{h^2 + b^2}) OBSOBSOBSOBSOBSOBSOBSOBSOBSOBS
metadata$cm.pr.pixel <- ((metadata$sensor.width.of.the.camera.mm)*(metadata$RelativeAltitude*100))/
  ((metadata$focal.length.of.the.camera.mm)*(sqrt(data1$ImageHeight^2 + data1$ImageWidth^2)))

metadata$cm.pr.pixel


# tilføj vektor med funktion bredde af billed i m
metadata$width.picture.m <- (metadata$cm.pr.pixel*metadata$ImageWidth)/100

str(metadata)


# tilføj vektor med funktion højde af billed i m
metadata$height.picture.m <- (metadata$cm.pr.pixel*metadata$ImageHeight)/100


# Vigtigt tilføj vektor med funktion m2
metadata$covered.m2 <- (metadata$width.picture.m *metadata$height.picture.m)

View(metadata$covered.m2)

View(metadata)

# skriv .csv
write.csv(metadata, file = "G:/Projekter/Klim_holme_bag_krap_UTM10m/metadata_areal.csv")



######################################################################################################
## mod_edgecoord

edgecoord_UTM <- function(Northing,Easting,height,width,angle){
  len = sqrt(((1/2*height)^2)+((1/2*width)^2))
  #len is distance to a corner of the picture
  # trigonometric functions in R use radians so first
  #you need to convert degrees to radians then you use that cos(theta) =A/C
  # and sin(theta) = B/C
  rel_angle <-acos((height*0.5)/len)*180/pi
  North <- len*cos((rel_angle-angle)*pi/180)
  East <- len*sin((rel_angle-angle)*pi/180)
  # we can use that all distances to the corners have symmetric length.
  coordupperright <- cbind(Northing + North,Easting + East)
  coordlowerleft <- cbind(Northing - North,Easting - East)
  coordupperleft <- cbind(Northing + North,Easting - East)
  coordlowerright <- cbind(Northing - North,Easting + East)
  colnames(coordupperright) <- c('Northing','Easting')
  colnames(coordlowerright) <- c('Northing','Easting')
  colnames(coordupperleft) <- c('Northing','Easting')
  colnames(coordlowerleft) <- c('Northing','Easting')
  res = list(coordupperright = coordupperright,
             coordlowerright = coordlowerright,coordupperleft = coordupperleft,
             coordlowerleft = coordlowerleft)
}
edgecoord_UTM


#F is called Fa since F is a reserved character in R.
# The first character in the second argument in writelines is the folder name
# ext is the file extension in our case jgw remember without .
# since this is added in filename.ext.
# easting and northing are x,y coordinates of



write_worldfile <- function(Northing, Easting,angle,
                            PixSize,filename,ext){
  A <- cos((pi / 180) * angle) * PixSize
  D <- sin((pi / 180) * angle) * -PixSize
  B <- sin((pi / 180) * angle) * -PixSize
  E <- cos((pi / 180) * angle) * -PixSize
  C <- Easting
  Fa <- Northing
  extension <- ext
  filename <-  gsub(".J.*","",filename)
  filename.ext <- paste(filename,extension, sep ='.')
  for (i in 1:length(filename.ext)){
    cufile <- filename.ext[i]
    write.table(c(A[i],D[i],B[i],E[i],C[i],Fa[i]),
                paste('G:/Projekter/Klim_holme_bag_krap_UTM10m/Thin',cufile,sep = '/'), sep = '\n', row.names = FALSE
                ,col.names = FALSE)
  }
}




######################################################################################################
## Write the wolrd file


dat_GPS <- read.table('G:/Projekter/Klim_holme_bag_krap_UTM10m/Thin_photos/K_F_B_K_10m.csv', header = TRUE, sep = ',', dec ='.')
dat_drone <- read.table('G:/Projekter/Klim_holme_bag_krap_UTM10m/metadata_areal.csv', header = TRUE, sep = ',', dec ='.')


coord <- edgecoord_UTM(Northing = dat_GPS$ycoord,Easting = dat_GPS$xcoord,
                       height =dat_drone$height.picture.m, 
                       width = dat_drone$width.picture.m, 
                       angle = dat_drone$GimbalYawDegree)
str(coord)

#UTM_upperleft <- degtoUTMconv(latitude = coord$coordupperleft[,1], longitude = coord$coordupperleft[,2])

write_worldfile(Easting = coord$coordupperleft[,2],
                Northing = coord$coordupperleft[,1],
                angle = dat_drone$GimbalYawDegree,
                PixSize = dat_drone$cm.pr.pixel/100,
                filename =dat_GPS$Name,
                ext = rep('jgw',108))


