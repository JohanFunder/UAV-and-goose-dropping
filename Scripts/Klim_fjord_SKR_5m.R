# Indl�s udtyndet csv
directory <- "G:/Projekter/Klim_holme_SKR_UTM10m/5m-20m_udtynding/5m_udtynding"

Klim_holme_SKR_5m <- file.path(directory,"5m_udtynding.csv")
Klim_holme_SKR_5m
Klim_SKR_5m <- read.csv(file=Klim_holme_SKR_5m, header= TRUE)
str(Klim_SKR_5m)


# Read udvalgte
udvalgte <- Klim_SKR_5m

# Select colomn vith id 
thin_vec <- udvalgte$Name

thin_vec


# identify the folders
current.folder <- "G:/Overflyvninger_lave/april_18_Klim_holme_skr�mark"

New <- dir.create("G:/Projekter/Klim_holme_SKR_UTM10m/Thin_5m")
new.folder <- ("New")

# find the files that you want => f�rst jpg
list.of.files <- thin_vec
list.of.files

# copy the files to the new folder
file.copy(file.path("G:/Overflyvninger_lave/april_18_Klim_holme_skr�mark",
                    list.of.files), "G:/Projekter/Klim_holme_SKR_UTM10m/Thin_5m", copy.mode = TRUE)




###########################################################################################



###### Metadata indl�sning 

# Indl�s relevant metadata og export til .csv
system2("exiftool", args="-RelativeAltitude -GPSlatitude -GPSlongitude -GimbalYawDegree -ImageHeight -ImageWidth 
        -FocalLength -SensorWidth -csv -t -r 
        G:/Projekter/Klim_holme_SKR_UTM10m/Thin_5m",
        stdout="G:/Projekter/Klim_holme_SKR_UTM10m/K_F_SKR_5m_photodata.csv",stderr = TRUE)

# Indl�s .csv
data1 <- read.csv("G:/Projekter/Klim_holme_SKR_UTM10m/K_F_SKR_5m_photodata.csv")
View(data1)

# Inds�t GPS koord dec og UTM.
GPS_UTM <- read.csv("G:/Projekter/Klim_holme_SKR_UTM10m/5m-20m_udtynding/5m_udtynding/5m_udtynding.csv")[ ,c('xcoord', 'ycoord')]

data <- data.frame(data1,GPS_UTM)
str(data)



# tilf�j col vectorer med "sensor width of the camera (mm)" and "focal length of the camera (mm)"
##OBS: PHANTOM 3 PRO
a <- c(4.62)
b <- c(6.16)

a1 <- rep(a,368)
b1 <- rep(b,368)


require(reshape2)
df <- (data.frame(a1,b1))
colnames(df) <- c("focal.length.of.the.camera.mm",
                  "sensor.width.of.the.camera.mm")

# Kombiner de 2 vektorer til metadata
metadata <- data.frame(data,df)
str(metadata)

# tilf�j vektor med funktion for pixel pr cm 
# Obs uden tilretning med diagonal
metadata$cm.pr.pixel <- ((metadata$sensor.width.of.the.camera.mm)*(metadata$RelativeAltitude*100))/
  ((metadata$focal.length.of.the.camera.mm)*(data1$ImageWidth))

metadata$cm.pr.pixel



# tilf�j vektor med funktion bredde af billed i m
metadata$width.picture.m <- (metadata$cm.pr.pixel*metadata$ImageWidth)/100

str(metadata)


# tilf�j vektor med funktion h�jde af billed i m
metadata$height.picture.m <- (metadata$cm.pr.pixel*metadata$ImageHeight)/100


# Vigtigt tilf�j vektor med funktion m2
metadata$covered.m2 <- (metadata$width.picture.m *metadata$height.picture.m)

View(metadata$covered.m2)

View(metadata)

# skriv .csv
write.csv(metadata, file = "G:/Projekter/Klim_holme_SKR_UTM10m/5m_metadata_areal.csv")

View(read.csv("G:/Projekter/Klim_holme_SKR_UTM10m/5m_metadata_areal.csv"))

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
                paste('G:/Projekter/Klim_holme_SKR_UTM10m/Thin_5m',cufile,sep = '/'), sep = '\n', row.names = FALSE
                ,col.names = FALSE)
  }
}




######################################################################################################
## Write the wolrd file


dat_GPS <- read.table('G:/Projekter/Klim_holme_SKR_UTM10m/5m_udtynding/5m_udtynding.csv', header = TRUE, sep = ',', dec ='.')
dat_drone <- read.table('G:/Projekter/Klim_holme_SKR_UTM10m/5m_metadata_areal.csv', header = TRUE, sep = ',', dec ='.')


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


