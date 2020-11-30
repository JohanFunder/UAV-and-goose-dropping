# Indlæsninging vandløb_lort og vandløb_billeder KLIM_SKR
dir <- "G:/P8_projekt/Projekter/Klim_holme_SKR_UTM10m/5m-20m_udtynding/Afstande_vandlob"
dir1 <- "G:/P8_projekt/Projekter/Klim_holme_SKR_UTM10m/5m-20m_udtynding/Afstande_vandlob"

Afstande_vandlob_5m <- file.path(dir,"Afstande_vandlob_5m.csv")
Afstande_vandlob_bil <- file.path(dir1,"Afstande_vandlob_5m_billeder.csv")

Afstande_vandlob_5m <- read.csv(file=Afstande_vandlob_5m, header= TRUE)
Afstande_vandlob_bil <- read.csv(file=Afstande_vandlob_bil, header= TRUE)

# opdel i intervaller til vandløb
binnedSamples_lort <- cut( Afstande_vandlob_5m$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60) )
binnedSamples_bil <- cut(Afstande_vandlob_bil$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60) )

lort <- table(binnedSamples_lort)
bil <- table(binnedSamples_bil)

x <- lort/bil

par(mar=c(8,5,4,4))
barplot(x, ylab="Droppings per photo",
main="Dropping density per photo",
names.arg=c("0m-5m","5m-10m2","10m-15m","15m-20m",
            "20m-25m","25m-30m","30m-35m","35m-40m","40m-45m","45m-50m","50m-55m","55m-60m"),las=2,
            cex.lab = 1.4, cex.main = 1.6)

mtext('Distance from ditch',at=7.3,side=1,outer=F,cex=1.4, padj = 7)  



tapply( binnedSamples_lort, binnedSamples_bil, sum )



#-------------------------------------------------------------------

# Indlæsninging vandløb_lort og vandløb_billeder Nørholm_V
dir2 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/5m_udtynding/Afstande_vandlob_5m_lort"
dir3 <- "G:/P8_projekt/Projekter/Noerholm_V_UTM10m/5m-20m_udtynding/5m_udtynding/Afstande_vandlob_5m_billeder"

Afstande_vandlob_5m <- file.path(dir2,"Afstande_vandlob_5m.csv")
Afstande_vandlob_bil <- file.path(dir3,"Afstande_vandlob_5m_billeder.csv")

Afstande_vandlob_5m <- read.csv(file=Afstande_vandlob_5m, header= TRUE)
Afstande_vandlob_bil <- read.csv(file=Afstande_vandlob_bil, header= TRUE)

# opdel i intervaller til vandløb
binnedSamples_lort <- cut( Afstande_vandlob_5m$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85) )
binnedSamples_bil <- cut(Afstande_vandlob_bil$distance, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85) )

tapply( binnedSamples_lort, binnedSamples_bil, sum )

plot(binnedSamples_lort,
xaxt = "n",
pch=19, xlab="Distance", ylab="Density pr. m2",
main="Density pr m2 with increasing distance")
axis(1, at=1:17, labels=c("0m-10m","10m-15m","15m-20m","20m-25m","0m-10m","0m-10m","0m-10m","0m-10m","0m-10m",
                          "0m-10m","0m-10m","0m-10m","0m-10m","0m-10m","0m-10m","0m-10m","0m-10m"))

# arrows(graf$hojde, graf$mean-graf$se, graf$hojde, graf$mean+graf$se , length=0.05, angle=90, code=3)

plot(binnedSamples_bil)

