
Ringe_drone <- read.csv("G:/P8_projekt/Ringe_drone/Ringe_drone_factor.csv", header= TRUE)
str(Ringe_drone)
Ringe_drone <- data.frame(Ringe_drone)

#Subset af procent afvigelse

f <-subset.data.frame(Ringe_drone[c(3,5)])
f

#Udregning af mean, median, var
m <-aggregate(f[2:2],list(f$Hojde),FUN = mean)
colnames(m)<-c("hojde", "mean")
m
m1<-aggregate(f[2:2],list(f$Hojde),FUN = median)
colnames(m1)<-c("hojde", "median")
m1
m2<-aggregate(f[2:2],list(f$Hojde),FUN = var)
m2

# Stdfejl
se <- function(x) sqrt(var(x)/length(x))
m3 <- aggregate(f[2:2],list(f$Hojde),FUN = se)
m3
colnames(m3)<-c("hojde", "se")
m3

Accuracy <- data.frame(m,m3$se)
Accuracy <- cbind(setNames(data.frame(Accuracy), c('hojde', 'mean','se')))
Accuracy
graf_median <- cbind(m1[2:5,],m3[2:5,])
graf_median



graf <- cbind(Accuracy[2:5,]$hojde, Accuracy[2:5,]$mean*100, Accuracy[2:5,]$se*100)
graf <- cbind(setNames(data.frame(graf), c('hojde', 'mean','se')))
graf

graf$mean

# Find konfidansintervaller
lowerbound <- c(graf$mean-graf$se*sqrt(15)/1.96)
lowerbound
upperbound <- c(graf$mean+graf$se*sqrt(15)/1.96)
upperbound

par(mfrow = c(1,1))
par(oma=c(2,2,2,2))
par(mar=c(4.3,4.3,2,2))
# Plot mean
plot(graf$hojde,graf$mean, 
     xaxt = "n", ylim=c(40,100),
     pch=19, cex=1.5, xlab="Altitude", ylab="Acuracy (%)", cex.axis = 1.2, 
     cex.lab = 1.6, cex.main = 1.7)

#main="Measured accuracy with increasing flying altitude", 

axis(1, at=1:5, labels=c("1m","2 m","3 m","4 m","5 m"), cex = 0.8,lwd.ticks = 2, cex.axis = 1.3)

abline(h=80, col="red", lwd=3, lty=2)
# Konfidansintervaller
arrows(graf$hojde, lowerbound, graf$hojde, upperbound , length=0.05, lwd=3.5, angle=90, code=3)



# Plot vendt om
plot(graf$mean, graf$hojde, xlim = c(40,100),
     yaxt = "n",
     pch=19, xlab="% acuracy", ylab="Altitude",
     main="Measured accuracy with increasing flying altitude")

axis(2, at=1:5, labels=c("1 m","2 m","3 m","4 m","5 m"))
abline(v=80, col="red", lwd=3, lty=2)

# Konfidansintervaller
arrows(lowerbound, graf$hojde, upperbound,graf$hojde , length=0.05, angle=90, code=3)


#Test af varians homogenitet
two_five <- Ringe_drone[Ringe_drone$Hojde == 2 | Ringe_drone$Hojde ==3 | Ringe_drone$Hojde == 4 | Ringe_drone$Hojde == 5,]
two_five



bartlett.test(Antal.lort~Hojde,Ringe_drone)

# Shapiro-wilks test
H2 <- Ringe_drone[Ringe_drone$Hojde == "2m",]
H3 <- Ringe_drone[Ringe_drone$Hojde == "3m",]
H4 <- Ringe_drone[Ringe_drone$Hojde == "4m",]
H5 <- Ringe_drone[Ringe_drone$Hojde == "5m",]

shapiro.test(H2$Antal.lort)
shapiro.test(H3$Antal.lort)
shapiro.test(H4$Antal.lort)
shapiro.test(H5$Antal.lort)






krus <- kruskal.test(Antal.lort~Hojde,two_five)
krus


two_five

TukeyHSD(krus, ordered =True)
TukeyHSD(aov(j))

j <-aov(Antal.lort ~ Hojde,data =  Ringe_drone)

summary(j)

plot(TukeyHSD(j))


two_five


t.test(H2$Antal.lort,H3$Antal.lort)
t.test(H2$Antal.lort,H4$Antal.lort)


