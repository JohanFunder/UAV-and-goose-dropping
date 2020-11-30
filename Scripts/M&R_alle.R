#For testmark 1
Alles_prik.1 <- read.csv("G:/P8_projekt/Projekter/R&M/Prøvemark_1/Alles_prik_1/Alles_prik_1.csv", header = TRUE)
Alles_prik.1 <- data.frame(Alles_prik.1$mit_prik,Alles_prik.1$mads_prik,Alles_prik.1$rune_prik)
colnames(Alles_prik.1) <- c("mit_prik","mads_prik","rune_prik")
summary(Alles_prik.1)

Alles_prik_1 <- stack(Alles_prik.1)
Alles_prik_1

anova.results_1 <- aov(values  ~ ind,data =  Alles_prik_1)
bartlett.test(values  ~ ind,data =  Alles_prik_1)

summary(anova.results)

# Kruskal wallis test
kruskal.test(values  ~ ind,data =  Alles_prik_1)


#For testmark 2
Alles_prik.2 <- read.csv("G:/P8_projekt/Projekter/R&M/Prøvemark_2/Alles_prik_2/Alles_prik_2.csv", header = TRUE)
Alles_prik.2 <- data.frame(Alles_prik.2$mit_prik,Alles_prik.2$mads_prik,Alles_prik.2$rune_prik)
colnames(Alles_prik.2) <- c("mit_prik","mads_prik","rune_prik")
summary(Alles_prik.2)

Alles_prik_2 <- stack(Alles_prik.2)
Alles_prik_2

anova.results_2 <- aov(values  ~ ind,data =  Alles_prik_2)
bartlett.test(values  ~ ind,data =  Alles_prik_2)
shapiro.test(Alles_prik.2$mit_prik)
shapiro.test(Alles_prik.2$mads_prik)

summary(anova.results)


# Kruskal wallis test
kruskal.test(values  ~ ind,data =  Alles_prik_2)


par(mfrow = c(1,2))
par(oma=c(2,2,2,2))
par(mar=c(2,5,2,0))
tuk_1 <- TukeyHSD(anova.results_1)
tuk_2 <- TukeyHSD(anova.results_2)
plot(tuk_1)
plot(tuk_2)
title("Tukey for comparison of dropping counts", outer=TRUE,cex.main=2)

#, ylab = c("per1-pers2","per1-pers3","per2-pers3")

# Boxplots
boxplot(Alles_prik.1/5.330399, xaxt="n", ylab=expression(Droppings ~ m^{-2}),
        cex.lab = 1.6, cex.main = 1.6, cex.axis = 1.3)
axis(1, at=1:3, labels=c("Person 1","Person 2","Person 3"),font = 2, cex = 1.4)
mtext("3a",at=0.6, side=1, outer=F, cex=1.7, padj = -20.)

boxplot(Alles_prik.2/7.803417, xaxt="n",
        cex.lab = 1.6, cex.main = 1.6, cex.axis = 1.1)
axis(1, at=1:3, labels=FALSE,font = 2, cex = 1.4)
text(seq(1, 3, by=1), par("usr")[3] - 0.2, labels=c("Person 1","Person 2","Person 3"),
     srt = -25, pos = 1, xpd = TRUE)

mtext("3b",at=0.6, side=1, outer=F, cex=1.7, padj = -20.)



