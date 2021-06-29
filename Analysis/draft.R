load("Data/ddef_datiECR.rda")
ddef=ddef_datiECR[ddef_datiECR$age_year<12.30,]
ddef<- subset(ddef, ddef$fas !="NA")
nrow(ddef) #847

#---- cluster madre ----

dcM <- dist(ddef[,c(60:71)], method = "euclidean") # distance matrix
fit <- hclust(dcM, method="ward.D")
plot(fit, main = "Dendrogramma")
# dal dendrogramma sembrano emergere 4 cluster
rect.hclust(fit, k=4, border="red")
ddef$gruppiattM <- as.factor(cutree(fit, k=4))
table(ddef$gruppiattM)


# profilo
fattore=rep(c("Ansia","Evitamento"),rep(dim(ddef)[1],2))
y=c(scale(ddef$Anxm),scale(ddef$Avm))
y1=c(ddef$Anxm,ddef$Avm)
gruppo=rep(ddef$gruppiattM,2)
dprof=data.frame(y,y1,fattore,gruppo)


dprof$nomi.gruppi=factor(dprof$gruppo,labels=c("Gruppo1: Sicuri (n = 231)","Gruppo 2: Ansiosi (n = 286)","Gruppo 3: Ansiosi-Evitanti (n = 100)","Gruppo 4: Evitanti (n = 230)"))
par(mar=c(6, 5, 4, 2) + 0.1)

sciplot::bargraph.CI(fattore, y, group = nomi.gruppi, data=dprof,legend=T,ylim=c(-2.4,1.6),x.leg=1.2, y.leg=-1.2,ylab="Punteggi standardizzati",xlab="",cex.lab=.8,cex.leg=.8, col=c("yellow","green","blue","red"))


#---- cluster padre ----

dcP <- dist(ddef[,c(99:110)], method = "euclidean") # distance matrix
fit <- hclust(dcP, method="ward.D")
plot(fit, main = "Dendrogramma")
# dal dendrogramma sembrano emergere 4 cluster
rect.hclust(fit, k=4, border="red")
ddef$gruppiattP <- as.factor(cutree(fit, k=4))

# Profilo
fattore=rep(c("Ansia","Evitamento"),rep(dim(ddef)[1],2))
y=c(scale(ddef$Anxp),scale(ddef$Avp))
y1=c(ddef$Anxp,ddef$Avp)
gruppo=rep(ddef$gruppiattP,2)
dprof=data.frame(y,y1,fattore,gruppo)

dprof$nomi.gruppi=factor(dprof$gruppo,labels=c("Gruppo1: Sicuri (n = 206)","Gruppo 2: Ansiosi (n = 230)","Gruppo 3: Evitanti (n = 311 )","Gruppo 4: Ansiosi-Evitanti (n = 100)"))

par(mar=c(6, 5, 4, 2) + 0.1)

sciplot::bargraph.CI(fattore, y, group = nomi.gruppi, data=dprof,legend=T,ylim=c(-2.4,1.6),x.leg=1.2, y.leg=-1.2,ylab="Punteggi standardizzati",xlab="",cex.lab=.8,cex.leg=.8, col=c("yellow","green","blue","red"))

prop.table(table(ddef$gruppiattP, ddef$gruppiattM), margin = 1)
str(ddef$gruppiattP)

cor(as.numeric(ddef$gruppiattP), as.numeric(ddef$gruppiattM))

#---- fit ext ----

hist(ddef$externalizing_sum)

fit_ext <- glm(round(externalizing_sum,0) ~ genere * gruppiattP * gruppiattM, ddef,
               family = "poisson")
summary(fit_ext)
car::Anova(fit_ext)
plot(fit_ext)

performance::check_zeroinflation(fit_ext)

plot(effects::allEffects(fit_ext))

# log transform
hist(log(ddef$externalizing_sum))

#---- fit ext no 0 ----

sum(ddef$externalizing_sum > 0)
ddef_ext0 <- ddef[ddef$externalizing_sum > 0, ]
hist(ddef_ext0$externalizing_sum)

fit_ext0 <- glm(round(externalizing_sum,0) ~ genere * gruppiattP * gruppiattM, ddef_ext0, family = "poisson")
summary(fit_ext0)
car::Anova(fit_ext0)
plot(fit_ext0)

plot(effects::allEffects(fit_ext0))


summary(fit_ext0$residuals)
hist(fit_ext0$residuals)


#---- fit int ----

hist(ddef$internalizing_sum)
sum(ddef$internalizing_sum > 0)

fit_int <- glm(round(internalizing_sum,0) ~ genere * gruppiattP * gruppiattM, ddef, family = "poisson")
summary(fit_int)
car::Anova(fit_int)


fit_int_ext <- lm(cbind(internalizing_sum, externalizing_sum) ~ genere * gruppiattP * gruppiattM, ddef)



performance::check_zeroinflation(fit_int)

plot(effects::allEffects(fit_int))


#----



