library(readr)
library(VGAM)
source("http://rfs.kvasaheim.com/rfs.R")
dt <- read_csv("D:Research/DI/Hungary 2018 Election Data - Sheet1 (2).csv")
attach(dt)
summary(dt)
View(dt)

depvar <- cbind(invalidVotes, validVotes) #3TOTAL
dFIDESZ <- FIDESZ/validVotes ##TOTAL
pInv <- invalidVotes/totalVotes
dDK <- DK/validVotes
dJOBBIK <- JOBBIK/validVotes
dLMP <- LMP/validVotes
dMSZP <- MSZP/validVotes

############################################################Budapest
dF <- dFIDESZ[county =="Budapest"] ##just for budapest
dV <- depvar[county == "Budapest",] ## ^^
dI <- pInv[county == "Budapest"]

modBudapest <- vglm( dV~dF, family=betabinomial )
summary(modBudapest) ##pval is 0.674
 
par(xaxs="i", yaxs="i")
par(las=1, family="serif")
par(cex.axis=0.9, font.lab=2,cex.lab=1.2)
par(mar=c(4,4,0,0)+0.5)
par(lend=1)

plot.new()
plot.window( xlim=c(0,1), ylim=c(0,0.08) )


axis(1)
axis(2)
title(xlab="PA Candidate Support")
title(ylab="Invalidation Rate")

newX = seq(0.25,0.45,length=10000)
prediction = predict(modBudapest, newdata=data.frame(dF=newX) )

lines(newX, logit.inv(prediction[,1]), lwd=5 )
lines(newX, logit.inv(prediction[,1]), col="lightblue")

points( dF, dI, pch=21,bg="maroon" )

##########################################################3##Bács-Kiskun
dF <- dFIDESZ[county =="Bács-Kiskun"] ##just for bacs
dV <- depvar[county == "Bács-Kiskun",] ## ^^
dI <- pInv[county == "Bács-Kiskun"]

modBácsKiskun <- vglm( dV~dF, family=betabinomial )
summary(modBácsKiskun) ##pval is 0.622

par(xaxs="i", yaxs="i")
par(las=1, family="serif")
par(cex.axis=0.9, font.lab=2,cex.lab=1.2)
par(mar=c(4,4,0,0)+0.5)
par(lend=1)

plot.new()
plot.window( xlim=c(0,1), ylim=c(0,0.08) )


axis(1)
axis(2)
title(xlab="PA Candidate Support")
title(ylab="Invalidation Rate")

newX = seq(0.50,0.61,length=10000)
prediction = predict(modBácsKiskun, newdata=data.frame(dF=newX) )

lines(newX, logit.inv(prediction[,1]), lwd=5 )
lines(newX, logit.inv(prediction[,1]), col="lightblue")

points( dF, dI, pch=21,bg="maroon" )

################################################################333Barayana

dF <- dFIDESZ[county =="Baranya"] ##just for Baranya
dV <- depvar[county == "Baranya",] ## ^^
dI <- pInv[county == "Baranya"]

modBaranya <- vglm( dV~dF, family=betabinomial )
summary(modBaranya) ##pvalue was 0.00468, so is signifigant

par(xaxs="i", yaxs="i")
par(las=1, family="serif")
par(cex.axis=0.9, font.lab=2,cex.lab=1.2)
par(mar=c(4,4,0,0)+0.5)
par(lend=1)

plot.new()
plot.window( xlim=c(0,1), ylim=c(0,0.08) )


axis(1)
axis(2)
title(xlab="PA Candidate Support")
title(ylab="Invalidation Rate")

newX = seq(0.30,0.61,length=10000)
prediction = predict(modBaranya, newdata=data.frame(dF=newX) )

lines(newX, logit.inv(prediction[,1]), lwd=5 )
lines(newX, logit.inv(prediction[,1]), col="lightblue")

points( dF, dI, pch=21,bg="maroon" )

#############################################Békés

dF <- dFIDESZ[county =="Békés"] ##just for Békés
dV <- depvar[county == "Békés",] ## ^^
dI <- pInv[county == "Békés"]

modBékés <- vglm( dV~dF, family=betabinomial )
summary(modBékés) ##pvalue was 0.0278, so is signifigant w/ 0.01

par(xaxs="i", yaxs="i")
par(las=1, family="serif")
par(cex.axis=0.9, font.lab=2,cex.lab=1.2)
par(mar=c(4,4,0,0)+0.5)
par(lend=1)

plot.new()
plot.window( xlim=c(0,1), ylim=c(0,0.08) )


axis(1)
axis(2)
title(xlab="PA Candidate Support")
title(ylab="Invalidation Rate")

newX = seq(0.30,0.61,length=10000)
prediction = predict(modBékés, newdata=data.frame(dF=newX) )

lines(newX, logit.inv(prediction[,1]), lwd=5 )
lines(newX, logit.inv(prediction[,1]), col="lightblue")

points( dF, dI, pch=21,bg="maroon" )

#########################################################Borsod-Abaúj-Zemplén

dF <- dFIDESZ[county =="Borsod-Abaúj-Zemplén"] ##just for Borsod-Abaúj-Zemplén
dV <- depvar[county == "Borsod-Abaúj-Zemplén",] ## ^^
dI <- pInv[county == "Borsod-Abaúj-Zemplén"]

modBorsodAbaújZemplén <- vglm( dV~dF, family=betabinomial )
summary(modBorsodAbaújZemplén) ##pvalue was 0.209, so none

par(xaxs="i", yaxs="i")
par(las=1, family="serif")
par(cex.axis=0.9, font.lab=2,cex.lab=1.2)
par(mar=c(4,4,0,0)+0.5)
par(lend=1)

plot.new()
plot.window( xlim=c(0,1), ylim=c(0,0.08) )


axis(1)
axis(2)
title(xlab="PA Candidate Support")
title(ylab="Invalidation Rate")

newX = seq(0.30,0.61,length=10000)
prediction = predict(modBorsodAbaújZemplén, newdata=data.frame(dF=newX) )

lines(newX, logit.inv(prediction[,1]), lwd=5 )
lines(newX, logit.inv(prediction[,1]), col="lightblue")

points( dF, dI, pch=21,bg="maroon" )

##############################################################Csongrád

dF <- dFIDESZ[county =="Csongrád"] ##just for Csongrád
dV <- depvar[county == "Csongrád",] ## ^^
dI <- pInv[county == "Csongrád"]

modCsongrád <- vglm( dV~dF, family=betabinomial )
summary(modCsongrád) ##pvalue was 0.0816 so none (i guess with 0.1 SL)

par(xaxs="i", yaxs="i")
par(las=1, family="serif")
par(cex.axis=0.9, font.lab=2,cex.lab=1.2)
par(mar=c(4,4,0,0)+0.5)
par(lend=1)

plot.new()
plot.window( xlim=c(0,1), ylim=c(0,0.08) )


axis(1)
axis(2)
title(xlab="PA Candidate Support")
title(ylab="Invalidation Rate")

newX = seq(0.30,0.61,length=10000)
prediction = predict(modCsongrád, newdata=data.frame(dF=newX) )

lines(newX, logit.inv(prediction[,1]), lwd=5 )
lines(newX, logit.inv(prediction[,1]), col="lightblue")

points( dF, dI, pch=21,bg="maroon" )

################################################################Fejér

dF <- dFIDESZ[county =="Fejér"] ##just for Fejér
dV <- depvar[county == "Fejér",] ## ^^
dI <- pInv[county == "Fejér"]

modFejér <- vglm( dV~dF, family=betabinomial )
summary(modFejér) ##pvalue was 0.639, nada

par(xaxs="i", yaxs="i")
par(las=1, family="serif")
par(cex.axis=0.9, font.lab=2,cex.lab=1.2)
par(mar=c(4,4,0,0)+0.5)
par(lend=1)

plot.new()
plot.window( xlim=c(0,1), ylim=c(0,0.08) )


axis(1)
axis(2)
title(xlab="PA Candidate Support")
title(ylab="Invalidation Rate")

newX = seq(0.30,0.61,length=10000)
prediction = predict(modFejér, newdata=data.frame(dF=newX) )

lines(newX, logit.inv(prediction[,1]), lwd=5 )
lines(newX, logit.inv(prediction[,1]), col="lightblue")

points( dF, dI, pch=21,bg="maroon" )

################################################################Gyor-Moson-Sopron
dF <- dFIDESZ[county =="Gyor-Moson-Sopron"] ##Hajdú-Bihar
dV <- depvar[county == "Gyor-Moson-Sopron",] ## ^^
dI <- pInv[county == "Gyor-Moson-Sopron"]

modGyorMosonSopron <- vglm( dV~dF, family=betabinomial )
summary(modGyorMosonSopron) ##pvalue was 0.858, nada

par(xaxs="i", yaxs="i")
par(las=1, family="serif")
par(cex.axis=0.9, font.lab=2,cex.lab=1.2)
par(mar=c(4,4,0,0)+0.5)
par(lend=1)

plot.new()
plot.window( xlim=c(0,1), ylim=c(0,0.08) )


axis(1)
axis(2)
title(xlab="PA Candidate Support")
title(ylab="Invalidation Rate")

newX = seq(0.30,0.61,length=10000)
prediction = predict(modGyorMosonSopron, newdata=data.frame(dF=newX) )

lines(newX, logit.inv(prediction[,1]), lwd=5 )
lines(newX, logit.inv(prediction[,1]), col="lightblue")

points( dF, dI, pch=21,bg="maroon" )

################################################################Hajdú-Bihar
dF <- dFIDESZ[county =="Hajdú-Bihar"] ##Hajdú-Bihar
dV <- depvar[county == "Hajdú-Bihar",] ## ^^
dI <- pInv[county == "Hajdú-Bihar"]

modHajdúBihar <- vglm( dV~dF, family=betabinomial )
summary(modHajdúBihar) ##pvalue was 0.305, nada

par(xaxs="i", yaxs="i")
par(las=1, family="serif")
par(cex.axis=0.9, font.lab=2,cex.lab=1.2)
par(mar=c(4,4,0,0)+0.5)
par(lend=1)

plot.new()
plot.window( xlim=c(0,1), ylim=c(0,0.08) )


axis(1)
axis(2)
title(xlab="PA Candidate Support")
title(ylab="Invalidation Rate")

newX = seq(0.30,0.61,length=10000)
prediction = predict(modHajdúBihar, newdata=data.frame(dF=newX) )

lines(newX, logit.inv(prediction[,1]), lwd=5 )
lines(newX, logit.inv(prediction[,1]), col="lightblue")

points( dF, dI, pch=21,bg="maroon" )

################################################################Heves
dF <- dFIDESZ[county =="Heves"] ##Hajdú-Bihar
dV <- depvar[county == "Heves",] ## ^^
dI <- pInv[county == "Heves"]

modHeves <- vglm( dV~dF, family=betabinomial )
summary(modHeves) ##pvalue was 0.519, nada

par(xaxs="i", yaxs="i")
par(las=1, family="serif")
par(cex.axis=0.9, font.lab=2,cex.lab=1.2)
par(mar=c(4,4,0,0)+0.5)
par(lend=1)

plot.new()
plot.window( xlim=c(0,1), ylim=c(0,0.08) )


axis(1)
axis(2)
title(xlab="PA Candidate Support")
title(ylab="Invalidation Rate")

newX = seq(0.30,0.61,length=10000)
prediction = predict(modHeves, newdata=data.frame(dF=newX) )

lines(newX, logit.inv(prediction[,1]), lwd=5 )
lines(newX, logit.inv(prediction[,1]), col="lightblue")

points( dF, dI, pch=21,bg="maroon" )

################################################################Komárom-Esztergom
dF <- dFIDESZ[county =="Komárom-Esztergom"] ##Komárom-Esztergom
dV <- depvar[county == "Komárom-Esztergom",] ## ^^
dI <- pInv[county == "Komárom-Esztergom"]

modKomáromEsztergom <- vglm( dV~dF, family=betabinomial )
summary(modKomáromEsztergom) ##pvalue was 0.00717, signifigant

par(xaxs="i", yaxs="i")
par(las=1, family="serif")
par(cex.axis=0.9, font.lab=2,cex.lab=1.2)
par(mar=c(4,4,0,0)+0.5)
par(lend=1)

plot.new()
plot.window( xlim=c(0,1), ylim=c(0,0.08) )


axis(1)
axis(2)
title(xlab="PA Candidate Support")
title(ylab="Invalidation Rate")

newX = seq(0.30,0.61,length=10000)
prediction = predict(modKomáromEsztergom, newdata=data.frame(dF=newX) )

lines(newX, logit.inv(prediction[,1]), lwd=5 )
lines(newX, logit.inv(prediction[,1]), col="lightblue")

points( dF, dI, pch=21,bg="maroon" )

################################################################Nógrád
dF <- dFIDESZ[county =="Nógrád"] ##Nógrád
dV <- depvar[county == "Nógrád",] ## ^^
dI <- pInv[county == "Nógrád"]

modNógrád <- vglm( dV~dF, family=betabinomial )
summary(modNógrád) ##NA... n is small

par(xaxs="i", yaxs="i")
par(las=1, family="serif")
par(cex.axis=0.9, font.lab=2,cex.lab=1.2)
par(mar=c(4,4,0,0)+0.5)
par(lend=1)

plot.new()
plot.window( xlim=c(0,1), ylim=c(0,0.08) )


axis(1)
axis(2)
title(xlab="PA Candidate Support")
title(ylab="Invalidation Rate")

newX = seq(0.30,0.61,length=10000)
prediction = predict(modNógrád, newdata=data.frame(dF=newX) )

lines(newX, logit.inv(prediction[,1]), lwd=5 )
lines(newX, logit.inv(prediction[,1]), col="lightblue")

points( dF, dI, pch=21,bg="maroon" )

################################################################Pest
dF <- dFIDESZ[county =="Pest"] ##Pest
dV <- depvar[county == "Pest",] ## ^^
dI <- pInv[county == "Pest"]

modPest <- vglm( dV~dF, family=betabinomial )
summary(modPest) ##0.128

par(xaxs="i", yaxs="i")
par(las=1, family="serif")
par(cex.axis=0.9, font.lab=2,cex.lab=1.2)
par(mar=c(4,4,0,0)+0.5)
par(lend=1)

plot.new()
plot.window( xlim=c(0,1), ylim=c(0,0.08) )


axis(1)
axis(2)
title(xlab="PA Candidate Support")
title(ylab="Invalidation Rate")

newX = seq(0.30,0.61,length=10000)
prediction = predict(modPest, newdata=data.frame(dF=newX) )

lines(newX, logit.inv(prediction[,1]), lwd=5 )
lines(newX, logit.inv(prediction[,1]), col="lightblue")

points( dF, dI, pch=21,bg="maroon" )

################################################################Somogy
dF <- dFIDESZ[county =="Somogy"] ##Somogy
dV <- depvar[county == "Somogy",] ## ^^
dI <- pInv[county == "Somogy"]

modSomogy <- vglm( dV~dF, family=betabinomial )
summary(modSomogy) ##0.413

par(xaxs="i", yaxs="i")
par(las=1, family="serif")
par(cex.axis=0.9, font.lab=2,cex.lab=1.2)
par(mar=c(4,4,0,0)+0.5)
par(lend=1)

plot.new()
plot.window( xlim=c(0,1), ylim=c(0,0.08) )


axis(1)
axis(2)
title(xlab="PA Candidate Support")
title(ylab="Invalidation Rate")

newX = seq(0.30,0.61,length=10000)
prediction = predict(modSomogy, newdata=data.frame(dF=newX) )

lines(newX, logit.inv(prediction[,1]), lwd=5 )
lines(newX, logit.inv(prediction[,1]), col="lightblue")

points( dF, dI, pch=21,bg="maroon" )

################################################################Szabolcs-Szatmár-Bereg
dF <- dFIDESZ[county =="Szabolcs-Szatmár-Bereg"] ##Szabolcs-Szatmár-Bereg
dV <- depvar[county == "Szabolcs-Szatmár-Bereg",] ## ^^
dI <- pInv[county == "Szabolcs-Szatmár-Bereg"]

modSzabolcsSzatmárBereg <- vglm( dV~dF, family=betabinomial )
summary(modSzabolcsSzatmárBereg) ##0.0806

par(xaxs="i", yaxs="i")
par(las=1, family="serif")
par(cex.axis=0.9, font.lab=2,cex.lab=1.2)
par(mar=c(4,4,0,0)+0.5)
par(lend=1)

plot.new()
plot.window( xlim=c(0,1), ylim=c(0,0.08) )


axis(1)
axis(2)
title(xlab="PA Candidate Support")
title(ylab="Invalidation Rate")

newX = seq(0.30,0.61,length=10000)
prediction = predict(modSzabolcsSzatmárBereg, newdata=data.frame(dF=newX) )

lines(newX, logit.inv(prediction[,1]), lwd=5 )
lines(newX, logit.inv(prediction[,1]), col="lightblue")

points( dF, dI, pch=21,bg="maroon" )

################################################################Tolna
dF <- dFIDESZ[county =="Tolna"] ##Tolna
dV <- depvar[county == "Tolna",] ## ^^
dI <- pInv[county == "Tolna"]

modTolna <- vglm( dV~dF, family=betabinomial )
summary(modTolna) ##VERY small p.. 7.86e-07

par(xaxs="i", yaxs="i")
par(las=1, family="serif")
par(cex.axis=0.9, font.lab=2,cex.lab=1.2)
par(mar=c(4,4,0,0)+0.5)
par(lend=1)

plot.new()
plot.window( xlim=c(0,1), ylim=c(0,0.08) )


axis(1)
axis(2)
title(xlab="PA Candidate Support")
title(ylab="Invalidation Rate")

newX = seq(0.30,0.61,length=10000)
prediction = predict(modTolna, newdata=data.frame(dF=newX) )

lines(newX, logit.inv(prediction[,1]), lwd=5 )
lines(newX, logit.inv(prediction[,1]), col="lightblue")

points( dF, dI, pch=21,bg="maroon" )

################################################################Vas
dF <- dFIDESZ[county =="Vas"] ##Vas
dV <- depvar[county == "Vas",] ## ^^
dI <- pInv[county == "Vas"]

modVas <- vglm( dV~dF, family=betabinomial )
summary(modVas) ##0.924

par(xaxs="i", yaxs="i")
par(las=1, family="serif")
par(cex.axis=0.9, font.lab=2,cex.lab=1.2)
par(mar=c(4,4,0,0)+0.5)
par(lend=1)

plot.new()
plot.window( xlim=c(0,1), ylim=c(0,0.08) )


axis(1)
axis(2)
title(xlab="PA Candidate Support")
title(ylab="Invalidation Rate")

newX = seq(0.30,0.61,length=10000)
prediction = predict(modVas, newdata=data.frame(dF=newX) )

lines(newX, logit.inv(prediction[,1]), lwd=5 )
lines(newX, logit.inv(prediction[,1]), col="lightblue")

points( dF, dI, pch=21,bg="maroon" )

################################################################Veszprém
dF <- dFIDESZ[county =="Veszprém"] ##Veszprém
dV <- depvar[county == "Veszprém",] ## ^^
dI <- pInv[county == "Veszprém"]

modVeszprém <- vglm( dV~dF, family=betabinomial )
summary(modVeszprém) ##0.554

par(xaxs="i", yaxs="i")
par(las=1, family="serif")
par(cex.axis=0.9, font.lab=2,cex.lab=1.2)
par(mar=c(4,4,0,0)+0.5)
par(lend=1)

plot.new()
plot.window( xlim=c(0,1), ylim=c(0,0.08) )


axis(1)
axis(2)
title(xlab="PA Candidate Support")
title(ylab="Invalidation Rate")

newX = seq(0.30,0.61,length=10000)
prediction = predict(modVeszprém, newdata=data.frame(dF=newX) )

lines(newX, logit.inv(prediction[,1]), lwd=5 )
lines(newX, logit.inv(prediction[,1]), col="lightblue")

points( dF, dI, pch=21,bg="maroon" )

################################################################Zala
dF <- dFIDESZ[county =="Zala"] ##Zala
dV <- depvar[county == "Zala",] ## ^^
dI <- pInv[county == "Zala"]

modZala <- vglm( dV~dF, family=betabinomial )
summary(modZala) ##0.00573

par(xaxs="i", yaxs="i")
par(las=1, family="serif")
par(cex.axis=0.9, font.lab=2,cex.lab=1.2)
par(mar=c(4,4,0,0)+0.5)
par(lend=1)

plot.new()
plot.window( xlim=c(0,1), ylim=c(0,0.08) )


axis(1)
axis(2)
title(xlab="PA Candidate Support")
title(ylab="Invalidation Rate")

newX = seq(0.30,0.61,length=10000)
prediction = predict(modZala, newdata=data.frame(dF=newX) )

lines(newX, logit.inv(prediction[,1]), lwd=5 )
lines(newX, logit.inv(prediction[,1]), col="lightblue")

points( dF, dI, pch=21,bg="maroon" )

################################################################OVERALL
bigmod <- vglm( depvar~dFIDESZ, family=betabinomial )
summary(bigmod) ##0.00169, is some DI


par(xaxs="i", yaxs="i")
par(las=1, family="serif")
par(cex.axis=0.9, font.lab=2,cex.lab=1.2)
par(mar=c(4,4,0,0)+0.5)
par(lend=1)

plot.new()
plot.window( xlim=c(0,1), ylim=c(0,0.03) )


axis(1)  
axis(2)
title(xlab="Party Support")
title(ylab="Invalidation Rate")

newX = seq(0.25,0.63,length=10000)
prediction = predict(bigmod, newdata=data.frame(dFIDESZ=newX) )

logit.inv( predict(bigmod, newdata=data.frame(dFIDESZ=0), interval="confidence" ) )

lines(newX, logit.inv(prediction[,1]), lwd=5 )
lines(newX, logit.inv(prediction[,1]), col="purple")

points( dFIDESZ, pInv, pch=21,bg="orange" )

#######################################################################
detach(dt)
which(dt$DK==0)
dt = dt[-which(dt$DK==0), ]
attach(dt)
summary(dt)
dDK <- DK/validVotes
pInv <- invalidVotes/totalVotes
depvar <- cbind(invalidVotes, validVotes)
bigmod2 <- vglm( depvar~dDK, family=betabinomial )
summary(bigmod2) ##0.00169, is some DI

par(xaxs="i", yaxs="i")
par(las=1, family="serif")
par(cex.axis=0.9, font.lab=2,cex.lab=1.2)
par(mar=c(4,4,0,0)+0.5)
par(lend=1)

plot.new()
plot.window( xlim=c(0,1), ylim=c(0,0.04) )


axis(1)  
axis(2)
title(xlab="Party Support")
title(ylab="Invalidation Rate")

newX = seq(0.05,0.61,length=10000)
prediction = predict(bigmod2, newdata=data.frame(dDK=newX) )

lines(newX, logit.inv(prediction[,1]), lwd=5 )
lines(newX, logit.inv(prediction[,1]), col="white")

points( dDK, pInv, pch=21,bg="green" )

############################################################3
detach(dt)
which(dt$JOBBIK==0)
dt = dt[-which(dt$JOBBIK==0), ]
attach(dt)
summary(dt)
bigmod3 <- vglm( depvar~dJOBBIK, family=betabinomial )
summary(bigmod3) ##0.00169, is some DI

par(xaxs="i", yaxs="i")
par(las=1, family="serif")
par(cex.axis=0.9, font.lab=2,cex.lab=1.2)
par(mar=c(4,4,0,0)+0.5)
par(lend=1)

plot.new()
plot.window( xlim=c(0,1), ylim=c(0,0.03) )


axis(1)  
axis(2)
title(xlab="Party Support")
title(ylab="Invalidation Rate")

newX = seq(0.05,0.45,length=10000)
prediction = predict(bigmod3, newdata=data.frame(dJOBBIK=newX) )

lines(newX, logit.inv(prediction[,1]), lwd=5 )
lines(newX, logit.inv(prediction[,1]), col="lightblue")

points( dJOBBIK, pInv, pch=21,bg="maroon" )
############################################################3
detach(dt)
which(dt$LMP==0)
dt = dt[-which(dt$LMP==0), ]
attach(dt)
summary(dt)
dLMP <- LMP/validVotes
pInv <- invalidVotes/totalVotes
depvar <- cbind(invalidVotes, validVotes)
bigmod4 <- vglm( depvar~dLMP, family=betabinomial )
summary(bigmod4) ##0.00169, is some DI

par(xaxs="i", yaxs="i")
par(las=1, family="serif")
par(cex.axis=0.9, font.lab=2,cex.lab=1.2)
par(mar=c(4,4,0,0)+0.5)
par(lend=1)

plot.new()
plot.window( xlim=c(0,1), ylim=c(0,0.08) )


axis(1)  
axis(2)
title(xlab="PA Candidate Support")
title(ylab="Invalidation Rate")

newX = seq(0.05,0.45,length=10000)
prediction = predict(bigmod4, newdata=data.frame(dLMP=newX) )

lines(newX, logit.inv(prediction[,1]), lwd=5 )
lines(newX, logit.inv(prediction[,1]), col="lightblue")

points( dLMP, pInv, pch=21,bg="maroon" )

############################################################3
detach(dt)
which(dt$MSZP==0)
dt = dt[-which(dt$MSZP==0), ]
attach(dt)
summary(dt)
dMSZP <- MSZP/validVotes
pInv <- invalidVotes/totalVotes
depvar <- cbind(invalidVotes, validVotes)
bigmod5 <- vglm( depvar~dMSZP, family=betabinomial )
summary(bigmod5) ##0.00169, is some DI

par(xaxs="i", yaxs="i")
par(las=1, family="serif")
par(cex.axis=0.9, font.lab=2,cex.lab=1.2)
par(mar=c(4,4,0,0)+0.5)
par(lend=1)

plot.new()
plot.window( xlim=c(0,1), ylim=c(0,0.03) )


axis(1)  
axis(2)
title(xlab="Party Support")
title(ylab="Invalidation Rate")

newX = seq(0.0,0.6,length=10000)
prediction = predict(bigmod5, newdata=data.frame(dMSZP=newX) )

lines(newX, logit.inv(prediction[,1]), lwd=5 )
lines(newX, logit.inv(prediction[,1]), col="lightblue")

points( dMSZP, pInv, pch=21,bg="maroon" )
