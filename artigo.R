#cardio
pdf(file="pollu_mp10grey.pdf", onefile=T)
pdf.options(colormodel="gray")
par(mfrow=c(1,1))


plotCI(c(15,25,35,65,75,85),c(mp10[7,1],mp10[7,2],mp10[7,5],mp10[7,3],mp10[7,4],mp10[7,6]),li=c(mp10[6,1],mp10[6,2],mp10[6,5],mp10[6,3],mp10[6,4],mp10[6,6]),ui=c(mp10[8,1],mp10[8,2],mp10[8,5],mp10[8,3],mp10[8,4],mp10[8,6]),pch=c(15,16,17,15,16,17),xaxt="n",ylim=c(-0.5,2.5),xlab="",yaxt="n",ylab="Mudança percentual (%)",xlim=c(0,100),col=c("#000000","#575757","#969696" ,"#000000","#575757","#969696","#000000","#575757","#969696"),slty=c(1,5,3,1,5,3),lwd=1.5)
axis(2,at=pretty(-1:2.5),labels=chartr(".", ",", as.character(pretty(-1:2.5)
)))
text(25,-0.3,expression(cardiovascular),cex=1.3)
text(75,-0.3,expression(respiratória),cex=1.3)
#text(125,-0.8,expression(O[3]),cex=1.3)
#mtext("Mudança percentual no risco",side=3,line=2.2,cex=1.2)
#mtext(expression(paste("por 10",mu,"g/",m^3," de aumento na concentração do poluente")),side=3,line=1,cex=1.2)
#mtext("doenças cardiovasculares",side=3,line=0.3)

abline(h=0,col="#000000",lwd=1.5,lty=2)

legend(30,2.2, bty="n", lwd=1.5, col=c("#000000","#575757","#969696"), legend=expression("Série temporal",paste(italic(Case-crossover)," - dia da semana"),paste(italic(Case-crossover)," - temperatura")),ncol=1,cex=0.7,lty=c(1,5,3,1,5,3),pch=c(15,16,17),seg.len = 3)

dev.off()


pdf(file="temp_mp10.pdf",onefile=T,paper="a4r",width=18)
par(mfrow=c(1,2))
plot(0, type="n", bty="n", main="a", xlab=expression("Temperatura ("~degree~C~")"),ylim=c(-6,12), lwd=1.5, xlim=c(5,30),ylab="Mudança percentual no risco (%)")

abline(h=0,col="black",lwd=1.5,lty=2)

polygon(c(xcA$cardio.tmed,rev(xcA$cardio.tmed)),c(xcA$lotemp1,rev(xcA$hitemp1)),col="#00000066"
,border=NA)
points(xcA$cardio.tmed,xcA$temp1,type="l",lwd=2.5,lty=1,col="#000000")
polygon(c(xcA$cardio.tmed,rev(xcA$cardio.tmed)),c(xcA$lotemp2,rev(xcA$hitemp2)),col="#57575755"
,border=NA)
points(xcA$cardio.tmed,xcA$temp2,type="l",lwd=6.5,lty=1,col="#575757")
#points(xcA$cardio.tmed,xcA$hitemp2,type="l",lty=2,lwd=1.1,col="#575757")
#points(xcA$cardio.tmed,xcA$lotemp2,type="l",lty=2,lwd=1.1,col="#575757")

points(xcA$cardio.tmed,xcA$temp13,type="l",lwd=2.5,lty=3,col="#969696")
points(xcA$cardio.tmed,xcA$hitemp13,type="l",lty=3,lwd=1.1,col="#969696")
points(xcA$cardio.tmed,xcA$lotemp13,type="l",lty=3,lwd=1.1,col="#969696")

legend(15,-4.5, bty="n", lwd=c(2.5,6.5,2.5), col=c("#000000","#575757","#969696"), legend=expression("Série temporal",paste(italic(Case-crossover)," - dia da semana"),paste(italic(Case-crossover)," - temperatura")),ncol=1,cex=0.7,lty=c(1,1,3),seg.len = 4)

#dev.off()


#respira+mp10

#pdf(file="temp_effect4.pdf",onefile=T)
plot(0, type="n", bty="n", main="b", xlab=expression("Temperatura ("~degree~C~")"),ylim=c(-6,12), lwd=1.5, xlim=c(5,30),ylab="Mudança percentual no risco (%)")
#mtext("Função de alisamento para Temp. média",3,line=2.5,cex=1.0)
#mtext("móvel 2 dias",3,line=1.5,cex=1.0)
#mtext(expression("doenças respiratórias - modelo MP"[10]),3,line=0.1,cex=0.8)
abline(h=0,col="black",lwd=1.5,lty=2)

polygon(c(xrA$respira.tmedmov2,rev(xrA$respira.tmedmov2)),c(xrA$lotemp3,rev(xrA$hitemp3)),col="#00000066"
,border=NA)
points(xrA$respira.tmedmov2,xrA$temp3,type="l",lwd=2.5,col="#000000")
polygon(c(xrA$respira.tmedmov2,rev(xrA$respira.tmedmov2)),c(xrA$lotemp4,rev(xrA$hitemp4)),col="#57575755"
,border=NA)
points(xrA$respira.tmedmov2,xrA$temp4,lty=1,type="l",lwd=6.5,col="#575757")
points(xrA$respira.tmedmov2,xrA$temp14,lty=3,type="l",lwd=2.5,col="#969696")
points(xrA$respira.tmedmov2,xrA$hitemp14,lty=3,type="l",lwd=1.1,col="#969696")
points(xrA$respira.tmedmov2,xrA$lotemp14,lty=3,type="l",lwd=1.1,col="#969696")

legend(15.8,-4.5, bty="n", col=c("#000000","#575757","#969696"), legend=expression("Série temporal",paste(italic(Case-crossover)," - dia da semana"),paste(italic(Case-crossover)," - temperatura")),ncol=1,cex=0.7,lty=c(1,1,3),lwd=c(2.5,6.5,2.5),seg.len = 4)

dev.off()
