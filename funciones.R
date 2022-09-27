# Función para graficar las capas 
graficarcapaC<-function(radio,capa, titulo){
  
  plot(NULL, xlim=c(0,2*radio), 
       ylim=c(0,2*radio), main=titulo,asp=1,
       xlab="",ylab="",axes=F,
       yaxt="n",xaxt="n",cex.main = 3)
  
  mtext(TeX("$\\mu m$"), side=1, line=4, cex=3)
  
  axis(2,cex.axis=2)
  axis(1,cex.axis=2)
  draw.circle(x=radio,y=radio,radius=radio,col="black") # capa grande
  
  for(i in 1:(dim(capa)[1])){
    if(capa$estado[i]==1){
      draw.circle(capa$x[i],capa$y[i],radius=capa$r[i],border="black",col="white")
    }
  }
}
