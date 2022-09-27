# Cargando capas generadas
CAPA100F5 <- read.csv("datos/CAPA100F5.csv")
CAPA200F5 <- read.csv("datos/CAPA200F5.csv")
CAPA300F5 <- read.csv("datos/CAPA300F5.csv")
CAPA400F5 <- read.csv("datos/CAPA400F5.csv")
CAPA500F5 <- read.csv("datos/CAPA500F5.csv")
CAPA600F5 <- read.csv("datos/CAPA600F5.csv")
CAPA700F5 <- read.csv("datos/CAPA700F5.csv")
CAPA800F5 <- read.csv("datos/CAPA800F5.csv")

coulterporometerdata<- read.csv("datos/coulterporometerdata.csv")

#Cargando distribución de particulas contaminantes
{
  porosTT<-read.csv("datos/poros.dat",header = FALSE)
  distribucionPa<-porosTT[30:50,]
  
  F5<-porosTT[1:29,]
  F5$V2<-F5$V2/100
  F5$V2<-1-F5$V2
  F5$V1<-F5$V1/2
  F5<-F5[order(F5$V1), ]
  
  
  distribucionPa$V2<-distribucionPa$V2/100
  distribucionPa$V2[21] <- 1
}

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


# Función que genera la muestra de partículas contaminantes
GenerarMuestra<-function(distribucion,n){
  u<-runif(n,min = 0,max = 1)
  muestra<-rep(0,n)
  #print(u)
  for(i in 1:n){
    for(j in 1:(dim(distribucion)[1]-1)){
      if(u[i] > distribucion[,2][j] & u[i]<=distribucion[,2][j+1] ){
        muestra[i]<-distribucion[,1][j]
        break
      }
    }
  }
  return(muestra)
}

# Función para lanzar las partículas
lanzarparticulasCC2<-function(capa,radio,muestra,numCAP){
  #numCAP número de capa sobre la cual es lanzada la part
  #graficarcapaC(radio,capa)
  # rojo capturadas
  # azul libres
  conteo<-0 # contador de particulas
  # capa<-capa[order(capa$r,decreasing = T),]
  capt<-muestra
  # 1 la particula o poro esta libre
  # 0 la particula esta capturada, el poro esta bloqueado
  
  muestra<-muestra[order(muestra[,2],decreasing = T),]# colocar las part libres primero
  NoCap<-sum(muestra[,2]) # numero de particulas no capturadas
  
  for(j in 1:NoCap){
    # generar posicion aleatoria sobre capa y definir region de busqueda
    {
      
      repeat{ 
        x<- runif(1,0,2*radio) 
        y<- runif(1,0,2*radio) 
        if(!((x-radio)^2 + (y-radio)^2 > (radio)^2)){ break } # Negation is crucial here!
      }

      rrr1<-x + 2*max(capa$r)
      rrr2<-x - 2*max(capa$r)
      rrr3<-y + 2*max(capa$r)
      rrr4<-y - 2*max(capa$r)
      # subconjunto de busqueda
      
      sub<-subset(capa, capa$x <= rrr1 & capa$x >= rrr2 & capa$y <= rrr3 & capa$y >= rrr4)
      kk<-dim(sub)[1] # cantidad de poros en la región de busqueda
    }
    if(kk==0){ # no hay ningun poro cerca, la particula es capturada
      #points(x=x,y=y,col ="red",pch=19,cex=0.20)
      muestra[j,2]<-0
      muestra[j,3]<-x
      muestra[j,4]<-y
      muestra[j,5]<-numCAP
    }else{ # buscar si la particula cae en un poro o no
      cc<-0 # conteo de poros en que la particula no esta
      for(i in 1:kk){ 
        dC<-sqrt( (sub$x[i]-x)^2 + (sub$y[i]-y)^2 ) # distancia entre el centro del poro y el punto
        if(dC < sub$r[i]){ # la particula esta adentro del poro
          if(sub$estado[i]==0){# el poro está obstruido, la particula queda capturada
            #points(x=x,y=y,col ="red",pch=19,cex=0.20)
            muestra[j,2]<-0
            muestra[j,3]<-x
            muestra[j,4]<-y
            muestra[j,5]<-numCAP
            break
          }else{ # el poro esta libre
            
            if(muestra[j,1] >= 2*sub$r[i]){ # la particula es mas grande que el poro, particula capturada
              #points(x=x,y=y,col ="red",pch=19,cex=0.20)
              muestra[j,2]<-0
              muestra[j,3]<-x
              muestra[j,4]<-y
              muestra[j,5]<-numCAP
              iii<-as.numeric(rownames(sub[i,]))
              capa$estado[iii]<-0
              conteo<-conteo+1
              #cat(conteo,"indice que cambia", iii,"\n")
              break
            }else{ # la particula es libre
              #points(x=x,y=y,col ="blue",pch=19,cex=0.20)
              break
            } 
            
          }# fin de else del if(sub$estado[i]==0)
          
          
        }else{ # la particula esta afuera del poro aumentar el conteo de poros revisados
          cc<-cc+1
        }
      }# fin del for que recorre los poros del subset
      
      if(cc==kk){ # esta afuera de todos los poros de la busqueda, queda capturado
        #points(x=x,y=y,col ="red",pch=19,cex=0.20)
        muestra[j,2]<-0 
        muestra[j,3]<-x
        muestra[j,4]<-y
        muestra[j,5]<-numCAP
      }
    }# fin de else de if(kk==0)
    
    
  }# fin del for que recorre particulas no capturadas
  
  porosidad<-sum(pi*capa$r^2*capa$estado)/(pi*radio^2) # cambio de la porosidad
  eff<-(sum(muestra[,2])/NoCap)*100 # porcentaje de particulas no capturadas
  retorno<-list(muestra,capa,100-eff,NoCap,porosidad)
  return(retorno)
  
} 

#Eficiencia en función del tamaño de muestra
Eficiencia <- function(numero_simulaciones=50, radio, capa){
  area <- pi*radio^2*1e-12
  NUM   <- ceiling( 1.7519e9*0.10*5*area)
  vectorEF <- c()
  for(i in 1: numero_simulaciones ){
    tamano<-GenerarMuestra(distribucionPa,NUM)
    estado<-rep(1,NUM)
    capaCapt<- rep(0,NUM)
    coorx<-rep(0,NUM) # coordenada en x de la captura
    coory<-rep(0,NUM) # coordenada en y de la captura
    particulas<-cbind(tamano,estado,coorx,coory,capaCapt)
    
    resultado <- lanzarparticulasCC2(capa, radio, particulas, 0)
    
    vectorEF[i] <- resultado[[3]]/100
  }
  S2<- var(vectorEF)
  z <- qnorm(0.025,lower.tail = F)
  ell <- mean(vectorEF)
  
  LI <- round(ell - z*sqrt(S2/numero_simulaciones),4)
  LS <- round(ell + z*sqrt(S2/numero_simulaciones),4)
  cat("Capa de radio: ", radio," Número de partículas lanzadas: ",NUM)
  cat("\nEficiencia: ", round(ell,4),"\nVarianza: ",round(S2,5),
      "\nIntervalo de confianza: ",LI,LS )
  
  invisible(c(ell, LI, LS))
  
}

# Función que genera la figura 11 
Figura11 <- function(){
  radiosT <- c(TeX("Modelo radio 100 ($\\mu m$)"),
               TeX("Modelo radio 200 ($\\mu m$)"),
               TeX("Modelo radio 300 ($\\mu m$)"),
               TeX("Modelo radio 400 ($\\mu m$)"),
               TeX("Modelo radio 500 ($\\mu m$)"),
               TeX("Modelo radio 600 ($\\mu m$)"),
               TeX("Modelo radio 700 ($\\mu m$)"),
               TeX("Modelo radio 800 ($\\mu m$)")
  )
  colores <- rainbow(10)
  {
    plot(density(coulterporometerdata$x,bw=0.29), col=colores[1],
         main = "",axes=F,lwd=3,
         xlab = "",
         ylab = "",
         yaxt="n",xaxt="n")
    mtext(TeX("Radio de poros ($\\mu m$)"), side=1, line=4, cex=3)
    
    axis(2,cex.axis=2)
    axis(1,cex.axis=2)
    
    
    lines(density(CAPA100F5$r), ylim=c(0,0.24),
          col=colores[2], lwd=3)
    
    
    lines(density(CAPA200F5$r),
          col=colores[3],lwd=3)
    
    lines(density(CAPA300F5$r),
          col=colores[4],lwd=3)
    
    lines(density(CAPA400F5$r),
          col=colores[5],lwd=3)
    
    lines(density(CAPA500F5$r),
          col=colores[6],lwd=3)
    
    lines(density(CAPA600F5$r),
          col=colores[7],lwd=3)
    
    lines(density(CAPA700F5$r,bw=0.29),
          col=colores[8],lwd=3)
    
    
    lines(density(CAPA800F5$r),
          col=colores[9],lwd=3)
    
    legend("topleft",bty = "n",cex=1,
           legend = c("Experimental Coulter Porometer",
                      radiosT[1:4]),
           lty = 1, col = colores[1:5], lwd = 3)
    
    legend("topright",bty = "n",cex=1,
           legend = radiosT[5:8],
           lty = 1, col = colores[6:9], lwd = 3)
  }
  
}

  
  
  
  