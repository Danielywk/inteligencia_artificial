library(dplyr) # cargando libreria
library(lubridate) #cargando libreria
library(nortest)

rendimientos<-read.csv("rendimientos.csv",header=TRUE)

#p1<-mean(rendimientos$fibraJVN)
p2<-mean(rendimientos$fibraw1k)
#p3<-mean(rendimientos$Bitcoin)
#p4<-mean(rendimientos$Cardano)
p5<-mean(rendimientos$etc_usd)
p6<-mean(rendimientos$Aluminio)
#p7<-mean(rendimientos$Cobre)
p8<-mean(rendimientos$Paladio)
p9<-mean(rendimientos$Plata)
p10<-mean(rendimientos$Etf)
activos<-(matrix(c(p2,p5,p6,p8,p9,p10)))*1

vba<-matrix(6,6)
ma<-matrix(1:1,6,6)
vba<-select(rendimientos,-Mes,-fibraJVN,-Bitcoin,-Cardano,-Cobre)

timepoini<-proc.time()
activos<-as.numeric(unlist(activos))

ma<-cov(vba)
macov<-matrix(c(ma),6,6)

"
colocando las varible a emplear para el programa

"
m1<-matrix(1:100,100,6)
mn1<-matrix(1:100,100,6)
mn2<-matrix(1:100,100,6)
mn3<-matrix(1:100,100,6)
v<-matrix(1:100,100,6)
mn4<-matrix(1:100,100,6)

hijos<-matrix(1:60,60,6)
hijosn<-matrix(1:60,60,6)
funcionObj1<-matrix()
estudiod<-matrix()
objetivo<-matrix()
objetivo1<-matrix()

lec1<-seq(1,6000,100)

pesos<-read.csv("sujetos.csv",header=TRUE)

for (repi in 1:1) {
  
  nn<-lec1[repi]
  nn1<-lec1[repi]+99
  
  m1<-pesos[nn:nn1,]   
  
  m1<-select(m1,-X)
  m1<-data.matrix(m1)
  
  "for(j in 1:6)
{
  
  for(i in 1:100)
  {
    
    m1[i,j]<-runif(1,0,1)
    
    
  }
  
}"
  
  #####normalizar
  for(i in 1:100)
  {
    
    aa<-m1[i,]
    b<-sum(aa)
    
    mn1[i,]<-aa/b
    
  }
  for (ciclo in 1:1500) {
    #####OBTENIENDO GANANCIA
    for(i in 1:100)  
    {
      
      resu<-0
      r<-0
      for (j in 1:6) {
        
        r<-(activos[j]*mn1[i,j])+r
        # r1[i]<-sum(r)
        
      }
      
      ######OBTIENDO RIESGO
      for (t in 1:6){
        
        for (h in 1:6){
          
          resu<-(macov[t,h]*mn1[i,t]*mn1[i,h])+resu
          
        }
        
      }
      ########funcion objetivo lista
      objetivo[i]<-r/resu
    }
    fobjetivo<-as.numeric(unlist(objetivo))
    funcionObj1[ciclo]<-max(fobjetivo)
    for(i in 1:100){
      al1<-round(runif(1,1,100))
      al2<-round(runif(1,1,100))
      while(al1==al2)
      {
        al2<-round(runif(1,1,100))
      }
      al3<-round(runif(1,1,100))
      while ((al1==al2)|(al1==al3) | (al2==al3)) {
        
        al3<-round(runif(1,1,100))
        
      }
      #F<-runif(1,0,0.4)
      F<-runif(1,0,1)
      v[i,]<- abs(mn1[al1,]-F*(mn1[al2,]-mn1[al3,]))
    }
    for(i in 1:100)
    {
      
      aa<-v[i,]
      b<-sum(aa)
      
      mn2[i,]<-aa/b
      
    }
    ###Recombinando a los asquerosos mutantes
    for (j in 1:100) {
      
      Cr<-runif(1,0,1) #antes era 0.5
      #Cr<-0.1
      for(i in 1:6){
        beta<-runif(1,0,1)
        if(beta<=Cr)
        {
          
          mn3[j,i]<-mn2[j,i]
        } else
        {
          mn3[j,i]<-mn1[j,i]
          
        }
      } 
    }
    for(i in 1:100)
    {
      aa<-mn3[i,]
      b<-sum(aa)
      mn4[i,]<-aa/b
      
    }
    #####OBTENIENDO GANANCIA
    for(i in 1:100) 
    {
      resu<-0
      r<-0
      for (j in 1:6) {
        
        r<-(activos[j]*mn4[i,j])+r
        # r1[i]<-sum(r)
        
      }
      ######OBTIENDO RIESGO
      for (t in 1:6){
        
        for (h in 1:6){
          
          resu<-(macov[t,h]*mn4[i,t]*mn4[i,h])+resu
          
        }
        
      }
      objetivo1[i]<-r/resu
    }
    fobjetivo2<-as.numeric(unlist(objetivo1))
    for(i in 1:100)
    {
      
      if(fobjetivo2[i]>=fobjetivo[i])
      {
        
        mn1[i,]<-mn4[i,]
      }  
      
      else
      {
        mn1[i,]<-mn1[i,]
        
      }
    }
  }
  estudiod[repi]<-max(funcionObj1)
}
  #ressi[repi]<-funcionObj1[100]
  resu1<-0
  for (t in 1:6){
    for (h in 1:6){
      resu1<-(macov[t,h]*mn1[1,t]*mn1[1,h])+resu1
    }
    
  }
  resu1<-resu1*100
  #resu11[repi]<-resu1
  r1<-0
  for (j in 1:6) {
    r1<-(activos[j]*mn1[1,j])+r1
    #r1[i]<-sum(r) 
  }
  r1<-r1*100
  #r11[repi]<-r1
  #montos[repi,]<-mn1[1,]
  algoritmo4<-funcionObj1
  #rela<-matrix(c(ressi,resu11,r11,montos),40,9)
  #write.csv(rela,file="evolucionEd2020.csv")
  tiempofin<-proc.time()-timepoini
  algoritmo4<-funcionObj1
  x<-seq(1:10000)
  plot(x,funcionObj1,type="l",log = 'x')
  tiempofin<-proc.time()-timepoini