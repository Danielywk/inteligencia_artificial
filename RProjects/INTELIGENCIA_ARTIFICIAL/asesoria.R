
# Asesoria de cómo programar un AG

# Definiendo FO
# Espacio de declaraciones
# Aquí declarar variables
x<-matrix(1,100,100,1)
y<-matrix(1,100,100,1)
z<-matrix(1,100,100,1)

# Tamaño liibre

funcionObjetivo<-matrix()

x<-runif(100,1,300)
y<-runif(100,1,300)
z<-runif(100,1,300)

xord<-runif(1,100,100,1)
yord<-runif(1,100,100,1)
zord<-runif(1,100,100,1)

xh<-runif(1, 80, 80, 1)
yh<-runif(1, 80, 80, 1)
zh<-runif(1, 80, 80, 1)

for (i in 1:100) {
  funcionObjetivo[i]<-((x[i]^2)*y[i])+((z[i]^3)*x[i])+(y[i]*z[i])+1
}
 funcionObjetivoOr<-sort(funcionObjetivo, decreasing = TRUE)
 
 for (i in 1:100) {
   for (j in 1:100) {
   if(funcionObjetivoOr[i]==funcionObjetivo[j]){
     xord[i]<-x[j]
     yord[i]<-y[j]
     zord[i]<-z[j]
   }
   }
 }
 a=seq(1:80, by=2)
 for (ww in 0) {
   
 
 c1<-round(runif(1.1,100))
 c2<-round(runif(1.1,100))
 
 while (c1==c2) {
   c2<-round(runif(1,1,100))
 }
 
 c3<-round(runif(1.1,100))
 while ((c1==c3)||(c2==c3)) {
   c2<-round(runif(1,1,100))
 }
 
 cv<-matrix(c(c1,c2c3))
 cvo<-sort(cv, decreasing = FALSE)
 
 xh[ww,]<-xord[cvo[1]]
 xh[ww+1,]<-xord[cvo[2]]
 
 yh[ww,]<-yord[cvo[1]]
 yh[ww+1,]<-yord[cvo[2]]
 
 zh[ww,]<-zord[cvo[1]]
 zh[ww+1,]<-zord[cvo[2]]
 }