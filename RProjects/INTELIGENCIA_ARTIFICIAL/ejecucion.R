# Cargar librerias
library(dplyr)
library(lubridate)
library(nortest)

rendimientos <- read.csv("rendimientos.csv", header = TRUE)

p1 <- mean(rendimientos$cemex)
p2 <- mean(rendimientos$facebook)
p3 <- mean(rendimientos$amazon)
p4 <- mean(rendimientos$apple)
p5 <- mean(rendimientos$bimbo)
p6 <- mean(rendimientos$elektra)

activos <- (matrix(c(p1, p2, p3, p4, p5, p6))) * 1
vba <- matrix(6, 6)
ma <- matrix(1:1, 6, 6)
vba <- select(rendimientos, -Mes)

activos <- as.numeric(unlist(activos))
ma <- cov(vba)
macov <- matrix(c(ma), 6, 6)

m1 <- matrix(1:100, 100, 6)
mn1 <- matrix(1:100, 100, 6)
mn2 <- matrix(1:100, 100, 6)
mn3 <- matrix(1:100, 100, 6)
hijos <- matrix(1:60, 60, 6)
hijosn <- matrix(1:60, 60, 6)

funcionObj1 <- matrix()
objetivoO <- matrix()
fobjetivo <- matrix()
objetivo <- matrix()
vec <- matrix()
estudioge <- matrix()

for (repi in 1:1) {
  for (j in 1:6)
  {
    for (i in 1:100)
    {
      m1[i, j] <- runif(1, 0, 1)
    }
  }

  # normalizar
  for (i in 1:100) {
    aa <- m1[i,]
    b <- sum(aa)
    mn1[i,] <- aa / b
  }
  # Iteraciones
  timepoini <- proc.time()
  for (ciclo in 1:1000) {
    for (i in 1:100)
    {
      resu <- 0
      r <- 0
      for (j in 1:6) {
        r <- (activos[j] * mn1[i, j]) + { r }
        # r1[i]<-sum(r)
      }
      ######ORTIENDO RIESGO

      for (t in 1:6) {

        for (h in 1:6) {

          resu <- (macov[t, h] * mn1[i, t] * mn1[i, h]) + resu
        }
      }
      ####funcion objetivo listo
      objetivo[i] <- r / resu
    }


    fobjetivo <- as.numeric(unlist(objetivo))
    funcionObj1[ciclo] <- max(fobjetivo)

    # Ordenamiento
    objetivoO <- sort(fobjetivo, decreasing = TRUE) ##ordenar padres

    for (i in 1:100) {
      for (j in 1:100) {
        if (objetivoO[i] == fobjetivo[j])
        {
          mn2[i,] <- mn1[j,]
        }
      }
    }

    # SELECCIÓN
    maxru = max(objetivoO);
    minru = min(objetivoO);
    porcr = 60

    for (k in 1:porcr) {
      rul = minru + ((maxru - minru) * (runif(1, 0, 1)))
      n = 100

      while (n > 1) {
        if ((rul > objetivoO[n]) & (rul < objetivoO[n - 1])) {
          mn3[k,] <- mn2[n,]
          n <- 1
        }else {
          n = n - 1
        }
      }
    }
    # Elementos que se van a cruzar de 2 en 2
    pr <- 2
    im <- 1
    k <- 0
    valor_alpha <- runif(1, 0, 1)
    #valor_alpha<-0.5
    #alpha<-0.7

    for (i in seq(1, 60, by = 2)) {
      for (j in 1:6) {

        a <- max(mn3[i, j], mn3[i + 1, j])
        b <- min(mn3[i, j], mn3[i + 1, j])
        I = a - b
        limi = abs((b - (I * valor_alpha)))
        lims = (a + (I * valor_alpha))
        hijos[i, j] = runif(1, min = limi, max = lims)
        hijos[i + 1, j] = runif(1, min = limi, max = lims)

        #hijos[i,j]-(mn3[i,j]*valor_alpha)+ ((1-valor_alpha)*mn3[i+1, j])
        #hijos[i+1, j]-(mn3[i+1, j]*valor_alpha)+((1-valor_alpha)*mn3[i,j])
      }
    }

    for (i in 1:60) {
      aa <- abs(hijos[i,])
      b <- sum(aa)
      hijosn[i,] <- aa / b
    }

    # Mutación: elección de 3 cromosomas
    inde1 = round(runif(1, 1, 59))
    inde2 = round(runif(1, 1, 59))
    while (inde1 == inde2) {
      inde2 = round(runif(1, 1, 59))
    }
    inde3 = round(runif(1, 1, 59))

    while ((inde1 == inde3) || (inde2 == inde3)) {
      inde3 = round(runif(1, 1, 59))
    }

    # Mutación uniforme
    for (j in 1:6) {
      vec[j] <- hijosn[inde1, j]
    }

    ctl <- (max(vec) - min(vec)) / 1

    for (j in 1:6) {
      #hijosn[inde, j]<-abs(hi josn[inde, j]+rnorm(1,0,ctl))
      hijosn[inde1, j] <- runif(1, min(hijos[, j]), max(hijos[, j]))
      hijosn[inde2, j] <- runif(1, min(hijos[, j]), max(hijos[, j]))
      hijosn[inde3, j] <- runif(1, min(hijos[, j]), max(hijos[, j]))
    }

    hijosn[inde1,] <- hijosn[inde1,] / sum(hijosn[inde1,])
    hijosn[inde2,] <- hijosn[inde2,] / sum(hijosn[inde2,])
    hijosn[inde3,] <- hijosn[inde3,] / sum(hijosn[inde3,])

    # Reemplazo de los hijos
    for (jj in 1:60) {
      mn2[40 + jj,] <- hijosn[jj,]
    }
    mn1 <- mn2
  }
  estudioge[repi] <- max(funcionObj1)

}

#------------------------------------
algoritmo2 <- funcionObj1

tiempofin <- proc.time() - timepoini

resu1 <- 0
for (t in 1:5) {

  for (h in 1:5) {

    resu1 <- (macov[t, h] * mn1[1, h]) + resu1

  }

}
resu1 <- resu1 * 100

r1 <- 0
for (j in 1:5) {
  r1 <- (activos[j] * mn1[1, j]) + r1
  #r1[i<-sum(r)
}
r1 <- r1 * 100

plot(funcionObj1, type = "l")

# Gráfica de ganancia-riesgo
x<-seq(1:1000) # ITERACIONES
plot(x, funcionObj1 ,type="l", log = 'x', xlab = "Iteraciones", ylab = "Función objetivo: Ganancia-Riesgo", col="red")