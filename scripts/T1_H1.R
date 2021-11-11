
getwd()
list.files()

# ZONA H1, hybrid zone   
# en este script voy a analizar como ven las flores los principales grupos de polinizadores de la
# zona: dipteros, coleopteros, lepidopteros e himenopteros
# este script sera un poco mas largo porque es la primera zona con la que trabajo y he hecho varios
# test que no tengo que repetir en los siguientes scripts == cada zona la anlizare en scripts separados
# porque si no va a ser demasiado largo y pudiera ser un poco caotico

H1 <- "/Users/delValle/Dropbox/UGR/T1_Hibrida_101-178"         
# cambiar por otro directorio si fuera necesario

  
library(pavo)

plotH1<-getspec(H1)
# voy a pulir un poco los espectros que se obtienen en bruto del esptrofotometro

plotH1.fix <- procspec(plotH1, fixneg = "addmin") 
# primero elimino posibles valores negativos anadiendo el valor absoluto del valor mas negativo a todo el espectro
plotH1.sm <- procspec(plotH1.fix, opt = "smooth", span = 0.2)
# luego suavizo un poco los espectros
is.rspec(plotH1.sm)   # [1] TRUE
plot(plotH1.sm$`T1_101a_Reflection__1__21-54-09-317`, type = "l")
plot(plotH1.sm$`T1_101b_Reflection__0__21-52-57-330`, type = "l")
# despues de ver los espectros, no tengo demasiado claro si es buena idea mezclar las mediciones
# de la parte basal y apical; de todas formas, voy a continuar con ello


# lo primero que hay que hacer es promediar la parte basal y apical, es decir, las 2 columnas contiguas
# medidas para cada muestra; voy a hacer primero un peque??o test con unas pocas columnas de forma manual 
# que me servira como referencia mas adelante para verificar que lo que estoy haciendo esta bien


plotH1.sm_manual <- plotH1.sm$wl
plotH1.sm_manual <- as.data.frame(plotH1.sm_manual)
plotH1.sm_manual$T1_101 <- apply(plotH1.sm[ ,c(2,3)], 1, mean, na.rm = TRUE)  # https://es.stackoverflow.com/questions/194515/c%C3%B3mo-promedio-variables-por-fila
plotH1.sm_manual$T1_102 <- apply(plotH1.sm[ ,c(4,5)], 1, mean, na.rm = TRUE)  
plotH1.sm_manual$T1_103 <- apply(plotH1.sm[ ,c(6,7)], 1, mean, na.rm = TRUE)  
plotH1.sm_manual$T1_104 <- apply(plotH1.sm[ ,c(8,9)], 1, mean, na.rm = TRUE)  
plotH1.sm_manual <- as.rspec(plotH1.sm_manual)
is.rspec(plotH1.sm_manual)
head(plotH1.sm_manual, n=5)  
# estas serian valores que tengo que obtener de las muestras 101 a 104, pero es inviable hacerlo manualmente
# asi que voy a crear un loop par que automaticamente me calcule el promedio cada 2 columnas:

test0 <- plotH1.sm$wl
test0 <- as.data.frame(test0)
colnames(test0) <- c("wl")
head(test0)

# en este primer test, voy a calcular los promedios solo desde la columna 2 a la 11: 
for(i in 2:11) {       
  test0[ , i] <- apply(plotH1.sm[ , c(i, i+1)], 1, mean, na.rm=TRUE)
}

# al comparar los resultados, veo que me ha sumado las columnas contiguas (ej: 2-3, 3-4, 4-5):
test0[1:5, 1:8]
plotH1.sm_manual[1:5, 1:5]
# esto hace que solo las columnas pares (V2, V4, V6, etc.) sean las correctas

# voy a probar a eliminar las columnas impares y a retener las pares, una vez que haya quitado
# la primera columna que tiene las longitudes de onda
test1 <- test0[ , -c(1)]
col_odd <- seq_len(ncol(test1)) %% 2  
test1 <- test1[ , col_odd == 1]
rm(col_odd)

# vuelvo a anadir la columna de wl que habia eliminado y ahora si que me coinciden los resultados:
test1$wl <- test0$wl
library(dplyr)
test1 <- test1 %>%
  select(wl, everything())

plotH1.sm_manual[1:5, 1:5]
test1[1:5, 1:5]
rm(test0)

# ahora solo me falta cambiar el nombre de las variables y ya estaria listo:
x0 <- c("wl")
x1 <- 101:125
x2 <- 127:150
x3 <- 152:158
x4 <- 160:178
# faltan algunas muestras (ej: 126), por eso las secuencias no van del 101 al 178 directamente
x_sum <- c(x1, x2, x3, x4)
x_sum <- paste("T1", x_sum, sep="_") 
head(x_sum)
# lo de arriba estaria bien para todas las muestras, pero para este pequeno test solo necesito 
# los nombres hasta desde el T_101 hasta el T1_105
x_sum_test1 <- c(x_sum[1:5])
names_test1 <- c(x0,x_sum_test1)
names_test1

# ya tengo listo el vector con los nombres de las columnas del test1
colnames(test1) <- names_test1
head(test1, n=5)
# y asi es como consigo obtener los valores promedios de cada muestra (T1_101, T1_102, etc.)
# con su correspondiente nomenclatura; con esto podria trabajar ya y extrapolarlo al conjunto 
# completo de datos

rm(x1)
rm(x2)
rm(x3)
rm(x4)
rm(x0)
rm(x_sum)
rm(x_sum_test1)
rm(test1)
rm(names_test1)
rm(i)



# option 1: para hacerlo de la forma que lo he hecho arriba, seria asi (pongo menos explicaciones):
plotH1_average <- plotH1.sm$wl
plotH1_average <- as.data.frame(plotH1_average)
colnames(plotH1_average) <- c("wl")
head(plotH1_average)

for(i in 2:151) {       
  plotH1_average[ , i] <- apply(plotH1.sm[ , c(i, i+1)], 1, mean, na.rm=TRUE)
}

plotH1_average[1:5,1:5]
plotH1_average[1:5,145:150]

plotH1_average2 <- plotH1_average[ , -c(1)]
col_odd <- seq_len(ncol(plotH1_average2)) %% 2  
plotH1_average2 <- plotH1_average2[ , col_odd == 1]
rm(col_odd)

plotH1_average2$wl <- plotH1_average$wl
library(dplyr)
plotH1_average2 <- plotH1_average2 %>%
  select(wl, everything())
plotH1_average2[1:4,1:5]

x0 <- c("wl")
x1 <- 101:125
x2 <- 127:150
x3 <- 152:158
x4 <- 160:178
x_sum <- c(x1, x2, x3, x4)
x_sum <- paste("T1", x_sum, sep="_") 
head(x_sum)
names_H1 <- c(x0,x_sum)
names_H1

colnames(plotH1_average2) <- names_H1
head(plotH1_average2, n=5)
plotH1_average2 <- as.rspec(plotH1_average2)
is.rspec(plotH1_average2)

rm(x0)
rm(x1)
rm(x2)
rm(x3)
rm(x4)
rm(x_sum)
# rm(names_H1)  me hara falta mas adelante
rm(i)
rm(plotH1_average)


# # option 2: para hacerlo mas fino, seria asi:
# plotH1_average <- plotH1.sm$wl
# plotH1_average <- as.data.frame(plotH1_average)
# colnames(plotH1_average) <- c("wl")
# head(plotH1_average)
# 
# i_seq <- seq(from=2, to=151, by=2)
# 
# for(i in i_seq) {
#         plotH1_average[ , i] <- apply(plotH1.sm[ , c(i, i+1)], 1, mean, na.rm=TRUE)
# }
# 
# 
# plotH1_average
# plotH1_average[1:5,1:5]
# plotH1.sm_manual[1:5,1:5]
# # no me termina de salir bien el loop, tengo que darle alguna vuelta mas



# variables colorimetricas (hue, chroma, spectral purity,. etc.)
colouremetric.variables.plotH1_average2 <- summary(plotH1_average2)
# write.csv2(colouremetric.variables.plotH1_average2, 
#            file="plotH1_average2_colourimetric_variables.csv", row.names = TRUE)

# los espectros de las 75 plantas presentes en la zona H1:
plot(plotH1_average2, type = "o", col = spec2rgb(plotH1_average2))
# extraigo los colores de cada espectro para colorear los puntos en los modelos de vision
rgb <- spec2rgb(plotH1_average2) 

# ahora se representan las 75 flores de la zona H1 en los distintos espacios de color:
# 1) modelo de vision de las ABEJAS:
vis.flowers.H1.bees <- vismodel(plotH1_average2,
                         visual = "apis", 
                         qcatch = "Ei", 
                         relative = FALSE,
                         vonkries = TRUE, 
                         achromatic = "l", 
                         bkg = "green", 
                         illum="D65"
)
hexagon.H1.bees <- colspace(vis.flowers.H1.bees, space = "hexagon")
hexplot(hexagon.H1.bees, 
        sectors="coarse", 
        labels=TRUE, 
        main="Hybrid Transect 1", 
        cex.main=1.5, 
        col="black")
# write.csv2(hexagon.H1.bees, file="hexagon.H1.bees.csv", row.names = TRUE)

# voy a depurar el grafico para poder exportarlo, incluyendo los colores de cada flor al ojo humano:
par(mar=c(1, 1, 1, 1))
hexplot(hexagon.H1.bees, 
        sectors="coarse", 
        achro=TRUE, 
        labels=FALSE, 
        cex=1.1, 
        labels.cex = 1.2, 
        col=rgb)
# exporto el grafico a 7.5 x 5.5 pulgadas en pdf


# 2) modelo de vision de las MARIPOSAS:
# voy a usar las sensibilidades de papilio xunthus, como hicimos en nuestro paper

papilio <-read.table("sensibilidad_papilio_xunthus.txt", header=T)
papilio <- as.rspec(papilio)
is.rspec(papilio)
head(papilio)

vis.flowers.H1.butterflies <- vismodel(plotH1_average2, 
                                visual = papilio, 
                                vonkries = TRUE,
                                illum= "D65",
                                bkg = "green")
tetraedro.H1.butterflies <- colspace(vis.flowers.H1.butterflies, space = "tcs")
plot(tetraedro.H1.butterflies) 
# write.csv2(tetraedro.H1.butterflies, file="tetraedro.H1.butterflies.csv", row.names = TRUE)

# voy a depurar el grafico para poder exportarlo, incluyendo los colores de cada flor al ojo humano:
par(mar=c(1, 1, 1, 1))
plot(tetraedro.H1.butterflies, 
     achro=TRUE, 
     labels=FALSE, 
     cex.main=1.2, 
     achro.size = 0.5, 
     achro.col = "grey", 
     zoom = 1, 
     out.lwd = 1, 
     col=rgb)
# exporto el grafico a 7.5 x 5.5 pulgadas en pdf



# 3) modelo de vision de las MOSCAS:
# voy a usar las sensibilidades de la mosca comun (incluido en pavo), como hicimos en nuestro paper
vis.flowers.H1.flies <- vismodel(plotH1_average2,
                                 bkg = "green",
                                 visual = "musca",
                                 achromatic = "md.r1",
                                 illum= "D65",
                                 vonkries = TRUE)
categorical.H1.flies <- colspace(vis.flowers.H1.flies, space = "categorical")
plot(categorical.H1.flies)
# write.csv2(categorical.H1.flies, file="categorical.H1.flies.csv", row.names = TRUE)

# voy a depurar el grafico para poder exportarlo, incluyendo los colores de cada flor al ojo humano:
par(mar=c(2, 2, 2, 2))
plot(categorical.H1.flies, 
     labels=TRUE, 
     cex=1.1, 
     labels.cex = 1.1,
     col=rgb)

# exporto el grafico a 7.5 x 5.5 pulgadas en pdf



# 4) modelo de vision de los escarabajos:
# voy a usar las sensibilidades de pygopleurus israelitus, como hacen Martinez-Harm et al 2020
p.israelitus <-read.table("sensibilidad_pygopleurus_israelitus.txt", header=T)
p.israelitus <- as.rspec(p.israelitus)
is.rspec(p.israelitus)
head(p.israelitus)

vis.flowers.H1.beetles <- vismodel(plotH1_average2, 
                                       visual = p.israelitus, 
                                       vonkries = TRUE,
                                       illum= "D65",
                                       bkg = "green")
triangle.H1.beetles <- colspace(vis.flowers.H1.beetles, space = "tri")
plot(triangle.H1.beetles) 
# write.csv2(triangle.H1.beetles, file="triangle.H1.beetles.csv", row.names = TRUE)

# voy a depurar el grafico para poder exportarlo, incluyendo los colores de cada flor al ojo humano:
par(mar=c(1, 1, 1, 1))
plot(triangle.H1.beetles, 
     achro=TRUE, 
     labels=FALSE, 
     cex.main=1.2, 
     achro.size = 0.5, 
     achro.col = "grey", 
     zoom = 1, 
     out.lwd = 1, 
     col=rgb)
# exporto el grafico a 7.5 x 5.5 pulgadas en pdf




# ahora hay que calcular los contrastes CROMATICOS, que no es mas que la distancia al centro;
# en pavo esta informacion se obtiene de la columna r.vec de cada modelo de vision
chrom_contrast_bees <- hexagon.H1.bees$r.vec
chrom_contrast_butterflies <- tetraedro.H1.butterflies$r.vec
chrom_contrast_flies <- categorical.H1.flies$r.vec
chrom_contrast_beetles <- triangle.H1.beetles$r.vec
names_H1 <- names_H1[-(1)]
chrom_contrast_table <- data.frame(names_H1, 
                                   chrom_contrast_bees,
                                   chrom_contrast_butterflies,
                                   chrom_contrast_flies,
                                   chrom_contrast_beetles)
head(chrom_contrast_table, n=5)

# ahora solo tengo que calcular las medias, medianas, max, min y se
mean <- apply(subset(chrom_contrast_table, select = c(2:5)), 2, mean, na.rm=TRUE)
sd <- apply(subset(chrom_contrast_table, select = c(2:5)), 2, sd, na.rm=TRUE)
se <- sd/sqrt(75)
min <- apply(subset(chrom_contrast_table, select = c(2:5)), 2, min, na.rm=TRUE)
max <- apply(subset(chrom_contrast_table, select = c(2:5)), 2, max, na.rm=TRUE)
median <- apply(subset(chrom_contrast_table, select = c(2:5)), 2, median, na.rm=TRUE)

chrom_contrast_statistics <- data.frame(mean, 
                                    min,
                                    max,
                                    median,
                                    se)
chrom_contrast_statistics

rm(mean)
rm(sd)
rm(se)
rm(min)
rm(max)
rm(median)
rm(chrom_contrast_bees)
rm(chrom_contrast_butterflies)
rm(chrom_contrast_flies)

# me faltaria calcular el contraste ACROMATICO, que solo se calcula para las abejas;
# se calcula como valor absoluto de Eg (columna "l" del hexagono) - 0.5
achrom_contrast_bees <- abs(hexagon.H1.bees$l-0.5)
achrom_contrast_table <- data.frame(names_H1, 
                                   achrom_contrast_bees)
head(achrom_contrast_table, n=6)

# ahora solo tengo que calcular las medias, medianas, max, min y se
mean(achrom_contrast_table$achrom_contrast_bees)  # 0.2751795
sd(achrom_contrast_table$achrom_contrast_bees)/sqrt(75)  # 0.008259978
min(achrom_contrast_table$achrom_contrast_bees)  # 0.07914547
max(achrom_contrast_table$achrom_contrast_bees)  # 0.3841021
median(achrom_contrast_table$achrom_contrast_bees)  # 0.2750608


# ahora voy a exportar las tablas de chromatic contrast (distancia al centro) y achromatic 
# contrast (green contrast) de cada zona/transecto; luego las analizare en un script aparte
# pero antes debo anadirles un par de columnas a ambas tablas indicandole transecto y zona

chrom_contrast_table$transect <- rep(1)
chrom_contrast_table$zone <- rep("hybrid")
chrom_contrast_table$zone2 <- rep("hybrid1")
head(chrom_contrast_table, n=6)
write.table(chrom_contrast_table, file="chrom_cont_H1.txt", sep=" ", dec=".", row.names= FALSE)

achrom_contrast_table$transect <- rep(1)
achrom_contrast_table$zone <- rep("hybrid")
achrom_contrast_table$zone2 <- rep("hybrid1")
head(achrom_contrast_table, n=6)
write.table(achrom_contrast_table, file="achrom_cont_H1.txt", sep=" ", dec=".", row.names= FALSE)

