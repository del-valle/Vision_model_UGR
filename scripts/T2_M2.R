
getwd()
list.files()

# ZONA M2, yellow zone transect 2
M2 <- "/Users/delValle/Dropbox/UGR/T2_Parental_Emx_401-480" 
# cambiar por otro directorio si fuera necesario

  
library(pavo)

plotM2<-getspec(M2)
# voy a pulir un poco los espectros que se obtienen en bruto del esptrofotometro

plotM2.fix <- procspec(plotM2, fixneg = "addmin") 
# primero elimino posibles valores negativos anadiendo el valor absoluto del valor mas negativo a todo el espectro
plotM2.sm <- procspec(plotM2.fix, opt = "smooth", span = 0.2)
# luego suavizo un poco los espectros
is.rspec(plotM2.sm)   # [1] TRUE
plot(plotM2.sm$`T2_401a_Reflection__145__20-33-14-081`, type = "l")
plot(plotM2.sm$`T2_401b_Reflection__144__20-32-36-083`, type = "l")
# en estos espectros apenas se perciben diferencias entre los espectros de la parte basal y apical


# lo primero que hay que hacer es promediar la parte basal y apical, es decir, las 2 columnas contiguas
# medidas para cada muestra; voy a hacer primero un pequeno test con unas pocas columnas de forma manual 
# que me servira como referencia mas adelante para verificar que lo que estoy haciendo esta bien


plotM2.sm_manual <- plotM2.sm$wl
plotM2.sm_manual <- as.data.frame(plotM2.sm_manual)
plotM2.sm_manual$T2_401 <- apply(plotM2.sm[ ,c(2,3)], 1, mean, na.rm = TRUE)  # https://es.stackoverflow.com/questions/194515/c%C3%B3mo-promedio-variables-por-fila
plotM2.sm_manual$T2_402 <- apply(plotM2.sm[ ,c(4,5)], 1, mean, na.rm = TRUE)  
plotM2.sm_manual$T2_403 <- apply(plotM2.sm[ ,c(6,7)], 1, mean, na.rm = TRUE)  
plotM2.sm_manual$T2_404 <- apply(plotM2.sm[ ,c(8,9)], 1, mean, na.rm = TRUE)  
plotM2.sm_manual <- as.rspec(plotM2.sm_manual)
is.rspec(plotM2.sm_manual)
head(plotM2.sm_manual, n=5)  
# estas serian valores que tengo que obtener de las muestras 10 a 13, pero es inviable hacerlo manualmente
# asi que voy a crear un loop par que automaticamente me calcule el promedio cada 2 columnas:



# option 1: para hacerlo de la forma que lo he hecho arriba, seria asi (pongo menos explicaciones):
plotM2_average <- plotM2.sm$wl
plotM2_average <- as.data.frame(plotM2_average)
colnames(plotM2_average) <- c("wl")
head(plotM2_average)

for(i in 2:147) {       
  plotM2_average[ , i] <- apply(plotM2.sm[ , c(i, i+1)], 1, mean, na.rm=TRUE)
}

plotM2_average[1:5,1:6]
plotM2_average[1:5,142:146]

plotM2_average2 <- plotM2_average[ , -c(1)]
col_odd <- seq_len(ncol(plotM2_average2)) %% 2  
plotM2_average2 <- plotM2_average2[ , col_odd == 1]
rm(col_odd)

plotM2_average2$wl <- plotM2_average$wl
library(dplyr)
plotM2_average2 <- plotM2_average2 %>%
  select(wl, everything())
plotM2_average2[1:4,1:5]

x0 <- c("wl")
x1 <- 401:439
x2 <- 441:445
x3 <- c("448")
x4 <- 450:467
x5 <- 470:471
x6 <- 473:480
x_sum <- c(x1,x2,x3,x4,x5,x6)
x_sum <- paste("T1", x_sum, sep="_") 
head(x_sum)
names_M2 <- c(x0,x_sum)
names_M2

colnames(plotM2_average2) <- names_M2
head(plotM2_average2, n=5)
plotM2_average2 <- as.rspec(plotM2_average2)
is.rspec(plotM2_average2)

rm(x0)
rm(x1)
rm(x2)
rm(x3)
rm(x4)
rm(x5)
rm(x6)
rm(x_sum)
# rm(names_H1)  me hara falta mas adelante
rm(i)
rm(plotM2_average)


# # option 2: para hacerlo mas fino, seria asi:
# plotM2_average <- plotM2.sm$wl
# plotM2_average <- as.data.frame(plotM2_average)
# colnames(plotM2_average) <- c("wl")
# head(plotM2_average)
# 
# i_seq <- seq(from=2, to=139, by=2)
# 
# for(i in i_seq) {
#         plotM2_average[ , i] <- apply(plotM2.sm[ , c(i, i+1)], 1, mean, na.rm=TRUE)
# }
# 
# 
# plotM2_average
# plotM2_average[1:5,1:5]
# plotM2.sm_manual[1:5,1:5]
# # no me termina de salir bien el loop, tengo que darle alguna vuelta mas



# variables colorimetricas (hue, chroma, spectral purity,. etc.)
colouremetric.variables.plotM2_average2 <- summary(plotM2_average2)
# write.csv2(colouremetric.variables.plotM2_average2, 
#            file="plotM2_average2_colourimetric_variables.csv", row.names = TRUE)

# los espectros de las 74 plantas presentes en la zona M2:
plot(plotM2_average2, type = "o", col = spec2rgb(plotM2_average2))
# extraigo los colores de cada espectro para colorear los puntos en los modelos de vision
rgb <- spec2rgb(plotM2_average2) 

# ahora se representan las 78 flores de la zona L2 en los distintos espacios de color:
# 1) modelo de vision de las ABEJAS:
vis.flowers.M2.bees <- vismodel(plotM2_average2,
                         visual = "apis", 
                         qcatch = "Ei", 
                         relative = FALSE,
                         vonkries = TRUE, 
                         achromatic = "l", 
                         bkg = "green", 
                         illum="D65"
)
hexagon.M2.bees <- colspace(vis.flowers.M2.bees, space = "hexagon")
hexplot(hexagon.M2.bees, 
        sectors="coarse", 
        labels=TRUE, 
        main="Purple Transect 1", 
        cex.main=1.5, 
        col="black")
# write.csv2(hexagon.M2.bees, file="hexagon.M2.bees.csv", row.names = TRUE)

# voy a depurar el grafico para poder exportarlo, incluyendo los colores de cada flor al ojo humano:
par(mar=c(1, 1, 1, 1))
hexplot(hexagon.M2.bees, 
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

vis.flowers.M2.butterflies <- vismodel(plotM2_average2, 
                                visual = papilio, 
                                vonkries = TRUE,
                                illum= "D65",
                                bkg = "green")
tetraedro.M2.butterflies <- colspace(vis.flowers.M2.butterflies, space = "tcs")
plot(tetraedro.M2.butterflies) 
# write.csv2(tetraedro.M2.butterflies, file="tetraedro.M2.butterflies.csv", row.names = TRUE)

# voy a depurar el grafico para poder exportarlo, incluyendo los colores de cada flor al ojo humano:
par(mar=c(1, 1, 1, 1))
plot(tetraedro.M2.butterflies, 
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
vis.flowers.M2.flies <- vismodel(plotM2_average2,
                                 bkg = "green",
                                 visual = "musca",
                                 achromatic = "md.r1",
                                 illum= "D65",
                                 vonkries = TRUE)
categorical.M2.flies <- colspace(vis.flowers.M2.flies, space = "categorical")
plot(categorical.M2.flies)
# write.csv2(categorical.M2.flies, file="categorical.M2.flies.csv", row.names = TRUE)

# voy a depurar el grafico para poder exportarlo, incluyendo los colores de cada flor al ojo humano:
par(mar=c(2, 2, 2, 2))
plot(categorical.M2.flies, 
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

vis.flowers.M2.beetles <- vismodel(plotM2_average2, 
                                   visual = p.israelitus, 
                                   vonkries = TRUE,
                                   illum= "D65",
                                   bkg = "green")
triangle.M2.beetles <- colspace(vis.flowers.M2.beetles, space = "tri")
plot(triangle.M2.beetles) 
# write.csv2(triangle.M2.beetles, file="triangle.M2.beetles.csv", row.names = TRUE)

# voy a depurar el grafico para poder exportarlo, incluyendo los colores de cada flor al ojo humano:
par(mar=c(1, 1, 1, 1))
plot(triangle.M2.beetles, 
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
chrom_contrast_bees <- hexagon.M2.bees$r.vec
chrom_contrast_butterflies <- tetraedro.M2.butterflies$r.vec
chrom_contrast_flies <- categorical.M2.flies$r.vec
chrom_contrast_beetles <- triangle.M2.beetles$r.vec
names_M2 <- names_M2[-(1)]
chrom_contrast_table <- data.frame(names_M2, 
                                   chrom_contrast_bees,
                                   chrom_contrast_butterflies,
                                   chrom_contrast_flies,
                                   chrom_contrast_beetles)  
head(chrom_contrast_table, n=5)

# ahora solo tengo que calcular las medias, medianas, max, min y se
mean <- apply(subset(chrom_contrast_table, select = c(2:5)), 2, mean, na.rm=TRUE)
sd <- apply(subset(chrom_contrast_table, select = c(2:5)), 2, sd, na.rm=TRUE)
se <- sd/sqrt(73)
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
achrom_contrast_bees <- abs(hexagon.M2.bees$l-0.5)
achrom_contrast_table <- data.frame(names_M2, 
                                   achrom_contrast_bees)
head(achrom_contrast_table, n=6)

# ahora solo tengo que calcular las medias, medianas, max, min y se
mean(achrom_contrast_table$achrom_contrast_bees)  # 0.3177443
sd(achrom_contrast_table$achrom_contrast_bees)/sqrt(73)  # 0.001959219
min(achrom_contrast_table$achrom_contrast_bees)  # 0.2715757
max(achrom_contrast_table$achrom_contrast_bees)  # 0.3453163
median(achrom_contrast_table$achrom_contrast_bees)  # 0.319534


# ahora voy a exportar las tablas de chromatic contrast (distancia al centro) y achromatic 
# contrast (green contrast) de cada zona/transecto; luego las analizare en un script aparte
# pero antes debo anadirles un par de columnas a ambas tablas indicandole transecto y zona

chrom_contrast_table$transect <- rep(2)
chrom_contrast_table$zone <- rep("yellow")
chrom_contrast_table$zone2 <- rep("yellow2")
head(chrom_contrast_table, n=6)
write.table(chrom_contrast_table, file="chrom_cont_M2.txt", sep=" ", dec=".", row.names= FALSE)

achrom_contrast_table$transect <- rep(2)
achrom_contrast_table$zone <- rep("yellow")
achrom_contrast_table$zone2 <- rep("yellow2")
head(achrom_contrast_table, n=6)
write.table(achrom_contrast_table, file="achrom_cont_M2.txt", sep=" ", dec=".", row.names= FALSE)

