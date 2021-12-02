
getwd()
list.files()

# ZONA H2, hybrid zone transect 2
H2 <- "/Users/delValle/Dropbox/Vision_model_UGR/T2_Hibrida_321-393"
# cambiar por otro directorio si fuera necesario

  
library(pavo)

plotH2<-getspec(H2)
# voy a pulir un poco los espectros que se obtienen en bruto del esptrofotometro

plotH2.fix <- procspec(plotH2, fixneg = "addmin") 
# primero elimino posibles valores negativos anadiendo el valor absoluto del valor mas negativo a todo el espectro
plotH2.sm <- procspec(plotH2.fix, opt = "smooth", span = 0.2)
# luego suavizo un poco los espectros
is.rspec(plotH2.sm)   # [1] TRUE
plot(plotH2.sm$`T2_321a_Reflection__2__23-50-51-877`, type = "l")
plot(plotH2.sm$`T2_321b_Reflection__1__23-50-21-878`, type = "l")
# en estos espectros apenas se perciben diferencias entre los espectros de la parte basal y apical


# lo primero que hay que hacer es promediar la parte basal y apical, es decir, las 2 columnas contiguas
# medidas para cada muestra; voy a hacer primero un pequeno test con unas pocas columnas de forma manual 
# que me servira como referencia mas adelante para verificar que lo que estoy haciendo esta bien


plotH2.sm_manual <- plotH2.sm$wl
plotH2.sm_manual <- as.data.frame(plotH2.sm_manual)
plotH2.sm_manual$T2_321 <- apply(plotH2.sm[ ,c(2,3)], 1, mean, na.rm = TRUE)  # https://es.stackoverflow.com/questions/194515/c%C3%B3mo-promedio-variables-por-fila
plotH2.sm_manual$T2_323 <- apply(plotH2.sm[ ,c(4,5)], 1, mean, na.rm = TRUE)  
plotH2.sm_manual$T2_324 <- apply(plotH2.sm[ ,c(6,7)], 1, mean, na.rm = TRUE)  
plotH2.sm_manual$T2_325 <- apply(plotH2.sm[ ,c(8,9)], 1, mean, na.rm = TRUE)  
plotH2.sm_manual <- as.rspec(plotH2.sm_manual)
is.rspec(plotH2.sm_manual)
head(plotH2.sm_manual, n=5)  
# estas serian valores que tengo que obtener de las muestras 321 a 393, pero es inviable hacerlo manualmente
# asi que voy a crear un loop par que automaticamente me calcule el promedio cada 2 columnas:



# option 1: para hacerlo de la forma que lo he hecho arriba, seria asi (pongo menos explicaciones):
plotH2_average <- plotH2.sm$wl
plotH2_average <- as.data.frame(plotH2_average)
colnames(plotH2_average) <- c("wl")
head(plotH2_average)

for(i in 2:139) {       
  plotH2_average[ , i] <- apply(plotH2.sm[ , c(i, i+1)], 1, mean, na.rm=TRUE)
}

plotH2_average[1:5,1:6]
plotH2_average[1:5,134:138]

plotH2_average2 <- plotH2_average[ , -c(1)]
col_odd <- seq_len(ncol(plotH2_average2)) %% 2  
plotH2_average2 <- plotH2_average2[ , col_odd == 1]
rm(col_odd)

plotH2_average2$wl <- plotH2_average$wl
library(dplyr)
plotH2_average2 <- plotH2_average2 %>%
  select(wl, everything())
plotH2_average2[1:4,1:5]

x0 <- c("wl")
x1 <- c("321")
x2 <- 323:349
x3 <- c("349b")
x4 <- c("349c")
x5 <- 350:375
x6 <- 377:378
x7 <- 380:382
x8 <- 384:388
x9 <- 391:393
x_sum <- c(x1,x2,x3,x4,x5,x6,x7,x8,x9)
x_sum <- paste("T1", x_sum, sep="_") 
head(x_sum)
names_H2 <- c(x0,x_sum)
names_H2

colnames(plotH2_average2) <- names_H2
head(plotH2_average2, n=5)
plotH2_average2 <- as.rspec(plotH2_average2)
is.rspec(plotH2_average2)

rm(x0)
rm(x1)
rm(x2)
rm(x3)
rm(x4)
rm(x5)
rm(x6)
rm(x7)
rm(x8)
rm(x9)
rm(x_sum)
# rm(names_H1)  me hara falta mas adelante
rm(i)
rm(plotH2_average)


# # option 2: para hacerlo mas fino, seria asi:
# plotH2_average <- plotH2.sm$wl
# plotH2_average <- as.data.frame(plotH2_average)
# colnames(plotH2_average) <- c("wl")
# head(plotH2_average)
# 
# i_seq <- seq(from=2, to=139, by=2)
# 
# for(i in i_seq) {
#         plotH2_average[ , i] <- apply(plotH2.sm[ , c(i, i+1)], 1, mean, na.rm=TRUE)
# }
# 
# 
# plotH2_average
# plotH2_average[1:5,1:5]
# plotH2.sm_manual[1:5,1:5]
# # no me termina de salir bien el loop, tengo que darle alguna vuelta mas



# variables colorimetricas (hue, chroma, spectral purity,. etc.)
colouremetric.variables.plotH2_average2 <- summary(plotH2_average2)
# write.csv2(colouremetric.variables.plotH2_average2, 
#            file="plotH2_average2_colourimetric_variables.csv", row.names = TRUE)

# los espectros de las 80 plantas presentes en la zona M1:
plot(plotH2_average2, type = "o", col = spec2rgb(plotH2_average2))
# extraigo los colores de cada espectro para colorear los puntos en los modelos de vision
rgb <- spec2rgb(plotH2_average2) 

# ahora se representan las 81 flores de la zona L1 en los distintos espacios de color:
# 1) modelo de vision de las ABEJAS:
vis.flowers.H2.bees <- vismodel(plotH2_average2,
                         visual = "apis", 
                         qcatch = "Ei", 
                         relative = FALSE,
                         vonkries = TRUE, 
                         achromatic = "l", 
                         bkg = "green", 
                         illum="D65"
)
hexagon.H2.bees <- colspace(vis.flowers.H2.bees, space = "hexagon")
hexplot(hexagon.H2.bees, 
        sectors="coarse", 
        labels=TRUE, 
        main="Purple Transect 1", 
        cex.main=1.5, 
        col="black")
# write.csv2(hexagon.H2.bees, file="hexagon.H2.bees.csv", row.names = TRUE)

# voy a depurar el grafico para poder exportarlo, incluyendo los colores de cada flor al ojo humano:
par(mar=c(1, 1, 1, 1))
hexplot(hexagon.H2.bees, 
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

vis.flowers.H2.butterflies <- vismodel(plotH2_average2, 
                                visual = papilio, 
                                vonkries = TRUE,
                                illum= "D65",
                                bkg = "green")
tetraedro.H2.butterflies <- colspace(vis.flowers.H2.butterflies, space = "tcs")
plot(tetraedro.H2.butterflies) 
# write.csv2(tetraedro.H1.butterflies, file="tetraedro.H1.butterflies.csv", row.names = TRUE)

# voy a depurar el grafico para poder exportarlo, incluyendo los colores de cada flor al ojo humano:
par(mar=c(1, 1, 1, 1))
plot(tetraedro.H2.butterflies, 
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
vis.flowers.H2.flies <- vismodel(plotH2_average2,
                                 bkg = "green",
                                 visual = "musca",
                                 achromatic = "md.r1",
                                 illum= "D65",
                                 vonkries = TRUE)
categorical.H2.flies <- colspace(vis.flowers.H2.flies, space = "categorical")
plot(categorical.H2.flies)
# write.csv2(categorical.L1.flies, file="categorical.L1.flies.csv", row.names = TRUE)

# voy a depurar el grafico para poder exportarlo, incluyendo los colores de cada flor al ojo humano:
par(mar=c(2, 2, 2, 2))
plot(categorical.H2.flies, 
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

vis.flowers.H2.beetles <- vismodel(plotH2_average2, 
                                   visual = p.israelitus, 
                                   vonkries = TRUE,
                                   illum= "D65",
                                   bkg = "green")
triangle.H2.beetles <- colspace(vis.flowers.H2.beetles, space = "tri")
plot(triangle.H2.beetles) 
# write.csv2(triangle.H2.beetles, file="triangle.H2.beetles.csv", row.names = TRUE)

# voy a depurar el grafico para poder exportarlo, incluyendo los colores de cada flor al ojo humano:
par(mar=c(1, 1, 1, 1))
plot(triangle.H2.beetles, 
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
chrom_contrast_bees <- hexagon.H2.bees$r.vec
chrom_contrast_butterflies <- tetraedro.H2.butterflies$r.vec
chrom_contrast_flies <- categorical.H2.flies$r.vec
chrom_contrast_beetles <- triangle.H2.beetles$r.vec
names_H2 <- names_H2[-(1)]
chrom_contrast_table <- data.frame(names_H2, 
                                   chrom_contrast_bees,
                                   chrom_contrast_butterflies,
                                   chrom_contrast_flies,
                                   chrom_contrast_beetles)
head(chrom_contrast_table, n=5)

# ahora solo tengo que calcular las medias, medianas, max, min y se
mean <- apply(subset(chrom_contrast_table, select = c(2:5)), 2, mean, na.rm=TRUE)
sd <- apply(subset(chrom_contrast_table, select = c(2:5)), 2, sd, na.rm=TRUE)
se <- sd/sqrt(69)
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
achrom_contrast_bees <- abs(hexagon.H2.bees$l-0.5)
achrom_contrast_table <- data.frame(names_H2, 
                                   achrom_contrast_bees)
head(achrom_contrast_table, n=6)

# ahora solo tengo que calcular las medias, medianas, max, min y se
mean(achrom_contrast_table$achrom_contrast_bees)  # 0.2539445
sd(achrom_contrast_table$achrom_contrast_bees)/sqrt(69)  # 0.01075652
min(achrom_contrast_table$achrom_contrast_bees)  # 0.008551132
max(achrom_contrast_table$achrom_contrast_bees)  # 0.357816
median(achrom_contrast_table$achrom_contrast_bees)  # 0.2813446


# ahora voy a exportar las tablas de chromatic contrast (distancia al centro) y achromatic 
# contrast (green contrast) de cada zona/transecto; luego las analizare en un script aparte
# pero antes debo anadirles un par de columnas a ambas tablas indicandole transecto y zona

chrom_contrast_table$transect <- rep(2)
chrom_contrast_table$zone <- rep("hybrid")
chrom_contrast_table$zone2 <- rep("hybrid2")
head(chrom_contrast_table, n=6)
write.table(chrom_contrast_table, file="chrom_cont_H2.txt", sep=" ", dec=".", row.names= FALSE)

achrom_contrast_table$transect <- rep(2)
achrom_contrast_table$zone <- rep("hybrid")
achrom_contrast_table$zone2 <- rep("hybrid2")
head(achrom_contrast_table, n=6)
write.table(achrom_contrast_table, file="achrom_cont_H2.txt", sep=" ", dec=".", row.names= FALSE)

