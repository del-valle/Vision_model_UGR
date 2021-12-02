
getwd()
list.files()

# ZONA M1, yellow zone
M1 <- "/Users/delValle/Dropbox/Vision_model_UGR/T1_Parental_Emx_21-100" 
# cambiar por otro directorio si fuera necesario

  
library(pavo)

plotM1<-getspec(M1)
# voy a pulir un poco los espectros que se obtienen en bruto del esptrofotometro

plotM1.fix <- procspec(plotM1, fixneg = "addmin") 
# primero elimino posibles valores negativos anadiendo el valor absoluto del valor mas negativo a todo el espectro
plotM1.sm <- procspec(plotM1.fix, opt = "smooth", span = 0.2)
# luego suavizo un poco los espectros
is.rspec(plotM1.sm)   # [1] TRUE
plot(plotM1.sm$`T1_21a_Reflection__1__16-58-58-723`, type = "l")
plot(plotM1.sm$`T1_21b_Reflection__0__16-58-00-736`, type = "l")
# en estos espectros apenas se perciben diferencias entre los espectros de la parte basal y apical


# lo primero que hay que hacer es promediar la parte basal y apical, es decir, las 2 columnas contiguas
# medidas para cada muestra; voy a hacer primero un pequeno test con unas pocas columnas de forma manual 
# que me servira como referencia mas adelante para verificar que lo que estoy haciendo esta bien


plotM1.sm_manual <- plotM1.sm$wl
plotM1.sm_manual <- as.data.frame(plotM1.sm_manual)
plotM1.sm_manual$T1_100 <- apply(plotM1.sm[ ,c(2,3)], 1, mean, na.rm = TRUE)  # https://es.stackoverflow.com/questions/194515/c%C3%B3mo-promedio-variables-por-fila
plotM1.sm_manual$T1_21 <- apply(plotM1.sm[ ,c(4,5)], 1, mean, na.rm = TRUE)  
plotM1.sm_manual$T1_22 <- apply(plotM1.sm[ ,c(6,7)], 1, mean, na.rm = TRUE)  
plotM1.sm_manual$T1_23 <- apply(plotM1.sm[ ,c(8,9)], 1, mean, na.rm = TRUE)  
plotM1.sm_manual <- as.rspec(plotM1.sm_manual)
is.rspec(plotM1.sm_manual)
head(plotM1.sm_manual, n=5)  
# estas serian valores que tengo que obtener de las muestras 100 a 23, pero es inviable hacerlo manualmente
# asi que voy a crear un loop par que automaticamente me calcule el promedio cada 2 columnas:



# option 1: para hacerlo de la forma que lo he hecho arriba, seria asi (pongo menos explicaciones):
plotM1_average <- plotM1.sm$wl
plotM1_average <- as.data.frame(plotM1_average)
colnames(plotM1_average) <- c("wl")
head(plotM1_average)

for(i in 2:161) {       
  plotM1_average[ , i] <- apply(plotM1.sm[ , c(i, i+1)], 1, mean, na.rm=TRUE)
}

plotM1_average[1:5,1:6]
plotM1_average[1:5,158:162]

plotM1_average2 <- plotM1_average[ , -c(1)]
col_odd <- seq_len(ncol(plotM1_average2)) %% 2  
plotM1_average2 <- plotM1_average2[ , col_odd == 1]
rm(col_odd)

plotM1_average2$wl <- plotM1_average$wl
library(dplyr)
plotM1_average2 <- plotM1_average2 %>%
  select(wl, everything())
plotM1_average2[1:4,1:5]

x0 <- c("wl")
x1 <- c("100")
x2 <- 21:99
x_sum <- c(x1, x2)
x_sum <- paste("T1", x_sum, sep="_") 
head(x_sum)
names_M1 <- c(x0,x_sum)
names_M1

colnames(plotM1_average2) <- names_M1
head(plotM1_average2, n=5)
plotM1_average2 <- as.rspec(plotM1_average2)
is.rspec(plotM1_average2)

rm(x0)
rm(x1)
rm(x_sum)
# rm(names_H1)  me hara falta mas adelante
rm(i)
rm(plotM1_average)


# # option 2: para hacerlo mas fino, seria asi:
# plotM1_average <- plotM1.sm$wl
# plotM1_average <- as.data.frame(plotM1_average)
# colnames(plotM1_average) <- c("wl")
# head(plotM1_average)
# 
# i_seq <- seq(from=2, to=161, by=2)
# 
# for(i in i_seq) {
#         plotM1_average[ , i] <- apply(plotM1.sm[ , c(i, i+1)], 1, mean, na.rm=TRUE)
# }
# 
# 
# plotM1_average
# plotM1_average[1:5,1:5]
# plotM1.sm_manual[1:5,1:5]
# # no me termina de salir bien el loop, tengo que darle alguna vuelta mas



# variables colorimetricas (hue, chroma, spectral purity,. etc.)
colouremetric.variables.plotM1_average2 <- summary(plotM1_average2)
# write.csv2(colouremetric.variables.plotM1_average2, 
#            file="plotM1_average2_colourimetric_variables.csv", row.names = TRUE)

# los espectros de las 80 plantas presentes en la zona M1:
plot(plotM1_average2, type = "o", col = spec2rgb(plotM1_average2))
# extraigo los colores de cada espectro para colorear los puntos en los modelos de vision
rgb <- spec2rgb(plotM1_average2) 

# ahora se representan las 81 flores de la zona L1 en los distintos espacios de color:
# 1) modelo de vision de las ABEJAS:
vis.flowers.M1.bees <- vismodel(plotM1_average2,
                         visual = "apis", 
                         qcatch = "Ei", 
                         relative = FALSE,
                         vonkries = TRUE, 
                         achromatic = "l", 
                         bkg = "green", 
                         illum="D65"
)
hexagon.M1.bees <- colspace(vis.flowers.M1.bees, space = "hexagon")
hexplot(hexagon.M1.bees, 
        sectors="coarse", 
        labels=TRUE, 
        main="Purple Transect 1", 
        cex.main=1.5, 
        col="black")
# write.csv2(hexagon.M1.bees, file="hexagon.M1.bees.csv", row.names = TRUE)

# voy a depurar el grafico para poder exportarlo, incluyendo los colores de cada flor al ojo humano:
par(mar=c(1, 1, 1, 1))
hexplot(hexagon.M1.bees, 
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

vis.flowers.M1.butterflies <- vismodel(plotM1_average2, 
                                visual = papilio, 
                                vonkries = TRUE,
                                illum= "D65",
                                bkg = "green")
tetraedro.M1.butterflies <- colspace(vis.flowers.M1.butterflies, space = "tcs")
plot(tetraedro.M1.butterflies) 
# write.csv2(tetraedro.H1.butterflies, file="tetraedro.H1.butterflies.csv", row.names = TRUE)

# voy a depurar el grafico para poder exportarlo, incluyendo los colores de cada flor al ojo humano:
par(mar=c(1, 1, 1, 1))
plot(tetraedro.M1.butterflies, 
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
vis.flowers.M1.flies <- vismodel(plotM1_average2,
                                 bkg = "green",
                                 visual = "musca",
                                 achromatic = "md.r1",
                                 illum= "D65",
                                 vonkries = TRUE)
categorical.M1.flies <- colspace(vis.flowers.M1.flies, space = "categorical")
plot(categorical.M1.flies)
# write.csv2(categorical.L1.flies, file="categorical.L1.flies.csv", row.names = TRUE)

# voy a depurar el grafico para poder exportarlo, incluyendo los colores de cada flor al ojo humano:
par(mar=c(2, 2, 2, 2))
plot(categorical.M1.flies, 
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

vis.flowers.M1.beetles <- vismodel(plotM1_average2, 
                                   visual = p.israelitus, 
                                   vonkries = TRUE,
                                   illum= "D65",
                                   bkg = "green")
triangle.M1.beetles <- colspace(vis.flowers.M1.beetles, space = "tri")
plot(triangle.M1.beetles) 
# write.csv2(triangle.M1.beetles, file="triangle.M1.beetles.csv", row.names = TRUE)

# voy a depurar el grafico para poder exportarlo, incluyendo los colores de cada flor al ojo humano:
par(mar=c(1, 1, 1, 1))
plot(triangle.M1.beetles, 
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
chrom_contrast_bees <- hexagon.M1.bees$r.vec
chrom_contrast_butterflies <- tetraedro.M1.butterflies$r.vec
chrom_contrast_flies <- categorical.M1.flies$r.vec
chrom_contrast_beetles <- triangle.M1.beetles$r.vec
names_M1 <- names_M1[-(1)]
chrom_contrast_table <- data.frame(names_M1, 
                                   chrom_contrast_bees,
                                   chrom_contrast_butterflies,
                                   chrom_contrast_flies,
                                   chrom_contrast_beetles)  
head(chrom_contrast_table, n=5)

# ahora solo tengo que calcular las medias, medianas, max, min y se
mean <- apply(subset(chrom_contrast_table, select = c(2:5)), 2, mean, na.rm=TRUE)
sd <- apply(subset(chrom_contrast_table, select = c(2:5)), 2, sd, na.rm=TRUE)
se <- sd/sqrt(80)
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
achrom_contrast_bees <- abs(hexagon.M1.bees$l-0.5)
achrom_contrast_table <- data.frame(names_M1, 
                                   achrom_contrast_bees)
head(achrom_contrast_table, n=6)

# ahora solo tengo que calcular las medias, medianas, max, min y se
mean(achrom_contrast_table$achrom_contrast_bees)  # 0.3341382
sd(achrom_contrast_table$achrom_contrast_bees)/sqrt(80)  # 0.003482124
min(achrom_contrast_table$achrom_contrast_bees)  # 0.1535166
max(achrom_contrast_table$achrom_contrast_bees)  # 0.385752
median(achrom_contrast_table$achrom_contrast_bees)  # 0.3362134


# ahora voy a exportar las tablas de chromatic contrast (distancia al centro) y achromatic 
# contrast (green contrast) de cada zona/transecto; luego las analizare en un script aparte
# pero antes debo anadirles un par de columnas a ambas tablas indicandole transecto y zona

chrom_contrast_table$transect <- rep(1)
chrom_contrast_table$zone <- rep("yellow")
chrom_contrast_table$zone2 <- rep("yellow1")
head(chrom_contrast_table, n=6)
write.table(chrom_contrast_table, file="chrom_cont_M1.txt", sep=" ", dec=".", row.names= FALSE)

achrom_contrast_table$transect <- rep(1)
achrom_contrast_table$zone <- rep("yellow")
achrom_contrast_table$zone2 <- rep("yellow1")
head(achrom_contrast_table, n=6)
write.table(achrom_contrast_table, file="achrom_cont_M1.txt", sep=" ", dec=".", row.names= FALSE)


