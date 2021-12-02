
getwd()
list.files()

# ZONA L1, purple zone
L1 <- "/Users/delValle/Dropbox/Vision_model_UGR/T1_Parental_El_179-259"
# cambiar por otro directorio si fuera necesario

  
library(pavo)

plotL1<-getspec(L1)
# voy a pulir un poco los espectros que se obtienen en bruto del esptrofotometro

plotL1.fix <- procspec(plotL1, fixneg = "addmin") 
# primero elimino posibles valores negativos anadiendo el valor absoluto del valor mas negativo a todo el espectro
plotL1.sm <- procspec(plotL1.fix, opt = "smooth", span = 0.2)
# luego suavizo un poco los espectros
is.rspec(plotL1.sm)   # [1] TRUE
plot(plotL1.sm$`T1_179a_Reflection__156__00-20-01-039`, type = "l")
plot(plotL1.sm$`T1_179b_Reflection__155__00-19-33-039`, type = "l")
# en estos espectros apenas se perciben diferencias entre los espectros de la parte basal y apical


# lo primero que hay que hacer es promediar la parte basal y apical, es decir, las 2 columnas contiguas
# medidas para cada muestra; voy a hacer primero un pequeno test con unas pocas columnas de forma manual 
# que me servira como referencia mas adelante para verificar que lo que estoy haciendo esta bien


plotL1.sm_manual <- plotL1.sm$wl
plotL1.sm_manual <- as.data.frame(plotL1.sm_manual)
plotL1.sm_manual$T1_179 <- apply(plotL1.sm[ ,c(2,3)], 1, mean, na.rm = TRUE)  # https://es.stackoverflow.com/questions/194515/c%C3%B3mo-promedio-variables-por-fila
plotL1.sm_manual$T1_180 <- apply(plotL1.sm[ ,c(4,5)], 1, mean, na.rm = TRUE)  
plotL1.sm_manual$T1_181 <- apply(plotL1.sm[ ,c(6,7)], 1, mean, na.rm = TRUE)  
plotL1.sm_manual$T1_182 <- apply(plotL1.sm[ ,c(8,9)], 1, mean, na.rm = TRUE)  
plotL1.sm_manual <- as.rspec(plotL1.sm_manual)
is.rspec(plotL1.sm_manual)
head(plotL1.sm_manual, n=5)  
# estas serian valores que tengo que obtener de las muestras 179 a 182, pero es inviable hacerlo manualmente
# asi que voy a crear un loop par que automaticamente me calcule el promedio cada 2 columnas:



# option 1: para hacerlo de la forma que lo he hecho arriba, seria asi (pongo menos explicaciones):
plotL1_average <- plotL1.sm$wl
plotL1_average <- as.data.frame(plotL1_average)
colnames(plotL1_average) <- c("wl")
head(plotL1_average)

for(i in 2:163) {       
  plotL1_average[ , i] <- apply(plotL1.sm[ , c(i, i+1)], 1, mean, na.rm=TRUE)
}

plotL1_average[1:5,1:6]
plotL1_average[1:5,158:162]

plotL1_average2 <- plotL1_average[ , -c(1)]
col_odd <- seq_len(ncol(plotL1_average2)) %% 2  
plotL1_average2 <- plotL1_average2[ , col_odd == 1]
rm(col_odd)

plotL1_average2$wl <- plotL1_average$wl
library(dplyr)
plotL1_average2 <- plotL1_average2 %>%
  select(wl, everything())
plotL1_average2[1:4,1:5]

x0 <- c("wl")
x1 <- 179:259
x1 <- paste("T1", x1, sep="_") 
head(x1)
names_L1 <- c(x0,x1)
names_L1

colnames(plotL1_average2) <- names_L1
head(plotL1_average2, n=5)
plotL1_average2 <- as.rspec(plotL1_average2)
is.rspec(plotL1_average2)

rm(x0)
rm(x1)
# rm(names_H1)  me hara falta mas adelante
rm(i)
rm(plotL1_average)


# # option 2: para hacerlo mas fino, seria asi:
# plotL1_average <- plotL1.sm$wl
# plotL1_average <- as.data.frame(plotL1_average)
# colnames(plotL1_average) <- c("wl")
# head(plotL1_average)
# 
# i_seq <- seq(from=2, to=163, by=2)
# 
# for(i in i_seq) {
#         plotL1_average[ , i] <- apply(plotL1.sm[ , c(i, i+1)], 1, mean, na.rm=TRUE)
# }
# 
# 
# plotL1_average
# plotL1_average[1:5,1:5]
# plotL1.sm_manual[1:5,1:5]
# # no me termina de salir bien el loop, tengo que darle alguna vuelta mas



# variables colorimetricas (hue, chroma, spectral purity,. etc.)
colouremetric.variables.plotL1_average2 <- summary(plotL1_average2)
# write.csv2(colouremetric.variables.plotL1_average2, 
#            file="plotL1_average2_colourimetric_variables.csv", row.names = TRUE)

# los espectros de las 81 plantas presentes en la zona L1:
plot(plotL1_average2, type = "o", col = spec2rgb(plotL1_average2))
# extraigo los colores de cada espectro para colorear los puntos en los modelos de vision
rgb <- spec2rgb(plotL1_average2) 

# ahora se representan las 81 flores de la zona L1 en los distintos espacios de color:
# 1) modelo de vision de las ABEJAS:
vis.flowers.L1.bees <- vismodel(plotL1_average2,
                         visual = "apis", 
                         qcatch = "Ei", 
                         relative = FALSE,
                         vonkries = TRUE, 
                         achromatic = "l", 
                         bkg = "green", 
                         illum="D65"
)
hexagon.L1.bees <- colspace(vis.flowers.L1.bees, space = "hexagon")
hexplot(hexagon.L1.bees, 
        sectors="coarse", 
        labels=TRUE, 
        main="Purple Transect 1", 
        cex.main=1.5, 
        col="black")
# write.csv2(hexagon.L1.bees, file="hexagon.L1.bees.csv", row.names = TRUE)

# voy a depurar el grafico para poder exportarlo, incluyendo los colores de cada flor al ojo humano:
par(mar=c(1, 1, 1, 1))
hexplot(hexagon.L1.bees, 
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

vis.flowers.L1.butterflies <- vismodel(plotL1_average2, 
                                visual = papilio, 
                                vonkries = TRUE,
                                illum= "D65",
                                bkg = "green")
tetraedro.L1.butterflies <- colspace(vis.flowers.L1.butterflies, space = "tcs")
plot(tetraedro.L1.butterflies) 
# write.csv2(tetraedro.H1.butterflies, file="tetraedro.H1.butterflies.csv", row.names = TRUE)

# voy a depurar el grafico para poder exportarlo, incluyendo los colores de cada flor al ojo humano:
par(mar=c(1, 1, 1, 1))
plot(tetraedro.L1.butterflies, 
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
vis.flowers.L1.flies <- vismodel(plotL1_average2,
                                 bkg = "green",
                                 visual = "musca",
                                 achromatic = "md.r1",
                                 illum= "D65",
                                 vonkries = TRUE)
categorical.L1.flies <- colspace(vis.flowers.L1.flies, space = "categorical")
plot(categorical.L1.flies)
# write.csv2(categorical.L1.flies, file="categorical.L1.flies.csv", row.names = TRUE)

# voy a depurar el grafico para poder exportarlo, incluyendo los colores de cada flor al ojo humano:
par(mar=c(2, 2, 2, 2))
plot(categorical.L1.flies, 
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

vis.flowers.L1.beetles <- vismodel(plotL1_average2, 
                                   visual = p.israelitus, 
                                   vonkries = TRUE,
                                   illum= "D65",
                                   bkg = "green")
triangle.L1.beetles <- colspace(vis.flowers.L1.beetles, space = "tri")
plot(triangle.L1.beetles) 
# write.csv2(triangle.L1.beetles, file="triangle.L1.beetles.csv", row.names = TRUE)

# voy a depurar el grafico para poder exportarlo, incluyendo los colores de cada flor al ojo humano:
par(mar=c(1, 1, 1, 1))
plot(triangle.L1.beetles, 
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
chrom_contrast_bees <- hexagon.L1.bees$r.vec
chrom_contrast_butterflies <- tetraedro.L1.butterflies$r.vec
chrom_contrast_flies <- categorical.L1.flies$r.vec
chrom_contrast_beetles <- triangle.L1.beetles$r.vec
names_L1 <- names_L1[-(1)]
chrom_contrast_table <- data.frame(names_L1, 
                                   chrom_contrast_bees,
                                   chrom_contrast_butterflies,
                                   chrom_contrast_flies,
                                   chrom_contrast_beetles)
head(chrom_contrast_table, n=5)

# ahora solo tengo que calcular las medias, medianas, max, min y se
mean <- apply(subset(chrom_contrast_table, select = c(2:5)), 2, mean, na.rm=TRUE)
sd <- apply(subset(chrom_contrast_table, select = c(2:5)), 2, sd, na.rm=TRUE)
se <- sd/sqrt(81)
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
achrom_contrast_bees <- abs(hexagon.L1.bees$l-0.5)
achrom_contrast_table <- data.frame(names_L1, 
                                   achrom_contrast_bees)
head(achrom_contrast_table, n=6)

# ahora solo tengo que calcular las medias, medianas, max, min y se
mean(achrom_contrast_table$achrom_contrast_bees)  # 0.2427705
sd(achrom_contrast_table$achrom_contrast_bees)/sqrt(81)  # 0.006375478
min(achrom_contrast_table$achrom_contrast_bees)  # 0.06649248
max(achrom_contrast_table$achrom_contrast_bees)  # 0.3674362
median(achrom_contrast_table$achrom_contrast_bees)  # 0.251627


# ahora voy a exportar las tablas de chromatic contrast (distancia al centro) y achromatic 
# contrast (green contrast) de cada zona/transecto; luego las analizare en un script aparte
# pero antes debo anadirles un par de columnas a ambas tablas indicandole transecto y zona

chrom_contrast_table$transect <- rep(1)
chrom_contrast_table$zone <- rep("purple")
chrom_contrast_table$zone2 <- rep("purple1")
head(chrom_contrast_table, n=6)
write.table(chrom_contrast_table, file="chrom_cont_L1.txt", sep=" ", dec=".", row.names= FALSE)

achrom_contrast_table$transect <- rep(1)
achrom_contrast_table$zone <- rep("purple")
achrom_contrast_table$zone2 <- rep("purple1")
head(achrom_contrast_table, n=6)
write.table(achrom_contrast_table, file="achrom_cont_L1.txt", sep=" ", dec=".", row.names= FALSE)

