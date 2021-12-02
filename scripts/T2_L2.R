
getwd()
list.files()

# ZONA L2, purple zone transect 2
L2 <- "/Users/delValle/Dropbox/Vision_model_UGR/T2_Parental_El_1-20_260-320"
# cambiar por otro directorio si fuera necesario

  
library(pavo)

plotL2<-getspec(L2)
# voy a pulir un poco los espectros que se obtienen en bruto del esptrofotometro

plotL2.fix <- procspec(plotL2, fixneg = "addmin") 
# primero elimino posibles valores negativos anadiendo el valor absoluto del valor mas negativo a todo el espectro
plotL2.sm <- procspec(plotL2.fix, opt = "smooth", span = 0.2)
# luego suavizo un poco los espectros
is.rspec(plotL2.sm)   # [1] TRUE
plot(plotL2.sm$`T2_10a_Reflection__14__00-26-14-978`, type = "l")
plot(plotL2.sm$`T2_10b_Reflection__13__00-25-34-978`, type = "l")
# en estos espectros apenas se perciben diferencias entre los espectros de la parte basal y apical


# lo primero que hay que hacer es promediar la parte basal y apical, es decir, las 2 columnas contiguas
# medidas para cada muestra; voy a hacer primero un pequeno test con unas pocas columnas de forma manual 
# que me servira como referencia mas adelante para verificar que lo que estoy haciendo esta bien


plotL2.sm_manual <- plotL2.sm$wl
plotL2.sm_manual <- as.data.frame(plotL2.sm_manual)
plotL2.sm_manual$T2_10 <- apply(plotL2.sm[ ,c(2,3)], 1, mean, na.rm = TRUE)  # https://es.stackoverflow.com/questions/194515/c%C3%B3mo-promedio-variables-por-fila
plotL2.sm_manual$T2_11 <- apply(plotL2.sm[ ,c(4,5)], 1, mean, na.rm = TRUE)  
plotL2.sm_manual$T2_12 <- apply(plotL2.sm[ ,c(6,7)], 1, mean, na.rm = TRUE)  
plotL2.sm_manual$T2_13 <- apply(plotL2.sm[ ,c(8,9)], 1, mean, na.rm = TRUE)  
plotL2.sm_manual <- as.rspec(plotL2.sm_manual)
is.rspec(plotL2.sm_manual)
head(plotL2.sm_manual, n=5)  
# estas serian valores que tengo que obtener de las muestras 10 a 13, pero es inviable hacerlo manualmente
# asi que voy a crear un loop par que automaticamente me calcule el promedio cada 2 columnas:



# option 1: para hacerlo de la forma que lo he hecho arriba, seria asi (pongo menos explicaciones):
plotL2_average <- plotL2.sm$wl
plotL2_average <- as.data.frame(plotL2_average)
colnames(plotL2_average) <- c("wl")
head(plotL2_average)

for(i in 2:155) {       
  plotL2_average[ , i] <- apply(plotL2.sm[ , c(i, i+1)], 1, mean, na.rm=TRUE)
}

plotL2_average[1:5,1:6]
plotL2_average[1:5,150:154]

plotL2_average2 <- plotL2_average[ , -c(1)]
col_odd <- seq_len(ncol(plotL2_average2)) %% 2  
plotL2_average2 <- plotL2_average2[ , col_odd == 1]
rm(col_odd)

plotL2_average2$wl <- plotL2_average$wl
library(dplyr)
plotL2_average2 <- plotL2_average2 %>%
  select(wl, everything())
plotL2_average2[1:4,1:5]

x0 <- c("wl")
x1 <- 10:19
x2 <- c("1")
x3 <- c("20")
x4 <- 261:299
x5 <- c("2")
x6 <- 300:311
x7 <- 313:320
x8 <- 3:4
x9 <- 6:8
x_sum <- c(x1,x2,x3,x4,x5,x6,x7,x8,x9)
x_sum <- paste("T1", x_sum, sep="_") 
head(x_sum)
names_L2 <- c(x0,x_sum)
names_L2

colnames(plotL2_average2) <- names_L2
head(plotL2_average2, n=5)
plotL2_average2 <- as.rspec(plotL2_average2)
is.rspec(plotL2_average2)

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
rm(plotL2_average)


# # option 2: para hacerlo mas fino, seria asi:
# plotL2_average <- plotL2.sm$wl
# plotL2_average <- as.data.frame(plotL2_average)
# colnames(plotL2_average) <- c("wl")
# head(plotL2_average)
# 
# i_seq <- seq(from=2, to=139, by=2)
# 
# for(i in i_seq) {
#         plotL2_average[ , i] <- apply(plotL2.sm[ , c(i, i+1)], 1, mean, na.rm=TRUE)
# }
# 
# 
# plotL2_average
# plotL2_average[1:5,1:5]
# plotL2.sm_manual[1:5,1:5]
# # no me termina de salir bien el loop, tengo que darle alguna vuelta mas



# variables colorimetricas (hue, chroma, spectral purity,. etc.)
colouremetric.variables.plotL2_average2 <- summary(plotL2_average2)
# write.csv2(colouremetric.variables.plotL2_average2, 
#            file="plotL2_average2_colourimetric_variables.csv", row.names = TRUE)

# los espectros de las 80 plantas presentes en la zona L2:
plot(plotL2_average2, type = "o", col = spec2rgb(plotL2_average2))
# extraigo los colores de cada espectro para colorear los puntos en los modelos de vision
rgb <- spec2rgb(plotL2_average2) 

# ahora se representan las 78 flores de la zona L2 en los distintos espacios de color:
# 1) modelo de vision de las ABEJAS:
vis.flowers.L2.bees <- vismodel(plotL2_average2,
                         visual = "apis", 
                         qcatch = "Ei", 
                         relative = FALSE,
                         vonkries = TRUE, 
                         achromatic = "l", 
                         bkg = "green", 
                         illum="D65"
)
hexagon.L2.bees <- colspace(vis.flowers.L2.bees, space = "hexagon")
hexplot(hexagon.L2.bees, 
        sectors="coarse", 
        labels=TRUE, 
        main="Purple Transect 1", 
        cex.main=1.5, 
        col="black")
# write.csv2(hexagon.L2.bees, file="hexagon.L2.bees.csv", row.names = TRUE)

# voy a depurar el grafico para poder exportarlo, incluyendo los colores de cada flor al ojo humano:
par(mar=c(1, 1, 1, 1))
hexplot(hexagon.L2.bees, 
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

vis.flowers.L2.butterflies <- vismodel(plotL2_average2, 
                                visual = papilio, 
                                vonkries = TRUE,
                                illum= "D65",
                                bkg = "green")
tetraedro.L2.butterflies <- colspace(vis.flowers.L2.butterflies, space = "tcs")
plot(tetraedro.L2.butterflies) 
# write.csv2(tetraedro.H1.butterflies, file="tetraedro.H1.butterflies.csv", row.names = TRUE)

# voy a depurar el grafico para poder exportarlo, incluyendo los colores de cada flor al ojo humano:
par(mar=c(1, 1, 1, 1))
plot(tetraedro.L2.butterflies, 
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
vis.flowers.L2.flies <- vismodel(plotL2_average2,
                                 bkg = "green",
                                 visual = "musca",
                                 achromatic = "md.r1",
                                 illum= "D65",
                                 vonkries = TRUE)
categorical.L2.flies <- colspace(vis.flowers.L2.flies, space = "categorical")
plot(categorical.L2.flies)
# write.csv2(categorical.L1.flies, file="categorical.L1.flies.csv", row.names = TRUE)

# voy a depurar el grafico para poder exportarlo, incluyendo los colores de cada flor al ojo humano:
par(mar=c(2, 2, 2, 2))
plot(categorical.L2.flies, 
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

vis.flowers.L2.beetles <- vismodel(plotL2_average2, 
                                   visual = p.israelitus, 
                                   vonkries = TRUE,
                                   illum= "D65",
                                   bkg = "green")
triangle.L2.beetles <- colspace(vis.flowers.L2.beetles, space = "tri")
plot(triangle.L2.beetles) 
# write.csv2(triangle.L2.beetles, file="triangle.L2.beetles.csv", row.names = TRUE)

# voy a depurar el grafico para poder exportarlo, incluyendo los colores de cada flor al ojo humano:
par(mar=c(1, 1, 1, 1))
plot(triangle.L2.beetles, 
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
chrom_contrast_bees <- hexagon.L2.bees$r.vec
chrom_contrast_butterflies <- tetraedro.L2.butterflies$r.vec
chrom_contrast_flies <- categorical.L2.flies$r.vec
chrom_contrast_beetles <- triangle.L2.beetles$r.vec
names_L2 <- names_L2[-(1)]
chrom_contrast_table <- data.frame(names_L2, 
                                   chrom_contrast_bees,
                                   chrom_contrast_butterflies,
                                   chrom_contrast_flies,
                                   chrom_contrast_beetles)  
head(chrom_contrast_table, n=5)

# ahora solo tengo que calcular las medias, medianas, max, min y se
mean <- apply(subset(chrom_contrast_table, select = c(2:5)), 2, mean, na.rm=TRUE)
sd <- apply(subset(chrom_contrast_table, select = c(2:5)), 2, sd, na.rm=TRUE)
se <- sd/sqrt(77)
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
achrom_contrast_bees <- abs(hexagon.L2.bees$l-0.5)
achrom_contrast_table <- data.frame(names_L2, 
                                   achrom_contrast_bees)
head(achrom_contrast_table, n=6)

# ahora solo tengo que calcular las medias, medianas, max, min y se
mean(achrom_contrast_table$achrom_contrast_bees)  # 0.2688288
sd(achrom_contrast_table$achrom_contrast_bees)/sqrt(77)  # 0.005670457
min(achrom_contrast_table$achrom_contrast_bees)  # 0.1062535
max(achrom_contrast_table$achrom_contrast_bees)  # 0.3736638
median(achrom_contrast_table$achrom_contrast_bees)  # 0.2706339


# ahora voy a exportar las tablas de chromatic contrast (distancia al centro) y achromatic 
# contrast (green contrast) de cada zona/transecto; luego las analizare en un script aparte
# pero antes debo anadirles un par de columnas a ambas tablas indicandole transecto y zona

chrom_contrast_table$transect <- rep(2)
chrom_contrast_table$zone <- rep("purple")
chrom_contrast_table$zone2 <- rep("pruple2")
head(chrom_contrast_table, n=6)
write.table(chrom_contrast_table, file="chrom_cont_L2.txt", sep=" ", dec=".", row.names= FALSE)

achrom_contrast_table$transect <- rep(2)
achrom_contrast_table$zone <- rep("purple")
achrom_contrast_table$zone2 <- rep("purple2")
head(achrom_contrast_table, n=6)
write.table(achrom_contrast_table, file="achrom_cont_L2.txt", sep=" ", dec=".", row.names= FALSE)

