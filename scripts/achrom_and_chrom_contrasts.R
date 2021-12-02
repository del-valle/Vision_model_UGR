library(ggplot2)
library(multcomp)


getwd()
list.files()

# he copiado en un txt todos los valores de conrtaste ACROMATICO que obtuve en los scripts
# T1_H1-T2-M2; luego copie en un excel todas las filas seguidas de los 6 txt que me genero R
# y el resultado es el archivo "achrom_cont_all.txt" que voy a cargar ahora
achrom <- read.table("/Users/delValle/Dropbox/Vision_model_UGR/achrom_cont_all.txt", header=TRUE)
achrom$transect <- as.factor(achrom$transect)  # factor
achrom$zone <- as.factor(achrom$zone)  # factor
achrom$zone2 <- as.factor(achrom$zone2)  # factor
str(achrom)
head(achrom)

aov_achrom <- aov(achrom_contrast_bees ~ transect*zone, data = achrom)
summary(aov_achrom)
#                Df Sum Sq Mean Sq F value Pr(>F)    
# transect        1 0.0015 0.00147   0.449 0.5031    
# zone            2 0.4529 0.22646  69.174 <2e-16 ***
# transect:zone   2 0.0521 0.02606   7.961 0.0004 ***
# Residuals     449 1.4699 0.00327           

# lo que vemos en el modelo es que no hay diferencias entre los transectos pero si entre las
# zonas (hibrida, morada y amarilla), lo cual era esperable
# la interaccion nos dice que las diferencias que se dan entre zonas no son las mismas en los transectos


aov_achrom_tukey <- glht(aov_achrom, linfct=mcp(zone="Tukey"))
summary(aov_achrom_tukey) # todas las comparaciones salen significativas
cld(aov_achrom_tukey)
# hybrid purple yellow 
#  "b"    "a"    "c" 
# estas son las diferencias entre las 3 zonas que vimos arriba en el modelo
plot_achrom <- ggplot(achrom, aes(x=zone, y=achrom_contrast_bees))
plot_achrom <- plot_achrom + geom_boxplot(aes(fill = zone)) + 
               scale_fill_manual(values=c("sienna3", "violet", "gold2", "sienna3", "violet", "gold2"))
plot_achrom

aov_achrom2 <- aov(achrom_contrast_bees ~ zone2, data = achrom)
aov_achrom_tukey2 <- glht(aov_achrom2, linfct=mcp(zone2="Tukey"))
summary(aov_achrom_tukey2)
cld(aov_achrom_tukey2)
# hybrid1 hybrid2 purple1 purple2 yellow1 yellow2 
#  "b"    "ab"     "a"    "ab"     "c"     "c" 
# y aqui vemos el efecto de la interaccion; si no fuera significativa, esperariamos que en el posthoc
# encontrasemos que hybrid1 y hybrid2 y que purple y purple2 estuvieran en los mismos grupos, respectivamente

plot_achrom2 <- ggplot(achrom, aes(x=transect, y=achrom_contrast_bees))
plot_achrom2 <- plot_achrom2 + geom_boxplot(aes(fill = zone)) + 
                scale_fill_manual(values=c("sienna3", "violet", "gold2", "sienna3", "violet", "gold2"))
plot_achrom2





# ahora voy a analizar el contraste CROMATICO (distancia al centro) de cada polinizador
chrom <- read.table("/Users/delValle/Dropbox/Vision_model_UGR/chrom_cont_all.txt", header=TRUE)
chrom$transect <- as.factor(chrom$transect)  # factor
chrom$zone <- as.factor(chrom$zone)  # factor
chrom$zone2 <- as.factor(chrom$zone2)  # factor
str(chrom)
head(chrom)

# abejas:
aov_chrom_bees <- aov(chrom_contrast_bees ~ transect*zone, data = chrom)
summary(aov_chrom_bees)
#                Df Sum Sq Mean Sq F value Pr(>F)    
# transect        1  0.574   0.574   106.9 <2e-16 ***
# zone            2  8.699   4.349   809.8 <2e-16 ***
# transect:zone   2  1.146   0.573   106.7 <2e-16 ***
# Residuals     449  2.411   0.005

# los dos factores y su interaccion salen significativos

aov_achrom_tukey_bees <- glht(aov_chrom_bees, linfct=mcp(zone="Tukey"))
summary(aov_achrom_tukey_bees) # todas las comparaciones salen significativas
cld(aov_achrom_tukey_bees)
# hybrid purple yellow 
#  "b"    "a"    "c" 
# estas son las diferencias entre las 3 zonas que vimos arriba en el modelo
plot_chrom_bees <- ggplot(chrom, aes(x=zone, y=chrom_contrast_bees))
plot_chrom_bees <- plot_chrom_bees + geom_boxplot(aes(fill = zone)) + 
                   scale_fill_manual(values=c("sienna3", "violet", "gold2", "sienna3", "violet", "gold2"))
plot_chrom_bees

aov_chrom_bees2 <- aov(chrom_contrast_bees ~ zone2, data = chrom)
aov_chrom_tukey_bees2 <- glht(aov_chrom_bees2, linfct=mcp(zone2="Tukey"))
summary(aov_chrom_tukey_bees2)
cld(aov_chrom_tukey_bees2)
# hybrid1 hybrid2 pruple2 purple1 yellow1 yellow2 
#  "b"     "c"     "a"     "a"     "c"     "d"

plot_chrom_bees2 <- ggplot(chrom, aes(x=transect, y=chrom_contrast_bees))
plot_chrom_bees2 <- plot_chrom_bees2 + geom_boxplot(aes(fill = zone)) + 
                    scale_fill_manual(values=c("sienna3", "violet", "gold2", "sienna3", "violet", "gold2"))
plot_chrom_bees2


# moscas:
aov_chrom_flies <- aov(chrom_contrast_flies ~ transect*zone, data = chrom)
summary(aov_chrom_flies)
#                Df Sum Sq Mean Sq F value Pr(>F)    
# transect        1 0.1828  0.1828   66.12 4.18e-15 ***
# zone            2 1.3183  0.6591  238.40  < 2e-16 ***
# transect:zone   2 0.1682  0.0841   30.43 4.06e-13 ***
# Residuals     449 1.2414  0.0028

# los dos factores y su interaccion salen significativos

aov_achrom_tukey_flies <- glht(aov_chrom_flies, linfct=mcp(zone="Tukey"))
summary(aov_achrom_tukey_flies) # la comparacion purple vs hybrid no sale significativa
cld(aov_achrom_tukey_flies)
# hybrid purple yellow 
#  "a"    "a"    "b" 
# estas son las diferencias entre las 3 zonas que vimos arriba en el modelo
plot_chrom_flies <- ggplot(chrom, aes(x=zone, y=chrom_contrast_flies))
plot_chrom_flies <- plot_chrom_flies + geom_boxplot(aes(fill = zone)) + 
                    scale_fill_manual(values=c("sienna3", "violet", "gold2", "sienna3", "violet", "gold2"))
plot_chrom_flies

aov_chrom_flies2 <- aov(chrom_contrast_flies ~ zone2, data = chrom)
aov_chrom_tukey_flies2 <- glht(aov_chrom_flies2, linfct=mcp(zone2="Tukey"))
summary(aov_chrom_tukey_flies2)
cld(aov_chrom_tukey_flies2)
# hybrid1 hybrid2 pruple2 purple1 yellow1 yellow2 
#  "a"     "b"     "a"     "a"     "c"     "d"

plot_chrom_flies2 <- ggplot(chrom, aes(x=transect, y=chrom_contrast_flies))
plot_chrom_flies2 <- plot_chrom_flies2 + geom_boxplot(aes(fill = zone)) + 
                     scale_fill_manual(values=c("sienna3", "violet", "gold2", "sienna3", "violet", "gold2"))
plot_chrom_flies2


# mariposas:
aov_chrom_butterflies <- aov(chrom_contrast_butterflies ~ transect*zone, data = chrom)
summary(aov_chrom_butterflies)
#                Df Sum Sq Mean Sq F value Pr(>F)    
# transect        1 0.2240  0.2240   220.8 <2e-16 ***
# zone            2 2.3162  1.1581  1141.2 <2e-16 ***
# transect:zone   2 0.4027  0.2013   198.4 <2e-16 ***
# Residuals     449 0.4557  0.0010

# los dos factores y su interaccion salen significativos

aov_achrom_tukey_butterflies <- glht(aov_chrom_butterflies, linfct=mcp(zone="Tukey"))
summary(aov_achrom_tukey_butterflies) # todas las comparaciones son significativas
cld(aov_achrom_tukey_butterflies)
# hybrid purple yellow 
#  "b"    "a"    "c" 
# estas son las diferencias entre las 3 zonas que vimos arriba en el modelo
plot_chrom_butterflies <- ggplot(chrom, aes(x=zone, y=chrom_contrast_butterflies))
plot_chrom_butterflies <- plot_chrom_butterflies + geom_boxplot(aes(fill = zone)) + 
                          scale_fill_manual(values=c("sienna3", "violet", "gold2", "sienna3", "violet", "gold2"))
plot_chrom_butterflies

aov_chrom_butterflies2 <- aov(chrom_contrast_butterflies ~ zone2, data = chrom)
aov_chrom_tukey_butterflies2 <- glht(aov_chrom_butterflies2, linfct=mcp(zone2="Tukey"))
summary(aov_chrom_tukey_butterflies2)
cld(aov_chrom_tukey_butterflies2)
# hybrid1 hybrid2 pruple2 purple1 yellow1 yellow2 
#  "b"     "cd"     "a"     "a"     "c"     "d"

plot_chrom_butterflies2 <- ggplot(chrom, aes(x=transect, y=chrom_contrast_butterflies))
plot_chrom_butterflies2 <- plot_chrom_butterflies2 + geom_boxplot(aes(fill = zone)) + 
                           scale_fill_manual(values=c("sienna3", "violet", "gold2", "sienna3", "violet", "gold2"))
plot_chrom_butterflies2


# escarabajos:
aov_chrom_beetles <- aov(chrom_contrast_beetles ~ transect*zone, data = chrom)
summary(aov_chrom_beetles)
#                Df Sum Sq Mean Sq F value Pr(>F)    
# transect        1 0.0857 0.08566   33.16 1.58e-08 ***
# zone            2 0.3126 0.15632   60.52  < 2e-16 ***
# transect:zone   2 0.1076 0.05382   20.84 2.22e-09 ***
# Residuals     449 1.1598 0.00258

# los dos factores y su interaccion salen significativos

aov_achrom_tukey_beetles <- glht(aov_chrom_beetles, linfct=mcp(zone="Tukey"))
summary(aov_achrom_tukey_beetles) # hybrid vs yellow marginalmente (0.05589 .), las demas si
cld(aov_achrom_tukey_beetles)
# hybrid purple yellow 
#  "b"    "a"    "b" 
# estas son las diferencias entre las 3 zonas que vimos arriba en el modelo
plot_chrom_beetles <- ggplot(chrom, aes(x=zone, y=chrom_contrast_beetles))
plot_chrom_beetles <- plot_chrom_beetles + geom_boxplot(aes(fill = zone)) + 
                      scale_fill_manual(values=c("sienna3", "violet", "gold2", "sienna3", "violet", "gold2"))
plot_chrom_beetles

aov_chrom_beetles2 <- aov(chrom_contrast_beetles ~ zone2, data = chrom)
aov_chrom_tukey_beetles2 <- glht(aov_chrom_beetles2, linfct=mcp(zone2="Tukey"))
summary(aov_chrom_tukey_beetles2)
cld(aov_chrom_tukey_beetles2)
# hybrid1 hybrid2 pruple2 purple1 yellow1 yellow2 
#  "bc"     "e"     "ab"     "a"     "cd"     "d"

plot_chrom_beetles2 <- ggplot(chrom, aes(x=transect, y=chrom_contrast_beetles))
plot_chrom_beetles2 <- plot_chrom_beetles2 + geom_boxplot(aes(fill = zone)) + 
                       scale_fill_manual(values=c("sienna3", "violet", "gold2", "sienna3", "violet", "gold2"))
plot_chrom_beetles2


