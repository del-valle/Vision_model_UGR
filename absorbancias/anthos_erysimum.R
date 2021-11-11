
setwd("~/Desktop/UGR/absorbancias")
list.files()
library(ggplot2)
library(multcomp)


# antocianinas
anthos <- read.table("anthos_erysimum.txt", header=TRUE)
anthos$Transecto <- as.factor(anthos$Transecto)
anthos$Zona <- as.factor(anthos$Zona)  # L, M, H
anthos$Zona2 <- as.factor(anthos$Zona2) # L1, L2, H1, H2, M1, M2


aov_ant <- aov(Abs_corrected_area ~ Transecto*Zona, data = anthos)
summary(aov_ant)
#              Df  Sum Sq Mean Sq F value   Pr(>F)    
# Transecto        1 0.02017 0.02017   43.28 1.45e-10 ***
# Zona             2 0.13689 0.06845  146.91  < 2e-16 ***
# Transecto:Zona   2 0.01467 0.00734   15.74 2.58e-07 ***
# Residuals      410 0.19102 0.00047

aov_ant_tukey <- glht(aov_ant, linfct=mcp(Zona="Tukey"))
summary(aov_ant_tukey) # las 3 comparaciones salen significativas
cld(aov_ant_tukey)
#  H   L   M 
# "b" "c" "a" 

plot_zones <- ggplot(anthos, aes(x=Zona, y=Abs_corrected_area))
plot_zones <- plot_zones + geom_boxplot(aes(fill = Zona)) + 
              scale_fill_manual(values=c("sienna3", "violet", "gold2"))
plot_zones

plot_transectos <- ggplot(anthos, aes(x=Transecto, y=Abs_corrected_area))
plot_transectos <- plot_transectos + geom_boxplot(aes(fill = Transecto)) + 
                   scale_fill_manual(values=c("green", "red"))
plot_transectos




aov_ant2 <- aov(Abs_corrected_area ~ Zona2, data = anthos)
summary(aov_ant2)
#              Df Sum Sq Mean Sq F value Pr(>F)    
# Zona2         5 0.1717 0.03435   73.72 <2e-16 ***
# Residuals   410 0.1910 0.00047

aov_ant_tukey2 <- glht(aov_ant2, linfct=mcp(Zona2="Tukey"))
summary(aov_ant_tukey2) # 
cld(aov_ant_tukey2)
#  H1  H2  L1  L2  M1  M2 
# "b" "a" "c" "b" "a" "a"
# hay prevalencia de amarillos en la H2, por eso se agrupan con M1 y M2
# y lo contrario sucede con H1, por eso se agrupa con L2
# L1 parece ser la zona donde las flores acumulan mayor cantidad de antocianinas

 

plot_zones2 <- ggplot(anthos, aes(x=Zona2, y=Abs_corrected_area))
plot_zones2 <- plot_zones2 + geom_boxplot(aes(fill = Zona2)) + 
               scale_fill_manual(values=c("sienna3", "sienna3", "violet", "violet", "gold2", "gold2"))
plot_zones2


anthos_average <- read.table("anthos_average.txt", header=TRUE)
anthos_average

plot_average <- ggplot(anthos_average, aes(x=zona, y=promedio))
plot_average <- plot_average + geom_point(shape=16, size=6, color=c("springgreen", "springgreen", "springgreen", "violetred1", "violetred1", "violetred1")) +
                geom_errorbar(aes(ymin=promedio-se, ymax=promedio+se), 
                              width=.3, position=position_dodge(0.05)) 
plot_average
                       





# carotenoides
carot <- read.table("carot_erysimum.txt", header=TRUE)
carot$Transecto <- as.factor(carot$Transecto)
carot$Zona <- as.factor(carot$Zona)  # L, M, H
carot$Zona2 <- as.factor(carot$Zona2) # L1, L2, H1, H2, M1, M2


aov_car <- aov(Abs_corrected_area ~ Transecto*Zona, data = carot)
summary(aov_car)
#                 Df  Sum Sq Mean Sq F value   Pr(>F)    
# Transecto        1 0.0658 0.06582   50.41 5.56e-12 ***
# Zona             2 0.3015 0.15074  115.45  < 2e-16 ***
# Transecto:Zona   2 0.0383 0.01914   14.66 7.08e-07 ***
# Residuals      409 0.5340 0.00131

aov_car_tukey <- glht(aov_car, linfct=mcp(Zona="Tukey"))
summary(aov_car_tukey) # las 3 comparaciones salen significativas
cld(aov_car_tukey)
#  H   L   M 
# "a" "a" "b" 

plot_zones <- ggplot(carot, aes(x=Zona, y=Abs_corrected_area))
plot_zones <- plot_zones + geom_boxplot(aes(fill = Zona)) + 
              scale_fill_manual(values=c("sienna3", "violet", "gold2"))
plot_zones

plot_transectos <- ggplot(carot, aes(x=Transecto, y=Abs_corrected_area))
plot_transectos <- plot_transectos + geom_boxplot(aes(fill = Transecto)) + 
                   scale_fill_manual(values=c("green", "red"))
plot_transectos


aov_car2 <- aov(Abs_corrected_area ~ Zona2, data = carot)
summary(aov_car2)
#              Df Sum Sq Mean Sq F value Pr(>F)    
# Zona2         5 0.4056 0.08111   62.13 <2e-16 ***
# Residuals   409 0.5340 0.00131

aov_car_tukey2 <- glht(aov_car2, linfct=mcp(Zona2="Tukey"))
summary(aov_car_tukey2) # 
cld(aov_car_tukey2)
#  H1  H2  L1  L2  M1  M2 
# "a" "b" "a" "a" "b" "c"
# hay prevalencia de amarillos en la H2, por eso se agrupa con M1
# y lo contrario sucede con H1, por eso se agrupa con L1 y L2
# M2 parece ser la zona donde las flores acumulan mayor cantidad de carotenoides
# existe una fuerte influencia del transecto



plot_zones2 <- ggplot(carot, aes(x=Zona2, y=Abs_corrected_area))
plot_zones2 <- plot_zones2 + geom_boxplot(aes(fill = Zona2)) + 
               scale_fill_manual(values=c("sienna3", "sienna3", "violet", "violet", "gold2", "gold2"))
plot_zones2


carot_average <- read.table("carot_average.txt", header=TRUE)
carot_average

plot_average <- ggplot(carot_average, aes(x=zona, y=promedio))
plot_average <- plot_average + geom_point(shape=16, size=6, color=c("springgreen", "springgreen", "springgreen", "violetred1", "violetred1", "violetred1")) +
                geom_errorbar(aes(ymin=promedio-se, ymax=promedio+se), 
                width=.3, position=position_dodge(0.05)) 
plot_average


