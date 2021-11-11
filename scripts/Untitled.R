L1 <- "~/Desktop/UGR/T1_Parental_El_179-259"
M1 <- "~/Desktop/UGR/T1_Parental_Emx_21-100"     
H2 <- "~/Desktop/UGR/T2_Hibrida_321-393"
L2 <- "~/Desktop/UGR/T2_Parental_El_1-20_260-320"
M2 <- "~/Desktop/UGR/T2_Parental_Emx_401-480" 




plotL1<-getspec(L1)
plotL1.sm <- procspec(plotL1, opt = "smooth", span = 0.2)
is.rspec(plotL1.sm) 

colouremtric.variables.plotL1.sm <- summary(plotL1.sm)
write.csv2(colouremtric.variables.plotL1.sm, file="plotL1.sm.csv", row.names = TRUE)

vis.flowers.L1 <- vismodel(plotL1.sm,
                           visual = "apis", qcatch = "Ei", relative = FALSE,
                           vonkries = TRUE, achromatic = "l", bkg = "green", illum="D65"
)
hexagon.L1 <- colspace(vis.flowers.L1, space = "hexagon")
hexplot(hexagon.L1, sectors="coarse", labels=TRUE, main="Purple Transect 1", cex.main=1.5, col="darkorchid")




plotM1<-getspec(M1)
plotM1.sm <- procspec(plotM1, opt = "smooth", span = 0.2)
is.rspec(plotM1.sm) 

colouremtric.variables.plotM1.sm <- summary(plotM1.sm)
write.csv2(colouremtric.variables.plotM1.sm, file="plotM1.sm.csv", row.names = TRUE)

vis.flowers.M1 <- vismodel(plotM1.sm,
                           visual = "apis", qcatch = "Ei", relative = FALSE,
                           vonkries = TRUE, achromatic = "l", bkg = "green", illum="D65"
)
hexagon.M1 <- colspace(vis.flowers.M1, space = "hexagon")
hexplot(hexagon.M1, sectors="coarse", labels=TRUE, main="Yellow Transect 1", cex.main=1.5, col="gold2")



plotH2<-getspec(H2)
plotH2.sm <- procspec(plotH1, opt = "smooth", span = 0.2)
is.rspec(plotH2.sm) 

colouremtric.variables.plotH2.sm <- summary(plotH2.sm)
write.csv2(colouremtric.variables.plotH2.sm, file="plotH2.sm.csv", row.names = TRUE)

vis.flowers.H2 <- vismodel(plotH2.sm,
                           visual = "apis", qcatch = "Ei", relative = FALSE,
                           vonkries = TRUE, achromatic = "l", bkg = "green", illum="D65"
)
hexagon.H2 <- colspace(vis.flowers.H2, space = "hexagon")
hexplot(hexagon.H2, sectors="coarse", labels=TRUE, main="Hybrid Transect 2", cex.main=1.5, col="gold2")



plotL2<-getspec(L2)
plotL2.sm <- procspec(plotL2, opt = "smooth", span = 0.2)
is.rspec(plotL2.sm) 

colouremtric.variables.plotL2.sm <- summary(plotL2.sm)
write.csv2(colouremtric.variables.plotL2.sm, file="plotL2.sm.csv", row.names = TRUE)

vis.flowers.L2 <- vismodel(plotL2.sm,
                           visual = "apis", qcatch = "Ei", relative = FALSE,
                           vonkries = TRUE, achromatic = "l", bkg = "green", illum="D65"
)
hexagon.L2 <- colspace(vis.flowers.L2, space = "hexagon")
hexplot(hexagon.L2, sectors="coarse", labels=TRUE, main="Purple Transect 2", cex.main=1.5, col="darkorchid")



plotM2<-getspec(M2)
plotM2.sm <- procspec(plotM2, opt = "smooth", span = 0.2)
is.rspec(plotM2.sm) 

colouremtric.variables.plotM2.sm <- summary(plotM2.sm)
write.csv2(colouremtric.variables.plotM2.sm, file="plotM2.sm.csv", row.names = TRUE)

vis.flowers.M2 <- vismodel(plotM2.sm,
                           visual = "apis", qcatch = "Ei", relative = FALSE,
                           vonkries = TRUE, achromatic = "l", bkg = "green", illum="D65"
)
hexagon.M2 <- colspace(vis.flowers.M2, space = "hexagon")
hexplot(hexagon.M2, sectors="coarse", labels=TRUE, main="Yellow Transect 2", cex.main=1.5, col="gold2")



plotH1_L1_M1<- cbind(plotH1,plotL1[,2:163],plotM1[,2:161])
dim(plotH1_L1_M1)
dim(plotH1)
dim(plotM1)
dim(plotL1)

plotH1_L1_M1
plotH1_L1_M1.sm <- procspec(plotH1_L1_M1, opt = "smooth", span = 0.2)
is.rspec(plotH1_L1_M1.sm) 

colouremtric.variables.plotH1_L1_M1.sm <- summary(plotH1_L1_M1.sm)
write.csv2(colouremtric.variables.plotH1_L1_M1.sm, file="plotH1_L1_M1.sm.csv", row.names = TRUE)

vis.flowers.plotH1_L1_M1 <- vismodel(plotH1_L1_M1.sm,
                                     visual = "apis", qcatch = "Ei", relative = FALSE,
                                     vonkries = TRUE, achromatic = "l", bkg = "green", illum="D65"
)
hexagon.H1_L1_M1 <- colspace(vis.flowers.plotH1_L1_M1, space = "hexagon")
hexplot(hexagon.H1_L1_M1, sectors="coarse", labels=TRUE, main="Yellow Transect 2", cex.main=1.5, col="gold2")

