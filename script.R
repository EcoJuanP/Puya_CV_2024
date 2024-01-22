# Tesis -----
# Juan Jose Pinzon
# Rasgos funcionales de Puya en zonas afectadas por regimen de fuego.
# Datos en : https://github.com/Antarticc
# 2023
citation("stats")
# Librerias ----
library(ggplot2)
library(ggpubr)
library(cowplot)
library(hrbrthemes)
library(GGally)
library(viridis)
library(ggpmisc)
library(dplyr)
library(reshape2)
library(car)
library(PMCMRplus)
library(gridExtra)
library(tidyverse)
library(tidyr)
library(gt)
library(glue)
library(tibble)
library(vegan)
library(moments)
library(FactoMineR)
library(factoextra)
library(textshape)
library(mdthemes)
# Datos ----
#Normalidad
# 2002
rownames(Datos_goudotiana2002) <- Datos_goudotiana2002$Ind
Datos_goudotiana2002$Ind <- NULL
Goudotiana2002shap <- data.frame(
  Variable <- character(),
  P_valor <- numeric(),
  stringsAsFactors = FALSE
)
for (col in names(Datos_goudotiana2002)) {
  if(length(unique(Datos_goudotiana2002[[col]])) > 1) {
    resultado_testG2 <- shapiro.test(Datos_goudotiana2002[[col]])
    Goudotiana2002shap<- rbind(Goudotiana2002shap, data.frame(Variable = col, P_valor = resultado_testG2$p.value))
  } else {
    warning(paste("No se pudo", col, "valores iguales"))
  }
}

#1988

rownames(Datos_goudotiana1988) <- Datos_goudotiana1988$Ind
Datos_goudotiana1988$Ind <- NULL
Goudotiana1988shap <- data.frame(
  Variable <- character(),
  P_valor <- numeric(),
  stringsAsFactors = FALSE
)
for (col in names(Datos_goudotiana1988)) {
  if(length(unique(Datos_goudotiana1988[[col]])) > 1) {
    resultado_testG1 <- shapiro.test(Datos_goudotiana1988[[col]])
    Goudotiana1988shap<- rbind(Goudotiana1988shap, data.frame(Variable = col, P_valor = resultado_testG1$p.value))
  } else {
    warning(paste("No se pudo", col, "valores iguales"))
  }
}

#sin
rownames(Datos_goudotianaSin) <- Datos_goudotianaSin$Ind
Datos_goudotianaSin$Ind <- NULL
GoudotianaSinshap <- data.frame(
  Variable <- character(),
  P_valor <- numeric(),
  stringsAsFactors = FALSE
)
for (col in names(Datos_goudotianaSin)) {
  if(length(unique(Datos_goudotianaS[[col]])) > 1) {
    resultado_testG <- shapiro.test(Datos_goudotianaSin[[col]])
    GoudotianaSinshap<- rbind(GoudotianaSinshap, data.frame(Variable = col, P_valor = resultado_testG$p.value))
  } else {
    warning(paste("No se pudo", col, "valores iguales"))
  }
}


#Varianza
rownames(Datos_goudotiana) <- Datos_goudotiana$Ind
Datos_goudotiana$Ind <- NULL
Goudotianaleven <- data.frame(
  Variable <- character(),
  P_valor <- numeric(),
  stringsAsFactors = FALSE
)
for (col in names(Datos_goudotiana)) {
  if(length(unique(Datos_goudotiana[[col]])) > 1) {
    resultado_testGl <- shapiro.test(Datos_goudotiana[[col]])
    Goudotianaleven<- rbind(Goudotianaleven, data.frame(Variable = col, P_valor = resultado_testG$p.value))
  } else {
    warning(paste("No se pudo", col, "valores iguales"))
  }
}

#ANOVA
#LT
anovalt <- aov(Datos_goudotiana$Lt ~ Datos_goudotiana$Fuegos)
summary(anovalt)
ggplot(data = Datos_goudotiana, aes(x = Fuegos, y = Lt, color = Fuegos)) +
  geom_boxplot() +
  theme_bw()
#Hmax
anovhmax <- aov(Datos_goudotiana$Hmax ~ Datos_goudotiana$Fuegos)
summary(anovhmax)
ggplot(data = Datos_goudotiana, aes(x = Fuegos, y = Hmax, color = Fuegos)) +
  geom_boxplot() +
  theme_bw()

#RD

anovRD <- aov(Datos_goudotiana$RD ~ Datos_goudotiana$Fuegos)
summary(anovRD)
ggplot(data = Datos_goudotiana, aes(x = Fuegos, y = RD, color = Fuegos)) +
  geom_boxplot() +
  theme_bw()

#LA

anovLA <- aov(Datos_goudotiana$LA ~ Datos_goudotiana$Fuegos)
summary(anovLA)
ggplot(data = Datos_goudotiana, aes(x = Fuegos, y = LA, color = Fuegos)) +
  geom_boxplot() +
  theme_bw()

#LDMC

anovLDMC <- aov(Datos_goudotiana$LDMC ~ Datos_goudotiana$Fuegos)
summary(anovLDMC)
ggplot(data = Datos_goudotiana, aes(x = Fuegos, y = LDMC, color = Fuegos)) +
  geom_boxplot() +
  theme_bw()

#SLA

anovSLA <- aov(Datos_goudotiana$SLA ~ Datos_goudotiana$Fuegos)
summary(anovSLA)
ggplot(data = Datos_goudotiana, aes(x = Fuegos, y = SLA, color = Fuegos)) +
  geom_boxplot() +
  theme_bw()

#Branching

anovBr <- aov(Datos_goudotiana$Branching.frequency.per.mm ~ Datos_goudotiana$Fuegos)
summary(anovBr)
ggplot(data = Datos_goudotiana, aes(x = Fuegos, y = Branching.frequency.per.mm, color = Fuegos)) +
  geom_boxplot() +
  theme_bw() + labs(y = "Branching Frequency(mm)")

#Average diameter

anovAd <- aov(Datos_goudotiana$Average.Diameter.mm ~ Datos_goudotiana$Fuegos)
summary(anovAd)
ggplot(data = Datos_goudotiana, aes(x = Fuegos, y = Average.Diameter.mm, color = Fuegos)) +
  geom_boxplot() +
  theme_bw() + labs(y = "Average Diameter(mm)")

#Surface area

anovsa <- aov(Datos_goudotiana$Surface.Area.mm2 ~ Datos_goudotiana$Fuegos)
summary(anovsa)
ggplot(data = Datos_goudotiana, aes(x = Fuegos, y = Surface.Area.mm2, color = Fuegos)) +
  geom_boxplot() +
  theme_bw() + labs(y = "Surface area(mm2)")

#Surface area

anovsrl <- aov(Datos_goudotiana$SRL ~ Datos_goudotiana$Fuegos)
summary(anovsrl)
ggplot(data = Datos_goudotiana, aes(x = Fuegos, y = SRL, color = Fuegos)) +
  geom_boxplot() +
  theme_bw() + labs(y = "SRL")

#RTD

anovrtd <- aov(Datos_goudotiana$RTD ~ Datos_goudotiana$Fuegos)
summary(anovrtd)
ggplot(data = Datos_goudotiana, aes(x = Fuegos, y = RTD, color = Fuegos)) +
  geom_boxplot() +
  theme_bw() + labs(y = "RTD")
#SRA

anovsra<- aov(SRA ~ Fuegos, data = Datos_goudotiana)
summary(anovsra)
ggplot(data = Datos_goudotiana, aes(x = Fuegos, y = SRA, color = Fuegos)) +
  geom_boxplot() +
  theme_bw() + labs(y = "SRA")

#No parametricos


#BI
kruskal.test(Bi ~ Fuegos, data = Datos_goudotiana)
ggplot(data = Datos_goudotiana, aes(x = Fuegos, y = Bi, color = Fuegos)) +
  geom_boxplot() +
  theme_bw() + labs(y = "Bi")

# Colonizacion
c2002 <- c(0,5,2,2,7,1)
c1988 <- c(0,14,5,18,6,9,0,5,1,1,9,0,2,7,4,5,7,2,2,0,0)
cSF <- c(4,2,5,6,0,0,0,3,3,0,0,2) 
cG <- c(0,5,2,2,7,1,0,1,4,2,5,6,0,2,7,4,0,2)
cS <- c(0,5,0)


shapiro.test(c2002)
shapiro.test(c1988)
shapiro.test(cSF)
shapiro.test(cG)

leveneTest(y = TotalC$TotalC, group = TotalC$Puya)
leveneTest(y = TotalC$TotalC, group = TotalC$RF)

anova2C <- aov(TotalC ~ Puya + RF, data = TotalC)
summary(anova2C)
plot(anova2C)

#

shapiro.test(TotalCG$TotalC)
shapiro.test(TotalCN$TotalC)

leveneTest(y= TotalCG$TotalC, group = TotalCG$RF)
leveneTest(y= TotalCN$TotalC, group = TotalCN$RF)

wilcox.test(TotalCN$TotalC ~ TotalCN$RF)
kruskal.test(TotalCG$TotalC ~ TotalCG$RF)


# PCA ----
#Rasgos
####
a<-textshape::column_to_rownames(Datos_por_individuo, loc = 1)

b <- subset(a, select = -c(Puya, Fuego, Fuegos))
suprs <- subset(b, select = -c(Branching.frequency.per.mm, Average.Diameter.mm, Surface.Area.mm2, SRL, SRA, RTD, Bi))
radirs <- subset(b, select = -c(Lt, Hmax, RD, LA, LDMC,SLA))
radirss30 <- radirs %>% filter(!row_number() %in% c(30))
puya <- as.factor(Datos_por_individuo$Puya)
bsp30 <- b %>% filter(!row_number() %in% c(30))
fuego <- as.factor(Datos_por_individuo$Fuego)
puyas30 <- puya[-30]
rpca1 <- princomp(bsp30, cor = TRUE)


puya <- factor(
  puya,levels = c("b","g","n","s"),
  labels = c("bicolor","goudotiana","nitida","santosii"))
puyas30 <- factor(
  puyas30,levels = c("b","g","n","s"),
  labels = c("bicolor","goudotiana","nitida","santosii"))

summary(rpca1)

rpca2 <- PCA(X = bsp30, scale.unit = TRUE, ncp = 13, graph = TRUE)

get_pca_ind(rpca2)
fviz_pca_biplot(rpca2,
                col.ind = puya,
                addEllipses = TRUE,
                repel = TRUE,
                label = "var")
colnames(radirss30)[3] <- "Surface Area"
rrpca <- PCA(X = radirss30, scale.unit = TRUE,ncp=13, graph = TRUE)
rrpcag<-fviz_pca_biplot(rrpca,
                col.ind = puyas30,
                addEllipses = TRUE,
                repel = TRUE,
                label = "var",
                col.var = "black",
                palette = c("red","blue","orange","forestgreen"),
                labelsize=8
                )
rrpcag1 <- rrpcag + theme_classic() +labs (title = "A)") + theme(text = element_text(face = "italic", size = 20),
                                                                                                  legend.position = "none"
) 
rrpcag1
rspca <- PCA(X = suprs, scale.unit = TRUE, ncp = 13, graph = TRUE)
rspcag<-fviz_pca_biplot(rspca,
                col.ind = puya,
                addEllipses = TRUE,
                repel = TRUE,
                label = "var",
                col.var = "black",
                palette = c("red","blue","orange","forestgreen"),
                legend.title = "Puya",
                labelsize=8)
rspcag1<- rspcag + theme_classic() +labs (title = "B)") + theme(text = element_text(face = "italic", size = 25),
                                                                           legend.text = element_text(face="italic", size = 20),
                                                                           legend.title = element_text(face="italic", size = 20)
)
rspcag1
ggarrange(rrpcag1,rspcag1, ncol = 2)
#### 
p2
c <- subset(b, select = -c(Branching.frequency.per.mm, SurfaceArea))
colnames(c)[7] <- "Average Diameter"
colnames(b)[8] <- "Average Diameter"
b <- b[-30,]

rpca3 <- PCA(X = b, scale.unit = TRUE, ncp = 13, graph = TRUE)
p2 <- puya
p2 <- factor(
  p2,levels = c("b","g","n","s"),
  labels = c("bicolor","goudotiana","nitida","santosii")
)
p2<-p2[-c(30)]
view(p2)
get_pca_ind(rpca3)

PCAPT <- fviz_pca_biplot(rpca3,
                col.ind = puyas30,
                geom.ind = "point",
                addEllipses = TRUE,
                label = "var",
                col.var = "black",
                palette = c("red","blue", "orange","forestgreen"),
                legend.title= "Puya",
                labelsize = 8,
                title = "")
PCAPT + theme_classic() + theme(text = element_text(face = "italic", size = 24), legend.text = element_text(face="italic", size = 20),
                                                                       legend.title = element_text(face="italic", size = 20)
                                
)
 
# Boxplots ----

# Estadisticos ----

shapiro.test(Nitida1988$Lt)
shapiro.test(Nitida1988$Hmax)
t.test(Nitida1988$Lt,NitidaSin$Lt)
t.test(Nitida1988$Hmax,NitidaSin$Hmax)
##
shapiro.test(Nitida1988$RD)
shapiro.test(NitidaSin$RD)
leveneTest(Datos_por_individuo_nitida$RD,Datos_por_individuo_nitida$Fuegos)
t.test(Nitida1988$RD,NitidaSin$RD, paired = TRUE)
##
shapiro.test(Nitida1988$LA)
shapiro.test(NitidaSin$LA)
leveneTest(Datos_por_individuo_nitida$LA,Datos_por_individuo_nitida$Fuegos)
t.test(Nitida1988$LA,NitidaSin$LA)
##
shapiro.test(Nitida1988$LDMC)
t.test(Nitida1988$LDMC,NitidaSin$LDMC)
##
shapiro.test(Nitida1988$SLA)
shapiro.test(NitidaSin$SLA)
leveneTest(Datos_por_individuo_nitida$SLA,Datos_por_individuo_nitida$Fuegos)
t.test(Nitida1988$SLA,NitidaSin$SLA, paired = TRUE)
##
shapiro.test(Nitida1988$Branching.frequency.per.mm)
shapiro.test(NitidaSin$Branching.frequency.per.mm)
leveneTest(Datos_por_individuo_nitida$Branching.frequency.per.mm,Datos_por_individuo_nitida$Fuegos)
t.test(Nitida1988$Branching.frequency.per.mm,NitidaSin$Branching.frequency.per.mm, var.equal = TRUE)
##
shapiro.test(Nitida1988$Average.Diameter.mm)
shapiro.test(NitidaSin$Average.Diameter.mm)
leveneTest(Datos_por_individuo_nitida$Average.Diameter.mm,Datos_por_individuo_nitida$Fuegos)
t.test(Nitida1988$Average.Diameter.mm,NitidaSin$Average.Diameter.mm, var.equal = TRUE)
#
shapiro.test(Nitida1988$Surface.Area.mm2)
shapiro.test(NitidaSin$Surface.Area.mm2)
leveneTest(Datos_por_individuo_nitida$Surface.Area.mm2,Datos_por_individuo_nitida$Fuegos)
t.test(Nitida1988$Surface.Area.mm2,NitidaSin$Surface.Area.mm2, var.equal = TRUE)
##
shapiro.test(Nitida1988$Bi)
shapiro.test(NitidaSin$Bi)
leveneTest(Datos_por_individuo_nitida$Bi,Datos_por_individuo_nitida$Fuegos)
t.test(Nitida1988$Bi,NitidaSin$Bi,var.equal = TRUE)

ggboxplot(Datos_por_individuo_nitida, x = "Fuegos", y = "Average.Diameter.mm")

# Otras graficas ----
abundancia <- data.frame(Fuego=rep(c("1988","2002","No Fire"), each = 4),
                         Especie=rep(c("goudotiana","santosii","nitida","bicolor"), times = 3),
                         Cantidad=c(99,0,48,0,116,0,0,0,91,45,665,6))
abundancia_a <- abundancia
abundancia_a$Fuego <- factor(abundancia_a$Fuego,
                             c("No Fire","1988","2002"))
ggplot(abundancia_a, aes(fill=Especie, y=Cantidad, x=Fuego)) +
  geom_bar(position='dodge', stat='identity') + md_theme_classic() +labs (x= "Fire", y = "Abundance", fill = "*Puya*") +
  scale_fill_discrete(breaks=c("bicolor", "goudotiana", "nitida","santosii"),
                      labels=c("*bicolor*", "*goudotiana*", "*nitida*","*santosii*")) + theme(text = element_text(size =30))
view(abundancia)

# diverisdad
#margalef
amrgaleftodo <- ((4-1)/log(1070)) 
amrgaleftodo
marf <- ((2-1)/log(263))
marf
marsf <- ((4-1)/log(807))
marsf

Chao2 <- (4+((2^2)/(2*2)))
Chao2

Jack1 <- 4+(2*((6-1)/6))
Jack1

uDsf <- sqrt(8^2+45^2+630^2+83^2+35^2+6^2)
uDsf
McintoshDsf <- ((907-uDsf)/(807-sqrt(807)))
McintoshDsf

uDf <- sqrt((31^2+41^2+12^2+85^2+58^2+36^2))
McintoshDf <- ((263-uDf)/(263-sqrt(263)))
McintoshDf


Shannonf <- -((0.817*log(0.817))+(0.183*log(0.183)))
Shannonf

Shannonsf <- -((0.11*log(0.11))+(0.06*log(0.06))+(0.82*log(0.82))+(0.01*log(0.01)))
Shannonsf

# Suelo
shapirosuelo <- data.frame(
  Variable <- character(),
  P_valor <- numeric(),
  stringsAsFactors = FALSE
)
for (col in names(suelo)) {
  if(length(unique(suelo[[col]])) > 1) {
    resultado_test <- shapiro.test(suelo[[col]])
    shapirosuelo<- rbind(shapirosuelo, data.frame(Variable = col, P_valor = resultado_test$p.value))
  } else {
    warning(paste("No se pudo", col, "valores iguales"))
  }
}

bartlett.test(pH ~ Fuego, data = SUELOPCA)

sueloaov <- aov(SUELOPCA$pH ~ Fuego, data = SUELOPCA)
ssaov <- summary(sueloaov)

##########m fit ----
dim(datos)
dim(suelo)
library(vegan)
library(MASS)
library(mgcv)
library(gplots)
library(ggplot2)
rownames(Datos_por_individuo_sin30) <- Datos_por_individuo_sin30$Id
Datos_por_individuo_sin30$Id <- NULL
row.names(Suelooporsitio) <- Suelooporsitio$Id
Suelooporsitio$Id <- NULL

prueba<- vegdist(Datos_por_individuo_sin30, method = "bray")


EF <- envfit(Datos_por_individuo_sin30,Suelooporsitio, perm = 999)
EF

nmds<-metaMDS(prueba, trace=FALSE, k=7)
nmdspar(mar=rep(2,4))
ordiplot(nmds, type = "p")
coords_nmds <-scores(nmds)

plot(EF)
coords_nmds <- scores(nmds)
F2002<- coords_nmds[c(1:5,30:34),]
F1988 <- coords_nmds[c(6:19,35:49),]
FSin <- coords_nmds[c(20:29,50:59),]
points(F2002, col = "red" , pch = 16 , cex = 1)
points(F1988, col = "orange" , pch = 16 , cex = 1)
points(FSin, col = "cyan", pch = 16, cex = 1)
legend(x=1,y=-0.5, legend=c("F2002","F1988","SIN"), 
       col=c("red","orange","cyan"), pch=16, bty="o", cex=0.7)


library(ggordiplots)
pH <- as.data.frame(Suelooporsitio$pH)
colnames(pH) <- "pH"
ord <- cmdscale(prueba)
plt1 <- gg_ordiplot(ord, groups = Fuegossin30$Fuegos)
plt2 <- gg_envfit(ord, Suelooporsitio, plot = FALSE, alpha = 1)
plt2$plot

plt2$plot +
  geom_path(data = plt1$df_ellipse, aes(x=x, y=y, color=Group)) +
  guides(color=guide_legend(title="Fuego"))

####
prueba.hel <- decostand(Datos_por_individuo_sin30, method = "hellinger")
pruebammds <- monoMDS(prueba)
orda <- rda(prueba.hel)

gg_ordiplot(orda, groups = Fuegossin30$Fuegos,choices = c(1,2), kind = "se", conf = 0.95, pt.size = 3)
gg_envfit(pruebammds, Suelooporsitio, perm = 9999, pt.size = 2, alpha = 1)

### con goudotiana

sitiosg <- Datos_goudotiana$Fuegos
sitiosg <- factor(
  sitiosg,levels = c("1988","2002","Sin"),
  labels = c("1988","2002","No Fire")
)
Datos_goudotiana$Fuego <- NULL
Datos_goudotiana$Fuegos <- NULL
Datos_goudotiana$Ind <- NULL
Suelosgoudotiana$Id <- NULL

gSvegdist <- vegdist(GoudotianaS)
gRvegdist <- vegdist(GoudotianaR)
ordgS <- cmdscale(gSvegdist)
ordgR <- cmdscale(gRvegdist)
gvegdist <- vegdist(Datos_goudotiana)
ordg <- cmdscale(gvegdist)


plot1g <- gg_ordiplot(ordg, groups = sitiosg)
plot2g <- gg_envfit(ordg,SuelosgoudotianaSRM, perm = 9999, plot = FALSE, alpha = 0.2, groups = sitiosg)
rrapgenv<- plot2g$plot +
  geom_path(data = plot1g$df_ellipse, aes(x=x, y=y, color= Group)) +
  guides(color=guide_legend(title="Fire")) + md_theme_classic() + labs(title = "B) ",
                                                                        caption = "Arrows significant at **0.2**") +
  theme(text = element_text(size = 20))
#

plot1gS <- gg_ordiplot(ordgS, groups = sitiosg)
plot2gS <- gg_envfit(ordgS,SuelosgoudotianaSRM, perm = 9999, plot = FALSE, alpha = 0.5, groups = sitiosg, )
rapgenv<-plot2gS$plot +
  geom_path(data = plot1gS$df_ellipse, aes(x=x, y=y, color= Group)) +
  guides(color=guide_legend(title="Fuego")) + md_theme_classic() + labs(title = "A)",
                                                                        caption = "Arrows significant at **0.5**") +
  theme(legend.position = "none",
        text = element_text(size = 20))
#
plot1gR <- gg_ordiplot(ordgR, groups = sitiosg)
plot2gR <- gg_envfit(ordgR,SuelosgoudotianaSRM, perm = 9999, plot = FALSE, alpha = 1, groups = sitiosg, label(size = 20))
rrpgenv <- plot2g$plot +
  geom_path(data = plot1g$df_ellipse, aes(x=x, y=y, color= Group)) +
  guides(color=guide_legend(title="Fuego"))  + labs(title = "*P.goudotiana* Radicular traits",
                                                                        caption = "No significance **(1)**") +
  theme(legend.position = "none",
        text = element_text(size = 20))
rrpgenv
plotenvsya <- ggarrange(rapgenv,rrapgenv,ncol = 2, nrow = 1)
plotenvsya
################

rownames(Ind_data_goudotiana)<- Ind_data_goudotiana$Ind
Fuegos <- Ind_data_goudotiana$Fuegos
Ind_data_goudotiana$Ind <- NULL
row.names(Datos_goudotiana) <- Datos_goudotiana$Ind


nmlt <- melt(Datos_por_individuo_nitida)
nmlt[nmlt =="Sin"] <- "No Fire"

ggplot(nmlt, aes(x = Fuegos, y = value), size = value) + geom_boxplot() + 
  facet_wrap(~ variable, scales = "free_y" ) + labs(x = "Fire" , y = "Value") 

colnames(Ind_data_goudotiana)[8] <- "Average Diameter" 
dglt <- melt(Datos_goudotiana)
dgsss <- subset(Ind_data_goudotiana, select = c("Hmax", "LA","LDMC","Lt","RD","SLA","Fuegos"))
dgrrr <- subset(Ind_data_goudotiana, select = c("Average Diameter", "Bi", "SRA", "Branching frequency", "Surface Area","RTD","SRL","Fuegos"))

colnames(dgrrr)[1] <- "Averagediameter" 

dgltss <- melt(dgsss)
dgltrr <- melt(dgrrr)

dglt[dglt == "Sin"] <- "No Fire"
dgltss[dgltss == "Sin"] <- "No Fire"
dgltrr[dgltrr == "Sin"] <- "No Fire"

dgltrr$Fuegos <- factor(dgltrr$Fuegos,
                        c("No Fire","1988","2002"))
dgltss$Fuegos <- factor(dgltss$Fuegos,
                             c("No Fire","1988","2002"))
gsb<-ggplot(dgltss, aes(x = Fuegos, y = value), size = value) + geom_boxplot(aes(color=Fuegos )) + theme_bw() +
  facet_wrap(~ variable, scales = "free_y",
             strip.position = "left",
             labeller = as_labeller(c(Hmax="H(cm)",LA="LA(cm2)",LDMC="LDMC(g)",Lt="Lt(cm)",RD="RD(cm)",SLA="SLA(cm2/g)"))) +
  labs(x = "Fire" , y = "Value") + ylab(NULL) +
  theme(text = element_text(size=29),
        legend.position = "none",
        strip.background = element_blank(),
        strip.placement = "outside")
gsb
grb<-ggplot(dgltrr, aes(x = Fuegos, y = value), size = value) + geom_boxplot(aes(color=Fuegos )) + theme_bw() +
  facet_wrap(~ variable, scales = "free_y",
             strip.position = "left",
             labeller = as_labeller(c(Averagediameter="Average Diameter(mm)",Bi="Bi(root tips/mm)",SRA="SRA(mm2/g)",
                                      Branchingfrquency="Branching frequency(mm)",RTD="RTD(g/mm3)",SRL="SRL(mm/g)",
                                      Surfacearea= "Surface Area (mm2)"))) +
  labs(x = "Fire" , y = "Value") + ylab(NULL) +
  theme(text = element_text(size=29),
        legend.position = "none",
        strip.background = element_blank(),
        strip.placement = "outside")
grb

##########

rownames(Ind_data_nitida) <- Ind_data_nitida$Ind
Ind_data_nitida$Ind <- NULL
Fnitida <- Ind_data_nitida$Fuegos
Ind_data_nitida$Fuegos <- NULL
Ind_data_nitida$Fuego <- NULL
Ind_data_nitida$Puya <-NULL


Fnitida[Fnitida=="Sin"] <- "No fire"
Ind_data_nitida['Fuegos'] <- c(Fnitida)
colnames(Ind_data_nitida)[9] <- 'Surfacearea'

nsn <- Ind_data_nitida[,c("Lt", "Hmax", "RD","LA","LDMC","SLA","Surfacearea","SRL","SRA","RTD","Bi","Fuegos")]
sn <- Datos_por_individuo_nitida[,c("Average diameter", "Branching frequency", "Fuegos")]

colnames(sn)[2] <- "Branchingfrequency"

snm <- melt(sn)

snm$Fuegos <- factor(snm$Fuegos,
                             c("No fire","1988"))
ggplot(snm, aes(x = Fuegos, y = value), size = value) + geom_boxplot(aes(color=Fuegos )) + theme_bw() +
  facet_wrap(~ variable, scales = "free_y",
             strip.position = "left",
             labeller = as_labeller(c(Averagediameter="Average Diameter(mm)",Branchingfrequency="Branching frequency(mm)"))) +
  labs(x = "Fire" , y = "Value") + ylab(NULL) +
  theme(text = element_text(size=29),
        legend.position = "none",
        strip.background = element_blank(),
        strip.placement = "outside")


nsnm<-melt(nsn)
nsnm$Fuegos <- factor(nsnm$Fuegos,
                        c("No fire","1988"))
ggplot(nsnm, aes(x = Fuegos, y = value), size = value) + geom_boxplot(aes(color=Fuegos )) + theme_bw() +
  facet_wrap(~ variable, scales = "free_y",
             strip.position = "left",
             labeller = as_labeller(c(Lt="Lt(cm)",Hmax="H(cm)",RD="RD(cm)",LA="LA(cm)",LDMC="LDMC(g)",
                                      SLA="SLA(cm2/g)",Surfacearea="Surface area (cm2)", SRL = "SRL (mm/g)",
                                      SRA= "SRA(mm2/g)", RTD= "RTD (g/mm3)", Bi = "Bi (root tips/mm)"))) +
  labs(x = "Fire" , y = "Value") + ylab(NULL) +
  theme(text = element_text(size=28),
        legend.position = "none",
        strip.background = element_blank(),
        strip.placement = "outside")
