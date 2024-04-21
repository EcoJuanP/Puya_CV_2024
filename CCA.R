######CCA 
library(CCA)
library(dplyr)

x <- Ind_data
xs<-x[,-c(1)]
rownames(xs)<- Ind_data$Ind
Y <- Test
ys <- Y[,-c(1)]
rownames(ys)<- Ind_data$Ind

W <- Testsrm
ws <- W[,-c(1)]
rownames(ws)<- Ind_data$Ind
###
correl <- matcor(xs,ys)
img.matcor(correl, type = 2)

resrege <- estim.regul(xs,ys, plt = TRUE,grid1 = seq(0.1,0.8,l=100),
                       grid2 = seq(0.05,0.4,l=100))

resrcc <- rcc(xs,ys,0.1,0.05)

plt.cc(resrcc, var.label = TRUE)
###
correlsrm <- matcor(xs,ws)
img.matcor(correlsrm, type = 2)

resregsrm <- estim.regul(xs,ws, plt = TRUE,grid1 = seq(0.1,0.8,l=100),
                         grid2 = seq(0.05,0.4,l=100))

resrccsrm <- rcc(xs,ws,0.1,0.4)
plt.cc(resrccsrm, var.label = TRUE)
##
logxs <- log(xs)
logys <- log(ys)
correllog <- matcor(logxs,logys)
img.matcor(correllog, type =2)
resrega <- estim.regul(logxs,logys, plt = TRUE, grid1 = seq(0.1,0.8,l=100),
                       grid2 = seq(0.05,0.4,l=100))
resrega
resrccsrmlog <- rcc(logxs,logys,0.1070707,0.2585859)
plt.cc(resrccsrmlog, var.label = TRUE, ind.names = Ind_data$Ind)
