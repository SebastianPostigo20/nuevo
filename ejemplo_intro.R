# Paquetes
library(pacman)
p_load(scatterplot3d,rgl,car,FactoMineR,
       factoextra,ggplot2,magick)
datos<-read.csv("Ejemplo_Multivariado.csv",
                sep = ";",
                stringsAsFactors = T)
str(datos)
attach(datos)
row.names(datos) <- datos$Empresa
datos$Empresa <- NULL
###############################################################################
## ANALISIS DESCRIPTIVO UNIVARIADO 
summary(Inversion)
ggplot(datos,aes(x=Inversion,y=0)) + 
  geom_point() +
  geom_text(aes(label=row.names(datos)),vjust=-1) +
  theme_light()
###############################################################################
## ANALISIS DESCRIPTIVO BIVARIADO 
summary(Inversion)
summary(Ventas)
ggplot(datos,aes(x=Inversion,y=Ventas)) + 
  geom_point() +
  geom_text(aes(label=row.names(datos)),vjust=-1) +
  geom_vline(xintercept=mean(Inversion),lty=5)+
  geom_hline(yintercept=mean(Ventas),lty=5)+
  theme_bw()
##############################################################################
### ANALISIS DESCRIPTIVO CON 3 DIMENSIONES
summary(datos)
library(scatterplot3d)
with(datos, {
  s3d <- scatterplot3d(Ventas, Antig, Inversion, 
                       color="blue", pch=19,     
                       type="p",                  
                       xlab="Ventas",
                       ylab="Antig",
                       zlab="Inversion")
  s3d.coords <- s3d$xyz.convert(Ventas, Antig, Inversion)
  text(s3d.coords$x, s3d.coords$y,    
       labels=row.names(datos),           
       cex=0.7, pos=4)          
})
library(plot3D)
with(datos, text3D(Ventas,Antig,Inversion, 
                   labels = rownames(datos),
                   col = gg.col(100), 
                   theta = 60, phi = 20,
                   xlab = "Ventas",
                   ylab ="Antigüedad", 
                   zlab = "Inversión", 
                   cex = 0.8, 
                   bty = "g", ticktype = "detailed", 
                   adj = 0.5, font = 2))
with(datos, scatter3D(Ventas,Antig,Inversion, 
                      col = gg.col(100), 
                      type = "h", pch = ".", add = TRUE))


###############################################################################
########## ANALISIS MULTIVARIADO DE COMPONENTES PRINCIPALES ###################
###############################################################################
library(FactoMineR)
res.pca <- PCA(datos, ncp=2,graph=FALSE)
library(factoextra)
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "steelblue",
                col.ind = "black" )
