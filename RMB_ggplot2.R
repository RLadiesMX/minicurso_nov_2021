#!/usr/bin/env Rscript
#  RMB_ggplot2.R
#  Copyright 2021- E. Ernestina Godoy Lozano (tinagodoy@gmail.com)
#                  Joselyn Chavez (joselynchavezf@gmail.com )
#
#  This program is a free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
#  MA 02110-1301, USA.
#  Texto sin acentos

######################################################
#####  Minicurso Red Mexicana de Bioinformatica  #####
#####            Graficando con ggplot2          #####
######################################################

#########################################################
####           Cargar librerias de trabajo           ####
#########################################################

library(ggplot2)
getwd()

#########################################################
####               Ejercicio: qplot()                ####
####             Diapositivas: 15-16                 ####
#########################################################

# Usar datos de un data.frame, "abiotic_variables.txt"
# Leer datos
abiotic <- read.delim("tablas/abiotic_variables.txt", header=T, row.names=1)

# Explorar los datos
View(abiotic)
dim(abiotic)
summary(abiotic)

qplot(x=Naphthalene, y=Deep, data=abiotic, geom="point")

# Añadir lineas y cambiar color de acuerdo a una variable categorica
qplot(x=Naphthalene, y=Deep, data = abiotic, 
      colour = Class, geom=c("point","line"))


#########################################################
####               Ejercicio: ggplot()               ####
####             Diapositivas: 17-37                 ####
#########################################################

## Usaremos un set de datos simulado "antibodies". Datos que provienen de 
## pacientes recuperados de Dengue
# Explorar los datos
antibodies <- read.delim("tablas/antibodies.txt", header=T)
View(antibodies)
summary(antibodies)
dim(antibodies)

# Agregar datos a un objeto con la funcion ggplot ()
p <- ggplot(data=antibodies)
p

## Añadir aesthetics
p <- p + aes(x= CDR3.length, y = VJ.identity, colour = V.GENE)
p

### Otra forma de hacer el codigo anterior en una sola linea
p <- ggplot(data=antibodies, aes(x = CDR3.length, y = VJ.identity, colour = V.GENE))

### Vamos a explorar nuestro objeto "p"
summary(p)

## Agregar los objetos geometricos a una capa (layers)
p <- p + geom_point()
p

# Añadiendo transformaciones estadisticas de los datos. En este caso estamos usando una recta ajustada por cuadrados minimos
p <- p + geom_smooth(method="lm")
p

### Hazlo en una linea
ggplot(data=antibodies, aes(x = CDR3.length, y = VJ.identity, colour = V.GENE)) + 
  geom_point() + 
  geom_smooth(method="lm")

## Facet grid (Cuadricula de facetas)
## Añadir una variable mas a nuestra tabla original
summary(antibodies$V.Nb.mutations)

antibodies$Class <- "Baja"
antibodies$Class[which(antibodies$V.Nb.mutations>=15 & antibodies$V.Nb.mutations<40)] <- "Media"
antibodies$Class[which(antibodies$V.Nb.mutations>=40)] <- "Alta"
antibodies$Class[which(antibodies$V.Nb.mutations==0)] <- "NM"

summary(as.factor(antibodies$Class))

### con una variable
### Vertical
ggplot(data=antibodies, aes(x = CDR3.length, y = VJ.identity, colour = V.GENE)) + 
  geom_point() + 
  geom_smooth(method="lm") +
  facet_grid(~ V.GENE)

### Horizontal
ggplot(data=antibodies, aes(x = CDR3.length, y = VJ.identity, colour = V.GENE)) + 
  geom_point() + 
  geom_smooth(method="lm") +
  facet_grid(V.GENE ~.)

### Quitar leyenda
ggplot(data=antibodies, aes(x=CDR3.length, y=VJ.identity, colour=V.GENE)) + 
  geom_point(show.legend = FALSE) + 
  geom_smooth(method="lm", show.legend = FALSE) +
  facet_grid(V.GENE~.)

### Con dos variables
ggplot(data=antibodies, aes(x=CDR3.length, y=VJ.identity, colour=V.GENE)) + 
  geom_point(show.legend = FALSE) + 
  geom_smooth(method="lm", show.legend = FALSE) +
  facet_grid(V.GENE~Class)

## Cambio de temas
ggplot(data=antibodies, aes(x = CDR3.length, y = VJ.identity, colour = V.GENE)) + 
  geom_point(show.legend = FALSE) + 
  geom_smooth(method="lm", show.legend = FALSE) +
  facet_grid(V.GENE~Class) +
  theme_bw()

ggplot(data=antibodies, aes(x = CDR3.length, y = VJ.identity, colour = V.GENE)) + 
  geom_point(show.legend = FALSE) + 
  geom_smooth(method="lm", show.legend = FALSE) +
  facet_grid(V.GENE~Class) +
  theme_linedraw()

#  Cambiar otros elementos en el tema
# Elementos de la cuadrilla
ggplot(data=antibodies, aes(x = CDR3.length, y = VJ.identity, colour = V.GENE)) + 
  geom_point(show.legend = FALSE) + 
  geom_smooth(method="lm", show.legend = FALSE) +
  facet_grid(V.GENE~Class) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "black"), 
        panel.grid.minor = element_line(linetype = "dotted"))


### ejemplo de Facet wrap
#### Cargar libreria extra
library(reshape2)
cdrs <- antibodies[,1:4]
View(cdrs)
cdrs_melt <- melt(cdrs)

# Explorar los datos ¿Que hace la funcion melt?
View(cdrs_melt)
dim(cdrs_melt)
summary(cdrs_melt)

# boxplot con ajuste de faceta (facet wrap)
ggplot(data= cdrs_melt, aes(x= V.GENE, y= value)) +
  geom_boxplot() +
  facet_wrap(~variable)

# Cambiar la apariencia
# Cambiar colores con paletas predeterminadas
ggplot(data= cdrs_melt, aes(x= V.GENE, y= value, fill= V.GENE)) +
  geom_boxplot() +
  facet_wrap(~variable) +
  scale_fill_brewer(palette = "Set1")

ggplot(data= cdrs_melt, aes(x= V.GENE, y= value, fill= V.GENE)) +
  geom_boxplot() +
  facet_wrap(~variable) +
  scale_fill_brewer(palette = "Reds")

ggplot(data= cdrs_melt, aes(x= V.GENE, y= value, fill= V.GENE)) +
  geom_boxplot() +
  facet_wrap(~variable) +
  scale_fill_brewer(palette = "PiYG")

ggplot(data= cdrs_melt, aes(x= V.GENE, y= value, fill= V.GENE)) +
  geom_boxplot() +
  facet_wrap(~variable) +
  scale_fill_brewer(palette = "PiYG",direction = -1)

ggplot(data= cdrs_melt, aes(x= V.GENE, y= value, fill= V.GENE)) +
  geom_boxplot() +
  facet_wrap(~variable) +
  scale_fill_brewer(palette = 3,direction = -1)


RColorBrewer::brewer.pal(5,name = "Reds")
View(RColorBrewer::brewer.pal.info) 

# Cambiar colores de manera manual
ggplot(data= cdrs_melt, aes(x= V.GENE, y= value, fill= V.GENE)) +
  geom_boxplot() +
  facet_wrap(~variable) +
  scale_fill_manual(values=c("deepskyblue", "seagreen2", "salmon"))

# Cambiar colores usando la escala viridis
library(viridis)

ggplot(data= cdrs_melt, aes(x= V.GENE, y= value, fill= V.GENE)) +
  geom_boxplot() +
  facet_wrap(~variable) +
  scale_fill_viridis(discrete = TRUE)

# Cambiar la etiqueta de los ejes y añadir un tema predeterminado
ggplot(data= cdrs_melt, aes(x= V.GENE, y= value, fill= V.GENE)) +
  geom_boxplot() +
  facet_wrap(~variable) +
  scale_fill_manual(values=c("deepskyblue", "seagreen2", "salmon")) +
  labs(title="CDRs Length", y="length (nt)", x="", fill="") +
  theme_linedraw()

ggplot(data= cdrs_melt, aes(x= V.GENE, y= value, fill= V.GENE)) +
  geom_boxplot() +
  facet_wrap(~variable) +
  scale_fill_manual(values=c("deepskyblue", "seagreen2", "salmon")) +
  labs(title="CDRs Length", y="length (nt)", x="", fill="") +
  theme_linedraw(base_size = 16)

# Cambiar la posicion de la leyenda
ggplot(data= cdrs_melt, aes(x= V.GENE, y= value, fill= V.GENE)) +
  geom_boxplot() +
  facet_wrap(~variable) +
  scale_fill_manual(values=c("deepskyblue", "seagreen2", "salmon")) +
  labs(title="CDRs Length", y="length (nt)", x="", fill="") +
  theme_linedraw() + 
  theme(legend.position = "bottom")

# Cambiar la posicion de los ejes
ggplot(data= cdrs_melt, aes(x= V.GENE, y= value, fill= V.GENE)) +
  geom_boxplot() +
  facet_wrap(~variable) +
  scale_fill_manual(values=c("deepskyblue", "seagreen2", "salmon")) +
  labs(title="CDRs Length", y="length (nt)", x="", fill="") +
  theme_linedraw() + 
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))

#### Guardar imagenes
# En que directorio me encuentro
getwd()
dir()

# Primera opcion
### PNG con una resolucion de 300 px
png("figures/CDRs_antibodies_data.png", width=10 *300, height=8*300, res= 300)
ggplot(data=antibodies, aes(x = CDR3.length, y = VJ.identity, colour = V.GENE)) + 
  geom_point(show.legend = FALSE) + 
  facet_grid(V.GENE~.) +
  theme_linedraw()
dev.off()

### PDF (Imagen vectorizada)
pdf("figures/CDRs_antibodies_data.pdf", width=10, height=8)
ggplot(data=antibodies, aes(x = CDR3.length, y = VJ.identity, colour = V.GENE)) + 
  geom_point(show.legend = FALSE) + 
  facet_grid(V.GENE~.) +
  theme_linedraw()
dev.off()

# Segunda opcion con una funcion propia de ggplot2
ggsave("figures/CDRs_antibodies_data.jpg", device= "jpg")

# Segunda opcion con una funcion propia de ggplot2, usando la opción de escala
ggsave("figures/CDRs_antibodies_data_scale.jpg", device= "jpg", scale = 2)

#### Ejercicio de practica

# Usaremos el set de datos abiotic que cargamos al inicio de esta sesion
abiotic <- read.delim("tablas/abiotic_variables.txt", header=T, row.names=1)
View(abiotic)

# 1) Genera un grafico de puntos que relacione la profundidad de muestreo con 
# los niveles de Fluoreno 

# 2) Colorea los puntos de acuerdo con la Clase del agua de muestreo

# 3) Modifica el título de los ejes para que aparezcan en espanol y agrega un
# titulo al grafico 

# 4) Agrega un tema al grafico que modifique las propiedades del panel de fondo, 
# puedes elegir un tema predeterminado o crear uno nuevo

# 5) Cambia la escala de color, puedes usar las escalas brewer, viridis o una
# lista de colores definidos manualmente

# 6) Separa el grafico en tres paneles o rengloes, usando facetas de acuerdo con 
# la clase, y elimina la aparición de la leyenda.

# 7) Guarda el grafico en un archivo dentro del directorio de trabajo
