ggplot(data= cdrs_melt, aes(x= V.GENE, y= value)) +
  geom_boxplot() +
  facet_wrap(~variable)

ggplot(data= cdrs_melt, aes(x= V.GENE, y= value, fill = V.GENE)) +
  geom_boxplot() +
  facet_wrap(~variable)

#### Cambiando los colores
ggplot(data= cdrs_melt, aes(x= V.GENE, y= value, fill = V.GENE)) +
  geom_boxplot() +
  facet_wrap(~variable) +
  scale_fill_brewer(palette = 4)


# Cambiar colores con paletas predeterminadas
ggplot(data= cdrs_melt, aes(x= V.GENE, y= value, fill = V.GENE)) +
  geom_boxplot() +
  facet_wrap(~variable) +
  scale_fill_brewer(palette = "Set1")


# Cambiar colores de manera manual usando nombres
ggplot(data= cdrs_melt, aes(x= V.GENE, y= value, fill = V.GENE)) +
  geom_boxplot() +
  facet_wrap(~variable) +
  scale_fill_manual(values = c("red","blue","yellow"))

ggplot(data= cdrs_melt, aes(x= V.GENE, y= value, fill = V.GENE)) +
  geom_boxplot() +
  facet_wrap(~variable) +
  scale_fill_manual(values = c("deepskyblue","seagreen2","salmon"))


# Cambiar colores de manera manual con codigo hexadecimal
ggplot(data= cdrs_melt, aes(x= V.GENE, y= value, fill = V.GENE)) +
  geom_boxplot() +
  facet_wrap(~variable) +
  scale_fill_manual(values = c("#03fc5e","#03b1fc","#0384fc"))


# Cambiar colores usando la escala viridis
library(viridis)

ggplot(data= cdrs_melt, aes(x= V.GENE, y= value, fill = V.GENE)) +
  geom_boxplot() +
  facet_wrap(~variable) +
  scale_fill_viridis(discrete = TRUE)


#### Cambiando los ejes

# Cambiar la etiqueta de los ejes y añadir un tema predeterminado

ggplot(data= cdrs_melt, aes(x= V.GENE, y= value, fill = V.GENE)) +
  geom_boxplot() +
  facet_wrap(~variable) +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "CDRs Length", x = "",y = "Length (nt)", fill= "")

# Cambiar la posicion de la leyenda
ggplot(data= cdrs_melt, aes(x= V.GENE, y= value, fill = V.GENE)) +
  geom_boxplot() +
  facet_wrap(~variable) +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "CDRs Length", x = "",y = "Length (nt)", fill= "") +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        axis.title.y = element_text(size = 18))


# Cambiar la posicion de los ejes
ggplot(data= cdrs_melt, aes(x= V.GENE, y= value, fill = V.GENE)) +
  geom_boxplot() +
  facet_wrap(~variable) +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "CDRs Length", x = "",y = "Length (nt)", fill= "") +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(angle = 45,vjust = 0.5, hjust = 0.5))


#### Guardar imagenes
# En que directorio me encuentro
getwd()

# Primera opcion
### PNG con una resolucion de 300 px
png("figures/CDRs_antibodies_data.png",width = 10*300, height = 8*300,res = 300)
ggplot(data= cdrs_melt, aes(x= V.GENE, y= value, fill = V.GENE)) +
  geom_boxplot() +
  facet_wrap(~variable) +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "CDRs Length", x = "",y = "Length (nt)", fill= "") +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(angle = 45,vjust = 0.5, hjust = 0.5))
dev.off()

### PDF (Imagen vectorizada)
pdf("figures/CDRs_antibodies_data.pdf",width = 10,height = 8)
ggplot(data= cdrs_melt, aes(x= V.GENE, y= value, fill = V.GENE)) +
  geom_boxplot() +
  facet_wrap(~variable) +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "CDRs Length", x = "",y = "Length (nt)", fill= "") +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(angle = 45,vjust = 0.5, hjust = 0.5))
dev.off()


# Segunda opcion con una funcion propia de ggplot2
ggplot(data= cdrs_melt, aes(x= V.GENE, y= value, fill = V.GENE)) +
  geom_boxplot() +
  facet_wrap(~variable) +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "CDRs Length", x = "",y = "Length (nt)", fill= "") +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(angle = 45,vjust = 0.5, hjust = 0.5))

ggsave("figures/CDRs_antibodies_data.jpg",device = "jpg",
       width = 10,height = 8,units = "cm")

# Segunda opcion con una funcion propia de ggplot2, usando la opción de escala
ggsave("figures/CDRs_antibodies_data.jpg",device = "jpg",scale = 2)

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

# 6) Separa el grafico en tres paneles o renglones, usando facetas de acuerdo con 
# la clase, y elimina la aparición de la leyenda.

# 7) Guarda el grafico en un archivo dentro del directorio de trabajo

