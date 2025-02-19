##***PREPARACION DE DATOS**
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
pacman::p_load(skimr,writexl,readr,readxl,data.table)
pacman::p_load(tidyverse,tidylog,summarytools,here,plotrix,lubridate, textshape) 
pacman::p_load(bipartite,ggforce, FSA, vegan, mvabund, reshape2, iNEXT,indicspecies, ggrepel) 

###*`Multivariate analysis CHAPTER 4`
#AUCHENORRYNCHA WOODY CROPS----
##WEB----
pacman::p_load(bipartite) 

tax<-read_excel(here("data/resultado_taxonomia_final2.xlsx")) 
tax1<-tax%>% dplyr::filter(año!=2020, !iden %in% c("noiden", "genero")) #delete year=2020 and Unidentifiable

tax2<-tax1%>% dplyr::filter(suborden=="Cicadomorpha")%>% 
  group_by(habitat2, subfamily) %>% #habitat and crops
  summarise(ind=sum(total, na.rm = TRUE)) %>%  
  ungroup() %>%  
  pivot_wider( names_from=subfamily,
               names_sep=".", 
               values_from=ind, 
               values_fn=sum, values_fill=0) %>%  
  ungroup()

tax2<-textshape::column_to_rownames(tax2, loc =1)
tax2 <- as.matrix(tax2)# transform the data into a matrix

ourweb<-tax2# customize the network (play with the different parameters)
# first create vectors specifying the colors for the high trophic level, interactions and lower trophic level
paleta_suave <- colors_cultivos <- c( "lightpink",   "dodgerblue",   "orange",   "olivedrab",    "purple")
high.colors <- c(rep("deeppink4",94))

low.colors <- rep(paleta_suave, times = c(1, 1, 1,1,1))  # Aquí ajustas según la cantidad de cada grupo

# Asignar colores suaves y variados para las interacciones
int.colors <- rep (c (rep("dimgray",500)))

plotweb (ourweb, method="normal", #other is method="cca"
         arrow="center",
         labsize= 0.5,
         text.rot="90", 
         col.low= low.colors,
         col.high= high.colors,         
         col.interaction= int.colors,
         bor.col.low="transparent",  
         bor.col.high="transparent",    
         bor.col.interaction="transparent" )


################################################################################################################

tax3<-tax1%>% dplyr::filter(suborden=="Cicadomorpha")%>% 
  group_by(crop,subfamily) %>% #habitat and crops
  summarise(ind=sum(total, na.rm = TRUE)) %>%  
  ungroup() %>%  
  pivot_wider( names_from=subfamily,
               names_sep=".", 
               values_from=ind, 
               values_fn=sum, values_fill=0) %>%  
  ungroup()

tax3<-textshape::column_to_rownames(tax3, loc =1)
tax3 <- as.matrix(tax3)# transform the data into a matrix

ourweb<-tax3# customize the network (play with the different parameters)
# first create vectors specifying the colors for the high trophic level, interactions and lower trophic level
paleta_suave <- colors_cultivos <- c( "lightpink",   "dodgerblue",   "orange",   "olivedrab",    "purple")
high.colors <- c(rep("deeppink4",94))

low.colors <- rep(paleta_suave, times = c(1, 1, 1,1,1))  # Aquí ajustas según la cantidad de cada grupo

# Asignar colores suaves y variados para las interacciones
int.colors <- rep (c (rep("dimgray",500)))

plotweb (ourweb, method="normal", #other is method="cca"
         arrow="center",
         labsize= 0.5,
         text.rot="90", 
         col.low= low.colors,
         col.high= high.colors,         
         col.interaction= int.colors,
         bor.col.low="transparent",  
         bor.col.high="transparent",    
         bor.col.interaction="transparent" )

#ALFA DIVERSITY AUCHENORRHYNCHA WOODY CROPS----
species<-dcast(data = tax1, formula = fecha+ crop ~ code,
               fun.aggregate = sum, value.var = "total")
sum(rowSums(species[, -c(1, 2,98)]) == 0) # 58 lines=0
species <- species[rowSums(species[, -c(1, 2,98)]) > 0, ] #delete 

sp<-species[,3:97]
metadata_sp <- as.factor(species$crop)

species$crop<-as.factor(species$crop)
sp_c<-species[,2] #metadato


library(FSA) # Para el test de Dunn
library(ggrepel)
diversity_alfa<-sp |> 
  as.data.frame() |> 
  mutate(
    Richness = specnumber(sp),
    Shannon = diversity(sp, index = "shannon"),
    Simpson = diversity(sp, index = "simpson"),
    Pielou = Shannon / log(Richness)  # Cálculo de Pielou's Evenness
  ) |> 
  bind_cols(sp_c) |> 
  rename(crop = ...100)


#Plot diversity

diversity_alfa_long <- diversity_alfa %>%
  pivot_longer(
    cols = c(Richness, Shannon, Simpson, Pielou),
    names_to = "Indice",
    values_to = "Valor"  )
diversity_alfa_long$Indice <- factor(diversity_alfa_long$Indice, 
                                      levels = c("Richness", "Shannon", "Simpson", "Pielou"))
#PLOT DIVERSITY
diversity_alfa_long <- diversity_alfa_long %>% filter(!is.na(Valor))
ggplot(diversity_alfa_long, aes(x = crop, y = Valor, fill = crop)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1, outlier.alpha = 0.8) +
  geom_jitter(width = 0.1, alpha = 0.5, size = 0.5) +
  facet_wrap(~ Indice, scales = "free_y") +
  theme_minimal(base_size = 16) +
  labs(title = "",
       x = "",
       y = expression (alpha~"Diversity indices")) +
  # Eliminar la leyenda
  theme(legend.position = "none",
        # Ajustar el fondo y las líneas de la cuadrícula para un aspecto más limpio
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank(),
        # Alinear mejor las etiquetas del facet
        strip.background = element_rect(fill = "white", color = "white"),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(size = 14, 
                                   color = "black",
                                   face = "bold")) 


#NMDS and PERMANOVA----
library(vegan)
library(mvabund)
#crop and species
library(reshape2)
species<-dcast(data = tax1, formula = fecha+habitat + crop ~ code,
               fun.aggregate = sum, value.var = "total")
sum(rowSums(species[, -c(1, 2,3,99)]) == 0) # 386 lines=0
species <- species[rowSums(species[, -c(1, 2,3,99)]) > 0, ] #delete 386 lines

sp<-species[,4:98]
species$crop<-as.factor(species$crop)
sp_c<-species[,3] 
species$habitat<-as.factor(species$habitat)
sp_h<-species[,2]


#species and crops

sp1 <- metaMDS(comm = sp,
               distance = "bray",
               trace = TRUE, 
               autotransform = FALSE, 
               trymax=199)
#Plot
plot(sp1$points, col = sp_c, pch = 16, 
     xlab = "NMDS1", ylab = "NMDS2", main = "NMDS Plot")
legend("topright", inset = c(-0.4, 0), 
       legend = unique(sp_c), 
       col = unique(sp_c),
       pch = 16, 
       title = "Crops",
       cex = 0.8,
       bty = "n")
       
#PERMANOVA
adonis2(sp ~ sp_c, method = "bray") #significative
#SUPUESTO DE HOMOGENEIDAD
b <- betadisper(vegdist(sp, method = "bray"), group = sp_c)
permutest(b, permutations = 999) #signifivative
boxplot(b)
#No hay ordenacion de comunidad por cultivo

#species and habitat

sp1 <- metaMDS(comm = sp,
               distance = "bray",
               trace = TRUE, 
               autotransform = FALSE, 
               trymax=10)
#Plot
plot(sp1$points, col = sp_h, pch = 16, 
     xlab = "NMDS1", ylab = "NMDS2", main = "NMDS Plot")
legend("topright", inset = c(-0.4, 0), 
       legend = unique(sp_h), 
       col = unique(sp_h),
       pch = 16, 
       title = "Habitat",
       cex = 0.8,
       bty = "n")

#PERMANOVA
adonis2(sp ~ sp_h,
        method = "bray") #Significative
#SUPUESTO DE HOMOGENEIDAD
b <- betadisper(vegdist(sp, method = "bray"), group = sp_h)
permutest(b, permutations = 999) #significative
boxplot(b)
#No hay ordenacion de comunidad por habitats

#La comunidad de Auchenorrhyncha es mi similar en los diferentes cultivos 
#y habitats

#habitat y cultivo  conjunto
species1<-dcast(data = tax1, formula = habitat2+fecha ~ code,
               fun.aggregate = sum, value.var = "total")
sum(rowSums(species1[, -c(1, 2,97)]) == 0) # 758 lines=0
species1 <- species1[rowSums(species1[, -c(1, 2,97)]) > 0, ] #delete 758 lines

sp<-species1[,3:97]
species1$habitat2<-as.factor(species1$habitat2)
sp_h<-species1[,1]
sp2 <- metaMDS(comm = sp,
               distance = "bray",
               trace = TRUE, 
               autotransform = FALSE, 
               trymax=99)
#Plot
plot(sp2$points, col = sp_h, pch = 16, 
     xlab = "NMDS1", ylab = "NMDS2", main = "NMDS Plot")
legend("topright", inset = c(-0.4, 0), 
       legend = unique(sp_h), 
       col = unique(sp_h),
       pch = 16, 
       title = "Habitat",
       cex = 0.8,
       bty = "n")

#PERMANOVA
adonis2(sp ~ sp_h,
        method = "bray") #Significative
#SUPUESTO DE HOMOGENEIDAD
b <- betadisper(vegdist(sp, method = "bray"), group = sp_h)
permutest(b, permutations = 999) #significative
boxplot(b)

# Auchenorrhyncha in Olive----
tax2<-tax1%>% dplyr::filter(crop=="Olive")
#habitat y cultivo  conjunto
species<-dcast(data = tax2, formula = fecha+id_finca~ code,
                fun.aggregate = sum, value.var = "total")
sum(rowSums(species[, -c(1,2, 73)]) == 0) # 16 lines=0
species <- species[rowSums(species[, -c(1, 2,73)]) > 0, ] #delete 16 lines

#Matriz
species<-species[,-98] #elimino columna NA
sp<-species[,3:72] #Community matriz (Abundance)
sp_plot <- as.factor(species$id_finca) #metadata
sp_habitat <- as.factor(species$habitat) #metadata
sp_full<-species[,1:2]
sp_full$habitat <- as.factor(sp_full$habitat) #metadata
sp_full$crop <- as.factor(sp_full$crop) #metadata

#Hay 386 filas con valor cero
#Aplico la transformacion de "hellinger"
sp_trans <- decostand(sp, method = "hellinger")
sp.sq <- sqrt(sp)
#NMDS
sp1<-metaMDS(comm = sp,
             distance = "bray",
             trace = TRUE,k = 2, trymax = 999, autotransform = FALSE)
print(sp1)

nmds_points <- as.data.frame(scores(sp1, display = "sites"))  # Solo coordenadas de plots
nmds_points$plot <- sp_plot  # Agregar nombres de los plots
nmds_points$habitat <- sp_habitat 

# Ajustar especies al NMDS
species_fit <- envfit(sp1, sp.sq, permutations = 999)
species_scores <- as.data.frame(scores(species_fit, "vectors"))
species_scores$species <- rownames(species_scores)  # Asignar nombres de especies


# Filtrar solo especies con alta significancia (opcional)
species_scores$r2 <- species_fit$vectors$r
species_scores <- species_scores[species_fit$vectors$pvals < 0.05, ]

ggplot(data = nmds_points, aes(x = NMDS1, y = NMDS2, color = plot)) +
  geom_point(size = 3, alpha = 0.8) +  # Puntos de los sitios
  geom_mark_ellipse(aes(fill = plot, group = plot), alpha = 0.2) +  # Elipses por cultivo
  geom_segment(data = species_scores, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")), color = "black", inherit.aes = FALSE) +  
  geom_text_repel(data = species_scores, aes(x = NMDS1, y = NMDS2, label = species),
                  color = "black", size = 3, inherit.aes = FALSE) +  
  theme_minimal()


ggplot(nmds_points, aes(x = NMDS1, y = NMDS2, color = class)) +
  geom_point(size = 3, alpha = 0.8) +  # Puntos con tamaño y transparencia
  stat_ellipse(aes(fill = class), alpha = 0.3, geom = "polygon", level = 0.95) +  # Elipses convexas por Plot
  # Etiquetas sin superposición
  theme_minimal() +
  labs(title = "Composición de species por tratamiento",
       x = "NMDS 1",
       y = "NMDS 2",
       color = "Plot") +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(size = 0.2, linetype = "dotted"),
    panel.grid.minor = element_blank()
  ) +
  scale_color_brewer(palette = "Set1") +  # Colores diferenciados para cada plot
  scale_fill_brewer(palette = "Set1", guide = "none")  

bray_dist <- vegdist(sp, method = "bray")
permanova<- adonis2(bray_dist ~ sp_crop,  permutations = 999)
print(permanova)

b <- betadisper(vegdist(sp, method = "bray"), group = sp_crop)
permutest(b, permutations = 999) 

boxplot(b)
