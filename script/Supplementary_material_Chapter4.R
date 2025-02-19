##***PREPARACION DE DATOS**
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
pacman::p_load(skimr,writexl,readr,readxl,data.table)
pacman::p_load(tidyverse,tidylog,summarytools,here,plotrix,lubridate) 
               

###*`SUPPLEMENTARY MATERIAL CHAPTER 4`
#AUCHENORRYNCHA WOODY CROPS----

tax<-read_excel(here("data/resultado_taxonomia_final2.xlsx")) 
tax1<-tax%>% dplyr::filter(año!=2020, !iden %in% c("noiden", "genero")) #delete year=2020

###*`S4.2.1 Rarefaction curves`*
species<-dcast(data = tax1, formula = crop ~ code,
               fun.aggregate = sum, value.var = "total")
species<-species[,1:96]
num_muestreos <- data.frame(
  crop = c("Almond", "Olive", "Vineyard", "Citrus", "Blueberry"),
  Num_sample = c(204, 794, 302, 210, 284))
species <- left_join(species, num_muestreos, by = "crop")
abundancia_matriz <- as.matrix(species[, -c(1, ncol(species))])
rownames(abundancia_matriz) <- species$crop
mincrop<-204
abundancia_normalizada <- abundancia_matriz / species$Num_sample
abundancia_estandarizada <- abundancia_normalizada * mincrop
abundancia_estandarizada <- round(abundancia_estandarizada)
# Transponer la matriz para que las especies sean filas y cultivos columnas (requisito para iNEXT)
abundancia_estandarizada <- t(abundancia_estandarizada)

library(iNEXT)
# Aplicar rarefacción con la matriz estandarizada utilizando iNEXT
rare_data <- iNEXT(as.data.frame(abundancia_estandarizada), 
                   q = 0, datatype = "abundance")

# Graficar las curvas de rarefacción con intervalos de confianza
ggiNEXT(rare_data, type = 1) + #type=1 rarefaccion, type=3 rarefaccion y extrapolacion
  labs(title = "Rarefaction Curves",
       x = "Standardized sample size",
       y = "Richness") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1", name = "Crop") +  # Define el nombre de la leyenda
  scale_fill_brewer(palette = "Set1", guide = "none") +  # Oculta la leyenda de relleno
  guides(shape = "none") +  # Oculta los símbolos de la leyenda
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(size = 0.1),  # Ajustar el grosor de la cuadrícula
    panel.grid.minor = element_blank()
  ) +
  geom_line(linewidth = 0.2)

###*`Table S4-2.1. Results of pairwise comparisons between crops for the alpha diversity indices`


###*`Tables S4.2.4 (Cicadomorpha) and S4.2.5 (Fulgoromorpha)`*
###*`SUPPLEMENTARY MATERIAL AUCHENORRHYNCHA ABUNDANCE BY CROPS`* 

cica<-tax1  %>%filter(suborden=='Cicadomorpha') %>%  
  group_by(family,subfamily,habitat,cultivo,code,species)%>%
  summarise(ind=sum(total, na.rm = TRUE)) %>%  
  pivot_wider( names_from=c(cultivo,habitat),
               values_from=ind, 
               values_fill=0,
               names_sort = TRUE) %>%
  ungroup() |> 
  arrange(family, subfamily, species)

fulgo<-tax1  %>%filter(suborden=='Fulgoromorpha') %>%  group_by(family,subfamily,habitat,cultivo,code,species)%>%
  summarise(ind=sum(total, na.rm = TRUE)) %>%  
  pivot_wider( names_from=c(cultivo,habitat),
               values_from=ind, values_fill=0,names_sort = TRUE) %>% 
  ungroup()|> 
  arrange(family, subfamily, species)

library(officer)

doc <- read_docx() %>%
  body_add_par("", style = "heading 1") %>%  
  body_add_table(cica, style = "table_template")  # table cica
doc <- read_docx() %>%
  body_add_par("", style = "heading 1") %>%  
  body_add_table(fulgo, style = "table_template")  # table fulgo

print(doc, target = "cica_table.docx") #save word
print(doc, target = "fulgo_table.docx") #save word
