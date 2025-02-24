


data1<-read_excel(here("data/ninfas_completa_por_spp_sep_2024.xlsx"))
data2<-data1 %>% 
  pivot_wider( names_from=vect_1, names_sep=".", 
               values_from=ind, values_fn=sum, values_fill=0) %>%  
  rename(sumNC="neophilaenus campestris", sumLC="lepyronia coleoptrata", 
         sumPH="philaenus spumarius", sumAC="aphrophora corticea")
#Fenologia
d_aphro<-data2 %>% 
  group_by( mes)%>%
  summarise(ncm=mean(sumNC, na.rm = TRUE),
            nce=std.error(sumNC, na.rm = TRUE),
            lcm=mean(sumLC, na.rm = TRUE),
            lce=std.error(sumLC, na.rm = TRUE),
            phm=mean(sumPH, na.rm = TRUE),
            phe=std.error(sumPH, na.rm = TRUE),
            acm=mean(sumAC, na.rm = TRUE),
            ace=std.error(sumAC, na.rm = TRUE),
            atotalm=mean ((sumNC+sumLC+sumPH+sumAC), na.rm = TRUE),
            atotale=std.error ((sumNC+sumLC+sumPH+sumAC), na.rm = TRUE)) 

# Convertir los datos al formato largo, vinculando los valores con sus errores correspondientes
d_aphro_long <- d_aphro %>%
  pivot_longer(cols = c(ncm, lcm, phm, atotalm), names_to = "variable", values_to = "value") %>%
  mutate(error_value = case_when(
    variable == "ncm" ~ nce,
    variable == "lcm" ~ lce,
    variable == "phm" ~ phe,
    variable == "atotalm" ~ atotale
  )) %>% rename(Species="variable")
d_aphro_long$Species <- factor(recode(d_aphro_long$Species,
                                      ncm = "Neophilaenus campestris",
                                      lcm = "Lepyronia coleoptrata",
                                      phm = "Philaenus spumarius",
                                      atotalm = "Aphrophoridae total"), 
                               levels = c("Aphrophoridae total", "Neophilaenus campestris",  "Lepyronia coleoptrata", "Philaenus spumarius"))

# Crear el gráfico de barras con los errores estándar correctamente vinculados
fig1 <- ggplot(d_aphro_long, aes(x = as.factor(mes), y = value, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = pmax(value - error_value, 0), ymax = value + error_value),  # Asegurarse que el mínimo no sea menor que cero
                position = position_dodge(width = 0.9), width = 0.2) + # Añadir barras de error
  scale_fill_manual(values = c("Neophilaenus campestris" = "#466983", "Lepyronia coleoptrata" = "#E7C76F", "Philaenus spumarius" = "#93CF7B", "Aphrophoridae total" = "#D3D3D3")) +  # Colores personalizados
  labs(title = "",
       x = "",
       y = "Density (mean ± se) of Aphrophoridae nymphs",
       fill = "Species") +
  scale_x_discrete(labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May")) +
  scale_y_continuous(limits = c(0, NA))+
  theme_minimal(base_size = 15) +  # Tema limpio con texto más grande
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),  # Centrar el título
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 12,color = "black"),   # Tamaño de los textos en el eje x
    axis.text.y = element_text(size = 12,color = "black"),     # Tamaño de los textos en el eje y
    legend.text = element_text(face = "italic",color = "black"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))
fig1

##############################################################################################################################################
###***DATOS DE DENSIDAD MEDIA DE NINFA POR MESES SEPARADAS POR PROVINCIAS**
###***Fig2**facet_wrap(~ provincia)  # Dividir el gráfico por cada provincia
d_aphro<-abundancia
##############################################################################################################################################
mapeo<-  c("ca01" = "cadiz", "ca02" = "cadiz", "ca03" ="cadiz", "co01" = "cordoba", "co02" ="cordoba", "co03" = "cordoba",
           "hu01" = "huelva", "hu02" = "huelva", "hu03" ="huelva", "hu04" = "huelva", "se01" = "sevilla", "se02" ="sevilla")
ninfas_completa2$provincia <- mapeo[ninfas_completa2$id_finca]
d_aphro<-ninfas_completa2 %>% 
  group_by( provincia, mes)%>%
  summarise(ncm=mean(sumNC, na.rm = TRUE),
            nce=std.error(sumNC, na.rm = TRUE),
            lcm=mean(sumLC, na.rm = TRUE),
            lce=std.error(sumLC, na.rm = TRUE),
            phm=mean(sumPH, na.rm = TRUE),
            phe=std.error(sumPH, na.rm = TRUE),
            acm=mean(sumAC, na.rm = TRUE),
            ace=std.error(sumAC, na.rm = TRUE),
            atotalm=mean ((sumNC+sumLC+sumPH+sumAC), na.rm = TRUE),
            atotale=std.error ((sumNC+sumLC+sumPH+sumAC), na.rm = TRUE)) 

# Convertir los datos al formato largo, vinculando los valores con sus errores correspondientes
d_aphro_long <- d_aphro %>%
  pivot_longer(cols = c(ncm, lcm, phm, atotalm), names_to = "variable", values_to = "value") %>%
  mutate(error_value = case_when(
    variable == "ncm" ~ nce,
    variable == "lcm" ~ lce,
    variable == "phm" ~ phe,
    variable == "atotalm" ~ atotale
  )) %>% rename(Species="variable")
d_aphro_long$Species <- factor(recode(d_aphro_long$Species,
                                      ncm = "Neophilaenus campestris",
                                      lcm = "Lepyronia coleoptrata",
                                      phm = "Philaenus spumarius",
                                      atotalm = "Aphrophoridae total"), 
                               levels = c("Aphrophoridae total", "Neophilaenus campestris",  "Lepyronia coleoptrata", "Philaenus spumarius"))

y_limit <- 12  ## Definir un límite común para el eje Y
# Crear el gráfico de barras con los errores estándar correctamente vinculados
A <-d_aphro_long %>%  
  filter(provincia=="cadiz") %>%
  ggplot( aes(x = as.factor(mes), y = value, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = pmax(value - error_value, 0), ymax = value + error_value),  # Asegurarse que el mínimo no sea menor que cero
                position = position_dodge(width = 0.9), width = 0.2) + # Añadir barras de error
  scale_fill_manual(values = c("Neophilaenus campestris" = "#466983", "Lepyronia coleoptrata" = "#E7C76F", "Philaenus spumarius" = "#93CF7B", "Aphrophoridae total" = "#D3D3D3")) +  # Colores personalizados
  labs(title = "Cádiz",
       x = "",
       y = "",
       fill = "Species") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5"), 
                   labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May")) +
  scale_y_continuous(limits = c(0, y_limit)) +
  theme_minimal(base_size = 15) +  # Tema limpio con texto más grande
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),  # Centrar el título
    axis.title.x = element_text(),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 12,color = "black"),   # Tamaño de los textos en el eje x
    axis.text.y = element_text(size = 12,color = "black"),     # Tamaño de los textos en el eje y
    legend.text = element_text(face = "italic",color = "black"),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))
A
B <-d_aphro_long %>%  
  filter(provincia=="cordoba") %>%
  ggplot( aes(x = as.factor(mes), y = value, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = pmax(value - error_value, 0), ymax = value + error_value),  # Asegurarse que el mínimo no sea menor que cero
                position = position_dodge(width = 0.9), width = 0.2) + # Añadir barras de error
  scale_fill_manual(values = c("Neophilaenus campestris" = "#466983", "Lepyronia coleoptrata" = "#E7C76F", "Philaenus spumarius" = "#93CF7B", "Aphrophoridae total" = "#D3D3D3")) +  # Colores personalizados
  labs(title = "Córdoba",
       x = "",
       y = "",
       fill = "Species") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5"), 
                   labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May")) +
  scale_y_continuous(limits = c(0, y_limit)) +
  theme_minimal(base_size = 15) +  # Tema limpio con texto más grande
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),  # Centrar el título
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 12,color = "black"),   # Tamaño de los textos en el eje x
    axis.text.y = element_text(size = 12,color = "black"),     # Tamaño de los textos en el eje y
    legend.text = element_text(face = "italic",color = "black"),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))
B
C <-d_aphro_long %>%  
  filter(provincia=="huelva") %>%
  ggplot( aes(x = as.factor(mes), y = value, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = pmax(value - error_value, 0), ymax = value + error_value),  # Asegurarse que el mínimo no sea menor que cero
                position = position_dodge(width = 0.9), width = 0.2) + # Añadir barras de error
  scale_fill_manual(values = c("Neophilaenus campestris" = "#466983", "Lepyronia coleoptrata" = "#E7C76F", "Philaenus spumarius" = "#93CF7B", "Aphrophoridae total" = "#D3D3D3")) +  # Colores personalizados
  labs(title = "Huelva",
       x = "",
       y = "",
       fill = "Species") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5"), 
                   labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May")) +
  scale_y_continuous(limits = c(0, y_limit)) +
  theme_minimal(base_size = 15) +  # Tema limpio con texto más grande
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),  # Centrar el título
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 12,color = "black"),   # Tamaño de los textos en el eje x
    axis.text.y = element_text(size = 12,color = "black"),     # Tamaño de los textos en el eje y
    legend.text = element_text(face = "italic",color = "black"),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))
C
D <-d_aphro_long %>%  
  filter(provincia=="sevilla") %>%
  ggplot( aes(x = as.factor(mes), y = value, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = pmax(value - error_value, 0), ymax = value + error_value),  # Asegurarse que el mínimo no sea menor que cero
                position = position_dodge(width = 0.9), width = 0.2) + # Añadir barras de error
  scale_fill_manual(values = c("Neophilaenus campestris" = "#466983", "Lepyronia coleoptrata" = "#E7C76F", "Philaenus spumarius" = "#93CF7B", "Aphrophoridae total" = "#D3D3D3")) +  # Colores personalizados
  labs(title = "Sevilla",
       x = "",
       y = "",
       fill = "Species") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5"), 
                   labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May")) +
  scale_y_continuous(limits = c(0, y_limit)) +
  theme_minimal(base_size = 15) +  # Tema limpio con texto más grande
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),  # Centrar el título
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 12,color = "black"),   # Tamaño de los textos en el eje x
    axis.text.y = element_text(size = 12,color = "black"),     # Tamaño de los textos en el eje y
    legend.text = element_text(face = "italic",color = "black"),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))
D

# Elimina las leyendas individuales de cada gráfico y los títulos de los ejes innecesarios
A <- A + theme(legend.position = "none")
B <- B + theme(legend.position = "none")
C <- C + theme(legend.position = "none")
D <- D + theme(legend.position = "none")
# Combina los gráficos con patchwork
Fig2 <- (A | B) / (C | D) +
  plot_annotation(title="Density (mean ± se) of Aphrophoridae nymphs", tag_levels = "A")+
  plot_layout(guides = "collect") &  # Combina las leyendas en una única
  theme(legend.position = "bottom") 
Fig2
fig1 <- fig1 + theme(legend.position = "none")
(fig1+Fig2)+plot_layout(heights = c(0.5, 2.5))
##############################################################################################################################################
##############################################################################################################################################
###***Fig2a**
##############################################################################################################################################

d_aphro<-abundancia %>% 
  group_by( provincia, mes)%>%
  summarise(ncm=mean(sumNC, na.rm = TRUE),
            nce=std.error(sumNC, na.rm = TRUE),
            lcm=mean(sumLC, na.rm = TRUE),
            lce=std.error(sumLC, na.rm = TRUE),
            phm=mean(sumPH, na.rm = TRUE),
            phe=std.error(sumPH, na.rm = TRUE),
            acm=mean(sumAC, na.rm = TRUE),
            ace=std.error(sumAC, na.rm = TRUE))


# Convertir los datos al formato largo, vinculando los valores con sus errores correspondientes
d_aphro_long <- d_aphro %>%
  pivot_longer(cols = c(ncm, lcm, phm), names_to = "variable", values_to = "value") %>%
  mutate(error_value = case_when(
    variable == "ncm" ~ nce,
    variable == "lcm" ~ lce,
    variable == "phm" ~ phe
  )) %>% rename(Species="variable")
d_aphro_long$Species <- factor(recode(d_aphro_long$Species,
                                      ncm = "Neophilaenus campestris",
                                      lcm = "Lepyronia coleoptrata",
                                      phm = "Philaenus spumarius"), 
                               levels = c( "Neophilaenus campestris",  "Lepyronia coleoptrata", "Philaenus spumarius"))

y_limit <- 14  ## Definir un límite común para el eje Y
# Crear el gráfico de barras con los errores estándar correctamente vinculados
A <-d_aphro_long %>%  
  filter(provincia=="cadiz") %>%
  ggplot( aes(x = as.factor(mes), y = value, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = pmax(value - error_value, 0), ymax = value + error_value),  # Asegurarse que el mínimo no sea menor que cero
                position = position_dodge(width = 0.9), width = 0.2) + # Añadir barras de error
  scale_fill_manual(values = c("Neophilaenus campestris" = "#466983", "Lepyronia coleoptrata" = "#E7C76F", "Philaenus spumarius" = "#93CF7B")) +  # Colores personalizados
  labs(title = "Cádiz",
       x = "",
       y = "",
       fill = "Species") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5"), 
                   labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May")) +
  scale_y_continuous(limits = c(0, y_limit)) +
  theme_minimal(base_size = 15) +  # Tema limpio con texto más grande
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),  # Centrar el título
    axis.title.x = element_text(),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 12,color = "black"),   # Tamaño de los textos en el eje x
    axis.text.y = element_text(size = 12,color = "black"),     # Tamaño de los textos en el eje y
    legend.text = element_text(face = "italic",color = "black"),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))
A
B <-d_aphro_long %>%  
  filter(provincia=="cordoba") %>%
  ggplot( aes(x = as.factor(mes), y = value, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = pmax(value - error_value, 0), ymax = value + error_value),  # Asegurarse que el mínimo no sea menor que cero
                position = position_dodge(width = 0.9), width = 0.2) + # Añadir barras de error
  scale_fill_manual(values = c("Neophilaenus campestris" = "#466983", "Lepyronia coleoptrata" = "#E7C76F", "Philaenus spumarius" = "#93CF7B")) +  # Colores personalizados
  labs(title = "Córdoba",
       x = "",
       y = "",
       fill = "Species") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5"), 
                   labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May")) +
  scale_y_continuous(limits = c(0, y_limit)) +
  theme_minimal(base_size = 15) +  # Tema limpio con texto más grande
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),  # Centrar el título
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 12,color = "black"),   # Tamaño de los textos en el eje x
    axis.text.y = element_text(size = 12,color = "black"),     # Tamaño de los textos en el eje y
    legend.text = element_text(face = "italic",color = "black"),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))
B
C <-d_aphro_long %>%  
  filter(provincia=="huelva") %>%
  ggplot( aes(x = as.factor(mes), y = value, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = pmax(value - error_value, 0), ymax = value + error_value),  # Asegurarse que el mínimo no sea menor que cero
                position = position_dodge(width = 0.9), width = 0.2) + # Añadir barras de error
  scale_fill_manual(values = c("Neophilaenus campestris" = "#466983", "Lepyronia coleoptrata" = "#E7C76F", "Philaenus spumarius" = "#93CF7B")) +  # Colores personalizados
  labs(title = "Huelva",
       x = "",
       y = "",
       fill = "Species") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5"), 
                   labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May")) +
  scale_y_continuous(limits = c(0, y_limit)) +
  theme_minimal(base_size = 15) +  # Tema limpio con texto más grande
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),  # Centrar el título
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 12,color = "black"),   # Tamaño de los textos en el eje x
    axis.text.y = element_text(size = 12,color = "black"),     # Tamaño de los textos en el eje y
    legend.text = element_text(face = "italic",color = "black"),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))
C
D <-d_aphro_long %>%  
  filter(provincia=="sevilla") %>%
  ggplot( aes(x = as.factor(mes), y = value, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = pmax(value - error_value, 0), ymax = value + error_value),  # Asegurarse que el mínimo no sea menor que cero
                position = position_dodge(width = 0.9), width = 0.2) + # Añadir barras de error
  scale_fill_manual(values = c("Neophilaenus campestris" = "#466983", "Lepyronia coleoptrata" = "#E7C76F", "Philaenus spumarius" = "#93CF7B")) +  # Colores personalizados
  labs(title = "Sevilla",
       x = "",
       y = "",
       fill = "Species") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5"), 
                   labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May")) +
  scale_y_continuous(limits = c(0, y_limit)) +
  theme_minimal(base_size = 15) +  # Tema limpio con texto más grande
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),  # Centrar el título
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 12,color = "black"),   # Tamaño de los textos en el eje x
    axis.text.y = element_text(size = 12,color = "black"),     # Tamaño de los textos en el eje y
    legend.text = element_text(face = "italic",color = "black"),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))
D

# Elimina las leyendas individuales de cada gráfico y los títulos de los ejes innecesarios
A <- A + theme(legend.position = "none")
B <- B + theme(legend.position = "none")
C <- C + theme(legend.position = "none")
D <- D + theme(legend.position = "none")
# Combina los gráficos con patchwork
Fig2a <- (A | B) / (C | D) +
  plot_annotation(title="Density (mean ± se) of Aphrophoridae nymphs",tag_levels = "A")+
  plot_layout(guides = "collect") &  # Combina las leyendas en una única
  theme(legend.position = "bottom") 

Fig2a
##############################################################################################################################################
###***DATOS DE DENSIDAD MEDIA DE NINFA POR MESES SEPARADAS POR CULTIVOS**
###***Fig3
##############################################################################################################################################
mapeo<-  c("ca01" = "alm", "ca02" = "oli", "ca03" ="vid", "co01" = "oli", "co02" ="cit", "co03" = "vid",
           "hu01" = "ara", "hu02" = "ara", "hu03" ="vid", "hu04" = "oli", "se01" = "oli", "se02" ="oli")
ninfas_completa2$cultivo <- mapeo[ninfas_completa2$id_finca]

d_aphro<-ninfas_completa2 %>% 
  group_by( cultivo,mes)%>%
  summarise(ncm=mean(sumNC, na.rm = TRUE),
            nce=std.error(sumNC, na.rm = TRUE),
            lcm=mean(sumLC, na.rm = TRUE),
            lce=std.error(sumLC, na.rm = TRUE),
            phm=mean(sumPH, na.rm = TRUE),
            phe=std.error(sumPH, na.rm = TRUE),
            acm=mean(sumAC, na.rm = TRUE),
            ace=std.error(sumAC, na.rm = TRUE),
            atotalm=mean ((sumNC+sumLC+sumPH+sumAC), na.rm = TRUE),
            atotale=std.error ((sumNC+sumLC+sumPH+sumAC), na.rm = TRUE))

# Convertir los datos al formato largo, vinculando los valores con sus errores correspondientes
d_aphro_long <- d_aphro %>%
  pivot_longer(cols = c(ncm, lcm, phm, atotalm), names_to = "variable", values_to = "value") %>%
  mutate(error_value = case_when(
    variable == "ncm" ~ nce,
    variable == "lcm" ~ lce,
    variable == "phm" ~ phe,
    variable == "atotalm" ~ atotale
  )) %>% rename(Species="variable")
d_aphro_long$Species <- factor(recode(d_aphro_long$Species,
                                      ncm = "Neophilaenus campestris",
                                      lcm = "Lepyronia coleoptrata",
                                      phm = "Philaenus spumarius",
                                      atotalm = "Aphrophoridae total"), 
                               levels = c("Aphrophoridae total", "Neophilaenus campestris",  "Lepyronia coleoptrata", "Philaenus spumarius"))
y_limit <- 4.5  ## Definir un límite común para el eje Y

A <-d_aphro_long %>%  
  filter(cultivo=="alm") %>%
  ggplot( aes(x = as.factor(mes), y = value, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = pmax(value - error_value, 0), ymax = value + error_value),  # Asegurarse que el mínimo no sea menor que cero
                position = position_dodge(width = 0.9), width = 0.2) + # Añadir barras de error
  scale_fill_manual(values = c("Neophilaenus campestris" = "#466983", "Lepyronia coleoptrata" = "#E7C76F", "Philaenus spumarius" = "#93CF7B", "Aphrophoridae total" = "#D3D3D3")) +  # Colores personalizados
  labs(title ="Almond",
       x = "",
       y = "",
       fill = "Species") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5"), 
                   labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May")) +
  scale_y_continuous(limits = c(0, y_limit)) +
  theme_minimal(base_size = 15) +  # Tema limpio con texto más grande
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),  # Centrar el título
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 12,color = "black"),   # Tamaño de los textos en el eje x
    axis.text.y = element_text(size = 12,color = "black"),     # Tamaño de los textos en el eje y
    legend.text = element_text(face = "italic",color = "black"),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))  

A 


B <-d_aphro_long %>%  
  filter(cultivo=="ara") %>%
  ggplot( aes(x = as.factor(mes), y = value, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = pmax(value - error_value, 0), ymax = value + error_value),  # Asegurarse que el mínimo no sea menor que cero
                position = position_dodge(width = 0.9), width = 0.2) + # Añadir barras de error
  scale_fill_manual(values = c("Neophilaenus campestris" = "#466983", "Lepyronia coleoptrata" = "#E7C76F", "Philaenus spumarius" = "#93CF7B", "Aphrophoridae total" = "#D3D3D3")) +  # Colores personalizados
  labs(title ="Blueberry",
       x = "",
       y = "",
       fill = "Species") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5"), 
                   labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May")) +
  scale_y_continuous(limits = c(0, y_limit)) +
  theme_minimal(base_size = 15) +  # Tema limpio con texto más grande
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),  # Centrar el título
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 12,color = "black"),   # Tamaño de los textos en el eje x
    axis.text.y = element_text(size = 12,color = "black"),     # Tamaño de los textos en el eje y
    legend.text = element_text(face = "italic",color = "black"),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))  

B    

C <-d_aphro_long %>%  
  filter(cultivo=="cit") %>%
  ggplot( aes(x = as.factor(mes), y = value, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = pmax(value - error_value, 0), ymax = value + error_value),  # Asegurarse que el mínimo no sea menor que cero
                position = position_dodge(width = 0.9), width = 0.2) + # Añadir barras de error
  scale_fill_manual(values = c("Neophilaenus campestris" = "#466983", "Lepyronia coleoptrata" = "#E7C76F", "Philaenus spumarius" = "#93CF7B", "Aphrophoridae total" = "#D3D3D3")) +  # Colores personalizados
  labs(title ="Citrus",
       x = "",
       y = "",
       fill = "Species") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5"), 
                   labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May")) +
  scale_y_continuous(limits = c(0, y_limit)) +
  theme_minimal(base_size = 15) +  # Tema limpio con texto más grande
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),  # Centrar el título
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 12,color = "black"),   # Tamaño de los textos en el eje x
    axis.text.y = element_text(size = 12,color = "black"),     # Tamaño de los textos en el eje y
    legend.text = element_text(face = "italic",color = "black"),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))  

C   

D <-d_aphro_long %>%  
  filter(cultivo=="oli") %>%
  ggplot( aes(x = as.factor(mes), y = value, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = pmax(value - error_value, 0), ymax = value + error_value),  # Asegurarse que el mínimo no sea menor que cero
                position = position_dodge(width = 0.9), width = 0.2) + # Añadir barras de error
  scale_fill_manual(values = c("Neophilaenus campestris" = "#466983", "Lepyronia coleoptrata" = "#E7C76F", "Philaenus spumarius" = "#93CF7B", "Aphrophoridae total" = "#D3D3D3")) +  # Colores personalizados
  labs(title ="Olive",
       x = "",
       y = "",
       fill = "Species") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5"), 
                   labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May")) +
  scale_y_continuous(limits = c(0, y_limit)) +
  theme_minimal(base_size = 15) +  # Tema limpio con texto más grande
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),  # Centrar el título
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 12,color = "black"),   # Tamaño de los textos en el eje x
    axis.text.y = element_text(size = 12,color = "black"),     # Tamaño de los textos en el eje y
    legend.text = element_text(face = "italic",color = "black"),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))  

D  

E <-d_aphro_long %>%  
  filter(cultivo=="vid") %>%
  ggplot( aes(x = as.factor(mes), y = value, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = pmax(value - error_value, 0), ymax = value + error_value),  # Asegurarse que el mínimo no sea menor que cero
                position = position_dodge(width = 0.9), width = 0.2) + # Añadir barras de error
  scale_fill_manual(values = c("Neophilaenus campestris" = "#466983", "Lepyronia coleoptrata" = "#E7C76F", "Philaenus spumarius" = "#93CF7B", "Aphrophoridae total" = "#D3D3D3")) +  # Colores personalizados
  labs(title ="Grapevine",
       x = "",
       y = "",
       fill = "Species") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5"), 
                   labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May")) +
  scale_y_continuous(limits = c(0,4.5)) +
  theme_minimal(base_size = 15) +  # Tema limpio con texto más grande
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),  # Centrar el título
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 12,color = "black"),   # Tamaño de los textos en el eje x
    axis.text.y = element_text(size = 12,color = "black"),     # Tamaño de los textos en el eje y
    legend.text = element_text(face = "italic",color = "black"),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))  

E  
# Elimina las leyendas individuales de cada gráfico y los títulos de los ejes innecesarios
A <- A + theme(legend.position = "none")
B <- B + theme(legend.position = "none")
C <- C + theme(legend.position = "none")
D <- D + theme(legend.position = "none")
E <- E + theme(legend.position = "none")
# Combina los gráficos con patchwork
Fig3 <- (C/D) |  (A/B/E)+ 
  plot_annotation(tag_levels = "A")+
  plot_layout(guides = "collect") &  # Combina las leyendas en una única
  theme(legend.position = "bottom")   
Fig3
##############################################################################################################################################
##############################################################################################################################################

###***Fig3a**
##############################################################################################################################################
d_aphro<-abundancia %>% 
  group_by( cultivo,mes)%>%
  summarise(ncm=mean(sumNC, na.rm = TRUE),
            nce=std.error(sumNC, na.rm = TRUE),
            lcm=mean(sumLC, na.rm = TRUE),
            lce=std.error(sumLC, na.rm = TRUE),
            phm=mean(sumPH, na.rm = TRUE),
            phe=std.error(sumPH, na.rm = TRUE),
            acm=mean(sumAC, na.rm = TRUE),
            ace=std.error(sumAC, na.rm = TRUE))


# Convertir los datos al formato largo, vinculando los valores con sus errores correspondientes
d_aphro_long <- d_aphro %>%
  pivot_longer(cols = c(ncm, lcm, phm), names_to = "variable", values_to = "value") %>%
  mutate(error_value = case_when(
    variable == "ncm" ~ nce,
    variable == "lcm" ~ lce,
    variable == "phm" ~ phe,
  )) %>% rename(Species="variable")
d_aphro_long$Species <- factor(recode(d_aphro_long$Species,
                                      ncm = "Neophilaenus campestris",
                                      lcm = "Lepyronia coleoptrata",
                                      phm = "Philaenus spumarius"), 
                               levels = c( "Neophilaenus campestris",  "Lepyronia coleoptrata", "Philaenus spumarius"))
y_limit <- 7  ## Definir un límite común para el eje Y

A <-d_aphro_long %>%  
  filter(cultivo=="alm") %>%
  ggplot( aes(x = as.factor(mes), y = value, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = pmax(value - error_value, 0), ymax = value + error_value),  # Asegurarse que el mínimo no sea menor que cero
                position = position_dodge(width = 0.9), width = 0.2) + # Añadir barras de error
  scale_fill_manual(values = c("Neophilaenus campestris" = "#466983", "Lepyronia coleoptrata" = "#E7C76F", "Philaenus spumarius" = "#93CF7B")) +  # Colores personalizados
  labs(title ="Almond",
       x = "",
       y = "",
       fill = "Species") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5"), 
                   labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May")) +
  scale_y_continuous(limits = c(0, y_limit)) +
  theme_minimal(base_size = 15) +  # Tema limpio con texto más grande
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),  # Centrar el título
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 12,color = "black"),   # Tamaño de los textos en el eje x
    axis.text.y = element_text(size = 12,color = "black"),     # Tamaño de los textos en el eje y
    legend.text = element_text(face = "italic",color = "black"),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))  

A 


B <-d_aphro_long %>%  
  filter(cultivo=="ara") %>%
  ggplot( aes(x = as.factor(mes), y = value, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = pmax(value - error_value, 0), ymax = value + error_value),  # Asegurarse que el mínimo no sea menor que cero
                position = position_dodge(width = 0.9), width = 0.2) + # Añadir barras de error
  scale_fill_manual(values = c("Neophilaenus campestris" = "#466983", "Lepyronia coleoptrata" = "#E7C76F", "Philaenus spumarius" = "#93CF7B")) +  # Colores personalizados
  labs(title ="Blueberry",
       x = "",
       y = "",
       fill = "Species") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5"), 
                   labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May")) +
  scale_y_continuous(limits = c(0, y_limit)) +
  theme_minimal(base_size = 15) +  # Tema limpio con texto más grande
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),  # Centrar el título
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 12,color = "black"),   # Tamaño de los textos en el eje x
    axis.text.y = element_text(size = 12,color = "black"),     # Tamaño de los textos en el eje y
    legend.text = element_text(face = "italic",color = "black"),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))  

B    

C <-d_aphro_long %>%  
  filter(cultivo=="cit") %>%
  ggplot( aes(x = as.factor(mes), y = value, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = pmax(value - error_value, 0), ymax = value + error_value),  # Asegurarse que el mínimo no sea menor que cero
                position = position_dodge(width = 0.9), width = 0.2) + # Añadir barras de error
  scale_fill_manual(values = c("Neophilaenus campestris" = "#466983", "Lepyronia coleoptrata" = "#E7C76F", "Philaenus spumarius" = "#93CF7B")) +  # Colores personalizados
  labs(title ="Citrus",
       x = "",
       y = "",
       fill = "Species") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5"), 
                   labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May")) +
  scale_y_continuous(limits = c(0, y_limit)) +
  theme_minimal(base_size = 15) +  # Tema limpio con texto más grande
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),  # Centrar el título
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 12,color = "black"),   # Tamaño de los textos en el eje x
    axis.text.y = element_text(size = 12,color = "black"),     # Tamaño de los textos en el eje y
    legend.text = element_text(face = "italic",color = "black"),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))  

C   

D <-d_aphro_long %>%  
  filter(cultivo=="oli") %>%
  ggplot( aes(x = as.factor(mes), y = value, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = pmax(value - error_value, 0), ymax = value + error_value),  # Asegurarse que el mínimo no sea menor que cero
                position = position_dodge(width = 0.9), width = 0.2) + # Añadir barras de error
  scale_fill_manual(values = c("Neophilaenus campestris" = "#466983", "Lepyronia coleoptrata" = "#E7C76F", "Philaenus spumarius" = "#93CF7B")) +  # Colores personalizados
  labs(title ="Olive",
       x = "",
       y = "",
       fill = "Species") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5"), 
                   labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May")) +
  scale_y_continuous(limits = c(0, y_limit)) +
  theme_minimal(base_size = 15) +  # Tema limpio con texto más grande
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),  # Centrar el título
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 12,color = "black"),   # Tamaño de los textos en el eje x
    axis.text.y = element_text(size = 12,color = "black"),     # Tamaño de los textos en el eje y
    legend.text = element_text(face = "italic",color = "black"),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))  

D  

E <-d_aphro_long %>%  
  filter(cultivo=="vid") %>%
  ggplot( aes(x = as.factor(mes), y = value, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = pmax(value - error_value, 0), ymax = value + error_value),  # Asegurarse que el mínimo no sea menor que cero
                position = position_dodge(width = 0.9), width = 0.2) + # Añadir barras de error
  scale_fill_manual(values = c("Neophilaenus campestris" = "#466983", "Lepyronia coleoptrata" = "#E7C76F", "Philaenus spumarius" = "#93CF7B")) +  # Colores personalizados
  labs(title ="Grapevine",
       x = "",
       y = "",
       fill = "Species") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5"), 
                   labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May")) +
  scale_y_continuous(limits = c(0,14)) +
  theme_minimal(base_size = 15) +  # Tema limpio con texto más grande
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),  # Centrar el título
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 12,color = "black"),   # Tamaño de los textos en el eje x
    axis.text.y = element_text(size = 12,color = "black"),     # Tamaño de los textos en el eje y
    legend.text = element_text(face = "italic",color = "black"),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))  

E  
# Elimina las leyendas individuales de cada gráfico y los títulos de los ejes innecesarios
A <- A + theme(legend.position = "none")
B <- B + theme(legend.position = "none")
C <- C + theme(legend.position = "none")
D <- D + theme(legend.position = "none")
E <- E + theme(legend.position = "none")
# Combina los gráficos con patchwork
Fig3a <- (C/D) |  (A/B/E)+
  plot_annotation( tag_levels = "A")+
  plot_layout(guides = "collect") &  # Combina las leyendas en una única
  theme(legend.position = "bottom")  
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################



d_aphro<-ninfas_completa2 %>% 
  group_by( year,mes)%>%filter(year!= '2020') %>% 
  summarise(ncm=mean(sumNC, na.rm = TRUE),
            nce=std.error(sumNC, na.rm = TRUE),
            lcm=mean(sumLC, na.rm = TRUE),
            lce=std.error(sumLC, na.rm = TRUE),
            phm=mean(sumPH, na.rm = TRUE),
            phe=std.error(sumPH, na.rm = TRUE),
            acm=mean(sumAC, na.rm = TRUE),
            ace=std.error(sumAC, na.rm = TRUE),
            atotalm=mean ((sumNC+sumLC+sumPH+sumAC), na.rm = TRUE),
            atotale=std.error ((sumNC+sumLC+sumPH+sumAC), na.rm = TRUE))


# Convertir los datos al formato largo, vinculando los valores con sus errores correspondientes
d_aphro_long <- d_aphro %>%
  pivot_longer(cols = c(ncm, lcm, phm,atotalm), names_to = "variable", values_to = "value") %>%
  mutate(error_value = case_when(
    variable == "ncm" ~ nce,
    variable == "lcm" ~ lce,
    variable == "phm" ~ phe,
    variable == "atotalm" ~ atotale
  )) %>% rename(Species="variable")
d_aphro_long$Species <- factor(recode(d_aphro_long$Species,
                                      ncm = "Neophilaenus campestris",
                                      lcm = "Lepyronia coleoptrata",
                                      phm = "Philaenus spumarius",
                                      atotalm = "Aphrophoridae total"), 
                               levels = c("Aphrophoridae total", "Neophilaenus campestris",  "Lepyronia coleoptrata", "Philaenus spumarius"))
y_limit <- 3  ## Definir un límite común para el eje Y

A <-d_aphro_long %>%  
  filter(year=="2019") %>%
  ggplot( aes(x = as.factor(mes), y = value, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = pmax(value - error_value, 0), ymax = value + error_value),  # Asegurarse que el mínimo no sea menor que cero
                position = position_dodge(width = 0.9), width = 0.2) + # Añadir barras de error
  scale_fill_manual(values = c("Neophilaenus campestris" = "#466983", "Lepyronia coleoptrata" = "#E7C76F", "Philaenus spumarius" = "#93CF7B", "Aphrophoridae total" = "#D3D3D3")) +  # Colores personalizados
  labs(title ="2019",
       x = "",
       y = "",
       fill = "Species") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5"), 
                   labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May")) +
  scale_y_continuous(limits = c(0, y_limit)) +
  theme_minimal(base_size = 15) +  # Tema limpio con texto más grande
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),  # Centrar el título
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 12,color = "black"),   # Tamaño de los textos en el eje x
    axis.text.y = element_text(size = 12,color = "black"),     # Tamaño de los textos en el eje y
    legend.text = element_text(face = "italic",color = "black"),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))  

A 


B <-d_aphro_long %>%  
  filter(year=="2021") %>%
  ggplot( aes(x = as.factor(mes), y = value, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = pmax(value - error_value, 0), ymax = value + error_value),  # Asegurarse que el mínimo no sea menor que cero
                position = position_dodge(width = 0.9), width = 0.2) + # Añadir barras de error
  scale_fill_manual(values = c("Neophilaenus campestris" = "#466983", "Lepyronia coleoptrata" = "#E7C76F", "Philaenus spumarius" = "#93CF7B", "Aphrophoridae total" = "#D3D3D3")) +  # Colores personalizados
  labs(title ="2021",
       x = "",
       y = "",
       fill = "Species") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5"), 
                   labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May")) +
  scale_y_continuous(limits = c(0, y_limit)) +
  theme_minimal(base_size = 15) +  # Tema limpio con texto más grande
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),  # Centrar el título
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 12,color = "black"),   # Tamaño de los textos en el eje x
    axis.text.y = element_text(size = 12,color = "black"),     # Tamaño de los textos en el eje y
    legend.text = element_text(face = "italic",color = "black"),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))  

B    

C <-d_aphro_long %>%  
  filter(year=="2022") %>%
  ggplot( aes(x = as.factor(mes), y = value, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = pmax(value - error_value, 0), ymax = value + error_value),  # Asegurarse que el mínimo no sea menor que cero
                position = position_dodge(width = 0.9), width = 0.2) + # Añadir barras de error
  scale_fill_manual(values = c("Neophilaenus campestris" = "#466983", "Lepyronia coleoptrata" = "#E7C76F", "Philaenus spumarius" = "#93CF7B", "Aphrophoridae total" = "#D3D3D3")) +  # Colores personalizados
  labs(title ="2022",
       x = "",
       y = "",
       fill = "Species") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5"), 
                   labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May")) +
  scale_y_continuous(limits = c(0, y_limit)) +
  theme_minimal(base_size = 15) +  # Tema limpio con texto más grande
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),  # Centrar el título
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 12,color = "black"),   # Tamaño de los textos en el eje x
    axis.text.y = element_text(size = 12,color = "black"),     # Tamaño de los textos en el eje y
    legend.text = element_text(face = "italic",color = "black"),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))  

C   

# Elimina las leyendas individuales de cada gráfico y los títulos de los ejes innecesarios
A <- A + theme(legend.position = "none")
B <- B + theme(legend.position = "none")
C <- C + theme(legend.position = "none")

# Combina los gráficos con patchwork
Fig4a <- (A/B/C)+
  plot_annotation( tag_levels = "A")+
  plot_layout(guides = "collect") &  # Combina las leyendas en una única
  theme(legend.position = "bottom") 
Fig4a
###############################################################################################
###############################################################################################
##***TABLA**

d_aphro<-abundancia %>% 
  group_by( year, provincia, cultivo, mes)%>%filter(year!= '2020') %>%
  count(name = "Cuadrantes")

abundancia %>% 
  group_by( year, vect_1)%>%filter(year!= '2020') %>%
  count(name = "Species")
ninfa %>% 
  group_by( year, vect_1)%>%filter(year!= '2020') %>%
  count(name = "Species")


##############################################################################################################################################
###***POSICION NINFA POR MESES**
###*+++Fig5

##############################################################################################################################################
##############################################################################################################################################
##Daros con densidad media
posicion<-data %>% 
  group_by( mes, vect_1)%>%
  summarise (basal=mean(basal_presencia, na.rm = TRUE),
             basale=std.error(basal_presencia, na.rm = TRUE),
             media=mean(media_presencia, na.rm = TRUE),
             mediae=std.error(media_presencia, na.rm = TRUE),
             apical=mean(apical_presencia, na.rm = TRUE),
             apicale=std.error(apical_presencia, na.rm = TRUE),
             libre=mean(libre_presencia, na.rm = TRUE),
             libree=std.error(libre_presencia, na.rm = TRUE))

posicion<- posicion[!is.na(posicion$vect_1), ] #eliminar NA de id_loc

posicion1 <- posicion %>%
  pivot_longer(cols = c(basal, media, apical,libre), names_to = "variable", values_to = "value") %>%
  mutate(error_value = case_when(
    variable == "basal" ~ basale,
    variable == "media" ~ mediae,
    variable == "apical" ~ apicale,
    variable == "libre" ~ libree
  )) %>% rename(Position="variable")
posicion1$Position <- factor(recode(posicion1$Position,
                                    basal = "Basal",
                                    media = "Middle",
                                    apical = "Apical",
                                    libre = "Free"), 
                             levels = c("Basal", "Middle", "Apical","Free" )) 

y_limit <- 1  ## Definir un límite común para el eje Y

A <-posicion1 %>%  
  filter(vect_1=="neophilaenus campestris",) %>%
  ggplot( aes(x = as.factor(mes), y = value, fill = Position)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = pmax(value - error_value, 0), ymax = value + error_value),  # Asegurarse que el mínimo no sea menor que cero
                position = position_dodge(width = 0.9), width = 0.2) + # Añadir barras de error
  scale_fill_manual(values = c("Basal" = "#D6E9F2", "Middle" = "#92C5DE", "Apical" = "#4393C3", "Free" = "#2166AC"))+                    
  labs(title ="Neophilaenus campestris",
       x = "",
       y = "",
       fill = "Position") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5"), 
                   labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May")) +
  scale_y_continuous(limits = c(0, y_limit)) +
  theme_minimal(base_size = 15) +  # Tema limpio con texto más grande
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),  # Centrar el título
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 12,color = "black"),   # Tamaño de los textos en el eje x
    axis.text.y = element_text(size = 12,color = "black"),     # Tamaño de los textos en el eje y
    legend.text = element_text(face = "italic",color = "black"),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))  
A

B <-posicion1 %>%  
  filter(vect_1=="lepyronia coleoptrata",) %>%
  ggplot( aes(x = as.factor(mes), y = value, fill = Position)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = pmax(value - error_value, 0), ymax = value + error_value),  # Asegurarse que el mínimo no sea menor que cero
                position = position_dodge(width = 0.9), width = 0.2) + # Añadir barras de error
  scale_fill_manual(values = c("Basal" = "#FEE6CE", "Middle" = "#FDAE6B", "Apical" = "#F16913", "Free" = "#D94801"))+                    
  labs(title ="Lepyronia coleoptrata",
       x = "",
       y = "",
       fill = "Position") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5"), 
                   labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May")) +
  scale_y_continuous(limits = c(0, y_limit)) +
  theme_minimal(base_size = 15) +  # Tema limpio con texto más grande
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),  # Centrar el título
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 12,color = "black"),   # Tamaño de los textos en el eje x
    axis.text.y = element_text(size = 12,color = "black"),     # Tamaño de los textos en el eje y
    legend.text = element_text(face = "italic",color = "black"),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))  
B

posicion1[posicion1$mes == 2 &posicion1$vect_1 == "philaenus spumarius", "value"] <- 0
posicion1[posicion1$mes == 5 & posicion1$vect_1 == "philaenus spumarius"& posicion1$Position == "Middle", "value"] <- 0
posicion1[posicion1$mes == 5 & posicion1$vect_1 == "philaenus spumarius"& posicion1$Position == "Apical", "value"] <- 0

C <-posicion1 %>%  
  filter(vect_1=="philaenus spumarius",) %>%
  ggplot( aes(x = as.factor(mes), y = value, fill = Position)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = pmax(value - error_value, 0), ymax = value + error_value),  # Asegurarse que el mínimo no sea menor que cero
                position = position_dodge(width = 0.9), width = 0.2) + # Añadir barras de error
  scale_fill_manual(values = c("Basal" = "#E5F5E0", "Middle" = "#A1D99B", "Apical" = "#41AB5D", "Free" = "#006D2C")) +                    
  labs(title ="Philaenus spumarius",
       x = "",
       y = "",
       fill = "Position") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5"), 
                   labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May")) +
  scale_y_continuous(limits = c(0, y_limit)) +
  theme_minimal(base_size = 15) +  # Tema limpio con texto más grande
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),  # Centrar el título
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 12,color = "black"),   # Tamaño de los textos en el eje x
    axis.text.y = element_text(size = 12,color = "black"),     # Tamaño de los textos en el eje y
    legend.text = element_text(face = "italic",color = "black"),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))  
C

Fig5 <- (A/B/C)+
  plot_annotation( tag_levels = "A")

####****DATOS CON PORCENTAJE DE NINFAS EN POSICION POR MES***
posicion<-data %>% 
  group_by( mes, vect_1)%>%
  summarise(atotal=sum ((basal_presencia+media_presencia+apical_presencia+libre_presencia), na.rm = TRUE),
            basal=(sum(basal_presencia, na.rm = TRUE)*100/atotal),
            media=(sum(media_presencia, na.rm = TRUE)*100/atotal),
            apical=(sum(apical_presencia, na.rm = TRUE)*100/atotal),
            libre=(sum(libre_presencia, na.rm = TRUE)*100/atotal))


posicion<- posicion[!is.na(posicion$vect_1), ] #eliminar NA de id_loc


posicion1 <- posicion %>%
  pivot_longer(cols = c(basal, media, apical,libre), names_to = "variable", values_to = "value") %>%
  rename(Position="variable")
posicion1$Position <- factor(recode(posicion1$Position,
                                    basal = "Basal",
                                    media = "Middle",
                                    apical = "Apical",
                                    libre = "Free"), 
                             levels = c("Basal", "Middle", "Apical","Free" )) 



y_limit <- 100  ## Definir un límite común para el eje Y

A <-posicion1 %>%  
  filter(vect_1=="neophilaenus campestris",) %>%
  ggplot( aes(x = as.factor(mes), y = value, fill = Position)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("Basal" = "#D6E9F2", "Middle" = "#92C5DE", "Apical" = "#4393C3", "Free" = "#2166AC"))+                    
  labs(title ="Neophilaenus campestris",
       x = "",
       y = "",
       fill = "Position") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5"), 
                   labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May")) +
  scale_y_continuous(limits = c(0,  y_limit )) +
  theme_minimal(base_size = 15) +  # Tema limpio con texto más grande
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),  # Centrar el título
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 12,color = "black"),   # Tamaño de los textos en el eje x
    axis.text.y = element_text(size = 12,color = "black"),     # Tamaño de los textos en el eje y
    legend.text = element_text(face = "italic",color = "black"),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))  
A

B <-posicion1 %>%  
  filter(vect_1=="lepyronia coleoptrata",) %>%
  ggplot( aes(x = as.factor(mes), y = value, fill = Position)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("Basal" = "#FEE6CE", "Middle" = "#FDAE6B", "Apical" = "#F16913", "Free" = "#D94801"))+                    
  labs(title ="Lepyronia coleoptrata",
       x = "",
       y = "",
       fill = "Position") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5"), 
                   labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May")) +
  scale_y_continuous(limits = c(0, y_limit)) +
  theme_minimal(base_size = 15) +  # Tema limpio con texto más grande
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),  # Centrar el título
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 12,color = "black"),   # Tamaño de los textos en el eje x
    axis.text.y = element_text(size = 12,color = "black"),     # Tamaño de los textos en el eje y
    legend.text = element_text(face = "italic",color = "black"),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))  
B

posicion1[posicion1$mes == 2 &posicion1$vect_1 == "philaenus spumarius", "value"] <- 0
posicion1[posicion1$mes == 5 & posicion1$vect_1 == "philaenus spumarius"& posicion1$Position == "Middle", "value"] <- 0
posicion1[posicion1$mes == 5 & posicion1$vect_1 == "philaenus spumarius"& posicion1$Position == "Apical", "value"] <- 0

C <-posicion1 %>%  
  filter(vect_1=="philaenus spumarius",) %>%
  ggplot( aes(x = as.factor(mes), y = value, fill = Position)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("Basal" = "#E5F5E0", "Middle" = "#A1D99B", "Apical" = "#41AB5D", "Free" = "#006D2C")) +                    
  labs(title ="Philaenus spumarius",
       x = "",
       y = "",
       fill = "Position") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5"), 
                   labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May")) +
  scale_y_continuous(limits = c(0, y_limit)) +
  theme_minimal(base_size = 15) +  # Tema limpio con texto más grande
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),  # Centrar el título
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 12,color = "black"),   # Tamaño de los textos en el eje x
    axis.text.y = element_text(size = 12,color = "black"),     # Tamaño de los textos en el eje y
    legend.text = element_text(face = "italic",color = "black"),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))  
C

Fig5 <- (A/B/C)+
  plot_annotation( tag_levels = "A")




###***POSICION NINFA global**
###*+++Fig6

##############################################################################################################################################
##############################################################################################################################################
posicion<-data %>% 
  group_by(  vect_1)%>%
  summarise (basal = sum(basal_presencia, na.rm = TRUE),
             media = sum(media_presencia, na.rm = TRUE),
             apical = sum(apical_presencia, na.rm = TRUE),
             libre = sum(libre_presencia, na.rm = TRUE),
             total = n() ) # Total de observaciones por especie


posicion<- posicion[!is.na(posicion$vect_1), ] #eliminar NA de id_loc
posicion <- posicion %>%
  mutate(
    basal = basal / total * 100,
    media = media / total * 100,
    apical = apical / total * 100,
    libre = libre / total * 100)

posicion1 <- posicion %>%
  pivot_longer(cols = c(basal, media, apical,libre), names_to = "variable", values_to = "value") %>%
  rename(Position="variable")
posicion1$Position <- factor(recode(posicion1$Position,
                                    basal = "Basal",
                                    media = "Middle",
                                    apical = "Apical",
                                    libre = "Free"), 
                             levels = c("Basal", "Middle", "Apical","Free" ))  
posicion1$Species <- factor(recode(posicion1$vect_1,
                                   'neophilaenus campestris' = "Neophilaenus campestris",
                                   'lepyronia coleoptrata' = "Lepyronia coleoptrata",
                                   'philaenus spumarius' = "Philaenus spumarius"),
                            levels = c("Neophilaenus campestris", "Lepyronia coleoptrata" ,"Philaenus spumarius" ))  

Fig6 <-posicion1 %>%  
  filter(vect_1!="aphrophora corticea") %>%
  ggplot( aes(x = Position, y = value, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("Neophilaenus campestris" = "#466983", "Lepyronia coleoptrata" = "#E7C76F", "Philaenus spumarius" = "#93CF7B", "Aphrophoridae total" = "#D3D3D3")) +  # Colores personalizados
  labs(title ="",
       x = "",
       y = "Foam Percentage (%)",
       fill = "Species") +
  scale_x_discrete(labels = c("Basal", "Middle", "Apical", "Free")) +
  scale_y_continuous(limits = c(0,100)) +
  theme_minimal(base_size = 15) +  # Tema limpio con texto más grande
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),  # Centrar el título
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 12,color = "black"),   # Tamaño de los textos en el eje x
    axis.text.y = element_text(size = 12,color = "black"),     # Tamaño de los textos en el eje y
    legend.text = element_text(face = "italic",color = "black"),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))  

Fig6    

###***Estadios**
###*+++Fig7
####****DATOS CON PORCENTAJE DE NINFAS EN POSICION POR MES***

##############################################################################################################################################
############################################################################################################################################
##############################################################################################
data<-read_excel("ninfas_datos_bruto2.xlsx", sheet ="nt")
data <- mutate(data, year = as.numeric(format(fecha, '%Y'))) #creo una variable año
sum(is.na(data$id_loc)) #Suma de valores NA
data<- data[!is.na(data$id_loc), ] #eliminar NA de id_loc



# Crear la columna 'posicion' basada en presencia (1) o ausencia (0)
data <- data %>%
  mutate(basal_presencia = ifelse(basal > 0, 1, 0),
         media_presencia = ifelse(media > 0, 1, 0),
         apical_presencia = ifelse(apical > 0, 1, 0),
         libre_presencia = ifelse(libre > 0, 1, 0),
         position = paste(basal_presencia, media_presencia, apical_presencia, libre_presencia, sep = "_"))


data1 <- data %>%
  group_by(mes,id_loc,id_finca,fecha,  cuadrante, habitat, vect_1,year,position,esp_host) %>%
  summarise(
    est_n1 = sum(est_n1, na.rm = TRUE),
    est_n3 = sum(est_n3, na.rm = TRUE),
    est_n5 = sum(est_n5, na.rm = TRUE),
    est_imago_E = sum(est_imago_E, na.rm = TRUE)
  ) %>%
  ungroup()

stage<-data1 %>% filter(vect_1!="NA") %>% 
  pivot_longer(cols = starts_with("est_"), names_to = "estadio", values_to = "ind") %>%
  filter(vect_1 != "aphrophora corticea",year!="2020") %>% # Filtrar la especie y año no deseada
  group_by( mes, vect_1)%>%
  mutate(Total = sum(ind, na.rm = TRUE),  # Calcular el total por mes y especie
         Percentage = (ind / Total) * 100) %>%
  ungroup()

stage <- stage %>%
  mutate(Stage = recode(estadio, 
                        est_n1 = "N1", 
                        est_n3 = "N3", 
                        est_n5 = "N5", 
                        est_imago_E = "Imago")) %>% 
  mutate(vect_1 = factor(vect_1, levels = c("neophilaenus campestris", "lepyronia coleoptrata", "philaenus spumarius")))  # Reordenar las especies


# Crear gráfico de barras apiladas
Fig7<-ggplot(stage, aes(x = mes, y = Percentage, fill = Stage)) +
  geom_bar(stat = "identity") +
  
  # Facetas para cada especie (vect_1)
  facet_wrap(~ vect_1, nrow = 1) +
  
  # Etiquetas y título
  labs(title = "",
       x = "",
       y = "Percentage of individuals (%)",
       fill = "Stage") +
  scale_fill_manual(values = c("N1" = "#A8E6CF", "N3" = "#DCE775", "N5" = "#FFD54F", "Imago" = "#FF8A65")) +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5"), 
                   labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May")) +
  # Tema minimalista
  theme_minimal(base_size = 15) +
  
  # Tema limpio con texto más grande
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),  # Centrar el título
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 12,color = "black"),   # Tamaño de los textos en el eje x
    axis.text.y = element_text(size = 12,color = "black"),     # Tamaño de los textos en el eje y
    legend.text = element_text(face = "bold",color = "black"),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))  

Fig7

#######***GRAFICO COMBINADO***
#######**++++++***
# Organizar los datos por mes, estadio, especie y posición
####****DATOS CON PORCENTAJE DE NINFAS EN POSICION POR MES***
posicion<-data %>% 
  group_by( mes, vect_1)%>%
  summarise(atotal=sum ((basal_presencia+media_presencia+apical_presencia+libre_presencia), na.rm = TRUE),
            basal=(sum(basal_presencia, na.rm = TRUE)*100/atotal),
            media=(sum(media_presencia, na.rm = TRUE)*100/atotal),
            apical=(sum(apical_presencia, na.rm = TRUE)*100/atotal),
            libre=(sum(libre_presencia, na.rm = TRUE)*100/atotal))


posicion<- posicion[!is.na(posicion$vect_1), ] #eliminar NA de id_loc


posicion1 <- posicion %>%
  pivot_longer(cols = c(basal, media, apical,libre), names_to = "variable", values_to = "value") %>%
  rename(Position="variable")
posicion1$Position <- factor(recode(posicion1$Position,
                                    basal = "Basal",
                                    media = "Middle",
                                    apical = "Apical",
                                    libre = "Free"), 
                             levels = c("Basal", "Middle", "Apical","Free" )) 


posicion1<-posicion1 %>% 
  mutate(vect_1 = factor(vect_1, levels = c("neophilaenus campestris", "lepyronia coleoptrata", "philaenus spumarius")))

D <-posicion1 %>%  
  filter(vect_1!="aphrophora corticea",) %>%
  ggplot( aes(x = as.factor(mes), y = value, fill = Position)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_wrap(~ vect_1, nrow = 1) +
  scale_fill_manual(values = c("Basal" = "#D6E9F2", "Middle" = "#92C5DE", "Apical" = "#4393C3", "Free" = "#2166AC"))+                    
  labs(title ="",
       x = "",
       y = "Percentage of individuals (%)",
       fill = "Position") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5"), 
                   labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May")) +
  scale_y_continuous(limits = c(0,  y_limit )) +
  theme_minimal(base_size = 15) +  # Tema limpio con texto más grande
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),  # Centrar el título
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje y
    axis.text.x = element_text(size = 12,color = "black"),   # Tamaño de los textos en el eje x
    axis.text.y = element_text(size = 12,color = "black"),     # Tamaño de los textos en el eje y
    legend.text = element_text(face = "italic",color = "black"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2))  
D

Fig10<-(Fig7/D)+
  plot_annotation( tag_levels = "A")
Fig10


