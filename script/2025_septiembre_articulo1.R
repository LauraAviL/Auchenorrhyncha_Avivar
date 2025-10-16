##***PREPARACION DE DATOS**
rm(list = ls(all.names = TRUE))
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
pacman::p_load(skimr,writexl,readr,readxl,data.table)
pacman::p_load(tidyverse,tidylog,summarytools,here,plotrix,lubridate, textshape) 
pacman::p_load(bipartite,ggforce, FSA, vegan, mvabund, reshape2, iNEXT,indicspecies, ggrepel) 
pacman::p_load(dplyr,tidyr,vegan,purrr)

# =========================
# 0) Data_Clean
# =========================
tax<-read_excel(here("data/resultado_taxonomia_final2.xlsx")) 
tax1<-tax |> 
  mutate( muestra_id = paste(id_finca, fecha, sep = "_")) |> 
  dplyr::filter(año!=2020, !iden %in% c("noiden", "genero"))  #delete year=2020 and Unidentifiable
metadato_tax1 <- tax1 |> distinct(crop,  muestra_id)


df <- tax1 |>
  group_by(crop, id_finca, fecha, muestra_id, code) |>
  summarise(n = sum(total, na.rm = TRUE), .groups = "drop")

metadato_df <- df |> distinct(crop,  muestra_id)

df<-df |>  filter(!is.na(code), n >= 0)

# === 1) Exploratorio (esfuerzo real + diversidad observada) ===
exploratorio <- metadato_tax1|> count(crop, name = "n_muestras_total") |>
  left_join(df |> distinct(crop, muestra_id) |> count(crop, name = "n_muestras_con_especies"),
            by = "crop") |>
  mutate(n_muestras_con_especies = replace_na(n_muestras_con_especies, 0L)) |>
  left_join(df |> group_by(crop) |>
              summarise(abund_total = sum(n), riqueza = n_distinct(code), .groups="drop"),
            by = "crop") |>
  left_join({
    M <- df|> group_by(crop, code) |> summarise(n = sum(n), .groups="drop") |>
      pivot_wider(names_from = code, values_from = n, values_fill = 0)
    tibble(crop = M$crop,
           shannon_H = diversity(as.matrix(M[,-1,drop=FALSE]), "shannon"),
           simpson   = diversity(as.matrix(M[,-1,drop=FALSE]), "simpson"))
  }, by = "crop") |>
  arrange(crop)

print(exploratorio)



#Matriz parcela+fecha suma de abundancia, si se separa por cultivo solo hay riesgo de atribuir al cultivo
#lo que en realidad es efecto del sitio (manejo, el paisaje, el microclima...)


metadato_df_0 <- df |> distinct(crop,  muestra_id)

matriz<-df |> 
  select(muestra_id, code, n) |>
  tidyr::pivot_wider(names_from = code, values_from = n, values_fill = 0) |> 
  select(-muestra_id)


# === 2) Curvas de acumulación (solo muestras con ≥1 especie para la curva) ===
set.seed(123)
mats <- df |>
  group_by(crop) |>
  group_map(~ {
    w <- pivot_wider(.x, names_from = code, values_from = n, values_fill = 0) |> arrange(muestra_id)
    if (nrow(w) == 0) return(NULL)
    mat <-data.matrix(w[,-1,drop=FALSE])
    rownames(mat) <- w$muestra_id
    mat[, colSums(mat) > 0, drop = FALSE]             # quita especies nunca observadas
  }, .keep = TRUE) |> 
  set_names(unique(df$crop))

specacc <- imap(mats, ~  if (!is.null(.x) && nrow(.x) >= 2)
  specaccum(.x, method = "random", permutations = min(200, max(1, factorial(nrow(.x))))) else NULL)

par(mfrow = c(ceiling(length(specacc)/2), 2), mar = c(4,4,2,1))
imap(specacc, ~ {
  if (is.null(.x)) {
    S <- if (is.null(mats[[.y]])) 0 else sum(colSums(mats[[.y]]) > 0)
    plot(1, S, xlim = c(1,2), ylim = c(0, max(2, S*1.2)), xlab = "Nº muestras", ylab = "Especies acumuladas",
         main = paste("Acumulación -", .y)); points(1, S, pch = 19)
  } else {
    plot(.x, ci.type = "poly", ci.col = "lightgray", ci.lty = 0,
         xlab = "Nº muestras (id_finca×fecha)", ylab = "Especies acumuladas", main = paste("Acumulación -", .y))
    lines(.x, lwd = 2)
  }
})
par(mfrow = c(1,1))


#===Curva de rarefacción===

##Rarefaction

# --- Incidence list: c(T, Q1, Q2, ...); T = nº total de muestras (incluye 0 especies) ---
incidence_list <- metadato_df_0 |>
  group_split(crop) |> set_names(unique(metadato_df_0$crop)) |>
  lapply(function(M) {
    crop_i <- unique(M$crop)
    Tm <- nrow(M)  # total samples for this crop
    A  <- df |> filter(crop == crop_i) |>
      select(muestra_id, code, n) |>
      pivot_wider(names_from = code, values_from = n, values_fill = 0)
    if (nrow(A) == 0) return(c(Tm))
    mat <- as.matrix(A[ , -1, drop = FALSE])
    Qi  <- colSums(mat > 0)
    c(Tm, as.numeric(Qi))
  })

# --- Rarefaction at common number of samples (minimum across crops) ---
n_common <- min(sapply(incidence_list, function(v) ifelse(length(v) > 0, v[1], Inf)))
div_raref <- estimateD(incidence_list, q = c(0,1,2),
                       datatype = "incidence_freq",
                       base = "size", level = n_common) |>
  arrange(Assemblage, Order.q)

print(div_raref)
# q=0: expected richness at n_common samples; q=1: Shannon-type; q=2: Simpson-type

# ---  rarefaction/extrapolation plot by number of samples ---
# Rarefacción/extrapolación con iNEXT
out_inc <- iNEXT(incidence_list, q = c(0,1,2), datatype = "incidence_freq")

# Extraer la tabla de rarefacción/extrapolación (sample-size based)
sb <- out_inc$iNextEst$size_based

# Renombrar 't' -> 'm' para compatibilidad con el plot
sb <- sb %>%
  rename(m = t) %>%
  mutate(
    Assemblage = factor(Assemblage),
    Order.q = factor(Order.q, levels = c(0,1,2),
                     labels = c("q=0","q=1","q=2"))
  )

ggplot(sb, aes(x = m, y = qD, color = Assemblage, fill = Assemblage)) +
  geom_ribbon(aes(ymin = qD.LCL, ymax = qD.UCL), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +   # usa linewidth en vez de size (aviso de ggplot2>=3.4)
  facet_wrap(~ Order.q, scales = "free_y") +
  labs(x = "Número de muestras (id_finca × fecha)",
       y = "Diversidad esperada",
       title = "Rarefacción/extrapolación basada en muestras por cultivo") +
  theme_bw(base_size = 14) +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "bottom")

#Matriz visita por especie (los ceros estan previamente eliminados)

matriz<-df |> 
  select(muestra_id, code, n) |>
  tidyr::pivot_wider(names_from = code, values_from = n, values_fill = 0)
metadato_df_0



