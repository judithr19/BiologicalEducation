# ==============================================================================
# ==============================================================================
# ==============================================================================
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/Documentos - MacBook Pro de Apple/Colaboraciones/Lizbeth/Biología/America")

# install.packages
library(readxl);library(dplyr);library(purrr)
library(psych);library(MBESS);library(tidyverse)
library(vegan);library(ggplot2)
library(effectsize);library(tidyr)
library(factoextra);library(writexl)


#_________________________________#
# Read Excel file
ema <- read_excel("datos.xlsx")
names(ema) <- as.character(unlist(ema[1, ]))
ema <- ema[-1, ]
names(ema) <- c(
  "ID",
  paste0("I", 1:28),
  "Amotivation",
  "External",
  "Introjected",
  "Identified",
  "IM_Knowledge",
  "IM_Accomplishment",
  "IM_Stimulation"
)
#________________________________#
# Basic checks
dim(ema)
names(ema)
str(ema)
head(ema)

# Total missing values
sum(is.na(ema))
# Number of students
nrow(ema)
# Number of variables
ncol(ema)

# Convertir a numéricos
ema <- ema |>
  mutate(across(-ID, as.numeric))
sapply(ema, class)
#________________________________#
#________________________________#
#________________________________#
# Estadística Descriptiva
stats_tbl <- map_dfr(
  names(ema)[30:36],
  \(v)
  data.frame(
    Scale   = v,
    Min     = min(ema[[v]]),
    Max     = max(ema[[v]]),
    Median  = median(ema[[v]]),
    IQR     = IQR(ema[[v]]),
    Mean    = mean(ema[[v]]),
    SD      = sd(ema[[v]]),
    Floor   = mean(ema[[v]] == 4) * 100,
    Ceiling = mean(ema[[v]] == 28) * 100
  )
)
Tabla1<-stats_tbl

#________________________________#
#________________________________#
#________________________________#
# Matriz de correlaciones
scales <- ema %>%
  select(Amotivation:IM_Stimulation)
cor_matrix <- cor(
  scales,
  method = "spearman",
  use = "pairwise.complete.obs"
)
Tabla2<-round(cor_matrix, 2)

#________________________________#
# Identificación de los ítems
scales <- list(
  Amotivation       = c("I5","I12","I19","I26"),
  External          = c("I1","I8","I15","I22"),
  Introjected       = c("I7","I14","I21","I28"),
  Identified        = c("I3","I10","I17","I24"),
  IM_Knowledge      = c("I2","I9","I16","I23"),
  IM_Accomplishment = c("I6","I13","I20","I27"),
  IM_Stimulation    = c("I4","I11","I18","I25")
)

# Confiabilidad de las escalas
reliability <- map_dfr(
  names(scales),
  function(s){
    dat <- ema[, scales[[s]]]
    alpha_val <- psych::alpha(dat)$total$raw_alpha
    omega_val <- psych::omega(
      dat,
      nfactors = 1,
      plot = FALSE
    )$omega.tot
    data.frame(
      Scale = s,
      Alpha = alpha_val,
      Omega = omega_val
    )
  }
)
Tabla3<-reliability %>%
  mutate(
    Alpha = round(Alpha, 3),
    Omega = round(Omega, 3)
  )

#Internal consistency for the current sample (N = 214) was good
#to excellent across the seven AMS subscales, with Cronbach’s alpha 
#coefficients ranging from 0.810 to 0.926 and McDonald’s omega 
#coefficients ranging from 0.813 to 0.927.

#________________________________#
# Gráfica
plot_data <- ema %>%
  select(
    Amotivation,
    External,
    Introjected,
    Identified,
    IM_Knowledge,
    IM_Accomplishment,
    IM_Stimulation
  ) %>%
  pivot_longer(
    everything(),
    names_to = "Scale",
    values_to = "Score"
  )

plot_data$Scale <- factor(
  plot_data$Scale,
  levels = c(
    "Amotivation",
    "External",
    "Introjected",
    "Identified",
    "IM_Stimulation",
    "IM_Accomplishment",
    "IM_Knowledge"
  ),
  labels = c(
    "Amotivation (A)",
    "Extrinsic Regulation (X)",
    "Introjected Regulation (T)",
    "Identified Regulation (D)",
    "IM to Experience Stimulation (E)",
    "IM to Accomplish (L)",
    "IM to Know (C)"
  )
)

cols <- c(
  "#B2182B",
  "#D6604D",
  "#E6A57E",
  "#A6C8D8",
  "#5A98B8",
  "#2F6DA8",
  "#083D77"
)

g1<-ggplot(plot_data,
       aes(x = Score,
           y = Scale,
           fill = Scale)) +
  geom_violin(
    trim = FALSE,
    colour = "grey20",
    linewidth = 0.5
  ) +
  geom_boxplot(
    width = 0.15,
    fill = "white",
    outlier.shape = NA
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 23,
    size = 3,
    fill = "#F4B000",
    colour = "black",
    aes(shape = "Mean")
  ) +
  scale_fill_manual(values = cols) +
  scale_shape_manual( values = 23,name = NULL) +
  scale_x_continuous(
    limits = c(4, 28),
    breaks = seq(4, 28, 4)
  ) +
  labs(
    x = "Composite Subscale Score (range: 4–28)",
    y = NULL
  ) +
  guides(
    fill  = "none",
    shape = guide_legend(title = NULL)
  )+
  theme_bw(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(
      colour = "grey80",
      linetype = "dashed"
    ),
    legend.position = c(.90, .08),
    legend.background = element_rect(
      fill = "white",
      colour = "grey80"
    )
  )
g1

#__________________________#
# Non-Metric Multidimensional Scaling (NMDS)
mot <- ema[, 30:36]
# Seed
set.seed(123)
nmds <- metaMDS(
  mot,
  distance = "bray",
  k = 2,
  autotransform = TRUE,
  trymax = 100
)
NMDS<-nmds$stress

# Environmental fitting
fit <- envfit(nmds, mot)
# Coordinates
vec <- as.data.frame(scores(fit, display = "vectors"))
vec$Scale <- rownames(vec)
vec

cols <- c(
  Amotivation         = "#c0392b",
  External            = "#e67e22",
  Introjected         = "#f1c40f",
  Identified          = "#27ae60",
  IM_Knowledge        = "#2980b9",
  IM_Accomplishment   = "#8e44ad",
  IM_Stimulation      = "#16a085"
)

g2<-ggplot(vec,
       aes(NMDS1, NMDS2,
           color = Scale,
           label = Scale)) +
  geom_hline(yintercept = 0,
             linewidth = .3,
             colour = "grey80") +
  geom_vline(xintercept = 0,
             linewidth = .3,
             colour = "grey80") +
  geom_segment(
    aes(x = 0,
        y = 0,
        xend = NMDS1,
        yend = NMDS2),
    linewidth = 1.1,
    arrow = arrow(
      length = unit(0.22, "cm")
    )
  ) +
  geom_point(
    size = 4
  ) +
  ggrepel::geom_text_repel(
    size = 5,
    box.padding = .4,
    segment.color = NA
  ) +
  scale_color_manual(values = cols) +
  labs(
    x = "NMDS1",
    y = "NMDS2",
    title = "Motivational Structure of AMS Subscales",
    subtitle = paste0(
      "NMDS (Bray-Curtis), Stress = ",
      round(nmds$stress, 3)
    )
  ) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )
#__________________________# 
#__________________________# 
#__________________________# 
# Más análisis de clústeres y MANOVA se encuentran en la sección siguiente.
mot <- ema[, 30:36]
mot_z <- scale(mot)
d <- dist(mot_z)
# cluster analysis
hc <- hclust(d, method = "ward.D2")
cluster <- cutree(hc, k = 2)

plot(hc,hang = -1,cex = 0.7,
     main = "Hierarchical clustering of students")

fviz_nbclust(
  mot_z,
  kmeans,
  method = "silhouette"
)

#Hierarchical clustering using Ward's method identified two optimal 
#clusters based on the maximum average silhouette width (0.35).
#This suggests a moderate cluster structure among students, supporting
#the existence of two distinct motivational profiles.

ema$cluster <- cutree(hc, k = 2)
table(ema$cluster)

fit <- manova(
  cbind(
    Amotivation,
    External,
    Introjected,
    Identified,
    IM_Knowledge,
    IM_Accomplishment,
    IM_Stimulation
  ) ~ factor(cluster),
  data = ema
)
Tabla4<-summary(fit, test = "Wilks")

summary.aov(fit)

perfil <- ema %>%
  group_by(cluster) %>%
  summarise(
    across(
      c(Amotivation,
        External,
        Introjected,
        Identified,
        IM_Knowledge,
        IM_Accomplishment,
        IM_Stimulation),
      mean
    )
  ) %>%
  pivot_longer(
    -cluster,
    names_to = "Scale",
    values_to = "Mean"
  )
perfil

g4 <- ggplot(
  perfil,
  aes(
    x = Scale,
    y = Mean,
    color = factor(cluster),
    group = factor(cluster)
  )
) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 4) +
  scale_color_manual(
    values = c("#1B4965", "#E76F51"),
    labels = c("Cluster 1", "Cluster 2")
  ) +
  labs(
    x = NULL,
    y = "Mean score",
    color = NULL
  ) +
  theme_classic(base_size = 16) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 13),
    axis.text.x = element_text(
      angle = 35,
      hjust = 1,
      face = "bold"
    ),
    axis.text.y = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    panel.border = element_blank()
  )

vars <- c(
  "Amotivation","External","Introjected","Identified",
  "IM_Knowledge","IM_Accomplishment","IM_Stimulation")

lapply(vars, function(v){
  model <- aov(
    as.formula(
      paste(v,"~ factor(cluster)")
    ), data = ema)
  eta_squared(model)
})

#Hierarchical clustering identified two groups of students (n = 147 and n = 67). 
#A MANOVA revealed significant multivariate differences between clusters 
#(Wilks' Λ = 0.297, F(7,206) = 69.60, p < .001). Follow-up univariate analyses 
#showed significant differences for all motivational dimensions except amotivation. 
#Students in Cluster 1 exhibited consistently higher scores across external, 
#introjected, identified, and intrinsic motivation dimensions than students 
#in Cluster 2.

#__________________________# 
#__________________________# 
#__________________________# 
# Resultados Finales

# Gráficas
g1     # Gráfica de violín y caja
g2     # NMDS
g4     # Gráfica de perfiles de clústeres
ggsave("Figure1.jpeg", g1, dpi = 300)
ggsave("Figure2.jpeg", g2, dpi = 300)
ggsave("Figure4.jpeg", g4, dpi = 300)


# Tablas
Tabla1 # Estadística Descriptiva
Tabla2 # Matriz de correlaciones
Tabla3 # Confiabilidad de las escalas
Tabla4 # MANOVA
# Nota: Tabla2 (matriz de correlaciones) tiene nombres de fila que conviene conservar
tabla2_df <- data.frame(
  Variable = rownames(Tabla2),
  Tabla2,
  row.names = NULL,
  check.names = FALSE
)

# Nota: Tabla4 es un objeto de clase summary.manova; extraemos su tabla numérica
tabla4_df <- as.data.frame(Tabla4$stats)
tabla4_df <- cbind(Term = rownames(tabla4_df), tabla4_df)
rownames(tabla4_df) <- NULL

write_xlsx(
  list(
    Tabla1_Descriptivos = Tabla1,
    Tabla2_Correlaciones = tabla2_df,
    Tabla3_Confiabilidad  = Tabla3,
    Tabla4_MANOVA       = tabla4_df
  ),
  path = "Tablas_Resultados.xlsx"
)
