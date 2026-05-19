library(readxl)
library(ggplot2)
library(dplyr)

# --- Cargar datos ---
df <- read_excel("datos.xlsx")

# --- Variables ---
target <- "Ag Barometer"
covs   <- names(df)[3:15]

# --- Calcular correlaciones y p-valores ---
resultados <- lapply(covs, function(cov) {
  tmp <- df[, c(target, cov)] |> na.omit()
  test <- cor.test(tmp[[target]], tmp[[cov]], method = "pearson")
  data.frame(
    variable = cov,
    r        = round(test$estimate, 3),
    p        = test$p.value,
    n        = nrow(tmp)
  )
}) |> bind_rows()

resultados <- resultados |>
  mutate(
    signo     = ifelse(r >= 0, "Positiva", "Negativa"),
    sig_label = ifelse(p < 0.05, "p < 0.05", "p ≥ 0.05"),
    label     = paste0("r = ", r)
  ) |>
  arrange(r)

resultados$variable <- factor(resultados$variable, levels = resultados$variable)

# --- Gráfico ---
ggplot(resultados, aes(x = r, y = variable, fill = signo, alpha = sig_label)) +
  geom_col(width = 0.6) +
  geom_vline(xintercept = 0, linewidth = 0.4, color = "grey40") +
  geom_text(
    aes(
      label = label,
      hjust = ifelse(r >= 0, -0.1, 1.1)
    ),
    size  = 3.2,
    color = "grey20"
  ) +
  scale_fill_manual(values = c("Positiva" = "#c0392b", "Negativa" = "#2980b9")) +
  scale_alpha_manual(values = c("p < 0.05" = 1, "p ≥ 0.05" = 0.4)) +
  scale_x_continuous(
    limits = c(-0.45, 0.45),
    breaks = seq(-0.4, 0.4, 0.1),
    expand = c(0, 0)
  ) +
  labs(
    title    = "Correlación de Pearson: Ag Barometer vs covariables",
    subtitle = "Barras con transparencia = p ≥ 0.05; barras sólidas = p < 0.05",
    x        = "Coeficiente de correlación (r)",
    y        = NULL,
    fill     = "Dirección",
    alpha    = "Significancia"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(color = "grey50", size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position  = "bottom"
  )

# --- Guardar ---
ggsave("correlaciones_agbarometer.png", width = 8, height = 6, dpi = 150)
