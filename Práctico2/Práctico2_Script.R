
################################################################################
# Estadística Inferencial - Curso 2025 - UCU
#
# Práctico 2 – Muestreo con población conocida (Censo 2023)
#
# Temas: SRS → Estratificación → Conglomerados → FPC/deff → n → TCL
################################################################################

##------------------------------------------------------------------------------
## 0) PAQUETES Y CONFIGURACIÓN -------------------------------------------------
##------------------------------------------------------------------------------

# install.packages(c("tidyverse","survey","ggplot2","scales"))

library(tidyverse)
library(survey)
library(ggplot2)
library(scales)
set.seed(2145)     # Fijamos semilla para reproducibilidad
theme_set(theme_minimal(base_size = 12))


##------------------------------------------------------------------------------
## 1) CARGA DE LA POBLACIÓN (Censo 2023, base de hogares) ----------------------
##------------------------------------------------------------------------------

# 1.1 Indicá la ruta y archivo .Rdata (ajusta si hace falta)

ruta   <- "D:/Dropbox/Docencia/Estadística inferencial UCU/práctico/practico2"     # <--- ajustar
archivo <- file.path(ruta, "data/hogares_ext_26_02.Rdata")
load(archivo)

# 1.2 Crear dataframe sobre el que vamos a trabajar
base <- x

# 1.3 En este práctico vamos a trabajar con estas variables:
#     - Acceso a Internet del hogar (binaria, vale 1 si tiene internet, 2 si no tiene): HOGCE11
#     - Cantidad de personas del hogar: HOGPR01_CON_RRAA
#     - Habitaciones totales: HOGHD00
#     - Estratos/PSU: REGION_4 (estratos) y LOCALIDAD (conglomerados)


##------------------------------------------------------------------------------
## 2) SELECCIÓN Y LIMPIEZA BÁSICA ----------------------------------------------
##------------------------------------------------------------------------------

# Elegimos solo las columnas que necesitamos
# Creamos:
#  - internet: indicador 1/0 
#  - ppr: personas por habitación (variable cuantitativa continua)
# Nos quedamos con observaciones completas para estas dos variables clave

pobl <- base %>%
  select(HOGCE11, HOGPR01_CON_RRAA, HOGHD00, REGION_4, LOCALIDAD) %>%
  rename(
    internet_raw = HOGCE11,
    personas     = HOGPR01_CON_RRAA,
    hab_tot      = HOGHD00,
    region4      = REGION_4,
    localidad    = LOCALIDAD
  ) %>%
  mutate(
    internet = case_when(
      is.numeric(internet_raw) ~ as.integer(internet_raw == 1),
      is.character(internet_raw) ~ as.integer(str_to_upper(internet_raw) %in% c("SI","SÍ","1","SÍ")),
      TRUE ~ NA_integer_
    ),
    personas = suppressWarnings(as.numeric(personas)),
    hab_tot  = suppressWarnings(as.numeric(hab_tot)),
    hab_tot  = if_else(hab_tot <= 0 | is.na(hab_tot), NA_real_, hab_tot),
    ppr      = personas / pmax(hab_tot, 1)  # evitamos división por 0
  ) %>%
  filter(!is.na(internet), !is.na(ppr)) %>%
  mutate(
    region4   = factor(region4),
    localidad = factor(localidad)
  )

# Tamaño de la población que usaremos (N)
Npop <- nrow(pobl)


##------------------------------------------------------------------------------
## 3) PARÁMETROS POBLACIONALES (CONOCIDOS) -------------------------------------
##------------------------------------------------------------------------------

# En este práctico asumimos que el censo es la población: podemos calcular
# el "verdadero" valor de cada parámetro para comparar con las estimaciones

p_verdadera   <- mean(pobl$internet)   # proporción de hogares con Internet
mu_verdadera  <- mean(pobl$ppr)        # media de personas por habitación
sd_ppr        <- sd(pobl$ppr)

cat("Parámetros poblacionales (verdaderos):\n")
cat("  Proporción Internet =", percent(p_verdadera, accuracy = 0.1), "\n")
cat("  Media PPR            =", round(mu_verdadera, 3), "\n\n")


##------------------------------------------------------------------------------
## 4) MUESTREO 1: SRS (Muestreo Aleatorio Simple, sin reemplazo) ---------------
##------------------------------------------------------------------------------

# Elegimos un tamaño de muestra n (cambiar y ver qué pasa)
n <- 1200

# Tomamos una muestra SRS de tamaño n
m_srs <- pobl %>%
  slice_sample(n = n, replace = FALSE) %>%
  mutate(fpc = Npop)  # FPC (corrección por población finita)

# Definimos el diseño muestral SRS con FPC, que en R es un objeto más
d_srs <- svydesign(ids = ~1, fpc = ~fpc, data = m_srs)

# Estimamos proporción e intervalo al 95%
est_p_srs <- svymean(~internet, d_srs)              # estimador y error estándar
ci_p_srs  <- confint(est_p_srs)                     # intervalo de confianza 95%

# Estimamos media de PPR e intervalo al 95%
est_mu_srs <- svymean(~ppr, d_srs)
ci_mu_srs  <- confint(est_mu_srs)


##------------------------------------------------------------------------------
## 5A) MUESTREO 2: ESTRATIFICADO (proporcional por REGION_4) --------------------
##------------------------------------------------------------------------------

# Paso 1: tamaños de estrato en la población (Nh)
Nh_tab <- pobl %>% count(region4, name = "Nh")

# Paso 2: asignación proporcional de n a cada estrato (nh)
nh_tab <- Nh_tab %>%
  mutate(nh = round(n * Nh / sum(Nh)))

# Paso 3: tomamos muestra en cada estrato
Nh_tab <- pobl %>% count(region4, name = "Nh")
nh_tab <- Nh_tab %>% mutate(nh = round(n * Nh / sum(Nh)))
# Ordenamos para alinear grupos y tamaños a muestrear
nh_tab_ord <- nh_tab %>% arrange(region4)
grupos <- pobl %>%
  arrange(region4) %>%              # asegura el mismo orden que nh_tab_ord
  group_split(region4)
# Muestra estratificada (si nh > tamaño del grupo, ajustamos con pmin)
m_str <- purrr::map2_dfr(
  grupos, nh_tab_ord$nh,
  ~ dplyr::slice_sample(.x, n = pmin(.y, nrow(.x)), replace = FALSE)
)
# Volvemos a unir Nh para usar FPC luego
m_str <- m_str %>% left_join(Nh_tab, by = "region4")

# Paso 4: diseño estratificado con FPC por estrato (con nh proporcional a Nh todos tienen igual peso,
# si hubiera que ponderar sería con opción ", weights = ~I(Nh/nh)")
d_str <- svydesign(ids = ~1, strata = ~region4, fpc = ~Nh, data = m_str)

# Estimaciones
est_p_str <- svymean(~internet, d_str)
ci_p_str  <- confint(est_p_str)

est_mu_str <- svymean(~ppr, d_str)
ci_mu_str  <- confint(est_mu_str)

## --- Tabla de dispersión por estrato (para decidir Neyman) -------------------
# Muestra, por cada región (estrato), tamaño Nh, proporción de Internet y su SD,
# y la media/SD de PPR (esta SD es la Sh que usa Neyman si optimizamos para PPR).

tab_disp <- pobl %>%
  group_by(region4) %>%
  summarise(
    Nh = n(),
    Wh = Nh / nrow(pobl),
    p_internet = mean(internet),              # proporción en el estrato
    sd_internet = sqrt(var(internet)),        # = sqrt(p_h * (1 - p_h))
    mean_ppr = mean(ppr),
    sd_ppr = sd(ppr),                         # <- Sh para Neyman (si optimizamos PPR)
    cv_ppr = sd_ppr / mean_ppr,
    .groups = "drop"
  ) %>%
  arrange(region4)

tab_disp_print <- tab_disp %>%
  transmute(
    region4, Nh,
    Wh = round(Wh, 3),
    p_internet = scales::percent(p_internet, accuracy = 0.1),
    sd_internet = round(sd_internet, 3),
    mean_ppr = round(mean_ppr, 3),
    sd_ppr = round(sd_ppr, 3),
    cv_ppr = round(cv_ppr, 3)
  )

print(tab_disp_print)


##------------------------------------------------------------------------------
## 5B) ESTRATIFICADO (Neyman por PPR) ------------------------------------------
##------------------------------------------------------------------------------

# Idea: asignación óptima para estimar una MEDIA: nh ∝ Nh * Sh,
# donde Sh es el desvío estándar de la variable objetivo en el estrato.
# Acá optimizamos para PPR.

nh_tab_n <- pobl %>%
  group_by(region4) %>%
  summarise(Nh = n(),
            Sh = sd(internet, na.rm = TRUE),   # <- cambiar a 'internet' para Neyman para proporción
            .groups = "drop") %>%
  mutate(nh = pmax(2, round(n * Nh * Sh / sum(Nh * Sh)))) %>%
  select(region4, Nh, nh)

# Muestreo por estrato (misma lógica split + map2_dfr que usaste antes)
nh_tab_n_ord <- nh_tab_n %>% arrange(region4)
grupos_n <- pobl %>% arrange(region4) %>% group_split(region4)

m_str_n <- purrr::map2_dfr(
  grupos_n, nh_tab_n_ord$nh,
  ~ dplyr::slice_sample(.x, n = pmin(.y, nrow(.x)), replace = FALSE)
) %>%
  left_join(nh_tab_n %>% select(region4, Nh), by = "region4")

# Diseño estratificado con FPC (N_h por estrato)
d_str_n <- svydesign(ids = ~1, strata = ~region4, fpc = ~Nh, data = m_str_n)

# Estimaciones (ambas variables, aunque el óptimo fue pensado para PPR)
est_p_str_n  <- svymean(~internet, d_str_n);  ci_p_str_n  <- confint(est_p_str_n)
est_mu_str_n <- svymean(~ppr,      d_str_n);  ci_mu_str_n <- confint(est_mu_str_n)


##------------------------------------------------------------------------------
## 6) MUESTREO 3: CONGLOMERADOS (1 etapa, PSU = LOCALIDAD) ---------------------
##------------------------------------------------------------------------------

# Idea simple: elegimos m localidades al azar y tomamos TODOS los hogares dentro
# Nota: el tamaño final puede no ser exactamente n (depende de cuántos hogares tenga cada localidad)

m <- 60  # número de localidades a seleccionar (cambiar y observar el efecto)

psu_sel <- pobl %>%
  distinct(localidad) %>%
  slice_sample(n = m) %>%
  pull(localidad)

m_clu <- pobl %>%
  filter(localidad %in% psu_sel)

# Diseño por conglomerados (con reemplazo, conservador)
d_clu <- svydesign(ids = ~localidad, data = m_clu)

# Estimaciones
est_p_clu <- svymean(~internet, d_clu)
ci_p_clu  <- confint(est_p_clu)

est_mu_clu <- svymean(~ppr, d_clu)
ci_mu_clu  <- confint(est_mu_clu)


##------------------------------------------------------------------------------
## 7) COMPARACIÓN DE RESULTADOS ENTRE DISEÑOS ----------------------------------
##------------------------------------------------------------------------------

res <- tibble(
  diseno = c("SRS (FPC)",
             "Estratificado (FPC)",
             "Estratificado Neyman (FPC)",
             "Conglomerados (WR)"),
  p_hat  = c(coef(est_p_srs)[1],   coef(est_p_str)[1],   coef(est_p_str_n)[1],   coef(est_p_clu)[1]),
  se_p   = c(SE(est_p_srs)[1],     SE(est_p_str)[1],     SE(est_p_str_n)[1],     SE(est_p_clu)[1]),
  lo_p   = c(ci_p_srs[1,1],        ci_p_str[1,1],        ci_p_str_n[1,1],        ci_p_clu[1,1]),
  hi_p   = c(ci_p_srs[1,2],        ci_p_str[1,2],        ci_p_str_n[1,2],        ci_p_clu[1,2]),
  mu_hat = c(coef(est_mu_srs)[1],  coef(est_mu_str)[1],  coef(est_mu_str_n)[1],  coef(est_mu_clu)[1]),
  se_mu  = c(SE(est_mu_srs)[1],    SE(est_mu_str)[1],    SE(est_mu_str_n)[1],    SE(est_mu_clu)[1]),
  lo_mu  = c(ci_mu_srs[1,1],       ci_mu_str[1,1],       ci_mu_str_n[1,1],       ci_mu_clu[1,1]),
  hi_mu  = c(ci_mu_srs[1,2],       ci_mu_str[1,2],       ci_mu_str_n[1,2],       ci_mu_clu[1,2])
)

print(res)
cat("\nRecuerdo: parámetros verdaderos — p =", percent(p_verdadera, 0.1),
    " | μ(PPR) =", round(mu_verdadera,3), "\n\n")

# Gráfico 1: Proporción de hogares con Internet -----------------
res_p <- res %>%
  transmute(diseno, est = p_hat, lo = lo_p, hi = hi_p)

ggplot(res_p, aes(x = diseno, y = est)) +
  geom_hline(yintercept = p_verdadera, linetype = "dashed") +
  geom_pointrange(aes(ymin = lo, ymax = hi)) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Estimación de la proporción de hogares con Internet",
    subtitle = "Punto e IC 95% por diseño muestral; línea punteada = valor verdadero (Censo)",
    x = "Diseño muestral",
    y = "Proporción"
  )

# Gráfico 2: Media de personas por habitación (PPR) -------------
res_mu <- res %>%
  transmute(diseno, est = mu_hat, lo = lo_mu, hi = hi_mu)

ggplot(res_mu, aes(x = diseno, y = est)) +
  geom_hline(yintercept = mu_verdadera, linetype = "dashed") +
  geom_pointrange(aes(ymin = lo, ymax = hi)) +
  coord_flip() +
  labs(
    title = "Estimación de la media de personas por habitación (PPR)",
    subtitle = "Punto e IC 95% por diseño muestral; línea punteada = valor verdadero (Censo)",
    x = "Diseño muestral",
    y = "Media (PPR)"
  )

# Comparación de varianzas para evaluar ganancia de precisión en estratos proporcionales
# Obs: La estratificación proporcional mejora precisión cuando el estratificador reduce la variabilidad dentro de estratos (ratio < 1).
# En nuestros datos, region4 ayuda para Internet pero casi nada para PPR (donde depende de las muestras -ver semilla)

var_ratio <- function(df, y, strata){
  S2 <- var(df[[y]], na.rm=TRUE)
  tab <- df %>% group_by({{strata}}) %>%
    summarise(Wh = n()/nrow(df), Sh2 = var(.data[[y]], na.rm=TRUE), .groups="drop")
  tibble(ratio = sum(tab$Wh * tab$Sh2) / S2,
         lectura = if_else(ratio < 1, "estratos (prop.) mejor en promedio", "SRS mejor en promedio"))
}

var_ratio(pobl, "internet", region4)
var_ratio(pobl, "ppr",      region4)



##------------------------------------------------------------------------------
## 8) TAMAÑO DE MUESTRA: fórmulas directas (con DEFF y FPC) --------------------
##------------------------------------------------------------------------------

z_alpha <- function(conf = 0.95) qnorm(1 - (1-conf)/2)

# Para la media (con valor de sd de la variable objetivo)
n_media <- function(E, sd, conf = 0.95, deff = 1, N = Inf){
  n0 <- (z_alpha(conf)*sd/E)^2
  n0 <- n0 * deff
  if(is.finite(N)) (n0*N)/(n0 + N - 1) else n0
}

# Para la proporción (si no sabés p, usar p=0.5 es conservador)
n_prop <- function(E, p = 0.5, conf = 0.95, deff = 1, N = Inf){
  n0 <- (z_alpha(conf)^2 * p*(1-p)) / (E^2)
  n0 <- n0 * deff
  if(is.finite(N)) (n0*N)/(n0 + N - 1) else n0
}

# Ejemplos (modificá E, deff y si querés usar FPC con Npop)
n_media_ej <- ceiling(n_media(E = 0.05, sd = sd_ppr, conf = 0.95, deff = 1.3, N = Npop))
n_prop_ej  <- ceiling(n_prop(E = 0.015, p = p_verdadera, conf = 0.95, deff = 1.3, N = Npop))
cat("Ejemplo n para MEDIA (PPR):", n_media_ej, "\n")
cat("Ejemplo n para PROP (Internet):", n_prop_ej, "\n\n")


##------------------------------------------------------------------------------
## 9) TEOREMA CENTRAL DEL LÍMITE (TCL): simulación simple ----------------------
##------------------------------------------------------------------------------

# Objetivo: ver cómo se distribuyen promedios muestrales al aumentar n

B  <- 2000                 # número de repeticiones Monte Carlo
ns <- c(15, 50, 300)       # tamaños muestrales a comparar

# 9.1 Distribución muestral de la media (PPR)
sim_mu <- tibble()
for(n_i in ns){
  medias <- numeric(B)
  for(b in 1:B){
    s <- pobl %>% slice_sample(n = n_i, replace = FALSE)
    medias[b] <- mean(s$ppr)
  }
  sim_mu <- bind_rows(sim_mu, tibble(xbar = medias, n = n_i))
}

# Curvas normales teóricas para comparar (una por cada n)
make_normal_curve <- function(mu, sd, n, k = 4, points = 200){
  x <- seq(mu - k*sd/sqrt(n), mu + k*sd/sqrt(n), length.out = points)
  tibble(x = x, dens = dnorm(x, mean = mu, sd = sd/sqrt(n)), n = n)
}
curvas_mu <- bind_rows(lapply(ns, function(n_i) make_normal_curve(mu_verdadera, sd_ppr, n_i)))

ggplot(sim_mu, aes(x = xbar)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40) +
  geom_line(data = curvas_mu, aes(x = x, y = dens), linewidth = 0.8) +
  facet_wrap(~ n, scales = "free_y") +
  labs(title = "TCL: distribución muestral de la MEDIA (PPR) bajo SRS",
       x = "Media muestral", y = "Densidad")

# 9.2 Distribución muestral de la proporción (Internet)
sim_p <- tibble()
for(n_i in ns){
  props <- numeric(B)
  for(b in 1:B){
    s <- pobl %>% slice_sample(n = n_i, replace = FALSE)
    props[b] <- mean(s$internet)
  }
  sim_p <- bind_rows(sim_p, tibble(phat = props, n = n_i))
}

# Curvas normales teóricas (varianza aproximada p(1-p)/n)
make_normal_curve_p <- function(p, n, k = 4, points = 200){
  sd_p <- sqrt(p*(1-p)/n)
  x <- seq(p - k*sd_p, p + k*sd_p, length.out = points)
  tibble(x = x, dens = dnorm(x, mean = p, sd = sd_p), n = n)
}
curvas_p <- bind_rows(lapply(ns, function(n_i) make_normal_curve_p(p_verdadera, n_i)))

ggplot(sim_p, aes(x = phat)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40) +
  geom_line(data = curvas_p, aes(x = x, y = dens), linewidth = 0.8) +
  facet_wrap(~ n, scales = "free_y") +
  scale_x_continuous(labels = percent) +
  labs(title = "TCL: distribución muestral de la PROPORCIÓN (Internet) bajo SRS",
       x = "Proporción muestral", y = "Densidad")


##------------------------------------------------------------------------------
## 10) GUIA DE ACTIVIDADES (para entregar) -------------------------------------
##------------------------------------------------------------------------------

#  a) Discutir cuándo/por qué el estratificado mejora precisión.
#  b) Mostrar el efecto de la FPC (con vs sin FPC) en SRS.
#  c) Calcular tamaños muestrales para objetivos de error E en media y proporción,
#     variando el deff (1; 1.3; 1.6) y usando/no usando FPC.
#  d) Analizar los dos gráficos del TCL y explicar qué cambia al aumentar n.


############################################################














