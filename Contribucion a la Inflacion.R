# =============================================================================
#  Dashboard IPC Bolivia - Versión con JSON externo y plantilla HTML separada
#  Genera:
#    - dashboard_data.json (datos ligeros, solo top contribuciones)
#    - dashboard.html      (interfaz que carga el JSON, a partir de template.html)
# =============================================================================

library(readxl)
library(jsonlite)
library(dplyr)
library(lubridate)
library(stringr)

# -- Rutas (ajústalas a tu entorno) -----------------------------------------
path_ipc  <- "D:/Usuario/Desktop/Inflación/IPC producto.xlsx"
path_pond <- "D:/Usuario/Desktop/Inflación/Ponderaciones IPC.xlsx"
dir_out   <- "D:/Usuario/Desktop/Inflación/Contribuciones por producto a la inflacion/"

# Ruta de tu plantilla HTML (puede ser absoluta o relativa)
template_file <- file.path(dir_out, "template.html")  # Ajusta si está en otra carpeta

# -- Parámetros de recorte -------------------------------------------------
TOP_GENERAL <- 50   # productos más influyentes en IPC General
TOP_DIV      <- 30   # productos más influyentes dentro de cada división

# Asegurar directorio de salida
if(!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)

# -- Lectura ----------------------------------------------------------------
cat("Leyendo archivos...\n")
df_ipc  <- read_excel(path_ipc)
df_pond <- read_excel(path_pond)

clean_cod <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "[\u00a0\\s]", "")
  ifelse(x=="NA"|is.na(x)|x=="", "GENERAL", x)
}
df_ipc$CODIGO  <- clean_cod(df_ipc$CODIGO)
df_pond$CODIGO <- clean_cod(df_pond$CODIGO)

date_cols <- setdiff(names(df_ipc), c("CODIGO","DESCRIPCION"))
cat("Periodos:", length(date_cols), "(", date_cols[1], "->", tail(date_cols,1), ")\n")

# -- Fechas -----------------------------------------------------------------
month_map <- c(ene=1,feb=2,mar=3,abr=4,may=5,jun=6,jul=7,ago=8,sep=9,sept=9,oct=10,nov=11,dic=12)
parse_date <- function(col) {
  p <- str_split(col, "-")[[1]]
  mo <- month_map[tolower(p[1])]
  yr <- as.integer(p[2]); if (yr < 100) yr <- 2000L + yr
  make_date(yr, mo, 1L)
}
dates       <- do.call(c, lapply(date_cols, parse_date))
date_labels <- format(dates, "%b-%Y") %>% str_to_title() %>% str_remove("\\.")

# -- Divisiones -------------------------------------------------------------
divisions <- list(
  "01"="Alimentos y Bebidas No Alcohólicas",
  "02"="Bebidas Alcohólicas y Tabaco",
  "03"="Prendas de Vestir y Calzados",
  "04"="Vivienda y Servicios Básicos",
  "05"="Muebles, Bienes y Servicios Dom.",
  "06"="Salud",
  "07"="Transporte",
  "08"="Comunicaciones",
  "09"="Recreación y Cultura",
  "10"="Educación",
  "11"="Alimentos y Bebidas Fuera del Hogar",
  "12"="Bienes y Servicios Diversos"
)

# -- IPC General ------------------------------------------------------------
gen_row    <- df_ipc[df_ipc$CODIGO == "GENERAL", , drop=FALSE]
ipc_gen_v  <- as.numeric(gen_row[1, date_cols])
ipc_general <- setNames(as.list(round(ipc_gen_v, 6)), date_labels)

# -- Productos --------------------------------------------------------------
prods_ipc  <- df_ipc[df_ipc$CODIGO != "GENERAL", , drop=FALSE]
prods_pond <- df_pond[df_pond$CODIGO != "GENERAL", c("CODIGO","PONDERADOR"), drop=FALSE]
prods_ipc$div <- substr(prods_ipc$CODIGO, 1, 2)
prods <- merge(prods_ipc, prods_pond, by="CODIGO", all.x=TRUE)

# -- IPC por división -------------------------------------------------------
cat("Calculando IPC por division...\n")
div_ipc <- list()
div_products <- list()

for (code in names(divisions)) {
  sub <- prods[prods$div == code, , drop = FALSE]
  sub$w_norm <- sub$PONDERADOR / sum(sub$PONDERADOR, na.rm = TRUE) * 100
  iv <- sapply(date_cols, function(dc) {
    round(sum(as.numeric(sub[[dc]]) * sub$w_norm / 100, na.rm = TRUE), 6)
  })
  div_ipc[[code]] <- setNames(as.list(iv), date_labels)
  
  div_products[[code]] <- lapply(seq_len(nrow(sub)), function(j) {
    row <- sub[j, , drop = FALSE]
    list(
      codigo = row$CODIGO,
      descripcion = row$DESCRIPCION,
      ponderador_global = as.numeric(row$PONDERADOR),
      ponderador_div    = as.numeric(row$w_norm),
      ipc = setNames(as.list(round(as.numeric(row[, date_cols]), 6)), date_labels)
    )
  })
}

# -- Inflaciones ------------------------------------------------------------
calc_inf <- function(ipc_s, labels, dts) {
  res <- vector("list", length(labels))
  for (i in seq_along(labels)) {
    val <- ipc_s[[labels[i]]]
    d   <- dts[i]
    men <- if (i > 1) round((val / ipc_s[[labels[i - 1]]] - 1) * 100, 4) else NULL
    if (month(d) == 1L) {
      acu <- men
    } else {
      bi <- NA_integer_
      for (j in (i - 1):1) {
        if (month(dts[j]) == 12L && year(dts[j]) == year(d) - 1L) {
          bi <- j
          break
        }
      }
      acu <- if (!is.na(bi)) round((val / ipc_s[[labels[bi]]] - 1) * 100, 4) else NULL
    }
    a12 <- if (i > 12L) round((val / ipc_s[[labels[i - 12L]]] - 1) * 100, 4) else NULL
    res[[i]] <- list(mensual = men, acumulada = acu, a12m = a12)
  }
  setNames(res, labels)
}
cat("Calculando inflaciones...\n")
inf_general <- calc_inf(ipc_general, date_labels, dates)
inf_div     <- lapply(div_ipc, calc_inf, labels = date_labels, dts = dates)

# -- Función base ----------------------------------------------------------
get_bi <- function(measure, i, d) {
  if (measure == "mensual") {
    return(if (i > 1L) i - 1L else NA_integer_)
  }
  if (measure == "acumulada") {
    if (month(d) == 1L) return(if (i > 1L) i - 1L else NA_integer_)
    for (j in (i - 1L):1L) {
      if (month(dates[j]) == 12L && year(dates[j]) == year(d) - 1L) return(j)
    }
    return(NA_integer_)
  }
  if (i > 12L) i - 12L else NA_integer_
}

safe_num <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0L) return(NA_real_)
  x[1]
}

# -- Contribuciones por división (top TOP_DIV) ------------------------------
cat("Calculando contribuciones por division (top", TOP_DIV, "productos)...\n")
contrib_data <- list()

for (code in names(divisions)) {
  contrib_data[[code]] <- list()
  for (measure in c("mensual", "acumulada", "a12m")) {
    contrib_data[[code]][[measure]] <- list()
    for (i in seq_along(date_labels)) {
      lbl <- date_labels[i]
      bi  <- get_bi(measure, i, dates[i])
      if (is.na(bi)) next
      blbl <- date_labels[bi]
      ipc_div_b <- safe_num(div_ipc[[code]][[blbl]])
      ipc_gen_b <- safe_num(ipc_general[[blbl]])
      
      ents <- lapply(div_products[[code]], function(p) {
        it <- safe_num(p$ipc[[lbl]])
        ib <- safe_num(p$ipc[[blbl]])
        cd <- if (!is.na(it) && !is.na(ib) && !is.na(ipc_div_b) && ipc_div_b != 0 && !is.na(p$ponderador_div)) {
          round((p$ponderador_div / 100) * ((it - ib) / ipc_div_b) * 100, 4)
        } else 0
        cg <- if (!is.na(it) && !is.na(ib) && !is.na(ipc_gen_b) && ipc_gen_b != 0 && !is.na(p$ponderador_global)) {
          round((p$ponderador_global / 100) * ((it - ib) / ipc_gen_b) * 100, 4)
        } else 0
        list(
          codigo = p$codigo,
          descripcion = p$descripcion,
          pond_div = p$ponderador_div,
          pond_global = p$ponderador_global,
          contrib_div = cd,
          contrib_gen = cg
        )
      })
      ord <- order(sapply(ents, function(e) abs(e$contrib_div)), decreasing = TRUE)
      contrib_data[[code]][[measure]][[lbl]] <- ents[ord][1:min(TOP_DIV, length(ents))]
    }
  }
}

# -- Contribuciones generales (top TOP_GENERAL) -----------------------------
cat("Calculando contribuciones generales (top", TOP_GENERAL, "productos)...\n")
contrib_general <- list()
for (measure in c("mensual", "acumulada", "a12m")) {
  contrib_general[[measure]] <- list()
  for (i in seq_along(date_labels)) {
    lbl <- date_labels[i]
    bi  <- get_bi(measure, i, dates[i])
    if (is.na(bi)) next
    blbl <- date_labels[bi]
    ipc_gen_b <- safe_num(ipc_general[[blbl]])
    all_c <- list()
    for (code in names(divisions)) {
      for (p in div_products[[code]]) {
        it <- safe_num(p$ipc[[lbl]])
        ib <- safe_num(p$ipc[[blbl]])
        cg <- if (!is.na(it) && !is.na(ib) && !is.na(ipc_gen_b) && ipc_gen_b != 0 && !is.na(p$ponderador_global)) {
          round((p$ponderador_global / 100) * ((it - ib) / ipc_gen_b) * 100, 4)
        } else 0
        all_c <- c(
          all_c,
          list(list(
            codigo = p$codigo,
            descripcion = p$descripcion,
            div_code = code,
            div_name = divisions[[code]],
            pond_global = p$ponderador_global,
            contrib_gen = cg
          ))
        )
      }
    }
    ord <- order(sapply(all_c, function(e) abs(e$contrib_gen)), decreasing = TRUE)
    contrib_general[[measure]][[lbl]] <- all_c[ord][1:min(TOP_GENERAL, length(all_c))]
  }
}

# -- Construir objeto final ------------------------------------------------
data_out <- list(
  date_labels = date_labels,
  divisions = divisions,
  ipc_general = ipc_general,
  inf_general = inf_general,
  inf_div = inf_div,
  contrib_data = contrib_data,
  contrib_general = contrib_general
)

# -- Guardar JSON externo --------------------------------------------------
json_file <- file.path(dir_out, "dashboard_data.json")
cat("Serializando JSON a", json_file, "...\n")
json_raw  <- toJSON(data_out, auto_unbox = TRUE, null = "null", na = "null", digits = 3)
writeLines(json_raw, json_file, useBytes = TRUE)
cat("  Tamaño JSON:", round(file.size(json_file)/1024), "KB\n")

# ==========================================================================
#  GENERAR HTML A PARTIR DE PLANTILLA EXTERNA
# ==========================================================================
html_output <- file.path(dir_out, "dashboard_contribucion_ipc.html")

if (file.exists(template_file)) {
  cat("Leyendo plantilla HTML desde", template_file, "...\n")
  html_lines <- readLines(template_file, warn = FALSE, encoding = "UTF-8")
  html_content <- paste(html_lines, collapse = "\n")
  
  # CORRECCIÓN AUTOMÁTICA de comillas dobles anidadas en eventos onclick
  # Convierte onclick="switchTab("general")" a onclick="switchTab('general')"
  html_content <- gsub('onclick="([a-zA-Z]+)\\(\\"([^\\"]+)\\"\\)"', 
                       'onclick="\\1(\'\\2\')"', 
                       html_content)
  # También para funciones con un argumento como setMeasure("mensual")
  html_content <- gsub('onclick="([a-zA-Z]+)\\(\\"([^\\"]+)\\"\\)"', 
                       'onclick="\\1(\'\\2\')"', 
                       html_content)
  
  # Opcional: Puedes añadir un marcador de fecha si tu plantilla lo usa
  # html_content <- gsub("\\{\\{FECHA_GENERACION\\}\\}", Sys.time(), html_content)
  
  # Escribir el archivo final
  writeLines(html_content, html_output, useBytes = TRUE)
  cat("HTML generado exitosamente en", html_output, "\n")
  cat("  Tamaño HTML:", round(file.size(html_output)/1024), "KB\n")
} else {
  cat("ERROR: No se encontró el archivo", template_file, "\n")
  cat("Asegúrate de que el archivo template.html esté en el directorio de trabajo.\n")
  stop("No se pudo generar el HTML porque falta la plantilla.")
}

cat("\n=== Proceso completado ===\n")
cat("Archivos generados:\n")
cat("  -", json_file, "\n")
cat("  -", html_output, "\n")
cat("\nAbre", html_output, "en tu navegador (ambos archivos deben estar en la misma carpeta).\n")