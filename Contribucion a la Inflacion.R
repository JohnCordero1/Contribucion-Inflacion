# =============================================================================
#  Dashboard IPC Bolivia
#  Uso: Rscript generar_dashboard_ipc.R  (o source() desde RStudio)
#  Requisitos: readxl, jsonlite, dplyr, lubridate, stringr
#  Entrada: IPC_producto.xlsx + Ponderaciones_IPC.xlsx (mismo directorio)
#  Salida:  dashboard_ipc.html
# =============================================================================

# -- 0. Paquetes ---------------------------------------------------------------
library(readxl)
library(jsonlite)
library(dplyr)
library(lubridate)
library(stringr)

# ── Rutas de archivos ─────────────────────────────────────────────

path_ipc  <- "D:/Usuario/Desktop/Inflación/IPC producto.xlsx"
path_pond <- "D:/Usuario/Desktop/Inflación/Ponderaciones IPC.xlsx"
path_out  <- "dashboard_ipc.html"

# ── 2. Lectura ───────────────────────────────────────────────────────
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

# -- 3. Fechas -----------------------------------------------------------------
month_map <- c(ene=1,feb=2,mar=3,abr=4,may=5,jun=6,jul=7,ago=8,sep=9,sept=9,oct=10,nov=11,dic=12)
parse_date <- function(col) {
  p <- str_split(col, "-")[[1]]
  mo <- month_map[tolower(p[1])]
  yr <- as.integer(p[2]); if (yr < 100) yr <- 2000L + yr
  make_date(yr, mo, 1L)
}
dates       <- do.call(c, lapply(date_cols, parse_date))
date_labels <- format(dates, "%b-%Y") %>% str_to_title() %>% str_remove("\\.")

# -- 4. Divisiones ------------------------------------------------------------
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

# -- 5. IPC General -----------------------------------------------------------
gen_row    <- df_ipc[df_ipc$CODIGO == "GENERAL", , drop=FALSE]
ipc_gen_v  <- as.numeric(gen_row[1, date_cols])
ipc_general <- setNames(as.list(round(ipc_gen_v, 6)), date_labels)

# -- 6. Productos -------------------------------------------------------------
prods_ipc  <- df_ipc[df_ipc$CODIGO != "GENERAL", , drop=FALSE]
prods_pond <- df_pond[df_pond$CODIGO != "GENERAL", c("CODIGO","PONDERADOR"), drop=FALSE]
prods_ipc$div <- substr(prods_ipc$CODIGO, 1, 2)
prods <- merge(prods_ipc, prods_pond, by="CODIGO", all.x=TRUE)

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

# -- 7. Inflacion --------------------------------------------------------------
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

# -- 8. Base para contribuciones ----------------------------------------------
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

cat("Calculando contribuciones por division...\n")
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
      # Guardar TODOS los productos. El recorte Top-N debe hacerse solo en la vista HTML.
      contrib_data[[code]][[measure]][[lbl]] <- ents[ord]
    }
  }
}

cat("Calculando contribuciones generales...\n")
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
    # Guardar TODOS los productos. El recorte Top-N debe hacerse solo en la vista HTML.
    contrib_general[[measure]][[lbl]] <- all_c[ord]
  }
}

# -- 9. JSON ------------------------------------------------------------------
cat("Serializando datos...\n")
data_out <- list(
  date_labels = date_labels,
  divisions = divisions,
  ipc_general = ipc_general,
  inf_general = inf_general,
  inf_div = inf_div,
  contrib_data = contrib_data,
  contrib_general = contrib_general
)

json_raw  <- toJSON(data_out, auto_unbox = TRUE, null = "null", na = "null", digits = 3)
json_safe <- gsub("<\\/script>", "<\\/script>", json_raw, fixed = TRUE)

cat("  JSON:", round(nchar(json_raw) / 1024), "KB\n")

# -- 10. Escribir HTML -------------------------------------------------------
cat("Generando HTML...\n")
f <- file(path_out, open="w", encoding="UTF-8")

cat("<!DOCTYPE html>\n", file=f)
cat("<html lang=\"es\">\n", file=f)
cat("<head>\n", file=f)
cat("<meta charset=\"UTF-8\">\n", file=f)
cat("<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n", file=f)
cat("<title>Dashboard IPC Bolivia</title>\n", file=f)
cat("<script src=\"https://cdnjs.cloudflare.com/ajax/libs/Chart.js/4.4.1/chart.umd.min.js\"></script>\n", file=f)
cat("<style>\n", file=f)
cat("@import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@300;400;500;600&family=IBM+Plex+Mono:wght@400;500&display=swap');\n", file=f)
cat(":root{--bg:#0d1117;--bg2:#161b22;--bg3:#21262d;--border:#30363d;--text:#e6edf3;--text2:#8b949e;\n", file=f)
cat("  --accent:#0054a6;--accent2:#3fb950;--warn:#f85149;--gold:#d29922;\n", file=f)
cat("  --font:\"IBM Plex Sans\",sans-serif;--mono:\"IBM Plex Mono\",monospace;}\n", file=f)
cat("*{box-sizing:border-box;margin:0;padding:0}\n", file=f)
cat("body{background:var(--bg);color:var(--text);font-family:var(--font);font-size:14px;min-height:100vh}\n", file=f)
cat("header{background:var(--bg2);border-bottom:1px solid var(--border);padding:16px 24px;display:flex;align-items:center;gap:16px;position:sticky;top:0;z-index:100}\n", file=f)
cat(".logo{font-family:var(--mono);font-size:11px;color:var(--accent);letter-spacing:.12em;text-transform:uppercase;border:1px solid var(--accent);padding:3px 8px;border-radius:3px;white-space:nowrap}\n", file=f)
cat("header h1{font-size:15px;font-weight:500}\n", file=f)
cat(".subtitle{font-size:12px;color:var(--text2);margin-left:auto;white-space:nowrap}\n", file=f)
cat(".container{max-width:1400px;margin:0 auto;padding:24px}\n", file=f)
cat(".tabs{display:flex;gap:2px;background:var(--bg2);border:1px solid var(--border);border-radius:8px;padding:4px;margin-bottom:24px;overflow-x:auto}\n", file=f)
cat(".tab{padding:7px 14px;border-radius:5px;cursor:pointer;font-size:13px;color:var(--text2);white-space:nowrap;transition:all .15s;border:none;background:none}\n", file=f)
cat(".tab:hover{color:var(--text);background:var(--bg3)}.tab.active{background:var(--accent);color:#fff;font-weight:500}\n", file=f)
cat(".tab-panel{display:none}.tab-panel.active{display:block}\n", file=f)
cat(".controls{display:flex;gap:12px;align-items:center;flex-wrap:wrap;margin-bottom:20px;padding:14px 16px;background:var(--bg2);border:1px solid var(--border);border-radius:8px}\n", file=f)
cat(".controls label{font-size:12px;color:var(--text2);text-transform:uppercase;letter-spacing:.05em;white-space:nowrap}\n", file=f)
cat("select{background:var(--bg3);border:1px solid var(--border);color:var(--text);padding:6px 10px;border-radius:5px;font-size:13px;font-family:var(--font);cursor:pointer;outline:none}\n", file=f)
cat("select:focus{border-color:var(--accent)}\n", file=f)
cat(".measure-btns{display:flex;gap:4px}\n", file=f)
cat(".mbtn{padding:5px 12px;border-radius:4px;border:1px solid var(--border);background:var(--bg3);color:var(--text2);font-size:12px;cursor:pointer;transition:all .15s;font-family:var(--font)}\n", file=f)
cat(".mbtn:hover{border-color:var(--accent);color:var(--accent)}.mbtn.active{background:var(--accent);border-color:var(--accent);color:#fff}\n", file=f)
cat(".kpi-row{display:grid;grid-template-columns:repeat(auto-fit,minmax(160px,1fr));gap:12px;margin-bottom:20px}\n", file=f)
cat(".kpi{background:var(--bg2);border:1px solid var(--border);border-radius:8px;padding:14px 16px;position:relative;overflow:hidden}\n", file=f)
cat(".kpi::before{content:\"\";position:absolute;top:0;left:0;right:0;height:2px}\n", file=f)
cat(".kpi.blue::before{background:var(--accent)}.kpi.green::before{background:var(--accent2)}.kpi.red::before{background:var(--warn)}\n", file=f)
cat(".kpi-label{font-size:11px;color:var(--text2);text-transform:uppercase;letter-spacing:.07em;margin-bottom:6px}\n", file=f)
cat(".kpi-val{font-family:var(--mono);font-size:24px;font-weight:500;line-height:1}\n", file=f)
cat(".kpi-val.pos{color:var(--warn)}.kpi-val.neg{color:var(--accent2)}.kpi-val.neu{color:var(--text)}\n", file=f)
cat(".kpi-sub{font-size:11px;color:var(--text2);margin-top:4px}\n", file=f)
cat(".card{background:var(--bg2);border:1px solid var(--border);border-radius:8px;padding:20px;margin-bottom:16px}\n", file=f)
cat(".card-title{font-size:12px;text-transform:uppercase;letter-spacing:.07em;color:var(--text2);margin-bottom:16px;display:flex;align-items:center;gap:8px}\n", file=f)
cat(".card-title .bar{display:inline-block;width:3px;height:14px;border-radius:2px;background:var(--accent)}\n", file=f)
cat("canvas{max-height:320px}\n", file=f)
cat(".grid2{display:grid;grid-template-columns:1fr 1fr;gap:16px}\n", file=f)
cat("@media(max-width:900px){.grid2{grid-template-columns:1fr}}\n", file=f)
cat(".contrib-wrap{overflow-x:auto}\n", file=f)
cat("table{width:100%;border-collapse:collapse;font-size:13px}\n", file=f)
cat("thead th{background:var(--bg3);padding:8px 12px;text-align:left;font-size:11px;text-transform:uppercase;letter-spacing:.06em;color:var(--text2);border-bottom:1px solid var(--border);white-space:nowrap}\n", file=f)
cat("tbody tr{border-bottom:1px solid #1c2028;transition:background .1s}tbody tr:hover{background:var(--bg3)}\n", file=f)
cat("tbody td{padding:7px 12px;color:var(--text)}tbody td.num{font-family:var(--mono);font-size:12px;text-align:right;white-space:nowrap}\n", file=f)
cat(".contrib-bar{display:inline-block;height:8px;border-radius:2px;vertical-align:middle;margin-right:6px;min-width:2px}\n", file=f)
cat(".heatmap-wrap{overflow-x:auto}\n", file=f)
cat(".hm-table{border-collapse:collapse;font-size:11px;white-space:nowrap}\n", file=f)
cat(".hm-table th{padding:5px 8px;background:var(--bg3);color:var(--text2);font-weight:500;border:1px solid var(--border);text-align:center}\n", file=f)
cat(".hm-table td{padding:5px 8px;border:1px solid var(--border);text-align:center;font-family:var(--mono);font-size:11px;cursor:default}\n", file=f)
cat(".hm-legend{display:flex;gap:16px;flex-wrap:wrap;font-size:11px;color:var(--text2);margin-bottom:12px}\n", file=f)
cat(".hm-legend span{display:inline-flex;align-items:center;gap:5px}\n", file=f)
cat(".hm-swatch{display:inline-block;width:12px;height:12px;border-radius:2px}\n", file=f)
cat("input[type=text]{background:var(--bg3);border:1px solid var(--border);color:var(--text);padding:6px 10px;border-radius:5px;font-size:13px;font-family:var(--font);outline:none;width:220px}\n", file=f)
cat("input[type=text]:focus{border-color:var(--accent)}\n", file=f)
cat(".badge{display:inline-block;padding:2px 7px;border-radius:10px;font-size:10px;font-family:var(--mono);font-weight:500}\n", file=f)
cat(".badge.pos{background:rgba(248,81,73,.15);color:var(--warn)}.badge.neg{background:rgba(63,185,80,.15);color:var(--accent2)}.badge.neu{background:var(--bg3);color:var(--text2)}\n", file=f)
cat("#loading{position:fixed;inset:0;background:var(--bg);display:flex;flex-direction:column;align-items:center;justify-content:center;gap:12px;z-index:999}\n", file=f)
cat(".spinner{width:36px;height:36px;border:3px solid var(--bg3);border-top-color:var(--accent);border-radius:50%;animation:spin .7s linear infinite}\n", file=f)
cat("@keyframes spin{to{transform:rotate(360deg)}}\n", file=f)
cat("#loading p{color:var(--text2);font-size:13px}\n", file=f)
cat("</style>\n", file=f)
cat("</head><body>\n", file=f)
cat("<div id=\"loading\"><div class=\"spinner\"></div><p>Cargando datos IPC...</p></div>\n", file=f)
cat("<header><div class=\"logo\">Fuente &middot; INE Bolivia</div><h1>Dashboard de la Inflación para Bolivia</h1><div class=\"subtitle\" id=\"hdr-range\"></div></header>\n", file=f)
cat("<div class=\"container\">\n", file=f)
cat("<div class=\"tabs\">\n", file=f)
cat("  <button class=\"tab active\" onclick=\"switchTab('general')\">IPC General</button>\n", file=f)
cat("  <button class=\"tab\" onclick=\"switchTab('divisiones')\">Por División</button>\n", file=f)
cat("  <button class=\"tab\" onclick=\"switchTab('heatmap')\">Heatmap Divisiones</button>\n", file=f)
cat("  <button class=\"tab\" onclick=\"switchTab('contrib_gen')\">Contribuciones al IPC</button>\n", file=f)
cat("  <button class=\"tab\" onclick=\"switchTab('contrib_div')\">Contribuciones por División</button>\n", file=f)
cat("</div>\n", file=f)
cat("<div class=\"tab-panel active\" id=\"tab-general\">\n", file=f)
cat("<div class=\"controls\"><label>Periodo</label><select id=\"gen-from\" onchange=\"updateGeneral()\"></select>\n", file=f)
cat("  <span style=\"color:var(--text2)\">&#8594;</span><select id=\"gen-to\" onchange=\"updateGeneral()\"></select>\n", file=f)
cat("  <div class=\"measure-btns\">\n", file=f)
cat("    <button class=\"mbtn active\" id=\"mbtn-mensual\" onclick=\"setMeasure('mensual')\">Mensual</button>\n", file=f)
cat("    <button class=\"mbtn\" id=\"mbtn-acumulada\" onclick=\"setMeasure('acumulada')\">Acumulada</button>\n", file=f)
cat("    <button class=\"mbtn\" id=\"mbtn-a12m\" onclick=\"setMeasure('a12m')\">12 meses</button>\n", file=f)
cat("  </div></div>\n", file=f)
cat("<div class=\"kpi-row\" id=\"gen-kpis\"></div>\n", file=f)
cat("<div class=\"card\"><div class=\"card-title\"><span class=\"bar\"></span>Inflación General &mdash; <em id=\"gen-chart-title\" style=\"color:var(--accent);font-style:normal\"></em></div><canvas id=\"chart-gen-line\"></canvas></div>\n", file=f)
cat("<div class=\"grid2\">\n", file=f)
cat("  <div class=\"card\"><div class=\"card-title\"><span class=\"bar\"></span>Nivel del Indice de Precios al Consumidor</div><canvas id=\"chart-gen-level\"></canvas></div>\n", file=f)
cat("  <div class=\"card\"><div class=\"card-title\"><span class=\"bar\"></span>Comparativa por División (último periodo)</div><canvas id=\"chart-div-bar\"></canvas></div>\n", file=f)
cat("</div></div>\n", file=f)
cat("<div class=\"tab-panel\" id=\"tab-divisiones\">\n", file=f)
cat("<div class=\"controls\"><label>División</label><select id=\"sel-div\" onchange=\"updateDivision()\"></select>\n", file=f)
cat("  <label>Periodo</label><select id=\"div-from\" onchange=\"updateDivision()\"></select>\n", file=f)
cat("  <span style=\"color:var(--text2)\">&#8594;</span><select id=\"div-to\" onchange=\"updateDivision()\"></select>\n", file=f)
cat("  <div class=\"measure-btns\">\n", file=f)
cat("    <button class=\"mbtn active\" id=\"dmbtn-mensual\" onclick=\"setDivMeasure('mensual')\">Mensual</button>\n", file=f)
cat("    <button class=\"mbtn\" id=\"dmbtn-acumulada\" onclick=\"setDivMeasure('acumulada')\">Acumulada</button>\n", file=f)
cat("    <button class=\"mbtn\" id=\"dmbtn-a12m\" onclick=\"setDivMeasure('a12m')\">12 meses</button>\n", file=f)
cat("  </div></div>\n", file=f)
cat("<div class=\"kpi-row\" id=\"div-kpis\"></div>\n", file=f)
cat("<div class=\"card\"><div class=\"card-title\"><span class=\"bar\"></span>Inflación División vs General</div><canvas id=\"chart-div-line\"></canvas></div>\n", file=f)
cat("</div>\n", file=f)
cat("<div class=\"tab-panel\" id=\"tab-heatmap\">\n", file=f)
cat("<div class=\"controls\"><label>Medida</label><div class=\"measure-btns\">\n", file=f)
cat("    <button class=\"mbtn active\" id=\"hmbtn-mensual\" onclick=\"setHmMeasure('mensual')\">Mensual</button>\n", file=f)
cat("    <button class=\"mbtn\" id=\"hmbtn-acumulada\" onclick=\"setHmMeasure('acumulada')\">Acumulada</button>\n", file=f)
cat("    <button class=\"mbtn\" id=\"hmbtn-a12m\" onclick=\"setHmMeasure('a12m')\">12 meses</button>\n", file=f)
cat("  </div><label>Año</label><select id=\"hm-year\" onchange=\"renderHeatmap()\"></select></div>\n", file=f)
cat("<div class=\"card\"><div class=\"card-title\"><span class=\"bar\"></span>Inflación por División y Mes</div>\n", file=f)
cat("<div class=\"hm-legend\">\n", file=f)
cat("  <span><span class=\"hm-swatch\" style=\"background:#f85149\"></span> Inflación (rojo)</span>\n", file=f)
cat("  <span><span class=\"hm-swatch\" style=\"background:#3fb950\"></span> Deflación (verde)</span>\n", file=f)
cat("  <span><span class=\"hm-swatch\" style=\"background:#21262d;border:1px solid #30363d\"></span> Sin dato</span>\n", file=f)
cat("  <span>Intensidad: 0% = gris, >=8 pp = color pleno</span>\n", file=f)
cat("</div>\n", file=f)
cat("<div class=\"heatmap-wrap\" id=\"hm-container\"></div></div></div>\n", file=f)
cat("<div class=\"tab-panel\" id=\"tab-contrib_gen\">\n", file=f)
cat("<div class=\"controls\"><label>Periodo</label><select id=\"cg-period\" onchange=\"renderContribGeneral()\"></select>\n", file=f)
cat("  <div class=\"measure-btns\">\n", file=f)
cat("    <button class=\"mbtn active\" id=\"cgmbtn-mensual\" onclick=\"setCgMeasure('mensual')\">Mensual</button>\n", file=f)
cat("    <button class=\"mbtn\" id=\"cgmbtn-acumulada\" onclick=\"setCgMeasure('acumulada')\">Acumulada</button>\n", file=f)
cat("    <button class=\"mbtn\" id=\"cgmbtn-a12m\" onclick=\"setCgMeasure('a12m')\">12 meses</button>\n", file=f)
cat("  </div><input type=\"text\" id=\"cg-search\" placeholder=\"Buscar producto...\" oninput=\"renderContribGeneral()\"></div>\n", file=f)
cat("<div class=\"kpi-row\" id=\"cg-kpis\"></div>\n", file=f)
cat("<div class=\"card\"><div class=\"card-title\"><span class=\"bar\"></span>Top contribuyentes a la inflación general</div>\n", file=f)
cat("<div class=\"contrib-wrap\" id=\"cg-table\"></div></div>\n", file=f)
cat("<div class=\"card\"><div class=\"card-title\"><span class=\"bar\"></span>Descomposición de la inflación general por división</div>\n", file=f)
cat("<canvas id=\"chart-cg-stacked\"></canvas></div></div>\n", file=f)
cat("<div class=\"tab-panel\" id=\"tab-contrib_div\">\n", file=f)
cat("<div class=\"controls\"><label>Division</label><select id=\"cd-div\" onchange=\"renderContribDiv()\"></select>\n", file=f)
cat("  <label>Periodo</label><select id=\"cd-period\" onchange=\"renderContribDiv()\"></select>\n", file=f)
cat("  <div class=\"measure-btns\">\n", file=f)
cat("    <button class=\"mbtn active\" id=\"cdmbtn-mensual\" onclick=\"setCdMeasure('mensual')\">Mensual</button>\n", file=f)
cat("    <button class=\"mbtn\" id=\"cdmbtn-acumulada\" onclick=\"setCdMeasure('acumulada')\">Acumulada</button>\n", file=f)
cat("    <button class=\"mbtn\" id=\"cdmbtn-a12m\" onclick=\"setCdMeasure('a12m')\">12 meses</button>\n", file=f)
cat("  </div><input type=\"text\" id=\"cd-search\" placeholder=\"Buscar producto...\" oninput=\"renderContribDiv()\"></div>\n", file=f)
cat("<div class=\"kpi-row\" id=\"cd-kpis\"></div>\n", file=f)
cat("<div class=\"card\"><div class=\"card-title\"><span class=\"bar\"></span>Contribución de productos a la inflación de la división</div>\n", file=f)
cat("<div class=\"contrib-wrap\" id=\"cd-table\"></div></div></div>\n", file=f)
cat("</div>\n", file=f)

# Inject JSON data
cat("<script>\n", file=f)
cat("const RAW = ", file=f)
cat(json_safe, file=f)
cat(";\n", file=f)
# JS logic
cat("\n", file=f)
cat("const S={genMeasure:\"mensual\",divMeasure:\"mensual\",hmMeasure:\"mensual\",cgMeasure:\"mensual\",cdMeasure:\"mensual\"};\n", file=f)
cat("const DIV_COLORS=[\"#0054a6\",\"#3fb950\",\"#f85149\",\"#d29922\",\"#39d0d8\",\"#bc8cff\",\"#ff7b72\",\"#79c0ff\",\"#56d364\",\"#ffa657\",\"#ff6eb4\",\"#a8daff\"];\n", file=f)
cat("let charts={};\n", file=f)
cat("function destroyChart(id){if(charts[id]){charts[id].destroy();delete charts[id];}}\n", file=f)
cat("function fmtPct(v){if(v===null||v===undefined)return\"--\";return(v>0?\"+\":\"\")+v.toFixed(2)+\"%\"}\n", file=f)
cat("function fmtIpc(v){if(v===null||v===undefined)return\"--\";return v.toFixed(2)}\n", file=f)
cat("function colorClass(v){if(v===null||v===undefined)return\"neu\";return v>0?\"pos\":v<0?\"neg\":\"neu\"}\n", file=f)
cat("// Heatmap colors: gray(33,38,45) -> red(248,81,73) for positive, -> green(63,185,80) for negative\n", file=f)
cat("function heatColor(v){\n", file=f)
cat("  if(v===null||v===undefined)return\"background:#21262d;color:#8b949e\";\n", file=f)
cat("  const t=Math.min(Math.abs(v),8)/8;\n", file=f)
cat("  const br=33,bg=38,bb=45;\n", file=f)
cat("  let r,g,b;\n", file=f)
cat("  if(v>0){r=Math.round(br+(248-br)*t);g=Math.round(bg+(81-bg)*t);b=Math.round(bb+(73-bb)*t);}\n", file=f)
cat("  else   {r=Math.round(br+(63-br)*t); g=Math.round(bg+(185-bg)*t);b=Math.round(bb+(80-bb)*t);}\n", file=f)
cat("  const txt=t>0.35?\"#fff\":\"#8b949e\";\n", file=f)
cat("  return\"background:rgb(\"+r+\",\"+g+\",\"+b+\");color:\"+txt;\n", file=f)
cat("}\n", file=f)
cat("function getFilteredLabels(fs,ts){\n", file=f)
cat("  const all=RAW.date_labels,fi=all.indexOf(fs.value),ti=all.indexOf(ts.value);\n", file=f)
cat("  return all.slice(fi,ti+1);\n", file=f)
cat("}\n", file=f)
cat("function makeKpi(label,val,sub,forceCls){\n", file=f)
cat("  const cls=forceCls||colorClass(val);\n", file=f)
cat("  return'<div class=\"kpi '+(cls===\"pos\"?\"red\":cls===\"neg\"?\"green\":\"blue\")+'\">'\n", file=f)
cat("    +'<div class=\"kpi-label\">'+label+'</div>'\n", file=f)
cat("    +'<div class=\"kpi-val '+cls+'\">'+fmtPct(val)+'</div>'\n", file=f)
cat("    +(sub?'<div class=\"kpi-sub\">'+sub+'</div>':\"\")\n", file=f)
cat("    +'</div>';\n", file=f)
cat("}\n", file=f)
cat("function chartOpts(unit){\n", file=f)
cat("  return{responsive:true,animation:{duration:300},interaction:{mode:\"index\",intersect:false},\n", file=f)
cat("    plugins:{\n", file=f)
cat("      legend:{labels:{color:\"#8b949e\",font:{size:11},boxWidth:12}},\n", file=f)
cat("      tooltip:{backgroundColor:\"#161b22\",borderColor:\"#30363d\",borderWidth:1,\n", file=f)
cat("        titleColor:\"#e6edf3\",bodyColor:\"#8b949e\",\n", file=f)
cat("        callbacks:{label:function(ctx){return\" \"+ctx.dataset.label+\": \"+(unit===\"%\"?fmtPct(ctx.raw):fmtIpc(ctx.raw))}}}\n", file=f)
cat("    },\n", file=f)
cat("    scales:{\n", file=f)
cat("      x:{ticks:{color:\"#8b949e\",font:{size:10},maxTicksLimit:20},grid:{color:\"#21262d\"}},\n", file=f)
cat("      y:{ticks:{color:\"#8b949e\",callback:function(v){return unit===\"%\"?v+\"%\":v}},grid:{color:\"#21262d\"}}\n", file=f)
cat("    }};\n", file=f)
cat("}\n", file=f)
cat("function initSelects(){\n", file=f)
cat("  const all=RAW.date_labels,last=all[all.length-1];\n", file=f)
cat("  [\"gen-from\",\"gen-to\",\"div-from\",\"div-to\"].forEach(function(id){\n", file=f)
cat("    const el=document.getElementById(id);\n", file=f)
cat("    all.forEach(function(l){el.add(new Option(l,l));});\n", file=f)
cat("    el.value=id.endsWith(\"-to\")?last:all[0];\n", file=f)
cat("  });\n", file=f)
cat("  const ds=document.getElementById(\"sel-div\");\n", file=f)
cat("  Object.entries(RAW.divisions).forEach(function(e){ds.add(new Option(e[1],e[0]));});\n", file=f)
cat("  const hy=document.getElementById(\"hm-year\");\n", file=f)
cat("  const years=[...new Set(all.map(function(l){return l.split(\"-\")[1];}))];\n", file=f)
cat("  years.forEach(function(y){hy.add(new Option(y,y));});\n", file=f)
cat("  hy.value=last.split(\"-\")[1];\n", file=f)
cat("  [\"cg-period\",\"cd-period\"].forEach(function(id){\n", file=f)
cat("    const el=document.getElementById(id);\n", file=f)
cat("    all.forEach(function(l){el.add(new Option(l,l));});\n", file=f)
cat("    el.value=last;\n", file=f)
cat("  });\n", file=f)
cat("  const cdd=document.getElementById(\"cd-div\");\n", file=f)
cat("  Object.entries(RAW.divisions).forEach(function(e){cdd.add(new Option(e[1],e[0]));});\n", file=f)
cat("  document.getElementById(\"hdr-range\").textContent=all[0]+\" -- \"+last+\"  |  \"+all.length+\" periodos\";\n", file=f)
cat("}\n", file=f)
cat("// TAB GENERAL\n", file=f)
cat("function setMeasure(m){\n", file=f)
cat("  S.genMeasure=m;\n", file=f)
cat("  [\"mensual\",\"acumulada\",\"a12m\"].forEach(function(x){document.getElementById(\"mbtn-\"+x).classList.toggle(\"active\",x===m);});\n", file=f)
cat("  updateGeneral();\n", file=f)
cat("}\n", file=f)
cat("function updateGeneral(){\n", file=f)
cat("  const labels=getFilteredLabels(document.getElementById(\"gen-from\"),document.getElementById(\"gen-to\"));\n", file=f)
cat("  if(!labels.length)return;\n", file=f)
cat("  const m=S.genMeasure,last=labels[labels.length-1],inf=RAW.inf_general;\n", file=f)
cat("  const lv=RAW.ipc_general[last];\n", file=f)
cat("  document.getElementById(\"gen-kpis\").innerHTML=\n", file=f)
cat("    makeKpi(\"Inflación Mensual\",(inf[last]&&inf[last].mensual!==undefined?inf[last].mensual:null),last)\n", file=f)
cat("    +makeKpi(\"Inflación Acumulada\",(inf[last]&&inf[last].acumulada!==undefined?inf[last].acumulada:null),last)\n", file=f)
cat("    +makeKpi(\"Inflación a 12 meses\",(inf[last]&&inf[last].a12m!==undefined?inf[last].a12m:null),last)\n", file=f)
cat("    +'<div class=\"kpi blue\"><div class=\"kpi-label\">Indice de Precios al Consumidor</div>'\n", file=f)
cat("    +'<div class=\"kpi-val neu\" style=\"font-size:20px\">'+(lv!=null?lv.toFixed(2):\"--\")+'</div>'\n", file=f)
cat("    +'<div class=\"kpi-sub\">(Base 100 = 2016)</div></div>';\n", file=f)
cat("  document.getElementById(\"gen-chart-title\").textContent=\n", file=f)
cat("    {mensual:\"Variación Mensual\",acumulada:\"Variación Acumulada\",a12m:\"Variación a 12 Meses\"}[m];\n", file=f)
cat("  destroyChart(\"gen-line\");\n", file=f)
cat("  charts[\"gen-line\"]=new Chart(document.getElementById(\"chart-gen-line\").getContext(\"2d\"),{\n", file=f)
cat("    type:\"line\",\n", file=f)
cat("    data:{labels:labels,datasets:[{label:\"Inflación General\",\n", file=f)
cat("      data:labels.map(function(l){return inf[l]&&inf[l][m]!==undefined?inf[l][m]:null;}),\n", file=f)
cat("      borderColor:\"#0054a6\",backgroundColor:\"rgba(88,166,255,.08)\",borderWidth:2,\n", file=f)
cat("      pointRadius:labels.length>36?0:3,pointHoverRadius:5,fill:true,tension:.3}]},\n", file=f)
cat("    options:chartOpts(\"%\")});\n", file=f)
cat("  destroyChart(\"gen-level\");\n", file=f)
cat("  charts[\"gen-level\"]=new Chart(document.getElementById(\"chart-gen-level\").getContext(\"2d\"),{\n", file=f)
cat("    type:\"line\",\n", file=f)
cat("    data:{labels:labels,datasets:[{label:\"IPC\",\n", file=f)
cat("      data:labels.map(function(l){return RAW.ipc_general[l]!=null?RAW.ipc_general[l]:null;}),\n", file=f)
cat("      borderColor:\"#d29922\",backgroundColor:\"rgba(210,153,34,.08)\",borderWidth:2,\n", file=f)
cat("      pointRadius:0,fill:true,tension:.3}]},\n", file=f)
cat("    options:chartOpts(\"\")});\n", file=f)
cat("  const divVals=Object.keys(RAW.divisions).map(function(c){\n", file=f)
cat("    return RAW.inf_div[c][last]&&RAW.inf_div[c][last][m]!==undefined?RAW.inf_div[c][last][m]:0;\n", file=f)
cat("  });\n", file=f)
cat("  const divNames=Object.values(RAW.divisions).map(function(n){return n.length>28?n.slice(0,26)+\"...\":n;});\n", file=f)
cat("  destroyChart(\"div-bar\");\n", file=f)
cat("  charts[\"div-bar\"]=new Chart(document.getElementById(\"chart-div-bar\").getContext(\"2d\"),{\n", file=f)
cat("    type:\"bar\",\n", file=f)
cat("    data:{labels:divNames,datasets:[{label:m,data:divVals,\n", file=f)
cat("      backgroundColor:divVals.map(function(v){return v>0?\"rgba(248,81,73,.75)\":\"rgba(63,185,80,.75)\";}),\n", file=f)
cat("      borderRadius:3}]},\n", file=f)
cat("    options:Object.assign({},chartOpts(\"%\"),{indexAxis:\"y\",\n", file=f)
cat("      plugins:{legend:{display:false},\n", file=f)
cat("        tooltip:{callbacks:{label:function(ctx){return fmtPct(ctx.raw);}}}\n", file=f)
cat("      }})});\n", file=f)
cat("}\n", file=f)
cat("// TAB DIVISIONES\n", file=f)
cat("function setDivMeasure(m){\n", file=f)
cat("  S.divMeasure=m;\n", file=f)
cat("  [\"mensual\",\"acumulada\",\"a12m\"].forEach(function(x){document.getElementById(\"dmbtn-\"+x).classList.toggle(\"active\",x===m);});\n", file=f)
cat("  updateDivision();\n", file=f)
cat("}\n", file=f)
cat("function updateDivision(){\n", file=f)
cat("  const labels=getFilteredLabels(document.getElementById(\"div-from\"),document.getElementById(\"div-to\"));\n", file=f)
cat("  if(!labels.length)return;\n", file=f)
cat("  const code=document.getElementById(\"sel-div\").value,m=S.divMeasure;\n", file=f)
cat("  const last=labels[labels.length-1],inf=RAW.inf_div[code],infG=RAW.inf_general;\n", file=f)
cat("  const ci=Object.keys(RAW.divisions).indexOf(code);\n", file=f)
cat("  document.getElementById(\"div-kpis\").innerHTML=\n", file=f)
cat("    makeKpi(\"Inflación Mensual\",(inf[last]&&inf[last].mensual!==undefined?inf[last].mensual:null),last)\n", file=f)
cat("    +makeKpi(\"Inflación Acumulada\",(inf[last]&&inf[last].acumulada!==undefined?inf[last].acumulada:null),last)\n", file=f)
cat("    +makeKpi(\"Inflación a 12 meses\",(inf[last]&&inf[last].a12m!==undefined?inf[last].a12m:null),last)\n", file=f)
cat("    +makeKpi(\"Inflación general \"+m,(infG[last]&&infG[last][m]!==undefined?infG[last][m]:null),\"Referencia\",\n", file=f)
cat("      colorClass(infG[last]&&infG[last][m]!==undefined?infG[last][m]:null));\n", file=f)
cat("  destroyChart(\"div-line\");\n", file=f)
cat("  charts[\"div-line\"]=new Chart(document.getElementById(\"chart-div-line\").getContext(\"2d\"),{\n", file=f)
cat("    type:\"line\",\n", file=f)
cat("    data:{labels:labels,datasets:[\n", file=f)
cat("      {label:\"Inflación en \" + RAW.divisions[code],\n", file=f)
cat("        data:labels.map(function(l){return inf[l]&&inf[l][m]!==undefined?inf[l][m]:null;}),\n", file=f)
cat("        borderColor:DIV_COLORS[ci],backgroundColor:\"transparent\",borderWidth:2,\n", file=f)
cat("        pointRadius:labels.length>36?0:3,tension:.3},\n", file=f)
cat("      {label:\"Inflación General\",\n", file=f)
cat("        data:labels.map(function(l){return infG[l]&&infG[l][m]!==undefined?infG[l][m]:null;}),\n", file=f)
cat("        borderColor:\"#8b949e\",backgroundColor:\"transparent\",borderWidth:1.5,\n", file=f)
cat("        borderDash:[4,3],pointRadius:0,tension:.3}]},\n", file=f)
cat("    options:chartOpts(\"%\")});\n", file=f)
cat("}\n", file=f)
cat("// TAB HEATMAP\n", file=f)
cat("function setHmMeasure(m){\n", file=f)
cat("  S.hmMeasure=m;\n", file=f)
cat("  [\"mensual\",\"acumulada\",\"a12m\"].forEach(function(x){document.getElementById(\"hmbtn-\"+x).classList.toggle(\"active\",x===m);});\n", file=f)
cat("  renderHeatmap();\n", file=f)
cat("}\n", file=f)
cat("function renderHeatmap(){\n", file=f)
cat("  const year=document.getElementById(\"hm-year\").value,m=S.hmMeasure;\n", file=f)
cat("  const ENM=[\"Jan\",\"Feb\",\"Mar\",\"Apr\",\"May\",\"Jun\",\"Jul\",\"Aug\",\"Sep\",\"Oct\",\"Nov\",\"Dec\"];\n", file=f)
cat("  const ESM=[\"Ene\",\"Feb\",\"Mar\",\"Abr\",\"May\",\"Jun\",\"Jul\",\"Ago\",\"Sep\",\"Oct\",\"Nov\",\"Dic\"];\n", file=f)
cat("  const labels=RAW.date_labels.filter(function(l){return l.indexOf(\"-\"+year)>-1;});\n", file=f)
cat("  let html='<table class=\"hm-table\"><thead><tr><th>División</th>';\n", file=f)
cat("  labels.forEach(function(l){const idx=ENM.indexOf(l.split(\"-\")[0]);html+='<th>'+(ESM[idx]||l.split(\"-\")[0])+'</th>';});\n", file=f)
cat("  html+='</tr></thead><tbody>';\n", file=f)
cat("  Object.entries(RAW.divisions).forEach(function(entry){\n", file=f)
cat("    const code=entry[0],name=entry[1];\n", file=f)
cat("    const short=name.length>32?name.slice(0,30)+\"...\":name;\n", file=f)
cat("    html+='<tr><td style=\"font-size:11px;color:var(--text);padding:5px 10px;white-space:nowrap;border:1px solid var(--border);text-align:left\">'+short+'</td>';\n", file=f)
cat("    labels.forEach(function(l){\n", file=f)
cat("      const v=RAW.inf_div[code][l]&&RAW.inf_div[code][l][m]!==undefined?RAW.inf_div[code][l][m]:null;\n", file=f)
cat("      const st=heatColor(v);\n", file=f)
cat("      const d=(v!==null&&v!==undefined)?((v>0?\"+\":\"\")+v.toFixed(2)+\"%\"):\"--\";\n", file=f)
cat("      html+='<td style=\"'+st+'\" title=\"'+name+' | '+l+' | '+d+'\">'+d+'</td>';\n", file=f)
cat("    });\n", file=f)
cat("    html+='</tr>';\n", file=f)
cat("  });\n", file=f)
cat("  html+='<tr><td style=\"font-size:11px;font-weight:600;color:var(--accent);padding:5px 10px;border:1px solid var(--border)\">IPC General</td>';\n", file=f)
cat("  labels.forEach(function(l){\n", file=f)
cat("    const v=RAW.inf_general[l]&&RAW.inf_general[l][m]!==undefined?RAW.inf_general[l][m]:null;\n", file=f)
cat("    const st=heatColor(v);\n", file=f)
cat("    const d=(v!==null&&v!==undefined)?((v>0?\"+\":\"\")+v.toFixed(2)+\"%\"):\"--\";\n", file=f)
cat("    html+='<td style=\"'+st+';font-weight:600\" title=\"General | '+l+' | '+d+'\">'+d+'</td>';\n", file=f)
cat("  });\n", file=f)
cat("  html+='</tr></tbody></table>';\n", file=f)
cat("  document.getElementById(\"hm-container\").innerHTML=html;\n", file=f)
cat("}\n", file=f)
cat("// TAB CONTRIB GENERAL\n", file=f)
cat("function setCgMeasure(m){\n", file=f)
cat("  S.cgMeasure=m;\n", file=f)
cat("  [\"mensual\",\"acumulada\",\"a12m\"].forEach(function(x){document.getElementById(\"cgmbtn-\"+x).classList.toggle(\"active\",x===m);});\n", file=f)
cat("  renderContribGeneral();\n", file=f)
cat("}\n", file=f)
cat("function renderContribGeneral(){\n", file=f)
cat("  const period=document.getElementById(\"cg-period\").value,m=S.cgMeasure;\n", file=f)
cat("  const infGen=RAW.inf_general[period] && RAW.inf_general[period][m] !== undefined ? RAW.inf_general[period][m]: null;\n", file=f)
cat("  const search=document.getElementById(\"cg-search\").value.toLowerCase();\n", file=f)
cat("  const data=RAW.contrib_general[m][period]||[];\n", file=f)
cat("  const rows=data.filter(function(r){return !search||r.descripcion.toLowerCase().indexOf(search)>-1||r.div_name.toLowerCase().indexOf(search)>-1;});\n", file=f)
cat("  document.getElementById(\"cg-kpis\").innerHTML =\n", file=f)
cat("      makeKpi(\n", file=f)
cat("        \"Inflación \" + (\n", file=f)
cat("          m===\"mensual\" ? \"Mensual\" :\n", file=f)
cat("          m===\"acumulada\" ? \"Acumulada\" :\n", file=f)
cat("          \"a 12 meses\"\n", file=f)
cat("        ),\n", file=f)
cat("        infGen,\n", file=f)
cat("        period\n", file=f)
cat("      )\n", file=f)
cat("      + '<div class=\"kpi blue\">'\n", file=f)
cat("      + '<div class=\"kpi-label\">Periodo</div>'\n", file=f)
cat("      + '<div class=\"kpi-val neu\" style=\"font-size:18px\">'\n", file=f)
cat("      + period\n", file=f)
cat("      + '</div></div>';\n", file=f)
cat("  const maxAbs=Math.max.apply(null,rows.map(function(r){return Math.abs(r.contrib_gen);}).concat([0.001]));\n", file=f)
cat("  let html='<table><thead><tr><th>#</th><th>Producto</th><th>División</th>'\n", file=f)
cat("    +'<th style=\"text-align:right\">Pond. Global</th><th style=\"text-align:right\">Contribución</th>'\n", file=f)
cat("    +'<th style=\"min-width:120px\">Barra</th></tr></thead><tbody>';\n", file=f)
cat("  rows.slice(0,50).forEach(function(r,i){\n", file=f)
cat("    const bw=Math.round(Math.abs(r.contrib_gen)/maxAbs*100);\n", file=f)
cat("    const bc=r.contrib_gen>0?\"var(--warn)\":\"var(--accent2)\";\n", file=f)
cat("    const bc2=r.contrib_gen>0?\"pos\":r.contrib_gen<0?\"neg\":\"neu\";\n", file=f)
cat("    html+='<tr><td style=\"color:var(--text2);font-family:var(--mono)\">'+(i+1)+'</td>'\n", file=f)
cat("      +'<td>'+r.descripcion+'</td><td style=\"color:var(--text2);font-size:11px\">'+r.div_name+'</td>'\n", file=f)
cat("      +'<td class=\"num\">'+r.pond_global.toFixed(4)+'</td>'\n", file=f)
cat("      +'<td class=\"num\"><span class=\"badge '+bc2+'\">'+fmtPct(r.contrib_gen)+'</span></td>'\n", file=f)
cat("      +'<td><div class=\"contrib-bar\" style=\"width:'+bw+'%;background:'+bc+'\"></div></td></tr>';\n", file=f)
cat("  });\n", file=f)
cat("  html+='</tbody></table>';\n", file=f)
cat("  if(rows.length>50)html+='<p style=\"color:var(--text2);font-size:11px;padding:8px 12px\">Mostrando 50 de '+rows.length+' productos</p>';\n", file=f)
cat("  document.getElementById(\"cg-table\").innerHTML=html;\n", file=f)
cat("  renderStackedDiv(m);\n", file=f)
cat("}\n", file=f)
cat("function renderStackedDiv(measure){\n", file=f)
cat("  const labels=RAW.date_labels;\n", file=f)
cat("  const dc={};Object.keys(RAW.divisions).forEach(function(c){dc[c]=[];});\n", file=f)
cat("  labels.forEach(function(lbl){\n", file=f)
cat("    const data=RAW.contrib_general[measure][lbl]||[];\n", file=f)
cat("    const bd={};\n", file=f)
cat("    data.forEach(function(r){bd[r.div_code]=(bd[r.div_code]||0)+r.contrib_gen;});\n", file=f)
cat("    Object.keys(RAW.divisions).forEach(function(c){dc[c].push(bd[c]!==undefined?parseFloat(bd[c].toFixed(4)):null);});\n", file=f)
cat("  });\n", file=f)
cat("  destroyChart(\"cg-stacked\");\n", file=f)
cat("  charts[\"cg-stacked\"]=new Chart(document.getElementById(\"chart-cg-stacked\").getContext(\"2d\"),{\n", file=f)
cat("    type:\"bar\",\n", file=f)
cat("    data:{labels:labels,datasets:Object.entries(RAW.divisions).map(function(e,i){\n", file=f)
cat("      return{label:e[1].length>30?e[1].slice(0,28)+\"...\":e[1],\n", file=f)
cat("        data:dc[e[0]],backgroundColor:DIV_COLORS[i]+\"cc\",borderColor:DIV_COLORS[i],\n", file=f)
cat("        borderWidth:.5,stack:\"stack\"};\n", file=f)
cat("    })},\n", file=f)
cat("    options:Object.assign({},chartOpts(\"%\"),{\n", file=f)
cat("      scales:{\n", file=f)
cat("        x:{stacked:true,ticks:{color:\"#8b949e\",font:{size:10},maxTicksLimit:20},grid:{color:\"#21262d\"}},\n", file=f)
cat("        y:{stacked:true,ticks:{color:\"#8b949e\",callback:function(v){return v+\"%\";}},grid:{color:\"#21262d\"}}\n", file=f)
cat("      },\n", file=f)
cat("      plugins:{\n", file=f)
cat("        legend:{labels:{color:\"#8b949e\",font:{size:10},boxWidth:10}},\n", file=f)
cat("        tooltip:{callbacks:{label:function(ctx){return ctx.dataset.label+\": \"+fmtPct(ctx.raw);}}}\n", file=f)
cat("      }})});\n", file=f)
cat("}\n", file=f)
cat("// TAB CONTRIB DIV\n", file=f)
cat("function setCdMeasure(m){\n", file=f)
cat("  S.cdMeasure=m;\n", file=f)
cat("  [\"mensual\",\"acumulada\",\"a12m\"].forEach(function(x){document.getElementById(\"cdmbtn-\"+x).classList.toggle(\"active\",x===m);});\n", file=f)
cat("  renderContribDiv();\n", file=f)
cat("}\n", file=f)
cat("function renderContribDiv(){\n", file=f)
cat("  const code=document.getElementById(\"cd-div\").value;\n", file=f)
cat("  const period=document.getElementById(\"cd-period\").value,m=S.cdMeasure;\n", file=f)
cat("  const search=document.getElementById(\"cd-search\").value.toLowerCase();\n", file=f)
cat("  const data=(RAW.contrib_data[code]&&RAW.contrib_data[code][m]&&RAW.contrib_data[code][m][period])||[];\n", file=f)
cat("  const infDiv=RAW.inf_div[code][period]&&RAW.inf_div[code][period][m]!==undefined?RAW.inf_div[code][period][m]:null;\n", file=f)
cat("  const infGen=RAW.inf_general[period]&&RAW.inf_general[period][m]!==undefined?RAW.inf_general[period][m]:null;\n", file=f)
cat("  document.getElementById(\"cd-kpis\").innerHTML=\n", file=f)
cat("    makeKpi(\"Inflación \"+m+ \" de la división\",infDiv,period)\n", file=f)
cat("    +makeKpi(\"Inflación General \"+m,infGen,\"referencia\",colorClass(infGen))\n", file=f)
cat("    +'<div class=\"kpi blue\"><div class=\"kpi-label\">División</div>'\n", file=f)
cat("    +'<div class=\"kpi-val neu\" style=\"font-size:14px;line-height:1.4\">'+RAW.divisions[code]+'</div></div>';\n", file=f)
cat("  const rows=data.filter(function(r){return !search||r.descripcion.toLowerCase().indexOf(search)>-1;});\n", file=f)
cat("  const maxAbs=Math.max.apply(null,rows.map(function(r){return Math.abs(r.contrib_div);}).concat([0.001]));\n", file=f)
cat("  const totalDiv = rows.reduce((s, r) => s + r.contrib_div, 0);\n", file=f)
cat("  const totalGen = rows.reduce((s, r) => s + r.contrib_gen, 0);\n", file=f)
cat("  let html='<table><thead><tr><th>#</th><th>Producto</th>'\n", file=f)
cat("    +'<th style=\"text-align:right\">Pond. División</th><th style=\"text-align:right\">Pond. Global</th>'\n", file=f)
cat("    +'<th style=\"text-align:right\">Contrib. División</th><th style=\"text-align:right\">Contrib. General</th>'\n", file=f)
cat("    +'<th style=\"min-width:120px\">Barra</th></tr></thead><tbody>';\n", file=f)
cat("  rows.forEach(function(r,i){\n", file=f)
cat("    const bw=Math.round(Math.abs(r.contrib_div)/maxAbs*100);\n", file=f)
cat("    const bc=r.contrib_div>0?\"var(--warn)\":\"var(--accent2)\";\n", file=f)
cat("    const bc2=r.contrib_div>0?\"pos\":r.contrib_div<0?\"neg\":\"neu\";\n", file=f)
cat("    const gc=r.contrib_gen>0?\"pos\":r.contrib_gen<0?\"neg\":\"neu\";\n", file=f)
cat("    html+='<tr><td style=\"color:var(--text2);font-family:var(--mono)\">'+(i+1)+'</td>'\n", file=f)
cat("      +'<td>'+r.descripcion+'</td>'\n", file=f)
cat("      +'<td class=\"num\">'+r.pond_div.toFixed(4)+'</td><td class=\"num\">'+r.pond_global.toFixed(4)+'</td>'\n", file=f)
cat("      +'<td class=\"num\"><span class=\"badge '+bc2+'\">'+fmtPct(r.contrib_div)+'</span></td>'\n", file=f)
cat("      +'<td class=\"num\"><span class=\"badge '+gc+'\">'+fmtPct(r.contrib_gen)+'</span></td>'\n", file=f)
cat("      +'<td><div class=\"contrib-bar\" style=\"width:'+bw+'%;background:'+bc+'\"></div></td></tr>';\n", file=f)
cat("  });\n", file=f)
cat("  const genCls = totalGen > 0 ? \"pos\" : totalGen < 0 ? \"neg\" : \"neu\";\n", file=f)
cat("  html += '<tr style=\"border-top:2px solid var(--border)\">'\n", file=f)
cat("    + '<td colspan=\"4\" style=\"font-weight:600;padding:8px 12px;color:var(--accent)\">TOTAL COMPUTADO</td>'\n", file=f)
cat("    + '<td class=\"num\"><span class=\"badge ' + (totalDiv>0?\"pos\":totalDiv<0?\"neg\":\"neu\") + '\">' + fmtPct(totalDiv) + '</span></td>'\n", file=f)
cat("    + '<td class=\"num\"><span class=\"badge ' + genCls + '\">' + fmtPct(totalGen) + '</span></td>'\n", file=f)
cat("    + '<td></td></tr>';\n", file=f)
cat("  html+='</tbody></table>';\n", file=f)
cat("  document.getElementById(\"cd-table\").innerHTML=html;\n", file=f)
cat("}\n", file=f)
cat("function switchTab(name){\n", file=f)
cat("  const tabs=[\"general\",\"divisiones\",\"heatmap\",\"contrib_gen\",\"contrib_div\"];\n", file=f)
cat("  document.querySelectorAll(\".tab\").forEach(function(t,i){t.classList.toggle(\"active\",tabs[i]===name);});\n", file=f)
cat("  document.querySelectorAll(\".tab-panel\").forEach(function(p){p.classList.toggle(\"active\",p.id===\"tab-\"+name);});\n", file=f)
cat("  if(name===\"divisiones\")updateDivision();\n", file=f)
cat("  if(name===\"heatmap\")renderHeatmap();\n", file=f)
cat("  if(name===\"contrib_gen\")renderContribGeneral();\n", file=f)
cat("  if(name===\"contrib_div\")renderContribDiv();\n", file=f)
cat("}\n", file=f)
cat("window.addEventListener(\"DOMContentLoaded\",function(){\n", file=f)
cat("  initSelects();updateGeneral();\n", file=f)
cat("  setTimeout(function(){document.getElementById(\"loading\").style.display=\"none\";},300);\n", file=f)
cat("});\n", file=f)
cat("\n", file=f)
cat("</script>\n", file=f)
cat("</body></html>\n", file=f)
close(f)

sz <- round(file.size(path_out)/1024)
cat("\n=== Listo ===\n")
cat("Archivo:", path_out, "\n")
cat("Tamanio:", sz, "KB\n")
cat("\nPara actualizar: reemplaza los Excel y vuelve a ejecutar.\n")