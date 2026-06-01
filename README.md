Dashboard IPC Bolivia – Inflación y contribuciones

Dashboard interactivo y autónomo para visualizar la evolución del Índice de Precios al Consumidor (IPC) de Bolivia, desagregado por divisiones y productos. Genera un archivo HTML que permite explorar la inflación mensual, acumulada e interanual, mapas de calor por meses y contribuciones a la inflación general y por división.

**Desarrollado en R** a partir de los datos del INE

## Características

- 📈 **IPC general** – evolución y nivel del índice base 2016.
- 📊 **Por división** – inflación de cada una de las 12 divisiones, comparada con el IPC general.
- 🌡️ **Heatmap** – inflación por división y mes (rojo = inflación, verde = deflación).
- 🧾 **Contribuciones al IPC general** – qué productos explican la inflación (top 50 por período).
- 🧩 **Contribuciones por división** – desglose dentro de cada división

---

## Requisitos

- **R** (≥ 4.0) con los siguientes paquetes instalados:
  ```r
  install.packages(c("readxl", "jsonlite", "dplyr", "lubridate", "stringr"))
