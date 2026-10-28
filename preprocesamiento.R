
datos <- read.csv("data/dataset.csv", na.strings = "NA")

# Construir la fecha y hora de cada observación
datos$fecha_hora <- as.POSIXct(
  with(datos, sprintf("%04d-%02d-%02d %02d:00:00",
                      year, month, day, hour)),
  format = "%Y-%m-%d %H:%M:%S",
  tz = "Asia/Shanghai"
)

datos <- datos[order(datos$fecha_hora), ]


plot(
  datos$fecha_hora,
  datos$pm2.5,
  type = "l",
  col = "steelblue4",
  lwd = 0.5,
  main = "Evolución horaria de PM2.5 en Beijing (2010–2014)",
  xlab = "Fecha",
  ylab = "Concentración de PM2.5 (µg/m³)"
)

datos$pm25_ausente <- is.na(datos$pm2.5)
sum(datos$pm25_ausente)
mean(datos$pm25_ausente) * 100

# Copia para análisis que requieran PM2.5 observado
datos_observados <- datos[!datos$pm25_ausente, ]

# Comprobaciones
pm25_cero <- !is.na(datos$pm2.5) & datos$pm2.5 == 0

rocio_superior <- !is.na(datos$DEWP) &
  !is.na(datos$TEMP) &
  datos$DEWP > datos$TEMP

columnas <- c("year", "month", "day", "hour", "pm2.5", "DEWP", "TEMP")

datos[pm25_cero, columnas]
datos[rocio_superior, columnas]

datos$pm25_cero <- pm25_cero
datos$revisar_rocio <- rocio_superior

# Mostrar las dos mediciones de PM2.5 iguales a cero
print(datos[pm25_cero, columnas], row.names = FALSE)

for (i in which(pm25_cero)) {
  
  # Seleccionar la fila del cero y sus filas cercanas
  filas <- seq.int(
    max(1, i - 2),
    min(nrow(datos), i + 2)
  )
  
  cat("\nMediciones alrededor del cero:\n")
  print(datos[filas, columnas], row.names = FALSE)
}

# Mostrar el registro con punto de rocío superior a temperatura
print(datos[rocio_superior, columnas], row.names = FALSE)


fila <- which(
  datos$year == 2010 &
    datos$month == 10 &
    datos$day == 8 &
    datos$hour == 7
)

registro_original <- datos[fila, columnas]
print(registro_original, row.names = FALSE)

# Corregir el registro
datos$DEWP[fila] <- datos$TEMP[fila]

print(datos[fila, columnas], row.names = FALSE)


# Agrupar períodos consecutivos con y sin datos
rachas <- rle(is.na(datos$pm2.5))

# Posiciones de inicio y fin de cada grupo
fin <- cumsum(rachas$lengths)
inicio <- fin - rachas$lengths + 1

# Conservar únicamente los grupos de valores NA
periodos_na <- data.frame(
  inicio = datos$fecha_hora[inicio[rachas$values]],
  fin = datos$fecha_hora[fin[rachas$values]],
  horas = rachas$lengths[rachas$values]
)

periodos_largos <- periodos_na[
  order(periodos_na$horas, decreasing = TRUE),
]

print(head(periodos_largos, 10), row.names = FALSE)

plot(
  periodos_na$inicio,
  periodos_na$horas,
  type = "h",
  lwd = 2,
  col = "steelblue4",
  main = "Duración de los períodos sin mediciones de PM2.5",
  xlab = "Fecha de inicio del período",
  ylab = "Número de horas consecutivas sin datos"
)
