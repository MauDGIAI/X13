# Función para obtener la ubicación de un caracter en específico
ubi <- function(caracter, cadena){ which(strsplit(cadena, "")[[1]] == caracter) }

# Función para cambiar un caracter por otro, dentro de otros dos caracteres
f1 <- function(caracter, reemplazo, izq, der, vector){
  if (izq == der) { 
    for (j in seq(1, length(which(vector == izq)), by = 2)) { 
      for (k in which(vector == izq)[j]:which(vector == der)[j+1])
      { if (vector[k]==caracter) { vector[k] <- reemplazo} } 
    }
  } else {
    for (j in 1:length(which(vector == izq))) { 
      for (k in which(vector == izq)[j]:which(vector == der)[j])
      { if (vector[k]==caracter) { vector[k] <- reemplazo} } 
    }
  }
  return(vector) 
} # Nota: el vector cadena debe estar dividido elemento a elemento

# Función para llenar lista "rubros" en el programa "SPC"
library(stringr)
f2 <- function(spc, tipo){
  switch(tipo,
         ARIMA = {
           espec <- substring(spc[grep("arima", spc)], ubi("=", spc[grep("arima", spc)]) + 3, nchar(spc[grep("arima", spc)]) - 2)
         },
         Trans = {
           espec <- substring(spc[grep("function", spc)], ubi("=", spc[grep("function", spc)]) + 3, nchar(spc[grep("function", spc)]) - 2)
           if (espec == "none") { espec <- "Sin transformación log" }
         },
         TD = {
           aux10 <- strsplit(str_to_title(spc[grep("variables", spc)]), split = " ")[[1]]
           if (sum(grepl("Td", aux10)) != 0) {
             espec <- substring(aux10[grep("Td", aux10)], ubi("T", aux10[grep("Td", aux10)]), nchar(aux10[grep("Td", aux10)]) - 2)
           } else { espec <- NULL }
         },
         LY = {
           aux10 <- strsplit(str_to_title(spc[grep("variables", spc)]), split = " ")[[1]]
           if (sum(grepl("Lp", aux10)) != 0) {
             espec <- substring(aux10[grep("Lp", aux10)], ubi("L", aux10[grep("Lp", aux10)]), nchar(aux10[grep("Lp", aux10)]) - 2)
           } else { espec <- NULL }
         },
         EE = {
           aux10 <- strsplit(str_to_title(spc[grep("variables", spc)]), split = " ")[[1]]
           if (sum(grepl("Ea", aux10)) != 0) {
             espec <- substring(aux10[grep("Ea", aux10)], ubi("E", aux10[grep("Ea", aux10)]), nchar(aux10[grep("Ea", aux10)]) - 2)
           } else { espec <- NULL }
         },
         Outs = {
           aux10 <- strsplit(spc[grep("variables", spc)], split = " ")[[1]]
           espec <- c(
             if (sum(grepl("19", aux10[-grep("20", aux10)])) != 0) {
               sub("\\(", "",gsub("c\\(", "",gsub(")", "", gsub(",", "", gsub('"', "", aux10[grep("19", aux10[-grep("20", aux10)])])))))
             } else { NULL },
             if (sum(grepl("20", aux10)) != 0) {
               sub("\\(", "",gsub("c\\(", "", gsub(")", "", gsub(",", "", gsub('"', "", aux10[grep("20", aux10)])))))
             } else { NULL }
           )
         },
         FE = {
           if (sum(grepl("seasonalma", spc)) != 0) {
             espec <- substring(spc[grep("seasonalma", spc)], ubi("(", spc[grep("seasonalma", spc)]) + 1, nchar(spc[grep("seasonalma", spc)]) - 3)
             espec <- gsub(",", "", gsub('"', "", espec))
           } else { espec <- NULL }
         },
         Obs = {
           aux13 <- tolower(gsub("c\\(", "\\(",gsub('"', "", spc)))
           espec <- c(
                      if (sum(grepl("adjust", aux13)) != 0) {
                        substring(aux13[grep("adjust", aux13)], ubi(".", aux13[grep("adjust", aux13)]) + 1, nchar(aux13[grep("adjust", aux13)]) - 1)
                      } else { NULL },
                      if (sum(grepl("tcrate", aux13)) != 0) {
                        substring(aux13[grep("tcrate", aux13)], ubi("n", aux13[grep("tcrate", aux13)]) + 2, nchar(aux13[grep("tcrate", aux13)]) - 1)
                      } else { NULL },
                      if (sum(grepl("sigmalim", aux13)) != 0) {
                        substring(aux13[grep("sigmalim", aux13)], ubi("s", aux13[grep("sigmalim", aux13)]), nchar(aux13[grep("sigmalim", aux13)]) - 1)
                      } else { NULL },
                      if (sum(grepl("excludefcst", aux13)) != 0) {
                        substring(aux13[grep("excludefcst", aux13)], ubi(".", aux13[grep("excludefcst", aux13)]) + 1, nchar(aux13[grep("excludefcst", aux13)]) - 1)
                      } else { NULL },
                      if (sum(grepl("trendma", aux13)) != 0) {
                        substring(aux13[grep("trendma", aux13)], ubi(".", aux13[grep("trendma", aux13)]) + 1, nchar(aux13[grep("trendma", aux13)]) - 1)
                      } else { NULL },
                      if (sum(grepl("calendarsigma", aux13)) != 0) {
                        substring(aux13[grep("calendarsigma", aux13)], ubi(".", aux13[grep("calendarsigma", aux13)]) + 1, nchar(aux13[grep("calendarsigma", aux13)]) - 1)
                      } else { NULL },
                      if (sum(grepl("x11.type", aux13)) != 0) {
                        substring(aux13[grep("x11.type", aux13)], ubi(".", aux13[grep("x11.type", aux13)]) + 1, nchar(aux13[grep("x11.type", aux13)]) - 1)
                      } else { NULL }
                      )
         }
         )
  return(espec)
}

# Función del CONVERTIDOR, para generar los archivos DAT a partir de un archivo de datos EXCEL
library(readxl)
f_conv <- function(periodo, anio, mestrim, ruta_datos, ruta_descarga){
  datos <- read_excel(ruta_datos, col_names = TRUE)
  datos <- as.data.frame(datos)
  anio <- as.numeric(anio); mestrim <- as.numeric(mestrim)
  switch (periodo,
    Mensual = {
      c1 <- rep(c(anio:(anio + round(nrow(datos)/11, 0))), each = 12)
      c2 <- rep(c(1:12), round(nrow(datos)/11, 0) + 1)
      for (i in names(datos)) {
        dat <- data.frame(C1 = c1[mestrim:(mestrim + nrow(datos) -1)],
                          C2 = c2[mestrim:(mestrim + nrow(datos) -1)],
                          C3 = datos[[i]])
        write.table(dat, paste(ruta_descarga, "/", i, ".dat", sep = ""), col.names = F, row.names = F, quote = F, sep = "\t")
      }
    },
    Trimestral = {
      c1 <- rep(c(anio:(anio + round(nrow(datos)/3, 0))), each = 4)
      c2 <- rep(c(1:4), round(nrow(datos)/3, 0) + 1)
      for (i in names(datos)) {
        dat <- data.frame(C1 = c1[mestrim:(mestrim + nrow(datos) -1)],
                          C2 = c2[mestrim:(mestrim + nrow(datos) -1)],
                          C3 = datos[[i]])
        write.table(dat, paste(ruta_descarga, "/", i, ".dat", sep = ""), col.names = F, row.names = F, quote = F, sep = "\t")
      }
    }
  )
}
