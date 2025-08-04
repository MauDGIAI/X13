library(dplyr)
library(plotly)
# setwd(ruta_SPC)
# datos <- read.table(paste(archivo_DAT,".dat",sep = ""))

DAT_plot <- function(datos, serie, SPC) {
  for (i in 1:length(SPC)) { #Eliminación de los comentarios en el SPC "#"
    if (grepl("#", SPC[i])) {
      SPC[i] <- substring(SPC[i], 1, ubi("#", SPC[i]) - 1)
    }
  }
  aux <- paste(SPC, collapse = " ") #Combinamos todo en una misma cadena
  aux <- gsub("\t", "", aux) #Quitamos los espacios con "tab", R los muestra como \t
  repeat{ aux <- gsub("  ", " ", aux) #Corregimos los casos donde haya dos o más "espacios" consecutivos ("  ", "   ", ...)
  if (grepl("  ", aux) == F) { break }
  }
  aux1 <- c(ubi("{", aux)[1]-8, ubi("}", aux)) #Obtenemos la ubicación de cada "}" y de la primera "{" menos 8 para el bucle
  aux2 <- NULL
  for (i in 2:length(aux1)) { #Unimos lo que está entre corchetes {} y cambiamos el "espacio" entre los () por "_"
    aux3 <- substring(aux, aux1[i-1]+2, aux1[i])
    aux3 <- substring(aux3, 1:nchar(aux3), 1:nchar(aux3))
    for (l in 2:length(aux3)) { if (aux3[l-1] == "{" & aux3[l] == " ") { aux3[l] <- "" } } #Para cuando haya "espacio" después de "{"
    for (l in 2:length(aux3)) { if (aux3[l] == "}" & aux3[l-1] == " ") { aux3[l-1] <- "" } } #Para cuando haya "espacio" antes de "}"
    for (l in 2:length(aux3)) { if (aux3[l-1] == "=" & aux3[l] == " ") { aux3[l] <- "" } } #Para cuando haya "espacio" después de "="
    for (l in 2:length(aux3)) { if (aux3[l] == "=" & aux3[l-1] == " ") { aux3[l-1] <- "" } } #Para cuando haya "espacio" antes de "="
    if (length(which(aux3=="(")) != 0) { aux3 <- f1(" ", "_", "(", ")", aux3) }#Identificamos si en la cadena existe algún "(", si no, se omite el bucle
    if (length(which(aux3=="'")) != 0) { aux3 <- f1(" ", "-", "'", "'", aux3) }#Identificamos si en la cadena existe algún "'", si no, se omite el bucle
    if (length(which(aux3=="\"")) != 0) { aux3 <- f1(" ", "-", "\"", "\"", aux3) }#Identificamos si en la cadena existe algún '"', si no, se omite el bucle
    aux2 <- c(aux2, paste(aux3, collapse = ""))
  }
  
  # Segundo: exportar archivo TXT que contenga el archivo SPC con el que se modelará en R
  spc <- NULL
  for (i in 1:length(aux2)) { #Crear las líneas del archivo TXT con sintaxis de R
    spc_i <- gsub("\\}", " ", gsub("\\{", " ", aux2[i]))
    aux4 <- NULL
    for (j in 1:(length(ubi(" ", spc_i))-1)) { #Utilizamos los separadores " " para pegar las cadenas de texto
      aux5 <- paste(substring(spc_i, 1, ubi(" ", spc_i)[1]-1), ".", substring(spc_i, ubi(" ", spc_i)[j]+1, ubi(" ", spc_i)[j+1]-1), sep = "")
      aux4 <- c(aux4, aux5)
    }
    for (k in 1:length(aux4)) { #Validamos que lo que está antes de cada "=" sea todo minúsculas
      aux4[k] <- paste(tolower(substring(aux4[k], 1, ubi("=", aux4[k]))), substring(aux4[k], ubi("=", aux4[k])+1, nchar(aux4[k])), sep = "")
    }
    spc <- c(spc, aux4, "")
  }
  
  for (i in 1:length(spc)) { #Concluimos con la sintaxis de R
    if (grepl("\\)\\(", spc[i]) == F) { #Tres casos: un parámetro, varios parámetros y especial para "arima.model"
      if (grepl("_", spc[i]) == F) {
        spc[i] <- ifelse(spc[i] == "", "", paste(gsub("=", ' = "', spc[i]), '",', sep = "")) #Caso con un parámetro
      } else {
        if (grepl("sigmalim", spc[i])) { spc[i] <- gsub(",", "", spc[i]) } #Excepción para la línea de "sigmalim"
        spc[i] <- gsub("_", '", "', gsub("\\)", '"),', gsub("=\\(", ' = c("', spc[i]))) #Varios parámetros
      }
    } else { #Solo aplica para "arima.model"
      spc[i] <- paste(gsub("_", " ", gsub("=", ' = "', spc[i])), '",', sep = "")
    }
  }
  
  rubros <- list(ARIMA = NULL, Trans = NULL, TD = NULL, LY = NULL, EE = NULL, Outs = NULL, FE = NULL, Obs = NULL)
  for (i in names(rubros)) {
    if (i == "Obs") {
      rubros[[which(names(rubros) == i)]] <- paste(gsub(" ", "",f2(spc, tipo = i)), collapse = " ")
    } else { rubros[[which(names(rubros) == i)]] <- paste(f2(spc, tipo = i), collapse = " ") }
  }
  
  especificaciones <- t(data.frame(rubros))
  colnames(especificaciones) <- ""
  row.names(especificaciones) <- c("ARIMA", "Transformación", "Trading Day", "Leap Year", "Efecto Easter",
                                   "Outliers", "Filtro Estacional", "Observaciones")
  
  outliers <- toupper(substring(strsplit(especificaciones[6], " ")[[1]], 3))
  if (max(datos$V2) == 12) {
    datos$fecha <- as.Date(paste(datos$V1, datos$V2, "1", sep = "/"))
    datos$outliers <- paste(datos$V1, ".",
                            ifelse(datos$V2 == 1, "JAN",
                                   ifelse(datos$V2 == 2, "FEB",
                                          ifelse(datos$V2 == 3, "MAR",
                                                 ifelse(datos$V2 == 4, "APR",
                                                        ifelse(datos$V2 == 5, "MAY",
                                                               ifelse(datos$V2 == 6, "JUN",
                                                                      ifelse(datos$V2 == 7, "JUL",
                                                                             ifelse(datos$V2 == 8, "AUG",
                                                                                    ifelse(datos$V2 == 9, "SEP",
                                                                                           ifelse(datos$V2 == 10, "OCT",
                                                                                                  ifelse(datos$V2 == 11, "NOV", "DEC"))))))))))),
                            sep = "")
    dif_color <- rep("#009ACD", nrow(datos))
    for (i in 1:12) {
      auxiliar1 <- paste(".", 1:12, sep = ""); auxiliar2 <- c(".JAN",".FEB",".MAR",".APR",".MAY",".JUN",".JUL",".AUG",".SEP",".OCT",".NOV",".DEC")
      outliers <- gsub(auxiliar1[i], auxiliar2[i], outliers, fixed = TRUE)
    }
    for (i in 1:length(outliers)) {
      if (!grepl("-", outliers[i])) {
        dif_color[which(datos$outliers == outliers[i])] <- "#CD0000"
      } else {
        dif_color[seq(which(datos$outliers == strsplit(outliers[i], "-")[[1]][1]),
                      which(datos$outliers == strsplit(outliers[i], "-")[[1]][2]),
                      by = 1)] <- "#8B0000"
      }
    }
    graph <- plot_ly(data = datos, x = ~datos$fecha, hovertemplate = 'Mes: %{x}<br>Índice: %{y}<extra></extra>') %>%
      layout(title = serie, xaxis = list(title = ""), yaxis = list(title = "")) %>%
      add_trace(y = ~datos$V3, name = "Línea", type = "scatter", mode = "lines", line = list(color="#009ACD")) %>% 
      add_trace(y = ~datos$V3, name = "Punto", type = "scatter", mode = "markers",
                marker = list(color = dif_color, size = 5))
  } else {
    for (i in 4:1) { datos$V2[which(datos$V2 == i)] <- c(1,4,7,10)[i] }
    datos$fecha <- as.Date(paste(datos$V1, datos$V2, "1", sep = "/"))
    for (i in 4:1) { datos$V2[which(datos$V2 == c(1,4,7,10)[i])] <- c("I","II","III","IV")[i] }
    datos$trim <- paste(datos$V2, datos$V1, sep = "-")
    dif_color <- rep("#009ACD", nrow(datos))
    for (i in 1:length(outliers)) {
      if (!grepl("-", outliers[i])) {
        outliers[i] <- ifelse(substring(outliers[i], nchar(outliers[i])) == "1", paste("I-", substring(outliers[i], 1, 4), sep = ""),
                              ifelse(substring(outliers[i], nchar(outliers[i])) == "2", paste("II-", substring(outliers[i], 1, 4), sep = ""),
                                     ifelse(substring(outliers[i], nchar(outliers[i])) == "3", paste("III-", substring(outliers[i], 1, 4), sep = ""),
                                            paste("IV-", substring(outliers[i], 1, 4), sep = ""))))
        dif_color[which(datos$trim == outliers[i])] <- "#CD0000"
      } else {
        aux21 <- strsplit(outliers[i], "-")[[1]][1]
        aux21 <- ifelse(substring(aux21, nchar(aux21)) == "1", paste("I-", substring(aux21, 1, 4), sep = ""),
                        ifelse(substring(aux21, nchar(aux21)) == "2", paste("II-", substring(aux21, 1, 4), sep = ""),
                               ifelse(substring(aux21, nchar(aux21)) == "3", paste("III-", substring(aux21, 1, 4), sep = ""),
                                      paste("IV-", substring(aux21, 1, 4), sep = ""))))
        aux22 <- strsplit(outliers[i], "-")[[1]][2]
        aux22 <- ifelse(substring(aux22, nchar(aux22)) == "1", paste("I-", substring(aux22, 1, 4), sep = ""),
                        ifelse(substring(aux22, nchar(aux22)) == "2", paste("II-", substring(aux22, 1, 4), sep = ""),
                               ifelse(substring(aux22, nchar(aux22)) == "3", paste("III-", substring(aux22, 1, 4), sep = ""),
                                      paste("IV-", substring(aux22, 1, 4), sep = ""))))
        dif_color[seq(which(datos$trim == aux21),
                      which(datos$trim == aux22),
                      by = 1)] <- "#8B0000"
      }
    }
    graph <- plot_ly(data = datos, x = ~datos$fecha, text = ~datos$trim, hovertemplate = 'Trimestre: %{text}<br>Índice: %{y}<extra></extra>') %>%
      layout(title = serie, xaxis = list(title = ""), yaxis = list(title = "")) %>%
      add_trace(y = ~datos$V3, name = "Línea", type = "scatter", mode = "lines", line = list(color="#009ACD")) %>% 
      add_trace(y = ~datos$V3, name = "Punto", type = "scatter", mode = "markers",
                marker = list(color = dif_color, size = 5))
  }
  return(graph)
}

# https://rpubs.com/Wences/graficos_interactivos
# https://r-charts.com/es/evolucion/series-temporales-ggplot2/
# https://r-charts.com/es/colores/
# https://ggplot2.tidyverse.org/articles/ggplot2-specs.html
# https://rpubs.com/juanhklopper/color_in_Plotly_for_R
# https://mastering-shiny.org/action-dynamic.html
# https://fontawesome.com/search
