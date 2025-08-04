#setwd(ruta_SPC)
# Primero: LEER los archivos SPC y acomodar el formato para facilitar la identificación de las componentes.
#SPC <- trimws(readLines(paste(archivo_SPC, ".spc", sep = ""), warn = F)) #Leemos los SPC tal cual y quitamos "espacios" al inicio y al final

f_SPC <- function(SPC){ #Inicio función "f_SPC"
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
  
  # Rellenar inputs --
  aux30 <- paste(especificaciones[3:6], collapse = " ")
  repeat{ aux30 <- gsub("  ", " ", aux30) #Corregimos los casos donde haya dos o más "espacios" consecutivos ("  ", "   ", ...)
  if (grepl("  ", aux30) == F) { break }
  }
  carga_mAnt <- c(especificaciones[1], especificaciones[2], trimws(aux30), especificaciones[7],
                  strsplit(especificaciones[8], " ")[[1]])
  # ------------------
  
  return(list("I" = especificaciones, "II" = carga_mAnt))
  
} #Fin de función "f_SPC"

###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ######
###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ######
###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ######
###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ######
###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ######

f_standarize <- function(GRAF){
  # *** # --- # +++ # ||| # °°° # DE AQUÍ
  for (i in 1:length(GRAF)) { #Eliminación de los comentarios en el SPC "#"
    if (grepl("#", GRAF[i]) & grepl("\\{", GRAF[i])) {
      GRAF[i] <- substring(GRAF[i], 1, ubi("#", GRAF[i]) - 1)
    }
    if (grepl("#", GRAF[i]) & grepl("\\}", GRAF[i])) {
      GRAF[i] <- substring(GRAF[i], 1, ubi("#", GRAF[i]) - 1)
    }
  }
  con_p <- grep("\\{", GRAF)[1] #Ubicación del primer "{"
  con_u <- grep("\\}", GRAF)[length(grep("\\}", GRAF))] #Ubicación del último "}"
  for (i in 1:length(GRAF)) { #Eliminación de los comentarios en el SPC "#"
    if (grepl("#", GRAF[i])) {
      if (i > con_p & i < con_u) {
        GRAF[i] <- substring(GRAF[i], 1, ubi("#", GRAF[i]) - 1)
      }
    }
  }
  # *** # --- # +++ # ||| # °°° # HASTA AQUÍ
  
  aux <- paste(GRAF, collapse = " ") #Combinamos todo en una misma cadena
  aux <- gsub("\t", "", aux) #Quitamos los espacios con "tab", R los muestra como \t
  repeat{ aux <- gsub("  ", " ", aux) #Corregimos los casos donde haya dos o más "espacios" consecutivos ("  ", "   ", ...)
  if (grepl("  ", aux) == F) { break }
  }
  aux2 <- strsplit(aux, "")[[1]]
  for (l in 2:length(aux2)) { if (aux2[l-1] == "{" & aux2[l] == " ") { aux2[l] <- "" } } #Para cuando haya "espacio" después de "{"
  for (l in 2:length(aux2)) { if (aux2[l] == "}" & aux2[l-1] == " ") { aux2[l-1] <- "" } } #Para cuando haya "espacio" antes de "}"
  for (l in 2:length(aux2)) { if (aux2[l-1] == "=" & aux2[l] == " ") { aux2[l] <- "" } } #Para cuando haya "espacio" después de "="
  for (l in 2:length(aux2)) { if (aux2[l] == "=" & aux2[l-1] == " ") { aux2[l-1] <- "" } } #Para cuando haya "espacio" antes de "="
  for (l in 2:length(aux2)) { if (aux2[l] == "#" & aux2[l+1] != " ") { aux2[l+1] <- paste(" ", aux2[l+1], sep = "") } } #Para cuando no haya "espacio" después de "#"
  for (l in 2:length(aux2)) { if (aux2[l-1] == "(" & aux2[l] == " ") { aux2[l] <- "" } } #Para cuando haya "espacio" después de "("
  for (l in 2:length(aux2)) { if (aux2[l] == ")" & aux2[l-1] == " ") { aux2[l-1] <- "" } } #Para cuando haya "espacio" antes de ")"
  if (length(which(aux2=="(")) != 0) { aux2 <- f1(" ", "__", "(", ")", aux2) }#Identificamos si en la cadena existe algún "(", si no, se omite el bucle
  if (length(which(aux2=="'")) != 0) { aux2 <- f1(" ", "-", "'", "'", aux2) }#Identificamos si en la cadena existe algún "'", si no, se omite el bucle
  if (length(which(aux2=="\"")) != 0) { aux2 <- f1(" ", "-", "\"", "\"", aux2) }#Identificamos si en la cadena existe algún '"', si no, se omite el bucle
  
  aux3 <- gsub("#", "*#", gsub("} ", "}*", gsub("series\\{", "*series\\{", paste(aux2, collapse = ""))))
  aux3 <- strsplit(aux3, "*", fixed = TRUE)[[1]]; aux3 <- trimws(aux3); aux3 <- aux3[-which(aux3 == "")]
  #### Parche para Rompimiento Estacional (PRE)
  if (sum(grepl("# seasonal/", aux3)) > 0) {
    romp_estacional <- substring(aux3[grep("//", aux3)], 1, ubi("/", aux3[grep("//", aux3)])[3])
    aux3[grep("//", aux3)] <- substring(aux3[grep("//", aux3)], (ubi("/", aux3[grep("//", aux3)])[3] + 2), nchar(aux3[grep("//", aux3)]))
    aux3 <- c(aux3[1:(grep("regression\\{", aux3) - 1)],
              paste(aux3[grep("regression\\{", aux3)], aux3[grep("regression\\{", aux3) + 1], sep = " "),
              aux3[(grep("regression\\{", aux3) + 2):length(aux3)])
  } else { romp_estacional <- "none" }
  ####
  
  aux4 <- NULL
  for (k in aux3) {
    if (sum(grepl("series\\{", k)) == 1) {
      k <- gsub("\\{", "\\{ ", k); k <- strsplit(k, " ")[[1]]; k <- gsub("__", " ", gsub("-", " ", k))
    }
    if (sum(grepl("transform\\{", k)) == 1) { k <- gsub("\\{", "\\{ ", k); k <- strsplit(k, " ")[[1]] }
    if (sum(grepl("regression\\{", k)) == 1) { k <- gsub("\\{", "\\{ ", k); k <- strsplit(k, " ")[[1]]; k <- gsub("__", " ", k) }
    if (sum(grepl("arima\\{", k)) == 1) { k <- strsplit(k, " ")[[1]]; k <- gsub("__", " ", k) }
    if (sum(grepl("estimate\\{", k)) == 1) { k <- strsplit(k, " ")[[1]]; k <- gsub("__", " ", k) }
    if (sum(grepl("x11\\{", k)) == 1) { k <- strsplit(k, " ")[[1]]; k <- gsub("__", " ", k) }
    aux4 <- c(aux4, k)
  }
  aux5 <- NULL
  for (k in aux4) {
    if (!grepl("\\{", k) & !grepl("#", k)) { k <- paste("    ", k, sep = "") }
    aux5 <- c(aux5, k)
  }
  aux5[grep("variables=", aux5)] <- gsub("Td1noLpyear", "Td1nolpyear", gsub("TdnoLpyear", "Tdnolpyear", aux5[grep("variables=", aux5)]))
  aux5[grep("variables=", aux5)] <- gsub("td1noLpyear", "Td1nolpyear", gsub("tdnoLpyear", "Tdnolpyear", aux5[grep("variables=", aux5)]))
  
  spc <- NULL
  for (k in aux5) {
    if (sum(grepl("variables=", k)) == 1) {
      if (grepl(" ", substring(k, ubi("(", k), ubi(")", k)[1]))) {
        if (grepl("seasonal/", substring(k, ubi("(", k), ubi(")", k)[1]))) {
          k <- c(trimws(substring(k, 1, ubi("/", k)[3]), which = "right"),
                 paste("               ", substring(k, (ubi("/", k)[3] + 1), nchar(k)), sep = ""))
        } else {
          if (grepl(".", k, fixed = TRUE)) {
            k <- c(trimws(substring(k, 1, (ubi(".", k)[1] - 7)), which = "right"),
                   paste("               ", substring(k, (ubi(".", k)[1] - 6), nchar(k)), sep = ""))
          } else {
            k <- c(trimws(substring(k, 1, ubi("=", k)[1]), which = "right"),
                   paste("               ", substring(k, (ubi("=", k)[1] + 1), nchar(k)), sep = ""))
          }
        }
        repeat { #Seccionamos los outliers para evitar renglones muy largos
          if (length(ubi(".", k[length(k)])) > 5) {
            kk <- c(k[1:(length(k) - 1)], trimws(substring(k[length(k)], 1, ubi(".", k[length(k)])[5] + ifelse(sum(grepl("period=4", aux5))==1, 2, 4)), which = "right"),
                    paste("               ", substring(k[length(k)], ubi(".", k[length(k)])[5] + ifelse(sum(grepl("period=4", aux5))==1, 3, 5), nchar(k[length(k)])), sep = ""))
            k <- kk
          } else { break }
        }
      }
    }
    if (sum(grepl("seasonalma=\\(", k)) == 1) {
      if (length(ubi(" ", substring(k, ubi("(", k), ubi(")", k)))) > 4) {
        k <- c(substring(k, 1, ubi(" ", k)[(length(ubi(" ", k)) - 5)]),
               paste("                ", substring(k, (ubi(" ", k)[(length(ubi(" ", k)) - 5)] + 1), nchar(k)), sep = ""))
      }
    }
    spc <- c(spc, k)
  }
  
  #### Parche para Rompimiento Estacional (PRE)
  if (romp_estacional != "none") {
    spc[grep("m1 m2 m3 m4", spc)] <- paste(spc[grep("m1 m2 m3 m4", spc)], romp_estacional, sep = " ")
  }
  ####
  
  return(spc)
}
