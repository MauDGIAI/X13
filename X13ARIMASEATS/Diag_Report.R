# DIAGNÓSTICO Y REPORTE
f_diag <- function(ruta_udg, nombre_serie){ #Inicio función "f_diag"
  udg <- read.table(paste(ruta_udg, "SPC_temp.udg", sep = "\\"), header = FALSE, sep = ":")
  aux <- trimws(udg[,2])
  for (i in 1:length(aux)) {
    repeat{ aux[i] <- gsub("  ", " ", aux[i]) #Corregimos los casos donde haya dos o más "espacios" consecutivos ("  ", "   ", ...)
    if (grepl("  ", aux[i]) == F) { break }
    }
  }
  diag_spc <- as.list(aux)
  names(diag_spc) <- gsub("AutoOutlier", "AutoOTLR", udg[,1]); color_rojo <- list()
  
  if (!is.null(diag_spc$`ftest$Trading Day`)) {
    diag_spc$`ftest$Trading Day` <- strsplit(diag_spc$`ftest$Trading Day`, " ")[[1]]
    color_rojo$TradingDayp <- ifelse(as.numeric(diag_spc$`ftest$Trading Day`[4]) > 0.05, "Trading Day no significativo", "")
    diag_spc$`ftest$Trading Day` <- paste("TradingDay[f-p", round(as.numeric(diag_spc$`ftest$Trading Day`[4]), 2), "]", sep = "")
  }
  if (!is.null(diag_spc$`1-Coefficient Trading Day$Weekday`)) {
    diag_spc$`1-Coefficient Trading Day$Weekday` <- strsplit(diag_spc$`1-Coefficient Trading Day$Weekday`, " ")[[1]]
    color_rojo$TradingDayt <- ifelse(abs(as.numeric(diag_spc$`1-Coefficient Trading Day$Weekday`[3])) < 1.96, "Trading Day no significativo", "")
    diag_spc$`1-Coefficient Trading Day$Weekday` <- paste("TradingDay[t-", round(as.numeric(diag_spc$`1-Coefficient Trading Day$Weekday`[3]), 2), "]", sep = "")
  }
  if (!is.null(diag_spc$`Leap Year$Leap Year`)) {
    diag_spc$`Leap Year$Leap Year` <- strsplit(diag_spc$`Leap Year$Leap Year`, " ")[[1]]
    color_rojo$Lpyear <- ifelse(abs(as.numeric(diag_spc$`Leap Year$Leap Year`[3])) < 1.96, "Lpyear no significativo", "")
    diag_spc$`Leap Year$Leap Year` <- paste("Lpyear[t-", abs(round(as.numeric(diag_spc$`Leap Year$Leap Year`[3]), 2)), "]", sep = "")
  }
  if (length(grep("Easter", names(diag_spc))) != 0) {
    aux0 <- gsub("\\]", "", substring(gsub("Easter\\[", "", names(diag_spc)[grep("Easter", names(diag_spc))]), 1, 2))
    diag_spc[[grep("Easter", names(diag_spc))]] <- strsplit(diag_spc[[grep("Easter", names(diag_spc))]], " ")[[1]]
    color_rojo$Easter <- ifelse(abs(as.numeric(diag_spc[[grep("Easter", names(diag_spc))]][3])) < 1.96, "Easter no significativo", "")
    diag_spc[[grep("Easter", names(diag_spc))]] <- paste("E", aux0, "[t", round(as.numeric(diag_spc[[grep("Easter", names(diag_spc))]][3]), 2), "]", sep = "")
  }
  if (length(grep("ftest\\$Seasonal", names(diag_spc))) != 0) {
    diag_spc[[grep("ftest\\$Seasonal", names(diag_spc))]] <- strsplit(diag_spc[[grep("ftest\\$Seasonal", names(diag_spc))]], " ")[[1]]
    color_rojo$Rompimiento <- ifelse(as.numeric(diag_spc[[grep("ftest\\$Seasonal", names(diag_spc))]][4]) > 0.05, "Rompimiento estacional no significativo", "")
    diag_spc[[grep("ftest\\$Seasonal", names(diag_spc))]] <- paste("SR[f-p", round(as.numeric(diag_spc[[grep("ftest\\$Seasonal", names(diag_spc))]][4]), 3), "]", sep = "")
  }
  if (length(grep("AutoOTLR\\$", names(diag_spc))) != 0) {
    aux0 <- gsub("AutoOTLR\\$", "", names(diag_spc)[grep("AutoOTLR\\$", names(diag_spc))])
    for (i in 1:length(aux0)) {
      diag_spc[[grep("AutoOTLR\\$", names(diag_spc))[i]]] <- strsplit(diag_spc[[grep("AutoOTLR\\$", names(diag_spc))[i]]], " ")[[1]]
      aux0[i] <- paste(aux0[i], "[t", round(as.numeric(diag_spc[[grep("AutoOTLR\\$", names(diag_spc))[i]]][3]), 2), "]", sep = "")
    }
    diag_spc$autooutliers <- paste(aux0, collapse = " ") } else { diag_spc$autooutliers <- "" }
  if (length(grep("Outlier\\$", names(diag_spc))) != 0) {
    aux0 <- gsub("Outlier\\$", "", names(diag_spc)[grep("Outlier\\$", names(diag_spc))])
    for (i in 1:length(aux0)) {
      diag_spc[[grep("Outlier\\$", names(diag_spc))[i]]] <- strsplit(diag_spc[[grep("Outlier\\$", names(diag_spc))[i]]], " ")[[1]]
      aux0[i] <- paste(aux0[i], "[t", round(as.numeric(diag_spc[[grep("Outlier\\$", names(diag_spc))[i]]][3]), 2), "]", sep = "")
    }
    diag_spc$outliers <- paste(aux0, collapse = " ") } else { diag_spc$outliers <- "" }
  if (length(grep("AR\\$Nonseasonal\\$01\\$", names(diag_spc))) != 0) {
    aux0 <- aux1 <- aux2 <- aux3 <- gsub("AR\\$Nonseasonal\\$01\\$", "", names(diag_spc)[grep("AR\\$Nonseasonal\\$01\\$", names(diag_spc))])
    for (i in 1:length(aux0)) {
      diag_spc[[grep("AR\\$Nonseasonal\\$01\\$", names(diag_spc))[i]]] <- strsplit(diag_spc[[grep("AR\\$Nonseasonal\\$01\\$", names(diag_spc))[i]]], " ")[[1]]
      aux0[i] <- paste(aux0[i], "[", round(as.numeric(diag_spc[[grep("AR\\$Nonseasonal\\$01\\$", names(diag_spc))[i]]][1]), 4), ", ",
                       round(as.numeric(diag_spc[[grep("AR\\$Nonseasonal\\$01\\$", names(diag_spc))[i]]][2]), 5), "]", sep = "")
      aux1[i] <- ifelse(abs(as.numeric(diag_spc[[grep("AR\\$Nonseasonal\\$01\\$", names(diag_spc))[i]]][1])) < 2*abs(as.numeric(diag_spc[[grep("AR\\$Nonseasonal\\$01\\$", names(diag_spc))[i]]][2])),
                        paste("ar(", aux1[i], ") no significativo", sep = ""), "")
      aux2[i] <- ifelse(abs(as.numeric(diag_spc[[grep("AR\\$Nonseasonal\\$01\\$", names(diag_spc))[i]]][1])) >= 1, "Modelo ARIMA no invertible", "")
      aux3[i] <- as.numeric(diag_spc[[grep("AR\\$Nonseasonal\\$01\\$", names(diag_spc))[i]]][1])
    }
    aux3 <- ifelse(sum(as.numeric(aux3)) >= 1, "Modelo ARIMA no invertible", ""); aux4 <- c(aux1, aux2, aux3)
    aux4 <- aux4[which(aux4 != "")]
    color_rojo$AR <- paste(aux4, collapse = ", ")
    diag_spc$AR <- paste(aux0, collapse = " ") } else { diag_spc$AR <- "" }
  if (length(grep("AR\\$Seasonal\\$12\\$", names(diag_spc))) != 0) {
    aux0 <- aux1 <- aux2 <- aux3 <- gsub("AR\\$Seasonal\\$12\\$", "", names(diag_spc)[grep("AR\\$Seasonal\\$12\\$", names(diag_spc))])
    for (i in 1:length(aux0)) {
      diag_spc[[grep("AR\\$Seasonal\\$12\\$", names(diag_spc))[i]]] <- strsplit(diag_spc[[grep("AR\\$Seasonal\\$12\\$", names(diag_spc))[i]]], " ")[[1]]
      aux0[i] <- paste(aux0[i], "[", round(as.numeric(diag_spc[[grep("AR\\$Seasonal\\$12\\$", names(diag_spc))[i]]][1]), 4), ", ",
                       round(as.numeric(diag_spc[[grep("AR\\$Seasonal\\$12\\$", names(diag_spc))[i]]][2]), 5), "]", sep = "")
      aux1[i] <- ifelse(abs(as.numeric(diag_spc[[grep("AR\\$Seasonal\\$12\\$", names(diag_spc))[i]]][1])) < 2*abs(as.numeric(diag_spc[[grep("AR\\$Seasonal\\$12\\$", names(diag_spc))[i]]][2])),
                        paste("sar(", aux1[i], ") no significativo", sep = ""), "")
      aux2[i] <- ifelse(abs(as.numeric(diag_spc[[grep("AR\\$Seasonal\\$12\\$", names(diag_spc))[i]]][1])) >= 0.95, "Parámetro con valor cercano a 1 o superior", "")
      aux3[i] <- as.numeric(diag_spc[[grep("AR\\$Seasonal\\$12\\$", names(diag_spc))[i]]][1])
    }
    aux3 <- ifelse(sum(as.numeric(aux3)) >= 0.95, "Suma de los valores de los parámetros cercano a 1 o superior", ""); aux4 <- c(aux1, aux2, aux3)
    aux4 <- aux4[which(aux4 != "")]
    color_rojo$SAR <- paste(aux4, collapse = ", ")
    diag_spc$SAR <- paste(aux0, collapse = " ") } else {
      if (length(grep("AR\\$Seasonal\\$04\\$", names(diag_spc))) != 0) {
        aux0 <- aux1 <- aux2 <- aux3 <- gsub("AR\\$Seasonal\\$04\\$", "", names(diag_spc)[grep("AR\\$Seasonal\\$04\\$", names(diag_spc))])
        for (i in 1:length(aux0)) {
          diag_spc[[grep("AR\\$Seasonal\\$04\\$", names(diag_spc))[i]]] <- strsplit(diag_spc[[grep("AR\\$Seasonal\\$04\\$", names(diag_spc))[i]]], " ")[[1]]
          aux0[i] <- paste(aux0[i], "[", round(as.numeric(diag_spc[[grep("AR\\$Seasonal\\$04\\$", names(diag_spc))[i]]][1]), 4), ", ",
                           round(as.numeric(diag_spc[[grep("AR\\$Seasonal\\$04\\$", names(diag_spc))[i]]][2]), 5), "]", sep = "")
          aux1[i] <- ifelse(abs(as.numeric(diag_spc[[grep("AR\\$Seasonal\\$04\\$", names(diag_spc))[i]]][1])) < 2*abs(as.numeric(diag_spc[[grep("AR\\$Seasonal\\$04\\$", names(diag_spc))[i]]][2])),
                            paste("sar(", aux1[i], ") no significativo", sep = ""), "")
          aux2[i] <- ifelse(abs(as.numeric(diag_spc[[grep("AR\\$Seasonal\\$04\\$", names(diag_spc))[i]]][1])) >= 0.95, "Parámetro con valor cercano a 1 o superior", "")
          aux3[i] <- as.numeric(diag_spc[[grep("AR\\$Seasonal\\$04\\$", names(diag_spc))[i]]][1])
        }
        aux3 <- ifelse(sum(as.numeric(aux3)) >= 0.95, "Suma de los valores de los parámetros cercano a 1 o superior", ""); aux4 <- c(aux1, aux2, aux3)
        aux4 <- aux4[which(aux4 != "")]
        color_rojo$SAR <- paste(aux4, collapse = ", ")
        diag_spc$SAR <- paste(aux0, collapse = " ") } else { diag_spc$SAR <- "" }
    }
  if (length(grep("MA\\$Nonseasonal\\$01\\$", names(diag_spc))) != 0) {
    aux0 <- aux1 <- aux2 <- aux3 <- gsub("MA\\$Nonseasonal\\$01\\$", "", names(diag_spc)[grep("MA\\$Nonseasonal\\$01\\$", names(diag_spc))])
    for (i in 1:length(aux0)) {
      diag_spc[[grep("MA\\$Nonseasonal\\$01\\$", names(diag_spc))[i]]] <- strsplit(diag_spc[[grep("MA\\$Nonseasonal\\$01\\$", names(diag_spc))[i]]], " ")[[1]]
      aux0[i] <- paste(aux0[i], "[", round(as.numeric(diag_spc[[grep("MA\\$Nonseasonal\\$01\\$", names(diag_spc))[i]]][1]), 4), ", ",
                       round(as.numeric(diag_spc[[grep("MA\\$Nonseasonal\\$01\\$", names(diag_spc))[i]]][2]), 5), "]", sep = "")
      aux1[i] <- ifelse(abs(as.numeric(diag_spc[[grep("MA\\$Nonseasonal\\$01\\$", names(diag_spc))[i]]][1])) < 2*abs(as.numeric(diag_spc[[grep("MA\\$Nonseasonal\\$01\\$", names(diag_spc))[i]]][2])),
                        paste("ma(", aux1[i], ") no significativo", sep = ""), "")
      aux2[i] <- ifelse(abs(as.numeric(diag_spc[[grep("MA\\$Nonseasonal\\$01\\$", names(diag_spc))[i]]][1])) >= 1, "Modelo ARIMA no causal", "")
      aux3[i] <- as.numeric(diag_spc[[grep("MA\\$Nonseasonal\\$01\\$", names(diag_spc))[i]]][1])
    }
    aux3 <- ifelse(sum(as.numeric(aux3)) >= 1, "Modelo ARIMA no causal", ""); aux4 <- c(aux1, aux2, aux3)
    aux4 <- aux4[which(aux4 != "")]
    color_rojo$MA <- paste(aux4, collapse = ", ")
    diag_spc$MA <- paste(aux0, collapse = " ") } else { diag_spc$MA <- "" }
  if (length(grep("MA\\$Seasonal\\$12\\$", names(diag_spc))) != 0) {
    aux0 <- aux1 <- aux2 <- aux3 <- gsub("MA\\$Seasonal\\$12\\$", "", names(diag_spc)[grep("MA\\$Seasonal\\$12\\$", names(diag_spc))])
    for (i in 1:length(aux0)) {
      diag_spc[[grep("MA\\$Seasonal\\$12\\$", names(diag_spc))[i]]] <- strsplit(diag_spc[[grep("MA\\$Seasonal\\$12\\$", names(diag_spc))[i]]], " ")[[1]]
      aux0[i] <- paste(aux0[i], "[", round(as.numeric(diag_spc[[grep("MA\\$Seasonal\\$12\\$", names(diag_spc))[i]]][1]), 4), ", ",
                       round(as.numeric(diag_spc[[grep("MA\\$Seasonal\\$12\\$", names(diag_spc))[i]]][2]), 5), "]", sep = "")
      aux1[i] <- ifelse(abs(as.numeric(diag_spc[[grep("MA\\$Seasonal\\$12\\$", names(diag_spc))[i]]][1])) < 2*abs(as.numeric(diag_spc[[grep("MA\\$Seasonal\\$12\\$", names(diag_spc))[i]]][2])),
                        paste("sma(", aux1[i], ") no significativo", sep = ""), "")
      aux2[i] <- ifelse(abs(as.numeric(diag_spc[[grep("MA\\$Seasonal\\$12\\$", names(diag_spc))[i]]][1])) >= 0.95, "Parámetro con valor cercano a 1 o superior", "")
      aux3[i] <- as.numeric(diag_spc[[grep("MA\\$Seasonal\\$12\\$", names(diag_spc))[i]]][1])
    }
    aux3 <- ifelse(sum(as.numeric(aux3)) >= 0.95, "Suma de los valores de los parámetros cercano a 1 o superior", ""); aux4 <- c(aux1, aux2, aux3)
    aux4 <- aux4[which(aux4 != "")]
    color_rojo$SMA <- paste(aux4, collapse = ", ")
    diag_spc$SMA <- paste(aux0, collapse = " ") } else {
      if (length(grep("MA\\$Seasonal\\$04\\$", names(diag_spc))) != 0) {
        aux0 <- aux1 <- aux2 <- aux3 <- gsub("MA\\$Seasonal\\$04\\$", "", names(diag_spc)[grep("MA\\$Seasonal\\$04\\$", names(diag_spc))])
        for (i in 1:length(aux0)) {
          diag_spc[[grep("MA\\$Seasonal\\$04\\$", names(diag_spc))[i]]] <- strsplit(diag_spc[[grep("MA\\$Seasonal\\$04\\$", names(diag_spc))[i]]], " ")[[1]]
          aux0[i] <- paste(aux0[i], "[", round(as.numeric(diag_spc[[grep("MA\\$Seasonal\\$04\\$", names(diag_spc))[i]]][1]), 4), ", ",
                           round(as.numeric(diag_spc[[grep("MA\\$Seasonal\\$04\\$", names(diag_spc))[i]]][2]), 5), "]", sep = "")
          aux1[i] <- ifelse(abs(as.numeric(diag_spc[[grep("MA\\$Seasonal\\$04\\$", names(diag_spc))[i]]][1])) < 2*abs(as.numeric(diag_spc[[grep("MA\\$Seasonal\\$04\\$", names(diag_spc))[i]]][2])),
                            paste("sma(", aux1[i], ") no significativo", sep = ""), "")
          aux2[i] <- ifelse(abs(as.numeric(diag_spc[[grep("MA\\$Seasonal\\$04\\$", names(diag_spc))[i]]][1])) >= 0.95, "Parámetro con valor cercano a 1 o superior", "")
          aux3[i] <- as.numeric(diag_spc[[grep("MA\\$Seasonal\\$04\\$", names(diag_spc))[i]]][1])
        }
        aux3 <- ifelse(sum(as.numeric(aux3)) >= 0.95, "Suma de los valores de los parámetros cercano a 1 o superior", ""); aux4 <- c(aux1, aux2, aux3)
        aux4 <- aux4[which(aux4 != "")]
        color_rojo$SMA <- paste(aux4, collapse = ", ")
        diag_spc$SMA <- paste(aux0, collapse = " ") } else { diag_spc$SMA <- "" }
    }
  if (length(grep("skewness", names(diag_spc))) != 0) {
    if (is.null(diag_spc$kurtosis)) {
      diag_spc$normal <- "?"
    } else {
      diag_spc$skewness <- strsplit(diag_spc$skewness, " ")[[1]]
      diag_spc$a <- strsplit(diag_spc$a, " ")[[1]]
      diag_spc$kurtosis <- strsplit(diag_spc$kurtosis, " ")[[1]]
      if (length(diag_spc$skewness) == 1) {
        if (length(diag_spc$a) == 1 & length(diag_spc$kurtosis) == 1) { diag_spc$normal <- "ok" } else { diag_spc$normal <- "failed" }
      } else {
        if (diag_spc$skewness[2] == "+") { diag_spc$normal <- "skewed +" } else { diag_spc$normal <- "skewed -" }
      }
    } } else { diag_spc$normal <- "" }
  if (length(grep("sigacf\\$", names(diag_spc))) != 0) {
    aux0 <- gsub("sigacf\\$", "", names(diag_spc)[grep("sigacf\\$", names(diag_spc))])
    for (i in 1:length(aux0)) {
      diag_spc[grep("sigacf\\$", names(diag_spc))][[i]] <- strsplit(diag_spc[grep("sigacf\\$", names(diag_spc))][[i]], " ")[[1]]
      if (abs(as.numeric(diag_spc[grep("sigacf\\$", names(diag_spc))][[i]][1])) < 
          2 * abs(as.numeric(diag_spc[grep("sigacf\\$", names(diag_spc))][[i]][2]))) { 
        aux0[i] <- NA }
    }
    if (length(aux0[!is.na(aux0)]) != 0) {
      diag_spc$ACF <- paste(aux0[!is.na(aux0)], collapse = " ")
      aux1 <- ifelse(length(aux0[!is.na(aux0)]) > 5, "Muchas lags en ACF significativas", "")
      if (diag_spc$freq == "12") {
        aux2 <- ifelse(sum(grepl("01", aux0),grepl("02", aux0),grepl("03", aux0),grepl("04", aux0),grepl("12", aux0),grepl("24", aux0)) > 0,
                       "Alguna de las siguientes lag en ACF significativa: 1, 2, 3, 4, 12 o 24", "")
      } else {
        aux2 <- ifelse(sum(grepl("01", aux0),grepl("02", aux0),grepl("03", aux0),grepl("04", aux0)) > 0,
                       "Alguna de las siguientes lag en ACF significativa: 1, 2, 3 o 4", "")
      }
      aux3 <- c(aux1, aux2); aux3 <- aux3[which(aux3 != "")]
      color_rojo$ACF <- paste(aux3, collapse = ", ")
    } else { diag_spc$ACF <- "" } } else { diag_spc$ACF <- "" }
  if (length(grep("sigpacf\\$", names(diag_spc))) != 0) {
    aux0 <- gsub("sigpacf\\$", "", names(diag_spc)[grep("sigpacf\\$", names(diag_spc))])
    for (i in 1:length(aux0)) {
      diag_spc[grep("sigpacf\\$", names(diag_spc))][[i]] <- strsplit(diag_spc[grep("sigpacf\\$", names(diag_spc))][[i]], " ")[[1]]
      if (abs(as.numeric(diag_spc[grep("sigpacf\\$", names(diag_spc))][[i]][1])) < 
          2 * abs(as.numeric(diag_spc[grep("sigpacf\\$", names(diag_spc))][[i]][2]))) { 
        aux0[i] <- NA }
    }
    if (length(aux0[!is.na(aux0)]) != 0) {
      diag_spc$PACF <- paste(aux0[!is.na(aux0)], collapse = " ")
      aux1 <- ifelse(length(aux0[!is.na(aux0)]) > 5, "Muchas lags en PACF significativas", "")
      if (diag_spc$freq == "12") {
        aux2 <- ifelse(sum(grepl("01", aux0),grepl("02", aux0),grepl("03", aux0),grepl("04", aux0),grepl("12", aux0),grepl("24", aux0)) > 0,
                       "Alguna de las siguientes lag en PACF significativa: 1, 2, 3, 4, 12 o 24", "")
      } else {
        aux2 <- ifelse(sum(grepl("01", aux0),grepl("02", aux0),grepl("03", aux0),grepl("04", aux0)) > 0,
                       "Alguna de las siguientes lag en PACF significativa: 1, 2, 3 o 4", "")
      }
      aux3 <- c(aux1, aux2); aux3 <- aux3[which(aux3 != "")]
      color_rojo$PACF <- paste(aux3, collapse = ", ")
    } else { diag_spc$PACF <- "" } } else { diag_spc$PACF <- "" }
  if (length(grep("peaks.td", names(diag_spc))) != 0) {
    if (grepl("rsd", diag_spc$peaks.td)) {
      aux0 <- c("spcrsd.t1", "spcrsd.t2")
      aux00 <- c("t1", "t2")
      for (i in 1:length(aux0)) {
        diag_rsd <- strsplit(diag_spc[[aux0[i]]], " ")[[1]]
        if (length(diag_rsd) == 2) {
          if (as.numeric(diag_rsd[1]) < as.numeric(diag_spc$siglevel)) {
            aux00[i] <- ""
          } else {
            aux00[i] <- paste(aux00[i], "[", diag_rsd[1], diag_rsd[2], "]", sep = "")
          }
        } else {
          aux00[i] <- ""
        }
      }
      aux1 <- aux00[aux00 != ""]
    } else { aux1 <- "" }
    if (grepl("rsd", diag_spc$peaks.seas)) {
      aux0 <- c("spcrsd.s1", "spcrsd.s2", "spcrsd.s3", "spcrsd.s4", "spcrsd.s5")
      aux00 <- c("s1", "s2", "s3", "s4", "s5")
      for (i in 1:length(aux0)) {
        diag_rsd <- strsplit(diag_spc[[aux0[i]]], " ")[[1]]
        if (length(diag_rsd) == 2) {
          if (as.numeric(diag_rsd[1]) < as.numeric(diag_spc$siglevel)) {
            aux00[i] <- ""
          } else {
            aux00[i] <- paste(aux00[i], "[", diag_rsd[1], diag_rsd[2], "]", sep = "")
          }
        } else {
          aux00[i] <- ""
        }
      }
      aux2 <- aux00[aux00 != ""]
    } else { aux2 <- "" }
    aux3 <- union(aux1, aux2)
    diag_spc$rsd_peak <- paste(aux3[aux3 != ""], collapse = " ")
    aux0 <- diag_spc$rsd_peak
    color_rojo$Peak_rsd <- ifelse(sum(grepl("t1", aux0),grepl("s1", aux0),grepl("s2", aux0),grepl("s3", aux0),grepl("s4", aux0)) > 0,
                                  "Algún pico en el espectro de los residuales", "")
    } else { diag_spc$rsd_peak <- "" }
  if (length(grep("spcori.s1", names(diag_spc))) != 0) {
    aux0 <- c("spcori.t1", "spcori.t2", "spcori.s1", "spcori.s2", "spcori.s3", "spcori.s4", "spcori.s5")
    aux00 <- c("t1", "t2", "s1", "s2", "s3", "s4", "s5")
    for (i in 1:length(aux0)) {
      if (is.null(diag_spc[[aux0[i]]])) {
        aux00[i] <- ""
      } else {
        diag_ori <- strsplit(diag_spc[[aux0[i]]], " ")[[1]]
        if (length(diag_ori) == 2) {
          if (as.numeric(diag_ori[1]) >= as.numeric(diag_spc$siglevel)) {
            aux00[i] <- paste(aux00[i], sep = "")
          } else { aux00[i] <- "" }
        } else {
          aux00[i] <- ""
        }
      }
    }
    aux1 <- aux00[aux00 != ""]
    diag_spc$ori_peak <- paste(aux1[aux1 != ""], collapse = " ")
    } else { diag_spc$ori_peak <- "" }
  if (length(grep("spcori.tukey.s1", names(diag_spc))) != 0) {
    aux0 <- c("spcori.tukey.s1", "spcori.tukey.s2", "spcori.tukey.s3", "spcori.tukey.s4", "spcori.tukey.s5", "spcori.tukey.s6", "spcori.tukey.td")
    aux00 <- c("s1", "s2", "s3", "s4", "s5", "s6", "td")
    for (i in 1:length(aux0)) {
      if (is.null(diag_spc[[aux0[i]]])) {
        aux00[i] <- ""
      } else {
        if (as.numeric(diag_spc[[aux0[i]]]) >= 0.90) {
          aux00[i] <- paste(aux00[i], sep = "")
        } else { aux00[i] <- "" }
      }
    }
    aux1 <- aux00[aux00 != ""]
    diag_spc$tukey_ori_peak <- paste("(p>.90)[", paste(aux1[aux1 != ""], collapse = " "), "]", sep = "")
    } else { diag_spc$tukey_ori_peak <- "" }
  if (length(grep("peaks.td", names(diag_spc))) != 0) {
    if (grepl("sa", diag_spc$peaks.td)) {
      aux0 <- c("spcsa.t1", "spcsa.t2")
      aux00 <- c("t1", "t2")
      for (i in 1:length(aux0)) {
        diag_g1 <- strsplit(diag_spc[[aux0[i]]], " ")[[1]]
        if (length(diag_g1) == 2) {
          if (as.numeric(diag_g1[1]) < as.numeric(diag_spc$siglevel)) {
            aux00[i] <- ""
          } else {
            aux00[i] <- paste(aux00[i], "[", diag_g1[1], diag_g1[2], "]", sep = "")
          }
        } else {
          aux00[i] <- ""
        }
      }
      aux1 <- aux00[aux00 != ""]
    } else { aux1 <- "" }
    if (grepl("sa", diag_spc$peaks.seas)) {
      aux0 <- c("spcsa.s1", "spcsa.s2", "spcsa.s3", "spcsa.s4", "spcsa.s5")
      aux00 <- c("s1", "s2", "s3", "s4", "s5")
      for (i in 1:length(aux0)) {
        diag_g1 <- strsplit(diag_spc[[aux0[i]]], " ")[[1]]
        if (length(diag_g1) == 2) {
          if (as.numeric(diag_g1[1]) < as.numeric(diag_spc$siglevel)) {
            aux00[i] <- ""
          } else {
            aux00[i] <- paste(aux00[i], "[", diag_g1[1], diag_g1[2], "]", sep = "")
          }
        } else {
          aux00[i] <- ""
        }
      }
      aux2 <- aux00[aux00 != ""]
    } else { aux2 <- "" }
    aux3 <- union(aux1, aux2)
    diag_spc$G1_peak <- paste(aux3[aux3 != ""], collapse = " ")
    aux0 <- diag_spc$G1_peak
    color_rojo$Peak_sa <- ifelse(sum(grepl("t1", aux0),grepl("s1", aux0),grepl("s2", aux0),grepl("s3", aux0),grepl("s4", aux0)) > 0,
                                  "Algún pico en el espectro de la serie desestacionalizada", "")
    } else { diag_spc$G1_peak <- "" }
  if (length(grep("peaks.td", names(diag_spc))) != 0) {
    if (grepl("irr", diag_spc$peaks.td)) {
      aux0 <- c("spcirr.t1", "spcirr.t2")
      aux00 <- c("t1", "t2")
      for (i in 1:length(aux0)) {
        diag_g2 <- strsplit(diag_spc[[aux0[i]]], " ")[[1]]
        if (length(diag_g2) == 2) {
          if (as.numeric(diag_g2[1]) < as.numeric(diag_spc$siglevel)) {
            aux00[i] <- ""
          } else {
            aux00[i] <- paste(aux00[i], "[", diag_g2[1], diag_g2[2], "]", sep = "")
          }
        } else {
          aux00[i] <- ""
        }
      }
      aux1 <- aux00[aux00 != ""]
    } else { aux1 <- "" }
    if (grepl("irr", diag_spc$peaks.seas)) {
      aux0 <- c("spcirr.s1", "spcirr.s2", "spcirr.s3", "spcirr.s4", "spcirr.s5")
      aux00 <- c("s1", "s2", "s3", "s4", "s5")
      for (i in 1:length(aux0)) {
        diag_g2 <- strsplit(diag_spc[[aux0[i]]], " ")[[1]]
        if (length(diag_g2) == 2) {
          if (as.numeric(diag_g2[1]) < as.numeric(diag_spc$siglevel)) {
            aux00[i] <- ""
          } else {
            aux00[i] <- paste(aux00[i], "[", diag_g2[1], diag_g2[2], "]", sep = "")
          }
        } else {
          aux00[i] <- ""
        }
      }
      aux2 <- aux00[aux00 != ""]
    } else { aux2 <- "" }
    aux3 <- union(aux1, aux2)
    diag_spc$G2_peak <- paste(aux3[aux3 != ""], collapse = " ")
    aux0 <- diag_spc$G2_peak
    color_rojo$Peak_irr <- ifelse(sum(grepl("t1", aux0),grepl("s1", aux0),grepl("s2", aux0),grepl("s3", aux0),grepl("s4", aux0)) > 0,
                                 "Algún pico en el espectro del componente irregular", "")
    } else { diag_spc$G2_peak <- "" }
  if (length(grep("peaks.seas", names(diag_spc))) != 0) {
    aux0 <- c("spcrsd.s1", "spcrsd.s2", "spcrsd.s3", "spcrsd.s4", "spcrsd.s5",
              "spcsa.s1", "spcsa.s2", "spcsa.s3", "spcsa.s4", "spcsa.s5",
              "spcirr.s1", "spcirr.s2", "spcirr.s3", "spcirr.s4", "spcirr.s5")
    aux00 <- c("rsd.s1", "rsd.s2", "rsd.s3", "rsd.s4", "rsd.s5",
               "sa.s1", "sa.s2", "sa.s3", "sa.s4", "sa.s5",
               "irr.s1", "irr.s2", "irr.s3", "irr.s4", "irr.s5")
    for (i in 1:length(aux0)) {
      if (is.null(diag_spc[[aux0[i]]])) {
        aux00[i] <- ""
      } else {
        diag_non_seas <- strsplit(diag_spc[[aux0[i]]], " ")[[1]]
        if (length(diag_non_seas) == 2) {
          if (as.numeric(diag_non_seas[1]) >= as.numeric(diag_spc$siglevel)) {
            aux00[i] <- ""
          } else {
            aux00[i] <- paste(aux00[i], "[", diag_non_seas[1], diag_non_seas[2], "]", sep = "")
          }
        } else {
          aux00[i] <- ""
        }
      }
    }
    aux2 <- aux00[aux00 != ""]
    diag_spc$non_seas_peak <- paste(aux2[aux2 != ""], collapse = " ")
    } else { diag_spc$non_seas_peak <- "" }
  if (length(grep("peaks.td", names(diag_spc))) != 0) {
    aux0 <- c("spcrsd.t1", "spcrsd.t2", "spcsa.t1", "spcsa.t2", "spcirr.t1", "spcirr.t2")
    aux00 <- c("rsd.t1", "rsd.t2", "sa.t1", "sa.t2", "irr.t1", "irr.t2")
    for (i in 1:length(aux0)) {
      if (is.null(diag_spc[[aux0[i]]])) {
        aux00[i] <- ""
      } else {
        diag_non_td <- strsplit(diag_spc[[aux0[i]]], " ")[[1]]
        if (length(diag_non_td) == 2) {
          if (as.numeric(diag_non_td[1]) >= as.numeric(diag_spc$siglevel)) {
            aux00[i] <- ""
          } else {
            aux00[i] <- paste(aux00[i], "[", diag_non_td[1], diag_non_td[2], "]", sep = "")
          }
        } else {
          if (diag_non_td != "nopeak"){ aux00[i] <- paste(aux00[i], "[", diag_non_td[1], "]", sep = "") } else { aux00[i] <- "" }
        }
      }
    }
    aux2 <- aux00[aux00 != ""]
    diag_spc$non_td_peak <- paste(aux2[aux2 != ""], collapse = " ")
    } else { diag_spc$non_td_peak <- "" }
  if (length(grep("crit", names(diag_spc))) != 0) {
    diag_spc$crit_val <- paste(diag_spc$aocrit,"/",diag_spc$lscrit,"/",diag_spc$tccrit, sep = "")
    } else { diag_spc$crit_val <- "--|--|--" }
  if (!is.null(diag_spc$seasonalma)) {
    diag_spc$seasonalma <- strsplit(diag_spc$seasonalma, " ")[[1]]
    if (sum(grepl(diag_spc$seasonalma[1], diag_spc$seasonalma)) == length(diag_spc$seasonalma)) { diag_spc$seasonalma <- diag_spc$seasonalma[1]}
    } else { diag_spc$seasonalma <- "" }
  
  
  vector <- c("freq", "transform", "samode", "span", "outlier.total", "niter", "nfcst", "modelspan", "arimamdl", "ftest$Trading Day",
              "1-Coefficient Trading Day$Weekday", "Leap Year$Leap Year", "aictest.td", "variance$mle", "aicc", "aape.0",
              "nlbq", "lblags", "nbpq", "bplags", "qsrsd", "qssrsd", "siglim", "finaltrendma", "f2.is", "trendma",
              "f2.ic", "f2.fsd8", "d11.f", "d11.3y.f", "f3.m01", "f3.m02", "f3.m03", "f3.m04", "f3.m05", "f3.m06",
              "f3.m07", "f3.m08", "f3.m09", "f3.m10", "f3.m11", "f3.q", "f3.qm2", "qsori", "qssori", "qsorievadj", "qssorievadj",
              "qssadj", "qsssadj", "qssadjevadj", "qsssadjevadj", "qsirr", "qssirr", "qsirrevadj", "qssirrevadj", "sfmsr")
  # El "easter"-"outliers"-"rompimiento estacional"-"autooutliers"-"AR,SAR,MA,SMA"-"picos rsd,ori,G1,G2" no entran.
  
  for (i in vector) {
    if (is.null(diag_spc[[i]])) {
      diag_spc[[i]] <- ""
    } else {
      diag_spc[[i]] <- strsplit(diag_spc[[i]], " ")[[1]]
    }
  }
  
  
  general <- data.frame(`Series Name` = nombre_serie,
                        Period = diag_spc$freq,
                        Transform = ifelse(length(diag_spc$transform) > 1, paste(diag_spc$transform, collapse = " "), diag_spc$transform),
                        Mode = diag_spc$samode[1],
                        Span = paste(diag_spc$span, collapse = " "),
                        `AO-LS-TC Crit Val` = diag_spc$crit_val,
                        `Num Outliers` = diag_spc$outlier.total,
                        `Num Iter` = diag_spc$niter,
                        `Num Forecasts` = diag_spc$nfcst,
                        `Forecast Mode` = diag_spc$aape.mode,
                        `Date-Time` = paste(diag_spc$date,diag_spc$time, sep = ", ")
                        )
  
  model_info <- data.frame(`Series Name` = nombre_serie,
                           `Model Span` = paste(diag_spc$modelspan, collapse = " "),
                           `ARIMA Model` = paste(diag_spc$arimamdl, collapse = " "),
                           `Trading Day` = paste(diag_spc$`ftest$Trading Day`,
                                                 diag_spc$`1-Coefficient Trading Day$Weekday`,
                                                 diag_spc$`Leap Year$Leap Year`,
                                                 diag_spc$aictest.td, sep = ""),
                           Holiday = ifelse(length(grep("Easter", names(diag_spc))) != 0, diag_spc[[grep("Easter", names(diag_spc))]], ""),
                           Seasonal = ifelse(length(grep("ftest\\$Seasonal", names(diag_spc))) != 0, diag_spc[[grep("ftest\\$Seasonal", names(diag_spc))]], ""),
                           `Coded Outliers` = diag_spc$outliers,
                           `Auto Outliers` = diag_spc$autooutliers,
                           Variance = diag_spc$`variance$mle`,
                           AR = diag_spc$AR,
                           SAR = diag_spc$SAR,
                           MA = diag_spc$MA,
                           SMA = diag_spc$SMA
                           )
  
  model_diagnostics <- data.frame(`Series Name` = nombre_serie,
                                  AICC = diag_spc$aicc,
                                  `aa FcE 3-yr` = diag_spc$aape.0,
                                  Normal = diag_spc$normal,
                                  `Num LBQ Fail` = diag_spc$nlbq,
                                  `Sig LBQ` = paste(diag_spc$lblags, collapse = " "),
                                  `Num BPQ Fail` = diag_spc$nbpq,
                                  `Sig BPQ` = paste(diag_spc$bplags, collapse = " "),
                                  `Sig ACF` = diag_spc$ACF,
                                  `Sig PACF` = diag_spc$PACF,
                                  `Resid Peaks` = diag_spc$rsd_peak,
                                  `QS Resid` = ifelse(length(diag_spc$qsrsd) > 1, diag_spc$qsrsd[2], ""),
                                  `QSS Resid` = ifelse(length(diag_spc$qssrsd) > 1, diag_spc$qssrsd[2], "")
                                  )
  
  x11 <- data.frame(`Series Name` = nombre_serie,
                    `Sigma Lim` = paste(diag_spc$siglim, collapse = " "),
                    `Seasonal MA` = ifelse(grepl("x", diag_spc$seasonalma[1]), paste(diag_spc$seasonalma, collapse = " "),
                                           paste(diag_spc$sfmsr, "*", sep = " ")),
                    `Trend MA` = ifelse(diag_spc$finaltrendma == "", diag_spc$trendma, paste(diag_spc$finaltrendma, "**", sep = " ")),
                    `IS Ratio` = diag_spc$f2.is,
                    `IC Ratio` = diag_spc$f2.ic,
                    D8F = diag_spc$f2.fsd8[[1]],
                    `D8F p-val` = diag_spc$f2.fsd8[[2]],
                    D11F = as.numeric(diag_spc$d11.f[[2]])/100,
                    `D11F 3yr` = as.numeric(diag_spc$d11.3y.f[[2]])/100,
                    M1 = diag_spc$f3.m01,
                    M2 = diag_spc$f3.m02,
                    M3 = diag_spc$f3.m03,
                    M4 = diag_spc$f3.m04,
                    M5 = diag_spc$f3.m05,
                    M6 = diag_spc$f3.m06,
                    M7 = diag_spc$f3.m07,
                    M8 = diag_spc$f3.m08,
                    M9 = diag_spc$f3.m09,
                    M10 = diag_spc$f3.m10,
                    M11 = diag_spc$f3.m11,
                    Q = diag_spc$f3.q,
                    Q2 = diag_spc$f3.qm2
                    )
  
  spectrum_QS <- data.frame(`Series Name` = nombre_serie,
                            `Sig Ori Peaks` = diag_spc$ori_peak,
                            `QS Ori` = ifelse(length(diag_spc$qsori) > 1, diag_spc$qsori[[2]], ""),
                            `QSS Ori` = ifelse(length(diag_spc$qssori) > 1, diag_spc$qssori[[2]], ""),
                            `QS Ori Adj Ext` = ifelse(length(diag_spc$qsorievadj) > 1, diag_spc$qsorievadj[[2]], ""),
                            `QSS Ori Adj Ext` = ifelse(length(diag_spc$qssorievadj) > 1, diag_spc$qssorievadj[[2]], ""),
                            `Tukey Ori Peaks` = diag_spc$tukey_ori_peak,
                            `Sig SAdj Peaks` = diag_spc$G1_peak,
                            `Sig Irr Peaks` = diag_spc$G2_peak,
                            `Nonsig Seasonal Peaks` = diag_spc$non_seas_peak,
                            `Nonsig TD Peaks` = diag_spc$non_td_peak,
                            `QS Sadj` = ifelse(length(diag_spc$qssadj) > 1, diag_spc$qssadj[[2]], ""),
                            `QSS Sadj` = ifelse(length(diag_spc$qsssadj) > 1, diag_spc$qsssadj[[2]], ""),
                            `QS Sadj Adj Ext` = ifelse(length(diag_spc$qssadjevadj) > 1, diag_spc$qssadjevadj[[2]], ""),
                            `QSS Sadj Adj Ext` = ifelse(length(diag_spc$qsssadjevadj) > 1, diag_spc$qsssadjevadj[[2]], ""),
                            `QS Irr` = ifelse(length(diag_spc$qsirr) > 1, diag_spc$qsirr[[2]], ""),
                            `QSS Irr` = ifelse(length(diag_spc$qssirr) > 1, diag_spc$qssirr[[2]], ""),
                            `QS Irr Adj Ext` = ifelse(length(diag_spc$qsirrevadj) > 1, diag_spc$qsirrevadj[[2]], ""),
                            `QSS Irr Adj Ext` = ifelse(length(diag_spc$qssirrevadj) > 1, diag_spc$qssirrevadj[[2]], "")
                            )
  
  # Código para colorear diagnóstico
  if (as.numeric(diag_spc$nlbq) > 7) {
    color_rojo$LBQ <- "Más lags con LBQ significativo de lo recomendado"
  } else { color_rojo$LBQ <- "" }
  if (!is.na(diag_spc$qsrsd[2])) {
    if (as.numeric(diag_spc$qsrsd[2]) < 0.01) { color_rojo$QS_rsd <- "QS para residuales significativo" }
  }
  if (!is.na(diag_spc$qssrsd[2])) {
    if (as.numeric(diag_spc$qssrsd[2]) < 0.01) { color_rojo$QS_rsd <- "QS para residuales significativo" }
  }
  if (!is.na(diag_spc$qssadj[2])) {
    if (as.numeric(diag_spc$qssadj[2]) < 0.01) { color_rojo$QS_sa <- "QS para serie desestacionalizada significativo" }
  }
  if (!is.na(diag_spc$qsssadj[2])) {
    if (as.numeric(diag_spc$qsssadj[2]) < 0.01) { color_rojo$QS_sa <- "QS para serie desestacionalizada significativo" }
  }
  if (!is.na(diag_spc$qsirr[2])) {
    if (as.numeric(diag_spc$qsirr[2]) < 0.01) { color_rojo$QS_irr <- "QS para componente irregular significativo" }
  }
  if (!is.na(diag_spc$qssirr[2])) {
    if (as.numeric(diag_spc$qssirr[2]) < 0.01) { color_rojo$QS_irr <- "QS para componente irregular significativo" }
  }
  
  color_rojo <- paste(color_rojo, ".", sep = "")
  color_rojo <- color_rojo[which(color_rojo != ".")]
  
  return(list("I" = general,
              "II" = model_info,
              "III" = model_diagnostics,
              "IV" = x11,
              "V" = spectrum_QS,
              "rojo" = ifelse(length(color_rojo) != 0, paste(color_rojo, collapse = " - - - - - - - - "), NA)))
  
} #Fin de función "f_diag"

###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ######
###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ######
###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ######
###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ######
###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ###### ------ ######

f_report <- function(diagnostic, ruta_out, critico){ #Inicio función "f_report"
  # con __diagnostic <- f_diag(ruta_udg, nombre_serie)__
  # 1 -- chi
  if(diagnostic$III$Num.LBQ.Fail == 0){
    chi <- ""
  } else {
    if(diagnostic$III$Num.LBQ.Fail > 5){
      chi <- "chi's<0.05"
    } else {
      chi <- paste("(", gsub(" ", ",", diagnostic$III$Sig.LBQ), ")<0.05", sep = "")
    }
  }
  # 2 -- acf
  if(diagnostic$III$Sig.ACF == ""){
    acf <- ""
  } else {
    acf <- diagnostic$III$Sig.ACF
    for (i in c("01","02","03","04","05","06","07","08","09")) { acf <- gsub(i, substring(i, 2), acf) }
    acf <- paste("sale acf ", gsub(" ", ", ", acf), sep = "")
    acf <- sub(" ,", " y ", paste(rev(str_split(acf, "")[[1]]), collapse = ""))
    acf <- paste(rev(str_split(acf, "")[[1]]), collapse = "")
  }
  # 3 -- pacf
  if(diagnostic$III$Sig.PACF == ""){
    pacf <- ""
  } else {
    if (acf == "") {
      pacf <- diagnostic$III$Sig.PACF
      for (i in c("01","02","03","04","05","06","07","08","09")) { pacf <- gsub(i, substring(i, 2), pacf) }
      pacf <- paste("sale pacf ", gsub(" ", ", ", pacf), sep = "")
      pacf <- sub(" ,", " y ", paste(rev(str_split(pacf, "")[[1]]), collapse = ""))
      pacf <- paste(rev(str_split(pacf, "")[[1]]), collapse = "")
    } else {
      pacf <- diagnostic$III$Sig.PACF
      for (i in c("01","02","03","04","05","06","07","08","09")) { pacf <- gsub(i, substring(i, 2), pacf) }
      pacf <- paste("pacf ", gsub(" ", ", ", pacf), sep = "")
      pacf <- sub(" ,", " y ", paste(rev(str_split(pacf, "")[[1]]), collapse = ""))
      pacf <- paste(rev(str_split(pacf, "")[[1]]), collapse = "")
    }
  }
  # 4 -- correlacion
  OUT <- readLines(paste(ruta_out, "SPC_temp.out", sep = "\\"))
  if (length(which(OUT==" ARMA Parameter Correlation matrix"))!=0) {
    out <- OUT[(which(OUT==" ARMA Parameter Correlation matrix")+3):(which(OUT==" Likelihood Statistics")-2)]
    out[out=="  Nonseasonal AR"] <- "ar"
    out[out=="  Seasonal AR"] <- "sar"
    out[out=="  Nonseasonal MA"] <- "ma"
    out[out=="  Seasonal MA"] <- "sma"
    auxiliar <- which(out=="ar"|out=="sar"|out=="ma"|out=="sma")
    auxiliar01 <- NULL
    for (i in 1:length(auxiliar)) {
      auxiliar02 <- ifelse(i==length(auxiliar),list(out[(auxiliar[i]+1):length(out)]),
                           list(out[(auxiliar[i]+1):(auxiliar[i+1]-1)]))
      names(auxiliar02) <- out[auxiliar[i]]
      auxiliar01 <- c(auxiliar01, auxiliar02)
    }
    auxiliar06 <- data.frame(Parametro_X = NA, Parametro_Y = NA, Correlacion = NA)
    for (j in 1:length(auxiliar01)) {
      auxiliar05 <- data.frame(Parametro_X = NA, Parametro_Y = NA, Correlacion = NA)
      for (k in 1:length(auxiliar01[[j]])) {
        auxiliar03 <- strsplit(paste(names(auxiliar01[j]),auxiliar01[[j]][k]), split = " ")[[1]]
        auxiliar03 <- auxiliar03[auxiliar03!="" & auxiliar03!="Lag"]
        auxiliar04 <- data.frame(Parametro_X = NA, Parametro_Y = NA, Correlacion = NA)
        for (l in 3:length(auxiliar03)) {
          auxiliar07 <- data.frame(Parametro_X = paste(auxiliar03[1],"(",ifelse(auxiliar03[2]=="12","1",
                                                                                ifelse(auxiliar03[2]=="24","2",
                                                                                       auxiliar03[2])),")", sep = ""),
                                   Parametro_Y = l-2, Correlacion = auxiliar03[l])
          auxiliar04 <- rbind(auxiliar04,auxiliar07)
        }
        auxiliar05 <- rbind(auxiliar05,auxiliar04)
      }
      auxiliar06 <- rbind(auxiliar06,auxiliar05)
    }
    auxiliar06 <- auxiliar06[complete.cases(auxiliar06),]
    n <- unique(auxiliar06$Parametro_X)
    for (i in 1:max(auxiliar06$Parametro_Y)) {
      for (j in 1:nrow(auxiliar06)) {
        ifelse(auxiliar06$Parametro_Y[j]==i, auxiliar06$Parametro_Y[j] <- n[i], auxiliar06$Parametro_Y[j])
      }
    }
    auxiliar10 <- NULL
    for (i in 1:nrow(auxiliar06)) {
      ifelse(auxiliar06$Parametro_X[i]!=auxiliar06$Parametro_Y[i] & abs(as.numeric(auxiliar06$Correlacion[i]))>=0.40,
             auxiliar09 <- paste("(",auxiliar06$Parametro_Y[i],",",auxiliar06$Parametro_X[i],")=",auxiliar06$Correlacion[i],sep = ""),
             auxiliar09 <- "")
      auxiliar10 <- c(auxiliar10,auxiliar09)
    }
    auxiliar10 <- auxiliar10[auxiliar10!=""]
    if (length(auxiliar10)==0) {
      correlacion <- ""
    } else {
      auxiliar11 <- NULL
      for (l in 1:length(auxiliar10)) { auxiliar11 <- paste(auxiliar11,auxiliar10[l],sep = ", ") }
      auxiliar11 <- substring(auxiliar11,3,nchar(auxiliar11))
      correlacion <- auxiliar11
    }
  } else {
    correlacion <- ""
  }
  # 5 -- parametros
  if(diagnostic$II$AR == ""){ # AR
    ar <- ""
  } else {
    aux1 <- strsplit(diagnostic$II$AR, "] ")[[1]]
    aux3 <- NULL
    for (i in 1:length(aux1)) {
      aux2 <- strsplit(substring(gsub("]", "",aux1[i]), 4), ", ")[[1]]
      if (abs(as.numeric(aux2[2]) * 2) > abs(as.numeric(aux2[1]))) {
        aux4 <- paste("ar(", as.numeric(substring(aux1[i], 1, 2)), ") ns", sep = "")
      } else {
        aux4 <- ""
      }
      aux3 <- c(aux3, aux4)
    }
    if (length(-which(aux3 == "")) != 0) { aux3 <- aux3[-which(aux3 == "")] }
    ar <- paste(aux3, collapse = ", ")
  }
  if(diagnostic$II$SAR == ""){ # SAR
    sar <- ""
  } else {
    aux1 <- strsplit(diagnostic$II$SAR, "] ")[[1]]
    aux3 <- NULL
    for (i in 1:length(aux1)) {
      aux2 <- strsplit(substring(gsub("]", "",aux1[i]), 4), ", ")[[1]]
      if (abs(as.numeric(aux2[2]) * 2) > abs(as.numeric(aux2[1]))) {
        aux4 <- paste("sar(", as.numeric(substring(aux1[i], 1, 2)), ") ns", sep = "")
      } else {
        aux4 <- ""
      }
      aux3 <- c(aux3, aux4)
    }
    if (length(-which(aux3 == "")) != 0) { aux3 <- aux3[-which(aux3 == "")] }
    sar <- paste(aux3, collapse = ", ")
  }
  if(diagnostic$II$MA == ""){ # MA
    ma <- ""
  } else {
    aux1 <- strsplit(diagnostic$II$MA, "] ")[[1]]
    aux3 <- NULL
    for (i in 1:length(aux1)) {
      aux2 <- strsplit(substring(gsub("]", "",aux1[i]), 4), ", ")[[1]]
      if (abs(as.numeric(aux2[2]) * 2) > abs(as.numeric(aux2[1]))) {
        aux4 <- paste("ma(", as.numeric(substring(aux1[i], 1, 2)), ") ns", sep = "")
      } else {
        aux4 <- ""
      }
      aux3 <- c(aux3, aux4)
    }
    if (length(-which(aux3 == "")) != 0) { aux3 <- aux3[-which(aux3 == "")] }
    ma <- paste(aux3, collapse = ", ")
  }
  if(diagnostic$II$SMA == ""){ # SMA
    sma <- ""
  } else {
    aux1 <- strsplit(diagnostic$II$SMA, "] ")[[1]]
    aux3 <- NULL
    for (i in 1:length(aux1)) {
      aux2 <- strsplit(substring(gsub("]", "",aux1[i]), 4), ", ")[[1]]
      if (abs(as.numeric(aux2[2]) * 2) > abs(as.numeric(aux2[1]))) {
        aux4 <- paste("sma(", as.numeric(substring(aux1[i], 1, 2)), ") ns", sep = "")
      } else {
        aux4 <- ""
      }
      aux3 <- c(aux3, aux4)
    }
    if (length(-which(aux3 == "")) != 0) { aux3 <- aux3[-which(aux3 == "")] }
    sma <- paste(aux3, collapse = ", ")
  }
  parametros <- c(ar, sar, ma, sma)
  parametros <- parametros[-which(parametros == "")]
  parametros <- paste(parametros, collapse = ", ")
  
  # 6 -- pico residuales
  if(diagnostic$III$Resid.Peaks == ""){
    residual <- ""
  } else {
    aux1 <- strsplit(diagnostic$III$Resid.Peaks, "] ")[[1]]
    aux3 <- substring(aux1, 1, 2)
    aux3 <- gsub("s5", "", gsub("t2", "", aux3))
    if (length(-which(aux3 == "")) != 0) { aux3 <- aux3[-which(aux3 == "")] }
    
    if (length(aux3) == 0){
      residual <- ""
    } else {
      if (grepl("t",paste(aux3, collapse = ""))) {
        residual <- paste(paste(gsub("t", "td", aux3), collapse = ", "), " en res", sep = "")
      } else {
        residual <- paste("PER(", paste(aux3, collapse = ","), ")", sep = "")
      }
    }
  }
  # 7 -- pico G1
  if(diagnostic$V$Sig.SAdj.Peaks == ""){
    g1 <- ""
  } else {
    aux1 <- strsplit(diagnostic$V$Sig.SAdj.Peaks, "] ")[[1]]
    aux3 <- substring(aux1, 1, 2)
    aux3 <- gsub("s5", "", gsub("t2", "", aux3))
    if (length(-which(aux3 == "")) != 0) { aux3 <- aux3[-which(aux3 == "")] }
    
    if (length(aux3) == 0){
      g1 <- ""
    } else {
      g1 <- paste(paste(gsub("t", "td", aux3), collapse = ", "), " en G1", sep = "")
    }
  }
  # 8 -- pico G2
  if(diagnostic$V$Sig.Irr.Peaks == ""){
    g2 <- ""
  } else {
    aux1 <- strsplit(diagnostic$V$Sig.Irr.Peaks, "] ")[[1]]
    aux3 <- substring(aux1, 1, 2)
    aux3 <- gsub("s5", "", gsub("t2", "", aux3))
    if (length(-which(aux3 == "")) != 0) { aux3 <- aux3[-which(aux3 == "")] }
    
    if (length(aux3) == 0){
      g2 <- ""
    } else {
      g2 <- paste(paste(gsub("t", "td", aux3), collapse = ", "), " en G2", sep = "")
    }
  }
  # 9 -- rompimiento
  if(diagnostic$II$Seasonal == ""){
    Romp <- ""
  } else {
    Romp <- cbind(Indicador = "name", final = diagnostic$II$Seasonal) #obtenemos la información del _DIAGNOSTICO_
    
    S_Romp <- data.frame(Indicador = NA, SEG = NA, final = NA) #definimos un data frame vacío con los mismos nombres
    for (i in 1:nrow(Romp)) { #con este "for" aislamos fila por fila el cuadro "Romp"
      auxiliar <- as.vector(Romp[i,]) #se toma la fila "i" del cuadro "Romp"
      if (auxiliar[2]!="") { #con este "if" se valida si hay o no hay Romp, es decir, si tiene valores de character vacíos ("")
        condition <- substring(auxiliar[2],
                               which(substring(auxiliar[2],1:nchar(auxiliar[2]),1:nchar(auxiliar[2]))=="["),
                               which(substring(auxiliar[2],1:nchar(auxiliar[2]),1:nchar(auxiliar[2]))=="]")) #obtenemos de la cadena del Romp solo lo que está dentro de los corchetes
        if (sum(substring(condition,1:nchar(condition),1:nchar(condition))=="p")==1) { #con este "if" se valida si se trata del valor "p" o "t" para la pruebea de significancia
          auxiliar01 <- substring(condition,
                                  which(substring(condition,1:nchar(condition),1:nchar(condition))=="p")+1,
                                  nchar(condition)-1) #se obtiene el valor del Romp sin signo (valor absoluto)
          auxiliar01 <- ifelse(auxiliar01<0.05,"",auxiliar[2]) #se verifica significancia (valor p < 0.05)
        } else {
          if (sum(substring(auxiliar[2],1:nchar(auxiliar[2]),1:nchar(auxiliar[2]))=="-")>=1) { #con este "if" se diferencia entre los valores negativos y positivos del Romp
            auxiliar01 <- substring(auxiliar[2],
                                    which(substring(auxiliar[2],1:nchar(auxiliar[2]),1:nchar(auxiliar[2]))=="-")+1,
                                    nchar(auxiliar[2])-1) #se obtiene el valor del Romp sin signo (valor absoluto)
            auxiliar01 <- ifelse(as.numeric(auxiliar01)>2,"",auxiliar[2]) #se verifica significancia (valor t > 2)
          } else { #aquí están los valores positivos del Romp
            auxiliar01 <- substring(auxiliar[2],
                                    which(substring(auxiliar[2],1:nchar(auxiliar[2]),1:nchar(auxiliar[2]))=="t")+1,
                                    nchar(auxiliar[2])-1) #se obtiene el valor del Romp sin signo (valor absoluto)
            auxiliar01 <- ifelse(as.numeric(auxiliar01)>2,"",auxiliar[2]) #se verifica significancia (valor t > 2)
          }
        }
      } else {
        condition <- auxiliar01 <- "" #si en la fila "i" hay valor character vacío ("")
      }
    }
    Romp <- auxiliar01
  }
  # 10 -- outliers
  if(diagnostic$II$Coded.Outliers == ""){
    outliers <- ""
  } else {
    outliers <- cbind(Indicador = "name", final = diagnostic$II$Coded.Outliers)
    
    for (i in 1:nrow(outliers)) { #con este "for" se obtiene un vector con la información para el "outliers" con todo y salida final para "S_outliers"
      auxiliar <- as.vector(outliers[i,]) #se toma la fila "n" del data frame "outliers"
      auxiliar01 <- which(substring(auxiliar[2],1:nchar(auxiliar[2]),1:nchar(auxiliar[2]))=="]") #se obtienen los divisores para solo obtener lo que está dentro de los corchetes y evitar errores futuros
      if(length(auxiliar01)==0){ #con este "if" se abordan los 3 casos de longitud 0, 1 y >=2 para obtener la información para el "outliers"
        columnas <- "" #para longitud 0 dejar un character vacío
      } else {
        if (length(auxiliar01)==1) { #para longitud 1 dejar lo mismo que el vector de outliers
          columnas <- auxiliar[2]
        } else { #para longitud mayor igual a 2, se hace un ciclo para dividir en columnas a cada elemento
          columnas <- NULL
          for (j in 1:length(auxiliar01)) { #ciclo para obtener un vector con los elementos separados de la variable "auxiliar"
            ifelse(j==1, auxiliar02 <- substring(auxiliar[2],1,auxiliar01[j]),
                   ifelse(j==length(auxiliar01), auxiliar02 <- substring(auxiliar[2],auxiliar01[j-1]+2,nchar(auxiliar[2])),
                          auxiliar02 <- substring(auxiliar[2],auxiliar01[j-1]+2,auxiliar01[j])))
            #en el "ifelse" anterior se distinguen 3 casos, donde se toma al primer elemento, al último y a los que están entre éstos dos
            columnas <- c(columnas,auxiliar02)
          }
        }
      }
      
      auxiliar05 <- NULL
      for (k in 1:length(columnas)) { #con este "for" aislamos dato por dato el vector "columnas"
        auxiliar03 <- columnas[k] #se toma el elemento "k" del vector "columnas"
        if (auxiliar03!="") { #con este "if" se valida si hay o no hay outliers, es decir, si tiene valores de character vacíos ("")
          condition <- substring(auxiliar03,
                                 which(substring(auxiliar03,1:nchar(auxiliar03),1:nchar(auxiliar03))=="["),
                                 which(substring(auxiliar03,1:nchar(auxiliar03),1:nchar(auxiliar03))=="]")) #obtenemos de la cadena del outlier en cuestión solo lo que está dentro de los corchetes
          if (sum(substring(condition,1:nchar(condition),1:nchar(condition))=="-")>=1) { #con este "if" se diferencia entre los valores negativos y positivos del outlier
            auxiliar04 <- substring(condition,
                                    which(substring(condition,1:nchar(condition),1:nchar(condition))=="-")+1,
                                    nchar(condition)-1) #se obtiene el valor del outlier sin signo (valor absoluto)
            auxiliar04 <- ifelse(as.numeric(auxiliar04)>=critico,
                                 "",auxiliar03) #se verifica significancia (valor t > valor_crítico)
          } else { #aquí están los valores positivos del outlier
            auxiliar04 <- substring(condition,
                                    which(substring(condition,1:nchar(condition),1:nchar(condition))=="t")+1,
                                    nchar(condition)-1) #se obtiene el valor del outlier sin signo (valor absoluto)
            auxiliar04 <- ifelse(as.numeric(auxiliar04)>=critico,
                                 "",auxiliar03) #se verifica significancia (valor t > valor_crítico)
          }
        } else {
          auxiliar04 <- "" #si en el elemento "k" hay valor character vacío ("")
        }
        auxiliar05 <- c(auxiliar05,auxiliar04)#se guardan los elementos en un vector resultante y se eliminan variables auxiliares
        
        if (length(auxiliar05)==sum(auxiliar05=="")) { #con este "if" se valida el vector final (salida final)
          auxiliar06 <- "" #porque pueden haber casos como: "" "" "" "". Se deja como un elemento ""
        } else { #cuando hay al menos un outlier
          auxiliar05 <- auxiliar05[auxiliar05!=""] #quitamos todos lo character vacíos ("")
          auxiliar06 <- NULL
          for (l in 1:length(auxiliar05)) { auxiliar06 <- paste(auxiliar06,auxiliar05[l],sep = " ") } #con este "for" se crea el vector character de los outliers no significativos
          auxiliar06 <- substring(auxiliar06,2,nchar(auxiliar06)) #depuramos el vector final
        }
      }
    }
    outliers <- auxiliar06
  }
  # 11 -- td
  if(diagnostic$II$Trading.Day == ""){
    td <- ""
  } else {
    td <- cbind(Indicador = "name", final = diagnostic$II$Trading.Day) #obtenemos la información del _DIAGNOSTICO_
    
    filas <- NULL
    for (i in 1:nrow(td)) { #con este "for" se obtiene un vector con la información para el "td"
      auxiliar <- as.vector(td[i,2]) #se toma la fila "n" y la columna 2 del data frame "td"
      auxiliar01 <- which(substring(auxiliar,1:nchar(auxiliar),1:nchar(auxiliar))=="]") #se obtienen los divisores para distinguir entre "TD" y "Lpyear"
      if(length(auxiliar01)==0){ #con este "if" se abordan los 3 casos de longitud 0, 1 y 2 para obtener la información para el "td"
        columnas <- "" #para longitud 0 dejar un character vacío
      } else {
        if (length(auxiliar01)==1) { #para longitud 1, validar si se trata de un "Lpyear" o un "TD"
          ifelse(length(c(which(substring(auxiliar,1:nchar(auxiliar),1:nchar(auxiliar))=="D"),
                          which(substring(auxiliar,1:nchar(auxiliar),1:nchar(auxiliar))=="d")))==0,
                 columnas <- "", columnas <- auxiliar) #se hace la validación para encontrar en el nombre del "TD" una "D" mayúscula o "d" minúscula
        } else {
          columnas <- substring(auxiliar,1,auxiliar01[1]) #para longitud 2, sólo dejar la información del primer elemento, siempre será primero el "TD"
        }
      }
      filas <- c(filas,columnas); rm(columnas,auxiliar,auxiliar01) #el resultado de cada iteración se va almacenando en este vector y se eliminan las variables auxiliares
    }
    
    td <- cbind(Indicador = td[,1], final = filas) #juntamos el resultado del "for" anterior con "td" en un mismo cuadro y eliminamos variable "filas"
    for (i in 1:nrow(td)) { #con este "for" aislamos fila por fila el cuadro "td"
      auxiliar <- as.vector(td[i,]) #se toma la fila "i" del data frame "td"
      if (auxiliar[2]!="") { #con este "if" se valida si hay o no hay TD, es decir, si tiene valores de character vacíos ("")
        condition <- substring(auxiliar[2],
                               which(substring(auxiliar[2],1:nchar(auxiliar[2]),1:nchar(auxiliar[2]))=="["),
                               which(substring(auxiliar[2],1:nchar(auxiliar[2]),1:nchar(auxiliar[2]))=="]")) #obtenemos de la cadena del TD solo lo que está dentro de los corchetes
        if (sum(substring(condition,1:nchar(condition),1:nchar(condition))=="p")==1) { #con este "if" se valida si se trata del valor "p" o "t" para la pruebea de significancia
          auxiliar01 <- substring(condition,
                                  which(substring(condition,1:nchar(condition),1:nchar(condition))=="p")+1,
                                  nchar(condition)-1) #se obtiene el valor del TD sin signo (valor absoluto)
          auxiliar01 <- ifelse(auxiliar01<0.05,"",auxiliar[2]) #se verifica significancia (valor p < 0.05)
        } else {
          if (sum(substring(auxiliar[2],1:nchar(auxiliar[2]),1:nchar(auxiliar[2]))=="-")>=1) { #con este "if" se diferencia entre los valores negativos y positivos del TD
            auxiliar01 <- substring(auxiliar[2],
                                    which(substring(auxiliar[2],1:nchar(auxiliar[2]),1:nchar(auxiliar[2]))=="-")+1,
                                    nchar(auxiliar[2])-1) #se obtiene el valor del TD sin signo (valor absoluto)
            auxiliar01 <- ifelse(as.numeric(auxiliar01)>2,"",auxiliar[2]) #se verifica significancia (valor t > 2)
          } else { #aquí están los valores positivos del TD
            auxiliar01 <- substring(auxiliar[2],
                                    which(substring(auxiliar[2],1:nchar(auxiliar[2]),1:nchar(auxiliar[2]))=="t")+1,
                                    nchar(auxiliar[2])-1) #se obtiene el valor del TD sin signo (valor absoluto)
            auxiliar01 <- ifelse(as.numeric(auxiliar01)>2,"",auxiliar[2]) #se verifica significancia (valor t > 2)
          }
        }
      } else {
        condition <- auxiliar01 <- "" #si en la fila "i" hay valor character vacío ("")
      }
    }
    td <- auxiliar01
    if (grepl("TradingDay", td)) {
      espec <- readLines(paste(ruta_out, "SPEC.spc", sep = "\\"))
      espec <- espec[grep("regression\\{", espec):grep("arima\\{", espec)]
      espec <- toupper(espec[-grep("save=", espec)])
      for (i in c("TD1NOLPYEAR", "TDNOLPYEAR", "TD1COEF", "TD")) { if (sum(grepl(i, espec)) == 1) { break } }
      td <- sub("TradingDay", str_to_title(i), td)
    }
  }
  # 12 -- lpyear
  if(diagnostic$II$Trading.Day == ""){
    lpyear <- ""
  } else {
    lpyear <- cbind(Indicador = "name", final = diagnostic$II$Trading.Day) #obtenemos la información del _DIAGNOSTICO_
    
    filas <- NULL
    for (i in 1:nrow(lpyear)) { #con este "for" se obtiene un vector con la información para el "lpyear"
      auxiliar <- as.vector(lpyear[i,2]) #se toma la fila "n" y la columna 2 del data frame "lpyear"
      auxiliar01 <- which(substring(auxiliar,1:nchar(auxiliar),1:nchar(auxiliar))=="]") #se obtienen los divisores para distinguir entre "lpyear" y "Lpyear"
      if(length(auxiliar01)==0){ #con este "if" se abordan los 3 casos de longitud 0, 1 y 2 para obtener la información para el "lpyear"
        columnas <- "" #para longitud 0 dejar un character vacío
      } else {
        if (length(auxiliar01)==1) { #para longitud 1, validar si se trata de un "Lpyear" o un "TD"
          ifelse(length(which(substring(auxiliar,1:nchar(auxiliar),1:nchar(auxiliar))=="D"))==0,
                 columnas <- auxiliar, columnas <- "") #se hace la validación para encontrar en el nombre del "lpyear" una "D" mayúscula o "d" minúscula
        } else {
          columnas <- substring(auxiliar,auxiliar01[1]+1,nchar(auxiliar)) #para longitud 2, sólo dejar la información del primer elemento, siempre será primero el "lpyear"
        }
      }
      filas <- c(filas,columnas); rm(columnas,auxiliar,auxiliar01) #el resultado de cada iteración se va almacenando en este vector y se eliminan las variables auxiliares
    }
    
    lpyear <- cbind(Indicador = lpyear[,1], final = filas); rm(filas) #juntamos el resultado del "for" anterior con "lpyear" en un mismo cuadro y eliminamos variable "filas"
    for (i in 1:nrow(lpyear)) { #con este "for" aislamos fila por fila el cuadro "lpyear"
      auxiliar <- as.vector(lpyear[i,]) #se toma la fila "i" del data frame "lpyear"
      if (auxiliar[2]!="") { #con este "if" se valida si hay o no hay lpyear, es decir, si tiene valores de character vacíos ("")
        if (sum(substring(auxiliar[2],1:nchar(auxiliar[2]),1:nchar(auxiliar[2]))=="-")>=1) { #con este "if" se diferencia entre los valores negativos y positivos del lpyear
          auxiliar01 <- substring(auxiliar[2],
                                  which(substring(auxiliar[2],1:nchar(auxiliar[2]),1:nchar(auxiliar[2]))=="-")+1,
                                  nchar(auxiliar[2])-1) #se obtiene el valor del lpyear sin signo (valor absoluto)
          auxiliar01 <- ifelse(as.numeric(auxiliar01)>2,"",auxiliar[2]) #se verifica significancia (valor t > 2)
        } else { #aquí están los valores positivos del lpyear
          auxiliar01 <- substring(auxiliar[2],
                                  which(substring(auxiliar[2],1:nchar(auxiliar[2]),1:nchar(auxiliar[2]))=="t")+1,
                                  nchar(auxiliar[2])-1) #se obtiene el valor del lpyear sin signo (valor absoluto)
          auxiliar01 <- ifelse(as.numeric(auxiliar01)>2,"",auxiliar[2]) #se verifica significancia (valor t > 2)
        }
      } else {
        auxiliar01 <- "" #si en la fila "i" hay valor character vacío ("")
      }
    }
    lpyear <- auxiliar01
  } 
  # 13 -- easter
  if(diagnostic$II$Holiday == ""){
    easter <- ""
  } else {
    easter <- cbind(Indicador = "name", final = diagnostic$II$Holiday) #obtenemos la información del _DIAGNOSTICO_
    for (n in 1:nrow(easter)) { #con este "for" aislamos fila por fila el cuadro "easter"
      auxiliar <- as.vector(easter[n,]) #se toma la fila "n" del data frame "easter"
      if (auxiliar[2]!="") { #con este "if" se valida si hay o no hay EASTER, es decir, si tiene valores de character vacíos ("")
        if (sum(substring(auxiliar[2],1:nchar(auxiliar[2]),1:nchar(auxiliar[2]))=="-")>=1) { #con este "if" se diferencia entre los valores negativos y positivos del EASTER
          auxiliar01 <- substring(auxiliar[2],
                                  which(substring(auxiliar[2],1:nchar(auxiliar[2]),1:nchar(auxiliar[2]))=="-")+1,
                                  nchar(auxiliar[2])-1) #se obtiene el valor del EASTER sin signo (valor absoluto)
          auxiliar01 <- ifelse(as.numeric(auxiliar01)>2,"",auxiliar[2]) #se verifica significancia (valor t > 2)
        } else { #aquí están los valores positivos del EASTER
          auxiliar01 <- substring(auxiliar[2],
                                  which(substring(auxiliar[2],1:nchar(auxiliar[2]),1:nchar(auxiliar[2]))=="t")+1,
                                  nchar(auxiliar[2])-1) #se obtiene el valor del EASTER sin signo (valor absoluto)
          auxiliar01 <- ifelse(as.numeric(auxiliar01)>2,"",auxiliar[2]) #se verifica significancia (valor t > 2)
        }
      } else {
        auxiliar01 <- "" #si en la fila "n" hay valor character vacío ("")
      }
    }
    easter <- auxiliar01
  }
  
  # # #
  rojo <- c(chi,acf,pacf,correlacion,parametros,residual,g1,g2,outliers,td,lpyear,easter, Romp)
  if (length(which(rojo == "")) > 0) { rojo <- rojo[-which(rojo == "")] }
  
  normal <- c(chi,acf,pacf,correlacion,parametros,residual,g1,g2,td,lpyear,easter, Romp)
  if (length(which(normal == "")) > 0) { normal <- normal[-which(normal == "")] }
  
  return(list("I" = paste(rojo, collapse = ", "),
              "II" = paste(normal, collapse = ", ")))
} #Fin de función "f_report"
