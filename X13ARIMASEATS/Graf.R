# Gráficas del X-13 al momento de correr un modelo.
library(dplyr)

f_graf <- function(ruta_spc, period){ #Inicio función "f_graf
  # ACF y PACF
  x13.acf <- read.table(paste(ruta_spc, "/temp/SPC_graficas.acf", sep = ""), skip = 2, sep = "\t")
  
  x13.pacf <- read.table(paste(ruta_spc, "/temp/SPC_graficas.pcf", sep = ""), skip = 2, sep = "\t")
  
  g1 <- plot_ly(x13.acf, x = x13.acf$V1, y = x13.acf$V2) %>% add_bars(name = " ", width = 0.1) %>%
    add_lines(y = x13.acf$V3*2, name = " ", color = I("black")) %>%
    add_lines(y = -x13.acf$V3*2, name = " ", color = I("black")) %>% layout(title = "ACF of the residuals")
  
  g2 <- plot_ly(x13.pacf, x = x13.pacf$V1, y = x13.pacf$V2) %>% add_bars(name = " ", width = 0.1) %>%
    add_lines(y = x13.pacf$V3*2, name = " ", color = I("black")) %>%
    add_lines(y = -x13.pacf$V3*2, name = " ", color = I("black")) %>% layout(title = "PACF of the residuals")
  
  # Seasonal Factors and SI Ratios by month
  d8 <- read.table(paste(ruta_spc, "/temp/SPC_graficas.d8", sep = ""), skip = 2, sep = "\t")
  d9 <- read.table(paste(ruta_spc, "/temp/SPC_graficas.d9", sep = ""), skip = 2, sep = "\t")
  d10 <- read.table(paste(ruta_spc, "/temp/SPC_graficas.d10", sep = ""), skip = 2, sep = "\t")
  mplot <- cbind(d8, V3 = d9$V2, V4 = d10$V2, V5 = substring(d8$V1,5,6))
  mplot <- mplot[order(mplot$V5, mplot$V1),]; V6 <- NULL
  for (i in unique(mplot$V5)) { V6 <- c(V6, rep(mean(mplot$V4[which(mplot$V5 == i)]), length(which(mplot$V5 == i)))) }
  mplot <- cbind(mplot, V6 = V6)
  for (i in unique(mplot$V5)) { mplot <- rbind(mplot, data.frame(V1=paste("****",i,sep = ""),V2=NA,V3=NA,V4=NA,V5=i,V6=NA)) }
  mplot <- mplot[order(mplot$V5, mplot$V1),]
  mplot <- cbind(mplot, V7 = paste(mplot$V5, substring(mplot$V1,3,4), sep = "~"))
  mplot$V3 <- replace(mplot$V3, mplot$V3 < -100, NA)
  
  g3 <- plot_ly(mplot, x = mplot$V7, y = mplot$V2, type = "scatter", mode='markers', name = "SI Ratio", color = I("gray40")) %>% 
    add_markers(y = mplot$V3, name = "Replaced SI Ratio", color = I("#00F5FF")) %>% 
    add_lines(y = mplot$V4, name = "Seasonal Factors", color = I("blue")) %>% 
    add_lines(y = mplot$V6, name = "Mean", color = I("pink"))
  
  if (period != 0) {
    # Spectrum of the Irregular
    spec_irr <- read.table(paste(ruta_spc, "/temp/SPC_graficas.sp2", sep = ""), skip = 2, sep = "\t")
    m <- min(spec_irr$V3)
    g4 <- plot_ly(spec_irr, x = spec_irr$V2, y = spec_irr$V3, type = "bar", name = " ", color = I("white")) %>%
      add_trace(y = (m + (m/25)) - spec_irr$V3, name = " ", color = I("steelblue4")) %>% 
      layout(title = "Spectrum of the Irregular", barmode = 'stack')
    
    # Spectrum of the Seasonally Adjusted Series
    spec_sa <- read.table(paste(ruta_spc, "/temp/SPC_graficas.sp1", sep = ""), skip = 2, sep = "\t")
    m <- min(spec_sa$V3)
    g5 <- plot_ly(spec_sa, x = spec_sa$V2, y = spec_sa$V3, type = "bar", name = " ", color = I("white")) %>% 
      add_trace(y = (m + (m/25)) - spec_sa$V3, name = " ", color = I("steelblue4")) %>% 
      layout(title = "Spectrum of the Seasonally Adjusted Series", barmode = 'stack')
    
    # Spectrum of the RegARIMA Residuals
    spec_r <- read.table(paste(ruta_spc, "/temp/SPC_graficas.spr", sep = ""), skip = 2, sep = "\t")
    m <- min(spec_r$V3)
    g6 <- plot_ly(spec_r, x = spec_r$V2, y = spec_r$V3, type = "bar", name = " ", color = I("white")) %>% 
      add_trace(y = (m + (m/25)) - spec_r$V3, name = " ", color = I("steelblue4")) %>% 
      layout(title = "Spectrum of the RegARIMA Residuals", barmode = 'stack')
    
    # Spectrum of the Transformed Prior Adjusted Original Series
    spec_orig <- read.table(paste(ruta_spc, "/temp/SPC_graficas.sp0", sep = ""), skip = 2, sep = "\t")
    m <- min(spec_orig$V3)
    g7 <- plot_ly(spec_orig, x = spec_orig$V2, y = spec_orig$V3, type = "bar", name = " ", color = I("white")) %>% 
      add_trace(y = (m + (m/25)) - spec_orig$V3, name = " ", color = I("steelblue4")) %>% 
      layout(title = "Spectrum of the Transformed Prior Adjusted Original Series", barmode = 'stack')
  } else {
    g4 <- plot_ly(type = "scatter", mode='markers')
    g5 <- plot_ly(type = "scatter", mode='markers')
    g6 <- plot_ly(type = "scatter", mode='markers')
    g7 <- plot_ly(type = "scatter", mode='markers')
  }
  
  # Original Series, Seasonally Adjusted Series and Trend
  ori <- read.table(paste(ruta_spc, "/temp/SPC_graficas.a1", sep = ""), skip = 2, sep = "\t")
  ori <- cbind(ori, V3 = substring(ori$V1,5,6)); frec <- max(as.numeric(unique(ori$V3))); anio <- substring(ori$V1[1],1,4)
  ori <- ts(data = ori$V2, frequency = frec, start = anio)
  if (frec == 12) { fechas <- as.character(zoo::as.yearmon(time(ori)))
  } else { fechas <- as.character(zoo::as.yearqtr(time(ori))) }
  ori <- cbind(fecha = fechas, as.data.frame(ori))
  
  s_adj <- read.table(paste(ruta_spc, "/temp/SPC_graficas.d11", sep = ""), skip = 2, sep = "\t")
  
  trend <- read.table(paste(ruta_spc, "/temp/SPC_graficas.d12", sep = ""), skip = 2, sep = "\t")
  
  g8 <- plot_ly(ori, x = reorder(ori$fecha, 1:nrow(ori)), y = ori$x, type = "scatter", mode = "lines",
                color = I("gray"), name = "Original") %>% 
    add_lines(y = s_adj$V2, name = "Seasonally Adjusted", color = I("blue")) %>%
    add_lines(y = trend$V2, name = "Trend", color = I("pink")) %>%
    layout(title = "Original Series, Seasonally Adjusted Series and Trend")
  
  # Forecasts
  if (period != 0) {
    ori2 <- rbind(ori[(nrow(ori) - 59):nrow(ori),], data.frame(fecha = 1:12, x = rep(NA, 12)))
    fcts <- read.table(paste(ruta_spc, "/temp/SPC_graficas.fct", sep = ""), skip = 2, sep = "\t")
    
    g9 <- plot_ly(ori2, x = reorder(ori2$fecha, 1:nrow(ori2)), y = ori2$x, type = "scatter", mode = "lines",
                  color = I("skyblue2"), name = " ") %>% 
      add_lines(y = c(rep(NA, nrow(ori2) - 12), fcts$V2), name = " ", color = I("cyan3")) %>%
      add_lines(y = c(rep(NA, nrow(ori2) - 12), fcts$V3), name = " ", color = I("gray")) %>%
      add_lines(y = c(rep(NA, nrow(ori2) - 12), fcts$V4), name = " ", color = I("gray")) %>% layout(title = "Forecasts")
  } else {
    ori2 <- rbind(ori[(nrow(ori) - 19):nrow(ori),], data.frame(fecha = 1:4, x = rep(NA, 4)))
    fcts <- read.table(paste(ruta_spc, "/temp/SPC_graficas.fct", sep = ""), skip = 2, sep = "\t")
    
    g9 <- plot_ly(ori2, x = reorder(ori2$fecha, 1:nrow(ori2)), y = ori2$x, type = "scatter", mode = "lines",
                  color = I("skyblue2"), name = " ") %>% 
      add_lines(y = c(rep(NA, nrow(ori2) - 4), fcts$V2), name = " ", color = I("cyan3")) %>%
      add_lines(y = c(rep(NA, nrow(ori2) - 4), fcts$V3), name = " ", color = I("gray")) %>%
      add_lines(y = c(rep(NA, nrow(ori2) - 4), fcts$V4), name = " ", color = I("gray")) %>% layout(title = "Forecasts")
  }
  
  # Logs of the Original Series
  g10 <- plot_ly(ori, x = reorder(ori$fecha, 1:nrow(ori)), y = log(ori$x), type = "scatter", mode = "lines") %>% 
    layout(title = "Logs of the Original Series")
  
  return(list(g1=g1, g2=g2, g3=g3, g4=g4, g5=g5, g6=g6, g7=g7, g8=g8, g9=g9, g10=g10))
  
} #Fin de función "f_graf"
