library(forecast)

f_neuronal <- function(datos){
  
  serie <- ts(datos$V3, start = c(min(datos$V1), min(datos$V2)), frequency = max(datos$V2))
  
  nnar <- nnetar(serie, p = 6, P = 1, size = 6, repeats = 50)#, lambda = 0)
  
  autoplot(forecast(nnar, PI = TRUE, h=12))
  
  pronostico <- forecast(nnar, PI = TRUE, h=12)
  
  fitted(nnar)
  
  # Ajustar modelo de red neuronal autorregresiva
  modelo_nnetar <- nnetar(serie)
  
  # Predecir la serie ajustada (sin estacionalidad)
  serie_sin_estacionalidad <- fitted(modelo_nnetar)
  
  # Graficar la serie original vs la desestacionalizada
  autoplot(cbind(Original = serie, Desestacionalizada = serie_sin_estacionalidad)) +
    ggtitle("Serie Original vs. Desestacionalizada (Red Neuronal)") +
    ylab("Valor") +
    xlab("Tiempo") +
    scale_color_manual(values = c("blue", "red"))
  
  
}

