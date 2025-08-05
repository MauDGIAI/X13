library(shiny)

meses <- as.list(c(1:12)); names(meses) <- as.character(1:12)

# Define server logic required
function(input, output) {
  observeEvent(TRUE, { 
    imay <- readLines("download/IMAY.spc", warn = F)
    imay <- paste(imay, collapse = "\n")
    output$spc_home <- renderText(imay)
    
    imay_dat <- readLines("download/Imay.dat", warn = F)
    imay_dat <- paste(imay_dat, collapse = "\n")
    output$dat_home <- renderText(imay_dat)
  })
  output$download1 <- downloadHandler(filename = "IMAY.spc",
    content = function(file) { write.table(as.data.frame(readLines("download/IMAY.spc", warn = F)), file, col.names = F, row.names = F, quote = F) }
  )
  output$download2 <- downloadHandler(filename = "Imay.dat",
    content = function(file) { write.table(as.data.frame(readLines("download/Imay.dat", warn = F)), file, col.names = F, row.names = F, quote = F) }
  )
  observeEvent(input$SPC_file, { 
    SPC_txt <- trimws(readLines(input$SPC_file$datapath, warn = F, encoding = "latin1"))
    source("Variables.R")
    source("SPC.R")
    SPC_txt <- f_standarize(SPC_txt) # Aquí se da formato a SPC_txt
    write.table(as.data.frame(SPC_txt), file = "temp/SPC_original.spc", col.names = F, row.names = F, quote = F)#Original
    
    SPC_txt <- paste(SPC_txt, collapse = "\n")
    updateTextAreaInput(inputId = "SPC_real", value = SPC_txt)
  })
  observeEvent(input$DAT_file, { 
    datos <- read.table(input$DAT_file$datapath)
    if (max(datos$V2) == 12) {
      if (nrow(datos) < 60) {
        showModal(modalDialog( title = "¡Epa!", easy_close = TRUE, 
                               "La serie requiere un mínimo de 5 años (60 meses)... si es tan amable, recargue la página."))
        input$DAT_file <- NULL
      } else {
        DAT_txt <- readLines(input$DAT_file$datapath, warn = F)
        write.table(as.data.frame(DAT_txt), file = "temp/DATA.dat", col.names = F, row.names = F, quote = F)
      }
    } else {
      if (max(datos$V2) == 4) {
        if (nrow(datos) < 20) {
          showModal(modalDialog( title = "¡Epa!", easy_close = TRUE, 
                                 "La serie requiere un mínimo de 5 años (20 trimestres)... si es tan amable, recargue la página."))
          input$DAT_file <- NULL
        } else {
          DAT_txt <- readLines(input$DAT_file$datapath, warn = F)
          write.table(as.data.frame(DAT_txt), file = "temp/DATA.dat", col.names = F, row.names = F, quote = F)
        }
      } else {
        showModal(modalDialog( title = "¡Ay!", easy_close = TRUE, 
                               "Solo se admiten series con periodicidad mensual o trimestral... si es tan amable, recargue la página."))
        input$DAT_file <- NULL
      }
    }
  })
  
  # . # . # . # . # . #
  output$DAT_graph <- renderPlotly({
    if(is.null(input$DAT_file)) { NULL } else { 
      datos <- read.table(input$DAT_file$datapath)
      SPC <- trimws(readLines(paste(getwd(), "/temp/SPC_original.spc", sep = ""), warn = F))
      source("Variables.R")
      source("Grafica_DAT.R")
      archivo_SPC <- substring(as.character(input$DAT_file$name), 1, ubi(".", as.character(input$DAT_file$name))-1)
      DAT_plot(datos, toupper(archivo_SPC), SPC)
    }
  })
  output$contenido <- renderTable({
    if(is.null(input$SPC_file)) { NULL } else { 
      SPC <- trimws(readLines(paste(getwd(), "/temp/SPC_original.spc", sep = ""), warn = F))
      source("Variables.R")
      source("SPC.R")
      f_SPC(SPC)$I
    }
  }, rownames = TRUE, colnames = FALSE,
  striped = TRUE, spacing = "l", align = "lc", width = "90%")
  # . # . # . # . # . #
  
  observeEvent(input$mant, {
    if (is.null(input$SPC_file)) {
      showModal(modalDialog( title = "¡Ups!", easy_close = TRUE, "Por favor, seleccione un archivo SPC."))
    } else {
      source("Variables.R")
      source("SPC.R")
      SPC_txt <- trimws(readLines(paste(getwd(), "/temp/SPC_original.spc", sep = ""), warn = F))
      SPC_txt <- f_standarize(SPC_txt) # Aquí se da formato a SPC_txt
      SPC_txt <- paste(SPC_txt, collapse = "\n")
      updateTextAreaInput(inputId = "SPC_real", value = SPC_txt)
    }
  })
  # . # . # . # . # . #
  output$mestrim <- renderUI({ 
    if (input$periodo == 12) { 
      selectInput("mes", label = h4("Mes"), choices = meses, selected = 1)
    } else {
      selectInput("trim", label = h4("Trimestre"), choices = list("I" = 1, "II" = 2, "III" = 3, "IV" = 4), selected = 1)
    }
  })
  observeEvent(input$conv, {
    if (is.null(input$datos_conv) | input$dat_descarga == "") {
      showModal(modalDialog(title = "¡Ups!", easy_close = TRUE, 
                            "Por favor, asegure seleccionar un archivo Excel (.xlsx) y escribir una ubicación de descarga."))
    } else {
      source("Variables.R")
      periodo <- ifelse(input$periodo == 12, "Mensual", "Trimestral")
      if (input$periodo == 12) { 
        f_conv(periodo, input$anio, input$mes, input$datos_conv$datapath, input$dat_descarga)
      } else {
        f_conv(periodo, input$anio, input$trim, input$datos_conv$datapath, input$dat_descarga)
      }
    }
  })
  output$resena2 <- renderUI({ 
    if (input$radio == 3) { 
      card(height = "100%",
           img(src = 'arima.jpg', align = "centre", width = "100%")
      )
    }
  })
  output$resena <- renderUI({ 
    if (input$radio == 1) { 
      card(height = "100%",
        includeHTML("html/convertidor.html"),
        img(src = 'convertidor.jpg', align = "centre", width = "100%")
      )
    } else {
      if (input$radio == 2) { 
        card(height = "100%",
             includeHTML("html/inicio.html"),
             img(src = 'inicio.jpg', align = "centre", width = "100%")
        )
      } else {
        if (input$radio == 3) { 
          card(height = "100%",
               includeHTML("html/arima.html"),
               img(src = 'arima_graficas.jpg', align = "centre", width = "100%"),
               img(src = 'arima_descarga.jpg', align = "centre", width = "100%")
          )
        }
      }
    }
  })

  
  observeEvent(input$run, {
    # ELIMINACIÓN DE ARCHIVOS
    temporales <- list.files(path = paste(getwd(), "/temp/", sep = ""), pattern = "SPC_temp")
    graficas <- list.files(path = paste(getwd(), "/temp/", sep = ""), pattern = "SPC_graficas")
    descarga <- list.files(path = paste(getwd(), "/temp/", sep = ""), pattern = "SPC_descarga")
    file.remove(paste(getwd(), "/temp/", c(temporales, graficas, descarga), sep = ""))
    
    if (is.null(input$SPC_file) | is.null(input$DAT_file)) {
      showModal(modalDialog( title = "¡Ups!", easy_close = TRUE, "Por favor, seleccione ambos archivos: SPC y DAT."))
    } else {
      withProgress(message = 'Calculation in progress...', style = "notification", value = 0.35, {
        
        write.table(as.data.frame(input$SPC_real), file = "temp/SPC_temp.spc", col.names = F, row.names = F, quote = F)
        write.table(as.data.frame(input$SPC_real), file = "temp/SPC_descarga.spc", col.names = F, row.names = F, quote = F)
        SPC <- trimws(readLines(paste(getwd(), "/temp/SPC_temp.spc", sep = ""), warn = F))
        for (i in 1:length(SPC)) { if (sum(grep("file", SPC) == i) == 1) { break } }
        SPC[i] <- paste("file=", "'", getwd(), "/temp/DATA.dat", "'", sep = "")
        write.table(as.data.frame(trimws(SPC)), file = "temp/SPC_temp.spc", col.names = F, row.names = F, quote = F)
        # system(paste(paste(getwd(), "/WinX13/x13as/x13as.exe ", sep = ""), '"', paste(getwd(), "/temp/SPC_temp", sep = ""), '"', " -s", sep = ""))
        # system(paste("C:/winx13_V2.5/WinX13/x13as/x13as.exe ", '"', paste(getwd(), "/temp/SPC_temp", sep = ""), '"', " -s", sep = ""))
        system2(paste(getwd(), "/WinX13/x13as/x13as.exe ", sep = ""), args = paste(getwd(), "/temp/SPC_temp", sep = ""), stdout = TRUE, stderr = TRUE)
        
        # CONDICIONAL PARA ERRORES EN EL SPC
        condition <- readLines(paste(getwd(), "/temp/SPC_temp.out", sep = ""), warn = F)
        if (length(condition) < 10) { #Este valor es arbitrario: (10)
          
          output$diagnostico1 <- renderDataTable({ NULL }, options = list(paging = FALSE, searching = FALSE, info = FALSE, ordering = FALSE))
          output$diagnostico2 <- renderDataTable({ NULL }, options = list(paging = FALSE, searching = FALSE, info = FALSE, ordering = FALSE))
          output$diagnostico3 <- renderDataTable({ NULL }, options = list(paging = FALSE, searching = FALSE, info = FALSE, ordering = FALSE))
          output$diagnostico4 <- renderDataTable({ NULL }, options = list(paging = FALSE, searching = FALSE, info = FALSE, ordering = FALSE))
          output$diagnostico5 <- renderDataTable({ NULL }, options = list(paging = FALSE, searching = FALSE, info = FALSE, ordering = FALSE))
          
          err <- readLines(paste(getwd(), "/temp/SPC_temp.err", sep = ""), warn = F)
          err <- paste(err, collapse = "\n")
          output$out <- renderText(err)
          
          output$graf1 <- renderPlotly({ NULL })
          output$graf2 <- renderPlotly({ NULL })
          output$graf3 <- renderPlotly({ NULL })
          output$graf4 <- renderPlotly({ NULL })
          output$graf5 <- renderPlotly({ NULL })
          output$graf6 <- renderPlotly({ NULL })
          output$graf7 <- renderPlotly({ NULL })
          output$graf8 <- renderPlotly({ NULL })
          output$graf9 <- renderPlotly({ NULL })
          output$graf10 <- renderPlotly({ NULL })
          
        } else {
          
          source("Variables.R")
          source("SPC.R")
          write.table(as.data.frame(input$SPC_real), file = "temp/SPC_graficas.spc", col.names = F, row.names = F, quote = F) # GRAFICAS
          GRAF <- trimws(readLines(paste(getwd(), "/temp/SPC_graficas.spc", sep = ""), warn = F))
          GRAF <- f_standarize(GRAF) # Aquí se da formato a GRAF
          for (i in 1:length(GRAF)) { if (sum(grep("file", GRAF) == i) == 1) { break } }
          GRAF[i] <- paste("file=", "'", getwd(), "/temp/DATA.dat", "'", sep = "")
          aux <- GRAF[grep("series",GRAF):grep("\\}",GRAF)[1]]; aux <- aux[-c(grep("title",aux), grep("file",aux))]
          if (grepl("save", paste(aux, collapse = " "))) {
            for (i in 1:length(GRAF)) { if (sum(grep("save=\\(", GRAF) == i) == 1) { break } }
            if (grepl("\\}",GRAF[i])) { GRAF[i] <- "save=(a1 b1 a18)}" } else { GRAF[i] <- "save=(a1 b1 a18)" }
          } else {
            for (i in 1:length(GRAF)) { if (sum(grep("period=", GRAF) == i) == 1) { break } }
            GRAF[i] <- paste(GRAF[i], "save=(a1 b1 a18)", sep = " ")
          }
          for (i in 1:length(GRAF)) { if (sum(grep("check\\{", GRAF) == i) == 1) { break } }
          GRAF[i] <- paste(gsub("\\}", "",GRAF[i]), "save=(acf pcf)} spectrum{save=(spr sp0 sp1 sp2)} forecast{save=(fct)}", sep = " ")
          aux <- GRAF[grep("x11\\{",GRAF):length(GRAF)]; aux <- aux[1:grep("\\}",aux)[1]]
          if (grepl("save=", paste(aux, collapse = " "))) {
            for (i in grep("x11\\{",GRAF):length(GRAF)) { if (sum(grep("save=\\(", GRAF) == i) == 1) { break } }
            if (grepl("\\}",GRAF[i])) { GRAF[i] <- "save=(d8 d9 d10 d11 d12 d13 e1 e2 e3)}" } else { GRAF[i] <- "save=(d8 d9 d10 d11 d12 d13 e1 e2 e3)" }
          } else {
            for (i in grep("x11\\{",GRAF):length(GRAF)) { if (sum(grep("\\}", GRAF) == i) == 1) { break } }
            GRAF[i] <- paste(gsub("\\}", "",GRAF[i]), "save=(d8 d9 d10 d11 d12 d13 e1 e2 e3)}", sep = " ")
          }
          write.table(as.data.frame(trimws(GRAF)), file = "temp/SPC_graficas.spc", col.names = F, row.names = F, quote = F)
          # system(paste(paste(getwd(), "/WinX13/x13as/x13as_html ", sep = ""), '"', paste(getwd(), "/temp/SPC_graficas", sep = ""), '"', " -s", sep = ""))
          # system(paste("C:/winx13_V2.5/WinX13/x13as/x13as.exe ", '"', paste(getwd(), "/temp/SPC_graficas", sep = ""), '"', " -s", sep = ""))
          system2(paste(getwd(), "/WinX13/x13as/x13as.exe ", sep = ""), args = paste(getwd(), "/temp/SPC_graficas", sep = ""), stdout = TRUE, stderr = TRUE)
          
          source("Diag_Report.R") # DIAGNOSTICO
          SS <- f_diag(paste(getwd(), "/temp", sep = ""), as.character(input$SPC_file$name))
          
          output$diagnostico1 <- renderDataTable({ SS$I }, options = list(paging = FALSE, searching = FALSE, info = FALSE, ordering = FALSE))
          output$diagnostico2 <- renderDataTable({ SS$II }, options = list(paging = FALSE, searching = FALSE, info = FALSE, ordering = FALSE))
          output$diagnostico3 <- renderDataTable({ SS$III }, options = list(paging = FALSE, searching = FALSE, info = FALSE, ordering = FALSE))
          output$diagnostico4 <- renderDataTable({ SS$IV }, options = list(paging = FALSE, searching = FALSE, info = FALSE, ordering = FALSE))
          output$diagnostico5 <- renderDataTable({ SS$V }, options = list(paging = FALSE, searching = FALSE, info = FALSE, ordering = FALSE))
          
          if (!is.na(SS$rojo)) {
            showNotification(SS$rojo, duration = NULL, type = "error")
          }
          
          incProgress(0.5)
          
          # ARCHIVO OUT
          out_txt <- readLines(paste(getwd(), "/temp/SPC_temp.out", sep = ""), warn = F)
          out_txt <- paste(out_txt, collapse = "\n")
          output$out <- renderText(out_txt)
          
          incProgress(0.25)
          
          source("Graf.R") # GRAFICAS X13
          graficas_x13 <- f_graf(getwd(), sum(grepl("period=12", SPC)))
          output$graf1 <- renderPlotly({ graficas_x13$g1 })
          output$graf2 <- renderPlotly({ graficas_x13$g2 })
          output$graf3 <- renderPlotly({ graficas_x13$g3 })
          output$graf4 <- renderPlotly({ graficas_x13$g4 })
          output$graf5 <- renderPlotly({ graficas_x13$g5 })
          output$graf6 <- renderPlotly({ graficas_x13$g6 })
          output$graf7 <- renderPlotly({ graficas_x13$g7 })
          output$graf8 <- renderPlotly({ graficas_x13$g8 })
          output$graf9 <- renderPlotly({ graficas_x13$g9 })
          output$graf10 <- renderPlotly({ graficas_x13$g10 })
          
        }
        setProgress(1)
      })
    }
  })
  
  observeEvent(input$salida, {
    source("Variables.R")
    if (input$nombre == "" | input$out_descarga == "") {
      showModal(modalDialog(title = "¡Epa!", easy_close = TRUE, 
                            "Por favor, escriba el nombre de sus archivos y una ubicación de descarga."))
    } else {
      bajar_salida <- c(list.files(path = paste(getwd(), "/temp/", sep = ""), pattern = "SPC_graficas"), 
                        list.files(path = paste(getwd(), "/temp/", sep = ""), pattern = "SPC_descarga"),
                        list.files(path = paste(getwd(), "/temp/", sep = ""), pattern = "SPC_temp.out"))
      bajar_salida <- bajar_salida[-c(which("SPC_graficas.spc" == bajar_salida), which("SPC_graficas.out" == bajar_salida))]
      for (i in bajar_salida) {
        denom <- as.data.frame(readLines(paste("temp/", i, sep = ""), warn = F))
        i <- paste(input$nombre, substring(i, ubi(".", i), nchar(i)), sep = "")
        write.table(denom, paste(input$out_descarga, "/", i, sep = ""), col.names = F, row.names = F, quote = F)
      }
    }
  })
  
}



