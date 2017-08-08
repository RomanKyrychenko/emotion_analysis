 #!/usr/local/bin/Rscript
library(tcltk2)

Sys.setlocale(,"UK_ua")

mydialog <- function(){
  xvar <- tclVar("90cde69ab770444e8aca06c3bb65e977")
  fvar <- tclVar("")
  tt <- tktoplevel()
  tkwm.title(tt,"Аналіз емоцій")
  x.entry <- tkentry(tt, textvariable=xvar)
  #f.entry <- tkentry(tt, textvariable=fvar)
  reset <- function() {
    tclvalue(xvar)<-""
  }
  reset.but <- tkbutton(tt, text="Скинути", command=reset)
  #submit <- function() {
  #  x <- as.numeric(tclvalue(xvar))
  #  tkmessageBox(message=paste("x= ", x, ""))
  #}
  filename <- function() {
    video <- tclvalue(tkgetOpenFile()) 
    if (!nchar(video)) {
      tkmessageBox(message = "Файл не обрано!")
    } else {
      tkmessageBox(message = paste("Обрано файл", video))
    }
    assign("video", video, envir = .GlobalEnv)
  }
  
  submit.but <- tkbutton(tt, text="Файл", command=filename)
  quit.but <- tkbutton(tt, text = "Завершити роботу", 
                       command = function() {
                         q(save = "no")
                         tkdestroy(tt)
                       }
  )
 assign("key",xvar,envir = .GlobalEnv)
  analyse <- function(){
    require(httr)
    apiUrl <- "https://westus.api.cognitive.microsoft.com/emotion/v1.0/recognizeinvideo?outputStyle=perFrame"
    #key <- xvar
    body_video = upload_file(video)
    
    faceEMO <- httr::POST(
      url = apiUrl,
      httr::content_type('application/octet-stream'),
      httr::add_headers(.headers = c('Ocp-Apim-Subscription-Key' = tclvalue(xvar))),
      body = body_video,
      encode = 'json'
    )
    
    operationLocation <- httr::headers(faceEMO)[["operation-location"]]
    pb <- tkProgressBar("Прогрес", "Проаналізовано %",0, 100, 50)
    while(TRUE){
      ret <- httr::GET(operationLocation,
                       httr::add_headers(.headers = c('Ocp-Apim-Subscription-Key' = tclvalue(xvar))))
      con <- httr::content(ret)
      if(is.null(con$status)){
        warning("Connection Error, retry after 1 minute")
        Sys.sleep(1)
      } else if (con$status == "Running" | con$status == "Uploading"){
        cat(paste0("status ", con$status, "\n"))
        cat(paste0("progress ", con$progress, "\n"))
        info <- sprintf("%d%% проаналізовано", round(con$progress))
        setTkProgressBar(pb,i, info,info)
        Sys.sleep(1)
      } else {
        cat(paste0("status ", con$status, "\n"))
        break()
      }
    }
    library(tidyverse)
    data <- (con$processingResult %>% jsonlite::fromJSON())$fragments
    data <- data[which(sapply(data$events,is.null)==F),]
    data$events <- purrr::map(data$events, function(events){
      events %>% purrr::map(function(event){
        jsonlite::flatten(event)
      }) %>% bind_rows()
    })
    data <-data %>% unnest(events)
    write.csv(data,"emotions.csv")
  }
  tt$env$menu <- tk2menu(tt)
  tkconfigure(tt, menu = tt$env$menu)
  tt$env$menuFile <- tk2menu(tt$env$menu, tearoff = FALSE)
  # Our cascaded menu
  tt$env$menuOpenRecent <- tk2menu(tt$env$menuFile, tearoff = FALSE)
  tkadd(tt$env$menuOpenRecent, "command", label = "Завантажити останнє",
        command = function() tkmessageBox(
          message = "I don't know how to open Recent File 1", icon = "error"))
  tkadd(tt$env$menuOpenRecent, "command", label = "Завантажити",
        command = function() tkmessageBox(
          message = "I don't know how to open Recent File 2", icon = "error"))
  tkadd(tt$env$menuFile, "cascade", label = "Відео",
        menu = tt$env$menuOpenRecent)
  tkadd(tt$env$menuFile, "command", label = "Завершити роботу",
        command = function() tkdestroy(tt))
  tkadd(tt$env$menu, "cascade", label = "Файл", menu = tt$env$menuFile)
  tkadd(tt$env$menu, "cascade", label = "Аналіз", menu = tt$env$menuFile)
  tkadd(tt$env$menu, "cascade", label = "Візуалізації", menu = tt$env$menuFile)
  tkadd(tt$env$menu, "cascade", label = "Показники", menu = tt$env$menuFile)
  tkadd(tt$env$menu, "cascade", label = "Допомога", menu = tt$env$menuFile)
  tkgrid(tklabel(tt,text="Введіть ключ Microsoft API"),columnspan=3, pady = 10)
  ea <- tkbutton(tt, text="Проаналізувати емоції", command=analyse)
  tkgrid(tklabel(tt,text="Ключ"), x.entry, pady= 10, padx= 20)
  tkgrid(submit.but, reset.but, ea, quit.but, pady= 10, padx= 10)
  #tt$env$frm <- tk2frame(tt, relief = "sunken", padding = 10)
  #require(tkrplot)
  #require(ggplot2)
  #
  #hscale <- 1  # Horizontal scaling
  #vscale <- 1   # Vertical scaling
  #
  #plotTk <- function() {
  #  x <- -100:100
  #  y <- x^2
  #  plot(ggplot()+geom_point(aes(x, y)))
  #}
  #
  #tt$env$plot <- tkrplot(tt, fun = plotTk,hscale = hscale, vscale = vscale)
  #tkgrid(tt$env$plot)
}

mydialog()

Sys.sleep(10000000000000000000000000000000000000000000000000000000000000000000000000000000000)
