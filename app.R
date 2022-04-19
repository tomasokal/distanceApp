library(shiny)
library(shinyjs)
library(geosphere)
library(htmltools)
library(leaflet)

# UI
ui <- fluidPage(
  # style = "max-width: 1200px;",
  tags$link(
    rel = "stylesheet",
    type = "text/css",
    href = "style.css"
  ),
  uiOutput(
    "header",
    class = "header-container"
  ),
  uiOutput(
    "selectors",
    class = "container"
  ),
  div(
    class = "output-container",
    leafletOutput("map"),
  ),
  useShinyjs()
)

# Server
server <- function(session, input, output) {

    # Header
    output$header <- shiny::renderUI({
      shiny::tagList(
        shiny::tags$h1(
          class = "header",
          "CALCULATE COORDINATE POINTS"
        ),
        shiny::tags$div(
          class = "contact-container",
          shiny::tags$div(
            class = "contact-text",
            shiny::a(href = "https://github.com/tomasokal/distanceApp", "CODE")
          ),
          shiny::tags$div(
            class = "contact-text",
            shiny::a(href = "http://tomasokal.com", "CONTACT")
          )
        )
      )
    })

    # Main Body Container
    output$selectors <- shiny::renderUI({
      shiny::tagList(
        shiny::tags$div(
          class = "instruction-container",
          shiny::tags$div(
            class = "instruction-text",
            shiny::p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.")
          ),
          shiny::tags$div(
            shiny::tags$div(
              class = "instruction-icon",
              shiny::tags$div(
                id = "icon-africa",
                shiny::icon("globe-africa")
              ),
              shinyjs::hidden(shiny::tags$div(
                id = "icon-americas",
                shiny::icon("globe-americas")
              )),
              shinyjs::hidden(shiny::tags$div(
                id = "icon-asia",
                shiny::icon("globe-asia")
              )),
              shinyjs::hidden(shiny::tags$div(
                id = "icon-europe",
                shiny::icon("globe-europe")
              ))
            )
          )
        ),
        shiny::hr(
          class = "hr"
        ),
        shiny::tags$div(
          class = "button-container",
          shiny::tags$div(
            class = "coordinate-container",
            shiny::numericInput(
              inputId = "lat",
              label = "Latitude",
              value = "48.780071",
              step = 0.000001,
              min = -90,
              max = 90,
              width = "175px"
            ),
            shiny::tags$div(
              class = "text-container",
              shiny::textOutput("lat_direc")
            )
          ),
          shiny::tags$div(
            class = "coordinate-container",
            shiny::numericInput(
              inputId = "lng",
              label = "Longitude",
              value = "18.577100",
              step = 0.000001,
              min = -180,
              max = 180,
              width = "175px"
            ),
            shiny::tags$div(
              class = "text-container",
              shiny::textOutput("lng_direc")
            )
          ),
          shiny::tags$div(
            class = "input-container",
            shiny::numericInput(
              inputId = "points",
              label = "Points",
              value = 150,
              min = 0,
              max = 500,
              step = 1,
              width = "175px"
            )
          ),
          shiny::tags$div(
            class = "input-container",
            shiny::numericInput(
              inputId = "distance",
              label = "Distance",
              value = 150,
              min = 0,
              max = 500,
              step = 1,
              width = "175px"
            )
          )
        ),
        shiny::hr(
          class = "hr"
        ),
        shiny::tags$div(
          class = "button-container",
          shiny::actionButton(
            inputId = "buttonRun",
            label = "Run",
            class = "button-run-circle"
          ),
          shiny::tags$div(
            class = "fun-icons",
            HTML('<img src="iconmonstr-triangle-2.svg" alt="Kiwi standing on oval">'),
            HTML('<img src="iconmonstr-square-4.svg" alt="Kiwi standing on oval">'),
            HTML('<img src="iconmonstr-circle-2.svg" alt="Kiwi standing on oval">')
          )
        )
      )
    })

    # Map 
    output$map <- renderLeaflet({
      leaflet() %>% 
        addTiles() %>%
        addMarkers(mapLng(), mapLat()) %>% 
        addProviderTiles(providers$OpenTopoMap)
    })

    # Cardinal direction indidcator
    output$lat_direc <- shiny::renderText({
      if (!is.na(input$lat)) {
        if (!substr(input$lat, 1, 1) == "-") {"N"} else {"S"}
      } else {"N"}
    })

    # Cardinal direction indidcator
    output$lng_direc <- shiny::renderText({
      if (!is.na(input$lng)) {
        if (!substr(input$lng, 1, 1) == "-") {"E"} else {"W"}
      } else {"E"}
    })

    mapLat<- reactive({
      input$lat
    }) %>%
    bindCache(input$lat) %>%
    bindEvent(input$buttonRun)

    mapLng<- reactive({
      input$lng
    }) %>%
    bindCache(input$lng) %>%
    bindEvent(input$buttonRun)
    
    # Update user input for latitude
    # When number goes over two digits, add a decimal
    observeEvent(input$lat, {

      if (!is.na(input$lat)) {

        if (nchar(input$lat) > 2 & !grepl(".", input$lat, fixed = TRUE)) {

          x <- input$lat

          if (substr(x, 1, 1) == "-") {val <- 3}
          if (!substr(x, 1, 1) == "-") {val <- 2}

          x <- sub(paste0("(.{", val, "})(.*)"), "\\1.\\2", x)

          shiny::updateNumericInput(
            session, 
            inputId = "lat", 
            value = as.numeric(x)
          )

        }

      }

    })

    # Update user input for latitude
    # When number goes over three digits, add a decimal
    observeEvent(input$lng, {

      if (!is.na(input$lng)) {

        if (nchar(input$lng) > 3 & !grepl(".", input$lng, fixed = TRUE)) {

          x <- input$lng

          if (substr(x, 1, 1) == "-") {val <- 4}
          if (!substr(x, 1, 1) == "-") {val <- 3}

          x <- sub(paste0("(.{", val, "})(.*)"), "\\1.\\2", x)

          shiny::updateNumericInput(
            session,
            inputId = "lng", 
            value = as.numeric(x)
          )

        }

      }
      
    })

    # Make globe update based on latitude and longitude
    globe_check <- reactive({
      list(input$lat, input$lng)
    })

    observeEvent(globe_check(), {

      if (is.null(input$lat) || is.null(input$lng)) {
        shinyjs::hide("icon-americas")
        shinyjs::show("icon-africa")
        shinyjs::hide("icon-asia")
        shinyjs::hide("icon-europe")
      } else {
        
        if (substr(input$lat, 1, 1) == "-" && substr(input$lng, 1, 1) == "-") {
          shinyjs::show("icon-americas")
          shinyjs::hide("icon-africa")
          shinyjs::hide("icon-asia")
          shinyjs::hide("icon-europe")
        }
        
        if (!substr(input$lat, 1, 1) == "-" && substr(input$lng, 1, 1) == "-") {
          shinyjs::hide("icon-americas")
          shinyjs::hide("icon-africa")
          shinyjs::hide("icon-asia")
          shinyjs::show("icon-europe")
        }
        
        if (substr(input$lat, 1, 1) == "-" && !substr(input$lng, 1, 1) == "-") {
          shinyjs::hide("icon-americas")
          shinyjs::hide("icon-africa")
          shinyjs::show("icon-asia")
          shinyjs::hide("icon-europe")
        }
        
        if (!substr(input$lat, 1, 1) == "-" && !substr(input$lng, 1, 1) == "-") {
          shinyjs::hide("icon-americas")
          shinyjs::show("icon-africa")
          shinyjs::hide("icon-asia")
          shinyjs::hide("icon-europe")
        }
        
      }

    })

    # observeEvent(input$buttonRun, {
      
      
    # })
      
      # browser()
      # 
      # latitude <- input$lat
      # longitude <- input$lng
      # distance <- input$distance
      # 
      # westStart <- geosphere::destPoint(c(longitude, latitude), 270, distance*75)
      # 
      # north <- destPoint(westStart, 0, seq(from = distance, to = (distance*75), by = distance))
      # south <- destPoint(westStart, 180, seq(from = distance, to = (distance*75), by = distance))
      # 
      # westCol <- rbind(north[order(nrow(north):1),], westStart, south)
      # 
      # allCols <- apply(westCol, 1, function(x) (data.table::as.data.table(destPoint(x, 90, seq(from = 0, to = (distance*150), by = distance)))))
      # 
      # allPts <- data.table::rbindlist(allCols)
      # allPts <- data.table::as.data.table(allPts)
      # allPts <- allPts[, j = .(x = round(lat, 6), y = round(lon, 6))]
      # allPts$api <- paste0(allPts$x, ',', allPts$y)
      # allPts <- allPts[1:100]
      # 
      # ll_prj <- "EPSG:4326"
      # 
      # check <-  elevatr::get_elev_point(locations = data.frame(x = 49.1647009, y = 18.8472173), prj = ll_prj, units = "meters", src = "aws", z = 14)
      # check1 <- data.frame(y = check$elevation / 150)
      # write(jsonlite::toJSON(check1), "elevation.json")
      # 
      # my_get <- function(coordinate) {
      #   httr::GET(
      #     url = "https://api.data-api.io/v1/eoglasna/", 
      #     query = (paste0(coordinate$x, ",", coordinate$y))
      #   )
      # }
      # 
      # lapply(attempt1, my_get)
      # 
      # Results <- lapply(toorder, PostFunciton)
      # 
      # part1 <- 'https://api.open-elevation.com/api/v1/lookup?locations='
      # part2 <- paste()
      # 
      # attempt1 <- list(allPts)
      # 
      # functionboi <- function(x) {
      #   
      #   part1 <- 'https://api.open-elevation.com/api/v1/lookup?locations='
      #   part2 <- noquote(paste0(x$x, ",", x$y))
      #   
      #   apiquery <- paste0(part1, part2, collapse = ",")
      #   
      #   res = httr::GET(apiquery)
      #   data = jsonlite::fromJSON(rawToChar(res$content))
      #   
      #   return(res)
      #   
      # }
      # 
      # res = httr::GET("https://api.open-elevation.com/api/v1/lookup?locations=49.1612143,18.8405987")
      # data = jsonlite::fromJSON(rawToChar(res$content))$results$elevation
      # 
      # attempt2 <- lapply(attempt1, function(x) functionboi(x))
      # 
      # 
      # print(check)

}

# Run the application
shiny::shinyApp(ui = ui, server = server)