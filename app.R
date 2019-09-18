# implementing app to visualize influence of margin size on predictive performance
library(shiny)

server <- shinyServer(function(input, output) {
  source("app_imports.R")

  data <- reactive({
    set.seed(123)
    sites <- readRDS("Daten/evidence.csv") %>%
      dplyr::filter(site == 1) %>%
      dplyr::select(lon, lat)
    nonsites <- buffsample(ssize = 8000, distance = input$buffer, returnsize = 12000)
    evidence <- generateEvidenceApp(sitesdata = sites, nonsitesdata = nonsites)
    evidence <- finalizeEvidence(evidence)
    subset_sites <- sample_n(sites, size = input$ssize)
    sp_sites <- sp::SpatialPoints(coords = subset_sites[, c("lon", "lat")], proj4string = predictors@crs)
    nonsites <- dplyr::filter(evidence, site == F) %>% dplyr::select(lon, lat)
    subset_nonsites <- sample_n(nonsites, size = input$ssize)
    sp_nonsites <- sp::SpatialPoints(coords = subset_nonsites[, c("lon", "lat")], proj4string = predictors@crs)
    form <- paste(input$variables, collapse = " + ")
    if (identical(form, "")) {
      form <- "site ~ dem"
    } else {
      form <- paste("site ~ dem", form, sep = " + ")
    }
    basefit <- glm(
      formula = form,
      family = binomial(),
      data = evidence
    )
    list(
      bsf = basefit, evd = evidence, formula = form, spsite = sp_sites,
      spnsite = sp_nonsites
    )
  })
  output$map <- renderPlot({
    df <- as.data.frame(predictors)
    pdata <- predict(data()$bsf, newdata = df, type = "response")
    x_pred <- predictors
    x_pred$pred <- pdata
    plot(x_pred$pred, main = "Predictive Map: Logistic Regression", ylab = "Latitude", xlab = "Longitude")
    if (input$points == T) {
      plot(data()$spsite, add = TRUE, col = "blue")
    }
    if (input$npoints == T) {
      plot(data()$spnsite, add = TRUE, col = "red")
    }
  })

  output$auroc <- renderPrint({
    pROC::auc(data()$evd$site, fitted(data()$bsf))
  })

  output$formel <- renderPrint({
    cat("Model formula: ", "\n", data()$formula)
  })
})


ui <- shinyUI(fluidPage(
  theme = shinytheme("journal"),
  titlePanel("Predictive Performance vs. Buffersize"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("buffer",
        "Radius of Buffer:",
        min = 50,
        max = 7500,
        value = 50
      ),
      sliderInput("ssize",
        "Number of Points to Render:",
        min = 50,
        max = 5000,
        value = 500
      ),
      checkboxGroupInput("variables", "Variable Selection:",
        c(
          "Temperature" = "temp", "Rain" = "rain", "Distance to Water" = "distance_water",
          "Frostdays" = "frostdays", "Sunhours" = "sunhours", "Topographic Position Index" = "tpi",
          "Slope" = "slope", "Aspect" = "aspect"
        ),
        selected = c(
          "Temperature" = "temp", "Rain" = "rain", "Distance to Water" = "distance_water",
          "Frostdays" = "frostdays", "Sunhours" = "sunhours", "Topographic Position Index" = "tpi",
          "Slope" = "slope", "Aspect" = "aspect"
        )
      ),
      checkboxInput("points", "Render Sites?", FALSE),
      checkboxInput("npoints", "Render Nonsites?", FALSE),
      submitButton(text = "Refresh!", icon = icon("refresh"), width = NULL)
    ),

    mainPanel(
      plotOutput("map"),
      textOutput("auroc"),
      textOutput("formel")
    )
  )
))


shinyApp(ui = ui, server = server)
