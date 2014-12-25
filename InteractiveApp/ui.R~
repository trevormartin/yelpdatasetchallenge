library(shiny)
library(shinyBS)
library(leaflet)

# Choices for drop-downs
colorvars <- c(
  "Significant" = "significant",
  "Overall Review Score" = "totalreviewscore",
  "Overall Sample Size" = "totalsamples",
  "P-value" = "pvalues",
  "Q-value" = "qvalues"
)
sizevars <- c(
  "Overall Review Score" = "totalreviewscore",
  "Overall Sample Size" = "totalsamples",
  "P-value" = "pvalues",
  "Q-value" = "qvalues"
)


shinyUI(navbarPage("Good Food, Bad Service", id="nav",

  tabPanel("Interactive Map",
    div(class="outer",
      
      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),
      
      leafletMap("map", width="100%", height="100%",
        initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
        options=list(
          center = c(36.16694,-115.1733),
          zoom = 11
          #maxBounds = list(list(36.34432,-114.9536), list(35.98955,-115.393)) # Show Las Vegas only 
        )
      ),
      
      absolutePanel(id = "controls", class = "modal", fixed = TRUE, draggable = TRUE,
        top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",
        
        h2("Business Explorer"),p("Full analysis at: ",a("www.goodfoodbadservice.com",href="http://www.goodfoodbadservice.com")),
        
        selectInput("color", "Color", colorvars, selected = "totalreviewscore"),
        selectInput("size", "Size", sizevars, selected = "totalsamples"),
	conditionalPanel("input.color == 'significant' || input.size == 'significant'",
	  # Only prompt for threshold when coloring or sizing by significance
          numericInput("threshold", "FDR threshold (percent false positives)", 5)
	),
        
        plotOutput("histpvals", height = 200),
	bsAlert(inputId = "intermap")
      ),
      
      tags$div(id="cite",
        'Data from the Yelp Dataset Challenge (http://www.yelp.com/dataset_challenge).'
      )
    )
  ),

  tabPanel("Data Explorer",
    bsAlert(inputId = "dataexp"),
    fluidRow(
      column(1,
        numericInput("minpval", "Min p-value", min=0, max=1, value=0)
      ),
      column(1,
        numericInput("maxpval", "Max p-value", min=0, max=1, value=1)
      )
    ),
    hr(),
    dataTableOutput("bustable")
  ),
  
  conditionalPanel("false", icon("crosshair"))
))
