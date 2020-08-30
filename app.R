library(dplyr)
library(plotly)
library(rworldmap)
library(shiny)
library(shinyalert)
library(shinyBS)
library(shinydashboard)
library(tibble)


# Simple functions --------------------------------------------------------


# Basic not in function
'%!in%' <- function(x, y)!('%in%'(x, y))

# Replace full stops with spaces
'full.stop.replace' <- function(string) gsub(" ", "\\.", string)

# Capitalise first letter of word 
capital.first.letter <- function(string) {
  substr(string, 1, 1) <- toupper(substr(string, 1, 1)) 
  
  string
}


shinyjscode <- "
shinyjs.init = function() {
  $(window).resize(shinyjs.calcHeight);
}
shinyjs.calcHeight = function() { 
  Shiny.onInputChange('plotHeight', $(window).height());
}
"

# Read in FSA data and format it ------------------------------------------


# Read in FSA data
fsa_data <- read.csv("FSA_data_competition_2020.csv", row.names=1)

# Set Dates to date format
fsa_data$Date.Added <- as.Date(fsa_data$Date.Added, format="%d/%m/%Y")
fsa_data$Date.of.Publishing <- as.Date(fsa_data$Date.of.Publishing, format="%d/%m/%Y")

# Capitalise fcm
fsa_data["Food..Feed.or.FCM"][fsa_data["Food..Feed.or.FCM"] == "fcm"] <- "FCM"

# Remove extra fullstops
colnames(fsa_data)[8] <- "Commodity.Product"
colnames(fsa_data)[10] <- "EU.non.EU.Country.of.Origin"
colnames(fsa_data)[18] <- "Food.Feed.or.FCM"

# Remove underscores
fsa_data$EU.non.EU.Country.of.Origin[fsa_data$EU.non.EU.Country.of.Origin == "non_EU"]<- "non EU"
fsa_data$EU.non.EU.Notifying.Country[fsa_data$EU.non.EU.Notifying.Country == "non_EU"]<- "non EU"

# Variables to be used in the app
most.useful.var <- c("Source.Type", "Alert.Type",
                     "Product.Category", "Commodity.Product",
                     "Country.of.Origin", "EU.non.EU.Country.of.Origin",
                     "Notified.by", "EU.non.EU.Notifying.Country",
                     "Hazard.Group", "Food.Feed.or.FCM",
                     "Manufacturer", "Brand", "Organisations",
                     "Is.A.Food.Article")

# Capitalise first letter of variables used
for (var in most.useful.var) {
  fsa_data[, var] <- capital.first.letter(fsa_data[, var])
}


# Shiny app code ----------------------------------------------------------


ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title="Food hazards from around the world",
                  titleWidth=350),
  
  dashboardSidebar(
    tags$head(tags$style(HTML('.help-btn {
                                margin: 0px 0px 0px 6px !important;
                                padding: 1px 4px !important;
                                font-size: 8px;
                              }
                              .popover {
                                color: #1e282c;
                              }
                              #world {
                                height: calc(100vh - 100px) !important;
                              }
                              #timeseries {
                                height: calc(100vh - 475px) !important;
                                min-height: 300px !important;
                              }'))),
    
    sidebarMenu(id="tab",
      menuItem("General", tabName="general", icon=icon("chart-bar")),
      menuItem("World map", tabName="world_map", icon=icon("globe-africa"))
    ),
    
    uiOutput("out1"),
    uiOutput("out2")

  ),

  
  dashboardBody(
    tabItems(
      tabItem(tabName="general",
              fluidRow(column(width=4,
                       list(
                         valueBox(format(nrow(fsa_data), big.mark=" "), 
                                  "Number of Alerts", icon=icon("arrow-up"), 
                                  color="blue", width=NULL),
                         valueBox(format(as.numeric(max(fsa_data$Date.of.Publishing)-min(fsa_data$Date.of.Publishing)),
                                         big.mark=" "),
                                  "Number of Days", 
                                  icon=icon("calendar"), color="orange", width=NULL),
                         valueBox(length(unique(fsa_data$Country.of.Origin)), "Number of Coutries",  
                                  icon=icon("globe-africa"), color="green", width=NULL))),
                box(status="warning", solidHeader=TRUE,
                    plotlyOutput("piechart", height=330), width=8)
              ),
              fluidRow(
                box(status="warning", solidHeader=TRUE,
                    plotlyOutput("timeseries"), width=12)
              )
      ),
      
      tabItem(tabName="world_map",
              fluidRow(
                box(status="warning", solidHeader=TRUE,
                    plotlyOutput("world"), width=12,
                    align="center")
              )
      )
    )
  ),
  
  useShinyalert()
  
)

server <- function(input, output, session) {
  # Start up message
  shinyalert(
              title="Welcome to the Food hazards from around the world app",
              text=paste("This app has two tabs: a general tab for exploring the dataset characteristics, ",
                         "and a world map to look at how countries from around the world differ. \n All graphs",
                         "are interactive so get exploring!"),
              closeOnEsc=TRUE,
              closeOnClickOutside=FALSE,
              html=FALSE,
              type="success",
              showConfirmButton=TRUE,
              showCancelButton=FALSE,
              confirmButtonText="OK",
              confirmButtonCol="#3c8dbc",
              timer=0,
              imageUrl="",
              animation=TRUE
            )
  
  # Tab specific inputs
  output$out1 <- renderUI({
    if (input$tab == "general") {
      dyn_ui <- list(sliderInput(inputId="max.no.cat",
                                 label="Maximum number of categories",
                                 min=2,
                                 max=10,
                                 value=3,
                                 ticks=FALSE),
                      
                      selectInput(inputId="column",
                                  label="Plotting variable",
                                  choices=gsub("\\.", " ", most.useful.var),
                                  selected="Country.of.Origin"),
                      
                      selectInput(inputId="period",
                                  label="Time frequency",
                                  choices=c("Daily", "Weekly", "Monthly", "Annually"),
                                  selected="Daily"))
    } else if (input$tab == "world_map") {
      dyn_ui <- list(
                     selectInput(inputId="projection",
                                 label="Map projection",
                                 choices=c("Orthographic", "Equirectangular"),
                                 selected="Orthographic"),
                     
                     selectInput(inputId="scale",
                                 label="Scale",
                                 choices=c("Logarithm", "Linear"),
                                 selected="Logarithm"),
                     
                     selectInput(inputId="country",
                                 label="Sum alerts over",
                                 choices=gsub("\\.", " ", c("Country.of.Origin", "Notified.by")),
                                 selected=gsub("\\.", " ", "Country.of.Origin")),
                     
                     div(selectInput(inputId="secondary.var",
                                     label=div(span("Filtering variable"),
                                               bsButton("q1", label="", icon=icon("question"), style="primary", 
                                                        size="extra-small", class="pull-right help-btn"),
                                               bsPopover("q1", title="",
                                                         content=paste("Filter the dataset to only include entries where", 
                                                         "the filtering variable is equal to the filtering value. Filtering",
                                                         "values appear once the filtering variable is selected."),
                                                         placement="top", trigger="hover")),
                                     choices=gsub("\\.", " ", c("None", "Source.Type", "Alert.Type", 
                                                                "Product.Category", "Hazard.Group", 
                                                                "Food.Feed.or.FCM")),
                                     selected="None")))
    }
    dyn_ui
  })
  
  # Extra inputs if filtering world map
  output$out2 <- renderUI({
    if (input$tab == "world_map") {
      req(input$secondary.var)
      if (input$secondary.var != "None") {
        optional.extra <- list(                     
          selectInput(inputId="secondary.var.select",
                      label="Filtering value",
                      choices=NULL,
                      selected=NULL),
          selectInput(inputId="secondary.var.type",
                      label=div(span("Filtering variable"),
                                bsButton("q2", label="", icon=icon("question"), style="primary", 
                                         size="extra-small", class="pull-right help-btn"),
                                bsPopover("q2", title="",
                                          content=paste("Plot the alerts matching the filter as an absolute total",
                                                        "for that country, or as a percentage of that",
                                                        "country&apos;s total alerts."),
                                          placement="top", trigger="hover")),
                      choices = c("Absolute", "Percentage"),
                      selected = "Absolute"))
      } else {
        optional.extra <- NULL
      }
    } else {
      optional.extra <- NULL
    }
    optional.extra
  })
  
  # Update max.no.cat input with variable
  observe({
    if (!is.null(input$column)) {
      updateSliderInput(session, "max.no.cat",
                        max=length(na.omit(unique(fsa_data[, full.stop.replace(input$column)]))),
                        value=min(c(10, length(na.omit(unique(fsa_data[, full.stop.replace(input$column)]))))))
    }
  })
  
  
  # Update optional inputs with selected filtering value
  observe({
    if (!is.null(input$secondary.var)) {
      if (input$secondary.var != "None") {
        updateSelectInput(session, "secondary.var.select",
                          choices=na.omit(unique(fsa_data[full.stop.replace(input$secondary.var)])[[1]]),
                          selected=na.omit(unique(fsa_data[full.stop.replace(input$secondary.var)])[[1]])[[1]])
      }
    }
    })
  
  # Calculate top x categories and their counts
  top.10 <- reactive({
    req(input$column)
    data <- data.frame(table(fsa_data[full.stop.replace(input$column)]))
    reordered.data <- data[order(-data$Freq), ]
    if (nrow(data) > input$max.no.cat) {
      limited.data <- reordered.data[1:(input$max.no.cat-1),]
      other.row <- data.frame(Var1="Other", Freq=sum(reordered.data$Freq[input$max.no.cat:dim(data)[1]]))
      other.data <- rbind(limited.data, other.row) 
    } else {
      other.data <- reordered.data
    }
    
    other.data
  })
  
  # Plot top x categories as a time series
  top.10.timeseries <- reactive({
    req(input$column)
    data <- data.frame(table(fsa_data[full.stop.replace(input$column)]))
    
    other.data <- top.10()
    
    # Format x axis nicely
    if (input$period == "Annually") {
      fsa_data$Date.of.Publishing <- format(fsa_data$Date.of.Publishing, "%Y")
    } else if (input$period == "Monthly") {
      fsa_data$Date.of.Publishing <- format(fsa_data$Date.of.Publishing, "%Y/%m")
    } else if (input$period == "Weekly") {
      fsa_data$Date.of.Publishing <- format(fsa_data$Date.of.Publishing, "%Y/%W")
    } else if (input$period == "Daily") {
      fsa_data$Date.of.Publishing <- format(fsa_data$Date.of.Publishing, "%Y/%m/%d")
    } 
    
    top.9 <- fsa_data %>% filter(!!as.symbol(full.stop.replace(input$column)) %in% as.vector(other.data$Var1))
    top.9.freq <- data.frame(table(top.9[c("Date.of.Publishing", full.stop.replace(input$column))]))
    names(top.9.freq)[2] <- "Var1"
    
    if (nrow(data) > input$max.no.cat) {
      other <- fsa_data %>% filter(!!as.symbol(full.stop.replace(input$column)) %!in% as.vector(other.data$Var1))
      other.freq <- data.frame(table(other["Date.of.Publishing"]))
      other.freq <- add_column(other.freq, d="Other", .after="Var1")
      names(other.freq) <- names(top.9.freq)
      top.10.data <- rbind(top.9.freq, other.freq)

    } else {
      top.10.data <- top.9.freq
    }
    
    # Forgets its a date for some reason
    if (input$period == "Daily") {
      top.10.data$Date.of.Publishing <- as.Date(top.10.data$Date.of.Publishing, format="%Y/%m/%d")
    } 
      
    top.10.data
  })
  
  # Match dataset to countries of world locations
  country.grouping <- reactive({
    req(input$country)
    fsa_countries <- unique(fsa_data[full.stop.replace(input$country)])[[1]]
    test.df <- data.frame(country=fsa_countries, madeup=rep(1, length(fsa_countries)))
    test <- joinCountryData2Map(test.df, joinCode="NAME", nameJoinColumn="country")
    
    mask <- which((test$madeup == 1) == TRUE)
    
    matched.countries <- test$country[mask]
    matched.lat <- test$LAT[mask]
    matched.lon <- test$LON[mask]
    
    matched.df <- data.frame(country=matched.countries, lat=matched.lat, lon=matched.lon)
    
    matched.df
  })
  
  
  # Pie chart of data 
  output$piechart <- renderPlotly({
    data <- top.10()
    fig <- plot_ly(data, labels=~Var1, values=~Freq, type="pie", height=330)
    fig <- fig %>% layout(title=paste("Proportion of alerts in the dataset split by", input$column))
  })
  
  # Time series of data 
  output$timeseries <- renderPlotly({
    
    data <- top.10.timeseries()
    
    # Plot
    fig <- plot_ly(data, x=~Date.of.Publishing, y=~Freq, color=~Var1, type="scatter", mode="lines")
    fig <- fig %>% layout(title=paste("Time series of alerts in the dataset split by", input$column), 
                          xaxis=list(title="Date of Publishing"), 
                          yaxis=list(title="Number of Alerts"))
  })
  
  # World map of data
  output$world <- renderPlotly({
    matched.df <- country.grouping()
    
    if (input$secondary.var == "None") {
      freq.table <- data.frame(table(fsa_data[full.stop.replace(input$country)]))
    } else {
      mask <- fsa_data[full.stop.replace(input$secondary.var)]==input$secondary.var.select
      freq.table <- data.frame(table(fsa_data[mask,][full.stop.replace(input$country)]))
      req(input$secondary.var.type)
      if (input$secondary.var.type == "Percentage") {
        freq.table <- merge(freq.table, data.frame(table(fsa_data[full.stop.replace(input$country)])), by="Var1", all=T)
        freq.table$Freq <- (freq.table$Freq.x / freq.table$Freq.y) * 100
      }
    }
    
    indices <- sapply(matched.df$country, function(i) which(i == freq.table))
    
    matched.df$no.alerts <- sapply(as.integer(indices), function(i) freq.table$Freq[i])
    if (input$scale == "Logarithm") {
      matched.df$no.alerts.scale <- log10(matched.df$no.alerts)
      scale.prefix <- "10 <sup>"
      scale.suffix <- "</sup> %"
    } else {
      matched.df$no.alerts.scale <- matched.df$no.alerts
      scale.prefix <- ""
      scale.suffix <- " %"
    }
    matched.df <- na.omit(matched.df)
    
    if (input$secondary.var == "None") {
      hovertext <- paste(matched.df$country, "-", matched.df$no.alerts, "alerts")
    } else if (input$secondary.var.type == "Percentage") {
      hovertext <- paste(matched.df$country, "-", formatC(signif(matched.df$no.alerts, digits=2), digits=2, format="fg"), "%")
    } else {
      hovertext <- paste(matched.df$country, "-", matched.df$no.alerts, "alerts")
    }
    
    fig <- plot_geo()
    
    fig <- fig %>% add_markers(
      data = matched.df, x=~lon, y=~lat, text=hovertext,
      size =1, hoverinfo="text", color=~no.alerts.scale,
      alpha = 0.5
    )
    
    geo <- list(
      projection = list(type=tolower(input$projection)),
      showland = TRUE,
      landcolor = toRGB("gray95")
    )
    
    fig <- fig %>% layout(geo=geo) %>% colorbar(tickprefix=scale.prefix, x=1.05, y=0.75)
    
    if (input$secondary.var == "None") {
      fig <- fig %>% colorbar(title="Number of Alerts")
    } else if (input$secondary.var.type == "Percentage") {
      fig <- fig %>% colorbar(title="Percentage of Alerts", ticksuffix=scale.suffix)
    } else {
      fig <- fig %>% colorbar(title="Number of Alerts")
    }
    
    fig
    
  })


}

shinyApp(ui, server)
