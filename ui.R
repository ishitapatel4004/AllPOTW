library(shiny)

ui <- fluidPage(
  
  # Title
  titlePanel("🚗 POTW LTV Tracker"),
  
  sidebarLayout(
    sidebarPanel(
      
      # 📌 Campaign Selector (Radio Buttons = one choice only)
      radioButtons("campaign", "🗓 Select POTW Campaign:",
                   choices = c("POTW Winter 2023", "POTW Spring 2023",
                               "POTW Winter 2024", "POTW Spring 2024",
                               "POTW Winter 2025", "POTW Spring 2025"),
                   selected = "POTW Spring 2025"),
      
      # 📅 Date Range (dynamic based on selected campaign)
      uiOutput("dynamic_date_ui"),
      
      # 📍 Location Selector
      tags$h4("📍 Select Locations:"),
      selectInput("selected_locations", label = "📍 Select Locations:",
                  choices = available_locations, 
                  selected = available_locations,
                  multiple = TRUE),
      
      # ✅ Location Filters
      checkboxInput("select_all", "✔ Select All Locations", value = TRUE),
      checkboxInput("select_full_service", "🚙 Select Full Service Locations", value = FALSE),
      checkboxInput("select_express", "⛽ Select Express Locations", value = FALSE),
      checkboxInput("show_percentages", "📊 Show Percentages", value = TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("📊 Summary Table", tableOutput("summary_table")),
        tabPanel("🔄 Conversion Analysis", tableOutput("conversion_analysis")),
        tabPanel("📈 Time Series", plotOutput("time_series")),
        tabPanel("📢 Marketing Impact", uiOutput("marketing_impact")),
        tabPanel("📈 YoY Analysis", uiOutput("YoY_Analysis")),
        tabPanel("📉 Sales Drop-Off", uiOutput("sales_dropoff")),
        tabPanel("📍 Location Performance", uiOutput("location_performance")),
        tabPanel("💰 Revenue & Membership Insights", uiOutput("Revenue_LTV_tab"))
      )
    )
  )
)
