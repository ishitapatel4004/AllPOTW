setwd("C:/Users/TWIntern/OneDrive - Team Car Wash/Desktop/AllPOTW")

library(shiny)
library(tidyverse)


# Begin server function
function(input, output, session) {
  fix_location_names <- function(df) {
    df %>%
      mutate(LOCATION = str_replace_all(LOCATION, c(
        "Monmouth Junct" = "Monmouth Junction",
        "Tices Ln"      = "Tices Lane",
        "Roselle Pk"    = "Roselle Park"
      )))
  }
  
  # ---- Dynamic Date UI: Constrain to selected campaign ----
  output$dynamic_date_ui <- renderUI({
    req(input$campaign)
    
    # Hardcoded date ranges by campaign
    campaign_ranges <- list(
      "POTW Spring 2023" = c(as.Date("2023-04-22"), as.Date("2023-05-23")),
      "POTW Winter 2023" = c(as.Date("2023-01-01"), as.Date("2023-02-27")),
      "POTW Spring 2024" = c(as.Date("2024-04-25"), as.Date("2024-05-27")),
      "POTW Winter 2024" = c(as.Date("2024-01-08"), as.Date("2024-03-12")),
      "POTW Spring 2025" = c(as.Date("2025-04-23"), as.Date("2025-05-24")),
      "POTW Winter 2025" = c(as.Date("2025-01-11"), as.Date("2025-02-05"))
    )
    
    range <- campaign_ranges[[input$campaign]]
    
    dateRangeInput("date_range", "📅 Filter by Date Range:",
                   start = range[1], end = range[2],
                   min = range[1], max = range[2])
  })
  
  full_service_locations <- c("Piscataway", "Westfield", "Summit", "Woodbridge")
  express_locations <- setdiff(available_locations, full_service_locations)
  
  observe({
    selected <- input$selected_locations
    
    if (input$select_all) {
      selected <- available_locations
    } else if (input$select_full_service) {
      selected <- full_service_locations
    } else if (input$select_express) {
      selected <- express_locations
    }
    
    updateSelectInput(session, "selected_locations",
                      choices = available_locations,
                      selected = selected)
  })
  
  filtered_data <- reactive({
    req(input$campaign, input$date_range, input$selected_locations)
    
    fix_location_names(POTW_All_Campaigns) %>%
      filter(Campaign == input$campaign) %>%
    filter(`CREATED DATE` >= input$date_range[1] & `CREATED DATE` <= input$date_range[2]) %>%
    filter(LOCATION %in% input$selected_locations)
  })
  
  

  
  # ---- Summary Table ---------------------
  output$summary_table <- renderTable({
    data <- filtered_data() %>%
      group_by(LOCATION) %>%
      summarise(
        `Total Washes` = sum(TOTAL_WASHES, na.rm = TRUE),
        `Free Washes` = sum(FREE_WASHES, na.rm = TRUE),
        `Redeemed Washes` = sum(REDEEMED_WASHES, na.rm = TRUE),
        `Eligible Washes` = sum(ELIGIBLE_WASHES, na.rm = TRUE),
        `Memberships Sold` = sum(SALES, na.rm = TRUE),
        `Conversion Rate Raw` = sum(SALES, na.rm = TRUE) / sum(ELIGIBLE_WASHES, na.rm = TRUE),
        `Total Memberships Sold` = sum(SALES, na.rm = TRUE),
        `Total Eligible Washes` = sum(ELIGIBLE_WASHES, na.rm = TRUE)
      ) %>%
      arrange(factor(LOCATION, levels = available_locations)) %>%
      mutate(across(c(`Total Washes`, `Free Washes`, `Redeemed Washes`, 
                      `Eligible Washes`, `Memberships Sold`), 
                    ~ formatC(.x, format = "f", digits = 0, big.mark = ",")))
    
    totals_row <- data %>%
      summarise(
        LOCATION = "TOTAL",
        `Total Washes` = sum(as.numeric(gsub(",", "", `Total Washes`))),
        `Free Washes` = sum(as.numeric(gsub(",", "", `Free Washes`))),
        `Redeemed Washes` = sum(as.numeric(gsub(",", "", `Redeemed Washes`))),
        `Eligible Washes` = sum(as.numeric(gsub(",", "", `Eligible Washes`))),
        `Memberships Sold` = sum(as.numeric(gsub(",", "", `Memberships Sold`))),
        `Conversion Rate Raw` = sum(`Memberships Sold`) / sum(`Eligible Washes`),
        `Total Memberships Sold` = sum(`Memberships Sold`),
        `Total Eligible Washes` = sum(`Eligible Washes`)
      ) %>%
      mutate(across(c(`Total Washes`, `Free Washes`, `Redeemed Washes`, 
                      `Eligible Washes`, `Memberships Sold`), 
                    ~ formatC(.x, format = "f", digits = 0, big.mark = ",")))
    
    data <- bind_rows(data, totals_row)
    
    # Handle Percentage Display
    if (input$show_percentages) {
      data <- data %>%
        mutate(`Conversion Rate` = paste0(round(`Conversion Rate Raw` * 100, 2), "%")) %>%
        select(-`Conversion Rate Raw`, -`Total Memberships Sold`, -`Total Eligible Washes`)
    } else {
      data <- data %>%
        mutate(`Conversion Rate` = paste(`Total Memberships Sold`, "/", `Total Eligible Washes`)) %>%
        select(-`Conversion Rate Raw`, -`Total Memberships Sold`, -`Total Eligible Washes`)
    }
    
    rownames(data) <- NULL
    data <- data %>%
      mutate(LOCATION = ifelse(LOCATION == "TOTAL", 
                               "<span style='color: blue; font-weight: bold;'>TOTAL</span>", 
                               LOCATION))
    return(data)
  }, sanitize.text.function = function(x) x)
  

  # ---- Conversion Analysis ------
  output$conversion_analysis <- renderTable({
    data <- filtered_data() %>%
      group_by(LOCATION) %>%
      summarise(
        `Total Eligible Washes` = sum(ELIGIBLE_WASHES, na.rm = TRUE),
        `Total Memberships Sold` = sum(SALES, na.rm = TRUE),
        `Conversion Rate Raw` = sum(SALES, na.rm = TRUE) / sum(ELIGIBLE_WASHES, na.rm = TRUE)
      ) %>%
      arrange(factor(LOCATION, levels = available_locations)) %>%
      mutate(across(c(`Total Eligible Washes`, `Total Memberships Sold`), 
                    ~ formatC(.x, format = "f", digits = 0, big.mark = ",")))
    
    totals_row <- data %>%
      summarise(
        LOCATION = "TOTAL",
        `Total Eligible Washes` = sum(as.numeric(gsub(",", "", `Total Eligible Washes`))),
        `Total Memberships Sold` = sum(as.numeric(gsub(",", "", `Total Memberships Sold`))),
        `Conversion Rate Raw` = sum(`Total Memberships Sold`) / sum(`Total Eligible Washes`)
      ) %>%
      mutate(across(c(`Total Eligible Washes`, `Total Memberships Sold`), 
                    ~ formatC(.x, format = "f", digits = 0, big.mark = ",")))
    
    data <- bind_rows(data, totals_row)
    
    # Show as percentage or ratio
    if (input$show_percentages) {
      data <- data %>%
        mutate(`Conversion Rate` = paste0(round(`Conversion Rate Raw` * 100, 2), "%")) %>%
        select(-`Conversion Rate Raw`)
    } else {
      data <- data %>%
        mutate(`Conversion Rate` = paste0(`Total Memberships Sold`, "/", `Total Eligible Washes`)) %>%
        select(-`Conversion Rate Raw`)
    }
    
    rownames(data) <- NULL
    data <- data %>%
      mutate(LOCATION = ifelse(LOCATION == "TOTAL", 
                               "<span style='color: blue; font-weight: bold;'>TOTAL</span>", 
                               LOCATION))
    return(data)
  }, sanitize.text.function = function(x) x)
  
  
  
  # ----- Time Series --------
  output$time_series <- renderPlot({
    data <- filtered_data() %>%
      group_by(`CREATED DATE`) %>%
      summarise(`Memberships Sold` = sum(SALES, na.rm = TRUE)) %>%
      arrange(`CREATED DATE`)
    
    # 🛑 Exit early if no data
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    ggplot(data, aes(x = `CREATED DATE`, y = `Memberships Sold`)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "red", size = 3) +
      geom_text(aes(label = `Memberships Sold`), vjust = -0.7, size = 4, fontface = "bold") +
      labs(title = "📈 Membership Sales Over Time",
           x = "Date", y = "Memberships Sold") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(face = "bold", size = 16)) +
      scale_x_date(date_labels = "%b %d", date_breaks = "1 day") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
  })
  
  #------- Marketing Impact ----
  output$marketing_impact <- renderUI({
    req(input$campaign)
    
    # Pull data for just the selected campaign
    campaign_data <- POTW_All_Campaigns %>%
      filter(Campaign == input$campaign)
    
    # Auto-detect campaign range
    start_date <- min(campaign_data$`CREATED DATE`, na.rm = TRUE)
    end_date <- max(campaign_data$`CREATED DATE`, na.rm = TRUE)
    
    # Auto-detect first two days
    sorted_dates <- sort(unique(campaign_data$`CREATED DATE`))
    day1 <- sorted_dates[1]
    day2 <- sorted_dates[2]
    
    # Sales figures
    total_sales <- sum(campaign_data$SALES, na.rm = TRUE)
    days_total <- as.numeric(difftime(end_date, start_date, units = "days")) + 1
    avg_daily_sales <- total_sales / days_total
    
    day1_sales <- campaign_data %>% filter(`CREATED DATE` == day1) %>% summarise(SALES = sum(SALES)) %>% pull(SALES)
    day2_sales <- campaign_data %>% filter(`CREATED DATE` == day2) %>% summarise(SALES = sum(SALES)) %>% pull(SALES)
    
    first_two_days_sales <- day1_sales + day2_sales
    day1_over_avg <- day1_sales - avg_daily_sales
    day2_over_avg <- day2_sales - avg_daily_sales
    first_two_days_percent <- (first_two_days_sales / total_sales) * 100
    revenue_generated <- (day1_over_avg + day2_over_avg) * 377.07
    
    format_number <- function(x) format(round(x, 0), big.mark = ",", scientific = FALSE)
    
    # Render UI
    tagList(
      h3("📢 Impact of Marketing Text on Membership Sales", style = "text-align: center; font-weight: bold;"),
      
      fluidRow(
        column(4, h4("📅 Campaign Duration", style = "text-align: center;"),
               p(paste(format_number(days_total), "days"), style = "text-align: center; font-size: 18px;")),
        column(4, h4("📊 Total Memberships Sold", style = "text-align: center;"),
               p(format_number(total_sales), style = "text-align: center; font-size: 18px;")),
        column(4, h4("📈 Avg Memberships Sold Per Day", style = "text-align: center;"),
               p(format_number(avg_daily_sales), style = "text-align: center; font-size: 18px;"))
      ),
      
      fluidRow(
        column(4, h4(paste0("📅 ", format(day1, "%b %d"), " Sales"), style = "text-align: center;"),
               p(format_number(day1_sales), style = "text-align: center; font-size: 18px;")),
        column(4, h4(paste0("📅 ", format(day2, "%b %d"), " Sales"), style = "text-align: center;"),
               p(format_number(day2_sales), style = "text-align: center; font-size: 18px;")),
        column(4, h4("🎯 First 2 Days Sales", style = "text-align: center;"),
               p(format_number(first_two_days_sales), style = "text-align: center; font-size: 18px; font-weight: bold; color: red;"),
               p(paste0("(", round(first_two_days_percent, 2), "% of total sales)"), style = "text-align: center; font-size: 16px; color: gray;"))
      ),
      
      fluidRow(
        column(4, h4(paste0("🚀 ", format(day1, "%b %d"), " Sales Over Avg"), style = "text-align: center;"),
               p(format_number(day1_over_avg), style = "text-align: center; font-size: 18px; color: blue;")),
        column(4, h4(paste0("🚀 ", format(day2, "%b %d"), " Sales Over Avg"), style = "text-align: center;"),
               p(format_number(day2_over_avg), style = "text-align: center; font-size: 18px; color: blue;")),
        column(4, h4("💰 Revenue from Over Avg Sales", style = "text-align: center;"),
               p(paste0("$", format_number(revenue_generated)), style = "text-align: center; font-size: 18px; font-weight: bold; color: green;"))
      ),
      
      # Dynamically render the bar chart
      renderPlot({
        bar_data <- tibble(
          Date = c(format(day1, "%b %d"), format(day2, "%b %d"), "Campaign Avg"),
          Sales = c(day1_sales, day2_sales, avg_daily_sales)
        )
        
        ggplot(bar_data, aes(x = Date, y = Sales, fill = Date)) +
          geom_bar(stat = "identity", width = 0.6) +
          geom_text(aes(label = format_number(Sales)), vjust = -0.5, size = 5, fontface = "bold") +
          labs(title = paste("📊 Memberships Sold on", format(day1, "%b %d"), "&", format(day2, "%b %d"), "vs Campaign Average"),
               x = "Date", y = "Memberships Sold") +
          theme_minimal() +
          scale_fill_manual(values = setNames(
            c("blue", "red", "gray"),
            c(format(day1, "%b %d"), format(day2, "%b %d"), "Campaign Avg")
          )) +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16))
        
        
      })
    )
  })
  
  
  #------ YoY Comparison------

  POTW_All_Campaigns <- POTW_All_Campaigns %>%
    mutate(
      Year = year(`CREATED DATE`),
      Service_Type = ifelse(LOCATION %in% full_service_locations, "Full Service", "Express")
    )
  
  
  campaign_lengths <- POTW_All_Campaigns %>%
    group_by(Year) %>%
    summarise(Campaign_Days = as.numeric(difftime(max(`CREATED DATE`), min(`CREATED DATE`), units = "days")) + 1, .groups = "drop")
  
  yoy_summary <- POTW_All_Campaigns %>%
    group_by(Year) %>%
    summarise(
      Total_Washes = sum(TOTAL_WASHES, na.rm = TRUE),
      Eligible_Washes = sum(ELIGIBLE_WASHES, na.rm = TRUE),
      Memberships_Sold = sum(SALES, na.rm = TRUE),
      Conversion_Rate = round(sum(SALES, na.rm = TRUE) / sum(ELIGIBLE_WASHES, na.rm = TRUE) * 100, 2),
      Revenue = sum(SALES, na.rm = TRUE) * 377.07,
      .groups = "drop"
    ) %>%
    left_join(campaign_lengths, by = "Year") %>%
    mutate(
      Efficiency_Sold_Per_Wash = round(Memberships_Sold / Total_Washes, 4),
      Sales_Per_Day = round(Memberships_Sold / Campaign_Days, 1),
      `Total Washes` = formatC(Total_Washes, format = "f", digits = 0, big.mark = ","),
      `Eligible Washes` = formatC(Eligible_Washes, format = "f", digits = 0, big.mark = ","),
      `Memberships Sold` = formatC(Memberships_Sold, format = "f", digits = 0, big.mark = ","),
      `Estimated Revenue ($)` = paste0("$", formatC(Revenue, format = "f", digits = 2, big.mark = ",")),
      `Conversion Rate` = paste0(Conversion_Rate, "%"),
      `Efficiency (Sold/Wash)` = Efficiency_Sold_Per_Wash,
      `Sales Per Day` = formatC(Sales_Per_Day, format = "f", digits = 1)
    )
  
  yoy_growth <- reactive({
    df <- POTW_All_Campaigns %>%
      mutate(Year = year(`CREATED DATE`)) %>%
      group_by(Year) %>%
      summarise(Sold = sum(SALES, na.rm = TRUE), .groups = "drop")
    
    sold_2023 <- df$Sold[df$Year == 2023]
    sold_2025 <- df$Sold[df$Year == 2025]
    
    if (!is.na(sold_2023) && sold_2023 > 0) {
      growth <- round(((sold_2025 - sold_2023) / sold_2023) * 100, 1)
      paste0("+", growth, "%")
    } else {
      "N/A"
    }
  })
  
  
  avg_sales_per_day_all_years <- reactive({
    df <- POTW_All_Campaigns %>%
      group_by(Year) %>%
      summarise(
        TotalSales = sum(SALES, na.rm = TRUE),
        Days = as.numeric(difftime(max(`CREATED DATE`), min(`CREATED DATE`), units = "days")) + 1,
        .groups = "drop"
      ) %>%
      summarise(
        TotalSalesAll = sum(TotalSales),
        TotalDaysAll = sum(Days),
        OverallDaily = round(TotalSalesAll / TotalDaysAll, 1)
      )
    
    paste0(df$OverallDaily, "/day")
  })
  
  
  output$yoy_kpi_table <- renderTable({
    yoy_summary %>%
      mutate(Year = as.character(as.integer(Year))) %>%
      select(
        Year,
        `Total Washes`,
        `Eligible Washes`,
        `Memberships Sold`,
        `Conversion Rate`,
        `Sales Per Day`,
        `Estimated Revenue ($)`
      )
  }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "s")
  output$auto_growth <- renderText({ yoy_growth() })
  output$auto_best_daily <- renderText({ avg_sales_per_day_all_years() })
  
  
  output$yoy_line_chart <- renderPlot({
    POTW_All_Campaigns %>%
      mutate(
        Year = year(`CREATED DATE`),
        MonthDay = format(`CREATED DATE`, "%b %d"),
        MonthDayDate = as.Date(format(`CREATED DATE`, "%m-%d"), format = "%m-%d")
      ) %>%
      group_by(Year, MonthDay, MonthDayDate) %>%
      summarise(Memberships_Sold = sum(SALES, na.rm = TRUE), .groups = "drop") %>%
      arrange(MonthDayDate) %>%
      ggplot(aes(x = reorder(MonthDay, MonthDayDate), y = Memberships_Sold, color = as.factor(Year), group = Year)) +
      geom_line(size = 1) +
      geom_point(size = 1.5) +
      labs(
        title = "📈 Memberships Sold by Day of Campaign (All Years)",
        x = "Day of Campaign", y = "Memberships Sold", color = "Year"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", hjust = 0.5)
      )
  })
  
  output$yoy_bar_chart <- renderPlot({
    ggplot(yoy_summary, aes(x = as.factor(Year), y = as.numeric(gsub("%", "", `Conversion Rate`)), fill = as.factor(Year))) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = `Conversion Rate`), vjust = -0.5, size = 5) +
      labs(title = "📉 Conversion Rate by Year", x = "Year", y = "Conversion Rate (%)") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
      guides(fill = "none")
  })
  
  output$location_heatmap <- renderPlot({
    loc_summary <- POTW_All_Campaigns %>%
      group_by(Year, LOCATION) %>%
      summarise(
        Eligible = sum(ELIGIBLE_WASHES, na.rm = TRUE),
        Sold = sum(SALES, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Conversion = Sold / Eligible)
    
    ggplot(loc_summary, aes(x = as.factor(Year), y = forcats::fct_reorder(LOCATION, Conversion, .fun = sum), fill = Conversion)) +
      geom_tile() +
      geom_text(aes(label = paste0(round(Conversion * 100, 1), "%")), size = 4) +
      scale_fill_gradient(low = "white", high = "#0073e6") +
      labs(title = "📍 Conversion Rates by Location & Year", x = "Year", y = "Location") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5))
  })
  
  output$service_type_share <- renderPlot({
    data <- POTW_All_Campaigns %>%
      group_by(Year, Service_Type) %>%
      summarise(Memberships = sum(SALES), .groups = "drop") %>%
      group_by(Year) %>%
      mutate(Percent = Memberships / sum(Memberships))
    
    ggplot(data, aes(x = as.factor(Year), y = Percent, fill = Service_Type)) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(label = paste0(round(Percent * 100, 1), "%")), 
                position = position_stack(vjust = 0.5), size = 5, color = "white", fontface = "bold") +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(title = "🔁 Express vs Full Service Membership Share", x = "Year", y = "Membership Share", fill = "Service Type") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5))
  })
  
  
output$top_bottom_locations <- renderTable({
  loc_metrics <- POTW_All_Campaigns %>%
    group_by(Year, LOCATION) %>%
    summarise(
      Eligible = sum(ELIGIBLE_WASHES),
      Sold = sum(SALES),
      Conversion = round(Sold / Eligible * 100, 2),
      .groups = "drop"
    )

  top_conv <- loc_metrics %>% group_by(Year) %>% slice_max(Conversion, n = 1) %>% mutate(Metric = "Top by Conversion")
  bot_conv <- loc_metrics %>% group_by(Year) %>% slice_min(Conversion, n = 1) %>% mutate(Metric = "Bottom by Conversion")
  top_sales <- loc_metrics %>% group_by(Year) %>% slice_max(Sold, n = 1) %>% mutate(Metric = "Top by Sales")
  bot_sales <- loc_metrics %>% group_by(Year) %>% slice_min(Sold, n = 1) %>% mutate(Metric = "Bottom by Sales")

  bind_rows(top_conv, bot_conv, top_sales, bot_sales) %>%
    mutate(
      Year = as.character(Year),
      Sold = formatC(Sold, format = "f", digits = 0, big.mark = ","),
      Conversion = paste0(round(Conversion, 2), "%")
    ) %>%
    arrange(Metric, Year)
}, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "s")

  
  output$exec_summary <- renderUI({
    tagList(
      h4("🧾 Executive Summary"),
      tags$ul(
        tags$li("📊 Revenue rose 77% from 2023 to 2025."),
        tags$li("📈 Conversion Rate climbed from 9.9% to 17.0%."),
        tags$li("⚡ Campaign Sales Efficiency improved each year."),
        tags$li("🏁 2025 had the best daily performance (Memberships per Day)."),
        tags$li("🔄 Full-Service sites consistently lead, but Express is gaining share.")
      )
    )
  })
  
  # ---- Render UI for YoY Tab ----
  output$YoY_Analysis <- renderUI({
    tagList(
      h2("📊 Year-over-Year Campaign Performance", style = "text-align: center; font-weight: bold;"),
      
      fluidRow(
        # LEFT SIDE (Table + KPI Boxes)
        column(6,
               h3("🧮 KPI Summary by Year", style = "text-align: center; font-weight: bold;"),
               tableOutput("yoy_kpi_table"),
               br(),
               div(style = "background-color:#f5f5f5; padding:15px; margin-bottom:10px; border-radius:8px; text-align:center;",
                   h4("📈 YoY Growth (Memberships Sold)"),
                   h2(textOutput("auto_growth"), style = "color:green; font-weight:bold;")
               ),
               div(style = "background-color:#f5f5f5; padding:15px; border-radius:8px; text-align:center;",
                   h4("📊 Avg Memberships Sold Per Day (All Years)"),
                   h2(textOutput("auto_best_daily"), style = "color:purple; font-weight:bold;")
               )
        ),
        
        # RIGHT SIDE: Taller bar chart
        column(6,
               h3("📉 Conversion Rate by Year", style = "text-align: center; font-weight: bold;"),
               plotOutput("yoy_bar_chart", height = "550px")  # Stretch height to align with left
        )
      ),
      
      hr(),
      
      fluidRow(
        column(12,
               h3("📈 Memberships Sold by Day of Campaign (All Years)", style = "text-align: center; font-weight: bold;"),
               plotOutput("yoy_line_chart", height = "400px")
        )
      ),
      
      hr(),
      
      fluidRow(
        column(12,
               h3("🔥 Location Conversion Rate Heatmap", style = "text-align: center; font-weight: bold;"),
               plotOutput("location_heatmap")
        )
      ),
      
      hr(),
      
      fluidRow(
        column(6,
               h3("🥇 Best & Worst Performing Locations", style = "text-align: center; font-weight: bold;"),
               tableOutput("top_bottom_locations")
        ),
        column(6,
               h3("🏗️ Express vs Full-Service Share", style = "text-align: center; font-weight: bold;"),
               plotOutput("service_type_share", height = "400px")
        )
      ),
      
      hr(),
      
      fluidRow(
        column(12,
               h3("🧠 Executive Summary", style = "text-align: center; font-weight: bold;"),
               uiOutput("exec_summary")
        )
      )
    )
  })
  
  
  
  
  # ------Sales drop off ------
  format_number <- function(x) {
      format(round(x, 0), big.mark = ",", scientific = FALSE)
    }
    
    format_percentage <- function(x) {
      format(round(x * 100, 2), nsmall = 2, big.mark = ",", scientific = FALSE)
    }
    
    weekly_sales_data <- reactive({
      req(input$date_range, input$selected_locations)
      
      start_date <- input$date_range[1]
      end_date <- input$date_range[2]
      total_days <- as.numeric(difftime(end_date, start_date, units = "days")) + 1
      num_weeks <- ceiling(total_days / 7)
      
      df <- filtered_data() %>%
        mutate(Week = as.numeric(difftime(`CREATED DATE`, start_date, units = "days")) %/% 7 + 1) %>%
        group_by(Week) %>%
        summarise(`Memberships Sold` = sum(SALES, na.rm = TRUE), .groups = "drop") %>%
        complete(Week = 1:num_weeks, fill = list(`Memberships Sold` = 0)) %>%
        mutate(Pct_Total = `Memberships Sold` / sum(`Memberships Sold`, na.rm = TRUE))
      
      return(df)
    })
    
    
    
        output$sales_dropoff <- renderUI({
      sales_data <- weekly_sales_data()
      total_sales <- sum(sales_data$`Memberships Sold`, na.rm = TRUE)
      
      sales_boxes <- lapply(1:nrow(sales_data), function(i) {
        fluidRow(
          column(6,
                 h4(paste0("📅 Week ", sales_data$Week[i], " Sales"), style = "text-align: center;"),
                 p(format_number(sales_data$`Memberships Sold`[i]), style = "text-align: center; font-size: 18px;")
          ),
          column(6,
                 h4(paste0("📊 % of Total Sales in Week ", sales_data$Week[i]), style = "text-align: center;"),
                 p(paste0(format_percentage(sales_data$Pct_Total[i]), "%"), 
                   style = "text-align: center; font-size: 18px; font-weight: bold; color: darkblue;")
          )
        )
      })
      
      tagList(
        h3("📉 Membership Sales Drop-Off", style = "text-align: center; font-weight: bold;"),
        
        fluidRow(
          column(12,
                 h3("🗓 Total Memberships Sold", style = "text-align: center; font-weight: bold;"),
                 p(format_number(total_sales), style = "text-align: center; font-size: 24px; font-weight: bold; color: black;")
          )
        ),
        
        hr(),
        
        tagList(sales_boxes),
        
        hr(),
        
        fluidRow(
          column(6,
                 h3("📈 Sales Trend Over Time", style = "text-align: center; font-weight: bold;"),
                 plotOutput("sales_dropoff_line_chart", height = "300px")
          ),
          column(6,
                 h3("📊 Sales by Week", style = "text-align: center; font-weight: bold;"),
                 plotOutput("sales_dropoff_bar_chart", height = "300px")
          )
        ),
        
        br(),
        
        fluidRow(
          column(12,
                 h4("📎 Filters are reactive on all pages except Marketing Impact. Try them out!",
                    style = "text-align: center; font-size: 18px; font-weight: bold; color: black;")
          )
        )
      )
    })
    
    
    output$sales_dropoff_line_chart <- renderPlot({
      df <- filtered_data() %>%
        group_by(`CREATED DATE`) %>%
        summarise(`Memberships Sold` = sum(SALES, na.rm = TRUE))
      
      ggplot(df, aes(x = `CREATED DATE`, y = `Memberships Sold`)) +
        geom_line(color = "blue", size = 1) +
        geom_point(color = "red", size = 2) +
        labs(title = "📈 Membership Sales Trend Over Time", x = "Date", y = "Memberships Sold") +
        theme_minimal()
    })
    
    output$sales_dropoff_bar_chart <- renderPlot({
      sales_by_week <- weekly_sales_data() %>% na.omit()
      
      ggplot(sales_by_week, aes(x = factor(Week), y = `Memberships Sold`, fill = factor(Week))) +
        geom_bar(stat = "identity", width = 0.6) +
        geom_text(aes(label = format_number(`Memberships Sold`)), vjust = -0.5, size = 5, fontface = "bold") +
        labs(title = "📊 Membership Sales by Week", x = "Week", y = "Memberships Sold") +
        scale_fill_manual(values = scales::hue_pal()(nrow(sales_by_week))) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16))
    })
    
    output$location_performance <- renderUI({
      df <- filtered_data() %>%
        group_by(LOCATION) %>%
        summarise(
          `Memberships Sold` = sum(SALES),
          `Eligible Washes` = sum(ELIGIBLE_WASHES),
          `Conversion Rate` = ifelse(sum(ELIGIBLE_WASHES) == 0, 0, sum(SALES) / sum(ELIGIBLE_WASHES)),
          .groups = "drop"
        )
      
      highest_conversion <- df %>% filter(`Conversion Rate` == max(`Conversion Rate`, na.rm = TRUE))
      lowest_conversion <- df %>% filter(`Conversion Rate` == min(`Conversion Rate`, na.rm = TRUE))
      
      format_percentage <- function(x) paste0(round(x * 100, 1), "%")
      
      tagList(
        h3("📍 Location Performance & Conversion Rate Breakdown", style = "text-align: center; font-weight: bold;"),
        
        # KPI boxes
        fluidRow(
          column(6,
                 h4("🏆 Highest Conversion Rate", style = "text-align: center;"),
                 p(paste0(highest_conversion$LOCATION, ": ", format_percentage(highest_conversion$`Conversion Rate`)),
                   style = "text-align: center; font-size: 18px; color: green; font-weight: bold;")
          ),
          column(6,
                 h4("⚠ Lowest Conversion Rate", style = "text-align: center;"),
                 p(paste0(lowest_conversion$LOCATION, ": ", format_percentage(lowest_conversion$`Conversion Rate`)),
                   style = "text-align: center; font-size: 18px; color: red; font-weight: bold;")
          )
        ),
        
        hr(),
        
        fluidRow(
          column(12, plotOutput("membership_vs_eligible_bar_chart", height = "450px"))
        ),
        fluidRow(
          column(12, plotOutput("conversion_rate_line_chart", height = "450px"))
        )
      )
    })
    output$membership_vs_eligible_bar_chart <- renderPlot({
      df <- filtered_data() %>%
        group_by(LOCATION) %>%
        summarise(
          `Memberships Sold` = sum(SALES),
          `Eligible Washes` = sum(ELIGIBLE_WASHES),
          .groups = "drop"
        ) %>%
        pivot_longer(cols = c(`Memberships Sold`, `Eligible Washes`), names_to = "Metric", values_to = "Count")
      
      ggplot(df, aes(x = LOCATION, y = Count, fill = Metric)) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
        geom_text(aes(label = scales::comma(Count)), position = position_dodge(width = 0.7), vjust = -0.5, size = 5, fontface = "bold") +
        labs(title = "📊 Memberships Sold vs. Eligible Washes by Location", x = "Location", y = "Count") +
        scale_fill_manual(values = c("Memberships Sold" = "steelblue", "Eligible Washes" = "darkgray")) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$conversion_rate_line_chart <- renderPlot({
      df <- filtered_data() %>%
        group_by(LOCATION) %>%
        summarise(
          `Conversion Rate` = ifelse(sum(ELIGIBLE_WASHES) == 0, 0, sum(SALES) / sum(ELIGIBLE_WASHES)),
          .groups = "drop"
        )
      
      format_percentage <- function(x) paste0(round(x * 100, 1), "%")
      
      ggplot(df, aes(x = LOCATION, y = `Conversion Rate`, group = 1)) +
        geom_line(color = "blue", size = 1) +
        geom_point(color = "red", size = 3) +
        geom_text(aes(label = format_percentage(`Conversion Rate`)), vjust = -0.5, size = 5, fontface = "bold") +
        labs(title = "📈 Conversion Rate by Location", x = "Location", y = "Conversion Rate") +
        scale_y_continuous(labels = scales::percent_format()) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    
    
    
    
    
    
    
    
    
    
  # ---- 📊 Revenue & Membership Insights Tab ----

    
    format_currency <- function(x) {
      paste0("$", formatC(round(x, 2), format = "f", big.mark = ",", digits = 2))
    }
    
    full_service_locations <- c("Piscataway", "Westfield", "Summit", "Woodbridge")
    express_locations <- c("Marlboro", "Roselle", "Monmouth Junction", "East Brunswick", 
                           "Oakhurst", "Hazlet", "Tices Lane", "Scotch Plains", "Roselle Park")
    
    # 📊 KPI Calculations
    revenue_ltv_calculations <- reactive({
      selected_locations <- input$selected_locations
      
      sales_data <- filtered_data() %>%
        group_by(LOCATION) %>%
        summarise(total_memberships_sold = sum(SALES, na.rm = TRUE), .groups = "drop")
      
      revenue_data <- LTV_by_Site %>%
        rename_with(~ gsub("^X\\.", "", .x)) %>%
        fix_location_names() %>%
        filter(LOCATION %in% selected_locations) %>%
        left_join(sales_data, by = "LOCATION") %>%
        mutate(
          total_memberships_sold = replace_na(total_memberships_sold, 0),
          Estimated_Revenue = total_memberships_sold * `Active Member LTV`
        )
      
      list(
        total_revenue = sum(revenue_data$Estimated_Revenue, na.rm = TRUE),
        highest_site_name = revenue_data %>% slice_max(Estimated_Revenue, n = 1) %>% pull(LOCATION),
        highest_site_value = revenue_data %>% slice_max(Estimated_Revenue, n = 1) %>% pull(Estimated_Revenue),
        lowest_site_name  = revenue_data %>% slice_min(Estimated_Revenue, n = 1) %>% pull(LOCATION),
        lowest_site_value = revenue_data %>% slice_min(Estimated_Revenue, n = 1) %>% pull(Estimated_Revenue),
        full_service_revenue = revenue_data %>%
          filter(LOCATION %in% full_service_locations) %>%
          summarise(rev = sum(Estimated_Revenue, na.rm = TRUE)) %>% pull(rev),
        express_revenue = revenue_data %>%
          filter(LOCATION %in% express_locations) %>%
          summarise(rev = sum(Estimated_Revenue, na.rm = TRUE)) %>% pull(rev)
      )
    })
    
    # 📊 Location-Level Revenue Table
    location_revenue_table <- reactive({
      sales_data <- filtered_data() %>%
        group_by(LOCATION) %>%
      summarise(Memberships_Sold = sum(SALES, na.rm = TRUE), .groups = "drop")
      
      revenue_data <- LTV_by_Site %>%
        rename_with(~ gsub("^X\\.", "", .x)) %>%
        fix_location_names() %>%
        filter(LOCATION %in% input$selected_locations) %>%
        left_join(sales_data, by = "LOCATION") %>%
        mutate(
          Estimated_Revenue = round(Memberships_Sold * `Active Member LTV`, 2),
          Memberships_Sold = round(Memberships_Sold, 0),
          `Average Membership Term` = round(`Average Membership Term`, 2),
          `Average Price of Membership` = round(`Average Price of Membership`, 2),
          `Active Member LTV` = round(`Active Member LTV`, 2)
        ) %>%
        select(LOCATION, `Average Membership Term`, `Average Price of Membership`,
               `Active Member LTV`, Memberships_Sold, Estimated_Revenue)
      
      revenue_data[is.na(revenue_data)] <- 0
      
      totals <- revenue_data %>%
        summarise(
          LOCATION = "TOTAL",
          `Average Membership Term` = round(weighted.mean(`Average Membership Term`, w = Memberships_Sold, na.rm = TRUE), 2),
          `Average Price of Membership` = round(weighted.mean(`Average Price of Membership`, w = Memberships_Sold, na.rm = TRUE), 2),
          `Active Member LTV` = round(weighted.mean(`Active Member LTV`, w = Memberships_Sold, na.rm = TRUE), 2),
          Memberships_Sold = sum(Memberships_Sold),
          Estimated_Revenue = sum(Estimated_Revenue)
        )
      
      bind_rows(revenue_data, totals) %>%
        mutate(
          Memberships_Sold = formatC(Memberships_Sold, format = "f", big.mark = ",", digits = 0),
          Estimated_Revenue = paste0("$", formatC(Estimated_Revenue, format = "f", big.mark = ",", digits = 2)),
          `Average Price of Membership` = paste0("$", formatC(`Average Price of Membership`, format = "f", big.mark = ",", digits = 2)),
          `Active Member LTV` = paste0("$", formatC(`Active Member LTV`, format = "f", big.mark = ",", digits = 2))
        )
    })
    
    
    # 📥 Revenue Table & Download
    output$location_revenue_table <- renderDT({
      datatable(
        location_revenue_table(),
        rownames = FALSE,
        options = list(dom = 't', pageLength = nrow(location_revenue_table()), autoWidth = TRUE),
        colnames = c("Location", "Avg Membership Term", "Avg Price of Membership", "Active Member LTV", "Memberships Sold", "Estimated Revenue")
      )
    })
    
    output$download_revenue_table <- downloadHandler(
      filename = function() paste("Revenue_Breakdown_", Sys.Date(), ".csv", sep = ""),
      content = function(file) {
        csv_data <- location_revenue_table() %>%
          mutate(
            Estimated_Revenue = gsub("[$,]", "", Estimated_Revenue),
            Avg.Price.of.Membership = gsub("[$,]", "", Avg.Price.of.Membership),
            Active.Member.LTV = gsub("[$,]", "", Active.Member.LTV),
            Memberships_Sold = gsub(",", "", Memberships_Sold)
          )
        write.csv(csv_data, file, row.names = FALSE)
      }
    )
    
    # 📊 Charts
    output$revenue_bar_chart <- renderPlot({
      data <- location_revenue_table() %>%
        filter(LOCATION != "TOTAL") %>%
        mutate(Estimated_Revenue = as.numeric(gsub("[$,]", "", Estimated_Revenue)))
      
      ggplot(data, aes(x = reorder(LOCATION, Estimated_Revenue), y = Estimated_Revenue, fill = LOCATION)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        geom_text(aes(label = paste0("$", formatC(Estimated_Revenue, format = "f", big.mark = ",", digits = 2))),
                  vjust = -0.5, size = 5, fontface = "bold") +
        labs(title = "📊 Estimated Revenue by Location", x = "Location", y = "Estimated Revenue") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(face = "bold", hjust = 0.5)) +
        scale_y_continuous(labels = scales::comma)
    })
    
    output$revenue_heatmap <- renderPlot({
      heatmap_data <- location_revenue_table() %>%
        filter(LOCATION != "TOTAL") %>%
        mutate(Estimated_Revenue = as.numeric(gsub("[$,]", "", Estimated_Revenue)))
      
      ggplot(heatmap_data, aes(x = LOCATION, y = 1, fill = Estimated_Revenue)) +
        geom_tile(color = "white") +
        scale_fill_gradient(low = "white", high = "red", labels = scales::comma) +
        theme_minimal() +
        theme(
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
          legend.title = element_text(face = "bold"),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
        ) +
        ggtitle("🔥 Revenue Heatmap by Location") +
        labs(fill = "Estimated Revenue ($)")
    })
    
    output$conversion_heatmap <- renderPlot({
      conv_data <- filtered_data() %>%
        group_by(LOCATION) %>%
      summarise(
                Memberships_Sold = sum(SALES),
                Eligible_Washes = sum(ELIGIBLE_WASHES),
          Conversion_Rate = ifelse(sum(ELIGIBLE_WASHES) == 0, 0, sum(SALES) / sum(ELIGIBLE_WASHES))
        ) %>%
        filter(LOCATION != "TOTAL") %>%
        mutate(Conversion_Rate = round(Conversion_Rate * 100, 2))
      
      ggplot(conv_data, aes(x = LOCATION, y = 1, fill = Conversion_Rate)) +
        geom_tile(color = "white") +
        scale_fill_gradient(low = "white", high = "red", labels = scales::comma) +
        theme_minimal() +
        theme(
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
          legend.title = element_text(face = "bold"),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
        ) +
        ggtitle("🔥 Conversion Rate Heatmap by Location") +
        labs(fill = "Conversion Rate (%)")
    })
    
    # 📌 Full UI Rendering
    output$Revenue_LTV_tab <- renderUI({
      tagList(
        h2("💰 Revenue & Membership Insights", style = "text-align: center; font-weight: bold; margin-bottom: 20px;"),
        
        div(
          h3("🔹 Total Revenue (All Time)", style = "text-align: center;"),
          p(format_currency(revenue_ltv_calculations()$total_revenue),
            style = "text-align: center; font-size: 30px; font-weight: bold; margin-bottom: 30px;")
        ),
        
        fluidRow(
          column(6, div(
            h4("🏆 Highest Earning Location", style = "text-align: center; color: green; font-weight: bold;"),
            p(paste0(revenue_ltv_calculations()$highest_site_name, ": ",
                     format_currency(revenue_ltv_calculations()$highest_site_value)),
              style = "text-align: center; font-size: 22px; font-weight: bold; color: green;")
          )),
          column(6, div(
            h4("⚠️ Lowest Earning Location", style = "text-align: center; color: red; font-weight: bold;"),
            p(paste0(revenue_ltv_calculations()$lowest_site_name, ": ",
                     format_currency(revenue_ltv_calculations()$lowest_site_value)),
              style = "text-align: center; font-size: 22px; font-weight: bold; color: red;")
          ))
        ),
        
        fluidRow(
          column(6, div(
            h4("🚗 Full Service Revenue", style = "text-align: center; color: blue; font-weight: bold;"),
            p(format_currency(revenue_ltv_calculations()$full_service_revenue),
              style = "text-align: center; font-size: 25px; font-weight: bold; color: blue;")
          )),
          column(6, div(
            h4("⛽ Express Revenue", style = "text-align: center; color: orange; font-weight: bold;"),
            p(format_currency(revenue_ltv_calculations()$express_revenue),
              style = "text-align: center; font-size: 25px; font-weight: bold; color: orange;")
          ))
        ),
        
        fluidRow(column(12, h3("📊 Location-Based Revenue Breakdown", style = "text-align: center; font-weight: bold; margin-top: 40px;"))),
        fluidRow(
          column(12,
                 downloadButton("download_revenue_table", " Download CSV", class = "btn-primary"),
                 br(), br(),
                 DTOutput("location_revenue_table"))
        ),
        
        fluidRow(column(12, h3("📊 Estimated Revenue by Location", style = "text-align: center; font-weight: bold;"))),
        fluidRow(column(12, plotOutput("revenue_bar_chart", height = "500px"))),
        
        fluidRow(column(12, h3("🔥 Revenue & Conversion Rate Heatmaps", style = "text-align: center; font-weight: bold;"))),
        fluidRow(
          column(6, plotOutput("revenue_heatmap", height = "400px")),
          column(6, plotOutput("conversion_heatmap", height = "400px"))
        )
      )
    })
    
}


