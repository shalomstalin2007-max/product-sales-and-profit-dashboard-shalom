library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(scales)
library(lubridate)

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Sales & Profit Insights", titleWidth = 300),
  dashboardSidebar(
    width = 300,
    fileInput("file", "Upload Superstore CSV", accept = ".csv"),
    tags$hr(),
    uiOutput("date_filter"),
    uiOutput("region_filter"),
    uiOutput("category_filter"),
    uiOutput("segment_filter"),
    uiOutput("ship_filter"),
    actionButton("reset", "Reset filters", icon = icon("refresh"), class = "btn-warning")
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper, .right-side {background-color:#f3f6fb;}
      .skin-blue .main-header .logo {font-weight:700; letter-spacing:.4px;}
      .small-box {border-radius:12px; box-shadow:0 5px 18px rgba(30,55,90,.10);}
      .box {border-radius:12px; border-top:3px solid #2c7be5; box-shadow:0 5px 18px rgba(30,55,90,.08);}
      .info-box {border-radius:12px;}
      .dashboard-title {font-size:20px;font-weight:700;color:#17324d;margin:5px 0 14px;}
      .help-note {color:#66788a;font-size:12px;}
    "))),
    fluidRow(
      box(width = 12, status = "primary", solidHeader = FALSE,
          div(class = "dashboard-title", "Product Sales and Profit Dashboard"),
          p("Explore revenue, profitability, order trends and product performance. Use the sidebar to filter the full dashboard.",
            class = "help-note"))
    ),
    fluidRow(
      valueBoxOutput("sales_box", width = 3),
      valueBoxOutput("profit_box", width = 3),
      valueBoxOutput("orders_box", width = 3),
      valueBoxOutput("margin_box", width = 3)
    ),
    fluidRow(
      box(title = "Sales trend", width = 8, status = "primary", plotlyOutput("trend", height = 300)),
      box(title = "Sales by category", width = 4, status = "primary", plotlyOutput("category_plot", height = 300))
    ),
    fluidRow(
      box(title = "Profit by sub-category", width = 6, status = "primary", plotlyOutput("subcat_plot", height = 340)),
      box(title = "Regional performance", width = 6, status = "primary", plotlyOutput("region_plot", height = 340))
    ),
    fluidRow(
      box(title = "Discount vs profit", width = 6, status = "primary", plotlyOutput("discount_plot", height = 320)),
      box(title = "Top 10 products by sales", width = 6, status = "primary", plotlyOutput("product_plot", height = 320))
    ),
    fluidRow(
      box(title = "Detailed order data", width = 12, status = "primary",
          downloadButton("download_csv", "Download filtered CSV", class = "btn-success"),
          downloadButton("download_summary", "Download summary", class = "btn-info"),
          br(), br(), DTOutput("data_table"))
    )
  )
)

server <- function(input, output, session) {
  default_path <- "Sample - Superstore.csv"

  raw_data <- reactive({
    if (!is.null(input$file)) {
      read.csv(input$file$datapath, stringsAsFactors = FALSE, check.names = FALSE)
    } else if (file.exists(default_path)) {
      read.csv(default_path, stringsAsFactors = FALSE, check.names = FALSE)
    } else {
      NULL
    }
  })

  normalized <- reactive({
    d <- raw_data()
    validate(need(!is.null(d), "Upload the Sample Superstore CSV file to begin."))
    names(d) <- make.names(names(d), unique = TRUE)

    required <- c("Sales", "Profit")
    validate(need(all(required %in% names(d)), "CSV must contain Sales and Profit columns."))

    date_col <- intersect(c("Order.Date", "OrderDate"), names(d))[1]
    if (length(date_col) == 0 || is.na(date_col)) d$Order.Date <- as.Date(NA) else {
      d$Order.Date <- as.Date(parse_date_time(d[[date_col]], orders = c("mdy", "ymd", "dmy")))
    }
    if (!"Order.ID" %in% names(d)) d$Order.ID <- seq_len(nrow(d))
    if (!"Product.Name" %in% names(d)) d$Product.Name <- "Unspecified"
    if (!"Category" %in% names(d)) d$Category <- "Unspecified"
    if (!"Sub.Category" %in% names(d)) d$Sub.Category <- "Unspecified"
    if (!"Region" %in% names(d)) d$Region <- "Unspecified"
    if (!"Segment" %in% names(d)) d$Segment <- "Unspecified"
    if (!"Ship.Mode" %in% names(d)) d$Ship.Mode <- "Unspecified"
    if (!"Discount" %in% names(d)) d$Discount <- 0
    d %>% mutate(
      Sales = as.numeric(Sales), Profit = as.numeric(Profit),
      Discount = as.numeric(Discount),
      Category = as.character(Category), Region = as.character(Region),
      Segment = as.character(Segment), Ship.Mode = as.character(Ship.Mode),
      Product.Name = as.character(Product.Name), Sub.Category = as.character(Sub.Category),
      Order.ID = as.character(Order.ID)
    ) %>% filter(!is.na(Sales), !is.na(Profit))
  })

  observeEvent(input$reset, {
    d <- normalized()
    updateSelectInput(session, "region", selected = unique(d$Region))
    updateSelectInput(session, "category", selected = unique(d$Category))
    updateSelectInput(session, "segment", selected = unique(d$Segment))
    updateSelectInput(session, "ship", selected = unique(d$Ship.Mode))
    if (any(!is.na(d$Order.Date))) {
      updateDateRangeInput(session, "dates", start = min(d$Order.Date, na.rm = TRUE),
                           end = max(d$Order.Date, na.rm = TRUE))
    }
  })

  output$date_filter <- renderUI({
    d <- normalized()
    if (any(!is.na(d$Order.Date))) {
      dateRangeInput("dates", "Order date range",
                     start = min(d$Order.Date, na.rm = TRUE),
                     end = max(d$Order.Date, na.rm = TRUE),
                     min = min(d$Order.Date, na.rm = TRUE),
                     max = max(d$Order.Date, na.rm = TRUE))
    } else helpText("No usable order date column found.")
  })
  output$region_filter <- renderUI({
    selectInput("region", "Region", choices = unique(normalized()$Region),
                selected = unique(normalized()$Region), multiple = TRUE)
  })
  output$category_filter <- renderUI({
    selectInput("category", "Category", choices = unique(normalized()$Category),
                selected = unique(normalized()$Category), multiple = TRUE)
  })
  output$segment_filter <- renderUI({
    selectInput("segment", "Customer segment", choices = unique(normalized()$Segment),
                selected = unique(normalized()$Segment), multiple = TRUE)
  })
  output$ship_filter <- renderUI({
    selectInput("ship", "Ship mode", choices = unique(normalized()$Ship.Mode),
                selected = unique(normalized()$Ship.Mode), multiple = TRUE)
  })

  filtered <- reactive({
    d <- normalized()
    if (!is.null(input$dates) && length(input$dates) == 2 && any(!is.na(d$Order.Date)))
      d <- d %>% filter(Order.Date >= input$dates[1], Order.Date <= input$dates[2])
    if (!is.null(input$region)) d <- d %>% filter(Region %in% input$region)
    if (!is.null(input$category)) d <- d %>% filter(Category %in% input$category)
    if (!is.null(input$segment)) d <- d %>% filter(Segment %in% input$segment)
    if (!is.null(input$ship)) d <- d %>% filter(Ship.Mode %in% input$ship)
    d
  })

  output$sales_box <- renderValueBox({
    valueBox(dollar(sum(filtered()$Sales, na.rm = TRUE)), "Total sales",
             icon = icon("dollar-sign"), color = "aqua")
  })
  output$profit_box <- renderValueBox({
    valueBox(dollar(sum(filtered()$Profit, na.rm = TRUE)), "Total profit",
             icon = icon("chart-line"), color = "green")
  })
  output$orders_box <- renderValueBox({
    valueBox(comma(n_distinct(filtered()$Order.ID)), "Orders",
             icon = icon("shopping-cart"), color = "purple")
  })
  output$margin_box <- renderValueBox({
    d <- filtered(); s <- sum(d$Sales, na.rm = TRUE)
    valueBox(ifelse(s == 0, "0%", percent(sum(d$Profit, na.rm = TRUE) / s, accuracy = .1)),
             "Profit margin", icon = icon("percentage"), color = "yellow")
  })

  output$trend <- renderPlotly({
    d <- filtered() %>% filter(!is.na(Order.Date)) %>%
      mutate(Month = floor_date(Order.Date, "month")) %>%
      group_by(Month) %>% summarise(Sales = sum(Sales), Profit = sum(Profit), .groups = "drop")
    validate(need(nrow(d) > 0, "No dated records in this selection."))
    p <- ggplot(d, aes(Month, Sales, text = paste0("Month: ", format(Month, "%b %Y"),
       "<br>Sales: ", dollar(Sales), "<br>Profit: ", dollar(Profit)))) +
      geom_line(color = "#2c7be5", linewidth = 1.2) + geom_area(fill = "#2c7be5", alpha = .12) +
      geom_point(color = "#174ea6", size = 2) + scale_y_continuous(labels = dollar) +
      labs(x = NULL, y = "Sales") + theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })
  output$category_plot <- renderPlotly({
    d <- filtered() %>% group_by(Category) %>%
      summarise(Sales = sum(Sales), Profit = sum(Profit), .groups = "drop")
    p <- ggplot(d, aes(reorder(Category, Sales), Sales, fill = Category,
       text = paste0(Category, "<br>Sales: ", dollar(Sales), "<br>Profit: ", dollar(Profit)))) +
      geom_col(show.legend = FALSE) + coord_flip() + scale_y_continuous(labels = dollar) +
      labs(x = NULL, y = "Sales") + theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })
  output$subcat_plot <- renderPlotly({
    d <- filtered() %>% group_by(Sub.Category) %>%
      summarise(Profit = sum(Profit), Sales = sum(Sales), .groups = "drop")
    p <- ggplot(d, aes(reorder(Sub.Category, Profit), Profit, fill = Profit >= 0,
       text = paste0(Sub.Category, "<br>Profit: ", dollar(Profit), "<br>Sales: ", dollar(Sales)))) +
      geom_col(show.legend = FALSE) + coord_flip() + scale_fill_manual(values = c("#e76f51", "#2a9d8f")) +
      scale_y_continuous(labels = dollar) + labs(x = NULL, y = "Profit") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })
  output$region_plot <- renderPlotly({
    d <- filtered() %>% group_by(Region) %>%
      summarise(Sales = sum(Sales), Profit = sum(Profit), .groups = "drop")
    p <- ggplot(d, aes(Region, Sales, fill = Region,
       text = paste0(Region, "<br>Sales: ", dollar(Sales), "<br>Profit: ", dollar(Profit)))) +
      geom_col(show.legend = FALSE) + scale_y_continuous(labels = dollar) +
      labs(x = NULL, y = "Sales") + theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })
  output$discount_plot <- renderPlotly({
    d <- filtered()
    p <- ggplot(d, aes(Discount, Profit, color = Category,
       text = paste0("Discount: ", percent(Discount), "<br>Profit: ", dollar(Profit),
                     "<br>Category: ", Category))) +
      geom_point(alpha = .55, size = 2) + geom_smooth(method = "loess", se = FALSE, color = "#253858") +
      scale_y_continuous(labels = dollar) + labs(x = "Discount", y = "Profit") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })
  output$product_plot <- renderPlotly({
    d <- filtered() %>% group_by(Product.Name) %>%
      summarise(Sales = sum(Sales), Profit = sum(Profit), .groups = "drop") %>%
      slice_max(Sales, n = 10, with_ties = FALSE)
    p <- ggplot(d, aes(reorder(Product.Name, Sales), Sales,
       text = paste0(Product.Name, "<br>Sales: ", dollar(Sales), "<br>Profit: ", dollar(Profit)))) +
      geom_col(fill = "#4c6fff") + coord_flip() + scale_y_continuous(labels = dollar) +
      labs(x = NULL, y = "Sales") + theme_minimal(base_size = 11)
    ggplotly(p, tooltip = "text")
  })
  output$data_table <- renderDT({
    datatable(filtered(), filter = "top", extensions = "Buttons",
      options = list(pageLength = 10, scrollX = TRUE, dom = "Bfrtip",
                     buttons = c("copy", "csv", "excel")),
      rownames = FALSE)
  })
  output$download_csv <- downloadHandler(
    filename = function() paste0("filtered_sales_data_", Sys.Date(), ".csv"),
    content = function(file) write.csv(filtered(), file, row.names = FALSE)
  )
  output$download_summary <- downloadHandler(
    filename = function() paste0("sales_profit_summary_", Sys.Date(), ".csv"),
    content = function(file) {
      d <- filtered()
      summary <- tibble(
        Metric = c("Total Sales", "Total Profit", "Profit Margin", "Orders", "Records"),
        Value = c(sum(d$Sales, na.rm = TRUE), sum(d$Profit, na.rm = TRUE),
                  ifelse(sum(d$Sales, na.rm = TRUE) == 0, 0,
                         sum(d$Profit, na.rm = TRUE) / sum(d$Sales, na.rm = TRUE)),
                  n_distinct(d$Order.ID), nrow(d))
      )
      write.csv(summary, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
