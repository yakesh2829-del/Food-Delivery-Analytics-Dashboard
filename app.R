library(shiny)
library(tidyverse)
library(lubridate)
library(DT)
library(scales)

orders <- read.csv(file.path("data", "food_delivery.csv"), stringsAsFactors = FALSE)
orders$Order_Date <- as.Date(orders$Order_Date)

fmt_inr <- function(x) paste0("₹", comma(round(x, 0)))

ui <- fluidPage(
  tags$head(
    tags$title("Foodly | Food Delivery Analytics"),
    tags$link(rel="preconnect", href="https://fonts.googleapis.com"),
    tags$link(rel="preconnect", href="https://fonts.gstatic.com", crossorigin="anonymous"),
    tags$link(href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&family=Plus+Jakarta+Sans:wght@600;700;800&display=swap", rel="stylesheet"),
    includeCSS("www/styles.css"),
    tags$script(src="nav.js")
  ),
  div(class="app-shell",
    div(class="topbar",
      div(class="brand", span(class="brand-mark", "🍔"), span("FOODLY")),
      div(class="topnav",
        actionLink("nav_dashboard", "Dashboard", class="nav-link active"),
        actionLink("nav_sales", "Analytics", class="nav-link"),
        actionLink("nav_restaurants", "Restaurants", class="nav-link"),
        actionLink("nav_orders", "Orders", class="nav-link"),
        actionLink("nav_about", "About", class="nav-link")
      ),
      div(class="nav-badge", "LIVE ANALYTICS")
    ),
    div(class="hero",
      div(class="hero-copy",
        div(class="eyebrow", "FOOD DELIVERY ANALYTICS"),
        h1("Your delivery business, at a glance."),
        p("Track orders, revenue, restaurants, ratings and delivery performance in one clean workspace.")
      ),
      div(class="hero-emoji", "🍕")
    ),
    div(class="filter-card",
      div(class="filter-title", "FILTER YOUR DATA", span(class="filter-dot")),
      div(class="filter-grid",
        dateRangeInput("dates", "Order date", start=min(orders$Order_Date), end=max(orders$Order_Date), width="100%"),
        selectInput("city", "City", c("All", sort(unique(orders$City))), width="100%"),
        selectInput("cuisine", "Cuisine", c("All", sort(unique(orders$Cuisine))), width="100%"),
        selectInput("status", "Order status", c("All", sort(unique(orders$Order_Status))), width="100%"),
        actionButton("reset", "Reset filters", class="reset-btn")
      )
    ),
    div(id="page_dashboard", class="page-section active-page",
      div(class="section-heading", div(h2("Good evening 👋"), p("Here's your food delivery performance.")), div(class="period-chip", textOutput("period_text", inline=TRUE))),
      div(class="kpi-grid",
        div(class="kpi-card orange", div(class="kpi-icon", "🛍️"), div(class="kpi-label", "ORDERS"), div(class="kpi-value", textOutput("orders_kpi")), div(class="kpi-note", "Filtered orders")),
        div(class="kpi-card red", div(class="kpi-icon", "₹"), div(class="kpi-label", "REVENUE"), div(class="kpi-value", textOutput("revenue_kpi")), div(class="kpi-note", "Gross revenue")),
        div(class="kpi-card purple", div(class="kpi-icon", "↗"), div(class="kpi-label", "AVG ORDER VALUE"), div(class="kpi-value", textOutput("aov_kpi")), div(class="kpi-note", "Per order")),
        div(class="kpi-card gold", div(class="kpi-icon", "★"), div(class="kpi-label", "AVG RATING"), div(class="kpi-value", textOutput("rating_kpi")), div(class="kpi-note", "Customer rating"))
      ),
      div(class="chart-grid wide-first",
        div(class="panel chart-panel wide", div(class="panel-head", h3("Monthly performance"), span("Orders & revenue")), plotOutput("monthly", height="330px")),
        div(class="panel chart-panel", div(class="panel-head", h3("Cuisine mix"), span("Revenue share")), plotOutput("cuisine", height="330px"))
      ),
      div(class="chart-grid",
        div(class="panel chart-panel", div(class="panel-head", h3("City performance"), span("Revenue")), plotOutput("city_plot", height="300px")),
        div(class="panel chart-panel", div(class="panel-head", h3("Order status"), span("Share of orders")), plotOutput("status_plot", height="300px"))
      )
    ),
    div(id="page_sales", class="page-section",
      div(class="section-heading", div(h2("Analytics"), p("Understand revenue, profit and customer experience."))),
      div(class="chart-grid wide-first", div(class="panel chart-panel wide", div(class="panel-head", h3("Revenue vs profit"), span("Monthly trend")), plotOutput("rev_profit", height="390px")), div(class="panel chart-panel", div(class="panel-head", h3("Payment method"), span("Order count")), plotOutput("payment", height="390px"))),
      div(class="panel chart-panel", div(class="panel-head", h3("Delivery time vs rating"), span("Customer experience")), plotOutput("delivery_rating", height="360px"))
    ),
    div(id="page_restaurants", class="page-section",
      div(class="section-heading", div(h2("Restaurant performance"), p("Find the restaurants driving your business."))),
      div(class="chart-grid wide-first", div(class="panel chart-panel wide", div(class="panel-head", h3("Top restaurants"), span("Revenue leaders")), plotOutput("top_rest", height="430px")), div(class="panel", div(class="panel-head", h3("Quick insight"), span("Filtered view")), uiOutput("restaurant_insight"))),
      div(class="panel table-panel", div(class="panel-head", h3("Restaurant scorecard"), span("Revenue · profit · rating · delivery")), DTOutput("rest_table"))
    ),
    div(id="page_orders", class="page-section",
      div(class="section-heading", div(h2("Orders"), p("Browse and export the filtered order dataset.")), downloadButton("download", "Download CSV", class="download-btn")),
      div(class="panel table-panel", div(class="panel-head", h3("Filtered orders"), span("Live table")), DTOutput("orders_table"))
    ),
    div(id="page_about", class="page-section",
      div(class="section-heading", div(h2("About Foodly"), p("A practical R/Shiny analytics project for food-delivery data."))),
      div(class="about-grid", div(class="about-card", span("01"), h3("What it does"), p("Combines operational order data with interactive analytics for revenue, restaurants, delivery performance, ratings and payment behavior.")), div(class="about-card", span("02"), h3("Technology"), p("Built with R, Shiny, tidyverse, lubridate, ggplot2, DT and scales.")), div(class="about-card", span("03"), h3("Project ready"), p("The project includes sample data, reusable filters, charts, tables, CSV export and Windows-friendly run instructions.")))
    )
  )
)

server <- function(input, output, session) {
  filtered <- reactive({
    d <- orders
    if (!is.null(input$dates) && length(input$dates) == 2) d <- d %>% filter(Order_Date >= input$dates[1], Order_Date <= input$dates[2])
    if (!is.null(input$city) && input$city != "All") d <- d %>% filter(City == input$city)
    if (!is.null(input$cuisine) && input$cuisine != "All") d <- d %>% filter(Cuisine == input$cuisine)
    if (!is.null(input$status) && input$status != "All") d <- d %>% filter(Order_Status == input$status)
    d
  })

  observeEvent(input$reset, {
    updateDateRangeInput(session, "dates", start=min(orders$Order_Date), end=max(orders$Order_Date))
    updateSelectInput(session, "city", selected="All")
    updateSelectInput(session, "cuisine", selected="All")
    updateSelectInput(session, "status", selected="All")
  })

  nav <- function(id) observeEvent(input[[paste0("nav_", id)]], { session$sendCustomMessage("showPage", id) }, ignoreInit=TRUE)
  nav("dashboard"); nav("sales"); nav("restaurants"); nav("orders"); nav("about")

  output$period_text <- renderText({
    d <- filtered(); if (!nrow(d)) return("No matching dates")
    paste(format(min(d$Order_Date), "%d %b %Y"), "–", format(max(d$Order_Date), "%d %b %Y"))
  })
  output$orders_kpi <- renderText(comma(nrow(filtered())))
  output$revenue_kpi <- renderText(fmt_inr(sum(filtered()$Revenue, na.rm=TRUE)))
  output$aov_kpi <- renderText(fmt_inr(if(nrow(filtered()) == 0) 0 else mean(filtered()$Revenue, na.rm=TRUE)))
  output$rating_kpi <- renderText(if(nrow(filtered()) == 0) "0.00 / 5" else sprintf("%.2f / 5", mean(filtered()$Customer_Rating, na.rm=TRUE)))

  chart_theme <- theme_minimal(base_family="Inter", base_size=11) +
    theme(plot.background=element_rect(fill="transparent", color=NA), panel.background=element_rect(fill="transparent", color=NA),
          panel.grid.minor=element_blank(), panel.grid.major.x=element_line(color="#edf1f5"), panel.grid.major.y=element_line(color="#edf1f5"),
          axis.title=element_blank(), axis.text=element_text(color="#64748b"), legend.position="bottom", legend.title=element_blank(),
          plot.margin=margin(5, 8, 5, 8))

  output$monthly <- renderPlot({
    d <- filtered() %>% mutate(Month=floor_date(Order_Date,"month")) %>% group_by(Month) %>% summarise(Orders=n(), Revenue=sum(Revenue), .groups="drop")
    validate(need(nrow(d)>0, "No data for the selected filters."))
    scale_factor <- max(d$Revenue, na.rm=TRUE) / max(d$Orders, na.rm=TRUE)
    d <- d %>% mutate(Orders_scaled=Orders*scale_factor)
    ggplot(d, aes(Month)) + geom_line(aes(y=Revenue, color="Revenue"), linewidth=1.2) + geom_point(aes(y=Revenue, color="Revenue"), size=2) +
      geom_line(aes(y=Orders_scaled, color="Orders"), linewidth=1.2, linetype="dashed") + geom_point(aes(y=Orders_scaled, color="Orders"), size=1.8) +
      scale_color_manual(values=c("Revenue"="#ff6b35","Orders"="#ef4444")) + scale_y_continuous(labels=label_number(big.mark=","), sec.axis=sec_axis(~./scale_factor, name="Orders")) + chart_theme
  })

  output$cuisine <- renderPlot({
    d <- filtered() %>% group_by(Cuisine) %>% summarise(Revenue=sum(Revenue), .groups="drop")
    validate(need(nrow(d)>0, "No data for the selected filters."))
    ggplot(d, aes(x=2, y=Revenue, fill=Cuisine)) + geom_col(width=1, color="white") + coord_polar(theta="y") + xlim(.5,2.5) +
      guides(fill=guide_legend(nrow=2)) + chart_theme + theme(axis.text=element_blank(), axis.ticks=element_blank(), panel.grid=element_blank(), legend.position="bottom")
  })

  output$city_plot <- renderPlot({
    d <- filtered() %>% group_by(City) %>% summarise(Revenue=sum(Revenue), .groups="drop") %>% arrange(Revenue)
    validate(need(nrow(d)>0, "No data for the selected filters."))
    ggplot(d, aes(x=Revenue, y=reorder(City, Revenue))) + geom_col(fill="#ff6b35", width=.65) + scale_x_continuous(labels=label_number(big.mark=",")) + chart_theme
  })

  output$status_plot <- renderPlot({
    d <- filtered() %>% count(Order_Status, name="Orders")
    validate(need(nrow(d)>0, "No data for the selected filters."))
    ggplot(d, aes(x=2, y=Orders, fill=Order_Status)) + geom_col(width=1, color="white") + coord_polar(theta="y") + xlim(.5,2.5) +
      guides(fill=guide_legend(nrow=2)) + chart_theme + theme(axis.text=element_blank(), axis.ticks=element_blank(), panel.grid=element_blank(), legend.position="bottom")
  })

  output$rev_profit <- renderPlot({
    d <- filtered() %>% mutate(Month=floor_date(Order_Date,"month")) %>% group_by(Month) %>% summarise(Revenue=sum(Revenue), Profit=sum(Profit), .groups="drop")
    validate(need(nrow(d)>0, "No data for the selected filters."))
    ggplot(d, aes(Month)) + geom_col(aes(y=Revenue, fill="Revenue"), alpha=.82, width=25) + geom_line(aes(y=Profit, color="Profit"), linewidth=1.2) + geom_point(aes(y=Profit, color="Profit"), size=2) +
      scale_fill_manual(values=c("Revenue"="#ff6b35")) + scale_color_manual(values=c("Profit"="#9333ea")) + chart_theme
  })

  output$payment <- renderPlot({
    d <- filtered() %>% count(Payment_Method, name="Orders")
    validate(need(nrow(d)>0, "No data for the selected filters."))
    ggplot(d, aes(x=reorder(Payment_Method, Orders), y=Orders)) + geom_col(fill="#ef4444", width=.65) + coord_flip() + chart_theme
  })

  output$delivery_rating <- renderPlot({
    d <- filtered()
    validate(need(nrow(d)>0, "No data for the selected filters."))
    ggplot(d, aes(Delivery_Time_Min, Customer_Rating)) + geom_point(alpha=.55, size=2.2, color="#ff6b35") + geom_smooth(method="lm", se=FALSE, color="#172033", linewidth=.8) + scale_y_continuous(limits=c(1,5)) + chart_theme
  })

  output$top_rest <- renderPlot({
    d <- filtered() %>% group_by(Restaurant) %>% summarise(Revenue=sum(Revenue), .groups="drop") %>% arrange(desc(Revenue)) %>% slice_head(n=10) %>% arrange(Revenue)
    validate(need(nrow(d)>0, "No data for the selected filters."))
    ggplot(d, aes(Revenue, reorder(Restaurant, Revenue))) + geom_col(fill="#ff6b35", width=.65) + scale_x_continuous(labels=label_number(big.mark=",")) + chart_theme
  })

  output$restaurant_insight <- renderUI({
    d <- filtered()
    top <- d %>% group_by(Restaurant) %>% summarise(Revenue=sum(Revenue), Rating=mean(Customer_Rating), Delivery=mean(Delivery_Time_Min), .groups="drop") %>% arrange(desc(Revenue)) %>% slice_head(n=1)
    if(nrow(top)==0) return(p("No data for this filter."))
    div(class="insight-card", div(class="insight-icon","🏆"), h3(top$Restaurant), p(paste0(fmt_inr(top$Revenue), " revenue")), div(class="insight-row", span("⭐ Rating"), strong(sprintf("%.2f", top$Rating))), div(class="insight-row", span("🚴 Delivery"), strong(sprintf("%.1f min", top$Delivery))), div(class="insight-row", span("📈 Position"), strong("#1")))
  })

  output$rest_table <- renderDT({
    filtered() %>% group_by(Restaurant) %>% summarise(Orders=n(), Revenue=round(sum(Revenue),2), Profit=round(sum(Profit),2), Avg_Rating=round(mean(Customer_Rating),2), Avg_Delivery_Min=round(mean(Delivery_Time_Min),1), .groups="drop") %>% arrange(desc(Revenue)) %>%
      datatable(options=list(pageLength=8, dom='tip', scrollX=TRUE), rownames=FALSE)
  })

  output$orders_table <- renderDT({
    datatable(filtered() %>% select(Order_ID,Order_Date,City,Restaurant,Cuisine,Food_Item,Quantity,Revenue,Delivery_Time_Min,Customer_Rating,Payment_Method,Order_Status), options=list(pageLength=15, dom='tip', scrollX=TRUE), rownames=FALSE)
  })

  output$download <- downloadHandler(filename=function() paste0("food_delivery_filtered_",Sys.Date(),".csv"), content=function(file) write.csv(filtered(),file,row.names=FALSE))
}

shinyApp(ui, server)
