# ============================================================
# KPB Tour Financial Decision Support Model — V2
# Dynamic cities | Scenario manager | Dashboard | Plotly
# Break-even | Sensitivity | Add / Duplicate / Remove city
# ============================================================

library(shiny)
library(bslib)
library(plotly)

# -----------------------------
# Helpers
# -----------------------------
fmt <- function(x, digits = 0) {
  if (length(x) == 0) return("—")
  out <- rep("—", length(x))
  ok <- !is.na(x) & is.finite(x)
  out[ok] <- paste0("$", formatC(round(x[ok], digits), format = "f", digits = digits, big.mark = ","))
  if (length(out) == 1) out else out
}

pct <- function(x, digits = 1) {
  if (length(x) == 0) return("—")
  out <- rep("—", length(x))
  ok <- !is.na(x) & is.finite(x)
  out[ok] <- paste0(formatC(x[ok] * 100, format = "f", digits = digits), "%")
  out
}

num <- function(x, digits = 0) {
  if (length(x) == 0) return("—")
  out <- rep("—", length(x))
  ok <- !is.na(x) & is.finite(x)
  out[ok] <- formatC(round(x[ok], digits), format = "f", digits = digits, big.mark = ",")
  out
}

new_city <- function(name = "New City", capacity = 2000, fill = 0.80,
                     ga_pct = 0.85, ga_price = 70, vip_price = 150,
                     fee = 0.08, promoter = 0.20,
                     venue_hire = 30000, addl_prod = 2500,
                     mkt_social = 2000, mkt_design = 600,
                     photo = 1000, promoter_flat = 0,
                     merch_cost = 0) {
  data.frame(
    id = paste0("city_", as.integer(runif(1, 1, 99999999))),
    city = as.character(name),
    capacity = as.numeric(capacity),
    fill = as.numeric(fill),
    ga_pct = as.numeric(ga_pct),
    ga_price = as.numeric(ga_price),
    vip_price = as.numeric(vip_price),
    fee = as.numeric(fee),
    promoter = as.numeric(promoter),
    venue_hire = as.numeric(venue_hire),
    addl_prod = as.numeric(addl_prod),
    mkt_social = as.numeric(mkt_social),
    mkt_design = as.numeric(mkt_design),
    photo = as.numeric(photo),
    promoter_flat = as.numeric(promoter_flat),
    merch_cost = as.numeric(merch_cost),
    stringsAsFactors = FALSE
  )
}

# -----------------------------
# UI
# -----------------------------
ui <- page_navbar(
  title = "KPB Tour Decision Support",
  id = "main_nav",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#17365D",
    secondary = "#5B6770"
  ),

  nav_panel(
    "Dashboard",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h4("Current Tour"),
        textInput("tour_name", NULL, value = "KPB Australia Tour"),
        textInput("currency", "Currency symbol", value = "$"),
        actionButton("reset_model", "Reset to example", class = "btn-outline-secondary w-100"),
        br(), br(),
        h5("Scenario"),
        selectInput("dashboard_scenario", NULL, choices = "Current model"),
        actionButton("load_dashboard_scenario", "Load selected scenario", class = "btn-primary w-100"),
        br(), br(),
        p(class = "text-muted",
          "Use the Cities and Costs tabs to change assumptions. The dashboard updates live.")
      ),
      div(
        class = "container-fluid",
        h2(textOutput("dash_title")),
        p(class = "text-muted", textOutput("dash_subtitle")),
        uiOutput("kpi_cards"),
        layout_columns(
          card(
            card_header("Profit by city"),
            plotlyOutput("plot_profit_city", height = "330px")
          ),
          card(
            card_header("Revenue vs cost by city"),
            plotlyOutput("plot_revenue_cost", height = "330px")
          ),
          col_widths = c(6, 6)
        ),
        layout_columns(
          card(
            card_header("P&L waterfall"),
            plotlyOutput("plot_waterfall", height = "380px")
          ),
          card(
            card_header("Cost composition"),
            plotlyOutput("plot_cost_mix", height = "380px")
          ),
          col_widths = c(7, 5)
        ),
        card(
          card_header("City economics"),
          tableOutput("tbl_city_economics")
        )
      )
    )
  ),

  nav_panel(
    "Cities",
    layout_columns(
      card(
        card_header(
          div(
            class = "d-flex justify-content-between align-items-center",
            span("Tour cities"),
            div(
              actionButton("add_city", "+ Add city", class = "btn-primary"),
              actionButton("duplicate_city", "Duplicate selected", class = "btn-outline-primary"),
              actionButton("remove_city", "Remove selected", class = "btn-outline-danger")
            )
          )
        ),
        p(class = "text-muted",
          "Select a city below, then edit its assumptions in the City Editor."),
        tableOutput("tbl_city_list")
      ),
      card(
        card_header("City Editor"),
        uiOutput("city_editor"),
        width = 5
      ),
      col_widths = c(7, 5)
    )
  ),

  nav_panel(
    "Tour Costs",
    layout_sidebar(
      sidebar = sidebar(
        h4("Touring party"),
        numericInput("band_members", "Band members", value = 7, min = 0, max = 30),
        numericInput("managers", "Managers / tour crew", value = 2, min = 0, max = 15),
        sliderInput("nights", "Nights in-country", min = 1, max = 30, value = 9),
        sliderInput("total_days", "Total tour days", min = 1, max = 30, value = 10),
        tags$hr(),
        h4("Unit costs"),
        numericInput("airfare_cost", "Return airfare / person", value = 2000, min = 0),
        numericInput("baggage_cost", "Baggage / instrument freight / band member", value = 150, min = 0),
        numericInput("visa_cost", "Visa / person", value = 405, min = 0),
        numericInput("insurance_cost", "Insurance / person", value = 60, min = 0),
        numericInput("hotel_rate", "Hotel rate / room / night", value = 180, min = 0),
        numericInput("van_rate", "Ground transport / day", value = 450, min = 0),
        numericInput("intercity_cost", "Inter-city travel / person", value = 180, min = 0),
        numericInput("perdiem_rate", "Per diem / person / day", value = 100, min = 0),
        numericInput("backline_cost", "Local musician / backline hire", value = 0, min = 0),
        numericInput("crew_tips", "Ground crew / stagehand tips & incidentals", value = 500, min = 0),
        sliderInput("contingency_pct", "Contingency", min = 0, max = 0.30, value = 0.10, step = 0.01)
      ),
      div(
        class = "container-fluid",
        h3("Touring party cost structure"),
        uiOutput("tour_cost_kpis"),
        layout_columns(
          card(
            card_header("Touring party costs"),
            plotlyOutput("plot_tour_costs", height = "400px")
          ),
          card(
            card_header("Tour cost detail"),
            tableOutput("tbl_tour_costs")
          ),
          col_widths = c(6, 6)
        )
      )
    )
  ),

  nav_panel(
    "Merchandise",
    layout_sidebar(
      sidebar = sidebar(
        checkboxInput("merch_on", "Include merchandise", value = FALSE),
        conditionalPanel(
          "input.merch_on == true",
          sliderInput("merch_units", "Total units sold", min = 0, max = 5000, value = 400, step = 10),
          sliderInput("merch_price", "Average selling price / unit", min = 0, max = 200, value = 35, step = 1)
        )
      ),
      div(
        class = "container-fluid",
        h3("Merchandise economics"),
        uiOutput("merch_kpis"),
        card(
          card_header("Merchandise by city"),
          tableOutput("tbl_merch")
        )
      )
    )
  ),

  nav_panel(
    "Break-even",
    div(
      class = "container-fluid",
      br(),
      h3("Break-even analysis"),
      p(class = "text-muted",
        "Break-even separates fixed costs from ticket-dependent fees and promoter percentages. Merchandise contribution is treated as an offset to fixed costs."),
      uiOutput("be_kpis"),
      layout_columns(
        card(
          card_header("Revenue and cost curve"),
          plotlyOutput("plot_breakeven", height = "430px")
        ),
        card(
          card_header("Break-even metrics"),
          tableOutput("tbl_breakeven")
        ),
        col_widths = c(7, 5)
      )
    )
  ),

  nav_panel(
    "Sensitivity",
    div(
      class = "container-fluid",
      br(),
      h3("Scenario sensitivity"),
      layout_sidebar(
        sidebar = sidebar(
          h4("Sensitivity controls"),
          sliderInput("sens_fill_min", "Minimum fill", min = 0.20, max = 0.80, value = 0.50, step = 0.05),
          sliderInput("sens_fill_max", "Maximum fill", min = 0.40, max = 1.00, value = 1.00, step = 0.05),
          sliderInput("sens_price_min", "Ticket price change — low", min = -0.30, max = 0, value = -0.10, step = 0.05),
          sliderInput("sens_price_max", "Ticket price change — high", min = 0, max = 0.30, value = 0.10, step = 0.05),
          sliderInput("sens_cost_change", "Fixed cost change", min = -0.20, max = 0.50, value = 0, step = 0.05),
          sliderInput("sens_points", "Grid points", min = 5, max = 15, value = 9, step = 1)
        ),
        div(
          layout_columns(
            card(
              card_header("Fill rate × ticket price"),
              plotlyOutput("plot_heatmap", height = "500px")
            ),
            card(
              card_header("Profit vs fill rate"),
              plotlyOutput("plot_fill_sensitivity", height = "500px")
            ),
            col_widths = c(7, 5)
          ),
          card(
            card_header("Sensitivity table"),
            tableOutput("tbl_sensitivity")
          )
        )
      )
    )
  )

  # nav_panel(
  #   "Scenarios",
  #   layout_columns(
  #     card(
  #       card_header("Scenario manager"),
  #       textInput("scenario_name", "Scenario name", value = "Base Case"),
  #       actionButton("save_scenario", "Save current scenario", class = "btn-success"),
  #       actionButton("delete_scenario", "Delete selected", class = "btn-outline-danger"),
  #       tags$hr(),
  #       selectInput("scenario_select", "Saved scenarios", choices = NULL),
  #       actionButton("load_scenario", "Load selected scenario", class = "btn-primary"),
  #       br(), br(),
  #       downloadButton("download_scenarios", "Download scenario results")
  #     ),
  #     card(
  #       card_header("Scenario comparison"),
  #       p(class = "text-muted",
  #         "Saved scenarios are snapshots of the assumptions at the time they are saved."),
  #       tableOutput("tbl_scenarios")
  #     ),
  #     col_widths = c(4, 8)
  #   )
  # ),

  # nav_panel(
  #   "Export",
  #   div(
  #     class = "container-fluid",
  #     br(),
  #     h3("Export"),
  #     p("Download the current city economics and scenario comparison for circulation or further analysis."),
  #     downloadButton("download_city_data", "Download city economics CSV", class = "btn-primary"),
  #     downloadButton("download_scenario_data", "Download scenario comparison CSV", class = "btn-outline-primary")
  #   )
  # )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {

  # Dynamic city store
  cities <- reactiveVal(
    rbind(
      new_city("Sydney", 3000, 0.90, 0.85, 70, 160, 0.08, 0.20,
               30000, 3000, 2500, 800, 1200, 0),
      new_city("Melbourne", 2000, 0.90, 0.85, 70, 150, 0.08, 0.20,
               30000, 2500, 2000, 600, 1000, 0)
    )
  )

  selected_city <- reactiveVal(NULL)
  scenarios <- reactiveVal(list())

  observe({
    df <- cities()
    if (nrow(df) == 0) {
      selected_city(NULL)
    } else if (is.null(selected_city()) || !selected_city() %in% df$id) {
      selected_city(df$id[1])
    }
  })

  # -------------------------
  # City editor helpers
  # -------------------------
  output$tbl_city_list <- renderTable({
    df <- cities()
    if (nrow(df) == 0) return(data.frame(Message = "No cities added."))
    out <- data.frame(
      Selected = ifelse(df$id == selected_city(), "●", ""),
      City = df$city,
      Capacity = num(df$capacity),
      `Target fill` = pct(df$fill),
      `GA price` = fmt(df$ga_price),
      `VIP price` = fmt(df$vip_price),
      check.names = FALSE
    )
    out
  }, striped = TRUE, bordered = FALSE, hover = TRUE)

  output$city_editor <- renderUI({
    df <- isolate(cities())
    if (nrow(df) == 0) return(p("Add a city to begin."))

    id <- isolate(selected_city())
    if (is.null(id) || !id %in% df$id) id <- df$id[1]
    row <- df[df$id == id, , drop = FALSE]

    tagList(
      selectInput("city_selector", "Editing city",
                  choices = setNames(df$id, df$city), selected = id),
      textInput("edit_city", "City name", value = row$city),
      sliderInput("edit_capacity", "Venue capacity", min = 500, max = 20000,
                  value = row$capacity, step = 50),
      sliderInput("edit_fill", "Target fill rate", min = 0, max = 1,
                  value = row$fill, step = 0.01),
      sliderInput("edit_ga_pct", "GA share of sold tickets", min = 0, max = 1,
                  value = row$ga_pct, step = 0.01),
      sliderInput("edit_ga_price", "GA ticket price", min = 0, max = 500,
                  value = row$ga_price, step = 5),
      sliderInput("edit_vip_price", "VIP ticket price", min = 0, max = 800,
                  value = row$vip_price, step = 5),
      sliderInput("edit_fee", "Ticketing fee (% gross)", min = 0, max = 0.25,
                  value = row$fee, step = 0.005),
      sliderInput("edit_promoter", "Promoter share (% net box office)", min = 0, max = 0.50,
                  value = row$promoter, step = 0.01),
      tags$hr(),
      h5("City costs"),
      numericInput("edit_venue", "Venue hire + house sound/lighting",
                   value = row$venue_hire, min = 0),
      numericInput("edit_prod", "Additional production",
                   value = row$addl_prod, min = 0),
      numericInput("edit_social", "Paid social marketing",
                   value = row$mkt_social, min = 0),
      numericInput("edit_design", "Design",
                   value = row$mkt_design, min = 0),
      numericInput("edit_photo", "Photography / videography",
                   value = row$photo, min = 0),
      numericInput("edit_promoter_flat", "Promoter fixed fee",
                   value = row$promoter_flat, min = 0),
      numericInput("edit_merch_cost", "Merch production cost",
                   value = row$merch_cost, min = 0),
      actionButton("apply_city", "Apply city changes", class = "btn-primary w-100")
    )
  })

  observe({
    df <- cities()
    if (!nrow(df)) return()

    id <- selected_city()
    if (is.null(id) || !id %in% df$id) id <- df$id[1]
    row <- df[df$id == id, , drop = FALSE]

    updateSelectInput(session, "city_selector",
                      choices = setNames(df$id, df$city),
                      selected = id)

    updateTextInput(session, "edit_city", value = row$city)
    updateSliderInput(session, "edit_capacity", value = row$capacity)
    updateSliderInput(session, "edit_fill", value = row$fill)
    updateSliderInput(session, "edit_ga_pct", value = row$ga_pct)
    updateSliderInput(session, "edit_ga_price", value = row$ga_price)
    updateSliderInput(session, "edit_vip_price", value = row$vip_price)
    updateSliderInput(session, "edit_fee", value = row$fee)
    updateSliderInput(session, "edit_promoter", value = row$promoter)
    updateNumericInput(session, "edit_venue", value = row$venue_hire)
    updateNumericInput(session, "edit_prod", value = row$addl_prod)
    updateNumericInput(session, "edit_social", value = row$mkt_social)
    updateNumericInput(session, "edit_design", value = row$mkt_design)
    updateNumericInput(session, "edit_photo", value = row$photo)
    updateNumericInput(session, "edit_promoter_flat", value = row$promoter_flat)
    updateNumericInput(session, "edit_merch_cost", value = row$merch_cost)
  })

  observeEvent(input$city_selector, {
    if (!is.null(input$city_selector)) selected_city(input$city_selector)
  }, ignoreInit = TRUE)

  observeEvent(input$apply_city, {
    id <- selected_city()
    if (is.null(id) || is.null(input$edit_city)) return()
    df <- cities()
    i <- match(id, df$id)
    if (is.na(i)) return()

    df[i, "city"] <- input$edit_city
    df[i, "capacity"] <- as.numeric(input$edit_capacity)
    df[i, "fill"] <- as.numeric(input$edit_fill)
    df[i, "ga_pct"] <- as.numeric(input$edit_ga_pct)
    df[i, "ga_price"] <- as.numeric(input$edit_ga_price)
    df[i, "vip_price"] <- as.numeric(input$edit_vip_price)
    df[i, "fee"] <- as.numeric(input$edit_fee)
    df[i, "promoter"] <- as.numeric(input$edit_promoter)
    df[i, "venue_hire"] <- as.numeric(input$edit_venue)
    df[i, "addl_prod"] <- as.numeric(input$edit_prod)
    df[i, "mkt_social"] <- as.numeric(input$edit_social)
    df[i, "mkt_design"] <- as.numeric(input$edit_design)
    df[i, "photo"] <- as.numeric(input$edit_photo)
    df[i, "promoter_flat"] <- as.numeric(input$edit_promoter_flat)
    df[i, "merch_cost"] <- as.numeric(input$edit_merch_cost)
    cities(df)
  })

  # Dynamic city actions
  observeEvent(input$add_city, {
    df <- cities()
    cities(rbind(df, new_city(paste0("City ", nrow(df) + 1))))
  })

  observeEvent(input$duplicate_city, {
    df <- cities()
    id <- selected_city()
    if (is.null(id) || !id %in% df$id) return()
    row <- df[df$id == id, , drop = FALSE]
    row$id <- paste0("city_", as.integer(runif(1, 1, 99999999)))
    row$city <- paste0(row$city, " — Copy")
    cities(rbind(df, row))
    selected_city(row$id)
  })

  observeEvent(input$remove_city, {
    df <- cities()
    id <- selected_city()
    if (nrow(df) <= 1 || is.null(id)) return()
    df <- df[df$id != id, , drop = FALSE]
    cities(df)
    selected_city(df$id[1])
  })

  # -------------------------
  # Calculations
  # -------------------------
  touring_calc <- reactive({
    party <- input$band_members + input$managers
    rooms <- ceiling(party / 2)

    items <- data.frame(
      Item = c(
        "Return airfare", "Excess baggage / instrument freight", "Visa",
        "Travel / medical insurance", "Accommodation",
        "Ground transport", "Inter-city travel", "Per diem",
        "Local musician / backline hire", "Ground crew tips / incidentals"
      ),
      Total = c(
        input$airfare_cost * party,
        input$baggage_cost * input$band_members,
        input$visa_cost * party,
        input$insurance_cost * party,
        input$hotel_rate * rooms * input$nights,
        input$van_rate * input$total_days,
        input$intercity_cost * party,
        input$perdiem_rate * party * input$total_days,
        input$backline_cost,
        input$crew_tips
      ),
      stringsAsFactors = FALSE
    )

    subtotal <- sum(items$Total)
    contingency <- subtotal * input$contingency_pct
    total <- subtotal + contingency

    list(items = items, party = party, rooms = rooms, subtotal = subtotal,
         contingency = contingency, total = total)
  })

  city_calc <- reactive({
    df <- cities()
    if (nrow(df) == 0) return(data.frame())

    out <- lapply(seq_len(nrow(df)), function(i) {
      x <- df[i, ]

      capacity <- as.numeric(x$capacity)
      fill <- max(0, min(as.numeric(x$fill), 1))
      ga_pct <- as.numeric(x$ga_pct)
      ga_price <- as.numeric(x$ga_price)
      vip_price <- as.numeric(x$vip_price)
      fee <- as.numeric(x$fee)
      promoter <- as.numeric(x$promoter)
      venue_hire <- as.numeric(x$venue_hire)
      addl_prod <- as.numeric(x$addl_prod)
      mkt_social <- as.numeric(x$mkt_social)
      mkt_design <- as.numeric(x$mkt_design)
      photo <- as.numeric(x$photo)
      promoter_flat <- as.numeric(x$promoter_flat)
      merch_cost <- as.numeric(x$merch_cost)

      sold <- round(capacity * fill)
      ga_sold <- round(sold * ga_pct)
      vip_sold <- sold - ga_sold

      gross <- (ga_sold * ga_price) + (vip_sold * vip_price)
      fees <- gross * fee
      net_box <- gross - fees
      promoter_pct_cost <- net_box * promoter
      net_ticket_rev <- net_box - promoter_pct_cost

      fixed_city_cost <- sum(venue_hire, addl_prod, mkt_social, mkt_design, photo, promoter_flat)
      variable_ticket_cost <- fees + promoter_pct_cost
      avg_gross_ticket <- ifelse(sold > 0, gross / sold, 0)
      net_contribution_per_ticket <- ifelse(sold > 0, net_ticket_rev / sold, 0)

      data.frame(
        id = x$id,
        City = x$city,
        Capacity = capacity,
        `Fill rate` = fill,
        `Tickets sold` = sold,
        `GA tickets` = ga_sold,
        `VIP tickets` = vip_sold,
        `Gross ticket revenue` = gross,
        `Ticketing fees` = fees,
        `Promoter %` = promoter_pct_cost,
        `Promoter fixed` = promoter_flat,
        `Net ticket revenue` = net_ticket_rev,
        `City fixed costs` = fixed_city_cost,
        `Ticket variable costs` = variable_ticket_cost,
        `Avg gross ticket` = avg_gross_ticket,
        `Net contribution / ticket` = net_contribution_per_ticket,
        `Merch cost` = merch_cost,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    })

    do.call(rbind, out)
  })

  model_calc <- reactive({
    cdf <- city_calc()
    tour <- touring_calc()

    city_fixed <- if (nrow(cdf)) sum(cdf$`City fixed costs`) else 0
    ticket_revenue <- if (nrow(cdf)) sum(cdf$`Net ticket revenue`) else 0
    tickets <- if (nrow(cdf)) sum(cdf$`Tickets sold`) else 0
    capacity <- if (nrow(cdf)) sum(cdf$Capacity) else 0

    merch_revenue <- if (isTRUE(input$merch_on)) input$merch_units * input$merch_price else 0
    merch_cost <- if (isTRUE(input$merch_on) && nrow(cdf)) sum(cdf$`Merch cost`) else 0
    merch_contribution <- merch_revenue - merch_cost

    total_revenue <- ticket_revenue + merch_revenue
    total_cost <- tour$total + city_fixed + merch_cost
    profit <- total_revenue - total_cost
    margin <- ifelse(total_revenue != 0, profit / total_revenue, NA)

    fixed_costs <- tour$total + city_fixed + merch_cost
    fixed_after_merch <- fixed_costs - merch_revenue

    weighted_net_ticket_contribution <- if (nrow(cdf) && tickets > 0) {
      ticket_revenue / tickets
    } else 0

    be_tickets <- if (weighted_net_ticket_contribution > 0) {
      max(0, fixed_after_merch / weighted_net_ticket_contribution)
    } else Inf

    be_fill <- if (capacity > 0) be_tickets / capacity else NA

    list(
      cities = cdf, tour = tour, ticket_revenue = ticket_revenue,
      tickets = tickets, capacity = capacity, merch_revenue = merch_revenue,
      merch_cost = merch_cost, merch_contribution = merch_contribution,
      total_revenue = total_revenue, total_cost = total_cost, profit = profit,
      margin = margin, fixed_costs = fixed_costs, fixed_after_merch = fixed_after_merch,
      net_ticket_contribution_per_ticket = weighted_net_ticket_contribution,
      be_tickets = be_tickets, be_fill = be_fill
    )
  })

  # -------------------------
  # Dashboard Outputs
  # -------------------------
  output$dash_title <- renderText(input$tour_name)
  output$dash_subtitle <- renderText({
    m <- model_calc()
    paste0(nrow(m$cities), " cities |  ", num(m$tickets), " projected tickets | ",
           num(m$capacity), " total capacity")
  })

  output$kpi_cards <- renderUI({
    m <- model_calc()
    fluidRow(
      column(2, div(class = "card p-3 mb-3", h6("Net ticket revenue"), h3(fmt(m$ticket_revenue)))),
      column(2, div(class = "card p-3 mb-3", h6("Total revenue"), h3(fmt(m$total_revenue)))),
      column(2, div(class = "card p-3 mb-3", h6("Total costs"), h3(fmt(m$total_cost)))),
      column(2, div(class = "card p-3 mb-3", h6("Projected profit"), h3(fmt(m$profit)))),
      column(2, div(class = "card p-3 mb-3", h6("Margin"), h3(pct(m$margin)))),
      column(2, div(class = "card p-3 mb-3", h6("Break-even fill"), h3(pct(m$be_fill))))
    )
  })

  output$tbl_city_economics <- renderTable({     d <- model_calc()$cities
    if (!nrow(d)) return(data.frame())

    data.frame(
      City = d$City,
      Capacity = num(d$Capacity),
      `Tickets sold` = num(d$`Tickets sold`),
      `Fill` = pct(d$`Fill rate`),
      `Net ticket revenue` = fmt(d$`Net ticket revenue`),
      `City fixed cost` = fmt(d$`City fixed costs`),
      `Net contribution / ticket` = fmt(d$`Net contribution / ticket`, 2),
      check.names = FALSE
    )
  })

  output$plot_profit_city <- renderPlotly({     d <- model_calc()$cities
    validate(need(nrow(d) > 0, "Add a city."))

    d$ProfitContribution <- d[["Net ticket revenue"]] - d[["City fixed costs"]]
    d <- d[order(d$ProfitContribution), , drop = FALSE]

    plot_ly(
      x = d$City, y = d$ProfitContribution, type = "bar",
      hovertemplate = "%{x}<br>Profit contribution: $%{y:,.0f}<extra></extra>"
    ) |> layout(xaxis = list(title = ""), yaxis = list(title = "Currency"), margin = list(b = 80))
  })

  output$plot_revenue_cost <- renderPlotly({     d <- model_calc()$cities
    validate(need(nrow(d) > 0, "Add a city."))

    p <- plot_ly(x = d$City)
    p <- add_bars(p, y = d[["Net ticket revenue"]], name = "Net ticket revenue")
    p <- add_bars(p, y = d[["City fixed costs"]], name = "City fixed costs")
    p |> layout(barmode = "group", yaxis = list(title = "Currency"))
  })

  output$plot_waterfall <- renderPlotly({
    m <- model_calc()
    x <- c("Net ticket revenue", "Merch contribution", "Touring costs", "City fixed costs", "Net profit")
    measure <- c("absolute", "relative", "relative", "relative", "total")
    y <- c(m$ticket_revenue, m$merch_contribution, -m$tour$total, -sum(m$cities$`City fixed costs`), m$profit)

    plot_ly(x = x, y = y, type = "waterfall", measure = measure,
            connector = list(line = list(color = "rgba(80,80,80,0.4)"))) |>
      layout(yaxis = list(title = "Currency"))
  })

  output$plot_cost_mix <- renderPlotly({
    m <- model_calc()
    city_cost <- if (nrow(m$cities)) sum(m$cities$`City fixed costs`) else 0

    d <- data.frame(
      Category = c("Touring party", "Venue / production / marketing", "Merchandise cost"),
      Cost = c(m$tour$total, city_cost, m$merch_cost)
    )
    d <- d[d$Cost > 0, ]

    plot_ly(d, labels = ~Category, values = ~Cost, type = "pie", textinfo = "label+percent")
  })

  # -------------------------
  # Tour Cost Outputs
  # -------------------------
  output$tour_cost_kpis <- renderUI({
    t <- touring_calc()
    fluidRow(
      column(3, div(class = "card p-3", h6("Party"), h3(num(t$party)))),
      column(3, div(class = "card p-3", h6("Rooms"), h3(num(t$rooms)))),
      column(3, div(class = "card p-3", h6("Subtotal"), h3(fmt(t$subtotal)))),
      column(3, div(class = "card p-3", h6("Total incl. contingency"), h3(fmt(t$total))))
    )
  })

  output$tbl_tour_costs <- renderTable({
    t <- touring_calc()
    out <- t$items
    out$Total <- fmt(out$Total)
    rbind(
      out,
      data.frame(Item = "Subtotal", Total = fmt(t$subtotal)),
      data.frame(Item = paste0("Contingency (", pct(input$contingency_pct), ")"), Total = fmt(t$contingency)),
      data.frame(Item = "TOTAL", Total = fmt(t$total))
    )
  })

  output$plot_tour_costs <- renderPlotly({     d <- touring_calc()$items
    plot_ly(d, x = ~Total, y = ~reorder(Item, Total), type = "bar", orientation = "h") |>
      layout(xaxis = list(title = "Currency"), yaxis = list(title = ""))
  })

  # -------------------------
  # Merchandise Outputs
  # -------------------------
  output$merch_kpis <- renderUI({
    m <- model_calc()
    fluidRow(
      column(4, div(class = "card p-3", h6("Merch revenue"), h3(fmt(m$merch_revenue)))),
      column(4, div(class = "card p-3", h6("Merch cost"), h3(fmt(m$merch_cost)))),
      column(4, div(class = "card p-3", h6("Net contribution"), h3(fmt(m$merch_contribution))))
    )
  })

  output$tbl_merch <- renderTable({     d <- model_calc()$cities
    data.frame(City = d$City, `Production cost` = fmt(d$`Merch cost`), check.names = FALSE)
  })

  # -------------------------
  # Break-even Outputs
  # -------------------------
  output$be_kpis <- renderUI({
    m <- model_calc()
    fluidRow(
      column(3, div(class = "card p-3", h6("Fixed costs"), h3(fmt(m$fixed_costs)))),
      column(3, div(class = "card p-3", h6("Net contribution / ticket"), h3(fmt(m$net_ticket_contribution_per_ticket, 2)))),
      column(3, div(class = "card p-3", h6("Break-even tickets"), h3(num(m$be_tickets)))),
      column(3, div(class = "card p-3", h6("Break-even fill"), h3(pct(m$be_fill))))
    )
  })

  output$tbl_breakeven <- renderTable({
    m <- model_calc()
    data.frame(
      Metric = c("Total capacity", "Projected tickets", "Projected fill", "Fixed costs",
                 "Merchandise contribution", "Net ticket contribution / ticket",
                 "Break-even tickets", "Break-even fill rate", "Headroom above break-even"),
      Value = c(num(m$capacity), num(m$tickets), pct(m$tickets / max(m$capacity, 1)),
                fmt(m$fixed_costs), fmt(m$merch_contribution),
                fmt(m$net_ticket_contribution_per_ticket, 2),
                ifelse(is.finite(m$be_tickets), num(m$be_tickets), "Not achievable"),
                pct(m$be_fill), pct((m$tickets - m$be_tickets) / max(m$capacity, 1))),
      check.names = FALSE
    )
  })

  output$plot_breakeven <- renderPlotly({
    m <- model_calc()
    max_tickets <- max(m$capacity, 1)
    q <- seq(0, max_tickets, length.out = 100)

    avg_ticket <- m$net_ticket_contribution_per_ticket
    fixed_after_merch <- m$fixed_after_merch

    d <- data.frame(
      Tickets = q,
      Revenue = q * avg_ticket,
      FixedCost = fixed_after_merch
    )

    p <- plot_ly(d, x = ~Tickets) |>
      add_lines(y = ~Revenue, name = "Net ticket contribution") |>
      add_lines(y = ~FixedCost, name = "Fixed costs after merch") |>
      layout(xaxis = list(title = "Tickets sold"), yaxis = list(title = "Currency"))

    if (is.finite(m$be_tickets) && m$be_tickets <= max_tickets) {
      p <- p |> add_markers(x = m$be_tickets, y = m$be_tickets * avg_ticket, name = "Break-even")
    }
    p
  })

  # -------------------------
  # Sensitivity Analysis
  # -------------------------
  sensitivity_data <- reactive({
    m <- model_calc()
    n <- input$sens_points

    fills <- seq(input$sens_fill_min, input$sens_fill_max, length.out = n)
    prices <- seq(input$sens_price_min, input$sens_price_max, length.out = n)
    base <- cities()

    calc_profit <- function(fill_val, price_change, cost_change) {
      total_ticket_rev <- 0
      city_fixed_costs <- 0

      for (i in seq_len(nrow(base))) {
        x <- base[i, ]
        cap <- as.numeric(x$capacity)
        sold <- round(cap * min(fill_val, 1))
        ga <- round(sold * as.numeric(x$ga_pct))
        vip <- sold - ga

        ga_p <- as.numeric(x$ga_price) * (1 + price_change)
        vip_p <- as.numeric(x$vip_price) * (1 + price_change)

        gross <- (ga * ga_p) + (vip * vip_p)
        net_box <- gross * (1 - as.numeric(x$fee))
        net_rev <- net_box * (1 - as.numeric(x$promoter))
        total_ticket_rev <- total_ticket_rev + net_rev

        city_fixed <- sum(as.numeric(x$venue_hire), as.numeric(x$addl_prod),
                          as.numeric(x$mkt_social), as.numeric(x$mkt_design),
                          as.numeric(x$photo), as.numeric(x$promoter_flat))
        city_fixed_costs <- city_fixed_costs + city_fixed
      }

      tour_cost <- m$tour$total * (1 + cost_change)
      tot_rev <- total_ticket_rev + m$merch_contribution
      tot_cost <- tour_cost + (city_fixed_costs * (1 + cost_change))
      tot_rev - tot_cost
    }

    grid <- expand.grid(Fill = fills, PriceChange = prices)
    grid$Profit <- mapply(calc_profit, grid$Fill, grid$PriceChange, MoreArgs = list(cost_change = input$sens_cost_change))
    
    list(grid = grid, fills = fills, prices = prices)
  })

  output$plot_heatmap <- renderPlotly({
    s <- sensitivity_data()
    mat <- matrix(s$grid$Profit, nrow = length(s$fills), ncol = length(s$prices))

    plot_ly(x = pct(s$prices), y = pct(s$fills), z = mat, type = "heatmap", colors = "RdYlGn") |>
      layout(xaxis = list(title = "Ticket Price Change"), yaxis = list(title = "Fill Rate"))
  })

  output$plot_fill_sensitivity <- renderPlotly({
    s <- sensitivity_data()
    df_base <- subset(s$grid, PriceChange == 0)

    plot_ly(df_base, x = ~pct(Fill), y = ~Profit, type = "scatter", mode = "lines+markers") |>
      layout(xaxis = list(title = "Fill Rate"), yaxis = list(title = "Profit"))
  })

  output$tbl_sensitivity <- renderTable({
    s <- sensitivity_data()
    mat <- matrix(fmt(s$grid$Profit), nrow = length(s$fills), ncol = length(s$prices))
    rownames(mat) <- pct(s$fills)
    colnames(mat) <- pct(s$prices)
    mat
  }, rownames = TRUE)

  # -------------------------
  # Scenario Management
  # -------------------------
  observeEvent(input$save_scenario, {
    req(input$scenario_name)
    sc <- scenarios()
    sc[[input$scenario_name]] <- model_calc()
    scenarios(sc)

    updateSelectInput(session, "scenario_select", choices = names(sc), selected = input$scenario_name)
    updateSelectInput(session, "dashboard_scenario", choices = c("Current model", names(sc)))
  })

  output$tbl_scenarios <- renderTable({
    sc <- scenarios()
    if (length(sc) == 0) return(data.frame(Message = "No scenarios saved."))

    out <- lapply(names(sc), function(name) {
      m <- sc[[name]]
      data.frame(
        Scenario = name,
        Tickets = num(m$tickets),
        `Total Revenue` = fmt(m$total_revenue),
        `Total Cost` = fmt(m$total_cost),
        Profit = fmt(m$profit),
        `Break-even fill` = pct(m$be_fill),
        check.names = FALSE
      )
    })
    do.call(rbind, out)
  })

  # Export Download Handlers
  output$download_city_data <- downloadHandler(
    filename = function() { paste0("city_economics_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(model_calc()$cities, file, row.names = FALSE) }
  )
}

shinyApp(ui = ui, server = server)