# ============================================================
# UNSW-NB15 Intrusion Detection - Interactive Dashboard (R Shiny + Plotly)
# Run with: shiny::runApp("dashboard")
# Folder must contain: dashboard_data.rds, rf_model.rds, unsw_recipe.rds
# ============================================================
library(shiny)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(recipes)
library(workflows)
library(parsnip)
library(ranger)

D          <- readRDS("dashboard_data.rds")
rf_model   <- readRDS("rf_model.rds")
recipe_obj <- readRDS("unsw_recipe.rds")

per_class <- function(model) {
  cm <- table(D$truth, D$preds[[model]])
  tp <- diag(cm); fp <- colSums(cm) - tp; fn <- rowSums(cm) - tp
  prec <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
  rec  <- ifelse(tp + fn == 0, 0, tp / (tp + fn))
  f1   <- ifelse(prec + rec == 0, 0, 2 * prec * rec / (prec + rec))
  tibble(class = rownames(cm), support = as.integer(rowSums(cm)),
         precision = round(prec, 3), recall = round(rec, 3), f1 = round(f1, 3))
}

# Scenario record = median Normal HTTP/TCP row, with attack-defining features
# overridden and derived fields recomputed so the record stays consistent.
build_row <- function(dur, spkts, dpkts, sbytes, dbytes, ct_dst_ltm, ct_srv_dst) {
  row <- D$template
  row$dur <- dur; row$spkts <- spkts; row$dpkts <- dpkts
  row$sbytes <- sbytes; row$dbytes <- dbytes
  row$rate  <- (spkts + dpkts - 1) / dur
  row$sload <- sbytes * 8 / dur
  row$dload <- dbytes * 8 / dur
  row$smean <- sbytes / max(spkts, 1)
  row$dmean <- dbytes / max(dpkts, 1)
  row$ct_dst_ltm <- ct_dst_ltm; row$ct_dst_src_ltm <- ct_dst_ltm
  row$ct_srv_dst <- ct_srv_dst; row$ct_srv_src     <- ct_srv_dst
  row$proto <- "tcp"; row$service <- "http"; row$state <- "CON"
  row
}

clamp <- function(x, lo, hi) min(max(x, lo), hi)

ui <- navbarPage(
  "Network Intrusion Detection - UNSW-NB15",

  tabPanel("Model comparison",
    fluidRow(
      column(7, plotlyOutput("cmp_plot", height = 440)),
      column(5, plotlyOutput("cv_plot", height = 440))
    ),
    helpText("Left: strict test macro-F1 (a class the model never predicts counts as 0) with 95% bootstrap CI.",
             "Right: CV macro-F1 (yardstick drops classes with no predictions) vs strict test macro-F1 -",
             "the two use different definitions, so the gap is not purely generalization."),
    h4("Test results"), DTOutput("cmp_table"),
    h4("Paired bootstrap: best model vs each other model"), DTOutput("paired_table")
  ),

  tabPanel("Per-class analysis",
    sidebarLayout(
      sidebarPanel(
        selectInput("model", "Model", choices = names(D$preds), selected = D$best),
        checkboxInput("norm", "Row-normalize (share of each true class)", TRUE),
        helpText("Rows = true class, columns = predicted class.")
      ),
      mainPanel(plotlyOutput("cm_plot", height = 520), DTOutput("pc_table"))
    )
  ),

  tabPanel("Feature importance",
    plotlyOutput("imp_plot", height = 540),
    helpText("Random Forest impurity importance (top 20 features after preprocessing).")
  ),

  tabPanel("Attack scenario tester",
    sidebarLayout(
      sidebarPanel(
        selectInput("preset", "Scenario preset",
                    c("Low-and-slow DoS (custom)" = "slow",
                      "Median normal HTTP (control)" = "normal")),
        sliderInput("dur",   "Connection duration (s)", 0.1, 120, 45, step = 0.1),
        sliderInput("spkts", "Source packets", 1, 60, 4),
        sliderInput("dpkts", "Destination packets", 0, 60, 3),
        sliderInput("sbytes", "Source bytes", 0, 5000, 120, step = 10),
        sliderInput("dbytes", "Destination bytes", 0, 20000, 90, step = 10),
        sliderInput("ct_dst_ltm", "Recent connections to same destination", 1, 60, 55),
        sliderInput("ct_srv_dst", "Recent connections, same service + destination", 1, 60, 48)
      ),
      mainPanel(
        h3(textOutput("pred_text")),
        plotlyOutput("prob_plot", height = 420),
        helpText("Starting point is the median Normal HTTP/TCP record from the dataset; only the",
                 "attack-defining features above are changed. Rate, load and mean packet size are",
                 "recomputed from the sliders so the record stays internally consistent.")
      )
    )
  )
)

server <- function(input, output, session) {

  output$cmp_plot <- renderPlotly({
    ci <- D$ci_table %>% mutate(model = factor(model, levels = rev(model)))
    plot_ly(ci, x = ~macro_f1, y = ~model, type = "bar", orientation = "h",
            marker = list(color = "#00A87E"),
            error_x = list(type = "data", symmetric = FALSE,
                           array = ci$ci_hi - ci$macro_f1,
                           arrayminus = ci$macro_f1 - ci$ci_lo)) %>%
      layout(title = "Test macro-F1 with 95% bootstrap CI",
             xaxis = list(title = "Macro F1 (strict)"), yaxis = list(title = ""))
  })

  output$cv_plot <- renderPlotly({
    validate(need(nrow(D$cv) > 0, "phase2_cv_comparison.csv was not found when preparing the dashboard."))
    cmp <- D$cv %>% inner_join(D$ci_table %>% select(model, test_f1 = macro_f1), by = "model")
    plot_ly(cmp, x = ~cv_f1, y = ~test_f1, type = "scatter", mode = "markers+text",
            text = ~model, textposition = "top center", marker = list(size = 10, color = "#1F3A5F")) %>%
      add_lines(x = c(0.3, 0.8), y = c(0.3, 0.8), inherit = FALSE,
                line = list(dash = "dot", color = "grey"), showlegend = FALSE) %>%
      layout(title = "CV vs test macro-F1",
             xaxis = list(title = "CV macro-F1 (unpredicted classes excluded)"),
             yaxis = list(title = "Test macro-F1 (unpredicted classes = 0)"),
             showlegend = FALSE)
  })

  output$cmp_table <- renderDT({
    datatable(D$ci_table %>% mutate(across(where(is.numeric) & !classes_never_predicted, ~ round(.x, 3))),
              rownames = FALSE, options = list(dom = "t", pageLength = 15))
  })

  output$paired_table <- renderDT({
    datatable(D$paired %>% mutate(across(where(is.numeric), ~ round(.x, 4))),
              rownames = FALSE, options = list(dom = "t", pageLength = 15))
  })

  output$cm_plot <- renderPlotly({
    df <- as.data.frame(table(Truth = D$truth, Predicted = D$preds[[input$model]]),
                        stringsAsFactors = FALSE)
    df <- df %>% group_by(Truth) %>% mutate(Share = ifelse(sum(Freq) == 0, 0, Freq / sum(Freq))) %>% ungroup()
    df$Z <- if (input$norm) df$Share else df$Freq
    plot_ly(df, x = ~Predicted, y = ~Truth, z = ~Z, type = "heatmap",
            colorscale = list(c(0, "#FFFFFF"), c(1, "#00A87E")),
            text = ~paste0("Count: ", Freq, "<br>Share of true class: ", round(Share, 3)),
            hoverinfo = "text") %>%
      layout(title = paste("Confusion matrix -", input$model),
             yaxis = list(autorange = "reversed"))
  })

  output$pc_table <- renderDT({
    datatable(per_class(input$model), rownames = FALSE, options = list(dom = "t", pageLength = 12))
  })

  output$imp_plot <- renderPlotly({
    imp <- D$rf_importance %>% mutate(feature = factor(feature, levels = rev(feature)))
    plot_ly(imp, x = ~importance, y = ~feature, type = "bar", orientation = "h",
            marker = list(color = "#1F3A5F")) %>%
      layout(title = "Random Forest feature importance (top 20)",
             xaxis = list(title = "Importance"), yaxis = list(title = ""))
  })

  observeEvent(input$preset, {
    if (input$preset == "slow") {
      v <- list(dur = 45, spkts = 4, dpkts = 3, sbytes = 120, dbytes = 90, ctd = 55, cts = 48)
    } else {
      t <- D$template
      v <- list(dur = t$dur, spkts = t$spkts, dpkts = t$dpkts, sbytes = t$sbytes,
                dbytes = t$dbytes, ctd = t$ct_dst_ltm, cts = t$ct_srv_dst)
    }
    updateSliderInput(session, "dur",    value = clamp(v$dur, 0.1, 120))
    updateSliderInput(session, "spkts",  value = clamp(round(v$spkts), 1, 60))
    updateSliderInput(session, "dpkts",  value = clamp(round(v$dpkts), 0, 60))
    updateSliderInput(session, "sbytes", value = clamp(v$sbytes, 0, 5000))
    updateSliderInput(session, "dbytes", value = clamp(v$dbytes, 0, 20000))
    updateSliderInput(session, "ct_dst_ltm", value = clamp(round(v$ctd), 1, 60))
    updateSliderInput(session, "ct_srv_dst", value = clamp(round(v$cts), 1, 60))
  })

  probs <- reactive({
    row <- build_row(input$dur, input$spkts, input$dpkts, input$sbytes, input$dbytes,
                     input$ct_dst_ltm, input$ct_srv_dst)
    baked <- bake(recipe_obj, new_data = row)
    p <- predict(rf_model, new_data = baked, type = "prob")
    tibble(class = sub("^\\.pred_", "", names(p)), prob = as.numeric(p[1, ]))
  })

  output$pred_text <- renderText({
    p <- probs()
    top <- p[which.max(p$prob), ]
    paste0("Random Forest prediction: ", top$class, " (", round(100 * top$prob, 1), "%)")
  })

  output$prob_plot <- renderPlotly({
    p <- probs() %>% arrange(prob) %>% mutate(class = factor(class, levels = class))
    plot_ly(p, x = ~prob, y = ~class, type = "bar", orientation = "h",
            marker = list(color = "#00A87E")) %>%
      layout(title = "Class probabilities", xaxis = list(title = "Probability", range = c(0, 1)),
             yaxis = list(title = ""))
  })
}

shinyApp(ui, server)