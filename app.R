library(shiny)
library(bslib)
library(thematic)
library(ggplot2)
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(janitor)
library(gt)
library(visdat)
library(hms)
library(rlang)

# ---------------- Theme / Palette ----------------
pal <- list(
  misty_rose = "#FFE4E1",
  powder_blue = "#B0E0E6",
  light_blue  = "#ADD8E6",
  slate_gray  = "#708090",
  bg          = "#F7F7F7",
  ink         = "#0B132B"
)

thematic::thematic_on(fg = pal$ink, bg = pal$bg, accent = pal$slate_gray)

theme_app <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  "body-bg" = pal$bg,
  "body-color" = pal$ink,
  "link-color" = pal$ink,
  "primary" = pal$powder_blue
)

gg_theme <- theme_minimal(base_family = "Inter") +
  theme(
    text = element_text(color = pal$ink),
    axis.title = element_text(color = pal$ink),
    axis.text  = element_text(color = pal$ink),
    plot.title = element_text(face = "bold", color = pal$ink),
    panel.grid.minor = element_blank()
  )

pastel_base <- c(
  pal$misty_rose,
  pal$powder_blue,
  pal$light_blue,
  pal$slate_gray,
  "thistle2",
  "palevioletred2",
  "slategray2",
  "lightsteelblue1"
)

palette_for_levels <- function(n) {
  colorRampPalette(pastel_base)(max(n, length(pastel_base)))
}

pastel_for <- function(levels_chr) {
  n <- length(levels_chr)
  cols <- palette_for_levels(n)[seq_len(n)]
  stats::setNames(cols, levels_chr)
}

# ---------------- Helpers ----------------
is_numish <- function(v) {
  if (is.numeric(v)) return(TRUE)
  if (!is.character(v)) return(FALSE)
  suppressWarnings(mean(!is.na(as.numeric(v))) >= 0.6)
}

miss_top_tbl <- function(df, k = 10) {
  tibble::tibble(
    var = names(df),
    n_miss = vapply(df, function(v) sum(is.na(v)), integer(1)),
    pct_miss = round(100 * vapply(df, function(v) mean(is.na(v)), numeric(1)), 2)
  ) |>
    arrange(desc(pct_miss), desc(n_miss)) |>
    slice_head(n = k)
}

# ---------------- UI ----------------
ui <- navbarPage(
  theme = theme_app,
  title = "DATA2x02 Survey Explorer",
  
  # --- Home ---
  tabPanel(
    "Home",
    fluidPage(
      h2("Welcome to DATA2x02 Survey Explorer"),
      p("Explore the cleaned survey data, visualize distributions and missingness, and run interactive hypothesis tests."),
      tags$ul(
        tags$li(strong("Data Visualization:"), " Missingness (table + matrix), One-variable distribution, Bedtime demo"),
        tags$li(strong("Statistical Tests:"), " Two-sample t-test, Chi-square (Goodness of Fit / Independence) with auto H0/H1 & assumptions")
      ),
      p("Tip: Upload your dataset (.xlsx/.csv) or use the bundled 'cleaned_survey.xlsx' in the working directory.")
    )
  ),
  
  # --- Data Visualization ---
  tabPanel(
    "Data Visualization",
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Upload .xlsx / .csv", accept = c(".xlsx",".csv")),
        checkboxInput("use_demo", "Use bundled cleaned_survey.xlsx", value = TRUE),
        hr(),
        uiOutput("one_var_picker")
      ),
      mainPanel(
        fluidRow(
          column(6, h4("Missingness (Top 10)"), gt_output("miss_tbl")),
          column(6, h4("Missingness Matrix"), plotOutput("miss_plot", height = "300px"))
        ),
        hr(),
        fluidRow(
          column(
            width = 6,
            h4("One-variable Distribution"),
            plotOutput("one_var_plot", height = "300px")
          ),
          column(
            width = 6,
            h4("Bedtime (clean demo)"),
            plotOutput("bedtime_plot", height = "300px")
          )
        )
      )
    )
  ),
  
  # --- Statistical Tests ---
  tabPanel(
    "Statistical Tests",
    sidebarLayout(
      sidebarPanel(
        fileInput("file_tests", "Upload .xlsx / .csv (optional)", accept = c(".xlsx",".csv")),
        checkboxInput("use_demo_tests", "Use bundled cleaned_survey.xlsx", value = TRUE),
        hr(),
        selectInput(
          "test_type", "Choose a test:",
          choices = c("Two-sample t-test", "Chi-square: Goodness of Fit", "Chi-square: Independence")
        ),
        numericInput("alpha", "Significance level (α)", value = 0.05, min = 0.001, max = 0.2, step = 0.01),
        hr(),
        
        # dynamic var
        uiOutput("test_var_inputs"),
        uiOutput("ui_pick_two_levels"),
        uiOutput("ui_ref_group"),
        uiOutput("ui_alt_hypothesis"),
        
        # t-test plot style
        conditionalPanel(
          condition = "input.test_type == 'Two-sample t-test'",
          selectInput(
            "tt_plot_style", "Plot style (two-sample):",
            choices = c("Violin + box" = "violin",
                        "Boxplot + jitter" = "box_jitter",
                        "Mean ± 95% CI" = "mean_ci",
                        "Overlapped density" = "density"),
            selected = "violin"
          ),
          checkboxInput("tt_show_points", "Show raw points (where applicable)", value = FALSE)
        ),
        
        hr(),
        uiOutput("assumption_hint")
      ),
      mainPanel(
        h4("Hypotheses"),
        verbatimTextOutput("hypothesis"),
        h4("Assumptions"),
        verbatimTextOutput("assumptions"),
        h4("Test Result"),
        verbatimTextOutput("test_result"),
        h4("Visualisation"),
        
        # t-test
        conditionalPanel(
          condition = "input.test_type == 'Two-sample t-test'",
          plotOutput("test_plot", height = "360px")
        ),
        
        # Chi-square: Independence
        conditionalPanel(
          condition = "input.test_type == 'Chi-square: Independence'",
          fluidRow(
            column(6, plotOutput("chi_stack_plot", height = "320px")),
            column(6, plotOutput("chi_resid_plot", height = "320px"))
          ),
          br(),
          gt_output("chi_indep_table")
        ),
        
        # Chi-square: GOF
        conditionalPanel(
          condition = "input.test_type == 'Chi-square: Goodness of Fit'",
          fluidRow(
            column(6, plotOutput("gof_bar_plot", height = "320px")),
            column(6, plotOutput("gof_resid_plot", height = "320px"))
          ),
          br(),
          gt_output("gof_table")
        )
      )
    )
  )
) 


server <- function(input, output, session) {
  # ---- Data for Visualization tab ----
  raw_df_viz <- reactive({
    if (!is.null(input$file)) {
      ext <- tolower(tools::file_ext(input$file$name))
      if (ext == "xlsx") read_excel(input$file$datapath)
      else if (ext == "csv") readr::read_csv(input$file$datapath, show_col_types = FALSE)
      else validate(need(FALSE, "Please upload .xlsx or .csv"))
    } else if (isTRUE(input$use_demo)) {
      if (file.exists("cleaned_survey.xlsx")) read_excel("cleaned_survey.xlsx")
      else validate(need(FALSE, "Demo .xlsx not found in working directory."))
    } else {
      validate(need(FALSE, "Please upload a dataset."))
    }
  })
  
  df_viz <- reactive({
    d <- raw_df_viz() |> janitor::clean_names()
    d <- d[, seq_len(min(ncol(d), length(new_names)))]
    names(d) <- new_names[seq_len(ncol(d))]
    d
  })
  
  output$one_var_picker <- renderUI({
    selectInput("onevar", "Pick a variable:", choices = names(df_viz()), selected = "wam")
  })
  
  output$miss_tbl <- render_gt({
    miss_top_tbl(df_viz(), 10) |>
      gt() |>
      fmt_number(columns = "pct_miss", decimals = 2) |>
      tab_style(style = cell_fill(color = pal$misty_rose),
                locations = cells_body(columns = "pct_miss")) |>
      cols_label(var = "variable", n_miss = "n_miss", pct_miss = "pct_miss")
  })
  
  output$miss_plot <- renderPlot({
    visdat::vis_miss(df_viz()) + gg_theme +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$one_var_plot <- renderPlot({
    d <- df_viz(); v <- input$onevar; req(v)
    x <- d[[v]]
    g <- ggplot()
    if (is_numish(x)) {
      xx <- suppressWarnings(as.numeric(x))
      g + geom_histogram(aes(x = xx), bins = 30,
                         fill = pal$powder_blue, color = pal$slate_gray) +
        gg_theme + labs(x = v, y = "Count", title = paste("Distribution of", v))
    } else {
      tab <- d |> count(!!sym(v), name = "n")
      g + geom_col(aes(x = reorder(!!sym(v), n), y = n), data = tab,
                   fill = pal$light_blue, color = pal$slate_gray) +
        coord_flip() + gg_theme + labs(x = v, y = "Count", title = paste("Distribution of", v))
    }
  })
  
  output$bedtime_plot <- renderPlot({
    d <- df_viz()
    req("usual_bedtime" %in% names(d))
    bt <- parse_bedtime(d$usual_bedtime) |> fix_noon_shift()
    validate(need(sum(!is.na(bt)) > 0, "No parseable times found"))
    tibble(bt = bt) |>
      ggplot(aes(x = bt)) +
      geom_histogram(binwidth = 3600, fill = pal$powder_blue, color = pal$slate_gray,
                     boundary = 0, closed = "right") +
      coord_polar() + gg_theme +
      labs(x = NULL, y = NULL, title = "Usual Bedtime (clean demo)")
  })
  
  
  # ---- Data for Tests tab ----
  raw_df_tests <- reactive({
    if (!is.null(input$file_tests)) {
      ext <- tolower(tools::file_ext(input$file_tests$name))
      if (ext == "xlsx") read_excel(input$file_tests$datapath)
      else if (ext == "csv") readr::read_csv(input$file_tests$datapath, show_col_types = FALSE)
      else validate(need(FALSE, "Please upload .xlsx or .csv"))
    } else if (isTRUE(input$use_demo_tests)) {
      if (file.exists("cleaned_survey.xlsx")) read_excel("cleaned_survey.xlsx")
      else validate(need(FALSE, "Demo .xlsx not found in working directory."))
    } else {
      validate(need(FALSE, "Please upload a dataset."))
    }
  })
  
  df_tests <- reactive({
    d <- raw_df_tests() |> janitor::clean_names()
    d <- d[, seq_len(min(ncol(d), length(new_names)))]
    names(d) <- new_names[seq_len(ncol(d))]
    d
  })
  
  # ---- Dynamic inputs (Tests tab) ----
  output$test_var_inputs <- renderUI({
    req(input$test_type)
    d <- df_tests()
    num_vars <- names(d)[vapply(d, is_numish, logical(1))]
    cat_vars <- setdiff(names(d), num_vars)
    
    if (input$test_type == "Two-sample t-test") {
      tagList(
        selectInput("num_var",  "Numeric variable", choices = num_vars, selected = "wam"),
        selectInput("group_var","Grouping (categorical)", choices = cat_vars, selected = "gender"),
        uiOutput("ui_pick_two_levels")
      )
    } else if (input$test_type == "Chi-square: Independence") {
      tagList(
        selectInput("cat_var_a","Categorical A", choices = cat_vars, selected = "gender"),
        selectInput("cat_var_b","Categorical B", choices = cat_vars, selected = "assignment_preference")
      )
    } else { # GOF
      tagList(
        selectInput("cat_var_gof","Categorical variable", choices = cat_vars, selected = "assignment_preference")
      )
    }
  })
  
  output$ui_pick_two_levels <- renderUI({
    req(input$test_type == "Two-sample t-test", input$group_var)
    d <- df_tests()
    levs <- levels(factor(d[[input$group_var]]))
    if (length(levs) < 2) return(helpText("This grouping variable has < 2 levels in data."))
    tagList(
      selectInput("level_a", "Choose level A", choices = levs, selected = levs[1]),
      selectInput("level_b", "Choose level B", choices = levs, selected = levs[min(2, length(levs))])
    )
  })
  
  output$ui_alt_hypothesis <- renderUI({
    if (input$test_type != "Two-sample t-test") return(NULL)
    selectInput("alt", "Alternative hypothesis",
                choices = c("Two-sided" = "two.sided",
                            "Greater (A > B)" = "greater",
                            "Less (A < B)" = "less"),
                selected = "two.sided")
  })
  
  output$ui_ref_group <- renderUI({
    if (input$test_type != "Two-sample t-test") return(NULL)
    req(input$group_var)
    d <- df_tests()
    g <- input$group_var
    levs <- sort(unique(d[[g]]))
    if (length(levs) >= 2) {
      tagList(
        helpText("Pick which level is Group A and which is Group B (affects 'greater/less')."),
        selectInput("level_a", "Group A (first level)", choices = levs),
        selectInput("level_b", "Group B (second level)", choices = levs, selected = levs[min(2,length(levs))])
      )
    }
  })
  
  output$assumption_hint <- renderUI({
    req(input$test_type)
    if (input$test_type == "Two-sample t-test") {
      HTML("<em>Requirements:</em> one numeric + one categorical (choose exactly two levels). We pick Welch or Student t automatically; you can choose one/two-sided.")
    } else if (input$test_type == "Chi-square: Independence") {
      HTML("<em>Requirements:</em> two categorical variables. We pick Pearson / Fisher / Monte Carlo automatically based on expected counts.")
    } else {
      HTML("<em>Requirements:</em> one categorical variable compared to a uniform (or supplied) distribution.")
    }
  })
  
  
  # ---- Test result ----
  output$test_result <- renderPrint({
    req(input$test_type)
    
    if (input$test_type == "Two-sample t-test") {
      req(input$num_var, input$group_var)
      d    <- df_tests()
      xvar <- input$num_var
      gvar <- input$group_var
      
      xv <- suppressWarnings(as.numeric(d[[xvar]]))
      gv <- factor(d[[gvar]])
      
      levs <- levels(gv)
      validate(need(length(levs) >= 2, "Grouping variable needs at least 2 levels."))
      
      if (!is.null(input$level_a) && !is.null(input$level_b)) {
        keep <- gv %in% c(input$level_a, input$level_b)
        xv   <- xv[keep]
        gv   <- droplevels(factor(gv[keep], levels = c(input$level_a, input$level_b)))
        lvlA <- input$level_a; lvlB <- input$level_b
      } else {
        keep <- gv %in% levs[1:2]
        xv   <- xv[keep]
        gv   <- droplevels(factor(gv[keep], levels = levs[1:2]))
        lvlA <- levels(gv)[1]; lvlB <- levels(gv)[2]
      }
      
      ok <- !is.na(xv) & !is.na(gv)
      xv <- xv[ok]; gv <- droplevels(gv[ok])
      
      validate(need(length(unique(gv)) == 2, "Still need two non-empty groups after NA removal."))
      validate(need(sum(gv == lvlA) >= 2 && sum(gv == lvlB) >= 2, "Each group needs at least 2 observations."))
      
      y1 <- xv[gv == lvlA]; y2 <- xv[gv == lvlB]
      alt   <- if (is.null(input$alt)) "two.sided" else input$alt
      alpha <- if (is.null(input$alpha)) 0.05 else input$alpha
      
      # --- Group summaries (n, mean, sd) ---
      n1 <- length(y1); n2 <- length(y2)
      m1 <- mean(y1, na.rm = TRUE); m2 <- mean(y2, na.rm = TRUE)
      s1 <- sd(y1, na.rm = TRUE);   s2 <- sd(y2, na.rm = TRUE)
      
      # Optional: Cohen's d (pooled SD), only if both groups have >=2 obs
      d_eff <- NA_real_
      if (n1 >= 2 && n2 >= 2) {
        sp <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
        if (sp > 0) d_eff <- (m1 - m2) / sp
      }
      
      v.equal <- FALSE
      reason  <- "Default to Welch t-test (variance/size may differ)."
      vt <- tryCatch(var.test(y1, y2), error = function(e) NULL)
      if (!is.null(vt)) {
        if (vt$p.value > alpha) {
          v.equal <- TRUE
          reason  <- sprintf("Student t-test (equal variances), var.test p=%.4f > α=%.3f", vt$p.value, alpha)
        } else {
          v.equal <- FALSE
          reason  <- sprintf("Welch t-test (unequal variances), var.test p=%.4f ≤ α=%.3f", vt$p.value, alpha)
        }
      }
      
      tt <- t.test(y1, y2, alternative = alt, var.equal = v.equal)
      
      h0 <- sprintf("H0: mean(%s in %s) = mean(%s in %s)", xvar, lvlA, xvar, lvlB)
      h1 <- switch(
        alt,
        "two.sided" = sprintf("H1: mean(%s in %s) ≠ mean(%s in %s)", xvar, lvlA, xvar, lvlB),
        "greater"   = sprintf("H1: mean(%s in %s) > mean(%s in %s)", xvar, lvlA, xvar, lvlB),
        "less"      = sprintf("H1: mean(%s in %s) < mean(%s in %s)", xvar, lvlA, xvar, lvlB)
      )
      decision <- ifelse(tt$p.value < alpha, "Reject H0", "Fail to reject H0")
      
      cat("Two-sample t-test\n")
      cat("Numeric:", xvar, " | Group:", gvar, "\n")
      cat("Groups: A =", lvlA, " | B =", lvlB, "\n")
      cat("Alternative:", alt, "\n")
      cat("Choice:", reason, "\n\n")
      cat(h0, "\n", h1, "\n\n", sep = "")
      cat(sprintf("Group summaries: %s  n=%d  mean=%.2f  SD=%.2f   |   %s  n=%d  mean=%.2f  SD=%.2f\n",
                  lvlA, n1, m1, s1, lvlB, n2, m2, s2))
      if (!is.na(d_eff)) cat(sprintf("Effect size (Cohen's d based on pooled SD): %.3f\n", d_eff))
      cat(sprintf("t = %.3f, df = %.2f, p-value = %.4f\n", tt$statistic, tt$parameter, tt$p.value))
      cat(sprintf("alpha = %.3f  ->  Decision: %s\n", alpha, decision))
      return(invisible(NULL))
    }
    
    if (input$test_type == "Chi-square: Independence") {
      req(input$cat_var_a, input$cat_var_b)
      d  <- df_tests()
      a  <- input$cat_var_a
      b  <- input$cat_var_b
      dd <- d |> dplyr::filter(!is.na(.data[[a]]), !is.na(.data[[b]]))
      tab <- table(dd[[a]], dd[[b]])
      validate(need(all(dim(tab) >= 2), "Need at least a 2x2 table."))
      
      tmp  <- suppressWarnings(chisq.test(tab, simulate.p.value = FALSE, correct = (all(dim(tab) == 2))))
      expc <- tmp$expected
      min_exp <- min(expc)
      
      if (all(dim(tab) == 2) && min_exp < 5) {
        res <- fisher.test(tab)
        cat("Test chosen: Fisher's Exact (2x2 with small expected counts)\n\n")
        print(res)
      } else if (min_exp < 5) {
        res <- suppressWarnings(chisq.test(tab, simulate.p.value = TRUE, B = 2000))
        cat("Test chosen: Chi-square with Monte Carlo p-value (sparse table)\n")
        cat(sprintf("Min expected cell = %.2f\n\n", min_exp))
        print(res)
      } else {
        res <- chisq.test(tab, correct = (all(dim(tab) == 2)))
        cat("Test chosen: Pearson's Chi-square\n\n")
        print(res)
      }
      return(invisible(NULL))
    }
    
    if (input$test_type == "Chi-square: Goodness of Fit") {
      req(input$cat_var_gof)
      d <- df_tests()
      g <- input$cat_var_gof
      x <- factor(d[[g]])
      x <- droplevels(x[!is.na(x)])
      validate(need(nlevels(x) >= 2, "This variable needs at least 2 categories."))
      
      obs <- table(x)
      k   <- length(obs)
      exp <- rep(1 / k, k)
      res <- chisq.test(x = obs, p = exp, rescale.p = TRUE, simulate.p.value = (any(obs < 5) && k > 4))
      
      cat("Chi-square: Goodness of Fit (expected = uniform)\n\n")
      print(res)
      return(invisible(NULL))
    }
  })
  
  
  
}

shinyApp(ui, server)