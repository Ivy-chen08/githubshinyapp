# app.R — DATA2x02 Survey Explorer (Home / Data Visualization / Statistical Tests)

# ---------------- Packages ----------------
library(shiny)
library(bslib)
library(thematic)
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(janitor)
library(ggplot2)
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
# ---------------- Column short names ----------------
new_names <- c(
  "timestamp","target_grade","assignment_preference","trimester_or_semester",
  "age","tendency_yes_or_no","pay_rent","stall_choice","weetbix_count",
  "weekly_food_spend","living_arrangements","weekly_alcohol","believe_in_aliens",
  "height","commute","daily_anxiety_frequency","weekly_study_hours","work_status",
  "social_media","gender","average_daily_sleep","usual_bedtime","sleep_schedule",
  "sibling_count","allergy_count","diet_style","random_number","favourite_number",
  "favourite_letter","drivers_license","relationship_status","daily_short_video_time",
  "computer_os","steak_preference","dominant_hand","enrolled_unit",
  "weekly_exercise_hours","weekly_paid_work_hours","assignments_on_time","used_r_before",
  "team_role_type","university_year","favourite_anime","fluent_languages",
  "readable_languages","country_of_birth","wam","shoe_size","books_read_highschool",
  "daily_water_intake_l","perceived_old_age","study_music_preference"
)

# ---------------- Helpers ----------------
is_numish <- function(v) {
  if (is.numeric(v)) return(TRUE)
  if (!is.character(v)) return(FALSE)
  suppressWarnings(mean(!is.na(as.numeric(v))) >= 0.6)
}

miss_top_tbl <- function(df, k = 10){
  tibble::tibble(
    var = names(df),
    n_miss = vapply(df, function(v) sum(is.na(v)), integer(1)),
    pct_miss = round(100 * vapply(df, function(v) mean(is.na(v)), numeric(1)), 2)
  ) |>
    arrange(desc(pct_miss), desc(n_miss)) |>
    slice_head(n = k)
}

# Robust bedtime parser
parse_bedtime <- function(x) {
  s <- str_squish(as.character(x))
  n <- length(s)
  secs <- rep(NA_real_, n)
  m <- str_match(s, ".*?(\\b\\d{1,2}):(\\d{2})(?::(\\d{2}))?\\b")
  ok <- !is.na(m[,1])
  if (any(ok)) {
    hh <- suppressWarnings(as.numeric(m[ok,2]))
    mm <- suppressWarnings(as.numeric(m[ok,3]))
    ss <- suppressWarnings(as.numeric(ifelse(is.na(m[ok,4]), "0", m[ok,4])))
    secs[ok] <- hh*3600 + mm*60 + ss
  }
  m2 <- str_match(tolower(s), "(\\b\\d{1,2}):(\\d{2})\\s*(am|pm)\\b")
  ok2 <- !is.na(m2[,1])
  if (any(ok2)) {
    hh <- as.numeric(m2[ok2,2]); mm <- as.numeric(m2[ok2,3]); ap <- m2[ok2,4]
    hh <- ifelse(ap=="pm" & hh<12, hh+12, ifelse(ap=="am" & hh==12, 0, hh))
    secs[ok2] <- hh*3600 + mm*60
  }
  out <- rep(as_hms(NA_real_), n)
  out[!is.na(secs)] <- hms::as_hms(secs[!is.na(secs)])
  out
}

fix_noon_shift <- function(hms_vec) {
  is_pm <- !is.na(hms_vec) &
    (hms_vec > hms::as_hms("06:00:00")) &
    (hms_vec < hms::as_hms("14:00:00"))
  res <- hms_vec
  res[is_pm] <- hms::as_hms((as.numeric(res[is_pm]) + 12*3600) %% (24*3600))
  res
}

palette_for_levels <- function(n) {
  colorRampPalette(c(pal$powder_blue, pal$light_blue, pal$misty_rose, pal$slate_gray))(max(n,2))
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
        
        # ---- Plot style for tests (only for t-test) ----
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
        
        # ---- Two-sample t-test ----
        conditionalPanel(
          condition = "input.test_type == 'Two-sample t-test'",
          plotOutput("test_plot", height = "360px")
        ),
        
        # ---- Chi-square: Independence plot + graph ----
        conditionalPanel(
          condition = "input.test_type == 'Chi-square: Independence'",
          fluidRow(
            column(6, plotOutput("chi_stack_plot", height = "320px")),
            column(6, plotOutput("chi_resid_plot", height = "320px")) 
          ),
          br(),
          gt_output("chi_indep_table") 
        ),
        
        # ---- Chi-square: GOF -> graph + chart ----
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

# ---------------- Server ----------------
server <- function(input, output, session){
  
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
  
  # ---------- helpers ----------
  pastel_cols <- c(
    pal$misty_rose, pal$powder_blue, pal$light_blue, pal$slate_gray,
    "thistle2","palevioletred2","slategray2","lightsteelblue1"
  )
  pastel_for <- function(levels_chr) {
    n <- length(levels_chr)
    cols <- rep(pastel_cols, length.out = n)
    stats::setNames(cols, levels_chr)
  }
  
  # ---------- Chi-square: Independence ----------
  chi_indep_data <- reactive({
    req(input$test_type == "Chi-square: Independence")
    req(input$cat_var_a, input$cat_var_b)
    d <- df_tests()
    a <- input$cat_var_a; b <- input$cat_var_b
    dd <- d %>% dplyr::filter(!is.na(.data[[a]]), !is.na(.data[[b]]))
    validate(need(nrow(dd) > 0, "No rows after filtering NAs."))
    
    tab <- table(dd[[a]], dd[[b]])
    validate(need(all(dim(tab) >= 2), "Need at least a 2x2 table."))
    
    suppressWarnings({ chisq <- chisq.test(tab, simulate.p.value = FALSE, correct = (all(dim(tab) == 2))) })
    list(a = a, b = b, dd = dd, tab = tab,
         exp = chisq$expected, stdres = chisq$stdres)
  })
  
  # ---------- Chi-square: GOF ----------
  gof_data <- reactive({
    req(input$test_type == "Chi-square: Goodness of Fit", input$cat_var_gof)
    d <- df_tests()
    g <- input$cat_var_gof
    x <- factor(d[[g]])
    x <- droplevels(x[!is.na(x)])
    validate(need(nlevels(x) >= 2, "This variable needs at least 2 categories."))
    
    obs <- table(x); k <- length(obs)
    chisq <- chisq.test(x = obs, p = rep(1/k, k), rescale.p = TRUE,
                        simulate.p.value = (any(obs < 5) && k > 4))
    list(var = g, levels = names(obs),
         obs = as.numeric(obs),
         exp_cnt = as.numeric(chisq$expected),
         stdres = as.numeric(chisq$stdres))
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
  
  # ---- Hypotheses & Assumptions text ----
  output$hypothesis <- renderPrint({
    a <- input$alpha %||% 0.05
    if (input$test_type == "Two-sample t-test") {
      cat("H0: The population means are equal across the two groups.\n",
          "H1: The population means differ.\n",
          sprintf("Significance level: α = %.3f", a), sep = "")
    } else if (input$test_type == "Chi-square: Goodness of Fit") {
      cat("H0: Observed category proportions match expected (uniform) proportions.\n",
          "H1: Observed category proportions differ from expected.\n",
          sprintf("Significance level: α = %.3f", a), sep = "")
    } else {
      cat("H0: The two categorical variables are independent.\n",
          "H1: The two categorical variables are associated.\n",
          sprintf("Significance level: α = %.3f", a), sep = "")
    }
  })
  
  output$assumptions <- renderPrint({
    if (input$test_type == "Two-sample t-test") {
      cat("- Independence within and between groups",
          "\n- Approximately normal distribution of the numeric variable within each group (or large n)",
          "\n- Homogeneity of variances (Welch t-test is robust if not)")
    } else if (input$test_type == "Chi-square: Goodness of Fit") {
      cat("- Observations are independent\n",
          "\n- Expected counts per category ideally ≥ 5 (using uniform expectation)")
    } else {
      cat("- Observations are independent\n",
          "\n- Expected counts in contingency table cells ideally ≥ 5")
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
  
  # ---- Test visualisations ----
  output$test_plot <- renderPlot({
    req(input$test_type == "Two-sample t-test")
    req(input$num_var, input$group_var)
    
    d    <- df_tests()
    xvar <- input$num_var
    gvar <- input$group_var
    
    dd <- d %>%
      dplyr::filter(!is.na(.data[[xvar]]), !is.na(.data[[gvar]]))
    dd[[xvar]] <- suppressWarnings(as.numeric(dd[[xvar]]))
    dd <- dplyr::filter(dd, !is.na(.data[[xvar]]))
    validate(need(nrow(dd) > 1, "Not enough non-missing observations."))
    
    if (!is.null(input$level_a) && !is.null(input$level_b)) {
      dd <- dplyr::filter(dd, .data[[gvar]] %in% c(input$level_a, input$level_b))
      dd[[gvar]] <- factor(dd[[gvar]], levels = c(input$level_a, input$level_b))
    } else {
      levs <- levels(factor(dd[[gvar]]))
      validate(need(length(levs) >= 2, "The grouping variable needs at least 2 levels."))
      dd <- dplyr::filter(dd, .data[[gvar]] %in% levs[1:2])
      dd[[gvar]] <- factor(dd[[gvar]], levels = levs[1:2])
    }
    validate(need(nrow(dd) > 1, "No rows after filtering to two groups."))
    
    levs2     <- levels(factor(dd[[gvar]]))
    fill_vals <- pastel_for(levs2)
    
    style    <- input$tt_plot_style %||% "violin"
    show_pts <- isTRUE(input$tt_show_points)
    
    #set seed
    jpos <- ggplot2::position_jitter(width = 0.15, height = 0, seed = 2902)
    

    if (style == "density") {
      g <- ggplot(dd, aes(x = .data[[xvar]], fill = .data[[gvar]])) +
        geom_density(alpha = .45, color = pal$slate_gray) +
        scale_fill_manual(values = fill_vals, name = gvar)
    } else if (style == "violin") {
      g <- ggplot(dd, aes(x = .data[[gvar]], y = .data[[xvar]], fill = .data[[gvar]])) +
        geom_violin(alpha = .7, color = pal$slate_gray) +
        geom_boxplot(width = .18, outlier.shape = NA, fill = "white", color = pal$slate_gray)
      if (show_pts) g <- g + geom_jitter(position = jpos, alpha = .35, size = 1, color = "dodgerblue4")
    } else if (style == "box_jitter") {
      g <- ggplot(dd, aes(x = .data[[gvar]], y = .data[[xvar]], fill = .data[[gvar]])) +
        geom_boxplot(
          outlier.shape = NA, width = .35,
          color = pal$ink,
          linewidth = 0.5
        ) +
        { if (show_pts) geom_jitter(width = .15, alpha = .55, size = 1.2, color = pal$ink) else NULL } +
        scale_fill_manual(values = fill_vals, guide = "none") +
        gg_theme +
        labs(x = gvar, y = xvar, title = paste("Two-sample plot:", xvar, "by", gvar)) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
    } else if (style == "mean_ci") {
      g <- ggplot(dd, aes(x = .data[[gvar]], y = .data[[xvar]], fill = .data[[gvar]])) +
        stat_summary(fun = mean, geom = "point", size = 3, color = "#222222") +
        stat_summary(fun.data = ggplot2::mean_cl_normal, geom = "errorbar",
                     width = .15, color = "#222222")
      if (show_pts) g <- g + geom_jitter(position = jpos, alpha = .35, size = 1, color = "#666666")
    }
    
    # 统一的主题与标签
    xlab <- if (style == "density") xvar else gvar
    ylab <- if (style == "density") "Density" else xvar
    
    g +
      scale_fill_manual(values = fill_vals, guide = if (style == "density") "legend" else "none") +
      gg_theme +
      labs(x = xlab, y = ylab,
           title = paste("Two-sample", if (style=="density") "density:" else "plot:", xvar, "by", gvar)) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
    
  })
  output$test_tables <- renderUI({
    if (input$test_type == "Chi-square: Independence") {
      req(input$cat_var_a, input$cat_var_b)
      d  <- df_tests()
      a  <- input$cat_var_a; b <- input$cat_var_b
      dd <- d %>% filter(!is.na(.data[[a]]), !is.na(.data[[b]]))
      tab <- table(dd[[a]], dd[[b]])
      gt_tbl <- gt::gt(as.data.frame.matrix(tab)) %>%
        gt::tab_header(title = "Contingency table (counts)")
      gt::gt_output("indep_gt") 
    } else if (input$test_type == "Chi-square: Goodness of Fit") {
      req(input$cat_var_gof)
      d <- df_tests()
      g <- input$cat_var_gof
      x <- factor(d[[g]])
      x <- droplevels(x[!is.na(x)])
      obs <- as.integer(table(x))
      k   <- length(obs)
      exp <- rep(sum(obs)/k, k)
      gof_df <- tibble::tibble(level = names(table(x)), observed = obs, expected = exp)
      gt::gt_output("gof_gt")
    } else {
      return(NULL)
    }
  })
  

  output$indep_gt <- gt::render_gt({
    req(input$test_type == "Chi-square: Independence", input$cat_var_a, input$cat_var_b)
    d  <- df_tests()
    a  <- input$cat_var_a; b <- input$cat_var_b
    dd <- d %>% filter(!is.na(.data[[a]]), !is.na(.data[[b]]))
    tab <- table(dd[[a]], dd[[b]])
    gt::gt(as.data.frame.matrix(tab)) %>%
      gt::tab_header(title = "Contingency table (counts)")
  })
  
  #GOF
  output$gof_gt <- gt::render_gt({
    req(input$test_type == "Chi-square: Goodness of Fit", input$cat_var_gof)
    d <- df_tests()
    g <- input$cat_var_gof
    x <- factor(d[[g]])
    x <- droplevels(x[!is.na(x)])
    obs <- as.integer(table(x))
    k   <- length(obs)
    exp <- rep(sum(obs)/k, k)
    gof_df <- tibble::tibble(level = names(table(x)), observed = obs, expected = exp)
    gt::gt(gof_df) %>% gt::tab_header(title = "Observed vs Expected (uniform)")
  })
  
  output$resid_plot <- renderPlot({
    req(input$test_type == "Chi-square: Independence", input$show_resid_heatmap)
    req(input$cat_var_a, input$cat_var_b)
    d  <- df_tests()
    a  <- input$cat_var_a; b <- input$cat_var_b
    dd <- d %>% filter(!is.na(.data[[a]]), !is.na(.data[[b]]))
    tab <- table(dd[[a]], dd[[b]])
    validate(need(all(dim(tab) >= 2), "Need at least a 2x2 table."))
    
    ct  <- suppressWarnings(chisq.test(tab))
    rs  <- ct$stdres
    dfp <- as.data.frame(as.table(rs))
    names(dfp) <- c("A","B","StdResid")
    
    ggplot(dfp, aes(x = B, y = A, fill = StdResid)) +
      geom_tile(color = "white") +
      geom_text(aes(label = sprintf("%.2f", StdResid)), color = pal$ink, size = 3) +
      scale_fill_gradient2(low = pal$misty_rose, mid = "white", high = pal$powder_blue) +
      gg_theme + labs(x = b, y = a, title = "Standardized residuals (heatmap)")
  })
  # ---------- Independence ----------
  output$chi_stack_plot <- renderPlot({
    req(input$test_type == "Chi-square: Independence")
    x <- chi_indep_data(); a <- x$a; b <- x$b; dd <- x$dd
    levs_b <- levels(factor(dd[[b]]))
    ggplot(dd, aes(x = .data[[a]], fill = .data[[b]])) +
      geom_bar(position = "fill", color = pal$slate_gray) +
      scale_fill_manual(values = pastel_for(levs_b), name = b) +
      scale_y_continuous(labels = scales::percent) +
      gg_theme + labs(x = a, y = "Proportion", title = paste("Stacked proportions:", a, "by", b)) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })
  
  # ---------- Independence：resid plot ----------
  output$chi_resid_plot <- renderPlot({
    req(input$test_type == "Chi-square: Independence")
    x <- chi_indep_data(); a <- x$a; b <- x$b
    dfh <- as.data.frame(as.table(x$stdres))
    names(dfh) <- c(a, b, "StdResidual")
    ggplot(dfh, aes(x = .data[[b]], y = .data[[a]], fill = StdResidual)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = pal$powder_blue, mid = "white", high = "indianred2") +
      gg_theme + labs(title = "Standardized residuals heatmap", x = b, y = a)
  })
  
  # ---------- Independence：Contigency table ----------
  output$chi_indep_table <- render_gt({
    req(input$test_type == "Chi-square: Independence")
    x <- chi_indep_data(); a <- x$a; b <- x$b
    df <- as.data.frame(x$tab) |>
      dplyr::rename(!!a := Var1, !!b := Var2, Observed = Freq) |>
      dplyr::mutate(Expected = as.vector(x$exp),
                    StdResidual = as.vector(x$stdres))
    gt::gt(df) |>
      gt::fmt_number(columns = c(Expected, StdResidual), decimals = 2) |>
      gt::tab_header(title = "Contingency table with expected & std. residuals")
  })
  
  # ---------- GOF：O vs E ----------
  output$gof_bar_plot <- renderPlot({
    req(input$test_type == "Chi-square: Goodness of Fit")
    gd <- gof_data()
    df <- tibble::tibble(level = factor(gd$levels, levels = gd$levels),
                         Observed = gd$obs, Expected = gd$exp_cnt)
    ggplot(df, aes(x = level)) +
      geom_col(aes(y = Observed), fill = pal$powder_blue, color = pal$slate_gray, width = .6) +
      geom_point(aes(y = Expected), size = 3, color = pal$misty_rose) +
      gg_theme + labs(x = gd$var, y = "Count", title = "GOF: observed vs expected (uniform)")
  })
  
  # ---------- GOF ----------
  output$gof_resid_plot <- renderPlot({
    req(input$test_type == "Chi-square: Goodness of Fit")
    gd <- gof_data()
    df <- tibble::tibble(level = factor(gd$levels, levels = gd$levels),
                         StdResidual = gd$stdres)
    ggplot(df, aes(x = level, y = StdResidual, fill = level)) +
      geom_col(width = .6, color = pal$slate_gray) +
      scale_fill_manual(values = pastel_for(gd$levels), guide = "none") +
      geom_hline(yintercept = 0, linetype = 2, color = pal$slate_gray) +
      gg_theme + labs(x = gd$var, y = "Std. residual", title = "GOF: standardized residuals by category")
  })
  
  # ---------- GOF:（Observed / Expected / StdResidual） ----------
  output$gof_table <- render_gt({
    req(input$test_type == "Chi-square: Goodness of Fit")
    gd <- gof_data()
    df <- tibble::tibble(
      level = factor(gd$levels, levels = gd$levels),
      Observed = gd$obs,
      Expected = gd$exp_cnt,
      StdResidual = gd$stdres
    )
    gt::gt(df) |>
      gt::fmt_number(columns = c(Expected, StdResidual), decimals = 2) |>
      gt::tab_header(title = paste0("Goodness-of-fit for ", gd$var))
  })
}

shinyApp(ui, server)
