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
  version = 5, bootswatch = "flatly",
  "body-bg" = pal$bg, "body-color" = pal$ink,
  "link-color" = pal$ink, "primary" = pal$powder_blue
)

gg_theme <- theme_minimal(base_family = "Inter") +
  theme(
    text = element_text(color = pal$ink),
    axis.title = element_text(color = pal$ink),
    axis.text  = element_text(color = pal$ink),
    plot.title = element_text(face = "bold", color = pal$ink),
    panel.grid.minor = element_blank()
  )

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
        h4("Visualisation")
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
  
}

shinyApp(ui, server)