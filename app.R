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



ui <- navbarPage(
  theme = theme_app,
  title = "DATA2x02 Survey Explorer",
  tabPanel("Home", h2("Welcome"), p("Placeholder")),
  tabPanel("Data Visualization", h2("Coming soon")),
  tabPanel("Statistical Tests", h2("Coming soon"))
)

server <- function(input, output, session) {}

shinyApp(ui, server)