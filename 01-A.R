
################################################################################
# munge/01-A.R
# Purpose:
#   Build master datasets for engagement + inactivity + re-engagement analysis
#   across multiple FutureLearn course runs.
################################################################################

library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(lubridate)
library(tidyr)

message("Running munge script: 01-A.R")

# -----------------------------------------------------------------------------#
# 1) Locate all CSV files
# -----------------------------------------------------------------------------#

all_csv <- list.files("data", pattern = "\\.csv$", full.names = TRUE)

enrol_files <- all_csv[str_detect(all_csv, "enrolments\\.csv$")]
step_files  <- all_csv[str_detect(all_csv, "step-activity\\.csv$|step_activity\\.csv$")]

if (length(enrol_files) == 0) stop("❌ No enrolments CSV files found in /data/")
if (length(step_files) == 0) stop("❌ No step-activity CSV files found in /data/")

# Extract run number from filename (ex: cyber-security-1_enrolments.csv -> run 1)
extract_run <- function(path) {
    str_extract(basename(path), "\\d+") %>% as.integer()
}

# -----------------------------------------------------------------------------#
# 2) Read + combine enrolments
# -----------------------------------------------------------------------------#

master_enrolments <- map_dfr(enrol_files, function(f) {

    run_num <- extract_run(f)

    read_csv(f, show_col_types = FALSE) %>%
        mutate(
            run = run_num,
            learner_id = as.character(learner_id),
            enrolment_time = ymd_hms(enrolled_at, quiet = TRUE),
            enrolment_date = as.Date(enrolment_time),
            completer = !is.na(fully_participated_at)
        ) %>%
        filter(!is.na(enrolment_time))
})

# -----------------------------------------------------------------------------#
# 3) Read + combine step activity
# -----------------------------------------------------------------------------#

master_step_activity <- map_dfr(step_files, function(f) {

    run_num <- extract_run(f)

    read_csv(f, show_col_types = FALSE) %>%
        mutate(
            run = run_num,
            learner_id = as.character(learner_id),
            activity_time = ymd_hms(first_visited_at, quiet = TRUE),
            activity_date = as.Date(activity_time)
        )
}) %>%
    filter(!is.na(activity_time))

# -----------------------------------------------------------------------------#
# 4) Master joined dataset (only activity AFTER enrolment)
# -----------------------------------------------------------------------------#

master_dataset <- master_step_activity %>%
    inner_join(
        master_enrolments %>% select(run, learner_id, enrolment_date, completer),
        by = c("run", "learner_id")
    ) %>%
    mutate(days_after_enrol = as.integer(activity_date - enrolment_date)) %>%
    filter(!is.na(days_after_enrol), days_after_enrol >= 0)

# -----------------------------------------------------------------------------#
# 5) Cycle 1 dataset: Daily engagement curve
# -----------------------------------------------------------------------------#

master_daily_engagement <- master_dataset %>%
    group_by(run, days_after_enrol) %>%
    summarise(
        active_learners = n_distinct(learner_id),
        total_events = n(),
        .groups = "drop"
    )

# -----------------------------------------------------------------------------#
# 6) DAILY TIMELINE (needed for Cycle 1 inactivity + Cycle 2)
# -----------------------------------------------------------------------------#

daily_active <- master_dataset %>%
    distinct(run, learner_id, activity_date, completer) %>%
    mutate(active = 1)

learner_day_grid <- daily_active %>%
    group_by(run, learner_id, completer) %>%
    summarise(
        start_date = min(activity_date),
        end_date   = max(activity_date),
        .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(activity_date = list(seq.Date(start_date, end_date, by = "day"))) %>%
    unnest(activity_date) %>%
    ungroup()

daily_timeline <- learner_day_grid %>%
    left_join(
        daily_active,
        by = c("run", "learner_id", "completer", "activity_date")
    ) %>%
    mutate(active = ifelse(is.na(active), 0, active)) %>%
    arrange(run, learner_id, activity_date)

master_daily_timeline <- daily_timeline %>%
    group_by(run, learner_id) %>%
    mutate(
        inactive = 1 - active,

        # inactivity streak counter
        inactivity_days = ifelse(
            inactive == 1,
            ave(inactive, cumsum(active == 1), FUN = cumsum),
            0
        ),

        next_day_active = lead(active),
        reengaged_next_day = ifelse(inactive == 1 & next_day_active == 1, 1, 0)
    ) %>%
    ungroup()

# -----------------------------------------------------------------------------#
# 7) CRISP-DM 1: Inactivity onset dataset (≥ 7 consecutive inactive days)
# -----------------------------------------------------------------------------#

cycle1_inactive_7days <- master_daily_timeline %>%
    filter(inactivity_days >= 7) %>%
    group_by(run, learner_id, completer) %>%
    summarise(
        first_inactive_date_7 = min(activity_date),
        .groups = "drop"
    ) %>%
    left_join(
        master_enrolments %>% select(run, learner_id, enrolment_date),
        by = c("run", "learner_id")
    ) %>%
    mutate(
        first_inactive_day_7 = as.integer(first_inactive_date_7 - enrolment_date)
    )

# -----------------------------------------------------------------------------#
# 8) CRISP-DM 2: Re-engagement probability curve
# -----------------------------------------------------------------------------#

cycle2_reengagement_curve <- master_daily_timeline %>%
    filter(inactive == 1) %>%
    mutate(inactivity_days = pmin(inactivity_days, 60)) %>%
    group_by(inactivity_days) %>%
    summarise(
        n = n(),
        reengage_prob = mean(reengaged_next_day, na.rm = TRUE),
        .groups = "drop"
    )

# -----------------------------------------------------------------------------#
# 9) CRISP-DM 2: Re-engagement by completion outcome
# -----------------------------------------------------------------------------#

cycle2_reengagement_by_completion <- master_daily_timeline %>%
    filter(inactive == 1) %>%
    mutate(inactivity_days = pmin(inactivity_days, 60)) %>%
    group_by(completer, inactivity_days) %>%
    summarise(
        n = n(),
        reengage_prob = mean(reengaged_next_day, na.rm = TRUE),
        .groups = "drop"
    )

# -----------------------------------------------------------------------------#
# 10) Cache objects for ProjectTemplate
# -----------------------------------------------------------------------------#

library(ProjectTemplate)

cache("master_enrolments")
cache("master_step_activity")
cache("master_dataset")
cache("master_daily_engagement")
cache("master_daily_timeline")
cache("cycle1_inactive_7days")
cache("cycle2_reengagement_curve")
cache("cycle2_reengagement_by_completion")

message("✅ SUCCESS: munge/01-A.R complete. Objects created & cached.")

