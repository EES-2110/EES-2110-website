library(pacman)
p_load(RSQLite)
p_load(tidyverse, magrittr, lubridate, rlang)
p_load(janitor)
p_load(here)

database <- "new_EES_3310_5310.sqlite3"
new_database <- "alt_EES_3310_5310.sqlite"

root_dir <- here::here()
planning_dir <- here::here("planning")

prefixes <- c(lab = "LAB", class = "CLS", homework = "HW", due_date = "DUE",
              exam = "EXAM", holiday = "VAC", event = "EVT" )
bases <- c(class = 1000, lab = 2000, homework = 3000, due_date = 4000,
           exam = 5000, holiday = 6000, event = 7000)
base_mods <- c(cancelled = 100, make_up = 200)

semester_db <- dbConnect(SQLite(), file.path(planning_dir, database))

cal <- tbl(semester_db, "calendar") %>% collect()
topics <- tbl(semester_db, "notices") %>%
  select(notice_id, topic_key = topic_id, topic) %>% collect()

reading_groups <- tbl(semester_db, "link_reading_group") %>% collect()
reading_links <- tbl(semester_db, "link_class_reading") %>% collect()

class_links <- tbl(semester_db, "link_cal_class") %>% collect()

lab_groups <- semester_db %>% tbl("lab_groups") %>% collect()
lab_links <- tbl(semester_db, "link_cal_lab") %>% collect()

events <- tbl(semester_db, "events") %>% collect()
event_links <- tbl(semester_db, "link_cal_event") %>% collect()

exams <- tbl(semester_db, "exams") %>% collect()
exam_links <- tbl(semester_db, "link_cal_exam") %>% collect()

holidays <- tbl(semester_db, "holidays") %>% collect()
holiday_links <- tbl(semester_db, "link_cal_holiday") %>% collect()

dbDisconnect(semester_db)

cal <- cal %>%
  mutate(cal_type = c("class", "lab", "exam", "holiday", "due_date",
                      "study_session", "event")[cal_id %/% 1000],
         cancelled = cal_id %/% 100 %% 10 == 2,
         make_up = cal_id %/% 100 %% 10 == 1)

topics <- topics %>%
  mutate(topic_key = str_replace(topic_key, "MIDTERM_TEST", "MIDTERM_EXAM"))

class_topics <- class_links %>% select(cal_id, class_id, rd_id) %>%
  filter(!is.na(class_id)) %>% full_join(reading_groups, by = "rd_id")

cal <- cal %>% left_join(select(class_topics, cal_id, class_key = rd_group),
                          by = "cal_id")

lab_links <- lab_links %>%
  left_join(lab_groups, by = "lab_id") %>%
  mutate(topic = str_replace(lab_group, "_LAB$", ""))

cal <- cal %>%
  left_join(select(lab_links, cal_id, lab_key = topic), by = "cal_id")

events <- events %>% full_join(event_links, by = "event_id") %>%
  mutate(event_key = map_chr(event, ~make_clean_names(.x, case = "all_caps")) %>%
           str_replace_all("MIDTERM_DEFICIENCIES_DUE", "MIDTERM_DEFICIENCIES"))

cal <- cal %>% left_join(select(events, cal_id, event_key),
                           by = "cal_id")

holidays <- holidays %>%
  full_join(holiday_links, by = "holiday_id") %>%
  mutate(holiday_key = holiday_name %>%
           map_chr(~make_clean_names(.x, case = "all_caps")) %>%
           str_replace_all("^MARTIN_.*_DAY$", "MLK_DAY"))

cal <- cal %>% full_join(select(holidays, cal_id, holiday_key), by = "cal_id")

exams <- exams %>% full_join(exam_links, by = "exam_id")

cal <- cal %>% full_join(select(exams, cal_id, exam_key), by = "cal_id")

alt_db <- dbConnect(SQLite(), file.path(planning_dir, new_database))


copy_to(alt_db, cal, name = "master_calendar",
        overwrite = TRUE, temporary = FALSE,
        unique_indexes = list("cal_id"),
        indexes = list("date", "class_key", "lab_key", "event_key",
                       "holiday_key", "exam_key"))

cal <- cal %>% mutate(index = seq(n()))

do_index <- function(df, key_col, type = NULL) {
  if (missing(key_col)) {
    key_col <- str_c(type, "_key")
  }
  qcol <- enquo(key_col)
  # message("enquosed")
  # print(qcol)
  if (is_character(quo_get_expr(qcol))) {
    # message("character")
    key <- key_col
    key_col <- ensym(key_col)
    qcol <- enquo(key_col)
  } else {
    # message("not character")
    key <- as.character(quo_get_expr(qcol))
  }
  key <- str_replace(key, "_key$", "")
  # message("key = ", key)
  if (is.null(type)) {
    type <- key
  }
  num_col_name <- str_c(key, "_num")
  num_col <- ensym(num_col_name)
  qnum <- enquo(num_col)
  df <- df %>% select(cal_id, date, !!qcol, cancelled, make_up) %>%
  arrange(date) %>%
    filter(!is.na(!!qcol)) %>%
    mutate(cal_type = type,
           !!num_col := seq_along(date),
           cal_id = bases[type] + !!num_col +
             base_mods['cancelled'] * cancelled +
             base_mods['make_up'] * make_up
    )
  invisible(df)
}

reindex <- function(df, key_col, idx_col = NULL) {
  qkcol <- enquo(key_col)
  if (is_character(quo_get_expr(qkcol))) {
    key = key_col
    key_col <- ensym(key_col)
    qkcol <- enquo(key_col)
  } else {
    key = as.character(quo_get_expr(qkcol))
  }
  key_base <- key %>% str_replace("_key$", "")

  if (is.null(idx_col)) {
    idx_col <- str_c(key_base, "_num")
  }
  qicol <- enquo(idx_col)

  if (is_character(quo_get_expr(qicol))) {
    idx_col <- ensym(idx_col)
    qicol <- enquo(idx_col)
  }
  dfi <- df %>% arrange(!!qicol) %>%
    select(!!qkcol) %>% distinct() %>%
    mutate(!!qicol := seq_along(!!qkcol))
  dfx <- df %>% select(-!!qicol) %>%
    left_join(dfi, by = key)
  invisible(dfx)
}

cal_prepare <- function(df, key_col, type = NULL) {
  if (missing(key_col)) {
    key_col <- str_c(type, "_key")
  }
  qcol <- enquo(key_col)
  # message("enquosed")
  # print(qcol)
  if (is_character(quo_get_expr(qcol))) {
    # message("character")
    key <- key_col
    key_col <- ensym(key_col)
    qcol <- enquo(key_col)
  } else {
    # message("not character")
    key <- as.character(quo_get_expr(qcol))
  }
  key <- str_replace(key, "_key$", "")
  # message("key = ", key)
  if (is.null(type)) {
    type <- key
  }
  prefix <- prefixes[type]

  df <- df %>% mutate(topic_key = str_c(prefix, !!qcol, sep = "_")) %>%
    select(-!!qcol)
  invisible(df)
}

classes <- cal %>% do_index(class_key) %>% reindex(class_key) %>%
  mutate(week = class_num %/% 3 + 1)

labs <- cal %>% do_index(lab_key) %>% reindex(lab_key)

exams <- cal %>% do_index(exam_key) %>% reindex(exam_key)

#
# homework <- cal %>% do_index(hw_key, "homework") %>% reindex(event_key)
#
# due_dates <- cal %>% do_index(due_key, "due_date") %>% reindex(due_key)
#

holidays <- cal %>% do_index(holiday_key) %>% reindex(holiday_key)

events <- cal %>% do_index(event_key) %>% reindex(event_key)

calendar <- bind_rows(
  cal_prepare(classes, class_key),
  cal_prepare(labs, lab_key),
  cal_prepare(exams, exam_key),
  # cal_prepare(homework, hw_key, "homework"),
  # cal_prepare(due_dates, due_key, "due_date"),
  cal_prepare(holidays, holiday_key),
  cal_prepare(events, event_key)
) %>% arrange(date, cal_id) %>%
  select(cal_id, date, class_num, week, topic_key, cal_type, cancelled,
         make_up, everything())

copy_to(alt_db, calendar, overwrite = TRUE, temporary = FALSE,
        unique_indexes = list("cal_id"),
        indexes = list("date", "topic_key"))

dbDisconnect(alt_db)
