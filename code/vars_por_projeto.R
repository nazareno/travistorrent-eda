library(tidyverse)
library(lubridate)
library(here)

travis = read_csv("data/travistorrent_8_2_2017.csv")

# Remove jobs idênticos no mesmo build
travis = travis %>% 
    select(-tr_job_id) %>% 
    group_by(gh_project_name) %>% 
    distinct() %>% 
    ungroup()

projetos = travis %>% 
    mutate(commit_date = ymd_hms(gh_first_commit_created_at)) %>% 
    group_by(gh_project_name) %>% 
    arrange(commit_date) %>% 
    summarize(team = median(gh_team_size), 
              lang = last(gh_lang), 
              sloc_end = max(gh_sloc), 
              sloc_med = median(gh_sloc),
              activity_period = max(1, interval(first(na.omit(commit_date)), 
                                         last(na.omit(commit_date))) %/% months(1)),  # sintaxe lubridate
              num_commits = sum(as.numeric(gh_num_commits_in_push), na.rm = TRUE),
              commits_per_month = num_commits / activity_period,
              tests_per_kloc = max(gh_test_cases_per_kloc), 
              total_builds = n(), 
              build_success_prop = sum(build_successful) / n(), 
              builds_per_month = n() / activity_period, 
              tests_added_per_build = sum(gh_diff_tests_added) / sloc_end, 
              tests_successful = mean(tr_log_num_tests_ok / tr_log_num_tests_run, na.rm = TRUE), 
              test_density = median(gh_test_cases_per_kloc), 
              test_size_avg = mean(gh_test_lines_per_kloc / max(gh_test_cases_per_kloc, 1), na.rm = TRUE))

projetos %>% 
    filter(num_commits > 0) %>% 
    write_csv(here::here("data/projetos.csv"))
