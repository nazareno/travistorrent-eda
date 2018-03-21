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
    group_by(gh_project_name) %>% 
    summarize(team = median(gh_team_size), 
              lang = last(gh_lang), 
              sloc_end = max(gh_sloc), 
              sloc_med = median(gh_sloc),
              activity_period = interval(ymd_hms(first(na.omit(gh_first_commit_created_at))), 
                                         ymd_hms(last(na.omit(gh_first_commit_created_at)))) %/% months(1), # sintaxe lubridate
              num_commits = sum(gh_num_commits_in_push),
              commits_per_month = num_commits / activity_period,
              tests_per_kloc = max(gh_test_cases_per_kloc), 
              build_success = sum(build_successful) / n())

projetos %>% 
    write_csv(here::here("data/projetos.csv"))
