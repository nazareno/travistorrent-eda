library(tidyverse)
library(here)

travis = read_csv("data/travistorrent_8_2_2017.csv")

# Remove jobs idênticos (há muitos)
travis = travis %>% 
    select(-tr_job_id) %>% 
    group_by(gh_project_name) %>% 
    distinct() %>% 
    ungroup()

projetos = travis %>% 
    group_by(gh_project_name) %>% 
    summarize(team = max(gh_team_size), 
              lang = last(gh_lang), 
              sloc = max(gh_sloc), 
              tests_per_kloc = max(gh_test_cases_per_kloc), 
              build_success = sum(build_successful) / n())

projetos %>% 
    write_csv(here("data/projetos.csv"))
