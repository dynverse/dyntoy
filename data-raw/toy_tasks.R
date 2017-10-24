library(dyntoy)

toy_tasks <- generate_toy_datasets(num_replicates = 2)

devtools::use_data(toy_tasks, overwrite = TRUE)


derp <- toy_tasks %>% rowwise() %>% mutate(any_pct_negative = any(milestone_percentages$percentage < 0)) %>% select(id:milestone_percentages, any_pct_negative)
