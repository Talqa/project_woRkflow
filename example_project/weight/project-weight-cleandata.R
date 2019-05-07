library('tidyverse')
library('googlesheets')

#LOAD RAW DATA------------------------------------------------------------------
#get access to sheets
data_sheet <- gs_title('project_weight')
data <- data_sheet %>% gs_read('Sheet1')

treatment <- gs_title('project_treatment') %>% gs_read('Sheet1')
#END load data---

#save raw data
write_csv(data, 'example_project/weight/rawdata/project-weight-rawdata.csv')
write_csv(treatment, 'example_project/weight/rawdata/project-treatment.csv')

#join data with treatment
data <- left_join(data, treatment)
View(data)

#CLEAN DATA---------------------------------------------------------------------
write_csv(data, 'example_project/weight/cleandata/project-weight-cleandata.csv')



