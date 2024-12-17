library(broom)
library(readxl)
library(tidyverse)
library(janitor)
library(gridExtra)

critical_value <- qt(0.975, df = 198)

#Construction-------------------------------------------------------------------

turkey_construction_market_model <- read_excel("C:/Users/hanna/OneDrive/Documentos/MNB mesterszak/3. félév/Adatalapú eseményelemzés/turkey_datastream.xlsx", sheet = "construction") %>% 
  clean_names() %>% 
  mutate(
    bist_national_100_adjusted_price = as.numeric(bist_national_100_adjusted_price)
  ) %>% 
  mutate(across(contains("adjusted"), ~ log(. / lead(.)), .names = "ln_return_{col}")) %>% 
  select(date, counter, contains("ln_return")) %>% 
  pivot_longer(3:15, names_to = "company", values_to = "return") %>% 
  group_by(company) %>%
  do({
    # Fit the model only for the estimation window (counter between -203 and -1)
    model <- lm(return ~ ln_return_bist_national_100_adjusted_price, data = filter(., counter > -203 & counter < -1))
    
    # Predict returns for all days (both estimation and event window)
    predicted_returns <- predict(model, newdata = .)
    
    # Add the predicted returns as a new column
    cbind(., predicted_return = predicted_returns)
  }) %>%
  ungroup() %>% 
  mutate(abnormal_return = return - predicted_return)

sd_abnormal_return_construction <- sd(turkey_construction_market_model$abnormal_return, na.rm = TRUE)
  
turkey_construction_significance <- turkey_construction_market_model %>% 
  filter(counter >= 0 & counter <= 7) %>% 
  mutate(
    t_statistic_abnormal = abnormal_return / sd_abnormal_return_construction
  ) %>% 
  group_by(company) %>%
  summarise(CAR = sum(abnormal_return, na.rm = TRUE)) %>%
  mutate(
    car_t_stat = CAR / (sd_abnormal_return_construction * sqrt(200)))

caar_construction <- mean(turkey_construction_significance$CAR)
caar_construction_t_stat <- caar_construction / (sd(turkey_construction_significance$CAR, na.rm = TRUE) / sqrt(nrow(turkey_construction_significance)))


#Insurance-------------------------------------------------------------------

turkey_insurance_market_model <- read_excel("C:/Users/hanna/OneDrive/Documentos/MNB mesterszak/3. félév/Adatalapú eseményelemzés/turkey_datastream.xlsx", sheet = "insurance") %>% 
  clean_names() %>% 
  mutate(
    bist_national_100_adjusted_price = as.numeric(bist_national_100_adjusted_price)
  ) %>% 
  mutate(across(contains("adjusted"), ~ log(. / lead(.)), .names = "ln_return_{col}")) %>% 
  select(date, counter, contains("ln_return")) %>% 
  pivot_longer(3:8, names_to = "company", values_to = "return") %>% 
  group_by(company) %>%
  do({
    # Fit the model only for the estimation window (counter between -203 and -1)
    model <- lm(return ~ ln_return_bist_national_100_adjusted_price, data = filter(., counter > -203 & counter < -1))
    
    # Predict returns for all days (both estimation and event window)
    predicted_returns <- predict(model, newdata = .)
    
    # Add the predicted returns as a new column
    cbind(., predicted_return = predicted_returns)
  }) %>%
  mutate(
    abnormal_return = return - predicted_return
    )

sd_abnormal_return_insurance <- sd(turkey_insurance_market_model$abnormal_return, na.rm = TRUE)

turkey_insurance_significance <- turkey_insurance_market_model %>% 
  filter(counter >= 0 & counter <= 7) %>% 
  mutate(
    t_statistic_abnormal = abnormal_return / first(sd_abnormal_return_insurance)
  ) %>% 
  group_by(company) %>%
  summarise(CAR = sum(abnormal_return, na.rm = TRUE)) %>% 
    mutate(
  car_t_stat = CAR / (sd_abnormal_return_insurance * sqrt(200))) 
    
caar_insurance <- mean(turkey_insurance_significance$CAR)
caar_insurance_t_stat <- caar_insurance / (sd(turkey_insurance_significance$CAR, na.rm = TRUE) / sqrt(nrow(turkey_insurance_significance)))

#Real estate-------------------------------------------------------------------

turkey_realestate_market_model <- read_excel("C:/Users/hanna/OneDrive/Documentos/MNB mesterszak/3. félév/Adatalapú eseményelemzés/turkey_datastream.xlsx", sheet = "real_estate") %>% 
  clean_names() %>% 
  mutate(
    bist_national_100_adjusted_price = as.numeric(bist_national_100_adjusted_price)
  ) %>% 
  mutate(across(contains("adjusted"), ~ log(. / lead(.)), .names = "ln_return_{col}")) %>% 
  select(date, counter, contains("ln_return")) %>% 
  pivot_longer(3:8, names_to = "company", values_to = "return") %>% 
  group_by(company) %>%
  do({
    # Fit the model only for the estimation window (counter between -203 and -1)
    model <- lm(return ~ ln_return_bist_national_100_adjusted_price, data = filter(., counter > -203 & counter < -1))
    
    # Predict returns for all days (both estimation and event window)
    predicted_returns <- predict(model, newdata = .)
    
    # Add the predicted returns as a new column
    cbind(., predicted_return = predicted_returns)
  }) %>%
  ungroup() %>% 
  mutate(
    abnormal_return = return - predicted_return
  )

sd_abnormal_return_realestate <- sd(turkey_realestate_market_model$abnormal_return, na.rm = TRUE)

turkey_realestate_significance <- turkey_realestate_market_model %>% 
  filter(counter >= 0 & counter <= 7) %>% 
  mutate(
    t_statistic_abnormal = abnormal_return / sd_abnormal_return_realestate
  ) %>% 
  group_by(company) %>%
  summarise(CAR = sum(abnormal_return, na.rm = TRUE)) %>% 
  mutate(
    car_t_stat = CAR / (sd_abnormal_return_realestate * sqrt(200)))

caar_realestate <- mean(turkey_realestate_significance$CAR)
caar_realestate_t_stat <- caar_realestate / (sd(turkey_realestate_significance$CAR, na.rm = TRUE) / sqrt(nrow(turkey_realestate_significance)))

#Technology-------------------------------------------------------------------

turkey_technology_market_model <- read_excel("C:/Users/hanna/OneDrive/Documentos/MNB mesterszak/3. félév/Adatalapú eseményelemzés/turkey_datastream.xlsx", sheet = "technology") %>% 
  clean_names() %>% 
  mutate(
    bist_national_100_adjusted_price = as.numeric(bist_national_100_adjusted_price)
  ) %>% 
  mutate(across(contains("adjusted"), ~ log(. / lead(.)), .names = "ln_return_{col}")) %>% 
  select(date, counter, contains("ln_return")) %>% 
  pivot_longer(3:8, names_to = "company", values_to = "return") %>% 
  group_by(company) %>%
  do({
    # Fit the model only for the estimation window (counter between -203 and -1)
    model <- lm(return ~ ln_return_bist_national_100_adjusted_price, data = filter(., counter > -203 & counter < -1))
    
    # Predict returns for all days (both estimation and event window)
    predicted_returns <- predict(model, newdata = .)
    
    # Add the predicted returns as a new column
    cbind(., predicted_return = predicted_returns)
  }) %>%
  ungroup() %>% 
  mutate(
    abnormal_return = return - predicted_return
  )

sd_abnormal_return_technology <- sd(turkey_technology_market_model$abnormal_return, na.rm = TRUE)

turkey_technology_significance <- turkey_technology_market_model %>% 
  filter(counter >= 0 & counter <= 7) %>% 
  mutate(
    t_statistic_abnormal = abnormal_return / sd_abnormal_return_technology
  ) %>% 
  group_by(company) %>%
  summarise(CAR = sum(abnormal_return, na.rm = TRUE)) %>% 
  mutate(
    car_t_stat = CAR / (sd_abnormal_return_technology * sqrt(200)))

caar_technology <- mean(turkey_technology_significance$CAR)
caar_technology_t_stat <- caar_technology / (sd(turkey_technology_significance$CAR, na.rm = TRUE) / sqrt(nrow(turkey_technology_significance)))

#Tourism-------------------------------------------------------------------

turkey_tourism_market_model <- read_excel("C:/Users/hanna/OneDrive/Documentos/MNB mesterszak/3. félév/Adatalapú eseményelemzés/turkey_datastream.xlsx", sheet = "tourism") %>% 
  clean_names() %>% 
  mutate(
    bist_national_100_adjusted_price = as.numeric(bist_national_100_adjusted_price)
  ) %>% 
  mutate(across(contains("adjusted"), ~ log(. / lead(.)), .names = "ln_return_{col}")) %>% 
  select(date, counter, contains("ln_return")) %>% 
  pivot_longer(3:8, names_to = "company", values_to = "return") %>% 
  group_by(company) %>%
  do({
    # Fit the model only for the estimation window (counter between -203 and -1)
    model <- lm(return ~ ln_return_bist_national_100_adjusted_price, data = filter(., counter > -203 & counter < -1))
    
    # Predict returns for all days (both estimation and event window)
    predicted_returns <- predict(model, newdata = .)
    
    # Add the predicted returns as a new column
    cbind(., predicted_return = predicted_returns)
  }) %>%
  ungroup() %>% 
  mutate(
    abnormal_return = return - predicted_return
  )

sd_abnormal_return_tourism <- sd(turkey_tourism_market_model$abnormal_return, na.rm = TRUE)

turkey_tourism_significance <- turkey_tourism_market_model %>% 
  filter(counter >= 0 & counter <= 7) %>% 
  mutate(
    t_statistic_abnormal = abnormal_return / sd_abnormal_return_tourism
  ) %>% 
  group_by(company) %>%
  summarise(CAR = sum(abnormal_return, na.rm = TRUE)) %>% 
  mutate(
    car_t_stat = CAR / (sd_abnormal_return_tourism * sqrt(200)))

caar_tourism <- mean(turkey_tourism_significance$CAR)
caar_tourism_t_stat <- caar_tourism / (sd(turkey_tourism_significance$CAR, na.rm = TRUE) / sqrt(nrow(turkey_tourism_significance)))

#Electricity-------------------------------------------------------------------

turkey_electricity_market_model <- read_excel("C:/Users/hanna/OneDrive/Documentos/MNB mesterszak/3. félév/Adatalapú eseményelemzés/turkey_datastream.xlsx", sheet = "electricity") %>% 
  clean_names() %>% 
  mutate(
    bist_national_100_adjusted_price = as.numeric(bist_national_100_adjusted_price)
  ) %>% 
  mutate(across(contains("adjusted"), ~ log(. / lead(.)), .names = "ln_return_{col}")) %>% 
  select(date, counter, contains("ln_return")) %>% 
  pivot_longer(3:8, names_to = "company", values_to = "return") %>% 
  group_by(company) %>%
  do({
    # Fit the model only for the estimation window (counter between -203 and -1)
    model <- lm(return ~ ln_return_bist_national_100_adjusted_price, data = filter(., counter > -203 & counter < -1))
    
    # Predict returns for all days (both estimation and event window)
    predicted_returns <- predict(model, newdata = .)
    
    # Add the predicted returns as a new column
    cbind(., predicted_return = predicted_returns)
  }) %>%
  ungroup() %>% 
  mutate(
    abnormal_return = return - predicted_return
  )

sd_abnormal_return_electricity <- sd(turkey_electricity_market_model$abnormal_return, na.rm = TRUE)

turkey_electricity_significance <- turkey_electricity_market_model %>% 
  filter(counter >= 0 & counter <= 7) %>% 
  mutate(
    t_statistic_abnormal = abnormal_return / sd_abnormal_return_electricity
  ) %>% 
  group_by(company) %>%
  summarise(CAR = sum(abnormal_return, na.rm = TRUE)) %>% 
  mutate(
    car_t_stat = CAR / (sd_abnormal_return_electricity * sqrt(200)))

caar_electricity <- mean(turkey_electricity_significance$CAR)
caar_electricity_t_stat <- caar_electricity / (sd(turkey_electricity_significance$CAR, na.rm = TRUE) / sqrt(nrow(turkey_electricity_significance)))

#Food-------------------------------------------------------------------

turkey_food_market_model <- read_excel("C:/Users/hanna/OneDrive/Documentos/MNB mesterszak/3. félév/Adatalapú eseményelemzés/turkey_datastream.xlsx", sheet = "food") %>% 
  clean_names() %>% 
  mutate(
    bist_national_100_adjusted_price = as.numeric(bist_national_100_adjusted_price)
  ) %>% 
  mutate(across(contains("adjusted"), ~ log(. / lead(.)), .names = "ln_return_{col}")) %>% 
  select(date, counter, contains("ln_return")) %>% 
  pivot_longer(3:8, names_to = "company", values_to = "return") %>% 
  group_by(company) %>%
  do({
    # Fit the model only for the estimation window (counter between -203 and -1)
    model <- lm(return ~ ln_return_bist_national_100_adjusted_price, data = filter(., counter > -203 & counter < -1))
    
    # Predict returns for all days (both estimation and event window)
    predicted_returns <- predict(model, newdata = .)
    
    # Add the predicted returns as a new column
    cbind(., predicted_return = predicted_returns)
  }) %>%
  ungroup() %>% 
  mutate(
    abnormal_return = return - predicted_return
  )

sd_abnormal_return_food <- sd(turkey_food_market_model$abnormal_return, na.rm = TRUE)

turkey_food_significance <- turkey_food_market_model %>% 
  filter(counter >= 0 & counter <= 7) %>% 
  mutate(
    t_statistic_abnormal = abnormal_return / sd_abnormal_return_food
  ) %>% 
  group_by(company) %>%
  summarise(CAR = sum(abnormal_return, na.rm = TRUE)) %>% 
  mutate(
    car_t_stat = CAR / (sd_abnormal_return_food * sqrt(200)))

caar_food <- mean(turkey_food_significance$CAR)
caar_food_t_stat <- caar_food / (sd(turkey_food_significance$CAR, na.rm = TRUE) / sqrt(nrow(turkey_food_significance)))

#Healthcare-------------------------------------------------------------------

turkey_healthcare_market_model <- read_excel("C:/Users/hanna/OneDrive/Documentos/MNB mesterszak/3. félév/Adatalapú eseményelemzés/turkey_datastream.xlsx", sheet = "healthcare") %>% 
  clean_names() %>% 
  mutate(
    bist_national_100_adjusted_price = as.numeric(bist_national_100_adjusted_price)
  ) %>% 
  mutate(across(contains("adjusted"), ~ log(. / lead(.)), .names = "ln_return_{col}")) %>% 
  select(date, counter, contains("ln_return")) %>% 
  pivot_longer(3:8, names_to = "company", values_to = "return") %>% 
  group_by(company) %>%
  do({
    # Fit the model only for the estimation window (counter between -203 and -1)
    model <- lm(return ~ ln_return_bist_national_100_adjusted_price, data = filter(., counter > -203 & counter < -1))
    
    # Predict returns for all days (both estimation and event window)
    predicted_returns <- predict(model, newdata = .)
    
    # Add the predicted returns as a new column
    cbind(., predicted_return = predicted_returns)
  }) %>%
  ungroup() %>% 
  mutate(
    abnormal_return = return - predicted_return
  )

sd_abnormal_return_healthcare <- sd(turkey_healthcare_market_model$abnormal_return, na.rm = TRUE)

turkey_healthcare_significance <- turkey_healthcare_market_model %>% 
  filter(counter >= 0 & counter <= 7) %>% 
  mutate(
    t_statistic_abnormal = abnormal_return / sd_abnormal_return_healthcare
  ) %>% 
  group_by(company) %>%
  summarise(CAR = sum(abnormal_return, na.rm = TRUE)) %>% 
  mutate(
    car_t_stat = CAR / (sd_abnormal_return_healthcare * sqrt(200)))

caar_healthcare <- mean(turkey_healthcare_significance$CAR)
caar_healthcare_t_stat <- caar_healthcare / (sd(turkey_healthcare_significance$CAR, na.rm = TRUE) / sqrt(nrow(turkey_healthcare_significance)))


#Debt -------------------------------------------------------------------

turkey_debt_model <- read_excel("C:/Users/hanna/OneDrive/Documentos/MNB mesterszak/3. félév/Adatalapú eseményelemzés/turkey_datastream.xlsx", sheet = "all") %>% 
  clean_names() %>% 
  mutate(date = as.Date(date)) %>%  # Ensure the date column is in Date format
  mutate(across(contains("adjusted"), ~ log(. / lead(.)), .names = "ln_return_{col}")) %>% 
  select(date, counter, contains("ln_return"), contains("common")) %>% 
  pivot_longer(
    cols = 4:260,  # Exclude date column from reshaping
    names_to = c("company"),  # Separate the column names into company and variable
    values_to = "value"  # Store all values in a single column
  ) %>% 
  na.omit() %>% 
  mutate(
    metric = case_when(
        str_detect(company, "price") ~ "return",
        TRUE ~ "debt"),
    company =  str_extract(company, "^(.*?)(?=_unadjusted_price|_adjusted_price|_total_debt_percent_common_equity)"),
    company = str_remove(company, "ln_return_")
  ) %>% 
  pivot_wider(
    names_from = "metric",
    values_from = "value"
  ) %>% 
  na.omit() %>% 
  group_by(company) %>%
  do({
    # Fit the model only for the estimation window (counter between -203 and -1)
    model <- lm(return ~ ln_return_bist_national_100_adjusted_price, data = filter(., counter > -203 & counter < -1))
    
    # Predict returns for all days (both estimation and event window)
    predicted_returns <- predict(model, newdata = .)
    
    # Add the predicted returns as a new column
    cbind(., predicted_return = predicted_returns)
  }) %>%
  ungroup() %>% 
  mutate(
    abnormal_return = return - predicted_return,
    z_score_debt = (debt - mean(debt, na.rm = TRUE))/sd(debt, na.rm = TRUE)
  ) %>% 
  filter(z_score_debt < 3) %>% 
  mutate(debt_category = ifelse(debt < median(debt), "Low Debt", "High Debt")) %>% 
  filter(debt_category %in% c("Low Debt", "High Debt"))

turkey_debt_significance <- turkey_debt_model %>%
  # Filter the data for the desired date range
  filter(counter >= 0 & counter <= 7) %>%
  # Group by company to summarize abnormal returns
  group_by(counter, debt_category) %>%
  summarise(CAR_debt = sum(abnormal_return, na.rm = TRUE)) %>%
  ungroup() %>%
  # Now you can filter based on debt_category
  filter(debt_category %in% c("Low Debt", "High Debt"))

debt_t_test_result <- t.test(CAR_debt ~ debt_category, data = turkey_debt_significance)

debt_t_test_result

hist(turkey_debt_model$debt)

unique(turkey_debt_model$company)

#ESG  -------------------------------------------------------------------

turkey_esg_model <- read_excel("C:/Users/hanna/OneDrive/Documentos/MNB mesterszak/3. félév/Adatalapú eseményelemzés/turkey_datastream.xlsx", sheet = "all") %>% 
  clean_names() %>% 
  mutate(date = as.Date(date)) %>%  # Ensure the date column is in Date format
  mutate(across(contains("adjusted"), ~ log(. / lead(.)), .names = "ln_return_{col}")) %>% 
  select(date, counter, contains("ln_return"), contains("environment")) %>% 
  pivot_longer(
    cols = 4:259,  # Exclude date column from reshaping
    names_to = c("company"),  # Separate the column names into company and variable
    values_to = "value"  # Store all values in a single column
  ) %>% 
  na.omit() %>% 
  mutate(
    metric = case_when(
      str_detect(company, "price") ~ "return",
      TRUE ~ "esg_score"),
    company =  str_extract(company, "^(.*?)(?=_unadjusted_price|_environment_pillar_score|_environmental_pillar_score)"),
    company = str_remove(company, "ln_return_")
  ) %>% 
  pivot_wider(
    names_from = "metric",
    values_from = "value"
  ) %>% 
  na.omit() %>% 
  group_by(company) %>%
  do({
    # Fit the model only for the estimation window (counter between -203 and -1)
    model <- lm(return ~ ln_return_bist_national_100_adjusted_price, data = filter(., counter > -203 & counter < -1))
    
    # Predict returns for all days (both estimation and event window)
    predicted_returns <- predict(model, newdata = .)
    
    # Add the predicted returns as a new column
    cbind(., predicted_return = predicted_returns)
  }) %>%
  ungroup() %>% 
  mutate(
    abnormal_return = return - predicted_return
  ) %>% 
  mutate(esg_category = ifelse(esg_score < median(esg_score), "Low ESG", "High ESG")) %>% 
  filter(esg_category %in% c("Low ESG", "High ESG"))

turkey_esg_significance <- turkey_esg_model %>%
  # Ensure you're keeping all companies with valid ESG ratings
  group_by(counter, esg_category) %>%
  # Filter within the event window for abnormal returns, but keep all valid ESG data
  filter(counter >= 0 & counter <= 7) %>%
  # Summarize abnormal returns per company and ESG category within the event window
  summarise(CAR_esg = sum(abnormal_return, na.rm = TRUE), .groups = "drop") %>%
  # Now you can filter based on debt_category, ensuring that companies are not dropped
  filter(esg_category %in% c("High ESG", "Low ESG"))

esg_t_test_result <- t.test(CAR_esg ~ esg_category, data = turkey_esg_significance)

print(esg_t_test_result)

hist(turkey_roe_model$roe)

unique(turkey_roe_model$company)

#ROE -------------------------------------------------------------------

turkey_roe_model <- read_excel("C:/Users/hanna/OneDrive/Documentos/MNB mesterszak/3. félév/Adatalapú eseményelemzés/turkey_datastream.xlsx", sheet = "all") %>% 
  clean_names() %>% 
  mutate(date = as.Date(date)) %>%  # Ensure the date column is in Date format
  mutate(across(contains("adjusted"), ~ log(. / lead(.)), .names = "ln_return_{col}")) %>% 
  select(date, counter, contains("ln_return"), contains("return_on_equity")) %>% 
  pivot_longer(
    cols = 4:259,  # Exclude date column from reshaping
    names_to = c("company"),  # Separate the column names into company and variable
    values_to = "value"  # Store all values in a single column
  ) %>% 
  na.omit() %>% 
  mutate(
    metric = case_when(
      str_detect(company, "price") ~ "return",
      TRUE ~ "roe"),
    company =  str_extract(company, "^(.*?)(?=_unadjusted_price|_return_on_equity_total_percent)"),
    company = str_remove(company, "ln_return_")
  ) %>% 
  pivot_wider(
    names_from = "metric",
    values_from = "value"
  ) %>% 
  na.omit() %>% 
  group_by(company) %>%
  do({
    # Fit the model only for the estimation window (counter between -203 and -1)
    model <- lm(return ~ ln_return_bist_national_100_adjusted_price, data = filter(., counter > -203 & counter < -1))
    
    # Predict returns for all days (both estimation and event window)
    predicted_returns <- predict(model, newdata = .)
    
    # Add the predicted returns as a new column
    cbind(., predicted_return = predicted_returns)
  }) %>%
  ungroup() %>% 
  mutate(
    abnormal_return = return - predicted_return,
    z_score_roe = (roe - mean(roe, na.rm = TRUE))/sd(roe, na.rm = TRUE)
  ) %>% 
  filter(z_score_roe < 3 & z_score_roe > -3) %>% 
  mutate(roe_category = ifelse(roe < median(roe), "Low roe", "High roe")) %>% 
  filter(roe_category %in% c("High roe", "Low roe"))

turkey_roe_significance <- turkey_roe_model %>%
  # Ensure you're keeping all companies with valid ESG ratings
  group_by(counter, roe_category) %>%
  # Filter within the event window for abnormal returns, but keep all valid ESG data
  filter(counter >= 0 & counter <= 7) %>%
  # Summarize abnormal returns per company and ESG category within the event window
  summarise(CAR_roe = sum(abnormal_return, na.rm = TRUE), .groups = "drop") %>%
  # Now you can filter based on debt_category, ensuring that companies are not dropped
  filter(roe_category %in% c("High roe", "Low roe"))

roe_t_test_result <- t.test(CAR_roe ~ roe_category, data = turkey_roe_significance)

print(roe_t_test_result)

#Plotting ------------------------------------------------------------------
affected_plot_data  <- turkey_construction_market_model %>% 
  left_join(turkey_insurance_market_model, by = "counter") %>% 
  left_join(turkey_electricity_market_model, by = "counter") %>% 
  left_join(turkey_food_market_model, by = "counter") %>% 
  left_join(turkey_healthcare_market_model, by = "counter") %>%
  left_join(turkey_technology_market_model, by = "counter") %>%
  left_join(turkey_tourism_market_model, by = "counter") %>%
  left_join(turkey_realestate_market_model, by = "counter") %>%
  pivot_longer(2:17, names_to = "return_type", values_to = "return") %>% 
  mutate(
    company = str_extract(return_type, "^[^_]+"),
    return_type = str_remove(return_type, "^[^_]+_"),
    company = str_to_title(company),
    return_type = str_remove(return_type, "return_"),
    return_type = str_to_title(return_type)
  ) %>% 
  pivot_wider(names_from = "company", values_from = "return") %>% 
  filter(counter >= 0 & counter <= 7) %>% 
  mutate(
    counter = as.factor(counter)
  )
  

affected_plot <- ggplot(affected_plot_data) +
  geom_line(aes(x = counter, y = Return, color = "Várható hozamok", group = return_type), linewidth = 1) +
  geom_line(aes(x = counter, y = Predicted, color = "Tényleges hozamok", group = return_type), linewidth = 1) +
  geom_vline(xintercept = "0", linetype = "dashed", color = "#018ad4", linewidth = 1) +
  labs(x = "", y = "") +
  facet_wrap(~ return_type, scales = "free_y") +
  scale_color_manual(values = c("Várható hozamok" = "#a1a875", "Tényleges hozamok" = "#642434")) +
  theme_minimal() +
  theme(legend.title= element_blank(),
        text = element_text(family = "serif"))

unaffected_plot_data  <- inf_techn_event %>% 
  left_join(food_event, by = "date") %>% 
  select(-t_statistic_abnormal_inf_techn) %>% 
  pivot_longer(19:22, names_to = "return_type", values_to = "return") %>% 
  mutate(
    company = str_extract(return_type, "^[^_]+"),
    return_type = str_remove(return_type, "^[^_]+_"),
    counter = as.factor(counter), 
    company = str_to_title(company)
  ) %>% 
  pivot_wider(names_from = return_type, values_from = return)

unaffected_plot <- ggplot(unaffected_plot_data) +
  geom_line(aes(x = counter, y = expected_returns, color = "Várható hozamok", group = 1), linewidth = 1) +
  geom_line(aes(x = counter, y = abnormal_returns, color = "Tényleges hozamok", group = 1), linewidth = 1) +
  geom_vline(xintercept = "0", linetype = "dashed", color = "#018ad4", linewidth = 1) +
  labs(x = "", y = "") +
  facet_wrap(~ company, scales = "free_y") +
  scale_color_manual(values = c("Várható hozamok" = "#a1a875", "Tényleges hozamok" = "#642434")) +
  theme_minimal() +
  theme(legend.title= element_blank())

summary_data <- read_excel("C:/Users/hanna/OneDrive/Documentos/MNB mesterszak/3. félév/Adatalapú eseményelemzés/turkey_earthquake_results_summary.xlsx", sheet = "cars_plot") %>% 
  mutate(
    sector = as.factor(szektor),
    sector = fct_reorder(sector, CAAR)
  )

summary_plot <- ggplot(summary_data) +
  geom_col(aes(x = sector, y = CAAR, fill = sector)) +
  labs(y = "Kumulált áltagos abnormális hozamok", x = "") +
  facet_wrap(~event_window) +
  theme_minimal() +
  theme(legend.title= element_blank(),
        axis.text.x = element_text(angle = 90, size = 13),
        legend.position = "none",
        panel.grid.major = element_blank(),
        text = element_text(family = "serif")) +
  scale_fill_manual(values = c("villamosenergia" = "#a1a875", "biztosítás" = "#a1a875", "IT" = "#642434", "építőipar" = "#a1a875", "turizmus" = "#a1a875", "élelmiszeripar" = "#642434", "ingatlan" = "#a1a875", "egészségügy" = "#642434"))

roe_plot <- ggplot(turkey_roe_significance) +
  geom_line(aes(x = counter, y = CAR_roe, color = roe_category, group = roe_category), linewidth = 1.5) +
  geom_line(aes(x = counter, y = CAR_roe, color = roe_category, group = roe_category), linewidth = 1.5) +
  labs(x = "", y = "", color = "ROE Category") +  # Add label for legend
  scale_color_manual(
    values = c(
      "Low roe" = "#a1a875",    # Replace with your desired color for "Low Debt" # Replace with your desired color for "Medium Debt"
      "High roe" = "#642434"    # Replace with your desired color for "High Debt"
    )
  ) +
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        legend.position = c(0.7,0.7))

esg_plot <- ggplot(turkey_esg_significance) +
  geom_line(aes(x = counter, y = CAR_esg, color = esg_category, group = esg_category), linewidth = 1.5) +
  geom_line(aes(x = counter, y = CAR_esg, color = esg_category, group = esg_category), linewidth = 1.5) +
  labs(x = "", y = "", color = "ESG Category") +  # Add label for legend
  scale_color_manual(
    values = c(
      "Low ESG" = "#a1a875",    # Replace with your desired color for "Low Debt" # Replace with your desired color for "Medium Debt"
      "High ESG" = "#642434"    # Replace with your desired color for "High Debt"
    )
  ) +
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        legend.position = c(0.7,0.7))

debt_plot <- ggplot(turkey_debt_significance) +
  geom_line(aes(x = counter, y = CAR_debt, color = debt_category, group = debt_category), linewidth = 1.5) +
  geom_line(aes(x = counter, y = CAR_debt, color = debt_category, group = debt_category), linewidth = 1.5) +
  labs(x = "", y = "Abnormális hozamok", color = "Debt Category") +  # Add label for legend
  scale_color_manual(
    values = c(
      "Low Debt" = "#a1a875",    # Replace with your desired color for "Low Debt" # Replace with your desired color for "Medium Debt"
      "High Debt" = "#642434"    # Replace with your desired color for "High Debt"
    )
  ) +
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        legend.position = c(0.7,0.7))


caars_turkey <- read_excel("C:/Users/hanna/OneDrive/Documentos/MNB mesterszak/3. félév/Adatalapú eseményelemzés/turkey_earthquake_results_summary.xlsx", sheet = "per_company_7days_2") %>% 
  mutate(
    szektor = as.factor(szektor),
    szektor = fct_reorder(szektor, CAAR)
  )



caars_turkey_plot <- ggplot(caars_turkey) +
  geom_col(aes(x = szektor, y = CAAR, fill = szektor)) +
  labs(y = "Kumulált abnormális hozamok", x = "") +
  theme_minimal() +
  theme(legend.title= element_blank(),
        axis.text.x = element_text(angle = 90, size = 13),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  scale_fill_manual(values = c("villamosenergia" = "#a1a875", "biztosítás" = "#a1a875", "IT" = "#642434", "építőipar" = "#a1a875", "turizmus" = "#a1a875", "élelmiszeripar" = "#642434", "ingatlan" = "#a1a875", "egészségügy" = "#642434"))



grid.arrange(debt_plot, esg_plot, roe_plot, ncol = 3)
