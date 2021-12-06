install.packages("rlang")
                            install.packages("tidymodels")
                            library("tidymodels")
                            library("tidyverse")
                            library("stringr")
                            # Dataset URL
                            dataset_url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/seoul_bike_sharing_converted_normalized.csv"
                            bike_sharing_df <- read_csv(dataset_url)
                            spec(bike_sharing_df)
                            bike_sharing_df <- bike_sharing_df %>% 
                              select(-DATE, -FUNCTIONING_DAY)
                            set.seed(1234)
                            data_split <- initial_split(bike_sharing_df, prop = .75)
                            # extracting training data and test data as two seperate dataframes
                            data_train <- training(data_split)
                            data_test <- testing(data_split)
                            library("dplyr")
                            # Use `linear_reg()` with engine `lm` and mode `regression`
                            lm_model_weather= linear_reg() %>% 
                              set_engine('lm') %>% 
                              set_mode('regression') %>%
                              fit(RENTED_BIKE_COUNT~TEMPERATURE + HUMIDITY + WIND_SPEED + VISIBILITY + DEW_POINT_TEMPERATURE + SOLAR_RADIATION + RAINFALL + SNOWFALL,data=data_train)
                            
                            
                            result1=lm_model_weather%>%
                              predict(new_data=data_test)%>%
                            mutate(truth = data_test$RENTED_BIKE_COUNT)
                            
                            result1[result1<0] <- 0
                           
                            print(result1)
                            rsq1=rsq(result1, truth = truth,
                                estimate = .pred)
                            lm_model_all=linear_reg() %>% set_engine('lm') %>%
                              set_mode('regression') %>% 
                              fit(RENTED_BIKE_COUNT~.,data=data_train)
                            result2=lm_model_all%>%
                              predict(new_data=data_test)%>%
                              mutate(truth = data_test$RENTED_BIKE_COUNT)
                            result2[result2<0] <- 0
                            rsq2=rsq(result2, truth = truth,
                                     estimate = .pred)
                            print(result2)
                            rmse1=rmse(result1, truth = truth,
                                 estimate = .pred)
                            rmse2=rmse(result2, truth = truth,
                                 estimate = .pred)
                            
                            abs_cof_df <- stack(abs(lm_model_all$fit$coefficients))
                             names(abs_cof_df) <- c("Coef", "Variable")
                             abs_cof_df <- abs_cof_df %>% 
                               select(Variable, Coef)
                            coefs_sorted <- arrange(abs_cof_df, -Coef) 
                            coefs_sorted <- na.omit(coefs_sorted)
                             
                               # Draw bar charts for the sorted coefficients
                               ggplot(data=coefs_sorted, aes(x= reorder(Variable,Coef),Coef)) +
                               geom_bar(stat = "identity") +
                               coord_flip() +
                               theme_minimal()
                            lm_poly=lm(RENTED_BIKE_COUNT ~ poly(TEMPERATURE, 4) + poly(HUMIDITY, 3) + 
                                          poly(DEW_POINT_TEMPERATURE,2) + poly(SOLAR_RADIATION,2) + 
                                          poly(SNOWFALL,2),data = data_train)
                            
                            result3=lm_poly%>%
                              predict(new_data=data_test)%>%
                              mutate(truth=data_test$RENTED_BIKE_COUNT)
                            
                            result3=lm_poly%>%
                              predict(new_data=data_test,truth=data_test$RENTED_BIKE_COUNT)
                                                        result3[result3<0] <- 0
                                                        rsq3=rsq(result3, truth = truth,
                                                                 estimate = .pred)
                            
                         
                     