setwd("~/Github/AgentBasedModel/")

#### Load data from national sources for calculating probabilities ####

## Survival analysis ##

survival_rates <- read.csv("./test.csv") %>% 
  mutate(cumulative_percent = In.state / 100,
         cumulative_prob_did_not = 1 - cumulative_percent,
         prob_recidivate = 1 - cumulative_prob_did_not / lag(cumulative_prob_did_not))

survival_rates$prob_recidivate[1] <- 1 - survival_rates$cumulative_prob_did_not[1]

survival_rates <- survival_rates %>% 
  mutate(prob_did_not_recidivate = 1 - prob_recidivate) %>% 
  select(prob_recidivate, prob_did_not_recidivate)



## Prison terms ##
prison_terms <- read.csv("./ncrp0908.csv") %>% 
  filter(X != "", 
         X != "Number of releases",
         X != "All offenses") %>% 
  rename(type = X,
         percent = X.3,
         median_term = X.5,
         mean_term = X.7) %>% 
  select(type, percent, median_term, mean_term)




#### Now the function with the model ####
agent <- function(months) {
  
  # set up the data frame
  df <- data.frame(month=numeric(0),
                   months_free=numeric(0),
                   rearrested_or_not=numeric(0),
                   prison_sentence=numeric(0))
  
  # loop through each month and see what happens			
  for (month in 1:months) {
    
    ### generate action for each month ###
    m.month <- month
    
    # Dependent on whether they were arrested or not
    m.months_free <- ifelse(month == 1, 1,
                            ifelse(m.prison_sentence > 0, 1,
                                   tmp.months_free + 1))
    
    ### TODO : base both of these on Utah data
    # Binary variable based on the conditional probability table from nationwide trends
    m.rearrested_or_not <- sample(x = c(1, 0), 
                                  size = 1, 
                                  replace = FALSE, 
                                  prob = c(as.matrix(survival_rates[m.months_free,])))
  
    
    # Add a prison sentence if there was an arrest
    # Also based on probability table from national trends 
    m.prison_sentence <- if_else(m.rearrested_or_not == 0, 0,
                                 as.numeric(
                                   sample(x = as.vector(prison_terms$median_term),
                                          size = 1,
                                          replace = FALSE,
                                          prob = c(prison_terms$percent))
                                 ))
    
    # add month to the data frame
    df[month,] <- c(m.month, m.months_free, m.rearrested_or_not, m.prison_sentence)
    
    # Make temporary vector for determining months free in the next pass
    tmp.months_free <- if_else(m.rearrested_or_not == 1, 0, m.months_free)
  }
  
  # output results
  return (df)
}



#### Unit tests ####
agent_test <- agent(60)


# # Here is a test to see if the percent of people re-arrested matches the data in the survival rates df
# count_rearrested <- 1
# 
# for (i in 1:1000) {
#   basic <- 0
#   agent_test <- agent(60)
#   arrested <- (sum(agent_test$rearrested_or_not))
#   if (arrested > 0) {
#     count_rearrested <- count_rearrested + 1
#   }
# }
