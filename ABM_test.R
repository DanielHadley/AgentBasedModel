setwd("~/Github/AgentBasedModel/")

#### Load data from national and local sources for calculating probabilities ####

## Survival analysis ##
# This is national data that comes from the BJS
survival_rates <- read.csv("./clean_data/mschpprts05f02.csv", 
                           stringsAsFactors = F) %>% 
  filter(X != "",
         X != "National") %>% 
  mutate(In.state = as.numeric(X.1),
         cumulative_percent = In.state / 100,
         cumulative_prob_did_not = 1 - cumulative_percent,
         prob_recidivate = 1 - cumulative_prob_did_not / lag(cumulative_prob_did_not),
         prob_did_not_recidivate = 1 - prob_recidivate) %>% 
  filter(In.state != 0) %>% 
  select(prob_recidivate, prob_did_not_recidivate)



## Prison terms ##
# Replaced national data with Utah specific data
# I pieced this data together using an email from Julie Christensen, which contained recidivism rates for the COD demographic, and the Justice Reinvestment Report, which contained the average sentencing:
# http://justice.utah.gov/Documents/CCJJ/Reports/Justice_Reinvestment_Report_2014.pdf
# Before getting to the crime types and average sentences, though, it's important to grasp this statistic: 70% of parolees are returned on a technical violation. Only 30% are returned for crimes. WOW. Of those 70%, I estimate that 37 of the 70% are for parole violations, and 33 of the 70 are for probation violations
prison_terms <- read.csv("./clean_data/utah_cod_recidivism_rates.csv", stringsAsFactors = FALSE) %>% 
  mutate(mean_time_served = round(mean_time_served))




#### Functions needed for the Model ####

# If he is free, how many months has he been free
calc_months_free <- function(m.month, m.prison_sentence, tmp.months_free) {
  ifelse(m.month == 1, 1,
         ifelse(m.prison_sentence > 1, 0,
                tmp.months_free + 1))
}


# Taking the survival analysis data from national trends, this will calculate the odds of being rearrested conditional on the number of months he has been free
calc_odds_of_being_rearrested <- function(m.months_free) {
  ifelse(m.months_free == 0, 0,
         sample(x = c(1, 0), 
                size = 1, 
                replace = FALSE, 
                prob = c(as.matrix(survival_rates[m.months_free,]))))
}


# If arrested what is the prison sentence. This samples from the crime frequencies and returns a sentence based on the selected crime.
define_crime_and_time <- function(m.rearrested_or_not, m.month, tmp.prison_sentence) {
  
  # First the index of crime and time, based on a probabilistic sample of crime types
  # It's a little ineffecient to do this every time, but NBD
  crime_index <- sample(x = c(1:nrow(prison_terms)),
                        size = 1,
                        replace = FALSE,
                        prob = c(prison_terms$frequency))
  
  # The crime
  crime <- if_else(m.rearrested_or_not == 1, 
                   prison_terms$offense_type[crime_index],
                   "")
  
  # Sentence, aka, the time
  prison_sentence <- if_else(m.rearrested_or_not == 1, 
                             as.numeric(prison_terms$mean_time_served[crime_index]),
                             ifelse(m.month == 1, 0,
                                    ifelse(m.rearrested_or_not == 0, 
                                           tmp.prison_sentence, 
                                           0)))
  
  to_return <- list(crime, prison_sentence)
  
  return(to_return)
}


# Simply for summing later and helping with costs
say_if_in_prison <- function(m.prison_sentence) {
  ifelse(m.prison_sentence > 0, 1, 0)
}


# The monthly costs of prison
calc_prison_costs <- function(m.is_in_prison) {
  marginal_cost_of_prison <- 5000 / 12
  if_else(m.is_in_prison == 1, marginal_cost_of_prison, 0)
}


#### Now the function with the model ####
agent <- function(months) {
  
  # Define some factors
  levels.crimetype <- factor(prison_terms$offense_type)
  
  # set up the data frame
  df <- data.frame(month=numeric(0),
                   months_free=numeric(0),
                   rearrested_or_not=numeric(0),
                   prison_sentence=numeric(0),
                   crime_type=factor(levels = levels.crimetype),
                   is_in_prison=numeric(0),
                   prison_costs=numeric(0))
  
  # loop through each month and see what happens			
  for (month in 1:months) {
    
    ### generate action for each month ###
    m.month <- month
    
    # Dependent on whether they were arrested or not
    m.months_free <- calc_months_free(m.month, m.prison_sentence, tmp.months_free)
    
    # Binary variable based on the conditional probability table from nationwide trends
    m.rearrested_or_not <- calc_odds_of_being_rearrested(m.months_free)
  
    
    # Add a crime and prison sentence if there was an arrest
    # Also based on probability table from national trends
    crime_and_time <- 
      define_crime_and_time(m.rearrested_or_not, m.month, tmp.prison_sentence)
    
    m.prison_sentence <- as.numeric(crime_and_time[2])
    
    m.crime_type <- as.character(crime_and_time[1])
    
    # For calculating the total months in prison later
    m.is_in_prison <- say_if_in_prison(m.prison_sentence)
    
    # Costs to prison system
    m.prison_costs <- calc_prison_costs(m.is_in_prison)
    
    
    # add month to the data frame
    df[month,] <- 
      c(m.month, m.months_free, m.rearrested_or_not, m.prison_sentence, 
        m.crime_type, m.is_in_prison, m.prison_costs)
    
    
    # Make temporary vector for determining months free in the next pass
    tmp.months_free <- if_else(m.rearrested_or_not == 1, 0, m.months_free)
    
    # Also, count down the time served from last month, if any
    tmp.prison_sentence <- ifelse(m.prison_sentence == 0, 0,
                                  m.prison_sentence - 1)
  }
  
  # output results
  return (df)
}



#### Unit tests ####
agent_test <- agent(60)


# Here is a test to see if the percent of people re-arrested matches the data in the survival rates df
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
