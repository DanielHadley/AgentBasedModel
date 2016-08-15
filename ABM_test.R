setwd("~/Github/AgentBasedModel/")
library(dplyr)


## Assumptions ##

# First and important: the recidivism rates and prison terms are determined by historical data. These *will* change with the JRI. Recidivism rates are likely to go down as a function of the number of parole and probabtion violations. Whether crime rates do is anyone's guess, since there is not a lot of money for rehabilitation. 
# There are ways to model this very simple using the recid_reduction_ovr_cntrl argument of the load_survival_data, but this only allows you to assume a reduction in re-arrests, not a change in the frequency of parole and probation violations.
# For future versions, we may want to think more about refining these estimates. 




#### Load data from national and local sources for calculating probabilities ####
## Prison terms ##
# Replaced national data with Utah specific data
# I pieced this data together using an email from Julie Christensen, which contained recidivism rates for the COD demographic, and the Justice Reinvestment Report, which contained the average sentencing:
# http://justice.utah.gov/Documents/CCJJ/Reports/Justice_Reinvestment_Report_2014.pdf
# Before getting to the crime types and average sentences, though, it's important to grasp this statistic: 70% of parolees are returned on a technical violation. Only 30% are returned for crimes. WOW. Of those 70%, I estimate that 37 of the 70% are for parole violations, and 33 of the 70 are for probation violations
prison_terms <- read.csv("./clean_data/utah_cod_recidivism_rates.csv", stringsAsFactors = FALSE) %>% 
  mutate(mean_time_served = round(mean_time_served))

# I do these so that I can add a blank in the function below
prison_terms[8,1] <- ""
prison_terms[8,4] <- 0



## Loads data on survival analysis, i.e., the time it takes people to recidivate, and turn it into a DF with the probability someone will recdiviate given the month free 
# The two arguments define the recidivism baseline and reduction:
# cntrl_inc_ovr_ntnl = what is the % increase (not % point increase) in our control group over national data (which looks similar to Utah's)? If 0, same as national trends
# recid_reduction_ovr_cntrl = having defined the control group above, what is the % we expect the program to reduce recidivism?
# The data that Julie sent suggest that the national trends for recidivism are very similar to the Utah trends (or at least they were between 2012 and 2015).
# The data also show that our study group had a recidivsm rate that was 74.3% after 36 months, whereas all prisoners had a rate of 64.2% (again, close to natural). The difference between the control group and national trends is (74.3 - 64.2) / 64.2, which I will list as the default arg for the function below: cntrl_inc_ovr_ntnl <- 0.157
load_survival_data <- function(cntrl_inc_ovr_ntnl, recid_reduction_ovr_cntrl){
  
  survival_rates <- read.csv("./clean_data/mschpprts05f02.csv", 
                             stringsAsFactors = F) %>% 
    filter(X != "",
           X != "National") %>% 
    mutate(In.state = as.numeric(X.1),
           In.state_control = 
             In.state + (In.state * cntrl_inc_ovr_ntnl),
           In.state_treatment = 
             In.state_control - (In.state_control * recid_reduction_ovr_cntrl),
           cumulative_percent = In.state_treatment / 100,
           cumulative_prob_did_not = 1 - cumulative_percent,
           prob_recidivate = 
             1 - cumulative_prob_did_not / lag(cumulative_prob_did_not),
           prob_did_not_recidivate = 1 - prob_recidivate) %>% 
    filter(In.state != 0) %>% 
    select(prob_recidivate, prob_did_not_recidivate)
}

# I could not put this inside a function (probably global environment thing)
# So I have it outside
# This is national data : 
survival_rates <- load_survival_data(0, 0)

# This is the baseline for the COD population
# survival_rates <- load_survival_data(.157, 0)

# And this is the treatment group with a 20% reduction
# survival_rates <- load_survival_data(.157, .2)




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


# If arrested what is the prison sentence? This samples from the crime frequencies and returns a sentence based on the selected crime.
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


# Simply for summing later and helping with cost functions below
say_if_in_prison <- function(m.prison_sentence) {
  ifelse(m.prison_sentence > 0, 1, 0)
}


# The monthly costs of prison
calc_prison_costs <- function(m.is_in_prison, m.month) {
  
  # My calculation of the marginal costs
  # See Analyze_Costs.R
  marginal_cost_of_prison <- 5200 / 12
  
  # Marginal long term
  marginal_cost_of_prison_lt <- 16376 / 12
  
  if_else(m.is_in_prison == 1 & m.month <= 36, marginal_cost_of_prison, 
          ifelse(m.is_in_prison == 1 & m.month > 36, marginal_cost_of_prison_lt,
                 0))
}


calc_crime_cost <- function(crime_type)




#### A function with the single-agent model : this generates data for one person ####
sim_single_agent <- function(months) {
  
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
    m.prison_costs <- calc_prison_costs(m.is_in_prison, m.month)
    
    
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




#### A function with the multi-agent model : this generates data for several ppl ####
sim_multi_agents <- function(n_agents, n_months){
  
  # set up the data frame
  df <- data.frame(prison_time=numeric(0),
                   months_free=numeric(0),
                   prison_costs=numeric(0),
                   arrests=numeric(0))
  
  # loop through each month and see what happens			
  for (agent in 1:n_agents) {
    
    ### generate action for each person ###
    single_agent <- sim_single_agent(n_months)
    
    # Sum things that happend to the agent
    a.prison_time <- sum(as.numeric(single_agent$is_in_prison))
    a.months_free <- sum(as.numeric(single_agent$months_free))
    a.prison_costs <- sum(as.numeric(single_agent$prison_costs))
    a.arrests <- sum(as.numeric(single_agent$rearrested_or_not))
    
    
    # add month to the data frame
    df[agent,] <- 
      c(a.prison_time, a.months_free, a.prison_costs, a.arrests)
    
  }
  
  # output results
  return (df)
  
}




#### Unit tests ####

# First we re-load the data we want
# This is the baseline for the COD population
survival_rates <- load_survival_data(.157, 0)

# Now test
single_agent_test <- sim_single_agent(60)

multi_agent_test <- sim_multi_agents(n_agents = 1000, n_months = 60)

# This is kind of clumpy because some ppl could go in right at the end of the 5 yrs
hist(multi_agent_test$prison_time)
mean(multi_agent_test$prison_time)

# This is kind of clumpy because some ppl could go in right at the end of the 5 yrs
hist(multi_agent_test$prison_costs)
mean(multi_agent_test$prison_costs)

# arrests
hist(multi_agent_test$arrests)
mean(multi_agent_test$arrests)
table(multi_agent_test$arrests)



