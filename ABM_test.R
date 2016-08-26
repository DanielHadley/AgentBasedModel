setwd("~/Github/AgentBasedModel/")
library(dplyr)


#### Assumptions ####

# First and important: the recidivism rates and prison terms are determined by historical data. These *will* change with the JRI. Recidivism rates are likely to go down as a function of the number of parole and probabtion violations. Whether crime rates do is anyone's guess, since there is not a lot of money for rehabilitation. 
# There are ways to model this very simple using the recid_reduction_ovr_cntrl argument of the load_survival_data, but this only allows you to assume a reduction in re-arrests, not a change in the frequency of parole and probation violations.
# For future versions, we may want to think more about refining these estimates. 




#### Load data from national and local sources for calculating probabilities ####

## Prison terms ##
# Replaced national data with Utah specific data
# I pieced this data together using an email from Julie Christensen, which contained recidivism rates for the COD demographic, and the Justice Reinvestment Report, which contained the average sentencing:
# http://justice.utah.gov/Documents/CCJJ/Reports/Justice_Reinvestment_Report_2014.pdf
# http://www.acluutah.org/criminal-justice/item/download/15_0cfccf37c91e9fb16be4ac2e89ca12f2
# Crime costs come from this report, which I extrapolated for parole violations:
# http://www.justice.utah.gov/Documents/CCJJ/Cost%20of%20Crime/Utah%20Cost%20of%20Crime%202012%20-%20Methods%20Review%20Cost.pdf
# Before getting to the crime types and average sentences, though, it's important to grasp this statistic: 70% of parolees are returned on a technical violation.
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


## The monthly costs of prison
# Marginal
calc_marginal_prison_costs <- function(m.is_in_prison, m.month) {
  
  # My calculation of the marginal costs
  # See Analyze_Costs.R
  marginal_cost_of_prison <- 5200 / 12
  
  # Marginal long term
  marginal_cost_of_prison_lt <- 16376 / 12
  
  if_else(m.is_in_prison == 1 & m.month <= 36, marginal_cost_of_prison, 
          ifelse(m.is_in_prison == 1 & m.month > 36, marginal_cost_of_prison_lt,
                 0))
}


# Total
# From the CPD data
calc_total_prison_costs <- function(m.is_in_prison) {
  
  total_cost_of_prison <- 30000 / 12
  
  if_else(m.is_in_prison == 1, total_cost_of_prison, 
          0)
}


### The costs of crime
# http://www.justice.utah.gov/Documents/CCJJ/Cost%20of%20Crime/Utah%20Cost%20of%20Crime%202012%20-%20Methods%20Review%20Cost.pdf
# courts
calc_total_court_costs <- function(m.rearrested_or_not, m.crime_type){
  
  ifelse(m.rearrested_or_not == 1, 
         prison_terms$court_cost[prison_terms$offense_type == m.crime_type],
         0)
  
}


# cops
calc_total_policing_costs <- function(m.rearrested_or_not, m.crime_type){
  
  ifelse(m.rearrested_or_not == 1, 
         prison_terms$police_cost[prison_terms$offense_type == m.crime_type],
         0)
  
}


# IF out of prison, in a shelter???
# Not sure how to think of this.
# Low end estimate: 386 out of 17,039 on parole or probation are in CCCs (2.2%)
# High end estimate: the frequent users in DC spent 25% in a shelter.
# Medium but conservative: "In CA, 10% of state’s parolees are homeless"
# http://www.urban.org/sites/default/files/alfresco/publication-pdfs/412504-Frequent-Users-of-Jail-and-Shelter-Systems-in-the-District-of-Columbia-An-Overview-of-the-Potential-for-Supportive-Housing.PDF
# http://www.endhomelessness.org/page/-/files/1101_file_Cho_Presentation.pdf
define_shelter_days <- function(m.is_in_prison){
  
  ifelse(m.is_in_prison == 1, 0,
         sample(x = (c(1,0)), 1, prob = c(.1, .9)))
  
}


# When they are on parole, most will not go to a homeless shelter, but rather a CCC
# The cost to operate those are in the CPD data from AP&P. 
# There are a lot of possibilities for calculating the costs when they are off parole. Yvette sent data on Palmer Court and other shelters. Julia sent some data on the cost to operate and number of folks in halfway houses. There is a huge variety in costs. I think Yvette's estimate is a good place to start, which is 10.10. 
calc_shelter_costs <- function(m.is_in_shelter, m.is_on_parole){
  
  shelter_cost <- 10.10
  CCC_cost <- mean(54, 102, 75, 143, 95)
  
  ifelse(m.is_in_shelter == 1 & m.is_on_parole == 0, shelter_cost * 30.5,
         ifelse(m.is_in_shelter == 1 & m.is_on_parole == 1, CCC_cost * 30.5,
         0))
  
}


# Adult parole costs (this group will not get probation)
# This data comes from the cost per day pdf given to me
calc_total_app_costs <- function(m.is_on_parole){
  
  # Based on the cost per day data
  # But minus the shelter,i.e., CCCs, because those are only used by a portion
  
  cost_per_day <- (.23 + .62 + .51) + #admin costs
    mean(7.06, 6.5, 5.48, 8.34, 6.13) + # Avg of Region costs
    .5 # community programs
    
  
  ifelse(m.is_on_parole == 1, cost_per_day * 30.5, 0)
  
}


# DWS & DHS
# average costs for DWS - food stamps, abd, WIOA, etc - are 75$ for adult males in the program
# It's probably safe to assume that the average COD parolee uses that
# DHS spends an average of $3000 per client on Substance use and $3000 per client on Mental Health, including inpatient, outpatient, crisis care, etc.
# Probably safe to assume there is some overlap here, but not total overlap. Conservatively, we will say $3000 total per parolee / yr, and hope that will make up for the fact that some probably do not use it at all. 
calc_total_DWS_DHS_costs <- function(m.is_in_prison){
  
  DWS <- 75
  DHS <- 3000 / 12
  
  ifelse(m.is_in_prison == 0, DHS + DWS, 0)
  
}




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
                   is_on_parole=numeric(0),
                   parole_sentence=numeric(0),
                   marginal_prison_costs=numeric(0),
                   total_prison_costs=numeric(0),
                   total_court_costs=numeric(0),
                   total_policing_costs=numeric(0),
                   is_in_shelter=numeric(0),
                   total_shelter_costs=numeric(0),
                   total_parole_costs=numeric(0),
                   total_DWS_DHS_costs=numeric(0))
  
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
    
    # And a parole sentence for when they get out
    m.parole_sentence <- ifelse(m.month == 1, 18, #the national average
                                ifelse(m.rearrested_or_not == 1, 
                                       m.prison_sentence * .4,
                                       tmp.parole_sentence))
    
    # Is on parole??
    m.is_on_parole <- if_else(m.is_in_prison == 1, 0,
                              ifelse(m.months_free <= m.parole_sentence, 1, 0))
    
    ### Costs of crimes commited
    # Prison
    m.marginal_prison_costs <- calc_marginal_prison_costs(m.is_in_prison, m.month)
    m.total_prison_costs <- calc_total_prison_costs(m.is_in_prison)
    
    # Courts
    m.total_court_costs <- calc_total_court_costs(m.rearrested_or_not, m.crime_type)
    
    # Cops
    m.total_policing_costs <- calc_total_policing_costs(m.rearrested_or_not, m.crime_type)
    
    # Shelters 
    m.is_in_shelter <- define_shelter_days(m.is_in_prison)
    m.total_shelter_costs <- calc_shelter_costs(m.is_in_shelter, m.is_on_parole)
    
    # Parole
    m.total_parole_costs <- calc_total_app_costs(m.is_on_parole)
    
    # DWS
    m.total_DWS_DHS_costs <- calc_total_DWS_DHS_costs(m.is_in_prison)
    
    
    # add month to the data frame
    df[month,] <- 
      c(m.month, m.months_free, m.rearrested_or_not, m.prison_sentence, 
        m.crime_type, m.is_in_prison, m.is_on_parole, m.parole_sentence, 
        m.marginal_prison_costs, m.total_prison_costs, m.total_court_costs, 
        m.total_policing_costs, m.is_in_shelter, m.total_shelter_costs, 
        m.total_parole_costs, m.total_DWS_DHS_costs)
    
    
    # Make temporary vector for determining months free in the next pass
    tmp.months_free <- if_else(m.rearrested_or_not == 1, 0, m.months_free)
    
    # Temporary parole sentence
    tmp.parole_sentence <- m.parole_sentence
    
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
                   arrests=numeric(0),
                   total_costs=numeric(0),
                   avg_total_cost_per_yr=numeric(0))
  
  # loop through each month and see what happens			
  for (agent in 1:n_agents) {
    
    ### generate action for each person ###
    single_agent <- sim_single_agent(n_months)
    
    # Sum things that happend to the agent
    a.prison_time <- sum(as.numeric(single_agent$is_in_prison))
    a.months_free <- sum(as.numeric(single_agent$months_free))
    a.total_prison_costs <- sum(as.numeric(single_agent$total_prison_costs))
    a.arrests <- sum(as.numeric(single_agent$rearrested_or_not))
    
    # Get total costs
    total_costs <- single_agent %>% 
      select(starts_with("total"))
    
    total_costs <- sapply(total_costs, as.numeric)
    a.total_costs <- sum(colSums(total_costs))
    
    a.avg_total_cost_per_yr <- a.total_costs / (n_months / 12)
    
    
    # add month to the data frame
    df[agent,] <- 
      c(a.prison_time, a.months_free, a.total_prison_costs, a.arrests, a.total_costs,
        a.avg_total_cost_per_yr)
    
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

# Costs
hist(multi_agent_test$total_costs)
mean(multi_agent_test$total_costs)

hist(multi_agent_test$avg_total_cost_per_yr)
mean(multi_agent_test$avg_total_cost_per_yr)
summary(multi_agent_test$avg_total_cost_per_yr)




#### Run the model using data extrapolated to represent JRI changes ####

### First load and change the sentencing data
prison_terms <- read.csv("./clean_data/utah_cod_recidivism_rates.csv", stringsAsFactors = FALSE) %>% 
  mutate(mean_time_served = round(mean_time_served))

# I do these so that I can add a blank in the function below
prison_terms[8,1] <- ""
prison_terms[8,4] <- 0

# One big change from the JRI is that prison sentences for technical violations are capped
# So we change the 15-month mean to a 2-month one
# page 39: http://www.utah.gov/pmn/files/172049.pdf
prison_terms$mean_time_served[prison_terms$offense_type == "Parole_Violation"] <- 2


### Additionally, we would expect JRI to decrease recidivism. ###
# I heard one estimate of 5%. That seems low, but conservative
# .157 is the increase in the baseline for the COD population
# .05 is the recidivism reduction
survival_rates <- load_survival_data(.157, .05)

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

# Costs
hist(multi_agent_test$total_costs)
mean(multi_agent_test$total_costs)

hist(multi_agent_test$avg_total_cost_per_yr)
mean(multi_agent_test$avg_total_cost_per_yr)
summary(multi_agent_test$avg_total_cost_per_yr)





