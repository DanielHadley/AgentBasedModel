#### Created by Daniel to estimate the costs of COD offenders  ####


setwd("~/Documents/2016-07_Utah_Recidivism/")

library(dplyr)
library(ggplot2)
library(ggplot2)
library(readxl)
library(plm) # pooled regression for cops


## Load Data ##

# Helpful doc: https://le.utah.gov/lrgc/briefings/bagelsbriefings.inmaterecidivism.pdf


### !! Areas for which I do not have data:
# Emergency room visits
# Psychiatric care
# State-provided medicine
# Housing 
# 1/2 way housing
# Substance abuse and mental health programs
# Jail vs prisons & rates pre / post treatment
## Rates of use for these services by COD individuals
## Estimated reductions in these services from the treatment


## Shelters
# From: Yvette Woodland <ywoodland@utah.gov>
# "Cost per night (total shelter budget including emergency services and case management) - $17.97 per night
# 
# Cost per night (basic shelter - just emergency shelter, no case management) - $10.10 per night
# 
# Palmer Court cost per night (includes case management and facility operations - does not include rental subsidy) - $16.80 per night."

### My question : what is the shelter utilization rate by this group? Are these costs borne by the state government? Here are shelter days from another project: http://www.urban.org/sites/default/files/alfresco/publication-pdfs/412504-Frequent-Users-of-Jail-and-Shelter-Systems-in-the-District-of-Columbia-An-Overview-of-the-Potential-for-Supportive-Housing.PDF

# Also this : https://jobs.utah.gov/housing/scso/documents/Utah_Ten_Year_Plan_May_12-2008.pdf


## Emergency room
# 


# DWS costs - food stamps, subsidies, job training
dws <- read_excel("./clean_data/Adult Males without Dependents PfS Data.xlsx")


# This data comes from Rob at the Department of Corrections
# I liberated from pdf using tabula
# I cleaned it up some and dropped all of the total columns and other clutter
prisons_all <- read.csv("./clean_data/Hadley_FY 2015 DIO CPD Master.csv")


# Adult parole and probations
parole <- read.csv("./clean_data/Hadley-FY_2015_APP_CPD_Master.csv")


# This data came from Sofia at the state
# http://www.justice.utah.gov/Documents/CCJJ/Cost%20of%20Crime/Utah%20Cost%20of%20Crime%202012%20-%20Methods%20Review%20Cost.pdf
cops <- read_excel("./clean_data/Police_Sheriff_data_Utah2012.xlsx")
courts <- read_excel("./clean_data/Prosecutor_data_Utah2012.xlsx")



### The data frame I need ###
Type <- c("Cops", "Courts", "DWS", "Parole", "Prisons")
Unit_Cost_Per_Year <- c(rep(NA, 5))
Marginal_Cost_Per_Year <- c(rep(NA, 5))

Costs_Final <- data.frame(Type, Unit_Cost_Per_Year, Marginal_Cost_Per_Year)




#### Prisons ####

# What I have learned about the data (from Rob at DOC):
# CPD = cost per day
# The Division of Institutional Operations or DIO manages the State of Utah's two primary correctional facilities, including the Central Utah Correctional Facility and the Utah State Prison. The state also sends more than 1,000 inmates out to county jail facilities around Utah through a jail contracting program.
# AP&P = Adult Probation and Parole
# IPP = The Inmate Placement Program

# All data for prisoners in the state prison
# all charges once they are 'given to us'
# Not showing anything that would be in county jails
# IPP: ~6800 inmates, but not enough space. Contract with jails and we pay them a per day fee. They cover a food cost. 
# Daily contract rate: ~$45
# Still, not a great marginal 
# 10% of inmates cost us 90% of our medical

# R&O, Wasatch, etc. are all housing units within the prison  

# Capital costs: it includes things like a boiler or building 
# A new building would go through the dept. of construction - they do bond out, but most are based on a direct appropriation 
# Draper admin was built in 2001, and we are paying the bond
# The "building and land" is office space where we house DIO administration
# We are paying a bond payment, and we charge DIO and IPP
# Maintenance - we've got some facilities 

# IF you could close a housing unit, you might see a large savings
# If you have 10 or 20 inmates, you probably need the same staff

# Typically, our top medical we do not send to the county jails. they are housed at the state prisons. 


# make everything numeric
prisons_all <- as.data.frame(apply(prisons_all, 2, function(x)gsub(',', '',x)))

for(i in c(2:ncol(prisons_all))) {
  prisons_all[,i] <- as.numeric(as.character(prisons_all[,i]))
}


# clean
prisons_all$X <- gsub("Motor Pool Vehicles \x89ې OS & M", 
                   "Motor Pool Vehicles", prisons_all$X)



# Let's drop the first two rows, which only contain number of FTEs and inmates
prisons <- prisons_all[3:nrow(prisons_all),]




#### Prisons: Initial Analysis ####
# Plot the specific categories
cost_by_category <- prisons %>%
  mutate(Cost = rowSums(.[2:27], na.rm = T)) %>%
  select(X, Cost) %>%
  arrange(Cost)


# c <- ggplot(data = cost_by_category, aes(x = X, y = Cost))
# c + geom_bar(stat="identity") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   scale_y_continuous(labels = scales::dollar) +
#   ggtitle("Cost By Category")
# 
# ggsave("./plots/Cost_By_Category.png")



# Plot the broad categories
cost_by_activity <- prisons[,2:27]
cost_by_activity[nrow(cost_by_activity) + 1, ] <-
  colSums(cost_by_activity, na.rm = T)

cost_by_activity <- cost_by_activity[39,]

cost_by_activity <- as.data.frame(t(cost_by_activity))

cost_by_activity  <- add_rownames(cost_by_activity , "Activity")

colnames(cost_by_activity) <- c("Activity","Cost")


# c <- ggplot(data = cost_by_activity, aes(x = Activity, y = Cost))
# c + geom_bar(stat="identity") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   scale_y_continuous(labels = scales::dollar) +
#   ggtitle("Cost By Activity")
# 
# ggsave("./plots/Cost_By_Activity.png")


### WOW: so much of the budget is related to personnel costs


# It looks like the cost / prisoner is in the right ballpark
# http://www.vera.org/files/price-of-prisons-utah-fact-sheet.pdf
total <- sum(cost_by_activity$Cost) + 7900000 #the $7.9 are indirect calculated by Vera
cost_per_prisoner <- total / 6813 


# Add it in to the main dataframe
Costs_Final$Unit_Cost_Per_Year[5] <- cost_per_prisoner




#### Prisons: a conservative estimate of short-run marginal costs ####
# Limited to variable costs - leaving out fixed and step-fixed
# take this as my guide: 
# http://archive.vera.org/sites/default/files/resources/downloads/marginal-costs-guide.pdf


prisons_variable <- prisons %>%
  
  # Take out strictly administrative costs - the first 4 columns
  select(-Executive.Director : - Motor.Pool) %>% 
  
  # I also take out IPP: it is unlikely that our prisoners will be placed in the local jails. Those costs tend to be lower
  select(-IPP, -IPP.Medical)


# Let's see all categories first
cat(paste(shQuote(prisons$X, type="cmd"), collapse=", "))

# Now a vector with fixed costs to take out of the rest
fixed_costs <- c("Buildings & Land", "Capital Lease Expense", "Communications", "In State Travel", "Janitorial Service Contracts", "Maintenance", "Out of State Travel", "Personnel", "Photocopy Expenses", "Rent", "Risk Management", "Security/Surveillance Equip.", "Staff Development", "State Funded Programs", "Utilities")

# Notice the ! to negate the in operator
prisons_variable <- prisons_variable %>% 
  filter(!X %in% fixed_costs)

sum_variabe <- prisons_variable %>% select(-X) %>% as.vector() %>% sum(na.rm = T)

variable_cost_per_offender <- sum_variabe / (6813 - 1596) # 1596 is the IPP patients

# $5k 
# This is comporable to the short-run marginal cost for Washington state 
# It is higher because the treatment group will likely be higher needs, and therefore not eligible for things like IPP

# Add it in to the main dataframe
Costs_Final$Marginal_Cost_Per_Year[5] <- variable_cost_per_offender




#### Prisons: the hard part: long-run marginal costs ####
# The difficult part is figuring out how much the personnel budget could fall
# Personnel is the largest driver
# The other hard thing is the capital costs for new prison space, which are not put into this budget


prisons_variable_long_term <- prisons %>%
  
  # This time we will keep in administrative costs - the first 4 columns
  # But we will scale them with the code below
  # select(-Executive.Director : - Motor.Pool) %>% 
  
  # I also take out IPP: it is unlikely that our prisoners will be placed in the local jails. Those costs tend to be lower
  select(-IPP, -IPP.Medical)


# Let's see all categories first
cat(paste(shQuote(prisons$X, type="cmd"), collapse=", "))

# Now a vector with fixed costs to take out of the rest
# Notice this time Personnel is not on the list
fixed_costs <- c("Buildings & Land", "Capital Lease Expense", "Communications", "In State Travel", "Janitorial Service Contracts", "Maintenance", "Out of State Travel", "Photocopy Expenses", "Rent", "Risk Management", "Security/Surveillance Equip.", "Staff Development", "State Funded Programs", "Utilities")

# Notice the ! to negate the in operator
prisons_variable_long_term <- prisons_variable_long_term %>% 
  filter(!X %in% fixed_costs)


## There's no surefire way to do this, but I am estimating that about 1/2 of the personnel and administrative costs are totally fixed, while the other 1/2 can probably scale with the number of inmates
# The scaler:
my_scaler <- .5

prisons_variable_long_term <- prisons_variable_long_term %>% 
  mutate(Executive.Director = Executive.Director * my_scaler,
         Department..Administration = Department..Administration * my_scaler,
         Division..Administration = Division..Administration * my_scaler,
         Motor.Pool = Motor.Pool * my_scaler)


prisons_variable_long_term[22, 4:25] <- prisons_variable_long_term[22, 4:25] * my_scaler


sum_variable_long_term <- prisons_variable_long_term %>% 
  select(-X) %>% 
  as.vector() %>% 
  sum(na.rm = T) 


variable_cost_per_offender_long_term <- sum_variable_long_term / (6813 - 1596) # 1596 is the IPP patients

## 16k: this is also in line with what the guide mentions for Washington state


rm(cost_by_activity, cost_by_category, prisons_all, prisons_variable, prisons_variable_long_term, cost_per_prisoner, c)




#### Cops ####
# Essentially the way Sofia got costs per crime was by using the police data here and regressing crimes on cost

options(scipen = 999)

cops <- cops[1:510,1:16]


# Multiple Linear Regression Example 

# cops_for_regression <- cops %>%
#   select(-Total_Crime_Index, -Violent, -Property) %>%
#   rename(Costs_2010 = `Police ($2010)`)

cops_for_regression <- cops %>% 
  select(-Homicide : -Total_Crime_Index) %>% 
  rename(Costs_2010 = `Police ($2010)`,
         is_city = `county=1, city=2`)


cop_cost_regression <- plm(Costs_2010 ~ is_city + Violent + Property + Pop, data = cops_for_regression, index = c("Agency", "Year"), model="pooling")
summary(cop_cost_regression)

# I'm thinking the marginal cost are 15% of total
violent_crime_marginal_cost <- cop_cost_regression$coefficients["Violent"] * .15
property_crime_marginal_cost <- cop_cost_regression$coefficients["Property"] * .15

## I can't seeem to replicate Sofia's results. Here are her numbers:
# violent_crime_cost_sof <- 4509
# property_crime_cost_sof <- 880


# One portion of the cost-benefit model requests the cost of a crime. This is a hard question, but basically I will calculate the expected value of a crime as the marginal cost of each crime type mutiplied by the it's value in the national frequency table for recidivism crime types.
crime_frequencies <- read.csv("./clean_data/ncrp0908.csv", stringsAsFactors = F) %>% 
  filter(X != "", 
         X != "Number of releases",
         X != "All offenses") %>% 
  rename(type = X,
         percent = X.3) %>% 
  mutate(percent = as.numeric(percent) / 100) %>% 
  select(type, percent)

violent_crime_types <- c("Homicide", "Kidnapping", "Rape", "Other sexual assault", "Assault", "Other violent")

crime_frequencies$expected_cost <- ifelse(
  crime_frequencies$type %in% violent_crime_types, 
  crime_frequencies$percent * violent_crime_marginal_cost, 
  crime_frequencies$percent * property_crime_marginal_cost)

expected_marginal_cost_of_crime <- sum(crime_frequencies$expected_cost)


#### Courts ####
# Essentially the way Sofia got costs per crime was by using the data here and regressing crimes on cost

# But I think that this data below does not capture murder and other felonies:
# "In the state of Utah, felony crimes are primarily prosecuted by county attorneys in the district courts."

options(scipen = 999)

courts <- courts[1:144,]


# Multiple Linear Regression Example 

# courts_for_regression <- courts %>% 
#   select(-Total_Crime_Index, -Violent, -Property) %>% 
#   rename(Costs_2010 = `Police ($2010)`)

courts_for_regression <- courts %>% 
  rename(Costs_2010 = `Prosecutor ($2010)`)


random_courts <- plm(Costs_2010 ~ Violent_no_murder + Property + Pop + Homicide, data = courts_for_regression, index = c("County", "Year"), model="pooling")
summary(random_courts)

# These values really don't make sense, so I am going to use Sofia's numbers and scale by 15%:
murder_marginal_cost_prosecute <- 62037 * .15
violent_crime_marginal_cost_prosecute <- 5443 * .15
property_crime_marginal_cost_prosecute <- 2284 * .15


# One portion of the cost-benefit model requests the cost of a crime. This is a hard question, but basically I will calculate the expected value of a crime as the marginal cost of each crime type mutiplied by the it's value in the national frequency table for recidivism crime types.

violent_crime_types_no_murder <- c("Kidnapping", "Rape", "Other sexual assault", "Assault", "Other violent")


crime_frequencies$expected_cost_prosecute <- ifelse(
  crime_frequencies$type == "Homicide", 
  murder_marginal_cost_prosecute * crime_frequencies$percent,
  ifelse(crime_frequencies$type %in% violent_crime_types_no_murder,
         violent_crime_marginal_cost_prosecute * crime_frequencies$percent,
         crime_frequencies$percent * property_crime_marginal_cost_prosecute))
         

expected_marginal_cost_of_crime_prosecute <- 
  sum(crime_frequencies$expected_cost_prosecute)