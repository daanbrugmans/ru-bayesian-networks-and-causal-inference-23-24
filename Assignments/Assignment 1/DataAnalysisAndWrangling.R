rm(list=ls())
setwd("C:/Users/Daan/Documents/Projecten/ru-bayesian-networks-and-causal-inference-23-24")

library(ggplot2)
library(dplyr)
library(dagitty)
library(bnlearn)
library(lavaan)

path_to_dataset = paste(getwd(), "/Assignments/Data/bank-full.csv", sep="")
unaltered_data <- read.csv(path_to_dataset, sep=";", stringsAsFactors=FALSE)

banking_dataset <- unaltered_data %>%
  rename(Age = age, AnnualBalance = balance, JobCategory = job,
         MaritalStatus = marital, EducationLevel = education,
         HasDefault = default, HasHousingLoan = housing, HasPersonalLoan = loan,
         CallDuration = duration, CurrentCampaignCalls = campaign,
         PreviousCampaignsCalls = previous, PreviousCampaignOutcome = poutcome,
         HasSubscribedToDeposit = y) %>%
  select(Age, AnnualBalance, CallDuration, CurrentCampaignCalls, EducationLevel,
         HasDefault, HasHousingLoan, HasPersonalLoan, JobCategory,
         MaritalStatus, PreviousCampaignsCalls, PreviousCampaignOutcome,
         HasSubscribedToDeposit)

# Age = "Bin into age groups"
# Annual Balance =  "Bin into group, comparing levels of annual household income"
# Call Duration = "Bin into groups, binning must be done carefully!"
# Current Campaign Calls = OK
# Education Level = OK
# Has Default = Convert to binary
# Has Housing Loan = Convert to binary
# Has Personal Loan = Convert to binary
# Job Category = OK
# Marital Status = OK
# Previous Campaign Calls = Convert to categorical
# Previous Campaign Outcome = OK
# Has Subscribed To Deposit = Convert to binary

ggplot(banking_dataset, aes(x=Age)) +
  geom_density()

ggplot(banking_dataset, aes(x=AnnualBalance)) +
  geom_density()

ggplot(banking_dataset, aes(x=CallDuration)) +
  geom_density()

ggplot(banking_dataset, aes(x=CallDuration, fill=HasSubscribedToDeposit)) +
  geom_density(alpha=0.5)

ggplot(banking_dataset, aes(x=CurrentCampaignCalls)) +
  geom_bar()

ggplot(banking_dataset, aes(x=EducationLevel)) +
  geom_bar()

ggplot(banking_dataset, aes(x=HasDefault)) +
  geom_bar()

ggplot(banking_dataset, aes(x=HasHousingLoan)) +
  geom_bar()

ggplot(banking_dataset, aes(x=HasPersonalLoan)) +
  geom_bar()

ggplot(banking_dataset, aes(x=JobCategory)) +
  geom_bar()

ggplot(banking_dataset, aes(x=MaritalStatus)) +
  geom_bar()

ggplot(banking_dataset, aes(x=PreviousCampaignsCalls)) +
  geom_bar()

ggplot(banking_dataset, aes(x=PreviousCampaignOutcome)) +
  geom_bar()

ggplot(banking_dataset, aes(x=HasSubscribedToDeposit, fill=HasSubscribedToDeposit)) +
  geom_bar()
