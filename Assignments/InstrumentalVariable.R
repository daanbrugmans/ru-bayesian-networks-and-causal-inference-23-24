rm(list=ls())
setwd("C:/Users/Daan/Documents/Projecten/ru-bayesian-networks-and-causal-inference-23-24")

library(dagitty)

  # Load banking dataset
path_to_dataset = paste(getwd(), "/Assignments/Data/banking-dataset-normalized.csv", sep="")
banking_dataset <- read.csv(path_to_dataset, sep=",", stringsAsFactors=T)

  # Load altered version of DAG
dag_altered <- dagitty('dag {
Age [pos="0,-2"]
AnnualBalance [pos="-.3,-1.65"]
CurrentCampaignCalls [pos="-.35,-1"]
HasDefault [pos="-0.45,-1.5"]
CallDuration [pos="-.35,-.5"]
EducationLevel [pos="-0.15, -2"]
HasHousingLoan [pos="-0.140,-1.220"]
JobCategory [pos="0,-1.5"]
HasPersonalLoan [pos="-0.3,-1.3"]
MaritalStatus [pos="-0.2,-1.65"]
PreviousCampaignOutcome [exposure,pos="-.15,-.75"]
PreviousCampaignsCalls [pos="-.25,-1.15"]
HasSubscribedToDeposit [outcome,pos="0,-.5"]
Age -> AnnualBalance
Age -> EducationLevel
Age -> JobCategory
Age -> HasHousingLoan
Age -> MaritalStatus
AnnualBalance -> HasDefault
AnnualBalance -> HasPersonalLoan
CurrentCampaignCalls -> CallDuration
HasDefault -> HasPersonalLoan
CallDuration -> HasSubscribedToDeposit
EducationLevel -> AnnualBalance
EducationLevel -> JobCategory
HasHousingLoan -> HasSubscribedToDeposit
JobCategory -> HasHousingLoan
JobCategory -> HasPersonalLoan
JobCategory -> HasSubscribedToDeposit
JobCategory -> MaritalStatus
JobCategory -> PreviousCampaignOutcome
MaritalStatus -> HasHousingLoan
PreviousCampaignOutcome -> CurrentCampaignCalls
PreviousCampaignOutcome -> HasHousingLoan
PreviousCampaignOutcome -> HasSubscribedToDeposit
PreviousCampaignsCalls -> PreviousCampaignOutcome
}
')
plot(dag_altered)

  # Identify instrumental variables in DAG
exposures(dag_altered) <- "PreviousCampaignOutcome"
outcomes(dag_altered) <- "HasSubscribedToDeposit"
instrumentalVariables(dag_altered) # IV found: PreviousCampaignsCalls

  # Calculate biased regression line for IV
biased_outcome_regression <- lm(HasSubscribedToDeposit ~ PreviousCampaignOutcome + PreviousCampaignsCalls, data = banking_dataset)
biased_outcome_regression

  # Calculate unbiased regression line for IV by d-separating on other paths from IV to outcome
    # The IV must only influence the outcome through the exposure.
    # That is, IV -> exposure -> outcome must be the only path through which IV can reach outcome.
    # All other paths from IV to outcome must be d-separated out.
    # This is done by calculating the regression line of the exposure given the IV and variables to be d-separated.
    # This adjusts the exposure to the d-separated variables.
    # The predictions of this regression are used as a replacement for the original exposure.variable.
adjusted_exposure_regression <- lm(PreviousCampaignOutcome ~ PreviousCampaignsCalls + CurrentCampaignCalls + HasHousingLoan + JobCategory, data = banking_dataset)
adjusted_exposure <- predict(adjusted_exposure_regression)

unbiased_outcome_regression <- lm(HasSubscribedToDeposit ~ adjusted_exposure + CurrentCampaignCalls + HasHousingLoan + JobCategory, data = banking_dataset)
unbiased_outcome_regression

iv_estimate <- coef(unbiased_outcome_regression)[2]
iv_estimate
