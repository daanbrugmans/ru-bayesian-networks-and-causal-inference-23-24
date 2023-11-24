rm(list=ls())
setwd("C:/Users/Daan/Documents/Projecten/ru-bayesian-networks-and-causal-inference-23-24")

library(dagitty)
library(ivreg)

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
PreviousCampaignOutcome [pos="-.15,-.75"]
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

  # ah
skunk <- ivreg(HasSubscribedToDeposit ~ PreviousCampaignOutcome | PreviousCampaignsCalls, data = banking_dataset)
summary(skunk)
