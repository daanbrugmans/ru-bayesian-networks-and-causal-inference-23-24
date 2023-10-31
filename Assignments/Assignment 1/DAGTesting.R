rm(list=ls())
setwd("C:/Users/Daan/Documents/Projecten/ru-bayesian-networks-and-causal-inference-23-24")

remotes::install_github("jtextor/dagitty/r")

library(dagitty)
library(fastDummies)
library(CCP)

path_to_dataset = paste(getwd(), "/Assignments/Data/banking-dataset-normalized.csv", sep="")
banking_dataset <- read.csv(path_to_dataset, sep=",", stringsAsFactors=FALSE)

dag_initial <- dagitty('dag {
Age [pos="-1.441,-1.094"]
AnnualBalance [pos="-1.269,-0.713"]
CurrentCampaignCalls [pos="-0.824,-0.388"]
HasDefault [pos="-1.198,-0.597"]
CallDuration [pos="-0.935,-0.664"]
EducationLevel [pos="-1.077,-1.102"]
HasHousingLoan [pos="-1.412,-0.549"]
JobCategory [pos="-1.198,-0.995"]
HasPersonalLoan [pos="-1.002,-0.776"]
MaritalStatus [pos="-1.351,-0.791"]
PreviousCampaignOutcome [pos="-1.226,-0.236"]
PreviousCampaignsCalls [pos="-0.983,-0.232"]
HasSubscribedToDeposit [outcome,pos="-1.231,-0.403"]
Age -> EducationLevel
Age -> JobCategory
Age -> MaritalStatus
AnnualBalance -> HasDefault
AnnualBalance -> HasHousingLoan
AnnualBalance -> HasPersonalLoan
AnnualBalance -> HasSubscribedToDeposit
CurrentCampaignCalls -> CallDuration
CurrentCampaignCalls -> HasSubscribedToDeposit
HasDefault -> HasPersonalLoan
HasDefault -> HasSubscribedToDeposit
CallDuration -> HasSubscribedToDeposit
EducationLevel-> JobCategory
HasHousingLoan -> HasDefault
HasHousingLoan -> HasSubscribedToDeposit
JobCategory -> AnnualBalance
HasPersonalLoan -> HasSubscribedToDeposit
MaritalStatus -> AnnualBalance
PreviousCampaignOutcome -> HasSubscribedToDeposit
PreviousCampaignsCalls -> CurrentCampaignCalls
PreviousCampaignsCalls -> PreviousCampaignOutcome
}
')

plot(dag_initial)
impliedConditionalIndependencies(dag_initial)
localTests(dag_initial, banking_dataset, type="cis.pillai")
