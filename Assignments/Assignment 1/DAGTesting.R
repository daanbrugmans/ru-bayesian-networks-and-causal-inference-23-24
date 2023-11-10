rm(list=ls())
setwd("C:/Users/Daan/Documents/Projecten/ru-bayesian-networks-and-causal-inference-23-24")

remotes::install_github("jtextor/dagitty/r")

library(dagitty)
library(fastDummies)
library(CCP)

path_to_dataset = paste(getwd(), "/Assignments/Data/banking-dataset-normalized.csv", sep="")
banking_dataset <- read.csv(path_to_dataset, sep=",", stringsAsFactors=T)

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

independency_tests <- localTests(dag_initial, banking_dataset, type="cis.pillai")
independency_tests
plotLocalTestResults(independency_tests)

get_canonical_correlations <- function(dag, dataset) {
  canonical_correlations <- c()

  for(variable_name in names(dag_initial)) {
    for(parent in parents(dag_initial,variable_name)) {
      other_parents <- setdiff(parents(dag_initial, variable_name), parent)
      tst <- ciTest(
        X=variable_name,
        Y=parent,
        Z=other_parents,
        banking_dataset,
        type="cis.pillai"
      )

      canonical_correlations <- rbind(
        canonical_correlations,
        data.frame(
          list(
            X=parent,
            A="->",
            Y=variable_name,
            cor=tst[,"estimate"],
            p=tst[,"p.value"]
          )
        )
      )
    }
  }

  canonical_correlations
}

get_dag_with_canonical_correlations <- function(dag, canonical_correlations) {
  cancor_edge_coefficients <- paste(
    canonical_correlations$X,
    canonical_correlations$A,
    canonical_correlations$Y,
    "[beta=",signif(canonical_correlations$cor,2),"] ", collapse="\n"
  )

  dag_with_coefficients <- dagitty(cancor_edge_coefficients)
  coordinates(dag_with_coefficients) <- coordinates(dag)

  dag_with_coefficients
}

canonical_correlations <- get_canonical_correlations(dag_initial, banking_dataset)
dag_initial_with_cancor <- get_dag_with_canonical_correlations(dag_initial, canonical_correlations)
plot(dag_initial_with_cancor, show.coefficients=T)
