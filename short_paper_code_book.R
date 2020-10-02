#First, download the exonerations dataset as exonerations


exonerations <- exonerations[-c(1,2,3,4,5,6), ] #deletes null states

#Cleaning the data to get rid of Nones in the date_recordinginterrog column, and replacing them with 2020
#which is meant to represent that is hasn't happened yet
#start by importing the exonerations dataset, entitled exonerations
exonerations$date_recordinginterrog <- as.character(exonerations$date_recordinginterrog)
exonerations$date_recordinginterrog[exonerations$date_recordinginterrog == "None"] <- "2020"
exonerations$date_recordinginterrog <- as.integer(exonerations$date_recordinginterrog)

#make it a data table
library(data.table)
exo_table <- as.data.table(exonerations, keep.rownames=FALSE)

#Using the eventStudy package to do my analysis
#install
install.packages("devtools")
devtools::install_github("hadley/devtools")
devtools::install_github("setzler/eventStudy/eventStudy")

#Calculate; be patient, it took me about 7 minutes to run through
library(eventStudy)
results <- ES(long_data = exo_table, outcomevar="fc", 
              unit_var="state", cal_time_var="convicted", 
              onset_time_var="date_recordinginterrog", cluster_vars="state", never_treat_action = "exclude")
#Plot results
ES_plot_levels(results, lower_event = -3, upper_event = 5) + ylab("Mean of the Outcome")
ES_plot_ATTs(results, lower_event = -3, upper_event = 5) + ylab("Mean of the Outcome")

#summarize results
summary(results)

#running lm() to doubble check results
my_model <- lm(fc~date_recordinginterrog + state + convicted, data = exonerations)
summary(my_model)
stargazer::stargazer(results, title="Results", align=TRUE)

#Installing stargazer to get latex importable code
install.packages("stargazer")
library(stargazer)
stargazer::stargazer(my_model, title="Results", align=TRUE)