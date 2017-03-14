#################################################################################################
#  Team DATA CRUNCHERS 
#  Volunteer Program - Question 2
#  Salesforce Datasets 
#################################################################################################

Cases <- read.csv("Case.csv")
Contact <- read.csv("Contact.csv")

keptCases <- c("ContactId", "Status", "Reason", "Date_of_Original_Request__c","CreatedDate",
               "ClosedDate","Type")
statCases <- Cases[keptCases]

keptContact <- c("Active_Color__c","ContactId","Confirmed_Hired_Date__c","Date_of_first_contact__c"
                 ,"a__Gender__c", "Service_Branch__c", "Security_Clearance_Description__c", "Highest_Level_of_Education_Completed__c",
                 "Status__c","Disability_Rating__c", "HHUSA_Workshop_Participant__c","Service_Rank__c",
                 "MailingState", "Hired_but_still_active_and_looking__c", "Foreign_Service__c",
                 "Reserves_National_Guard__c", "Enrolled_in_School__c", "Bilingual__c", "Willing_to_Relocate__c")
activeContact <- Contact[keptContact]

#  Full join of data frames (load 'dplyr' package)
Fjoin <- full_join(activeContact, statCases, by = c('ContactId'))

#  How many of all the contacts have requested a volunteer to work with?

Fjoin$Type_int <- ifelse(Fjoin$Type == "Volunteer Request", 1, 0)
Fjoin$Type_int[is.na(Fjoin$Type)] <- 0
sum(Fjoin$Type_int == 1)
# ans: 1826
#find ratio of volunteer request to total contacts
1826/64549
# ans: 0.02828859 or 2.8%

#------------------------------------------------------------------------------------------------
# Question 2a:  Is there a specific demographic profile of clients that use volunteer services?
#------------------------------------------------------------------------------------------------

# Stepwise regression to find the a good fit for y (Type "Volunteer Request") 
fit <- lm(Type_int ~ a__Gender__c + Service_Branch__c + Security_Clearance_Description__c + Highest_Level_of_Education_Completed__c +
                 Status__c + Disability_Rating__c + HHUSA_Workshop_Participant__c + Service_Rank__c +
                 MailingState + Hired_but_still_active_and_looking__c + Foreign_Service__c +
                 Reserves_National_Guard__c + Enrolled_in_School__c + Bilingual__c + Willing_to_Relocate__c, data=na.omit(Fjoin)) 

#  load MASS package
step <- stepAIC(fit, direction="both")

# display results
step$anova

# Final Model:
# Type_int ~ HHUSA_Workshop_Participant__c + Foreign_Service__c + 
#            Reserves_National_Guard__c + Enrolled_in_School__c + Bilingual__c + Willing_to_Relocate__c

# Parse data into traing and test sets to check final model that stepwise regression found

