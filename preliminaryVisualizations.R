#####################################################################################
#   DATA CRUNCHERS - Exploratory Visualizations of the Volunteer Program Questions  #
#####################################################################################

Cases <- read.csv("Case.csv")
Contacts <- read.csv("Contact.csv")
Users <- read.csv("User.csv")

# Create dataframes that subset useful variables from each data set (more to be added I'm sure, but for the sake of memory...)

userVars <- c("Id","CompanyName","Division","Department","City","ContactId","UserRoleId", "CreatedById")
subUser <- Users[userVars]
contactVars <- c("Id","AccountId","OwnerId","CreatedById","Marital_Status__c","a__Gender__c", "Disability_percentage_60_or_above__c", "Desired_Earnings_Type__c","Date_Submitted_for_Hire__c","Hire_Heroes_USA_Confirmed_Hire__c","Reserves_National_Guard__c","Military_Branch__c","Current_Volunteer__c")
subContact <- Contacts[contactVars]

# Merge dataframes and use new dataframe as needed

Cases_Contact <- merge(Cases, subContact, by="OwnerId") 

#---------------#
# Some examples #
#---------------#

# Are there certain volunteer activities that are more effective than others?
# Grouped Barplot of 'Reason' (type of volunteer activity) and Volunteer Status
ReasonStatusBar <- table(Case$Reason, Case$Status)
barplot(ReasonStatusBar, main="Type of Volunteer Activity and the Status",
        xlab="Status", col=c("darkblue","red", "green"),
        legend = rownames(ReasonStatusBar), beside=TRUE)

# Is there a specific demographic profile of clients that use volunteer services?
# stacked Barplot of Veteran's Gender and Volunteer Status
gender <- table(Cases_Contact$a__Gender__c, Cases_Contact$Status)
barplot(gender, main="Gender and the Volunteer Status",
        xlab="Status", col=c("darkblue","red", "green"),
        legend = rownames(gender))
Cases <- read.table("Case.xlsx")
Users <- read.table("user1.xlsx")
Cases_User <- merge(Case, user1, by="Id") 

#Sample code for data retrieval and data cleansing
tr0<-read.csv(file="file.csv",stringsAsFactors = FALSE)
tr1<-dplyr::select(tr0,-var1,-var2,-var3,.....)
tr1
View(tr1)
write.csv(tr1, file = "file.csv",row.names=FALSE)

#"IsClosed" column of "Task.csv" was having values in 0's and 1's. They were convereted to Yes/No
# by using below code for easy visualization in tableau.
convertIsClosed<-function(s)
{
  vYes<-rep("Yes",length(s))
  vNo<-rep("No",length(s))
  
  return(ifelse(s==1,vYes,vNo))
}
tr2<-mutate(tr1,IsClosed=convertIsClosed(IsClosed))
View(tr2)
head(tr2)


#The following "Hmisc" package of R was applied on "Discussion_Topic__c" variable of Task.csv to figure out probabilities of clients' 
#participation in various hiring/application process. Result of the applied function is printed below.

Hmisc::describe(tr2[, c("Discussion_Topic__c")])

#Discussion_Topic__c 
#n missing  unique 
#141064  324105      17 

#Confirmed Hire / Hire Follow-Up (4756, 3%) 
#DD-214 / Original Resume Needed (8782, 6%), 
#Follow-Up (51428, 36%) 
#General Counseling (9859, 7%), 
#Initial Assessment (25916, 18%), 
#Inquiry (763, 1%) 
#Interviewing (9128, 6%), 
#Job Board (2996, 2%), 
#Job Match (80, 0%), 
#Job Sourcing (6309, 4%) 
#LinkedIn (1686, 1%), 
#Military / Veteran Benefits (195, 0%), 
#Networking (780, 1%) 
#O2O Interview Coordination (67, 0%), 
#Other (6539, 5%) 
#Resume Revision (11726, 8%), 
#Salary / Job Offer Negotiation (54, 0%) 
