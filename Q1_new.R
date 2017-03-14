#################################################################################################
#  Team DATA CRUNCHERS 
#  Volunteer Program - Question 1
#  Salesforce Datasets 
#################################################################################################

Cases <- read.csv("Case.csv")
Contact <- read.csv("Contact.csv")
str(Contact, list.len=ncol(Contact))

keptCases <- c("ContactId", "Status", "Reason", "Date_of_Original_Request__c","CreatedDate",
               "ClosedDate")
statCases <- Cases[keptCases]
keptContact <- c("Active_Color__c","ContactId", "Hire_Heroes_USA_Confirmed_Hire__c","Confirmed_Hired_Date__c",
                 "Date_turned_grey__c", "Date_turned_green__c", "Date_of_first_contact__c")
activeContact <- Contact[keptContact]
#-------------------------------------------------------------------------------------------------
# Question 1a:  Does working with a volunteer raise the probability of getting hired?
#-------------------------------------------------------------------------------------------------
#Find the sums of each Active Color status
summary(activeContact$Active_Color__c)
# add Red + Grey + Gray + Blue + Green + Purple (omitting Black and incompleted rows) for the total
7374 + 21 + 3484  + 10602 + 3015  + 16097
# proportion of "Blue" (confirmed hire) out of the total - 7374/40593 or .182 (18.2%)
7374/40593


# Inner_Join the two data frames (install/load the 'dplyr' package first)
joind<-inner_join(statCases, activeContact,by=c('ContactId'))


# Find the proportion of "Completed" status who were "Blue" (confirmed hire)  ans: 479

blue_completed <- subset(joind, Active_Color__c == "Blue" & Status == "Completed")

# Total "Blue" in the Active Color variable     
blue <- which(joind$Active_Color__c == "Blue")
# Total "Completed" in the Status variable     ans: 958
completed <- which(joind$Status == "Completed")

# Ratio of Blue & Completed status to total Completed:  479/958  = .5 or 50%
479/958

# Conducting a Binomial Test to compare the two proportions for equality
prop.test(c(479,7374), c(958,40593))

# It appears that these two groups have a statistically significant difference
# and would not be likely (p<2.2e-16) to happen by chance

# Make some charts, but first replace the "Status" variable (8 levels) with a new 2-level one that
# is simply "Completed" and "Incompled"
joind$Volunteer_Complete <- ifelse(joind$Status == "Completed",
c("Completed"), c("Incomplete"))

# Also do the same with the Active Color... reduce to "Hired" "Not_Hired"
# This will make the chart cleaner and easier to look at
joind$Volunteer_Hired <- ifelse(joind$Active_Color__c == "Blue", 
c("Hired"), c("Not_Hired"))

volTable <- table(joind$Volunteer_Complete, joind$Volunteer_Hired)
barplot(volTable, main="Complete vs Incompled Activity with a Volunteer",
                 xlab="Job Status", col=c("darkblue","red"),
                 legend = rownames(volTable))


#  Now that we know that that working with a volunteer increases the likelihood of getting hired,
#  we should look at the 'Reason' variable (Case data set) to see which of it's three factors
#  (Career Counseling, Federal Resume Review, & Mock Interview) has the most influence on 
#  getting hired. This will be part 'c' of question 1



#------------------------------------------------------------------------------------------------
# Question 1b:  Does working with a volunteer raise the probability of getting hired SOONER?
#------------------------------------------------------------------------------------------------

#  Full join of data frames
Fjoin <- full_join(activeContact, statCases, by = c('ContactId'))

#  First find out the average # of days it took for all vets (who were hired) to get hired from 
#  initial signup at HHUSA in general.
days_H <- as.Date(Fjoin$Confirmed_Hired_Date__c, format='%m/%d/%Y') - 
          as.Date(Fjoin$Date_of_first_contact__c, format='%m/%d/%Y')
daysDiff <- na.omit(days_H)
mean(daysDiff)
# Showing an average 'Time difference of 178.9975 days'from first contact to hire

#  Find the average # of days til hire WITHOUT the subset of vets who used volunteer services

blue_incompleted <- subset(Fjoin, is.na(Fjoin$Status) | Status != "Completed")
days_I <- as.Date(blue_incompleted$Confirmed_Hired_Date__c, format='%m/%d/%Y') - 
  as.Date(blue_incompleted$Date_of_first_contact__c, format='%m/%d/%Y')

daysIncomp <- na.omit(days_I)
mean(daysIncomp)
#  Showing an average 'Time difference of 179.4905 days'

# And now for the vets who used volunteer services and were hired...

NumDays <- as.Date(blue_completed$Confirmed_Hired_Date__c, format='%m/%d/%Y') - 
           as.Date(blue_completed$Date_of_first_contact__c, format='%m/%d/%Y')

completeDays <- na.omit(NumDays)
mean(completeDays)
#  Showing an average 'Time difference of 174.2407 days' from first contact to hire for vets that
#  completed the volunteer program.  Probably not a significant difference, but will do a 
#  comparative means test to see...

#  Compare the two means, but first test the homogeneity of variances
var.test(daysIncomp, completeDays)
#  Looks like the variances are unequal, so specify FALSE in the var.equal parameter on t.test
t.test(daysIncomp, completeDays, var.equal=FALSE, paired=FALSE)

#  t = 0.5657 days, df = 467.986, p-value = 0.5719 days
#  Looks like the means are equal.  So we can conclude that while working with a volunteer
#  increases the likelihood of being hired, it does not increase the likelihood of being hired
#  sooner... 
#  (examine the 'Reason' variable, see if one of those three factors makes a time difference)

#  Make a visualization (maybe a histogram or bell curve) of the days it takes to get hired 

#----------------------------------------------------------------------------------------------
# Scatter plot the date range variable on the y-axis and explore other variables of interest on the
# x-axis for Question 2  








#------------------------------------------------------------------------------------------------
# Question 1c:  Are there certain volunteer activities that are more effective than others?
#------------------------------------------------------------------------------------------------

# This question refers to the Reason variable
summary(joind$Reason)

reason.type <- joind$Reason
table(reason.type)
table(blue_completed$Reason)

# find ratios for each factor of Reason
# Career Couseling  (.189 or ~ 19% )
62/328
# Federal Resume Review (.160 or ~ 16%) 
50/311
# Mock Interview (.309 or ~ 31%)
367/1187

# Make an indicator variable from the Volunteer_Complete variable

volComplete_int <- ifelse(joind$Volunteer_Complete == "Completed", 1, 0)

# visualize means of each level of Reason (gplot package)
plotmeans(volComplete_int ~ joind$Reason)

# Fit new indicator variable with anova and 'Reason' as explanatory variable 
fit <- aov(volComplete_int ~ joind$Reason)

summary(fit)
TukeyHSD(fit, ordered=FALSE, conf.level=0.95)

summary.lm(fit)




