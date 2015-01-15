#####################################################################
########## Analysis of Family Independence Initiative Data ##########
##########                 Alex Jutca                      ##########
##########              November 3, 2014                   ##########
#####################################################################

## SET UP FOR WORK
rm(list = ls())
getwd()
setwd("C:/Users/ajutca/Google Drive/Team (Shared)/Advisory/Kellogg_2014_CBA/WKKF Grantee Projects/FII/3. Phase II/Data")

## LOG OUTPUT
sink("worklog.txt")

## IMPORT DATA SETS
household.journal <-read.csv("20141017_FII_Extract_HouseholdJournal_AJ.csv")
personal.journal  <-read.csv("20141017_FII_Extract_PersonalJournalAlt_AJ.csv")
fam_member.journal<-read.csv("20141017_FII_Extract_FamilyMember_NA.csv")
family.journal    <-read.csv("20141017_FII_Extract_Families.csv")
bal_sheet.journal <-read.csv("20141017_FII_Extract_PersonalAssetOrLiabilityAll_AJ.csv")
social.journal    <-read.csv("CyclicalSurveys 8'11'14.csv",skip=1,header=T)

## KEEP KEY VARIABLES FROM DATA SETS
household.journal <-subset(household.journal, select=c(Id, ChildSupportIncome, EITC, FoodStampIncome, WICIncome, RentalIncome,
                           RentalIncomeAmount, OtherLumpIncome, SavingsGoal, HousingSubsidized, HousingSubsidizedAmount, 
                           HousingRentAmount, HousingHappinessScore, HousingHappinessReason, HousingHappinessImprove,
                           LiabilitiesGoal, CurrentGoals, GoalProgress, OwnHome, MortgageAmount, CommunityHelpGiven,
                           InvestmentCircle, ReceiveWicIncome, DoctorVisit))

personal.journal  <-subset(personal.journal, select=c(JournalDate, EmploymentIncome, OtherWorkIncome, BusinessIncome,
                           BusinessNewEmployees, SSIIncome, UnemploymentIncome, CalWorksIncome, TransitionalMAIncome,
                           FamilyCode, FamilyId, FamilyMemberId, FamilyName,
                           HouseholdJournalId, City, State, ZipCode, PrimaryLanguage, SecondaryLanguage, ServiceLocation,
                           GroupCode, FamilyType))

fam_member.journal<-subset(fam_member.journal, select=c(Id, FirstName, MiddleName, LastName, Birthday, Ethnicity, Gender, EducationLevel,
                           SchoolName, PrimaryActivity, SecondaryActivity, HealthInsurance, EligibleToVote, RegisteredToVote,
                           VotedInLastElection))

##################################
## CLEANING THE RAW DATA SETS   ##
##################################
## TYPOS THAT NEED FIXED-- INCOME ATTRIBUTED TO WRONG MEMBER OF THE HOUSEHOLD
personal.journal$JournalDate<-as.Date(personal.journal$JournalDate, format="%m/%d/%Y")
personal.journal$FamilyMemberId[(personal.journal$FamilyId==523 & personal.journal$FamilyMemberId==1593)] <-1577 
personal.journal$FamilyMemberId[(personal.journal$FamilyId==354 & personal.journal$FamilyMemberId==742)]  <-741
personal.journal$FamilyMemberId[(personal.journal$FamilyId==522 & personal.journal$FamilyMemberId==1601)] <-1588
personal.journal$FamilyMemberId[(personal.journal$FamilyId==394 & personal.journal$FamilyMemberId==887)]  <-2350
personal.journal$FamilyMemberId[(personal.journal$FamilyId==462 & personal.journal$FamilyMemberId==1520)] <-1511
personal.journal$FamilyMemberId[(personal.journal$FamilyId==521 & personal.journal$FamilyMemberId==1591)] <-1576
personal.journal$FamilyMemberId[(personal.journal$FamilyId==580 & personal.journal$FamilyMemberId==1962)] <-1960
personal.journal$FamilyMemberId[(personal.journal$FamilyId==104 & personal.journal$FamilyMemberId==70)]   <-72
personal.journal$FamilyMemberId[(personal.journal$FamilyId==944 & personal.journal$FamilyMemberId==3677)] <-3567
personal.journal$FamilyMemberId[(personal.journal$FamilyId==803 & personal.journal$FamilyMemberId==3147)] <-2977
personal.journal$FamilyMemberId[(personal.journal$FamilyId==165 & personal.journal$FamilyMemberId==1132)] <-1136
personal.journal$FamilyMemberId[(personal.journal$FamilyId==65  & personal.journal$FamilyMemberId==762)]  <-763
personal.journal$FamilyMemberId[(personal.journal$FamilyId==589 & personal.journal$FamilyMemberId==2089)] <-2091
personal.journal$FamilyMemberId[(personal.journal$FamilyId==76  & personal.journal$FamilyMemberId==10)]   <-9
personal.journal$FamilyMemberId[(personal.journal$FamilyId==20  & personal.journal$FamilyMemberId==2389)] <-4
personal.journal$FamilyMemberId[(personal.journal$FamilyId==2   & personal.journal$FamilyMemberId==938)]  <-399

personal.journal$FamilyMemberId[(personal.journal$FamilyId==762 & personal.journal$FamilyMemberId == 2848 
                               & personal.journal$JournalDate=="2014-07-01")] <- 2844

personal.journal$FamilyMemberId[(personal.journal$FamilyId==594 & personal.journal$FamilyMemberId == 2072
                               & personal.journal$JournalDate=="2014-01-01")] <- 2361

personal.journal <- personal.journal[!(personal.journal$FamilyId==221 & personal.journal$FamilyMemberId == 380 
                                   & personal.journal$JournalDate=="2014-06-01"),]  # drop this observation

personal.journal <- personal.journal[!(personal.journal$FamilyId==485 & personal.journal$FamilyMemberId == 1389 
                                     & personal.journal$JournalDate=="2014-04-01"),]# drop this observation

personal.journal <- personal.journal[!(personal.journal$FamilyId==589 & personal.journal$FamilyMemberId == 2091 
                                   & personal.journal$JournalDate=="2014-08-01"),]  # drop this observation

personal.journal$FamilyMemberId[personal.journal$FamilyId==102  & (personal.journal$FamilyMemberId==2280|
                                personal.journal$FamilyMemberId==2279)] <- 3358

personal.journal$FamilyMemberId[personal.journal$FamilyId==262  & 
                               (personal.journal$FamilyMemberId==504 | 
                                personal.journal$FamilyMemberId==2261)] <- 508

personal.journal$SSIIncome[personal.journal$FamilyId==754 &
                           personal.journal$FamilyMemberId==2752] <- personal.journal$SSIIncome[personal.journal$FamilyId==754 & 
                                                                     personal.journal$FamilyMemberId==2762] # looks like SSI was wrongly attributed

personal.journal <- personal.journal[!(personal.journal$FamilyId == 754 & 
                                     personal.journal$FamilyMemberId == 2762),]  # drop this observation

personal.journal <- personal.journal[!(personal.journal$FamilyId == 456 & 
                                     personal.journal$FamilyMemberId == 1292),]  # drop this observation; I think they meant to attribute to father, can't trust it

#######################
## MERGING DATA SETS ##
#######################
# GET THE TWO DATA SETS WITH INDIVIDUAL-LEVEL VARIABLES
merged.data.individual <- merge(personal.journal, fam_member.journal, by.x="FamilyMemberId", by.y="Id")

# MERGE THE INDIVIDUAL DATA SETS WITH THE HOUSEHOLD JOURNAL
merged.data.household  <- merge(merged.data.individual, household.journal, by.x="HouseholdJournalId", by.y="Id")

# INSPECT THE TYPES OF VARIABLES
str(merged.data.household)

##################################
## CLEANING THE MERGED DATA SET ##
##################################
# REMOVE THE TEST OBSERVATIONS
merged.data.household$FamilyName <- as.character(merged.data.household$FamilyName)
merged.data.household$LastName   <- as.character(merged.data.household$LastName)
merged.data.household$FirstName  <- as.character(merged.data.household$FirstName)
merged.data.household            <- merged.data.household[!(grepl("Test",merged.data.household$FamilyName)  |
                                                            grepl("Test",merged.data.household$LastName)) , ]

# CONVERT DATES USING AS.DATE
merged.data.household$JournalDate <- as.Date(merged.data.household$JournalDate, format="%m/%d/%Y")
merged.data.household$Birthday    <- as.Date(merged.data.household$Birthday, format="%m/%d/%Y")

# SORT THE DATA
sorted.merged <- merged.data.household[order(merged.data.household$FamilyMemberId, 
                                           merged.data.household$JournalDate, 
                                           merged.data.household$HouseholdJournalId),]

# CREATE AGE VARIABLE
sorted.merged$age  <- (sorted.merged$JournalDate - sorted.merged$Birthday)
sorted.merged$age2 <- trunc(as.numeric(sorted.merged$age/365.25)) # create age variable

# CHECK THE OLDEST/YOUNGEST PEOPLE IN THE DATABASE & AGE-RELATED MISTAKES THAT NEED CORRECTED
test <- sorted.merged[,c("JournalDate","Birthday", "EducationLevel","PrimaryActivity", "age", 
                      "age2","FamilyId","FamilyMemberId","EmploymentIncome")]
# View(test[order(test$age), ]) # view observations in ascending order of age
# View(test[order(-test$age), ]) # view observations in descending order of age

sorted.merged$Birthday[sorted.merged$Birthday == "2997-09-16"] <- as.Date("1997-09-16") # fix this person's DOB
sorted.merged$Birthday[sorted.merged$Birthday == "1069-01-16"] <- as.Date("1969-01-16") # and another
sorted.merged$Birthday[sorted.merged$Birthday == "0199-01-18"] <- as.Date("1990-01-18") # and another

sorted.merged$age <- (sorted.merged$JournalDate - sorted.merged$Birthday)
sorted.merged$age2 <- trunc(as.numeric(sorted.merged$age/365.25)) # redo age variable to account for fixed Birthdays above

## SEVERAL PROBLEMS NEED ADDRESSED (E.G., BORN IN 1899, KIDS WITH LARGE INCOMES, NEGATIVE AGES)
# First, let's flag the unusual age answers and display those examples
sorted.merged$ageflag[sorted.merged$age<0    | 
                      sorted.merged$age2>100 | 
                     (sorted.merged$age2>=0 & sorted.merged$age2<=17 & sorted.merged$EmploymentIncome>0)] <- 1

flagged <- subset(sorted.merged, sorted.merged$ageflag==1, select=c("ageflag", "age2","age","Birthday","JournalDate",
                                                                  "PrimaryActivity","EducationLevel","FamilyId","FamilyMemberId",
                                                                  "EmploymentIncome"))
unique.flagged<-flagged[ ! duplicated( flagged[c("FamilyMemberId","FamilyId")]),] # look at unique FamilyMemberIds and FamilyIds
# View(unique.flagged[order(unique.flagged$age), ])

##############################################################################################################
# Looking at the flagged data, I'm reasonably comfortable with those with EmploymentIncome who are 12 and up.#
# I'm less comfortable with some of the other flagged observations (e.g., young income earners)              #
# I might be able to fix them by going through on an individual basis and seeing if I can identify anything  #
# after employing a quick rule to simplify the analysis                                                      #
##############################################################################################################

# second, l's investigate fixes for flagged observations
sorted.merged <- sorted.merged[!(sorted.merged$FamilyId==96 & sorted.merged$FamilyMemberId == 45),]    # drop this observation, appears to be a duplicate of 96 & 43
sorted.merged <- sorted.merged[!(sorted.merged$FamilyId==287 & sorted.merged$FamilyMemberId == 564),]  # drop this observation, appears to create a new entry for pre-existing FII member
sorted.merged$age2[(sorted.merged$Birthday== "1899-01-31")] <- NA                                    # some birth years are 1899, which must be wrong
sorted.merged <- sorted.merged[!(sorted.merged$FamilyId==438 & sorted.merged$FamilyMemberId == 1224),] # drop this observation
sorted.merged$age2[(sorted.merged$FamilyId==590 & sorted.merged$FamilyMemberId==3161)] <- NA         # age seems to reference the wrong person 
                                                                                                     # (right person isn't in database)

# we can safely employ the following rule to fix incorrect ages:
sorted.merged$age2[sorted.merged$age2<=10 & (sorted.merged$PrimaryActivity=="Work"                 |  
                                             sorted.merged$PrimaryActivity=="Unemployed"           |
                                             sorted.merged$EducationLevel=="Associate Degree"      | 
                                             sorted.merged$EducationLevel=="Bachelor's Degree"     |
                                             sorted.merged$EducationLevel=="Graduate Degree"       |
                                             sorted.merged$EducationLevel=="High School (10 - 12)" |
                                             sorted.merged$EducationLevel=="Vocational Degree")] <- NA

## VIEW DATA AFTER SETTING AGE==NA PER ABOVE RULES
sorted.merged$ageflag2 <- NULL
sorted.merged$ageflag2[(sorted.merged$age<0    | 
                        sorted.merged$age2>100 | 
                       (sorted.merged$age2>=0  & 
                        sorted.merged$age2<=9  & 
                        sorted.merged$EmploymentIncome>0)|
                       (sorted.merged$age2>=10 &
                        sorted.merged$age2<=17 &
                        sorted.merged$EmploymentIncome>=1000))
                     & !is.na(sorted.merged$age2)] <- 1

flagged2 <- subset(sorted.merged, sorted.merged$ageflag2==1, select=c("ageflag2", "age2","age","Birthday","JournalDate",
                                                                    "PrimaryActivity","EducationLevel","FamilyId",
                                                                    "FamilyMemberId","EmploymentIncome"))

unique.flagged2 <- flagged2[ ! duplicated( flagged2[c("FamilyMemberId","FamilyId")]),] # look at unique FamilyMemberIds and FamilyIds
# View(unique.flagged2[order(unique.flagged2$age), ])

## AFTER CLEARING SOME OF THE OTHER QUESTIONABLE OBSERVATIONS {ABOVE IN THE 'CLEANING RAW DATA SETS' SECTION}
sorted.merged$age2[sorted.merged$FamilyMemberId %in% c(2793,3325,3048,184,96)] <- NA

## GENDER RESPONSE ISSUE
sorted.merged$Gender[sorted.merged$Gender=="N"] <- "M"  # assumes that those who wrote "N" meant to write "M"

## IMPUTATIONS FOR MISSING RESPONSES
# for missing gender:
table(sorted.merged$Gender)
# View(sorted.merged[sorted.merged$Gender=="",])

# capitalize last and first names
sorted.merged$FirstName <- toupper(sorted.merged$FirstName)
sorted.merged$LastName  <- toupper(sorted.merged$LastName)

# remove spaces
sorted.merged$FirstName <- gsub(" ", "", sorted.merged$FirstName)
sorted.merged$LastName  <- gsub(" ", "", sorted.merged$LastName)

sorted.merged$Gender[sorted.merged$Gender=="" & sorted.merged$FirstName %in% c("LORETTA","ELIZABETH",
                                                                               "ELEANOR","GRACE",
                                                                               "JUANITA","ESTELLA",
                                                                               "KAREN","PAOLA")] <- "F"
sorted.merged$Gender[sorted.merged$Gender=="" & sorted.merged$FirstName %in% c("PAUL","WIBERT",
                                                                               "WILBUR", "ROLANDO",
                                                                               "PEDRO","JASON")] <- "M"

# for missing ethnicity:
# View(sorted.merged[sorted.merged$Ethnicity=="",])
sorted.merged$Ethnicity[sorted.merged$PrimaryLanguage=="Spanish"] <- "Latino"

# for cities:
sorted.merged$City <- as.character(sorted.merged$City) # convert to character
sorted.merged$City <- gsub(" ", "", sorted.merged$City)# eliminate spaces
sorted.merged$City <- toupper(sorted.merged$City)      # capitalize cities

unique.cities <- sorted.merged[ ! duplicated( sorted.merged[c("City")]),]
# View(unique.cities$City)
# View(unique.cities$City[order(unique.cities$City)])

sorted.merged$City[substr(sorted.merged$City,1,3)=="DOR"]    <-"DORCHESTER"
sorted.merged$City[grepl("CHESTER",sorted.merged$City)]      <-"DORCHESTER"
sorted.merged$City[substr(sorted.merged$City,1,3)=="DET"]    <-"DETROIT"
sorted.merged$City[grepl("OIT",sorted.merged$City)]          <-"DETROIT"
sorted.merged$City[substr(sorted.merged$City,1,7)=="SANFRAN"]<-"SANFRANCISCO"
sorted.merged$City[substr(sorted.merged$City,1,3)=="MAT"]    <-"MATTAPAN"
sorted.merged$City[substr(sorted.merged$City,1,3)=="JAM"]    <-"JAMAICAPLAIN"
sorted.merged$City[substr(sorted.merged$City,1,4)=="EAST"]   <-"EASTBOSTON"
sorted.merged$City[substr(sorted.merged$City,1,2)=="E."]     <-"EASTBOSTON"
sorted.merged$City[substr(sorted.merged$City,1,2)=="E,"]     <-"EASTBOSTON"
sorted.merged$City[grepl("SEA",sorted.merged$City)]          <-"CHELSEA"
sorted.merged$City[grepl("BURY",sorted.merged$City)]         <-"ROXBURY"
sorted.merged$City[grepl("S.BOSTON",sorted.merged$City)]     <-"SOUTHBOSTON"
sorted.merged$City[grepl("SOBRANTE",sorted.merged$City)]     <-"ELSOBRANTE"

# for service locations
sorted.merged$ServiceLocation <- as.character(sorted.merged$ServiceLocation) # convert to character
sorted.merged$ServiceLocation <- gsub(" ", "", sorted.merged$ServiceLocation)# eliminate spaces
sorted.merged$ServiceLocation <- toupper(sorted.merged$ServiceLocation) # capitalize 

sorted.merged$ServiceLocation[ ! duplicated( sorted.merged[c("ServiceLocation")])]
table(sorted.merged$ServiceLocation)
# View(sorted.merged[sorted.merged$ServiceLocation=="",])

# for family types
sorted.merged$FamilyType[ ! duplicated( sorted.merged[c("FamilyType")])]
# table(sorted.merged$FamilyType)
# View(sorted.merged[sorted.merged$FamilyType=="",])

sorted.merged$FamilyType<-as.character(sorted.merged$FamilyType) # convert to character

sorted.merged$FamilyType[sorted.merged$FamilyType=="" &  substr(sorted.merged$FamilyCode,3,3)=="R"]                                <-"FII Ripple"
sorted.merged$FamilyType[sorted.merged$FamilyType=="" & (substr(sorted.merged$FamilyCode,3,3)!="R" & sorted.merged$FamilyCode!="")]<-"FII Core"

# for education
table(sorted.merged$EducationLevel)
# View(sorted.merged[sorted.merged$EducationLevel=="",])

sorted.merged$EducationLevel<-as.character(sorted.merged$EducationLevel) # convert to character
sorted.merged$EducationLevel<-gsub(" ", "", sorted.merged$EducationLevel)# eliminate spaces
sorted.merged$EducationLevel<-toupper(sorted.merged$EducationLevel)      # capitalize 

sorted.merged$SchoolName<-as.character(sorted.merged$SchoolName) # convert to character
sorted.merged$SchoolName<-gsub(" ", "", sorted.merged$SchoolName)# eliminate spaces
sorted.merged$SchoolName<-toupper(sorted.merged$SchoolName)      # capitalize

sorted.merged$PrimaryActivity<-as.character(sorted.merged$PrimaryActivity) # convert to character
sorted.merged$PrimaryActivity<-gsub(" ", "", sorted.merged$PrimaryActivity)# eliminate spaces
sorted.merged$PrimaryActivity<-toupper(sorted.merged$PrimaryActivity)      # capitalize 

sorted.merged$EducationLevel[sorted.merged$EducationLevel == "" & sorted.merged$age2<5 & !(is.na(sorted.merged$age2))]<-"NONE"
sorted.merged$EducationLevel[sorted.merged$EducationLevel == "" & grepl("ELEMENTARY", sorted.merged$SchoolName) 
                                                                & !(sorted.merged$age2>13)]                           <-"ELEMENTARYSCHOOL(1-6)"
sorted.merged$EducationLevel[sorted.merged$EducationLevel == "" & grepl("HIGH", sorted.merged$SchoolName) 
                                                                & !(sorted.merged$age2<12)]                           <-"HIGHSCHOOL(10-12)"
sorted.merged$EducationLevel[sorted.merged$EducationLevel == "" & sorted.merged$PrimaryActivity=="DAYCARE" 
                                                                & !(sorted.merged$age2>6)]                            <-"NONE"

# disagreements b/w binary and continuous variables
sorted.merged$HousingSubsized[sorted.merged$HousingSubsidizedAmount>0] <- "Yes"
sorted.merged$ReceiveWICIncome[sorted.merged$WICIncome>0] <- "Yes"

# check dates
# View(sorted.merged$JournalDate[ ! duplicated( sorted.merged[c("JournalDate")])]) # unique JournalDates
sorted.merged$DayofWk <- as.numeric(format(sorted.merged$JournalDate, "%d")) # day
sorted.merged$Month   <- as.numeric(format(sorted.merged$JournalDate, "%m")) # month
sorted.merged$Year    <- as.numeric(format(sorted.merged$JournalDate, "%Y")) # year

# analyze data by month to account for the fact that not all JournalDates are on the 1st
library(zoo)
sorted.merged$JournalMoYr <- as.yearmon(sorted.merged$JournalDate) # month & year of JournalDate is  now variable
sorted.merged$JournalMoYr <- as.Date(sorted.merged$JournalMoYr, format="%b %Y")

## ADDITIONAL FIXES THAT WERE SPOTTED LATER AND NEED CLEANED FOR HOUSEHOLDS
## (most of these relate to very long durations in FII that seemed wrong; trying to err on conservative side, though)
## changes made 01/06/2015
sorted.merged$JournalMoYr[(sorted.merged$FamilyId == 325 & sorted.merged$JournalMoYr == "2011-09-01" )]  <- "2013-09-01"
sorted.merged$JournalMoYr[(sorted.merged$FamilyId == 312 & sorted.merged$JournalMoYr == "2011-09-01" )]  <- "2013-09-01"
sorted.merged$JournalMoYr[(sorted.merged$FamilyId == 310 & sorted.merged$JournalMoYr == "2011-06-01" )]  <- "2013-06-01"
sorted.merged$JournalMoYr[(sorted.merged$FamilyId == 308 & sorted.merged$JournalMoYr == "2011-09-01" )]  <- "2013-09-01"
sorted.merged$JournalMoYr[(sorted.merged$FamilyId == 282 & sorted.merged$JournalMoYr == "2011-06-01" )]  <- "2013-06-01"
sorted.merged$JournalMoYr[(sorted.merged$FamilyId == 274 & sorted.merged$JournalMoYr == "2012-09-01" )]  <- "2013-09-01"
sorted.merged$JournalMoYr[(sorted.merged$FamilyId == 270 & sorted.merged$JournalMoYr == "2011-09-01" )]  <- "2013-09-01"
sorted.merged$JournalMoYr[(sorted.merged$FamilyId == 264 & sorted.merged$JournalMoYr == "2011-08-01" )]  <- "2013-08-01"
sorted.merged$JournalMoYr[(sorted.merged$FamilyId == 264 & sorted.merged$JournalMoYr == "2011-09-01" )]  <- "2013-09-01"
sorted.merged$JournalMoYr[(sorted.merged$FamilyId == 254 & sorted.merged$JournalMoYr == "2012-09-01" )]  <- "2013-09-01"
sorted.merged$JournalMoYr[(sorted.merged$FamilyId == 238 & sorted.merged$JournalMoYr == "2012-10-01" )]  <- "2013-10-01"
sorted.merged$JournalMoYr[(sorted.merged$FamilyId == 236 & sorted.merged$JournalMoYr == "2011-09-01" )]  <- "2013-09-01"
sorted.merged$JournalMoYr[(sorted.merged$FamilyId == 227 & sorted.merged$JournalMoYr == "2012-10-01" )]  <- "2013-10-01"
sorted.merged$JournalMoYr[(sorted.merged$FamilyId == 214 & sorted.merged$JournalMoYr == "2011-09-01" )]  <- "2013-09-01"
sorted.merged$JournalMoYr[(sorted.merged$FamilyId == 201 & sorted.merged$JournalMoYr == "2013-01-01" )]  <- "2014-01-01"
sorted.merged$JournalMoYr[(sorted.merged$FamilyId == 194 & sorted.merged$JournalMoYr == "2011-09-01" )]  <- "2013-09-01"
sorted.merged$JournalMoYr[(sorted.merged$FamilyId == 190 & sorted.merged$JournalMoYr == "2011-08-01" )]  <- "2013-08-01"
sorted.merged$JournalMoYr[(sorted.merged$FamilyId == 179 & sorted.merged$JournalMoYr == "2011-06-01" )]  <- "2014-06-01"
sorted.merged$JournalMoYr[(sorted.merged$FamilyId == 156 & sorted.merged$JournalMoYr == "2011-09-01" )]  <- "2013-09-01"
sorted.merged$JournalMoYr[(sorted.merged$FamilyId == 107 & sorted.merged$JournalMoYr == "2012-10-01" )]  <- "2013-10-01"
sorted.merged$JournalMoYr[(sorted.merged$FamilyId == 102 & sorted.merged$JournalMoYr == "2012-09-01" )]  <- "2013-09-01"

sorted.merged <- sorted.merged[!(sorted.merged$FamilyId == 12),] # drop this observation - weird income situation (goes from 15k to 0)

# set up a dummy count variable
sorted.merged$Count <- 1

# compute the frequency of reporting periods
freqs <- aggregate(Count ~ JournalMoYr, data=sorted.merged, FUN=length)

# bring in libraries for plot
library(ggplot2)
library(lubridate)
library(scales)

# set a blank background
old <- theme_update(panel.background = element_rect(colour = "pink"))

# establish minimum and maximum dates
min.date <- min(sorted.merged$JournalMoYr)
max.date <- max(sorted.merged$JournalMoYr)

# plot the frequency of journal entries by month
g <- ggplot(data=freqs,aes(x=JournalMoYr,y=Count)) + 
          geom_bar(stat='identity')  + 
          scale_x_date(limits = c(min.date, max.date), breaks= seq(min.date,max.date,180), labels = date_format("%B %Y")) +
          theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) + 
          ylab('Frequency') +
          xlab('Date') +
          labs(title = 'Journal Entries by Month') 
print(g)
ggsave(file="Monthly_Individuals_Reporting.pdf")

# create frequency of unique family journal entries by month
freqs2 <- aggregate(sorted.merged$FamilyId, by=list(sorted.merged$JournalMoYr),
                                          function(x) length(unique(x)))

# plot the frequency of unique family journal entries by month
g2 <- ggplot(data=freqs2, aes(x=Group.1,y=x), fill = "white", color = "black")+ 
           geom_bar(stat='identity')  + 
           scale_x_date(labels = date_format("%B %Y"), breaks= seq(min.date,max.date,180), limits = c(min.date, max.date)) +
           theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) + 
           ylab('Frequency') +
           xlab('Date') +
           labs(title = 'Unique Family Entries by Month')
print(g2)
ggsave(file="Monthly_Families_Reporting.pdf")

# Create a variable that measures family reporting tenure
# Two separate variables:
# 1 - the length of time between first and last entry for a given family
# 2 - the number of entries for each family and individual 

# calculate entries for each individual and show what family they belong to
familymember.count <- aggregate(sorted.merged$FamilyMemberId, by=list(FamilyId=sorted.merged$FamilyId, 
                                                                     FamilyMemberId=sorted.merged$FamilyMemberId),
                                                             function(x) length((x)))

# make a function that calculates the number of months in which one could potentially have submitted
# a journal entry, using first and last journal dates
elapsed_months <- function(end_date, start_date) {
            ed <- as.POSIXlt(end_date)
            sd <- as.POSIXlt(start_date)
(12 * (ed$year - sd$year) + (ed$mon - sd$mon)) + 1
}

# apply this function to each FamilyMemberId to get number of months in FII database
library(plyr)
tenure.familymemberid <- ddply(sorted.merged, .(FamilyMemberId), summarize, 
                                                               mindate  = min(JournalMoYr), 
                                                               maxdate  = max(JournalMoYr), 
                                                               diffdate = elapsed_months(maxdate,mindate))

# check order of tenure (max should be 25 - 24 months of FII enrollment, anything above that is weird)
# tenure.familymemberid[order(tenure.familymemberid$diffdate), ]

# distribution of respondents by length of time in data system
g3 <- ggplot(data=tenure.familymemberid) +
           geom_histogram(aes(x = diffdate, y = ..count../sum(..count..)),binwidth = 1, fill = "white", color = "black") +
           scale_y_continuous(labels = percent_format()) +
           labs(x = "Length of Time Reporting (in Months)", 
                y = "Proportion of Respondents",
                title = "Distribution of Respondent Tenure in FII Reporting System")
print(g3)
ggsave(g3, file= "Dist_Respondent_Tenure.pdf")

# some oddities: there are a few individuals whose entry in system is > 25 months
# not clear how we resolve this problem (if it is a problem)

# number of monthly entries for each individual
entries.by.individual <- aggregate(sorted.merged$FamilyMemberId, 
                                   by=list(FamilyMemberId=sorted.merged$FamilyMemberId),
                                   function(x) length(x))

max(entries.by.individual$x) #[1] 14 - that's the most entries any person has 
min(entries.by.individual$x) #[1] 1 - that's the fewest entries any person has

# plot distribution of individual reporting frequency
g4 <- ggplot(data=entries.by.individual, aes(x=FamilyMemberId,y=x))+ 
      geom_bar(stat='identity')  + 
      theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) + 
      ylab('Frequency') +
      xlab('Unique Person ID') +
      labs(title = 'Reporting Frequency for Each Journal Respondent')
print(g4)

# distribution of respondents by reporting frequency
g5 <- ggplot(data = entries.by.individual) +
      geom_histogram(aes(x = x, y = ..count../sum(..count..)),binwidth = 1, fill = "white", color = "black") +
      scale_y_continuous(labels = percent_format()) +
      labs(x = "Number of Monthly Journal Entries", 
           y = "Proportion of Respondents",
           title = "Distribution of Respondent Reporting Frequency")    
print(g5)
ggsave(g5, file= "Dist_Respondent_Frequency.pdf")

# what is the response rate? (i.e., out of the total months you are in FII, for what percentage have you submitted a journal entry)
hit.rate<-merge(tenure.familymemberid, entries.by.individual, by.x = "FamilyMemberId", by.y = "FamilyMemberId")
hit.rate$responserate<-round(hit.rate$x/hit.rate$diffdate, digits = 2)

# distribution of response rates
g6<-ggplot(data = hit.rate) +
  geom_histogram(aes(x = responserate, y = ..count../sum(..count..)),binwidth = 0.1000001, fill = "white", color = "black") +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Response Rates", 
       y = "Proportion of Respondents",
       title = "Distribution of Response Rates")  +
  scale_x_continuous(limits = c(0, 1.01))
print(g6)
ggsave(g6, file= "Dist_Response_Rates.pdf")

######################################################
############# ATTRITION ANALYSIS #####################
######################################################

# Want to examine (1) attrition; (2) selective reporting

# FIRST, subset the data so that we're only looking at those who have >1 entry
# we'll want to use data where there are > 1 observations for family-level analysis
# for individuals, to get things like total income it's important not to delete those entries

# merge the count data for the number of entries for each FamilyMemberId
analytic.data <- merge(sorted.merged, familymember.count, by.x="FamilyMemberId", by.y="FamilyMemberId")
analytic.data <- rename(analytic.data, c("x"="ReportingMos"))

# subset data for those w/ >1 observation
analytic.sub  <- subset(analytic.data, analytic.data$ReportingMos>1) 

# create a PERIODS variable, which will equate to the cumulative sum
# of the number of times a person has reported so that we can commonly 
# plot and analyze the progress a family makes through FII

cum.count <- ddply(analytic.sub, .(FamilyMemberId), summarize, 
                    Periods  = cumsum(Count))
 
cum.count2 <- cbind(cum.count, analytic.sub$JournalMoYr)
colnames(cum.count2) <- c('FamilyMemberId','Periods','JournalMoYr')

# dataset w/ Periods merged into it
my.data <- merge(analytic.sub, cum.count2, by=c("FamilyMemberId","JournalMoYr"))

# dataset with max/min/diff- date merged into it  
my.data <- merge(my.data, tenure.familymemberid, by = c("FamilyMemberId","FamilyMemberId"))

# subset dataset so that it's not cutting off people via censoring
# exclude those whose last month is 2014-08-01 or 2014-09-01 to get proper attrition measure
my.data.sub <- subset(my.data, !(maxdate == "2014-08-01"| maxdate == "2014-09-01"))

# subset dataset for FII Core members
my.data.core.sub <- subset(my.data.sub, my.data$FamilyType=="FII Core")

# Subset for FII Core members additionally - COMMENTED OUT B/C OLD/MISLEADING (12/19/2014)
# my.data.fiicore <- subset(my.data, my.data$JournalMoYr<="2014-09-01" & my.data$FamilyType=="FII Core") 

# table of counts by period
attrition <- (as.data.frame(table(my.data.sub$Periods)))
attrition.core <- (as.data.frame(table(my.data.core.sub$Periods)))
colnames(attrition) <- c('Periods','Count')
colnames(attrition.core) <- c('Periods','Count')

# calculate Pr(Not Enrolled in the current or subsequent month | Enrolled in prior month)
attrition.rate <- round(c(NA,1-attrition$Count[-1]/attrition$Count[-length(attrition$Count)]),digits=2)
attrition.rate.core <- round(c(NA,1-attrition.core$Count[-1]/attrition.core$Count[-length(attrition.core$Count)]),digits=2)

attrition2 <- cbind(attrition.rate, attrition)
attrition2$Periods <- as.numeric(attrition2$Periods)

attrition2.core <- cbind(attrition.rate.core, attrition.core)
attrition2.core$Periods <- as.numeric(attrition2.core$Periods)

attrition.w.core <- merge(attrition2, attrition2.core,  by = c('Periods'), all = TRUE)

# plot the attrition rate for all members and FII Core )
attrition.plot <- ggplot(data = attrition.w.core, aes(x=Periods, na.rm = TRUE)) + 
                         geom_line(aes(y = attrition.rate, colour = "attrition.rate"))           + 
                         geom_line(aes(y = attrition.rate.core, colour = "attrition.rate.core")) +
                         labs(x = "Number of Monthly Journals Completed", 
                             y = "Attrition",
                              title = "Attrition by Reporting Period") +
                              ylim(0,0.8) + 
                              xlim(0,15)  +
                         theme(legend.title=element_blank())           +
                         scale_fill_discrete(labels=c("Overall","Core Families")) + 
                         scale_color_discrete(labels = c("FII Overall", "FII Core")) + 
                         theme(legend.key = element_rect(fill = NA, colour = NA, size = 0.25))
print(attrition.plot)
ggsave(attrition.plot, file= "Attrition_Rate_Revised.pdf")

# plot the last month of reporting for each person in FII
lastdate.plot <- ggplot(data = tenure.familymemberid) +
                        geom_histogram(aes(x = maxdate, y = ..count../sum(..count..)),binwidth = 30, fill = "white", color = "black") +
                        scale_y_continuous(labels = percent_format()) +
                        #scale_x_date(labels = date_format("%B %Y")) +
                        #theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) + 
                        labs(x = "Last Monthly Journal Entry", 
                             y = "Proportion of Respondents",
                             title = "Distribution of Respondents' Last Journal Entry Date")    
print(lastdate.plot)
ggsave(lastdate.plot, file= "Dist_LastResponse.pdf")
# looks like ~70% of FII Respondents were still active as 2014-07-01 (last reporting month is September)

# plot the first month of reporting for each person in FII
firstdate.plot <- ggplot(data = tenure.familymemberid) +
                        geom_histogram(aes(x = mindate, y = ..count../sum(..count..)),binwidth = 30, fill = "white", color = "black") +
                        scale_y_continuous(labels = percent_format()) +
                        #scale_x_date(labels = date_format("%B %Y")) +
                        #theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) + 
                        labs(x = "First Monthly Journal Entry", 
                             y = "Proportion of Respondents",
                             title = "Distribution of Respondents' First Journal Entry Date")    
print(firstdate.plot)
ggsave(firstdate.plot, file= "Dist_FirstResponse.pdf")


###################################################################################
## EXAMINING THE DISTRIBUTION OF INCOME AND THE CREATION OF NEW INCOME VARIABLES ##
###################################################################################

## RULE: WE WANT TO EVALUATE PROGRESS AT THE FAMILY LEVEL, WHICH MEANS WE NEED FAMILY INCOME VARIABLES TO AGGREGATE PERFORMANCE
## LET'S START BY AGGREGATING VERTICALLY (ACROSS PEOPLE), THEN AGGREGATE HORIZONTALLY (ACROSS CATEGORIES)
## IT MIGHT MAKE SENSE TO DIVIDE INCOME TO (1) EARNED & CAPITAL INCOME and (2) WELFARE INCOME & SUBSIDIES
## CHECK PROGRESS OF FAMILIES OVER TIME, TOO.

## CHECK FOR OUTLIERS AMONG INCOME and EXPENSE VARIABLES:
##  "EmploymentIncome"        "OtherWorkIncome"         "EITC"
##  "BusinessIncome"          "SSIIncome"               "UnemploymentIncome"      "CalWorksIncome"  "TransitionalMAIncome"  
##  "RentalIncomeAmount"      "OtherLumpIncome"  "HousingSubsidizedAmount" "HousingRentAmount"  "MortgageAmount"  "ChildSupportIncome"


# flag outliers - 3 SDs or more from mean
flag.outliers <- lapply(my.data[c("EmploymentIncome", "OtherWorkIncome","EITC", "BusinessIncome", "SSIIncome",
                                  "UnemploymentIncome", "CalWorksIncome","TransitionalMAIncome", "RentalIncomeAmount",
                                  "OtherLumpIncome","HousingSubsidizedAmount","HousingRentAmount", "MortgageAmount", "ChildSupportIncome")], 
                                    function(x) ifelse(abs(x - mean(x))/sd(x) > 3,1,0) )

# rename variables acc. to convention: Varname.outlier
names(flag.outliers) <- paste(c("EmploymentIncome", "OtherWorkIncome","EITC", "BusinessIncome", "SSIIncome",
                                "UnemploymentIncome", "CalWorksIncome","TransitionalMAIncome", "RentalIncomeAmount",
                                "OtherLumpIncome","HousingSubsidizedAmount","HousingRentAmount", "MortgageAmount", "ChildSupportIncome"),
                                "outlier", sep=".")

my.data <- cbind(my.data, flag.outliers)

# fix EmploymentIncome outliers that appear to be errant
# View(my.data[my.data$EmploymentIncome.outlier==1,])
# It seems as if some of these EmploymentIncome entries need to be divided by 10 (at least)

my.data$EmploymentIncome[my.data$FamilyMemberId %in% c(3176,1614,342,3643,2594) & my.data$EmploymentIncome.outlier==1] <- 
                         my.data$EmploymentIncome[my.data$FamilyMemberId %in% c(3176,1614,342,3643,2594) & my.data$EmploymentIncome.outlier==1] / 10

# fix OtherWorkIncome outliers that appear to be errant
# View(my.data[my.data$OtherWorkIncome.outlier==1,])
# appears to be some double counting and other problems, so let's adjust the figures

my.data$OtherWorkIncome[my.data$FamilyMemberId==2717 & my.data$OtherWorkIncome.outlier==1] <- 0
my.data$EmploymentIncome[my.data$FamilyMemberId %in% c(1353, 1300) & my.data$OtherWorkIncome.outlier==1] <- 0
my.data$OtherWorkIncome[my.data$FamilyMemberId==2482 & my.data$OtherWorkIncome.outlier==1] <- 
                        my.data$OtherWorkIncome[my.data$FamilyMemberId==2482 & my.data$OtherWorkIncome.outlier==1] / 10

# fix EITC outliers that appear to be errant
# View(my.data[my.data$EITC.outlier==1,])
# don't appear to be any problems

# fix BusinessIncome outliers that appear to be errant
# View(my.data[my.data$BusinessIncome.outlier==1,])
# looks like there are some instances where BusinessIncome = EmploymentIncome
# in those cases, set EmploymentIncome == 0
# otherwise, no outliers themselves look worrying

my.data$EmploymentIncome[my.data$BusinessIncome == my.data$EmploymentIncome] <- 0

# fix SSIIncome outliers that appear to be errant
# View(my.data[my.data$SSIIncome.outlier==1,])

my.data$SSIIncome[my.data$FamilyMemberId == 49 & my.data$SSIIncome.outlier==1] <- 
                  my.data$SSIIncome[my.data$FamilyMemberId == 49 & my.data$SSIIncome.outlier==1] / 10

# fix UnemploymentIncome outliers that appear to be errant
# View(my.data[my.data$UnemploymentIncome.outlier==1,])
my.data$UnemploymentIncome[my.data$UnemploymentIncome > 10000 & my.data$UnemploymentIncome.outlier==1] <- 
  my.data$UnemploymentIncome[my.data$UnemploymentIncome > 10000 & my.data$UnemploymentIncome.outlier==1] / 10

my.data$EmploymentIncome[my.data$UnemploymentIncome == my.data$EmploymentIncome] <- 0
my.data$OtherWorkIncome[my.data$UnemploymentIncome == my.data$OtherWorkIncome] <- 0

# fix CalWorksIncome outliers that appear to be errant
# View(my.data[my.data$CalWorksIncome.outlier==1,])

my.data$EmploymentIncome[my.data$CalWorksIncome == my.data$EmploymentIncome] <- 0


# fix TransitionalMAIncome outliers that appear to be errant
# View(my.data[my.data$TransitionalMAIncome.outlier==1,])

my.data$TransitionalMAIncome[my.data$FamilyMemberId == 879 & my.data$TransitionalMAIncome.outlier==1] <- 
                             my.data$TransitionalMAIncome[my.data$FamilyMemberId == 879 & my.data$TransitionalMAIncome.outlier==1] / 100


# fix RentalIncomeAmount outliers that appear to be errant
# View(my.data[my.data$RentalIncomeAmount.outlier==1,])

my.data$EmploymentIncome[my.data$RentalIncomeAmount == my.data$EmploymentIncome] <- 0

# fix OtherLumpIncome outliers that appear to be errant
# View(my.data[my.data$OtherLumpIncome.outlier==1,])

my.data$EmploymentIncome[my.data$OtherLumpIncome == my.data$EmploymentIncome] <- 0

# fix HousingSubsidizedAmount outliers that appear to be errant
# View(my.data[my.data$HousingSubsidizedAmount.outlier==1,])
# no bad data

# fix HousingRentAmount outliers that appear to be errant
# View(my.data[my.data$HousingRentAmount.outlier==1,])

my.data$HousingRentAmount[my.data$FamilyMemberId == 1156 & my.data$HousingRentAmount.outlier==1] <- 
                          my.data$HousingRentAmount[my.data$FamilyMemberId == 1156 & my.data$HousingRentAmount.outlier==1] / 100


# fix MortgageAmount outliers that appear to be errant
# View(my.data[my.data$MortgageAmount.outlier==1,])
# HouseholdJournalIds 260, 7708, 191, 731, 2152, 1595, 172, 325 - have very large mortgage amounts
# part of the problem is that respondents might be confused by whether FII is asking for mortgage payment or mortgage outstanding
# some seem to alternate between whether they're reporting stock or flow, but others just seem to miss a decimal point
# the latter are fixed below

my.data$MortgageAmount[my.data$FamilyMemberId %in% c(158,379,381) & my.data$MortgageAmount.outlier==1] <- 
                       my.data$MortgageAmount[my.data$FamilyMemberId %in% c(158,379,381) & my.data$MortgageAmount.outlier==1] / 100

# fix ChildSupportIncome outliers that appear to be errant
# View(my.data[my.data$ChildSupportIncome.outlier==1,])
# looks like some people with  ChildSupportIncome > 10,000, which is improbably large, so let's divide by 10

my.data$ChildSupportIncome[my.data$ChildSupportIncome > 10000 & my.data$ChildSupportIncome.outlier==1] <- 
  my.data$ChildSupportIncome[my.data$ChildSupportIncome > 10000 & my.data$ChildSupportIncome.outlier==1] / 10


## What variables are captured at the HH level and which are at the individual level?
## Important implications for how we think about calculating HH aggregates
## HH-level: ChildSupportIncome, EITC, FoodStampIncome, WICIncome, RentalIncomeAmount, OtherLumpIncome, HousingSubsidizedAmount, HousingRentAmount, OwnHome, MortgageAmount, InvestmentCircle
## Individual-level: EmploymentIncome, OtherWorkIncome, BusinessIncome, SSIIncome, UnemploymentIncome, CalWorksIncome, TransitionalMAIncome 

## LET'S CREATE HH-LEVEL AGGREGATE VARS FOR INDIVIDUAL-LEVEL VARIABLES
hh.vars <- aggregate(my.data[c("EmploymentIncome","OtherWorkIncome","BusinessIncome","SSIIncome",
                           "UnemploymentIncome","CalWorksIncome","TransitionalMAIncome")],                           
                 by = list(Date = my.data$JournalMoYr, FamilyID = my.data$FamilyId.x), 
                 function(x) sum(x))

## HERE, I'M COPYING THE HH-LEVEL VARIABLES SO THAT WE CAN WORK W THESE 
## VARS IN A SEPARATE DATAFRAME, CORDONING ALL HH-LEVEL ANALYSIS TO ONE DATAFRAME
hh.vars2 <- aggregate(my.data[c("ChildSupportIncome", "EITC", "FoodStampIncome", "WICIncome", "RentalIncomeAmount", 
                                "OtherLumpIncome", "HousingSubsidizedAmount", "HousingRentAmount", 
                                "MortgageAmount")],                           
                      by = list(Date = my.data$JournalMoYr, FamilyID = my.data$FamilyId.x), 
                      function(x) mean(x))

# rename vars with HH.varname convention
names(hh.vars) <- paste(c("Date","FamilyID","EmploymentIncome", "OtherWorkIncome","BusinessIncome", "SSIIncome",
                          "UnemploymentIncome", "CalWorksIncome","TransitionalMAIncome"), "HH", sep=".")

names(hh.vars2) <- paste(c("Date","FamilyID","ChildSupportIncome", "EITC", "FoodStampIncome", "WICIncome", 
                           "RentalIncomeAmount","OtherLumpIncome", "HousingSubsidizedAmount", "HousingRentAmount",
                           "MortgageAmount"), "HH", sep=".")

# merge to get household numeric income and expense data
my.hh.data <- merge(hh.vars, hh.vars2, by=c("FamilyID.HH", "Date.HH"))

# family characteristics we want to merge with our income variables
temp.cols <- unique(my.data[,c("FamilyId.x", "FamilyType","ServiceLocation", "GroupCode")])

# merge family characteristics with income variables
my.hh.data <- merge(my.hh.data, temp.cols, by.x="FamilyID.HH", by.y="FamilyId.x")

## INCOME CATEGORIES 
# non-K&L income (welfare + child support)
my.hh.data$nonKL.inc.HH <- my.hh.data$ChildSupportIncome.HH + my.hh.data$EITC.HH + my.hh.data$FoodStampIncome.HH +
                           my.hh.data$WICIncome.HH + my.hh.data$HousingSubsidizedAmount.HH + my.hh.data$SSIIncome.HH +
                           my.hh.data$CalWorksIncome.HH + my.hh.data$TransitionalMAIncome.HH + my.hh.data$UnemploymentIncome.HH 

# monthly welfare income
my.hh.data$welfare.inc.HH <- my.hh.data$EITC.HH + my.hh.data$FoodStampIncome.HH +
                             my.hh.data$WICIncome.HH + my.hh.data$HousingSubsidizedAmount.HH + my.hh.data$SSIIncome.HH +
                             my.hh.data$CalWorksIncome.HH + my.hh.data$TransitionalMAIncome.HH + my.hh.data$UnemploymentIncome.HH 

# monthly welfare income ex-EITC
my.hh.data$welfare.exEITC.inc.HH <- my.hh.data$FoodStampIncome.HH + my.hh.data$WICIncome.HH + 
                                    my.hh.data$HousingSubsidizedAmount.HH + my.hh.data$SSIIncome.HH +
                                    my.hh.data$CalWorksIncome.HH + my.hh.data$TransitionalMAIncome.HH + 
                                    my.hh.data$UnemploymentIncome.HH

# monthly capital (K) & labor (L) income
my.hh.data$KL.inc.HH <- my.hh.data$EmploymentIncome.HH + my.hh.data$OtherWorkIncome.HH + my.hh.data$BusinessIncome.HH +
                        my.hh.data$OtherLumpIncome.HH + my.hh.data$RentalIncomeAmount.HH
  
# monthly housing expenses
my.hh.data$hsg.exp.HH <- my.hh.data$MortgageAmount.HH + my.hh.data$HousingRentAmount.HH

# total monthly income at HH level
my.hh.data$TotalInc.HH <- my.hh.data$nonKL.inc.HH + my.hh.data$KL.inc.HH 

# add variable for cumulative months household is reporting in FII
my.hh.data <- ddply(my.hh.data, .(FamilyID.HH), mutate, 
                    mindate = min(Date.HH),
                    reportingmos = elapsed_months(Date.HH, mindate),
                    maxmos = max(reportingmos))

# add variable for cumulative reporting periods (i.e., number of logged FII meetings)
# some people can be in FII for years (reportingmos), but only report a few times (Periods)
my.hh.data$Count <- 1
my.hh.data <- ddply(my.hh.data, .(FamilyID.HH), mutate, 
                    Periods  = cumsum(Count),
                    maxpds = max(Periods))

# distribution of welfare income by household-month 
dist.welfare <- ggplot(data = my.hh.data) +
  geom_histogram(aes(x = welfare.inc.HH, y = ..count../sum(..count..)), binwidth = 250, fill = "white", color = "black") +
  scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(breaks = seq(0,12000,by=1000),
                      labels = c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000, 11000, 12000),
                      limits = c(0,12000),expand=c(0,0)) +
  #theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) + 
  labs(x = "Monthly Household Government Assistance Payments and Subsidies", 
       y = "Proportion of Monthly Household Observations",
       title = "Distribution of Monthly Household Government Assistance Payments and Subsidies") 
library(gridExtra)
dist.welfare.wfoot <- arrangeGrob(dist.welfare, sub = textGrob("Note: Government assistance includes child support income, EITC, SNAP, WIC, housing subsidies, SSI, TANF,\nand Unemployment Insurance benefits.", 
                                  x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontface = "plain", fontsize = 9)))
print(dist.welfare.wfoot)
ggsave(file= "Dist_Welfare.pdf", dist.welfare.wfoot)

# distribution of K&L income by household-month observation   
dist.klinc <- ggplot(data = my.hh.data) +
  geom_histogram(aes(x = KL.inc.HH, y = ..count../sum(..count..)),binwidth = 500, fill = "white", color = "black") +
  scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(breaks = seq(0,15000,by=500),
                     labels = c(0, rep("", 4), 2500, rep("", 4), 5000,  rep("", 4), 7500,  rep("", 4), 10000, rep("", 4), 12500, rep("",4), 15000),
                     limits = c(0,15000),expand=c(0,0)) +
  labs(x = "Monthly Household Capital and Labor Income", 
       y = "Proportion of Monthly Household Observations",
       title = "Distribution of Monthly Household Capital and Labor Income") 
dist.klinc.wfoot <- arrangeGrob(dist.klinc, sub = textGrob("Note: Income from capital and labor includes earned income, business income, lump sum income, rental income, and other work income.", 
                                                             x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontface = "plain", fontsize = 9)))
print(dist.klinc.wfoot)
ggsave(file= "Dist_Capital_Labor_Income.pdf", dist.klinc.wfoot)

# distribution of total income by household-month observation
dist.totinc <- ggplot(data = my.hh.data) +
  geom_histogram(aes(x = TotalInc.HH, y = ..count../sum(..count..)),binwidth = 500, fill = "white", color = "black") +
  scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(breaks = seq(0,15000,by=500),
                     labels = c(0, rep("", 4), 2500, rep("", 4), 5000,  rep("", 4), 7500,  rep("", 4), 10000, rep("", 4), 12500, rep("",4), 15000),
                     limits = c(0,15000),expand=c(0,0)) +
  labs(x = "Total Monthly Household Income", 
       y = "Proportion of Monthly Household Observations",
       title = "Distribution of Total Monthly Household Income") 
dist.totinc.wfoot <- arrangeGrob(dist.totinc, sub = textGrob("Note: Monthly total income includes income from capital, labor, and government assistance.", 
                                                             x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontface = "plain", fontsize = 9)))
print(dist.totinc.wfoot)
ggsave(file= "Dist_Total_Income.pdf", dist.totinc.wfoot)

# density plot of total income by household-month observation
plot(density(my.hh.data$TotalInc.HH)) + abline(v=median(my.hh.data$TotalInc.HH))

###################################################################################
##            COMPARE DISTRIBUTIONS OF INCOME AT BASELINE AND                    ##
##                  6, 9, AND 12 MONTHS AFTER ENROLLMENT                         ##
###################################################################################

# VARIOUS SUBSETS FOR MONTHS
sixmos.hh <- subset(my.hh.data, maxmos >= 7)
ninemos.hh <- subset(my.hh.data, maxmos >= 10)
twelvemos.hh <- subset(my.hh.data, maxmos >= 13)

sixmos.hh.one <- subset(sixmos.hh, reportingmos == 1)
ninemos.hh.one <- subset(ninemos.hh, reportingmos == 1)
twelvemos.hh.one <- subset(twelvemos.hh, reportingmos == 1)

sixmos.hh.after <- subset(sixmos.hh, reportingmos == 7)
ninemos.hh.after <- subset(ninemos.hh, reportingmos == 10)
twelvemos.hh.after <- subset(twelvemos.hh, reportingmos >= 13) # this is different to account for those who are beyond one year figure

# mean and median income figures - 6 months
TotalInc.six.one.mean <- mean(sixmos.hh.one$TotalInc.HH)
TotalInc.six.after.mean <- mean(sixmos.hh.after$TotalInc.HH)

TotalInc.six.one.med <- median(sixmos.hh.one$TotalInc.HH)
TotalInc.six.after.med <- median(sixmos.hh.after$TotalInc.HH)

KL.inc.six.one.mean <- mean(sixmos.hh.one$KL.inc.HH)
KL.inc.six.after.mean <- mean(sixmos.hh.after$KL.inc.HH)

KL.inc.six.one.med <- median(sixmos.hh.one$KL.inc.HH)
KL.inc.six.after.med <- median(sixmos.hh.after$KL.inc.HH)

Welfare.inc.six.one.mean <- mean(sixmos.hh.one$welfare.inc.HH)
Welfare.inc.six.after.mean <- mean(sixmos.hh.after$welfare.inc.HH)

Welfare.inc.six.one.med <- median(sixmos.hh.one$welfare.inc.HH)
Welfare.inc.six.after.med <- median(sixmos.hh.after$welfare.inc.HH)

# mean and median income figures - 9 months
TotalInc.nine.one.mean <- mean(ninemos.hh.one$TotalInc.HH)
TotalInc.nine.after.mean <- mean(ninemos.hh.after$TotalInc.HH)

TotalInc.nine.one.med <- median(ninemos.hh.one$TotalInc.HH)
TotalInc.nine.after.med <- median(ninemos.hh.after$TotalInc.HH)

KL.inc.nine.one.mean <- mean(ninemos.hh.one$KL.inc.HH)
KL.inc.nine.after.mean <- mean(ninemos.hh.after$KL.inc.HH)

KL.inc.nine.one.med <- median(ninemos.hh.one$KL.inc.HH)
KL.inc.nine.after.med <- median(ninemos.hh.after$KL.inc.HH)

Welfare.inc.nine.one.mean <- mean(ninemos.hh.one$welfare.inc.HH)
Welfare.inc.nine.after.mean <- mean(ninemos.hh.after$welfare.inc.HH)

Welfare.inc.nine.one.med <- median(ninemos.hh.one$welfare.inc.HH)
Welfare.inc.nine.after.med <- median(ninemos.hh.after$welfare.inc.HH)

# mean and median income figures - 12 months and beyond
TotalInc.twelve.one.mean <- mean(twelvemos.hh.one$TotalInc.HH)
TotalInc.twelve.after.mean <- mean(twelvemos.hh.after$TotalInc.HH)

TotalInc.twelve.one.med <- median(twelvemos.hh.one$TotalInc.HH)
TotalInc.twelve.after.med <- median(twelvemos.hh.after$TotalInc.HH)

KL.inc.twelve.one.mean <- mean(twelvemos.hh.one$KL.inc.HH)
KL.inc.twelve.after.mean <- mean(twelvemos.hh.after$KL.inc.HH)

KL.inc.twelve.one.med <- median(twelvemos.hh.one$KL.inc.HH)
KL.inc.twelve.after.med <- median(twelvemos.hh.after$KL.inc.HH)

Welfare.inc.twelve.one.mean <- mean(twelvemos.hh.one$welfare.inc.HH)
Welfare.inc.twelve.after.mean <- mean(twelvemos.hh.after$welfare.inc.HH)

Welfare.inc.twelve.one.med <- median(twelvemos.hh.one$welfare.inc.HH)
Welfare.inc.twelve.after.med <- median(twelvemos.hh.after$welfare.inc.HH)

Welfare.exEITC.inc.twelve.one.mean <- mean(twelvemos.hh.one$welfare.exEITC.inc.HH)
Welfare.exEITC.inc.twelve.after.mean <- mean(twelvemos.hh.after$welfare.exEITC.inc.HH)

Welfare.exEITC.inc.twelve.one.med <- median(twelvemos.hh.one$welfare.exEITC.inc.HH)
Welfare.exEITC.inc.twelve.after.med <- median(twelvemos.hh.after$welfare.exEITC.inc.HH)

# SUBSET FOR FAMILY TYPE
sixmos.hh.core <- subset(sixmos.hh, FamilyType == "FII Core")
ninemos.hh.core <- subset(ninemos.hh, FamilyType == "FII Core")
twelvemos.hh.core <- subset(twelvemos.hh, FamilyType == "FII Core")

sixmos.hh.core.one <- subset(sixmos.hh.core, reportingmos == 1)
ninemos.hh.core.one <- subset(ninemos.hh.core, reportingmos == 1)
twelvemos.hh.core.one <- subset(twelvemos.hh.core, reportingmos == 1)

sixmos.hh.core.after <- subset(sixmos.hh.core, reportingmos == 7)
ninemos.hh.core.after <- subset(ninemos.hh.core, reportingmos == 10)
twelvemos.hh.core.after <- subset(twelvemos.hh.core, reportingmos >= 13)

# mean and median income figures - 6 months | FII Core
TotalInc.six.core.one.mean <- mean(sixmos.hh.core.one$TotalInc.HH)
TotalInc.six.core.after.mean <- mean(sixmos.hh.core.after$TotalInc.HH)

TotalInc.six.core.one.med <- median(sixmos.hh.core.one$TotalInc.HH)
TotalInc.six.core.after.med <- median(sixmos.hh.core.after$TotalInc.HH)

KL.inc.six.core.one.mean <- mean(sixmos.hh.core.one$KL.inc.HH)
KL.inc.six.core.after.mean <- mean(sixmos.hh.core.after$KL.inc.HH)

KL.inc.six.core.one.med <- median(sixmos.hh.core.one$KL.inc.HH)
KL.inc.six.core.after.med <- median(sixmos.hh.core.after$KL.inc.HH)

Welfare.inc.six.core.one.mean <- mean(sixmos.hh.core.one$welfare.inc.HH)
Welfare.inc.six.core.after.mean <- mean(sixmos.hh.core.after$welfare.inc.HH)

Welfare.inc.six.core.one.med <- median(sixmos.hh.core.one$welfare.inc.HH)
Welfare.inc.six.core.after.med <- median(sixmos.hh.core.after$welfare.inc.HH)

# mean and median income figures - 9 months | FII Core
TotalInc.nine.core.one.mean <- mean(ninemos.hh.core.one$TotalInc.HH)
TotalInc.nine.core.after.mean <- mean(ninemos.hh.core.after$TotalInc.HH)

TotalInc.nine.core.one.med <- median(ninemos.hh.core.one$TotalInc.HH)
TotalInc.nine.core.after.med <- median(ninemos.hh.core.after$TotalInc.HH)

KL.inc.nine.core.one.mean <- mean(ninemos.hh.core.one$KL.inc.HH)
KL.inc.nine.core.after.mean <- mean(ninemos.hh.core.after$KL.inc.HH)

KL.inc.nine.core.one.med <- median(ninemos.hh.core.one$KL.inc.HH)
KL.inc.nine.core.after.med <- median(ninemos.hh.core.after$KL.inc.HH)

Welfare.inc.nine.core.one.mean <- mean(ninemos.hh.core.one$welfare.inc.HH)
Welfare.inc.nine.core.after.mean <- mean(ninemos.hh.core.after$welfare.inc.HH)

Welfare.inc.nine.core.one.med <- median(ninemos.hh.core.one$welfare.inc.HH)
Welfare.inc.nine.core.after.med <- median(ninemos.hh.core.after$welfare.inc.HH)

# mean and median income figures - 12 months and beyond | FII Core
TotalInc.twelve.core.one.mean <- mean(twelvemos.hh.core.one$TotalInc.HH)
TotalInc.twelve.core.after.mean <- mean(twelvemos.hh.core.after$TotalInc.HH)

TotalInc.twelve.core.one.med <- median(twelvemos.hh.core.one$TotalInc.HH)
TotalInc.twelve.core.after.med <- median(twelvemos.hh.core.after$TotalInc.HH)

KL.inc.twelve.core.one.mean <- mean(twelvemos.hh.core.one$KL.inc.HH)
KL.inc.twelve.core.after.mean <- mean(twelvemos.hh.core.after$KL.inc.HH)

KL.inc.twelve.core.one.med <- median(twelvemos.hh.core.one$KL.inc.HH)
KL.inc.twelve.core.after.med <- median(twelvemos.hh.core.after$KL.inc.HH)

Welfare.inc.twelve.core.one.mean <- mean(twelvemos.hh.core.one$welfare.inc.HH)
Welfare.inc.twelve.core.after.mean <- mean(twelvemos.hh.core.after$welfare.inc.HH)

Welfare.inc.twelve.core.one.med <- median(twelvemos.hh.core.one$welfare.inc.HH)
Welfare.inc.twelve.core.after.med <- median(twelvemos.hh.core.after$welfare.inc.HH)

Welfare.exEITC.inc.twelve.core.one.mean <- mean(twelvemos.hh.core.one$welfare.exEITC.inc.HH)
Welfare.exEITC.inc.twelve.core.after.mean <- mean(twelvemos.hh.core.after$welfare.exEITC.inc.HH)

Welfare.exEITC.inc.twelve.core.one.med <- median(twelvemos.hh.core.one$welfare.exEITC.inc.HH)
Welfare.exEITC.inc.twelve.core.after.med <- median(twelvemos.hh.core.after$welfare.exEITC.inc.HH)

#######################
## SIX MONTH CHANGES ##
#######################
# Total Income
tot.inc.six <- ggplot() + geom_density(aes(x=TotalInc.HH, color="Baseline"), data=sixmos.hh.one) +
  geom_density(aes(x=TotalInc.HH, color = "6 months"), data=sixmos.hh.after) +
  geom_vline(aes(xintercept = TotalInc.six.one.mean, colour="Baseline")) + 
  geom_vline(aes(xintercept = TotalInc.six.after.mean, colour="6 months")) + 
    labs(x = "Monthly Total Income", 
       y = "Density",
       title = "Household Total Income at Baseline and Six Months After FII Enrollment")  +
  theme(legend.title=element_blank())

# K & L Income
kl.inc.six <- ggplot() + geom_density(aes(x=KL.inc.HH, color="Baseline"), data=sixmos.hh.one) +
  geom_density(aes(x=KL.inc.HH, color= "6 months"), data=sixmos.hh.after) +
  geom_vline(aes(xintercept = KL.inc.six.one.mean, colour="Baseline")) + 
  geom_vline(aes(xintercept = KL.inc.six.after.mean, colour="6 months")) + 
  labs(x = "Monthly Capital and Labor Income", 
       y = "Density",
       title = "Household Capital and Labor Income at Baseline and Six Months After FII Enrollment")  +
  theme(legend.title=element_blank())

# Welfare Income
welfare.inc.six <- ggplot() + geom_density(aes(x=welfare.inc.HH, color="Baseline"), data=sixmos.hh.one) +
  geom_density(aes(x=welfare.inc.HH, color="6 months"), data=sixmos.hh.after) + 
  geom_vline(aes(xintercept = Welfare.inc.six.one.mean, color="Baseline")) + 
  geom_vline(aes(xintercept = Welfare.inc.six.after.mean, color="6 months")) + 
  labs(x = "Monthly Welfare Income", 
       y = "Density",
       title = "Household Welfare Income at Baseline and Six Months After FII Enrollment")  +
    theme(legend.title=element_blank())

#########################
## NINE MONTH CHANGES  ##
#########################
# Total Income
tot.inc.nine <- ggplot() + geom_density(aes(x=TotalInc.HH, color="Baseline"), data=ninemos.hh.one) +
  geom_density(aes(x=TotalInc.HH, color = "9 months"), data=ninemos.hh.after) +
  geom_vline(aes(xintercept = TotalInc.nine.one.mean, colour="Baseline")) + 
  geom_vline(aes(xintercept = TotalInc.nine.after.mean, colour="9 months")) + 
  labs(x = "Monthly Total Income", 
       y = "Density",
       title = "Household Total Income at Baseline and Nine Months After FII Enrollment")  +
  theme(legend.title=element_blank())

# K & L Income
kl.inc.nine <- ggplot() + geom_density(aes(x=KL.inc.HH, color="Baseline"), data=ninemos.hh.one) +
  geom_density(aes(x=KL.inc.HH, color= "9 months"), data=ninemos.hh.after) +
  geom_vline(aes(xintercept = KL.inc.nine.one.mean, colour="Baseline")) + 
  geom_vline(aes(xintercept = KL.inc.nine.after.mean, colour="9 months")) + 
  labs(x = "Monthly Capital and Labor Income", 
       y = "Density",
       title = "Household Capital and Labor Income at Baseline and Nine Months After FII Enrollment")  +
  theme(legend.title=element_blank())

# Welfare Income
welfare.inc.nine <- ggplot() + geom_density(aes(x=welfare.inc.HH, color="Baseline"), data=ninemos.hh.one) +
  geom_density(aes(x=welfare.inc.HH, color="9 months"), data=ninemos.hh.after) + 
  geom_vline(aes(xintercept = Welfare.inc.nine.one.mean, color="Baseline")) + 
  geom_vline(aes(xintercept = Welfare.inc.nine.after.mean, color="9 months")) + 
  labs(x = "Monthly Welfare Income", 
       y = "Density",
       title = "Household Welfare Income at Baseline and Nine Months After FII Enrollment")  +
  theme(legend.title=element_blank())

#############################
## >= TWELVE MONTH CHANGES ##
#############################
# Total Income
tot.inc.twelve <- ggplot() + geom_density(aes(x=TotalInc.HH, color="Baseline"), data=twelvemos.hh.one) +
  geom_density(aes(x=TotalInc.HH, color = "12 months or more"), data=twelvemos.hh.after) +
  geom_vline(aes(xintercept = TotalInc.twelve.one.mean, colour="Baseline")) + 
  geom_vline(aes(xintercept = TotalInc.twelve.after.mean, colour="12 months or more")) + 
  labs(x = "Monthly Total Income", 
       y = "Density",
       title = "Household Total Income at Baseline and Twelve Months or More\nAfter FII Enrollment")  +
  theme(legend.title=element_blank())
print(tot.inc.twelve)
ggsave(file= "TotInc_12mo.pdf", tot.inc.twelve)

# K & L Income
kl.inc.twelve <- ggplot() + geom_density(aes(x=KL.inc.HH, color="Baseline"), data=twelvemos.hh.one) +
  geom_density(aes(x=KL.inc.HH, color= "12 months or more"), data=twelvemos.hh.after) +
  geom_vline(aes(xintercept = KL.inc.twelve.one.mean, colour="Baseline")) + 
  geom_vline(aes(xintercept = KL.inc.twelve.after.mean, colour="12 months or more")) + 
  labs(x = "Monthly Capital and Labor Income", 
       y = "Density",
       title = "Household Capital and Labor Income at Baseline and Twelve Months or More\nAfter FII Enrollment")  +
  theme(legend.title=element_blank())

# Welfare Income
welfare.inc.twelve <- ggplot() + geom_density(aes(x=welfare.inc.HH, color="Baseline"), data=twelvemos.hh.one) +
  geom_density(aes(x=welfare.inc.HH, color="12 months or more"), data=twelvemos.hh.after) + 
  geom_vline(aes(xintercept = Welfare.inc.twelve.one.mean, color="Baseline")) + 
  geom_vline(aes(xintercept = Welfare.inc.twelve.after.mean, color="12 months or more")) + 
  labs(x = "Monthly Welfare Income", 
       y = "Density",
       title = "Household Welfare Income at Baseline and Twelve Months or More After FII Enrollment")  +
  theme(legend.title=element_blank())
print(welfare.inc.twelve)
ggsave(file= "Welfare_12mo.pdf", welfare.inc.twelve)

##################################
## SIX MONTH CHANGES - FII CORE ##
##################################
# Total Income
tot.inc.six.core <- ggplot() + geom_density(aes(x=TotalInc.HH, color="Baseline"), data=sixmos.hh.core.one) +
  geom_density(aes(x=TotalInc.HH, color = "6 months"), data=sixmos.hh.core.after) +
  geom_vline(aes(xintercept = TotalInc.six.core.one.mean, colour="Baseline")) + 
  geom_vline(aes(xintercept = TotalInc.six.core.after.mean, colour="6 months")) + 
  labs(x = "Monthly Total Income", 
       y = "Density",
       title = "Household Total Income at Baseline and Six Months After FII Enrollment for FII Core")  +
  theme(legend.title=element_blank())

# K & L Income
kl.inc.six.core <- ggplot() + geom_density(aes(x=KL.inc.HH, color="Baseline"), data=sixmos.hh.core.one) +
  geom_density(aes(x=KL.inc.HH, color= "6 months"), data=sixmos.hh.core.after) +
  geom_vline(aes(xintercept = KL.inc.six.core.one.mean, colour="Baseline")) + 
  geom_vline(aes(xintercept = KL.inc.six.core.after.mean, colour="6 months")) + 
  labs(x = "Monthly Capital and Labor Income", 
       y = "Density",
       title = "Household Capital and Labor Income at Baseline and Six Months After FII Enrollment for FII Core")  +
  theme(legend.title=element_blank())

# Welfare Income
welfare.inc.six.core <- ggplot() + geom_density(aes(x=welfare.inc.HH, color="Baseline"), data=sixmos.hh.core.one) +
  geom_density(aes(x=welfare.inc.HH, color="6 months"), data=sixmos.hh.core.after) + 
  geom_vline(aes(xintercept = Welfare.inc.six.core.one.mean, color="Baseline")) + 
  geom_vline(aes(xintercept = Welfare.inc.six.core.after.mean, color="6 months")) + 
  labs(x = "Monthly Welfare Income", 
       y = "Density",
       title = "Household Welfare Income at Baseline and Six Months After FII Enrollment for FII Core")  +
  theme(legend.title=element_blank())

###################################
## NINE MONTH CHANGES - FII Core ##
###################################
# Total Income
tot.inc.nine.core <- ggplot() + geom_density(aes(x=TotalInc.HH, color="Baseline"), data=ninemos.hh.core.one) +
  geom_density(aes(x=TotalInc.HH, color = "9 months"), data=ninemos.hh.core.after) +
  geom_vline(aes(xintercept = TotalInc.nine.core.one.mean, colour="Baseline")) + 
  geom_vline(aes(xintercept = TotalInc.nine.core.after.mean, colour="9 months")) + 
  labs(x = "Monthly Total Income", 
       y = "Density",
       title = "Household Total Income at Baseline and Nine Months After FII Enrollment for FII Core")  +
  theme(legend.title=element_blank())

# K & L Income
kl.inc.nine.core <- ggplot() + geom_density(aes(x=KL.inc.HH, color="Baseline"), data=ninemos.hh.core.one) +
  geom_density(aes(x=KL.inc.HH, color= "9 months"), data=ninemos.hh.core.after) +
  geom_vline(aes(xintercept = KL.inc.nine.core.one.mean, colour="Baseline")) + 
  geom_vline(aes(xintercept = KL.inc.nine.core.after.mean, colour="9 months")) + 
  labs(x = "Monthly Capital and Labor Income", 
       y = "Density",
       title = "Household Capital and Labor Income at Baseline and Nine Months After FII Enrollment for FII Core")  +
  theme(legend.title=element_blank())

# Welfare Income
welfare.inc.nine.core <- ggplot() + geom_density(aes(x=welfare.inc.HH, color="Baseline"), data=ninemos.hh.core.one) +
  geom_density(aes(x=welfare.inc.HH, color="9 months"), data=ninemos.hh.core.after) + 
  geom_vline(aes(xintercept = Welfare.inc.nine.core.one.mean, color="Baseline")) + 
  geom_vline(aes(xintercept = Welfare.inc.nine.core.after.mean, color="9 months")) + 
  labs(x = "Monthly Welfare Income", 
       y = "Density",
       title = "Household Welfare Income at Baseline and Nine Months After FII Enrollment for FII Core")  +
  theme(legend.title=element_blank())

#######################################
## > TWELVE MONTH CHANGES - FII Core ##
#######################################
# Total Income
tot.inc.twelve.core <- ggplot() + geom_density(aes(x=TotalInc.HH, color="Baseline"), data=twelvemos.hh.core.one) +
  geom_density(aes(x=TotalInc.HH, color = "12 months or more"), data=twelvemos.hh.core.after) +
  geom_vline(aes(xintercept = TotalInc.twelve.core.one.mean, colour="Baseline")) + 
  geom_vline(aes(xintercept = TotalInc.twelve.core.after.mean, colour="12 months or more")) + 
  labs(x = "Monthly Total Income", 
       y = "Density",
       title = "Household Total Income at Baseline and Twelve Months or More\n After FII Enrollment for FII Core")  +
  theme(legend.title=element_blank())
print(tot.inc.twelve.core)
ggsave(file= "TotalInc_12mo_Core.pdf", tot.inc.twelve.core)

# K & L Income
kl.inc.twelve.core <- ggplot() + geom_density(aes(x=KL.inc.HH, color="Baseline"), data=twelvemos.hh.core.one) +
  geom_density(aes(x=KL.inc.HH, color= "12 months or more"), data=twelvemos.hh.core.after) +
  geom_vline(aes(xintercept = KL.inc.twelve.core.one.mean, colour="Baseline")) + 
  geom_vline(aes(xintercept = KL.inc.twelve.core.after.mean, colour="12 months or more")) + 
  labs(x = "Monthly Capital and Labor Income", 
       y = "Density",
       title = "Household Capital and Labor Income at Baseline and Twelve Months or More\n After FII Enrollment for FII Core")  +
  theme(legend.title=element_blank())

# Welfare Income
welfare.inc.twelve.core <- ggplot() + geom_density(aes(x=welfare.inc.HH, color="Baseline"), data=twelvemos.hh.core.one) +
  geom_density(aes(x=welfare.inc.HH, color="12 months or more"), data=twelvemos.hh.core.after) + 
  geom_vline(aes(xintercept = Welfare.inc.twelve.core.one.mean, color="Baseline")) + 
  geom_vline(aes(xintercept = Welfare.inc.twelve.core.after.mean, color="12 months or more")) + 
  labs(x = "Monthly Welfare Income", 
       y = "Density",
       title = "Household Welfare Income at Baseline and Twelve Months or More\n After FII Enrollment for FII Core")  +
  theme(legend.title=element_blank())
print(welfare.inc.twelve.core)
ggsave(file= "Welfare_12mo_Core.pdf", welfare.inc.twelve.core)

###################################################################################
##            COMPARE DISTRIBUTIONS OF INCOME AT BASELINE AND                    ##
##            6, 9, AND 12 REPORTING PERIODS AFTER ENROLLMENT                    ##
##            Looking at dosage as opposed to pure time in FII                   ##
###################################################################################
# subset for various exposure lengths
sixpds.hh <- subset(my.hh.data, maxpds >= 7)
ninepds.hh <- subset(my.hh.data, maxpds >= 10)
twelvepds.hh <- subset(my.hh.data, maxpds >= 13)

sixpds.hh.one <- subset(sixpds.hh, Periods == 1)
ninepds.hh.one <- subset(ninepds.hh, Periods == 1)
twelvepds.hh.one <- subset(twelvepds.hh, Periods == 1)

sixpds.hh.after <- subset(sixpds.hh, Periods == 7)
ninepds.hh.after <- subset(ninepds.hh, Periods == 10)
twelvepds.hh.after <- subset(twelvepds.hh, Periods >= 13) # this is different to account for those who exceed 12 reporting periods

# mean and median income figures - 6 reports
TotalInc.sixpds.one.mean <- mean(sixpds.hh.one$TotalInc.HH)
TotalInc.sixpds.after.mean <- mean(sixpds.hh.after$TotalInc.HH)

TotalInc.sixpds.one.med <- median(sixpds.hh.one$TotalInc.HH)
TotalInc.sixpds.after.med <- median(sixpds.hh.after$TotalInc.HH)

KL.inc.sixpds.one.mean <- mean(sixpds.hh.one$KL.inc.HH)
KL.inc.sixpds.after.mean <- mean(sixpds.hh.after$KL.inc.HH)

KL.inc.sixpds.one.med <- median(sixpds.hh.one$KL.inc.HH)
KL.inc.sixpds.after.med <- median(sixpds.hh.after$KL.inc.HH)

Welfare.inc.sixpds.one.mean <- mean(sixpds.hh.one$welfare.inc.HH)
Welfare.inc.sixpds.after.mean <- mean(sixpds.hh.after$welfare.inc.HH)

Welfare.inc.sixpds.one.med <- median(sixpds.hh.one$welfare.inc.HH)
Welfare.inc.sixpds.after.med <- median(sixpds.hh.after$welfare.inc.HH)

# mean and median income figures - 9 reports
TotalInc.ninepds.one.mean <- mean(ninepds.hh.one$TotalInc.HH)
TotalInc.ninepds.after.mean <- mean(ninepds.hh.after$TotalInc.HH)

TotalInc.ninepds.one.med <- median(ninepds.hh.one$TotalInc.HH)
TotalInc.ninepds.after.med <- median(ninepds.hh.after$TotalInc.HH)

KL.inc.ninepds.one.mean <- mean(ninepds.hh.one$KL.inc.HH)
KL.inc.ninepds.after.mean <- mean(ninepds.hh.after$KL.inc.HH)

KL.inc.ninepds.one.med <- median(ninepds.hh.one$KL.inc.HH)
KL.inc.ninepds.after.med <- median(ninepds.hh.after$KL.inc.HH)

Welfare.inc.ninepds.one.mean <- mean(ninepds.hh.one$welfare.inc.HH)
Welfare.inc.ninepds.after.mean <- mean(ninepds.hh.after$welfare.inc.HH)

Welfare.inc.ninepds.one.med <- median(ninepds.hh.one$welfare.inc.HH)
Welfare.inc.ninepds.after.med <- median(ninepds.hh.after$welfare.inc.HH)

# mean and median income figures - 12 reports and beyond
TotalInc.twelvepds.one.mean <- mean(twelvepds.hh.one$TotalInc.HH)
TotalInc.twelvepds.after.mean <- mean(twelvepds.hh.after$TotalInc.HH)

TotalInc.twelvepds.one.med <- median(twelvepds.hh.one$TotalInc.HH)
TotalInc.twelvepds.after.med <- median(twelvepds.hh.after$TotalInc.HH)

KL.inc.twelvepds.one.mean <- mean(twelvepds.hh.one$KL.inc.HH)
KL.inc.twelvepds.after.mean <- mean(twelvepds.hh.after$KL.inc.HH)

KL.inc.twelvepds.one.med <- median(twelvepds.hh.one$KL.inc.HH)
KL.inc.twelvepds.after.med <- median(twelvepds.hh.after$KL.inc.HH)

Welfare.inc.twelvepds.one.mean <- mean(twelvepds.hh.one$welfare.inc.HH)
Welfare.inc.twelvepds.after.mean <- mean(twelvepds.hh.after$welfare.inc.HH)

Welfare.inc.twelvepds.one.med <- median(twelvepds.hh.one$welfare.inc.HH)
Welfare.inc.twelvepds.after.med <- median(twelvepds.hh.after$welfare.inc.HH)

Welfare.exEITC.inc.twelvepds.one.mean <- mean(twelvepds.hh.one$welfare.exEITC.inc.HH)
Welfare.exEITC.inc.twelvepds.after.mean <- mean(twelvepds.hh.after$welfare.exEITC.inc.HH)

Welfare.exEITC.inc.twelvepds.one.med <- median(twelvepds.hh.one$welfare.exEITC.inc.HH)
Welfare.exEITC.inc.twelvepds.after.med <- median(twelvepds.hh.after$welfare.exEITC.inc.HH)

# SUBSET FOR FAMILY TYPE
sixpds.hh.core <- subset(sixpds.hh, FamilyType == "FII Core")
ninepds.hh.core <- subset(ninepds.hh, FamilyType == "FII Core")
twelvepds.hh.core <- subset(twelvepds.hh, FamilyType == "FII Core")

sixpds.hh.core.one <- subset(sixpds.hh.core, Periods == 1)
ninepds.hh.core.one <- subset(ninepds.hh.core, Periods == 1)
twelvepds.hh.core.one <- subset(twelvepds.hh.core, Periods == 1)

sixpds.hh.core.after <- subset(sixpds.hh.core, Periods == 7)
ninepds.hh.core.after <- subset(ninepds.hh.core, Periods == 10)
twelvepds.hh.core.after <- subset(twelvepds.hh.core, Periods >= 13)

# mean and median income figures - 6 periods | FII Core
TotalInc.sixpds.core.one.mean <- mean(sixpds.hh.core.one$TotalInc.HH)
TotalInc.sixpds.core.after.mean <- mean(sixpds.hh.core.after$TotalInc.HH)

TotalInc.sixpds.core.one.med <- median(sixpds.hh.core.one$TotalInc.HH)
TotalInc.sixpds.core.after.med <- median(sixpds.hh.core.after$TotalInc.HH)

KL.inc.sixpds.core.one.mean <- mean(sixpds.hh.core.one$KL.inc.HH)
KL.inc.sixpds.core.after.mean <- mean(sixpds.hh.core.after$KL.inc.HH)

KL.inc.sixpds.core.one.med <- median(sixpds.hh.core.one$KL.inc.HH)
KL.inc.sixpds.core.after.med <- median(sixpds.hh.core.after$KL.inc.HH)

Welfare.inc.sixpds.core.one.mean <- mean(sixpds.hh.core.one$welfare.inc.HH)
Welfare.inc.sixpds.core.after.mean <- mean(sixpds.hh.core.after$welfare.inc.HH)

Welfare.inc.sixpds.core.one.med <- median(sixpds.hh.core.one$welfare.inc.HH)
Welfare.inc.sixpds.core.after.med <- median(sixpds.hh.core.after$welfare.inc.HH)

# mean and median income figures - 9 periods | FII Core
TotalInc.ninepds.core.one.mean <- mean(ninepds.hh.core.one$TotalInc.HH)
TotalInc.ninepds.core.after.mean <- mean(ninepds.hh.core.after$TotalInc.HH)

TotalInc.ninepds.core.one.med <- median(ninepds.hh.core.one$TotalInc.HH)
TotalInc.ninepds.core.after.med <- median(ninepds.hh.core.after$TotalInc.HH)

KL.inc.ninepds.core.one.mean <- mean(ninepds.hh.core.one$KL.inc.HH)
KL.inc.ninepds.core.after.mean <- mean(ninepds.hh.core.after$KL.inc.HH)

KL.inc.ninepds.core.one.med <- median(ninepds.hh.core.one$KL.inc.HH)
KL.inc.ninepds.core.after.med <- median(ninepds.hh.core.after$KL.inc.HH)

Welfare.inc.ninepds.core.one.mean <- mean(ninepds.hh.core.one$welfare.inc.HH)
Welfare.inc.ninepds.core.after.mean <- mean(ninepds.hh.core.after$welfare.inc.HH)

Welfare.inc.ninepds.core.one.med <- median(ninepds.hh.core.one$welfare.inc.HH)
Welfare.inc.ninepds.core.after.med <- median(ninepds.hh.core.after$welfare.inc.HH)

# mean and median income figures - 12 periods and beyond | FII Core
TotalInc.twelvepds.core.one.mean <- mean(twelvepds.hh.core.one$TotalInc.HH)
TotalInc.twelvepds.core.after.mean <- mean(twelvepds.hh.core.after$TotalInc.HH)

TotalInc.twelvepds.core.one.med <- median(twelvepds.hh.core.one$TotalInc.HH)
TotalInc.twelvepds.core.after.med <- median(twelvepds.hh.core.after$TotalInc.HH)

KL.inc.twelvepds.core.one.mean <- mean(twelvepds.hh.core.one$KL.inc.HH)
KL.inc.twelvepds.core.after.mean <- mean(twelvepds.hh.core.after$KL.inc.HH)

KL.inc.twelvepds.core.one.med <- median(twelvepds.hh.core.one$KL.inc.HH)
KL.inc.twelvepds.core.after.med <- median(twelvepds.hh.core.after$KL.inc.HH)

Welfare.inc.twelvepds.core.one.mean <- mean(twelvepds.hh.core.one$welfare.inc.HH)
Welfare.inc.twelvepds.core.after.mean <- mean(twelvepds.hh.core.after$welfare.inc.HH)

Welfare.inc.twelvepds.core.one.med <- median(twelvepds.hh.core.one$welfare.inc.HH)
Welfare.inc.twelvepds.core.after.med <- median(twelvepds.hh.core.after$welfare.inc.HH)

Welfare.exEITC.inc.twelvepds.core.one.mean <- mean(twelvepds.hh.core.one$welfare.exEITC.inc.HH)
Welfare.exEITC.inc.twelvepds.core.after.mean <- mean(twelvepds.hh.core.after$welfare.exEITC.inc.HH)

Welfare.exEITC.inc.twelvepds.core.one.med <- median(twelvepds.hh.core.one$welfare.exEITC.inc.HH)
Welfare.exEITC.inc.twelvepds.core.after.med <- median(twelvepds.hh.core.after$welfare.exEITC.inc.HH)

########################
## SIX PERIOD CHANGES ##
########################
# Total Income
tot.inc.sixpds <- ggplot() + geom_density(aes(x=TotalInc.HH, color="Baseline"), data=sixpds.hh.one) +
  geom_density(aes(x=TotalInc.HH, color = "6 periods"), data=sixpds.hh.after) +
  geom_vline(aes(xintercept = TotalInc.sixpds.one.mean, colour="Baseline")) + 
  geom_vline(aes(xintercept = TotalInc.sixpds.after.mean, colour="6 periods")) + 
  labs(x = "Monthly Total Income", 
       y = "Density",
       title = "Household Total Income at Baseline and Six Reporting Periods After FII Enrollment")  +
  theme(legend.title=element_blank())

# K & L Income
kl.inc.sixpds <- ggplot() + geom_density(aes(x=KL.inc.HH, color="Baseline"), data=sixpds.hh.one) +
  geom_density(aes(x=KL.inc.HH, color= "6 periods"), data=sixpds.hh.after) +
  geom_vline(aes(xintercept = KL.inc.sixpds.one.mean, colour="Baseline")) + 
  geom_vline(aes(xintercept = KL.inc.sixpds.after.mean, colour="6 periods")) + 
  labs(x = "Monthly Capital and Labor Income", 
       y = "Density",
       title = "Household Capital and Labor Income at Baseline and Six Reporting Periods After FII Enrollment")  +
  theme(legend.title=element_blank())

# Welfare Income
welfare.inc.sixpds <- ggplot() + geom_density(aes(x=welfare.inc.HH, color="Baseline"), data=sixpds.hh.one) +
  geom_density(aes(x=welfare.inc.HH, color="6 periods"), data=sixpds.hh.after) + 
  geom_vline(aes(xintercept = Welfare.inc.six.one.mean, color="Baseline")) + 
  geom_vline(aes(xintercept = Welfare.inc.six.after.mean, color="6 periods")) + 
  labs(x = "Monthly Welfare Income", 
       y = "Density",
       title = "Household Welfare Income at Baseline and Six Reporting Periods After FII Enrollment")  +
  theme(legend.title=element_blank())

#########################
## NINE PERIOD CHANGES ##
#########################
# Total Income
tot.inc.ninepds <- ggplot() + geom_density(aes(x=TotalInc.HH, color="Baseline"), data=ninepds.hh.one) +
  geom_density(aes(x=TotalInc.HH, color = "9 periods"), data=ninepds.hh.after) +
  geom_vline(aes(xintercept = TotalInc.ninepds.one.mean, colour="Baseline")) + 
  geom_vline(aes(xintercept = TotalInc.ninepds.after.mean, colour="9 periods")) + 
  labs(x = "Monthly Total Income", 
       y = "Density",
       title = "Household Total Income at Baseline and Nine Reporting Periods After FII Enrollment")  +
  theme(legend.title=element_blank())

# K & L Income
kl.inc.ninepds <- ggplot() + geom_density(aes(x=KL.inc.HH, color="Baseline"), data=ninepds.hh.one) +
  geom_density(aes(x=KL.inc.HH, color= "9 periods"), data=ninepds.hh.after) +
  geom_vline(aes(xintercept = KL.inc.ninepds.one.mean, colour="Baseline")) + 
  geom_vline(aes(xintercept = KL.inc.ninepds.after.mean, colour="9 periods")) + 
  labs(x = "Monthly Capital and Labor Income", 
       y = "Density",
       title = "Household Capital and Labor Income at Baseline and Nine Reporting Periods After FII Enrollment")  +
  theme(legend.title=element_blank())

# Welfare Income
welfare.inc.ninepds <- ggplot() + geom_density(aes(x=welfare.inc.HH, color="Baseline"), data=ninepds.hh.one) +
  geom_density(aes(x=welfare.inc.HH, color="9 periods"), data=ninepds.hh.after) + 
  geom_vline(aes(xintercept = Welfare.inc.nine.one.mean, color="Baseline")) + 
  geom_vline(aes(xintercept = Welfare.inc.nine.after.mean, color="9 periods")) + 
  labs(x = "Monthly Welfare Income", 
       y = "Density",
       title = "Household Welfare Income at Baseline and Nine Reporting Periods After FII Enrollment")  +
  theme(legend.title=element_blank())

###########################
## TWELVE PERIOD CHANGES ##
###########################
# Total Income
tot.inc.twelvepds <- ggplot() + geom_density(aes(x=TotalInc.HH, color="Baseline"), data=twelvepds.hh.one) +
  geom_density(aes(x=TotalInc.HH, color = "12 periods"), data=twelvepds.hh.after) +
  geom_vline(aes(xintercept = TotalInc.twelvepds.one.mean, colour="Baseline")) + 
  geom_vline(aes(xintercept = TotalInc.twelvepds.after.mean, colour="12 periods")) + 
  labs(x = "Monthly Total Income", 
       y = "Density",
       title = "Household Total Income at Baseline and Twelve Reporting Periods\n After FII Enrollment")  +
  theme(legend.title=element_blank())
print(tot.inc.twelvepds)
ggsave(file= "TotalInc_12pds.pdf", tot.inc.twelvepds)

# K & L Income
kl.inc.twelvepds <- ggplot() + geom_density(aes(x=KL.inc.HH, color="Baseline"), data=twelvepds.hh.one) +
  geom_density(aes(x=KL.inc.HH, color= "12 periods"), data=twelvepds.hh.after) +
  geom_vline(aes(xintercept = KL.inc.twelvepds.one.mean, colour="Baseline")) + 
  geom_vline(aes(xintercept = KL.inc.twelvepds.after.mean, colour="12 periods")) + 
  labs(x = "Monthly Capital and Labor Income", 
       y = "Density",
       title = "Household Capital and Labor Income at Baseline and Twelve Reporting Periods After FII Enrollment")  +
  theme(legend.title=element_blank())

# Welfare Income
welfare.inc.twelvepds <- ggplot() + geom_density(aes(x=welfare.inc.HH, color="Baseline"), data=twelvepds.hh.one) +
  geom_density(aes(x=welfare.inc.HH, color="12 periods"), data=twelvepds.hh.after) + 
  geom_vline(aes(xintercept = Welfare.inc.twelvepds.one.mean, color="Baseline")) + 
  geom_vline(aes(xintercept = Welfare.inc.twelvepds.after.mean, color="12 periods")) + 
  labs(x = "Monthly Welfare Income", 
       y = "Density",
       title = "Household Welfare Income at Baseline and Twelve Reporting Periods\n After FII Enrollment")  +
  theme(legend.title=element_blank())
print(welfare.inc.twelvepds)
ggsave(file= "Welfare_12pds.pdf", welfare.inc.twelvepds)

###################################
## SIX PERIOD CHANGES - FII CORE ##
###################################
# Total Income
tot.inc.sixpds.core <- ggplot() + geom_density(aes(x=TotalInc.HH, color="Baseline"), data=sixpds.hh.core.one) +
  geom_density(aes(x=TotalInc.HH, color = "6 periods"), data=sixpds.hh.core.after) +
  geom_vline(aes(xintercept = TotalInc.sixpds.core.one.mean, colour="Baseline")) + 
  geom_vline(aes(xintercept = TotalInc.sixpds.core.after.mean, colour="6 periods")) + 
  labs(x = "Monthly Total Income", 
       y = "Density",
       title = "Household Total Income at Baseline and Six Periods After FII Enrollment for FII Core")  +
  theme(legend.title=element_blank())

# K & L Income
kl.inc.sixpds.core <- ggplot() + geom_density(aes(x=KL.inc.HH, color="Baseline"), data=sixpds.hh.core.one) +
  geom_density(aes(x=KL.inc.HH, color= "6 periods"), data=sixpds.hh.core.after) +
  geom_vline(aes(xintercept = KL.inc.sixpds.core.one.mean, colour="Baseline")) + 
  geom_vline(aes(xintercept = KL.inc.sixpds.core.after.mean, colour="6 months")) + 
  labs(x = "Monthly Capital and Labor Income", 
       y = "Density",
       title = "Household Capital and Labor Income at Baseline and Six Periods After FII Enrollment for FII Core")  +
  theme(legend.title=element_blank())

# Welfare Income
welfare.inc.sixpds.core <- ggplot() + geom_density(aes(x=welfare.inc.HH, color="Baseline"), data=sixpds.hh.core.one) +
  geom_density(aes(x=welfare.inc.HH, color="6 periods"), data=sixpds.hh.core.after) + 
  geom_vline(aes(xintercept = Welfare.inc.sixpds.core.one.mean, color="Baseline")) + 
  geom_vline(aes(xintercept = Welfare.inc.sixpds.core.after.mean, color="6 periods")) + 
  labs(x = "Monthly Welfare Income", 
       y = "Density",
       title = "Household Welfare Income at Baseline and Six Periods After FII Enrollment for FII Core")  +
  theme(legend.title=element_blank())

####################################
## NINE PERIOD CHANGES - FII Core ##
####################################
# Total Income
tot.inc.ninepds.core <- ggplot() + geom_density(aes(x=TotalInc.HH, color="Baseline"), data=ninepds.hh.core.one) +
  geom_density(aes(x=TotalInc.HH, color = "9 periods"), data=ninepds.hh.core.after) +
  geom_vline(aes(xintercept = TotalInc.ninepds.core.one.mean, colour="Baseline")) + 
  geom_vline(aes(xintercept = TotalInc.ninepds.core.after.mean, colour="9 periods")) + 
  labs(x = "Monthly Total Income", 
       y = "Density",
       title = "Household Total Income at Baseline and Nine Periods After FII Enrollment for FII Core")  +
  theme(legend.title=element_blank())

# K & L Income
kl.inc.ninepds.core <- ggplot() + geom_density(aes(x=KL.inc.HH, color="Baseline"), data=ninepds.hh.core.one) +
  geom_density(aes(x=KL.inc.HH, color= "9 periods"), data=ninepds.hh.core.after) +
  geom_vline(aes(xintercept = KL.inc.ninepds.core.one.mean, colour="Baseline")) + 
  geom_vline(aes(xintercept = KL.inc.ninepds.core.after.mean, colour="9 periods")) + 
  labs(x = "Monthly Capital and Labor Income", 
       y = "Density",
       title = "Household Capital and Labor Income at Baseline and Nine Periods After FII Enrollment for FII Core")  +
  theme(legend.title=element_blank())

# Welfare Income
welfare.inc.ninepds.core <- ggplot() + geom_density(aes(x=welfare.inc.HH, color="Baseline"), data=ninepds.hh.core.one) +
  geom_density(aes(x=welfare.inc.HH, color="9 periods"), data=ninepds.hh.core.after) + 
  geom_vline(aes(xintercept = Welfare.inc.ninepds.core.one.mean, color="Baseline")) + 
  geom_vline(aes(xintercept = Welfare.inc.ninepds.core.after.mean, color="9 periods")) + 
  labs(x = "Monthly Welfare Income", 
       y = "Density",
       title = "Household Welfare Income at Baseline and Nine Periods After FII Enrollment for FII Core")  +
  theme(legend.title=element_blank())

#######################################
## TWELVE PERIOD CHANGES - FII Core  ##
#######################################
# Total Income
tot.inc.twelvepds.core <- ggplot() + geom_density(aes(x=TotalInc.HH, color="Baseline"), data=twelvepds.hh.core.one) +
  geom_density(aes(x=TotalInc.HH, color = "12 periods"), data=twelvepds.hh.core.after) +
  geom_vline(aes(xintercept = TotalInc.twelvepds.core.one.mean, colour="Baseline")) + 
  geom_vline(aes(xintercept = TotalInc.twelvepds.core.after.mean, colour="12 periods")) + 
  labs(x = "Monthly Total Income", 
       y = "Density",
       title = "Household Total Income at Baseline and Twelve Periods\n After FII Enrollment for FII Core")  +
  theme(legend.title=element_blank())
print(tot.inc.twelvepds.core)
ggsave(file= "TotInc_12pds_Core.pdf", tot.inc.twelvepds.core)

# K & L Income
kl.inc.twelvepds.core <- ggplot() + geom_density(aes(x=KL.inc.HH, color="Baseline"), data=twelvepds.hh.core.one) +
  geom_density(aes(x=KL.inc.HH, color= "12 periods"), data=twelvepds.hh.core.after) +
  geom_vline(aes(xintercept = KL.inc.twelvepds.core.one.mean, colour="Baseline")) + 
  geom_vline(aes(xintercept = KL.inc.twelvepds.core.after.mean, colour="12 periods")) + 
  labs(x = "Monthly Capital and Labor Income", 
       y = "Density",
       title = "Household Capital and Labor Income at Baseline and Twelve Periods After FII Enrollment for FII Core")  +
  theme(legend.title=element_blank())

# Welfare Income
welfare.inc.twelvepds.core <- ggplot() + geom_density(aes(x=welfare.inc.HH, color="Baseline"), data=twelvepds.hh.core.one) +
  geom_density(aes(x=welfare.inc.HH, color="12 periods"), data=twelvepds.hh.core.after) + 
  geom_vline(aes(xintercept = Welfare.inc.twelvepds.core.one.mean, color="Baseline")) + 
  geom_vline(aes(xintercept = Welfare.inc.twelvepds.core.after.mean, color="12 periods")) + 
  labs(x = "Monthly Welfare Income", 
       y = "Density",
       title = "Household Welfare Income at Baseline and Twelve Periods\n After FII Enrollment for FII Core")  +
  theme(legend.title=element_blank())
print(welfare.inc.twelvepds.core)
ggsave(file= "Welfare_12pds_Core.pdf", welfare.inc.twelvepds.core)

####################################################
##      Summary of 12-Month Income Changes by     ##    
##        Service Location and Family Type        ##
####################################################
## BOSTON
# core
BOS.total.core <- mean(twelvemos.hh.core.after$TotalInc.HH[twelvemos.hh.core.after$ServiceLocation=="BOSTON" & twelvemos.hh.core.after$FamilyType=="FII Core"]) - 
                  mean(twelvemos.hh.core.one$TotalInc.HH[twelvemos.hh.core.one$ServiceLocation=="BOSTON" & twelvemos.hh.core.one$FamilyType=="FII Core"])
BOS.welfare.core <- mean(twelvemos.hh.core.after$welfare.inc.HH[twelvemos.hh.core.after$ServiceLocation=="BOSTON" & twelvemos.hh.core.after$FamilyType=="FII Core"]) - 
                    mean(twelvemos.hh.core.one$welfare.inc.HH[twelvemos.hh.core.one$ServiceLocation=="BOSTON" & twelvemos.hh.core.one$FamilyType=="FII Core"])
BOS.kl.core <- mean(twelvemos.hh.core.after$KL.inc.HH[twelvemos.hh.core.after$ServiceLocation=="BOSTON" & twelvemos.hh.core.after$FamilyType=="FII Core"]) - 
               mean(twelvemos.hh.core.one$KL.inc.HH[twelvemos.hh.core.one$ServiceLocation=="BOSTON" & twelvemos.hh.core.one$FamilyType=="FII Core"])
BOS.core.number <- length(twelvemos.hh.core.one$TotalInc.HH[twelvemos.hh.core.one$ServiceLocation=="BOSTON" & twelvemos.hh.core.one$FamilyType=="FII Core"])  

# overall
BOS.total <- mean(twelvemos.hh.after$TotalInc.HH[twelvemos.hh.after$ServiceLocation=="BOSTON"]) - 
             mean(twelvemos.hh.one$TotalInc.HH[twelvemos.hh.one$ServiceLocation=="BOSTON"])
BOS.welfare <- mean(twelvemos.hh.after$welfare.inc.HH[twelvemos.hh.after$ServiceLocation=="BOSTON"]) - 
               mean(twelvemos.hh.one$welfare.inc.HH[twelvemos.hh.one$ServiceLocation=="BOSTON"])
BOS.kl <- mean(twelvemos.hh.after$KL.inc.HH[twelvemos.hh.after$ServiceLocation=="BOSTON"]) - 
          mean(twelvemos.hh.one$KL.inc.HH[twelvemos.hh.one$ServiceLocation=="BOSTON"])
BOS.number <- length(twelvemos.hh.one$TotalInc.HH[twelvemos.hh.one$ServiceLocation=="BOSTON"])

## DETROIT
# core
DET.total.core <- mean(twelvemos.hh.core.after$TotalInc.HH[twelvemos.hh.core.after$ServiceLocation=="DETROIT" & twelvemos.hh.core.after$FamilyType=="FII Core"]) - 
                  mean(twelvemos.hh.core.one$TotalInc.HH[twelvemos.hh.core.one$ServiceLocation=="DETROIT" & twelvemos.hh.core.one$FamilyType=="FII Core"])
DET.welfare.core <- mean(twelvemos.hh.core.after$welfare.inc.HH[twelvemos.hh.core.after$ServiceLocation=="DETROIT" & twelvemos.hh.core.after$FamilyType=="FII Core"]) - 
                    mean(twelvemos.hh.core.one$welfare.inc.HH[twelvemos.hh.core.one$ServiceLocation=="DETROIT" & twelvemos.hh.core.one$FamilyType=="FII Core"])
DET.kl.core <- mean(twelvemos.hh.core.after$KL.inc.HH[twelvemos.hh.core.after$ServiceLocation=="DETROIT" & twelvemos.hh.core.after$FamilyType=="FII Core"]) - 
               mean(twelvemos.hh.core.one$KL.inc.HH[twelvemos.hh.core.one$ServiceLocation=="DETROIT" & twelvemos.hh.core.one$FamilyType=="FII Core"])
DET.core.number <- length(twelvemos.hh.core.one$TotalInc.HH[twelvemos.hh.core.one$ServiceLocation=="DETROIT" & twelvemos.hh.core.one$FamilyType=="FII Core"])  

# overall
DET.total <- mean(twelvemos.hh.after$TotalInc.HH[twelvemos.hh.after$ServiceLocation=="DETROIT"]) - 
             mean(twelvemos.hh.one$TotalInc.HH[twelvemos.hh.one$ServiceLocation=="DETROIT"])
DET.welfare <- mean(twelvemos.hh.after$welfare.inc.HH[twelvemos.hh.after$ServiceLocation=="DETROIT"]) - 
               mean(twelvemos.hh.one$welfare.inc.HH[twelvemos.hh.one$ServiceLocation=="DETROIT"])
DET.kl <- mean(twelvemos.hh.after$KL.inc.HH[twelvemos.hh.after$ServiceLocation=="DETROIT"]) - 
          mean(twelvemos.hh.one$KL.inc.HH[twelvemos.hh.one$ServiceLocation=="DETROIT"])
DET.number <- length(twelvemos.hh.one$TotalInc.HH[twelvemos.hh.one$ServiceLocation=="DETROIT"])

## FRESNO
# core
FRESNO.total.core <- mean(twelvemos.hh.core.after$TotalInc.HH[twelvemos.hh.core.after$ServiceLocation=="FRESNO" & twelvemos.hh.core.after$FamilyType=="FII Core"]) - 
                     mean(twelvemos.hh.core.one$TotalInc.HH[twelvemos.hh.core.one$ServiceLocation=="FRESNO" & twelvemos.hh.core.one$FamilyType=="FII Core"])
FRESNO.welfare.core <- mean(twelvemos.hh.core.after$welfare.inc.HH[twelvemos.hh.core.after$ServiceLocation=="FRESNO" & twelvemos.hh.core.after$FamilyType=="FII Core"]) - 
                       mean(twelvemos.hh.core.one$welfare.inc.HH[twelvemos.hh.core.one$ServiceLocation=="FRESNO" & twelvemos.hh.core.one$FamilyType=="FII Core"])
FRESNO.kl.core <- mean(twelvemos.hh.core.after$KL.inc.HH[twelvemos.hh.core.after$ServiceLocation=="FRESNO" & twelvemos.hh.core.after$FamilyType=="FII Core"]) - 
                  mean(twelvemos.hh.core.one$KL.inc.HH[twelvemos.hh.core.one$ServiceLocation=="FRESNO" & twelvemos.hh.core.one$FamilyType=="FII Core"])
FRESNO.core.number <- length(twelvemos.hh.core.one$TotalInc.HH[twelvemos.hh.core.one$ServiceLocation=="FRESNO" & twelvemos.hh.core.one$FamilyType=="FII Core"])  

# overall
FRESNO.total <- mean(twelvemos.hh.after$TotalInc.HH[twelvemos.hh.after$ServiceLocation=="FRESNO"]) - 
                mean(twelvemos.hh.one$TotalInc.HH[twelvemos.hh.one$ServiceLocation=="FRESNO"])
FRESNO.welfare <- mean(twelvemos.hh.after$welfare.inc.HH[twelvemos.hh.after$ServiceLocation=="FRESNO"]) - 
                  mean(twelvemos.hh.one$welfare.inc.HH[twelvemos.hh.one$ServiceLocation=="FRESNO"])
FRESNO.kl <- mean(twelvemos.hh.after$KL.inc.HH[twelvemos.hh.after$ServiceLocation=="FRESNO"]) - 
             mean(twelvemos.hh.one$KL.inc.HH[twelvemos.hh.one$ServiceLocation=="FRESNO"])
FRESNO.number <- length(twelvemos.hh.one$TotalInc.HH[twelvemos.hh.one$ServiceLocation=="FRESNO"])

## NEW ORLEANS
# core
NO.total.core <- mean(twelvemos.hh.core.after$TotalInc.HH[twelvemos.hh.core.after$ServiceLocation=="NEWORLEANS" & twelvemos.hh.core.after$FamilyType=="FII Core"]) - 
                 mean(twelvemos.hh.core.one$TotalInc.HH[twelvemos.hh.core.one$ServiceLocation=="NEWORLEANS" & twelvemos.hh.core.one$FamilyType=="FII Core"])
NO.welfare.core <- mean(twelvemos.hh.core.after$welfare.inc.HH[twelvemos.hh.core.after$ServiceLocation=="NEWORLEANS" & twelvemos.hh.core.after$FamilyType=="FII Core"]) - 
                   mean(twelvemos.hh.core.one$welfare.inc.HH[twelvemos.hh.core.one$ServiceLocation=="NEWORLEANS" & twelvemos.hh.core.one$FamilyType=="FII Core"])
NO.kl.core <- mean(twelvemos.hh.core.after$KL.inc.HH[twelvemos.hh.core.after$ServiceLocation=="NEWORLEANS" & twelvemos.hh.core.after$FamilyType=="FII Core"]) - 
              mean(twelvemos.hh.core.one$KL.inc.HH[twelvemos.hh.core.one$ServiceLocation=="NEWORLEANS" & twelvemos.hh.core.one$FamilyType=="FII Core"])
NO.core.number <- length(twelvemos.hh.core.one$TotalInc.HH[twelvemos.hh.core.one$ServiceLocation=="NEWORLEANS" & twelvemos.hh.core.one$FamilyType=="FII Core"])  

# overall
NO.total <- mean(twelvemos.hh.after$TotalInc.HH[twelvemos.hh.after$ServiceLocation=="NEWORLEANS"]) - 
            mean(twelvemos.hh.one$TotalInc.HH[twelvemos.hh.one$ServiceLocation=="NEWORLEANS"])
NO.welfare <- mean(twelvemos.hh.after$welfare.inc.HH[twelvemos.hh.after$ServiceLocation=="NEWORLEANS"]) - 
              mean(twelvemos.hh.one$welfare.inc.HH[twelvemos.hh.one$ServiceLocation=="NEWORLEANS"])
NO.kl <- mean(twelvemos.hh.after$KL.inc.HH[twelvemos.hh.after$ServiceLocation=="NEWORLEANS"]) - 
         mean(twelvemos.hh.one$KL.inc.HH[twelvemos.hh.one$ServiceLocation=="NEWORLEANS"])
NO.number <- length(twelvemos.hh.one$TotalInc.HH[twelvemos.hh.one$ServiceLocation=="NEWORLEANS"])

## OAKLAND
# core
OAK.total.core <- mean(twelvemos.hh.core.after$TotalInc.HH[twelvemos.hh.core.after$ServiceLocation=="OAKLAND" & twelvemos.hh.core.after$FamilyType=="FII Core"]) - 
                  mean(twelvemos.hh.core.one$TotalInc.HH[twelvemos.hh.core.one$ServiceLocation=="OAKLAND" & twelvemos.hh.core.one$FamilyType=="FII Core"])
OAK.welfare.core <- mean(twelvemos.hh.core.after$welfare.inc.HH[twelvemos.hh.core.after$ServiceLocation=="OAKLAND" & twelvemos.hh.core.after$FamilyType=="FII Core"]) - 
                    mean(twelvemos.hh.core.one$welfare.inc.HH[twelvemos.hh.core.one$ServiceLocation=="OAKLAND" & twelvemos.hh.core.one$FamilyType=="FII Core"])
OAK.kl.core <- mean(twelvemos.hh.core.after$KL.inc.HH[twelvemos.hh.core.after$ServiceLocation=="OAKLAND" & twelvemos.hh.core.after$FamilyType=="FII Core"]) - 
               mean(twelvemos.hh.core.one$KL.inc.HH[twelvemos.hh.core.one$ServiceLocation=="OAKLAND" & twelvemos.hh.core.one$FamilyType=="FII Core"])
OAK.core.number <- length(twelvemos.hh.core.one$TotalInc.HH[twelvemos.hh.core.one$ServiceLocation=="OAKLAND" & twelvemos.hh.core.one$FamilyType=="FII Core"])  

# overall
OAK.total <- mean(twelvemos.hh.after$TotalInc.HH[twelvemos.hh.after$ServiceLocation=="OAKLAND"]) - 
             mean(twelvemos.hh.one$TotalInc.HH[twelvemos.hh.one$ServiceLocation=="OAKLAND"])
OAK.welfare <- mean(twelvemos.hh.after$welfare.inc.HH[twelvemos.hh.after$ServiceLocation=="OAKLAND"]) - 
               mean(twelvemos.hh.one$welfare.inc.HH[twelvemos.hh.one$ServiceLocation=="OAKLAND"])
OAK.kl <- mean(twelvemos.hh.after$KL.inc.HH[twelvemos.hh.after$ServiceLocation=="OAKLAND"]) - 
          mean(twelvemos.hh.one$KL.inc.HH[twelvemos.hh.one$ServiceLocation=="OAKLAND"])
OAK.number <- length(twelvemos.hh.one$TotalInc.HH[twelvemos.hh.one$ServiceLocation=="OAKLAND"])

## SAN FRANCISCO
# core
SF.total.core <- mean(twelvemos.hh.core.after$TotalInc.HH[twelvemos.hh.core.after$ServiceLocation=="SANFRANCISCO" & twelvemos.hh.core.after$FamilyType=="FII Core"]) - 
                 mean(twelvemos.hh.core.one$TotalInc.HH[twelvemos.hh.core.one$ServiceLocation=="SANFRANCISCO" & twelvemos.hh.core.one$FamilyType=="FII Core"])
SF.welfare.core <- mean(twelvemos.hh.core.after$welfare.inc.HH[twelvemos.hh.core.after$ServiceLocation=="SANFRANCISCO" & twelvemos.hh.core.after$FamilyType=="FII Core"]) - 
                   mean(twelvemos.hh.core.one$welfare.inc.HH[twelvemos.hh.core.one$ServiceLocation=="SANFRANCISCO" & twelvemos.hh.core.one$FamilyType=="FII Core"])
SF.kl.core <- mean(twelvemos.hh.core.after$KL.inc.HH[twelvemos.hh.core.after$ServiceLocation=="SANFRANCISCO" & twelvemos.hh.core.after$FamilyType=="FII Core"]) - 
              mean(twelvemos.hh.core.one$KL.inc.HH[twelvemos.hh.core.one$ServiceLocation=="SANFRANCISCO" & twelvemos.hh.core.one$FamilyType=="FII Core"])
SF.core.number <- length(twelvemos.hh.core.one$TotalInc.HH[twelvemos.hh.core.one$ServiceLocation=="SANFRANCISCO" & twelvemos.hh.core.one$FamilyType=="FII Core"])  

# overall
SF.total <- mean(twelvemos.hh.after$TotalInc.HH[twelvemos.hh.after$ServiceLocation=="SANFRANCISCO"]) - 
            mean(twelvemos.hh.one$TotalInc.HH[twelvemos.hh.one$ServiceLocation=="SANFRANCISCO"])
SF.welfare <- mean(twelvemos.hh.after$welfare.inc.HH[twelvemos.hh.after$ServiceLocation=="SANFRANCISCO"]) - 
              mean(twelvemos.hh.one$welfare.inc.HH[twelvemos.hh.one$ServiceLocation=="SANFRANCISCO"])
SF.kl <- mean(twelvemos.hh.after$KL.inc.HH[twelvemos.hh.after$ServiceLocation=="SANFRANCISCO"]) - 
         mean(twelvemos.hh.one$KL.inc.HH[twelvemos.hh.one$ServiceLocation=="SANFRANCISCO"])
SF.number <- length(twelvemos.hh.one$TotalInc.HH[twelvemos.hh.one$ServiceLocation=="SANFRANCISCO"])

## ALL LOCATIONS
# core
ALL.total.core <- TotalInc.twelve.core.after.mean - TotalInc.twelve.core.one.mean 
ALL.welfare.core <- Welfare.inc.twelve.core.after.mean - Welfare.inc.twelve.core.one.mean
ALL.kl.core <- KL.inc.twelve.core.after.mean - KL.inc.twelve.core.one.mean
ALL.core.number <- length(twelvemos.hh.core.one$TotalInc.HH)

# overall
ALL.total <- TotalInc.twelve.after.mean - TotalInc.twelve.one.mean 
ALL.welfare <- Welfare.inc.twelve.after.mean - Welfare.inc.twelve.one.mean 
ALL.kl <- KL.inc.twelve.after.mean - KL.inc.twelve.one.mean
ALL.number <- length(twelvemos.hh.one$TotalInc.HH)

## COMBINE INTO TABLE
totalincome <- rbind(BOS.total.core, BOS.total, DET.total.core, DET.total, FRESNO.total.core, 
                     FRESNO.total, NO.total.core, NO.total, OAK.total.core, OAK.total, SF.total.core, SF.total, ALL.total.core, ALL.total)

welfare <- rbind(BOS.welfare.core, BOS.welfare, DET.welfare.core, DET.welfare, FRESNO.welfare.core, 
                 FRESNO.welfare, NO.welfare.core, NO.welfare, OAK.welfare.core, OAK.welfare, SF.welfare.core, SF.welfare, ALL.welfare.core, ALL.welfare)

kl <- rbind(BOS.kl.core, BOS.kl, DET.kl.core, DET.kl, FRESNO.kl.core, 
            FRESNO.kl, NO.kl.core, NO.kl, OAK.kl.core, OAK.kl, SF.kl.core, SF.kl, ALL.kl.core, ALL.kl)

numobs <- rbind(BOS.core.number, BOS.number, DET.core.number, DET.number, FRESNO.core.number, 
                FRESNO.number, NO.core.number, NO.number, OAK.core.number, OAK.number, SF.core.number, SF.number, ALL.core.number, ALL.number)

first.table.income <- cbind(totalincome, welfare, kl, numobs)

# output results
write.csv(first.table.income, file = "table_of_12mochanges.csv")

## DIFFERENCES FROM BASELINE
# all families
twelvemos.hh <- ddply(twelvemos.hh, .(FamilyID.HH), transform, kl.change = KL.inc.HH - KL.inc.HH[1])
twelvemos.hh <- ddply(twelvemos.hh, .(FamilyID.HH), transform, TotalInc.change = TotalInc.HH - TotalInc.HH[1])
twelvemos.hh <- ddply(twelvemos.hh, .(FamilyID.HH), transform, welfare.change = welfare.inc.HH - welfare.inc.HH[1]) 

# core families
twelvemos.hh.core <- ddply(twelvemos.hh.core, .(FamilyID.HH), transform, kl.change = KL.inc.HH - KL.inc.HH[1])
twelvemos.hh.core <- ddply(twelvemos.hh.core, .(FamilyID.HH), transform, TotalInc.change = TotalInc.HH - TotalInc.HH[1])
twelvemos.hh.core <- ddply(twelvemos.hh.core, .(FamilyID.HH), transform, welfare.change = welfare.inc.HH - welfare.inc.HH[1])

##########################################################################
##                  SOCIAL WELFARE BENEFITS OF FII                      ##
##########################################################################

## ASSUMPTIONS & INPUTS
mpc <- 0.9

sf.sales.tax  <- 0.0875
oak.sales.tax <- 0.09
bos.sales.tax <- 0.0625
no.sales.tax  <- 0.09
det.sales.tax <- 0.06
fresno.sales.tax <- 0.08225

sf.income.tax  <- 0.06
oak.income.tax <- 0.06
bos.income.tax <- 0.052
no.income.tax  <- 0.04  
det.income.tax <- 0.0425 + 0.024
fresno.income.tax <- 0.06

## CALCULATE THE MARGINAL INCOME TAX REVENUE OF FII
# all families
if (twelvemos.hh$ServiceLocation == "BOSTON"){
  twelvemos.hh$inctaxrev <- twelvemos.hh$kl.change * bos.income.tax
  } else if (twelvemos.hh$ServiceLocation == "SANFRANCISCO") {
  twelvemos.hh$inctaxrev <- twelvemos.hh$kl.change * sf.income.tax
  } else if (twelvemos.hh$ServiceLocation == "OAKLAND") {
    twelvemos.hh$inctaxrev <- twelvemos.hh$kl.change * oak.income.tax
  } else if (twelvemos.hh$ServiceLocation == "FRESNO") {
    twelvemos.hh$inctaxrev <- twelvemos.hh$kl.change * fresno.income.tax
  } else if (twelvemos.hh$ServiceLocation == "DETROIT") {
    twelvemos.hh$inctaxrev <- twelvemos.hh$kl.change * det.income.tax
  } else {
    twelvemos.hh$inctaxrev <- twelvemos.hh$kl.change * no.income.tax
  }

# core families
if (twelvemos.hh.core$ServiceLocation == "BOSTON"){
  twelvemos.hh.core$inctaxrev <- twelvemos.hh.core$kl.change * bos.income.tax
} else if (twelvemos.hh.core$ServiceLocation == "SANFRANCISCO") {
  twelvemos.hh.core$inctaxrev <- twelvemos.hh.core$kl.change * sf.income.tax
} else if (twelvemos.hh.core$ServiceLocation == "OAKLAND") {
  twelvemos.hh.core$inctaxrev <- twelvemos.hh.core$kl.change * oak.income.tax
} else if (twelvemos.hh.core$ServiceLocation == "FRESNO") {
  twelvemos.hh.core$inctaxrev <- twelvemos.hh.core$kl.change * fresno.income.tax
} else if (twelvemos.hh.core$ServiceLocation == "DETROIT") {
  twelvemos.hh.core$inctaxrev <- twelvemos.hh.core$kl.change * det.income.tax
} else {
  twelvemos.hh.core$inctaxrev <- twelvemos.hh.core$kl.change * no.income.tax
}

## CALCULATE THE MARGINAL SALES TAX REVENUE OF FII
# all families
if (twelvemos.hh$ServiceLocation == "BOSTON"){
  twelvemos.hh$salestaxrev <- twelvemos.hh$TotalInc.change * bos.sales.tax * mpc
} else if (twelvemos.hh$ServiceLocation == "SANFRANCISCO") {
  twelvemos.hh$salestaxrev <- twelvemos.hh$TotalInc.change * sf.sales.tax * mpc
} else if (twelvemos.hh$ServiceLocation == "OAKLAND") {
  twelvemos.hh$salestaxrev <- twelvemos.hh$TotalInc.change * oak.sales.tax * mpc
} else if (twelvemos.hh$ServiceLocation == "FRESNO") {
  twelvemos.hh$salestaxrev <- twelvemos.hh$TotalInc.change * fresno.sales.tax * mpc
} else if (twelvemos.hh$ServiceLocation == "DETROIT") {
  twelvemos.hh$salestaxrev <- twelvemos.hh$TotalInc.change * det.sales.tax * mpc
} else {
  twelvemos.hh$salestaxrev <- twelvemos.hh$TotalInc.change * no.sales.tax * mpc
}

# core families
if (twelvemos.hh.core$ServiceLocation == "BOSTON"){
  twelvemos.hh.core$salestaxrev <- twelvemos.hh.core$TotalInc.change * bos.sales.tax * mpc
} else if (twelvemos.hh.core$ServiceLocation == "SANFRANCISCO") {
  twelvemos.hh.core$salestaxrev <- twelvemos.hh.core$TotalInc.change * sf.sales.tax * mpc
} else if (twelvemos.hh.core$ServiceLocation == "OAKLAND") {
  twelvemos.hh.core$salestaxrev <- twelvemos.hh.core$TotalInc.change * oak.sales.tax * mpc
} else if (twelvemos.hh.core$ServiceLocation == "FRESNO") {
  twelvemos.hh.core$salestaxrev <- twelvemos.hh.core$TotalInc.change * fresno.sales.tax * mpc
} else if (twelvemos.hh.core$ServiceLocation == "DETROIT") {
  twelvemos.hh.core$salestaxrev <- twelvemos.hh.core$TotalInc.change * det.sales.tax * mpc
} else {
  twelvemos.hh.core$salestaxrev <- twelvemos.hh.core$TotalInc.change * no.sales.tax * mpc
}

## TOTAL MONTHLY MARGINAL BENEFITS TO ENROLLMENT
twelvemos.hh$tot.pub.benefits <- twelvemos.hh$inctaxrev + twelvemos.hh$salestaxrev - twelvemos.hh$welfare.change
twelvemos.hh.core$tot.pub.benefits <- twelvemos.hh.core$inctaxrev + twelvemos.hh.core$salestaxrev - twelvemos.hh.core$welfare.change

## SUMMED OVER EACH OF 12 MONTHS
lapply(twelvemos.hh[c("salestaxrev", "inctaxrev","welfare.change", "tot.pub.benefits")], function(x) sum(x))

sum.by.family <- ddply(twelvemos.hh, .(FamilyID.HH), summarize, salestaxrev.sum = sum(salestaxrev), inctaxrev.sum = sum(inctaxrev), 
                       welfare.sum = sum(welfare.change), tot.pub.benefits.sum = sum(tot.pub.benefits))

# same analysis -  for fii core
lapply(twelvemos.hh.core[c("salestaxrev", "inctaxrev", "welfare.change", "tot.pub.benefits")], function(x) sum(x))

sum.by.core.family <- ddply(twelvemos.hh.core, .(FamilyID.HH), summarize, salestaxrev.sum = sum(salestaxrev), inctaxrev.sum = sum(inctaxrev), 
                            welfare.sum = sum(welfare.change), tot.pub.benefits.sum = sum(tot.pub.benefits))

## YEAR-OVER-YEAR DIFFERENCES (only keep the year-over-year change and then annualize it)
## N.B. USE THE AVERAGE VALUE FOR ALL CATEGORIES AFTER REPORTING MONTH 13 TO ACCOUNT FOR VERY LONG-DATED JOURNAL ENTRIES
twelvemos.hh.after <- subset(twelvemos.hh, reportingmos >= 13) 
twelvemos.hh.core.after <- subset(twelvemos.hh.core, reportingmos >= 13) 

lapply(twelvemos.hh.after[c("salestaxrev", "inctaxrev","welfare.change", "tot.pub.benefits")], function(x) sum(x)*12)
lapply(twelvemos.hh.core.after[c("salestaxrev", "inctaxrev","welfare.change", "tot.pub.benefits")], function(x) sum(x)*12)

# get the average change after 12 months or longer of FII participation across each var
social.welfare <- ddply(twelvemos.hh.after, .(FamilyID.HH), summarize, salestaxrev.avg = mean(salestaxrev), inctaxrev.avg = mean(inctaxrev), 
                             welfare.avg = mean(welfare.change), tot.pub.benefits.avg= mean(tot.pub.benefits), klgain.avg = mean(kl.change), incgain.avg=mean(TotalInc.change))
social.welfare <- as.data.frame(social.welfare)
# take mean of families' annual (or longer) changes in welfare and annualize them
lapply(social.welfare, function(x) mean(x)*12)

# core families
social.welfare.core <- ddply(twelvemos.hh.core.after, .(FamilyID.HH), summarize, salestaxrev.avg = mean(salestaxrev), inctaxrev.avg = mean(inctaxrev), 
                              welfare.avg = mean(welfare.change), tot.pub.benefits.avg= mean(tot.pub.benefits), klgain.avg = mean(kl.change), incgain.avg=mean(TotalInc.change))
social.welfare.core <- as.data.frame(social.welfare.core)

# take mean of families' annual (or longer) changes in welfare and annualize them
lapply(social.welfare.core, function(x) mean(x)*12)





#############################################################################
##                   RETURN ON INVESTMENT CALCULATION                      ##
#############################################################################

# Cost estimates for a  year are ......




############################################################################
##                          PEER EFFECTS                                  ##
############################################################################

# Effects of your group members' income changes on  yours


peerinfo <- unique(twelvemos.hh.after[,c("FamilyID.HH", "GroupCode")])
peer.data <- merge(social.welfare, peerinfo, by=c("FamilyID.HH"))

peer.data <- peer.data[order(peer.data$GroupCode), ]

write.csv(peer.data, file = "group_changes.csv")

peer.data.changes <- read.csv("group_changes_revised.csv") 

# plot a scatterplot; on x, your income change; on y, your group members' average income change
plot(peer.data.changes$klgain.avg, peer.data.changes$kl.gp)
cor(peer.data.changes$klgain.avg, peer.data.changes$kl.gp)


## STOP LOGGING WORK
sink()
