
inspection_reports <- read.csv('mine_inspections_current.csv')

attach(inspection_reports)
#gives the structure of the database
str(inspection_reports)
#dplyr has the glimpse function. Shows more information than the str function
library(tidyverse)
library(stringr)
glimpse(inspection_reports)


names(inspection_reports)
#get the number of rows

#the summary function gives a summary of each column. And a summary of the distribition of integer columns. Gives counts of strings of data
summary(county)
summary(Permittee)
summary(date) 
summary(inspection_reports)

#head function gives the first 6 rows by default
#you can add more rows with the head(x, n=25 ) n = X function 
head(inspection_reports)

#tail function gives you access to the bottom of your data 

tail(inspection_reports)

# example of how to make a new dataframe filtering for multiple conditions 

SiteConditionSubset <- filter(inspection_reports, sitecondition =='N'| sitecondition =='no answer given')
ViolationsOnly <- filter(inspection_reports, sitecondition =='N')
compliance_violations <- filter(inspection_reports, sitecondition =='N'| Compliance == 'Yes') 

compliance_violations_grouped <- inspection_reports %>%
  filter(sitecondition =="N"| Compliance == "Yes") %>%
  group_by(Permittee) %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(count = n()) %>%
  #arrange the list in descending order
  arrange(desc(count)) 

summary(SiteConditionSubset, maxsum = 10)

summary(ViolationsOnly, maxsum = 10)
summary(violations_compliance, maxsum = 10)


test_inspection_records <- inspection_reports
test_inspection_records$Compliance<-sapply(test_inspection_records$Compliance, tolower)
test_inspection_records$Compliance<-sapply(test_inspection_records$Compliance, tolower)
test_compliance_violations <- filter(test_inspection_records, sitecondition =='N'| Compliance == 'yes') 

write.csv(ViolationsOnly, file = 'ViolationsOnly.csv')
write.csv(violations_compliance, file = 'violations_compliance.csv')


summary(inspection_reports)
#-------s------------s----------

#filters out any comments cntaining novo and ignores the case. 
#Here's teh original example from -- https://blog.exploratory.io/filter-with-text-data-952df792c2ba--- filter(str_detect(str_to_lower(ORIGIN_CITY_NAME), "new york"))

violations_with_novo <- filter(inspection_reports, str_detect(str_to_lower(comments), "novo"))
  



# cut it when he starts talking about it htting the root ball. And then describe it from there. 


#he's found all kinds of stuff......but what he's really after is flushable wipes. 