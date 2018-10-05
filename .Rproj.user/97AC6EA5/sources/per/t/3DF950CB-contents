library(tidyverse)

#so first, I tried to pull in all of the inspections I got from my first FOIA request. It didn't really go well, but I am going to be able to use the permit names from it
#inspection_reports <- read.csv('mine_inspections_current.csv')
#now i group and count by the permit and site name so I can join below
#mine_names <- inspection_reports %>%
#  group_by(siteName, permit) %>%
#  summarise(count = n())
#theres a few doibles and one to check. I'm going to write this to a CSV and quickly edit the documemnt. 
#write_csv(mine_names, "minenames.csv")
#now I've corrected the few duplicates. And read it back in. We should be good. 
mine_names <- read_csv("minenames.csv")

#here are the two databases I've built. the novo_full is all of the novos and its fines.
Novo_full <- read_csv("full_novos.csv")
#the novo_log is the dept. of mines-pfrivided 
Novo_log <- read_csv("Novo_log.csv")

# A quick function I found online that trims out pesky white spaces, which has made joining a little tricky. 
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
Novo_log$Permit_no <- trim(Novo_log$Permit_no)
Novo_full$NOVO_num <- trim(Novo_full$NOVO_num)
Novo_log$NOVO_num <- trim(Novo_log$NOVO_num)

#now I'm joining the sheets I've devised. The mine log, which is each novo and the mine number. And the Novo_full whicch is a log of all violations.
novo_all <- left_join(Novo_full, Novo_log, by = c("NOVO_num" = "NOVO_num"))

#Now we can have a CSV copy, if needed.
#write_csv(novo_all, "novo_all.csv")

#now I want to join another sheet that I've created. above mine_names. It has the specific mine names...I didn't take that information down when I was making my database. 

novo_w_names <- left_join(novo_all, mine_names, by = c("Permit_no.x" ="permit" ))


#one of the probelms I've encountered is many fields were not filled in. So to get a better picture on dates, I've also created a separate date field for the date it was issued. 
#Now I want to see when these fines were paid. First I convert to the proper date
novo_w_names$received_date <- as.Date(novo_w_names$received_date ,"%m/%d/%y")
novo_w_names$Novo_date <- as.Date(novo_w_names$Novo_date, "%m/%d/%y")
#Then I go ahead and create some columns to look at the months and years
novo_w_names <- mutate(novo_w_names, novo_year = format(novo_w_names$Novo_date, "%Y"), received_year = format(novo_w_names$received_date, "%Y"))

#now I can see how much was paid in each year
finespaid <- novo_w_names %>%
  group_by(received_year)%>%
  summarise(history_of_violations = sum(history_viol, na.rm = T), total_fine = sum(history_viol, seriousness_viol, negligence_viol, effect_viol_reclamation, assessment_failure, na.rm = T), good_faith = sum(good_faith, na.rm = T), total_assess = sum(total_assess, na.rm = T),  total_paid = sum(total_paid, na.rm = T))

fines_issued <- novo_w_names %>%
  group_by(violation_issued_fy) %>%
  summarise( history_of_violations = sum(history_viol, na.rm = T), total_fine = sum(history_viol, seriousness_viol, negligence_viol, effect_viol_reclamation, assessment_failure, na.rm = T), good_faith = sum(good_faith, na.rm = T), total_assess = sum(total_assess, na.rm = T),  total_paid = sum(total_paid, na.rm = T))

#so What I didn't realize when I was doing the above calculations, it's in fiscal year, not actual year. So I wasn't getting acurate num,bers. Now I've gone back and used a regex to say match one or more (+) of the characters that aren't 'of' (the negating [^ ]), followed by end of string ($). her'es the stack ex I got it from https://stackoverflow.com/questions/41986730/create-new-column-with-dplyr-mutate-and-substring-of-existing-column
novo_w_names <- mutate(novo_w_names, fy = str_extract(quarter_year_resolved, "[^of]+$"))

#now I can see which fiscal year fines were issued. I decided to remove table rock mine and underwood and PAugh and Althouse because they are all LAOC mines...and their fines are to be handled differently. Because the numbers are so high and the fines were never paid. I can note it out if I want a total.
novo_fy <- novo_w_names %>%
  #filter(siteName != "Table Rock Mine" & siteName != "Underwood Mine" & siteName != "Althouse Hill Mine"  & siteName != "Paugh Tract/ Potomac Camp") %>%
  group_by( siteName, permittee) %>%
  summarise(history_of_violations = sum(history_viol, na.rm = T), total_fine = sum(history_viol, seriousness_viol, negligence_viol, effect_viol_reclamation, assessment_failure, na.rm = T), good_faith = sum(good_faith, na.rm = T), total_assess = sum(total_assess, na.rm = T),  total_paid = sum(total_paid, na.rm = T))


#so first I wanted to look at the violation number and the date and see how much each company owed per novo. Again, I'm trying to look through
novo_and_fines <- novo_w_names  %>% 
  filter(siteName != "Table Rock Mine" & siteName != "Underwood Mine" & siteName != "Althouse Hill Mine"  & siteName != "Paugh Tract/ Potomac Camp") %>%
  group_by(NOVO_num, violation_issued_fy, permittee, siteName ) %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(history_of_violations = sum(history_viol), total_fine = sum(history_viol, seriousness_viol, negligence_viol, effect_viol_reclamation, assessment_failure), good_faith = sum(good_faith), total_assess = sum(total_assess),  total_paid = sum(total_paid))  %>%
  arrange(desc( siteName)) 

#now let's see how many times there was no good faith money off and when there was.
#To do this Im making a small ifelse row to show when there's no good faith and when there is.
novo_w_names <- mutate(novo_w_names, goodfaith_yesno = if_else(novo_w_names$good_faith <= -.1 , "good_faith", "no_good_faith"))

# I removed the Table Rock Mine site, which was eventually forfeited by the company. 
good_faith_yesno <- novo_w_names %>%
  filter(siteName != "Table Rock Mine") %>%
  group_by(goodfaith_yesno) %>%
  summarise(count = n())


#NOw I want to look at each company and how many fines they racked up... 
company_and_fines <- novo_w_names  %>% 
  group_by(permittee, siteName) %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(history_of_violations = sum(history_viol), total_fine = sum(history_viol, seriousness_viol, negligence_viol, effect_viol_reclamation, assessment_failure), good_faith = sum(good_faith), total_assess = sum(total_assess),  total_paid = sum(total_paid))  %>%
  arrange(desc(total_fine)) 


#now we're just looking at site names, or the mines themselves
mine_and_fines <- novo_w_names  %>% 
  group_by(siteName) %>%
  #creates a count with the row name being count. The n value is the number of times it occurs
  summarise(history_of_violations = sum(history_viol), total_fine = sum(history_viol, seriousness_viol, negligence_viol, effect_viol_reclamation, assessment_failure), good_faith = sum(good_faith), total_assess = sum(total_assess),  total_paid = sum(total_paid))  %>%
  arrange(desc(total_fine)) 

mines_forfeit <- novo_w_names %>%
  group_by(siteName, penalty_status) %>%
  summarise(count =n())


