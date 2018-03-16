# Parsing inspection reports
# QDR

# Read all lines of the text file.
# Each element of the vector raw_text is a character string with one line of the file.
# Change this line to the correct file path and file name.
raw_text <- readLines('/Users/chriscioffi/Desktop/R/inspections.txt')

library(purrr) # Has a lot of useful functions for mapping the same function to each element of a list
library(Hmisc) # I used this library just for a function to capitalize words

# Get rid of leading and trailing spaces on each line so it is easier to read stuff.
# This takes a couple minutes to run.
raw_text <- map_chr(raw_text, trimws, which = 'both')

# It looks like each separate inspection report begins with a line that contains "MARYLAND DEPARTMENT OF THE ENVIRONMENT"
# So we can split the whole thing up using that as a key.
# Then make a list object with each element of the list being one inspection report as a vector of lines.
split_key <- 'MARYLAND DEPARTMENT OF THE ENVIRONMENT'

first_lines <- which(raw_text == split_key) # This is the location of the first line of each inspection report
last_lines <- c(first_lines[-1] - 1, length(raw_text)) # This is the location of the last line of each inspection report

# Pull out each first line through last line as a separate element of a list
inspections_list <- map2(first_lines, last_lines, ~ raw_text[.x:.y])

# --------------------------------------------------------------------------------------

# Now we can do stuff like get the county name for each of the inspection reports by checking which line has the word County at the beginning and then pulling out the word that comes after it.
# It doesn't matter if the county ID is in a different line on each report

county_txt <- map_chr(inspections_list, function(x) grep('^County:', x, value = TRUE)) # Pull out the right line
county <- map_chr(strsplit(county_txt, ' '), function(x) x[length(x)]) # Get the last word from each of those lines
county <- capitalize(tolower(county)) # Some are all caps, some lowercase, so convert each to a capitalized word

table(county)
# There are some abbreviations in here so get rid of them
county[county=='Al'] <- 'Allegany'
county[county=='Ga'] <- 'Garrett'


# --------------------------------------------------------------------------------------

#gets report number 

Report_num <- map_chr(inspections_list, function(x) grep('^Report Number:', x, value = TRUE)) # Pull out the right line
Report_num_full <- map_chr(strsplit(Report_num, ' '), function(x) x[length(x)]) # Get the last word from each of those lines

# --------------------------------------------------------------------------------------

#gets permit number 

permit_num <- map_chr(inspections_list, function(x) grep('^Permit Number:', x, value = TRUE)) # Pull out the right line
permit_num_full <- map_chr(strsplit(permit_num, ' '), function(x) x[length(x)]) # Get the last word from each of those lines

# --------------------------------------------------------------------------------------

#gets report date 

date_num <- map_chr(inspections_list, function(x) grep('^Inspection Date:', x, value = TRUE)) # Pull out the right line
date_num_full <- map_chr(strsplit(date_num, ' '), function(x) x[length(x)]) # Get the last word from each of those lines

# --------------------------------------------------------------------------------------

# Code to pull out site conditions

# *** this is just to look
#site_cond <- map_chr(inspections_list, function(x) grep('^Site Condition', x, value = TRUE)) # Pull out the right line

#table(site_cond) # We can see from this that some have no answer, most say either C or N, and there is one weird one
# ***

# Define function to do everything at once: 
# pull out line with site condition, then get rid of the question and only keep the answer
# Uses gsub function to get rid of question (with fixed = TRUE to say match as is)
# if there isn't an answer, it will return a blank line

# this function can be mapped directly on inspections_list (in other words nothing above this is needed, it was just to look at)

get_site_cond <- function(x) {
  line <- grep('^Site Condition', x, value = TRUE) # Get line
  answer <- gsub(pattern = 'Site Condition (Compliant [C]; NOVO Issued [N]):', replacement = '', line, fixed = TRUE) # Replace the question with a blank
  answer <- trimws(answer) # Get rid of the extra spaces
  answer[answer == ''] <- 'no answer given' # Make it more obvious when no answer was given
  return(answer)
}

site_cond <- map_chr(inspections_list, get_site_cond) # Apply it to the 
table(site_cond)

# --------------------------------------------------------------------------------------

#site condition function. Modified from QDR

get_site_cond <- function(x) {
  line <- grep('^Site Condition', x, value = TRUE) # Get line
  answer <- gsub(pattern = 'Site Condition (Compliant [C]; NOVO Issued [N]):', replacement = '', line, fixed = TRUE) # Replace the question with a blank
  answer <- trimws(answer) # Get rid of the extra spaces
  answer[answer == ''] <- 'no answer given' # Make it more obvious when no answer was given
  return(answer)
}

site_cond <- map_chr(inspections_list, get_site_cond) # Apply it to the 
table(site_cond)

# --------------------------------------------------------------------------------------
#site status function 

get_site_status <- function(x) {
  line <- grep('^Site Status', x, value = TRUE) # Get line
  answer <- gsub(pattern = 'Site Status (Active [A]; Inactive [I]; Completed [C]; Not Started [NS]; Revoked [R]):', replacement = '', line, fixed = TRUE) # Replace the question with a blank
  answer <- trimws(answer) # Get rid of the extra spaces
  answer[answer == ''] <- 'no answer given' # Make it more obvious when no answer was given
  return(answer)
}

site_status <- map_chr(inspections_list, get_site_status) # Apply it to the 
table(site_status)

# --------------------------------------------------------------------------------------
#find site name function 

get_site_name <- function(x) {
  line <- grep('^Site Name:', x, value = TRUE) # Get line
  answer <- gsub(pattern = 'Site Name:', replacement = '', line, fixed = TRUE) # Replace the question with a blank
  answer <- trimws(answer) # Get rid of the extra spaces
  answer[answer == ''] <- 'no answer given' # Make it more obvious when no answer was given
  return(answer)
}

site_name <- map_chr(inspections_list, get_site_name) # Apply it to the 
table(site_name)

# --------------------------------------------------------------------------------------
#find permittee field 

get_permittee <- function(x) {
  line <- grep('^Permittee:', x, value = TRUE) # Get line
  if (length(line) == 0) return('Not filled out')
  answer <- gsub(pattern = 'Permittee:', replacement = '', line, fixed = TRUE) # Replace the question with a blank
  answer <- trimws(answer) # Get rid of the extra spaces
  answer[answer == ''] <- 'no answer given' # Make it more obvious when no answer was given
  return(answer)
}

site_permittee<- map_chr(inspections_list, get_permittee) # Apply it to the 
table(site_permittee)

# --------------------------------------------------------------------------------------
#find evidence collected field 

get_evidence <- function(x) {
  line <- grep('^Evidence Collected', x, value = TRUE) # Get line
  if (length(line) == 0) return('Not filled out')
  answer <- gsub(pattern = 'Evidence Collected (Visual Observation [VO]; Photos [P]; Attachments [A]):', replacement = '', line, fixed = TRUE) # Replace the question with a blank
  answer <- trimws(answer) # Get rid of the extra spaces
  answer[answer == ''] <- 'no answer given' # Make it more obvious when no answer was given
  return(answer)
}

evidence_field <- map_chr(inspections_list, get_evidence) # Apply it to the 
table(evidence_field)

evidence_field[evidence_field=='VO             A'] <- 'A'
evidence_field[evidence_field=='VO      A'] <- 'A'
evidence_field[evidence_field=='VO      P'] <- 'P'
evidence_field[evidence_field=='VO      P      A'] <- 'P'
table(evidence_field)
# --------------------------------------------------------------------------------------
#find did require compliance assistance 

get_compliance <- function(x) {
  line <- grep('^Did this inspection involve', x, value = TRUE) # Get line
  if (length(line) == 0) return('no answer given')
  answer <- gsub(pattern = 'Did this inspection involve Compliance Assistance?', replacement = '', line, fixed = TRUE) # Replace the question with a blank
  answer <- trimws(answer) # Get rid of the extra spaces
  answer[answer == ''] <- 'no answer given' # Make it more obvious when no answer was given
  return(answer)
}

compliance_yesno <- map_chr(inspections_list, get_compliance) # Apply it to the 
table(compliance_yesno)

# There are some abbreviations in here so get rid of them
compliance_yesno[compliance_yesno==':'] <- 'no answer given'
compliance_yesno[compliance_yesno== ': N/A'] <- 'N/A'
compliance_yesno[compliance_yesno==': YES'] <- 'Yes'
compliance_yesno[compliance_yesno=='Did this inspection involve Compliance Assistance'] <- 'no answer given'
compliance_yesno[compliance_yesno=='Did this inspection involve Compliance Assistance ?'] <- 'no answer given'
compliance_yesno[compliance_yesno=='Did this inspection involve Compliance Assistance ?   N/A'] <- 'N/A'
compliance_yesno[compliance_yesno==' N/A'] <- 'N/A'
compliance_yesno[compliance_yesno=='Did this inspection involve Compliance Assistance: N/A'] <- 'N/A'
compliance_yesno[compliance_yesno=='Did this inspection involve Compliance Assistance: NO'] <- 'N/A'
compliance_yesno[compliance_yesno=='Did this inspection involve Compliance Assistance: YES'] <- 'Yes'
compliance_yesno[compliance_yesno=='Did this inspection involve Compliance Assistance ?   Yes'] <- 'Yes'
compliance_yesno[compliance_yesno=='Did this inspection involve Compliance Assistance ?  N/A'] <- 'N/A'
compliance_yesno[compliance_yesno=='Did this inspection involve Compliance Assistance ?  YES'] <- 'Yes'
compliance_yesno[compliance_yesno=='Did this inspection involve Compliance Assistance:'] <- 'no answer given'

table(compliance_yesno)
# --------------------------------------------------------------------------------------

# Now we can do stuff like get the county name for each of the inspection reports by checking which line has the word County at the beginning and then pulling out the word that comes after it.
# It doesn't matter if the county ID is in a different line on each report

county_txt <- map_chr(inspections_list, function(x) grep('^County:', x, value = TRUE)) # Pull out the right line
county <- map_chr(strsplit(county_txt, ' '), function(x) x[length(x)]) # Get the last word from each of those lines
county <- capitalize(tolower(county)) # Some are all caps, some lowercase, so convert each to a capitalized word

# -------------------------------------------------------------------------------------

# Another example would be if you want the mine inspector's name.
inspectorname_txt <- map_chr(inspections_list, function(x) grep('^Mine Inspector:', x, value = TRUE)[1]) # Pull out the right line (had to get only the first one since that text appears on later lines as well)

# This one is tougher since it is more inconsistent. Sometimes the first and last names are there and sometimes there is more than one mine inspector listed.
# Everything that appears after the colon is the mine inspector's name so split the string by that.
inspectorname <- map_chr(strsplit(inspectorname_txt, ':'), function(x) x[length(x)])
inspectorname <- trimws(inspectorname, which = 'both') # Get rid of spaces at beginning and end
table(inspectorname) # You can see from this that it is still pretty inconsistent. We can write some code to clean it up.

# First convert everything to uppercase so it is consistent
inspectorname <- toupper(inspectorname)
unique_names <- c('ARRINGTON','MURRAY','MURPHY','MONGOLD','SCHARTIGER') # Names of the inspectors

# Get which if any of the names are in each of the inspection rows and make a table of them
inspector_check <- map(inspectorname, function(x) map_lgl(unique_names, function(y) grepl(y, x)))
inspectorname_final <- map_chr(inspector_check, function(x) paste(unique_names[x], collapse = ','))

# -------------------------------------------------------------------------------------

# Get answers to the yes or no questions (30 per report)

# Define a function that will be mapped to each element of the list
get_yes_or_no <- function(x) {
  # Loop through 30 values of i and find each line beginning with that number.
  answers <- character(30)
  for (i in 1:30) {
    n <- ifelse(i < 10, paste0('0', i), paste0(i)) # add a zero before it if it's a one digit number
    question_line <- grep(paste0('^', n, '.'), x, value = TRUE) # get the line with that question
    question_line <- unlist(strsplit(question_line, ' ')) # split it into words
    answers[i] <- question_line[length(question_line)] # Get the last word        
  }
  return(answers)
}

all_answers <- map(inspections_list, get_yes_or_no) # extract all answers

# check whether it worked by seeing if 30 answers came back from each one and if all are characters
all(map_int(all_answers,length) == 30)
all(map_chr(all_answers,class) == 'character')

all_answers <- do.call(rbind, all_answers) # combine into a matrix
sort(table(all_answers)) # almost all say No, N/A, or Yes, but a few don't.

all_answers <- as.data.frame(all_answers)
names(all_answers) <- paste('Q', 1:ncol(all_answers), sep = '_')

# -------------------------------------------------------------------------------------

# Get the comments

# Define function
# identify the line number that says comments, and the one after it that says mine inspector, and get all lines between.
get_comments <- function(x) {
  comment_linenum <- grep('FINDINGS AND COMMENTS', x)
  inspector_linenum <- grep('Mine Inspector:', x)
  # only get the inspector line number that's after the comment line number
  inspector_linenum <- inspector_linenum[inspector_linenum > comment_linenum]
  # return all lines between, pasted into one string
  paste(x[(comment_linenum + 1):(inspector_linenum - 1)], collapse = ' ')
}

all_comments <- map_chr(inspections_list, get_comments)

# -------------------------------------------------------------------------------------

# Now if we want we can combine these things into a data frame and do stuff like make plots.

inspection_data <- data.frame(county = county,
                              inspector = inspectorname_final,
                              siteName = site_name,
                              Permittee = site_permittee,
                              reportNumber = Report_num_full, 
                              permit = permit_num_full,
                              date = date_num_full,
                              sitecondition = site_cond,
                              sitestatus = site_status,
                              EvidenceCollected = evidence_field,
                              Compliance = compliance_yesno, 
                              all_answers,
                              comments = all_comments
                              )

write.csv(inspection_data, '/Users/chriscioffi/Desktop/R/mine_inspections.csv')
#---------------------- useful graphing items I've not been using as of yet. 


library(dplyr)
library(cowplot)

# Make a table by county and name
inspection_table <- inspection_data %>%
  group_by(county, inspector) %>%
  summarize(n_inspections = n())

# Make a bar graph by county x name combo
ggplot(inspection_data, aes(x = inspector, fill = county, group = county)) +
  geom_bar(stat = 'count') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

summary(all_comments)


