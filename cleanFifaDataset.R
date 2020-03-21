### Fifa: Data Cleaning
## Import the libraries you might need:
library(readr)
library(readxl)
library(data.table)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lettercase)
library(janitor)

## Import fifa dataset:
setwd('~/Codestorage/fifa')
fifa = read.csv('fifaUncleaned.csv', header=TRUE , sep=',', na.strings=c("","NA")); # https://www.kaggle.com/karangadiya/fifa19

## Explore your data a bit:
# General structure:
dim(fifa)
str(fifa)
head(fifa)
colnames(fifa)
# View(fifa)

# General plots:
boxplot(fifa$Overall) 
hist(fifa$Overall) # 'Overall' rating is normally distributed

fifa %>%
  filter(!Position %in% c("GK", "Unknown")) %>%
  group_by(Age) %>%
  summarise(Rating = mean(Overall)) %>%
  ggplot(aes(x= Age, y= Rating, group = 1)) +
  geom_line() # 'Overall' rating increases and  plateaus/decreases at 30-35years old

## Data Cleaning
# For valuation fields, standardise units, convert to numerical data type:
colnames(fifa)
str(fifa)
fifa$ValueAmount <- str_sub(fifa$Value,-1,-1) # get units only (K/M)
fifa$Value <- substring(fifa$Value, 2) # remove currency
fifa$Value <- gsub("[^0-9.]", "",  fifa$Value) # remove units (K/M)

fifa$Wage <- substring(fifa$Wage, 2) # remove currency
fifa$Wage <- gsub("[^0-9.]", "",  fifa$Wage) # remove units (K/M)

fifa$ReleaseClauseAmount <- str_sub(fifa$Release.Clause,-1,-1) # get units only (K/M)
fifa$Release.Clause <- substring(fifa$Release.Clause, 2) # remove currency
fifa$Release.Clause <- gsub("[^0-9.]", "",  fifa$Release.Clause) # remove units (K/M)

fifa$Wage <- as.numeric((fifa$Wage))
fifa$Value <- as.numeric(fifa$Value)
fifa$Release.Clause <- as.numeric(fifa$Release.Clause)

# Standardise wages and value units
fifa$ValueAmount <- revalue(fifa$ValueAmount, c("M"= 1000000))
fifa$ValueAmount <- revalue(fifa$ValueAmount, c("K"= 1000))
fifa$ValueAmount <- as.numeric((fifa$ValueAmount))
fifa$Value <- fifa$Value * fifa$ValueAmount

fifa$ReleaseClauseAmount <- revalue(fifa$ReleaseClauseAmount, c("M"= 1000000))
fifa$ReleaseClauseAmount <- revalue(fifa$ReleaseClauseAmount, c("K"= 1000))
fifa$ReleaseClauseAmount <- as.numeric((fifa$ReleaseClauseAmount))
fifa$Release.Clause <- fifa$Release.Clause * fifa$ReleaseClauseAmount

# Normalise
fifa$Release.Clause = fifa$Release.Clause/1000000
fifa$Value = fifa$Value/1000000

fifa = dplyr::select(fifa, -c(ValueAmount, ReleaseClauseAmount))

head(fifa$Value)
head(fifa$Wage)
head(fifa$Release.Clause)

# Check data types
head(fifa$Height, 1)
fifa$Height
fifa$Height = as.numeric(gsub("\'", ".", fifa$Height))
fifa$Weight
fifa$Weight = as.numeric(gsub("lbs", "", fifa$Weight))

fifa$Joined
fifa$Joined = as.Date(fifa$Joined, "%b %d, %Y")
str(fifa)

# Handle NAs
sum(is.na(fifa)) #~76984 NAs

# NAs: rm cols where ALL values are NA:
fifa = fifa[, colSums(is.na(fifa)) != nrow(fifa)]
colnames(fifa) # no change

# NAs: rm cols where more than 40% values are NA:
fifa = fifa[, colMeans(is.na(fifa)) <= .4]
colnames(fifa) # removed ~5 cols

# NAs: rm cols where single value for all rows:
fifa = Filter(function(x)(length(unique(x))>1), fifa) # no change

# NAs: Replace NAs with mean of col --> this prob not best way to do it but leave as is for now
for(i in 1:ncol(fifa)) {
  fifa[is.na(fifa[,i]), i] = mean(fifa[,i], na.rm = TRUE)
}

# For position performnces reported with buffer, remove buffer (everything from '+'):
positionsWithBuffer = c("LS", "ST", "RS", "LW", "LF", "CF", "RF", "RW", "LAM", "CAM", "RAM", "LM", "LCM", "CM", "RCM",
                        "RM", "LWB", "LDM", "CDM", "RDM", "RWB", "LB", "LCB", "CB", "RCB", "RB")
for(p in positionsWithBuffer) {
  fifa[,p] = as.numeric(gsub("\\+.*", "", fifa[,p]))
} 

fifa$LS
fifa$RB

# Standardise col names to camel case:
colnames(fifa)
fifa = janitor::clean_names(fifa, 'lower_camel')
colnames(fifa)

# Separate player name into separate cols- first vs. last:
fifa = separate(fifa, col='name', into=c('firstName', 'lastName'), sep=' ', remove = FALSE, convert = FALSE)
str(fifa)

# Separate work rate into separate cols- offensive vs. defensive:
fifa = separate(fifa, col='workRate', into=c('offensiveWorkRate', 'defensiveWorkRate'), sep='\\/ ', remove = TRUE, convert = FALSE)
str(fifa)

# Parse year from 'contractValidUntil' col, report as string:
fifa = mutate(fifa, contractValidUntil = ifelse(!is.na(as.Date(contractValidUntil, '%Y')), format(as.Date(contractValidUntil, '%Y'), format = '%Y'),
                                                        format(as.Date(contractValidUntil, '%b %d, %Y'), format = '%Y'))) # zamn this is inefficient
class(fifa$contractValidUntil) # character

# Rm stray entries in 'bodyType' col:
class(fifa$bodyType)
unique(fifa$bodyType)
acceptedBodyTypeLevels = c('Lean', 'Normal', 'Stocky')

fifa = mutate(fifa, bodyType = ifelse(!(bodyType %in% acceptedBodyTypeLevels), NA, as.character(bodyType))) # rm entries like "cristiano ronaldo" haha
class(fifa$bodyType)
str(fifa)

# Write cleaned fifa file to csv:
currDate = currDate = Sys.Date() # date in format "YYYY-MM-DD"
write.csv(fifa, paste0("fifaCleaned.csv"), row.names = TRUE)


