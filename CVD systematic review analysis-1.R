library(geofacet)
library(data.table)
library(tidyverse)
library(ggplot2)
library(patchwork)  ### for combining plots
# setting working directory
setwd("U:/ManWin/My Documents/James Oguta/My PhD Folder-2023-2024/My Systematic Review/Synthesis/Tables/Final/Analyses")
# reading datasets
cvd_data<- read.csv("Final Dataset.csv")

cvd_data<- as.data.table(cvd_data)


#changing variable names to lower case
names(cvd_data)<- tolower(names(cvd_data))

#removing decimals from column names
names(cvd_data) <- gsub("\\.", "", names(cvd_data))

# renaming variables
 cvd_data <- cvd_data %>% 
   rename(
     evaluation = evaluationtype,
     prevention = preventiontype,
     intervention = interventiontype,
     cvd_outcomes = cvdoutcomesassesed,
     model = modeltype,
     risk = cvdriskequation,
     d_rate = discountrate,
     horizon = timehorizon,
     start_age = minimumageyears,
     equity = equitydimensions,
     sensitivity = sensitivityanalysis,
     evpi= evpiassessed,
     guideline = reportingguidelineused,
     ceacf = ceacsceafspresented, 
     adaptation= modeladaptation,
     funding = typeoffundingsources,
     conflict= conflictofinterest
   )

#checking types of variables
 str(cvd_data)
 
#selecting a subset of variables
 selected <- c("authors", "country", "year", "evaluation", "prevention", "intervention", "cvd_outcomes",
               "multimorbidity", "model", "risk", "perspective", "d_rate", "start_age", "horizon", "cyclelength",
               "outcomemeasure", "equity", "sensitivity", "evpi", "ceacf", "validation", "callibration",
               "adaptation", "funding", "conflict")
subset_cvd <- cvd_data %>% 
  select(all_of(selected))

to_factor <- c("country", "evaluation", "prevention", "intervention", "cvd_outcomes",
               "model", "risk", "perspective", "d_rate", "start_age", "horizon", "cyclelength",
              "software", "outcomemeasure", "equity", "sensitivity", "evpi", "ceacf", "validation",
              "callibration","adaptation", "funding", "conflict")
str(subset_cvd)

#defining factor variables
cvd <- cvd_data %>% 
  mutate_at(to_factor, as.factor)

# Creating a subset of country dataset###
country <- cvd %>% select(id, authors, country)
summary(country$country)

#creating list of countries
country_list <- c("Cameroon", "South Africa", "Nigeria", "Ghana",
                  "Kenya", "Uganda", "Tanzania", "Ethiopia", "Multicountry")
# Initialize an empty list to store binary variables
country_flags <- list()

# Loop through each country
for (count in country_list) {
  # Check if the country is present in the observation
  country_flag <- as.integer(grepl(count, country$country))
  
  # Add the binary variable to the list
  country_flags[[count]] <- country_flag
}

# Convert the list to a data frame
country_df <- as.data.frame(country_flags)

# Bind the binary variables to the main dataset
country_dataset <- cbind(country, country_df)

# Save the dataset as a CSV file
write.csv(country_dataset, "country_dataset.csv", row.names = FALSE)

dataset_wide <- country_dataset[, 4:12]


# Reshape data into a long format
dataset_long <- reshape2::melt(as.matrix(dataset_wide), id.vars = "authors", variable.name = "studies", value.name = "number")
dataset_long <- dataset_long[dataset_long$number > 0, ]  # Keep only rows where disease is present

authors <- cvd_data$authors
writeLines(authors,"data.txt")
colnames(dataset_long)[colnames(dataset_long) == "Var1"] ="study"
colnames(dataset_long)[colnames(dataset_long) == "Var2"] ="country"



dataset_long$study <- factor(dataset_long$study, levels = 1:27, labels = c(
  "Aminde et al. (2019)", "Aminde et al. (2020)","Basu et al. (2019)","Basu et al. (2021)",
  "Bertram et al. (2021)","Davari et al. (2022)","Ekwunife et al. (2013)","Gad et al. (2020)",
  "Gaziano et al. (2005)","Gaziano et al. (2006)","Gaziano et al. (2014)","Gaziano et al. (2015)",
  "Kasaie et al. (2020)","Lim et al. (2007)","Lin et al. (2019)","Manyema et al. (2016)",
  "Ngalesoni et al. (2016a)","Ngalesoni et al. (2016b)","Ngalesoni et al. (2017)","Ortegon et al. (2012)",
  "Pozo-Martin et al. (2021)","Robberstad et al. (2007)","Rosendaal et al. (2016)",
  "Sando et al. (2020)","Subramanian et al. (2019)","Tolla et al. (2016)","Watkins et al. (2015)"
))

dataset_long$country <- ifelse(dataset_long$country=="South.Africa","South Africa",
                               ifelse(dataset_long$country=="Ethiopia","Ethiopia", 
                                      ifelse(dataset_long$country=="Nigeria", "Nigeria",
                                             ifelse(dataset_long$country=="Kenya", "Kenya",
                                                    ifelse(dataset_long$country=="Uganda", "Uganda",
                                                           ifelse(dataset_long$country=="Tanzania", "Tanzania",
                                                                  ifelse(dataset_long$country=="Cameroon", "Cameroon",
                                                                         ifelse(dataset_long$country=="Multicountry", "Multicountry",
                                                                                ifelse(dataset_long$country=="Ghana","Ghana",
                                                                                       dataset_long$country)))))))))
                                                                                

###################################################################################
##Categorizing year variable
subset_cvd$yearcat <- ifelse(subset_cvd$year<2010, "2005-2009",
                              ifelse(subset_cvd$year>=2010 & subset_cvd$year<2015, "2010-2015",
                                     ifelse(subset_cvd$year>=2015 & subset_cvd$year<2020, "2015-2019",
                                            ifelse(subset_cvd$year>=2020, "2020-2022", NA))))



subset_cvd$prevention <- ifelse(subset_cvd$intervention=="Diet","Primordial",
                                ifelse(subset_cvd$intervention=="Multiple interventions", "Multiple",
                                       subset_cvd$prevention))

subset_cvd$prevention <- ifelse(subset_cvd$prevention=="Both","Primary & Secondary",
                                subset_cvd$prevention)

subset_cvd$model <- ifelse(subset_cvd$model=="Proportional multi-state life table Markov model","PMLTMM",
                           ifelse(subset_cvd$model=="Mathematical functions", 
                                  "Mathematical Equations",
                                  ifelse(subset_cvd$model=="Cohort Markov model", "Markov", subset_cvd$model))) 

subset_cvd$perspective <- ifelse(subset_cvd$perspective=="Public provider","Healthcare Provider",
                                subset_cvd$perspective)

# Search for "Framingham" in the risk variable
subset_cvd$framingham <- ifelse(grepl("Framingham", subset_cvd$risk), 1, 0)



# Search for "who" or "Absolute" in the risk variable
subset_cvd$who <- ifelse(grepl("WHO|Absolute", subset_cvd$risk, ignore.case = TRUE), 1, 0)


# Search for "Globorisk" in the risk variable
subset_cvd$globorisk <- ifelse(grepl("Globorisk", subset_cvd$risk), 1, 0)

# Search for "Globorisk" in the risk variable
subset_cvd$cox <- ifelse(grepl("cox", subset_cvd$risk), 1, 0)

sum (subset_cvd$framingham)
sum (subset_cvd$who)
sum (subset_cvd$globorisk)


risk_columns <- c("framingham", "who", "globorisk", "cox" )
risk_wide <- subset_cvd[, ..risk_columns,drop=FALSE] # the .. syntax within the square brackets to select columns dynamically

# Reshape data into a long format
risk_long <- reshape2::melt(as.matrix(risk_wide), id.vars = "authors", variable.name = "risk", value.name = "number")
risk_long <- risk_long[risk_long$number > 0, ]  # Keep only rows where disease is present

authors <- cvd_data$authors
colnames(risk_long)[colnames(risk_long) == "Var1"] ="study"
colnames(risk_long)[colnames(risk_long) == "Var2"] ="risk"


####Cleaning the horizon column

subset_cvd$horizon <- ifelse(grepl("Lifetime", subset_cvd$horizon), "Lifetime",
                             ifelse(grepl("10 years", subset_cvd$horizon), "10 years", 
                                    subset_cvd$horizon))

###Model Outcomes###
# Search for "Ischemic heart disease" in the risk variable
subset_cvd$ihd <- ifelse(grepl("Ischemic heart |ischemic heart|infarction|Coronary|coronary|congestive|Congestive", subset_cvd$cvd_outcomes, ignore.case = TRUE), 1, 0)

# Search for "Coronary heart disease" in the risk variable
subset_cvd$stroke <- ifelse(grepl("Stroke |stroke", subset_cvd$cvd_outcomes, ignore.case = TRUE), 1, 0)

# Search for "Coronary heart disease" in the risk variable
subset_cvd$cva <- ifelse(grepl("Cerebrovascular |derebrovascular|CVA", subset_cvd$cvd_outcomes, ignore.case = TRUE), 1, 0)

# Search for "Coronary heart disease" in the risk variable
subset_cvd$hhd <- ifelse(grepl("hypertensive |HHD", subset_cvd$cvd_outcomes, ignore.case = TRUE), 1, 0)

# Search for "Coronary heart disease" in the risk variable
subset_cvd$angina <- ifelse(grepl("Angina |angina", subset_cvd$cvd_outcomes, ignore.case = TRUE), 1, 0)

# Search for "Cardiac Arrest" in the risk variable
subset_cvd$ca <- ifelse(grepl("Cardiac Arrest| arrest", subset_cvd$cvd_outcomes, ignore.case = TRUE), 1, 0)

# Search for "Atherosclerosis" in the risk variable
subset_cvd$athero <- ifelse(grepl("athero", subset_cvd$cvd_outcomes, ignore.case = TRUE), 1, 0)


outcome_columns <- c("ihd", "stroke", "cva", "angina","ca","athero" )
outcome_wide <- subset_cvd[, ..risk_columns,drop=FALSE] # the .. syntax within the square brackets to select columns dynamically

# Reshape data into a long format
outcome_long <- reshape2::melt(as.matrix(outcome_wide), id.vars = "authors", variable.name = "outcome", value.name = "number")
outcome_long <- outcome_long[outcome_long$number > 0, ]  # Keep only rows where disease is present

authors <- cvd_data$authors
colnames(outcome_long)[colnames(outcome_long) == "Var1"] ="study"
colnames(outcome_long)[colnames(outcome_long) == "Var2"] ="outcome"


### Characteristics of Included Studies###############
###General characteristics#########

# Graphing year of publication

p1 <- ggplot(subset_cvd, aes(yearcat)) + 
  geom_bar() +
  labs(x = "Publication Year", y = "Number of studies") +
  scale_y_continuous(breaks = 0:13)

p1

# Plotting combinations of studies in each country
p2 <- ggplot(dataset_long, aes(x = country, fill = as.factor(study))) +
  geom_bar(stat = "count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +  # Center the title
  scale_y_continuous(breaks = seq(0, 27, 1)) +
  labs(x = "Country", y = "Count of Studies") +
  scale_fill_discrete(name = "Study") +
  ggtitle("Number of Studies per Country")
p2

# To create a bar graph using ggplot2- Alternative 2
p3 <- ggplot(dataset_long, aes(x = country, y = number)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "No of studies", title = "Number of studies by country")

p3

# Graphing year of publication by prevention type-Pete to advise on this.

p4 <- ggplot(subset_cvd) + 
  geom_bar(aes(x = yearcat, fill = prevention), position = "stack") + 
  labs(x = "year")+
  scale_y_continuous(breaks = 0:13)
p4

# Graphing intervention by prevention type

p5 <- ggplot(subset_cvd) + 
  geom_bar(aes(prevention, fill = intervention), position = "stack") + 
  labs(x = "Type of prevention")

p5

p6 <- p1+p2+p3+p4+p5  ####Pete to advise on what is important here
p6
###########Characteristics of DAMs####################
####Evaluation type####
p7 <- ggplot(subset_cvd, aes(evaluation)) +
  geom_bar(stat = "count") +
  labs(x = "Evaluation type", y= "Count of Studies", title = "Type of Evaluation") +
  theme(plot.title = element_text(hjust = 0.5))

p7


####Model Typologies####
p8 <- ggplot(subset_cvd, aes(model)) +
  geom_bar(stat = "count") +
  labs(x = "Model type", y= "Count of Studies", title = "Type of Model") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks = 0:13)

p8

####Evaluation type####
p9 <- ggplot(subset_cvd, aes(perspective)) +
  geom_bar(stat = "count") +
  labs(x = "Perspective", y= "Count of Studies", title = "Perspective of Evaluation") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks = 0:11)

p9

####Horizon####
p10 <- ggplot(subset_cvd, aes(horizon)) +
  geom_bar(stat = "count") +
  labs(x = "Time Horizon", y= "Count of Studies", title = "Time Horizon") +
  theme(plot.title = element_text(hjust = 0.5))

p10


# To create a bar graph of CVD risk equations

p11 <- ggplot(risk_long, aes(risk, number)) +
  geom_bar(stat = "identity") +
  labs(x = "Risk equation", y = "No of studies", title = "CVD Risk Equations") +
  theme(plot.title = element_text(hjust = 0.5))

p11

# To create a bar graph of CVD Outcomes

p12 <- ggplot(outcome_long, aes(outcome, number)) +
  geom_bar(stat = "identity") +
  labs(x = "CVD Outcome", y = "No of studies", title = "CVD Outcome") +
  theme(plot.title = element_text(hjust = 0.5))

p12

p13 <- p7+p8+p9+p10+p11+p12



ggsave(file = "plot2.pdf", plot = p13, width = 20, height = 5)

####Type of sensitivity analysis####
p14 <- ggplot(subset_cvd, aes(sensitivity)) +
  geom_bar(stat = "count") +
  labs(x = "Sensityvity analysis", y= "Count of Studies", title = "Type of sensitivity analysis") +
  theme(plot.title = element_text(hjust = 0.5))

p14



