## library(geofacet)
library(data.table)
library(tidyverse)
library(ggplot2)
library(patchwork)  ### for combining plots
library(ggthemes)
library(ggpubr)

## setting working directory
## setwd("U:/ManWin/My Documents/James Oguta/My PhD Folder-2023-2024/My Systematic Review/Synthesis/Tables/Final/Analyses")
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
 selected <- c("id", "authors", "country", "year", "evaluation", "prevention", "intervention", "cvd_outcomes",
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
                                       ifelse(subset_cvd$intervention=="Tobacco control","Primordial",
                                       subset_cvd$prevention)))

subset_cvd$prevention <- ifelse(subset_cvd$prevention=="Both","Primary & Secondary",
                                subset_cvd$prevention)
subset_cvd$intervention <- ifelse(subset_cvd$intervention=="HSS","Health Systems Strengthening",
                                subset_cvd$intervention)

subset_cvd$model <- ifelse(subset_cvd$model=="Proportional multi-state life table Markov model","Multistate Life table model",
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

risk_long$risk <- ifelse(risk_long$risk=="framingham","Framingham",
                         ifelse(risk_long$risk== "who", "WHO",
                                ifelse(risk_long$risk== "globorisk", "Globorisk",
                                       ifelse(risk_long$risk=="cox", "Cox Model",risk_long$risk))))


####Cleaning the horizon column

subset_cvd$horizon <- ifelse(grepl("Lifetime", subset_cvd$horizon), "Lifetime",
                             ifelse(grepl("10 years", subset_cvd$horizon), "10 years", 
                                    subset_cvd$horizon))

###Model Outcomes###
# Search for "Ischemic heart disease" in the outcome variable
subset_cvd$ihd <- ifelse(grepl("Ischemic heart |ischemic heart|infarction|Coronary|coronary|congestive|Congestive", subset_cvd$cvd_outcomes, ignore.case = TRUE), 1, 0)

# Search for "Stroke" in the outcome variable
subset_cvd$stroke <- ifelse(grepl("Stroke |stroke", subset_cvd$cvd_outcomes, ignore.case = TRUE), 1, 0)

# Search for "CVA" in the outcome variable
subset_cvd$cva <- ifelse(grepl("Cerebrovascular |derebrovascular|CVA", subset_cvd$cvd_outcomes, ignore.case = TRUE), 1, 0)

# Search for "HHD" in the outcome variable
subset_cvd$hhd <- ifelse(grepl("hypertensive |HHD", subset_cvd$cvd_outcomes, ignore.case = TRUE), 1, 0)

# Search for "Angina" in the outcome variable
subset_cvd$angina <- ifelse(grepl("Angina |angina", subset_cvd$cvd_outcomes, ignore.case = TRUE), 1, 0)

# Search for "Cardiac Arrest" in the outcome variable
subset_cvd$ca <- ifelse(grepl("Cardiac Arrest| arrest", subset_cvd$cvd_outcomes, ignore.case = TRUE), 1, 0)

# Search for "Atherosclerosis" in the outcome variable
subset_cvd$athero <- ifelse(grepl("athero", subset_cvd$cvd_outcomes, ignore.case = TRUE), 1, 0)


outcome_columns <- c("ihd", "stroke", "cva", "angina","ca","athero" )
outcome_wide <- subset_cvd[, ..outcome_columns,drop=FALSE] # the .. syntax within the square brackets to select columns dynamically

# Reshape data into a long format
outcome_long <- reshape2::melt(as.matrix(outcome_wide), id.vars = "authors", variable.name = "outcome", value.name = "number")
outcome_long <- outcome_long[outcome_long$number > 0, ]  # Keep only rows where disease is present

authors <- cvd_data$authors
colnames(outcome_long)[colnames(outcome_long) == "Var1"] ="study"
colnames(outcome_long)[colnames(outcome_long) == "Var2"] ="outcome"

outcome_long$outcome <- ifelse(outcome_long$outcome=="ihd","Ischemic Heart Disease",
                         ifelse(outcome_long$outcome=="stroke","Stroke",
                                ifelse(outcome_long$outcome=="angina","Angina",
                                       ifelse(outcome_long$outcome=="ca","Cardiac Arrest",
                                              ifelse(outcome_long$outcome=="cva","Cerebrovascular Accident",
                                                     ifelse(outcome_long$outcome=="athero","Atherosclerosis",outcome_long$outcome))))))
                                              

########################################################################################################################
###Creating a subset of the dataset to select a few variables for characteristics

to_subset <- c("id", "authors", "country", "year", "evaluation", "prevention", "intervention", "model")

# Creating a subset of study characteristics dataset###
characteristics <- subset_cvd %>% select(id, authors, country, year, evaluation,prevention,intervention, model)
characteristics$year <- as.character(subset_cvd$year)

# renaming authors to study
characteristics<-rename(characteristics, study= authors)

##Duplicating Lin 2019 and assigning it to Nigeria (b) and South Africa (a)

# Filter the dataframe to select the observation to duplicate Lin et al 2019 
observation_to_duplicate <- characteristics %>% filter(study == "Lin et al. (2019)") %>% slice(1)
##renaming/assigning the original variable to south africa before appending
characteristics$study[grepl("Lin", characteristics$study) & grepl("South", characteristics$country)] <- "Lin et al. (2019a)"
characteristics$country[grepl("Lin", characteristics$study) & grepl("South", characteristics$country)] <- "South Africa"

# Duplicate the selected observation and append it to the original dataframe
characteristics <- bind_rows(characteristics, observation_to_duplicate)

#renaming/assigning duplicate variable to Nigeria
characteristics$study[grepl("Lin", characteristics$study) & grepl("Nigeria", characteristics$country)] <- "Lin et al. (2019b)"
characteristics$country[grepl("Lin", characteristics$study) & grepl("Nigeria", characteristics$country)] <- "Nigeria"
###Changing Gaziano et al 2015 (Guatemala, Mexico and South Africa to South Africa for graphing
characteristics$country[grepl("Gaziano", characteristics$study) & grepl("Guatemala", characteristics$country)] <- "South Africa"

characteristics$country[grepl("Multicountry", characteristics$country)] <- "Multicountry"

str(characteristics)

## Create factor variables
characteristics$country <- factor(characteristics$country, levels = unique(characteristics$country),
                               ordered = TRUE)
characteristics$prevention <- factor(characteristics$prevention, levels = unique(characteristics$prevention),
                                  ordered = FALSE)
characteristics$intervention <- factor(characteristics$intervention, levels = unique(characteristics$intervention),
                                     ordered = FALSE)
characteristics$model <- factor(characteristics$model, levels = unique(characteristics$model),
                                       ordered = FALSE)

### Characteristics of Included Studies###############
###General characteristics#########

# Graphing year of publication

p1 <- ggplot(subset_cvd, aes(yearcat)) + 
  geom_bar() +
  labs(x = "Publication Year", y = "Number of studies") +
  scale_y_continuous(breaks = 0:13)

p1

# Plotting combinations of studies in each country
## NOTE Lin 2019: down as Nigeria and South Africa
## going to assume these are separate studies rather than both needing coding as multicountry
## TODO check and correct

dataset_long$study <- as.character(dataset_long$study) #to char for editing
dataset_long$study[grepl("Lin", dataset_long$study) & grepl("South", dataset_long$country)] <- "Lin et al. (2019a)"
dataset_long$study[grepl("Lin", dataset_long$study) & grepl("Nigeria", dataset_long$country)] <- "Lin et al. (2019b)"
dataset_long$study <- factor(dataset_long$study,
  levels = unique(dataset_long$study),
  ordered = TRUE
)

table(dataset_long$study)
summary(dataset_long$study)
## make factor again:
dataset_long$country <- factor(dataset_long$country, levels = unique(dataset_long$country),
                               ordered = TRUE)



## select colors:
clz <- tableau_color_pal("Tableau 10")(10)
sclz <- clz[as.integer((dataset_long$country))] # manual study colors
names(sclz) <- dataset_long$study               #making named colors

p2 <- ggplot(dataset_long,
             aes(x = country,
                 fill = study)) +
  geom_bar(stat = "count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +  # Center the title
  scale_y_continuous(breaks = seq(0, 27, 1)) +
  labs(x = "Country", y = "Count") +
  scale_fill_manual(values=sclz)+
  theme_classic() + grids()
p2

## TODO might make sense to order the countries meaningfully:
## eg alphabetcal + multicountry, geographically, or by county


# To create a bar graph using ggplot2- Alternative 2
p3 <- ggplot(dataset_long, aes(x = country, y = number)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "No of studies", title = "Number of studies by country")

p3


## relevel logically
subset_cvd$prevention <- factor(subset_cvd$prevention,
  levels = unique(subset_cvd$prevention)[c(1, 4, 5, 2, 3)],
  ordered = TRUE
)

# Graphing year of publication by prevention type-Pete to advise on this.
## TODO it's a bit weird to use these different-sized age bins - makes it hard to interpret trend- To discuss
p4 <- ggplot(subset_cvd) + 
  geom_bar(aes(x = yearcat, fill = prevention), position = "stack") + 
  labs(x = "Year",y="Count")+
  scale_fill_gdocs()+
  scale_y_continuous(breaks = 0:13)+
  theme_classic() + grids() +
  theme(legend.position = c(0.2,0.8))
p4

# Graphing intervention by prevention type


p5 <- ggplot(subset_cvd) + 
  geom_bar(aes(prevention, fill = intervention), position = "stack") + 
  labs(x = "Type of prevention",y="Count") +
  scale_fill_colorblind()+
  theme_classic() + grids() +
  theme(legend.position = c(0.8, 0.8))
p5

p6 <- p2 / (p4 + plot_spacer()+ p5) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))
p6

ggsave(file = "plot1.pdf", plot = p6, width = 20, height = 11)
ggsave(file = "plot1.png", plot = p6, width = 20, height = 11, dpi = 300)

# Graphing prevention level and intervention type by country
## make factor again:

characteristics$country <- factor(characteristics$country, levels = unique(dataset_long$country),
                               ordered = TRUE)

p7 <- ggplot(characteristics) + 
  geom_bar(aes(country, fill = prevention), position = "stack") + 
  labs(x = "Type of prevention by country",y="Count") +
  scale_fill_colorblind()+
  theme_classic() + grids() +
  theme(legend.position = c(0.6, 0.8))
p7


p8 <- ggplot(characteristics) + 
  geom_bar(aes(x = country, fill = intervention), position = "stack") + 
  labs(x = "Type of intervention by country",y="Count")+
  scale_fill_gdocs()+
  scale_y_continuous(breaks = 0:8)+
  theme_classic() + grids() +
  theme(legend.position = c(0.6,0.8))
p8

p9 <- p7 / p8 +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))
p9
ggsave(file = "plot2.pdf", plot = p9, width = 16, height = 10)
ggsave(file = "plot2.png", plot = p9, width = 16, height = 10, dpi = 300)



###########Characteristics of DAMs####################
####Model type####
p10 <- ggplot(characteristics) + 
  geom_bar(aes(country, fill = model), position = "stack") + 
  labs(x = "Type of model by country",y="Count") +
  scale_fill_colorblind()+
  theme_classic() + grids() +
  theme(legend.position = c(0.5, 0.8))
p10

p11 <- ggplot(characteristics, aes(model)) +
  geom_bar(stat = "count") +
  scale_y_continuous(breaks = 0:25)+
  labs(x = "Model type", y= "Count") +
  theme(plot.title = element_text(hjust = 0.5))

p11



####Evaluation type####
p12 <- ggplot(characteristics, aes(evaluation)) +
  geom_bar(stat = "count") +
  scale_y_continuous(breaks = 0:25)+
  labs(x = "Evaluation type", y= "Count") +
  theme(plot.title = element_text(hjust = 0.5))

p12


####Perspective####
p13 <- ggplot(subset_cvd, aes(perspective)) +
  geom_bar(stat = "count") +
  labs(x = "Perspective", y= "Count") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks = 0:11)

p13

####Horizon####
p14 <- ggplot(subset_cvd, aes(horizon)) +
  geom_bar(stat = "count") +
  labs(x = "Time Horizon", y= "Count") +
  scale_y_continuous(breaks = 0:20)+
  theme(plot.title = element_text(hjust = 0.5))

p14

# To create a bar graph of CVD risk equations

p15 <- ggplot(risk_long, aes(risk, number)) +
  geom_bar(stat = "identity") +
  labs(x = "CVD Risk equation", y = "Count") +
  scale_y_continuous(breaks = 0:11)+
  theme(plot.title = element_text(hjust = 0.5))

p15

# To create a bar graph of CVD Outcomes

p16 <- ggplot(outcome_long, aes(outcome, number)) +
  geom_bar(stat = "identity") +
  labs(x = "CVD Outcome", y = "Count") +
  scale_y_continuous(breaks = 0:25)+
  theme(plot.title = element_text(hjust = 0.5))

p16

p17 <- (p12+p11)/(p13+p14)/(p15+p16)+
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))

p17

ggsave(file = "plot3.pdf", plot = p17, width = 20, height = 11)
ggsave(file = "plot3.png", plot = p17, width = 20, height = 11, dpi = 300)

####Type of sensitivity analysis####
p18 <- ggplot(subset_cvd, aes(sensitivity)) +
  geom_bar(stat = "count") +
  labs(x = "Sensitivity analysis", y= "Count", title = "Type of sensitivity analysis") +
  theme(plot.title = element_text(hjust = 0.5))

p18



