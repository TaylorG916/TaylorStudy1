#####Data sorting before importing to R####

#The experiment was conducting using Qualtrics software. Prior to importing 
#the data into R, irrelevant columns were removed (Qualtrics exports lots 
#of information such as start and end dates). I also created new columns to 
#indicate which trait condition each participant was allocated to (trustworthiness,
#dominance, or attractiveness) and participant number. The 100 retest items 
#were moved to a separate dataset because Rstudio confuses them with the actual 
#test items and then renames them strangely. 

#The data was also screened for invalid cases prior to importing into R. 
#19 participants were removed in total. 4 were removed because they were
#familiar with some of the identities. 7 participants were removed due 
#to presenting > 75% of same responses. This was done using frequencies 
#analysis on SPSS. A further 8 participants were removed due to displaying
#negative test-retest correlations.Finally, an additional 6 participants were removed due 
#to not  completing the experiment.

#####Required packages####
install.packages("tidyverse") #Data sorting
install.packages("lme4") #mixed effects models
install.packages("ggplot2") #Creating plots
install.packages("remotes") 
remotes::install_github("dgrtwo/drlib") #To reorder data within different facets of a plot
install.packages("psych") #Descriptive stats by group
install.packages("afex") #Adds p-values to mixed models
install.packages("readxl") #allows for importing excel files
install.packages("haven") #allows for importing SPSS files
install.packages("scales") #Makes it easier to manipulate the y-axis for ggplots

library("tidyverse") 
library("lme4")
library("ggplot2")
library("drlib")
library("Hmisc")
library("psych")
library("afex")
library("readxl")
library("haven")
library("scales")



#####Import data and convert to long format####

#Import data from excel and rename dataset.
library(readxl)
Facedata_retest_removed_ <- read_excel("Facedata (retest removed).xlsx")
Facedata <- Facedata_retest_removed_
View(Facedata)

#Convert from wide to long format.
#Currently the dataframe is in wide format and needs to be converted into long 
#format to perform linear mixed effects analysis (this will come later).
#To do this I will use the "gather" function from the tidyverse package. 
#The first part of the function creates a new dataframe ("Datalong"). To make 
#the data long form, you need to make two new columns: the image ID number and 
#trait ratings for each image.
Datalong <- gather(Facedata, ImageID, Rating, FA001:MH020) 
View(Datalong)

#Create a new column to consisting of the 17 identities of the images
Datalong$Identity <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)

#This tells R which identity each image corresponds to. Importantly, this 
#needs to be done before you make these variables into factors. It is essentially
#telling R that if the image ID is less than or equal to e.g.FA020 then it is
#identity 1. Identity 17 doesn't need to be fully typed out because the 
#"ifelse" means that if it isn't any of the other image ID's then it will 
#be coded as 17. 
Datalong$Identity <- ifelse(Datalong$ImageID <= "FA020", 1, 
ifelse(Datalong$ImageID <= "FB020", 2,
ifelse(Datalong$ImageID <= "FC020", 3,
ifelse(Datalong$ImageID <= "FD020", 4,
ifelse(Datalong$ImageID <= "FE020", 5,
ifelse(Datalong$ImageID <= "FF020", 6,
ifelse(Datalong$ImageID <= "FG020", 7,
ifelse(Datalong$ImageID <= "FH020", 8,
ifelse(Datalong$ImageID <= "FI020", 9,
ifelse(Datalong$ImageID <= "MA020", 10,
ifelse(Datalong$ImageID <= "MB020", 11,
ifelse(Datalong$ImageID <= "MC020", 12,
ifelse(Datalong$ImageID <= "MD020", 13,
ifelse(Datalong$ImageID <= "ME020", 14,
ifelse(Datalong$ImageID <= "MF020", 15,
ifelse(Datalong$ImageID <= "MG020", 16, 17))))))))))))))))

#We need to export the data back into excel in order to calculate the variability
#scores. The varaince scores will be calculated by subtracting the mean identity
#rating from the score of each each image. This needs to be done separately for
#each participant for each of the three traits.
#This code will save the dataset as an excel spreadsheet.
write.csv(Datalong, file = "Facedata long form.csv")

#####Import data for mixed effects and create factors####

#This will import a new dataset which includes the computed variability scores.
#The data is on sheet 3 of the excel spreadsheet (named "Facedata long form).
#The dataframe with the variance scores will be named "DataVar"
library(readxl)
Facedata_long_form_with_variance_removing_retest_final_1 <- read_excel("Facedata long form_with variance (removing retest final)-1.xlsx", 
                                                                           sheet = "Facedata long form")
DataVar <- Facedata_long_form_with_variance_removing_retest_final_1
View(DataVar)

#This will get rid of redundant columns 
DataVar$ID <- NULL
DataVar$`#` <- NULL
DataVar$ImID <- NULL
DataVar$...16 <- NULL
DataVar$...17 <- NULL
DataVar$...18 <- NULL
DataVar$...19 <- NULL

#Convert columns into factors. This tells R to treat the categorical variables 
#as factors. 
DataVar$Trait <- factor(DataVar$Trait)
DataVar$ImageID <- factor(DataVar$ImageID) 
DataVar$Gender <- factor(DataVar$Gender)
DataVar$`Familiar Yes/No` <- factor(DataVar$`Familiar Yes/No`)
DataVar$Identity <- factor(DataVar$Identity) 

#Rename the levels of trait as 1 = Attractiveness, 2 = Dominance, 3 = Trustworthiness 
factor(DataVar$Trait, levels = c("Attractiveness", "Dominance", "Trustworthiness"))
levels(DataVar$Trait)

#Rename factor levels for identity (1 = FA, 2 = FB etc)
levels(DataVar$Identity) <- c("FA", "FB", "FC", "FD", "FE", "FF", "FG", "FH", 
                              "FI", "MA", "MB", "MC", "MD", "ME", "MF", "MG", "MH")



#####Assessing variability across trait dimensions (using linear mixed effects)#### 

#Descriptive statistics across each trait
describeBy(x = DataVar, group = DataVar$Trait)


#We will conduct linear mixed effects models using the "lme4" package 
#to investigate the influence of the type of trait and the identity of the 
#images on the variability of rating scores. Participants were nested in the 
#type of trait (R accounts for this nesting automatically based on the data)
#and the images were nested in identities. As such, this was a 2-level model.
#The variability scores will be included as the dependent variable in each of 
#the models. Participants were entered as a random effect. 

#M1 is the null model that only includes the random effect.
M1 <- lmer(Variance ~ (1|Participant), data = DataVar, REML = FALSE)

#M2 includes Trait as a fixed effect
M2 <- lmer(Variance ~ Trait + (1|Participant), data = DataVar, REML = FALSE)

#M3 includes identity as a fixed effect
M3 <- lmer(Variance ~ Identity + (1|Participant), data = DataVar, REML = FALSE)

#M4 includes both trait and identity as a fixed effect
M4 <- lmer(Variance ~ Trait + Identity + (1|Participant), data = DataVar, REML = FALSE)

#M5 is the final model which includes the interaction of trait and identity along
#with the main effects of each. Using "*" computes the interaction as well as the
#the main effects whereas ":" only computes the interaction. 
M5 <- lmer(Variance ~ Trait*Identity + (1|Participant), data = DataVar, REML = FALSE)

#Here we run an analysis of variance to compare the fit of the different models.
anova(M1, M2, M3, M4, M5)

#This will give the output for the full model (M5)
summary(M5)

#####Plots ####

#The first plot will show the rating scores rank ordered by mean identity scores

#This dataset was created by aggregating scores across participants in order to 
#get a single rating score for each image. 
library(haven)
Rank <- read_sav("Rank.sav")
View(Rank)

#Make each categorical variable into a factor
Rank$ImageID <- factor(Rank$ImageID)
Rank$Identity_first <- factor(Rank$Identity_first)
Rank$Trait <- factor(Rank$Trait)

factor(Rank$Trait, levels = c("Trustworthy", "Dominance", "Attractiveness"))

#This creates a geom point plot which shows the avergage score for each image
#which is rank ordered according to the mean of each identity. The three traits 
#are displayed in separate facets of the figure.
ggplot(Rank) +
  aes(x = reorder_within(Identity_first, Rating_mean, Trait), y = Rating_mean, fill = Trait, colour = Trait, group = Identity_first) +
  geom_point(binaxis = 'y', stackdir = "center", stackratio = 1, dotsize = 0.3) +
  stat_summary(fun.y = mean, geom = "point", lwd = 2, col = "black", aes(group = 3)) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "", y = "Mean Rating", fill = "Trait") +
  facet_wrap(~ Trait, scales = "free", nrow = 3) + 
  scale_y_continuous(limits = c(1,9), breaks = pretty_breaks(n = 9)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_blank()) +
  theme(legend.position = "none")

#This second plot will show the avergage variability score for each identity 
#across the three different traits. The dataset was create by aggregating the 
#variability scores across participants and image IDs. As such, the dataframe 
#consists of an average variablity score for each identity on each trait. 

#Import data
library(haven)
Variance_between_traits_collapsed_across_participants_ <- read_sav("Variance between traits(collapsed across participants).sav")
VarCollapse <-Variance_between_traits_collapsed_across_participants_
View(VarCollapse)

#Make factors
VarCollapse$Trait <- factor(VarCollapse$Trait)
VarCollapse$Idenity <- factor(VarCollapse$Identity)

#Geom line plot showing the mean variability score for each identity across the 
#the three traits. 
ggplot(data = VarCollapse, aes(Identity, Variance_mean_mean, group = Trait, colour = Trait)) +
  geom_line() +
  geom_point() +
  labs(x= "Identity", y = "Mean Variability Score") +
  scale_y_continuous(limits = c(0, 1.5), breaks = pretty_breaks(n = 9)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = "bottom") 

###Retest correlations####

#After completing the rating task, participants were asked to rate a subset of 
#100 images (5 identities) a second time. This allows us to assess the consistency
#of responses within participants.

#Import the "Test and retest scores 1 (sheet 4)" dataset. The negative and zero
#correlations were removed when screening for invalid cases. 

Test_and_retest_scores_1 <- read_excel("Test-Retest/Test and retest scores-1.xlsx", 
                                            sheet = "Sheet4")
Retest <- Test_and_retest_scores_1
View(Retest)

#Make trait and participants into factors 
Retest$Trait <- factor(Retest$Trait)
Retest$Participant <- factor(Retest$Participant)

#rename levels
levels(Retest$Trait) <- c("Trustworthiness", "Dominance", "Attractiveness")
levels(Retest$Trait)

#This will reorder the levels for trait to make attractiveness frist (for
#consistency with the order of the other plots). Changes the actual order, 
#not just the labels.
Retest$Trait1 <- factor(Retest$Trait, levels = c("Attractiveness", "Dominance", "Trustworthiness"))

#descritpive stats
describeBy(x = Retest, group = Retest$Trait)

#This will create a histogram to show the distribution of participant retest 
#correlations for each trait. 
ggplot(Retest, aes(x = Correlation)) +
  geom_histogram(position="identity", colour="grey40", alpha=0.2, bins = 10) +
  facet_wrap(~ Trait1, nrow = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black")) 



###Unresolved issues####

#1. When running the mixed effects models, M2 and M4 come out with the exact 
#same values. I have no idea why this is happening. Nothing much seems to 
#change this.

#2. I don't know how to account for the fact that the images are nested within 
#identities. I am not including the image IDs in the model so I don't know how I 
#can account for this nesting without it being included in the model. 



