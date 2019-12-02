library(dplyr) # Piping symble
library(readr) # Importing data frame
library(ggplot2) # Plitting
library(tidyr) # Data cleaning
library(xtable) # Used to generate latex output
library(pander) # Used to generate tables that are more readable (not shown in output)
library(gridExtra) # Used to plot multiple plots at once
library(lmtest) # Used for Durbin Watson Test
rm(list = ls()) # Reset global environment

# ---------------------
# Importing the dataset
# ---------------------
eyecontact <- read_csv("eyecontact.txt", 
                       col_names = FALSE, col_types = cols(X5 = col_skip()))
colnames(eyecontact) <- c("ID", "Score", "Gender", "Photo")
eyecontact %>%
  mutate(Gender = ifelse(Gender == 1, "Male"            , "Female")) %>%
  mutate(Photo  = ifelse(Photo  == 1, "No eye contact" , ifelse(
                         Photo  == 2, "Eye contact"    , "Eye contact and smile"))) -> eyecontactLabeled
# The eye contactLabeled dataset is best used for creating visualisations since it is more clear
# for the reader what the data represents
# ---------------------

# -------------------------
# Exploratory data analysis
# -------------------------
# Differences between kind of pictures
eyecontactLabeled %>%
  group_by(Photo) %>%
  summarise("Obs."    = n(),
            "Average" = round(mean(Score)  , 2),
            "Median"  = round(median(Score), 2),
            "Sd."     = round(sd(Score)    , 2),   
            "Minimum" = round(min(Score)   , 2), 
            "Maximum" = round(max(Score)   , 2)) %>%
  xtable

plotList <- list() #Needed to plot both boxplot graphs in one picture
plotList[["kindOfPicture"]] <- eyecontactLabeled %>%
  ggplot(aes(x = reorder(Photo, Score, fun = median), y = Score, colour = Photo, fill = Photo)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Type of picture", y = "Score given by recruiters",
       title = "Boxplots showing the scores given by recruiters",
       subtitle = "Split based on the different kinds of pictures") +
  scale_fill_manual(values=c("#1261A0", "#58CCED", "#072F5F")) +
  scale_colour_manual(values=c("#1261A0", "#58CCED", "#072F5F")) +
  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", 
               fun.data = function(x){c(y=median(x), ymin=median(x), ymax=median(x))})

# Differences between the gender
eyecontactLabeled %>%
  group_by(Gender) %>%
  summarise("Obs."    = n(),
            "Average" = round(mean(Score)  , 2),
            "Median"  = round(median(Score), 2),
            "Sd."     = round(sd(Score)    , 2),   
            "Minimum" = round(min(Score)   , 2), 
            "Maximum" = round(max(Score)   , 2)) %>%
  xtable


plotList[["Gender"]] <- eyecontactLabeled %>%
  ggplot(aes(x = reorder(Gender, Score, fun = median), y = Score, colour = Gender, fill = Gender)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Gender of the recruiter", y = "Score given by recruiters",
       title = "Boxplots showing the scores given by recruiters",
       subtitle = "Split based on the gender of the recruiter") +
  scale_fill_manual(values=c("#3895D3", "#81D4FA")) +
  scale_colour_manual(values=c("#3895D3", "#81D4FA")) +
  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", 
               fun.data = function(x){c(y=median(x), ymin=median(x), ymax=median(x))})

do.call("grid.arrange", c(plotList, ncol=2)) # Plotting the two graphs in one picture
remove(plotList) #Keep global environment clean 

# Differences for the combination of gender and type of picture
eyecontactLabeled %>%
  mutate(Groups = paste(Photo, Gender, sep = " - ")) %>%
  group_by(Groups) %>%
  summarise("Obs."    = n(),
            "Average" = round(mean(Score)  , 2),
            "Median"  = round(median(Score), 2),
            "Sd."     = round(sd(Score)    , 2),   
            "Minimum" = round(min(Score)   , 2), 
            "Maximum" = round(max(Score)   , 2)) %>%
  xtable
plotList <- list() #Needed to plot both boxplot graphs in one picture

plotList[["boxplotCombi"]] <- eyecontactLabeled %>%
  mutate(Groups = paste(Photo, Gender, sep = "\n")) %>%
  ggplot(aes(x = reorder(Groups, Score, fun = median), y = Score, colour = Groups, fill = Groups)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Group", y = "Score given by recruiters",
       title = "Boxplots showing the scores given by recruiters",
       subtitle = "Split based on the gender of the recruiter and the kind of picture") +
  scale_fill_manual(values=c("#81D4FA", "#58CCED","#3895D3", "#1261A0", "#072F5F", "#0288D1")) +
  scale_colour_manual(values=c("#81D4FA", "#58CCED","#3895D3", "#1261A0", "#072F5F", "#0288D1")) +
  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", 
               fun.data = function(x){c(y=median(x), ymin=median(x), ymax=median(x))}) +
  theme(legend.position = "none") 

plotList[["pointsCombi"]] <- eyecontactLabeled %>%
  mutate(Groups = paste(Photo, Gender, sep = "\n")) %>%
  ggplot(aes(x = reorder(Groups, Score, fun = median), y = Score, colour = Groups, fill = Groups)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Group", y = "Score given by recruiters",
       title = "Scatterplot showing the scores given by recruiters",
       subtitle = "Split based on the gender of the recruiter and the kind of picture") +
  scale_fill_manual(values=c("#81D4FA", "#58CCED","#3895D3", "#1261A0", "#072F5F", "#0288D1")) +
  scale_colour_manual(values=c("#81D4FA", "#58CCED","#3895D3", "#1261A0", "#072F5F", "#0288D1")) +
  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="black", 
               fun.data = function(x){c(y=mean(x), ymin=mean(x), ymax=mean(x))}) +
  theme(legend.position = "none")
do.call("grid.arrange", c(plotList, ncol=2))
remove(plotList) # Keeping global environment clean

# ------------------------- Here starts Francisco's code
#First of all, creating the model
eye_contact <- eyecontact # Francisco uses _, he must be Mexican
model_for_anova <- lm(Score ~ Gender * Photo, data = eye_contact)
summary(model_for_anova)

#Creating a qqPlot with the residuals
###Small observation: Please let us know if we should follow the same color scale
###as the ggplot2 graphs
car::qqPlot(model_for_anova$residuals)

#Also important to take a look at the density of the residuals
ggplot(fortify(model_for_anova), aes(x = .resid, y = ..density..)) + 
  geom_histogram(fill = "#81D4FA", color =  "#58CCED" , bins = 20) + 
  geom_density() + theme_minimal() +
  labs(x = "Residual value", y = "Density", title = "Histogram with density plot of the residuals",
       subtitle = "Used to asses the normality assumption")

#And the tests for normality
kolg_test <- ks.test(model_for_anova$residuals, "pnorm", alternative = 'two.sided')
sh_test <- shapiro.test(model_for_anova$residuals)

#Creating the tables to show both tests
recompiling_stats <- rbind(c(kolg_test$statistic, kolg_test$p.value), c(sh_test$statistic, sh_test$p.value))
colnames(recompiling_stats) <- c('Test Statistic', 'P-Value')
rownames(recompiling_stats) <- c('Kolmogorov-Smirnov Test', 'Shapiro-Wilks Test')
recompiling_stats %>% knitr::kable('latex', label = 'Normality tests', booktabs = TRUE) %>% 
  kableExtra::kable_styling(latex_options = c('hold_position'), full_width = FALSE)

#Plot to assess equality of variance
ggplot(fortify(model_for_anova), aes(x = .fitted, y = rstandard(model_for_anova))) + 
  geom_point() + xlab('Fitted Value') + ylab('Standardized residuals') + theme_minimal() +
  stat_summary(geom = 'crossbar', width = 0.65, fatten = 0, color = 'black',
               fun.data = function(x){c(y=mean(x), ymin=mean(x), ymax=mean(x))}) +
  labs(title = "Plot of standardized residuals vs the fitted values", subtitle = "Used to assess homoscedasticity assumption")

#Formal test for the equality of variance
car::leveneTest(Score ~ Gender*Photo, data = eye_contact)
eyecontactLabeled %>%
  mutate(groups = paste(Gender, Photo, sep =  " - ")) -> temp
bartlett.test(Score ~ groups, data = temp)
remove(temp)

#Independence
res_graph <- data.frame(lag_res = rstandard(model_for_anova)[-c(1)], 
                   res = rstandard(model_for_anova)[-c(dim(eyecontact)[1])])
ggplot(res_graph, aes(x = lag_res, y = res)) + geom_point() + 
  geom_smooth(method = 'lm', se = FALSE) + xlab('Lagged Residuals') + 
  ylab('Residuals') + theme_minimal() +
  labs(title = "Lagged residual vs residual plot", subtitle = "Used to assess independence assumption")

dwtest(model_for_anova)
