
##---------------------------------------------------------------
##                  Loading Packages and Data                  --
##---------------------------------------------------------------


library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(GGally) # Extension to 'ggplot2'
library(knitr) # A General-Purpose Package for Dynamic Report Generation in R
library(ggcorrplot) # Visualization of a Correlation Matrix using 'ggplot2'
library(factoextra) # Extract and Visualize the Results of Multivariate Data Analyses
library(cluster) # "Finding Groups in Data": Cluster Analysis Extended Rousseeuw et al.
library(pastecs) # Package for Analysis of Space-Time Ecological Series
library(reshape2) # Flexibly Reshape Data: A Reboot of the Reshape Package
library(clValid) # Validation of Clustering Results
library(naniar) # Data Structures, Summaries, and Visualisations for Missing Data
library(DEGreport) # Report of DEG analysis

# reading data
raw_lines <- readLines("data/FPL.csv") # reading data by line
raw_lines <- gsub('(^"|"$)', "", raw_lines) # removing the outer quotes and then using double double quotes as quotes
raw_data <- read.csv(textConnection(raw_lines), quote = '""', header = TRUE, row.names = 1) # reading data


##---------------------------------------------------------------
##                  Exploratory Data Analysis                  --
##---------------------------------------------------------------

dim(raw_data) # dimensions: 480 x 16 matrix
str(raw_data) # structure of data
head(raw_data) # fist six rows
gg_miss_var(raw_data) # viz missing value
summary(raw_data) # summary of data
stat.desc(raw_data)

# Density Plot
raw_data %>% 
  melt(
    # ID variables - all the variables to keep but not split apart on
     id.vars=c('Team', 'Position'),
     # The source columns
     measure.vars=c(colnames(raw_data)[-c(1,2)]),
     # add name to variables and new variable
     variable.name="measurment",
     value.name="value"
) %>% # set scales free since all variables in different range
  ggplot(aes(value)) +geom_density(color = 'steelblue', fill = 'steelblue') + facet_wrap(~measurment, scales = "free") +
  labs(title = 'Density Plot of Variables') + theme(plot.title=element_text(color='black',hjust=0.5,size=12)) 


# correlation  calculation

correlationMatrix <- cor(raw_data[,-c(1,2)], use = 'complete.obs', method = 'pearson') # corr matrix
kable(correlationMatrix) # print corr matrix
correlationMatrix[correlationMatrix <= 0.7 | correlationMatrix ==1] <- ""

# Corr vizz
ggcorr(data = raw_data[,-c(1,2)], name = "corr", label = TRUE, method ='complete.obs' )+
  labs(title="Correlation Matrix of Numeric Variables")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))

# Scatter Plots of The Most Correlated Variables

list(
  transmute(raw_data, x = Threat, y = Cost, dataset = 'Threat vs Cost'),
  transmute(raw_data, x = points, y = Creativity, dataset = 'Points vs Creativity'),
  transmute(raw_data, x = Minutes, y = Influence, dataset = 'Minutes vs Influence'),
  transmute(raw_data, x = Bonus, y = Influence, dataset = 'Bonus vs Influence'),
  transmute(raw_data, x = points, y = Influence, dataset = 'points vs Influence'),
  transmute(raw_data, x = Goals_scored, y = Threat, dataset = 'Goals_scored vs Threat'),
  transmute(raw_data, x = points, y = Threat, dataset = 'points vs Threat'),
  transmute(raw_data, x = Goals_conceded, y = Influence, dataset = 'Goals_conceded vs Influence'),
  transmute(raw_data, x = Minutes, y = Goals_conceded, dataset = 'Minutes vs Goals_conceded')
)%>%
  bind_rows() %>%
  ggplot(aes(x, y)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)+
  geom_cor(method = 'pearson')+ labs(title = 'Scatter Plots of The Most Correlated Variables', xlab = '', ylab = '')+
  theme(plot.title=element_text(color='black',hjust=0.5,size=12)) +
  facet_wrap(~ dataset, scales = "free")




