#Problem #2
library(tidyverse)

#Step 1: Load the FIW data into R.

FIW <- read.csv(file = 'data/2020_All_Data_FIW_2013-2020.csv', skip = 1)
head(FIW)


#Step 2: Keep only the most recent year of data. Keep only the countries (not the territories).

library(dplyr)


FIW2020 <- filter(FIW, Edition == '2020') #Filter for 2020 data only


FIW2020 %>% 
  select(Country.Territory, C.T, Edition, A, B, C, D, E, F, G) %>% # select relevant variables
  filter(C.T == "c") %>%
  head(10) # output the first 10 rows


#Step 3: Of the 25 questions, some are about democracy (i.e. are political leaders elected?) and others are
#about freedom/liberty (i.e. can people practice their religion without fear of persecution? is there a
#free and independent media? etc.). Create one variable called democracy_score, equal to the sum of
#all the scores on questions related to democracy, and another variable called liberty_score, equal to
#the sum of all the scores on questions related to liberty.

FIW2020 <- FIW2020 %>% #create democracy_score
  mutate(democracy_score = A + B + C)

summary(FIW2020$democracy_score)

FIW2020 <- FIW2020 %>% #create liberty_score
  mutate(liberty_score = D + E + F + G)

summary(FIW2020$liberty_score)

#Step 4: Visualize the relationship between liberty and democracy.
ggplot(FIW2020, aes(x=democracy_score, y=liberty_score)) +
  geom_point(color = 'cyan', shape=1)+
  theme_classic() +
  labs(x = 'Democracy Scores',
       y = 'Liberty Scores')
  

#Step 5: Split the countries into democratic and non-democratic (your choice where you split) and conduct a
#bivariate hypothesis test. Are the democratic countries more free on average than the non-democratic
#countries?

#ENB: I am setting the democracy cut-off at countries with scores of =/>21

#splitting up the countries by democracy_score



FIW2020 <- FIW2020 %>%
  mutate(democracy_status = case_when(democracy_score < 21  ~ 'NonDemocratic', TRUE ~ 'Democratic'))


#choose a test
#These are not normal distributions so we will go with t-test

test1 <- FIW2020 %>%
  filter(democracy_status %in% c('NonDemocratic', 'Democratic')) %>%
  t.test(liberty_score ~ democracy_status, data = .)


test1

#Welch Two Sample t-test

#data:  liberty_score by democracy_status
#t = 24.775, df = 197.46, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  27.63798 32.41829
#sample estimates:
#  mean in group Democratic mean in group NonDemocratic 
#47.14050                    17.11236 



#checking to make sure all the observations are present - 210
summary(FIW2020$democracy_status)

