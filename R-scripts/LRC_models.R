#dependencies
library(nls.multstart)
library(ggplot2)
library(broom)
library(purrr)
library(dplyr)
library(tidyr)
library(nlstools)

# #set working directory
# Andy <- "C:/Users/apo/Documents/Manuscripts/Conor Amax N/Working Data Files"
# Jack <- "C:\\Users\\Jack\\Documents\\TEAL\\Andy_light_response_curves"
# dir <- Jack # change this to Andy
# setwd(dir)

fit_run <- 1 #You'll be doing this a bunch? 
#I put this in here so we can do some sort of tracking on the .csv export later

#read in the NH Light Response Curve data and clean it up
data <- read.csv("New Hampshire Light Response Curve Data.csv",
                 header = T,
                 stringsAsFactors = F,
                 na.strings = c("", "NA", -9999))

#Ok -- in the sake of transparent coding, I've converted this to tidyverse pipe code
#if you look you'll see that I've made it so any punctuation is reduced.
# if you did want to keep the ones with _ we can easily replace [[:punct:]] with \\. like you had
#next I rename the two variabnles you had. You can add to this list and rename more if you want
cleandata <- data%>%
  select_all(~gsub("[[:punct:]]", "", .))%>%
  rename(PAR = PARSetpointumolm2s1,
         PSN = NetCO2uptakenmolgleaf1s1) 

rm(data); gc() #remote data -- we're not actually using it.

#this could be easily just piped into the chunk of code above, but I've pulled it out so that
#if you want you can chose a different set of variables to look at
datadf <- cleandata %>%
  select(SampleName, Site, Plot, Species, Year, Date, PSN, PAR, a1init, Amaxinit)



#undertanding th structure here:
#group_by and nest act to condense the data structure down
#We can see that all the data is condensed into tibbles for each 'sampleName'
#call PSNfit1$data to view the new structure
#mutate allows for the creation of new variables -- so within that we're creating fit
# fit relys on purr::map, which allows us to apply a function (nls_multstart) 
# to all the nested data 

#a question to Andy -- should we make this PSN variable generic -- 
#this is what you'll be changing, isn't it?
#So this is the model -- and everything after this is summary stats.
PSNfit <- datadf %>%
  group_by(.,SampleName) %>%
  nest() %>%
  mutate(fit = purrr::map(data, ~ nls_multstart(PSN ~ (a1*PAR*Amax1)/(a1*PAR+Amax1),
                                                data = .x,
                                                iter = 1000,
                                                start_lower = c(a1=0, Amax1=0),
                                                start_upper = c(a1=5, Amax1=600),
                                                supp_errors = 'Y',
                                                na.action = na.omit)))

#get summary
#from broom:
# Glance accepts a model object (in our case -- fit) and returns a tibble::tibble() 
# with exactly one row of model summaries. The summaries are typically
# goodness of fit measures, p-values for hypothesis tests on residuals, 
# or model convergence information.

#if we call info without unnest, we see that all the useful information from glance 
#is still bound up in the tibble. (info$summary --- which we created with mutate) 
# Unnest breaks that tibble down --
# we're still left with two lists 'data' and 'fit' because they contain a structure
# that cannot be unnested (i.e. they're not just numbers or characters)
#-- they show the models,etc. 
#I'm dropping the 'data' and 'fit' list columns here -- we don't need them for further analysis?

info <- PSNfit %>% 
  mutate(summary = map(fit, glance))%>%
  unnest(summary) %>%
  select(., -data, -fit)


#get params
#from broom:
# tidy: constructs a data frame that summarizes the model's statistical findings.
# This includes coefficients and p-values for each term in a regression, 
# per-cluster information in clustering applications,
# or per-test information for multtest functions.


#A note: I still don't understand why we need ".," in the mutate call. 
#I'm wondering if it some how acts on all 'layer's of the tibble?
# but as best as I can tell, it works even if we don't include it?

params <- PSNfit %>% 
  mutate(., p = map(fit, tidy)) %>%
  unnest(p)

#get confidence intervals, using the confint2 function from nlstools
#breakdown:
#mutate -- create feature called cis containing confidence intervals
#then we're convertiing it to a data.frame
#unnest pulls the confidence intervals out -- making them each a column in CI
#next we rename to low and high
#I'm a bit perplexed by the group_by -- running up to this point and then with it appear to do the same thing?
#finally, we create a new var -- term -- where we've pulled out 
# a1 and Amax1 from $fit -- these are the variables from our iniital equation?
#and ungroup it all

CI <- PSNfit %>%
  mutate(., cis = map(fit, confint2),
         cis = map(cis, data.frame)) %>%
  unnest(cis) %>%
  rename(., conf.low = X2.5.., conf.high = X97.5..) %>%
  group_by(., SampleName) %>%
  mutate(., term = c('a1', 'Amax1')) %>%
  ungroup() 
#I didn't need the select(., -data, -fit) component here

params <- merge(params, CI, by = intersect(names(params), names(CI)))

#I've got this blocked out right now because I'm realizing that Amax and A already have a 1 
#I'm assuming that this corresponds with the mdoel run??
#anyway, I'd but that fit_run variable at the top of the script --
#if we wanted to stick the model run number onto the term (A, Amax) we can use this paste line

#params$term <- paste(params$term, fit_run, sep = '_')

#there may be a better way to do this, but I think it works fine
#first, nest takes all the variables and nests them into the key -- value_col
#spread converts from 'long' to 'wide' format data with a column for A1 and Amax1
#unnest does that -- it unpacks value_col, and assigned the new name


params <- params%>%
  nest(estimate, std.error, statistic, p.value, conf.low, conf.high, .key = 'value_col')%>%
  spread(key = term, value = value_col)%>%
  unnest(a1,Amax1, .sep = '_')

#just calling this join for now -- join params and info
join <- full_join(params, info)


#this will write out the file as a .csv -- name it how you like.
#write.csv(join, 'test_output.csv')

#get predictions
#from broom
# augment: add columns to the original data that was modeled.
# This includes predictions, residuals, and cluster assignments.
preds <- PSNfit %>%
  mutate(., p = map(fit, augment)) %>%
  unnest(p)



#Is this in a form that is actually useful?
j2 <- full_join(cleandata,preds)
