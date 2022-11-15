##########################################################################################################
#Project: Prescribed Fire in Tallgrass Priarie Meta-Analysis 

#Contributors: Kathryn Bloodworth, Dirac Twidwall, Alice Boyle, Ellen Welti, Marissa Ahlering, Brian Obermeyer, Chris Helzer, Bob Hamilton, Elizabeth Bach, Clare Kazanski, Sally Koerner 

#Coder: Kathryn Bloodworth, Sarah Gora
##########################################################################################################

#### Install and load libraries ####
#had to first download XQuartz on mac, then followed these steps https://www.andrewheiss.com/blog/2012/04/17/install-r-rstudio-r-commander-windows-osx/, then had to install Rcmdr 

###Gora version Install packages, latest R version
###For pc Desktop, only metagear is needed, no other packages

#install.packages("zip")
library(zip)
#install.packages("mgcv")
library(mgcv)
#Needed on all computers
#install.packages("Rcmdr")
library(Rcmdr)
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.11")
BiocManager::install("EBImage")
#install.packages("Rcpp")
library(Rcpp)
#install.packages("tcltk2")
library(tcltk2)#install.packages("data.table-package ")
library(data.table)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("metagear")
library(metagear)
#install.packages("arsenal")
library(arsenal)



#### Set Working Directories ####

#Bloodworth - Desktop
setwd("/Users/kjbloodw/Box/TNC_TGP_RxFire/Data")

#Bloodworth - Mac
setwd("~/Library/CloudStorage/Box-Box/TNC_TGP_RxFire/Data")


##### Pre Extraction Steps #####
#### Read in Data frame with first 10 papers ####

Web_of_Science_Articles<-read.csv("Papers/Articles_For_Screening_07_28_2020.csv", header = T)

#### Setting up screening tasks ####
#Following http://lajeunesse.myweb.usf.edu/metagear/metagear_basic_vignette.html#installation-and-dependencies 

#prime the study-reference dataset - function adds 4 new columns: Study_ID (unique number for each reference), Reviewers (an empty column with NAs that will populate later with reviewers), 2 columns for Include (will contain the screening efforts by both reviewers)

References_screening_ready<-effort_initialize(Web_of_Science_Articles)

names(References_screening_ready)

### Randomly delegate screening efforts to two reviewers (Kathryn and Sarah) ###
References_unscreened_<- effort_distribute(References_screening_ready, dual = TRUE, reviewers = c("Kathryn", "Sarah"), initialize = TRUE, save_split = TRUE,)


#From Initialization through assignment of papers (effort distribute) all in one step -- assigns 50/50
#References_unscreened<-effort_distribute(Web_of_Science_Articles, reviewers = c("Kathryn", "Sarah"), initialize = TRUE, save_split = TRUE)

### Randomly delegating screening efforts with two reviewers (Kathryn and Sarah), where Kathryn takes 80% of studies ### save_splot = TRUE allows this particular split to be saved to a file
#References_unscreened_60 <- effort_distribute(References_screening_ready, reviewers = Reviewers, effort = c(60,40), save_split = TRUE)

# show that files are saved in working directory by name of reviewer
list.files(pattern = "effort")


abstract_screener(file = file.choose("effort_Kathryn.csv"),
                  aReviewer = "Kathryn",
                  reviewerColumnName = "REVIEWERS_A",
                  unscreenedColumnName = "INCLUDE_A",
                  unscreenedValue = "not vetted",
                  abstractColumnName = "Abstract",
                  titleColumnName = "Article.Title",
                  browserSearch = "https://www.google.com/search?q=",
                  fontSize = 13,
                  windowWidth = 70,
                  windowHeight = 16,
                  theButtons = c("YES", "maybe", "NO"),
                  keyBindingToButtons = c("y", "m", "n"),
                  buttonSize = 10,
                  highlightColor = "powderblue",
                  highlightKeywords = c("fire","burn","grassland","tallgrass prairie","prairie", "savanna", "rangeland"))

abstract_screener(file = file.choose("effort_Sarah.csv"),
                  aReviewer = "Sarah",
                  reviewerColumnName = "REVIEWERS_B",
                  unscreenedColumnName = "INCLUDE_B",
                  unscreenedValue = "not vetted",
                  abstractColumnName = "Abstract",
                  titleColumnName = "Article.Title",
                  browserSearch = "https://www.google.com/search?q=",
                  fontSize = 13,
                  windowWidth = 70,
                  windowHeight = 16,
                  theButtons = c("YES", "maybe", "NO"),
                  keyBindingToButtons = c("y", "m", "n"),
                  buttonSize = 10,
                  highlightColor = "powderblue",
                  highlightKeywords = c("fire","burn","grassland","tallgrass prairie","prairie", "savanna", "rangeland"))

###This does not work with dual reviewers -- cannot get effort summary to work without error 

#Remerge files from both reviewers (do this if reviewers use data frames in the working directory to vet the references -- each reviewer must change the column "Include" to either a YES or NO)
#References_screened <- effort_merge()
#References_screened[c("STUDY_ID", "REVIEWERS_A", "INCLUDE_A", "REVIEWERS_B", "INCLUDE_B")]

#References_screened_1<-as.array(References_screened)


#See how many were vetted yes, no, not vetted. Review progress and check percentage of usable papers
#References_screened_Summary <- effort_summary(References_screened, dual = TRUE)

##### trying something different #####

#To get this to work I had to remove columns that interfered with merging, including the unused Reviewer in each dataframe

Kathryn<-read.csv("effort_Kathryn.csv")%>%
  select("STUDY_ID","REVIEWERS_A","INCLUDE_A") #%>% 
#rename("INCLUDE"="INCLUDE_A") %>% 
#rename("REVIEWERS"="REVIEWERS_A")

Sarah<-read.csv("effort_Sarah.csv") %>% 
  select(-"REVIEWERS_A",-"INCLUDE_A") #%>% 
#rename("INCLUDE"="INCLUDE_B")%>% 
#rename("REVIEWERS"="REVIEWERS_B")

theRefs_screened <- Kathryn %>% 
  left_join(Sarah)


screening_checks<-theRefs_screened %>% 
  select("STUDY_ID","INCLUDE_A","INCLUDE_B")

#instead of using effort_summary -- we looked together at each individual outcome and if we dissagreed, we went into excel and manually changed the answer of one of our reviews to match the other
theRefs_screened[c("STUDY_ID", "REVIEWERS_A", "INCLUDE_A","REVIEWERS_B","INCLUDE_B")]

## Sarah and Kathryn reviewed each paper together and decided on final answer when original answers were disagreed on, then each cell was changed individually

#figure out why NA isn't changin to NO
theRefs_screened[1,5]<-"NO" #change sarah's answer
theRefs_screened[2,3]<-"NO" #change kathryn's answer
theRefs_screened[2,5]<-"NO"
theRefs_screened[9,3]<-"NO"
theRefs_screened[10,5]<-"NO"
theRefs_screened[17,3]<-"NO"
theRefs_screened[19,5]<-"NO"
theRefs_screened[35,3]<-"NO"
theRefs_screened[37,3]<-"NO"
theRefs_screened[39,3]<-"NO"
theRefs_screened[39,5]<-"NO"
theRefs_screened[40,3]<-"NO"
theRefs_screened[44,5]<-"NO"
theRefs_screened[47,5]<-"NO"
theRefs_screened[52,5]<-"NO"
theRefs_screened[63,3]<-"NO"
theRefs_screened[52,5]<-"NO"
theRefs_screened[65,5]<-"NO"
theRefs_screened[66,5]<-"YES"
theRefs_screened[68,5]<-"YES"
theRefs_screened[73,5]<-"NO"
theRefs_screened[76,5]<-"NO"
theRefs_screened[86,3]<-"NO"
theRefs_screened[88,5]<-"NO"
theRefs_screened[91,5]<-"NO"
theRefs_screened[95,5]<-"NO"
theRefs_screened[97,3]<-"YES"
theRefs_screened[110,5]<-"NO"
theRefs_screened[111,3]<-"NO"
theRefs_screened[115,5]<-"NO"
theRefs_screened[132,5]<-"YES"
theRefs_screened[141,3]<-"NO"
theRefs_screened[145,3]<-"NO"
theRefs_screened[146,3]<-"NO"
theRefs_screened[148,3]<-"NO"
theRefs_screened[150,5]<-"NO"
theRefs_screened[186,5]<-"NO"
theRefs_screened[194,5]<-"NO"
theRefs_screened[213,3]<-"YES"
theRefs_screened[233,3]<-"NO"
theRefs_screened[246,5]<-"NO"
theRefs_screened[261,5]<-"NO"
theRefs_screened[263,5]<-"NO"
theRefs_screened[268,5]<-"NO"
theRefs_screened[269,5]<-"NO"
theRefs_screened[274,3]<-"YES"
theRefs_screened[285,5]<-"NO"
theRefs_screened[291,5]<-"NO"
theRefs_screened[293,5]<-"YES"
theRefs_screened[302,5]<-"NO"
theRefs_screened[303,5]<-"NO"
theRefs_screened[304,5]<-"NO"
theRefs_screened[319,5]<-"NO"
theRefs_screened[323,5]<-"NO"
theRefs_screened[326,5]<-"NO"
theRefs_screened[237,3]<-"YES"
theRefs_screened[337,3]<-"NO"
theRefs_screened[341,5]<-"NO"
theRefs_screened[342,5]<-"NO"
theRefs_screened[397,3]<-"NO"
theRefs_screened[429,5]<-"NO"
theRefs_screened[459,5]<-"NO"
theRefs_screened[469,5]<-"YES"
theRefs_screened[473,5]<-"NO"
theRefs_screened[493,5]<-"NO"
theRefs_screened[501,5]<-"NO"
theRefs_screened[536,5]<-"NO"
theRefs_screened[539,5]<-"NO"
theRefs_screened[596,5]<-"NO"
theRefs_screened[633,5]<-"NO"



theRefs_screened[c("STUDY_ID", "REVIEWERS_A", "INCLUDE_A","REVIEWERS_B","INCLUDE_B")]
#Determining how many differences

Kathryn_1<-theRefs_screened%>%
  select(-"REVIEWERS_B",-"INCLUDE_B") %>% 
  rename("INCLUDE"="INCLUDE_A") %>% 
  rename("REVIEWERS"="REVIEWERS_A")

Sarah_1<-theRefs_screened %>% 
  select(-"REVIEWERS_A",-"INCLUDE_A") %>% 
  rename("INCLUDE"="INCLUDE_B")%>% 
  rename("REVIEWERS"="REVIEWERS_B")


summary(comparedf(Kathryn_1,Sarah_1))

#### Second set of screening process ####

#merge together              

#bring in dataframe with new papers that also includes ~642 of the same papers that we already screened
Second_Screen<-read.csv("Papers/Articles_For_Screening_second_round.csv",header=T)

Second_Screen_new_col<-Second_Screen %>% 
  add_column(STUDY_ID=NA,.after = "Date.of.Export") %>%
  add_column(REVIEWERS_A="Kathryn",.after = "Date.of.Export") %>% 
  add_column(INCLUDE_A="not vetted",.after = "Date.of.Export") %>%
  add_column(REVIEWERS_B="Sarah",.after = "Date.of.Export") %>% 
  add_column(INCLUDE_B="not vetted",.after = "Date.of.Export") %>%
  add_column(STUDY_ID.1=NA,.after = "Date.of.Export") %>% 
  add_column(X=NA,.after = "Date.of.Export")

Second_Screen_merged<- Second_Screen %>% 
  right_join(theRefs_screened)

Second_Screen_New_papers<-Second_Screen_merged %>% 
  rbind(Second_Screen_new_col)

Second_Screen_Unique<- Second_Screen_New_papers[!duplicated(Second_Screen_New_papers$Article.Title),] %>% 
  rename("REVIEWERS_A1"="REVIEWERS_A") %>% 
  rename("REVIEWERS_B1"="REVIEWERS_B") %>% 
  rename("INCLUDE_A1"="INCLUDE_A") %>% 
  rename("INCLUDE_B1"="INCLUDE_B")

Second_Screen_Unique[c("STUDY_ID", "REVIEWERS_A1", "INCLUDE_A1","REVIEWERS_B1","INCLUDE_B1")]

References_unscreened_second<- effort_distribute(Second_Screen_Unique, dual = TRUE, reviewers = c("Kathryn_Second", "Sarah_Second"), initialize = TRUE, save_split = TRUE,) 

abstract_screener(file = file.choose("effort_Kathryn_Second.csv"),
                  aReviewer = "Kathryn",
                  reviewerColumnName = "REVIEWERS_A1",
                  unscreenedColumnName = "INCLUDE_A1",
                  unscreenedValue = "not vetted",
                  abstractColumnName = "Abstract",
                  titleColumnName = "Article.Title",
                  browserSearch = "https://www.google.com/search?q=",
                  fontSize = 13,
                  windowWidth = 70,
                  windowHeight = 16,
                  theButtons = c("YES", "maybe", "NO"),
                  keyBindingToButtons = c("y", "m", "n"),
                  buttonSize = 10,
                  highlightColor = "powderblue",
                  highlightKeywords = c("fire","burn","grassland","tallgrass prairie","prairie", "savanna", "rangeland"))


abstract_screener(file = file.choose("effort_Sarah_Second.csv"),
                  aReviewer = "Sarah",
                  reviewerColumnName = "REVIEWERS_B1",
                  unscreenedColumnName = "INCLUDE_B1",
                  unscreenedValue = "not vetted",
                  abstractColumnName = "Abstract",
                  titleColumnName = "Article.Title",
                  browserSearch = "https://www.google.com/search?q=",
                  fontSize = 13,
                  windowWidth = 70,
                  windowHeight = 16,
                  theButtons = c("YES", "maybe", "NO"),
                  keyBindingToButtons = c("y", "m", "n"),
                  buttonSize = 10,
                  highlightColor = "powderblue",
                  highlightKeywords = c("fire","burn","grassland","tallgrass prairie","prairie", "savanna", "rangeland"))

#remove columns that interfered with merging, including the unused Reviewer in each dataframe

Kathryn_Second<-read.csv("effort_Kathryn_Second.csv")%>%
  select("STUDY_ID","REVIEWERS_A1","INCLUDE_A1") #%>% 

Sarah_Second<-read.csv("effort_Sarah_Second.csv") %>% 
  select(-"REVIEWERS_A1",-"INCLUDE_A1") #%>% 

theRefs_screened_second <- Kathryn_Second %>% 
  left_join(Sarah_Second)

screening_checks_second<-theRefs_screened_second %>% 
  select("STUDY_ID","INCLUDE_A1","INCLUDE_B1")

#instead of using effort_summary -- we looked together at each individual outcome and if we dissagreed, we went into excel and manually changed the answer of one of our reviews to match the other
theRefs_screened_second[c("STUDY_ID", "REVIEWERS_A1", "INCLUDE_A1","REVIEWERS_B1","INCLUDE_B1")]


theRefs_screened_second[644,75]<-"NO" #change sarah's answer
theRefs_screened_second[649,3]<-"NO" #change kathryn's answer
theRefs_screened_second[662,75]<-"YES"
theRefs_screened_second[664,3]<-"YES"
theRefs_screened_second[666,75]<-"YES"
theRefs_screened_second[672,3]<-"YES"
theRefs_screened_second[674,3]<-"NO"
theRefs_screened_second[683,75]<-"NO" 
theRefs_screened_second[686,75]<-"NO" 
theRefs_screened_second[688,75]<-"YES"
theRefs_screened_second[691,3]<-"YES"
theRefs_screened_second[695,75]<-"NO" 
theRefs_screened_second[701,75]<-"YES"
theRefs_screened_second[705,3]<-"NO"
theRefs_screened_second[708,3]<-"YES"
theRefs_screened_second[714,75]<-"YES"
theRefs_screened_second[719,3]<-"NO"
theRefs_screened_second[728,3]<-"NO"
theRefs_screened_second[729,75]<-"NO"
theRefs_screened_second[746,3]<-"NO"
theRefs_screened_second[748,3]<-"NO"
theRefs_screened_second[752,3]<-"NO"
theRefs_screened_second[759,75]<-"NO"
theRefs_screened_second[760,75]<-"YES"
theRefs_screened_second[761,3]<-"NO"
theRefs_screened_second[762,75]<-"YES"
theRefs_screened_second[764,75]<-"NO"
theRefs_screened_second[767,3]<-"NO"
theRefs_screened_second[778,75]<-"NO"
theRefs_screened_second[779,75]<-"NO"
theRefs_screened_second[790,75]<-"NO"
theRefs_screened_second[791,75]<-"YES" #michigan
theRefs_screened_second[792,75]<-"NO"
theRefs_screened_second[794,75]<-"NO"
theRefs_screened_second[796,75]<-"YES" #arkansas? 
theRefs_screened_second[799,75]<-"YES" #michigan
theRefs_screened_second[810,75]<-"YES" 
theRefs_screened_second[811,3]<-"YES"
theRefs_screened_second[813,75]<-"YES" 
theRefs_screened_second[814,75]<-"NO" 
theRefs_screened_second[818,3]<-"YES"
theRefs_screened_second[823,3]<-"YES"
theRefs_screened_second[831,75]<-"NO" 
theRefs_screened_second[839,3]<-"YES"
theRefs_screened_second[842,3]<-"YES"
theRefs_screened_second[843,3]<-"NO"
theRefs_screened_second[849,75]<-"NO"
theRefs_screened_second[850,75]<-"NO"
theRefs_screened_second[865,75]<-"YES"
theRefs_screened_second[867,75]<-"YES"



Kathryn_1_second<-theRefs_screened_second%>%
  select(-"REVIEWERS_B1",-"INCLUDE_B1") %>% 
  rename("INCLUDE"="INCLUDE_A1") %>% 
  rename("REVIEWERS"="REVIEWERS_A1")

Sarah_1_second<-theRefs_screened_second %>% 
  select(-"REVIEWERS_A1",-"INCLUDE_A1") %>% 
  rename("INCLUDE"="INCLUDE_B1")%>% 
  rename("REVIEWERS"="REVIEWERS_B1")


summary(comparedf(Kathryn_1_second,Sarah_1_second))

#### Third set of screening process ####

#merge together              

#bring in dataframe with new papers that also includes ~800 of the same papers that we already screened and has duplicates as a result of two different runs being merged
Third_Screen<-read.csv("Papers/Articles_For_Screening_third_round.csv",header=T)

Third_Screen_Only_Unique<- Third_Screen[!duplicated(Third_Screen$Article.Title),] %>% 
  select(-X)

Third_Screen_new_col<-Third_Screen_Only_Unique %>% 
  add_column(STUDY_ID=NA,.after = "Date.of.Export") %>%
  add_column(REVIEWERS_A1="Kathryn",.after = "Date.of.Export") %>% 
  add_column(INCLUDE_A1="not vetted",.after = "Date.of.Export") %>%
  add_column(REVIEWERS_B1="Sarah",.after = "Date.of.Export") %>% 
  add_column(INCLUDE_B1="not vetted",.after = "Date.of.Export") %>%
  add_column(STUDY_ID.2=NA,.after = "Date.of.Export")

Third_Screen_merged<- Third_Screen_Only_Unique %>% 
  right_join(theRefs_screened_second) %>% 
  select(-X,-REVIEWERS_B,-INCLUDE_B,-STUDY_ID.1)

Third_Screen_New_papers<-Third_Screen_merged %>% 
  rbind(Third_Screen_new_col)

Third_Screen_Unique<- Third_Screen_New_papers[!duplicated(Third_Screen_New_papers$Article.Title),] %>% 
  rename("REVIEWERS_A2"="REVIEWERS_A1") %>% 
  rename("REVIEWERS_B2"="REVIEWERS_B1") %>% 
  rename("INCLUDE_A2"="INCLUDE_A1") %>% 
  rename("INCLUDE_B2"="INCLUDE_B1")

Third_Screen_Unique[c("STUDY_ID", "REVIEWERS_A2", "INCLUDE_A2","REVIEWERS_B2","INCLUDE_B2")]

References_unscreened_third<- effort_distribute(Third_Screen_Unique, dual = TRUE, reviewers = c("Kathryn_Third", "Sarah_Third"), initialize = TRUE, save_split = TRUE,) 

abstract_screener(file = file.choose("effort_Kathryn_third.csv"),
                  aReviewer = "Kathryn",
                  reviewerColumnName = "REVIEWERS_A2",
                  unscreenedColumnName = "INCLUDE_A2",
                  unscreenedValue = "not vetted",
                  abstractColumnName = "Abstract",
                  titleColumnName = "Article.Title",
                  browserSearch = "https://www.google.com/search?q=",
                  fontSize = 13,
                  windowWidth = 70,
                  windowHeight = 16,
                  theButtons = c("YES", "maybe", "NO"),
                  keyBindingToButtons = c("y", "m", "n"),
                  buttonSize = 10,
                  highlightColor = "powderblue",
                  highlightKeywords = c("fire","burn","grassland","tallgrass prairie","prairie", "savanna", "rangeland","Asia","Africa","New Zealand","Australia","Europe"))


abstract_screener(file = file.choose("effort_Sarah_third.csv"),
                  aReviewer = "Sarah",
                  reviewerColumnName = "REVIEWERS_B2",
                  unscreenedColumnName = "INCLUDE_B2",
                  unscreenedValue = "not vetted",
                  abstractColumnName = "Abstract",
                  titleColumnName = "Article.Title",
                  browserSearch = "https://www.google.com/search?q=",
                  fontSize = 13,
                  windowWidth = 70,
                  windowHeight = 16,
                  theButtons = c("YES", "maybe", "NO"),
                  keyBindingToButtons = c("y", "m", "n"),
                  buttonSize = 10,
                  highlightColor = "powderblue",
                  highlightKeywords = c("fire","burn","grassland","tallgrass prairie","prairie", "savanna", "rangeland","Asia","Africa","New Zealand","Australia","Europe"))

#remove columns that interfered with merging, including the unused Reviewer in each dataframe

Kathryn_Third<-read.csv("effort_Kathryn_third.csv")%>%
  select("STUDY_ID","REVIEWERS_A2","INCLUDE_A2") #%>% 

Sarah_Third<-read.csv("effort_Sarah_third.csv") %>% 
  select(-"REVIEWERS_A2",-"INCLUDE_A2") #%>% 

theRefs_screened_third<- Kathryn_Third %>% 
  left_join(Sarah_Third)

screening_checks_third<-theRefs_screened_third %>% 
  select("STUDY_ID","INCLUDE_A2","INCLUDE_B2")

#instead of using effort_summary -- we looked together at each individual outcome and if we dissagreed, we went into excel and manually changed the answer of one of our reviews to match the other
theRefs_screened_third[c("STUDY_ID", "REVIEWERS_A2", "INCLUDE_A2","REVIEWERS_B2","INCLUDE_B2")]

theRefs_screened_third[,76]<-"NO" #change sarah's answer
theRefs_screened_third[,3]<-"NO" #change kathryn's answer

Kathryn_1_third<-theRefs_screened_third%>%
  select(-"REVIEWERS_B2",-"INCLUDE_B2") %>% 
  rename("INCLUDE"="INCLUDE_A2") %>% 
  rename("REVIEWERS"="REVIEWERS_A2")

Sarah_1_third<-theRefs_screened_third %>% 
  select(-"REVIEWERS_A2",-"INCLUDE_A2") %>% 
  rename("INCLUDE"="INCLUDE_B2")%>% 
  rename("REVIEWERS"="REVIEWERS_B2")

summary(comparedf(Kathryn_1_third,Sarah_1_third))

#### First Round Data Extraction Analysis #### 

#### Load Libraries and Set Working Directory #### 
# lots of libraries that mostly get the base map shapes and colors
#install.packages("wesanderson")
library(wesanderson)
#devtools::install_github("ropenscilabs/rnaturalearth")
library("rnaturalearth")
library(sp)
#install.packages("rnaturalearthdata")
library("rnaturalearthdata")
#install.packages("rgeos")
library("rgeos")
#install.packages("maps")
library(maps)
library(tidyverse)
#install.packages("reshape")
library(reshape)
#install.packages("mapproj")
library(mapproj)
# setting the color palatte
# install.packagesTR("devtools")
#devtools::install_github("katiejolly/nationalparkcolors")
library(nationalparkcolors)
GeneralGrantpal<-park_palette("GeneralGrant", 7)
CraterLakepal <- park_palette("CraterLake", 7)



#working directory - KB - Mac
setwd("~/Library/CloudStorage/Box-Box/TNC_TGP_RxFire/Data")

#### Set ggplot base ####
#Set ggplot2 theme to black and white
theme_set(theme_bw())
#Update ggplot2 theme - make box around the x-axis title size 30, vertically justify x-axis title to 0.35, Place a margin of 15 around the x-axis title.  Make the x-axis title size 30. For y-axis title, make the box size 30, put the writing at a 90 degree angle, and vertically justify the title to 0.5.  Add a margin of 15 and make the y-axis text size 25. Make the plot title size 30 and vertically justify it to 2.  Do not add any grid lines.  Do not add a legend title, and make the legend size 20
theme_update(axis.title.x=element_text(size=60, vjust=-0.35, margin=margin(t=15)),
             axis.text.x=element_text(size=60), axis.title.y=element_text(size=60, angle=90, vjust=0.5,
                                                                          margin=margin(r=15)), axis.text.y=element_text(size=60), plot.title =
               element_text(size=60, vjust=2), panel.grid.major=element_blank(),
             panel.grid.minor=element_blank(), legend.title=element_blank(),
             legend.text=element_text(size=60))

#### Load in Data ####

#load in second round sceening data extraction
InitialDataExtraction<-read.csv("Second_Round_Screening/UPDATED_Second_Round_Screening_data_extraction_edited.csv")


#### Clean Up Data ####
BasicDataExtraction<-InitialDataExtraction %>% 
  select(-Updated_Master,-Assigned_To,-Accessible,-Downloaded,-Can.we.request.it.,-Requested,-X,-X.1,-X.2,-Study_ID_New) %>% 
  #keep only data that we are interested in 
  filter(Keeping=="yes") %>% 
  #reassign unique identifier for each study
  mutate(New_Study_ID = row_number())

#### Number of studies for each response variable ####

#Create data frame with just response variables
ResponseVariables<-BasicDataExtraction %>% 
  select(Total_Soil_Carbon,Total_Soil_Nitrogen,Microbial_Biomass,Arthropods,Birds,Small_Mammals,Plants)
  
#Create data frame with information on how many studies looked at each response variable
ResponseVariables_Counted<-as.data.frame(sapply(X = ResponseVariables, FUN = table)) %>% 
  #subset data to just look at counts of those papers that looked at each response variable
  subset(rownames(ResponseVariables_Counted) %in% "yes")

rownames(ResponseVariables_Counted)<-NULL

#Make ResponseVariables_Counted Long instead of wide
ResponseVariables_Counted_Long<-melt(ResponseVariables_Counted)
  
#Create graph with response variables on x axis and number of papers on y axis
ggplot(ResponseVariables_Counted_Long,aes(x=reorder(variable,-value),y=value,fill=variable))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.
  geom_bar(stat="identity")+
  #Label the x-axis
  xlab("Response Variables")+
  #Label the y-axis 
  ylab("Number of Papers")+
  scale_fill_manual(values=CraterLakepal)+
  #Make the y-axis expanded
  expand_limits(y=200)+
  theme(axis.text.x=element_text(angle=45, hjust=1),legend.position = "NONE")+
  scale_x_discrete(limits = c("Plants", "Birds", "Arthropods", "Small_Mammals","Total_Soil_Nitrogen","Microbial_Biomass","Total_Soil_Carbon"),breaks= c("Plants", "Birds", "Arthropods", "Small_Mammals","Total_Soil_Nitrogen","Microbial_Biomass","Total_Soil_Carbon"),labels = c("Plants", "Birds", "Arthropods", "Small Mammals","Soil Nitrogen","Microbial Biomass","Soil Carbon"))+
  #add text with count above bar graphs
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25, size=20)
#Save at the graph at 1400x1500

#### Number of Studies that looked at 2 or more variables compared to 1 ####
NumResponseVariables<-BasicDataExtraction %>% 
  select(New_Study_ID,Total_Soil_Carbon,Total_Soil_Nitrogen,Microbial_Biomass,Arthropods,Birds,Small_Mammals,Plants) %>% 
  #make each yes a 1 and each no a 0
  mutate(Total_Soil_Carbon_Count=ifelse(Total_Soil_Carbon=="yes",1,0)) %>% 
  mutate(Total_Soil_Nitrogen_Count=ifelse(Total_Soil_Nitrogen=="yes",1,0)) %>% 
  mutate(Microbial_Biomass_Count=ifelse(Microbial_Biomass=="yes",1,0)) %>% 
  mutate(Arthropods_Count=ifelse(Arthropods=="yes",1,0)) %>% 
  mutate(Birds_Count=ifelse(Birds=="yes",1,0)) %>%
  mutate(Small_Mammals_Count=ifelse(Small_Mammals=="yes",1,0)) %>% 
  mutate(Plants_Count=ifelse(Plants=="yes",1,0)) %>% 
  #Remove old columns
  select(-Total_Soil_Carbon,-Total_Soil_Nitrogen,-Microbial_Biomass,-Arthropods,-Birds,-Small_Mammals,-Plants) %>% 
  #count up number of response variables studied for each paper
  mutate(NumResponseVariables = select(., Total_Soil_Carbon_Count:Plants_Count) %>% 
  rowSums(na.rm = TRUE)) %>% 
  count(NumResponseVariables)
  
#Create graph with number of response variables studied in each paper on x axis and number of papers on y axis
ggplot(subset(NumResponseVariables,NumResponseVariables!=0),aes(x=NumResponseVariables,y=n))+
  #Make a bar graph where the height of the bars is equal to the data (stat=identity) and you preserve the vertical position while adjusting the horizontal(position_dodge), and fill in the bars with the color grey.
  geom_bar(stat="identity")+
  #Make an error bar that represents the standard error within the data and place the error bars at position 0.9 and make them 0.2 wide.
  #Label the x-axis "Treatment"
  xlab("Number of Response Variables")+
  #Label the y-axis "Species Richness"
  ylab("Number of Papers")+
  #Make the y-axis extend to 50
  expand_limits(y=200)+
  #add text with count above bar graphs
  geom_text(aes(label= n), position=position_dodge(width=0.9), vjust=-0.25, size=10)
#Save at the graph at 1400x1500

#### Map of Study Sites ####

#Map information
world <- ne_countries(scale = "medium", returnclass = "sp")

#create a dataframe with just lat/long measurements
Map_Dataframe<-BasicDataExtraction %>% 
  filter(Longitude!="",Longitude!="N/A",Longitude!="-") %>% 
  mutate(Lat=as.numeric(Latitude)) %>% 
  mutate(Long=as.numeric(Longitude)) %>% 
  select(New_Study_ID,Lat,Long)

#create dataframe with just NA map data
NA_MapData<-map_data("world") %>% 
  filter(region==c("USA","Canada"))

#dataframe with only US - TGP states in it
TGP_MapData<-map_data("state")%>% 
  filter(region==c("illinois","indiana","iowa","kansas","michigan","minnesota","missouri","nebraska","north dakota","ohio","oklahoma","south dakota","texas","wisconsin"))

#dataframe with only Canada - TGP providences
TGP_MapData_Canada <- canada.cities %>% 
  filter(country.etc==c("SK","MB"))

#create a dataframe for just response variables
Map_ResponseVariables <- BasicDataExtraction %>% 
  filter(Longitude!="",Longitude!="N/A",Longitude!="-") %>% 
  mutate(Lat=as.numeric(Latitude)) %>% 
  mutate(Long=as.numeric(Longitude))

##lat/long per Response variables 
#Soil Carbon
Map_ResponseVariables_SC<-Map_ResponseVariables %>% 
  select(New_Study_ID,Total_Soil_Carbon,Lat,Long) %>% 
  filter(Total_Soil_Carbon=="yes") %>% 
  mutate(Response_Variable=ifelse(Total_Soil_Carbon=="yes","Soil Carbon","")) %>% 
  mutate(Unique_ID="a") %>% 
  mutate(New_Study_ID=paste(New_Study_ID,Unique_ID,sep="")) %>% 
  select(-Total_Soil_Carbon,-Unique_ID)

#Soil Nitrogen
Map_ResponseVariables_SN<-Map_ResponseVariables %>% 
  select(New_Study_ID,Total_Soil_Nitrogen,Lat,Long) %>% 
  filter(Total_Soil_Nitrogen=="yes")%>% 
  mutate(Response_Variable=ifelse(Total_Soil_Nitrogen=="yes","Soil Nitrogen","")) %>% 
  mutate(Unique_ID="b") %>% 
  mutate(New_Study_ID=paste(New_Study_ID,Unique_ID,sep="")) %>% 
  select(-Total_Soil_Nitrogen,-Unique_ID)

#Microbial_Biomass
Map_ResponseVariables_MB<-Map_ResponseVariables %>% 
  select(New_Study_ID,Microbial_Biomass,Lat,Long) %>% 
  filter(Microbial_Biomass=="yes")%>% 
  mutate(Response_Variable=ifelse(Microbial_Biomass=="yes","Microbial Biomass","")) %>% 
  mutate(Unique_ID="c") %>% 
  mutate(New_Study_ID=paste(New_Study_ID,Unique_ID,sep="")) %>% 
  select(-Microbial_Biomass,-Unique_ID)

#Arthropods
Map_ResponseVariables_Arth<-Map_ResponseVariables %>% 
  select(New_Study_ID,Arthropods,Lat,Long) %>% 
  filter(Arthropods=="yes")%>% 
  mutate(Response_Variable=ifelse(Arthropods=="yes","Arthropods","")) %>% 
  mutate(Unique_ID="d") %>% 
  mutate(New_Study_ID=paste(New_Study_ID,Unique_ID,sep="")) %>% 
  select(-Arthropods,-Unique_ID)

#Birds
Map_ResponseVariables_Bird<-Map_ResponseVariables %>% 
  select(New_Study_ID,Birds,Lat,Long) %>% 
  filter(Birds=="yes")%>% 
  mutate(Response_Variable=ifelse(Birds=="yes","Birds","")) %>% 
  mutate(Unique_ID="e") %>% 
  mutate(New_Study_ID=paste(New_Study_ID,Unique_ID,sep="")) %>% 
  select(-Birds,-Unique_ID)

#Small_Mammals
Map_ResponseVariables_SMam<-Map_ResponseVariables %>% 
  select(New_Study_ID,Small_Mammals,Lat,Long) %>% 
  filter(Small_Mammals=="yes")%>% 
  mutate(Response_Variable=ifelse(Small_Mammals=="yes","Small Mammals","")) %>% 
  mutate(Unique_ID="f") %>% 
  mutate(New_Study_ID=paste(New_Study_ID,Unique_ID,sep="")) %>% 
  select(-Small_Mammals,-Unique_ID)

#Plants
Map_ResponseVariables_Pl<-Map_ResponseVariables %>% 
  select(New_Study_ID,Plants,Lat,Long) %>% 
  filter(Plants=="yes")%>% 
  mutate(Response_Variable=ifelse(Plants=="yes","Plants","")) %>%
  mutate(Unique_ID="g") %>% 
  mutate(New_Study_ID=paste(New_Study_ID,Unique_ID,sep="")) %>% 
  select(-Plants,-Unique_ID)

#create one dataframe with each study 
Map_ResponseVariables_LatLong<-Map_ResponseVariables_Arth %>% 
  rbind(Map_ResponseVariables_Bird) %>% 
  rbind(Map_ResponseVariables_MB) %>% 
  rbind(Map_ResponseVariables_Pl) %>% 
  rbind(Map_ResponseVariables_SC) %>% 
  rbind(Map_ResponseVariables_SMam) %>% 
  rbind(Map_ResponseVariables_SN) 

#Write a csv file onto the computer
write.csv(Map_ResponseVariables_LatLong, file = "Map_ResponseVariables_LatLong.csv")


#map of locations of meta-analysis studies
ggplot()+
  geom_polygon(data = NA_MapData, aes(x=long, y = lat, group = group), fill="white",colour="darkgray", alpha=0.3) +
  geom_polygon(data = TGP_MapData, aes(x=long, y=lat, group = region, fill = region),fill="gray")+
  geom_polygon(data = TGP_MapData_Canada, aes(x=long, y=lat, group = country.etc, fill = country.etc),fill="gray")+
  borders("state",colour="black") +
  xlim(-180,-50)+
  geom_point(data=Map_ResponseVariables_LatLong, mapping=aes(x=Long,y=Lat,fill=Response_Variable),size=3.5,shape=21) +  #this is the dataframe of lat/long, and the points are being colored by num_codominants, with the point shape and size specified at the end fill=response variable
  scale_colour_manual(values=GeneralGrantpal)+
  theme(text=element_text(size=20, colour="black"),axis.text.x=element_text(size=20, colour="black"),axis.text.y=element_text(size=20, colour="black")) + #formatting the text
  ylab(expression("Latitude "*degree*""))+ #labels for the map x and y axes
  xlab(expression("Longitude "*degree*"")) +
  labs(fill="Response Variable") + #legend label
  theme(legend.position=c(0.15,0.2))  #legend position
#export at 1500 x 1000
  

#### Full Data Extraction ####

#load packages

library(lmerTest)
#install.packages("multcomp")
library(multcomp)
#install.packages("data.table")
library(data.table)
#install.packages("tidyverse")
library(tidyverse)

#### Read in Data ####

#read in dataframe with data from main extraction
Data_extraction<-read.csv("Data Extraction/PrescribedFire_DataExtraction_Main.csv")

#### Calculate Methodological Information ####
#Get basic information about data collected so far
length(unique(Data_extraction$PDF_Study_ID)) #40 unique paper IDs

#determine how many papers are in each fire return interval category
Fire_categories<-Data_extraction %>% 
  group_by(Treatment_Category) %>% 
  summarise(Count_Treatment_Cat=n()) %>% 
  ungroup() #255 data points for 1yr interval, 202 data points for 2-4yr interval, 193 data points for fire+grazing 

#Determine how many years the data points came from
Study_Years<-Data_extraction %>% 
  group_by(Year) %>% 
  summarise(Count_Year=n()) %>% 
  ungroup() 

#Determine how many data points of each response variables
Response_Variables<-Data_extraction %>% 
  group_by(Response_Variable) %>% 
  summarise(Count_ResponseVariable=n()) %>% 
  ungroup() 

#Determine how many studies of each response variables
Response_Variables_Study<-Data_extraction %>% 
  group_by(PDF_Study_ID,Response_Variable) %>% 
  summarise(Count_StudyID=n()) %>% 
  ungroup() %>% 
  group_by(Response_Variable) %>% 
  summarise(Count_StudyID_Variable=n())


#Removing_Papers<-BasicDataExtraction %>% #83 papers were removed from study
  #filter(Data.extraction.Screening..nos.=="no") %>% 
  #select(Reason.for.removing..big.data.extraction.,PDF_Study_ID,Author,Title,Latitude,Longitude,Total_Soil_Carbon,Total_Soil_Nitrogen,Microbial_Biomass,Arthropods,Birds,Small_Mammals,Plants,Data.extraction.Screening..nos.) %>% 
  #add in column to say if we could extract data from paper based on a 1 year fire return interval as "control" instead of no burn
  #mutate(One_year_burn=ifelse(PDF_Study_ID==811, "could compare annual to 1 2 year burn",ifelse(PDF_Study_ID==212,"annual vs 4-6 year",ifelse(PDF_Study_ID==760,"annual vs 3yr but data not presented in extractable way",ifelse(PDF_Study_ID==648,"prescribed fire was studied but no context as to interval - check supplimental",ifelse(PDF_Study_ID==437,"One site was annually burned in spring (April) throughout the three years of the study (2000–2002) while the other site was left unburned - both were burned prior to study",ifelse(PDF_Study_ID==458,"annual vs 4 year - KNZ",ifelse(PDF_Study_ID==1146,"annual vs 4 year - KNZ",ifelse(PDF_Study_ID==602, "annual vs 4 year - KNZ", ifelse(PDF_Study_ID==586,"annual vs 4 year - KNZ",ifelse(PDF_Study_ID==966,"annual vs 4 year - KNZ",ifelse(PDF_Study_ID==883,"annual vs 4 year - KNZ",NA))))))))))))

#### Calculate Response Ratio - Hedges' G ####

#Response Ratio by Hand
RR_by_Hand<-Data_extraction %>% 
  #take mean of sample numbers that gave a range 
  mutate(Sample_number_NoBurn=ifelse(Sample_number_NoBurn=="140-150",145,Sample_number_NoBurn)) %>% 
  mutate(Sample_number_Category=ifelse(Sample_number_Category=="140-150",145,Sample_number_Category)) %>% 
  #Remove paper 594 because sample number is not known -  #go back and remove most of this after editing the data
  filter(PDF_Study_ID!=594 & PDF_Study_ID!=1097 & PDF_Study_ID!=121 & PDF_Study_ID!=453 & PDF_Study_ID!="507d" & PDF_Study_ID!="533v" & PDF_Study_ID!="533p")

RR_by_Hand$Sample_number_Category=as.numeric(RR_by_Hand$Sample_number_Category)
RR_by_Hand$Sample_number_NoBurn=as.numeric(RR_by_Hand$Sample_number_NoBurn)

RR_Calc<-RR_by_Hand %>% 
  #calculate standard deviation
  mutate(Standard_Dev_Cat=Standard_Error_Category/(sqrt(Sample_number_Category)),Standard_Dev_Cont=Standard_Error_NoBurn/(sqrt(Sample_number_NoBurn))) %>% 
  #Calculate pooled SD
  mutate(PooledSD=sqrt(((Sample_number_Category-1)*Standard_Dev_Cat^2 + (Sample_number_NoBurn-1)*Standard_Dev_Cont^2) / (Sample_number_Category+Sample_number_NoBurn-2))) %>% 
  #calculate RR (Hedges G)
  mutate(Hedges_G_RR=(Mean_Category-Mean_NoBurn)/PooledSD) %>% 
  #get direction of RR
  mutate(Response_Direction=ifelse(Hedges_G_RR<1,-1,ifelse(Hedges_G_RR>1,1,Hedges_G_RR))) %>% 
  #Create column with absolute values of hedges G number
  mutate(Abs_Hedges_G_RR=abs(Hedges_G_RR)) %>% 
  #LnRR + 1
  mutate(LN_Hedges_G_RR=log(Abs_Hedges_G_RR+1)) %>% 
  #multiply response direction by LnRR
  mutate(LnRR=(Response_Direction*LN_Hedges_G_RR)) %>% 
  #create two new columns with response variables seperated out into RV and Data Type
  separate(Response_Variable,c("ResponseVariable","DataType"),sep="_") %>% 
  #remove rarity
  filter(Data_Type!="rarity") %>% 
  group_by(PDF_Study_ID,ResponseVariable,Data_Type,DataType, Treatment_Category) %>% 
  mutate(Study_Point_n=length(Study_Point)) %>% 
  ungroup()


#### Visualize the data with Histograms ####
#histogram of all data
hist(RR_Calc$LnRR)

#ggplot with facet wrap of data type and response variables
ggplot(RR_Calc, aes(x=LnRR, color=Treatment_Category,fill=Treatment_Category))+
  geom_histogram(position="identity",alpha=0.5)+
  facet_grid(Data_Type ~ ResponseVariable)
#save at 2000 x 1000

#### Statistical Models ####

#### Plant Glm Models ####
### Use basic glm for all analyses

RR_Calc$Treatment_Category=as.factor(RR_Calc$Treatment_Category)

#Look at Diversity of Plants
Diversity_Plants_glm <- glm(LnRR ~ Treatment_Category, data = subset(RR_Calc,ResponseVariable=="Plant" & Data_Type=="diversity"))
anova(Diversity_Plants_glm) 
#post hoc test for lmer test
summary(glht(Diversity_Plants_glm, linfct = mcp(Treatment_Category = "Tukey"), test = adjusted(type = "BH")))

#with random effect of PDF_Study ID
Diversity_Plants_Glmm <- lmer(LnRR ~ Treatment_Category + (1|PDF_Study_ID), data = subset(RR_Calc,ResponseVariable=="Plant" & Data_Type=="diversity"))
anova(Diversity_Plants_Glmm)

#with random effect of Study_Point_n nested in PDF_Study ID 
Diversity_Plants_Glmm_nest <- lmer(LnRR ~ Treatment_Category + (1|PDF_Study_ID:Study_Point_n), data = subset(RR_Calc,ResponseVariable=="Plant" & Data_Type=="diversity"))
anova(Diversity_Plants_Glmm_nest)

#Compare AIC Values
AIC(Diversity_Plants_glm,Diversity_Plants_Glmm,Diversity_Plants_Glmm_nest) #Diversity_Plants_glm (simplest) is the best

### 

#Look at Abundance of Plants 
Abundance_Plants_glm <- glm(LnRR ~ Treatment_Category, data = subset(RR_Calc,ResponseVariable=="Plant" & Data_Type=="abundance"))
anova(Abundance_Plants_glm) 

#with random effect of PDF_Study ID
Abundance_Plants_Glmm <- lmer(LnRR ~ Treatment_Category + (1|PDF_Study_ID), data = subset(RR_Calc,ResponseVariable=="Plant" & Data_Type=="abundance"))
anova(Abundance_Plants_Glmm)

#with random effect of Study_Point_n nested in PDF_Study ID 
Abundance_Plants_Glmm_nest <- lmer(LnRR ~ Treatment_Category + (1|PDF_Study_ID:Study_Point_n), data = subset(RR_Calc,ResponseVariable=="Plant" & Data_Type=="abundance"))
anova(Abundance_Plants_Glmm_nest)

#Compare AIC Values
AIC(Abundance_Plants_glm,Abundance_Plants_Glmm,Abundance_Plants_Glmm_nest) #Abundance_Plants_Glmm is the best but AIC is 1513.589 compared to glm which is 1521.799

#### Arthropod GLMs ####

#Look at Diversity of Arthropods
Diversity_Arthropods_glm <- glm(LnRR ~ Treatment_Category, data = subset(RR_Calc,ResponseVariable=="Arthropod" & Data_Type=="diversity"))
anova(Diversity_Arthropods_glm) 

#with random effect of PDF_Study ID
Diversity_Arthropods_Glmm <- lmer(LnRR ~ Treatment_Category + (1|PDF_Study_ID), data = subset(RR_Calc,ResponseVariable=="Arthropod" & Data_Type=="diversity"))
anova(Diversity_Arthropods_Glmm)

#with random effect of Study_Point_n nested in PDF_Study ID 
Diversity_Arthropods_Glmm_nest <- lmer(LnRR ~ Treatment_Category + (1|PDF_Study_ID:Study_Point_n), data = subset(RR_Calc,ResponseVariable=="Arthropod" & Data_Type=="diversity"))
anova(Diversity_Arthropods_Glmm_nest)

#Compare AIC Values
AIC(Diversity_Arthropods_glm,Diversity_Arthropods_Glmm,Diversity_Arthropods_Glmm_nest) #Diversity_Arthropods_GLmm is the best but AIC of Glmm is 51.02890 and AIC of glm= 54.09142

### 

#Look at Abundance of Arthropods 
Abundance_Arthropods_glm <- glm(LnRR ~ Treatment_Category, data = subset(RR_Calc,ResponseVariable=="Arthropod" & Data_Type=="abundance"))
anova(Abundance_Arthropods_glm) 

#with random effect of PDF_Study ID
Abundance_Arthropods_Glmm <- lmer(LnRR ~ Treatment_Category + (1|PDF_Study_ID), data = subset(RR_Calc,ResponseVariable=="Arthropod" & Data_Type=="abundance"))
anova(Abundance_Arthropods_Glmm)

#with random effect of Study_Point_n nested in PDF_Study ID 
Abundance_Arthropods_Glmm_nest <- lmer(LnRR ~ Treatment_Category + (1|PDF_Study_ID:Study_Point_n), data = subset(RR_Calc,ResponseVariable=="Arthropod" & Data_Type=="abundance"))
anova(Abundance_Arthropods_Glmm_nest)

#Compare AIC Values
AIC(Abundance_Arthropods_glm,Abundance_Arthropods_Glmm,Abundance_Arthropods_Glmm_nest) #Abundance_Arthropods_Glmm is the best but AIC is 152.7377 compared to glm which is 154.7938




#### Graphs of RR by Response Variable ####

#Make Dataframe for graphs 
RR_Calc_Avg<-RR_Calc %>% 
  group_by(ResponseVariable, Treatment_Category, Data_Type) %>%
  summarize(std=sd(LnRR,na.rm=TRUE),Mean=mean(LnRR,na.rm=TRUE),n=length(LnRR)) %>%
  mutate(St_Error=std/sqrt(n)) %>% 
  ungroup()

#Abundance
ggplot(data=subset(RR_Calc_Avg,Data_Type=="abundance"),aes(x=Mean, y=Treatment_Category)) +
  geom_vline(aes(xintercept = 0), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmin=Mean-St_Error,xmax=Mean+St_Error), size = .8, height = .2, color = "gray50")+
  geom_point(size=4) +
  facet_wrap(~ResponseVariable)
#save at 2000x1000


#Diversity
ggplot(data=subset(RR_Calc_Avg,Data_Type=="diversity"),aes(x=Mean, y=Treatment_Category)) +
  geom_vline(aes(xintercept = 0), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmin=Mean-St_Error,xmax=Mean+St_Error), size = .8, height = .2, color = "gray50")+
  geom_point(size=4) +
  facet_wrap(~ResponseVariable)
#save at 2000x1000



#### Graphs ####
#map of big data extraction data points so far
ggplot()+
  geom_polygon(data = NA_MapData, aes(x=long, y = lat, group = group), fill="white",colour="darkgray", alpha=0.3) +
  geom_polygon(data = TGP_MapData, aes(x=long, y=lat, group = region, fill = region),fill="gray")+
  geom_polygon(data = TGP_MapData_Canada, aes(x=long, y=lat, group = country.etc, fill = country.etc),fill="gray")+
  borders("state",colour="black") +
  xlim(-180,-50)+
  geom_count(data=Data_extraction, mapping=aes(x=Longitude,y=Latitude,fill=PDF_Study_ID)) +  #this is the dataframe of lat/long, and the points are being colored by num_codominants, with the point shape and size specified at the end fill=response variable
  scale_size_area()+
  scale_colour_manual(values=GeneralGrantpal)+
  theme(text=element_text(size=20, colour="black"),axis.text.x=element_text(size=20, colour="black"),axis.text.y=element_text(size=20, colour="black")) + #formatting the text
  ylab(expression("Latitude "*degree*""))+ #labels for the map x and y axes
  xlab(expression("Longitude "*degree*"")) +
  labs(fill="Response Variable") + #legend label
  theme(legend.position=c(0.15,0.2))  #legend position
#export at 1500 x 1000



#### Tried but decided not to do ####

## Plant Linear Regression Models ##

### not going to do these because there is not enough spread of the data across longitude to accurately compare

#Create function for linear regression
linreg = function (formula) {
  m=lm(formula)
  list(slope=coefficients(m)[2], intercept=coefficients(m)[1], adj.r2=summary(m)$adj.r.squared, f=summary(m)$fstatistic[1],p.val=summary(m)$coefficients[2,4])
}

#Look at abundance of Plants
#create data table of plant abundance
RR_Plant_Abundance<-data.table(subset(RR_Calc,ResponseVariable=="Plant" & Data_Type=="abundance"))

#linreg is x by y
Abundance_Plants_regression <- RR_Plant_Abundance[,linreg(Longitude~LnRR),by=Treatment_Category]
Abundance_Plants_regression #negative r2 means so much spread that there is no trend (could be noice)

#Look at diversity of Plants
#create data table of plant diversity
RR_Plant_Diversity<-data.table(subset(RR_Calc,ResponseVariable=="Plant" & Data_Type=="diversity"))

#linreg is x by y
Diversity_Plants_regression <- RR_Plant_Diversity[,linreg(Longitude~LnRR),by=Treatment_Category]
Diversity_Plants_regression

## Graphs of Regressions (by longitude) ##

## Abundance 
ggplot(data=subset(RR_Calc,Data_Type=="abundance"), aes(x=-Longitude,y=LnRR,fill=Treatment_Category,color=Treatment_Category))+
  geom_jitter()+
  facet_wrap(~ResponseVariable)
#geom_smooth(data=subset(RR_Plant_Abundance,Treatment_Category=="2-4yr"),aes(x=-Longitude,y=LnRR,fill=Treatment_Category,color=Treatment_Category),method="lm",se=FALSE)

## Diversity
ggplot(data=subset(RR_Calc,Data_Type=="diversity"), aes(x=-Longitude,y=LnRR,fill=Treatment_Category,color=Treatment_Category))+
  geom_jitter()+
  facet_wrap(~ResponseVariable)

##Plant Diversity
ggplot(RR_Plant_Diversity, aes(x=-Longitude,y=LnRR,fill=Treatment_Category,color=Treatment_Category))+
  geom_jitter()+
  geom_smooth(data=subset(RR_Plant_Diversity,Treatment_Category=="2-4yr"),aes(x=-Longitude,y=LnRR,fill=Treatment_Category,color=Treatment_Category),method="lm",se=FALSE)

