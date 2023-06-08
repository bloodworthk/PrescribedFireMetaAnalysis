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
  filter(region==c("USA")) %>% 
  filter(subregion!="Hawaii" & subregion!="Alaska")

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



#### Full Data Extraction ####

#load packages

#install.packages("multcomp")
library(multcomp)
#install.packages("data.table")
library(data.table)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("devtools")
library(devtools)
#install.packages("ggimage")
library(ggimage)
library(patchwork)
#install.packages("rsvg")
library(rsvg)
library(vegan) 

#### Read in Data ####

#Bloodworth - Mac
setwd("~/Library/CloudStorage/Box-Box/TNC_TGP_RxFire/Data")

#Bloodworth - PC
setwd("/Users/kjbloodw/Box/TNC_TGP_RxFire/Data")

#### Set ggplot base ####
#Set ggplot2 theme to black and white
theme_set(theme_bw())
#Update ggplot2 theme - make box around the x-axis title size 30, vertically justify x-axis title to 0.35, Place a margin of 15 around the x-axis title.  Make the x-axis title size 30. For y-axis title, make the box size 30, put the writing at a 90 degree angle, and vertically justify the title to 0.5.  Add a margin of 15 and make the y-axis text size 25. Make the plot title size 30 and vertically justify it to 2.  Do not add any grid lines.  Do not add a legend title, and make the legend size 20
theme_update(axis.title.x=element_text(size=30, vjust=-0.35, margin=margin(t=15)),
             axis.text.x=element_text(size=30), axis.title.y=element_text(size=30, angle=90, vjust=0.5,margin=margin(r=15)), axis.text.y=element_text(size=30), plot.title = element_text(size=30, vjust=2), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.title=element_text(size=30), legend.text=element_text(size=30))

#read in dataframe with data from main extraction
Data_extraction<-read.csv("Data Extraction/PrescribedFire_DataExtraction_Main.csv") %>% 
  #remove north pines, NE site (not TGP)
  filter(Longitude!=-99.80000) %>% 
  #make column with latlong together
  mutate(Lat_Long=paste(Latitude,Longitude,sep="_"))
  

#### Calculate Methodological Information ####
#Get basic information about data collected so far
length(unique(Data_extraction$PDF_Study_ID)) #39 unique paper IDs

#determine how many studies are in each fire return interval category
Data_extraction %>% 
  select(PDF_Study_ID,Treatment_Category) %>% 
  unique() %>% 
  group_by(Treatment_Category) %>% 
  summarise(Count_Treatment_Cat=n()) %>% 
  ungroup() #249 data points for 1yr interval, 236 data points for 2-4yr interval, 191 data points for fire+grazing 

#determine how many data points are in each fire return interval category
Data_extraction %>% 
  group_by(Treatment_Category) %>% 
  summarise(Count_Treatment_Cat=n()) %>% 
  ungroup() #249 data points for 1yr interval, 236 data points for 2-4yr interval, 191 data points for fire+grazing 

#determine how many unique locations there are
length(unique(Data_extraction$Lat_Long)) #19 unique locations

#determine how many unique locations there are by fire return interval
Data_extraction %>% 
  select(Lat_Long,Treatment_Category) %>% 
  unique() %>% 
  group_by(Treatment_Category) %>% 
  summarise(Count_Treatment_Cat=n()) %>% 
  ungroup() #1 year: 6 unique locations, 2-4yr: 8 unique locations, fire+grazing: 11 unique locations

#Determine how many years the data points came from
Data_extraction %>% 
  group_by(Year) %>% 
  summarise(Count_Year=n()) %>% 
  ungroup() 

#determine how many total diversity and abundance studies
Data_extraction %>% 
  filter(Response_Variable!="TotalSoilCarbon" & Response_Variable!="TotalSoilNitrogen") %>% 
  group_by(Data_Type) %>% 
  summarise(Count_ResponseVariable=n()) %>% 
  ungroup() 

#Determine how many data points of each response variables
Data_extraction %>% 
  group_by(Response_Variable) %>% 
  summarise(Count_ResponseVariable=n()) %>% 
  ungroup() 

#Determine how many studies of each response variables
Data_extraction %>% 
  select(Response_Variable,PDF_Study_ID) %>% 
  unique() %>% 
  group_by(Response_Variable) %>% 
  summarise(Count_StudyID_Variable=n())

#### Calculate Response Ratio - Hedges' G ####

#Response Ratio by Hand
RR_by_Hand<-Data_extraction %>% 
  #take mean of sample numbers that gave a range 
  mutate(Sample_number_NoBurn=ifelse(Sample_number_NoBurn=="140-150",145,Sample_number_NoBurn)) %>% 
  mutate(Sample_number_Category=ifelse(Sample_number_Category=="140-150",145,Sample_number_Category)) %>% 
  #Remove paper 594 because sample number is not known -  #go back and remove most of this after editing the data
  filter(PDF_Study_ID!=594 & PDF_Study_ID!=1097 & PDF_Study_ID!=121 & PDF_Study_ID!=453 & PDF_Study_ID!="507d" & PDF_Study_ID!="533v" & PDF_Study_ID!="533p") 

#replace typo of abundance 
RR_by_Hand$Response_Variable<-gsub("Abundace","Abundance", RR_by_Hand$Response_Variable)

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

##### Publication Graphs ####

#### Plot Abundance * Diversity ####

Abund_Div_Setup<-RR_Calc %>% 
  select(PDF_Study_ID,Study_Point,Treatment_Category, ResponseVariable,Data_Type,LnRR) %>% 
  spread(key=Data_Type,value=LnRR, fill=NA) 

SoilNutrients<-Abund_Div_Setup %>% 
  filter(ResponseVariable!="Arthropod"& ResponseVariable!="Bird" & ResponseVariable!="Plant"& ResponseVariable!="SmallMammal")

Abund_Div_Abundance<-Abund_Div_Setup %>% 
  select(-diversity) %>% 
  na.omit(abundance) %>% 
  group_by(ResponseVariable, Treatment_Category) %>%
  summarize(std_ab=sd(abundance,na.rm=TRUE),Mean_ab=mean(abundance,na.rm=TRUE),n_ab=length(abundance)) %>%
  mutate(St_Error_ab=std_ab/sqrt(n_ab)) %>% 
  mutate(margin_ab=qt(0.975,df=n_ab-1)*Mean_ab/sqrt(n_ab)) %>%  
  mutate(lowerinterval_ab=Mean_ab-margin_ab) %>% 
  mutate(upperinterval_ab=Mean_ab+margin_ab) %>% 
  ungroup() 

Abund_Div_Diversity<-Abund_Div_Setup %>% 
  select(-abundance) %>% 
  na.omit(diversity) %>% 
  group_by(ResponseVariable, Treatment_Category) %>%
  summarize(std_div=sd(diversity,na.rm=TRUE),Mean_div=mean(diversity,na.rm=TRUE),n_div=length(diversity)) %>%
  mutate(St_Error_div=std_div/sqrt(n_div)) %>%  
  mutate(margin_div=qt(0.975,df=n_div-1)*Mean_div/sqrt(n_div)) %>% 
  mutate(lowerinterval_div=Mean_div-margin_div) %>% 
  mutate(upperinterval_div=Mean_div+margin_div) %>% 
  ungroup() 

Abund_Div<-Abund_Div_Abundance %>% 
  left_join(Abund_Div_Diversity)

Abund_Div[is.na(Abund_Div)] <- 0

ResponseVariable_Images<-Abund_Div %>% 
  select(ResponseVariable) %>% 
  unique() %>% 
  mutate(image = c("https://pixabay.com/get/g62f737c4685073e92844fa5418e568ca183ae365391db78dac252bde2e2a308572ef2ab000f3393c02854b95d8055616_640.png", #arthropod
                          "https://pixabay.com/get/g8b2b7cdc66d0e5b9298e21cbf371f5b7d7329625d6cb0b2d8c92b9e16f40a99470226f8c87c8de366db923c005af0905_640.png", #bird
                          "https://pixabay.com/get/gc3dbba2a82a3d6ffedcb624d831362f421c16ee7bfbac8e5dd724ff48c4241bd8735a12fb28b1225e80f5e04c132283e_640.png", #plant
                          "https://pixabay.com/get/g0187cf40449977ec9aefa8a1863036a131197d65580f90ad970e50f2cabe238b6759638d4df91dfc13e9dab86bba19f6_640.png", #small Mammal
                          "https://pixabay.com/get/g080314708fe1c54beacf6ca4c1268e315e65876a9ebc8fd0996f6bcaad4cd73997e7cc80093463dd33d15d0b85d274f8.svg",
                          "https://img.icons8.com/external-bearicons-glyph-bearicons/256/external-Nitrogen-periodic-table-bearicons-glyph-bearicons.png"))


Abund_Div_Image<-Abund_Div %>% 
  left_join(ResponseVariable_Images) %>% 
  select(ResponseVariable,Treatment_Category,n_ab, Mean_ab,lowerinterval_ab,upperinterval_ab,n_div,Mean_div, lowerinterval_div, upperinterval_div,image) 

Abund_Div_Image[Abund_Div_Image==0]<-NA

levels(Abund_Div_Image$ResponseVariable) <- c(levels(Abund_Div_Image$ResponseVariable),'') # add blank level

Abund_Div_Image_Biotic<-Abund_Div_Image %>% 
  filter(ResponseVariable!="TotalSoilCarbon"& ResponseVariable!="TotalSoilNitrogen")

Abund_Div_Image_Abiotic<-Abund_Div_Image %>% 
  filter(ResponseVariable!="Arthropod"& ResponseVariable!="Bird" & ResponseVariable!="Plant"& ResponseVariable!="SmallMammal")

#create x axis title for figures
x_title <- expression(paste("Mean Effect Size (Hedges' ", italic("g"), ")"))
x_title_ab <- expression(paste("Mean Effect Size (Hedges' ", italic("g"), ") Abundance"))
x_title_div <- expression(paste("Mean Effect Size (Hedges' ", italic("g"), ") Diversi"))

#### Diversity and Abundance Figure ####

Fire1yr_Abundance<-ggplot(data=subset(Abund_Div_Image_Biotic,Treatment_Category=="1yr"),aes(x=Mean_ab, y=ResponseVariable,shape=ResponseVariable,size=ResponseVariable)) +
  geom_vline(xintercept=0, linetype="dashed",size=2,color="grey25")+
  geom_image(aes(image=image))+
  geom_errorbarh(aes(xmin=lowerinterval_ab,xmax=upperinterval_ab), size = 2, height = .5)+
  scale_shape_manual(values=c(15,16,17,21,1),labels = c("Birds","Arthropods","Plants","Small Mammals",""), breaks = c("Bird","Arthropod","Plant","SmallMammal",""),limits=c('SmallMammal','Plant','Bird','Arthropod',''),drop = FALSE)+
  scale_y_discrete(labels = c("Birds (n=0,0)","Arthropods (n=4,0)","Plants (n=142,71)","Small Mammals (n=11,0)",""), breaks = c("Bird","Arthropod","Plant","SmallMammal",""),limits=c('SmallMammal','Plant','Bird','Arthropod',''),drop = FALSE)+
  scale_size_manual(values=c(0.2,0.2,0.1,0.2,0.2),labels = c("Birds","Arthropods","Plants","Small Mammals",""), breaks = c("Bird","Arthropod","Plant","SmallMammal",""),limits=c('SmallMammal','Plant','Bird','Arthropod',''),drop = FALSE)+
  xlab(x_title_ab)+
  ylab("Response Variable")+
  xlim(-5,5)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position="none")+
  annotate("text", x=-2.8, y=5, label = "A. Annual Fire", size=20)

Fire2_4yr_Abundance<-ggplot(data=subset(Abund_Div_Image_Biotic,Treatment_Category=="2-4yr"),aes(x=Mean_ab, y=ResponseVariable,shape=ResponseVariable,size=ResponseVariable)) +
  geom_vline(xintercept=0, linetype="dashed",size=2,color="grey25")+
  geom_image(aes(image=image))+
  geom_errorbarh(aes(xmin=lowerinterval_ab,xmax=upperinterval_ab), size = 2, height = .5)+
  scale_shape_manual(values=c(15,16,17,21,1),labels = c("Birds","Arthropods","Plants","Small Mammals",""), breaks = c("Bird","Arthropod","Plant","SmallMammal",""),limits=c('SmallMammal','Plant','Bird','Arthropod',''),drop = FALSE)+
  scale_y_discrete(labels = c("Birds (n=1,3)","Arthropods (n=30,2)","Plants (n=144,26)","Small Mammals (n=2,0)",""), breaks = c("Bird","Arthropod","Plant","SmallMammal",""),limits=c('SmallMammal','Plant','Bird','Arthropod',''),drop = FALSE)+
  scale_size_manual(values=c(0.2,0.2,0.1,0.2,0.2),labels = c("Birds","Arthropods","Plants","Small Mammals",""), breaks = c("Bird","Arthropod","Plant","SmallMammal",""),limits=c('SmallMammal','Plant','Bird','Arthropod',''),drop = FALSE)+
  xlab(x_title_ab)+
  ylab("Response Variable")+
  xlim(-10,10)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position="none")+
  annotate("text", x=-2.2, y=5, label = "C. 2-4 Year Fire Regime", size=20)


FireGrazing_Abundance<-ggplot(data=subset(Abund_Div_Image_Biotic,Treatment_Category=="fire + grazing"),aes(x=Mean_ab, y=ResponseVariable,shape=ResponseVariable,size=ResponseVariable)) +
  geom_vline(xintercept=0, linetype="dashed",size=2,color="grey25")+
  geom_image(aes(image=image))+
  geom_errorbarh(aes(xmin=lowerinterval_ab,xmax=upperinterval_ab), size = 2, height = .5)+
  scale_shape_manual(values=c(15,16,17,21,1),labels = c("Birds","Arthropods","Plants","Small Mammals",""), breaks = c("Bird","Arthropod","Plant","SmallMammal",""),limits=c('SmallMammal','Plant','Bird','Arthropod',''),drop = FALSE)+
  scale_y_discrete(labels = c("Birds (n=22,0)","Arthropods (n=28,4)","Plants (n=50,44)","Small Mammals (n=0,0)",""), breaks = c("Bird","Arthropod","Plant","SmallMammal",""),limits=c('SmallMammal','Plant','Bird','Arthropod',''),drop = FALSE)+
  scale_size_manual(values=c(0.2,0.2,0.1,0.2,0.2),labels = c("Birds","Arthropods","Plants","Small Mammals",""), breaks = c("Bird","Arthropod","Plant","SmallMammal",""),limits=c('SmallMammal','Plant','Bird','Arthropod',''),drop = FALSE)+
  xlab(x_title_ab)+
  ylab("Response Variable")+
  xlim(-1,1)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position="none")+
  annotate("text", x=-0.4, y=5, label = "E. Fire with Grazing", size=20)

Fire1yr_Diversity<-ggplot(data=subset(Abund_Div_Image_Biotic,Treatment_Category=="1yr"),aes(x=Mean_div, y=ResponseVariable,shape=ResponseVariable,size=ResponseVariable)) +
  geom_vline(xintercept=0, linetype="dashed",size=2,color="grey25")+
  geom_image(aes(image=image))+
  geom_errorbarh(aes(xmin=lowerinterval_div,xmax=upperinterval_div), size = 2, height = .5)+
  scale_shape_manual(values=c(15,16,17,21,1),labels = c("Birds","Arthropods","Plants","Small Mammals",""), breaks = c("Bird","Arthropod","Plant","SmallMammal",""),limits=c('SmallMammal','Plant','Bird','Arthropod',''),drop = FALSE)+
  scale_y_discrete(labels = c("Birds","Arthropods","Plants","Small Mammals",""), breaks = c("Bird","Arthropod","Plant","SmallMammal",""),limits=c('SmallMammal','Plant','Bird','Arthropod',''),drop = FALSE)+
  scale_size_manual(values=c(0.2,0.2,0.1,0.2,0.2),labels = c("Birds","Arthropods","Plants","Small Mammals",""), breaks = c("Bird","Arthropod","Plant","SmallMammal",""),limits=c('SmallMammal','Plant','Bird','Arthropod',''),drop = FALSE)+
  xlab(x_title_div)+
  ylab("Response Variable")+
  xlim(-40,40)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position="none")+
  annotate("text", x=-22, y=5, label = "B. Annual Fire", size=20)

Fire2_4yr_Diversity<-ggplot(data=subset(Abund_Div_Image_Biotic,Treatment_Category=="2-4yr"),aes(x=Mean_div, y=ResponseVariable,shape=ResponseVariable, size=ResponseVariable)) +
  geom_vline(xintercept=0, linetype="dashed",size=2, color="grey25")+
  geom_image(aes(image=image))+
  geom_errorbarh(aes(xmin=lowerinterval_div,xmax=upperinterval_div), size = 2, height = .5)+
  scale_shape_manual(values=c(15,16,17,21,1),labels = c("Birds","Arthropods","Plants","Small Mammals",""), breaks = c("Bird","Arthropod","Plant","SmallMammal",""),limits=c('SmallMammal','Plant','Bird','Arthropod',''),drop = FALSE)+
  scale_y_discrete(labels = c("Birds","Arthropods","Plants","Small Mammals",""), breaks = c("Bird","Arthropod","Plant","SmallMammal",""),limits=c('SmallMammal','Plant','Bird','Arthropod',''),drop = FALSE)+
  scale_size_manual(values=c(0.2,0.2,0.1,0.2,0.2),labels = c("Birds","Arthropods","Plants","Small Mammals",""), breaks = c("Bird","Arthropod","Plant","SmallMammal",""),limits=c('SmallMammal','Plant','Bird','Arthropod',''),drop = FALSE)+
  xlab(x_title_div)+
  ylab("Response Variable")+
  xlim(-4,4)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position="none")+
  annotate("text", x=-0.9, y=5, label = "D. 2-4 Year Fire Regime", size=20)


FireGrazing_Diversity<-ggplot(data=subset(Abund_Div_Image_Biotic,Treatment_Category=="fire + grazing"),aes(x=Mean_div, y=ResponseVariable,shape=ResponseVariable,size=ResponseVariable)) +
  geom_vline(xintercept=0, linetype="dashed",size=2,color="grey25")+
  geom_image(aes(image=image))+
  geom_errorbarh(aes(xmin=lowerinterval_div,xmax=upperinterval_div), size = 2, height = .5)+
  scale_shape_manual(values=c(15,16,17,21,1),labels = c("Birds","Arthropods","Plants","Small Mammals",""), breaks = c("Bird","Arthropod","Plant","SmallMammal",""),limits=c('SmallMammal','Plant','Bird','Arthropod',''),drop = FALSE)+
  scale_y_discrete(labels = c("Birds","Arthropods","Plants","Small Mammals",""), breaks = c("Bird","Arthropod","Plant","SmallMammal",""),limits=c('SmallMammal','Plant','Bird','Arthropod',''),drop = FALSE)+
  scale_size_manual(values=c(0.2,0.2,0.1,0.2,0.2),labels = c("Birds","Arthropods","Plants","Small Mammals",""), breaks = c("Bird","Arthropod","Plant","SmallMammal",""),limits=c('SmallMammal','Plant','Bird','Arthropod',''),drop = FALSE)+
  xlab(x_title_div)+
  ylab("Response Variable")+
  xlim(-5,5)+
  theme(axis.text.y=element_blank(),axis.text.x=element_text(size=55),axis.title.y=element_blank(),axis.title.x=element_text(size=55),legend.position="none")+
  annotate("text", x=-1.9, y=5, label = "F. Fire with Grazing", size=20)


Fire1yr_Abundance+
  Fire1yr_Diversity+
  Fire2_4yr_Abundance+
  Fire2_4yr_Diversity+
  FireGrazing_Abundance+
  FireGrazing_Diversity+ 
  plot_layout(ncol = 2,nrow = 3)#save at 3000 x 3000


#### Soil Nutrients Figure ####

ggplot(data=SoilNutrients, aes(y=Treatment_Category,x=abundance,color=ResponseVariable)) +
  geom_vline(xintercept=0, linetype="dashed",color="grey25", size=2)+
  geom_boxplot(lwd=2,position=position_dodge(1))+
  scale_color_manual(values=c("darkslategray","darkslategray4"),labels = c("Total Soil Carbon","Total Soil Nitrogen"), breaks = c("TotalSoilCarbon","TotalSoilNitrogen"),limits=c("TotalSoilNitrogen","TotalSoilCarbon"),name="Soil Nutrients")+
  scale_y_discrete(labels = c("Annual Fire","2-4 Year Fire","Fire with Grazing"), breaks = c("1yr","2-4yr","fire + grazing"),limits=c("fire + grazing","2-4yr","1yr"))+
  xlab(x_title)+
  ylab("Fire Return Interval")+
  xlim(-4,4)+
  theme(axis.text.y=element_text(size=55),axis.text.x=element_text(size=55),axis.title.y=element_text(size=55),axis.title.x=element_text(size=55),legend.text=element_text(size=40),legend.title=element_blank(),legend.position = c(0.2,0.15),legend.key = element_rect(size=30), legend.key.size = unit(7.0, 'lines'))
#save at 2500x1500
  


