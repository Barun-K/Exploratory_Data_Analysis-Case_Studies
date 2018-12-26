install.packages("tidyr")
library(tidyr)
library(dplyr)
library(ggplot2)
q1_d <- read.csv("D://DSML-M/SEM1/EDA/CSV Files/rajanand-crime-in-india/39_Specific_purpose_of_kidnapping_and_abduction.csv")
View(q1_d)
str(q1)
q1_d$K_A_Cases_Reported = as.numeric(as.character(q1_d$K_A_Cases_Reported,na.rm=TRUE))
names(q1)
 
q1 <- q1_d %>% select(AreaName = ï..Area_Name,Sub_Group_Name,K_A_Cases_Reported) %>%
  filter(Sub_Group_Name != "14. Total (Sum of 1-13 Above)") %>%
  group_by(AreaName,Sub_Group_Name) %>% summarise(Count_A = sum(K_A_Cases_Reported)) %>%
  top_n(3,Count_A)
View(q1)

ggplot(q1,aes(x=Sub_Group_Name,y=Count_A)) + 
  geom_bar(stat = "Identity",aes(fill = Sub_Group_Name)) +
  facet_wrap('AreaName')

#----------------------------------------------------------------------------------

q2_d <- read.csv("D://DSML-M/SEM1/EDA/CSV Files/rajanand-crime-in-india/21_Offenders_known_to_the_victim.csv")

View(q2_d)
str(q2_d)
names(q2_d)

q2_d_long <- gather(q2_d,Victims_From,CountN,No_of_Cases_in_which_offenders_were_known_to_the_Victims:No_of_Cases_in_which_offenders_were_Relatives)
View(q2_d_long)

q2 <- q2_d_long %>% select(Victims_From,CountN) %>% 
  group_by(Victims_From) %>% summarise(Total = sum(CountN))

View(q2)

ggplot(q2,aes(x=Victims_From,y=Total)) +
  geom_bar(stat="Identity",aes(fill=Victims_From))

#--------------------------------------------------------------------

q3_dfa <- read.csv("D://DSML-M/SEM1/EDA/CSV Files/rajanand-crime-in-india/18_03_Juveniles_arrested_Family_background.csv")
q3_ded <- read.csv("D://DSML-M/SEM1/EDA/CSV Files/rajanand-crime-in-india/18_01_Juveniles_arrested_Education.csv")
q3_dec <- read.csv("D://DSML-M/SEM1/EDA/CSV Files/rajanand-crime-in-india/18_02_Juveniles_arrested_Economic_setup.csv")
View(q3_dfalong)
q3_dfalong <- gather(q3_dfa,FamBGround,TotalFa,Family_back_ground_Homeless:Family_back_ground_Total)
q3_dedlong <- gather(q3_ded,EduLev,TotalEd,Education_Above_Primary_but_below_Matric_or_Higher_Secondary:Education_Upto_primary)
q3_declong <- gather(q3_dec,EcSetup,TotalEc,Economic_Set_up_Annual_Income_250001_to_50000:Economic_Set_up_Upper_middle_income_from_200001_to_300000)

q3_fa <- q3_dfalong %>% select(FamBGround,TotalFa) %>% 
  group_by(FamBGround) %>% summarise(CountFa=sum(TotalFa))
View(q3_fa)
q3_ed <- q3_dedlong %>% select(EduLev,TotalEd) %>% 
  group_by(EduLev) %>% summarise(CountEd=sum(TotalEd))
View(q3_ed)
q3_ec <- q3_declong %>% select(EcSetup,TotalEc) %>%
  group_by(EcSetup) %>% summarise(CountEc = sum(TotalEc))
  

ggplot(q3_fa,aes(x=FamBGround,y=CountFa)) +
  geom_bar(stat="Identity",aes(fill=FamBGround))

ggplot(q3_ed,aes(x=EduLev,y=CountEd)) +
  geom_bar(stat="Identity",aes(fill=EduLev))

ggplot(q3_ec,aes(x=EcSetup,y=CountEc)) +
  geom_bar(stat="Identity",aes(fill=EcSetup))

#------------------------------------------------------------
