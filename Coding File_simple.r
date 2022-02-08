################################################################################
########### INTERMEDIATE ECONOMETRICS PROJECT 2021 #############################
       # # # ********************************** # # #

############################ TOPIC: ############################################
## Women only: Wage gap between women that are married or cohabitation and others
################################################################################

###################### CALL THE PACKAGES #######################################
################################################################################

install.packages("RCurl") 
library(RCurl) 
library(stargazer) 
library(ggplot2) 
library(dplyr)

###################### IMPORT CPS2019 DATASET ##################################
################################################################################

x <- getURL("https://raw.githubusercontent.com/hdlinhnguyen/IE-Project-2021-data/main/IE%20Project%202021.csv")
CPS <- read.csv(text = x) # # # it can take you 15 seconds to load getURL ;)
View(CPS)

### Codebook: https://github.com/hdlinhnguyen/IE-Project-2021-data/blob/8e3b9d76df2d803b35d0e6476dc59b13e3cdfe53/CPS%20Codebook_IE%20Project%202021.pdf

############################# DATA PREPARATION #################################
###################### LEVELS OF CATEGORICAL VARIABLES #########################
####################### for OCC2010 and IND1990 ################################

# A categorical variable: OCCUPATION and its levels 
# 1 = Chief or Manager or Director in MANAGEMENT, BUSINESS, SCIENCE, AND ARTS
CPS$OCC2010[CPS$OCC2010<= "430"] = 1
# 2 = BUSINESS OPERATIONS SPECIALISTS
CPS$OCC2010[CPS$OCC2010> "430"& CPS$OCC2010<"800"] = 2
# 3 = FINANCIAL SPECIALISTS
CPS$OCC2010[CPS$OCC2010> "730"& CPS$OCC2010<"1000"] = 3
# 4 =       COMPUTER AND MATHEMATICAL
CPS$OCC2010[CPS$OCC2010> "950"& CPS$OCC2010<"1300"] = 4       
# 5 =        ARCHITECTURE AND ENGINEERING           
CPS$OCC2010[CPS$OCC2010> "1240"& CPS$OCC2010<"1550"] = 5   
# 6 =       TECHNICIANS
CPS$OCC2010[CPS$OCC2010 > "1530"& CPS$OCC2010<"1600"] = 6   
#7  =       LIFE, PHYSICAL, AND SOCIAL SCIENCE$
CPS$OCC2010[CPS$OCC2010 > "1560"& CPS$OCC2010 <"2000"] = 7   
#8  =   COMMUNITY AND SOCIAL SERVICES
CPS$OCC2010[CPS$OCC2010 > "1980"& CPS$OCC2010 <"2100"] = 8
# 9 =   LEGAL
CPS$OCC2010[CPS$OCC2010 > "2060"& CPS$OCC2010 <"2200"] = 9
#10 =        EDUCATION, TRAINING, AND LIBRARY
CPS$OCC2010[CPS$OCC2010 > "2150"& CPS$OCC2010<"2600"] = 10
#11 =        ARTS, DESIGN, ENTERTAINMENT, SPORTS, AND MEDIA
CPS$OCC2010[CPS$OCC2010> "2550"& CPS$OCC2010<"3000"] = 11
# 12 =  HEALTHCARE PRACTITIONERS AND TECHNICAL
CPS$OCC2010[CPS$OCC2010> "2920"& CPS$OCC2010<"3600"] = 12
# 13 =  HEALTHCARE SUPPORT
CPS$OCC2010[CPS$OCC2010 > "3540"& CPS$OCC2010<"3700"] = 13
# 14 = PROTECTIVE SERVICE
CPS$OCC2010[CPS$OCC2010 > "3650"& CPS$OCC2010<"4000"] = 14
# 15 = FOOD PREPARATION AND SERVING
CPS$OCC2010[CPS$OCC2010 > "3950"& CPS$OCC2010<"4200"] = 15
# 16 = BUILDING AND GROUNDS CLEANING AND MAINTENANCE
CPS$OCC2010[CPS$OCC2010 > "4150" & CPS$OCC2010<"4300"] = 16
# 17 = PERSONAL CARE AND SERVICE
CPS$OCC2010[CPS$OCC2010> "4250" & CPS$OCC2010 <"4700"] = 17
# 18 = SALES AND RELATED
CPS$OCC2010[CPS$OCC2010> "4650"& CPS$OCC2010<"5000"] = 18
# 19 = OFFICE AND ADMINISTRATIVE SUPPORT
CPS$OCC2010[CPS$OCC2010 > "4965"& CPS$OCC2010<"6005"] = 19
# 20 = FARMING, FISHING, AND FORESTRY
CPS$OCC2010[CPS$OCC2010> "5940"& CPS$OCC2010<"6200"] = 20
# 21 =  CONSTRUCTION
CPS$OCC2010[CPS$OCC2010> "6130"& CPS$OCC2010<"6800"] = 21
# 22 =         EXTRACTION
CPS$OCC2010[CPS$OCC2010 > "6765" & CPS$OCC2010 <"7000"] = 22
# 23 = INSTALLATION, MAINTENANCE, AND REPAIR
CPS$OCC2010[CPS$OCC2010 > "6940"& CPS$OCC2010<"7700"] = 23
# 24 = production
CPS$OCC2010[CPS$OCC2010> "7630" & CPS$OCC2010<"9000"] = 24
# 25 =       TRANSPORTATION AND MATERIAL MOVING
CPS$OCC2010[CPS$OCC2010> "8965"& CPS$OCC2010<"9800"] = 25
# 26 = MILITARY SPECIFIC
CPS$OCC2010[CPS$OCC2010 > "9750"& CPS$OCC2010<"9840"] = 26

CPS$OCC2010 <- factor(CPS$OCC2010)
# To checck if the transformation into factor worked 
is.factor(CPS$OCC2010)
levels(CPS$OCC2010) 

# A Categorical variable : INDUSTRY and its levels 
# 1 = AGRICULTURE, FORESTRY, AND FISHERIES
CPS$IND1990[CPS$IND1990>"0" &CPS$IND1990<"40"]= 1
# 2 = MINING
CPS$IND1990[CPS$IND1990>"32"&CPS$IND1990<"60"]= 2
# 3 = CONSTRUCTION
CPS$IND1990[CPS$IND1990 =="60"]= 3
# 4 = MANUFACTURING
CPS$IND1990[CPS$IND1990>"60"&CPS$IND1990<"400"]= 4
# 5 = TRANSPORTATION, COMMUNICATIONS, AND OTHER PUBLIC UTILITIES
CPS$IND1990[CPS$IND1990>"392"&CPS$IND1990<"500"]= 5
# 6 =        WHOLESALE TRADE
CPS$IND1990[CPS$IND1990>"472"&CPS$IND1990<"580"]= 6
# 7 = RETAIL TRADE
CPS$IND1990[CPS$IND1990>"571"&CPS$IND1990<"700"]= 7
# 8 = FINANCE, INSURANCE, AND REAL ESTATE
CPS$IND1990[CPS$IND1990>"691"&CPS$IND1990<"721"]= 8
#9  =  BUSINESS AND REPAIR SERVICES
CPS$IND1990[CPS$IND1990>"712"&CPS$IND1990<"761"]= 9
#10 = PERSONAL SERVICES
CPS$IND1990[CPS$IND1990>"760"&CPS$IND1990<"800"]= 10
#11 = ENTERTAINMENT AND RECREATION SERVICES
CPS$IND1990[CPS$IND1990>"791"&CPS$IND1990<"812"]= 11
#12 = PROFESSIONAL AND RELATED SERVICES
CPS$IND1990[CPS$IND1990>"810"&CPS$IND1990<"900"]= 12
#13 = PUBLIC ADMINISTRATION
CPS$IND1990[CPS$IND1990>"893"&CPS$IND1990<"940"]= 13
#14 = ACTIVE DUTY MILITARY
CPS$IND1990[CPS$IND1990>"932"&CPS$IND1990<"970"]= 14

CPS$IND1990 <- factor(CPS$IND1990)
# To checck if the transformation into factor worked 
is.factor(CPS$IND1990)
levels(CPS$IND1990)


############################# DATA CLEANING ####################################
############## and CREATION OF A SUBSET FOR ONLY WOMEN WORKING FULLTIME ########
################################################################################
# Data cleaning, we only keep WOMEN working full time, full necessary information
OWO <- subset(CPS, CPS$SEX=="2" & CPS$WKSTAT=="11" & CPS$INCWAGE != "99999999" 
     & CPS$INCWAGE != "0" & CPS$UHRSWORKLY != "999" & CPS$OCC2010 != "9999"
     & CPS$IND1990 != "998" & CPS$EDUC99 != "00") 

# OWO IS ONLY WOMEN, after cleaning we have 25914 observations

# WKSTAT = 11	Full-time hours (35+), usually full-time

###################### CREATION OF THE DEPENDENT VARIABLE ######################
################################################################################
### First step: Calculation of the Wage per hour
OWO$Wageperhour <- (OWO$INCWAGE/(OWO$WKSWORK1*OWO$UHRSWORKLY)) 
# Wage per hour = The total yearly wage / The total number of hours usually worked 
hist(OWO$Wageperhour) # to see Wageperhour distribution is not normal

### Second step: Take log of Wage per hour = log(Wage per hour)
OWO$logWageperhour = log(OWO$Wageperhour)  # Here is the DEPENDENT VARIABLE we use
hist(OWO$logWageperhour) # to see log(Wageperhour) is almost normal distribution


###################### CREATION OF THE DUMMY VARIABLES: ########################
####################### Married and White (RACE) ###############################
### Dummy variable created to characterize 2 groups: married or living as a couple, and others
OWO$Married <- (OWO$MARST =="1" | OWO$PECOHAB=="1")
OWO$marriedncohab = (OWO$Married=="TRUE") 

table(OWO$marriedncohab) ### to see the number of Married or Cohabitation people and others 

### Dummy variable created to differ RACE: White and Others  
OWO$White <- (OWO$RACE =="100")
OWO$White = (OWO$White=="TRUE") 

table(OWO$White) # # to see the number of White people and Others

################################################################################
################### TABLE 1: SUMMARY STATISTICS ################################
################################################################################

stargazer(OWO) # please read the Table 1 in pdf file

##########DISTRIBUTION OF OCCUPATION, INDUSTRY AND EDUCATION ###################
################################################################################

################################ OCCUPATION ####################################

table(OWO$OCC2010) # gives the number of observations for each level 

### Bar chart for Occupation distribution
x1 = c(14358,9562,1297,697)
labels1 = c("MANAGER or DIRECTOR","EXTRATION WORKER","PRODUCTION WORKER",
            "TRANPORTATION WORKER")

df1 = data.frame(x1 = c(14358,9562,1297,697), labels1 = c("MANAGER OR DIRECTOR",
                                                          "EXTRATION WORKER","PRODUCTION WORKER","TRANPORTATION WORKER")) 

ggplot(df1, aes(x=labels1, weight =  x1, fill= labels1)) +  geom_bar(width = 1, position = "stack") + 
  theme(plot.background=element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title =element_blank(), axis.line = element_line(colour = "grey"),
        legend.key.size = unit(1, 'cm'),
        legend.text = element_text(size=12))

################################## INDUSTRY ###################################

table(OWO$IND1990) # gives the number of observations for each level
### Bar chart for Industry distribution
x2 = c(2275,1979,415,6258,359,13018,1610)
labels2 = c("AGRICULTURE, FORESTRY, AND FISHERIES","MINING","CONSTRUCTION",
            "PERSONAL SERVICES","ENTERTAINMENT AND RECREATION SERVICES",
            "PROFESSIONAL AND RELATED SERVICES",
            "PUBLIC ADMINISTRATION")


df2 = data.frame(x2 = c(2275,1979,415,6258,359,13018,1610), labels2 = c("AGRICULTURE, FORESTRY, AND FISHERIES",
                                                                        "MINING","CONSTRUCTION",
                                                                        "PERSONAL SERVICES",
                                                                        "ENTERTAINMENT AND RECREATION SERVICES",
                                                                        "PROFESSIONAL AND RELATED SERVICES",
                                                                        "PUBLIC ADMINISTRATION")) 

ggplot(df2, aes(x=labels2, weight =  x2, fill= labels2)) +  geom_bar(width = 1, position = "stack") + 
  theme(plot.background=element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title =element_blank(), axis.line = element_line(colour = "grey"),
        legend.key.size = unit(1, 'cm'),
        legend.text = element_text(size=9))


######################### PIE FOR EDUCATION  ##########################
table(OWO$EDUC99)  # gives the number of observations for each group of education 
### Pie chart for Industry distribution
labels3 = c("LESS THAN HIGH SCHOOL EDUCATION 5%","HIGH SCHOOL GRADUATE 38%","ASSOCIATE DEGREE 12%",
            "BACHELOR'S DEGREE 28%","MASTER'S DEGREE 14%","PROFESSIIONAL DEGREE 2%","DOCTORATE DEGREE 2%") 
x3 = c(1254,9840,3125,7129,3537,417,612) # we create new groups and count observations thanks to table(.)

DF3 = data.frame(x3 = c(1254,9840,3125,7129,3537,417,612), labels3 = c("LESS THAN HIGH SCHOOL EDUCATION 5%","HIGH SCHOOL GRADUATE 38%","ASSOCIATE DEGREE 12%",
                                                                       "BACHELOR'S DEGREE 28%","MASTER'S DEGREE 14%","PROFESSIIONAL DEGREE 2%","DOCTORATE DEGREE 2%"))
ggplot(DF3, aes(x = 1, weight =  x3, fill = labels3)) +
  geom_bar(width = 1, position = "stack") +
  coord_polar(theta = "y") +
  geom_text(aes(x = 1.3, y = "" ,label = "")) + 
  theme(panel.background = element_blank(), axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.title =element_blank())


################################################################################
################################# TABLE 2 ######################################
################################################################################

########################### REGRESSION FOR TABLE 2 ############################# 
################################################################################


# 1. Primary model 1: Log(wage)_i = \beta_0 + \beta_1*married_i + \u_i
model1 <- lm(logWageperhour ~ marriedncohab, data=OWO)

# HERE IS THE SAME REGRESSION WHICH INCLUDES PROGRESSIVELY ADDITIONAL
# CONTROL VARIABLES: AGE, EDU, RACE, OCC, IND, NUMBER OF CHILDREN UNDER 5 YEARS OLD
################################################################################

# 2. Model 2: Estimate the effect of AGE
model2 <- lm(logWageperhour ~ marriedncohab + AGE, data=OWO)

# 3. Model 3: Estimate the effect of AGE, EDU
model3 <- lm(logWageperhour ~ marriedncohab + AGE + EDUC99, data=OWO)

# 4. Model 4: Estimate the effect of AGE, EDU, RACE
model4 <- lm(logWageperhour ~ marriedncohab + EDUC99 + AGE + White, data=OWO)

# 5. Model 5: Estimate the effect of AGE, EDU, RACE, OCC
# The reference level for OCC is level 1 = MANAGEMENT, BUSINESS, SCIENCE, AND ARTS
model5 <- lm(logWageperhour ~ marriedncohab + EDUC99 + AGE + White + OCC2010, data=OWO)

# 6. Model 6: Estimate the effect of AGE, EDU, RACE, OCC, IND

# The reference level for IND is level 1 = AGRICULTURE, FORESTRY, AND FISHERIES
model6 <- lm(logWageperhour ~ marriedncohab + EDUC99 + AGE + White + OCC2010 
                                                    + IND1990, data=OWO)

# 7. Model 7: Estimate the effect of AGE, EDU, RACE, OCC, IND, NUMBER OF CHILDREN UNDER 5 YEARS OLD
model7 <- lm(logWageperhour ~ marriedncohab + EDUC99 + AGE  + White + OCC2010 
                                                   + IND1990 + NCHLT5, data=OWO)

stargazer(model1, model2, model3, model4, model5, model6, model7,
          align = TRUE)


#########################################################################################
#### 3.ESTIMATING SEPARATELY FOR THOSE WITH LESS AND MORE THAN HIGH_SCHOOL EDUCATION ####
#########################################################################################

############################# Table 3.1 LESS THAN  HIGHSCHOOL EDUCATION  ########################
#################################################################################################

less_highschool <- subset(OWO, OWO$EDUC99 <= "10")

# 1. the primary model for those who have less_highschool degree 
reg1 = lm(formula = logWageperhour ~ marriedncohab, data = less_highschool )

# 2. Estimate the effect of age 
reg2 = lm(formula = logWageperhour ~ marriedncohab + AGE, data = less_highschool)

# 3. Estimate the effect of  age, euctation 
reg3 = lm(formula = logWageperhour ~ marriedncohab + AGE + EDUC99, data = less_highschool)

# 4. Estimate the effect of age, education,  occupation,
reg4 = lm(formula = logWageperhour ~ marriedncohab + AGE + EDUC99 + OCC2010, data = less_highschool)

# 5 Estimate the effect of age, education, occupation, industry 
reg5 = lm(formula = logWageperhour ~ marriedncohab + AGE + EDUC99 + OCC2010+ IND1990, data = less_highschool)
summary(reg5)

# 6 Estimate the effect of age, education, occupation, industry, Race  
reg6 = lm(formula = logWageperhour ~ marriedncohab + AGE + EDUC99 + OCC2010+ IND1990 + White, data = less_highschool)

#7 Estimate the effect of age, education, occupation, industry, Race, Children 
reg7 = lm(formula = logWageperhour ~ marriedncohab + AGE + EDUC99 + OCC2010+ IND1990 + White + NCHLT5, data = less_highschool)


stargazer(reg1, reg2, reg3, reg4, reg5, reg6, reg7,
          align = TRUE)


############################# Table 3.2  MORE THAN HIGHSCHOOL EDUCATION ######################
##############################################################################################

# We do the same thing for more_highschool people
more_highschool <- subset(OWO, OWO$EDUC99 > "10")

# 1. the primary model for those who have  have more than high school education
reg11 = lm(formula = logWageperhour ~ marriedncohab, data = more_highschool )

# 2. Estimate the effect of age 
reg22 = lm(formula = logWageperhour ~ marriedncohab + AGE, data = more_highschool)

# 3. Estimate the effect of  age, euctation 
reg33 = lm(formula = logWageperhour ~ marriedncohab + AGE + EDUC99, data = more_highschool)

# 4. Estimate the effect of age, education,  occupation,
reg44 = lm(formula = logWageperhour ~ marriedncohab + AGE + EDUC99 + OCC2010, data = more_highschool)

# 5 Estimate the effect of age, education, occupation, industry 
reg55 = lm(formula = logWageperhour ~ marriedncohab + AGE + EDUC99 + OCC2010 
                                            + IND1990, data = more_highschool)

# 6 Estimate the effect of age, education, occupation, industry, Race  
reg66 = lm(formula = logWageperhour ~ marriedncohab + AGE + EDUC99 + OCC2010 
                                      + IND1990 + White, data = more_highschool)

#7 Estimate the effect of age, education, occupation, industry, Race, Children 
reg77 = lm(formula = logWageperhour ~ marriedncohab + AGE + EDUC99 + OCC2010 
                           + IND1990 + White + NCHLT5, data = more_highschool)

stargazer(reg11, reg22, reg33, reg44, reg55, reg66, reg77,
          align = TRUE)

################################################################################
################################# TABLE 4 ######################################
################################################################################

#Idea: We do the same regressions as in Table 2 for Women, but here for Men
# So we estimate the wage gap between married men or men living in a couple and others 

OM <- subset(CPS, CPS$SEX=="1" & CPS$WKSTAT=="11" & CPS$INCWAGE != "99999999" 
              & CPS$INCWAGE != "0" & CPS$UHRSWORKLY != "999" & CPS$OCC2010 != "9999"
              & CPS$IND1990 != "998" & CPS$EDUC99 != "00") 

# OM is ONLY MEN

###################### CREATION OF THE DUMMY VARIABLE: ##############################
####################### Married and White (RACE) ####################################
### Variable created to characterize 2 groups MARRIED(=1) OR UNMARRIED(=0) for regressions
OM$Married <- (OM$MARST =="1" | OM$PECOHAB=="1")
OM$marriedncohab = (OM$Married=="TRUE") 

### The following variable (White) turns RACE into a dummy variable : White (=TRUE) and Others(=FALSE)  
OM$White <- (OM$RACE =="100")
OM$White = (OM$White=="TRUE") 
View(OM)

###################### CREATION OF THE DEPENDENT VARIABLE ###########################
#####################################################################################
### Calculation of the Wage per hour
OM$Wageperhour <- (OM$INCWAGE/(OM$WKSWORK1*OM$UHRSWORKLY)) 

# Wage per hour = The total yearly wage / The total number of hours usually worked 
### Then we take the log of Wage per hour = log(Wage per hour)
OM$logWageperhour = log(OM$Wageperhour)    # Here is the DEPENDENT VARIABLE we use


########################### REGRESSION FOR TABLE 4 ############################# 
################################################################################

# 1. Primary model 1: Log(wage)_i = \beta_0 + \beta_1*married_i + \u_i
model1 <- lm(logWageperhour ~ marriedncohab, data=OM)

# HERE IS THE SAME REGRESSION WHICH INCLUDE PROGRESSIVELY ADDITIONAL
# CONTROL VARIABLES: AGE, EDU, RACE, OCC, IND, NUMBER OF CHILDREN UNDER 5 YEARS OLD

# 2. Model 2: Estimate the effect of AGE
model2 <- lm(logWageperhour ~ marriedncohab + AGE, data=OM)

# 3. Model 3: Estimate the effect of AGE, EDU
model3 <- lm(logWageperhour ~ marriedncohab + AGE + EDUC99, data=OM)

# 4. Model 4: Estimate the effect of AGE, EDU, RACE
model4 <- lm(logWageperhour ~ marriedncohab + EDUC99 + AGE + White, data=OM)

# 5. Model 5: Estimate the effect of AGE, EDU, RACE, OCC
model5 <- lm(logWageperhour ~ marriedncohab + EDUC99 + AGE + White + OCC2010, data=OM)

# 6. Model 6: Estimate the effect of AGE, EDU, RACE, OCC, IND
model6 <- lm(logWageperhour ~ marriedncohab + EDUC99 + AGE + White + OCC2010 
                                                             + IND1990, data=OM)

# 7. Model 7: Estimate the effect of AGE, EDU, RACE, OCC, IND, NUMBER OF CHILDREN UNDER 5 YEARS OLD
model7 <- lm(logWageperhour ~ marriedncohab + EDUC99 + AGE  + White + OCC2010 
                                                    + IND1990 + NCHLT5, data=OM)

stargazer(model1, model2, model3, model4, model5, model6, model7,
          align = TRUE)


################################################################################
########################### THE END ############################################
################################################################################

