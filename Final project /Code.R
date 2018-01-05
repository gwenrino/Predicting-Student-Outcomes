library(tidyverse)

# Read in data
d_math <- read.csv2("~/Documents/Data_Science_Learning/Student_Outcomes/Data/student-mat.csv")
d_port <- read.csv2("~/Documents/Data_Science_Learning/Student_Outcomes/Data/student-por.csv")

## DATA WRANGLING

# How many students are enrolled in both math and Portuguese? 
# Merge based on attributes not associated with course.
d_both <- merge(d_math, d_port, by=c("school", "sex", "age", "address", 
                "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob",
                "reason", "guardian", "traveltime", "activities", "nursery",
                "higher", "internet", "romantic", "famrel", "freetime", 
                "goout", "Dalc", "Walc", "health", "absences"))
print(nrow(d_both)) # There are 100 students enrolled in both courses

# Tidy up variable types, labels of variables
d_math$studytime <- factor(d_math$studytime, labels = c("<2 hrs", "2-5 hrs", "5-10 hrs", ">10 hrs"))
d_port$studytime <- factor(d_port$studytime, labels = c("<2 hrs", "2-5 hrs", "5-10 hrs", ">10 hrs"))
d_math$failures <- factor(d_math$failures, labels = c("0", "1", "2", "3"))
d_port$failures <- factor(d_port$failures, labels = c("0", "1", "2", "3"))
d_math$famsize <- factor(d_math$famsize, labels = c(">3", "<=3"))
d_port$famsize <- factor(d_port$famsize, labels = c(">3", "<=3"))
d_math$Medu <- factor(d_math$Medu, labels = c("None", "Primary", "Middle", "Secondary", "Higher"))
d_port$Medu <- factor(d_port$Medu, labels = c("None", "Primary", "Middle", "Secondary", "Higher"))
d_math$Fedu <- factor(d_math$Fedu, labels = c("None", "Primary", "Middle", "Secondary", "Higher"))
d_port$Fedu <- factor(d_port$Fedu, labels = c("None", "Primary", "Middle", "Secondary", "Higher"))
d_math$traveltime <- factor(d_math$traveltime, labels = c("<15 min", "15-30 min", "30-60 min", ">60 min"))
d_port$traveltime <- factor(d_port$traveltime, labels = c("<15 min", "15-30 min", "30-60 min", ">60 min"))
d_math$famrel <- factor(d_math$famrel, labels = c("Very Bad", "Poor", "Fair", "Good", "Excellent"))  
d_port$famrel <- factor(d_port$famrel, labels = c("Very Bad", "Poor", "Fair", "Good", "Excellent")) 
d_math$freetime <- factor(d_math$freetime, labels = c("Very Low", "Low", "Medium", "High", "Very High"))
d_port$freetime <- factor(d_port$freetime, labels = c("Very Low", "Low", "Medium", "High", "Very High"))
d_math$goout <- factor(d_math$goout, labels = c("Very Low", "Low", "Medium", "High", "Very High"))
d_port$goout <- factor(d_port$goout, labels = c("Very Low", "Low", "Medium", "High", "Very High"))
d_math$Dalc <- factor(d_math$Dalc, labels = c("Very Low", "Low", "Medium", "High", "Very High"))
d_port$Dalc <- factor(d_port$Dalc, labels = c("Very Low", "Low", "Medium", "High", "Very High"))
d_math$Walc <- factor(d_math$Walc, labels = c("Very Low", "Low", "Medium", "High", "Very High"))
d_port$Walc <- factor(d_port$Walc, labels = c("Very Low", "Low", "Medium", "High", "Very High"))
d_math$health <- factor(d_math$health, labels = c("Very Bad", "Poor", "Fair", "Good", "Excellent"))  
d_port$health <- factor(d_port$health, labels = c("Very Bad", "Poor", "Fair", "Good", "Excellent")) 

# Create complete dataset d_total: combine math and Portuguese datasets with "course" as variable
course <- rep("math", times = length(d_math$school))
d_math <- cbind(d_math, course)

course <- rep("port", times = length(d_port$school))
d_port <- cbind(d_port, course)

d_total <- rbind(d_math, d_port)
d_total$course <- factor(d_total$course, labels = c("Math", "Portuguese"))

rm(d_both, course)

## EXPLORATORY DATA ANALYSIS

# Distribution of G3 grades
mean(d_total$G3, na.rm = TRUE) # ~11.34
median(d_total$G3, na.rm = TRUE) # 11

ggplot(d_total, aes(x = G3)) +
  geom_histogram(aes(y = ..density..), binwidth = 1) +
  stat_function(fun = dnorm,
                args = list(mean = mean(d_total$G3, na.rm = TRUE), sd = sd(d_total$G3, na.rm = TRUE)),
                lwd = 1,
                col = "red") +
  xlab("Final Grade")

# How do distributions of math and Portuguese grades compare?
ggplot(d_total, aes(x = G3)) +
  geom_histogram(aes(y = ..density..), binwidth = 1) +
  stat_function(fun = dnorm,
                args = list(mean = mean(d_total$G3, na.rm = TRUE), sd = sd(d_total$G3, na.rm = TRUE)),
                lwd = 1,
                col = "red") +
  xlab("Final Grade") +
  facet_grid(. ~ course) 

ggplot(d_total, aes(x = G3, y = ..density.., col = course)) +
  geom_freqpoly(binwidth = 1, position = "identity") +
  xlab("Final Grade")

ggplot(d_total, aes(x=course, y=G3,colour=course)) + 
  geom_boxplot(notch = T) +
  xlab("Math vs. Portuguese") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.1))

mean(d_math$G3, na.rm = TRUE) # ~10.42
median(d_math$G3, na.rm = TRUE) # 11
mean(d_port$G3, na.rm = TRUE) # ~11.91
median(d_port$G3, na.rm = TRUE) # 12

nrow(d_math[d_math$G3 < 2, ]) / nrow(d_math) # ~9.6% 
nrow(d_port[d_port$G3 < 2, ]) / nrow(d_port) # ~2.5% 

nrow(d_math[d_math$G3 >= 18, ]) / nrow(d_math) # ~4.6%
nrow(d_port[d_port$G3 >= 18, ]) / nrow(d_port) # ~2.6%

# Mean and median of math grades are lower, and math students failed at almost four times the rate of Portuguese students.
# However, math students received highest marks at nearly twice the rate that Portuguese students did.

t.test(G3 ~ course, data = d_total) # p-value = 2.215e-08, significant

# Who are the students with the lowest final grades (G3 = 0 or 1)?
nrow(d_total[total$G3 < 2, ]) # There are 54 students in this group. I wonder what they have in common?

# Who are the students with the highest grades (G3 >= 18)?
nrow(d_total[d_total$G3 >= 18, ]) # There are 35 students in this group.

## G1 and G2 as predictor variables

# I would assume that a student's interim grades are excellent predictors of her/his final grade.
# Is this correct?

ggplot(d_total, aes(x=G1, y=G3)) +
         geom_point(position = "jitter", alpha=0.6) +
         xlab("First period grade") + ylab("Final grade")

ggplot(d_total, aes(x=G2, y=G3)) +
  geom_point(position = "jitter", alpha=0.6) +
  xlab("Second period grade") + ylab("Final grade")

# There appears to be a  strong linear relationship between G1 and G3 and between G2 and G3.

summary(aov(G3 ~ G1 + G2, data = d_total)) # ANOVA confirms significance of G1 and G2.

## Correlations between final grade and the attributes associated with course

# STUDYTIME: weekly study time within the course subject (math or Portuguese)
ggplot(d_total, aes(x=studytime, y=G3,colour=studytime)) + 
  geom_boxplot(notch = T) +
  xlab("Study Time Per Week") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.1))
# Highest median grades among students who study 5-10 hrs/week.

ggplot(d_total, aes(x=studytime, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) + 
  xlab("Study Time Per Week") + ylab("Final Grade")
# Fewer extremely low grades among students who study more; no Portuguese failures among those who study more.

summary(aov(G3 ~ studytime, data = d_total)) # p-value is 9.92e-07, significant.

# Is it different for math and Portuguese?
summary(aov(G3 ~ studytime, data = d_math)) # p-value = ~5.2%, not significant (barely)
summary(aov(G3 ~ studytime, data = d_port)) # p-value = 1.09e-10, significant
# Study time has a bigger impact on Portuguese grades than on math grades.
summary(aov(G3 ~ studytime*course, data = d_total)) # Confirmed.

# FAILURES: number of past class failures within the course subject (math or Portuguese)
ggplot(d_total, aes(x=failures, y=G3,colour=failures)) + 
  geom_boxplot(notch = T) +
  xlab("# Courses Previously Failed") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.3))
# Higher median grades among students with 0 failures. No high grades among students with several failures.

# Previous failures appear rare. What % of students never previously failed?
nrow(d_total[d_total$failures == 0, ]) / nrow(d_total) # ~82.5% of students.

# A portion of students absolutely fail regardless of whether they've failed before, 
# but this is much more likely for students who have previously failed.
nrow(d_total[d_total$G3 < 2 & d_total$failures == 0, ]) / nrow(d_total[d_total$failures == 0, ]) # ~2.8%
nrow(d_total[d_total$G3 < 2 & d_total$failures == 1, ]) / nrow(d_total[d_total$failures == 1, ]) # 15%
nrow(d_total[d_total$G3 < 2 & d_total$failures == 2, ]) / nrow(d_total[d_total$failures == 2, ]) # ~18.2%
nrow(d_total[d_total$G3 < 2 & d_total$failures == 3, ]) / nrow(d_total[d_total$failures == 3, ]) # 20%

summary(aov(G3 ~ failures, data = d_total)) # p-value < 2e-16, significant.

# Is there a difference between math and Portuguese?
summary(aov(G3 ~ failures, data = d_math)) # p-value = 1.47e-13
summary(aov(G3 ~ failures, data = d_port)) # p-value < 2e-16
# The relationship between failures and final grades is significant for both.
summary(aov(G3 ~ failures*course, data = d_total))
# However, see results of above. There is a difference.

# SCHOOLSUP: extra educational support within the course subject (math or Portuguese)
ggplot(d_total, aes(x=schoolsup, y=G3,color=schoolsup)) + 
  geom_boxplot(notch = T) +
  xlab("Extra Educational Support") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.3))
# Students not getting support have higher median and mean grades.

t.test(G3 ~ schoolsup, data = d_total) # p-value = 0.0007223, significant.

# Is there a difference between math and Portuguese? 
ggplot(d_total, aes(x=schoolsup, y=G3, color=course)) +
  geom_boxplot(notch = T) +
  xlab("Extra Educational Support") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.3)) +
  facet_grid(. ~ course) 
t.test(G3 ~ schoolsup, data = d_math) # p-value = 0.01974, significant.
t.test(G3 ~ schoolsup, data = d_port) # p-value = 0.02675, significant.
# School support seems to have a similar impact on Portuguese and math students.

# FAMSUP: family educational support within the course subject (math or Portuguese)
ggplot(d_total, aes(x=famsup, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Family Educational Support") + ylab("Final Grade")
  
# No apparent correlations here, confirmed by t-test giving p-value = 67.3%.
t.test(G3 ~ famsup, data = d_total)

# PAID: extra paid classes within the course subject (math or Portuguese) 
ggplot(d_total, aes(x=paid, y=G3,color=paid)) + 
  geom_boxplot(notch = T) +
  xlab("Extra Paid Classes") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.3))
# Higher median grades among students who don't get support.

ggplot(d_total, aes(x=paid, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Extra Paid Classes") + ylab("Final Grade")
# A lot of math students pay for extra help! What percent? Compared to Portuguese students?
sum(d_math$paid == "yes") / nrow(d_math) # ~46%
sum(d_port$paid == "yes") / nrow(d_port) # ~6%

# Does paid help improve outcomes for math students?
nrow(d_math[d_math$G3 < 2 & d_math$paid == "yes", ]) / sum(d_math$paid == "yes") # ~4.4%
nrow(d_math[d_math$G3 < 2 & d_math$paid == "no", ]) / sum(d_math$paid == "no") # ~14%
# Yes, paid math help decreases the rate of math failure substantially.

# T-test analysis of impact of paid help shows it only helps for math.
t.test(G3 ~ paid, data = d_total) # p-value = 9.6%, not significant.
t.test(G3 ~ paid, data = d_math) # p-value = ~3.8%, significant.
t.test(G3 ~ paid, data = d_port) # p-value = ~12.3%, not significant.

## Correlations between final grade and the attributes NOT associated with course

# SCHOOL: student's school 
ggplot(d_total, aes(x=school, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("School") + ylab("Final Grade")
# It looks like a lot more students fail math at GP than at MS
nrow(d_total[d_total$school == "GP" & d_total$course == "Math" & d_total$G3 < 2, ]) / nrow(d_total[d_total$school == "GP", ]) # ~4.4%
nrow(d_total[d_total$school == "MS" & d_total$course == "Math" & d_total$G3 < 2, ]) / nrow(d_total[d_total$school == "MS", ]) # ~1.5%
# GP has about 3 times the math failure rate of MS.
nrow(d_total[d_total$school == "GP" & d_total$course == "Portuguese" & d_total$G3 < 2, ]) / nrow(d_total[d_total$school == "GP", ]) # ~.26%
nrow(d_total[d_total$school == "MS" & d_total$course == "Portuguese" & d_total$G3 < 2, ]) / nrow(d_total[d_total$school == "MS", ]) # ~5.1%
# MS has a much higher rate of Portuguese failure than GP.

# Is there a significant relationship between school and final grade? Yes.
t.test(G3 ~ school, data = d_total) # p-value = 5.168e-05, significant

# SEX: student's sex 
ggplot(d_total, aes(x=sex, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Sex") + ylab("Final Grade")
# No apparent relationship, confirmed by t-test.
t.test(G3 ~ sex, data = d_total) # p-value = ~30.9%

# Do female and male students perform similarly in math and Portuguese classes?
ggplot(d_total, aes(x=sex, y=G3, col = sex)) +
  geom_point(position = "jitter", alpha = 0.6) +
  facet_grid(. ~ course) +
  xlab("Sex") + ylab("Final Grade")
t.test(G3 ~ sex, data = d_math) # p-value = 0.03958,  significant
t.test(G3 ~ sex, data = d_port) # p-value = 0.001125, significant
# I don't understand this outcome. If sex is not a significant contributor to final grade in the whole dataset, 
# how can it be a significant contributor to final grades in the datasets that make up the whole dataset?

ggplot(d_total, aes(x=sex, y=G3, color=sex)) +
  geom_boxplot(notch = T) +
  xlab("Sex") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.3)) +
  facet_grid(. ~ course) 
# It appears that female students do better in Portuguese and male students do better in math.
# Maybe the impact of sex on math grades "cancels out" the impact of sex on Portuguese grades in the total dataset?

summary(lm(G3~sex, data = d_total))
summary(lm(G3~sex*course, data = d_total))

# Compare percent of students with lowest grades who are female to students with lowest grades who are male.
nrow(d_total[d_total$G3 < 2 & d_total$sex == "F", ]) / nrow(d_total[d_total$G3 < 2, ]) # ~55.6%
nrow(d_total[d_total$G3 < 2 & d_total$sex == "M", ]) / nrow(d_total[d_total$G3 < 2, ]) # ~44.4%

# Compare percent of students with highest grades who are female to students with highest grades who are male.
nrow(d_total[d_total$G3 >= 18 & d_total$sex == "F", ]) / nrow(d_total[d_total$G3 >= 18, ]) # ~54.3%
nrow(d_total[d_total$G3 >= 18 & d_total$sex == "M", ]) / nrow(d_total[d_total$G3 >= 18, ]) # ~44.7%
# A higher percentage of females are among the most successful and also among the least successful students.
# Or, a higher percentage of male students receive middle grades.

# AGE: student's age
# How many students of each age?
table(d_total$age)

ggplot(d_total, aes(x=age, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Age") + ylab("Final Grade")
# No failures among students older than 19, though there aren't many of these. Is age significant? Yes.
summary(aov(G3 ~ age, data = d_total)) # p-value = 4.93e-05

# What age student is most likely to get lowest grades?
ggplot(subset(d_total, d_total$G3 < 2), aes(x = age)) +
  geom_histogram(aes(y = ..density.., fill = course), binwidth = 1, position = "dodge") +
  xlab("Age")
# 18-year-olds are most likely to fail both subjects.
# Not a single 15-year-old failed Portuguese.
# Failure rates in math seem less affected by age than failures rates in Portuguese.

# ADDRESS: student's home address type (rural or urban) 
ggplot(d_total, aes(x=address, y=G3, color=address)) +
  geom_boxplot(notch = T) +
  xlab("Rural or Urban") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.3))
# Higher median grades among urban students.
t.test(G3 ~ address, data = d_total) # p-value = 0.0002071, significant

# Maybe a higher percentage of rural students fail?
nrow(d_total[d_total$address == "R" & d_total$G3 < 2, ]) / nrow(d_total[d_total$address == "R", ]) # ~7.0%
nrow(d_total[d_total$address == "U" & d_total$G3 < 2, ]) / nrow(d_total[d_total$address == "U", ]) # ~4.5%
# Yes, a higher percentage of rural students fail than urban students.
# Significant? No. P-value = 32.46%
t.test(G3 ~ address, data = d_total[d_total$G3 < 2, ])
# Address is also not significant among students who receive the highest grades. P-value = 96.24%
t.test(G3 ~ address, data = d_total[d_total$G3 >= 18, ])

# FAMSIZE: family size
ggplot(d_total, aes(x=famsize, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Family Size") + ylab("Final Grade")
# No obvious relationship, but t-test shows significance.
t.test(G3 ~ famsize, data = d_total) # p-value = 0.03691, significant.

ggplot(d_total, aes(x=famsize, y=G3,color=famsize)) +
  geom_boxplot(notch = T) +
  xlab("Family Size") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.3))
# Higher median grades among students with smaller family size.

# PSTATUS: parents' cohabitation status 
ggplot(d_total, aes(x=Pstatus, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Parents' Cohabitation Status") + ylab("Final Grade")
# No obvious relationship, confirmed by t-test.
t.test(G3 ~ Pstatus, data = d_total) # p-value = 29.57%, not significant.

# MEDU: mother's education
ggplot(d_total, aes(x=Medu, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Mother's Education") + ylab("Final Grade")
# It appears students are more likely to be enrolled if their mother has a higher level of education.

# But is mother's education correlated with final grade? Yes!
summary(aov(G3 ~ Medu, data = d_total)) # p-value = 2.9e-10, significant.

# Maybe students whose mothers have less education are more likely to fail? Yes.
nrow(d_total[d_total$Medu %in% c("none", "primary", "upper primary") & d_total$G3 < 2, ]) / 
  nrow(d_total[d_total$Medu %in% c("none", "primary", "upper primary"), ]) # 6.6%
nrow(d_total[d_total$Medu %in% c("secondary", "higher") & d_total$G3 < 2, ]) / 
  nrow(d_total[d_total$Medu %in% c("secondary", "higher"), ]) # ~3.9%

ggplot(d_total, aes(x=Medu, y=G3,color=Medu)) +
  geom_boxplot(notch = F) +
  xlab("Mother's Education") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.1))
# Impact of mother's education on median grade shows here.

# Does mother's level of education make more of a difference to female students?
ggplot(d_total, aes(x=Medu, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  facet_grid(. ~ sex) +
  xlab("Mother's Education") + ylab("Final Grade")
# It appears that more female students whose mothers have little education are enrolled.
nrow(d_total[d_total$Medu %in% c("none", "primary", "upper primary") & d_total$sex == "F", ]) / nrow(d_total[d_total$Medu %in% c("none", "primary", "upper primary"), ])
# Yes, 62% of students whose mothers have little education are female.

# But does mother's level of education make more of a difference to female students' grades?
summary(aov(G3 ~ Medu, data = d_total[d_total$sex == "F", ])) # p-value = 2.5e-05
summary(aov(G3 ~ Medu, data = d_total[d_total$sex == "M", ])) # p-value = 1.92e-05
# No, it is significant for both male and female students' grades.

# FEDU: father's education
ggplot(d_total, aes(x=Fedu, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Father's Education") + ylab("Final Grade")
# No obvious relationship, but one-way ANOVA test shows there is one.
summary(aov(G3 ~ Fedu, data = d_total)) # p-value = 2.67e-06

ggplot(d_total, aes(x=Fedu, y=G3,color=Fedu)) +
  geom_boxplot(notch = F) +
  xlab("Father's Education") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.1))
# As with Medu, students whose fathers have higher education levels, on average, get higher grades.

# Interesting that for both Medu and Fedu, students whose parents have no education have higher median grades
# than students whose parents have a primary education. Maybe these are especially motivated students.

# MJOB: Mother's job
ggplot(d_total, aes(x=Mjob, y=G3,color=Mjob)) +
  geom_boxplot(notch = T) +
  xlab("Mother's Job") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.1))
# Students whose mothers work as teachers or in health careers have higher average grades.

summary(aov(G3 ~ Mjob, data = d_total)) # p-value = 2.92e-06, signficant

# Seems to make sense that impact of Mjob correlates with impact of Medu...

# FJOB: Father's job
ggplot(d_total, aes(x=Fjob, y=G3,color=Fjob)) +
  geom_boxplot(notch = T) +
  xlab("Father's Job") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.1))
# Except for students whose fathers are teachers, there is not as apparent an impact on average grades as for Mjob.

summary(aov(G3 ~ Fjob, data = d_total)) # p-value = ~0.90%, significant

# REASON: Reason for choosing this school
ggplot(d_total, aes(x=reason, y=G3,color=reason)) +
  geom_boxplot(notch = T) +
  xlab("Reason for Choosing School") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.1))
# Some different in median grades based on this variable.

summary(aov(G3 ~ reason, data = d_total)) # p-value = ~0.05%, significant

# GUARDIAN: Student's guardian
ggplot(d_total, aes(x=guardian, y=G3,color=guardian)) +
  geom_boxplot(notch = T) +
  xlab("Guardian") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.2))
# Students' average grades are higher when their guardian is their father.

summary(aov(G3 ~ guardian, data = d_total)) # p-value = ~1.10%, significant

# TRAVELTIME: Home to school travel time
ggplot(d_total, aes(x=traveltime, y=G3,color=traveltime)) +
  geom_boxplot(notch = F) +
  xlab("Travel Time to School") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.2))
# Higher median grades among students who travel <15 minutes to get to school.

summary(aov(G3 ~ traveltime, data = d_total)) # p-value = ~1.10%, significant

# ACTIVITIES: Extra-curricular activities 
ggplot(d_total, aes(x=activities, y=G3,color=activities)) +
  geom_boxplot(notch = T) +
  xlab("Extra Curricular Activities") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.3))
# Higher median grades among students who do extra-curricular activities.

t.test(G3 ~ activities, data = d_total) # p-value = 27.26%, not significant

# NURSERY: Attended nursery school
ggplot(d_total, aes(x=nursery, y=G3,color=nursery)) +
  geom_boxplot(notch = T) +
  xlab("Attended Nursery School") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.3))
# Higher median grades among students who attended nursery school.

t.test(G3 ~ nursery, data = d_total) # p-value = 19.02%, not significant

# HIGHER: Wants to pursue higher education 
ggplot(d_total, aes(x=higher, y=G3,color=higher)) +
  geom_boxplot(notch = T) +
  xlab("Wants to Pursue Higher Ed") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.3))
# Significantly higher median grades among students who want to pursue higher education.

t.test(G3 ~ higher, data = d_total) # p-value = 3.68e-13, significant

# INTERNET: Internet access at home 
ggplot(d_total, aes(x=internet, y=G3,color=internet)) +
  geom_boxplot(notch = T) +
  xlab("Internet Access at Home") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.3))
# Higher median grades among students who have internet access.

t.test(G3 ~ internet, data = d_total) # p-value = 0.0005902, significant

# ROMANTIC: In a romantic relationship 
ggplot(d_total, aes(x=romantic, y=G3,color=romantic)) +
  geom_boxplot(notch = T) +
  xlab("In a Romantic Relationship") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.3))
# Higher median grades among students who are not in a romantic relationship.

t.test(G3 ~ romantic, data = d_total) # p-value = 0.002203, significant

# FAMREL: Quality of family relationships
ggplot(d_total, aes(x=famrel, y=G3,color=famrel)) +
  geom_boxplot(notch = T) +
  xlab("Quality of Family Relationships") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.1))
# Better family relationships correlated with higher median grades.

summary(aov(G3 ~ famrel, data = d_total)) # p-value = ~7.9%, not significant

# FREETIME: Free time after school
ggplot(d_total, aes(x=freetime, y=G3,color=freetime)) +
  geom_boxplot(notch = T) +
  xlab("Free Time After School") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.1))
# Highest median grades among students with "low" amount of free time after school.

summary(aov(G3 ~ freetime, data = d_total)) # p-value = 3.61%, significant

# GOOUT: Going out with friends
ggplot(d_total, aes(x=goout, y=G3,color=goout)) +
  geom_boxplot(notch = T) +
  xlab("Going Out with Friends") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.1))
# Higher median grades among students with "low" and "medium" time to go out with friends.

summary(aov(G3 ~ goout, data = d_total)) # p-value = ~0.15%, significant

# DALC: Workday alcohol consumption
ggplot(d_total, aes(x=Dalc, y=G3,color=Dalc)) +
  geom_boxplot(notch = F) +
  xlab("Workday Alcohol Consumption") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.1))
# Highest average grades among students with lowest workday alcohol consumption.

summary(aov(G3 ~ Dalc, data = d_total)) # p-value = 2.65e-05, significant

# WALC: Weekend alcohol consumption
ggplot(d_total, aes(x=Walc, y=G3,color=Walc)) +
  geom_boxplot(notch = T) +
  xlab("Weekend Alcohol Consumption") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.1))
# Lower average grades among students with higher weekend alcohol consumption.

summary(aov(G3 ~ Walc, data = d_total)) # p-value = ~0.02%, significant

# HEALTH: Current health status
ggplot(d_total, aes(x=health, y=G3,color=health)) +
  geom_boxplot(notch = T) +
  xlab("Current Health Status") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.1))
# Unclear impact?

summary(aov(G3 ~ health, data = d_total)) # p-value = ~0.97%, significant

# ABSENCES: Number of school absences
ggplot(d_total, aes(x=absences, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Number of School Absences") + ylab("Final Grade")

summary(aov(G3 ~ absences, data = d_total)) # p-value = 14%, not significant
# Is this a proper use of the ANOVA test?

nrow(d_total[d_total$absences == 0 & d_total$G3 < 2, ]) # 54
# All of the students who absolutely failed had 0 absences. Surprising!
# And the (few) students with very high numbers of absences got near-average grades.

## MODELING

# Multiple linear regression

# Try with all predictors except those that didn't show correlation with G3 in exploratory analysis.
lin.model.1 <- lm(G3 ~ .-G1 -G2 -famsup -Pstatus -activities -nursery -famrel -absences, data = d_total)
summary(lin.model.1) 
# Not a good model! F-statistic: 7.935, R-squ: 0.33

# Try again, with only those predictors that showed the most significant p-values.
lin.model.2 <- lm(G3 ~ school + course + failures + studytime, data = d_total)
summary(lin.model.2)
# Not better. F-stat: 36.53, R-squ = 0.22

lin.model.2.math <- lm(G3 ~ school + failures + studytime, data = d_math)
summary(lin.model.2.math)
lin.model.2.port <- lm(G3 ~ school + failures + studytime, data = d_port)
summary(lin.model.2.port)
# This model is better for Portuguese than for math, but still not good at all.

lin.model.3.math <- lm(G3 ~ failures + studytime, data = d_math)
summary(lin.model.3.math)
lin.model.3.port <- lm(G3 ~ failures + studytime, data = d_port)
summary(lin.model.3.port)
# Not good. I'm surprised, since these seem to be such significant variables in exploratory analysis.

# How to select which predictors to use? Try forward stepwise and backward stepwise selection.

library(leaps)
regfit.fwd.1 <- regsubsets(G3 ~ . -G1 -G2, data = d_total, method = "forward")
regfit.bwd.1 <- regsubsets(G3 ~ . -G1 -G2, data = d_total, method = "backward")

summary(regfit.fwd.1) 
# Best 1 var mod = failures1
# Best 2 var mod = + failures3
# Best 3 var mod = + failures2 
# Best 4 var mod = + coursePortuguese
# Best 5 var mod = + higheryes
# Best 6 var mod = + schoolMS
# Best 7 var mod = + MeduHigher
# Best 8 var mod = + schoolsupyes
summary(regfit.bwd.1) 
# Best 1 var mod = failures1
# Best 2 var mod = + failures3
# Best 3 var mod = + failures2 
# Best 4 var mod = + coursePortuguese
# Best 5 var mod = + higheryes
# Best 6 var mod = + schoolMS
# Best 7 var mod = + schoolsupyes
# Best 8 var mod = + studytime5-10 hrs

# Seems that best lm might include failures, course, higher, school, and (maybe) schoolsup
lin.model.4 <- lm(G3 ~ failures + course + higher + school, data = d_total)
summary(lin.model.4)
# Even this is NOT a good model! R-squ = .226 and F-stat = 50.4

# In all these models I left out G1 and G2 because it seems uninteresting 
# to model final grades by considering interim grades.
# However, I see in the Cortez/Silva paper about this dataset that the researchers 
# did include G1 and/or G2 and found them essential.

# Maybe I'll try one more time, this time including G1 and G2.
regfit.fwd.2 <- regsubsets(G3 ~ ., data = d_total, method = "forward")
regfit.bwd.2 <- regsubsets(G3 ~ ., data = d_total, method = "backward")

summary(regfit.fwd.2)
# Best 1 var mod = G2
# Best 2 var mod = + coursePortuguese
# Best 3 var mod = + G1
# Best 4 var mod = + absences
# Best 5 var mod = + failures1
# Best 6 var mod = + traveltime>60 min
# Best 7 var mod = + DalcLow
# Best 8 var mod = + gooutVery High
summary(regfit.bwd.2)
# Best 1 var mod = G2
# Best 2 var mod = + coursePortuguese
# Best 3 var mod = + G1
# Best 4 var mod = + absences
# Best 5 var mod = + failures1
# Best 6 var mod = + traveltime>60 min
# Best 7 var mod = + DalcLow
# Best 8 var mod = + romanticyes

# Results are quite different! Try lm with different predictors, including G1 and G2.

# First the best 7 variable model using results from regsubsets() above.
lin.model.5 <- lm(G3 ~ G1 + G2 + course + absences + failures + traveltime + Dalc, data = d_total)
summary(lin.model.5)
# This is a much better model, R-squ = .846 and F-stat = 403.1

# Try again, this time just with "intuitive" predictors from the above model.
lin.model.6 <- lm(G3 ~ G1 + G2 + course + absences + failures, data = d_total)
summary(lin.model.6)
# This one is even better, R-squ = .843 and F-stat = 797

# How about just G1 and G2?
lin.model.7 <- lm(G3 ~ G1 + G2, data = d_total)
summary(lin.model.7)
# Well, this one is best of all. R-squ = .832 and F-stat = 2582

# That said, as an educator, I ALREADY know that a student who is failing at G1 is at high risk of
# failing the course, and a student who has failed both G1 and G2 is obviously in grave academic danger.
# And a student getting high marks at G1 and G2 is of course likely to get high marks at G3.
# It would be MUCH more helpful to predict likely student outcomes BEFORE G1.

# Work with this model on a training and test set. First split the dataset.
dt = sort(sample(nrow(d_total), nrow(d_total)*.7))
Train.reg <- d_total[dt,]
Test.reg <- d_total[-dt,]

# Apply the model to the training set.
lm.mod <- lm(G3 ~ G1 + G2, data = Train.reg)
summary(lm.mod) # Adjusted R-squ = 0.80

# Now use it to predict the test set.
lm.pred <- predict(lm.mod, newdata = Test.reg)
SSE = sum((Test.reg$G3 - lm.pred)^2)
SST = sum((Test.reg$G3 - mean(Train.reg$G3))^2)
1 - SSE/SST # out of sample R-squ is .90

# How do diagnostic plots look for this model? 
plot(lin.model.7)

# The Residuals vs. Fitted plot shows the strong linear relationship, 
# and also the sizeable group of students who were passing at midterm but failed at G3.

# The Normal Q-Q plot shows that the residuals in the lowest quantile are not normally distributed.
# This also makes sense given the group of students who were passing at midterm but failed at G3.

# The Scale Location plot also reveals the outlier group.

# The Residuals vs. Leverage plot shows no data outside Cook's distance.

# Observations of particular concern are 265, 342, 559.
d_total[265, 31:33]
d_total[342, 31:33]
d_total[559, 31:33]
# All have G1 + G2 >= 19 and G3 = 0, students who were doing well at midterm but dropped out completely.

# How many students in the dataset fall into this category?
nrow(d_total[d_total$G1 + d_total$G2 >= 19 & d_total$G3 == 0, ]) #6
# Who are they?
subset(d_total, d_total$G1 + d_total$G2 >= 19 & d_total$G3 == 0, select = c(G1, G2, G3))
# 260, 265, 297, 335, 342, 559

# How much better would the model be without these students?
dropouts <- subset(d_total, d_total$G1 + d_total$G2 >= 19 & d_total$G3 == 0)
d_adjusted <- anti_join(d_total, dropouts)
  
lin.model.adj <- lm(G3 ~ G1 + G2, data = d_adjusted)
summary(lin.model.adj)
# Adjusted R-squared:  0.8588, F-statistic:  3155
plot(lin.model.adj)
# Plots look the same as when those students were included! R-squ and F-stat are better, though.
# I think the issue is ALL the students who dropped out, not just the previously high-achieving dropouts.

# Test this idea by making a model that excludes all the dropouts (students with G1 and/or G2 > 0 but G3 = 0)
zeroes <- subset(d_total, d_total$G3 == 0) 
dropouts.all <- subset(zeroes, zeroes$G1 != 0 | zeroes$G2 != 0)
d_adj.all <- anti_join(d_total, dropouts.all)

lin.model.adj.all <- lm(G3 ~ G1 + G2, data = d_adj.all)
summary(lin.model.adj.all)
# Adjusted R-squared: 0.9027, F-statistic:  4592
plot(lin.model.adj.all)
# Removing the dropouts eliminates all the irregularities in the diagnostic plots.

# So the linear model is great at predicting most of the outcomes, 
# but it fails to anticipate the dropouts (students who start out passing but ultimately fail).

# Rethinking big picture: Bin G3 rather than predict numeric grade? 
# I'm thinking about outcomes that suggest action by educators.
# Which students are in danger of failing?

# First, convert G3 to categorical variable called "outcome" with levels "fail" and "pass".
d_math_cat <- d_math %>% mutate(outcome=ifelse(G3<8,"fail","pass")) %>%
  select(-G3)
d_port_cat <- d_port %>% mutate(outcome=ifelse(G3<8,"fail","pass")) %>%
  select(-G3)
d_math_cat$outcome <- as.factor(d_math_cat$outcome)
d_port_cat$outcome <- as.factor(d_port_cat$outcome)
d_total_cat <- rbind(d_math_cat, d_port_cat)

# Split training and test set
library(caTools)
set.seed(77)
split <- sample.split(d_total_cat$outcome, SplitRatio = 0.75)
Train.cat <- subset(d_total_cat, split == TRUE) 
Test.cat <- subset(d_total_cat, split == FALSE)
Test.cat <- Test.cat[-which(Test.cat$Fedu == "None"), ]

# LOGISTIC REGRESSION

# First, calculate baseline to beat (percent of total outcomes that are the most likely outcome, "pass").
nrow(d_total_cat[d_total_cat$outcome == "pass", ])/nrow(d_total_cat) # 90.4%

# Because the majority outcome "pass" is so much more common than the minority outcome "fail", 
# accuracy is not that great a measure of goodness of fit. Used "balanced accuracy" instead
# (arithmetic mean of accuracy of prediction of each class).

# Presumably G1 and G2 would be very effective predictors of outcome.
log.model.1 <- glm(outcome ~ G1 + G2, data = Train.cat, family = binomial)
summary(log.model.1) # AIC: 190.57
log.pred.1 <- predict(log.model.1, newdata = Test.cat, type = "response")
table(Test.cat$outcome, log.pred.1 > 0.5) # Confusion matrix
(17+234)/261 # Accuracy: 0.96
((17/(17+8))+(234/(234+2)))/2 # Balanced accuracy: 0.84
17/(17+8) # Specificity: 0.68
# Well, this model does a good job identifying students who pass, but not so much students who fail.

# Would it do better if the data were better balanced?
# Use the ROSE package to balance the dataset by four methods

library(ROSE)

# Unbalanced dataset
table(Train.cat$outcome) # How many pass, how many fail in unbalanced training set?

# Balance training set by oversampling "fail"
Train.cat.over <- ovun.sample(outcome ~., data = Train.cat, method = "over", N=1416)$data
table(Train.cat.over$outcome) # Numbers in each class

# Balance training set by undersampling "pass"
Train.cat.under <- ovun.sample(outcome ~., data = Train.cat, method = "under", N=150)$data
table(Train.cat.under$outcome) # Numbers in each class

# Balance training set by both over- and undersampling
Train.cat.both <- ovun.sample(outcome ~., data = Train.cat, method = "both", p=0.5, N=783, seed = 1)$data
table(Train.cat.both$outcome) # Numbers in each class

# Balance training set by creating synthetic minority class observations
Train.cat.syn <- ROSE(outcome ~., data = Train.cat, seed = 1)$data
table(Train.cat.syn$outcome) # Numbers in each class

# Recreate logistic regression model using each of the four balanced datasets.
# Which has best results?

# Using dataset with "fail" oversampled
log.model.1.over <- glm(outcome ~ G1 + G2, data = Train.cat.over, family = binomial)
summary(log.model.1.over) # AIC: 646.71
log.pred.1.over <- predict(log.model.1.over, newdata = Test.cat, type = "response")
table(Test.cat$outcome, log.pred.1.over > 0.5) # Confusion matrix
(24+220)/261 # Accuracy: 0.935
((24/(24+1))+(220/(220+16)))/2 # Balanced accuracy: 0.946
24/(24+1) # Specificity: 0.96

# Using dataset with "pass" undersampled
log.model.1.under <- glm(outcome ~ G1 + G2, data = Train.cat.under, family = binomial)
summary(log.model.1.under) # AIC: 68.06
log.pred.1.under <- predict(log.model.1.under, newdata = Test.cat, type = "response")
table(Test.cat$outcome, log.pred.1.under > 0.5) # Confusion matrix
(24+220)/261 # Accuracy: 0.935
((24/(24+1))+(220/(220+16)))/2 # Balanced accuracy: 0.946
24/(24+1) # Specificity: 0.96

# Using dataset with both over- and undersampling
log.model.1.both <- glm(outcome ~ G1 + G2, data = Train.cat.both, family = binomial)
summary(log.model.1.both) # AIC: 357.95
log.pred.1.both <- predict(log.model.1.both, newdata = Test.cat, type = "response")
table(Test.cat$outcome, log.pred.1.both > 0.5) # Confusion matrix
(24+220)/261 # Accuracy: 0.935
((24/(24+1))+(220/(220+16)))/2 # Balanced accuracy: 0.946
24/(24+1) # Specificity: 0.96

# Using dataset with synthetic "fail" observations
log.model.1.syn <- glm(outcome ~ G1 + G2, data = Train.cat.syn, family = binomial)
summary(log.model.1.syn) # AIC: 463.2
log.pred.1.syn <- predict(log.model.1.syn, newdata = Test.cat, type = "response")
table(Test.cat$outcome, log.pred.1.syn > 0.5) # Confusion matrix
(24+220)/261 # Accuracy: 0.935
((24/(24+1))+(220/(220+16)))/2 # Balanced accuracy: 0.946
24/(24+1) # Specificity: 0.96

# All four of these models make identical predictions, according to the confusion matrixes,
# so their accuracy and specificity are the same.
# However, the model made from the dataset with undersampling has the lowest AIC, so it is the best.

# How about logistic regression models built on combinations of the same variables that I tried in the linear regression models?
# Try each of these with the balanced datasets.

# With "failures" as only predictor
log.model.2 <- glm(outcome ~ failures, data = Train.cat, family = binomial)
summary(log.model.2) # AIC: 452.03
log.model.2.over <- glm(outcome ~ failures, data = Train.cat.over, family = binomial)
summary(log.model.2.over) # AIC: 1762.7
log.model.2.under <- glm(outcome ~ failures, data = Train.cat.under, family = binomial)
summary(log.model.2.under) # AIC: 196.43
log.model.2.both <- glm(outcome ~ failures, data = Train.cat.both, family = binomial)
summary(log.model.2.both) # AIC: 970.73
log.model.2.syn <- glm(outcome ~ failures, data = Train.cat.syn, family = binomial)
summary(log.model.2.syn) # AIC: 970.73
# Best version uses undersampling

# "Failures" and "course" as predictors
log.model.3 <- glm(outcome ~ failures + course, data = Train.cat, family = binomial)
summary(log.model.3) # AIC: 434.15
log.model.3.over <- glm(outcome ~ failures + course, data = Train.cat.over, family = binomial)
summary(log.model.3.over) # AIC: 1628
log.model.3.under <- glm(outcome ~ failures + course, data = Train.cat.under, family = binomial)
summary(log.model.3.under) # AIC: 182.29
log.model.3.both <- glm(outcome ~ failures + course, data = Train.cat.both, family = binomial)
summary(log.model.3.both) # AIC: 928.7
log.model.3.syn <- glm(outcome ~ failures + course, data = Train.cat.syn, family = binomial)
summary(log.model.3.syn) # AIC: 928.7
# Best version uses undersampling

# "Failures", "course", "higher" as predictors
log.model.4 <- glm(outcome ~ failures + course + higher, data = Train.cat, family = binomial)
summary(log.model.4) # AIC: 431.76
log.model.4.over <- glm(outcome ~ failures + course + higher, data = Train.cat.over, family = binomial)
summary(log.model.4.over) # AIC: 1606.8
log.model.4.under <- glm(outcome ~ failures + course + higher, data = Train.cat.under, family = binomial)
summary(log.model.4.under) # AIC: 180.4
log.model.4.both <- glm(outcome ~ failures + course + higher, data = Train.cat.both, family = binomial)
summary(log.model.4.both) # AIC: 918.95
log.model.4.syn <- glm(outcome ~ failures + course + higher, data = Train.cat.syn, family = binomial)
summary(log.model.4.syn) # AIC: 918.95
# Best version uses undersampling

# "Failures", "course", "higher", "school" as predictors
log.model.5 <- glm(outcome ~ failures + course + higher + school, data = Train.cat, family = binomial)
summary(log.model.5) # AIC: 427.87 
log.model.5.over <- glm(outcome ~ failures + course + higher + school, data = Train.cat.over, family = binomial)
summary(log.model.5.over) # AIC: 1555.4
log.model.5.under <- glm(outcome ~ failures + course + higher + school, data = Train.cat.under, family = binomial)
summary(log.model.5.under) # AIC: 172.25
log.model.5.both <- glm(outcome ~ failures + course + higher + school, data = Train.cat.both, family = binomial)
summary(log.model.5.both) # AIC: 904.76
log.model.5.syn <- glm(outcome ~ failures + course + higher + school, data = Train.cat.syn, family = binomial)
summary(log.model.5.syn) # AIC: 904.76
# Best version uses undersampling

# Logistic regression model with all variables except G1 and G2?
log.model.6 <- glm(outcome ~ . -G1 -G2, data = Train.cat, family = binomial)
summary(log.model.6) # AIC: 457.01
log.model.6.over <- glm(outcome ~ . -G1 -G2, data = Train.cat.over, family = binomial)
summary(log.model.6.over) # AIC: 1192.6
log.model.6.under <- glm(outcome ~ . -G1 -G2, data = Train.cat.under, family = binomial)
summary(log.model.6.under) # AIC: 142
log.model.6.both <- glm(outcome ~ . -G1 -G2, data = Train.cat.both, family = binomial)
summary(log.model.6.both) # AIC: 760.29
log.model.6.syn <- glm(outcome ~ . -G1 -G2, data = Train.cat.syn, family = binomial)
summary(log.model.6.syn) # AIC: 766.89
# Best version uses undersampling

# Log.model.6.under has the lowest AIC, so try this one.
log.pred.6.under <- predict(log.model.6.under, newdata = Test.cat, type = "response")
table(Test.cat$outcome, log.pred.6.under > 0.5) # Confusion matrix
(17+165)/261 # Accuracy: 0.697
((17/(17+8))+(165/(165+71)))/2 # Balanced accuracy: 0.69
17/(17+8) # Specificity: 0.68

# So, the best logistic regression model that does not include G1 and G2 is not a strong model at all!

# DECISION TREES

# Because decision trees do not require linear relationships between variables, 
# this machine learning method might be more successful.

# Test models with AUC of the ROC curve, accuracy, balanced accuracy, and specificity.

# G1 and G2 as only predictors in a decision tree -- try this on the unbalanced dataset first.
library(rpart)
library(rpart.plot)
tree.mod.1 <- rpart(outcome ~ G1 + G2, data = Train.cat, method = "class") # Build model
tree.pred.1 <- predict(tree.mod.1, newdata = Test.cat, type = "class") # Apply to test set
roc.curve(Test.cat$outcome, tree.pred.1, plotit = FALSE) # AUC: 0.836
table(Test.cat$outcome, tree.pred.1) # Confusion matrix
(17+234)/261 # Accuracy: 0.96
((17/(17+8))+(234/(234+2)))/2 # Balanced accuracy: 0.84
17/(17+8) # Specificity: 0.68

# Is the model improved by using a balanced training set?

# Oversample
tree.mod.1.over <- rpart(outcome ~ G1 + G2, data = Train.cat.over, method = "class") # Build model
tree.pred.1.over <- predict(tree.mod.1.over, newdata = Test.cat, type = "class") # Apply to test set

# Undersample
tree.mod.1.under <- rpart(outcome ~ G1 + G2, data = Train.cat.under, method = "class") # Build model
tree.pred.1.under <- predict(tree.mod.1.under, newdata = Test.cat, type = "class") # Apply to test set

# Both
tree.mod.1.both <- rpart(outcome ~ G1 + G2, data = Train.cat.both, method = "class") # Build model
tree.pred.1.both <- predict(tree.mod.1.both, newdata = Test.cat, type = "class") # Apply to test set

# Synthetic
tree.mod.1.syn <- rpart(outcome ~ G1 + G2, data = Train.cat.syn, method = "class") # Build model
tree.pred.1.syn <- predict(tree.mod.1.syn, newdata = Test.cat, type = "class") # Apply to test set

# How good is each of these models? 

# Oversample
roc.curve(Test.cat$outcome, tree.pred.1.over, plotit = FALSE) # AUC: 0.946
table(Test.cat$outcome, tree.pred.1.over) # Confusion matrix
(24+220)/(1+24+220+16) # Accuracy: 0.93
((24/(24+1))+(220/(220+16)))/2 # Balanced accuracy: 0.95
24/(24+1) # Specificity: 0.96

# Undersample
roc.curve(Test.cat$outcome, tree.pred.1.under, plotit = FALSE) # AUC: 0.899
table(Test.cat$outcome, tree.pred.1.under) # Confusion matrix
(24+198)/(1+24+198+38) # Accuracy: 0.85
((24/(24+1))+(198/(198+38)))/2 # Balanced accuracy: 0.899
24/(24+1) # Specificity: 0.96

# Both
roc.curve(Test.cat$outcome, tree.pred.1.both, plotit = FALSE) # AUC: 0.946
table(Test.cat$outcome, tree.pred.1.both) # Confusion matrix
(24+220)/(1+24+220+16) # Accuracy: 0.93
((24/(24+1))+(220/(220+16)))/2 # Balanced accuracy: 0.95
24/(24+1) # Specificity: 0.96

# Synthetic
roc.curve(Test.cat$outcome, tree.pred.1.syn, plotit = FALSE) # AUC: 0.905
table(Test.cat$outcome, tree.pred.1.syn) # Confusion matrix
(21+229)/(4+21+229+7) # Accuracy: 0.96
((21/(21+4))+(229/(229+7)))/2 # Balanced accuracy: 0.91
21/(21+4) # Specificity: 0.84

# The best G1/G2 models use either oversampling or both oversampling and undersampling to balance the dataset. 
# These methods produce models with identically high AUCs of ROC curves, balanced accuracies that beat the baseline, 
# and very high specificity (accuracy rate of true negatives).

# Here is a visual representation of the best G1/G2 decision tree. It uses the data balanced with both over- and undersampling.
prp(tree.mod.1.both)
# It may be a good predictive model, but it's not very interesting. It simply predicts that all students with G2 greater than or equal to 8.5 will pass.

# How about a decision tree model built on all predictors except G1 and G2?
tree.mod.2 <- rpart(outcome ~ . -G1 -G2, data = Train.cat, method = "class")
prp(tree.mod.2)
# This model looks exciting! 
tree.pred.2 <- predict(tree.mod.2, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.2, plotit = FALSE) # AUC: 0.563
table(Test.cat$outcome, tree.pred.2) # Confusion matrix
(4+228)/261 # Accuracy: 0.889
((4/(21+4))+(228/(228+8)))/2 # Balanced accuracy: 0.56
4/(4+21) # Specificity: 0.16
# Sadly, the AUC of the ROC curve is terribly low, analysis of the confusion matrix shows 
# the balanced accuracy is far below baseline, and the specificity is only 16%. This is not a good model!
  
# Does using a balanced training set improve this model? 
# Try the dataset balanced using over- and undersampling.
tree.mod.2.both <- rpart(outcome ~ . -G1 -G2, data = Train.cat.both, method = "class")
tree.pred.2.both <- predict(tree.mod.2.both, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.2.both, plotit = FALSE) # AUC: 0.737
table(Test.cat$outcome, tree.pred.2.both) # Confusion matrix
(16+197)/261 # Accuracy: 0.816
((16/(16+9))+(197/(197+39)))/2 # Balanced accuracy: 0.74
16/(16+9) # Specificity: 0.64
# Balancing the training data does improve the model, 
# but it still isn't nearly as good as the G1 and G2 decision tree model.

# Any better results with datasets balanced by other methods?
tree.mod.2.over <- rpart(outcome ~ . -G1 -G2, data = Train.cat.over, method = "class")
tree.pred.2.over <- predict(tree.mod.2.over, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.2.over, plotit = FALSE) # AUC: 0.687

tree.mod.2.under <- rpart(outcome ~ . -G1 -G2, data = Train.cat.under, method = "class")
tree.pred.2.under <- predict(tree.mod.2.under, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.2.under, plotit = FALSE) # AUC: 0.786

tree.mod.2.syn <- rpart(outcome ~ . -G1 -G2, data = Train.cat.syn, method = "class")
tree.pred.2.syn <- predict(tree.mod.2.syn, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.2.syn, plotit = FALSE) # AUC: 0.785

# The model built from the dataset balanced by undersampling has a slightly higher AUC of ROC,
# but not much better.

# Try building decision tree models using the variables identified in the stepwise process (failures, course, higher, school). 
# Use data balanced by all four methods.

# Predictor = "failures"
tree.mod.3 <- rpart(outcome ~ failures, data = Train.cat, method = "class")
tree.pred.3 <- predict(tree.mod.3, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.3, plotit = FALSE) # AUC: 0.50

tree.mod.3.over <- rpart(outcome ~ failures, data = Train.cat.over, method = "class")
tree.pred.3.over <- predict(tree.mod.3.over, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.3.over, plotit = FALSE) # AUC: 0.705

tree.mod.3.under <- rpart(outcome ~ failures, data = Train.cat.under, method = "class")
tree.pred.3.under <- predict(tree.mod.3.under, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.3.under, plotit = FALSE) # AUC: 0.705

tree.mod.3.both <- rpart(outcome ~ failures, data = Train.cat.both, method = "class")
tree.pred.3.both <- predict(tree.mod.3.both, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.3.both, plotit = FALSE) # AUC: 0.705

tree.mod.3.syn <- rpart(outcome ~ failures, data = Train.cat.syn, method = "class")
tree.pred.3.syn <- predict(tree.mod.3.syn, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.3.syn, plotit = FALSE) # AUC: 0.705

# Predictors = "failures", "course"
tree.mod.4 <- rpart(outcome ~ failures + course, data = Train.cat, method = "class")
tree.pred.4 <- predict(tree.mod.4, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.4, plotit = FALSE) # AUC: 0.50

tree.mod.4.over <- rpart(outcome ~ failures + course, data = Train.cat.over, method = "class")
tree.pred.4.over <- predict(tree.mod.4.over, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.4.over, plotit = FALSE) # AUC: 0.781

tree.mod.4.under <- rpart(outcome ~ failures + course, data = Train.cat.under, method = "class")
tree.pred.4.under <- predict(tree.mod.4.under, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.4.under, plotit = FALSE) # AUC: 0.781

tree.mod.4.both <- rpart(outcome ~ failures + course, data = Train.cat.both, method = "class")
tree.pred.4.both <- predict(tree.mod.4.both, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.4.both, plotit = FALSE) # AUC: 0.781

tree.mod.4.syn <- rpart(outcome ~ failures + course, data = Train.cat.syn, method = "class")
tree.pred.4.syn <- predict(tree.mod.4.syn, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.4.syn, plotit = FALSE) # AUC: 0.781

# Predictors = "failures", "course", "higher"
tree.mod.5 <- rpart(outcome ~ failures + course + higher, data = Train.cat, method = "class")
tree.pred.5 <- predict(tree.mod.5, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.5, plotit = FALSE) # AUC: 0.50

tree.mod.5.over <- rpart(outcome ~ failures + course + higher, data = Train.cat.over, method = "class")
tree.pred.5.over <- predict(tree.mod.5.over, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.5.over, plotit = FALSE) # AUC: 0.781

tree.mod.5.under <- rpart(outcome ~ failures + course + higher, data = Train.cat.under, method = "class")
tree.pred.5.under <- predict(tree.mod.5.under, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.5.under, plotit = FALSE) # AUC: 0.781

tree.mod.5.both <- rpart(outcome ~ failures + course + higher, data = Train.cat.both, method = "class")
tree.pred.5.both <- predict(tree.mod.5.both, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.5.both, plotit = FALSE) # AUC: 0.781

tree.mod.5.syn <- rpart(outcome ~ failures + course + higher, data = Train.cat.syn, method = "class")
tree.pred.5.syn <- predict(tree.mod.5.syn, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.5.syn, plotit = FALSE) # AUC: 0.781

# Predictors = "failures", "course", "higher", "school"
tree.mod.6 <- rpart(outcome ~ failures + course + higher + school, data = Train.cat, method = "class")
tree.pred.6 <- predict(tree.mod.6, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.6, plotit = FALSE) # AUC: 0.574

tree.mod.6.over <- rpart(outcome ~ failures + course + higher + school, data = Train.cat.over, method = "class")
tree.pred.6.over <- predict(tree.mod.6.over, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.6.over, plotit = FALSE) # AUC: 0.781

tree.mod.6.under <- rpart(outcome ~ failures + course + higher + school, data = Train.cat.under, method = "class")
tree.pred.6.under <- predict(tree.mod.6.under, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.6.under, plotit = FALSE) # AUC: 0.781

tree.mod.6.both <- rpart(outcome ~ failures + course + higher + school, data = Train.cat.both, method = "class")
tree.pred.6.both <- predict(tree.mod.6.both, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.6.both, plotit = FALSE) # AUC: 0.798

tree.mod.6.syn <- rpart(outcome ~ failures + course + higher + school, data = Train.cat.syn, method = "class")
tree.pred.6.syn <- predict(tree.mod.6.syn, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.6.syn, plotit = FALSE) # AUC: 0.798

# The very best of these model (tied for the highest AUC) are tree.mod.6.both and tree.mod.6.syn.
# How good are these models?

table(Test.cat$outcome, tree.pred.6.both) # Confusion matrix
table(Test.cat$outcome, tree.pred.6.syn) # Confusion matrix
# They give the same results, so confusion matrixes are identical.
(24+150)/261 # Accuracy: 0.667
((24/(24+1))+(150/(150+86)))/2# Balanced accuracy: 0.798
24/(1+24) # Specificity: 0.96
# These models do a good job identifying the "fails", but miss many of the "passes". 
# Their accuracy is not very good.

# Just for comparison, make model with all predictors, including G1 and G2
tree.mod.7 <- rpart(outcome ~ ., data = Train.cat, method = "class")
tree.pred.7 <- predict(tree.mod.7, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.7, plotit = FALSE) # AUC: 0.758

tree.mod.7.over <- rpart(outcome ~ ., data = Train.cat.over, method = "class")
tree.pred.7.over <- predict(tree.mod.7.over, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.7.over, plotit = FALSE) # AUC: 0.936

tree.mod.7.under <- rpart(outcome ~ ., data = Train.cat.under, method = "class")
tree.pred.7.under <- predict(tree.mod.7.under, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.7.under, plotit = FALSE) # AUC: 0.946

tree.mod.7.both <- rpart(outcome ~ ., data = Train.cat.both, method = "class")
tree.pred.7.both <- predict(tree.mod.7.both, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.7.both, plotit = FALSE) # AUC: 0.906

tree.mod.7.syn <- rpart(outcome ~ ., data = Train.cat.syn, method = "class")
tree.pred.7.syn <- predict(tree.mod.7.syn, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.7.syn, plotit = FALSE) # AUC: 0.901

# The best of these (highest AUC) is tree.pred.7.under.
table(Test.cat$outcome, tree.pred.7.under) # Confusion matrix
(24+220)/261 # Accuracy: 0.935
((24/(24+1))+(220/(220+16)))/2# Balanced accuracy: 0.946
24/(1+24) # Specificity: 0.96
# Including G1 and G2 makes a much better decision tree model.

# What does this model look like?
prp(tree.mod.7.under)

# A model that includes all predictors is essentially identical to a model that includes just G1 and G2.
# Adding more variables doesn't improve the model.

# tree.pred.1.both (G1 and G2 only) AUC: 0.946, balanced accuracy: 0.95, specificity: 0.96
# tree.pred.7.under (all predictors) AUC: 0.946, balanced accuracy: 0.946, specificy: 0.96

# RANDOM FOREST

# Perhaps a random forest, with its prediction capabilities, might work?

library(randomForest)

# Random forest with only G1 and G2 as predictors.

# Not balanced
forest.mod.1 <- randomForest(outcome ~ G1 + G2, data = Train.cat, nodesize = 100, ntree = 200)
forest.pred.1 <- predict(forest.mod.1, newdata = Test.cat)
roc.curve(Test.cat$outcome, forest.pred.1, plotit = FALSE) # AUC: 0.856
table(Test.cat$outcome, forest.pred.1) # Confusion matrix
(17+234)/261 # Accuracy: 0.962
((18/(18+7))+(234/(234+2)))/2 # Balanced accuracy: 0.856
18/(18+7) # Specificity: 0.72

# Since about 10% of the outcomes are "fail", set classwt=c(0.1, 0.9) as randomForest argument to balance dataset.
forest.mod.1.bal <- randomForest(outcome ~ G1 + G2, data = Train.cat, nodesize = 100, ntree = 500, classwt=c(0.1, 0.9))
forest.pred.1.bal <- predict(forest.mod.1.bal, newdata = Test.cat)
roc.curve(Test.cat$outcome, forest.pred.1.bal, plotit = FALSE) # AUC: 0.836
table(Test.cat$outcome, forest.pred.1.bal) # Confusion matrix
(17+234)/261 # Accuracy: 0.962
((17/(17+8))+(234/(234+2)))/2 # Balanced accuracy: 0.836
17/(17+8) # Specificity: 0.68

# Some surprises in this outcome: first, the results with classwt are not better than unbalanced;
# second, the G1/G2 random forest model is not as good as the G1/G2 decision tree model.

# Random forest with all predictors except G1 and G2.

# Not balanced
forest.mod.2 <- randomForest(outcome ~ . -G1 -G2, data = Train.cat, nodesize = 100, ntree = 500)
forest.pred.2 <- predict(forest.mod.2, newdata = Test.cat)
roc.curve(Test.cat$outcome, forest.pred.2, plotit = FALSE) # AUC: 0.50
table(Test.cat$outcome, forest.pred.2) # Confusion matrix
(0+236)/261 # Accuracy: 0.90
((0/25)+(236/236))/2 # Balanced accuracy: 0.5
0/(0+25) # Specificity: 0

# Balanced using classwt
forest.mod.2.bal <- randomForest(outcome ~ . -G1 -G2, data = Train.cat, nodesize = 100, ntree = 500, classwt=c(0.1, 0.9))
forest.pred.2.bal <- predict(forest.mod.2.bal, newdata = Test.cat)
roc.curve(Test.cat$outcome, forest.pred.2.bal, plotit = FALSE) # AUC: 0.50
table(Test.cat$outcome, forest.pred.2.bal) # Confusion matrix
(0+236)/261 # Accuracy: 0.90
((0/25)+(236/236))/2 # Balanced accuracy: 0.5
0/(0+25) # Specificity: 0

# Without G1 and G2, a random forest model predicts that all students will pass. This is a terrible model.

# Random forest using variables identified in stepwise process (same variables as used in best linear regression and best logistic regression).

# Predictor: failures
# Unbalanced
forest.mod.3 <- randomForest(outcome ~ failures, data = Train.cat, nodesize = 100, ntree = 500)
forest.pred.3 <- predict(forest.mod.3, newdata = Test.cat)
roc.curve(Test.cat$outcome, forest.pred.3, plotit = FALSE) # AUC: 0.50

# Balanced
forest.mod.3.bal <- randomForest(outcome ~ failures, data = Train.cat, nodesize = 100, ntree = 500, classwt=c(0.1, 0.9))
forest.pred.3.bal <- predict(forest.mod.3.bal, newdata = Test.cat)
roc.curve(Test.cat$outcome, forest.pred.3.bal, plotit = FALSE) # AUC: 0.50

# Predictor: failures + course
# Unbalanced
forest.mod.4 <- randomForest(outcome ~ failures + course, data = Train.cat, nodesize = 100, ntree = 500)
forest.pred.4 <- predict(forest.mod.4, newdata = Test.cat)
roc.curve(Test.cat$outcome, forest.pred.4, plotit = FALSE) # AUC: 0.50

# Balanced
forest.mod.4.bal <- randomForest(outcome ~ failures + course, data = Train.cat, nodesize = 100, ntree = 500, classwt=c(0.1, 0.9))
forest.pred.4.bal <- predict(forest.mod.4.bal, newdata = Test.cat)
roc.curve(Test.cat$outcome, forest.pred.4.bal, plotit = FALSE) # AUC: 0.50

# Predictor: failures + course + higher
# Not balanced
forest.mod.5 <- randomForest(outcome ~ failures + course + higher, data = Train.cat, nodesize = 100, ntree = 500)
forest.pred.5 <- predict(forest.mod.5, newdata = Test.cat)
roc.curve(Test.cat$outcome, forest.pred.5, plotit = FALSE) # AUC: 0.50

# Balanced
forest.mod.5.bal <- randomForest(outcome ~ failures + course + higher, data = Train.cat, nodesize = 100, ntree = 500, classwt=c(0.1, 0.9))
forest.pred.5.bal <- predict(forest.mod.5.bal, newdata = Test.cat)
roc.curve(Test.cat$outcome, forest.pred.5.bal, plotit = FALSE) # AUC: 0.50

# Predictor: failures + course + higher + school
# Not balanced
forest.mod.6 <- randomForest(outcome ~ failures + course + higher + school, data = Train.cat, nodesize = 100, ntree = 500)
forest.pred.6 <- predict(forest.mod.6, newdata = Test.cat)
roc.curve(Test.cat$outcome, forest.pred.6, plotit = FALSE) # AUC: 0.50

# Balanced
forest.mod.6.bal <- randomForest(outcome ~ failures + course + higher + school, data = Train.cat, nodesize = 100, ntree = 500, classwt=c(0.1, 0.9))
forest.pred.6.bal <- predict(forest.mod.6.bal, newdata = Test.cat)
roc.curve(Test.cat$outcome, forest.pred.6.bal, plotit = FALSE) # AUC: 0.50

# All of these models are complete failures.

# For comparison, a random forest using all variables (including G1 and G2)
# Not balanced
forest.mod.7 <- randomForest(outcome ~ ., data = Train.cat, nodesize = 100, ntree = 500)
forest.pred.7 <- predict(forest.mod.7, newdata = Test.cat)
roc.curve(Test.cat$outcome, forest.pred.7, plotit = FALSE) # AUC: 0.718

# Balanced
forest.mod.7.bal <- randomForest(outcome ~ ., data = Train.cat, nodesize = 100, ntree = 500, classwt = c(0.1, 0.9))
forest.pred.7.bal <- predict(forest.mod.7.bal, newdata = Test.cat)
roc.curve(Test.cat$outcome, forest.pred.7.bal, plotit = FALSE) # AUC: 0.698

table(Test.cat$outcome, forest.pred.7) # Confusion matrix
(11+235)/261 # Accuracy: 0.94
((11/(11+14))+(235/(235+1)))/2 # Balanced accuracy: 0.718
11/(11+14) # Specificity: 0.44

# Even with G1 and G2, this really is not a very good model! It's good at predicting who will pass,
# but not at predicting who will fail.

# Random forest does not seem to be a good method with this dataset.

# I've tried four modeling methods (linear regression, logistic regression, decision trees, and random forests)
# and none is effective if they do not include G1 and G2 as predictors.

## MODELING WITH G1 BUT WITHOUT G2

# One last thing to try: educators would like to know which students are at risk of failing as early as possible.
# What if I try building models with G1 but not G2? Would one of these work?

# No need to try linear regression. We already know that the dropouts are not predictable by linear regression.
# And the dropouts are the students we'd most like to predict!

# Try logistic regression.

# G1 only
log.model.7 <- glm(outcome ~ G1, data = Train.cat, family = binomial)
summary(log.model.7) # AIC: 306.72
log.model.7.over <- glm(outcome ~ G1, data = Train.cat.over, family = binomial)
summary(log.model.7.over) # AIC: 1052.7
log.model.7.under <- glm(outcome ~ G1, data = Train.cat.under, family = binomial)
summary(log.model.7.under) # AIC: 106.45
log.model.7.both <- glm(outcome ~ G1, data = Train.cat.both, family = binomial)
summary(log.model.7.both) # AIC: 581.84
log.model.7.syn <- glm(outcome ~ G1, data = Train.cat.syn, family = binomial)
summary(log.model.7.syn) # AIC: 675.68
# Model with lowest AIC uses undersampling; test it
log.pred.7.under <- predict(log.model.7.under, newdata = Test.cat, type = "response")
table(Test.cat$outcome, log.pred.7.under > 0.5) # Confusion matrix
(22+211)/261 # Accuracy: 0.89
((22/(22+3))+(211/(211+25)))/2 # Balanced accuracy: 0.89
22/(22+3) # Specificity: 0.88

# All predictors except G2
log.model.8 <- glm(outcome ~ . -G2, data = Train.cat, family = binomial)
summary(log.model.8) # AIC: 343.73
log.model.8.over <- glm(outcome ~ . -G2, data = Train.cat.over, family = binomial)
summary(log.model.8.over) # AIC: 689.95
log.model.8.under <- glm(outcome ~ . -G2, data = Train.cat.under, family = binomial)
summary(log.model.8.under) # AIC: 144
log.model.8.both <- glm(outcome ~ . -G2, data = Train.cat.both, family = binomial)
summary(log.model.8.both) # AIC: 408.36
log.model.8.syn <- glm(outcome ~ . -G2, data = Train.cat.syn, family = binomial)
summary(log.model.8.syn) # AIC: 535.86
# Model with lowest AIC uses undersampling; test it
log.pred.8.under <- predict(log.model.8.under, newdata = Test.cat, type = "response")
table(Test.cat$outcome, log.pred.8.under > 0.5) # Confusion matrix
(22+176)/261 # Accuracy: 0.76
((22/(22+3))+(176/(176+60)))/2 # Balanced accuracy: 0.81
22/(22+3) # Specificity: 0.88

# Presume that undersampling is the best balancing method.

# Check AIC on G1 + combination of (failures + course + higher + school)
log.model.9.under <- glm(outcome ~ G1 + failures, data = Train.cat.under, family = binomial)
summary(log.model.9.under) # AIC: 109.94
log.model.10.under <- glm(outcome ~ G1 + failures + course, data = Train.cat.under, family = binomial)
summary(log.model.10.under) # AIC: 105.4
log.model.11.under <- glm(outcome ~ G1 + failures + course + higher, data = Train.cat.under, family = binomial)
summary(log.model.11.under) # AIC: 106.97
log.model.12.under <- glm(outcome ~ G1 + failures + course + higher + school, data = Train.cat.under, family = binomial)
summary(log.model.12.under) # AIC: 108.55

# log.model.10.under has lowest AIC, so test this one.
log.pred.10.under <- predict(log.model.10.under, newdata = Test.cat, type = "response")
table(Test.cat$outcome, log.pred.10.under > 0.5) # Confusion matrix
(23+201)/261 # Accuracy: 0.86
((23/(23+2))+(201/(201+35)))/2 # Balanced accuracy: 0.89
23/(23+2) # Specificity: 0.92

# log.model.10.under is a good model: balanced accuracy and specificity are both pretty good.

# Can I do better with decision trees? (Try with different balancing methods.)

# G1 only
tree.mod.8 <- rpart(outcome ~ G1, data = Train.cat, method = "class")
tree.pred.8 <- predict(tree.mod.8, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.8, plotit = FALSE) # AUC: 0.672

tree.mod.8.over <- rpart(outcome ~ G1, data = Train.cat.over, method = "class")
tree.pred.8.over <- predict(tree.mod.8.over, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.8.over, plotit = FALSE) # AUC: 0.854

tree.mod.8.under <- rpart(outcome ~ G1, data = Train.cat.under, method = "class")
tree.pred.8.under <- predict(tree.mod.8.under, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.8.under, plotit = FALSE) # AUC: 0.887 BEST

tree.mod.8.both <- rpart(outcome ~ G1, data = Train.cat.both, method = "class")
tree.pred.8.both <- predict(tree.mod.8.both, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.8.both, plotit = FALSE) # AUC: 0.854

tree.mod.8.syn <- rpart(outcome ~ G1, data = Train.cat.syn, method = "class")
tree.pred.8.syn <- predict(tree.mod.8.syn, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.8.syn, plotit = FALSE) # AUC: 0.854

# All predictors except G2
tree.mod.9 <- rpart(outcome ~ . -G2, data = Train.cat, method = "class")
tree.pred.9 <- predict(tree.mod.9, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.9, plotit = FALSE) # AUC: 0.841

tree.mod.9.over <- rpart(outcome ~ . -G2, data = Train.cat.over, method = "class")
tree.pred.9.over <- predict(tree.mod.9.over, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.9.over, plotit = FALSE) # AUC: 0.871 BEST

tree.mod.9.under <- rpart(outcome ~ . -G2, data = Train.cat.under, method = "class")
tree.pred.9.under <- predict(tree.mod.9.under, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.9.under, plotit = FALSE) # AUC: 0.827

tree.mod.9.both <- rpart(outcome ~ . -G2, data = Train.cat.both, method = "class")
tree.pred.9.both <- predict(tree.mod.9.both, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.9.both, plotit = FALSE) # AUC: 0.854

tree.mod.9.syn <- rpart(outcome ~ . -G2, data = Train.cat.syn, method = "class")
tree.pred.9.syn <- predict(tree.mod.9.syn, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.9.syn, plotit = FALSE) # AUC: 0.868

# G1 + combination of (failures + course + higher + school)

# G1 + failures
tree.mod.10 <- rpart(outcome ~ G1 + failures, data = Train.cat, method = "class")
tree.pred.10 <- predict(tree.mod.10, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.10, plotit = FALSE) # AUC: 0.594

tree.mod.10.over <- rpart(outcome ~ G1 + failures, data = Train.cat.over, method = "class")
tree.pred.10.over <- predict(tree.mod.10.over, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.10.over, plotit = FALSE) # AUC: 0.848

tree.mod.10.under <- rpart(outcome ~ G1 + failures, data = Train.cat.under, method = "class")
tree.pred.10.under <- predict(tree.mod.10.under, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.10.under, plotit = FALSE) # AUC: 0.887 BEST

tree.mod.10.both <- rpart(outcome ~ G1 + failures, data = Train.cat.both, method = "class")
tree.pred.10.both <- predict(tree.mod.10.both, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.10.both, plotit = FALSE) # AUC: 0.848

tree.mod.10.syn <- rpart(outcome ~ G1 + failures, data = Train.cat.syn, method = "class")
tree.pred.10.syn <- predict(tree.mod.10.syn, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.10.syn, plotit = FALSE) # AUC: 0.855

# G1 + failures + course
tree.mod.11 <- rpart(outcome ~ G1 + failures + course, data = Train.cat, method = "class")
tree.pred.11 <- predict(tree.mod.11, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.11, plotit = FALSE) # AUC: 0.692

tree.mod.11.over <- rpart(outcome ~ G1 + failures + course, data = Train.cat.over, method = "class")
tree.pred.11.over <- predict(tree.mod.11.over, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.11.over, plotit = FALSE) # AUC: 0.848

tree.mod.11.under <- rpart(outcome ~ G1 + failures + course, data = Train.cat.under, method = "class")
tree.pred.11.under <- predict(tree.mod.11.under, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.11.under, plotit = FALSE) # AUC: 0.887 BEST

tree.mod.11.both <- rpart(outcome ~ G1 + failures + course, data = Train.cat.both, method = "class")
tree.pred.11.both <- predict(tree.mod.11.both, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.11.both, plotit = FALSE) # AUC: 0.848

tree.mod.11.syn <- rpart(outcome ~ G1 + failures + course, data = Train.cat.syn, method = "class")
tree.pred.11.syn <- predict(tree.mod.11.syn, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.11.syn, plotit = FALSE) # AUC: 0.855

# G1 + failures + course + higher
tree.mod.12 <- rpart(outcome ~ G1 + failures + course + higher, data = Train.cat, method = "class")
tree.pred.12 <- predict(tree.mod.12, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.12, plotit = FALSE) # AUC: 0.727

tree.mod.12.over <- rpart(outcome ~ G1 + failures + course + higher, data = Train.cat.over, method = "class")
tree.pred.12.over <- predict(tree.mod.12.over, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.12.over, plotit = FALSE) # AUC: 0.848

tree.mod.12.under <- rpart(outcome ~ G1 + failures + course + higher, data = Train.cat.under, method = "class")
tree.pred.12.under <- predict(tree.mod.12.under, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.12.under, plotit = FALSE) # AUC: 0.887 BEST

tree.mod.12.both <- rpart(outcome ~ G1 + failures + course + higher, data = Train.cat.both, method = "class")
tree.pred.12.both <- predict(tree.mod.12.both, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.12.both, plotit = FALSE) # AUC: 0.848

tree.mod.12.syn <- rpart(outcome ~ G1 + failures + course + higher, data = Train.cat.syn, method = "class")
tree.pred.12.syn <- predict(tree.mod.12.syn, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.12.syn, plotit = FALSE) # AUC: 0.855

# G1 + failures + course + higher + school
tree.mod.13 <- rpart(outcome ~ G1 + failures + course + higher + school, data = Train.cat, method = "class")
tree.pred.13 <- predict(tree.mod.13, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.13, plotit = FALSE) # AUC: 0.727

tree.mod.13.over <- rpart(outcome ~ G1 + failures + course + higher + school, data = Train.cat.over, method = "class")
tree.pred.13.over <- predict(tree.mod.13.over, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.13.over, plotit = FALSE) # AUC: 0.848

tree.mod.13.under <- rpart(outcome ~ G1 + failures + course + higher + school, data = Train.cat.under, method = "class")
tree.pred.13.under <- predict(tree.mod.13.under, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.13.under, plotit = FALSE) # AUC: 0.887 BEST

tree.mod.13.both <- rpart(outcome ~ G1 + failures + course + higher + school, data = Train.cat.both, method = "class")
tree.pred.13.both <- predict(tree.mod.13.both, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.13.both, plotit = FALSE) # AUC: 0.848

tree.mod.13.syn <- rpart(outcome ~ G1 + failures + course + higher + school, data = Train.cat.syn, method = "class")
tree.pred.13.syn <- predict(tree.mod.13.syn, newdata = Test.cat, type = "class")
roc.curve(Test.cat$outcome, tree.pred.13.syn, plotit = FALSE) # AUC: 0.855

# Test the best decision tree models.
table(Test.cat$outcome, tree.pred.8.under) # Confusion matrix
(22+211)/261 # Accuracy: 0.89
((22/(22+3))+(211/(211+25)))/2 # Balanced accuracy: 0.89
22/(22+3) # Specificity: 0.88
# What does it mean to have a decision tree with only one variable?

table(Test.cat$outcome, tree.pred.9.over) # Confusion matrix
(21+213)/261 # Accuracy: 0.897
((21/(21+4))+(213/(213+23)))/2 # Balanced accuracy: 0.87
21/(21+4) # Specificity: 0.84

table(Test.cat$outcome, tree.pred.10.under) # Confusion matrix
(22+211)/261 # Accuracy: 0.89
((22/(22+3))+(211/(211+25)))/2 # Balanced accuracy: 0.89
22/(22+3) # Specificity: 0.88

table(Test.cat$outcome, tree.pred.11.under) # Confusion matrix
(22+211)/261 # Accuracy: 0.89
((22/(22+3))+(211/(211+25)))/2 # Balanced accuracy: 0.89
22/(22+3) # Specificity: 0.88

table(Test.cat$outcome, tree.pred.12.under) # Confusion matrix
(22+211)/261 # Accuracy: 0.89
((22/(22+3))+(211/(211+25)))/2 # Balanced accuracy: 0.89
22/(22+3) # Specificity: 0.88

table(Test.cat$outcome, tree.pred.13.under) # Confusion matrix
(22+211)/261 # Accuracy: 0.89
((22/(22+3))+(211/(211+25)))/2 # Balanced accuracy: 0.89
22/(22+3) # Specificity: 0.88

# Most of these models (8, 10, 11, 12, 13) produce identical confusion matrixes.
# These are all the models that use undersampling as the balancing method.
# These models are good, but not great.

# Finally, try random forest

# G1 only
# Not balanced
forest.mod.8 <- randomForest(outcome ~ G1, data = Train.cat, nodesize = 100, ntree = 500)
forest.pred.8 <- predict(forest.mod.8, newdata = Test.cat)
roc.curve(Test.cat$outcome, forest.pred.8, plotit = FALSE) # AUC: 0.857

# Balanced
forest.mod.8.bal <- randomForest(outcome ~ G1, data = Train.cat, nodesize = 100, ntree = 500, classwt = c(0.1, 0.9))
forest.pred.8.bal <- predict(forest.mod.8.bal, newdata = Test.cat)
roc.curve(Test.cat$outcome, forest.pred.8.bal, plotit = FALSE) # AUC: 0.857

# All predictors except G2
# Not balanced
forest.mod.9 <- randomForest(outcome ~ . -G2, data = Train.cat, nodesize = 100, ntree = 500)
forest.pred.9 <- predict(forest.mod.9, newdata = Test.cat)
roc.curve(Test.cat$outcome, forest.pred.9, plotit = FALSE) # AUC: 0.50

# Balanced
forest.mod.9.bal <- randomForest(outcome ~ . -G2, data = Train.cat, nodesize = 100, ntree = 500, classwt = c(0.1, 0.9))
forest.pred.9.bal <- predict(forest.mod.9.bal, newdata = Test.cat)
roc.curve(Test.cat$outcome, forest.pred.9.bal, plotit = FALSE) # AUC: 0.50

# G1 + combination of (failures + course + higher + school)

# G1 + failures

# Not balanced
forest.mod.10 <- randomForest(outcome ~ G1 + failures, data = Train.cat, nodesize = 100, ntree = 500)
forest.pred.10 <- predict(forest.mod.10, newdata = Test.cat)
roc.curve(Test.cat$outcome, forest.pred.10, plotit = FALSE) # AUC: 0.627

# Balanced
forest.mod.10.bal <- randomForest(outcome ~ G1 + failures, data = Train.cat, nodesize = 100, ntree = 500, classwt = c(0.1, 0.9))
forest.pred.10.bal <- predict(forest.mod.10.bal, newdata = Test.cat)
roc.curve(Test.cat$outcome, forest.pred.10.bal, plotit = FALSE) # AUC: 0.685

# G1 + failures + course

# Not balanced
forest.mod.11 <- randomForest(outcome ~ G1 + failures + course, data = Train.cat, nodesize = 100, ntree = 500)
forest.pred.11 <- predict(forest.mod.11, newdata = Test.cat)
roc.curve(Test.cat$outcome, forest.pred.11, plotit = FALSE) # AUC: 0.598

# Balanced
forest.mod.11.bal <- randomForest(outcome ~ G1 + failures + course, data = Train.cat, nodesize = 100, ntree = 500, classwt = c(0.1, 0.9))
forest.pred.11.bal <- predict(forest.mod.11.bal, newdata = Test.cat)
roc.curve(Test.cat$outcome, forest.pred.11.bal, plotit = FALSE) # AUC: 0.538

# G1 + failures + course + higher

# Not balanced
forest.mod.12 <- randomForest(outcome ~ G1 + failures + course + higher, data = Train.cat, nodesize = 100, ntree = 500)
forest.pred.12 <- predict(forest.mod.12, newdata = Test.cat)
roc.curve(Test.cat$outcome, forest.pred.12, plotit = FALSE) # AUC: 0.812

# Balanced
forest.mod.12.bal <- randomForest(outcome ~ G1 + failures + course + higher, data = Train.cat, nodesize = 100, ntree = 500, classwt = c(0.1, 0.9))
forest.pred.12.bal <- predict(forest.mod.12.bal, newdata = Test.cat)
roc.curve(Test.cat$outcome, forest.pred.12.bal, plotit = FALSE) # AUC: 0.821

# G1 + failures + course + higher + school

# Not balanced
forest.mod.13 <- randomForest(outcome ~ G1 + failures + course + higher + school, data = Train.cat, nodesize = 100, ntree = 500)
forest.pred.13 <- predict(forest.mod.13, newdata = Test.cat)
roc.curve(Test.cat$outcome, forest.pred.13, plotit = FALSE) # AUC: 0.674

# Balanced
forest.mod.13.bal <- randomForest(outcome ~ G1 + failures + course + higher + school, data = Train.cat, nodesize = 100, ntree = 500, classwt = c(0.1, 0.9))
forest.pred.13.bal <- predict(forest.mod.13.bal, newdata = Test.cat)
roc.curve(Test.cat$outcome, forest.pred.13.bal, plotit = FALSE) # AUC: 0.749

# Test the best random forest models
table(Test.cat$outcome, forest.pred.8.bal) # Confusion matrix
(19+225)/261 # Accuracy: 0.93
((19/(19+6))+(225/(225+11)))/2 # Balanced accuracy: 0.86
19/(19+6) # Specificity: 0.76

table(Test.cat$outcome, forest.pred.12.bal) # Confusion matrix
(17+227)/261 # Accuracy: 0.93
((17/(17+8))+(227/(227+9)))/2 # Balanced accuracy: 0.82
17/(17+8) # Specificity: 0.68

# The first one is ok, not that great. Besides, what does it even mean to use a decision tree based
# model when there is only one variable?

## Try predicting G1, then G2, then G3 as categorical variables

# Convert G1 to factor with levels "fail", "pass"

d_math_catG1 <- d_math %>% mutate(G1outcome=ifelse(G1<8,"fail","pass")) %>%
  select(-G1)
d_port_catG1 <- d_port %>% mutate(G1outcome=ifelse(G1<8,"fail","pass")) %>%
  select(-G1)
d_total_catG1 <- rbind(d_math_catG1, d_port_catG1)
d_total_catG1$G1outcome <- as.factor(d_total_catG1$G1outcome)

# Split training and test set
set.seed(44)
split <- sample.split(d_total_catG1$G1outcome, SplitRatio = 0.75)
Train.catG1 <- subset(d_total_catG1, split == TRUE) 
Test.catG1 <- subset(d_total_catG1, split == FALSE)
Test.catG1 <- Test.catG1[-which(Test.catG1$Fedu == "None"), ]

# Examine balance in training set
table(Train.catG1$G1outcome)
# Balance training set by oversampling "fail"
Train.catG1.over <- ovun.sample(G1outcome ~., data = Train.catG1, method = "over", N=1386)$data
# Balance training set by undersampling "pass"
Train.catG1.under <- ovun.sample(G1outcome ~., data = Train.catG1, method = "under", N=180)$data
# Balance training set by both over- and undersampling
Train.catG1.both <- ovun.sample(G1outcome ~., data = Train.catG1, method = "both", p=0.5, N=783, seed = 1)$data
# Balance training set by creating synthetic minority class observations
Train.catG1.syn <- ROSE(G1outcome ~., data = Train.catG1, seed = 1)$data

# Baseline 88.5% = pass
nrow(d_total_catG1[d_total_catG1$G1outcome == "pass", ])/nrow(d_total_catG1)

# Try predicting G1 with logistic regression

# Using unbalanced dataset
log.model.13 <- glm(G1outcome ~ . -G2 -G3, data = Train.catG1, family = binomial)
summary(log.model.13) # AIC: 502.57
log.pred.13 <- predict(log.model.13, newdata = Test.catG1, type = "response")
table(Test.catG1$G1outcome, log.pred.13 > 0.5) # Confusion matrix
(12+223)/261 # Accuracy: 0.900
((12/(12+18))+(223/(223+6)))/2 # Balanced accuracy: 0.687
12/(12+18) # Specificity: 0.40

# Using dataset with "fail" oversampled
log.model.13.over <- glm(G1outcome ~ . -G2 -G3, data = Train.catG1.over, family = binomial)
summary(log.model.13.over) # AIC: 1261.9
log.pred.13.over <- predict(log.model.13.over, newdata = Test.catG1, type = "response")
table(Test.catG1$G1outcome, log.pred.13.over > 0.5) # Confusion matrix
(21+178)/261 # Accuracy: 0.762
((21/(21+9))+(178/(178+53)))/2 # Balanced accuracy: 0.735
21/(21+9) # Specificity: 0.70

# Using dataset with "pass" undersampled
log.model.13.under <- glm(G1outcome ~ . -G2 -G3, data = Train.catG1.under, family = binomial)
summary(log.model.13.under) # AIC: 1261.9
log.pred.13.under <- predict(log.model.13.under, newdata = Test.catG1, type = "response")
table(Test.catG1$G1outcome, log.pred.13.under > 0.5) # Confusion matrix
(22+161)/261 # Accuracy: 0.701
((22/(22+8))+(161/(161+68)))/2 # Balanced accuracy: 0.718
22/(22+8) # Specificity: 0.733

# Using dataset with both over- and undersampling
log.model.13.both <- glm(G1outcome ~ . -G2 -G3, data = Train.catG1.both, family = binomial)
summary(log.model.13.both) # AIC: 767.26
log.pred.13.both <- predict(log.model.13.both, newdata = Test.catG1, type = "response")
table(Test.catG1$G1outcome, log.pred.13.both > 0.5) # Confusion matrix
(20+163)/261 # Accuracy: 0.701
((20/(20+10))+(163/(163+66)))/2 # Balanced accuracy: 0.689
20/(20+10) # Specificity: 0.667

# Using dataset with synthetic "fail" observations
log.model.13.syn <- glm(G1outcome ~ . -G2 -G3, data = Train.catG1.syn, family = binomial)
summary(log.model.13.syn) # AIC: 766.83
log.pred.13.syn <- predict(log.model.13.syn, newdata = Test.catG1, type = "response")
table(Test.catG1$G1outcome, log.pred.13.syn > 0.5) # Confusion matrix
(21+163)/261 # Accuracy: 0.705
((21/(21+9))+(163/(163+66)))/2 # Balanced accuracy: 0.706
21/(21+9) # Specificity: 0.70

# No logistic regression model beats the baseline

# Try predicting G1 with decision trees

# Unbalanced dataset
tree.mod.14 <- rpart(G1outcome ~ . -G2 -G3, data = Train.catG1, method = "class")
tree.pred.14 <- predict(tree.mod.14, newdata = Test.catG1, type = "class")
roc.curve(Test.catG1$G1outcome, tree.pred.14, plotit = FALSE) # AUC: 0.603

# Balanced by oversampling
tree.mod.14.over <- rpart(G1outcome ~ . -G2 -G3, data = Train.catG1.over, method = "class")
tree.pred.14.over <- predict(tree.mod.14.over, newdata = Test.catG1, type = "class")
roc.curve(Test.catG1$G1outcome, tree.pred.14.over, plotit = FALSE) # AUC: 0.728 BEST

# Balanced by undersampling
tree.mod.14.under <- rpart(G1outcome ~ . -G2 -G3, data = Train.catG1.under, method = "class")
tree.pred.14.under <- predict(tree.mod.14.under, newdata = Test.catG1, type = "class")
roc.curve(Test.catG1$G1outcome, tree.pred.14.under, plotit = FALSE) # AUC: 0.721

# Balanced by both over- and undersampling
tree.mod.14.both <- rpart(G1outcome ~ . -G2 -G3, data = Train.catG1.both, method = "class")
tree.pred.14.both <- predict(tree.mod.14.both, newdata = Test.catG1, type = "class")
roc.curve(Test.catG1$G1outcome, tree.pred.14.both, plotit = FALSE) # AUC: 0.673

# Balanced with synthetic data
tree.mod.14.syn <- rpart(G1outcome ~ . -G2 -G3, data = Train.catG1.syn, method = "class")
tree.pred.14.syn <- predict(tree.mod.14.syn, newdata = Test.catG1, type = "class")
roc.curve(Test.catG1$G1outcome, tree.pred.14.syn, plotit = FALSE) # AUC: 0.681

# Test the best one
table(Test.catG1$G1outcome, tree.pred.14.over) # Confusion matrix
(21+173)/261 # Accuracy: 0.743
((21/(21+9))+(173/(173+56)))/2 # Balanced accuracy: 0.728
21/(21+9) # Specificity: 0.70

# No decision tree model beats the baseline

# Try random forest

# Not balanced
forest.mod.14 <- randomForest(G1outcome ~ . -G2 -G3, data = Train.catG1, nodesize = 100, ntree = 500)
forest.pred.14 <- predict(forest.mod.14, newdata = Test.catG1)
roc.curve(Test.catG1$G1outcome, forest.pred.14, plotit = FALSE) # AUC: 0.50

# Balanced
forest.mod.14.bal <- randomForest(G1outcome ~ . -G2 -G3, data = Train.catG1, nodesize = 100, ntree = 500, classwt = c(0.1, 0.9))
forest.pred.14.bal <- predict(forest.mod.14.bal, newdata = Test.catG1)
roc.curve(Test.catG1$G1outcome, forest.pred.14.bal, plotit = FALSE) # AUC: 0.50

# With AUC at 0.5, no random forest beats the baseline.

# With information in this dataset, we CANNOT predict G1!

# Convert G2 to factor

d_math_catG2 <- d_math %>% mutate(G2outcome=ifelse(G2<8,"fail","pass")) %>%
  select(-G2)
d_port_catG2 <- d_port %>% mutate(G2outcome=ifelse(G2<8,"fail","pass")) %>%
  select(-G2)
d_total_catG2 <- rbind(d_math_catG2, d_port_catG2)
d_total_catG2$G2outcome <- as.factor(d_total_catG2$G2outcome)

# Split training and test set
set.seed(49)
split <- sample.split(d_total_catG2$G2outcome, SplitRatio = 0.75)
Train.catG2 <- subset(d_total_catG2, split == TRUE) 
Test.catG2 <- subset(d_total_catG2, split == FALSE)
Test.catG2 <- Test.catG2[-which(Test.catG2$Medu == "None"), ]
Test.catG2 <- Test.catG2[-which(Test.catG2$Fedu == "None"), ]

# Examine balance in training set
table(Train.catG2$G2outcome)
# Balance training set by oversampling "fail"
Train.catG2.over <- ovun.sample(G2outcome ~., data = Train.catG2, method = "over", N=1420)$data
# Balance training set by undersampling "pass"
Train.catG2.under <- ovun.sample(G2outcome ~., data = Train.catG2, method = "under", N=146)$data
# Balance training set by both over- and undersampling
Train.catG2.both <- ovun.sample(G2outcome ~., data = Train.catG2, method = "both", p=0.5, N=783, seed = 1)$data
# Balance training set by creating synthetic minority class observations
Train.catG2.syn <- ROSE(G2outcome ~., data = Train.catG2, seed = 1)$data

# Baseline 90.7% = pass
nrow(d_total_catG2[d_total_catG2$G2outcome == "pass", ])/nrow(d_total_catG2)

# Try predicting G2 with logistic regression

# Using unbalanced dataset
log.model.14 <- glm(G2outcome ~ . -G3, data = Train.catG2, family = binomial)
summary(log.model.14) # AIC: 304.18
log.pred.14 <- predict(log.model.14, newdata = Test.catG2, type = "response")
table(Test.catG2$G2outcome, log.pred.14 > 0.5) # Confusion matrix
(11+229)/261 # Accuracy: 0.919
((11/(11+13))+(229/(229+8)))/2 # Balanced accuracy: 0.712
11/(11+13) # Specificity: 0.458

# Using dataset with "fail" oversampled
log.model.14.over <- glm(G2outcome ~ . -G3, data = Train.catG2.over, family = binomial)
summary(log.model.14.over) # AIC: 550.53
log.pred.14.over <- predict(log.model.14.over, newdata = Test.catG2, type = "response")
table(Test.catG2$G2outcome, log.pred.14.over > 0.5) # Confusion matrix
(14+214)/261 # Accuracy: 0.874
((14/(14+10))+(214/(214+23)))/2 # Balanced accuracy: 0.743
14/(14+10) # Specificity: 0.583

# Using dataset with "pass" undersampled
log.model.14.under <- glm(G2outcome ~ . -G3, data = Train.catG2.under, family = binomial)
summary(log.model.14.under) # AIC: 142
log.pred.14.under <- predict(log.model.14.under, newdata = Test.catG2, type = "response")
table(Test.catG2$G2outcome, log.pred.14.under > 0.5) # Confusion matrix
(19+170)/255 # Accuracy: 0.741
((19/(19+5))+(170/(170+61)))/2 # Balanced accuracy: 0.764
19/(19+5) # Specificity: 0.792

# Using dataset with both over- and undersampling
log.model.14.both <- glm(G2outcome ~ . -G3, data = Train.catG2.both, family = binomial)
summary(log.model.14.both) # AIC: 144
log.pred.14.both <- predict(log.model.14.both, newdata = Test.catG2, type = "response")
table(Test.catG2$G2outcome, log.pred.14.both > 0.5) # Confusion matrix
(10+202)/255 # Accuracy: 0.831
((10/(10+14))+(202/(202+29)))/2 # Balanced accuracy: 0.646
10/(10+14) # Specificity: 0.412

# Using dataset with synthetic "fail" observations
log.model.14.syn <- glm(G2outcome ~ . -G3, data = Train.catG2.syn, family = binomial)
summary(log.model.14.syn) # AIC: 323.4
log.pred.14.syn <- predict(log.model.14.syn, newdata = Test.catG2, type = "response")
table(Test.catG2$G2outcome, log.pred.14.syn > 0.5) # Confusion matrix
(11+207)/255 # Accuracy: 0.855
((11/(11+13))+(207/(207+24)))/2 # Balanced accuracy: 0.677
11/(11+13) # Specificity: 0.458

# No logistic regression model beats the baseline

# Try predicting G2 with decision trees

# Unbalanced dataset
tree.mod.15 <- rpart(G2outcome ~ . -G3, data = Train.catG2, method = "class")
tree.pred.15 <- predict(tree.mod.15, newdata = Test.catG2, type = "class")
roc.curve(Test.catG2$G2outcome, tree.pred.15, plotit = FALSE) # AUC: 0.772

# Balanced by oversampling
tree.mod.15.over <- rpart(G2outcome ~ . -G3, data = Train.catG2.over, method = "class")
tree.pred.15.over <- predict(tree.mod.15.over, newdata = Test.catG2, type = "class")
roc.curve(Test.catG2$G2outcome, tree.pred.15.over, plotit = FALSE) # AUC: 0.858

# Balanced by undersampling
tree.mod.15.under <- rpart(G2outcome ~ . -G3, data = Train.catG2.under, method = "class")
tree.pred.15.under <- predict(tree.mod.15.under, newdata = Test.catG2, type = "class")
roc.curve(Test.catG2$G2outcome, tree.pred.15.under, plotit = FALSE) # AUC: 0.863 BEST

# Balanced by both over- and undersampling
tree.mod.15.both <- rpart(G2outcome ~ . -G3, data = Train.catG2.both, method = "class")
tree.pred.15.both <- predict(tree.mod.15.both, newdata = Test.catG2, type = "class")
roc.curve(Test.catG2$G2outcome, tree.pred.15.both, plotit = FALSE) # AUC: 0.814

# Balanced with synthetic data
tree.mod.15.syn <- rpart(G2outcome ~ . -G3, data = Train.catG2.syn, method = "class")
tree.pred.15.syn <- predict(tree.mod.15.syn, newdata = Test.catG2, type = "class")
roc.curve(Test.catG2$G2outcome, tree.pred.15.syn, plotit = FALSE) # AUC: 0.795

# Test the best one
table(Test.catG2$G2outcome, tree.pred.15.under) # Confusion matrix
(20+206)/255 # Accuracy: 0.886
((20/(20+4))+(206/(206+25)))/2 # Balanced accuracy: 0.863
20/(20+4) # Specificity: 0.833

# This model is almost as good as baseline. What does it look like?
prp(tree.mod.15.under)

# So the best decision tree model for G2 prediction isn't as good as baseline. 
# Even considering all variables, this best model makes decision just based on G1.
# Really, only interim grades seem to be predictive at all.

# Try random forest

# Not balanced
forest.mod.15 <- randomForest(G2outcome ~ . -G3, data = Train.catG2, nodesize = 100, ntree = 500)
forest.pred.15 <- predict(forest.mod.15, newdata = Test.catG2)
roc.curve(Test.catG2$G2outcome, forest.pred.15, plotit = FALSE) # AUC: 0.50

# Balanced
forest.mod.15.bal <- randomForest(G2outcome ~ . -G3, data = Train.catG2, nodesize = 100, ntree = 500, classwt = c(0.1, 0.9))
forest.pred.15.bal <- predict(forest.mod.15.bal, newdata = Test.catG2)
roc.curve(Test.catG2$G2outcome, forest.pred.15.bal, plotit = FALSE) # AUC: 0.50

# Nope.