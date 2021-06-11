
# Let's start by importing the file GSS.csv, 
# which is saved in my working directory.
# If the file you are using is not in your
# working directory, you will need to specify
# the full path to the file, including the file
# name.
GSS <- read.csv("GSS.csv")

# Let's take a look at the data. We'll look at the first 10 cases.
head(GSS,10)

# Since we will only be using one data file
# (or dataframe) for this project, let's go
# ahead and "attach" the dataframe so RStudio
# knows where to look for the variables.
attach(GSS)

# If we didn't attach the dataframe, we would have
# to use something like this: DATAFRAME$VARIABLE.
# For example, if we want to examine the first 10 
# observations of the age variable:
head(GSS$age,10)

# But since we attached the dataframe, we don't have to
# keep telling RStudio which datafame we are using...
# with one major exception: When we modify or create a
# variable.
# But let's look at the number of responses by year.
table(year)

detach(GSS)

# From here I notice a few important details:
# 1) There is a blank row where the year is missing.
# 2) There is was probably text or a caption in the 
#    file because there is a year called "Source."
# Let's delete these. Since year is a numeric variable,
# one of the easiest ways to fix the issues is to first
# tell R to treat year as a numeric variable.
# You will get a warning message -- but you can ignore
# this.

GSS$year<-as.numeric(GSS$year)

# Let's look at year now that we've modified it:
table(GSS$year)

# Notice now that only the numeric years show up in the 
# table. But... Remember that error message above?
# We can examine how many missing observations (NAs)
# are present using the command sum(is.na(variable name)).

attach(GSS)
sum(is.na(year))

# There are two missing observations (which correspond
# to missing observation we saw before and the "Source"
# observation.

# I want to mention that you can also put ! in front of
# many commands to indicate "NOT". For example, !is.na
# means "Not Missing":
sum(!is.na(year))

# There are 34,809 observations that are not missing year.

# Another way to get this information in a nicer version
# is to use an add-on package called summarytools.
# These add-on packages are called libraries.
# You can load these packages by using require(),
# or library(). "Require" will download the package
# if it is not already installed on your computer. "Library"
# will not:
install.packages("summarytools")
require(summarytools)

# We will now use the freq() command
freq(year)

# Now let's drop the two missing observations in year.
# There are few ways to do this, but I'll start with
# the subset command.

# This is a good time to mention that R has a lot of
# helpful documentation, including syntax. Click the
# Help tab to the right and search for "subset".
# The basic syntax for subset is subset(x, subset, ...),
# where x is the source file, and the subset is the condition
# to be met:
GSS<-subset(GSS,!is.na(GSS$year))
detach(GSS)

attach(GSS)
freq(year)

# Let's look at the frequencies for the other variables.
# The variable "age" is the age of the respondent:
freq(age)
detach(GSS)

# So here we can see another type of missing value...
# variable levels called "Don't know" or "No answer".
# Let's tell R that these values are missing (NAs):
GSS$age[GSS$age=="Don't know"|GSS$age=="No answer"]<-NA

freq(GSS$age)

# While we're at it, let's go ahead and change "89 or older"
# to 89 so that we can treat the variable as numeric:
GSS$age[GSS$age=="89 or older"]<-89
freq(GSS$age)

# Don't forget to now tell RStudio that this is a
# numeric variable:
GSS$age<-as.numeric(GSS$age)

# Now let's take a look at the distribution of age.
# A quick way to do this is to use a histogram:
hist(GSS$age)

# We can see that the data appear skewed, as there
# is a long tail to the right.
# We can also look at this using a few different
# commands, but my favorite comes from the
# summarytools library:
descr(GSS$age)

# Let's go ahead and look at the rest of the variables:

# Childs -- the number of children that the respondent has
freq(GSS$childs)

# Let's do the same to treat "childs" as numeric:
GSS$childs[GSS$childs=="Eight or more"]<-8
GSS$childs<-as.numeric(GSS$childs)

freq(GSS$sex)
freq(GSS$race)

# Colrac -- Should a racist be allowed to teach at a college?
freq(GSS$colrac)

# Librac -- Should racist books be allowed in the library?
freq(GSS$librac)

# We will use colrac for this demonstration. You can try using
# librac for practice on your own.

# Question: Does the attitude towards racist teachers vary by age?
# Let's look at the mean age for just people that think racist
# teachers should be allowed:
descr(GSS$age[GSS$colrac=="Allowed"])

# Now let's look at the mean age for just people that think racist
# teachers should not be allowed:
descr(GSS$age[GSS$colrac=="Not allowed"])

# We could also use the aggregate function to get just
# the mean by level:
aggregate(GSS$age,by=list(GSS$colrac),mean,na.rm=TRUE)

# We could also look at the median, using just the
# aggregate function:
aggregate(GSS$age,by=list(GSS$colrac),median,na.rm=TRUE)

# Both the mean and median suggest that younger people
# tended to think that racist teachers should be allowed.
# But is this difference statistically significant?
# We can use the t-test to compare using the t.test()
# command
t.test(age ~ colrac, data=GSS)

# We could also look at the differences in age graphically:
table(GSS$colrac)
ggboxplot(GSS, x = "colrac",y = "age",
          color = "colrac",
          xlab="Should racists be allowed to teach on campus?",
          ylab="Age")+
  theme(legend.position = "none")+
  ggsave("age_boxplot.png",dpi=300)
  

# Let's look at childs now:
hist(GSS$childs)
t.test(childs ~ colrac, data=GSS)

# Here it is more readily apparent that the data are
# skewed, so a more appropriate test is the
# Wilcoxon Rank Sum test, which is a non-parametric
# test (used when data are skewed):
wilcox.test(childs ~ colrac, data=GSS)

# Let's look at that graphically, too.
ggboxplot(GSS, x = "colrac",y = "childs",
          color = "colrac",
          xlab="Should racists be allowed to teach on campus?",
          ylab="Number of Children")+
  theme(legend.position = "none")+
  ggsave("childs_boxplot.png",dpi=300)

# Here we see that there is a statistical significant 
# difference in the number of children between opinions,
# but in a practical sense, there really isn't a difference.
# This is an effect of a very large sample size.
# Let's take a look when we only use data from 2018.

# Here, I'm going to subset the data using a different
# command, into a new file:
GSS2<-GSS[which(GSS$year==2018),]

# Let's re-run the t-test on this smaller sample:
hist(GSS2$childs)
t.test(childs ~ colrac, data=GSS2)
# Here it is more readily apparent that the data are
# skewed, so a more appropriate test is the
# Wilcoxon Rank Sum test, which is a non-parametric
# test (used when data are skewed):
wilcox.test(childs ~colrac,data=GSS2)

# Notice that the two p-values vary between tests.

# Rather than subsetting the data to a single year,
# we could also get a smaller sample size by using
# the command slice_sample() in the dplyr library:
install.packages("dplyr")
require(dplyr)
GSS3 <- GSS %>% slice_sample(n=6962)

# Let's take a look at year again for this smaller sample:
freq(GSS3$year)

# And we can again look at the histogram for childs:
hist(GSS3$childs)

# To demonstrate that the random sample is approximately
# representative of the larger sample, we could also look
# at the frequency of men and women:
freq(GSS$sex)
freq(GSS3$sex)

# Question: Does race affect your views on racist teachers?
ctable(GSS3$race,GSS3$colrac)

# This certainly suggests that White individuals are more
# tolerant of racist teachers. But is the difference statistically
# significant? We can use chi-squared to see if this is the
# case. There are at least two ways to find this value:
ctable(GSS3$race,GSS3$colrac,chisq=TRUE)
chisq.test(GSS3$race,GSS3$colrac)

# But our data table is more of a rectangle... so the chi-square
# can be harder to interpret. All we know is that at least one of 
# the races hold different beliefs. (In this case, it is readily
# apparent... but sometimes it is not as clear).
# There are a couple of ways we could approach this...
# 1) Create a new "binary" variable (two-levels):
GSS3$white<-ifelse(
  GSS3$race=="White","White","Non-White"
)

ctable(GSS3$white,GSS3$colrac,chisq=TRUE)
chisq.test(GSS3$white,GSS3$colrac)

# Now it is easier to interpret, that there is a significant
# difference between the two group (Over the course of the survey,
# whites were significantly more tolerant of racist teachers than
# non-whites).

# 2) Look at pairwise comparisons between races:
# For this, I'll use logistic regression, and then look
# at the pairwise differences. For this, we will use another
# add-on package: multcomp
install.packages("multcomp")
require(multcomp)

# We need to tell R that race is a categorical variable (a factor).
# While we are at it, let's also define colrac as a factor.
# We have to do this because regressions must use numbers like
# 1=Yes and 0=No.
GSS3$race<-as.factor(GSS3$race)
GSS3$colrac<-as.factor(GSS3$colrac)

logistic<-glm(colrac~race,family=binomial,data=GSS3)
comps <- glht(logistic, linfct = mcp(race = "Tukey"))
summary(comps)

# From these pairwise comparisons, we can see there is a significant
# difference in the odds (likelihood) of a white person being tolerant
# to a racist teacher on campus, compared to Blacks(p<0.001) and
# Other races (p=0.005). Logistic regression is a special type of
# regression -- and is considered an "advanced" method... so you
# most likely will not run into in a basic statistics course. For now,
# a chi-square is probably your best bet.
# Let's try it again with sex:
ctable(GSS3$sex,GSS3$colrac,chisq=TRUE)
chisq.test(GSS3$white,GSS3$colrac)

# From this, we can conclude that men are significantly more likely
# to be tolerant of racist teachers on campus.


# Now it's your turn... Using the librac variable...

# Is there a difference in age between opinions on racist library books?
t.test(y~x)

# Is there a difference in opinion between races?
ctable(x,y,chisq = TRUE)