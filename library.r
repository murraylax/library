library("psych");
library("MASS");
library("lmtest")
library("pscl")

data <- read.csv("library_results.csv");

v.class <- 2-data$Q26;
encourage <- data$Q28;
v.encourage.occ <- (encourage < 3) + 0; # Union of Occasionally and Frequently
v.encourage.freq <- (encourage < 2) + 0; # Just Frequently
list <- data$Q37;
v.list.occ <- (list < 3) + 0; # Union of Occasionally and Frequently
v.list.freq <- (list < 2) + 0; # Just Frequently
college <- data$Q17;
v.cba <- (college == 1) + 0;
v.csh <- (college == 3) + 0;
v.semesters <- data$Q38;
gpa <- data$Q40;
gpa[gpa==1] <- NA;
gpa <- 18 - gpa;
v.gpa <- gpa*0.1 + 2.35;
v.female <- data$Q7 -1;
v.age <- data$Q8 + 16;
v.dorm <- (data$Q2==1) + 0;
v.dorm.ever <- (data$Q50==1)+0;
v.act <- 22 - data$Q13 + 15;
v.parents.somecollege <- (data$Q9 > 2) + 0; # Some college or greater
v.parents.fourcollege <- (data$Q9 > 4) + 0; # Four year degree or greater
v.nonwhite <- (data$Q47 != 5) + 0;
v.computerconf <- (4-data$Q48);
v.computerconf.bin <- (data$Q48 < 3) + 0;

# ############################################## #
# Information seeking behavior outcome variables
# ############################################## #
# Get help from librarian or reference desk: 0-4 scale
v.gethelp <- 5-data$Q6_1;
# Perform online searches for articles: 0-4 scale
v.onlinesearch <- 5-data$Q6_2;
# Browse physical copies of books or periodicals
v.browse <- 5-data$Q6_3;

v.askq <- 4 - data$Q22;
v.howoften <- data$Q5 - 1;
v.uselib <- (data$Q4==1) + 0;
v.usecampus <- (data$Q4==2) + 0;
ebsco <- data$Q29;
ebsco[ebsco==6] <- 5;
v.ebsco <- 5-ebsco;
subject <- data$Q31
subject[subject==6] <- 5
v.subject <- 5-subject
v.searchdata <- pmax(v.subject,v.ebsco);


# Use books for research 0-4 scale
v.res.books <- 5 - data$Q23_1;
# Use scholarly articles for research 0-4 scale
v.res.articles <- 5 - data$Q23_2;
# Use popular press articles for research: 0-4 scale
v.res.poppress <- 5 - data$Q23_3;
# Use online blogs, wikipedia, etc: 0-4 scale
v.res.wiki <- 5 - data$Q23_4;

# Where students start their search
v.start.lib <- (data$Q34>3 & data$Q34<7) + 0;
v.start.popsearch <- (data$Q34==1 | data$Q34==3 | data$Q34==7) + 0;
v.start.googlescholar <- (data$Q34==2) + 0;
v.start.scholar <- ((data$Q34>3 & data$Q34<7) | (data$Q34==2)) + 0;


# ################################################################## #
# ##### Explanatory variables for information seeking behavior ##### #
# ################################################################## #
# Dummy for whether or not a librarian visited their class
# College (dummies for CBA and CSH (College of Science and Health), CLS is the baseline)
# How much professors encouraged students to use library search tools (Dummies for occasionally and frequently, rarely or never is the baseline)
# How often professors give students a reference list to assist with research (Dummies for occasionally and frequently, rarely or never is the baseline)
# Age  (ratio)
# GPA
# ACT score
# parents' highest level of education (dummies for some college and four year college - less than college is the baseline)
# Race (dummy for non-white)

v.explain <- cbind(v.encourage.occ,v.list.occ,v.class,v.cba,v.csh,v.age,v.act,v.parents.fourcollege,v.female,v.nonwhite)
lm.gethelp <- lm(v.gethelp ~ v.explain)
lm.onlinesearch <- lm(v.onlinesearch ~ v.explain)
lm.browse <- lm(v.browse ~ v.explain)
lm.res.books <- lm(v.res.books ~ v.explain);
lm.res.articles <- lm(v.res.articles ~ v.explain);
lm.res.poppress <- lm(v.res.poppress ~ v.explain);
lm.res.wiki <- lm(v.res.wiki ~ v.explain);
lm.ebsco <- lm(v.ebsco ~ v.explain);
lm.subject <- lm(v.subject ~ v.explain);
lm.searchdata <- lm(v.searchdata ~ v.explain)
lm.start.lib <- lm(v.start.lib ~ v.explain)
lm.start.popsearch <- lm(v.start.popsearch ~ v.explain);
lm.start.googlescholar <- lm(v.start.googlescholar ~ v.explain)

#Summary statistics for attributes students value
tb_relevant <- table(data$Q36_6); #Relevant subject matter
med_relevant <- interp.median(data$Q36_6);
tb_fulltext <- table(data$Q36_1); #Full text available online
med_fulltext <- interp.median(data$Q36_1);
tb_understand <- table(data$Q36_8); #Understand content
med_understand <- interp.median(data$Q36_8);
tb_recent <- table(data$Q36_2); #Recent pub
med_recent <- interp.median(data$Q36_2);
tb_authorrep <- table(data$Q36_4); #Author reputation
med_authorrep <- interp.median(data$Q36_4);
tb_peerrev <- table(data$Q36_3); #Peer reviewed
med_peerrev <- interp.median(data$Q36_3);
tb_sourcerep <- table(data$Q36_5); #Source reputation
med_sourcerep <- interp.median(data$Q36_5);
tb_avail <- table(data$Q36_7); #Physical availability 
med_avail <- interp.median(data$Q36_7);

## Logistic regression for a scholarly starting point
idx <- complete.cases(cbind(v.start.scholar,v.explain));
lm.start.scholar <- glm(v.start.scholar[idx] ~ v.explain[idx,], family=binomial(logit))
loglik.start.scholar <- logLik(lm.start.scholar)
lrtest.start.scholar <- lrtest(lm.start.scholar)
pR2.start.scholar <- pR2(lm.start.scholar)

## Ordered logit for electronic databases
idx <- complete.cases(cbind(v.searchdata,v.explain))
lm.elecdatabase <- polr(factor(v.searchdata[idx]) ~ v.explain[idx,])
loglik.elecdatabase <- logLik(lm.elecdatabase)
lrtest.elecdatabase <- lrtest(lm.elecdatabase)
pR2.elecdatabase <- pR2(lm.elecdatabase)


## Ordered logit for sources students use
## Articles
idx <- complete.cases(cbind(v.res.articles, v.explain))
lm.res.articles <- polr(factor(v.res.articles)[idx] ~ v.explain[idx,])
loglik.res.articles <- logLik(lm.res.articles)
lrtest.res.articles <- lrtest(lm.res.articles)
pR2.res.articles <- pR2(lm.res.articles)

# Wikipedia
idx <- complete.cases(cbind(v.res.wiki,v.explain))
lm.res.wiki <- polr(factor(v.res.wiki)[idx] ~ v.explain[idx,]);
loglik.res.wiki <- logLik(lm.res.wiki)
lrtest.res.wiki <- lrtest(lm.res.wiki)
pR2.res.wiki <- pR2(lm.res.wiki)

#Books
idx <- complete.cases(cbind(v.res.books,v.explain))
lm.res.books <- polr(factor(v.res.books)[idx] ~ v.explain[idx,]);
lrtest.res.books <- lrtest(lm.res.books)
pR2.res.books <- pR2(lm.res.books)

# Popular press
idx <- complete.cases(cbind(v.res.poppress, v.explain))
lm.res.poppress <- polr(factor(v.res.poppress)[idx] ~ v.explain[idx,]);
lrtest.res.poppress <- lrtest(lm.res.poppress)
pR2.res.poppress <- pR2(lm.res.poppress)

## Outcome variables for ordered logit for attributes
v.val.relevant <- data$Q36_6 #Relevant subject matter
v.val.fulltext <- data$Q36_1 #Full text available online
v.val.understand <- data$Q36_8 #Understand content
v.val.recent <- data$Q36_2; #Recent pub
v.val.authorrep <- data$Q36_4; #Author rep
v.val.peerrev <- data$Q36_3; # Peer reviewed
v.val.sourcerep <- data$Q36_5; # Source reputation
v.val.avail <- data$Q36_7 # Source reputation

## Ordered logit for attributes
## Also make nice table for copying and pasting (in variable 'tab')
tab <- 1:10

# Relevant source
idx <- complete.cases(cbind(v.val.relevant, v.explain))
lmm <- polr(factor(v.val.relevant)[idx] ~ v.explain[idx,]);
lm.val.relevant <- lmm;
lrtest.val.relevant <- lrtest(lmm);
pR2.val.relevant <- pR2(lmm);
s <- summary(lmm)
tab <- cbind(tab, s$coefficients[1:10,1], 2*pnorm(-1*abs(s$coefficients[1:10,3])))

# Full text available
idx <- complete.cases(cbind(v.val.fulltext, v.explain))
lmm <- polr(factor(v.val.fulltext[idx]) ~ v.explain[idx,]);
lm.val.fulltext <- lmm;
lrtest.val.fulltext <- lrtest(lmm);
pR2.val.fulltext <- pR2(lmm);
s <- summary(lmm)
tab <- cbind(tab, s$coefficients[1:10,1], 2*pnorm(-1*abs(s$coefficients[1:10,3])))

# Ability to understand the source
idx <- complete.cases(cbind(v.val.understand, v.explain))
lmm <- polr(factor(v.val.understand)[idx] ~ v.explain[idx,]);
lm.val.understand <- lmm;
lrtest.val.understand <- lrtest(lm.val.understand)
pR2.val.understand <- pR2(lm.val.understand)
s <- summary(lmm)
tab <- cbind(tab, s$coefficients[1:10,1], 2*pnorm(-1*abs(s$coefficients[1:10,3])))

# Recent publication
idx <- complete.cases(cbind(v.val.recent, v.explain))
lmm <- polr(factor(v.val.recent[idx]) ~ v.explain[idx,])
lm.val.recent <- lmm;
lrtest.val.recent <- lrtest(lmm);
pR2.val.recent <- pR2(lmm);
s <- summary(lmm)
tab <- cbind(tab, s$coefficients[1:10,1], 2*pnorm(-1*abs(s$coefficients[1:10,3])))

# Author reputation
idx <- complete.cases(cbind(v.val.authorrep, v.explain))
lmm <- polr(factor(v.val.authorrep)[idx] ~ v.explain[idx,])
lm.val.authorrep <- lmm;
lrtest.val.authorrep <- lrtest(lmm)
pR2.val.authorrep <- pR2(lmm)
s <- summary(lmm)
tab <- cbind(tab, s$coefficients[1:10,1], 2*pnorm(-1*abs(s$coefficients[1:10,3])))

# Peer review
idx <- complete.cases(cbind(v.val.peerrev, v.explain))
lmm <- polr(factor(v.val.peerrev[idx]) ~ v.explain[idx,])
lm.val.peerrev <- lmm
lrtest.val.peerrev <- lrtest(lmm)
pR2.val.peerrev <- pR2(lmm)
s <- summary(lmm)
tab <- cbind(tab, s$coefficients[1:10,1], 2*pnorm(-1*abs(s$coefficients[1:10,3])))

# Source reputation
idx <- complete.cases(cbind(v.val.sourcerep, v.explain))
lmm <- polr(factor(v.val.sourcerep)[idx] ~ v.explain[idx,]);
s <- summary(lmm)
lm.val.sourcerep <- lmm
lrtest.val.sourcerep <- lrtest(lmm)
pR2.val.sourcerep <- pR2(lmm)
tab <- cbind(tab, s$coefficients[1:10,1], 2*pnorm(-1*abs(s$coefficients[1:10,3])))

# Physical availability
idx <- complete.cases(cbind(v.val.avail, v.explain))
lmm <- polr(factor(v.val.avail)[idx] ~ v.explain[idx,]);
s <- summary(lmm)
lm.val.avail <- lmm
lrtest.val.avail <- lrtest(lmm)
pR2.val.avail <- pR2(lmm)
tab <- cbind(tab, s$coefficients[1:10,1], 2*pnorm(-1*abs(s$coefficients[1:10,3])))
write.csv(x=tab,file="results.csv")
