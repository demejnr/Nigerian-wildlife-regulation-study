# model fitting -----------------------------------------------------------------------------------------------
library(ordinal)
suppressMessages(library(ordinal))
packageVersion("ordinal")
require(MASS)
library(RVAideMemoire)
library(lme4)
library(performance)
library(see)
library(MuMIn)

# the model
Knowledge.long$score <- as.factor(Knowledge.long$score)
Knowledge.long$score <- ordered(Knowledge.long$score, levels=c("1", "2", "3"))
Knowledge.long$score

Knowledge.long_m <- na.omit(Knowledge.long)
# We will use information theory

Knowledge_0 <- clmm(score ~ 1 + (1|Rank), data= Knowledge.long_m)
Knowledge_sp <- clmm(score ~ Location + (1|Rank), data= Knowledge.long_m)
Knowledge_org <- clmm(score ~ Organization + (1|Rank), data= Knowledge.long_m)
Knowledge_sporg <- clmm(score ~ Location*Organization + (1|Rank), data= Knowledge.long_m)
Knowledge_sp.org <- clmm(score ~ Location + Organization + (1|Rank), data= Knowledge.long_m)
Knowledge_sporg.spcoun <- clmm(score ~ Location + Organization + Location*Organization +
                                 (1|Rank), data= Knowledge.long_m)
models_Knowledge<- model.sel (Knowledge_0, Knowledge_sp, Knowledge_org, Knowledge_sporg, 
                              Knowledge_sp.org, Knowledge_sporg.spcoun, rank=AIC)
models_Knowledge

Knowledge_delta4 <- subset(models_Knowledge, delta <= 4, recalc.weights = TRUE)
Knowledge_delta4

# Perception of CITES workability

Perception.df <- dt[,c(2:6, 18:23)]
Perception.df
Perception.long <- melt(Perception.df, id.vars=c("Location","Organization", "Rank"))
names(Perception.long)[4:5] <- c("Location", "value")
head(Perception.long, 5)
levels(Perception.long$Organization) <- c("NIS", "NDLEA", "NCS", "KSSMB", "SON", "SAHCO", "NAQS", "NESREA")

# Invert the scores to make the interpretation more natural
Perception.long$score.inverted <- Perception.long$value
Perception.long$score.inverted <- as.factor(Perception.long$score.inverted)
Perception.long$score.inverted <- revalue(Perception.long$score.inverted, 
                                          c("3" = 1, "2" = 2, "1"= 3))

Perception.long$score.inverted <- as.integer(as.character(Perception.long$score.inverted))
Perception.long_1 <- na.omit(Perception.long)

# Summmarizing the inverted scores -------------------------------------------------------------
Perception.summary_org <- summarySE(Perception.long_1, measurevar= "score.inverted", 
                                    groupvars=c("Location", "Organization"), na.rm=T)

png(filename="Perception.png", 
    units="in", 
    width=15, 
    height=11, 
    res=1000)
ggplot(Perception.summary_org, aes(x=Organization, y=score.inverted, col=Organization) , 
       y=score.inverted) + geom_errorbar(aes(ymin= score.inverted-ci, ymax= score.inverted+ci), 
                                         width=0.4, cex=1.5) + geom_point(cex=2) + ylab("Perception of stakeholders on local CITES conservation regulations") + 
  xlab("Organization") + mytheme + facet_wrap(~Location) 
dev.off() 

Perception.summary <- summarySE(Perception.long_1, measurevar= "score.inverted", 
                                groupvars=c("Organization"), na.rm=T)
png(filename="Perception_org.png", 
    units="in", 
    width=12, 
    height=8, 
    res=1000)
ggplot(Perception.summary, aes(x=reorder(Organization, -score.inverted) , 
                               y=score.inverted)) + geom_errorbar(aes(ymin= score.inverted-ci, ymax= score.inverted+ci), 
                                                                  width=0.4, cex=1.5) + geom_point(cex=2) + ylab("Perception of stakeholders on local CITES conservation regulations") + 
  xlab("Organization") + mytheme 
dev.off()

# the model
Perception.long$score <- as.factor(Perception.long$score)
Perception.long$score <- ordered(Perception.long$score, levels=c("1", "2", "3"))
Perception.long$score

Perception.long_m <- na.omit(Perception.long)

#the models
Perception_0 <- clmm(score ~ 1 + (1|Rank), data= Perception.long_m)
Perception_sp <- clmm(score ~ Location + (1|Rank), data= Perception.long_m)
Perception_org <- clmm(score ~ Organization + (1|Rank), data= Perception.long_m)
Perception_sporg <- clmm(score ~ Location*Organization + (1|Rank), data= Perception.long_m)
Perception_sp.org <- clmm(score ~ Location + Organization + (1|Rank), data= Perception.long_m)
Perception_sporg.spcoun <- clmm(score ~ Location + Organization + Location*Organization +
                                  (1|Rank), data= Perception.long_m)
models_Perception<- model.sel (Perception_0, Perception_sp, Perception_org, Perception_sporg, 
                               Perception_sp.org, Perception_sporg.spcoun, rank=AIC)
models_Perception

Perception_delta4 <- subset(models_Perception, delta <= 4, recalc.weights = TRUE)
Perception_delta4

# Enforcement of CITES regulations

Action.df <- dt[,c(2:6, 24:29)]
Action.df
Action.long <- melt(Action.df, id.vars=c("Location","Organization", "Rank"))
names(Action.long)[4:5] <- c("Location", "value")
head(Action.long, 5)
levels(Action.long$Organization) <- c("NIS", "NDLEA", "NCS", "KSSMB", "SON", "SAHCO", "NAQS", "NESREA")

# Invert the scores to make the interpretation more natural
Action.long$score.inverted <- Action.long$value
Action.long$score.inverted <- as.factor(Action.long$score.inverted)
Action.long$score.inverted <- revalue(Action.long$score.inverted, 
                                      c("3" = 1, "2" = 2, "1"= 3))

Action.long$score.inverted <- as.integer(as.character(Action.long$score.inverted))
Action.long_1 <- na.omit(Action.long)

# Summmarizing the inverted scores -------------------------------------------------------------
Action.summary_org <- summarySE(Action.long_1, measurevar= "score.inverted", 
                                groupvars=c("Location", "Organization"), na.rm=T)

png(filename="Action.png", 
    units="in", 
    width=15, 
    height=11, 
    res=1000)
ggplot(Action.summary_org, aes(x=Organization, y=score.inverted, col=Organization) , 
       y=score.inverted) + geom_errorbar(aes(ymin= score.inverted-ci, ymax= score.inverted+ci), 
                                         width=0.4, cex=1.5) + geom_point(cex=2) + ylab("Level of local CITES regulations enforcement") + 
  xlab("Organization") + mytheme + facet_wrap(~Location) 
dev.off() 


# by organization
Action.summary <- summarySE(Action.long_1, measurevar= "score.inverted", 
                            groupvars=c("Organization"), na.rm=T)
png(filename="Action_org.png", 
    units="in", 
    width=12, 
    height=8, 
    res=1000)
ggplot(Action.summary, aes(x=reorder(Organization, -score.inverted) , 
                           y=score.inverted)) + geom_errorbar(aes(ymin= score.inverted-ci, ymax= score.inverted+ci), 
                                                              width=0.4, cex=1.5) + geom_point(cex=2) + ylab("Level of local CITES regulations enforcement") + 
  xlab("Organization") + mytheme 
dev.off()

# the model
Action.long$score <- as.factor(Action.long$score)
Action.long$score <- ordered(Action.long$score, levels=c("1", "2", "3"))
Action.long$score

Action.long_m <- na.omit(Action.long)

#the models
Action_0 <- clmm(score ~ 1 + (1|Rank), data= Action.long_m)
Action_sp <- clmm(score ~ Location + (1|Rank), data=Action.long_m)
Action_org <- clmm(score ~ Organization + (1|Rank), data= Action.long_m)
Action_sporg <- clmm(score ~ Location*Organization + (1|Rank), data= Action.long_m)
Action_sp.org <- clmm(score ~ Location + Organization + (1|Rank), data= Action.long_m)
Action_sporg.spcoun <- clmm(score ~ Location + Organization + Location*Organization +
                              (1|Rank), data= Action.long_m)
models_Action<- model.sel(Action_0, Action_sp, Action_org, Action_sporg, 
                          Action_sp.org, Action_sporg.spcoun, rank=AIC)
models_Action

Action_delta4 <- subset(models_Action, delta <= 4, recalc.weights = TRUE)
Action_delta4

#interview data analysis---------------------------------------------------------------------
dat <- read.csv("data.csv", header=TRUE)
head(dat)
str(dat)
names(dat)
fix(dat)

Trading.df <- dat[,c(8)]
head(Trading.df,5)

Trading.df <- as.data.frame(Trading.df)

Trading.df[] <- lapply (Trading.df, function(x) factor(x, 
                                                       labels = c("Family Heritage", "Source of income", "Alternative source of income", "Highly beneficial", 
                                                                  "There is benefit in it", "Interest and livelihood")))


title.next <- c("What is  the purpose of going into the vulture selling business?")

Trading.summary <- likert(items = Trading.df)
plot(Trading.summary, centered=T, # This controls stacked versus the "centered" option
     ordered=T, plot.percents=FALSE) + ggtitle(title.next)

# summarise for profesion and location data to see the pattern of responses ---------------------------------------
Profession <- as.factor(dat$Role.played.in.the.business.profession)
Trading.summary.Profession <- likert(Trading.df, grouping=Profession)

Location_dat <- as.factor(dat$Community)
Trading.summary.Location <- likert(Trading.df, grouping=Location_dat)

P1 <- plot(Trading.summary.Profession, centered=T, # This controls stacked versus the "centered" option
           ordered=T, plot.percents=FALSE) 

P2 <- plot(Trading.summary.Location , centered=T, # This controls stacked versus the "centered" option
           ordered=T, plot.percents=FALSE) + ggtitle(title.next)

png(filename="Reason_trading.png", 
    units="in", 
    width=14, 
    height=12, 
    res=500)
cowplot::plot_grid(P2, P1, labels = c('a', 'b'), nrow = 2)
dev.off()

# supply chain ---------------------------------------------------------------------------------------------
Supply.df <- dat[,c(10)]
head(Supply.df,5)

Supply.df <- as.data.frame(Supply.df)

Supply.df[] <- lapply (Supply.df, function(x) factor(x, 
                                                     labels = c("Yes", "No")))


title.next <- c("Do you make use and or supply vulture specimen?")

Supply.summary <- likert(items = Supply.df)
plot(Supply.summary, centered=T, # This controls stacked versus the "centered" option
     ordered=T, plot.percents=FALSE) + ggtitle(title.next)

# summarise for profesion and location data to see the pattern of responses ---------------------------------------
Profession <- as.factor(dat$Role.played.in.the.business.profession)
Supply.summary.Profession <- likert(Supply.df, grouping=Profession)

Location_dat <- as.factor(dat$Community)
Supply.summary.Location <- likert(Supply.df, grouping=Location_dat)

P3 <- plot(Supply.summary.Profession, centered=T, # This controls stacked versus the "centered" option
           ordered=T, plot.percents=FALSE) 

P4 <- plot(Supply.summary.Location , centered=T, # This controls stacked versus the "centered" option
           ordered=T, plot.percents=FALSE) + ggtitle(title.next)

png(filename="Supply_trading.png", 
    units="in", 
    width=14, 
    height=12, 
    res=500)
cowplot::plot_grid(P4, P3, labels = c('a', 'b'), nrow = 2)
dev.off()


# supply chain ---------------------------------------------------------------------------------------------
Supply_chain.df <- dat[,c(10)]
head(Supply_chain.df,5)

Supply_chain.df <- as.data.frame(Supply_chain.df)

Supply_chain.df[] <- lapply (Supply_chain.df, function(x) factor(x, 
                                                                 labels = c("Frequent", "Occassional", "Sometimes", "Don't Know", "Can't Say")))


title.next <- c("How often is the demand for vultures?")

Supply_chain.summary <- likert(items = Supply_chain.df)
plot(Supply_chain.summary, centered=T, # This controls stacked versus the "centered" option
     ordered=T, plot.percents=FALSE) + ggtitle(title.next)

# summarise for profesion and location data to see the pattern of responses ---------------------------------------
Profession <- as.factor(dat$Role.played.in.the.business.profession)
Supply_chain.summary.Profession <- likert(Supply_chain.df, grouping=Profession)

Location_dat <- as.factor(dat$Community)
Supply_chain.summary.Location <- likert(Supply_chain.df, grouping=Location_dat)

P5 <- plot(Supply_chain.summary.Profession, centered=T, # This controls stacked versus the "centered" option
           ordered=T, plot.percents=FALSE) 

P6 <- plot(Supply_chain.summary.Location , centered=T, # This controls stacked versus the "centered" option
           ordered=T, plot.percents=FALSE) + ggtitle(title.next)

png(filename="Supply_chain.png", 
    units="in", 
    width=14, 
    height=12, 
    res=500)
cowplot::plot_grid(P6, P5, labels = c('a', 'b'), nrow = 2)
dev.off()

# Demand chain ---------------------------------------------------------------------------------------------
dat <- na.omit(dat)
Demand_chain.df <- dat[,c(11)]
head(Demand_chain.df,5)

Demand_chain.df <- as.data.frame(Demand_chain.df)

Demand_chain.df <- na.omit(Demand_chain.df)

Demand_chain.df[] <- lapply (Demand_chain.df, function(x) factor(x, 
                                                                 labels = c("Interested people", "People with specific needs", "Interested people and wildlife traders",
                                                                            "Traditionalist and fellow hunters")))


title.next <- c("Who are the usual people in demand for vultures?")

Demand_chain.summary <- likert(items = Demand_chain.df)
plot(Demand_chain.summary, centered=T, # This controls stacked versus the "centered" option
     ordered=T, plot.percents=FALSE) + ggtitle(title.next)

# summarise for profesion and location data to see the pattern of responses ---------------------------------------
Profession <- as.factor(dat$Role.played.in.the.business.profession)
Demand_chain.summary.Profession <- likert(Demand_chain.df, grouping=Profession)

Location_dat <- as.factor(dat$Community)
Demand_chain.summary.Location <- likert(Demand_chain.df, grouping=Location_dat)

P7 <- plot(Demand_chain.summary.Profession, centered=T, # This controls stacked versus the "centered" option
           ordered=T, plot.percents=FALSE) 

P8 <- plot(Demand_chain.summary.Location , centered=T, # This controls stacked versus the "centered" option
           ordered=T, plot.percents=FALSE) + ggtitle(title.next)

png(filename="Demand_chain.png", 
    units="in", 
    width=14, 
    height=12, 
    res=500)
cowplot::plot_grid(P8, P7, labels = c('a', 'b'), nrow = 2)
dev.off()

# Demand chain ---------------------------------------------------------------------------------------------
dat <- na.omit(dat)
Demand_meeting.df <- dat[,c(13)]
head(Demand_meeting.df,5)

Demand_meeting.df <- as.data.frame(Demand_meeting.df)

Demand_meeting.df <- na.omit(Demand_meeting.df)

Demand_meeting.df[] <- lapply (Demand_meeting.df, function(x) factor(x, 
                                                                     labels = c("Not very difficult", "Somewhat difficult", "Not easy", "Very easy", "Easy")))


title.next <- c("How easy is meeting the demand in vulture specimen?")

Demand_meeting.summary <- likert(items = Demand_meeting.df)
plot(Demand_meeting.summary, centered=T, # This controls stacked versus the "centered" option
     ordered=T, plot.percents=FALSE) + ggtitle(title.next)

# summarise for profesion and location data to see the pattern of responses ---------------------------------------
Profession <- as.factor(dat$Role.played.in.the.business.profession)
Demand_meeting.summary.Profession <- likert(Demand_meeting.df, grouping=Profession)

Location_dat <- as.factor(dat$Community)
Demand_meeting.summary.Location <- likert(Demand_meeting.df, grouping=Location_dat)

P9 <- plot(Demand_meeting.summary.Profession, centered=T, # This controls stacked versus the "centered" option
           ordered=T, plot.percents=FALSE) 

P10 <- plot(Demand_meeting.summary.Location , centered=T, # This controls stacked versus the "centered" option
            ordered=T, plot.percents=FALSE) + ggtitle(title.next)

png(filename="Demand_meeting.png", 
    units="in", 
    width=14, 
    height=12, 
    res=500)
cowplot::plot_grid(P10, P9, labels = c('a', 'b'), nrow = 2)
dev.off()