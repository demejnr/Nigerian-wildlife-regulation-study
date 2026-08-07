# level for the respondents -------------------------------------------------------------------
table(dt$Organization)
dt$Organization <- as.factor(dt$Organization)
levels(dt$Organization) <- c("NIS", "NDLEA", "NCS", "KSSMB", "SON", "SAHCO", "NAQS", "NESREA")
table(dt$Location)
dt$Location <- as.factor(dt$Location)


Knowledge.df <- dt[,c(2:17)]
Knowledge.df
Knowledge.long <- melt(Knowledge.df, id.vars=c("Location","Organization", "Rank"))
names(Knowledge.long)[4:5] <- c("Location", "value")
head(Knowledge.long, 5)
levels(Knowledge.long$Organization) <- c("NIS", "NDLEA", "NCS", "KSSMB", "SON", "SAHCO", "NAQS", "NESREA")

# Invert the scores to make the interpretation more natural
Knowledge.long$score.inverted <- Knowledge.long$value
Knowledge.long$score.inverted <- as.factor(Knowledge.long$score.inverted)
Knowledge.long$score.inverted <- revalue(Knowledge.long$score.inverted, 
                                         c("3" = 1, "2" = 2, "1"= 3))

Knowledge.long$score.inverted <- as.integer(as.character(Knowledge.long$score.inverted))
Knowledge.long_1 <- na.omit(Knowledge.long)

# Summmarizing the inverted scores -------------------------------------------------------------
Knowledge.summary <- summarySE(Knowledge.long_1, measurevar= "score.inverted", 
                               groupvars="Organization", na.rm=T)
Knowledge.summary

png(filename="Knowledge_org.png", 
    units="in", 
    width=10, 
    height=8, 
    res=1000)
ggplot(Knowledge.summary, aes(x=reorder(Organization, -score.inverted) , 
                              y=score.inverted)) + geom_errorbar(aes(ymin= score.inverted-ci, ymax= score.inverted+ci), 
                                                                 width=0.4, cex=1.5) + geom_point(cex=2) + ylab("Conservation knowledge level of stakeholders") + 
  xlab("Organization") + mytheme 
dev.off() 


# by organization
Knowledge.summary_org <- summarySE(Knowledge.long_1, measurevar= "score.inverted", 
                                   groupvars=c("Location", "Organization"), na.rm=T)
png(filename="Knowledge.png", 
    units="in", 
    width=15, 
    height=11, 
    res=1000)
ggplot(Knowledge.summary_org, aes(x=Organization, y=score.inverted, col=Organization), 
       y=score.inverted) + geom_errorbar(aes(ymin= score.inverted-ci, ymax= score.inverted+ci), 
                                         width=0.4, cex=1.5) + geom_point(cex=2) + ylab("Conservation knowledge level of stakeholders") + 
  xlab("Organization") + mytheme + facet_wrap(~Location)
dev.off()