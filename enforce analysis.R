library(haven)
library(caret)
library(gbm)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(pdp)
library(iml)

enforce.data <- read_dta("enforce data.dta")
attach(enforce.data)
enforce.data$Others<-as.factor(others)
enforce.data$Uleader<-as.factor(uleader)
enforce.data$Statefor<-as.factor(statefor)
enforce.data$Enforce<-as.factor(enforce)
enforce.data$India<-as.factor(india)
enforce.data$Kenya<-as.factor(kenya)
enforce.data$Mad<-as.factor(mad)
enforce.data$Nepal<-as.factor(nepal)
enforce.data$Uganda<-as.factor(uganda)

#Get data in form to be used by different packages
#Factor dependent variable for caret
#Numeric dependent variable for GBM
enforce.train<-enforce.data[c(1,2,4:7,9:14, 22:30)]
enforce.model<-enforce.data[c(1,2,4:7,9:14, 16, 22:24, 26:30)]
#Tuning the model, gbm.fraction fixed at 0.7 due to small sample size
model<-Enforce~.
metric<-"Accuracy"
fitControl<-trainControl(method="repeatedcv", number =10, repeats = 5)
grid<-expand.grid(interaction.depth=c(2:5), n.trees=seq(100, 5000, 100), shrinkage =c(0.001,0.005, 0.01), n.minobsinnode=c(5, 10))
set.seed(1234)
enforce.tune<-train(model, data=enforce.train, method="gbm", trControl=fitControl, tuneGrid=grid, metric=metric, bag.fraction=0.7)
enforce.tune
max(enforce.tune$results["Accuracy"])
#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were n.trees = 1600, interaction.depth
#= 4, shrinkage = 0.01 and n.minobsinnode = 10.
#[1] 0.7571964

###Using GBM
model<-enforce~.
set.seed(1234)
enforce.gbm<- gbm(formula =model, distribution = "bernoulli", 
                   data = enforce.model, n.trees = 1600, interaction.depth = 4,
                   shrinkage = 0.01, bag.fraction=0.7, n.minobsinnode=10)


#Variable Importance
importance <- tibble::as_tibble(gbm::summary.gbm(enforce.gbm, 
                                                 plotit = FALSE))

importance



#Importance Plot
impplot<- ggplot(data=importance, aes(x = forcats::fct_reorder(.f = var, .x = rel.inf), 
                                      y = rel.inf))+
  geom_col()+
  coord_flip()+
  theme(axis.title = element_text(size=7)) + 
  xlab('Variables') +
  scale_x_discrete(labels=c("Madagascar", "Kenya", "India", 
                            "State forest", "Others", 
                            "Social capital", "Nepal", "Uganda", 
                            "Land ownership", "Subsistence dependence",
                            "Poverty", "Economic heterogeneity", "Forest size", 
                            "Literacy", "Interest heterogeneity",
                            "Leadership", "College education",
                            "Group size", "Distance to market", 
                            "Local rulemaking")) +
  ylab('Relative Influence') +
  theme(panel.border = element_rect(fill=NA) )+
  theme(panel.background = element_blank())+
  theme(axis.text=element_text(size=5))+
  theme(plot.margin = margin(5,10,5,5))



pdf("impplot.pdf", width = 3, height = 4)
impplot
dev.off()

#Calculate Partial Dependence Plots
p1<-partial(enforce.gbm, pred.var="localrules", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=1600)
p2<-partial(enforce.gbm, pred.var="market", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=1600)
p3<-partial(enforce.gbm, pred.var="uhhnum", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=1600)
p4<-partial(enforce.gbm, pred.var="college", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=1600)
p5<-partial(enforce.gbm, pred.var="Uleader", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=1600)
p6<-partial(enforce.gbm, pred.var="fracinterest", plot=FALSE, prob=TRUE, plot.engine="ggplot", trim.outliers=TRUE, n.trees=1600)
p7<-partial(enforce.gbm, pred.var="literate", plot=FALSE, prob=TRUE, plot.engine="ggplot", trim.outliers=TRUE, n.trees=1600)
p8<-partial(enforce.gbm, pred.var="fsize", plot=FALSE, prob=TRUE, plot.engine="ggplot", trim.outliers=TRUE, n.trees=1600)
p9<-partial(enforce.gbm, pred.var="fracecon", plot=FALSE, prob=TRUE, plot.engine="ggplot", trim.outliers=TRUE, n.trees=1600)
p10<-partial(enforce.gbm, pred.var="poor", plot=FALSE, prob=TRUE, plot.engine="ggplot", trim.outliers=TRUE, n.trees=1600)
p11<-partial(enforce.gbm, pred.var="subsispercent", plot=FALSE, prob=TRUE, plot.engine="ggplot", trim.outliers=TRUE, n.trees=1600)
p12<-partial(enforce.gbm, pred.var="ownland", plot=FALSE, prob=TRUE, plot.engine="ggplot", trim.outliers=TRUE, n.trees=1600)
p13<-partial(enforce.gbm, pred.var="Uganda", plot=FALSE, prob=TRUE, plot.engine="ggplot", trim.outliers=TRUE, n.trees=1600)
p14<-partial(enforce.gbm, pred.var="Nepal", plot=FALSE, prob=TRUE, plot.engine="ggplot", trim.outliers=TRUE, n.trees=1600)
p15<-partial(enforce.gbm, pred.var="soccap", plot=FALSE, prob=TRUE, plot.engine="ggplot", trim.outliers=TRUE, n.trees=1600)
p16<-partial(enforce.gbm, pred.var="Others", plot=FALSE, prob=TRUE, plot.engine="ggplot", trim.outliers=TRUE, n.trees=1600)
p17<-partial(enforce.gbm, pred.var="Statefor", plot=FALSE, prob=TRUE, plot.engine="ggplot", trim.outliers=TRUE, n.trees=1600)
p18<-partial(enforce.gbm, pred.var="India", plot=FALSE, prob=TRUE, plot.engine="ggplot", trim.outliers=TRUE, n.trees=1600)
p19<-partial(enforce.gbm, pred.var="Kenya", plot=FALSE, prob=TRUE, plot.engine="ggplot", trim.outliers=TRUE, n.trees=1600)
p20<-partial(enforce.gbm, pred.var="Mad", plot=FALSE, prob=TRUE, plot.engine="ggplot", trim.outliers=TRUE, n.trees=1600)

#Generate plots
plot1<-autoplot(p1, xlab="Local rulemaking (%)", ylab="Partial dependence", aes(size=1))+ylim(0, 0.8)+
  theme(legend.position="none") +theme(axis.text = element_text(size=5))+theme(axis.title=element_text(size=7))+
  theme(panel.border = element_rect(fill=NA) )+theme(panel.background = element_blank())+
  theme(panel.background = element_blank())+ theme(panel.border = element_rect(fill=NA))+
  ggtitle("(A)")+theme(plot.title=element_text(size=7, hjust=0.5))
plot2<-autoplot(p2, xlab="Distance to market (minutes)", ylab="Partial dependence", aes(size=1))+ylim(0, 0.8)+ 
  theme(legend.position="none") +theme(axis.text = element_text(size=5))+ theme(axis.title=element_text(size=7))+
  theme(axis.title.y = element_blank())+ theme(axis.text.y = element_blank())+ theme(axis.ticks.y = element_blank()) +
  theme(panel.background = element_blank())+ theme(panel.border = element_rect(fill=NA)) + ggtitle("(B)")+
  theme(plot.title=element_text(size=7, hjust=0.5))

plot3<-autoplot(p3, xlab="Group size (households)", ylab="Partial dependence", aes(size=1))+ylim(0, 0.8) +  
  theme(legend.position="none") +theme(axis.text = element_text(size=5))+
  theme(axis.title=element_text(size=7))+theme(panel.border = element_rect(fill=NA) )+
  theme(panel.background = element_blank())+ theme(panel.border = element_rect(fill=NA) )+
  theme(axis.title.y = element_blank())+ theme(axis.ticks.y = element_blank())+theme(axis.text.y = element_blank())+
  ggtitle("(C)")+ theme(plot.title=element_text(size=7, hjust=0.5))

# Generate plots for supplementary material
plot4<-autoplot(p4, xlab="College education (%)", ylab="Partial dependence", aes(size=1))+ylim(0, 0.8) +  theme(legend.position="none") +
  theme(axis.text = element_text(size=10))+theme(axis.title=element_text(size=10))+theme(panel.border = element_rect(fill=NA) )+
  theme(panel.background = element_blank()) 
plot5<-ggplot(p5, aes(x=Uleader, y=yhat))+ xlab("Leadership")+ ylab("Partial dependence")+
  ylim(0, 0.8)+geom_bar(stat="identity", fill="black") + theme(legend.position="none") +
  theme(axis.text = element_text(size=10))+theme(axis.title=element_text(size=10))+
  theme(panel.background = element_blank())+theme(panel.border = element_rect(fill=NA))+ theme(axis.title.y = element_blank())+ 
  theme(axis.text.y = element_blank())+ theme(axis.ticks.y = element_blank())+ scale_x_discrete(labels=c("0"="Absent", "1"= "Present"))

plot6<-autoplot(p6, xlab="Interest heterogeneity", ylab="Partial dependence", aes(size=1)) +ylim(0, 0.8) +  
  theme(legend.position="none") +theme(axis.text = element_text(size=10))+theme(axis.title=element_text(size=10))+
  theme(panel.border = element_rect(fill=NA) )+theme(panel.background = element_blank())+ theme(axis.title.y = element_blank())+ 
  theme(axis.text.y = element_blank())+ theme(axis.ticks.y = element_blank())
plot7<-autoplot(p7, xlab="Literacy (%)", ylab="Partial dependence", aes(size=1))+ylim(0, 0.8) +  
  theme(legend.position="none") +theme(axis.text = element_text(size=10))+theme(axis.title=element_text(size=10))+
  theme(panel.border = element_rect(fill=NA) )+theme(panel.background = element_blank())
plot8<-autoplot(p8, xlab=" Forest size (ha)", ylab="Partial dependence", aes(size=1))+ylim(0, 0.8) +  theme(legend.position="none") +
  theme(axis.text = element_text(size=10))+theme(axis.title=element_text(size=10))+theme(panel.border = element_rect(fill=NA) )+
  theme(panel.background = element_blank())  + theme(axis.title.y = element_blank())+ 
  theme(axis.text.y = element_blank())+ theme(axis.ticks.y = element_blank())
plot9<-autoplot(p9, xlab="Economic heterogeneity", ylab="Partial dependence", aes(size=1))+ylim(0, 0.8) +  theme(legend.position="none") +
  theme(axis.text = element_text(size=10))+theme(axis.title=element_text(size=10))+theme(panel.border = element_rect(fill=NA) )+
  theme(panel.background = element_blank()) + theme(axis.title.y = element_blank())+ 
  theme(axis.text.y = element_blank())+ theme(axis.ticks.y = element_blank())
plot10<-autoplot(p10, xlab="Poverty (%)", ylab="Partial dependence", aes(size=1))+ylim(0, 0.8) +  theme(legend.position="none") +
  theme(axis.text = element_text(size=10))+theme(axis.title=element_text(size=10))+theme(panel.border = element_rect(fill=NA) )+
  theme(panel.background = element_blank())
plot11<-autoplot(p11, xlab="Subsistence dependence (%)", ylab="Partial dependence", aes(size=1))+ylim(0, 0.8) +  
  theme(legend.position="none") +theme(axis.text = element_text(size=10))+theme(axis.title=element_text(size=10))+
  theme(panel.border = element_rect(fill=NA) )+theme(panel.background = element_blank())+ theme(axis.title.y = element_blank())+ 
  theme(axis.text.y = element_blank())+ theme(axis.ticks.y = element_blank())
plot12<-autoplot(p12, xlab="Land ownership (%)", ylab="Partial dependence", aes(size=1))+ylim(0, 0.8) +  
  theme(legend.position="none") +theme(axis.text = element_text(size=10))+theme(axis.title=element_text(size=10))+
  theme(panel.border = element_rect(fill=NA) )+theme(panel.background = element_blank())+ theme(axis.title.y = element_blank())+ 
  theme(axis.text.y = element_blank())+ theme(axis.ticks.y = element_blank())
plot13<-ggplot(p13, aes(x=Uganda, y=yhat))+ xlab("Uganda")+ ylab("Partial dependence")+ylim(0, 0.8)+geom_bar(stat="identity", fill="black") + 
  theme(legend.position="none") +theme(axis.text = element_text(size=10))+theme(axis.title=element_text(size=10))+
  theme(panel.background = element_blank())+theme(panel.border = element_rect(fill=NA))+scale_x_discrete(labels=c("0"="No", "1"= "Yes"))
plot14<-ggplot(p14, aes(x=Nepal, y=yhat))+ xlab("Nepal")+ ylab("Partial dependence")+ylim(0, 0.8)+geom_bar(stat="identity", fill="black") + 
  theme(legend.position="none") +theme(axis.text = element_text(size=10))+theme(axis.title=element_text(size=10))+
  theme(panel.background = element_blank())+theme(panel.border = element_rect(fill=NA))+scale_x_discrete(labels=c("0"="No", "1"= "Yes"))+ 
  theme(axis.title.y = element_blank())+ theme(axis.text.y = element_blank())+ theme(axis.ticks.y = element_blank())
plot15<-autoplot(p15, xlab="Social capital", ylab="Partial dependence", aes(size=1))+ylim(0, 0.8) + theme(legend.position="none") +
  theme(axis.text = element_text(size=10))+theme(axis.title=element_text(size=10))+theme(panel.border = element_rect(fill=NA) )+
  theme(panel.background = element_blank())+ theme(axis.title.y = element_blank())+ 
  theme(axis.text.y = element_blank())+ theme(axis.ticks.y = element_blank())
plot16<-ggplot(p16, aes(x=Others, y=yhat))+ xlab("Other groups")+ ylab("Partial dependence")+ylim(0, 0.8)+
  geom_bar(stat="identity", fill="black") + theme(legend.position="none") +theme(axis.text = element_text(size=10))+
  theme(axis.title=element_text(size=10))+theme(panel.background = element_blank())+theme(panel.border = element_rect(fill=NA))+
  scale_x_discrete(labels=c("0"="No", "1"= "Yes"))
plot17<-ggplot(p17, aes(x=Statefor, y=yhat))+ xlab("State forest")+ ylab("Partial dependence")+ylim(0, 0.8)+
  geom_bar(stat="identity", fill="black") + theme(legend.position="none") +theme(axis.text = element_text(size=10))+
  theme(axis.title=element_text(size=10))+theme(panel.background = element_blank())+theme(panel.border = element_rect(fill=NA))+
  scale_x_discrete(labels=c("0"="No", "1"= "Yes")) + theme(axis.title.y = element_blank())+ 
  theme(axis.text.y = element_blank())+ theme(axis.ticks.y = element_blank())
plot18<-ggplot(p18, aes(x=India, y=yhat))+ xlab("India")+ ylab("Partial dependence")+ylim(0, 0.8)+geom_bar(stat="identity", fill="black") + 
  theme(legend.position="none") +theme(axis.text = element_text(size=10))+theme(axis.title=element_text(size=10))+
  theme(panel.background = element_blank())+theme(panel.border = element_rect(fill=NA))+scale_x_discrete(labels=c("0"="No", "1"= "Yes"))+ 
  theme(axis.title.y = element_blank())+ 
  theme(axis.text.y = element_blank())+ theme(axis.ticks.y = element_blank())
plot19<-ggplot(p19, aes(x=Kenya, y=yhat))+ xlab("Kenya")+ ylab("Partial dependence")+ylim(0, 0.8)+geom_bar(stat="identity", fill="black") + 
  theme(legend.position="none") +theme(axis.text = element_text(size=10))+theme(axis.title=element_text(size=10))+theme(panel.background = element_blank())+
  theme(panel.border = element_rect(fill=NA))+scale_x_discrete(labels=c("0"="No", "1"= "Yes"))
plot20<-ggplot(p20, aes(x=Mad, y=yhat))+ xlab("Madagascar")+ ylab("Partial dependence")+ylim(0, 0.8)+
  geom_bar(stat="identity", fill="black") + theme(legend.position="none") +theme(axis.text = element_text(size=10))+theme(axis.title=element_text(size=10))+
  theme(panel.background = element_blank())+theme(panel.border = element_rect(fill=NA))+scale_x_discrete(labels=c("0"="No", "1"= "Yes")) +
  theme(axis.title.y = element_blank())+ 
  theme(axis.text.y = element_blank())+ theme(axis.ticks.y = element_blank())

pdf("combined.pdf", width = 7, height = 3.5)
grid.arrange(plot1, plot2, plot3, ncol = 3, nrow=1)
dev.off()

tiff("rest.tiff", units="in", res=300, width = 7, height = 10.5)
grid.arrange(plot4, plot5, plot6, plot7, plot8, plot9, plot10,
             plot11, plot12, plot13, plot14, plot15, plot16,
             plot17, plot18, plot19, plot20, ncol = 3, nrow=6, clip="off")
dev.off()

#Interactions
X<-enforce.model[c(1:12, 14:21)]
X<-as.data.frame(X)
predict.fun=function(object, newdata)
{predict(object, newdata, n.trees=1600)}
predictor <-Predictor$new(enforce.gbm, data=X, y = enforce.model$enforce, type="prob", class="1", predict.fun=function(object, newdata)
{predict(object, newdata, n.trees=1600)}
)


iml_rules<-Interaction$new(predictor, feature="localrules", grid.size = 177)
View(iml_rules$results)
plot(iml_rules)
iml_market<-Interaction$new(predictor, feature="market", grid.size=177)
View(iml_market$results)
plot(iml_market)
iml_grp<-Interaction$new(predictor, feature="uhhnum", grid.size=177)
View(iml_grp$results)
plot(iml_grp)


#Partial Dependence plots for Interactions
pdp1<-partial(enforce.gbm, pred.var=c("market", "literate"), prob=TRUE, 
              trim.outliers = TRUE, chull=TRUE, n.trees=1600)
pdp2<-partial(enforce.gbm, pred.var=c("market", "Uleader"), prob=TRUE, 
              trim.outliers = TRUE, n.trees=1600)

pdp3<-partial(enforce.gbm, pred.var=c("uhhnum", "Uleader"), 
              prob=TRUE, trim.outliers = TRUE, n.trees=1600)

pdp3plot <- pdp3 %>%
  mutate(Leader = if_else(Uleader == 0, 'Absent', 'Present'))




pdpplot1<-autoplot(pdp1) + xlab("Distance to market (minutes)")+
  ylab("Literacy (%)")+
  theme(panel.border = element_rect(fill=NA) )+
  theme(panel.background = element_blank())+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+
  theme(legend.title = element_blank())
pdpplot2<- ggplot(data=pdp2, aes(x=market,y=yhat, group=Uleader)) +  
  geom_line(aes(linetype = Uleader))+xlab("Distance to market (minutes)")+ 
  ylab("Partial dependence")+ theme(axis.text = element_text(size=5))+
  theme(axis.title=element_text(size=7))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(fill=NA))+ ylim(0,1)+
  theme(legend.position="none")+ggtitle("(A)") + theme(plot.title=element_text(size=7, hjust=0.5))+
  theme(legend.text=element_text(size=7))+ theme(legend.title = element_text(size=7))
                                                  
pdpplot3<- ggplot(data=pdp3plot, aes(x=uhhnum,y=yhat, group=Leader)) +  
  geom_line(aes(linetype = Leader))+xlab("Group size (households)")+ 
  ylab("Partial dependence")+ theme(axis.text = element_text(size=5))+
  theme(axis.title=element_text(size=7))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(fill=NA))+ ylim(0,1)+labs(linetype="Leadership")+
  theme(axis.title.y = element_blank())+theme(axis.text.y = element_blank())+ggtitle("(B)") +  
  theme(legend.key = element_rect(fill = NA)) + theme(axis.ticks.y = element_blank())+
  theme(plot.title=element_text(size=7, hjust=0.5))+ theme(legend.text=element_text(size=7))+
  theme(legend.title = element_text(size=7))
  


pdf("interactions.pdf",width =7, height=3.5)
grid.arrange(pdpplot2, pdpplot3, ncol = 2, nrow=1)
dev.off()

tiff("interactionslit.tiff", res=300, width = 4, height = 4)
pdpplot1
dev.off()

#Corrplots
library(corrplot)
X$Others<-as.numeric(X$Others)
X$Uleader<-as.numeric(X$Uleader)
X$Statefor<-as.numeric(X$Statefor)
X$India<-as.numeric(X$India)
X$Kenya<-as.numeric(X$Kenya)
X$Mad<-as.numeric(X$Mad)
X$Nepal<-as.numeric(X$Nepal)
X$Uganda<-as.numeric(X$Uganda)
M<-cor(X)
colnames(M)<-c("Forest size", "Group size", "Subsistence dependence", "Poverty rate", 
               "Social capital", "Distance to market", "Local rulemaking", 
                "College education", "Literacy rate", "Interest heterogeneity", "Economic heterogeneity", 
               "Land ownership", "Other groups", "Leadership", "State forest", "India", "Kenya",
               "Madagascar", "Nepal", "Uganda")
rownames(M)<-c("Forest size", "Group size", "Subsistence dependence", "Poverty rate", 
               "Social capital", "Distance to market", "Local rulemaking", 
               "College education", "Literacy rate", "Interest heterogeneity", "Economic heterogeneity", 
               "Land ownership", "Other groups", "Leadership", "State forest", "India", "Kenya",
               "Madagascar", "Nepal", "Uganda")

tiff("corr.tiff", width=10, height=7, res=300, units="in")
corrplot(M, method="color", type="lower", tl.cex=0.8, addCoef.col = "black", number.cex=0.55, tl.col="black",tl.srt=25, tl.offset=0.6)
dev.off()
