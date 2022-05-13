#install.packages("aod")
library(tidyverse)
library(caret)
theme_set(theme_bw())
library(ggpubr)
library(plot3D)
library(waffle)
library(extrafont)
library(vcd)
library("plot3D")
library(ggplot2)
library("dplyr")
library(xlsx)
library(ggplot2)
library(ggbeeswarm)
library(psych)
library(dplyr)
library(hrbrthemes)
#install.packages("frame")
library(plotrix)
library(pdp)
library(lessR)
library(dplyr)
library(maditr)
library(aod)
library(MASS)
library(ggpubr)
library(plot3D)
library(waffle)
library(extrafont)

library(vcd)
library("plot3D")

library(ggplot2)

library(dplyr)
library(xlsx)

library(ggplot2)
library(ggbeeswarm)
library(psych)

library(plyr)
library(hrbrthemes)
install.packages("rlang")
library(pdp)
library(lessR)
#library(operators) 
library(magrittr)
library(FSA)
library(ggplot2)
#install.packages("hrbrthemes")
#remove.packages(operators)
library(dplyr)
library(grid)
library(gridExtra)
library(mosaic)
library(hrbrthemes)
#install.packages("ROCR")
library(interactions)
library(jtools)
library(sjPlot)
library(sjmisc)
library(ggplot2)
theme_set(theme_sjplot())
library(ROCR)
###################
#
#
#
#
#
#
#
#DATA LOADING AND ANALYSIS
#
#
#
#
#
#
#
#
#
##############

# Load the data and remove NAs
mydata <- read.csv("/Users/adamszequi/Desktop/MS1-thesis/Telco-Customer-Churn.csv",stringsAsFactors=T)

#Data Cleaning
sapply(mydata, function(x) sum(is.na(x)))#check for missing values
str(mydata)
mydata <- mydata[complete.cases(mydata), ]
mydata

#combine  “No internet service” with “No” for six columns, they are: 
#“OnlineSecurity”, “OnlineBackup”, “DeviceProtection”, “TechSupport”, 
#“streamingTV”, “streamingMovies”.
cols_recode1 <- c(10:15)
for(i in 1:ncol(mydata[,cols_recode1])) {
  mydata[,cols_recode1][,i] <- as.factor(plyr::mapvalues
                                         (mydata[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}

#ccombine “No phone service” with “No” for column “MultipleLines”
mydata$MultipleLines <- as.factor(plyr::mapvalues(mydata$MultipleLines, 
                                                  from=c("No phone service"),
                                                  to=c("No")))

#group tenure: “0–12 Month”, “12–24 Month”, “24–48 Months”, “48–60 Month”, “> 60 Month”
tenure_by_year<- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-1year')
  }else if(tenure > 12 & tenure <= 24){
    return('1-2years')
  }else if (tenure > 24 & tenure <= 48){
    return('2-3years')
  }else if (tenure > 48 & tenure <=60){
    return('3-4years')
  }else if (tenure > 60){
    return('>4years')
  }
}

#make a column of average total charge
avg_total_charge<-mydata$TotalCharges/mydata$tenure
mydata$avg_total_charge<-avg_total_charge

mydata$tenure_by_year <- sapply(mydata$tenure,tenure_by_year)
mydata$tenure_by_year <- as.factor(mydata$tenure_by_year)

#change senior citizen to factor variable
mydata$SeniorCitizen <- as.factor(plyr::mapvalues(mydata$SeniorCitizen,
                                                  from=c("0","1"),
                                                  to=c("Below 65", "Above 65")))
mydata$customerID <- NULL

str(mydata)

################################                           
#
##```{r }
#ts_model_1 <- arima(ts_sqrt_0.4, order=c(0,1,1),seasonal=list(order=c(0,1,1),period=4))
#ts_model_2 <- arima(ts_sqrt_0.4, order=c(1,1,1),seasonal=list(order=c(1,1,1), period=4))
#ts_model_3<- arima(ts_sqrt_0.4, order=c(0,1,2),seasonal=list(order=c(0,1,2), period=4))
#ts_model_4<- arima(ts_sqrt_0.4, order=c(2,1,2),seasonal=list(order=c(2,1,2), period=4))
#data.frame('Model-1' = ts_model_1$aic, 'Model-2' = ts_model_2$aic, 'Model-3' = ts_model_2$aic,'Model-4' = ts_model_2$aic)

#
#
#
#EXPLORATORY DATA ANALYSIS
#
#
#
#
#
################################

###################
count=0
nocount=0
for (val in mydata%>% pull(gender)) {
  if(val  == 'Male')  
    count = count+1
  if(val  == 'Female') 
    nocount = nocount+1
}
countpercent=(count/(count+nocount))*100
nocountpercent=(nocount/(count+nocount))*100

print(round(countpercent))
print(round(nocountpercent))


##########################
# Create Data
data <- data.frame(group=c('Male','Female') ,value=c(3549,3483))
data
############
data <- data %>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(data$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

# Basic piechart
genderplot<-ggplot(data, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  #theme(legend.position="bottom") +
  geom_text(aes(y = ypos, label = paste0(round(prop),'%')), color = "white", size=6) +
  scale_fill_brewer(palette='Set1')
genderplot <- genderplot + guides(fill=guide_legend(title="Gender"))

genderplot

#########################
#
#
#
#
#
#Univariate Binary Variables
#
#
#
#
#
#
##############################
age_table=mydata$SeniorCitizen
table(age_table)
#################
data_age <- data.frame(group=c('Above 65','Below 65') ,value=c(1142,5890))
data_age
############
data_age <- data_age %>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(data$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
data_age
# Basic piechart
ageplot<-ggplot(data_age, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  #theme(legend.position="bottom") +
  geom_text(aes(y = ypos, label = paste0(round(prop),'%')), color = "white", size=6) +
  scale_fill_brewer(palette='Set1')
ageplot <- ageplot + guides(fill=guide_legend(title="Age"))

ageplot
################
#################
churn_table=table(mydata$Churn)
churn_table
#################
data_churn <- data.frame(group=c('No','Yes') ,value=c(5163,1869))
data_churn
############
data_churn <- data_churn %>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(data$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
data_churn
# Basic piechart 
churnplot<-ggplot(data_churn, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  #theme(legend.position="bottom") +
  geom_text(aes(y = ypos, label = paste0(round(prop),'%')), color = "white", size=6) +
  scale_fill_brewer(palette='Set1')
churnplot <- churnplot + guides(fill=guide_legend(title="Churn"))

churnplot
################
#################
partner_table=table(mydata$Partner)
partner_table
#################
data_part <- data.frame(group=c('No','Yes') ,value=c(3639,3393))
data_part
############
data_part <- data_part%>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(data$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
data_part
# Basic piechart 
partplot<-ggplot(data_part, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  #theme(legend.position="bottom") +
  geom_text(aes(y = ypos, label = paste0(round(prop),'%')), color = "white", size=6) +
  scale_fill_brewer(palette='Set1')
partplot <- partplot + guides(fill=guide_legend(title="Partner"))

partplot

################
#################
depend_table=table(mydata$Dependents)
depend_table
#################
data_depend <- data.frame(group=c('No','Yes') ,value=c(4933,2099))
data_depend
############
data_depend <- data_depend%>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(data$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
data_depend
# Basic piechart 
dependplot<-ggplot(data_depend, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  #theme(legend.position="bottom") +
  geom_text(aes(y = ypos, label = paste0(round(prop),'%')), color = "white", size=6) +
  scale_fill_brewer(palette='Set1')
dependplot <-dependplot + guides(fill=guide_legend(title="Dependents"))

dependplot

################
#################
phone_table=table(mydata$PhoneService)
phone_table
#################
data_phone <- data.frame(group=c('No','Yes') ,value=c(680,6352))
data_phone
############
data_phone <- data_phone%>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(data$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
data_phone
# Basic piechart 
phoneplot<-ggplot(data_phone, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  #theme(legend.position="bottom") +
  geom_text(aes(y = ypos, label = paste0(round(prop),'%')), color = "white", size=6) +
  scale_fill_brewer(palette='Set1')
phoneplot <-phoneplot + guides(fill=guide_legend(title="Phone Service"))

phoneplot

############
grid.arrange(phoneplot, genderplot,dependplot,partplot,churnplot,ageplot, nrow = 3,ncol=3)

###########################
#
#
#
#
##
#
#
#

#Univariate Multople Variables
#
#
#
#
#
#
#
#
#
#############################
########################
plotdata <- mydata %>%
  dplyr::count(Contract)%>%
  mutate(pct = n / sum(n),
         pctlabel = paste0(round(pct*100), "%"))
plotdata
# plot the bars as percentages, 
# in decending order with bar labels
de=ggplot(plotdata, 
          aes(x = reorder(Contract, -pct),
              y = pct)) + 
  geom_bar(stat = "identity", 
           fill = "grey", 
           color = "black") +
  geom_text(aes(label = pctlabel), 
            vjust = -0.25) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Contract type", 
       y = "% of signees per contract", 
       title  = "Contracts")
de
########################
plotdata <- mydata %>%
  dplyr::count(MultipleLines) %>%
  mutate(pct = n / sum(n),
         pctlabel = paste0(round(pct*100), "%"))

# plot the bars as percentages, 
# in decending order with bar labels
cd=ggplot(plotdata, 
          aes(x = reorder(MultipleLines, -pct),
              y = pct)) + 
  geom_bar(stat = "identity", 
           fill = "grey", 
           color = "black") +
  geom_text(aes(label = pctlabel), 
            vjust = -0.25) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Subsctiption status", 
       y = "% of customers with multiple lines", 
       title  = "Multiple lines")
cd
##########################
########################
plotdata <- mydata %>%
  dplyr::count(InternetService) %>%
  mutate(pct = n / sum(n),
         pctlabel = paste0(round(pct*100), "%"))

# plot the bars as percentages, 
# in decending order with bar labels
ab=ggplot(plotdata, 
          aes(x = reorder(InternetService, -pct),
              y = pct)) + 
  geom_bar(stat = "identity", 
           fill = "grey", 
           color = "black") +
  geom_text(aes(label = pctlabel), 
            vjust = -0.25) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Type of Internet Service", 
       y = "% of customers suscribed to each type", 
       title  = "Internet Service")
ab
########################
plotdata <- mydata %>%
  dplyr::count(OnlineBackup) %>%
  mutate(pct = n / sum(n),
         pctlabel = paste0(round(pct*100), "%"))

# plot the bars as percentages, 
# in decending order with bar labels
bc=ggplot(plotdata, 
          aes(x = reorder(OnlineBackup, -pct),
              y = pct)) + 
  geom_bar(stat = "identity", 
           fill = "grey", 
           color = "black") +
  geom_text(aes(label = pctlabel), 
            vjust = -0.25) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Subscription status", 
       y = "% of customers suscribed to service", 
       title  = "Backup Service")
bc
#######################
grid.arrange(ab, bc,cd,de ,nrow = 2,ncol=2)
######################
#
#
#
#
#
#
#
#MULTIVARIATE
#
#
#

#
#
#
#
#
##############################

w=mydata$tenure_group
w

ten=hist(x,probability=TRUE,xlab='Tenure', ylab = "Frequency", main = "Histogram and Boxplot of customers tenure",
         col = rgb(0, 0, 0, alpha = 0.5), axes = TRUE)
axis(1) # Adds horizontal axis
par(new = TRUE)
boxplot(x, horizontal = TRUE, axes = FALSE,
        lwd = 2, col = rgb(0, 0, 0, alpha = 0.2))

##################################

average_charges=hist(y,probability=TRUE,xlab='Average Monthly Charges by Tenure', ylab = "Frequency", main = "Histogram and Boxplot of Average Monthly Chrges",
                     col = rgb(0, 0, 0, alpha = 0.5), axes = TRUE)
axis(1) # Adds horizontal axis
par(new = TRUE)
boxplot(y, horizontal = TRUE, axes = FALSE,
        lwd = 2, col = rgb(0, 0, 0, alpha = 0.2))

###################################
monthly_charges=hist(w,probability=TRUE,xlab='Monthly Charges ', ylab = "Frequency", main = "Histogram and Boxplot of Monthly charges",
                     col = rgb(0, 0, 0, alpha = 0.5), axes = TRUE)
axis(1) # Adds horizontal axis
par(new = TRUE)
boxplot(z, horizontal = TRUE, axes = FALSE,
        lwd = 2, col = rgb(0, 0, 0, alpha = 0.2))
##################################
#w=mydata$tenure_group
x=mydata$tenure
y=mydata$TotalCharges/mydata$tenure
z=mydata$MonthlyCharges

m=qplot(x,
        geom="histogram",
        binwidth = 5,  
        main = "Customer's tenure", 
        xlab = "Tenure in Months",  
        fill=I("black"), 
        col=I("red"), 
        alpha=I(.2),
        xlim=c(10,50))

n=qplot(y,
        geom="histogram",
        binwidth = 5,  
        main = "Average Monthly Charge", 
        xlab = "Charge",  
        fill=I("black"), 
        col=I("red"), 
        alpha=I(.2),
        xlim=c(10,50))

o=qplot(z,
        geom="histogram",
        binwidth = 5,  
        main = "Lastest Month's Charge", 
        xlab = "Charge",  
        fill=I("black"), 
        col=I("red"), 
        alpha=I(.2),
        xlim=c(10,60))

#p=qplot(w,
# geom="histogram",
#binwidth = 5,  
#main = "Length of stay", 
#xlab = "Length in Years",  
#fill=I("black"), 
#col=I("red"), 
# alpha=I(.2),
#xlim=c(10,60))
#p
#p=barplot(prop.table(table(w)))

ggp=ggplot(mydata, aes(x=tenure_group),levels=c(10:60)) +
  geom_bar(fill="grey",col="red")+
  ggtitle("Customer's Tenure Length in Years") +
  xlab("Years") + ylab("Number that Stayed")
#scale_x_continuous(name = "Speed of cars", limits = c(10, 60))
#+labs(title = "Length of stay", x = "Years") 
ggp

grid.arrange(grobs=list(n,o,ggp),nrow = 1)

#################################
#
#
#
#
#
##
#
#
#
#
#
#
#
#MULTIVARIATE ANALYSIS
#
#
#
#
#
#
##
#
#
#
#
#
#
#
#
#
#
#
#
#
##########################
ggplot(mydata, aes(x=Churn, y=tenure_group, fill=Churn)) + 
  geom_boxplot()
#########
#histogram of churn and tenure
ggplot(mydata,aes(x=tenure_group, y=Churn, fill=Churn))+
  geom_bar(stat = "identity")

#######################
#histogram of churn and age
ggplot(mydata,aes(x=age, y=churn, fill=churn))+
  geom_bar(stat = "identity")

########################
#histogram of churn and gender
ggplot(mydata,aes(x=gender, y=Churn, fill=Churn))+
  geom_bar(stat = "identity")

#########################
#histogram of churn and gender
ggplot(mydata,aes(x=age, y=churn,group_by(churn), fill=churn))+
  geom_bar(stat = "identity")

##########################
ggplot(x=SeniorCitizen, y=Churn, fill=Churn) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=len), vjust=1.6, color="white", size=3.5)+
  theme_minimal()

#############################
#do permutation of names and plot EDA for 2d and 3d
names(mydata)
combn(length(mydata),2)

column=c("customerID","gender","SeniorCitizen","Partner","Dependents"      
         ,"tenure","PhoneService","MultipleLines","InternetService","OnlineSecurity" ,"OnlineBackup"    
         ,"DeviceProtection","TechSupport","StreamingTV","StreamingMovies" 
         ,"Contract","PaperlessBilling","PaymentMethod","MonthlyCharges"  
         ,"TotalCharges","Churn")

# Create the loop.vector (all the columns)
loop.vector <- 1:21

for(i in loop.vector){
  # loop plot all other variables against churn with churn 
  plots=ggplot(mydata, aes(x=Churn, y=mydata[,i], fill=Churn)) + 
    geom_boxplot()
  #plots + ggtitle(mydata[,i])#, subtitle = “My subtitle”)
  #p + labs(title = “Main title”, subtitle = “My subtitle”, caption = “My caption”) 
  print(plots+ggtitle(mydata[,i]))
}

for(i in loop.vector){
  # loop plot all other variables against churn without churn 
  plots=ggplot(mydata, aes(x=Churn, y=mydata[,i])) + 
    geom_boxplot()
  print(plots+ggtitle(mydata[,i]))
}

names(mydata)

############################
#1
#histogram of churn and age
plotted=ggplot(mydata, aes(x= Churn ,  group=SeniorCitizen)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count",show.legend = FALSE ) +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(title='Plot of Age Range by Churn',y = "% of Customers", fill="Senior Citizen") +
  facet_grid(~SeniorCitizen) +
  scale_y_continuous(labels = scales::percent)
#plotted + scale_fill_discrete(name="Age",
#breaks=c("0", "1"),
#labels=c("Above 65", "Below 65"))
plotted
#2
#histogram of churn and gender
plottedg<-ggplot(mydata, aes(x= Churn,  group=gender)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count",show.legend = FALSE ) +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(title='Plot of Gender by Churn',y = "Percent", fill="gender") +
  facet_grid(~gender) +
  scale_y_continuous(labels = scales::percent)
#plottedg + scale_fill_discrete(name="Age",
#breaks=c("1", "2"),
#labels=c("Customer Remained", "Customer Churned"))
plottedg
#grid.arrange(grobs=list(G1,G2,G3,G4),nrow = 2,ncol=2)
#mosaic of churn and phone service 
mydata
My_table <-structable(mydata$Churn ~ mydata$PhoneService)
mosaicplot(My_table,
           main = "Countries Mosaic Plot",
           xlab = "Colors",
           ylab = "Countries",
           color="blue"
           #shade=T, 
           #colorize = T,
           #gp = gpar(fill=matrix(c("red","blue"))))
)

#mosaic of churn and partner 
My_table <-structable(mydata$Churn ~ mydata$Partner)
My_table
mosaicplot(My_table,
           main = "Mosaic Plot of Churn against Partners",
           xlab = "Churn",
           ylab = "Partner",
           color="blue"
           #shade=T, 
           #colorize = T,
           #gp = gpar(fill=matrix(c("red","blue"))))
)

#histogram of churn and partner
plottedw<-ggplot(mydata, aes(x= Churn,  group=Partner)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Churn") +
  facet_grid(~Partner) +
  scale_y_continuous(labels = scales::percent)
#plottedw + scale_fill_discrete(name="Age",
#breaks=c("1", "2"),
#labels=c("Customer Remained", "Customer Churned"))

plottedw + theme(legend.position = "none")
plottedw
#histogram of churn and multiple lines 
plottedf<-ggplot(mydata, aes(x= Churn,  group=MultipleLines)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Churn") +
  facet_grid(~MultipleLines) +
  scale_y_continuous(labels = scales::percent)
#plottedf + scale_fill_discrete(name="Churn",
#breaks=c("Yes", "No"),
#labels=c("Customer Remained", "Customer Churned"))
plottedf + theme(legend.position = "none")
plottedf

#histogram of churn and internet service lines 
plottedd<-ggplot(mydata, aes(x= Churn,  group=InternetService)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count",show.legend = FALSE ) +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(title='Internet Service Subscription by Churn',y = "Percentage of People Subscrined", fill="Churn") +
  facet_grid(~InternetService) +
  scale_y_continuous(labels = scales::percent)
#plottedm + scale_fill_discrete(name="Churn",
#breaks=c("1", "2"),
#labels=c("Customer Remained", "Customer Churned"))
#plottedm + theme(legend.position = "none")
plottedd

##histogram of churn and OnlineSecurity 
plottedi<-ggplot(mydata, aes(x= Churn,  group=OnlineSecurity )) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Churn") +
  facet_grid(~OnlineSecurity ) +
  scale_y_continuous(labels = scales::percent)
#plottedi + scale_fill_discrete(name="Churn",
#breaks=c("1", "2"),
#labels=c("Customer Remained", "Customer Churned"))
plottedi + theme(legend.position = "none")
plottedi

##boxplot of churn and tenure 
ggplot(mydata, aes(x=Churn, y=tenure, fill=Churn)) + 
  geom_boxplot()

#histogram of churn and tenure
ggplot(mydata,aes(x=tenure, y=Churn, fill=Churn))+
  geom_bar(stat = "identity")

#ggswarm of churn and tenure 
#the longer they stay , less likely it is to leave on both churn and non churn
ggplot(mydata, 
       aes(x = Churn, 
           y = mydata$tenure, 
           color=factor(mydata$Churn))) +
  geom_quasirandom(alpha = 0.7,
                   size = 1.5) + 
  theme_minimal() +
  theme(legend.position = "none")

##histogram of churn and OnlineBackup 
plottedz<-ggplot(mydata, aes(x= tenure_group,  group=Churn )) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count",show.legend = FALSE )  +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Churn") +
  facet_grid(~OnlineBackup ) +
  scale_y_continuous(labels = scales::percent)
#plottedz + scale_fill_discrete(name="Churn",
#breaks=c("1", "2"),
#labels=c("Customer Remained", "Customer Churned"))
plottedz

#histogram of churn and DeviceProtection  
plottedx<-ggplot(mydata, aes(x= Churn,  group=DeviceProtection  )) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Churn") +
  facet_grid(~DeviceProtection ) +
  scale_y_continuous(labels = scales::percent)
plottedx + scale_fill_discrete(name="Churn",
                               breaks=c("1", "2"),
                               labels=c("Customer Remained", "Customer Churned"))
plottedx

#histogram of churn and Contract
plottedm<-ggplot(mydata, aes(x= Churn,  group=Contract  )) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count",show.legend = FALSE )+
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(title='Plot of Contract Type and Churn',y = "Percentage of Customers", fill="Churn") +
  facet_grid(~Contract ) +
  scale_y_continuous(labels = scales::percent)
#plottedm + scale_fill_discrete(name="Churn",
#breaks=c("1", "2"),
#labels=c("Customer Remained", "Customer Churned"))

#plottedm + theme(legend.position = "none")
plottedm

#mosaic of churn and PaperlessBilling
My_table <-structable(mydata$Churn ~ mydata$PaperlessBilling)
mosaic(My_table)

#histogram of churn and PaperlessBilling
plottedx<-ggplot(mydata, aes(x= Churn,  group=PaperlessBilling)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Churn") +
  facet_grid(~PaperlessBilling) +
  scale_y_continuous(labels = scales::percent)
#plottedx + scale_fill_discrete(name="Churn",
#breaks=c("1", "2"),
#labels=c("Customer Remained", "Customer Churned"))
plottedx + theme(legend.position = "none")
plottedx

#histogram for churn PaymentMethod
plottedy<-ggplot(mydata, aes(x= Churn,  group=PaymentMethod)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Churn") +
  facet_grid(~PaymentMethod) +
  scale_y_continuous(labels = scales::percent)
plottedy + scale_fill_discrete(name="Churn",
                               breaks=c("1", "2"),
                               labels=c("Customer Remained", "Customer Churned"))
plottedy

#ggswarm of churn and TotalCharges  
#totalcharges vs churn vs tenure
#while non churners are all over the place, churners are concentrated along 
#the edges indicating high charges but not age dependent 
ggplot(data = mydata, mapping = aes(x = mydata$TotalCharges/mydata$tenure, y = mydata$SeniorCitizen, 
                                    color = mydata$Churn, 
)) +
  geom_boxplot(alpha=0.2)+
  geom_smooth()#+
#facet_wrap(~MultipleLines)
##
##
##
G1=ggplot(data = mydata, mapping = aes(x =mydata$SeniorCitizen, y =  mydata$TotalCharges/mydata$tenure   , 
                                       color = Churn, 
)) +
  geom_boxplot(alpha=0.2)+
  facet_wrap(~Churn)+
  ggtitle("BoxPlot of Age and Average Monthly Charge grouped by Churn") +
  xlab("Age") + ylab("Average Monthly Charge")#+
#labs(title="Plot of length \n by dose", x ="Dose (mg)", y = "Teeth length")+
#scale_fill_discrete(name = "New Legend Title")
#+
#facet_wrap(~MultipleLines)
#descriptive statistics of total charges broken down by churners and non-churners
data(mydata)
describe.by(mydata$TotalCharges, mydata$Churn)

#histogram of total charges broken down by churn but doesnt tell us much 
ggplot(mydata, aes(x=mydata$TotalCharges,fill=Churn)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

#qqplot of total charges grouped by color
#we can see that for the same charged those who churned have higher z-values 
#than those who remained therefore higher quantiles which indicates
#that maximum values for quantiles of churners are higher than non churners 
#meaning that on average, churners churn at higher chrges than nonchurners
ggplot(mydata, aes(sample=mydata$TotalCharges ,color=factor(Churn)))+stat_qq()

#Online security and all other srivces vs total chrges separated by churners and non churners
#more people were likely to churn when they didnt subscribe service 
ggplot(data = mydata, mapping = aes(x = mydata$TotalCharges/mydata$tenure, y = mydata$OnlineSecurity, 
                                    color = mydata$Churn, 
)) +
  geom_boxplot(alpha=0.7)+
  geom_smooth()

#violin plot
ggplot(data = mydata, mapping = aes(x = TotalCharges/tenure, y = OnlineSecurity, 
                                    color = Churn, 
)) +
  geom_violin(alpha=0.7)



#dependents vs Age by churn
#interesting top left
#senior citizen=transform to cateorical
ggplot(data = mydata, mapping = aes(x =mydata$Dependents  , y = mydata$SeniorCitizen, 
                                    color = mydata$Churn, 
)) +
  geom_boxplot(alpha=0.7)+
  geom_smooth()

##partner vs Age by churn
ggplot(data = mydata, mapping = aes(x = mydata$Partner, y = mydata$SeniorCitizen, 
                                    color = mydata$Churn, 
)) +
  geom_boxplot(alpha=0.7)+
  geom_smooth()

##
##
###age vs Tenure by churn
G2=ggplot(data = mydata, mapping = aes(x =mydata$SeniorCitizen, y = mydata$tenure  ,
                                       color = Churn, 
)) +
  geom_boxplot(alpha=0.7)+
  facet_wrap(~Churn)+
  ggtitle("BoxPlot of Age and Tenure grouped by Churn") +
  xlab("Tenure(Months)") + ylab("Age")

#
#
#not interesting
#
#contract vs tenure vs churn
#most people who had longer contracts tended to remain early while those who had 
#shorter contracts tended to churn
G4=ggplot(data = mydata, mapping = aes(x =mydata$Contract, y =  mydata$tenure , 
                                       color = Churn, 
)) +
  geom_boxplot(alpha=0.7)+
  facet_wrap(~Churn)+
  ggtitle("BoxPlot of Type of Contract and Tenure grouped by Churn") +
  xlab("Contract Type") + ylab("Tenure")
G4
#payment method vs tenure vs churn
#interesting but need to do more numerical analysis to determine actual differences 
#besides bar graph
ggplot(data = mydata, mapping = aes(x = mydata$tenure, y = mydata$PaymentMethod, 
                                    color = mydata$Churn, 
)) +
  geom_point(alpha=0.7)+
  geom_smooth()

#payment method vs charges by churn
#nothing clear to see here
ggplot(data = mydata, mapping = aes(x = mydata$TotalCharge, y = mydata$PaymentMethod, 
                                    color = mydata$Churn, 
)) +
  geom_point(alpha=0.7)+
  geom_smooth()

##partner vs dependents by churn
ggplot(data = mydata, mapping = aes(x = mydata$Partner, y = mydata$Dependents, 
                                    color = mydata$Churn, 
)) +
  geom_boxplot(alpha=0.7)+
  facet_wrap(~Churn)

#multiple lines vs tenure by churn
ggplot(data = mydata, mapping = aes(x = mydata$tenure ,y = mydata$MultipleLines, 
                                    color = mydata$Churn, 
)) +
  geom_point(alpha=0.7)+
  facet_wrap(~Churn)

#
#
#
#multiple lines vs total charges by churn
G3=ggplot(data = mydata, mapping = aes(x = mydata$MultipleLines ,y = mydata$TotalCharges/mydata$tenure, 
                                       color = Churn, 
)) +
  geom_boxplot(alpha=0.7)+
  facet_wrap(~Churn)+
  ggtitle("BoxPlot of Multiple Line Service and Average Monthly Charge grouped by Churn")+ 
  xlab("Multiple Lines Subscription") + ylab("Average Monthly Charge")


#multiple lines vs ALL services by churn
ggplot(data = mydata, mapping = aes(x = mydata$MultipleLines ,y = mydata$OnlineSecurity, 
                                    color = mydata$Churn, 
)) +
  geom_point(alpha=0.7)+
  geom_smooth()

#multiple lines vs internet service by churn
ggplot(data = mydata, mapping = aes(x = mydata$MultipleLines ,y = mydata$InternetService, 
                                    color = mydata$Churn, 
)) +
  geom_point(alpha=0.7)+
  facet_wrap(~Churn)

#charges vs internet service by churn
#more churners used fiber optics consistent with multiple lines vs phone service
ggplot(data = mydata, mapping = aes(x = mydata$TotalCharges ,y = mydata$InternetService, 
                                    color = mydata$Churn, 
)) +
  geom_boxplot(alpha=0.7)+
  facet_wrap(~Churn)

#dependents vs internet service by churn(kids might make use of internet)
#fiber optics still stands out 
ggplot(data = mydata, mapping = aes(x = mydata$Dependents ,y = mydata$InternetService, 
                                    color = mydata$Churn, 
)) +
  geom_boxplot(alpha=0.7)+
  facet_wrap(~Churn)

##age vs internet service by churn
#fibre optics still stands out
ggplot(data = mydata, mapping = aes(x = mydata$SeniorCitizen ,y = mydata$InternetService, 
                                    color = mydata$Churn, 
)) +
  geom_boxplot(alpha=0.7)+
  facet_wrap(~Churn)

read(mydata)

###############
str(mydata)
###############
# Create a basic bar
pie = ggplot(df, aes(x="", y=gender, fill=gender)) + geom_bar(stat="identity", width=1)
# Convert to pie (polar coordinates) and add labels
pie = pie + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(value*100), "%")), position = position_stack(vjust = 0.5))
# Add color scale (hex colors)
pie = pie + scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999")) 
# Remove labels and add title
pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = "Phones - Market Share")
# Tidy up the theme
pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(hjust = 0.5, color = "#666666"))


grid.arrange(grobs=list(G1,G2,G3,G4),nrow = 2,ncol=2)
grid.arrange(grobs=list(plotted,plottedg,plottedm,plottedd),nrow = 1)

