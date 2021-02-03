#Code for main data ----

#loading data sets into environment
state_cases <- read.csv("United_States_COVID-19_Cases_and_Deaths_by_State_over_Time_1230.csv", header = TRUE)
state_population <- read.csv("nst-est2019-alldata.csv", header = TRUE)

#finding state abbreviations for cases and death data from CDC
unique(state_cases[order(state_cases$state),]$state)

#finding state names for Census Data on population
unique(state_population$NAME)

#Removing United States, Northeast, Midwest, South, West Regions, and Puerto Rico from Data
state_population <- state_population[-c(1, 2, 3, 4, 5, 57,14),]

#[MAIN] SECTION 2: [Data Integration] - Mapping data values & merging data sets

library(dplyr)
library(wrapr)
library(ggplot2)
library(scales)
library(plotly)
library(car)

#mapping values between data sets based on State abbreviations
map <- qc(Alabama = AL, Alaska = AK, Arizona = AZ, Arkansas = AR,
          California = CA, Colorado = CO, Connecticut = CT, Delaware = DE,
          Florida = FL, Georgia = GA, Hawaii = HI, 
          Idaho = ID, Illinois = IL, Indiana = IN, Iowa = IA, Kansas = KS,
          Kentucky = KY, Louisiana = LA, Maine = ME, Maryland = MD,
          Massachusetts = MA, Michigan = MI, Minnesota = MN, Mississippi = MS, 
          "District of Columbia" = DC,
          Missouri = MO, Montana = MT, Nebraska = NE, Nevada = NV,
          "New Hampshire" = NH, "New Jersey" = NJ, "New Mexico" = NM, "New York" = NY,
          "North Carolina" = NC, "North Dakota" = ND, Ohio = OH, Oklahoma = OK,
          Oregon = OR, Pennsylvania = PA, "Rhode Island" = RI,
          "South Carolina" = SC, "South Dakota" = SD, Tennessee = TN, Texas = TX,
          Utah = UT, Vermont = VT, Virginia = VA, Washington = WA,
          "West Virginia" = WV, Wisconsin = WI, Wyoming = WY)
state_population$STATE_AB <- map[state_population$NAME]          
state_population <- state_population %>%
  select("STATE_AB", everything())

#Only saving the the estimates for state population 
state_population <- state_population[c(1,6,3,7,18)]

# Merging two data sets based on State Abbreviations, making main data
data <- merge(state_cases, state_population, by.x="state", by.y="STATE_AB")
data$cases_perc <- data$tot_cases/data$POPESTIMATE2019*100
data$death_perc <- data$tot_death/data$tot_cases*100
data$death_perc_by_population <- data$tot_death/data$POPESTIMATE2019*100
data <- data %>%
  select(c("state", "NAME", "cases_perc", "death_perc", "death_perc_by_population"), everything())
data$submission_date <- as.Date(as.character(data$submission_date),format='%m/%d/%Y')

#Integrating potential factors data set
cofounders <- read.csv("US_states_religion_and_politics.csv", header = TRUE)
colnames(cofounders)[1]<-c("State")
urban_perc <- read.csv("PctUrbanRural_State.csv", header = TRUE)

#Only keeping columns related to Urban Area percentages and Urbanized Area percentages
urban_perc <- urban_perc[c(2,5,6,7,8,9,11,13,14)]

#it shows up as just "State" for Mac, but "?..State" for Windows, please change accordingly
additional_data <- merge(urban_perc, cofounders, by.x="STATENAME", by.y="State")

data <- merge(data, additional_data, by.x="NAME",by.y="STATENAME")

#import age_distribution_data
age_distribution <- read.csv("age_distribution_data.csv", header = TRUE)
colnames(age_distribution)[1]<-c("State")
data <- merge(data, age_distribution, by.x="NAME", by.y="State")

#Cleaning the environment of unnecessary data sets
remove(cofounders,urban_perc,state_population,state_cases,age_distribution,additional_data,map)

#If we want to only look at the most recent situation, the day our data was last collected: 
#current_situation<-data%>%filter(submission_date=="10/24/2020") 

#[MAIN] SECTION 3: [Data Modification] - Finding the date of the first case for each State 

library(plyr)

#filtering dates with 0 total cases
initial <- data%>%
  filter(tot_cases!=0)

start_dates <- ddply(initial, .var = "NAME", .fun = function(x) {
  return(subset(x, tot_cases %in% min(tot_cases)))
}
)

start_dates <- ddply(start_dates, .var = "NAME", .fun = function(x) {
  return(subset(x, submission_date %in% min(submission_date)))
}
)

#only need columns "Name" and "start_date" to merge
start_dates <- start_dates[c(1,6)]
colnames(start_dates) <- c("Name","start_date")

#Merge start_dates with original data with State Names
data_adjusted <- merge(start_dates, data, by.x="Name", by.y="NAME")

#Creating column "days_since_first_case" for a numerical determination of how long Covid-19 has been in the state
data_adjusted <- data_adjusted%>%
  mutate(days_since_first_case = as.numeric(submission_date-start_date))%>%
  mutate(days_since_start = as.numeric(submission_date-as.Date('2020-01-22')))%>%  
  select(c("Name", "submission_date", "days_since_first_case", "days_since_start"), everything())%>%
  filter(days_since_first_case >= 0)%>%filter(submission_date<as.Date('2020-12-13'))

#To look at data under a chronological, alphabetical ordered format:
#click the header of column "submission_date" first, then click the header of column "Name"

#remove unnecessary data from environment: initial, cofounders, and start_dates from environment
remove(start_dates)
remove(initial)

#[MAIN] SECTION 4: [Data Clustering] - Grouping States by Polynomial Regression Coefficients 

#Running and Returning Coefficient and its Signs from linear model tests
data_adjusted <- data_adjusted[-c(60,61)]

#Death_perc_pop
coefficients <- data.frame("hi","hi")
colnames(coefficients)<-c("State","Death_Perc_Pop_Coefficient")
for (name in unique(data_adjusted$Name)){
  T <- data_adjusted %>%
    filter(Name==name)
  regression <- lm(death_perc_by_population ~ days_since_first_case + I(days_since_first_case^2), data = T)
  coefficients <- rbind(coefficients,c(name,regression$coefficients[3]))
}

coefficients <- coefficients[-1,]

#updating data_adjusted with coefficient signs 
data_adjusted <- merge(data_adjusted,coefficients,by.x="Name", by.y="State")

death_pop_stat <- rep(0,nrow(data_adjusted))
for (i in 1:nrow(data_adjusted)){
  if (data_adjusted$Death_Perc_Pop_Coefficient[i]>0){death_pop_stat[i]<-1}
}
data_adjusted <- cbind(data_adjusted,death_pop_stat)

data_adjusted$death_pop_stat <- factor(data_adjusted$death_pop_stat, 
                                       levels = c("0", "1"), 
                                       labels = c("Negative", "Positive"))


#remove unnecessary data from environment: name, T, and regression
remove(name)
remove(T)
remove(regression)
remove(death_pop_stat)
remove(i)
remove(coefficients)
remove(data)


monthly_test <- data_adjusted %>%
  filter(days_since_first_case == 0 | days_since_first_case == 30 | days_since_first_case == 60 | days_since_first_case == 90|
           days_since_first_case == 120 | days_since_first_case == 150 | days_since_first_case == 180 |
           days_since_first_case == 210 | days_since_first_case == 240 | days_since_first_case == 270)
monthly_test$new_death <- 0
monthly_test <- monthly_test[with(monthly_test, order(submission_date)),]
monthly_test <- monthly_test[with(monthly_test, order(Name)),]
row.names(monthly_test) <- NULL
for (c in 1:nrow(monthly_test)){
  monthly_test[c,18] <- ifelse(monthly_test[c,2]==monthly_test[c,5], monthly_test[c,15], monthly_test[c,15]-monthly_test[c-1,15])
}
monthly_test$monthly_death_pop <- monthly_test$new_death/monthly_test$POPESTIMATE2019*100
monthly_test<-monthly_test%>%
  filter(!is.na(monthly_death_pop))

monthly_test2 <- as.data.frame(tapply(monthly_test$monthly_death_pop, monthly_test$Name,median))
colnames(monthly_test2) <- c("Median_Death_Rate_by_Month") 
monthly_test2 <- tibble::rownames_to_column(monthly_test2, "Name")
data <- merge(monthly_test2, monthly_test, by.x="Name", by.y="Name")

covid_monthly <- data
colnames(covid_monthly)[1] <- 'STATENAME'

monthly <- covid_monthly[,c(1,63,4)]
month<-rbind(monthly$monthly_death_pop[1:10])

for (c in 1:49){
  month<-rbind(month,rbind(monthly$monthly_death_pop[((c*10)+1):(10*(c+1))]))
}
row.names(month)<-unique(monthly$STATENAME)
month <- month[,-c(1)]

library(amap)
cluster2 <- Dist(month,method='correlation')
fits<-hclust(cluster2, method="complete")

test <- cutree(fits, k=2)
test <- as.data.frame(test,row.names =NULL)
test <- tibble::rownames_to_column(test, "State")
covid_monthly <- merge(covid_monthly, test, by.x = 'STATENAME', by.y = 'State')
colnames(covid_monthly)[ncol(covid_monthly)] <- "corr2_label"

#corr4_3_label
test <- cutree(fits, k=4)
test <- as.data.frame(test,row.names =NULL)
test <- tibble::rownames_to_column(test, "State")
covid_monthly <- merge(covid_monthly, test, by.x = 'STATENAME', by.y = 'State')
colnames(covid_monthly)[ncol(covid_monthly)] <- "corr4_label"


covid_monthly$corr2_label <- factor(covid_monthly$corr2_label, 
                                    levels = c("1", "2"), 
                                    labels = c("Gradual", "Spike"))
covid_monthly$corr4_3_label= case_when(covid_monthly$corr4_label == 3 ~ "Initial_Spike",
                                       covid_monthly$corr4_label == 4 ~ "Initial_Spike",
                                       covid_monthly$corr4_label == 2 ~ "Two_Humped",
                                       covid_monthly$corr4_label == 1 ~ "Recent_Spike")

covid_monthly$corr3_label= case_when(covid_monthly$corr4_3_label == "Initial_Spike" ~ 1,
                                     covid_monthly$corr4_3_label == "Recent_Spike" ~ 3,
                                     covid_monthly$corr4_3_label == "Two_Humped" ~ 2)
data_adjusted$daily_death_pop <- data_adjusted$new_death/data_adjusted$POPESTIMATE2019*100
newest <- newest <- ddply(covid_monthly[,c(1,4,64,65,66,67)], .var = "STATENAME", .fun = function(x) {
  return(subset(x, days_since_first_case %in% max(days_since_first_case)))
}
) 
newest <- newest[,-c(2)]
data_adjusted <- merge(data_adjusted, newest,by.x= "Name", by.y="STATENAME")
data_adjusted  <- data_adjusted [with(data_adjusted , order(submission_date)),]
data_adjusted  <- data_adjusted [with(data_adjusted , order(Name)),]
row.names(data_adjusted ) <- NULL


#cumulative
usa <- as.data.frame(tapply(data_adjusted$tot_cases, data_adjusted$submission_date, sum))
usa<-cbind(usa,tapply(data_adjusted$tot_death, data_adjusted$submission_date, sum))
usa<-cbind(usa,tapply(data_adjusted$POPESTIMATE2019, data_adjusted$submission_date, sum))
colnames(usa)<-c('total_cases','total_death','total_population')
usa$total_death_perc <- usa$total_death/usa$total_population*100
usa$total_cases_perc <- usa$total_cases/usa$total_population*100
usa <- tibble::rownames_to_column(usa, "submission_date")
usa$submission_date <- as.Date(usa$submission_date)

simulation <- data_adjusted[data_adjusted$days_since_first_case<271,]
usa2 <- as.data.frame(tapply(simulation$tot_death, simulation$days_since_first_case, sum))
usa2<-cbind(usa2,tapply(simulation$tot_cases, simulation$days_since_first_case, sum))
usa2<-cbind(usa2,tapply(simulation$POPESTIMATE2019, simulation$days_since_first_case, sum))
colnames(usa2)<-c('total_death','total_cases','total_population')
usa2$total_death_perc <- usa2$total_death/usa2$total_population*100
usa2$total_cases_perc <- usa2$total_cases/usa2$total_population*100
usa2$mortality_rate <- usa2$total_death/usa2$total_cases
usa2 <- tibble::rownames_to_column(usa2, "days_since_first_case")
usa2$days_since_first_case <- as.numeric(usa2$days_since_first_case)

#regression grouped
negative<-simulation[simulation$death_pop_stat=='Negative',]
positive<-simulation[simulation$death_pop_stat=='Positive',]
regression <- cbind(as.data.frame(tapply(negative$tot_death, negative$days_since_first_case, sum)),
                    tapply(negative$tot_cases, negative$days_since_first_case, sum),rep(c('Negative'),each=271))
colnames(regression)<-c('total_death','total_cases','regression')
regression <- tibble::rownames_to_column(regression, "days_since_first_case")
temp <- cbind(as.data.frame(tapply(positive$tot_death, positive$days_since_first_case, sum)),
              tapply(positive$tot_cases, positive$days_since_first_case, sum),rep(c('Positive'),each=271))
colnames(temp)<-c('total_death','total_cases','regression')
temp <- tibble::rownames_to_column(temp, "days_since_first_case")
regression <- rbind(regression,temp)
temp <- cbind(as.data.frame(tapply(simulation$tot_death, simulation$days_since_first_case, sum)),
              tapply(simulation$tot_cases, simulation$days_since_first_case, sum),rep(c('Composite'),each=271))
colnames(temp)<-c('total_death','total_cases','regression')
temp <- tibble::rownames_to_column(temp, "days_since_first_case")
regression <- rbind(regression,temp)
regression$days_since_first_case <- as.numeric(regression$days_since_first_case)


new_death <- regression$total_death
new_cases <- regression$total_cases
for (i in (c(0,271,542))){
  for (c in (1:271)){
    if (c>1){
      index = i+c
      new_death[index]<- regression$total_death[index]-regression$total_death[index-1]
      new_cases[index]<- regression$total_cases[index]-regression$total_cases[index-1]  
    }}}

regression$new_death <- new_death
regression$new_cases <- new_cases
regression$mortality_rate <- regression$total_death/regression$total_cases
regression$new_mortality_rate <- regression$new_death/regression$new_cases
regression$total_death_perc <- regression$total_death/327533774*100
regression$total_cases_perc <- regression$total_cases/327533774*100
regression<-regression[regression$new_death>0,]
regression<-regression[regression$new_cases>0,]
regression$new_death_perc <- regression$new_death/327533774*100
regression$new_cases_perc <- regression$new_cases/327533774*100
regression_max <- ddply(regression, .var = "regression", .fun = function(x) {
  return(subset(x, total_death %in% max(total_death))[,c(2,4)])
}
) 
colnames(regression_max) <- c('max_death','regression')
regression <- merge(regression, regression_max, by.x="regression", by.y="regression")
regression$adjusted_new_death_perc <- regression$new_death/regression$max_death*100

#corr4_3_label grouped
initial_spike<-simulation[simulation$corr4_3_label=='Initial_Spike',]
recent_spike<-simulation[simulation$corr4_3_label=='Recent_Spike',]
two_humped<-simulation[simulation$corr4_3_label=='Two_Humped',]
cluster <- cbind(as.data.frame(tapply(initial_spike$tot_death, initial_spike$days_since_first_case, sum)),
                 tapply(initial_spike$tot_cases, initial_spike$days_since_first_case, sum),rep(c('Initial_Spike'),each=271))
colnames(cluster)<-c('total_death','total_cases','correlation_cluster')
cluster <- tibble::rownames_to_column(cluster, "days_since_first_case")
temp <- cbind(as.data.frame(tapply(recent_spike$tot_death, recent_spike$days_since_first_case, sum)),
              tapply(recent_spike$tot_cases, recent_spike$days_since_first_case, sum),rep(c('recent_Spike'),each=271))
colnames(temp)<-c('total_death','total_cases','correlation_cluster')
temp <- tibble::rownames_to_column(temp, "days_since_first_case")
cluster <- rbind(cluster,temp)
temp <- cbind(as.data.frame(tapply(two_humped$tot_death, two_humped$days_since_first_case, sum)),
              tapply(two_humped$tot_cases, two_humped$days_since_first_case, sum),rep(c('Two_humped'),each=271))
colnames(temp)<-c('total_death','total_cases','correlation_cluster')
temp <- tibble::rownames_to_column(temp, "days_since_first_case")
cluster <- rbind(cluster,temp)
temp <- cbind(as.data.frame(tapply(simulation$tot_death, simulation$days_since_first_case, sum)),
              tapply(simulation$tot_cases, simulation$days_since_first_case, sum),rep(c('Composite'),each=271))
colnames(temp)<-c('total_death','total_cases','correlation_cluster')
temp <- tibble::rownames_to_column(temp, "days_since_first_case")
cluster <- rbind(cluster,temp)
cluster$days_since_first_case <- as.numeric(cluster$days_since_first_case)


new_death <- cluster$total_death
new_cases <- cluster$total_cases
for (i in (c(0,271,542,813))){
  for (c in (1:271)){
    if (c>1){
      index = i+c
      new_death[index]<- cluster$total_death[index]-cluster$total_death[index-1]
      new_cases[index]<- cluster$total_cases[index]-cluster$total_cases[index-1]  
    }}}

cluster$new_death <- new_death
cluster$new_cases <- new_cases
cluster$mortality_rate <- cluster$total_death/cluster$total_cases
cluster$new_mortality_rate <- cluster$new_death/cluster$new_cases
cluster$total_death_perc <- cluster$total_death/327533774*100
cluster$total_cases_perc <- cluster$total_cases/327533774*100
cluster<-cluster[cluster$new_death>0,]
cluster<-cluster[cluster$new_cases>0,]
cluster$new_death_perc <- cluster$new_death/327533774*100
cluster$new_cases_perc <- cluster$new_cases/327533774*100
simulation <- simulation[simulation$new_death>=0,]
remove(new_cases, new_death, index, i, c, initial_spike, 
       recent_spike, two_humped, test, temp, monthly_test,
       monthly_test2, monthly, month, negative, positive, data, regression_max)
newest <- ddply(covid_monthly, .var = "STATENAME", .fun = function(x) {
  return(subset(x, days_since_first_case %in% max(days_since_first_case)))
}
)


## Figure 1
#Figure 1a. Cumulative COVID-19 Death Percentage in 50 States
ggplot(simulation, aes(x=days_since_first_case, y=death_perc_by_population, group=Name,color=death_pop_stat)) +
  geom_line()+ labs(y="COVID-19 Death Percentage (%)",
                    x="Days since first case",color=NULL) +scale_color_manual(values=c("#00BFC4","#F8766D")) +
  theme(
    legend.position = c(0.03, 0.97),
    legend.justification = c("left", "top")
  )
#Figure 1b. Daily New COVID-19 Death Percentage in 50 States
ggplot(regression[regression$regression!='Composite',], aes(x=days_since_first_case, y=new_death_perc, group = regression, color = regression)) +
  geom_line() + labs(y = " Daily New COVID-19 Death Percentage (%)", x="Days since first case",color=NULL)+
  scale_color_manual(values=c('#00BFC4','#F8766D')) +
  theme(
    legend.position = c(0.97, 0.97),
    legend.justification = c("right", "top")
  )
#Figure 1c. Monthly New Death  Percentage in 50 States
ggplot(covid_monthly, aes(x=days_since_first_case, y=monthly_death_pop, group=STATENAME,color=death_pop_stat)) +
  geom_line()+ labs(y="Monthly New COVID-19 Death Percentage (%)", x="Days since first case",color=NULL) +scale_color_manual(values=c("#00BFC4","#F8766D"))  +
  theme(
    legend.position = c(0.85, 0.97),
    legend.justification = c("right", "top")
  )

#Figure 2. Agglomerative Hierarchical Clustering using Correlation as Similarity Measure based on Monthly New Death Percentages
plot(fits, label=rownames(cluster2), hang=-1, main = NULL, ylab = NULL, xlab = NULL)
rect.hclust(fits, k=4, border="red")

## Figure 3
#Figure 3a: Monthly New Death Percentage in 50 States Marked by Correlation Clusters
ggplot(covid_monthly, aes(x=days_since_first_case, y=monthly_death_pop, group=STATENAME,color=corr4_3_label)) +
  geom_line()+ labs(y="Monthly New COVID-19 Deaths Percentage (%)",
                    x="Days Since First Case",color=NULL) +scale_color_manual(values=c("#6fcfd1",'#e66155', '#faa134'))  +
  theme(
    legend.position = c(0.85, 0.97),
    legend.justification = c("right", "top")
  )
#Figure 3b. Daily New COVID-19 Death Percentage in Correlation Clusters
ggplot(cluster, aes(x=days_since_first_case, y=new_death_perc, group = correlation_cluster, color = correlation_cluster)) +
  geom_line() + labs(y = "Daily New COVID-19 Death Percentage (%)", x="Days since first case",color=NULL)+
  scale_color_manual(values=c('#9e9e9e',"#6fcfd1",'#e66155', '#faa134'))  +
  theme(
    legend.position = c(0.85, 0.97),
    legend.justification = c("right", "top")
  )
#Figure 3c. Correlation Cluster Map US
all_states <- map_data("state")
newest$STATENAME <- tolower(newest$STATENAME)
temp <- newest[,c(1,8,9,10,62,66)]
dc <- as.data.frame(t(as.data.frame(as.vector(rep(0,6)))))
dc[c(1,6)]<-c('district of columbia','Initial_Spike')
colnames(dc)<-colnames(temp)
temp <- rbind(temp,dc)
all_states <- merge(all_states, temp, by.x="region", by.y="STATENAME")
p <- ggplot(data = all_states,
            aes(x = long, y = lat,
                group = group, fill = as.factor(corr4_3_label),
                text = paste0(region,'<br>Cases Perc:', cases_perc, 
                              '<br>Death Perc_pop:', death_perc_by_population,
                              '<br>Death Perc:', death_perc,
                              '<br>death_pop_stat:', death_pop_stat)))+ geom_polygon(color = "gray90", size = 0.1)  + 
  scale_fill_manual(values = c("#6fcfd1",'#e66155', '#faa134'))+
  labs(title = NULL, x = NULL, y = NULL, fill = NULL) +
  theme(
    legend.position = c(0.03, 0.03),
    legend.justification = c("left", "bottom")
  )
p

## Figure 4
#Figure 4a.
boxplot(AREAPCT_URBAN~as.factor(corr4_3_label), data = newest, xlab = "", 
        ylab = "Urban Area Percentage (%)", main = "",col=c("#6fcfd1",'#e66155', '#faa134'))
#Figure 4b.
boxplot(POPPCT_URBAN~as.factor(corr4_3_label), data = newest, xlab = "", 
        ylab = "Urban Population Percentage (%)", main = "",col=c("#6fcfd1",'#e66155', '#faa134'))

## Figure 5
#Figure 5a.
boxplot(cases_perc~as.factor(corr4_3_label), data = newest, xlab = "", 
        ylab = "Case Percentage (%)", main = "",col=c("#6fcfd1",'#e66155', '#faa134'))
#Figure 5b.
boxplot(death_perc~as.factor(corr4_3_label), data = newest, xlab = "", 
        ylab = "Mortality Rate (%)", main = "",col=c("#6fcfd1",'#e66155', '#faa134'))
#Figure 5c.
boxplot(death_perc_by_population~as.factor(corr4_3_label), data = newest, xlab = "", 
        ylab = "Death Percentage (%)", main = "",col=c("#6fcfd1",'#e66155', '#faa134'))


# Code for Entropy--DAYS SINCE FIRST CASE  ----
county <- read.csv('PctUrbanRural_County.csv', header=TRUE)
county2<-county%>%
  filter(STATENAME!='District of Columbia' & STATENAME!='Puerto Rico')
newest <- ddply(covid_monthly, .var = "STATENAME", .fun = function(x) {
  return(subset(x, days_since_first_case %in% max(days_since_first_case)))
}
) 
test_data_25 <- data_adjusted%>%
  filter(tot_death > 10)
test_data_25 <- ddply(test_data_25[c(1,2)], .var = "Name", .fun = function(x) {
  return(subset(x, submission_date %in% min(submission_date)))
}
)
names(test_data_25) <- c("Name", "First_benchmark_date")
newest <- merge(newest, test_data_25, by.x="STATENAME", by.y="Name")
state_info <- newest
#C ADJUSTED
adjusted_entropy <- as.data.frame(unique(county2$STATENAME))
colnames(adjusted_entropy)<-'State'
index <- c(9,14)
for (num in index){
  data5 <- c(0)
  for (name in unique(county2$STATENAME)){
    hold <- 0
    temp <- county2[county2$STATENAME==name,]
    for (i in 1:nrow(temp)){
      if (temp[i,num] == 0){
        next
      }       
      c = temp[i,num]/sum(temp$AREA_COU)
      product = -c*log(c)
      hold <- cbind(hold,product)    
    } 
    c = sum(temp[,num])/sum(temp$AREA_COU)
    hold <- cbind(sum(hold)/c-log(sum(temp$AREA_COU)))
    data5 <- rbind(data5, hold)
  }
  data5 <- as.data.frame(data5[-c(1),])
  rownames(data5)<-NULL
  colnames(data5) <- c(paste('Final_Entropy_',colnames(county[num]),sep=''))
  data5 <- sapply(data5, as.numeric)
  adjusted_entropy <- cbind(adjusted_entropy,data5)
}


state_info <- merge(state_info, adjusted_entropy[,c(1,2,3)], by.x='STATENAME',by.y='State')

#C ADJUSTED
index <- c(7,12)
for (num in index){
  data5 <- c(0)
  for (name in unique(county2$STATENAME)){
    hold <- 0
    temp <- county2[county2$STATENAME==name,]
    for (i in 1:nrow(temp)){
      if (temp[i,num] == 0){
        next
      }       
      c = temp[i,num]/sum(temp$POP_COU)
      product = -c*log(c)
      hold <- cbind(hold,product)    
    } 
    c = sum(temp[,num])/sum(temp$POP_COU)
    hold <- cbind(sum(hold)/c-log(sum(temp$POP_COU)))
    data5 <- rbind(data5, hold)
  }
  data5 <- as.data.frame(data5[-c(1),])
  rownames(data5)<-NULL
  colnames(data5) <- c(paste('Final_Entropy_',colnames(county[num]),sep=''))
  data5 <- sapply(data5, as.numeric)
  adjusted_entropy <- cbind(adjusted_entropy,data5)
}

remove(product,num,name,index,i,c,data5,hold,county,temp)
state_info <- merge(state_info, adjusted_entropy[,c(1,4,5)], by.x='STATENAME',by.y='State')
state_info$start_date <- as.Date(state_info$start_date)
state_info$start_day <- as.numeric(state_info$start_date - as.Date('2020-01-22'))
state_info$bench_day <- as.numeric(state_info$First_benchmark_date - as.Date('2020-01-22'))

## Figure 6
#Figure 6a. Scatterplot of Urban Area Entropy and Log Mortality Rate on 270 Days since First Case 
ggplot(state_info, aes(x=Final_Entropy_AREA_URBAN, y=log(death_perc), color=corr4_3_label)) +
  geom_point() + labs(y = "log(Mortality Rate %)", x='Urban Area Entropy', color=NULL)+
  scale_color_manual(values=c("#6fcfd1",'#e66155', '#faa134')) +
  theme(
    legend.position = c(0.97, 0.97),
    legend.justification = c("right", "top")
  )
#Figure 6b. Scatterplot of Urban Population Entropy and Log Mortality Rate on 270 Days since First Case 
ggplot(state_info, aes(x=Final_Entropy_POP_URBAN, y=log(death_perc), color=corr4_3_label)) +
  geom_point() + labs(y = "log(Mortality Rate %)", x='Urban Population Entropy', color=NULL)+
  scale_color_manual(values=c("#6fcfd1",'#e66155', '#faa134')) +
  theme(
    legend.position = c(0.97, 0.97),
    legend.justification = c("right", "top")
  )
#Figure 6c. Scatterplot of Urban Area Entropy and Case Percentages on 270 Days since First Case 
ggplot(state_info, aes(x=Final_Entropy_AREA_URBAN, y=cases_perc, color=corr4_3_label)) +
  geom_point() + labs(y = "Case Percentage (%)", x='Urban Area Entropy', color=NULL)+
  scale_color_manual(values=c("#6fcfd1",'#e66155', '#faa134')) +
  theme(
    legend.position = c(0.03, 0.97),
    legend.justification = c("left", "top")
  )

## Figure 7
#Figure 7a.
boxplot(Final_Entropy_AREA_URBAN~corr4_3_label, data = state_info, xlab = "", 
        ylab = "Urban Area Entropy", main = "",col=c("#6fcfd1",'#e66155', '#faa134'))
#Figure 7b.
boxplot(Final_Entropy_POP_URBAN~corr4_3_label, data = state_info, xlab = "", 
        ylab = "Urban Population Entropy", main = "",col=c("#6fcfd1",'#e66155', '#faa134'))

##Table 1
# row=1, column=1
summary(lm(Final_Entropy_AREA_URBAN~log(death_perc), data = state_info))
cor.test(state_info$Final_Entropy_AREA_URBAN, log(state_info$death_perc), method = c("pearson"))
cor.test(state_info$Final_Entropy_AREA_URBAN, log(state_info$death_perc), method = c("spearman"))

# row=1, column=2
summary(lm(Final_Entropy_AREA_URBAN~log(death_perc_by_population), data = state_info))
cor.test(state_info$Final_Entropy_AREA_URBAN, log(state_info$death_perc_by_population), method = c("pearson"))
cor.test(state_info$Final_Entropy_AREA_URBAN, log(state_info$death_perc_by_population), method = c("spearman"))

# row=1, column=3
summary(lm(Final_Entropy_AREA_URBAN~cases_perc, data = state_info))
cor.test(state_info$Final_Entropy_AREA_URBAN, state_info$cases_perc, method = c("pearson"))
cor.test(state_info$Final_Entropy_AREA_URBAN, state_info$cases_perc, method = c("spearman"))

# row=1, column=4
my_anova <- lm(Final_Entropy_AREA_URBAN~corr4_3_label, data=state_info)
Anova(my_anova)
summary(lm(Final_Entropy_AREA_URBAN~corr4_3_label, data=state_info))

#row=2, column=1
summary(lm(Final_Entropy_POP_URBAN~log(death_perc), data = state_info))
cor.test(state_info$Final_Entropy_POP_URBAN, log(state_info$death_perc), method = c("pearson"))
cor.test(state_info$Final_Entropy_POP_URBAN, log(state_info$death_perc), method = c("spearman"))

#row=2, column=2
summary(lm(Final_Entropy_POP_URBAN~log(death_perc_by_population), data = state_info))
cor.test(state_info$Final_Entropy_POP_URBAN, log(state_info$death_perc_by_population), method = c("pearson"))
cor.test(state_info$Final_Entropy_POP_URBAN, log(state_info$death_perc_by_population), method = c("spearman"))

#row=2, column=3
summary(lm(Final_Entropy_POP_URBAN~cases_perc, data = state_info))
cor.test(state_info$Final_Entropy_POP_URBAN, state_info$cases_perc, method = c("pearson"))
cor.test(state_info$Final_Entropy_POP_URBAN, state_info$cases_perc, method = c("spearman"))

#row=2, column=4
my_anova <- lm(Final_Entropy_POP_URBAN~corr4_3_label, data=state_info)
Anova(my_anova)
summary(lm(Final_Entropy_POP_URBAN~corr4_3_label, data=state_info))


