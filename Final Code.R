###############################################################################
# FINAL PROJECT CODE
############################

#Set Up ----------------------------------------------------------------------

library(rlang)
library(psych)
library(ggplot2); library(ggthemes); library(gridExtra) #Plot graphs 
library(dplyr); library(tidyr); library(tidyverse) #Data wrangling
library(skimr) #Reading graphical summaries of data
library(lubridate) #Dates
library(plyr); library(mice)
library(quantmod);library(xts);library(zoo) # Using xts class objects
library(forecast) #Set of forecasting functions
library(fpp); library(fpp2) #Datasets from Forecasting text by Rob Hyndman
library(tseries) #Statistical test
library(stringr) #String tidying
library(ggmap) #Plot maps
library(RColorBrewer) #Color scale on maps


View(data)
str(data)


###############################################################################
# TIME-SERIES ANALYSIS --------------------------------------------------------
###############################################################################


#The plot of count of all crime cases over months in 2019-2021 in LA ----
crime_each_month_every_year <- count(ts_data, "Month_Yr")
print(crime_each_month_every_year)
crime_each_month_every_year_drop <- crime_each_month_every_year %>% filter(Month_Yr != '0022-04')
print(crime_each_month_every_year_drop) # I dropped the row of 2022-04 because it was significantly lower than rest of the data, and I think it is because the collection for data in April is not finished yet. 

ggplot(data=crime_each_month_every_year_drop, aes(x=Month_Yr, y=freq, group=1)) +
  geom_line()+
  geom_point()+
  xlab("Time(Year-Month)") +
  scale_y_continuous(name="Total Count of Crime Cases per Month") +
  ggtitle("Monthly Count of Crime Cases from 2019-01 to 2021-03") +
  theme(axis.text.x = element_text(angle = 45))

#The plot of count of all crime cases over 365 days in 2021 in LA ----
crime_21 <- ts_data %>% filter(Month_Yr == c('0021-01', '0021-02', '0021-03', '0021-04', '0021-05', '0021-06', '0021-07', '0021-08', '0021-09', '0021-10', '0021-11', '0021-12'))
crime_each_day <- count(crime_21, "DATE.OCC")
print(crime_each_day)

ggplot(data=crime_each_day, aes(x=DATE.OCC, y=freq, group=1)) +
  geom_line()+
  xlab("Date") + ylab("Total Count of Crime Cases per Day") +
  ggtitle("Daily Count of Crime Cases in 2021")

#Plot of Top 5 crime types in July ----
crime_type_7_top5 <- top_n(crime_type_7_order, n=5, count)
View(crime_type_7_top5)

ggplot(data=crime_type_7_top5, aes(x=Crm.Cd.Desc, y=count)) +
  geom_bar(stat='identity')+
  xlab("Type of Crime") + ylab("Total Count of Crime Case") + ggtitle("Top 5 Kinds of Crime in July 2021") +
  theme(axis.text.x = element_text(color = "gray",angle = 45))


#Plot of Top 5 crime types in October ----
crime_type_10_top5 <- top_n(crime_type_10_order, n=5, count)
View(crime_type_10_top5)

ggplot(data=crime_type_10_top5, aes(x=Crm.Cd.Desc, y=count)) +
  geom_bar(stat='identity')+
  xlab("Type of Crime") + ylab("Total Count of Crime Case") + ggtitle("Top 5 Kinds of Crime in October") +
  scale_x_discrete('Type of Crime', labels = c('a', 'b', 'c', 'd', 'e'))
View(crime_type_10_top5)

# try to do the daily analysis ----
ym1 = as.data.frame(table(ts_data$Month_Yr))
ym1 = ym1[-c(40), ]
ym_raw = ym1[,2]

#stationary process ----
monthly = ts(ym_raw , frequency = 12, start = c(2019,1))
mon_const_var = BoxCox(monthly,lambda = BoxCox.lambda(monthly))
mon_const_var_no_seasonality = diff(x = mon_const_var,lag = 12) 

month1 <- diff(monthly,differences = 1)	

#plot the stationary process ----
dat = cbind(original = monthly,
            const_var = month1,
            no_seasonality = mon_const_var_no_seasonality)
library(ggthemes)
autoplot(dat,facets=T,colour = T)+ylab('')+xlab('Year')+theme_bw()

#ARIMA model ----
m8 <- Arima(monthly, order=c(0,1,1), seasonal = list(order = c(0,2,1), period = 12))
m8.predict <- forecast:::forecast.Arima(m8, h = 12, level = c(50, 90))
plot(m8.predict)


###############################################################################
# SPATIAL ANALYSIS ------------------------------------------------------------
###############################################################################


#Count of every criminal activity by type ----
sort(unique(data$crm.type))
sort(table(data$crm.type), decreasing = T)


#Exclude "Others" criminal activity types to exclude. Note: crm.type ==Others is miscellaneous vs crm.type.cat == others ----
newData <- data[!data$crm.type == "Others", ]
table(newData$crm.type, newData$year)


#Now, let's proceed with mapping and graphing ----
if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap")
library(ggmap)
library(tmap)

register_google(key = "API_key")

la_map <- get_map(location = c(-118.3436849, 34.0522342),
                  zoom = 10, scale = 4, maptype = "toner")

#Mapping of violent, financial, or other crimes  from 2020 to present ----
newData_map <- newData_map[!newData_map$year == "2019", ]
ggmap(la_map) +
  geom_point(data = newData_map, 
             mapping = aes(x = LON, 
                           y = LAT, 
                           color = crm.type.cat),
             size = 0.5) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)


#Let's look year 2020 and 2021 ----

#Showing graph of crime rates increasing each year except 2019 since we're looking from the beginning of the pandemic that started in 2020
year1 <- newData_map[newData_map$year == "2020", ]
year2 <- newData_map[newData_map$year == "2021", ]


#2020 ----
ggmap(la_map) +
  geom_point(data = year1, 
             mapping = aes(x = LON, 
                           y = LAT, 
                           color = crm.type.cat),
             size = 0.1) +
  xlab("Longitude") +
  ylab("Latitude")  +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)


#2021 ----
ggmap(la_map) +
  geom_point(data = year2, 
             mapping = aes(x = LON, 
                           y = LAT, 
                           color = crm.type.cat),
             size = 0.1) +
  xlab("Longitude") +
  ylab("Latitude")  +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)


#Understand criminal activities by the police patrolling community areas ----

#8 = West LA Area ----
wla_map <- get_map(location = c(-118.465200, 34.071200), 
                   zoom = 12, scale = 8, maptype = "toner")
wla <- newData_map[newData_map$AREA.NAME == "West LA", ]
ggmap(wla_map, darken = c(0.2, "white")) +
  geom_point(data = wla, 
             mapping = aes(x = LON,y = LAT, color = crm.type.cat), size = 1.5) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)

table(wla$crm.type)/nrow(data)*100


#12 = 77th Street ----
st77_map <- get_map(location = c(-118.307500, 33.970300), 
                    zoom = 13, scale = 8, maptype = "toner")
st77 <- newData_map[newData_map$AREA.NAME == "77th Street", ]
ggmap(st77_map, darken = c(0.2, "white")) +
  geom_point(data = st77, 
             mapping = aes(x = LON,y = LAT, color = crm.type.cat), size = 1.5) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)

table(st77$crm.type)/nrow(data)*100


#14 = Pacific ----
pac_map <- get_map(location = c(-118.426200, 33.962300), 
                   zoom = 12, scale = 8, maptype = "toner")
pac <- newData_map[newData_map$AREA.NAME == "Pacific", ]
ggmap(pac_map, darken = c(0.2, "white")) +
  geom_point(data = pac, 
             mapping = aes(x = LON,y = LAT, color = crm.type.cat), size = 1.5) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)

table(pac$crm.type)/nrow(data)*100


#18 = Southeast ----
se_map <- get_map(location = c(-118.243683, 33.922235), 
                  zoom = 12, scale = 8, maptype = "toner")
se <- newData_map[newData_map$AREA.NAME == "Southeast", ]
ggmap(se_map, darken = c(0.2, "white")) +
  geom_point(data = se, 
             mapping = aes(x = LON,y = LAT, color = crm.type.cat), size = 1.35) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)

table(se$crm.type)/nrow(data)*100


#Compare proportions of areas by crm.type ----
CrimeArea_Prop <- round(prop.table(table(newData_map$AREA.NAME, newData_map$crm.type.cat), 
                                   1), digits = 4)*100
CrimeArea_Prop <- as.data.frame.matrix(CrimeArea_Prop)
class(CrimeArea_Prop)

#Top 6 and Bottom 6 Locations with high and low violent crime rate
CrimeArea_Prop_Violent <- CrimeArea_Prop %>%
  select(`Violent Crime`) %>%
  arrange(desc(`Violent Crime`))
CrimeArea_Prop_Violent

CrimeArea_Prop_Violent_high <- CrimeArea_Prop_Violent %>%
  select(`Violent Crime`) %>%
  arrange(desc(`Violent Crime`)) %>%
  head(n = 5) 
CrimeArea_Prop_Violent_high

CrimeArea_Prop_Violent_low <- CrimeArea_Prop_Violent %>%
  select(`Violent Crime`) %>%
  arrange(desc(`Violent Crime`)) %>%
  tail(n = 5) 
CrimeArea_Prop_Violent_low


#Top 6 and Bottom 6 Locations with high and low financial crime rate
CrimeArea_Prop_Financial <- CrimeArea_Prop %>%
  select(`Financial Crime`) %>%
  arrange(desc(`Financial Crime`))
CrimeArea_Prop_Financial

CrimeArea_Prop_Financial_high <- CrimeArea_Prop_Financial %>%
  select(`Financial Crime`) %>%
  arrange(desc(`Financial Crime`)) %>%
  head(n = 5)
CrimeArea_Prop_Financial_high

CrimeArea_Prop_Financial_low <- CrimeArea_Prop_Financial %>%
  select(`Financial Crime`) %>%
  arrange(desc(`Financial Crime`)) %>%
  tail(n = 5)
CrimeArea_Prop_Financial_low



#Police locations on LA map and crm.type proportions for top 2's ----
pol_lon <- c(-118.277978135, -118.24951873058, -118.531371, -118.410486587,
             -118.289208833, -118.21287140481, -118.3306698, -118.468117012,
             -118.381256, -118.256085266, -118.1878501, -118.29115, 
             -118.41986, -118.266973, -118.27544, -118.3049806,
             -118.599583812, -118.451355, -118.26685, -118.54761, 
             -118.243683)
pol_lat <- c(33.970057517, 34.009038908691, 34.256869, 34.2531425605, 
             33.7575360638, 34.044723832702, 34.095818, 34.2731517133,
             34.187042, 34.0125249685, 34.1180642, 34.05023, 
             33.99164, 34.0567094, 33.93858, 34.0106033,
             34.2211926286, 34.189857, 34.05686, 34.19336,
             34.052235)

pol_loc <- as.data.frame(matrix(c(pol_lon, pol_lat), ncol = 2, byrow = F))


ggmap(la_map) +
  geom_point(data = newData_map, 
             mapping = aes(x = LON, 
                           y = LAT, 
                           color = crm.type.cat),
             size = 0.5) +
  geom_point(data = pol_loc, mapping = aes(V1, V2), size = 2, color = "black")+
  geom_label(data = pol_loc, mapping = aes(V1, V2), 
             label = c("77th Street", "Central", "Devonshire",
                       "Foothill", "Harbor", "Hollenbeck",
                       "Hollywood", "Mission", "N Hollywood",
                       "Newton", "Northeast", "Olympic", 
                       "Pacific", "Rampart", "Southeast",
                       "Southwest", "Topanga", "Van Nuys", 
                       "West LA", "West Valley", "Wilshire"),
             nudge_y = 0.002, size = 2) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)


#Most Violent Crimes          
sort(table(se$crm.type), decreasing = T)
3989/sum(table(se$crm.type))
table(se$crm.type, se$year)["Simple Assault", ]

sort(table(st77$crm.type), decreasing = T)
4766 / sum(table(st77$crm.type))
table(st77$crm.type, st77$year)["Simple Assault", ]


#Most Financial Crimes
sort(table(wla$crm.type), decreasing = T)
6216/sum(table(wla$crm.type))
table(wla$crm.type, wla$year)["Theft/Auto Repair", ]

sort(table(pac$crm.type), decreasing = T)
8928 / sum(table(pac$crm.type))
table(pac$crm.type, pac$year)["Theft/Auto Repair", ]


###############################################################################
# CLUSTER ANALYSIS ------------------------------------------------------------
###############################################################################


#Select Columns for Cluster ----
Crime_Cluster = cl_data[,c(8,9,10)]
as.numeric(Crime_Cluster)

#Change the character to numeric type for cluster analysis ----
Crime_Cluster$Vict.Descent <- as.numeric(factor(Crime_Cluster$Vict.Descent))
Crime_Cluster$Vict.Sex <- as.numeric(factor(Crime_Cluster$Vict.Sex))

#Check the data type after changing
str(Crime_Cluster)

#Scale the variable to standardize it ----
Crime_Cluster$Vict.Age = scale(Crime_Cluster$Vict.Age)

#Impute missing data ----
set.seed(1706)
Crime_Cluster = complete(mice(Crime_Cluster, use.matcher=T))


#Determine the number of the clusters using elbow method to finding the optimal number of clusters ----

set.seed(123)
k.max <- 15
cr_data <- Crime_Cluster
wss <- sapply(1:k.max, 
              function(k){kmeans(cr_data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


#Set the number of the cluster to 2 ----
set.seed(1706)

#Check the row in each cluster ----
km2 = kmeans(x=Crime_Cluster, centers=2, iter.max=100)
k_segments2 = km2$cluster
table(k_segments2)

#Express the cluster on a scatterplot, we flatten the data onto 2 dimensions by conducting a factor analysis with varimax rotation. ----
temp = data.frame(cluster = factor(k_segments2),
                  factor1= fa(Crime_Cluster, nfactors=2, rotate='varimax')$scores[,1],
                  factor2= fa(Crime_Cluster, nfactors=2, rotate='varimax')$scores[,2])

ggplot(temp, aes(x=factor1,y=factor2,col=cluster))+
  geom_point()

#Combine the cluster column back with the original Crime data and named it Crime2 ----
Crime2 = cbind(cl_data,k_segments2)

#Create the table and explore the mean age of each cluster ----
Crime2 %>%
  select(Vict.Age, k_segments2)%>%
  group_by(k_segments2)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  data.frame()

#Filter out the gender besides male and female and use table function see the mode of the gender in each cluster. ----
Crime2 = filter(Crime2,Vict.Sex != "H")
Crime2 =filter(Crime2,Vict.Sex != "X")
table(Crime2$k_segments2,Crime2$Vict.Sex)

#Use table function see the mode of the descent in each cluster. ----
table(Crime2$k_segments2,Crime2$Vict.Descent)

#Do the same analysis again using the 2019-2022 dataset: "data" ----
#Select columns for cluster
Crime_Cluster_2=cl_data[,c(8,9,10)]

###Change the character to numeric type for cluster analysis
Crime_Cluster_2$Vict.Descent <- as.numeric(factor(Crime_Cluster_2$Vict.Descent))
Crime_Cluster_2$Vict.Sex <- as.numeric(factor(Crime_Cluster_2$Vict.Sex))

###Scale the variable to standardize it
ts_data$Vict.Age=scale(ts_data$Vict.Age)

###Determine the number of the clusters using elbow method to finding the optimal number of clusters
set.seed(123)
k.max <- 15
cr_data <- Crime_Cluster_2
wss <- sapply(1:k.max, 
              function(k){kmeans(cr_data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

###Set the number of the cluster to 2
set.seed(1706)
km2_new = kmeans(x=Crime_Cluster_2, centers=2, iter.max=100)

###Check the row in each cluster 
k_segments2_new = km2_new$cluster
table(k_segments2_new)

###Express the cluster on a scatterplot, we flatten the data onto 2 dimensions by conducting a factor analysis with varimax rotation.
temp = data.frame(cluster = factor(k_segments2_new),
                  factor1= fa(Crime_Cluster_2, nfactors=2, rotate='varimax')$scores[,1],
                  factor2= fa(Crime_Cluster_2, nfactors=2, rotate='varimax')$scores[,2])
ggplot(temp, aes(x=factor1,y=factor2,col=cluster))+
  geom_point()

###Combine the cluster column back with the original Crime data and named it Crime2
Crime2_new = cbind(cl_data,k_segments2_new)

###Create the table and explore the mean age of each cluster 
Crime2_new %>%
  select(Vict.Age, k_segments2_new)%>%
  group_by(k_segments2_new)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  data.frame()

###Filter out the gender besides male and female and use table function see the mode of the gender in each cluster. 
Crime2_new = filter(Crime2_new,Vict.Sex != "H")
Crime2_new =filter(Crime2_new,Vict.Sex != "X")
Crime2_new =filter(Crime2_new,Vict.Sex != "")
Crime2_new =filter(Crime2_new,Vict.Sex != "N")
table(Crime2_new$k_segments2,Crime2_new$Vict.Sex)

###Use table function see the mode of the descent in each cluster.
table(Crime2_new$k_segments2,Crime2_new$Vict.Descent)

