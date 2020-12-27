options(warn=-1)
library(dplyr, warn.conflicts = F) # For data manipulation
library(stringr) # For working with strings 
library(ggplot2) # For data visualizations 
library(ggmap) # For data visualizations 
library(cowplot) # Add-on to ggplot 
library(gridExtra) # For working with grid arrange 
library(ggpubr) # For customizing plots 
library(readr) # For loading dataset 
library(data.table) # To work faster with large dataset 
library(corrplot) # Used for correlation matrix 
library(tidyverse) # Standard 
library(leaflet) # Used for plotting maps 
library(ggcorrplot) # Used for correlation matrix 
library(caTools) # Used for splitting data 
library(glmnet) # Used for modelling
library(tibble)
library(texreg) # For summarizing several models 
library(broom) # to use the function tidy. 


# Set seed for modelling
set.seed(12345)

# Creating a list of possible na values to make sure of consistency  
na_strings <- c("NA", "N A", "N / A", "N/A", "N/ A", "Not Available", "NOt available")

# Loading dataset 
listings <- read.csv("/Users/niklasmadsen/Desktop/5. Semester/Introduction to Data Science for Business and Social Applications/listings.csv"
                      , na=na_strings)

#listings <- read.csv('/Users/niklasmadsen/Desktop/5. Semester/Introduction to Data Science for Business and Social Applications/listings.csv')

# Exploring dataset 

# Shape of datset 
dim(listings)
#Showing first 6 rows 
head(listings)
# Showing column names 
names(listings)
# Showing str types 
str(listings)

# Data Wrangling and Cleaning --------------------------------------------------------------------

#After looking at amenities from the str(). We are dealing with a list inside the values. 
#Thus, we split amenities from one row to multiple columns as some of these are deemed key predictors

parsed_amenities <-
  listings %>% 
  .$amenities %>% 
  sub("^\\{(.*)\\}$", "\\1\n", x = .) %>% 
  lapply(function(x) names(read_csv(x)))
amenities <-
  unique(unlist(parsed_amenities)) %>% 
  .[!grepl("translation missing", .)] %>% 
  setNames(., .) %>% 
  lapply(function(x) vapply(parsed_amenities, "%in%", logical(1), x = x)) %>% 
  as_data_frame()

# Creating new dataframe 
df1 <- cbind(listings,amenities) 

cols <- sapply(df1,is.logical)

df1[,cols] <- lapply(df1[,cols], as.numeric)

# Now we have our full data frame and can start reducing

# Dropping irrelevant variables based on initial manual selection

drop <- c("listing_url","medium_url","scrape_id","scrape_id", "summary", "space", "description", "experiences_offered", 
         "notes", "transit", "access", "interaction", "house_rules", "thumbnail_url", "picture_url", "xl_picture_url", 
         "host_url", "host_about", "host_location", "host_thumbnail_url", "host_picture_url", "neighbourhood_cleansed",
         "neighbourhood_group_cleansed", "market", "city", "country_code", "state","zipcode","is_location_exact", "calendar_last_scraped", "license", 
            "jurisdiction_names", "is_business_travel_ready", "require_guest_phone_verification", "last_scraped",
         "neighborhood_overview", "amenities", "requires_license", "host_listings_count", "street", "host_verifications",
         "smart_location", "country","name","host_id", "host_name", "host_response_time",
         "host_neighbourhood", "bed_type","minimum_minimum_nights", 
         "maximum_minimum_nights","minimum_maximum_nights","maximum_maximum_nights", "minimum_nights_avg_ntm","calculated_host_listings_count",
         "calculated_host_listings_count_entire_homes","calculated_host_listings_count_private_rooms",
         "calculated_host_listings_count_shared_rooms","number_of_reviews_ltm", "maximum_nights_avg_ntm", "calendar_updated")
df = df1[,!(names(df1) %in% drop)]

# Based on market insights we drop the following amenities
names(df)

amenities_to_drop = c('Cable TV', 'Paid', 'parking off premises', 
                      'Smoke detector', 'Hangers', 'Hair dryer', 'Iron', 'Laptop friendly workspace',
                      'Room-darkening shades', 'Hot water', 'Microwave', 'Coffee maker', 'Refrigerator',
                      'Dishes and silverware', 'Cooking basics', 'Stove', 'Garden or backyard', 
                      'Luggage dropoff allowed', 'Paid parking on premises', 'Indoor fireplace', 
                      'Dryer', 'Fire extinguisher', 'Shampoo', 'Bathtub', 'High chair', 'Crib', 'Patio or balcony', 
                      'Buzzer/wireless intercom', 'Lockbox', 
                      'Baby bath', 'Changing table', 'Children’s books and toys', 'Pack ’n Play/travel crib', 
                      'Children’s dinnerware', 'Window guards', 'Table corner guards','Other',
                      'Hot tub', 'Carbon monoxide detector' , 'First aid kit',
                      'Pets live on this property', 'Cat(s)','Baby monitor', 'Elevator', 'Wheelchair accessible',
                      'Safety card', 'Outlet covers',
                      'Private entrance', 'Air conditioning', 'Free street parking', 'Lock on bedroom door' , 
                      'Babysitter recommendations', 'Extra pillows and blankets', 'Ethernet connection' ,'BBQ grill' 
                       , 'Waterfront' , 'Lake access', 'Shower gel', 'Baking sheet' ,
                      'Barbecue utensils', 'Trash can' , '24-hour check-in', 'Keypad'
                       ,'Building staff', 'Body soap' , 'Bath towel', 'Toilet paper', 'Hot water kettle' , 
                      'Heated floors','Espresso machine','Formal dining area', 'Day bed' ,'Convection oven' ,
                      'Netflix', 'HBO GO', 'High-resolution computer monitor' ,'Outdoor seating' ,'Full kitchen' ,
                      'Bedroom comforts', 'Dog(s)', 'Breakfast' ,'Other pet(s)', 
                      'Game console' ,'EV charger', 'Single level home' ,'Flat path to guest entrance', 
                      'Well-lit path to entrance', 'Stair gates' ,'Washer/Dryer', 'Ground floor access',
                      'No stairs or steps to enter', 'No stairs or steps to enter_1' ,'Pocket wifi', 'Firm mattress',
                      'Wide hallways' ,'Smart lock' ,'Bread maker', 'Pool' ,'Beach essentials', 'Fireplace guards' ,
                      'Beachfront', 'Ski-in/Ski-out', 'Wide entrance for guests', 'Doorman', 'Handheld shower head',
                      'Electric profiling bed', 'Fixed grab bars for shower', 'Fixed grab bars for toilet', 'Smart TV' 
                       ,'Soaking tub', 'DVD player', 'Rain shower', 'Sound system', 'Sauna', 
                      'Murphy bed', 'Wine cooler' , 'Heated towel rack' , 'TV_1', 'Wide entrance', 
                      'Accessible-height bed', 'Wide clearance to shower, toilet',
                      'No stairs or steps to enter_2', 'Wide doorway to guest bathroom', 
                      'No stairs or steps to enter_3' , 'Wide entryway',  'Walk-in shower' , 'Breakfast table', 
                      'Jetted tub', 'Disabled parking spot', 'Extra space around bed', 'Accessible-height toilet', 
                      'Wide clearance to shower', 'Outdoor kitchen', 'Tennis court', 'Sun loungers',
                      'Outdoor parking', 'Air purifier', 'Kitchenette', 'Step-free shower', 'Gas oven', 'Steam oven', 
                      'Pillow-top mattress', 'Fire pit', 'Printer', 'Standing valet', 'Ceiling fan', 
                      'Memory foam mattress', 'Amazon Echo', 'Projector and screen', 'En suite bathroom',
                      'Central air conditioning', 'Mini fridge', 'Beach view', 'Double oven') #, "X1","Children????Ts dinnerware",
                    #"Children????Ts books and toys","Pack ????Tn Play/travel crib","Paid parking off premises")
# Hvad laver de her i bunden? Skal væk 
df = df[,!(names(df) %in% amenities_to_drop)]


# Exploring variables 
summary(df)
names(df)

dim(df)

# We assess that the predictors all appear relevant. Now we need to explore the columns more in depth  

table(df$property_type)
# We realize that the property_type are a character, and since we are going to use droplevels, we convert it to a factor
df$property_type <- as.factor(df$property_type)

# First, we remove levels of property_type with less than or equal to 20 observations.
str(df$property_type)
table(df$property_type)
df = df[!as.numeric(df$property_type) %in%
                          which(table(df$property_type) <= 20), ]
df$property_type = droplevels(df$property_type)
#Let's explore the remaining property types
table(df$property_type)
#We now change the type back to a character as we need to transform the strings
df$property_type <- as.character(df$property_type)
str(df$property_type)
dim(df)
# A quick look suggest that there are many property types and some overlaps. We thus group the values into a more reasonable property type 

df$property_type[df$property_type == "Serviced apartment"] <- "Apartment"
df$property_type[df$property_type == "Loft"] <- "Apartment"
df$property_type[df$property_type == "Condominium"] <- "Apartment"
df$property_type[df$property_type == "Hostel"] <- "Other"
df$property_type[df$property_type == "Houseboat"] <- "Other"
df$property_type[df$property_type == "Villa"] <- "House"
df$property_type[df$property_type == "Townhouse"] <- "House"

#Now, let's look at the new table of property_type. Since we changed the value to character, all is good.  
table(df$property_type)


# Removing listings with 0 review. Removing a little less than 5K rows.  
df <- subset(df, number_of_reviews > 0)
dim(df)

# We now replace na values with 0 for relevant predictors
df$security_deposit[is.na(df$security_deposit)] <- 0
df$cleaning_fee[is.na(df$cleaning_fee)] <- 0

# As we have many character strings with t or f, we replace these with a numerical dummy for simplicity 

df$host_is_superhost <- as.numeric(ifelse(df$host_is_superhost == 't', 1, 0))
df$instant_bookable <- as.numeric(ifelse(df$instant_bookable == 't', 1, 0))
df$require_guest_profile_picture <- as.numeric(ifelse(df$require_guest_profile_picture == 't', 1, 0))
df$host_has_profile_pic <- as.numeric(ifelse(df$host_has_profile_pic == 't',1,0))
df$host_identity_verified <- as.numeric(ifelse(df$host_identity_verified == 't',1,0))
df$has_availability <- as.numeric(ifelse(df$has_availability == 't',1,0))

# We observe that price is listed as a character due to a dollar sign and thus has to make this value numerical 
str(df$price)

df$price = as.numeric(gsub("[\\$,]", "", df$price))

# Let's see if it has changed to numeric.
str(df$price)
# Check for NaN values in our target variable. 
df %>% count(is.na(price))

# Doing the same for weekly, cleaning fee, security deposit and monthly price - found by looking at str()
df$weekly_price = as.numeric(gsub("[\\$,]", "", df$weekly_price))
df$monthly_price = as.numeric(gsub("[\\$,]", "", df$monthly_price))
df$cleaning_fee = as.numeric(gsub("[\\$,]", "", df$cleaning_fee))
df$security_deposit = as.numeric(gsub("[\\$,]", "", df$security_deposit))
df$extra_people = as.numeric(gsub("[\\$,]", "", df$extra_people))

# Next, we are interested in the neighbourhoods as we consider them to be significant in predicting price.
table(df$neighbourhood)

# Some statistics on the neighborhoods. 
#Group by neighborhood to compare the median / mean price for each neighborhood. 
median_neighbourhood_price_before = df %>%
  group_by(neighbourhood) %>%
  summarize(average_price = mean(price), std_price = sd(price),
            min_price = min(price), max_price = max(price), first_quantile=quantile(price, 0.25),
            median_price = median(price), 
            third_quantiale=quantile(price, 0.75))

median_neighbourhood_price_before

# Convert factor to character as we did with property_type 
df$neighbourhood <- as.character(df$neighbourhood)
#  Reducing this column as we observe 23 different neighborhoods with many overlaps 
# In addition we reshape some of the existing listings due to formatting issues 
df$neighbourhood[df$neighbourhood == "Amager Vest"] <- "Amager"
df$neighbourhood[df$neighbourhood == "Amager Øst"] <- "Amager"
df$neighbourhood[df$neighbourhood == "Islands Brygge"] <- "Amager"
df$neighbourhood[df$neighbourhood == "Amagerbro"] <- "Amager"
df$neighbourhood[df$neighbourhood == "Teglholmen"] <- "CPH SV"
df$neighbourhood[df$neighbourhood == "Sluseholmen"] <- "CPH SV"
df$neighbourhood[df$neighbourhood == "Kastrup"] <- "Amager"
df$neighbourhood[df$neighbourhood == "Christianshavn"] <- "Indre By"
df$neighbourhood[df$neighbourhood == "Nyboder"] <- "Indre By"
df$neighbourhood[df$neighbourhood == "Holmen"] <- "Indre By"
df$neighbourhood[df$neighbourhood == "Christianshavn"] <- "Indre By"
df$neighbourhood[df$neighbourhood == "Kødbyen"] <- "Vesterbro"
df$neighbourhood[df$neighbourhood == "Amager Vest"] <- "Amager"
df$neighbourhood[df$neighbourhood == "København NV"] <- "CPH NV"


#Changing Valby, Vanløse and Brønshøj to "outer cph"
df$neighbourhood[df$neighbourhood == "Valby"] <- "Outer CPH"
df$neighbourhood[df$neighbourhood == "Brønshøj"] <- "Outer CPH"
df$neighbourhood[df$neighbourhood == "Vanløse"] <- "Outer CPH"

# Let's explore the change 
table(df$neighbourhood)

# Group by neighborhood to compare the median / mean price for each neighbourhood.
neigbourhood_prices = df %>%
  group_by(neighbourhood) %>%
  summarize(average_price = mean(price), std_price = sd(price),
            min_price = min(price), max_price = max(price), first_quantile=quantile(price, 0.25),
            median_price = median(price), 
            third_quantiale=quantile(price, 0.75))

#Changing to two decimals 
decimals_neighborhood <- sapply(neigbourhood_prices, is.numeric)
neigbourhood_prices[decimals_neighborhood] <- lapply(neigbourhood_prices[decimals_neighborhood], round, 2)
#Showing the df 
neigbourhood_prices


# Looking at summary a final time before going into visualizations / modeling. 
summary(df)

# From the summary we can see that there is a lot of NA values in the dataset. Thus, we create a for loop to identify those.
# We exclude the variables where the majority of values are NA's. Four variables are excluded.
countNA = 0
for (i in 1:ncol(df)){
    
    countNA[i] = sum(is.na(df[, i]))
    
}

df = df[, -which(countNA > 20000)]




# Visualizations --------------------------------------------------------------------


# Figure 1) Histogram of price distribution 

hist_plot=ggplot(data = df, 
       aes(df$price)) + 
  geom_histogram(bins = 80,
                 col = "#000000", 
                 fill = "#FF5A5F", 
                 alpha = .9) + 
  ggtitle("Figure A) Distribution of price")+
  labs(x = "Price", y = "Number of listings") + 
  theme_minimal() 


#Transformed distribution of Price
hist_log=ggplot(df, aes(price)) +
  geom_histogram(bins = 80, aes(y = stat(density)), col = "#000000", 
                 fill = "#FF5A5F", alpha=.9 , fun=dnorm)+ 
  theme_minimal()+ labs(x = "Log10 Price", y = "Density")+
  geom_density(alpha = 0.5) +ggtitle("Figure B) Distribution of log price") +
  scale_x_log10()

# Plotting both histograms in one and using ggplotGrob to align 
grid.arrange(rbind(ggplotGrob(hist_plot), ggplotGrob(hist_log), 
                   size = "first"))

## Figure 2 - Side by side boxplot of log price for each neighborhood 

# Side-by-side boxplots of price, for each neighborhood.
ggplot(df, 
       aes(x =(df$neighbourhood), 
           y = (df$price), 
           fill = df$neighbourhood)) + 
  geom_boxplot(alpha = .5, outlier.alpha = .9) +
  scale_y_continuous(name = "Price") + 
  scale_x_discrete(name = "Neighbourhoods") + 
  theme_minimal() +
  theme(legend.position = "bottom") + 
  theme(legend.text = element_text(size = 8)) + 
  labs(fill = "")

# Appendix - Side-by-side boxplots of logarithm of price, for each neighborhood.
ggplot(df, 
       aes(x =(df$neighbourhood), 
           y = log(df$price), 
           fill = df$neighbourhood)) + 
  geom_boxplot(alpha = .5, outlier.alpha = .4) +
  scale_y_continuous(name = "Logarithm of Price") + 
  scale_x_discrete(name = "Neighbourhoods") + 
  theme_minimal() +
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom") + 
  theme(legend.text = element_text(size = 6)) + 
  labs(fill = "")



## Appendix - side by side boxplot of log price for each property type

# Side-by-side boxplots of logarithm of price, for each property
ggplot(df, 
       aes(x =(df$property_type), 
           y = log(df$price), 
           fill = df$property_type)) + 
  geom_boxplot(alpha = .6, outlier.alpha = .4) +
  scale_y_continuous(name = "Logarithm of Price") + 
  scale_x_discrete(name = "Neighbourhoods") + 
  theme_minimal() +
  labs(fill = "")

# Removing price outliers and creating new df 

filtered_df <- df%>% filter((price>200)& price<5000)

dim(filtered_df)

### Figure 4 ) Combined plot
# Plot A - Average price compared to number of listings by neighborhood

data_by_region <- filtered_df%>%
  group_by(neighbourhood)%>%
  summarize(count = n(),
            avg_price = mean(price),
            med_price = median(price))


mean_price_neighborhood=ggplot(data_by_region, aes(x=reorder(neighbourhood, -count), count, fill = neighbourhood))+
  geom_col(col = "black")+
  #geom_line(aes(neighbourhood, avg_price*10), size=1, group=1, orientation="x")+
  geom_label(aes(neighbourhood, avg_price, label = paste("DKK", round(avg_price), sep = "")), fill = "black", col = "white")+
  labs(y = "Total apartments / Avg. daily price",x='',
       title = "Figure A) Avg.daily price compared to # listings by neighbourhood")+
  theme_minimal()+
  theme(legend.position = "none")



by.neighbor = filtered_df %>%
  group_by(neighbourhood) %>%
  summarize(med.price = median(price))


# Plot B - Median price by neighborhood 
median_price= by.neighbor %>%
  ggplot(aes(x=reorder(neighbourhood, -med.price), y=med.price)) + 
  geom_bar(fill="#FF5A5F", stat='identity') +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  geom_label(aes(neighbourhood, med.price, label = paste("DKK", round(med.price), sep = "")), fill = "black", col = "white")+
  labs(x='', y='Median price', title='Figure B) Median daily price by neighbourhood')+
  theme_minimal()


# Plotting both together
grid.arrange(mean_price_neighborhood, median_price)


## Appendix- Map plots - most expensive and least expensive
height <- max(df$latitude) - min(df$latitude)
width <- max(df$longitude) - min(df$longitude)
copenhagen <- c(bottom  = min(df$latitude)  - 0.1 * height, 
                top     = max(df$latitude)  + 0.1 * height,
                left    = min(df$longitude) - 0.1 * width,
                right   = max(df$longitude) + 0.1 * width)
#Getting map 
map <- get_stamenmap(copenhagen, zoom = 10, maptype = "terrain")

# Plot A - highest prices 
highest_prices <- df %>%
  filter(price > 5000)
map1 <- ggmap(map) +
  geom_point(data=highest_prices, aes(longitude, latitude, size = price, color = factor(neighbourhood)), alpha=0.7)+
  scale_colour_brewer(type="seq", palette=1, name = "Neighbourhood")+
  scale_size("Price")+
  labs(x = "", y = "", title = "Locations > 5000 DKK per night")+
  theme_classic() + theme(plot.caption = element_text(color = "gray45", size = 8))

#plot map1 
map1

# Plot B - lowest prices 
lowest_prices <- df %>%
  filter(price < 150)
map2 <- ggmap(map) +
  geom_point(data=lowest_prices, aes(longitude, latitude, size = price, color = factor(neighbourhood)), alpha=0.7)+
  scale_colour_brewer(type="seq", palette=1, name = "Neighbourhood")+
  scale_size("Price")+
  labs(x = "", y = "", title = "Locations < 150 DKK per night") +
  theme_classic()+
  theme(plot.caption = element_text(color = "gray45", size = 8))
map2

#grid.arrange(map1, map2, ncol=1)

## Figure 5 - Correlation matrix 

# Creating new dataframe and selecting relecant variables to explore
raw_data_corr <- filtered_df%>%
  select(accommodates, bathrooms, beds, bedrooms, price, host_is_superhost, Wifi, cleaning_fee, 
         number_of_reviews, review_scores_rating
  )

#Cleaning for inf, na and nan
corr_plot <- raw_data_corr[, sapply(raw_data_corr, is.numeric)]
corr_plot <- corr_plot[complete.cases(corr_plot), ]
correlation_matrix <- cor(corr_plot, method = "spearman")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
p.mat <- cor_pmat(correlation_matrix)
corrplot(correlation_matrix, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text color
         # Get significance
         p.mat = p.mat, sig.level = 0.8, insig = "blank",
         diag=FALSE 
)


### Appendix -  several plots on accomodates, bedrooms and bathrooms compared to price. 


#Scatterplots of logarithm of price with respect to accommodates, bathrooms, bedrooms, and beds, respectively.
p1 = ggplot(filtered_df, 
            aes(x = filtered_df$accommodates, 
                y = (filtered_df$price))) + 
  geom_point(alpha = 0.4) + 
  geom_smooth(method = "lm", fill = "red3", size = 0.50)+
  #annotate("text", x = 5.5, y = 7, label = "italic(r) == 0.324", parse = T, size = 5)+
  labs(y = 'Price', x = 'Accommodates') +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1, 17, 2)) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

p1
p2 = ggplot(filtered_df, 
            aes(x = filtered_df$bathrooms, 
                y = (filtered_df$price))) + 
  geom_point(alpha = 0.4) + 
  geom_smooth(method = "lm", fill = "#FF5A5F", size = 0.50)+
  theme_minimal() +
  labs(x = 'Bathrooms', y = 'Price') +
  scale_x_continuous(breaks = seq(1, 17, 2))+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))
p2

p3 = ggplot(filtered_df, 
            aes(x = filtered_df$bedrooms, 
                y = (filtered_df$price))) + 
  geom_point(alpha = 0.4) + 
  geom_smooth(method = "lm", fill = "#FF5A5F", size = 0.50)+
  theme_minimal() +
  labs(x = 'Bedrooms', y = 'Price') +
  scale_x_continuous(breaks = seq(1, 17, 2))+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

p4 = ggplot(filtered_df, 
            aes(x = filtered_df$beds, 
                y = (filtered_df$price))) + 
  geom_point(alpha = 0.4) + 
  geom_smooth(method = "lm", fill = "#FF5A5F", size = 0.50)+
  theme_minimal() +
  labs(x = 'Beds', y = 'Price') +
  scale_x_continuous(breaks = seq(1, 17, 2))+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

grid.arrange(p1,p2,p3,p4)


# Appendix - descriptive statistics on review scores

rating_scores = filtered_df %>%
  select(review_scores_rating) %>%
  summarize(average_score = mean(review_scores_rating, na.rm=TRUE), std = sd(review_scores_rating, na.rm=TRUE),
            min_score = min(review_scores_rating, na.rm=TRUE), max_score = max(review_scores_rating, na.rm=TRUE), first_quantile=quantile(review_scores_rating, na.rm=TRUE, 0.25),
            median_score = median(review_scores_rating, na.rm=TRUE), 
            third_quantile=quantile(review_scores_rating,na.rm=TRUE, 0.75))


#Changing the decimals to 2
decimals <- sapply(rating_scores, is.numeric)
rating_scores[decimals] <- lapply(rating_scores[decimals], round, 2)

#Looking at the result 
rating_scores


scores= filtered_df %>%
  filter(!is.na(review_scores_rating)) %>%
  group_by(review_scores_rating) %>%
  summarize(med.price = median(price),
            num = n()) %>%
  #num simply count the number of review. The max number of review are 634 and the max num is 7555
  #meaning that there are 7555 with the same score rating.
  ggplot(aes(x=review_scores_rating/10, y=med.price, size=num)) +
  geom_point(color='#FF5A5F', alpha=0.5) +
  labs(x = 'Score', y='Median Price', size='# Reviews',
       title='Figure A) Median Price by Review Rating Score') +
  geom_smooth(method='lm', show.legend = FALSE) +
  theme_minimal()

### Comparing to cleanliness 

clean=filtered_df %>%
  filter(!is.na(review_scores_cleanliness)) %>%
  group_by(review_scores_cleanliness) %>%
  summarize(med.price = median(price),
            num = n()) %>%
  ggplot(aes(x=review_scores_cleanliness, y=med.price, size=num)) +
  geom_point(color='#FF5A5F', alpha=0.5) +
  labs(x = 'Score', y='Median Price', size='# Reviews',
       title='Figure B) Median Price by Review Cleanliness Score') +
  geom_smooth(method='lm', show.legend = FALSE) +
  theme_minimal()

grid.arrange(scores, clean)


## Figure 6 - top 50 prices

# get top 50 listings by price
top_df <- df %>% top_n(n = 50, wt = price)

summary(top_df$price)

# get background map
top_height <- max(top_df$latitude) - min(top_df$latitude)
top_width <- max(top_df$longitude) - min(top_df$longitude)
top_borders <- c(bottom  = min(top_df$latitude)  - 0.1 * top_height,
                 top     = max(top_df$latitude)  + 0.1 * top_height,
                 left    = min(top_df$longitude) - 0.1 * top_width,
                 right   = max(top_df$longitude) + 0.1 * top_width)
# Getting map from ggmap package 
top_map <- get_stamenmap(top_borders, zoom = 12, maptype = "toner-lite")

# Plot map 
ggmap(top_map) +
  geom_point(data = top_df, mapping = aes(x = longitude, y = latitude, size=price), color='#FF5A5F', alpha=0.5)  +
  labs(x="",y="", title='Top 50 most expensive listings') +
  theme_minimal()


### Appendix - map of listings compared to neighbourhood group 
#choosing random colours 
pal <- colorFactor(palette = c("red", "darkorange", "green", 
                               "gold", "black", "blue", "yellow", "purple" ), 
                   domain = df$neighbourhood)

# Save map to html. Plot is in the paper 
map_plot <- leaflet(data = df) %>%
addTiles() %>%
addCircleMarkers(~longitude, ~latitude, color = ~pal(neighbourhood),
                 weight = 1, radius=1,
                 label = paste("Name:", df$neighbourhood)) %>% 
addLegend("bottomright", pal = pal, values = ~neighbourhood,
          title = "Area", opacity = 1)


htmlwidgets::saveWidget(map_plot, "map_plot.html")

# display_html('<iframe src="map_plot.html" width=100% height=450></iframe>')


# Predictive Modeling --------------------------------------------------------------------


# First we split the data 

#splitting dataset in train / test with a ratio of 80/20 
train.index <- sample.split(filtered_df$price, SplitRatio = .8, group = NULL)
train <- filtered_df[train.index, ]    
test <- filtered_df[!train.index, ]
#Let's have a quick look into shape of the new df's. 
dim(train)
dim(test)

# Dropping all na rows for better modelling
new_training = na.omit(train)
# Examine new training data 
names(new_training)

#Now we need to further split the data into x_train, x_test, y_train and y_test 
# With the tests being our target variable price 
#x_train we select some columns that we believe are good predictors
# We need to convert into a matrix as glmnet does not work with dataframes. 

# Creating x_train as matrix 
x_train = new_training %>% select(bedrooms, bathrooms, accommodates, 
                                  neighbourhood, Wifi, TV, host_identity_verified,
                                  review_scores_cleanliness, review_scores_rating,
                                  property_type, beds, availability_365, Kitchen, 
                                  Essentials, `Smoking allowed`, `Self check-in`, 
                                  Terrace, host_acceptance_rate, host_is_superhost, 
                                  number_of_reviews, instant_bookable, Oven, 
                                  `Family/kid friendly`, host_has_profile_pic, 
                                  cleaning_fee, Internet, `Host greets you`, 
                                  `Free parking on premises`, `Suitable for events`, `Cleaning before checkout`,
                                  `Pets allowed`, `Bed linens`, cancellation_policy, review_scores_checkin, 
                                  review_scores_accuracy, availability_30, availability_60, availability_90,
                                  minimum_nights, maximum_nights, cleaning_fee, security_deposit, Balcony, 
                                  `Bathroom essentials`, `Private living room`, Gym, `Cleaning before checkout`,
                                  `Long term stays allowed`, Dishwasher, Heating, `Paid parking off premises`,
                                  reviews_per_month, review_scores_checkin, review_scores_communication, review_scores_value, 
                                  review_scores_location, host_total_listings_count, host_has_profile_pic,
                                  room_type, Washer, extra_people, guests_included,
                                  has_availability, require_guest_profile_picture
) %>% data.matrix()
#Check the shape of x_train 
dim(x_train)

# Getting x_test, our target variable
y_train=new_training$price

# getting y_train from the test dataset 
new_testing = na.omit(test)
x_test = new_testing %>% select(bedrooms, bathrooms, accommodates, 
                                neighbourhood, Wifi, TV, host_identity_verified,
                                review_scores_cleanliness, review_scores_rating,
                                property_type, beds, availability_365, Kitchen, 
                                Essentials, `Smoking allowed`, `Self check-in`, 
                                Terrace, host_acceptance_rate, host_is_superhost, 
                                number_of_reviews, instant_bookable, Oven, 
                                `Family/kid friendly`, host_has_profile_pic, 
                                cleaning_fee, Internet, `Host greets you`, 
                                `Free parking on premises`, `Suitable for events`, `Cleaning before checkout`,
                                `Pets allowed`, `Bed linens`, cancellation_policy, 
                                review_scores_accuracy, availability_30, availability_60, availability_90,
                                minimum_nights, maximum_nights, cleaning_fee, security_deposit,
                                Balcony, `Bathroom essentials`, `Private living room`, Gym, `Cleaning before checkout`,
                                `Long term stays allowed`, Dishwasher, Heating, `Paid parking off premises`,
                                reviews_per_month, review_scores_checkin, review_scores_communication, review_scores_value, 
                                review_scores_location, host_total_listings_count, host_has_profile_pic,
                                room_type, Washer, extra_people, guests_included,
                                has_availability, require_guest_profile_picture
) %>% data.matrix()

dim(x_test)

# Getting our target variable that we want to predict on
y_test=new_testing$price 

# Not part of the paper! 
# Creating a simple linear regression to analyse number of reviews / host is superhost
lm_reviews <- lm(price ~  number_of_reviews + host_is_superhost, data = filtered_df)
# Creating new data to test the model 
newdata <- expand.grid(number_of_reviews = seq(min(filtered_df$number_of_reviews),
                                        max(filtered_df$number_of_reviews), by= 300),
                      host_is_superhost = c(0, 1))
newdata$preds <- predict(lm_reviews, newdata = newdata)
head(newdata, 5)

# Visualize the predictions 
ggplot(newdata, aes(x = number_of_reviews, y = preds,
                   colour = factor(host_is_superhost))) +
  geom_line(size = 1.5) +
  theme_minimal() + theme(legend.position = "bottom") +
  geom_smooth(method = "lm", size = 0.50)+
  theme_minimal() +
  labs(y = "Predicted price", x = "# of reviews",
       colour = "Host is superhost?") +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

# Multiple linear regression and baseline model 

# First model - baseline 
mlr_before_train <- lm(price ~ accommodates + availability_365 + bathrooms + 
                         neighbourhood + review_scores_cleanliness,  data = df)

summary(mlr_before_train)

# Visualize results 
par(mfrow=c(2,2)) 
plot(mlr_before_train)


# Second model on filtered_df 
mlr_1 <- lm(price ~ accommodates + availability_365 + bathrooms + 
              neighbourhood + review_scores_cleanliness,  data = filtered_df)

# Looking at summary to find significant variables and adjusted R-squared 
summary(mlr_1)
#Looking at coefficient 
coef(mlr_1)
# Visualize results 
par(mfrow=c(2,2)) 
plot(mlr_1)

#Creating a function to evaluate the train and test data 
# parameters: lm_model is the model we have fitted
# dataframe is the train or test df
# predicted_lm is the predicted values using predict() function
# price is our target variable and will be y_train or y_test 

get_lm_results = function(lm_model, dataframe, predicted_lm, price){
  residuals = dataframe[,price] - predicted_lm
  residuals_2 = residuals**2
  rows = length(predicted_lm)
  r_squared = as.character(round(summary(lm_model)$r.squared, 3))
  adjusted_r_squared = as.character(round(summary(lm_model)$adj.r.squared, 3))
  print(r_squared) # Print R-squared 
  print(adjusted_r_squared) # Print Adjusted R-squared
  # Calculate and print RMSE 
  print(as.character(round(sqrt(sum(residuals_2)/rows), 2)))
}

# Predict the model on train data
mlr_1_predict_train = predict(mlr_1, newdata = new_training)
get_lm_results(mlr_1, new_training, mlr_1_predict_train, price="price")
# Predict the model on test data
mlr_1_predict_test = predict(mlr_1, newdata = new_testing)
get_lm_results(mlr_1, new_testing, mlr_1_predict_test, price = 'price')

# Second model with new selection of variables based on analysis 
mlr_2 <- lm(price ~ bedrooms + host_identity_verified + TV + 
              property_type + review_scores_rating,  data = new_training)

summary(mlr_2)
#Looking at coefficient 
coef(mlr_2)

#Adding the significant variables from model 1 and 2 together 
mlr_3 <- lm(price ~ bedrooms + host_identity_verified + TV + 
              property_type + review_scores_rating + accommodates + 
              availability_365 + bathrooms + 
              neighbourhood + review_scores_cleanliness, data = new_training)

#Looking at summary
summary(mlr_3)
#Looking at coefficient 
coef(mlr_3)
# Predict the model on train data
mlr_3_predict_train = predict(mlr_3, newdata = new_training)
get_lm_results(mlr_3, new_training, mlr_3_predict_train, price="price")
# Predict the model on test data
mlr_3_predict_test = predict(mlr_3, newdata = new_testing)
get_lm_results(mlr_3, new_testing, mlr_3_predict_test, price = 'price')

# let's plot our predictions vs actual price 
plot(y_test,mlr_1_predict_test,xlab="Actual price",ylab="Predicted  price",
     main="Prediction using multiple linear regression")
abline(0,1, col="red")


texreg::screenreg(list(mlr_before_train, mlr_1, mlr_2, mlr_3))


### RIDGE Regression

# Let's fit the ridge model

# Let's run the model using cross validation so that we can find the optimal lambda 
ridge = cv.glmnet(x_train, y_train, alpha = 0, nfolds=5 )
summary(ridge)

#Plot ridge to see regularization 
plot(ridge)

best_lambda=ridge$lambda.min
best_lambda

## Let's have a look at the top 10 significant features 
coef(ridge, s = "lambda.min") %>%
  tidy() %>%
  filter(row != "(Intercept)") %>%
  top_n(15, wt = abs(value)) %>%
  ggplot(aes(value, reorder(row, value))) +
  geom_point() +
  theme_minimal()+
  ggtitle("Top 25 variables") +
  xlab("Coefficient") +
  ylab(NULL)


#Fitting the model before predicting
ridge_model <- glmnet(x_train, y_train, alpha = 0, lambda = best_lambda, nfolds=5)


# Function to compute R-squared and RMSE 
get_results <- function(target, predicted, df) {
  # Parameters: target is either y_train or y_test (the price)
  # predicted is the fitted model on the function predict()
  # df is the dataframe and can either be the train or test df
  SSE <- sum((predicted - target)^2)
  SST <- sum((target - mean(target))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  # Get RMSE and R-Squared  
  data.frame(
    RMSE = RMSE,
    R_squared = R_square
  )
  
}


# Prediction and evaluation on train data
predictions_train <- predict(ridge_model, s = best_lambda, newx = x_train)
get_results(y_train, predictions_train, new_training)

# Prediction and evaluation on test data
predictions_test <- predict(ridge_model, s = best_lambda, newx = x_test)
get_results(y_test, predictions_test, new_testing)


### Lasso regression 

# Fit model on training data 
lasso = cv.glmnet(x_train, y_train, alpha = 1, nfolds=5)
summary(lasso)
plot(lasso)

# Finding the best lambda value for the model 
lambda_best <- lasso$lambda.min 
lambda_best

# Looking at the coefficients 
coef(lasso, s=lambda_best)
lasso.coef <- predict(lasso,type = "coefficients", s = lambda_best)[1:39,]
names(lasso.coef[lasso.coef !=0])

#Let's visualize after fitting 
plot(lasso$glmnet.fit, xvar="lambda", label=TRUE)

# Creating the model with the best lambda and CV = 10 
lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = lambda_best, nfolds=5)

# Getting the RMSE and Adjusted R-squared for train
predictions_lasso_train <- predict(lasso_model, s = best_lambda, newx = x_train)
get_results(y_train, predictions_lasso_train, train)

# Getting the RMSE and Adjusted R-squared for test 
predictions_lasso_test <- predict(lasso_model, s = best_lambda, newx = x_test)
get_results(y_test, predictions_lasso_test, test)

# Calculate test MSE
mean((predictions_lasso_train - y_test)^2)

##Plot both model to compare coefficients  

# plot ridge model
par(mfrow = c(1, 2))
plot(ridge$glmnet.fit, xvar = "lambda", main = "Ridge penalty\n\n")
abline(v = log(ridge$lambda.min), col = "red", lty = "dashed")
abline(v = log(ridge$lambda.1se), col = "blue", lty = "dashed")

# plot lasso model
plot(lasso$glmnet.fit, xvar = "lambda", main = "Lasso penalty\n\n")
abline(v = log(lasso$lambda.min), col = "red", lty = "dashed")
abline(v = log(lasso$lambda.1se), col = "blue", lty = "dashed")



## Plot predicted price for both model 
plot(y_test,predictions_lasso_test,xlab="Actual price",ylab="Predicted  price",
     main="Prediction using Lasso regression")
abline(0,1, col="red")

plot(y_test,predictions_test,xlab="Actual price",ylab="Predicted  price",
     main="Prediction using Ridge regression")
abline(0,1, col="red")


# Multiple linear regression model with same variables used as with ridge and lasso 
mlr_4 <- lm (price ~ bedrooms + bathrooms + accommodates +
               neighbourhood + Wifi + TV + host_identity_verified+
               review_scores_cleanliness+ review_scores_rating+
               property_type+ beds+ availability_365+ Kitchen+ 
               Essentials+ `Smoking allowed`+ `Self check-in`+ 
               Terrace+ host_acceptance_rate+ host_is_superhost+ 
               number_of_reviews+ instant_bookable+ Oven+ 
               `Family/kid friendly`+ host_has_profile_pic+ 
               cleaning_fee+ Internet+ `Host greets you`+ 
               `Free parking on premises`+ `Suitable for events`+ `Cleaning before checkout`+
               `Pets allowed`+ `Bed linens`+ cancellation_policy+ review_scores_checkin+ 
               review_scores_accuracy+ availability_30+ availability_60+ availability_90+
               minimum_nights+ maximum_nights+ cleaning_fee+ security_deposit, data = new_training)
summary(mlr_4)

# Predict the model on train data
mlr_4_predict_train = predict(mlr_4, newdata = new_training)
get_lm_results(mlr_4, new_training, mlr_4_predict_train, price="price")
# Predict the model on test data
mlr_4_predict_test = predict(mlr_4, newdata = new_testing)
get_lm_results(mlr_4, new_testing, mlr_4_predict_test, price="price")




# Not part of the paper - used for testing other models outside of the course. 

# Building an xgboost model

library(mltools)
library(caret)
library(xgboost)

# Run xgb.cv
xgboost_cv <- xgb.cv(data = x_train, 
             label = y_train,
             nrounds = 100,
             nfold = 5,
             objective = "reg:squarederror",
             eta = 0.3,
             max_depth = 6,
             early_stopping_rounds = 5,
             verbose = 0
)

# Get the evaluation log
eval_log <- xgboost_cv$evaluation_log

# The number of trees to use, as determined by xgb.cv
xgboost_cv$best_ntreelimit


# Run XGBOOST model
xgb_model <- xgboost(data = x_train, # training data as matrix
                          label = y_train,  # target variable 
                          nrounds = 15,       # number of trees
                          objective = "reg:squarederror", # objective
                          eta = 0.3,
                          max_depth = 6,
                          verbose = 0  # silent
)

# Make predictions
#pred_xgb = predict(xgb_model, newdata = x_test)
#get_results(y_test, pred_xgb, test)







# test --------------------------------------------------------------------






