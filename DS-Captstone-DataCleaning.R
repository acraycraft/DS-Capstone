setwd("C:/Users/ashto/Documents/Wellesley/2023_Spring/DS Capstone/Data")

## READING THE DATA ##
bos_bnb_full <- read.csv('Boston-airbnb-listings.csv', header=TRUE)
dc_bnb_full <- read.csv('dc-airbnb-listings.csv', header=TRUE)
sf_bnb_full <- read.csv('sf-airbnb-listings.csv', header=TRUE)
head(sf_bnb_full)
#datasets = [bos_bnb_full,dc_bnb_full,sf_bnb_full] #HELP, for loops?
  #--> write a function, to practice
    #function name should be intuitive, short
    #should tackle small pieces of the big problem

#formatting correct columns
bos_bnb_limited <- bos_bnb_full[c("Price",'Review.Scores.Cleanliness','Review.Scores.Communication','Minimum.Nights','Beds','Bathrooms','Guests.Included','Reviews.per.Month','Room.Type','Neighbourhood.Cleansed','Cancellation.Policy')]
bos_bnb_limited <- as.data.frame(unclass(bos_bnb_limited), stringsAsFactors = TRUE)
dc_bnb_limited <- dc_bnb_full[c("Price",'Review.Scores.Cleanliness','Review.Scores.Communication','Minimum.Nights','Beds','Bathrooms','Guests.Included','Reviews.per.Month','Room.Type','Neighbourhood.Cleansed','Cancellation.Policy')]
dc_bnb_limited <- as.data.frame(unclass(dc_bnb_limited), stringsAsFactors = TRUE)
sf_bnb_limited <- sf_bnb_full[c("Price",'Review.Scores.Cleanliness','Review.Scores.Communication','Minimum.Nights','Beds','Bathrooms','Guests.Included','Reviews.per.Month','Room.Type','Neighbourhood.Cleansed','Cancellation.Policy')]
sf_bnb_limited <- as.data.frame(unclass(sf_bnb_limited), stringsAsFactors = TRUE)

#visualizing neighborhood levels
par(mar=c(11,4,2,1)+.1)
plot(bos_bnb_limited$Neighbourhood.Cleansed, col="#FF5A5F", las=2, border="white")
plot(dc_bnb_limited$Neighbourhood.Cleansed, col="#FF5A5F", las=2, border="white")
plot(sf_bnb_limited$Neighbourhood.Cleansed, col="#FF5A5F", las=2, border="white")
  # --> figure out how to clean neighborhoods for dc and sf

#condensing Boston neighborhood levels
levels(bos_bnb_limited$Neighbourhood.Cleansed)[levels(bos_bnb_limited$Neighbourhood.Cleansed)=="Bay Village"] <-"Back Bay"
levels(bos_bnb_limited$Neighbourhood.Cleansed)[levels(bos_bnb_limited$Neighbourhood.Cleansed)=="Charlestown"] <-"East Boston"
levels(bos_bnb_limited$Neighbourhood.Cleansed)[levels(bos_bnb_limited$Neighbourhood.Cleansed)=="Hyde Park"] <-"Dorchester"
levels(bos_bnb_limited$Neighbourhood.Cleansed)[levels(bos_bnb_limited$Neighbourhood.Cleansed)=="Mattapan"] <-"Dorchester"
levels(bos_bnb_limited$Neighbourhood.Cleansed)[levels(bos_bnb_limited$Neighbourhood.Cleansed)=="Roxbury"] <-"Dorchester"
levels(bos_bnb_limited$Neighbourhood.Cleansed)[levels(bos_bnb_limited$Neighbourhood.Cleansed)=="Roslindale"] <-"Jamaica Plain"
levels(bos_bnb_limited$Neighbourhood.Cleansed)[levels(bos_bnb_limited$Neighbourhood.Cleansed)=="West Roxbury"] <-"Jamaica Plain"
levels(bos_bnb_limited$Neighbourhood.Cleansed)[levels(bos_bnb_limited$Neighbourhood.Cleansed)=="West End"] <-"Downtown"
levels(bos_bnb_limited$Neighbourhood.Cleansed)[levels(bos_bnb_limited$Neighbourhood.Cleansed)=="Leather District"] <-"Downtown"
levels(bos_bnb_limited$Neighbourhood.Cleansed)[levels(bos_bnb_limited$Neighbourhood.Cleansed)=="Longwood Medical Area"] <-"Mission Hill"
  # --> double-check
  # above 65 counts

#condensing DC neighborhood levels
levels(dc_bnb_limited$Neighbourhood.Cleansed)[levels(dc_bnb_limited$Neighbourhood.Cleansed)=="Hawthorne, Barnaby Woods, Chevy Chase"] <-"A"
levels(dc_bnb_limited$Neighbourhood.Cleansed)[levels(dc_bnb_limited$Neighbourhood.Cleansed)=="Friendship Heights, American University Park, Tenleytown"] <-"A"
levels(dc_bnb_limited$Neighbourhood.Cleansed)[levels(dc_bnb_limited$Neighbourhood.Cleansed)=="Spring Valley, Palisades, Wesley Heights, Foxhall Crescent, Foxhall Village, Georgetown Reservoir"] <-"A"
levels(dc_bnb_limited$Neighbourhood.Cleansed)[levels(dc_bnb_limited$Neighbourhood.Cleansed)=="North Cleveland Park, Forest Hills, Van Ness"] <-"B"
levels(dc_bnb_limited$Neighbourhood.Cleansed)[levels(dc_bnb_limited$Neighbourhood.Cleansed)=="Colonial Village, Shepherd Park, North Portal Estates"] <-"B"
levels(dc_bnb_limited$Neighbourhood.Cleansed)[levels(dc_bnb_limited$Neighbourhood.Cleansed)=="Near Southeast, Navy Yard"] <-"B"
levels(dc_bnb_limited$Neighbourhood.Cleansed)[levels(dc_bnb_limited$Neighbourhood.Cleansed)=="Takoma, Brightwood, Manor Park"] <-"C"
levels(dc_bnb_limited$Neighbourhood.Cleansed)[levels(dc_bnb_limited$Neighbourhood.Cleansed)=="Lamont Riggs, Queens Chapel, Fort Totten, Pleasant Hill"] <-"C"
levels(dc_bnb_limited$Neighbourhood.Cleansed)[levels(dc_bnb_limited$Neighbourhood.Cleansed)=="Brookland, Brentwood, Langdon"] <-"C"
levels(dc_bnb_limited$Neighbourhood.Cleansed)[levels(dc_bnb_limited$Neighbourhood.Cleansed)=="Woodridge, Fort Lincoln, Gateway"] <-"C"
levels(dc_bnb_limited$Neighbourhood.Cleansed)[levels(dc_bnb_limited$Neighbourhood.Cleansed)=="North Michigan Park, Michigan Park, University Heights"] <-"D"
levels(dc_bnb_limited$Neighbourhood.Cleansed)[levels(dc_bnb_limited$Neighbourhood.Cleansed)=="Historic Anacostia"] <-"D"
levels(dc_bnb_limited$Neighbourhood.Cleansed)[levels(dc_bnb_limited$Neighbourhood.Cleansed)=="Mayfair, Hillbrook, Mahaning Heights"] <-"D"
levels(dc_bnb_limited$Neighbourhood.Cleansed)[levels(dc_bnb_limited$Neighbourhood.Cleansed)=="Deanwood, Burrville, Grant Park, Lincoln Heights, Fairmont Heights"] <-"D"
levels(dc_bnb_limited$Neighbourhood.Cleansed)[levels(dc_bnb_limited$Neighbourhood.Cleansed)=="River Terrace, Benning, Greenway, Dupont Park"] <-"D"
levels(dc_bnb_limited$Neighbourhood.Cleansed)[levels(dc_bnb_limited$Neighbourhood.Cleansed)=="Capitol View, Marshall Heights, Benning Heights"] <-"D"
levels(dc_bnb_limited$Neighbourhood.Cleansed)[levels(dc_bnb_limited$Neighbourhood.Cleansed)=="Twining, Fairlawn, Randle Highlands, Penn Branch, Fort Davis Park, Fort Dupont"] <-"D"
levels(dc_bnb_limited$Neighbourhood.Cleansed)[levels(dc_bnb_limited$Neighbourhood.Cleansed)=="Fairfax Village, Naylor Gardens, Hillcrest, Summit Park"] <-"D"
levels(dc_bnb_limited$Neighbourhood.Cleansed)[levels(dc_bnb_limited$Neighbourhood.Cleansed)=="Sheridan, Barry Farm, Buena Vista"] <-"D"
levels(dc_bnb_limited$Neighbourhood.Cleansed)[levels(dc_bnb_limited$Neighbourhood.Cleansed)=="Douglas, Shipley Terrace"] <-"D"
levels(dc_bnb_limited$Neighbourhood.Cleansed)[levels(dc_bnb_limited$Neighbourhood.Cleansed)=="Congress Heights, Bellevue, Washington Highlands"] <-"D"

#condensing SF neighborhood levels (if statements? function with input as neighborhoods to consolidate (vector) and nbhd consiolidated to? loop?)
levels(sf_bnb_limited$Neighbourhood.Cleansed)[levels(sf_bnb_limited$Neighbourhood.Cleansed)=="Crocker Amazon"] <-"South SF"
levels(sf_bnb_limited$Neighbourhood.Cleansed)[levels(sf_bnb_limited$Neighbourhood.Cleansed)=="Diamond Heights"] <-"Noe Valley+"
levels(sf_bnb_limited$Neighbourhood.Cleansed)[levels(sf_bnb_limited$Neighbourhood.Cleansed)=="Excelsior"] <-"South SF"
levels(sf_bnb_limited$Neighbourhood.Cleansed)[levels(sf_bnb_limited$Neighbourhood.Cleansed)=="Glen Park"] <-"Noe Valley+"
levels(sf_bnb_limited$Neighbourhood.Cleansed)[levels(sf_bnb_limited$Neighbourhood.Cleansed)=="Noe Valley"] <-"Noe Valley+"
levels(sf_bnb_limited$Neighbourhood.Cleansed)[levels(sf_bnb_limited$Neighbourhood.Cleansed)=="Lakeshore"] <-"South SF"
levels(sf_bnb_limited$Neighbourhood.Cleansed)[levels(sf_bnb_limited$Neighbourhood.Cleansed)=="Ocean View"] <-"South SF"
levels(sf_bnb_limited$Neighbourhood.Cleansed)[levels(sf_bnb_limited$Neighbourhood.Cleansed)=="Outer Mission"] <-"South SF"
levels(sf_bnb_limited$Neighbourhood.Cleansed)[levels(sf_bnb_limited$Neighbourhood.Cleansed)=="Parkside"] <-"Outer Sunset+"
levels(sf_bnb_limited$Neighbourhood.Cleansed)[levels(sf_bnb_limited$Neighbourhood.Cleansed)=="Outer Sunset"] <-"Outer Sunset+"
levels(sf_bnb_limited$Neighbourhood.Cleansed)[levels(sf_bnb_limited$Neighbourhood.Cleansed)=="Twin Peaks"] <-"Outer Sunset+"
levels(sf_bnb_limited$Neighbourhood.Cleansed)[levels(sf_bnb_limited$Neighbourhood.Cleansed)=="West of Twin Peaks"] <-"Outer Sunset+"
levels(sf_bnb_limited$Neighbourhood.Cleansed)[levels(sf_bnb_limited$Neighbourhood.Cleansed)=="Visitacion Valley"] <-"South SF"
levels(sf_bnb_limited$Neighbourhood.Cleansed)[levels(sf_bnb_limited$Neighbourhood.Cleansed)=="Treasure Island/YBI"] <-"Mission+"
levels(sf_bnb_limited$Neighbourhood.Cleansed)[levels(sf_bnb_limited$Neighbourhood.Cleansed)=="Mission"] <-"Mission+"
levels(sf_bnb_limited$Neighbourhood.Cleansed)[levels(sf_bnb_limited$Neighbourhood.Cleansed)=="Pacific Heights"] <-"Pacific Heights+"
levels(sf_bnb_limited$Neighbourhood.Cleansed)[levels(sf_bnb_limited$Neighbourhood.Cleansed)=="Parkside"] <-"Pacific Heights+"
levels(sf_bnb_limited$Neighbourhood.Cleansed)[levels(sf_bnb_limited$Neighbourhood.Cleansed)=="Presidio"] <-"Pacific Heights+"
levels(sf_bnb_limited$Neighbourhood.Cleansed)[levels(sf_bnb_limited$Neighbourhood.Cleansed)=="Presidio Heights"] <-"Pacific Heights+"
levels(sf_bnb_limited$Neighbourhood.Cleansed)[levels(sf_bnb_limited$Neighbourhood.Cleansed)=="Seacliff"] <-"Pacific Heights+"

  #--> remove Golden Gate Park
  sf_bnb_limited <- sf_bnb_limited[!(sf_bnb_limited$Neighbourhood.Cleansed=='Golden Gate Park'),] #removing 4 rows



## DEALING WITH MISSINGNESS ##
sum(is.na(bos_bnb_limited$Price)) #9 ~ 0.35%
sum(is.na(dc_bnb_limited$Price)) #102 ~ 2.46%
sum(is.na(sf_bnb_limited$Price)) #83 ~ 1.98%
# --> visualize y missingness by neighborhood to make sure it can just be deleted

#delete missingness in y
bos_bnb_clean <- bos_bnb_limited[-which(is.na(bos_bnb_limited$Price)),] #removing 9 rows
dc_bnb_clean <- dc_bnb_limited[-which(is.na(dc_bnb_limited$Price)),] #removing 9 rows
sf_bnb_clean <- sf_bnb_limited[-which(is.na(sf_bnb_limited$Price)),] #removing 83 rows


#missingness in x

  #identifying missingness
  summary(bos_bnb_clean)
  summary(dc_bnb_clean)
  summary(sf_bnb_clean)
    #-> lots in review variables


  #multiple imputation for missingness
  library(mice)
  bos_bnb_clean <- complete(mice(bos_bnb_clean))
  dc_bnb_clean <- complete(mice(dc_bnb_clean))
  sf_bnb_clean <- complete(mice(sf_bnb_clean))
  
## ADDRESSING MULTICOLLINEARITY ##
  
  #install.packages("car")
  library('car')
  b_quant <- bos_bnb_clean[c('Price', 'Review.Scores.Cleanliness','Review.Scores.Communication','Minimum.Nights',
                           'Beds','Bathrooms','Guests.Included','Reviews.per.Month')]
  dc_quant <- dc_bnb_clean[c('Price', 'Review.Scores.Cleanliness','Review.Scores.Communication','Minimum.Nights',
                               'Beds','Bathrooms','Guests.Included','Reviews.per.Month')]
  sf_quant <- sf_bnb_clean[c('Price', 'Review.Scores.Cleanliness','Review.Scores.Communication','Minimum.Nights',
                               'Beds','Bathrooms','Guests.Included','Reviews.per.Month')]
  
  b_lm.quant <- lm(Price~., data=b_quant)
  dc_lm.quant <- lm(Price~., data=dc_quant)
  sf_lm.quant <- lm(Price~., data=sf_quant)
  
  vif(b_lm.quant)
  vif(dc_lm.quant)
  vif(sf_lm.quant) # -> none above ten, no multicollinearity to deal with
  
write.csv(bos_bnb_clean, "C:/Users/ashto/Documents/Wellesley/2023_Spring/DS Capstone/Data/bos_clean.csv", row.names=FALSE)
write.csv(dc_bnb_clean, "C:/Users/ashto/Documents/Wellesley/2023_Spring/DS Capstone/Data/dc_clean.csv", row.names=FALSE)
write.csv(sf_bnb_clean, "C:/Users/ashto/Documents/Wellesley/2023_Spring/DS Capstone/Data/sf_clean.csv", row.names=FALSE)

