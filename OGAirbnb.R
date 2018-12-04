################### Importing Partically Cleaned Data #############
air = read.csv("Airbnb.csv")
air = air[,-c(1)] # Removing weird new variable

################## Updating Variable Structure ##############
# Change some variables to 0 & 1 from t & f
air$host_has_profile_pic = ifelse(air$host_has_profile_pic == "t",1,0)
air$host_identity_verified = ifelse(air$host_identity_verified == "t",1,0)
air$host_is_superhost = ifelse(air$host_is_superhost == "t",1,0)
air$instant_bookable = ifelse(air$instant_bookable == "t",1,0)
air$is_business_travel_ready = ifelse(air$is_business_travel_ready == "t",1,0)
air$has_availability = ifelse(air$has_availability == "t",1,0)
air$is_location_exact = ifelse(air$is_location_exact == "t",1,0)
air$require_guest_profile_picture = ifelse(air$require_guest_profile_picture == "t",1,0)
air$require_guest_phone_verification = ifelse(air$require_guest_phone_verification == "t",1,0)

######################### DUMMY VARIABLES ########################
# Dummy for Room Type
levels(air$room_type)

air$EntirePlace = ifelse(air$room_type == "Entire home/apt",1,0)
air$PrivateRoom = ifelse(air$room_type == "Private room",1,0)
air$SharedRoom = ifelse(air$room_type == "Shared room",1,0)
air = air[,-c(22)]

# Dummy for Response Time
levels(air$host_response_time)

air$Respond_fewDaysOrMore = ifelse(air$host_response_time == "a few days of more",1,0)
air$Respond_WithinDay = ifelse(air$host_response_time== "within a day",1,0)
air$Respond_WithinFewHours = ifelse(air$host_response_time == "within a few hours",1,0)
air$Respond_WithinTheHour = ifelse(air$host_response_time == "within an hour",1,0)
air = air[,-c(10)]

#Dummy for Property Type
levels(air$property_type)
air$Apartment = ifelse(air$property_type == "Apartment",1,0)
air$BedNBreakdast = ifelse(air$property_type == "Bed and breakfast",1,0)
air$Condominium = ifelse(air$property_type == "Condominium",1,0)
air$GuestSuite = ifelse(air$property_type == "Guest suite",1,0)
air$House = ifelse(air$property_type == "House",1,0)
air$Loft = ifelse(air$property_type == "Loft",1,0)
air$Other = ifelse(air$property_type == "Other",1,0)
air$ServicedApartment = ifelse(air$property_type == "Serviced apartment",1,0)
air$Townhouse = ifelse(air$property_type == "Townhouse",1,0)
air = air[,-c(20)]

# Dummy for Bed Type
levels(air$bed_type)

air$Bed_Couch = ifelse(air$bed_type == "Couch",1,0)
air$Bed_Futon = ifelse(air$bed_type == "Futon",1,0)
air$Bed_PullOutSofa = ifelse(air$bed_type == "Pull-out Sofa",1,0)
air$Bed_Bed = ifelse(air$bed_type == "Real Bed",1,0)
air = air[,-c(24)]

# Dummy for Cancellation Policy
levels(air$cancellation_policy)

air$Cancel_Flexible = ifelse(air$cancellation_policy == "felxible",1,0)
air$Cancel_Moderate = ifelse(air$cancellation_policy == "moderate",1,0)
air$Cancel_Strict = ifelse(air$cancellation_policy == "strict_14_with_grace_period",1,0)
air = air[,-c(50)]

# Dummy Variable for Space (if its filled out then 1, else 0)
air$space = ifelse(air$space == "",0,1)

# Dummy Variable for Description (if its filled out then 1, else 0)
air$description = ifelse(air$description == "",0,1)
air = air[,-c(3)] # they are all filled in so it doesnt matter

# Dummy Variable for Transit (if its filled out then 1, else 0)
air$transit = ifelse(air$transit == "",0,1)

# Dummy Variable for Access (if its filled out then 1, else 0)
air$access = ifelse(air$access == "",0,1)

# Dummy Variable for Interaction (if its filled out then 1, else 0)
air$interaction = ifelse(air$interaction == "",0,1)

# Dummy Variable for House Rules (if its filled out then 1, else 0)
air$house_rules = ifelse(air$house_rules == "",0,1)

# Dummy Variable for Host_About (if its filled out then 1, else 0)
air$host_about = ifelse(air$host_about == "",0,1)


######################## Removing NA Values #########################
# removing NAs in review_scores_rating
air = air[!is.na(air$review_scores_rating),]
sum(is.na(air$review_scores_rating)) #check for 0 NAs

# removing NAs in review_scores_accuracy
air = air[!is.na(air$review_scores_accuracy),]
sum(is.na(air$review_scores_accuracy))

# Adding 0 to security_deposit if its blank
#assumption: blank security depoist fee is $0
air$security_deposit = as.character(air$security_deposit) # turning column to character
air$security_deposit = sub("^$", "$0.00", air$security_deposit) # adding 0s to blank cells

# Adding 0 to cleaning_fee if its blank
air$cleaning_fee = as.character(air$cleaning_fee) # turning column to character
air$cleaning_fee = sub("^$", "$0.00", air$cleaning_fee) # adding 0s to blank cells

##################### Cleaning Amenities Columns #########################
air$amenities <- gsub("[{\\\"}]", "", air$amenities) #now the amenities column separated each amenity by a comma
air$amens = strsplit(air$amenities,split=',', fixed=TRUE)
air$amens = as.vector(air$amens)

# TV dummy
air$hasTV = ifelse(grepl("TV",air$amens),1,0)
table(air$hasTV)
# Internet 
air$hasInternet = ifelse(grepl("Internet",air$amens),1,0)
table(air$hasInternet)
# Wifi
air$hasWifi = ifelse(grepl("Wifi",air$amens),1,0)
table(air$hasWifi)
# Shampoo
air$hasShampoo = ifelse(grepl("Shampoo",air$amens),1,0)
table(air$hasShampoo)
# Heat 
air$hasHeat = ifelse(grepl("Heat",air$amens),1,0)
table(air$hasHeat)
# Air Conditioning 
air$hasAC = ifelse(grepl("Air conditioning",air$amens),1,0)
table(air$hasAC)
# Breakfast 
air$hasBreakfast = ifelse(grepl("Breakfast",air$amens),1,0)
table(air$hasBreakfast)
# Desk/workspace
air$hasDesk = ifelse(grepl("workspace",air$amens),1,0)
table(air$hasDesk)
# Fireplace
air$hasFireplace = ifelse(grepl("fireplace",air$amens),1,0)
table(air$hasFireplace)
# iron 
air$hasIron = ifelse(grepl("Iron",air$amens),1,0)
table(air$hasIron)
# Hair dryer 
air$hasHairDryer = ifelse(grepl("Hair dryer",air$amens),1,0)
table(air$hasHairDryer)
# Private entrance 
air$hasPrivateEnt = ifelse(grepl("Private entrance",air$amens),1,0)
table(air$hasPrivateEnt)
# Kitchen 
air$hasKitchen = ifelse(grepl("Kitchen",air$amens),1,0)
table(air$hasKitchen)

# all unique amenities
uniques = function(){
  amenities = vector()
  for (i in length(air$amens)){
   amenities_unique = air$amens[[i]]
    }
    return(amenities)
  }
  
uniques()
  



write.csv(air, file = "CleanAirBnb.csv")



######### splitting amenities #############

# Remove {\"} from the amenitiies column
air$amenities <- gsub("[{\\\"}]", "", air$amenities) #now the amenities column separated each amenity by a comma

# Splitting the amenities column into columns for each amenity? This was we have a binary column for each.
install.packages("qdapTools")
library(qdapTools)

one = as.data.frame(mtabulate(s[[1]]))
two = as.data.frame(mtabulate(s[[2]]))
mtabulate(s[[2]])

#what are all of the unique amneities in all of the listings?
uniques <- sapply(strsplit(air$amenities, ",", fixed = TRUE), function(x) 
  paste(unique(x), collapse = ","))
uniques

amenities = as.data.frame(unique(unlist(air$amenities, use.names = FALSE)))

library(sqldf)               
sqldf("select distinct(amenities) from air")

#one big list
air2 = strsplit(as.character(air$amenities), ",")

air2 = as.data.frame(air2)
unique(air2)

air3 = as.matrix(air2)


#loop that adds the value of the row amenity if its unique

benchmark = unique(air2[[1]])
as.vector(benchmark)


