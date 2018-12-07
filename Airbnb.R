################### Importing Partically Cleaned Data #############
air = read.csv("Airbnb2.csv")
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
air = air[,-c(22)] #remove room_type column
table(air$EntirePlace)

# Dummy for Response Time
levels(air$host_response_time)

air$Respond_fewDaysOrMore = ifelse(air$host_response_time == "a few days of more",1,0)
air$Respond_WithinDay = ifelse(air$host_response_time== "within a day",1,0)
air$Respond_WithinFewHours = ifelse(air$host_response_time == "within a few hours",1,0)
air$Respond_WithinTheHour = ifelse(air$host_response_time == "within an hour",1,0)
air = air[,-c(10)] # remove host_response_time column

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
air = air[,-c(20)] # remove property_type 

# Dummy for Bed Type
levels(air$bed_type)

air$Bed_Couch = ifelse(air$bed_type == "Couch",1,0)
air$Bed_Futon = ifelse(air$bed_type == "Futon",1,0)
air$Bed_PullOutSofa = ifelse(air$bed_type == "Pull-out Sofa",1,0)
air$Bed_Bed = ifelse(air$bed_type == "Real Bed",1,0)
air = air[,-c(24)] # remove bed_type variable

# Dummy for Cancellation Policy
levels(air$cancellation_policy)

air$Cancel_Flexible = ifelse(air$cancellation_policy == "flexible",1,0)
air$Cancel_Moderate = ifelse(air$cancellation_policy == "moderate",1,0)
air$Cancel_Strict = ifelse(air$cancellation_policy == "strict_14_with_grace_period",1,0)
air = air[,-c(50)] # remove cancellation policy column

# Dummy Variable for Space (if its filled out then 1, else 0)
air$space = ifelse(air$space == "",0,1)

# Dummy Variable for Description (if its filled out then 1, else 0)
#air$description = ifelse(air$description == "",0,1)
#air = air[,-c(3)] # they are all filled in so it doesnt matter

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
table(is.na(air$review_scores_rating)) # 77 NA values 
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
# Family/kid friendly
air$isFamilyFriendly = ifelse(grepl("Family/kid friendly",air$amens),1,0)
table(air$isFamilyFriendly)

# all unique amenities
count=0
uniques = function(){
    amenities = vector()
    for (i in 1:length(air$amens)){
        for (j in 1:length(air$amens[[i]])){
            amenities[count] = air$amens[[i]][[j]]
        }
        count = count+1
    }
    return(amenities)
}

amenities = uniques()
unique(amenities)
  
# Adding Property Price Column
air$zipcode = as.numeric(air$zipcode) #turning the column to a numeric one

air$square_meter = air$square_feet*0.32048 #creating square meter column

air$propertyPrice = ifelse(air$zipcode == 75001, air$square_meter*10777, 
       ifelse(air$zipcode == 75002, air$square_meter*9341, 
              ifelse(air$zipcode == 75003, air$square_meter*10296,
                     ifelse(air$zipcode == 75004, air$square_meter*11655,
                            ifelse(air$zipcode == 75005, air$square_meter*10828,
                                   ifelse(air$zipcode == 75006, air$square_meter*12442,
                                          ifelse(air$zipcode == 75007, air$square_meter*12171,
                                                 ifelse(air$zipcode == 75008, air$square_meter*10762,
                                                        ifelse(air$zipcode == 75009, air$square_meter*8682,
                                                               ifelse(air$zipcode == 75010, air$square_meter*7718,
                                                                      ifelse(air$zipcode == 75011, air$square_meter*8161,
                                                                             ifelse(air$zipcode == 75012, air$square_meter*7724,
                                                                                    ifelse(air$zipcode == 75013, air$square_meter*7770,
                                                                                           ifelse(air$zipcode == 75014, air$square_meter*8368,
                                                                                                  ifelse(air$zipcode == 75015, air$square_meter*8581,
                                                                                                         ifelse(air$zipcode == 75016, air$square_meter*8851,
                                                                                                                ifelse(air$zipcode == 75017, air$square_meter*8504,
                                                                                                                       ifelse(air$zipcode == 75018, air$square_meter*7420,
                                                                                                                              ifelse(air$zipcode == 75019, air$square_meter*6507,
                                                                                                                                     ifelse(air$zipcode == 75020, air$square_meter*6792,0)
                                                                                                                                     )
                                                                                                                              )
                                                                                                                       )
                                                                                                                )
                                                                                                         )
                                                                                                  )
                                                                                           )
                                                                                    )
                                                                             )
                                                                      )
                                                               )
                                                        )
                                                 )
                                          )
                                   )
                            )
                     )
              
       )
)



# removing 0's from property price row(cause we dont have their price per square foot)
air = air[!air$propertyPrice == 0,]

# are their any NAs in the data?
which(is.na(air))
any(is.na(air))
air = na.omit(air)
any(is.na(air)) #all NAs gone

#Turning columns 26, 27, 29, 30 to numerics (theyre prices right now)
#removing the dollar sign
air$price <- as.numeric(gsub("[$,]", "", air$price))
air$security_deposit <- as.numeric(gsub("[$,]", "", air$security_deposit))
air$cleaning_fee <- as.numeric(gsub("[$,]", "", air$cleaning_fee))
air$extra_people <- as.numeric(gsub("[$,]", "", air$extra_people))

#making the income per month column
air$IncomePerMonth = ((air$price*30)*air$occupancy_rate)





############ graphs ############

# 1. creating a map of the locations in france
map = google_map(key = "AIzaSyB7V1GRvLN_nfnocXtJb0gVAFl5g7nLKlE", location = "paris", zoom = 8)
map

library(ggmap)
map = get_map(location = "Europe", zoom = 4)
