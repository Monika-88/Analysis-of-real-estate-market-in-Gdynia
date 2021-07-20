library(lubridate)
library(stringr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(gsubfn)
library(tidyr)


df <- RawData_Obs

df <- df[!duplicated(df$Name) & !duplicated(df$Id_offer) , ]

df[df == "None"]<- NA
df[df == ""]<- NA

#Removing diacritics in Polish

unwanted_array = list('¹'='a', 'æ'='c', 'ê'='e', '³'='l', 'ñ'='n', 'ó'='o', 
                      'œ'='s', 'Ÿ'='z', '¿'='z', '¥'='A', 'Æ'='C', 'Ê'='E',
                      '£'='L', 'Ñ'='N', 'Ó'='O', 'Œ'='S', ''='Z', '¯'='Z')


for (i in colnames(df)){
  df[[i]] <-  chartr(paste(names(unwanted_array), collapse=''),
                     paste(unwanted_array, collapse=''),
                     df[[i]])
}

#Change string date with month written in words and day written in X not XX format
#(date_of_publish, actualisation_date, distinction_date) to date type.


dict = list("January" = "01","stycznia" = "01", "February"="02", "lutego"="02",
            "March"="03", "marca"="03", "April"="04", "kwietnia"="04",
            "May" = "05", "maja" = "05", "Jun"="06", "czerwca"="06", 
            "July" = "07" , "lipca" = "07", "August"="08", "sierpnia"="08",
            "September"="09", "wrzesnia"="09", "October"="10", "pazdziernika"="10",
            "November" = "11", "listopada" = "11", "December"="12", "grudnia"="12", 
            "1"="01", "2"="02", "3"="03", "4"="04", "5"="05", "6"="06", "7"="07",
            "8"="08", "9"="09" )


#divide the date (day, month, year) into separate columns

edition_dop <- as.data.frame(str_split_fixed(df$Date_of_publish, " ", 4))
edition_ad <- as.data.frame(str_split_fixed(df$Actualisation_date, " ", 4))
edition_dd <- as.data.frame(str_split_fixed(df$Distinction_date, " ", 4))

#removing "of" from date

edition_dop$V2[edition_dop$V2 == "of"] <- ""
edition_ad$V2[edition_ad$V2 == "of"] <- ""
edition_dd$V2[edition_dd$V2 == "of"] <- ""

#change of month and day according to the "dict" list

for (i in 1:33){edition_dop <- replace(edition_dop, edition_dop == names(dict[i]), dict[i])}
for (i in 1:33){edition_ad <- replace(edition_ad, edition_ad == names(dict[i]), dict[i])}
for (i in 1:33){edition_dd <- replace(edition_dd, edition_dd == names(dict[i]), dict[i])}

#merge formatted columns (day, month, year) to get date format

edition_dop <- edition_dop %>% 
  mutate(date_of_publish=paste(V1, V2, V3, V4, sep = "")) %>% 
  mutate(date_of_publish=dmy(date_of_publish))

edition_ad <- edition_ad %>% 
  mutate(actualization_date=paste(V1, V2, V3, V4, sep = "")) %>% 
  mutate(actualization_date=dmy(actualization_date))

edition_dd <- edition_dd %>% 
  mutate(distinction_date=paste(V1, V2, V3, V4, sep = "")) %>% 
  mutate(distinction_date=dmy(distinction_date))



#adding formatted columns to the df table

df <- cbind(df, edition_dop$date_of_publish)
df <- cbind(df, edition_ad$actualization_date)
df <- cbind(df, edition_dd$distinction_date)

#removing old columns that are no longer useful after formatting

df <- df %>% 
  select(-Date_of_publish,-Actualisation_date, -Distinction_date)

#changing the name of new columns

colnames(df)[15] <- "Date_of_publish"
colnames(df)[16] <- "Actualization_date"
colnames(df)[17] <- "Distinction_date"


#change the date format

df$Date <- as.Date(df$Date)
df$Year_of_construction <- year(as.Date(df$Year_of_construction, format = "%Y" ))

#age of the apartment at the time of sale

df$Apartment_age <- (as.integer(format(Sys.Date(), "%Y"))) - as.integer(df$Year_of_construction)


#removing undesirable characters and blank lines from the price and views columns


df$Price <- df$Price %>% 
  str_replace_all(" zl","") %>%
  str_replace_all(" ","") %>%
  str_replace_all(",",".") #%>%
  #str_match(Price, "[0-9]+")

df$Price <- round(as.numeric(df$Price),0)
df$Price_in_thousands <- round(df$Price/1000,0)


df$Views <- str_replace_all(df$Views," ","")

#changing the format of the following columns to numeric

cols_to_numeric <- c('Id_offer', 'Price', 'Surface', 'Views')


df[cols_to_numeric] <- as.numeric(unlist(df[cols_to_numeric]))

df <- df %>%
  mutate(Price_m2 = Price / Surface) %>% 
  mutate(Price_m2 = round(Price_m2, 0)) 



df$District_1 <- NA 
df$District_2 <- NA

df$District_1 <- as.character(df$District_1)
df$District_2 <- as.character(df$District_2)


df <- df %>%
  mutate(District_1 = case_when(
    str_detect(District, "Babie|babie|bie |e Do|Doly") ~ "Babie Doly",
    str_detect(District, "Chwa|chwa|warz|rzno|no-W|-Wic|iczl|zlin|lino") ~ "Chwarzno-Wiczlino",
    str_detect(District, "Chyl|chyl|ylon|onia") ~ "Chylonia",
    str_detect(District, "sowa") ~ "Cisowa",
    str_detect(District, "Dabr|dabr|brow|rowa") ~ "Dabrowa",
    str_detect(District, "Dzia|dzia|ialk|i Le|esne") ~ "Dzialki Lesne",
    str_detect(District, "Grab|grab|abow|owek") ~ "Grabowek",
    str_detect(District, "Kami|kami|mien|enna|na G| Gor|Gora") ~ "Kamienna Gora",
    str_detect(District, "Karw|karw|rwin|winy") ~ "Karwiny",
    str_detect(District, "Lesz|lesz|szczyn|ynki") ~ "Leszczynki",
    str_detect(District, "Maly|maly|ly K") ~ "Maly Kack",
    str_detect(District, "Oblu|oblu|luze") ~ "Obluze",
    str_detect(District, "Oksy|oksy|sywi|ywie") ~ "Oksywie",
    str_detect(District, "Orlo|orlo") ~ "Orlowo",
    str_detect(District, "Pogo|pogo") ~ "Pogorze",
    str_detect(District, "Pust|pust|stki|ki C| Cis|owsk|skie|ie-D|-Dem|empt|ptow|towo") ~ "Pustki Cisowskie-Demptowo",
    str_detect(District, "Redl|redl|dlow") ~ "Redlowo",
    str_detect(District, "Srod|srod|odmi|mies|esci|Centru|Cen|entrum|ynia Po|Port") ~ "Srodmiescie",
    str_detect(District, "Wiel|wiel|elki|ki K|Fika|kako|kowo|Kacze|Buki") ~ "Wielki Kack",
    str_detect(District, "no-L|-Les|-les|esni|nicz|czow|owka") ~ "Witomino",
    str_detect(District, "Wito|wito|tomi|mino|no-R|-Rad|adio|iost|stac|acja") ~ "Witomino",
    str_detect(District, "Wzgo|e sw|sw. |. Ma|Maks|ksym|ymil|ilia|iana") ~ "Wzgorze sw. Maksymiliana",
    
    
    
    TRUE ~ District_1
  )
  )


df <- df %>%
  
  
  mutate(District_2 = case_when(
    str_detect(Name, "Babie|Doly") ~ "Babie Doly",
    str_detect(Name, "Chwar|warzn|arzno|Wiczl|iczli|lino") ~ "Chwarzno-Wiczlino",
    str_detect(Name, "Chylo|lonia") ~ "Chylonia",
    str_detect(Name, "isowa") ~ "Cisowa",
    str_detect(Name, "Dabro|browa") ~ "Dabrowa",
    str_detect(Name, "zialki|lesne") ~ "Dzialki Lesne",
    str_detect(Name, "Grabow|bowek") ~ "Grabowek",
    str_detect(Name, "Kamien|ienna|Gora") ~ "Kamienna Gora",
    str_detect(Name, "Karwin|rwiny") ~ "Karwiny",
    str_detect(Name, "Leszcz|czynki") ~ "Leszczynki",
    str_detect(Name, "Maly|ly K") ~ "Maly Kack",
    str_detect(Name, "Obluz|bluze") ~ "Obluze",
    str_detect(Name, "Oksyw|sywie") ~ "Oksywie",
    str_detect(Name, "Orlow|rlowo") ~ "Orlowo",
    str_detect(Name, "Pogor") ~ "Pogorze",
    str_detect(Name, "Pustki|Cisowsk|Demmpt|ptowo") ~ "Pustki Cisowskie-Demptowo",
    str_detect(Name, "Redlo|dlowo") ~ "Redlowo",
    str_detect(Name, "Srodm|odmie|escie|Centru|entrum") ~ "Srodmiescie",
    str_detect(Name, "Wielk|ielki|ki Ka|Fikak|kakowo|Kacze|Buki") ~ "Wielki Kack",
    str_detect(Name, "Lesni|niczow|czowka") ~ "Witomino",
    str_detect(Name, "Witom|itomi|omino|Radio|diost|stacj|tacja") ~ "Witomino",
    str_detect(Name, "Wzgor|Maksym|ksymil|ymilia|iliana") ~ "Wzgorze sw. Maksymiliana",
    
    
    TRUE ~ District_2
  )
  )


df <- df %>% 
  
  mutate (District_3 = ifelse(is.na(District_1), df$District_2, df$District_1))

df$District <- as.factor(df$District_3) 

df <- df %>% 
  select(-District_1, -District_2, -District_3)

df$Heating_type <- as.character(NA)
df$Heating_type1 <- as.character(NA)
df$Heating_type2 <- as.character(NA)


df <- df %>%
  
  mutate(Heating_type1 = case_when(
    str_detect(Heating_type_information, "gaz|Gaz") ~ "Gas",
    str_detect(Heating_type_information, "Komin|komin") ~ "Fireplace",
    str_detect(Heating_type_information, "Elek|elek") ~ "Electric",
    str_detect(Heating_type_information, "Pomp|pomp") ~ "Heat pump",
    str_detect(Heating_type_information, "Kot|kot|Piec|piec|komin|Komin|wlas|Wlas") ~ "Own boiler room",
    
    
    TRUE ~ Heating_type1
  )
  )

df <- df %>%
  
  mutate(Heating_type2 = case_when(
    str_detect(Heating_type_information, "co|OPEC|CO|Co|C.|c.|Opec|miej|Miej|MIEJ|centr|Centr|CENT") ~ "District heating",
    
    
    TRUE ~ Heating_type2
  )
  )

df <- df %>% 
  
  mutate (Heating_type = ifelse(is.na(Heating_type1), df$Heating_type2, df$Heating_type1))


df <- df %>% 
  select(-Heating_type_information, -Heating_type1, -Heating_type2)

df$Number_of_rooms <- as.numeric(df$Number_of_rooms)
df$Building_height <- as.numeric(df$Building_height)

df <- df %>% 
  select(-Date, -Street, -Views, -Additional_information, -Id_offer,
         -Year_of_construction, -Date_of_publish, -Actualization_date,
         -Distinction_date, -Name) %>% 
  filter(Price>30000)%>% 
  filter(Price_m2>2000) %>% 
  filter(Surface/Number_of_rooms>7) %>% 
  filter(Floor>0) %>% 
  filter(Building_height>0) %>%
  na.omit()


df <-df %>% 
  mutate(df, Floor = replace(Floor, Floor == "parter", "0")) 

districs <-  df %>%
  group_by(District) %>%
  summarise(mediana = median(Price), 
            ilosc = n()) %>%
  mutate(Better_district = ifelse(mediana > 580000,1,0)) %>%
  select(District, Better_district)


df <- left_join(df, districs, by = "District")

df$Floor <- as.numeric(unlist(df$Floor))

high_floor <- df %>% 
  mutate(Good_view = ifelse(Floor>11, 1, 0))%>%
  select(Floor, Good_view) %>% 
  distinct(Floor, .keep_all = TRUE)

df <- left_join(df, high_floor, by = "Floor")


df_heating_type <- df %>% 
  group_by(Heating_type)%>%
  summarise(srednia = mean(Price), 
            ilosc = n()) %>% 
  mutate(Better_heating = ifelse(srednia>800000, 1, 0))%>% 
  select(Heating_type, Better_heating) 


df <- left_join(df, df_heating_type, by = "Heating_type")



df$District <- as.factor(df$District)
df$Heating_type <- as.factor(df$Heating_type)




