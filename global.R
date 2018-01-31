library(shinyjs)
library(shinydashboard)
library(DT)
library(data.table)
library(googleVis)
library(dplyr)
library(leaflet)
library(sp)
library(ggmap)
library(maptools)
library(broom)
library(httr)
library(rgdal)
library(V8)
library(geojsonio)

#create neighborhood borders in SpatialPolygonsDataFrame
#r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson') #GET is a function in the "httr" package
#yankee <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F) #readOGR is a function in the "rgdal" package
yankee = geojsonio::geojson_read(x = "./NY-Neighborhoods.geojson", method="local", what = "sp")


#s <- GET('http://s3-us-west-2.amazonaws.com/boundaries.latimes.com/archive/1.0/boundary-set/la-county-neighborhoods-current.geojson')
#dodger <- readOGR(content(s,'text'), 'OGRGeoJSON', verbose = F)
dodger = geojsonio::geojson_read(x = "./la-county-neighborhoods-current.geojson", method = "local", what = "sp")


#convert SPDF to dataframe, clean up resulting DF, and add a column to join to fact data
#yankee = yankee %>% as.data.frame(.) %>% select(., 'neighborhood', 'borough', 'X.id')
#dodger = dodger %>% as.data.frame(.) %>% select(., 'name', 'metadata', 'resource_uri')

#remove row names
#rownames(yankee) = NULL
#rownames(dodger) = NULL

#load CSV for neighborhood facts
fax_b = fread('NHStatic.csv', header = TRUE, stringsAsFactors = FALSE)

#Filter by city for map calculations
nyfax = fax_b %>% filter(., CityID == 'New York') %>% mutate(., IncomeRank = rank(nyfax$MedHHIncome)/length(nyfax$MedHHIncome)*100, SizeRank = rank(nyfax$AveHouseholdSize)/length(nyfax$AveHouseholdSize)*100, VioRank = rank(nyfax$ViolentCrime)/length(nyfax$ViolentCrime)*100, PropRank = rank(nyfax$PropertyCrime)/length(nyfax$PropertyCrime)*100, EdRank = rank(nyfax$MedEducation)/length(nyfax$MedEducation)*100, MedAge = rank(nyfax$MedAge)/length(nyfax$MedAge)*100, ForeignRank = rank(nyfax$PctForeignBorn)/length(nyfax$PctForeignBorn)*100) %>% mutate(., AveTransport = (.$WalkScore + .$TransitScore + .$BikeScore)/3)
colnames(nyfax)[19:24] = c("Household Income", "Household Size", "Violent Crime per 1000", "Property Crime per 1000",
                           "Median Educational Attainment", "Foreign Born Population")
lafax = fax_b %>% filter(., CityID == 'Los Angeles') %>% mutate(., IncomeRank = rank(lafax$MedHHIncome)/length(lafax$MedHHIncome)*100, SizeRank = rank(lafax$AveHouseholdSize)/length(lafax$AveHouseholdSize)*100, VioRank = rank(lafax$ViolentCrime)/length(lafax$ViolentCrime)*100, PropRank = rank(lafax$PropertyCrime)/length(lafax$PropertyCrime)*100, EdRank = rank(lafax$MedEducation)/length(lafax$MedEducation)*100, MedAge = rank(lafax$MedAge)/length(lafax$MedAge)*100, ForeignRank = rank(lafax$PctForeignBorn)/length(lafax$PctForeignBorn)*100) %>% mutate(., AveTransport = (.$WalkScore + .$TransitScore + .$BikeScore)/3)
colnames(lafax)[19:24] = c("Household Income", "Household Size", "Violent Crime per 1000", "Property Crime per 1000",
                           "Median Educational Attainment", "Foreign Born Population")

#create variable with colnames as choice for dropdown
choice = colnames(fax_b)[c(-1,-2,-3, -4)]

#Insights Transformations

  #Household Income by Education  
candlefax1 = fax_b %>% select(., CityID, MedEducation, MedHHIncome)

cf1_ops = list(width="600px", height="600px",
               title="Median Household Income by Education Level",
               hAxis="{title:'Median Education Level'}",
               vAxis="{title:'Median Household Income'}", legend="none")

candlequants1 = candlefax1 %>% group_by(., MedEducation) %>%
  summarise(., low = min(MedHHIncome), open = quantile(MedHHIncome, .25), close = quantile(MedHHIncome, .75), high = max(MedHHIncome)) %>% 
  arrange(., MedEducation[c(3,2,4,1)])

  #Violent and Property Crime Rates

scatterfax1 = fax_b %>% select(., ViolentCrime, PropertyCrime)

sf1_ops = list(width="600px", height="600px",
                  title="Violent Crime vs Property Crime Rates",
                  hAxis="{title:'Violent Crime Rate (per 1,000)', minValue: 0, maxValue: 40}",
                  vAxis="{title:'Property Crime Rate (per 1,000)', minValue: 0, maxValue: 100}", legend="none")

scatterfax1[is.na(scatterfax1)] = 0

  #Household Income as a Function of Household Size

scatterfax2 = fax_b %>% select(., AveHouseholdSize, MedHHIncome)

sf2_ops = list(width="600px", height="600px",
                title="Household Income as a Function of Household Size",
                hAxis="{title:'Average Household Size'}",
                vAxis="{title:'Median Household Income'}", legend="none")

  #Violent Crime Rate Histogram

histofax1 = fax_b %>% select(., ViolentCrime, NhoodName)

h1_ops = list(width="600px", height="600px",
              title="Violent Crime Distribution",
              hAxis="{title:'Violent Crime Rate (per 1,000)'}",
              vAxis="{title:'Number of Neighborhoods'}", legend='none',
              histogram="{bucketSize: 2.5, minValue: 0, maxValue: 20}")

  #Property Crime Rate Histogram
histofax2 = fax_b %>% select(., PropertyCrime, NhoodName)

h2_ops = list(width="600px", height="600px",
              title="Property Crime Distribution",
              hAxis="{title:'Property Crime Rate (per 1,000)'}",
              vAxis="{title:'Number of Neighborhoods'}", legend='none',
              histogram="{bucketSize: 2.5, minValue: 0, maxValue: 20}")

  #Median Age Hisogram
histofax3 = fax_b %>% select(., MedAge, NhoodName)

h3_ops = list(width="600px", height="600px",
              title="Median Age Distribution",
              hAxis="{title:'Median Age'}",
              vAxis="{title:'Number of Neighborhoods'}", legend='none',
              histogram="{bucketSize: 1}")

  #Median Education Histogram
histofax4 = fax_b %>% select(., MedEducation, NhoodName)

EduNum = unique(histofax4$MedEducation)

histofax4 = histofax4 %>% mutate(., MedEducation = ifelse(.$MedEducation == EduNum[1], 1, (ifelse(.$MedEducation == EduNum[3], 2, ifelse(.$MedEducation == EduNum[2], 4, ifelse(.$MedEducation == EduNum[4], 0, 3))))))

h4_ops = list(width="600px", height="600px",
              title="Median Education Distribution",
              hAxis="{title:'Median Education'}",
              vAxis="{title:'Number of Neighborhoods'}", legend='none',
              histogram="{bucketSize: 1}")

#   #Average Transit Score
# scatterfax3 = fax_b[,c('CityID', 'NhoodName','WalkScore', 'TransitScore')]
# scatterfax3$NY = ifelse(fax_b$CityID == 'New York', scatterfax3$WalkScore, NA)
# scatterfax3$LA = ifelse(fax_b$CityID == 'Los Angeles', scatterfax3$WalkScore, NA)
#   
#   #fax_b %>% select(., NhoodName, WalkScore, TransitScore)
# 
# sf3_ops = list(width="600px", height="600px",
#                title="Transit Score and Walk Score by Neighborhood",
#                hAxis="{title:'Walk Score'}",
#                vAxis="{title:'Transit Score'}")

#Gradient map properties

  #LA Gradients
colIncome = colorQuantile("Spectral", domain = dodger$MedHHIncome, n = 5)
colHHSize = colorQuantile("Spectral", domain = dodger$AveHouseholdSize, n = 5)
colVio = colorQuantile("Spectral", domain = dodger$ViolentCrime, n = 5)
colProp = colorQuantile("Spectral", domain = dodger$PropertyCrime, n = 5)
colEd = colorFactor("Spectral", domain = dodger$MedEducation)
colAge = colorQuantile("Spectral", domain = dodger$MedAge, n = 5)
colWhite = colorQuantile("Spectral", domain = dodger$PctWhite, n = 5)
colBlack = colorQuantile("Spectral", domain = dodger$PctBlack, n = 5)
colAsian = colorQuantile("Spectral", domain = dodger$PctAsian, n = 5)
colHispanic = colorQuantile("Spectral", domain = dodger$PctHispanic, n = 5)
colForeign = colorQuantile("Spectral", domain = dodger$PctForeignBorn, n = 5)
colWalk = colorQuantile("Spectral", domain = dodger$WalkScore, n = 5)
colTrans = colorQuantile("Spectral", domain = dodger$TransitScore, n = 5)
colBike = colorQuantile("Spectral", domain = dodger$BikeScore, n = 5)

  #NY Gradients
NYcolIncome = colorQuantile("Spectral", domain = yankee$MedHHIncome, n = 5)
NYcolHHSize = colorQuantile("Spectral", domain = yankee$AveHouseholdSize, n = 5)
NYcolVio = colorQuantile("Spectral", domain = yankee$ViolentCrime, n = 5)
NYcolProp = colorQuantile("Spectral", domain = yankee$PropertyCrime, n = 5)
NYcolEd = colorFactor("Spectral", domain = yankee$MedEducation)
NYcolAge = colorQuantile("Spectral", domain = yankee$MedAge, n = 5)
NYcolWhite = colorQuantile("Spectral", domain = yankee$PctWhite, n = 5)
NYcolBlack = colorQuantile("Spectral", domain = yankee$PctBlack, n = 5)
NYcolAsian = colorQuantile("Spectral", domain = yankee$PctAsian, n = 5)
NYcolHispanic = colorQuantile("Spectral", domain = yankee$PctHispanic, n = 5)
NYcolForeign = colorQuantile("Spectral", domain = yankee$PctForeignBorn, n = 5)
NYcolWalk = colorQuantile("Spectral", domain = yankee$WalkScore, n = 5)
NYcolTrans = colorBin("Spectral", domain = yankee$TransitScore, bins = 5, pretty = TRUE)
NYcolBike = colorQuantile("Spectral", domain = yankee$BikeScore, n = 5)

#Matching Algorithm
matchmaker = function(a,z){ 
  b = abs(z - a) 
  b = which.min(b) 
  mymatch = c(b)
  c = abs(z - a[-b])
  c = which.min(c)
  if (c > b){
    c = c+1
  }
  mymatch = c(mymatch, c)
  d = abs(z - a[c(-b,-c)])
  d = which.min(d)
  if (d > c){
    d = d+1
  }
  if (d > b){
    d = d+1
  }
  mymatch = c(mymatch, d)
  mymatch
} #returns 3 matching indexes

#unique cities for UI
city = unique(fax_b$CityID)

#Neighborhoods for UI
hoods = fax_b$NhoodName

#Feature to match for UI
feature = colnames(nyfax)[19:24] #fix these indices when I add more features

#Stats for match data table
tablestats = colnames(fax_b)[c(5:10,15)]