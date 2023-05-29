#Draw map for Fig1

install.packages(c("ggplot2", "devtools", "dplyr", "stringr"))
install.packages(c("maps", "mapdata"))
devtools::install_github("dkahle/ggmap")

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(ggrepel)

#Get state of south carolina
states <- map_data("state")
sc_df <- subset(states, region == "south carolina")

gg1 <- ggplot() + 
  geom_polygon(data = sc_df, aes(x=long, y = lat, group = group), fill = "gray", color = "black") + 
  coord_fixed(1.3)
gg1

labs <- data.frame(
  long = c(-82.41458, -80.9623, -79.97755),
  lat = c(34.84389, 34.09398, 32.88228),
  locs = c("Northern", "Middle", "Southern"),
  stringsAsFactors = FALSE
)  

gg2<-gg1 + 
  geom_point(data = labs, aes(x = long, y = lat), color = "black", size = 5) +
  geom_point(data = labs, aes(x = long, y = lat), color = "yellow", size = 4)

#

#ggx<-gg1 + 
#geom_point(data = labs, aes(x = long, y = lat), color = "black", size = 5) +
#geom_point(data = labs, aes(x = long, y = lat), color = "yellow", size = 4) +
#geom_text(aes(x = long, y = lat, label = locs), data = labs)
#ggx 

#
gg3<-gg2 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

gg3 



gg3 +  geom_label_repel(aes(x = long, y = lat, label = locs), data = labs, 
                        box.padding   = 0.35, 
                        point.padding = 0.5,
                        segment.color = 'grey50') +
  theme_classic()

######################

data1 <- read.csv("C:/Users/scommod/Desktop/SCAFs/blank_corrected_dataset_1_14_19_metaboanalyst_v.csv")


Parklane_metals <- read.csv("C:/Users/scommod/Desktop/SCAFs/Parklane_metals.csv")


names(data1)
names(Parklane_metals)

colnames(data1)[3] <- "DATE"

str(Parklane_metals$DATE)
Parklane_metals$DATE<-as.Date(Parklane_metals$DATE, format = "%m/%d/%Y")

#take these out since there are no values
myvars <- names(Parklane_metals) %in% c("CarbonateCarbonCSNPM2.5LCTOT",
                                        "ECCSNPM2.5LCTOT",              
                                        "EuropiumPM2.5LC",             
                                        "GalliumPM2.5LC",               
                                        "GoldPM2.5LC",                  
                                        "HafniumPM2.5LC", 
                                        "IridiumPM2.5LC",
                                        "LanthanumPM2.5LC",
                                        "MercuryPM2.5LC",               
                                        "MolybdenumPM2.5LC",
                                        "NiobiumPM2.5LC",
                                        "OC1CSNUnadjustedPM2.5LCTOT",
                                        "OC2CSNUnadjustedPM2.5LCTOT",
                                        "OC3CSNUnadjustedPM2.5LCTOT",
                                        "OC4CSNUnadjustedPM2.5LCTOT",
                                        "OCCSNUnadjustedPM2.5LCTOT",
                                        "OPCSNPM2.5LCTOT",
                                        "SamariumPM2.5LC",
                                        "ScandiumPM2.5LC",
                                        "TantalumPM2.5LC",	
                                        "TerbiumPM2.5LC",
                                        "TungstenPM2.5LC",
                                        "YttriumPM2.5LC")
Parklane_metals1 <- Parklane_metals[!myvars]

str(data1$DATE)
data1$DATE<-as.Date(data1$DATE, format = "%d-%b-%y")

######
#merge data sets by location

#Start with Parklane (Middle)
data1$Run_Order<-NULL

middle <- subset(data1, Location == "Middle")
north <- subset(data1, Location == "Northern")
south <- subset(data1, Location == "Southern")

#data<-merge(data1, Parklane_metals, by ="DATE")

#merge each site to Parklane metals

north.data<-merge(north, Parklane_metals1, by ="DATE")
middle.data<-merge(middle, Parklane_metals1, by ="DATE")
south.data<-merge(south, Parklane_metals1, by ="DATE")

north.data$Location<-NULL
middle.data$Location<-NULL
south.data$Location<-NULL

north.data$DATE<-NULL
middle.data$DATE<-NULL
south.data$DATE<-NULL

write.csv(north.data, "C:/Users/scommod/Desktop/SCAFs/north_cor.csv", row.names=FALSE)
write.csv(middle.data, "C:/Users/scommod/Desktop/SCAFs/middle_cor.csv", row.names=FALSE)
write.csv(south.data, "C:/Users/scommod/Desktop/SCAFs/south_cor.csv", row.names=FALSE)



#correlations

#############################
#Correlations between Parklane and LA Method

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")

library("ggpubr")

#1 Magnesium
a<-ggscatter(middle.data, x = "MagnesiumPM2.5LC", y = "Mg24", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "Magnesium from CSN", ylab = "Magnesium")
type='lower'

#2 Aluminum
b<-ggscatter(middle.data, x = "AluminumPM2.5LC", y = "Al27", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "Aluminum from CSN", ylab = "Aluminum")
type='lower'

#3 Silicon
c<-ggscatter(middle.data, x = "SiliconPM2.5LC", y = "Si29", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "Silicon from CSN", ylab = "Silicon")
type='lower'

#4 Phosphorus (0 value for all CSN data)

#5 Calcium 43
ggscatter(middle.data, x = "CalciumPM2.5LC", y = "Ca43", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Calcium from CSN", ylab = "Calcium-43")
type='lower'

#6 Calcium 44 (most significant isotope)
d<-ggscatter(middle.data, x = "CalciumPM2.5LC", y = "Ca44", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "Calcium from CSN", ylab = "Calcium-44")
type='lower'

#create a variable called totalca
middle.data$totalca <-middle.data$Ca44 + middle.data$Ca43

#6 Total calcium
ggscatter(middle.data, x = "CalciumPM2.5LC", y = "totalca", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Calcium from CSN", ylab = "Total Calcium")
type='lower'

#7 Titanium
e<-ggscatter(middle.data, x = "TitaniumPM2.5LC", y = "Ti47", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "Titanium from CSN", ylab = "Titanium")
type='lower'

#8 Vanadium (marginally significant)
f<-ggscatter(middle.data, x = "VanadiumPM2.5LC", y = "V51", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "Vanadium from CSN", ylab = "Vanadium")
type='lower'

#9 Chromium (marginally significant)
g<-ggscatter(middle.data, x = "ChromiumPM2.5LC", y = "Cr52", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "Chromium from CSN", ylab = "Chromium")
type='lower'

#10 Manganese 
h<-ggscatter(middle.data, x = "ManganesePM2.5LC", y = "Mn55", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "Manganese from CSN", ylab = "Manganese")
type='lower'

#11 Iron 
i<-ggscatter(middle.data, x = "IronPM2.5LC", y = "Fe56", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "Iron from CSN", ylab = "Iron")
type='lower'

#12 Cobalt (not significant)
ggscatter(middle.data, x = "CobaltPM2.5LC", y = "Co59", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Cobalt from CSN", ylab = "Cobalt")
type='lower'

#13 Nickel (not significant)
ggscatter(middle.data, x = "NickelPM2.5LC", y = "Ni60", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Nickel from CSN", ylab = "Nickel")
type='lower'

#14 Copper (not significant)
ggscatter(middle.data, x = "CopperPM2.5LC", y = "Cu63", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Copper from CSN", ylab = "Copper")
type='lower'

#15 Zinc a (not significant)
ggscatter(middle.data, x = "ZincPM2.5LC", y = "Zn64", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Zinc from CSN", ylab = "Zinc")
type='lower'

#16 Zinc b (marginally significant)
j<-ggscatter(middle.data, x = "ZincPM2.5LC", y = "Zn66", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "Zinc from CSN", ylab = "Zinc-66")
type='lower'

#create a variable called totalZinc
middle.data$totalzinc <-middle.data$Zn64 + middle.data$Zn66

# Zinc total Zinc (not significant)
ggscatter(middle.data, x = "ZincPM2.5LC", y = "totalzinc", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Zinc from CSN", ylab = "Zinc")
type='lower'

#18 Arsenic

k<-ggscatter(middle.data, x = "ArsenicPM2.5LC", y = "As75", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "Arsenic from CSN", ylab = "Arsenic")
type='lower'

#19 Selenium (not significant)

ggscatter(middle.data, x = "SeleniumPM2.5LC", y = "Se78", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Selenium from CSN", ylab = "Selenium")
type='lower'


#20 Rubidium (nonsignificant)

ggscatter(middle.data, x = "RubidiumPM2.5LC", y = "Rb85", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Rubidium from CSN", ylab = "Rubidium")
type='lower'

#21 Strontium (marginally significant)

l<-ggscatter(middle.data, x = "StrontiumPM2.5LC", y = "Sr88", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "Strontium from CSN", ylab = "Strontium")
type='lower'


#22 Molybdenum #No data on MolybdenumPM2.5LC

ggscatter(middle.data, x = "MolybdenumPM2.5LC", y = "Mo95", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Molybdenum from CSN", ylab = "Molybdenum")
type='lower'

#23 Cadmium (not significant)

ggscatter(middle.data, x = "CadmiumPM2.5LC", y = "Cd111", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Cadmium from CSN", ylab = "Cadmium")
type='lower'

#24 Antimony (not significant)

ggscatter(middle.data, x = "AntimonyPM2.5LC", y = "Sb121", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Antimony from CSN", ylab = "Antimony")
type='lower'


#25 Cesium (marginally significant)

m<-ggscatter(middle.data, x = "CesiumPM2.5LC", y = "Cs133", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "Cesium from CSN", ylab = "Cesium")
type='lower'


#26 Barium (not significant)

ggscatter(middle.data, x = "BariumPM2.5LC", y = "Ba137", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Barium from CSN", ylab = "Barium")
type='lower'

ggscatter(middle.data, x = "BariumPM2.5LC", y = "Ba138", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Barium from CSN", ylab = "Barium")
type='lower'

#create a variable called totalba (not much variation in Ba measurements)
middle.data$totalba <-middle.data$Ba137 + middle.data$Ba137


ggscatter(middle.data, x = "BariumPM2.5LC", y = "totalba", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Barium from CSN", ylab = "Barium")
type='lower'

#27 Mercury (No data)

ggscatter(middle.data, x = "MercuryPM2.5LC", y = "Hg202", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Mercury from CSN", ylab = "Mercury")
type='lower'

str(middle.data$MercuryPM2.5LC)

#28 Lead (not significant)

ggscatter(middle.data, x = "LeadPM2.5LC", y = "Pb208", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Lead from CSN", ylab = "Lead")
type='lower'

#29 Bismuth (no data)
ggscatter(middle.data, x = "BismuthPM2.5LC", y = "Bi209", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Bismuth from CSN", ylab = "Bismuth")
type='lower'

str(middle.data$BismuthPM2.5LC)

#30 Bromine
n<-ggscatter(middle.data, x = "BrominePM2.5LC", y = "Br79", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "Bromine from CSN", ylab = "Bromine")
type='lower'


#31 Tin (not significant, not much variation in CSN data)
ggscatter(middle.data, x = "TinPM2.5LC", y = "Sn118", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Tin from CSN", ylab = "Tin")
type='lower'

#########################################################

#Final graph for comparing 14 CSN elements and LA method

#1 Magnesium #molecular weight: 24.305
middle.data$Mg_ppm<-(24.45*middle.data$MagnesiumPM2.5LC/24.305)*1000
a<-ggscatter(middle.data, x = "Mg_ppm", y = "Mg24", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",title = "Spearman correlation",
             xlab = "Magnesium from CSN, mg/kg", ylab = "Magnesium, mg/kg")
type='lower'

t1<-ggscatter(middle.data, x = "Mg_ppm", y = "Mg24", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman",title = "Spearman correlation",
              xlab = "Magnesium from CSN, mg/kg", ylab = "Magnesium, mg/kg")
type='lower'

t2<-ggscatter(middle.data, x = "MagnesiumPM2.5LC", y = "Mg24", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "spearman", title = "Spearman correlation",
              xlab = "Magnesium from CSN, ug/m3", ylab = "Magnesium, mg/kg")
type='lower'

t3<-ggscatter(middle.data, x = "MagnesiumPM2.5LC", y = "Mg24", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson",title = "Pearson correlation",
              xlab = "Magnesium from CSN, ug/m3", ylab = "Magnesium, mg/kg")
type='lower'

t4<-ggscatter(middle.data, x = "Mg_ppm", y = "Mg24", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson",title = "Pearson correlation",
              xlab = "Magnesium from CSN, mg/kg", ylab = "Magnesium, mg/kg")
type='lower'

ggarrange(t1,t2,t3,t4, 
          labels = c("i", "ii", "iii", "iv"),
          hjust=-0.8,
          ncol = 2, nrow = 2, common.legend = TRUE, legend = "right")

#a+ scale_x_continuous(expand = c(0, 0), limits = c(0,NA)) 

#2 Aluminum#molecular weight: 26.981539
b<-ggscatter(middle.data, x = "AluminumPM2.5LC", y = "Al27", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "Aluminum from CSN", ylab = "Aluminum")
type='lower'

#3 Silicon#molecular weight: 28.0855
c<-ggscatter(middle.data, x = "SiliconPM2.5LC", y = "Si29", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "Silicon from CSN", ylab = "Silicon")
type='lower'

#4 Phosphorus (0 value for all CSN data)#molecular weight:30.973762

#5 Calcium 43#molecular weight: 40.078
d<-ggscatter(middle.data, x = "CalciumPM2.5LC", y = "Ca43", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "Calcium from CSN", ylab = "Calcium-43")
type='lower'

#6 Calcium 44 (most significant isotope)#molecular weight: 40.078
e<-ggscatter(middle.data, x = "CalciumPM2.5LC", y = "Ca44", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "Calcium from CSN", ylab = "Calcium-44")
type='lower'



#7 Titanium#molecular weight: 47.867
f<-ggscatter(middle.data, x = "TitaniumPM2.5LC", y = "Ti47", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "Titanium from CSN", ylab = "Titanium")
type='lower'

#8 Vanadium (marginally significant)#molecular weight: 50.9415
g<-ggscatter(middle.data, x = "VanadiumPM2.5LC", y = "V51", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "Vanadium from CSN", ylab = "Vanadium")
type='lower'

#9 Chromium (marginally significant)#molecular weight: 51.9961
h<-ggscatter(middle.data, x = "ChromiumPM2.5LC", y = "Cr52", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "Chromium from CSN", ylab = "Chromium")
type='lower'

#10 Manganese#molecular weight: 54.938044 
i<-ggscatter(middle.data, x = "ManganesePM2.5LC", y = "Mn55", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "Manganese from CSN", ylab = "Manganese")
type='lower'

#11 Iron #molecular weight: 55.845
j<-ggscatter(middle.data, x = "IronPM2.5LC", y = "Fe56", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "Iron from CSN", ylab = "Iron")
type='lower'



#16 Zinc b (marginally significant) #molecular weight: 65.38
k<-ggscatter(middle.data, x = "ZincPM2.5LC", y = "Zn66", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "Zinc from CSN", ylab = "Zinc-66")
type='lower'



#18 Arsenic #molecular weight: 74.9216

l<-ggscatter(middle.data, x = "ArsenicPM2.5LC", y = "As75", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "Arsenic from CSN", ylab = "Arsenic")
type='lower'


#21 Strontium (marginally significant) #molecular weight: 87.62

m<-ggscatter(middle.data, x = "StrontiumPM2.5LC", y = "Sr88", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "Strontium from CSN", ylab = "Strontium")
type='lower'



#25 Cesium (marginally significant) #molecular weight: 132.90545

n<-ggscatter(middle.data, x = "CesiumPM2.5LC", y = "Cs133", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "Cesium from CSN", ylab = "Cesium")
type='lower'



#30 Bromine #molecular weight: 79.904
o<-ggscatter(middle.data, x = "BrominePM2.5LC", y = "Br79", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "Bromine from CSN", ylab = "Bromine")
type='lower'

#31 Sulfur #molecular weight: 32.065
p<-ggscatter(middle.data, x = "SulfurPM2.5LC", y = "S34", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "spearman",
             xlab = "Sulfur from CSN", ylab = "Sulfur")
type='lower'

############
#plots on same page
dev.new(width=5, height=7)

#install.packages("gridExtra")
library("gridExtra")

grid.arrange(a,b,c,d,e,f,g,h,i,j,k,l,m,o, ncol=7)

ggarrange(a,b,c,d,e,f,g,h,i,j,k,l,m,o, 
          labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N"),
          hjust=-0.8,
          ncol = 2, nrow = 7, common.legend = TRUE, legend = "right")

#alphabetical order of elements         
ggarrange(b,l,o,d,e,h,j,a,i,c,m,f,g,k, 
          labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N"),
          hjust=-0.8,
          ncol = 7, nrow = 2, common.legend = TRUE, legend = "right")
#1800 width will work

#New Fig
#########################
#alphabetical order of elements         
ggarrange(b,l,o,j,a,i,c,p, 
          labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
          hjust=-0.8,
          ncol = 2, nrow = 4, common.legend = TRUE, legend = "right")
#1800 width will work

ggarrange(b,l,o,j,a,i,c,p, 
          labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
          hjust=-0.8,
          ncol = 4, nrow = 2, common.legend = TRUE, legend = "right")
#1800 width will work