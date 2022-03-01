#################
# Ending the HIV epidemic jurisdiction analysis
# Citation: Webster JL, Thorpe LE, Duncan DT, Gracely EJ, Goldstein ND. Alignment of ending the HIV epidemic priority jurisdictions with availability of HIV service organizations, 2019. Manuscript in preparation.
# 8/26/21 -- Neal Goldstein
#################


### FUNCTIONS ###

library("rgdal") #readOGR
library("sp") #shapefile
library("RColorBrewer") #color palette
library("GISTools") #scale and compass
library("raster") #area function
library(tidycensus) #retrieve ACS data, note if error installing on MacOS see: https://github.com/r-quantities/units/issues/1


### READ DATA ###

#state and county cartographic boundaries: https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
county_carto = readOGR("cb_2018_us_county_500k", "cb_2018_us_county_500k")
state_carto = readOGR("cb_2018_us_state_20m", "cb_2018_us_state_20m")

#HIV data for EHE jurisdictions: https://www.cdc.gov/nchhstp/atlas/index.htm
ehe_counties = read.csv("AtlasPlusTableData HIV EHE counties.csv", as.is=T, skip=14, na.strings=c("NA","Data not available","Data suppressed"), stringsAsFactors=F)
ehe_states = read.csv("AtlasPlusTableData HIV EHE states.csv", as.is=T, skip=14, na.strings=c("NA","Data not available","Data suppressed"), stringsAsFactors=F)

#CDC NPIN organizatons: https://npin.cdc.gov/search/organizations/map
orgs = read.csv("organizations_by_location--2021_7_5.csv", as.is=T, stringsAsFactors=F)

#census population data
census_api_key("paste api key here") #obtain key from: http://api.census.gov/data/key_signup.html
population_state = get_decennial(geography="state", table="P001", year=2010)
population_county = get_decennial(geography="county", table="P001", year=2010)


### PREPARE SPATIAL DATA ###

#border map: 48 contiguous U.S. states (HI 15, AK 02) + PR
us_carto = state_carto[as.numeric(as.character(state_carto$STATEFP)) %in% c(1,4,5,6,8,9,10,12,13,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,44,45,46,47,48,49,50,51,53,54,55,56,72), ]

#retain only EHE priority counties
county_carto = county_carto[as.numeric(paste(as.character(county_carto$STATEFP),as.character(county_carto$COUNTYFP),sep="")) %in% as.numeric(unique(ehe_counties$FIPS)), ]
county_carto$AREA = area(county_carto) / 1000000 #area in sq km

#retain only EHE priority states
state_carto = state_carto[as.numeric(as.character(state_carto$STATEFP)) %in% as.numeric(unique(ehe_states$FIPS)), ]
state_carto$AREA = area(state_carto) / 1000000 #area in sq km

#set projected coordinate system for U.S.
us_carto_proj = spTransform(us_carto,CRS("+init=epsg:2163"))
state_carto_proj = spTransform(state_carto,CRS("+init=epsg:2163"))
county_carto_proj = spTransform(county_carto,CRS("+init=epsg:2163"))

# #need to transform AK, HI to fit under U.S. for single map; see https://stackoverflow.com/questions/13757771/relocating-alaska-and-hawaii-on-thematic-map-of-the-usa-with-ggplot2
# fixup <- function(usa,alaskaFix,hawaiiFix){
#   
#   alaska=usa[usa$STATEFP=="02",]
#   alaska = fix1(alaska,alaskaFix)
#   proj4string(alaska) <- proj4string(usa)
#   
#   hawaii = usa[usa$STATEFP=="15",]
#   hawaii = fix1(hawaii,hawaiiFix)
#   proj4string(hawaii) <- proj4string(usa)
#   
#   usa = usa[! usa$STATEFP %in% c("02","15"),]
#   usa = rbind(usa,alaska,hawaii)
#   
#   return(usa)
#   
# }
# 
# fix1 <- function(object,params){
#   r=params[1];scale=params[2];shift=params[3:4]
#   object = elide(object,rotate=r)
#   size = max(apply(bbox(object),1,diff))/scale
#   object = elide(object,scale=size)
#   object = elide(object,shift=shift)
#   object
# }
# 
# us_map = fixup(us_carto_proj,c(-35,2,-2500000,-2500000),c(-35,1,5500000,-1600000))
# rm(fix1,fixup,us_carto,us_carto_proj)


### PREPARE METRICS DATA ###

#organizations
orgs_hiv_test = orgs[grep("HIV", orgs$field_org_svc_testing, ignore.case=F), ]
orgs_hiv_prev = orgs[grep("HIV", orgs$field_org_svc_prevention, ignore.case=F), ]
orgs_hiv_care = orgs[grep("HIV", orgs$field_org_svc_care, ignore.case=F), ]
orgs_hiv_prep = orgs[grep("PrEP", orgs$field_org_svc_care, ignore.case=F), ]

#merge to create various indicators: prevention (test, prev, prep), treatment (care), all
orgs_hiv_prevention = rbind(orgs_hiv_test, orgs_hiv_prev[is.na(match(orgs_hiv_prev$field_org_nid, orgs_hiv_test$field_org_nid)), ])
orgs_hiv_prevention = rbind(orgs_hiv_prevention, orgs_hiv_prep[is.na(match(orgs_hiv_prep$field_org_nid, orgs_hiv_prevention$field_org_nid)), ])
orgs_hiv_treatment = orgs_hiv_care
orgs_hiv_all = rbind(orgs_hiv_prevention, orgs_hiv_treatment[is.na(match(orgs_hiv_treatment$field_org_nid, orgs_hiv_prevention$field_org_nid)), ])
rm(orgs_hiv_test, orgs_hiv_prev, orgs_hiv_care, orgs_hiv_prep, orgs)

#assemble metrics data
ehe_data = data.frame("FIPS"=c(unique(ehe_counties$FIPS), unique(ehe_states$FIPS)), "Geography"=c(rep("County", length(unique(ehe_counties$FIPS))), rep("State", length(unique(ehe_states$FIPS)))), "Name"=NA, "Population"=NA, "Area"=NA, "HIV_prevalence"=NA, "HIV_diagnoses"=NA, "HIV_suppressed_percent"=NA, "HIV_prep_percent"=NA, "HIV_linked_percent"=NA, "HIV_status_percent"=NA, "Org_HIV_prevention"=NA, "Org_HIV_treatment"=NA, "Org_HIV_any"=NA, stringsAsFactors=F)

for (i in 1:nrow(ehe_data)) {
  
  ehe_data$Name[i] = ifelse(ehe_data$Geography[i]=="County", ehe_counties$Geography[ehe_counties$FIPS==ehe_data$FIPS[i]][1],
                            ehe_states$Geography[ehe_states$FIPS==ehe_data$FIPS[i]][1])
  
  ehe_data$Population[i] = ifelse(ehe_data$Geography[i]=="County", population_county$value[as.numeric(population_county$GEOID)==ehe_data$FIPS[i]],
                                  population_state$value[as.numeric(population_state$GEOID)==ehe_data$FIPS[i]])
  
  ehe_data$Area[i] = ifelse(ehe_data$Geography[i]=="County", county_carto$AREA[as.numeric(paste(as.character(county_carto$STATEFP),as.character(county_carto$COUNTYFP),sep=""))==ehe_data$FIPS[i]],
                            state_carto$AREA[as.numeric(as.character(state_carto$STATEFP))==ehe_data$FIPS[i]])
  
  ehe_data$HIV_prevalence[i] = ifelse(ehe_data$Geography[i]=="County", as.numeric(sub(",", "", ehe_counties$Cases..95..CI.RSE.[ehe_counties$FIPS==ehe_data$FIPS[i] & ehe_counties$Indicator=="HIV prevalence" & ehe_counties$Year==2019])),
                                      as.numeric(sub(",", "", ehe_states$Cases..95..CI.RSE.[ehe_states$FIPS==ehe_data$FIPS[i] & ehe_states$Indicator=="HIV prevalence" & ehe_states$Year==2019])))
  
  ehe_data$HIV_diagnoses[i] = ifelse(ehe_data$Geography[i]=="County", as.numeric(sub(",", "", ehe_counties$Cases..95..CI.RSE.[ehe_counties$FIPS==ehe_data$FIPS[i] & ehe_counties$Indicator=="HIV diagnoses" & ehe_counties$Year==2019])),
                                     as.numeric(sub(",", "", ehe_states$Cases..95..CI.RSE.[ehe_states$FIPS==ehe_data$FIPS[i] & ehe_states$Indicator=="HIV diagnoses" & ehe_states$Year==2019])))
  
  ehe_data$HIV_suppressed_percent[i] = ifelse(ehe_data$Geography[i]=="County", as.numeric(ehe_counties$Percent..95..CI.RSE.[ehe_counties$FIPS==ehe_data$FIPS[i] & ehe_counties$Indicator=="HIV viral suppression" & ehe_counties$Year==2019]),
                                              as.numeric(ehe_states$Percent..95..CI.RSE.[ehe_states$FIPS==ehe_data$FIPS[i] & ehe_states$Indicator=="HIV viral suppression" & ehe_states$Year==2019]))
  
  ehe_data$HIV_prep_percent[i] = ifelse(ehe_data$Geography[i]=="County", as.numeric(ehe_counties$Percent..95..CI.RSE.[ehe_counties$FIPS==ehe_data$FIPS[i] & ehe_counties$Indicator=="PrEP coverage and number of persons prescribed" & ehe_counties$Year==2019]),
                                        as.numeric(ehe_states$Percent..95..CI.RSE.[ehe_states$FIPS==ehe_data$FIPS[i] & ehe_states$Indicator=="PrEP coverage and number of persons prescribed" & ehe_states$Year==2019]))
  
  ehe_data$HIV_linked_percent[i] = ifelse(ehe_data$Geography[i]=="County", as.numeric(ehe_counties$Percent..95..CI.RSE.[ehe_counties$FIPS==ehe_data$FIPS[i] & ehe_counties$Indicator=="Linkage to HIV care" & ehe_counties$Year==2019]),
                                          as.numeric(ehe_states$Percent..95..CI.RSE.[ehe_states$FIPS==ehe_data$FIPS[i] & ehe_states$Indicator=="Linkage to HIV care" & ehe_states$Year==2019]))
  
  ehe_data$HIV_status_percent[i] = ifelse(ehe_data$Geography[i]=="County", as.numeric(strsplit(ehe_counties$Percent..95..CI.RSE.[ehe_counties$FIPS==ehe_data$FIPS[i] & ehe_counties$Indicator=="Knowledge of Status" & ehe_counties$Year==2019], " (", fixed=T)[[1]][1]),
                                          as.numeric(strsplit(ehe_states$Percent..95..CI.RSE.[ehe_states$FIPS==ehe_data$FIPS[i] & ehe_states$Indicator=="Knowledge of Status" & ehe_states$Year==2019], " (", fixed=T)[[1]][1]))
  
  ehe_data$Org_HIV_prevention[i] = ifelse(ehe_data$Geography[i]=="County", length(grep(sub(" County", "", ehe_data$Name[i]), paste(orgs_hiv_prevention$field_org_county, orgs_hiv_prevention$field_org_state, sep=", "))), 
                                          nrow(orgs_hiv_prevention[orgs_hiv_prevention$field_org_state==state.abb[match(ehe_data$Name[i],state.name)], ]))
  
  ehe_data$Org_HIV_treatment[i] = ifelse(ehe_data$Geography[i]=="County", length(grep(sub(" County", "", ehe_data$Name[i]), paste(orgs_hiv_treatment$field_org_county, orgs_hiv_treatment$field_org_state, sep=", "))), 
                                         nrow(orgs_hiv_treatment[orgs_hiv_treatment$field_org_state==state.abb[match(ehe_data$Name[i],state.name)], ]))
  
  ehe_data$Org_HIV_any[i] = ifelse(ehe_data$Geography[i]=="County", length(grep(sub(" County", "", ehe_data$Name[i]), paste(orgs_hiv_all$field_org_county, orgs_hiv_all$field_org_state, sep=", "))), 
                                   nrow(orgs_hiv_all[orgs_hiv_all$field_org_state==state.abb[match(ehe_data$Name[i],state.name)], ]))
}
rm(i, ehe_counties, ehe_states, orgs_hiv_all, orgs_hiv_prevention, orgs_hiv_treatment, population_county, population_state)

#save data
save.image("ehe_data.RData")


### ANALYSIS ###

#load data
load("ehe_data.RData")

#geographical area indicator
ehe_data$Geography_cat = ifelse(ehe_data$Geography=="State", 2, ifelse(ehe_data$Area<median(ehe_data$Area[ehe_data$Geography=="County"]), 0, 1))

#denominators
ehe_data$Org_HIV_prevention = (ehe_data$Org_HIV_prevention/ehe_data$Population*100000)
ehe_data$Org_HIV_treatment = (ehe_data$Org_HIV_treatment/ehe_data$Population*100000)
ehe_data$Org_HIV_any = (ehe_data$Org_HIV_any/ehe_data$Population*100000)
ehe_data$HIV_prevalence = (ehe_data$HIV_prevalence/ehe_data$Population*1000)
ehe_data$HIV_diagnoses = (ehe_data$HIV_diagnoses/ehe_data$Population*1000)

#plot preferences for geography
plot_sym = c(1,1,2)
plot_siz = c(1,1.5,1)

#STATUS by prevention facilities
pdf("Figure 1A.pdf", paper="letter")
plot(ehe_data$HIV_status_percent, ehe_data$Org_HIV_prevention, pch=plot_sym[ehe_data$Geography_cat+1], cex=plot_siz[ehe_data$Geography_cat+1], xlab="Percent aware of positive HIV status", ylab="Organizations per 100,000")
par(adj=0)
title("A)")
par(adj=0.5)
par(xpd=T)
legend("top", legend=c("Smaller county", "Larger county", "State"), pch=plot_sym, pt.cex=plot_siz, cex=0.9, title="Size (area) of jurisdiction", horiz=T, inset=-0.1, bg="white")
par(xpd=F)

#draw regression line and standard error of estimate; see: https://www.sciencedirect.com/topics/mathematics/standard-error-of-estimate
model = lm(Org_HIV_prevention ~ HIV_status_percent + as.factor(Geography_cat), data=ehe_data)
summary(model)
abline(a=coef(model)[1], b=coef(model)[2])
#abline(a=confint(model)[1], b=confint(model)[2], lty=2)
#abline(a=confint(model)[5], b=confint(model)[6], lty=2)
#polygon(x=c(par('usr')[1],par('usr')[2],par('usr')[2],par('usr')[1]), y=c((par('usr')[1]*confint(model)[6]+confint(model)[5]),(par('usr')[2]*confint(model)[6]+confint(model)[5]),(par('usr')[2]*confint(model)[2]+confint(model)[1]),(par('usr')[1]*confint(model)[2]+confint(model)[1])), col=adjustcolor("grey",alpha.f=0.1), border=NA)
Se = sd(ehe_data$Org_HIV_prevention)*sqrt((1-summary(model)$r.squared)*((nrow(ehe_data)-1)/(nrow(ehe_data)-2)))
abline(a=(coef(model)[1]+(2*Se)), b=coef(model)[2], lty=2)
abline(a=(coef(model)[1]-(2*Se)), b=coef(model)[2], lty=2)
polygon(x=c(par('usr')[1],par('usr')[2],par('usr')[2],par('usr')[1]), y=c((par('usr')[1]*coef(model)[2]+(coef(model)[1]-(2*Se))),(par('usr')[2]*coef(model)[2]+(coef(model)[1]-(2*Se))),(par('usr')[2]*coef(model)[2]+(coef(model)[1]+(2*Se))),(par('usr')[1]*coef(model)[2]+(coef(model)[1]+(2*Se)))), col=adjustcolor("grey",alpha.f=0.1), border=NA)
dev.off()

#determine if any jurisdictions fall outside 2*Se
#text(ehe_data$HIV_prep_percent, ehe_data$Org_HIV_prevention, ehe_data$Name)
lower2Se = approxfun(x=c(par('usr')[1],par('usr')[2]), y=c((par('usr')[1]*coef(model)[2]+(coef(model)[1]-(2*Se))),(par('usr')[2]*coef(model)[2]+(coef(model)[1]-(2*Se)))))
upper2Se = approxfun(x=c(par('usr')[2],par('usr')[1]), y=c((par('usr')[2]*coef(model)[2]+(coef(model)[1]+(2*Se))),(par('usr')[1]*coef(model)[2]+(coef(model)[1]+(2*Se)))))
ehe_data$Name[which(lower2Se(ehe_data$HIV_status_percent) > ehe_data$Org_HIV_prevention)]
ehe_data$Name[which(upper2Se(ehe_data$HIV_status_percent) < ehe_data$Org_HIV_prevention)]
rm(lower2Se,upper2Se)

#PREP by prevention facilities
pdf("Figure 1B.pdf", paper="letter")
plot(ehe_data$HIV_prep_percent, ehe_data$Org_HIV_prevention, pch=plot_sym[ehe_data$Geography_cat+1], cex=plot_siz[ehe_data$Geography_cat+1], xlab="Percent prescribed PrEP", ylab="Organizations per 100,000")
par(adj=0)
title("B)")
par(adj=0.5)
par(xpd=T)
legend("top", legend=c("Smaller county", "Larger county", "State"), pch=plot_sym, pt.cex=plot_siz, cex=0.9, title="Size (area) of jurisdiction", horiz=T, inset=-0.1, bg="white")
par(xpd=F)

#draw regression line and standard error of estimate; see: https://www.sciencedirect.com/topics/mathematics/standard-error-of-estimate
model = lm(Org_HIV_prevention ~ HIV_prep_percent + as.factor(Geography_cat), data=ehe_data)
summary(model)
abline(a=coef(model)[1], b=coef(model)[2])
#abline(a=confint(model)[1], b=confint(model)[2], lty=2)
#abline(a=confint(model)[5], b=confint(model)[6], lty=2)
#polygon(x=c(par('usr')[1],par('usr')[2],par('usr')[2],par('usr')[1]), y=c((par('usr')[1]*confint(model)[6]+confint(model)[5]),(par('usr')[2]*confint(model)[6]+confint(model)[5]),(par('usr')[2]*confint(model)[2]+confint(model)[1]),(par('usr')[1]*confint(model)[2]+confint(model)[1])), col=adjustcolor("grey",alpha.f=0.1), border=NA)
Se = sd(ehe_data$Org_HIV_prevention)*sqrt((1-summary(model)$r.squared)*((nrow(ehe_data)-1)/(nrow(ehe_data)-2)))
abline(a=(coef(model)[1]+(2*Se)), b=coef(model)[2], lty=2)
abline(a=(coef(model)[1]-(2*Se)), b=coef(model)[2], lty=2)
polygon(x=c(par('usr')[1],par('usr')[2],par('usr')[2],par('usr')[1]), y=c((par('usr')[1]*coef(model)[2]+(coef(model)[1]-(2*Se))),(par('usr')[2]*coef(model)[2]+(coef(model)[1]-(2*Se))),(par('usr')[2]*coef(model)[2]+(coef(model)[1]+(2*Se))),(par('usr')[1]*coef(model)[2]+(coef(model)[1]+(2*Se)))), col=adjustcolor("grey",alpha.f=0.1), border=NA)
dev.off()

#determine if any jurisdictions fall outside 2*Se
#text(ehe_data$HIV_prep_percent, ehe_data$Org_HIV_prevention, ehe_data$Name)
lower2Se = approxfun(x=c(par('usr')[1],par('usr')[2]), y=c((par('usr')[1]*coef(model)[2]+(coef(model)[1]-(2*Se))),(par('usr')[2]*coef(model)[2]+(coef(model)[1]-(2*Se)))))
upper2Se = approxfun(x=c(par('usr')[2],par('usr')[1]), y=c((par('usr')[2]*coef(model)[2]+(coef(model)[1]+(2*Se))),(par('usr')[1]*coef(model)[2]+(coef(model)[1]+(2*Se)))))
ehe_data$Name[which(lower2Se(ehe_data$HIV_prep_percent) > ehe_data$Org_HIV_prevention)]
ehe_data$Name[which(upper2Se(ehe_data$HIV_prep_percent) < ehe_data$Org_HIV_prevention)]
rm(lower2Se,upper2Se)

#PREVALENCE by treatment facilities
pdf("Figure 1C.pdf", paper="letter")
plot(ehe_data$HIV_prevalence, ehe_data$Org_HIV_treatment, pch=plot_sym[ehe_data$Geography_cat+1], cex=plot_siz[ehe_data$Geography_cat+1], xlab="HIV prevalence per 1,000 people", ylab="Organizations per 100,000")
par(adj=0)
title("C)")
par(adj=0.5)
par(xpd=T)
legend("top", legend=c("Smaller county", "Larger county", "State"), pch=plot_sym, pt.cex=plot_siz, cex=0.9, title="Size (area) of jurisdiction", horiz=T, inset=-0.1, bg="white")
par(xpd=F)

#draw regression line and standard error of estimate; see: https://www.sciencedirect.com/topics/mathematics/standard-error-of-estimate
model = lm(Org_HIV_treatment ~ HIV_prevalence + as.factor(Geography_cat), data=ehe_data)
summary(model)
abline(a=coef(model)[1], b=coef(model)[2])
#abline(a=confint(model)[1], b=confint(model)[2], lty=2)
#abline(a=confint(model)[5], b=confint(model)[6], lty=2)
#polygon(x=c(par('usr')[1],par('usr')[2],par('usr')[2],par('usr')[1]), y=c((par('usr')[1]*confint(model)[6]+confint(model)[5]),(par('usr')[2]*confint(model)[6]+confint(model)[5]),(par('usr')[2]*confint(model)[2]+confint(model)[1]),(par('usr')[1]*confint(model)[2]+confint(model)[1])), col=adjustcolor("grey",alpha.f=0.1), border=NA)
Se = sd(ehe_data$Org_HIV_treatment)*sqrt((1-summary(model)$r.squared)*((nrow(ehe_data)-1)/(nrow(ehe_data)-2)))
abline(a=(coef(model)[1]+(2*Se)), b=coef(model)[2], lty=2)
abline(a=(coef(model)[1]-(2*Se)), b=coef(model)[2], lty=2)
polygon(x=c(par('usr')[1],par('usr')[2],par('usr')[2],par('usr')[1]), y=c((par('usr')[1]*coef(model)[2]+(coef(model)[1]-(2*Se))),(par('usr')[2]*coef(model)[2]+(coef(model)[1]-(2*Se))),(par('usr')[2]*coef(model)[2]+(coef(model)[1]+(2*Se))),(par('usr')[1]*coef(model)[2]+(coef(model)[1]+(2*Se)))), col=adjustcolor("grey",alpha.f=0.1), border=NA)
dev.off()

#determine if any jurisdictions fall outside 2*Se
#text(ehe_data$HIV_prevalence, ehe_data$Org_HIV_treatment, ehe_data$Name)
lower2Se = approxfun(x=c(par('usr')[1],par('usr')[2]), y=c((par('usr')[1]*coef(model)[2]+(coef(model)[1]-(2*Se))),(par('usr')[2]*coef(model)[2]+(coef(model)[1]-(2*Se)))))
upper2Se = approxfun(x=c(par('usr')[2],par('usr')[1]), y=c((par('usr')[2]*coef(model)[2]+(coef(model)[1]+(2*Se))),(par('usr')[1]*coef(model)[2]+(coef(model)[1]+(2*Se)))))
ehe_data$Name[which(lower2Se(ehe_data$HIV_prevalence) > ehe_data$Org_HIV_treatment)]
ehe_data$Name[which(upper2Se(ehe_data$HIV_prevalence) < ehe_data$Org_HIV_treatment)]
rm(lower2Se,upper2Se)

#DIAGNOSES by treatment facilities
pdf("Figure 1D.pdf", paper="letter")
plot(ehe_data$HIV_diagnoses, ehe_data$Org_HIV_treatment, pch=plot_sym[ehe_data$Geography_cat+1], cex=plot_siz[ehe_data$Geography_cat+1], xlab="New HIV diagnoses per 1,000 people", ylab="Organizations per 100,000")
par(adj=0)
title("D)")
par(adj=0.5)
par(xpd=T)
legend("top", legend=c("Smaller county", "Larger county", "State"), pch=plot_sym, pt.cex=plot_siz, cex=0.9, title="Size (area) of jurisdiction", horiz=T, inset=-0.1, bg="white")
par(xpd=F)

#draw regression line and standard error of estimate; see: https://www.sciencedirect.com/topics/mathematics/standard-error-of-estimate
model = lm(Org_HIV_treatment ~ HIV_diagnoses + as.factor(Geography_cat), data=ehe_data)
summary(model)
abline(a=coef(model)[1], b=coef(model)[2])
#abline(a=confint(model)[1], b=confint(model)[2], lty=2)
#abline(a=confint(model)[5], b=confint(model)[6], lty=2)
#polygon(x=c(par('usr')[1],par('usr')[2],par('usr')[2],par('usr')[1]), y=c((par('usr')[1]*confint(model)[6]+confint(model)[5]),(par('usr')[2]*confint(model)[6]+confint(model)[5]),(par('usr')[2]*confint(model)[2]+confint(model)[1]),(par('usr')[1]*confint(model)[2]+confint(model)[1])), col=adjustcolor("grey",alpha.f=0.1), border=NA)
Se = sd(ehe_data$Org_HIV_treatment)*sqrt((1-summary(model)$r.squared)*((nrow(ehe_data)-1)/(nrow(ehe_data)-2)))
abline(a=(coef(model)[1]+(2*Se)), b=coef(model)[2], lty=2)
abline(a=(coef(model)[1]-(2*Se)), b=coef(model)[2], lty=2)
polygon(x=c(par('usr')[1],par('usr')[2],par('usr')[2],par('usr')[1]), y=c((par('usr')[1]*coef(model)[2]+(coef(model)[1]-(2*Se))),(par('usr')[2]*coef(model)[2]+(coef(model)[1]-(2*Se))),(par('usr')[2]*coef(model)[2]+(coef(model)[1]+(2*Se))),(par('usr')[1]*coef(model)[2]+(coef(model)[1]+(2*Se)))), col=adjustcolor("grey",alpha.f=0.1), border=NA)
dev.off()

#determine if any jurisdictions fall outside 2*Se
#text(ehe_data$HIV_prevalence, ehe_data$Org_HIV_treatment, ehe_data$Name)
lower2Se = approxfun(x=c(par('usr')[1],par('usr')[2]), y=c((par('usr')[1]*coef(model)[2]+(coef(model)[1]-(2*Se))),(par('usr')[2]*coef(model)[2]+(coef(model)[1]-(2*Se)))))
upper2Se = approxfun(x=c(par('usr')[2],par('usr')[1]), y=c((par('usr')[2]*coef(model)[2]+(coef(model)[1]+(2*Se))),(par('usr')[1]*coef(model)[2]+(coef(model)[1]+(2*Se)))))
ehe_data$Name[which(lower2Se(ehe_data$HIV_diagnoses) > ehe_data$Org_HIV_treatment)]
ehe_data$Name[which(upper2Se(ehe_data$HIV_diagnoses) < ehe_data$Org_HIV_treatment)]
rm(lower2Se,upper2Se)

#LINKED by treatment facilities
pdf("Figure 1E.pdf", paper="letter")
plot(ehe_data$HIV_linked_percent, ehe_data$Org_HIV_treatment, pch=plot_sym[ehe_data$Geography_cat+1], cex=plot_siz[ehe_data$Geography_cat+1], xlab="Percent linked to care", ylab="Organizations per 100,000")
par(adj=0)
title("E)")
par(adj=0.5)
par(xpd=T)
legend("top", legend=c("Smaller county", "Larger county", "State"), pch=plot_sym, pt.cex=plot_siz, cex=0.9, title="Size (area) of jurisdiction", horiz=T, inset=-0.1, bg="white")
par(xpd=F)

#draw regression line and standard error of estimate; see: https://www.sciencedirect.com/topics/mathematics/standard-error-of-estimate
model = lm(Org_HIV_treatment ~ HIV_linked_percent + as.factor(Geography_cat), data=ehe_data)
summary(model)
abline(a=coef(model)[1], b=coef(model)[2])
#abline(a=confint(model)[1], b=confint(model)[2], lty=2)
#abline(a=confint(model)[5], b=confint(model)[6], lty=2)
#polygon(x=c(par('usr')[1],par('usr')[2],par('usr')[2],par('usr')[1]), y=c((par('usr')[1]*confint(model)[6]+confint(model)[5]),(par('usr')[2]*confint(model)[6]+confint(model)[5]),(par('usr')[2]*confint(model)[2]+confint(model)[1]),(par('usr')[1]*confint(model)[2]+confint(model)[1])), col=adjustcolor("grey",alpha.f=0.1), border=NA)
Se = sd(ehe_data$Org_HIV_treatment)*sqrt((1-summary(model)$r.squared)*((nrow(ehe_data)-1)/(nrow(ehe_data)-2)))
abline(a=(coef(model)[1]+(2*Se)), b=coef(model)[2], lty=2)
abline(a=(coef(model)[1]-(2*Se)), b=coef(model)[2], lty=2)
polygon(x=c(par('usr')[1],par('usr')[2],par('usr')[2],par('usr')[1]), y=c((par('usr')[1]*coef(model)[2]+(coef(model)[1]-(2*Se))),(par('usr')[2]*coef(model)[2]+(coef(model)[1]-(2*Se))),(par('usr')[2]*coef(model)[2]+(coef(model)[1]+(2*Se))),(par('usr')[1]*coef(model)[2]+(coef(model)[1]+(2*Se)))), col=adjustcolor("grey",alpha.f=0.1), border=NA)
dev.off()

#determine if any jurisdictions fall outside 2*Se
#text(ehe_data$HIV_prevalence, ehe_data$Org_HIV_treatment, ehe_data$Name)
lower2Se = approxfun(x=c(par('usr')[1],par('usr')[2]), y=c((par('usr')[1]*coef(model)[2]+(coef(model)[1]-(2*Se))),(par('usr')[2]*coef(model)[2]+(coef(model)[1]-(2*Se)))))
upper2Se = approxfun(x=c(par('usr')[2],par('usr')[1]), y=c((par('usr')[2]*coef(model)[2]+(coef(model)[1]+(2*Se))),(par('usr')[1]*coef(model)[2]+(coef(model)[1]+(2*Se)))))
ehe_data$Name[which(lower2Se(ehe_data$HIV_linked_percent) > ehe_data$Org_HIV_treatment)]
ehe_data$Name[which(upper2Se(ehe_data$HIV_linked_percent) < ehe_data$Org_HIV_treatment)]
rm(lower2Se,upper2Se)

#SUPPRESSED by treatment facilities
pdf("Figure 1F.pdf", paper="letter")
plot(ehe_data$HIV_suppressed_percent, ehe_data$Org_HIV_treatment, pch=plot_sym[ehe_data$Geography_cat+1], cex=plot_siz[ehe_data$Geography_cat+1], xlab="Percent virally suppressed", ylab="Organizations per 100,000")
par(adj=0)
title("F)")
par(adj=0.5)
par(xpd=T)
legend("top", legend=c("Smaller county", "Larger county", "State"), pch=plot_sym, pt.cex=plot_siz, cex=0.9, title="Size (area) of jurisdiction", horiz=T, inset=-0.1, bg="white")
par(xpd=F)

#draw regression line and standard error of estimate; see: https://www.sciencedirect.com/topics/mathematics/standard-error-of-estimate
model = lm(Org_HIV_treatment ~ HIV_suppressed_percent + as.factor(Geography_cat), data=ehe_data)
summary(model)
abline(a=coef(model)[1], b=coef(model)[2])
#abline(a=confint(model)[1], b=confint(model)[2], lty=2)
#abline(a=confint(model)[5], b=confint(model)[6], lty=2)
#polygon(x=c(par('usr')[1],par('usr')[2],par('usr')[2],par('usr')[1]), y=c((par('usr')[1]*confint(model)[6]+confint(model)[5]),(par('usr')[2]*confint(model)[6]+confint(model)[5]),(par('usr')[2]*confint(model)[2]+confint(model)[1]),(par('usr')[1]*confint(model)[2]+confint(model)[1])), col=adjustcolor("grey",alpha.f=0.1), border=NA)
Se = sd(ehe_data$Org_HIV_treatment)*sqrt((1-summary(model)$r.squared)*((nrow(ehe_data)-1)/(nrow(ehe_data)-2)))
abline(a=(coef(model)[1]+(2*Se)), b=coef(model)[2], lty=2)
abline(a=(coef(model)[1]-(2*Se)), b=coef(model)[2], lty=2)
polygon(x=c(par('usr')[1],par('usr')[2],par('usr')[2],par('usr')[1]), y=c((par('usr')[1]*coef(model)[2]+(coef(model)[1]-(2*Se))),(par('usr')[2]*coef(model)[2]+(coef(model)[1]-(2*Se))),(par('usr')[2]*coef(model)[2]+(coef(model)[1]+(2*Se))),(par('usr')[1]*coef(model)[2]+(coef(model)[1]+(2*Se)))), col=adjustcolor("grey",alpha.f=0.1), border=NA)
dev.off()

#determine if any jurisdictions fall outside 2*Se
#text(ehe_data$HIV_prevalence, ehe_data$Org_HIV_treatment, ehe_data$Name)
lower2Se = approxfun(x=c(par('usr')[1],par('usr')[2]), y=c((par('usr')[1]*coef(model)[2]+(coef(model)[1]-(2*Se))),(par('usr')[2]*coef(model)[2]+(coef(model)[1]-(2*Se)))))
upper2Se = approxfun(x=c(par('usr')[2],par('usr')[1]), y=c((par('usr')[2]*coef(model)[2]+(coef(model)[1]+(2*Se))),(par('usr')[1]*coef(model)[2]+(coef(model)[1]+(2*Se)))))
ehe_data$Name[which(lower2Se(ehe_data$HIV_suppressed_percent) > ehe_data$Org_HIV_treatment)]
ehe_data$Name[which(upper2Se(ehe_data$HIV_suppressed_percent) < ehe_data$Org_HIV_treatment)]
rm(lower2Se,upper2Se)

