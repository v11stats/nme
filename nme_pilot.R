library(tidyverse); library(janitor)
library(readr); library(gt)
library(readxl); library(openxlsx)
setwd("~/git/nme")
Pilot <- read_excel("Pilot.xlsx")

Pilot <- Pilot %>% select(-`Primary Column`,-ParticipantOther, -CountyOther, # remove empty cols and Module responses
                 -SettingOther, - starts_with("Module"))
Pilot$rno <- 1:nrow(Pilot) # add unique column to track responses
Pilot <- Pilot %>% relocate(rno)
Qual <- Pilot %>% select(rno, FeelingsOther, MainReasonOther, RaceOther, OverallComments)
names(Qual)
names(Pilot)
# separate multi-selects into new db
vaccine <-Pilot %>% select(rno, Vaccine) %>% 
  separate_longer_delim(cols = 2, delim = " | ") 
rme <- Pilot %>% select(rno,nme= `Reason for NME`) %>%
  separate_longer_delim(cols = 2, delim = " | ")
access <- Pilot %>% select(rno, VacAccess) %>%
  separate_longer_delim(cols = 2, delim = " | ")
feelings <- Pilot %>% select(rno, VacFeelings) %>%
  separate_longer_delim(cols = 2, delim = " | ")
race <- Pilot %>% select(rno, `Race/Ethnicity`) %>%
  separate_longer_delim(cols = 2, delim = " | ")

tabyl(vaccine$Vaccine) %>% adorn_pct_formatting()

unique(rme$nme)
tabyl(rme$nme)

pLong <- Pilot %>% select(-Vaccine, -`Reason for NME`, - VacAccess, - VacFeelings, - `Race/Ethnicity`,
        -FeelingsOther, - MainReasonOther, - RaceOther, - OverallComments)
x <- full_join(pLong, vaccine, by="rno")

max(Pilot$`Survey Date`)
Pilot <- Pilot %>%
  mutate(date = ymd_hms(`Survey Date`))

as.numeric(difftime(max(Pilot$date), min(Pilot$date),units = 'weeks'))

x <- unique(Pilot$MainReason)
y <- c(T,T,F,T,F,T,F,T,F,NA,F,NA,T,F) # divide into beliefs or not
xx <-as.data.frame(cbind(x,y))
xx %>% arrange(y)
table(xx$y)
compare <- data.frame('MainReason'=x,'belief'=y)
Pilot <- left_join(Pilot,compare, by="MainReason")
Pilot$`#children`[which(Pilot$`#children`==-1)] <- 0

Pilot %>% tabyl(ParticipantType) %>% adorn_pct_formatting() %>% gt()

Pilot %>% tabyl(`#children`)%>% adorn_pct_formatting() %>% gt()
Pilot %>% tabyl(FutureVaccination)%>% adorn_pct_formatting() %>% gt()
Pilot %>% tabyl(County)%>% adorn_pct_formatting() %>% gt()
Pilot %>% tabyl(Setting)%>% adorn_pct_formatting() %>% gt()


race %>% tabyl(`Race/Ethnicity`) %>% arrange(desc(n)) %>% adorn_pct_formatting() %>% gt()
race %>% tabyl(`Race/Ethnicity`) %>% arrange(desc(n)) %>% adorn_pct_formatting() %>% adorn_totals()
rme %>% group_by(rno) %>% add_count() %>%
  ungroup() %>% tabyl(nme,n) %>% gt()


x <- Pilot %>% tabyl(County) %>% select(-percent, - valid_percent) %>% na.omit()
# Map
################################################################################
require(spdep);require(sf)
or.shp <- st_read("~/gis/CountyToFips/or_shp_fips.shp") 
myPal <-  c("#B9B9B9","#D1E5F0","#AECCE2","#8BB2D5","#6799C7","#447FBA","#2166AC")
xx <- x %>% select(county=County, n)
or1 <- left_join(or.shp,xx, by="county")

#build colors for the text labels
or1$build <- or1 %>% select(county,n) %>% mutate(
  build = case_when(
    county  == 'Multnomah' & n>0 ~ '#2166AC',
    county  != 'Multnomah' & n>0 ~ '#000000',
    .default = '#B9B9B9'
  )) %>% pull(build)

# get colors for counties
or1 <- or1 %>% mutate(quantile = ntile(n, 6))
nums <- ifelse(is.na(or1$quantile),1,or1$quantile+1)
or1 <- or1 %>%
  mutate(label_above = ifelse(build=='#2166AC', n,"" ),
         label_below = ifelse(build=="#000000",n,""))

ggplot(or1)+
  # geom_sf(data=or1,aes(fill = ifelse(n>0,n,0)),lwd=.1,colour='black') +
  geom_sf(data=or1,aes(),fill=myPal[nums],lwd=.3,colour='white',
          show.legend = F) +    
  
  geom_sf_text(data=or1,
               aes(label=label_above, col=build),
               vjust= -.6,hjust=-.8,#fontface = "bold",
               stat = "sf_coordinates",show.legend = F
  )+
  geom_sf_text(data=or1,
               aes(label=label_below, col=build),
               #fontface = "bold",
               stat = "sf_coordinates",show.legend = F
  )+
  scale_color_identity()+
  coord_sf(expand = FALSE) +
  theme_void()
################################################################################

# Belief or feeling reasons
Pilot %>% 
  filter(belief) %>%
  tabyl(MainReason)%>%  arrange(desc(n)) %>% adorn_pct_formatting() %>% gt()
feelings %>% na.omit() %>% tabyl(VacFeelings) %>% arrange(desc(n)) %>% adorn_pct_formatting() %>% gt()
# Access reasons
Pilot %>% 
  filter(! belief) %>%
  tabyl(MainReason) %>% arrange(desc(n)) %>% adorn_pct_formatting() %>% gt()
access %>% na.omit() %>% tabyl(VacAccess) %>% arrange(desc(n)) %>% adorn_pct_formatting() %>% gt()

Pilot <-Pilot %>%  mutate(mth = month.abb[month(date)])
Pilot <-Pilot %>%  mutate(m2 = month(date))
rm(pLong,x,xx)

pLong <- Pilot %>% select(-Vaccine, -`Reason for NME`, - VacAccess, - VacFeelings, - `Race/Ethnicity`,
                          -FeelingsOther, - MainReasonOther, - RaceOther, - OverallComments)
pLong <- full_join(pLong, rme, by="rno")
x <-pLong %>% select(rno, mth, nme) %>% 
  group_by(rno) %>% count(nme) %>% 
  ungroup() %>% na.omit() %>%
  select(rno,n) %>% 
  group_by(rno) %>%
  summarise(tot= sum(n)) %>% filter(tot>1)
Pilot %>% select(`Reason for NME`)
Pilot$two <- Pilot %>% select(`Reason for NME`) %>%
  mutate(two = case_when(
    is.na(`Reason for NME`) ~ NA,
    str_detect(`Reason for NME`, "\\|") ~ 2,
    .default = 1  )) %>% pull(two)

Pilot$label <- ifelse(Pilot$two==1,Pilot$`Reason for NME`,"Both")
Pilot$label <- Pilot %>% select(label) %>%
  mutate(x = case_when(label=="Reasons related to ideas or feelings toward vaccination" ~ '"Feelings"',
                       label=="Reasons related to accessing vaccine" ~ '"Access"',
                       label=="Both" ~ " Both",
                       T ~ label)) %>% pull(x)
Pilot %>% 
  group_by(mth, two) %>% count(label) %>% ungroup() %>%
  mutate(mth = factor(mth, levels=month.abb)) %>%
  ggplot(aes(mth,n, fill=label, label=n)) +
  geom_bar(position="stack", stat="identity", color="black", lwd=.1)+
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette = 'Blues')+
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(fill="NME Reason:", x="Month", y="Count")
  
  
v2 <-vaccine %>%
  mutate(got = 1) %>%
  pivot_wider(names_from = Vaccine, values_from = got, values_fill = 0)
names(v2) <- c('rno','Hep A','Hep B',"Polio","Tdap","MMR","Varicella","Hib")
v2$sum <-rowSums(v2[,2:8])
summary(v2$sum)
v2 %>% ggplot(aes(sum))+ geom_histogram(bins = 7, fill="skyblue4")+
  theme_classic()+
  labs(x="Number of Vaccines with a nonmedical exemption",y= "Responses (n)")


v2 %>% tabyl(sum) %>% arrange(desc(n)) %>% adorn_pct_formatting() %>% gt()
v2 %>% summarise(across(.cols=2:8, sum)) %>% gt()
x <- as.matrix(v2[,2:8])
xx <-colnames(x)
x <- x[,c(xx[7],xx[3],xx[6],xx[5],xx[2],xx[1],xx[4])]
heatmap(t(x), Rowv = NA, Colv = NA)
my_colors<- colorRampPalette(c("#FFFFFF", "#2166AC")) 
heatmap(t(x), Rowv = NA, Colv = NA, col = my_colors(2), 
         xlab = "Respondent number")


write_csv(Qual, file = "nme_pilot_qual.csv", col_names = T)

Qual$MainReasonOther %>% na.omit() 

#Thoughts from the meeting: 
# For multiseelects , give the overall percentage, and also the weighted avgs.
# It would be nice to look into the NA's Why aren't they answering? 
# Do they have some sort of category that they fit into?
# Some parents are seeking 7 vaccines, is that more clustered around the time of exclusion
# provide some additional insights for the next meeting








