library(tidyverse); library(janitor)
library(readr); library(gt)
library(readxl); library(openxlsx)


setwd("~/git/nme")

Pilot <- read_excel("NME Survey a_b_9_9_24.xlsx")
Pilot <- Pilot %>% select(-ParticipantOther, -CountyOther, # remove empty cols and Module responses
                 -SettingOther, - starts_with("Module"))
Pilot$rno <- 1:nrow(Pilot) # add unique column to track responses

db <-Pilot %>% rename(survey=`Primary Column`)
db <- db %>% relocate(rno)
db <- db %>% filter(!is.na(`Survey Date`))
rm(Pilot,old)
Qual <- db %>% select(survey,rno, FeelingsOther, MainReasonOther, RaceOther, OverallComments)
names(Qual)

# 3 dbs -------------------------------------------------------------------
unique(db$survey)
pilot <- db %>% filter(survey=="pilot")
alpha<- db %>% filter(survey=="alpha")
beta <- db %>% filter(survey=="beta")
ab <- db %>% filter(survey=="alpha" | survey=="beta")

# body --------------------------------------------------------------------

surv_all <- db

# separate multi-selects into new db
vaccine <-surv_all %>% select( rno, Vaccine) %>% 
  separate_longer_delim(cols = 2, delim = " | ") 
rme <- surv_all %>% select(rno,nme= `Reason for NME`) %>%
  separate_longer_delim(cols = 2, delim = " | ")
access <- surv_all %>% select(rno, VacAccess) %>%
  separate_longer_delim(cols = 2, delim = " | ")
feelings <- surv_all %>% select(rno, VacFeelings) %>%
  separate_longer_delim(cols = 2, delim = " | ")
race <- surv_all %>% select(rno, `Race/Ethnicity`) %>%
  separate_longer_delim(cols = 2, delim = " | ")

tabyl(vaccine$Vaccine) %>% adorn_pct_formatting()

unique(rme$nme)
tabyl(rme$nme)

pLong <- surv_all %>% select(-Vaccine, -`Reason for NME`, - VacAccess, - VacFeelings, - `Race/Ethnicity`,
        -FeelingsOther, - MainReasonOther, - RaceOther, - OverallComments)
x <- full_join(pLong, vaccine, by="rno")
surv_all <- surv_all %>%
  mutate(date = ymd_hms(`Survey Date`))
surv_all <-surv_all %>%  mutate(mth = month.abb[month(date)])
surv_all <-surv_all %>%  mutate(yr = year(date))
surv_all <-surv_all %>%  mutate(m2 = month(date))
surv_all$exemptions <-str_count(surv_all$Vaccine,"\\|")+1
surv_all <- surv_all %>% relocate(exemptions, .after = Vaccine)
paste0(format(min(surv_all$`Survey Date`), "%d/%b/%Y")," to ",format(max(surv_all$`Survey Date`), "%d/%b/%Y"))




as.numeric(difftime(max(surv_all$date), min(surv_all$date),units = 'weeks'))

(x <- unique(surv_all$MainReason))
#beta y <- c(T,T,T,T,F,NA)
#pilot y <- c(F,T,T,NA,F,T,NA,T,F,T,T,F,F,F) # divide into beliefs or not
#db y <- c(F,T,T,NA,F,T,NA,T,F,T,T,F,F,F,T) # divide into beliefs or not
#alpha   y <- c(T,T,T,NA,T,T,T,F,F,T) # divide into beliefs or not
xx <-as.data.frame(cbind(x,y))
xx %>% arrange(y)
table(xx$y)
compare <- data.frame('MainReason'=x,'belief'=y)
surv_all <- left_join(surv_all,compare, by="MainReason")
surv_all$`#children`[which(surv_all$`#children`==-1)] <- 0

# extra visuals -----------------------------------------------------------


surv_all %>% tabyl(ParticipantType) %>% adorn_pct_formatting() %>% gt()

surv_all %>% tabyl(`#children`)%>% adorn_pct_formatting() %>% gt()
surv_all %>% tabyl(FutureVaccination,survey)%>% adorn_pct_formatting() %>% gt()
surv_all %>% tabyl(County)%>% adorn_pct_formatting() %>% gt()
surv_all %>% tabyl(Setting)%>% adorn_pct_formatting() %>% gt()

library(table1)
table1(~ FutureVaccination | survey , data=surv_all)
df <- surv_all 
df$survey <- ifelse(df$survey=="pilot",".pilot",df$survey)
table1(~ label | survey, data = df)
table1(~label + exemptions | survey, data = df)

df <- df %>% drop_na(`Reason for NME`)
table1(~ exemptions + survey | label, data = df)


race %>% tabyl(`Race/Ethnicity`) %>% arrange(desc(n)) %>% adorn_pct_formatting() %>% adorn_totals()
rme %>% group_by(rno) %>% add_count() %>%
  ungroup() %>% tabyl(nme,n) %>% gt()


  
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


#write_csv(Qual, file = "nme_surv_all_qual.csv", col_names = T)

Qual$MainReasonOther %>% na.omit() 

#Thoughts from the meeting: 
# For multiseelects , give the overall percentage, and also the weighted avgs.
# It would be nice to look into the NA's Why aren't they answering? 
# Do they have some sort of category that they fit into?
# Some parents are seeking 7 vaccines, is that more clustered around the time of exclusion
# provide some additional insights for the next meeting


# plots for each grouping -------------------------------------------------

race %>% tabyl(`Race/Ethnicity`) %>% arrange(desc(n)) %>% adorn_pct_formatting() %>% gt()

# Map ---------------------------------------------------------------------


x <- surv_all %>% tabyl(County)  %>% na.omit()


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


tabyl(surv_all$belief)
# Belief or feeling reasons -----------------------------------------------

surv_all %>% 
  filter(belief) %>%
  tabyl(MainReason)%>%  arrange(desc(n)) %>% adorn_pct_formatting() %>% gt()
feelings %>% na.omit() %>% tabyl(VacFeelings) %>% arrange(desc(n)) %>% adorn_pct_formatting() %>% gt()



# Access reasons ----------------------------------------------------------


surv_all %>% 
  filter(! belief) %>%
  tabyl(MainReason) %>% arrange(desc(n)) %>% adorn_pct_formatting() %>% gt()
access %>% na.omit() %>% tabyl(VacAccess) %>% arrange(desc(n)) %>% adorn_pct_formatting() %>% gt()


# heatmap -----------------------------------------------------------------

vaccine <- vaccine %>% 
  mutate(Vaccine = case_when(
    Vaccine == "Diphtheria, Tetanus, Pertussis (DTaP or Tdap)" ~ "Tdap",
    Vaccine == "Hepatitis A" ~ "Hep A",
    Vaccine == "Hepatitis B" ~ "Hep B",
    Vaccine == "Measles, Mumps and Rubella (MMR)"  ~ "MMR",
    Vaccine == "Haemophilus influenza type b (Hib)" ~ "Hib",
    T ~ Vaccine  ))

vaccine$cnt <-vaccine %>% select(v=Vaccine) %>%
  group_by(v) %>% 
  add_count(v) %>% pull(n) 

v2 <-vaccine %>%
  mutate(got = 1) %>%
  arrange(desc(cnt)) %>%
  select(-cnt)%>%
  pivot_wider(names_from = Vaccine, values_from = got, values_fill = 0)
#names(v2) <- c('rno','Hep A','Hep B',"Polio","Tdap","MMR","Varicella","Hib")
v2$sum <-rowSums(v2[,2:8])
names(v2) <- c("rno",paste0(names(v2[,2:8])," (N=",colSums(v2[,2:8]), ")") )


x <- as.matrix(v2[,2:8])
xx <-colnames(x)
x <- x[,c(xx[7],xx[3],xx[6],xx[5],xx[2],xx[1],xx[4])]
my_colors<- colorRampPalette(c("#FFFFFF", "#2166AC")) 
heatmap(t(x), Rowv = NA, Colv = NA, col = my_colors(2), 
        xlab = "Respondent number")


# Figure 3. Month of Survey Completion Stratified by NME Reason Gr --------



rm(pLong,x,xx)

pLong <- surv_all %>% select(-Vaccine, -`Reason for NME`, - VacAccess, - VacFeelings, - `Race/Ethnicity`,
                             -FeelingsOther, - MainReasonOther, - RaceOther, - OverallComments)
pLong <- full_join(pLong, rme, by="rno")
x <-pLong %>% select(rno, mth, nme) %>% 
  group_by(rno) %>% count(nme) %>% 
  ungroup() %>% na.omit() %>%
  select(rno,n) %>% 
  group_by(rno) %>%
  summarise(tot= sum(n)) %>% filter(tot>1)
#surv_all %>% select(`Reason for NME`)
surv_all$two <- surv_all %>% select(`Reason for NME`) %>%
  mutate(two = case_when(
    is.na(`Reason for NME`) ~ NA,
    str_detect(`Reason for NME`, "\\|") ~ 2,
    .default = 1  )) %>% pull(two)

surv_all$label <- ifelse(surv_all$two==1,surv_all$`Reason for NME`,"Both")
surv_all$label <- surv_all %>% select(label) %>%
  mutate(x = case_when(label=="Reasons related to ideas or feelings toward vaccination" ~ '"Feelings"',
                       label=="Reasons related to accessing vaccine" ~ '"Access"',
                       label=="Both" ~ " Both",
                       T ~ label)) %>% pull(x)
surv_all %>% 
  group_by(mth, two) %>% count(label) %>% ungroup() %>%
  mutate(mth = factor(mth, levels=month.abb)) %>%
  ggplot(aes(mth,n, fill=label, label=n)) +
  geom_bar(position="stack", stat="identity", color="black", lwd=.1)+
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette = 'Blues')+
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(fill="NME Reason:", x="Month", y="Count")


# Respondent Incidence vs. Month of Year  ---------------------------------
surv_all$lab2<- surv_all %>% 
  select(label) %>%
  group_by(label) %>% add_count(label) %>%
  mutate(lab2 = paste0( label," (N=",n,")")) %>% pull(lab2)



surv_all %>% 
  mutate(mth = factor(mth, levels=month.abb)) %>%
  ggplot(aes(rno,mth, color=lab2))+geom_point()+
  geom_vline(xintercept = c(59,93), color="red", lty="dashed")+
  scale_color_brewer(type = "qual", palette = 6) +
  scale_x_continuous(n.breaks = 33)+
  theme_light()
surv_all %>% ggplot(aes(rno,survey, color=survey))+geom_point()+
  scale_x_continuous(n.breaks = 33)


# divisions ---------------------------------------------------------------

surv_all %>% tabyl(label) %>% adorn_pct_formatting() %>% gt()


# qual --------------------------------------------------------------------
write.xlsx(Qual,"qual.xlsx")

