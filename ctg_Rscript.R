#----------Libraries----------
library(readxl)
library(stringr)
library(dplyr)
library(readr)
library(tidyr)
library(stringi)
library(RColorBrewer)
library(ggplot2)
#----------Directories----------
setwd("/Users/yc954/Desktop/Yale/Courses/Fall24/eph608")
#----------Data----------
ctg_studies <-read_xlsx("ctg-studies.xlsx")
#----------Pre-processing----------
# Remove entries that do not correspond to dengue vaccines
colnames(ctg_studies)
tmp_input <- ctg_studies$Interventions
names(tmp_input) <- ctg_studies$`NCT Number`
# create vector storing acceptable characters corresponding to dengue vaccines
keep_intervention_vaccine <- c('BIOLOGICAL')
tmp_keep1 <- tmp_input[str_detect(tmp_input,pattern=keep_intervention_vaccine)]
tmp_kick1 <- tmp_input[!str_detect(tmp_input,pattern=keep_intervention_vaccine)]

# Manually read tmp_kick1_df$`Brief Summary` to see which ones are for dengue vaccines
tmp_kick1_df <- ctg_studies%>%
  filter(`NCT Number`%in% names(tmp_kick1))
tmp_keep2 <- tmp_kick1_df$`NCT Number`[c(20,26)]

# Manually read tmp_keep1_df$`Brief Summary` to see which ones are for dengue vaccines
tmp_keep1_df <- ctg_studies%>%
  filter(`NCT Number`%in%names(tmp_keep1))
tmp_kick2 <-tmp_keep1_df$`NCT Number`[c(8,11,22,56,58,104,106,107,108,114,115,117,118,119,121,122)] # 115 is for melanoma..? 
tmp_keep3 <- tmp_keep1_df$`NCT Number`[!tmp_keep1_df$`NCT Number` %in% tmp_kick2]
tmp_keep1_df[115,]
# Combine all entries to be retained
keep_NCT <- c(tmp_keep2,tmp_keep3)
rm(list=ls(pattern='tmp*'))

# One final check 
tmp_check<-ctg_studies%>%
  filter(`NCT Number`%in%keep_NCT)%>%
  select(c(`NCT Number`,`Study Title`,Conditions,Interventions,Sponsor,Collaborators,`Funder Type`,
           `Start Date`,Locations))

tmp_suspicious <- tmp_check%>%filter(Interventions%in%c("BIOLOGICAL: Dengue virus 3 Live Virus Human Challenge (DENV-3-LVHC)",
                                      "BIOLOGICAL: Dengue-1 Virus-Live Virus Human Challenge (DENV-1-LVHC)"))

keep_NCT <- tmp_check$`NCT Number`[!tmp_check$`NCT Number`%in%tmp_suspicious$`NCT Number`]
rm(list=ls(pattern='tmp*'))

# df for visuals
df_ctg_filtered <- ctg_studies%>%
  filter(`NCT Number`%in%keep_NCT)
table(df_ctg_filtered$Phases)
table(df_ctg_filtered$Locations)

# preparing df for visuals
# we want entries by NCT Number, Interventions, Locations, YearEarliest # Either study start date or primary completion date, if study start date not available.
vec_NCT <- df_ctg_filtered$`NCT Number`
tmp_vec_INT <- df_ctg_filtered$Interventions
tmp_vec_YEAR <- df_ctg_filtered$`Start Date`

tmp_index <-which(is.na(tmp_vec_YEAR))

tmp_vec_YEAR[tmp_index] <- df_ctg_filtered$`Primary Completion Date`[tmp_index]
tmp_vec_YEAR[tmp_index]
tmp_vec_LOC <- df_ctg_filtered$Locations

# we want to 'clean up' all tmp_vecs 
length(unique(tmp_vec_INT))
tmp_vec_INT_2 <- str_split(tmp_vec_INT,pattern='BIOLOGICAL:')
tmp_vec_INT_3 <- unlist(tmp_vec_INT_2)
tmp_vec_INT_4 <- unique(str_remove_all(tmp_vec_INT_3,pattern='[|]+'))
head(sort(tmp_vec_INT_4))

##------------VaccineName------------
# we want to 'clean up' all tmp_vecs 
# Manually pick those corresponding to a dengue vaccine
write_delim(as.data.frame(tmp_vec_INT_4),file='tmp_vec_INT_4_20241125.txt',
            eol='\n',col_names = F)
tmp_vec_INT_5<-read_xlsx("tmp_vec_INT_4_20241125.xlsx")
table(tmp_vec_INT_5$VaccineName) # append as new columns to final dataframe

# check if any of the original interventions contain more than one VaccineName
# populate tmp_df_INT with binary indicators according to lookup table in tmp_vec_INT_5
tmp_df_INT <- as.data.frame(matrix(ncol=1+length(unique(na.omit(tmp_vec_INT_5$VaccineName))),nrow=length(tmp_vec_INT)))
colnames(tmp_df_INT) <- c("Interventions",unique(na.omit(tmp_vec_INT_5$VaccineName)))
tmp_df_INT$Interventions <- tmp_vec_INT

# for loop
vec_VaccineName <- colnames(tmp_df_INT)[-1]
query_input <- tmp_df_INT$Interventions
for(i in 1:length(vec_VaccineName)){
  # i <- 1 
  i_VN <- vec_VaccineName[i]
  i_LU <-tmp_vec_INT_5%>%filter(VaccineName==i_VN)%>%select(Name)
  tmp<-str_detect(query_input, str_c(i_LU$Name, collapse = "|"))
  tmp_df_INT[tmp,i+1] <- "TRUE"
}

# inspect entries with all NAs 
write_delim(tmp_df_INT,file='tmp_df_INT.tsv',delim="\t") # manually match entries, some fail lookup because of parentheses
tmp_df_INT_2<- read_xlsx("tmp_df_INT.xlsx")

# pivot_longer now...

tmp_df_INT_3_index <- apply(tmp_df_INT_2[,2:20],2,function(x) str_detect(x, pattern="TRUE"))

tmp_df_INT_2$VaccineName <- NA
for(i in 1:nrow(tmp_df_INT_2)){
  #i <- 1 
  val_new <- names(which(tmp_df_INT_3_index[i,]))
  if(length(val_new)!=0){
    tmp_df_INT_2$VaccineName[i]<-val_new
  } else{
    tmp_df_INT_2$VaccineName[i]<-NA
  }
  
}
df_INT_4 <- tmp_df_INT_2%>%
  select(c(Interventions,VaccineName))
rm(list=ls(pattern='tmp*'))
tmp_df_INT_4 <- left_join(df_INT_4,df_ctg_filtered, join_by(Interventions==Interventions))
write_delim(tmp_df_INT_4,file='tmp_df_INT_4.tsv',delim='\t')
tmp_df_INT_5<-read_xlsx('tmp_df_INT_4.xlsx')
tmp_df_INT_6 <- tmp_df_INT_5%>%
  distinct()%>%
  select(c(`NCT Number`,VaccineName))
write_delim(tmp_df_INT_6,file="NCT_VaccineName.tsv",delim='\t')
rm(list=ls(pattern='tmp*'))
##------------Date------------
rm(list=ls())
ctg_studies <-read_xlsx("ctg-studies.xlsx")
df_INT <- read_delim(file="NCT_VaccineName.tsv",delim='\t')
tmp_df_YEAR <- ctg_studies%>%filter(`NCT Number`%in%df_INT$`NCT Number`)%>%
  select(c(`NCT Number`,`Start Date`,`Primary Completion Date`))%>%
  mutate(EarliestYear=str_sub(`Start Date`,start=1,end=4))%>%
  mutate(EarliestYear=case_when(is.na(EarliestYear)~str_sub(`Primary Completion Date`,start=1,end=4),
                                ,.default=EarliestYear))%>%
  select(c(`NCT Number`,EarliestYear))
df_YEAR <- left_join(df_INT,tmp_df_YEAR,join_by(`NCT Number`==`NCT Number`))
write_delim(df_YEAR,file="NCT_EarliestYear.tsv",delim="\t")

##------------Locations------------
rm(list=ls())
ctg_studies <-read_xlsx("ctg-studies.xlsx")
df_YEAR <- read_delim(file="NCT_EarliestYear.tsv",delim="\t")
tmp_df_LOC <- ctg_studies%>%filter(`NCT Number`%in%df_YEAR$`NCT Number`)%>%
  select(c(`NCT Number`,Locations))
tmp_df_LOC_2 <- unlist(str_split(tmp_df_LOC$Locations,pattern='[|]+'))
which(str_count(tmp_df_LOC_2,pattern=',')==9)
tmp_df_LOC_3 <- c()
for(i in 1:length(tmp_df_LOC_2)){
  i_start<-stri_locate_last(tmp_df_LOC_2[i],regex=", ")[2]
  i_stop <- nchar(tmp_df_LOC_2[i])
  tmp_df_LOC_3[i] <- str_trim(str_sub(tmp_df_LOC_2[i],i_start,i_stop))
}
vec_LOC <- unique(na.omit(tmp_df_LOC_3))
tmp_df_LOC_4 <- as.data.frame(matrix(nrow=nrow(tmp_df_LOC),ncol=2+length(vec_LOC)))
colnames(tmp_df_LOC_4) <- c(colnames(tmp_df_LOC),vec_LOC)
tmp_df_LOC_4$`NCT Number` <- tmp_df_LOC$`NCT Number`
tmp_df_LOC_4$Locations <- tmp_df_LOC$Locations
for(i in 1:nrow(tmp_df_LOC_4)){
#  i <- 1
  i_query <- tmp_df_LOC_4$Locations[i]
  i_ind <- rep(FALSE,length(vec_LOC))
  for(j in 1:length(vec_LOC)){
    i_ind[j] <- str_detect(i_query, vec_LOC[j])
  }
  tmp_df_LOC_4[i,3:ncol(tmp_df_LOC_4)] <- i_ind
}
write_delim(tmp_df_LOC_4,file="NCT_Countries.tsv",delim="\t")
##----------Phase----------
rm(list=ls())
ctg_studies <-read_xlsx("ctg-studies.xlsx")
tmp_df_LOC_4 <- read_delim(file="NCT_Countries.tsv",delim="\t")
tmp_df_PHASE <- ctg_studies%>%
  filter(`NCT Number`%in%tmp_df_LOC_4$`NCT Number`)%>%
  select(c(`NCT Number`,Phases))%>%
  mutate(PhaseLatest=case_when(str_detect(Phases,pattern="[|]") ~ str_sub(Phases,start=8,end=13),
                               .default=Phases))%>%
  select(c(`NCT Number`,PhaseLatest))
write_delim(tmp_df_PHASE,file="NCT_PhaseLatest.tsv",delim="\t")

##----------Collating----------
rm(list=ls())
ctg_studies <-read_xlsx("ctg-studies.xlsx")
df_YEAR <- read_delim(file="NCT_EarliestYear.tsv",delim="\t")
tmp_df_PHASE <- read_delim(file="NCT_PhaseLatest.tsv",delim="\t")
tmp_df_LOC_4 <- read_delim(file="NCT_Countries.tsv",delim="\t")
df_all_1 <- left_join(df_YEAR,tmp_df_PHASE,join_by(`NCT Number`==`NCT Number`))
tmp_Sponsor <- ctg_studies %>%
  filter(`NCT Number` %in% df_all_2$`NCT Number`)%>%
  select(c(`NCT Number`,Sponsor))
df_all_2 <- left_join(df_all_1,tmp_Sponsor,join_by(`NCT Number`==`NCT Number`))
df_all_3 <- left_join(df_all_2,tmp_df_LOC_4,join_by(`NCT Number`==`NCT Number`))
write_delim(df_all_3,file="NCT_Collated.tsv",delim="\t") # manually impute VaccineNames to be 'rDEN', gave up classifying vaccines by valency
##----------Classifying Vaccine Names----------
# Manually... 
vec_Countries <- colnames(NCT_Collated)[7:33]
vec_Sponsors <- unique(NCT_Collated$Sponsor)
tmp<-NCT_Collated%>%group_by(Sponsor)%>%summarise(nCount=n())%>%arrange(nCount)
#mybigpal <- colorRampPalette( brewer.pal(9,"Spectral") )(length(vec_Sponsors))
#names(mybigpal)<-vec_Sponsors

n <- 16
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
mysample<-sample(col_vector, n)
pie(rep(1,n), col=mysample)
saveRDS(mysample,'mybigpal_16.rds')
mybigpal <- mysample
names(mybigpal) <- vec_Sponsors

NCT_Collated <- read_delim(file="NCT_Collated.tsv",delim="\t") # manually impute VaccineNames to be 'rDEN', gave up classifying vaccines by valency
vis_1 <- ggplot(NCT_Collated)+
  geom_bar(stat="count",aes(x=EarliestYear,fill=Sponsor))+
  theme_bw()+
  facet_grid(rows=vars(PhaseLatest))+
  labs(x="Year",y="Number of clinical trials")+
  scale_fill_manual(values=mybigpal,
                    breaks=tmp$Sponsor)
pdf(file="DENV_vaccinesByPhases.pdf",width=14,height=8)
print(vis_1)
dev.off()
# For PHASE3, PHASE2 only
# Reading their original things..
vec_wanted <- c("PHASE2")
df_forManual <- NCT_Collated%>%filter(PhaseLatest%in%vec_wanted)
sort(table(df_forManual$Sponsor))

df_forManual_LU <- ctg_studies%>%
  filter(`NCT Number`%in%df_forManual$`NCT Number`)
LU_slice <- df_forManual_LU
table(str_sub(LU_slice$`Other IDs`,
        start=1,
        end=str_locate(LU_slice$`Other IDs`,pattern="[|]")[,1]-1))

# transposing, countries are rows now 
ggplot(df_transposed,aes(x=Year,y=Country))+
  geom_tile(aes(fill=NCTcount))
#----------Visuals----------
rm(list=ls())
NCT_Collated <- read_delim(file="NCT_Collated.tsv",delim="\t")
NCT_Collated$SponsorNew <- NCT_Collated$Sponsor
NCT_Collated$SponsorNew[which(NCT_Collated$Sponsor=="Sanofi Pasteur, a Sanofi Company")  ] <- "Sanofi"
# 15 unique sponsors for 106 clinical trials
sort(table(NCT_Collated$SponsorNew))
# Breakdown of phases
table(NCT_Collated$PhaseLatest)

##----------By Sponsor----------
# Can we plot SponsorNew (x), NumberofStudies (y), broken down by phases?
palette_phase <- RColorBrewer::brewer.pal(n=3,name="Greens")
names(palette_phase) <- c("PHASE1","PHASE2","PHASE3")
df_tmp_sponsor <- NCT_Collated%>%group_by(SponsorNew) %>% mutate(NCTcount = n())%>%
  select(c(SponsorNew,NCTcount))%>%
  distinct()
df_tmp_sponsor_annotated <- read_xlsx("/Users/yc954/Desktop/Yale/Courses/Fall24/eph608/df_tmp_sponsor.xlsx")
df_tmp_sponsor_annotated%>%group_by(Country)%>%summarise(AllNCTCount=sum(NCTcount))
df_tmp_sponsor_annotated%>%group_by(PublicPrivateGovtAcademia)%>%summarise(AllNCTCount=sum(NCTcount))

NCT_Collated%>%filter(SponsorNew=="Takeda")%>%group_by(PhaseLatest)%>%summarise(NCTall=n())

fig_1 <- NCT_Collated%>%group_by(SponsorNew) %>% mutate(NCTcount = n()) %>%
  ggplot(aes(y=reorder(SponsorNew,-NCTcount)))+
  geom_bar(stat="count",aes(fill =PhaseLatest))+
  theme_bw()+
  scale_fill_manual(values=palette_phase)+
  labs(x="Number of published clinical trials",y="Sponsor",fill="Phase")
fig_1

##----------By Country----------
tmp_byCountry <- t(cbind(NCT_Collated$`NCT Number`,NCT_Collated[,7:33]))
colnames(tmp_byCountry)<-tmp_byCountry[1,]
tmp_byCountry <- tmp_byCountry[-1,]

# pivot_longer, DIY, Entries represent countries.  
vec_Countries <- colnames(NCT_Collated)[7:33]
df_byCountry <- data.frame(Country=vec_Countries,
                           Phase1=NA, Phase2=NA, Phase3=NA)
for(i in 1:nrow(df_byCountry)){
 # i = 1 
  i_country=df_byCountry[i,1]
  LU_col <- which(colnames(NCT_Collated)==i_country)
  i_df_LU <- NCT_Collated[which(NCT_Collated[,LU_col]==TRUE),]
  i_df_LONG <- cbind(Country=rep(i_country,nrow(i_df_LU)),i_df_LU[,1:4])
  write_delim(i_df_LONG,file=paste0(c("i_df_LONG_i_",i,"_.tsv"),collapse=""),delim="\t")
}

# cat i_df_LONG_i_* > i_df_LONG_ALL.tsv
i_df_LONG <- read_delim(file="i_df_LONG_ALL.tsv",delim="\t")%>%distinct()%>%
  filter(Country!="Country")
df_tmp_country <- i_df_LONG%>%group_by(Country,PhaseLatest)%>%summarise(NCTall=n())
df_tmp_country_2 <- df_tmp_country%>%group_by(Country)%>%summarise(NCTallAll=sum(NCTall))
fig_2 <- i_df_LONG%>%
  group_by(Country) %>% mutate(NCTcount = n())%>%
  ggplot(aes(y=reorder(Country,-NCTcount)))+
  geom_bar(stat="count",aes(fill=PhaseLatest))+
  scale_fill_manual(values=palette_phase)+
  theme_bw()+
  labs(x="Number of published clinical trials",y="Country",fill="Phase")

fig_2

fig_2b <- i_df_LONG%>%
  group_by(Country) %>% mutate(NCTcount = n())%>%
  ggplot(aes(y=reorder(Country,-NCTcount)))+
  geom_bar(stat="count",aes(fill=PhaseLatest),col="grey70")+
  facet_grid(cols=vars(PhaseLatest))+
  scale_fill_manual(values=palette_phase)+
  theme_bw()+
  labs(x="Number of published clinical trials",y="Participating Country")
fig_2b


pdf(file="DENV_trials_byPartcountrybyPhase.pdf",width=14,height=8)
print(fig_2b)
dev.off()

cdc_countries <- c("Afghanistan","Burkina Faso","Cape Verde", "Central African Republic", "Colombia",
                   "Costa Rica","Cuba", "Dominican Republic","Ecuador", "El Salvador",
                   "Ethiopia","French Polynesia","Ghana","Grenada",
                   "Guadeloupe", "Guatemala", "Honduras", "India",
                   "Iran","Mali", "Mexico",
                   "Pakistan","Panama","Philippines","Saint Lucia","Sudan","Trinidad and Tobago")
vec_Countries
cdc_countries[which(cdc_countries%in%vec_Countries)]
`%!in%` = Negate(`%in%`)

library(countrycode)
df_tmp_country_3 <- data.frame(country = cdc_countries[which(cdc_countries%!in%vec_Countries)])

df_tmp_country_3$continent <- countrycode(sourcevar = df_tmp_country_3[, "country"],
                            origin = "country.name",
                            destination = "continent")
df_tmp_country_3%>%group_by(continent)%>%summarise(ncount=n())

df_tmp_country_4 <- data.frame(country=vec_Countries)

df_tmp_country_4$continent <- countrycode(sourcevar = df_tmp_country_4[, "country"],
                                          origin = "country.name",
                                          destination = "continent")

df_tmp_country_4%>%group_by(continent)

install.packages("wbstats")
library(wbstats)
gdp_capita <- wb_data("NY.GDP.PCAP.CD", mrv = 1)
df_tmp_country_5 <- gdp_capita %>%filter(country%in%vec_Countries | iso3c%in%c("TWN","VNM"))
vec_Countries[vec_Countries%!in%df_tmp_country_5$country]
df_tmp_country_5$country[which(df_tmp_country_5$country=="Viet Nam")] <- "Vietnam"

df_tmp_country_6 <- left_join(df_tmp_country_5,df_tmp_country_2,join_by(country==Country))%>%
  select(c(iso2c,iso3c,country,`NY.GDP.PCAP.CD`,NCTallAll))
plot(df_tmp_country_6$NCTallAll,log(df_tmp_country_6$NY.GDP.PCAP.CD))

##----------By Phase----------
# E.g., for Phase 3, which countries, timeline, and vaccine? 
# To do: for every vaccineName, is there sufficient info to describe how each one works?
tmp_Phase3 <- i_df_LONG %>%filter(PhaseLatest=="PHASE3")
table(tmp_Phase3$VaccineName)
table(tmp_Phase3$Country)

tmp_Phase2 <- i_df_LONG %>%filter(PhaseLatest=="PHASE2")
table(tmp_Phase2$VaccineName)
table(tmp_Phase2$Country)
##-----------By Year------------
NCT_Collated <- read_delim(file="NCT_Collated.tsv",delim="\t") # manually impute VaccineNames to be 'rDEN', gave up classifying vaccines by valency
vec_Countries <- colnames(NCT_Collated)[7:33]
i_df_LONG <- read_delim(file="i_df_LONG_ALL.tsv",delim="\t")%>%distinct()%>%
  filter(Country!="Country")
palette_count <- RColorBrewer::brewer.pal(n=6,name="Reds")
names(palette_count) <- 1:6
fig3 <- ggplot(i_df_LONG,aes(x=EarliestYear,y=Country))+
  stat_bin_2d(aes(fill=as.factor(after_stat(count))),position = "identity",color='grey70')+
  facet_grid(rows=vars(PhaseLatest))+
  theme_bw()+
  scale_fill_manual(values=palette_count)+
  labs(x="Year",y="Participating Country",fill="Number of clinical trials")+
  theme(legend.position="bottom")
pdf(file="DENV_trialsByPartcountryYear.pdf",width=14,height=8)
print(fig3)
dev.off()

fig3b <- ggplot(i_df_LONG,aes(x=EarliestYear,y=Country))+
  stat_bin_2d(aes(fill=as.factor(after_stat(count))),position = "identity",color='grey70')+
  theme_bw()+
  scale_fill_manual(values=palette_count)+
  labs(x="Year",y="Participating Country",fill="Number of clinical trials")+
  theme(legend.position="bottom")
fig3b

tmp_df_year <- i_df_LONG%>%
  group_by(Country)%>%
  summarise(FirstYear=as.numeric(min(EarliestYear)))%>%
  arrange(FirstYear)

ggplot(,aes(x=reorder(Country,desc(FirstYear)),y=FirstYear)+
  geom_bar(stat='identity')+
  theme_bw()

fig3c <- ggplot(tmp_df_year, aes(y = reorder(Country, desc(FirstYear)))) + 
  geom_bar(stat="identity",aes(x=FirstYear))+
  theme_bw()+
  labs(x="Year of first published clinical trial",y="Participating Country")+
  coord_cartesian(xlim=c(2002,2022))+
  scale_x_continuous(breaks=seq(2002,2022,1))
  
pdf(file="DENV_partcountrybyFirstyear.pdf",width=14,height=8)
print(fig3c)
dev.off()

