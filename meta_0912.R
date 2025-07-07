# date:2024-9-12
# meta-analysis of hMPV among adult and pediatric
# ======= import data =======
library(readxl)
library(dplyr)
library(magrittr)
library(meta)
library(ggplot2)
# adult
raw_adult <- read_excel('/Users/heyinan/Desktop/adult&ped_review.xlsx',
                    sheet = 'adult_R') %>% data.frame()


# pediatric
raw_ped <- read_excel('/Users/heyinan/Desktop/adult&ped_review.xlsx',
                          sheet = 'ped_R') %>% data.frame()

colnames(raw_adult)
colnames(raw_ped)

# ======= data clean =======
# remain column need and clead
dat_adult <- raw_adult[,c('Author','publish.year',
                            'Country',"study.trpe",'total.sample',
                            'hMPV.postive.case',
                            'prevalence',"seasonality.peak","quality_score",'test_method_cat')]
dat_adult$Population <- 'adult'
dat_ped <- raw_ped[,c('Author','publish.year',
                            'Country',"study.type",'total.sample',
                            'hMPV.postive.case',
                            'prevalence',"seasonality","quality_score",'test_method_cat')]
dat_ped$Population <- 'pediatric'


# comb

colnames(dat_adult) <- c('Author','Publish_year', 'Country','Study_design','Total','Positive',
                    'Positive_rate','Seasonality',"Quality",'test_method_cat','Population')
colnames(dat_ped) <- c('Author','Publish_year', 'Country','Study_design','Total','Positive',
                         'Positive_rate','Seasonality',"Quality",'test_method_cat','Population')

dat1 <- rbind(dat_adult,dat_ped)
head(dat1)
dim(dat1)
# revise "Author" to be surname
for (i in 1:nrow(dat1)) {
  x = dat1$Author[i]
  y <- strsplit(x, split = ' ')
  z <- last(y[[1]])
  dat1$Author[i] <- z
}

# final included study: dat1
head(dat1)
table(dat1$Country)
# for table 1
# df_t1 <- dat1
# head(df_t1,10)
# colnames(df_t1)[1] <- 'author_old'
# df_t1$Author <- paste(df_t1$author_old,df_t1$Publish_year,sep ='-')
# df_t1 <- df_t1[,c('Population','Author','Country','Study_design','Total','Positive',
#                   'Positive_rate','Seasonality')]
# 
# write.csv(df_t1,'Table1_describtive table of included studies_v2.csv',
#           row.names = F)


# add contient
table(dat1$Country)
dat1$Continent <- ifelse(dat1$Country=='Bangladesh','Asia',
                         ifelse(dat1$Country=='Cameroon','Africa',
                                ifelse(dat1$Country=='China','Asia',
                                       ifelse(dat1$Country=='Colombia','South America',
                                              ifelse(dat1$Country=='Croatia','Europe',
                                                     ifelse(dat1$Country=='Germany','Europe',
                                                            ifelse(dat1$Country=='Ghana','Africa',
                                                                   ifelse(dat1$Country=='India','Asia',
                                                                          ifelse(dat1$Country=='Iran','Asia',
                                                                                 ifelse(dat1$Country=='Italy','Europe',
                                                                                        ifelse(dat1$Country=='Japan','Asia',
                                                                                               ifelse(dat1$Country=='Jordan','Asia',
                                                                                                      ifelse(dat1$Country=='Korea','Asia',
                                                                                                             ifelse(dat1$Country=='Lebanon','Asia',
                                                                                                                    ifelse(dat1$Country=='New Zealand','Oceania',
                                                                                                                           ifelse(dat1$Country=='Norway','Europe',
                                                                                                                                  ifelse(dat1$Country=='Pakistan','Asia',
                                                                                                                                         ifelse(dat1$Country=='Peru','South America',
                                                                                                                                                ifelse(dat1$Country=='Poland','Europe',
                                                                                                                                                       ifelse(dat1$Country=='Saudi Arabia','Asia',
                                                                                                                                                              ifelse(dat1$Country=='USA','North America',
                                                                                                                                                                     ifelse(dat1$Country=='Vietnam','Asia',
                                                                                                                                                                            ifelse(dat1$Country=='Kenya','Africa',
                                                                                                                                                                                   ifelse(dat1$Country=='Morocco','Africa',
                                                                                                                                                                                          ifelse(dat1$Country=='Spain','Europe',
                                                                                                                                                                                                 ifelse(dat1$Country=='Argentina','South America',
                                                                                                                                                                                                        ifelse(dat1$Country=='Taiwan','Asia',
                                                                                                                                                                                                               ifelse(dat1$Country=='Ecuador','South America','NA'))))))))))))))))))))))))))))




table(dat1$Continent)
# for describe ======
# one study with both adult and pediactric data
# two study with both adult and pediactric data

# dat1[dat1$Author == 'Piñana',] # 7 and 48
# dat1[dat1$Author == 'Seo',] # 6 and 41

udat1 <- dat1
dim(udat1)
udat1$author_year <- paste0(udat1$Author, udat1$Publish_year)
head(udat1)
table(duplicated(udat1$author_year))
udat1 <- udat1[!duplicated(udat1$author_year),]
dim(udat1)
#
table(udat1$test_method_cat)
#
table(udat1$Population)
#
table(udat1$Continent)
round(prop.table(table(udat1$Continent))*100,2)
#
table(udat1$test_method_cat)
#
table(udat1$Quality)
round(prop.table(table(udat1$Quality))*100,2)
# ======= meta-analysis =======
# main
# delete low quality study
# table(dat1$Quality<5)
# dat1 <- dat1[dat1$Quality>=5,]
# mean(dat1$Quality)
# median(dat1$Quality)
# table(dat1$Quality)
# dim(dat1)
table(dat1$Population)
meta <- metaprop(Positive, Total, 
                 studlab = paste0(dat1$Author,'-',dat1$Publish_year),
                 subgroup = Population,
                 data=dat1,
                 fixed = F)

meta
sum(dat1$Total)
sum(dat1$Positive)

# sensitivity analysis 
# lower than 6
table(dat1$Quality<6)
sdat1 <- dat1[!dat1$Quality<6,]
dim(sdat1)
meta_s <- metaprop(Positive, Total, 
                 studlab = paste0(sdat1$Author,'-',sdat1$Publish_year),
                 subgroup = Population,
                 data=sdat1,
                 fixed = F)

meta_s
# ====== forest plot =========
pdf('forest_full_0423.pdf', height = 16, width = 15)
forest(meta, fontsize = 8, plotwidth = "10cm", xlim = c(0.00,0.30),
       fs.heading=8.5,
       text.random.w = 'Weight',
       comb.fixed = F,
       level.comb=meta$level.comb,
       leftlabs = c('Study','Positive case','Total'),
       rightlabs = c('Positive rate','95%CI','Weight'),
       digits = 3,
       xlab= 'Proportion',
       squaresize = 0.5, col.square="navy", col.square.lines="navy",
       col.diamond="navy", col.diamond.lines="navy",
       col.by="black")
dev.off()

# ========= funnel plot =========
library(metafor)
# metabias(meta, method.bias = 'linreg', plotit = T, k.min = 5)
# // No test for small study effects conducted for meta-analysis with subgroups. 
# without subgroup
meta2 <- metaprop(Positive, Total, 
                 studlab = paste0(dat1$Author,'-',dat1$Publish_year),
                 # subgroup = Population,
                 data=dat1,
                 fixed = F)
meta2
metabias(meta2, method.bias = 'linreg', plotit = T, k.min = 5)

pdf('Funnel_plot_0423.pdf', height = 5, width = 8)
funnel(meta2,comb.fixed = F, xlim = c(-4.5, -1), fontsize = 8)
legend(x = -1.6, y = 0,
       legend = c("Egger's test:  ","p-value = 0.3014" ),
       cex=0.7)
dev.off()


# ========= subgroup analysis by age_group ==========
dat2 <- read_excel('/Users/heyinan/Desktop/adult&ped_review.xlsx',
                   sheet = 'pediatric_byAGE') %>% data.frame()
head(dat2)
table(is.na(dat2$comb_total))
dat2 <- dat2[!is.na(dat2$comb_positive),]
colnames(dat2)
dat2 <- dat2[,c("Author","publish.year", "Country",
                "comb_total","comb_positive","comb_prevalence",
                "Age_group_standard")]

colnames(dat2) <- c("Author",'Publish_year','Country',
                    'Total','Positive','Prevalence','Age_group_year')

head(dat2)
length(unique(dat2$Author))
table(dat2$Age_group_year)
# dat2$Age_group <- ifelse(dat2$Age_group_year=='(0,1]','(>0to<=1)',
#                        ifelse(dat2$Age_group_year=='(1,2]','(>1to<=2)',
#                               ifelse(dat2$Age_group_year=='(2,3]','(>2to<=3)',
#                                      ifelse(dat2$Age_group_year=='(3,4]','(>3to<=4)',
#                                             ifelse(dat2$Age_group_year=='(4,5]','(>4to<=5)',
#                                                    ifelse(dat2$Age_group_year=='(5,7]','(>5to<=7)','(>7to<=14)'))))))
# 
# 
# 
# combined age (2,3] and (3,4]
dat2$Age_group <- ifelse(dat2$Age_group_year=='(0,1]','(>0to<=1)',
                         ifelse(dat2$Age_group_year=='(1,2]','(>1to<=2)',
                                ifelse(dat2$Age_group_year=='(2,3]','(>2to<=4)',
                                       ifelse(dat2$Age_group_year=='(3,4]','(>2to<=4)',
                                              ifelse(dat2$Age_group_year=='(4,5]','(>4to<=5)','(>5to<18)')))))





table(dat2$Age_group)

# check if exclude pneumonia
dat2$author_year <- paste0(dat2$Author, dat2$Publish_year)
# pneumonia_author <- c('Betty E Owor2016','Leigh M Howard2021','I Jroundi2016','John W Oketch2019','Yuqing Wang2014')
# table(dat2$author_year %in% pneumonia_author)
# dat2 <- dat2[!dat2$author_year %in% author,]
#
sub_meta <- metaprop(Positive, Total, 
                 studlab = paste0(dat2$Author,'-',dat2$Publish_year),
                 subgroup = Age_group,
                 data=dat2,
                 fixed = F)

sub_meta
summary(sub_meta)
# pdf('forest_plot_AgeGrpcheck_excludePneumoia.pdf', height = 15, width = 10)
pdf('forest_plot_AgeGrpcheck_new_0526.pdf', height = 15, width = 10)

forest(sub_meta, fontsize = 8, plotwidth = "10cm", 
       xlim = c(0.00,0.50),
       comb.fixed = F,
       level.comb=sub_meta$level.comb,
       leftlabs = c('Study','Positive case','Total'),
       rightlabs = c('Positive rate','95%CI','Weight'),
       digits = 3,
       xlab= 'Proportion',
       fs.heading=8.5, # 标题大小
       squaresize = 0.5, col.square="navy", col.square.lines="navy",
       col.diamond="navy", col.diamond.lines="navy",
       col.by="black"
       )
dev.off()
 

# ======== heatmap2 ============
dim(dat1)
# heat2 <- read.csv('heatmap_adult2.csv')
heat <- read_excel("heatmap_full.xlsx", 
                   sheet = 'heatmap_adult2') %>% data.frame()

# head(heat)
table(heat$Country)
# heat2$Season <- as.factor(heat2$Season)
X_order <- c("Early_Spring","Middle_Spring","Late_Spring",
             'Early_Summer','Middle_Summer',"Late_Summer",
             'Early_Autumn','Middle_Autumn',"Late_Autumn",
             'Early_Winter','Middle_Winter',"Late_Winter")

pdf('heatmap_adult_0422.pdf', height=8, width = 15)
ggplot(data = heat, mapping = aes(x = Season,
                                   y = Country,
                                   fill = Prevalence)) +
  geom_tile() +
  scale_x_discrete(limits = X_order)+
  xlab(label = "Season") +
  ggtitle("Heatmap of seasonality of adult hMPV poupation") +
  theme(plot.title = element_text(face = 'bold', hjust = 0.5))

dev.off()

# pediatric
heat <- read_excel("heatmap_full.xlsx", 
                   sheet = 'heatmap_ped') %>% data.frame()

# heat
# heat2$Season <- as.factor(heat2$Season)
X_order <- c("Early_Spring","Middle_Spring","Late_Spring",
             'Early_Summer','Middle_Summer',"Late_Summer",
             'Early_Autumn','Middle_Autumn',"Late_Autumn",
             'Early_Winter','Middle_Winter',"Late_Winter")

pdf('heatmap_ped_0422_newLeg.pdf', height=8, width = 15)
ggplot(data = heat, mapping = aes(x = Season,
                                  y = Country,
                                  fill = Prevalence)) +
  geom_tile() +
  scale_x_discrete(limits = X_order)+
  scale_fill_gradient(breaks = c(0,0.025,0.05,0.075,0.1,0.125))+
  xlab(label = "Season") +
  ggtitle("Heatmap of seasonality of pediatric hMPV poupation") +
  theme(plot.title = element_text(face = 'bold', hjust = 0.5)) 
   # +
  # scale_fill_gradient(low = 'blue', high = 'red') +
  # scale_fill_gradientn(colors = rev(terrain.colors(5)))

dev.off()

# dry and rain season
heat <- read_excel("heatmap_full.xlsx", 
                   sheet = 'heatmap_ped_dry-rain') %>% data.frame()

heat
X_order <- c("Dry season",'Rainy season')

pdf('heatmap_dry&rainy_0422_newLeg.pdf', height=5, width = 8)
ggplot(data = heat, mapping = aes(x = Season,
                                  y = Country,
                                  fill = Prevalence)) +
  geom_tile() +
  scale_x_discrete(limits = X_order)+
  xlab(label = "Season") +
  # ggtitle("Heatmap of seasonality of pediatric hMPV poupation") +
  # theme(plot.title = element_text(face = 'bold', hjust = 0.5)) +
  # scale_fill_gradient(low = 'blue', high = 'red') +
  scale_fill_gradient(breaks = c(0,0.025,0.05,0.075,0.1,0.125))+
  theme(axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8))
dev.off()

# ====== symptom =========
sym <- read.csv('symptom_meta.csv')
sym
table(sym$Symptom)
table(sym$Population)

# Revise_March24/2025: do adult&ped separately
table(sym$Population)
psym <- sym[sym$Population=='Adult',]
symlist <- unique(psym$Symptom)
for (jsym in symlist) {
  print('#=============')
  print(jsym)
  # jsym <- "Dyspnea" 
  jwork <- psym[psym$Symptom == jsym,]
  # jwork
  # print("sum n_hMPV")
  # print(sum(jwork$N_hMPV))
  # print("sum n_sym")
  # print(sum(jwork$n_symptom))
  # print(paste0('Number of study: ',nrow(jwork)))
  meta <- metaprop(n_symptom, N_hMPV, 
                   studlab = Author_year,
                   # subgroup = Population,
                   data=jwork,
                   fixed = F)
  
  print(meta)
  # pdf(paste0('forest_', jsym, '.pdf'), height = 8, width = 10)
  # forest(meta,
  #        leftlabs = c('Study',paste0(jsym,' case'),'Total hMPV'),)
  # dev.off()
}

# all
meta <- metaprop(n_symptom, N_hMPV, 
                 studlab = Author_year,
                 subgroup = Symptom,
                 data=sym,
                 fixed = F)

meta
# ======== meta-regression ========
dim(dat1)
# df_t1 <- dat1
# meta_reg <- metaprop(Positive, Total, 
#                   studlab = paste0(df_t1$Author,'-',df_t1$Publish_year),
#                   # subgroup = Population,
#                   data=df_t1,
#                   fixed = F)
# meta_reg

# dat1 <- df_t1
head(dat1)
dim(dat1)
table(dat1$Country)

#
table(dat1$Continent)
dat1[dat1$Continent=='NA',]
dim(dat1)
#
meta2 <- metaprop(Positive, Total, 
                  studlab = paste0(dat1$Author,'-',dat1$Publish_year),
                  # subgroup = Population,
                  data=dat1,
                  fixed = F)
meta2
# year
year <- dat1$Publish_year
meta_reg <- metareg(meta2, ~ year)
meta_reg
# sample size
size <- dat1$Total
meta_reg <- metareg(meta2, ~ size)
meta_reg
# population
table(dat1$Population)
prop.table(table(dat1$Population))
pop <- dat1$Population
meta_reg <- metareg(meta2, ~ pop)
meta_reg

# contient
contient <- dat1$Continent
table(contient)
prop.table(table(contient))*100
meta_reg <- metareg(meta2, ~ contient)
meta_reg

# delete low quality one: Sobkowiak-2020
# ped <- raw_ped[raw_ped$quality_score != 4,]

# method
table(raw_adult$test_method_cat)
table(raw_ped$test_method_cat)
method <- c(raw_adult$test_method_cat,raw_ped$test_method_cat)
length(method)
table(method)
prop.table(table(method))*100
method <- as.factor(method)
method <- relevel(method, ref = 'Nasal or throat swab')
meta_reg <- metareg(meta2, ~ method)
meta_reg

# study period type
# add "Less than one year"
# raw_ped$Period_of_inclusion <- ifelse(raw_ped$study_duration_year<1, 'Less than one year',
#                                       raw_ped$Period_of_inclusion)
# 
# period <- c(raw_adult$Period_of_inclusion, raw_ped$Period_of_inclusion)
# table(period)
# period <- as.factor(period)
# period <- relevel(period, ref = 'NA')
# meta_reg <- metareg(meta2, ~ period)
# meta_reg

# study duration
# duration <- c(raw_adult$study_duration_year, raw_ped$study_duration_year)
# duration <- as.numeric(duration)
# library(mice) # Multiple Imputation by Chained Equation
# dur_df <- dat1
# dur_df$duration <- duration
# imputed_data <- mice(dur_df, m=5, maxit=50, method='pmm', seed=500)
# complete_data <- complete(imputed_data)
# complete_data
# dur_complet <- complete_data$duration
# meta_reg <- metareg(meta2, ~ dur_complet)
# meta_reg
#
# dur_cat <- 0
# for (i in (1:length(duration))) {
#   # i = 2
#   a <- ifelse(is.na(duration[i]),'NA',
#               ifelse(duration[i]<1, '< 1 year',
#                      ifelse((duration[i]>=1)&(duration[i]<2),'<=1 to 2 years',
#                             ifelse((duration[i]>=2)&(duration[i]<5),'<=2 to 5 years','>= 5 years'))))
#   
#   a
#   dur_cat[i] <- a
#  
# }
# dur_cat <- as.factor(dur_cat)
# dur_cat <- relevel(dur_cat, ref = 'NA')
# meta_reg <- metareg(meta2, ~ dur_cat)
# meta_reg

# quality score
quality <- c(raw_adult$quality_score, raw_ped$quality_score)
quality
meta_reg <- metareg(meta2, ~ quality)
meta_reg
# multi ===
meta_reg <- metareg(meta2, ~ 
                      year
                    +
                      size
                    +pop
                    +contient
                    +method
                    # +period
                    # +dur_df$duration
                    +quality)
meta_reg



# ======= characteristics 
dia <- c(raw_adult$diagnosis, raw_ped$diagnosis)
dia <- tolower(dia)
table(dia)



