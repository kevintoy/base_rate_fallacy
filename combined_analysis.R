#yangfeng's data#
data_y<-read.csv("~/psychology of chance and uncertainty/data_4_30_y_english.csv",encoding="UTF-8")
#getting rid of those who already filled the survey 
data_y_cleaned<-data_y[(data_y$previous.survey.filled==0),]

data_y_cleaned$divining_sex_effective[data_y_cleaned$sex_divining_ability==1]<-"low"
data_y_cleaned$divining_sex_effective[data_y_cleaned$sex_divining_ability==2]<-"low"
data_y_cleaned$divining_sex_effective[data_y_cleaned$sex_divining_ability==3]<-"mediocre"
data_y_cleaned$divining_sex_effective[data_y_cleaned$sex_divining_ability==4]<-"high"
data_y_cleaned$divining_sex_effective[data_y_cleaned$sex_divining_ability==5]<-"high"


library(ggplot2)
library(scales)


library(plyr)
cdata <- ddply(data_y_cleaned, c("divining_sex_effective"), summarise,
               N    = length(sex_random_guess),
               mean = mean(sex_random_guess),
               sd   = sd(sex_random_guess),
               se   = sd / sqrt(N)
)

ggplot(data=cdata, aes(x=factor(divining_sex_effective,levels = c("low","mediocre","high")), y=mean)) +
  geom_bar(stat="identity", width=0.5)+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=0.2)+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14))+
  theme(strip.text.x = element_text(size = 12, colour = "black", angle = 0),strip.text.y=element_text(size = 12, colour = "black", angle = 0))+
  ylab("number of correct guesses of baby sex (out of 100)")+
  xlab("attitude towards 50% success rate diviner (y)")+
  geom_hline(yintercept=50, linetype="dashed", color = "red")+
  ylim(0,70)


hist_r<-ggplot(data_y_cleaned, aes(x=sex_random_guess)) + geom_histogram(breaks = seq(0, 100, by = 5),binwidth = 0.5)+
  scale_x_continuous(breaks = seq(0, 100, by=5))+
  aes(y=stat(count)/sum(stat(count))) + 
  scale_y_continuous(labels = scales::percent,limits = c(0, 0.5))+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14))+
  theme(axis.text.x = element_text(size = 14, angle = 45),strip.text.y=element_text(size = 12, colour = "black", angle = 0))+
  xlab("No. of correct guesses out of 100 guesses (Yunnan college)")+
  ylab("percentage")
hist_r


#other college data (including rukeye's)"
data_o_r<-read.csv("~/psychology of chance and uncertainty/data_4_30_o_r_english.csv")
#getting rid of those who already filled the survey 

data_o_r$divining_sex_effective[data_o_r$sex_divining_ability==1]<-"low"
data_o_r$divining_sex_effective[data_o_r$sex_divining_ability==2]<-"low"
data_o_r$divining_sex_effective[data_o_r$sex_divining_ability==3]<-"mediocre"
data_o_r$divining_sex_effective[data_o_r$sex_divining_ability==4]<-"high"
data_o_r$divining_sex_effective[data_o_r$sex_divining_ability==5]<-"high"


library(ggplot2)
library(scales)


library(plyr)
cdata <- ddply(data_o_r, c("divining_sex_effective"), summarise,
               N    = length(sex_random_guess),
               mean = mean(sex_random_guess),
               sd   = sd(sex_random_guess),
               se   = sd / sqrt(N)
)


ggplot(data=cdata, aes(x=factor(divining_sex_effective,levels = c("low","mediocre","high")), y=mean)) +
  geom_bar(stat="identity", width=0.5)+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=0.2)+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14))+
  theme(strip.text.x = element_text(size = 12, colour = "black", angle = 0),strip.text.y=element_text(size = 12, colour = "black", angle = 0))+
  ylab("number of correct guesses of baby sex (out of 100)")+
  xlab("attitude towards 50% success rate diviner (o_r)")+
  geom_hline(yintercept=50, linetype="dashed", color = "red")+
  ylim(0,70)

hist_o_r<-ggplot(data_o_r, aes(x=sex_random_guess)) + geom_histogram(breaks = seq(0, 100, by = 5),binwidth = 0.5)+
  scale_x_continuous(breaks = seq(0, 100, by=5))+
  aes(y=stat(count)/sum(stat(count))) + 
  scale_y_continuous(labels = scales::percent,limits = c(0, 0.5))+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14))+
  theme(axis.text.x = element_text(size = 14, angle = 45),strip.text.y=element_text(size = 12, colour = "black", angle = 0))+
  xlab("No. of correct guesses out of 100 guesses (other colleges)")+
  ylab("percentage")
hist_o_r


#wenjuanxing data#
data_4_20<-read.csv("~/psychology of chance and uncertainty/wenjuanxing_4_26_english.csv",encoding = "UTF-8")

data_4_20$divining_sex_effective[data_4_20$sex_divining_ability==1]<-"very low"
data_4_20$divining_sex_effective[data_4_20$sex_divining_ability==2]<-"low"
data_4_20$divining_sex_effective[data_4_20$sex_divining_ability==3]<-"mediocre"
data_4_20$divining_sex_effective[data_4_20$sex_divining_ability==4]<-"high"
data_4_20$divining_sex_effective[data_4_20$sex_divining_ability==5]<-"very high"

#check average time it takes to for the first 515 subjects (10 of questions) in data_4_20 to complete the survey and in wenjuanxing_reversed

data_4_20_first515<-data_4_20[1:515,]
#remove outlier in wenjuanxing_reverse (row 269)
wenjuanxing_reversed<-wenjuanxing_reversed[-269,]

t.test(data_4_20_first515$time,wenjuanxing_reversed$time)
#no significent difference

#now check histogram
time_hist<-ggplot(wenjuanxing_reversed, aes(x=time)) + geom_histogram(boundary = 10, binwidth = 20)+
  scale_x_continuous(breaks = seq(0, 200, by=20))+
  aes(y=stat(count)/sum(stat(count))) + 
  scale_y_continuous(labels = scales::percent,limits = c(0, 0.7))+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16))+
  theme(axis.text.x = element_text(size = 16, angle = 45),strip.text.y=element_text(size = 12, colour = "black", angle = 0))+
  xlab("number of correct guesses (multiple choice)")+
  ylab("percentage")+
  geom_vline(aes(xintercept = mean(time,na.rm=TRUE)),col='red',size=0.8,linetype="dashed")
time_hist


library(plyr)
cdata <- ddply(data_4_20, c("divining_sex_effective"), summarise,
               N    = length(sex_random_guess),
               mean = mean(sex_random_guess),
               sd   = sd(sex_random_guess),
               se   = sd / sqrt(N)
)

ggplot(data=cdata, aes(x=factor(divining_sex_effective,levels = c("very low","low","mediocre","high","very high")), y=mean)) +
  geom_bar(stat="identity", width=0.5)+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=0.2)+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14))+
  theme(strip.text.x = element_text(size = 12, colour = "black", angle = 0),strip.text.y=element_text(size = 12, colour = "black", angle = 0))+
  ylab("number of correct guesses of baby sex (out of 100)")+
  xlab("attitude towards 50% success rate diviner (wenjuanxing)")+
  geom_hline(yintercept=50, linetype="dashed", color = "red")+
  ylim(0,70)

#histogram for abcd random guess
hist_wenjuanxing<-ggplot(data_4_20, aes(x=abcd_random_guess)) + geom_histogram(boundary = 2.5, binwidth = 5)+
  scale_x_continuous(breaks = seq(0, 100, by=5),limit=c(0,100))+
  aes(y=stat(count)/sum(stat(count))) + 
  scale_y_continuous(labels = scales::percent,limits = c(0, 0.5))+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16))+
  theme(axis.text.x = element_text(size = 16, angle = 45),strip.text.y=element_text(size = 12, colour = "black", angle = 0))+
  xlab("number of correct guesses (multiple choice)")+
  ylab("percentage")+
  geom_vline(aes(xintercept = mean(abcd_random_guess,na.rm=TRUE)),col='red',size=0.8,linetype="dashed")
hist_wenjuanxing


#wenjuanxing data reversed
wenjuanxing_reversed<-read.csv("~/psychology of chance and uncertainty/wenjuanxing_reversed.csv")

wenjuanxing_reversed$divining_sex_effective[wenjuanxing_reversed$sex_divining_ability==1]<-"very high"
wenjuanxing_reversed$divining_sex_effective[wenjuanxing_reversed$sex_divining_ability==2]<-"high"
wenjuanxing_reversed$divining_sex_effective[wenjuanxing_reversed$sex_divining_ability==3]<-"mediocre"
wenjuanxing_reversed$divining_sex_effective[wenjuanxing_reversed$sex_divining_ability==4]<-"low"
wenjuanxing_reversed$divining_sex_effective[wenjuanxing_reversed$sex_divining_ability==5]<-"very low"

wenjuanxing_reversed$sex_random_guess<-(100-wenjuanxing_reversed$sex_random_guess_wrong)# 100 minus sex_random_guess_wrong
wenjuanxing_reversed$TF_random_guess<-(100-wenjuanxing_reversed$TF_random_guess_wrong)
#remove those who answered abcd question incorrectly (wrong number=25)
#wenjuanxing_reversed<-subset(wenjuanxing_reversed, abcd_random_guess_wrong!=25)

library(plyr)
cdata <- ddply(wenjuanxing_reversed, c("divining_sex_effective"), summarise,
               N    = length(sex_random_guess),
               mean = mean(sex_random_guess),
               sd   = sd(sex_random_guess),
               se   = sd / sqrt(N)
)

ggplot(data=cdata, aes(x=factor(divining_sex_effective,levels = c("very low","low","mediocre","high","very high")), y=mean)) +
  geom_bar(stat="identity", width=0.5)+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=0.2)+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14))+
  theme(strip.text.x = element_text(size = 12, colour = "black", angle = 0),strip.text.y=element_text(size = 12, colour = "black", angle = 0))+
  ylab("number of correct guesses of baby sex (out of 100)")+
  xlab("attitude towards 50% success rate diviner (wenjuanxing_reversed)")+
  geom_hline(yintercept=50, linetype="dashed", color = "red")+
  ylim(0,70)

lm_wjx_reverse<-lm(data=wenjuanxing_reversed,sex_random_guess~sex_divining_ability)
summary(lm_wjx_reverse) #this part doesn't quite make sense. significant in the opposite direction



hist_wenjuanxing_reversed<-ggplot(wenjuanxing_reversed, aes(x=sex_random_guess)) + geom_histogram(boundary = 2.5, binwidth = 5)+
  scale_x_continuous(breaks = seq(0, 100, by=5),limits=c(0,100))+
  aes(y=stat(count)/sum(stat(count))) + 
  scale_y_continuous(labels = scales::percent,limits = c(0, 0.55))+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14))+
  theme(axis.text.x = element_text(size = 14, angle = 45),strip.text.y=element_text(size = 12, colour = "black", angle = 0))+
  xlab("number of correct guesses (fetal sex)")+
  ylab("percentage")+
  geom_vline(aes(xintercept = mean(sex_random_guess,na.rm=TRUE)),col='red',size=0.8,linetype="dashed")
hist_wenjuanxing_reversed

t.test(wenjuanxing_reversed$sex_random_guess, mu=50, alternative="two.sided") #not significantly different from 50. 

#mturk data#
mturk_data<-read.csv("~/psychology of chance and uncertainty/mturk_data_english.csv")
#reorder ability code
mturk_data$divining_sex_effective[mturk_data$sex_divining_ability_reordered==1]<-"very low"
mturk_data$divining_sex_effective[mturk_data$sex_divining_ability_reordered==2]<-"low"
mturk_data$divining_sex_effective[mturk_data$sex_divining_ability_reordered==3]<-"mediocre"
mturk_data$divining_sex_effective[mturk_data$sex_divining_ability_reordered==4]<-"high"
mturk_data$divining_sex_effective[mturk_data$sex_divining_ability_reordered==5]<-"very high"
#get country from lon/lat
library(sp)
library(rworldmap)

# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2country = function(points)
{  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # convert our list of points to a SpatialPoints object
  
  # pointsSP = SpatialPoints(points, proj4string=CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
  
  #setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  # return the ADMIN names of each country
  indices$ADMIN  
  #indices$ISO3 # returns the ISO3 code 
  #indices$continent   # returns the continent (6 continent model)
  #indices$REGION   # returns the continent (7 continent model)
}

lon_lat_data<-mturk_data[,c(1,2)]
country_code<-coords2country(lon_lat_data)
mturk_data$country<-country_code



mturk_US<-mturk_data[mturk_data$country %in% "United States of America",] 
mturk_india<-mturk_data[mturk_data$country %in% "India",] 

library(plyr)
cdata <- ddply(mturk_US, c("divining_sex_effective"), summarise,
               N    = length(sex_random_guess),
               mean = mean(sex_random_guess,na.rm = TRUE),
               sd   = sd(sex_random_guess,na.rm = TRUE),
               se   = sd / sqrt(N)
)

ggplot(data=cdata, aes(x=factor(divining_sex_effective,levels = c("very low","low","mediocre","high","very high")), y=mean)) +
  geom_bar(stat="identity", width=0.5)+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=0.2)+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14))+
  theme(strip.text.x = element_text(size = 12, colour = "black", angle = 0),strip.text.y=element_text(size = 12, colour = "black", angle = 0))+
  ylab("number of correct guesses of baby sex (out of 100)")+
  xlab("attitude towards 50% success rate diviner (mturkers)" )+
  geom_hline(yintercept=50, linetype="dashed", color = "red")+
  ylim(0,70)

hist_mturk_us<-ggplot(mturk_US, aes(x=sex_random_guess)) + geom_histogram(breaks = seq(0, 100, by = 5),binwidth = 0.5)+
  scale_x_continuous(breaks = seq(0, 100, by=5))+
  aes(y=stat(count)/sum(stat(count))) + 
  scale_y_continuous(labels = scales::percent,limits = c(0, 0.5))+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14))+
  theme(axis.text.x = element_text(size = 14, angle = 45),strip.text.y=element_text(size = 12, colour = "black", angle = 0))+
  xlab("No. of correct guesses out of 100 guesses (mturk US)")+
  ylab("percentage")
hist_mturk_us

write.csv(mturk_US,"mturk_US.csv")


#hongxi data#
hongxi_data<-read.csv("~/psychology of chance and uncertainty/hongxi_data.csv")

#check if older people/less educated people are more likely to think 50% diviners are of high ability
lm_div_abi<-lm(data=hongxi_data,sex_divining_ability~sex+age+education)
summary(lm_div_abi)

hongxi_data$divining_sex_effective[hongxi_data$sex_divining_ability==1]<-"very low"
hongxi_data$divining_sex_effective[hongxi_data$sex_divining_ability==2]<-"low"
hongxi_data$divining_sex_effective[hongxi_data$sex_divining_ability==3]<-"mediocre"
hongxi_data$divining_sex_effective[hongxi_data$sex_divining_ability==4]<-"high"
hongxi_data$divining_sex_effective[hongxi_data$sex_divining_ability==5]<-"very high"

#check if those who think 50% diviner has high ability are more likely to think dreams are accurate predictor of baby sex
lm1<-lm(data=hongxi_data, sex_divining_ability~sex_random_guess+sex+age)
summary(lm1) #not significant


library(plyr)
cdata <- ddply(hongxi_data, c("divining_sex_effective"), summarise,
               N    = length(sex_random_guess),
               mean = mean(sex_random_guess,na.rm=TRUE),
               sd   = sd(sex_random_guess,na.rm=TRUE),
               se   = sd / sqrt(N)
)

ggplot(data=cdata, aes(x=factor(divining_sex_effective,levels = c("very low","low","mediocre","high","very high")), y=mean)) +
  geom_bar(stat="identity", width=0.5)+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=0.2)+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14))+
  theme(strip.text.x = element_text(size = 12, colour = "black", angle = 0),strip.text.y=element_text(size = 12, colour = "black", angle = 0))+
  ylab("number of correct guesses of baby sex (out of 100)")+
  xlab("evaluation of diviner with 50% success rate")+
  geom_hline(yintercept=50, linetype="dashed", color = "red")+
  ylim(0,70)


hist_hongxi<-ggplot(hongxi_data, aes(x=sex_random_guess)) + geom_histogram(breaks = seq(0, 100, by = 5),binwidth = 0.5)+
  scale_x_continuous(breaks = seq(0, 100, by=5))+
  aes(y=stat(count)/sum(stat(count))) + 
  scale_y_continuous(labels = scales::percent,limits = c(0, 0.5))+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14))+
  theme(axis.text.x = element_text(size = 14, angle = 45),strip.text.y=element_text(size = 12, colour = "black", angle = 0))+
  xlab("No. of correct guesses out of 100 guesses for a random guesser")+
  ylab("percentage")
hist_hongxi

#universal ability barplot maker#
# remove NAs to make the performance evaluation graph
hongxi_data_cleaned<-hongxi_data[!is.na(hongxi_data$sex_divining_ability), ]
qual_data_chinese_cleaned<-qual_data_chinese[!is.na(qual_data_chinese$sex_divining_ability), ]
data_o_r_cleaned<-data_o_r[!is.na(data_o_r$sex_divining_ability), ]
mturk_US_cleaned<-mturk_US[!is.na(mturk_US$sex_divining_ability), ]
wenjuanxing_reversed_cleaned<-wenjuanxing_reversed[!is.na(wenjuanxing_reversed$sex_divining_ability), ]

data_4_20_cleaned<-data_4_20[!is.na(data_4_20$sex_divining_ability), ]
#one sampled t-test to show that mean of the distribution significantly deviates from 50
t.test(qual_data_chinese$sex_random_guess, mu=50, alternative="two.sided")
#for qual_data_chinese_cleaned, reverse the order of ability judgement (1=5, 2=4)
qual_data_chinese_cleaned$sex_divining_ability_new[qual_data_chinese_cleaned$sex_divining_ability==1]<-"five"
qual_data_chinese_cleaned$sex_divining_ability_new[qual_data_chinese_cleaned$sex_divining_ability==2]<-"four"
qual_data_chinese_cleaned$sex_divining_ability_new[qual_data_chinese_cleaned$sex_divining_ability==3]<-"three"
qual_data_chinese_cleaned$sex_divining_ability_new[qual_data_chinese_cleaned$sex_divining_ability==4]<-"two"
qual_data_chinese_cleaned$sex_divining_ability_new[qual_data_chinese_cleaned$sex_divining_ability==5]<-"one"

qual_data_chinese_cleaned$sex_divining_ability[qual_data_chinese_cleaned$sex_divining_ability_new=="one"]<-1
qual_data_chinese_cleaned$sex_divining_ability[qual_data_chinese_cleaned$sex_divining_ability_new=="two"]<-2
qual_data_chinese_cleaned$sex_divining_ability[qual_data_chinese_cleaned$sex_divining_ability_new=="three"]<-3
qual_data_chinese_cleaned$sex_divining_ability[qual_data_chinese_cleaned$sex_divining_ability_new=="four"]<-4
qual_data_chinese_cleaned$sex_divining_ability[qual_data_chinese_cleaned$sex_divining_ability_new=="five"]<-5

p_barplot<-ggplot(wenjuanxing_reversed_cleaned, aes(x = factor(sex_divining_ability))) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_x_discrete(labels=c("very low", "low","mediocre","high","very high"))+
  scale_y_continuous(labels = scales::percent,limits = c(0, 0.5))+ 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14))+
  theme(strip.text.x = element_text(size = 12, colour = "black", angle = 0),strip.text.y=element_text(size = 12, colour = "black", angle = 0))+
  xlab("performance evaluation")+
  ylab("percentage")
p_barplot


#universal number of sex random guess
library(scales)
hist_sex <- ggplot(wenjuanxing_reversed, aes(x = sex_random_guess)) +
  geom_histogram(binwidth = 5, boundary = 0, closed = "left") +  # Adjusted boundary
  scale_x_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 101)) +  # Adjusted limits
  aes(y = stat(count) / sum(after_stat(count))) +
  scale_y_continuous(labels = percent, limits = c(0, 0.5)) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 45),
        strip.text.y = element_text(size = 12, colour = "black", angle = 0)) +
  xlab("number of correct guesses (fetal sex)") +
  ylab("percentage") +
  geom_vline(aes(xintercept = mean(sex_random_guess, na.rm = TRUE)), col = 'red', size = 0.8, linetype = "dashed")

hist_sex

#universal number of boys plot#
hist_boys<-ggplot(qual_data_chinese, aes(x=boy_number)) + geom_histogram(boundary = 2.5, binwidth = 5, closed = "left")+
  scale_x_continuous(breaks = seq(0, 100, by=10),limits = c(0, 100))+
  aes(y=stat(count)/sum(stat(count))) + 
  scale_y_continuous(labels = scales::percent,limits = c(0, 0.5))+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16))+
  theme(axis.text.x = element_text(size = 16, angle = 45),strip.text.y=element_text(size = 12, colour = "black", angle = 0))+
  xlab("number of boys")+
  ylab("percentage")+
  geom_vline(aes(xintercept = mean(boy_number,na.rm=TRUE)),col='red',size=0.8,linetype="dashed")
hist_boys

#universal coin heads plot#
hist_coin<-ggplot(qual_data_chinese, aes(x=coin_head)) + 
  geom_histogram( boundary = 2.5, binwidth = 5, closed = "left")+
  scale_x_continuous(breaks = seq(0, 100, by=10),limits = c(0, 100))+
  aes(y=stat(count)/sum(stat(count))) + 
  scale_y_continuous(labels = scales::percent,limits = c(0, 0.5))+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16))+
  theme(axis.text.x = element_text(size = 16, angle = 45),strip.text.y=element_text(size = 12, colour = "black", angle = 0))+
  xlab("number of heads")+
  ylab("percentage")+
  geom_vline(aes(xintercept = mean(coin_head,na.rm=TRUE)),col='red',size=0.8,linetype="dashed")
hist_coin

#universal TF random guess plot#
hist_TF<-ggplot(qual_data_chinese, aes(x=TF_random_guess)) + geom_histogram(boundary = 2.5, binwidth = 5, closed = "left")+
  scale_x_continuous(breaks = seq(0, 100, by=10),limits = c(0, 100))+
  aes(y=stat(count)/sum(stat(count))) + 
  scale_y_continuous(labels = scales::percent,limits = c(0, 0.5))+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16))+
  theme(axis.text.x = element_text(size = 16, angle = 45),strip.text.y=element_text(size = 12, colour = "black", angle = 0))+
  xlab("number of correct guesses (TF question)")+
  ylab("percentage")+
  geom_vline(aes(xintercept = mean(TF_random_guess,na.rm=TRUE)),col='red',size=0.8,linetype="dashed")
hist_TF

#combine data to see if evaluation of ability vs. sex random guess
mturk_data$sex_divining_ability<-mturk_data$sex_divining_ability_reordered
qual_data_chinese$sex_divining_ability<-qual_data_chinese$sex_divining_ability_reordered

combined_data<-rbind.fill(data_4_20,data_o_r,data_y,mturk_US,qual_data_chinese)
#linear regression to see if people with certain demographic features are less likely to deviate from correct answers
combined_data$dist_from_50_sex<-abs(combined_data$sex_random_guess-50)
lm_sex<-lm(data=combined_data,dist_from_50_sex~sex+age+education)
summary(lm_sex)

combined_data$dist_from_50_TF<-abs(combined_data$TF_random-50)
lm_TF<-lm(data=combined_data,dist_from_50_TF~sex+age+education)
summary(lm_TF)


combined_data<-combined_data[!is.na(combined_data$sex_divining_ability), ]

#check spearman's correlation first
cor.test(combined_data$sex_divining_ability, combined_data$sex_random_guess,  method = "spearman")

combined_data$divining_sex_effective[combined_data$sex_divining_ability==1]<-"very low"
combined_data$divining_sex_effective[combined_data$sex_divining_ability==2]<-"low"
combined_data$divining_sex_effective[combined_data$sex_divining_ability==3]<-"mediocre"
combined_data$divining_sex_effective[combined_data$sex_divining_ability==4]<-"high"
combined_data$divining_sex_effective[combined_data$sex_divining_ability==5]<-"very high"
library(plyr)
cdata <- ddply(combined_data, c("divining_sex_effective"), summarise,
               N    = length(sex_random_guess),
               mean = mean(sex_random_guess,na.rm=TRUE),
               sd   = sd(sex_random_guess,na.rm=TRUE),
               se   = sd / sqrt(N)
)

ggplot(data=cdata, aes(x=factor(divining_sex_effective,levels = c("very low","low","mediocre","high","very high")), y=mean)) +
  geom_bar(stat="identity", width=0.5)+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=0.2)+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14))+
  theme(strip.text.x = element_text(size = 12, colour = "black", angle = 0),strip.text.y=element_text(size = 12, colour = "black", angle = 0))+
  ylab("number of correct guess (fetal sex)")+
  xlab("performance evaluation")+
  geom_hline(yintercept=50, linetype="dashed", color = "red")+
  ylim(0,50)

#analysis for randomized data
randomized_data<-read.csv("~/psychology of chance and uncertainty/randomized/5_20.csv")
randomized_data<-randomized_data[!is.na(randomized_data$sex_divining_ability), ]

randomized_data$divining_sex_effective[randomized_data$sex_divining_ability==1]<-"very low"
randomized_data$divining_sex_effective[randomized_data$sex_divining_ability==2]<-"low"
randomized_data$divining_sex_effective[randomized_data$sex_divining_ability==3]<-"mediocre"
randomized_data$divining_sex_effective[randomized_data$sex_divining_ability==4]<-"high"
randomized_data$divining_sex_effective[randomized_data$sex_divining_ability==5]<-"very high"

library(plyr)
cdata <- ddply(randomized_data, c("divining_sex_effective"), summarise,
               N    = length(sex_random_guess),
               mean = mean(sex_random_guess,na.rm=TRUE),
               sd   = sd(sex_random_guess,na.rm=TRUE),
               se   = sd / sqrt(N)
)

ggplot(data=cdata, aes(x=factor(divining_sex_effective,levels = c("very low","low","mediocre","high","very high")), y=mean)) +
  geom_bar(stat="identity", width=0.5)+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=0.2)+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14))+
  theme(strip.text.x = element_text(size = 12, colour = "black", angle = 0),strip.text.y=element_text(size = 12, colour = "black", angle = 0))+
  ylab("number of correct guesses of baby sex (out of 100)")+
  xlab("Evaluation of diviner of 50% success rate (combined)")+
  geom_hline(yintercept=50, linetype="dashed", color = "red")+
  ylim(0,70)


#analysis of qualtrics data (chinese)

qual_data_chinese<-read.csv("~/psychology of chance and uncertainty/qualtrics data/transferred_data.csv")
qual_data_chinese$sex_random_guess<-(qual_data_chinese$sex_random_guess-1)*5
qual_data_chinese$boy_number<-(qual_data_chinese$boy_number-1)*5
qual_data_chinese$coin_head<-(qual_data_chinese$coin_head-1)*5
qual_data_chinese$TF_random_guess<-(qual_data_chinese$TF_random_guess-1)*5


qual_data_chinese_cleaned$divining_sex_effective[qual_data_chinese_cleaned$sex_divining_ability==1]<-"very high"
qual_data_chinese_cleaned$divining_sex_effective[qual_data_chinese_cleaned$sex_divining_ability==2]<-"high"
qual_data_chinese_cleaned$divining_sex_effective[qual_data_chinese_cleaned$sex_divining_ability==3]<-"mediocre"
qual_data_chinese_cleaned$divining_sex_effective[qual_data_chinese_cleaned$sex_divining_ability==4]<-"low"
qual_data_chinese_cleaned$divining_sex_effective[qual_data_chinese_cleaned$sex_divining_ability==5]<-"very low"

cdata <- ddply(qual_data_chinese_cleaned, c("divining_sex_effective"), summarise,
               N    = length(sex_random_guess),
               mean = mean(sex_random_guess,na.rm=TRUE),
               sd   = sd(sex_random_guess,na.rm=TRUE),
               se   = sd / sqrt(N)
)

ggplot(data=cdata, aes(x=factor(divining_sex_effective,levels = c("very low","low","mediocre","high","very high")), y=mean)) +
  geom_bar(stat="identity", width=0.5)+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=0.2)+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14))+
  theme(strip.text.x = element_text(size = 12, colour = "black", angle = 0),strip.text.y=element_text(size = 12, colour = "black", angle = 0))+
  ylab("number of correct guesses of baby sex (out of 100)")+
  xlab("Evaluation of diviner of 50% success rate (combined)")+
  geom_hline(yintercept=50, linetype="dashed", color = "red")+
  ylim(0,70)

##check if those who think 50% diviner has high ability are more likely to random guessing would have fewer successes
lm1<-lm(data=qual_data_chinese_cleaned, sex_divining_ability~sex_random_guess+sex+age)
summary(lm1) #significant (p=0.02)
#check if those who pick 50 in sex_random_guess still report above mediocre in performance evaluation
qual_data_chinese_sex_50<-qual_data_chinese[which(qual_data_chinese$sex_random_guess==50),]
summary(qual_data_chinese_sex_50$sex_divining_ability) #over 70% those who pick 50 give mediocre or above evaluation for 50% performance
##check what demographic variables are associted with "more correct" (smaller deviation from 50) guesses
qual_data_chinese$dist_from_50_sex<-abs(qual_data_chinese$sex_random_guess-50)
lm_sex<-lm(data=qual_data_chinese,dist_from_50_sex~sex+age+education)
summary(lm_sex)

qual_data_chinese$dist_from_50_TF<-abs(qual_data_chinese$TF_random-50)
lm_TF<-lm(data=qual_data_chinese,dist_from_50_TF~sex+age+education)
summary(lm_TF)

qual_data_chinese$dist_from_50_boys<-abs(qual_data_chinese$boy_number-50)
lm_boys<-lm(data=qual_data_chinese,dist_from_50_boys~sex+age+education)
summary(lm_boys)

qual_data_chinese$dist_from_50_coin<-abs(qual_data_chinese$coin_head-50)
lm_coin<-lm(data=qual_data_chinese,dist_from_50_coin~sex+age+education)
summary(lm_coin)

#test whether distributions are the same
ks_test_result <- ks.test(mturk_US$coin_head, mturk_US$boy_number)
print(ks_test_result)


#plot the violin plots

long_data <- data_4_20 %>%
  select(boy_number, sex_random_guess, coin_head, TF_random_guess) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

long_data$Variable <- factor(long_data$Variable, levels = c("boy_number", "sex_random_guess", "coin_head", "TF_random_guess"))

# Your updated ggplot code with customized y-axis breaks
ggplot(long_data, aes(x = Variable, y = Value)) +
  geom_violin(trim = FALSE, fill = "#69b3a2", color = "#2b8cbe") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
  scale_x_discrete(labels = c("boy_number" = "Number of boys",
                              "sex_random_guess" = "Number of correct guesses (fetal sex)",
                              "coin_head" = "Number of coin heads",
                              "TF_random_guess" = "Number of correct guesses (T/F questions)")) +
  scale_y_continuous(breaks = seq(0, 100, by = 10),limits = c(0, 100)) + # Customizing y-axis breaks
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Violin Plot Comparison with Custom X-axis Labels", x = "Variable", y = "Value")


#add mean and CI to violin plots
ggplot(long_data, aes(x = Variable, y = Value, color = Variable)) +
  geom_violin(trim = FALSE) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, 
               conf.int = 0.95, color = "red") +
  scale_x_discrete(labels = c("boy_number" = "Number of boys",
                              "coin_head" = "Number of coin heads",
                              "sex_random_guess" = "Number of correct guess (fetal sex)",
                              "TF_random_guess" = "Number of correct guess (T/F questions)")) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Violin Plot with 95% CI for Means", x = "Variable", y = "Value")

#4_27_2022 qualtrics data (??????????????????,??????????????????)with reversed framing

data_reversed<-read.csv("~/psychology of chance and uncertainty/qualtrics data/qualtrics_reversed.csv", fileEncoding = "UTF-8")


data_reversed$sex_random_guess<-(100-5*(data_reversed$sex_random_guess_wrong-1))# 100 minus sex_random_guess_wrong
data_reversed$TF_random_guess<-(100-5*(data_reversed$TF_random_guess_wrong-1))

hist_sex<-ggplot(data_reversed, aes(x=sex_random_guess)) + geom_histogram(boundary = 2.5, binwidth = 5, closed = "left")+
  scale_x_continuous(breaks = seq(0, 100, by=10),limits = c(0, 100))+
  aes(y=stat(count)/sum(stat(count))) + 
  scale_y_continuous(labels = scales::percent,limits = c(0, 0.5))+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16))+
  theme(axis.text.x = element_text(size = 16, angle = 45),strip.text.y=element_text(size = 12, colour = "black", angle = 0))+
  xlab("number of correct guesses (fetal sex)")+
  ylab("percentage")+
  geom_vline(aes(xintercept = mean(sex_random_guess,na.rm=TRUE)),col='red',size=0.8,linetype="dashed")
hist_sex

hist_TF<-ggplot(data_reversed, aes(x=TF_random_guess)) + geom_histogram(boundary = 2.5, binwidth = 5, closed = "left")+
  scale_x_continuous(breaks = seq(0, 100, by=10),limits = c(0, 100))+
  aes(y=stat(count)/sum(stat(count))) + 
  scale_y_continuous(labels = scales::percent,limits = c(0, 0.5))+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16))+
  theme(axis.text.x = element_text(size = 16, angle = 45),strip.text.y=element_text(size = 12, colour = "black", angle = 0))+
  xlab("number of correct guesses (TF question)")+
  ylab("percentage")+
  geom_vline(aes(xintercept = mean(TF_random_guess,na.rm=TRUE)),col='red',size=0.8,linetype="dashed")
hist_TF

data_reversed_cleaned<-data_reversed[!is.na(data_reversed$sex_divining_ability), ]

data_reversed_cleaned$sex_divining_ability_new[data_reversed_cleaned$sex_divining_ability==1]<-"five"
data_reversed_cleaned$sex_divining_ability_new[data_reversed_cleaned$sex_divining_ability==2]<-"four"
data_reversed_cleaned$sex_divining_ability_new[data_reversed_cleaned$sex_divining_ability==3]<-"three"
data_reversed_cleaned$sex_divining_ability_new[data_reversed_cleaned$sex_divining_ability==4]<-"two"
data_reversed_cleaned$sex_divining_ability_new[data_reversed_cleaned$sex_divining_ability==5]<-"one"

data_reversed_cleaned$sex_divining_ability[data_reversed_cleaned$sex_divining_ability_new=="one"]<-1
data_reversed_cleaned$sex_divining_ability[data_reversed_cleaned$sex_divining_ability_new=="two"]<-2
data_reversed_cleaned$sex_divining_ability[data_reversed_cleaned$sex_divining_ability_new=="three"]<-3
data_reversed_cleaned$sex_divining_ability[data_reversed_cleaned$sex_divining_ability_new=="four"]<-4
data_reversed_cleaned$sex_divining_ability[data_reversed_cleaned$sex_divining_ability_new=="five"]<-5


p_barplot<-ggplot(data_reversed_cleaned, aes(x = factor(sex_divining_ability))) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_x_discrete(labels=c("very low", "low","mediocre","high","very high"))+
  scale_y_continuous(labels = scales::percent,limits = c(0, 0.5))+ 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14))+
  theme(strip.text.x = element_text(size = 12, colour = "black", angle = 0),strip.text.y=element_text(size = 12, colour = "black", angle = 0))+
  xlab("performance evaluation")+
  ylab("percentage")
p_barplot

t.test(data_reversed$TF_random_guess, mu=50, alternative="two.sided")
