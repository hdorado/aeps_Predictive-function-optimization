
# Training and comparison of models
# Hugo Andres Dorado B.
# 04-01-2019

rm(list = ls())

library(here)
library(caret)
library(caretEnsemble)
library(randomForest)
library(nnet)
library(kernlab)
library(plyr)

load('RESULTS/Final_model.RData')

source('SCRIPTS//GBHS_functions.R')

nams <- read.csv(here::here('DATA/nams_matriz_mexico.csv'))

dataset <- read.csv('RESULTS/Process_dataset.csv')

nam_fixed <- as.character(nams[,'name_short_name'][nams$Type %in% c('S','W')]) # Names of variables to keep fixed

nam_opt <- as.character(nams[,'name_short_name'][nams$Type %in% c('M')])

names(dataset)[!(names(dataset)%in% nam_opt)] # Check that variables to optimize correspond

scale <- as.character(nams$Scale) # Define the variables scale

varsToOpt <- dataset[,nam_opt]

varsToOpt_scales <- scale[names(dataset) %in% nam_opt] # Optimization scales

ranges <- extract_ranges(varsToOpt,scale = varsToOpt_scales)

# Parameters

hms1       <- 5   # Harmony memory size
hmcr1      <- 0.85 # harmony memory considering rate
par1       <- 0.30 # pith ajusting rate
maxNumImp1 <- 30 # 1000 Maximun number of improvisations (Numero de improvisaciones debe ser grande)

# Create an empty folder to store the results

if(!dir.exists('RESULTS/IMPROVE_FITNESS')){dir.create('RESULTS/IMPROVE_FITNESS')}

# Optimizations

pb <- txtProgressBar(min = 0, max = nrow(dataset), style = 3)

test_improve <- lapply(seq(nrow(dataset)),function(w){
  ghs <-
    GHS_MM(hms =hms1,hmcr = hmcr1 , par = par1,
        maxNumImp = maxNumImp1,
        fitnessfun=fitnessfun,
        model_train=model_train,
        fixedValues=as.data.frame(dataset[w,nam_fixed]),
        ranges = ranges, 
        scales = varsToOpt_scales, 
        namFile = paste("RESULTS/IMPROVE_FITNESS/opt_ghs","_",w,'.txt',sep= ''),
        ghrp = F)
    setTxtProgressBar(pb, w)
    ghs
  }
)

close(pb)

# Results analysis

realYield <- dataset$Yield

predicYield  <- predict(model_train,dataset)

improveYield <- sapply(test_improve,function(x){x[[2]]$fitness})

real <- data.frame(data ='Real',Yield = realYield)

estimate <- data.frame( data ='Current',Yield = predicYield)

optimized <- data.frame( data ='Optimized',Yield = improveYield)

Diangostic <- rbind(real,estimate,optimized)

f <-ggplot(Diangostic,aes(x=data,y=Yield))+geom_boxplot()+theme_bw()+ylab('Yield (ton/ha)')+
  xlab('Predictors')

g <- data.frame(im = improveYield, predicYield, dif= improveYield- predicYield )

t.test(improveYield,predicYield)



gEnd <- ggplot(Diangostic[Diangostic$data!="Real",], aes(Yield, fill = data, ..count..))+
  geom_density(alpha = 0.5) +scale_fill_manual(name = "Farming practices",values=c('red','blue'))+
  theme_bw()+xlab('Yield (ton/ha)')+ylab('Number of cropping events')+
  geom_vline(xintercept = mean(predicYield),colour = 'red',linetype = "dashed")+
  geom_vline(xintercept = mean(improveYield),colour = 'blue',linetype = "dashed")

gEnd
ggsave('RESULTS/performacComp.png',f,height = 3,width = 4)

estimate <- data.frame( data =paste0('Current ','(mu=',round(mean(predicYield),2),',sd=',round(sd(predicYield),2),')'),Yield = predicYield)

optimized <- data.frame( data =paste0('Optimized ','(mu=',round(mean(improveYield),2),',sd=',round(sd(improveYield),2),')'),Yield = improveYield)

Diangostic <- rbind(real,estimate,optimized)

summary(estimate$Yield)

ggsave('RESULTS/comparation_dist.png',gEnd,height = 4,width = 6)

save(test_improve,g,file='RESULTS/test_improve.RData')

save(test_improve,g,file='RESULTS/test_improve.RData')



