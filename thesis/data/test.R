#installing packages
library(tidyverse)
library(dplyr)
library(tidysynth)
library(ggplot2)
library(WDI)
library(readxl)
library(haven)
library(pwt10)
library(zoo)
library(augsynth)
library(gsynth)
library(synthdid)
library(panelView)
library(datawizard)
#Loading data

data<-WDI(country=c("AR", "BR", "CL", "CO", "EG", "HU","IN",
                    "ID", "IR", "MY", "MX", "SA","PH","PL", "ZA",
                    "TH", "TR", "RU")
          ,
          indicator=c("NY.GDP.MKTP.KN","NE.GDI.FTOT.ZS","NE.TRD.GNFS.ZS",
                      "SE.PRM.ENRR","SE.SEC.ENRR", 	
                      "FP.CPI.TOTL.ZG"),
start=1991,
end=2019)
colnames(data)[4:9]<-c("gdp","inv","trade", "primenr", "secenr", "inflation")

data<-data%>%
  mutate(primenr=na.approx(primenr),
         secenr=na.approx(secenr),
         gdp=log(gdp))
 
#de-meaned vars (ferman, 2019)#
data<-data%>%
  group_by(iso2c)%>%
  mutate_at(c(4:9),~.x-mean(.x))%>%
  ungroup()


#############Conflict and democracy variables acemoglu########
ddcg<-read_dta("DDCGdata_final.dta")
ddcg<-ddcg%>%select(country_name,year,taxratio,)


#######################################################
# ESTIMATION
########################################################

# 'vanilla' synthetic control (Abadie et al 2010)


data_out <-
  data%>%
  
  # initial the synthetic control object
  synthetic_control(outcome = gdp, # outcome
                    unit = country, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "India", # unit where the intervention occurred
                    i_time = 2011, # time period when the intervention occurred
                    generate_placebos=T # generate placebo synthetic controls (for inference)
  ) %>%
  
  # Generate the aggregate predictors used to fit the weights
  
  # average log income, retail price of cigarettes, and proportion of the
  # population between 15 and 24 years of age from 1980 - 1988
  generate_predictor(time_window = 1991:2011,
                     inv = mean(inv, na.rm = T),
                     trade = mean(trade, na.rm = T),
                     primenr = mean(primenr, na.rm = T),
                     secenr=mean(secenr, na.rm=T)) %>%
  
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = 1991:2011, # time to use in the optimization task
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
  ) %>%
  
  # Generate the synthetic control
  generate_control()

#counterfactual plot

data_out%>%plot_trends()


#augsynth

data<-data%>%mutate(treated=ifelse(country=="India"&year>2010,1,0))
asyn<-augsynth(gdp~treated,iso2c,year,data,progfunc="Ridge",scm=TRUE,fixedeff=T)
plot(asyn)



#gsynth-baseline model


baseline <- gsynth(gdp~treated, data = data,
              index = c("country","year"), force = "two-way",
              CV = TRUE, r = c(0, 5), se =FALSE)
#baseline with EM and MC methods

EM <- gsynth(gdp~treated, data = data,
                   index = c("country","year"), EM=TRUE, force = "two-way",
                   CV = TRUE, r = c(0, 5), se =FALSE)

MC <- gsynth(gdp~treated, data = data,
                   index = c("country","year"),estimator="mc", force = "two-way",
             inference="nonparametric", nboots=500,CV = TRUE, r = c(0, 5), se =TRUE)
#controls-unbalanced panel

#removing obs with NA

data<-data%>%drop_na()
control1<- gsynth(gdp~treated+trade+primenr+secenr+inflation,min.T0=8, data = data,
                   index = c("country","year"),inference="parametric",
                   CV = TRUE, r = c(0, 5), se =TRUE, parallel=TRUE,nboots=1000, seed=09800)

plot(control1, type="ct")
plot(control1, type="gap")

#############################################################################
#PLOTS
#################################################3


#tidysynth
data("smoking")

smoking_out <-
  
  smoking %>%
  
  # initial the synthetic control object
  synthetic_control(outcome = cigsale, # outcome
                    unit = state, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "California", # unit where the intervention occurred
                    i_time = 1988, # time period when the intervention occurred
                    generate_placebos=T # generate placebo synthetic controls (for inference)
  ) %>%
  
  # Generate the aggregate predictors used to fit the weights
  
  # average log income, retail price of cigarettes, and proportion of the
  # population between 15 and 24 years of age from 1980 - 1988
  generate_predictor(time_window = 1980:1988,
                     ln_income = mean(lnincome, na.rm = T),
                     ret_price = mean(retprice, na.rm = T),
                     youth = mean(age15to24, na.rm = T)) %>%
  
  # average beer consumption in the donor pool from 1984 - 1988
  generate_predictor(time_window = 1984:1988,
                     beer_sales = mean(beer, na.rm = T)) %>%
  
  # Lagged cigarette sales 
  generate_predictor(time_window = 1975,
                     cigsale_1975 = cigsale) %>%
  generate_predictor(time_window = 1980,
                     cigsale_1980 = cigsale) %>%
  generate_predictor(time_window = 1988,
                     cigsale_1988 = cigsale) %>%
  
  
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = 1970:1988, # time to use in the optimization task
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
  ) %>%
  
  # Generate the synthetic control
  generate_control()

p1<-smoking_out%>%plot_trends()+
  labs(title="",y="per-capita cigarette sales (in packs)")+
  theme_classic()
ggsave("caltrends.jpg", width=8, height=4)

p2<-smoking_out%>%plot_placebos()+
  labs(title="",y="gap in per-capita cigarette sales (in packs)")+
  theme_classic()
ggsave("calplacebo.jpg", width=8, height=4)

p3<-smoking_out%>%plot_mspe_ratio()+
  labs(title="")+
  theme_classic()
ggsave("calmspe.jpg", width=8, height=4)

tab<-smoking_out%>%grab_signficance()
  
kable(tab[1:10,c(1,2,7)], "latex")

#synthdid
data('california_prop99')
setup = panel.matrices(california_prop99)
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)
plot(tau.hat)

