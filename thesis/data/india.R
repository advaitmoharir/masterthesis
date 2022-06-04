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
library(imfr)
library(datawizard)
library(varhandle)
library(ggpubr)
options(digits=16)
#Loading data

data<-WDI(country=c("AR", "BR", "CL", "CO", "EG", "HU","IN","CN",
                    "ID", "IR", "MY", "MX", "SA","PH","PL", "ZA",
                    "TH", "TR", "RU")
          ,
          indicator=c("NY.GDP.MKTP.KN", "NE.GDI.FTOT.ZS","NE.TRD.GNFS.ZS",
                      "SE.PRM.ENRR","SE.SEC.ENRR", 	
                      "FP.CPI.TOTL.ZG"),
start=1991,
end=2019)
#Changing column names
colnames(data)[4:9]<-c("gdp","inv","trade", "primenr", "secenr", "inflation")

#extrapolating primary and secondary edu, and taking log gdp
data<-data%>%
  mutate(primenr=na.approx(primenr),
         secenr=na.approx(secenr),
         gdp=log(gdp))
data<-data%>%mutate(gdp=log(gdp))
#de-meaned vars (ferman, 2019)#
data<-data%>%
group_by(iso2c)%>%
mutate_at(c(4:9),~.x-mean(.x))%>%
 ungroup()

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
                     primenr=mean(primenr,na.rm=T),
                     secenr = mean(secenr, na.rm = T)) %>%
  
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = 1991:2011, # time to use in the optimization task
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
  ) %>%
  
  # Generate the synthetic control
  generate_control()

#counterfactual plot

p1<-data_out%>%plot_trends()
#Creating a data frame storing the original and synth gdp outputs.
#This will be updated to include estimates from gsynth
output<-data_out[[6]][[1]]

#weights
tab1<-data_out%>%grab_unit_weights()


#Adding treatment variable (1 post-treatment, 0 pre-treatment)

data<-data%>%mutate(treated=ifelse(country=="India"&year>2010,1,0))

#gsynth-baseline model (no covariates)


baseline <- gsynth(gdp~treated, data = data,
              index = c("country","year"), force = "two-way",
              CV = TRUE, r = c(0, 5), se =FALSE)
baseline2 <- gsynth(gdp~treated, data = data,
                   index = c("country","year"), force="two-way",
                   CV = TRUE, r = c(0, 5), se =TRUE, ,
                   nboots=1000, inference="parametric")
#Updating output tab
output$gsynth_baseline<-baseline[["Y.ct"]]
p2<-plot(baseline, type="ct")

#gsynth with controls-unbalanced panel
#removing obs with NA

data<-data%>%drop_na()
#with all controls
control1<- gsynth(gdp~treated+trade+inv+primenr+secenr,min.T0=8, data = data,
                   index = c("country","year"),inference="parametric",force="two-way",
                   CV = TRUE, r = c(0, 5), se =TRUE, parallel=TRUE,nboots=1000, seed=09800)

p3<-plot(control1, type="ct")
p4<-plot(control1, type="gap")
#with only trade and investment
control2<- gsynth(gdp~treated+trade+inv,min.T0=8, data = data,
                  index = c("country","year"),inference="parametric",force="two-way",
                  CV = TRUE, r = c(0, 5), se =TRUE, parallel=TRUE,nboots=1000, seed=09800)

p5<-plot(control2, type="ct")
p6<-plot(control2, type="gap")

#with trade, investment and inflation
control3<- gsynth(gdp~treated+trade+inv+inflation,min.T0=8, data = data,
                  index = c("country","year"),inference="parametric",
                  CV = TRUE, r = c(0, 5), se =TRUE, parallel=TRUE,nboots=1000, seed=09800)

p7<-plot(control3, type="ct")
p8<-plot(control3, type="gap")

#Updating output df
output$control1<-control1[["Y.ct"]]
output$control2<-control2[["Y.ct"]]
output$control3<-control3[["Y.ct"]]

#Renaming columns
output<-output%>%rename(synth=synth_y,
                        gsynth_base=gsynth_baseline,
                        gsynth_ctr1=control1,
                        gsynth_ctr2=control2,
                        gsynth_ctr3=control3)
p1<-output%>%
  pivot_longer(2:5)%>%
  ggplot(aes(x=time_unit, y=value, color=name))+geom_line(size=0.8)+
  geom_vline(xintercept = 2011, linetype=1)+xlim(1991,2020)+
  scale_colour_Publication()+theme_Publication()

p2<-output%>%
  pivot_longer(2:7)%>%
  ggline(x=time_unit, y=value, color=name)
#growth rates
final<-as.data.frame(control2[c("time","Y.tr", "Y.ct")])%>%
  rename(tr=India,ct=India.1)%>%
  mutate(tr=tr-lag(tr), ct=ct-lag(ct), error=tr-ct)
###############################################################
#####Estimating tidysynth and gsynth with quarterly data########
##################################################################

#extracting data from IMFData package##


#Code for real GDP is  NGDP_R_XDC#
#setting parameters
dataimf<-imf_data(
  database_id="IFS",
  indicator="NGDP_R_SA_XDC",
  country = c("AR", "BR", "CL", "CO", "EG", "HU","IN","CN",
              "ID", "IR", "MY", "MX", "SA","PH","PL", "ZA",
              "TH", "TR", "RU"),
  start = 1996,
  end = 2019,
  freq = "Q",
  return_raw = FALSE,
  print_url = FALSE,
  times = 10
)

dataimf<-dataimf%>%
  separate(year_quarter, c("year", "qtr"))%>%
  mutate(qtr=as.numeric(factor(qtr)), 
         treated=ifelse(iso2c=="IN"&year>2010,1,0),
         year=as.numeric(year),
         year_qtr=year+(qtr/4),
         NGDP_R_SA_XDC=log(NGDP_R_SA_XDC))%>%
  rename(gdp=NGDP_R_SA_XDC)%>%
  arrange(iso2c)

#running gsynth baseline
baselineq <- gsynth(gdp~treated, data = dataimf,
                   index = c("iso2c","year_qtr"), force = "two-way",
                   CV = TRUE, r = c(0, 5), se =FALSE)

baselineq2 <- gsynth(gdp~treated, data = dataimf,
                    index = c("iso2c","year_qtr"), force = "two-way",
                    CV = TRUE, r = c(0, 5), se =TRUE, inference="parametric",
                    nboots=1000)
p10<-plot(baselineq, type="ct")
syn<-augsynth(gdp~treated,iso2c,year_qtr,dataimf,progfunc="Ridge", scm=T,fixedeff=T)
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

theme_Publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.spacing = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}