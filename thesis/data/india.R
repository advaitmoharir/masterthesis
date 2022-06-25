#installing packages
library(dplyr)
library(tidysynth)
library(tidyr)
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
library(kableExtra)
library(datawizard)
library(varhandle)
library(stargazer)
library(ggpubr)
options(digits=3)
#Loading data

data<-WDI(country=c("AR", "BR", "CL","CN", "CO", "EG", "HU","IN",
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
#de-meaned vars (ferman, 2019)#

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

p1<-data_out%>%plot_differences()
#Creating a data frame storing the original and synth gdp outputs.
#This will be updated to include estimates from gsynth
output<-data_out[[6]][[1]]

#weights
#tab1<-data_out%>%grab_unit_weights()


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
data_ctr<-data%>%drop_na()

#gsynth with controls-unbalanced panel
#removing obs with NA

#p4<-plot(control1, type="gap")
#with only trade and investment
control1<- gsynth(gdp~treated+trade+inv,min.T0=8, data = data_ctr,
                  index = c("country","year"),inference="parametric",force="two-way",
                  CV = TRUE, r = c(0, 5), se =TRUE, parallel=TRUE,nboots=1000, seed=09800)

p5<-plot(control2, type="ct")
#p6<-plot(control2, type="gap")
#with all controls
control2<- gsynth(gdp~treated+trade+inv+primenr+secenr,min.T0=8, data = data_ctr,
                  index = c("country","year"),inference = "parametric",force="two-way",
                  CV = TRUE, r = c(0, 5), se =TRUE, parallel=TRUE,nboots=1000, seed=09800)


#with matrix xompletion - for Appendix
control3<- gsynth(gdp~treated+trade+inv,min.T0=8, data = data_ctr,
                  index = c("country","year"),estimator="mc",inference = "nonparametric",force="two-way",
                  CV = TRUE, r = c(0, 5), se =TRUE, parallel=TRUE,nboots=1000, seed=09800)



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
######################PLOTS#################
###########################################

#First define the publication theme

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
            panel.grid.major.x = element_blank(),
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
p1<-output%>%
  pivot_longer(2:3)%>%
  ggplot(aes(x=time_unit, y=value, color=name))+geom_line(size=0.8)+
  geom_vline(xintercept = 2011, linetype="dotted")+xlim(1991,2020)+
  scale_colour_Publication(name="", labels=c("India", "Synthetic India"))+theme_Publication()+
labs(x="",y="log(GDP)")

ggsave("figure/indiasynth.jpeg",width=6,height=4, device = "jpeg")

#MSPE Ratio plot
p2<-data_out%>%plot_mspe_ratio()+theme_Publication()+
  scale_colour_Publication()+labs(title="")
ggsave("figure/msperatio.jpeg",width=6,height=4, device = "jpeg")

#Variable weights
p2<-data_out%>%plot_weights()+theme_Publication()+labs(title="")
ggsave("figure/weights.jpeg",width=8,height=4, device = "jpeg")


#P values table

tab1<-data_out%>%grab_signficance()
#Get table in md and tex
kable(tab1[1:10,c(1,2,7)], "markdown")#for xaringan slides
kable(tab1[1:10,c(1,2,7)], "latex")# for thesis pdf
##
p3<-output%>%
  select(time_unit,real_y,gsynth_base)%>%
  pivot_longer(2:3)%>%
  ggplot(aes(x=time_unit, y=value, color=name))+geom_line(size=0.8)+
  geom_vline(xintercept = 2011, linetype="dotted")+xlim(1991,2020)+
  scale_colour_Publication(name="", labels=c("Synthetic India (GSCM)","India"))+theme_Publication()+
  labs(x="",y="log(GDP)")

ggsave("figure/indiagsynth.jpeg",width=6,height=4, device = "jpeg")

p3<-output%>%
  select(time_unit,real_y,gsynth_ctr1)%>%
  pivot_longer(2:3)%>%
  ggplot(aes(x=time_unit, y=value, color=name))+geom_line(size=0.8)+
  geom_vline(xintercept = 2011, linetype="dotted")+xlim(1991,2020)+
  scale_colour_Publication(name="", labels=c("Synthetic India (GSCM + All Covariates)","India"))+theme_Publication()+
  labs(x="",y="log(GDP)")

ggsave("figure/indiagsynthc1.jpeg",width=6,height=4, device = "jpeg")
p4<-output%>%
  select(time_unit,real_y,gsynth_ctr2)%>%
  pivot_longer(2:3)%>%
  ggplot(aes(x=time_unit, y=value, color=name))+geom_line(size=0.8)+
  geom_vline(xintercept = 2011, linetype="dotted")+xlim(1991,2020)+
  scale_colour_Publication(name="", labels=c("Synthetic India (GSCM+ Economic Covariates)","India"))+theme_Publication()+
  labs(x="",y="log(GDP)")

ggsave("figure/indiagsynthc2.jpeg",width=6,height=4, device = "jpeg")

p5<-output%>%
  select(time_unit,real_y,gsynth_ctr3)%>%
  pivot_longer(2:3)%>%
  ggplot(aes(x=time_unit, y=value, color=name))+geom_line(size=0.8)+
  geom_vline(xintercept = 2011, linetype="dotted")+xlim(1991,2020)+
  scale_colour_Publication(name="", labels=c("Synthetic India (GSCM+ Matrix Completion)","India"))+theme_Publication()+
  labs(x="",y="log(GDP)")

ggsave("figure/indiagsynthmc.jpeg",width=6,height=4, device = "jpeg")


##GAP PLOTS#
p5<-output%>%
  ggplot(aes(x=time_unit, y=real_y-synth))+geom_line(size=0.8)+
  geom_vline(xintercept = 2011, linetype="dotted")+xlim(1991,2020)+
  scale_colour_Publication()+theme_Publication()+
  labs(x="",y="gap in log(GDP)")
ggsave("figure/indiasynthgap.jpeg",width=6,height=4, device = "jpeg")

p6<-output%>%
  ggplot(aes(x=time_unit, y=real_y-gsynth_base))+geom_line(size=0.8)+
  geom_vline(xintercept = 2011, linetype="dotted")+xlim(1991,2020)+
  scale_colour_Publication()+theme_Publication()+
  labs(x="",y="gap in log(GDP)")
ggsave("figure/indiagsynthgap.jpeg",width=6,height=4, device = "jpeg")

p7<-output%>%
  ggplot(aes(x=time_unit, y=real_y-gsynth_ctr1))+geom_line(size=0.8)+
  geom_vline(xintercept = 2011, linetype="dotted")+xlim(1991,2020)+
  scale_colour_Publication()+theme_Publication()+
  labs(x="",y="gap in log(GDP)")
ggsave("figure/indiagsynthgapc1.jpeg",width=6,height=4, device = "jpeg")

p8<-output%>%
ggplot(aes(x=time_unit, y=real_y-gsynth_ctr2))+geom_line(size=0.8)+
  geom_vline(xintercept = 2011, linetype="dotted")+xlim(1991,2020)+
  scale_colour_Publication()+theme_Publication()+
  labs(x="",y="gap in log(GDP)")
ggsave("figure/indiagsynthgapc2.jpeg",width=6,height=4, device = "jpeg")

#Table of growth rate gap
diff_gr<-output%>%
  mutate_at(c(2:6), ~.x-lag(.x))%>%
  mutate(diff_s=real_y-synth,
         diff_gb=real_y-gsynth_base,
         diff_gc1=real_y-gsynth_ctr1,
         diff_gc2=real_y-gsynth_ctr2)%>%
  select(time_unit,diff_s,diff_gb,diff_gc1,diff_gc2)%>%
  filter(time_unit>2010)
#diff in growth rates

#table output
kable(diff_gr*100, "markdown")
kable(diff_gr*100, "latex")

#######################################################
#######################################################
#############ROBUSTNESS CHECKS######################33
##############################################

##Backdating

#tidysynth

data_bd <-
  data%>%
  
  # initial the synthetic control object
  synthetic_control(outcome = gdp, # outcome
                    unit = country, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "India", # unit where the intervention occurred
                    i_time = 2005, # time period when the intervention occurred
                    generate_placebos=T # generate placebo synthetic controls (for inference)
  ) %>%
  
  # Generate the aggregate predictors used to fit the weights
  
  # average log income, retail price of cigarettes, and proportion of the
  # population between 15 and 24 years of age from 1980 - 1988
  generate_predictor(time_window = 1991:2005,
                     inv = mean(inv, na.rm = T),
                     trade = mean(trade, na.rm = T),
                     primenr=mean(primenr,na.rm=T),
                     secenr = mean(secenr, na.rm = T)) %>%
  
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = 1991:2005, # time to use in the optimization task
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
  ) %>%
  
  # Generate the synthetic control
  generate_control()

bd_output<-data_bd[[6]][[1]]
p12<-  bd_output%>%
  pivot_longer(2:3)%>%
  ggplot(aes(x=time_unit, y=value, color=name))+geom_line(size=0.8)+
  geom_vline(xintercept = 2005, linetype="dotted")+xlim(1991,2020)+
  scale_colour_Publication(name="", labels=c("India", "Synthetic India"))+theme_Publication()+
  labs(x="",y="log(GDP)")
ggsave("figure/indiasynthbd.jpeg",width=6,height=4, device = "jpeg")

##backdating with gsynth
bd<-data%>%mutate(treated=ifelse(country=="India"&year>2004,1,0))

#gsynth-baseline model (no covariates)


baseline_bd <- gsynth(gdp~treated, data = bd,
                   index = c("country","year"), force = "two-way",
                   CV = TRUE, r = c(0, 5), se =FALSE)
baseline2_bd <- gsynth(gdp~treated, data = data,
                    index = c("country","year"), force="two-way",
                    CV = TRUE, r = c(0, 5), se =TRUE, ,
                    nboots=1000, inference="parametric")
#Updating output tab
bd_output$gsynth_baseline<-baseline_bd[["Y.ct"]]
p13<-bd_output%>%
  select(time_unit,real_y,gsynth_baseline)%>%
  pivot_longer(2:3)%>%
  ggplot(aes(x=time_unit, y=value, color=name))+geom_line(size=0.8)+
  geom_vline(xintercept = 2005, linetype="dotted")+xlim(1991,2020)+
  scale_colour_Publication(name="", labels=c("Synthetic India (GSCM)","India"))+theme_Publication()+
  labs(x="",y="log(GDP)")
ggsave("figure/indiagsynthbd.jpeg",width=6,height=4, device = "jpeg")

######################

#\########################################
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
                    CV = TRUE, r = c(0, 5), se =TRUE, inference="jackknife",
                    nboots=1000)
p10<-plot(baselineq, type="ct")
p11<-p10[["data"]]%>%ggplot(aes(x=time, y=outcome, color=type))+
  geom_line(size=0.8)+geom_vline(xintercept = 2011, linetype="dotted")+xlim(1996,2020)+
  scale_colour_Publication(name="", labels=c("Synthetic India", "India"))+theme_Publication()+labs(x="",y="log(GDP)")
  
ggsave("figure/indiagsynthqtr.jpeg",width=6,height=4, device = "jpeg")

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

#################NAGRAJ FIGS#############

#Reading csv
nagaraj<-read.csv("data/nagaraj.csv")
colnames(nagaraj)[6]<-"Utilites"
colnames(nagaraj)[8]<-"Hotels and restaurants"

nagaraj%>%
  filter(Year==2012)%>%
  pivot_longer(3:10)%>%
  ggplot(aes(x=name, y=value, fill=Base))+theme_Publication()+scale_colour_Publication()+
  geom_col(position="dodge")+labs(x="", y="Growth (%)")+coord_flip()
ggsave("figure/nagaraj1.jpeg", width=6, height=4)

nagaraj%>%
  filter(Year==2013)%>%
  pivot_longer(3:10)%>%
  ggplot(aes(x=name, y=value, fill=Base))+theme_Publication()+scale_colour_Publication()+
  geom_col(position="dodge")+labs(x="", y="Growth (%)")+coord_flip()
ggsave("figure/nagaraj2.jpeg", width=6, height=4)
