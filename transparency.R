
# Getting the transparency international data
u16 <- "http://files.transparency.org/content/download/2060/13252/file/CPI2016_FullDataSetWithRegionalTables.xlsx"
u15<-"http://files.transparency.org/content/download/1950/12812/file/2015_CPI_DataMethodologyZIP.zip"
u14<-"http://files.transparency.org/content/download/1857/12438/file/CPI2014_DataBundle.zip"
u13<-"http://files.transparency.org/content/download/702/3015/file/CPI2013_DataBundle.zip"
u12<-"http://files.transparency.org/content/download/533/2213/file/2012_CPI_DataPackage.zip"



download.file(url=u16, destfile="e16.xlsx")
download.file(url=u15, destfile="z15.zip")
download.file(url=u14, destfile="z14.zip")
download.file(url=u13, destfile="z13.zip")
download.file(url=u12, destfile="z12.zip")

unzip("z15.zip")
unzip("z14.zip")
unzip("z13.zip")
unzip("z12.zip")

library(readxl)
library(gdata)

ti16 <- read_excel("e16.xlsx")
data16 <- ti16[,c(1,2,6:18)]
names(data16)<- c("country", "transparency_score", "World.Bank.CPIA",
                              "World.Economic.Forum.EOS", "Global.Insight.Country.Risk.Ratings",
                              "Bertelsmann.Foundation.Transformation.Index", "African.Development.Bank.CPIA",
                              "IMD.World.Competitiveness.Yearbook", "Bertelsmann.Foundation.Sustainable.Governance.Index",
                              "World.Justice.Project.Rule.of.Law.Index", "PRS.International.Country.Risk.Guide",       
                              "Varities.of.Democracy.Project", "Economist.Intelligence.Unit.Country.Ratings",
                              "Freedom.House.Nations.in.Transit.Ratings", "PERC.Asia.Risk.Guide")
ti15 <- read_excel("Data & methodology/Data/CPI 2015_data.xlsx", skip=1)
data15 <- ti15[,c(3,2,6,7,15,8,9,10,11,12,13,14,17,16)]
names(data15)<- c("country", "transparency_score", "World.Bank.CPIA",
                  "World.Economic.Forum.EOS", "Global.Insight.Country.Risk.Ratings",
                  "Bertelsmann.Foundation.Transformation.Index", "African.Development.Bank.CPIA",
                  "IMD.World.Competitiveness.Yearbook", "Bertelsmann.Foundation.Sustainable.Governance.Index",
                  "World.Justice.Project.Rule.of.Law.Index", "PRS.International.Country.Risk.Guide",       
                  "Economist.Intelligence.Unit.Country.Ratings","Freedom.House.Nations.in.Transit.Ratings",
                  "PERC.Asia.Risk.Guide")
ti14 <- read_excel("CPI2014_DataBundle/CPI 2014_Regional with data source scores_final.xlsx", skip=2)
data14 <- ti14[,c(2,6,18,19,22,15,13,16,14,20,17,21,24,23)]
names(data14)<- c("country", "transparency_score", "World.Bank.CPIA",
                  "World.Economic.Forum.EOS", "Global.Insight.Country.Risk.Ratings",
                  "Bertelsmann.Foundation.Transformation.Index", "African.Development.Bank.CPIA",
                  "IMD.World.Competitiveness.Yearbook", "Bertelsmann.Foundation.Sustainable.Governance.Index",
                  "World.Justice.Project.Rule.of.Law.Index", "PRS.International.Country.Risk.Guide",       
                  "Economist.Intelligence.Unit.Country.Ratings","Freedom.House.Nations.in.Transit.Ratings",
                  "PERC.Asia.Risk.Guide")
ti13 <- read.xls("CPI2013_DataBundle/CPI2013_GLOBAL_WithDataSourceScores.xls", skip=1, stringsAsFactors=FALSE, na.strings=0)
data13 <- ti13[,c(2,7,19,20,23,16,14,17,15,21,18,22,26,24,25)]
names(data13)<- c("country", "transparency_score", "World.Bank.CPIA",
                  "World.Economic.Forum.EOS", "Global.Insight.Country.Risk.Ratings",
                  "Bertelsmann.Foundation.Transformation.Index", "African.Development.Bank.CPIA",
                  "IMD.World.Competitiveness.Yearbook", "Bertelsmann.Foundation.Sustainable.Governance.Index",
                  "World.Justice.Project.Rule.of.Law.Index", "PRS.International.Country.Risk.Guide",       
                  "Economist.Intelligence.Unit.Country.Ratings","Freedom.House.Nations.in.Transit.Ratings",
                  "PERC.Asia.Risk.Guide", "Transparency.International.Bribe.Payers.Survey")

ti12 <- read.xls("2012_CPI_DataPackage/CPI2012_Results.xls", skip=1, stringsAsFactors=FALSE, na.strings=0)
data12 <- ti12[,c(2,4,17,18,21,14,12,15,13,19,16,20,24,22,23)]
names(data12)<- c("country", "transparency_score", "World.Bank.CPIA",
                  "World.Economic.Forum.EOS", "Global.Insight.Country.Risk.Ratings",
                  "Bertelsmann.Foundation.Transformation.Index", "African.Development.Bank.CPIA",
                  "IMD.World.Competitiveness.Yearbook", "Bertelsmann.Foundation.Sustainable.Governance.Index",
                  "World.Justice.Project.Rule.of.Law.Index", "PRS.International.Country.Risk.Guide",       
                  "Economist.Intelligence.Unit.Country.Ratings","Freedom.House.Nations.in.Transit.Ratings",
                  "PERC.Asia.Risk.Guide", "Transparency.International.Bribe.Payers.Survey")
rm(ti12,ti13,ti14,ti15,ti16)

# putting it to use
library(dplyr)
library(tidyr)
library(ggplot2)

long16 <- data16 %>% gather(measure, score, 3:15) %>% mutate(year=2016)
long15 <- data15 %>% gather(measure, score, 3:14) %>% mutate(year=2015)
long14 <- data14 %>% gather(measure, score, 3:14) %>% mutate(year=2014)
long13 <- data13 %>% gather(measure, score, 3:15) %>% mutate(year=2013)
long12 <- data12 %>% gather(measure, score, 3:15) %>% mutate(year=2012)

transparent <- bind_rows(long16,long15,long14,long13,long12) %>% filter(!is.na(score))

transparent %>% filter(year > 2014) %>% group_by(year, country) %>%
  summarise(mscr = mean(score)) %>% ggplot(aes(x=mscr)) +
  geom_histogram(bins=10) + facet_wrap(~year) + ggtitle("2016 with new source")

transparent %>% filter(year > 2014, measure != "Varities.of.Democracy.Project") %>% group_by(year, country) %>%
  summarise(mscr = mean(score)) %>% ggplot(aes(x=mscr)) +
  geom_histogram(bins=10) + facet_wrap(~year) + ggtitle("2016 without new source")
## looks like some of the movement caused by introducing a new source


transparent %>% group_by(year, country) %>%
  summarise(mscr = mean(score)) %>% group_by(year) %>%
  summarise(prop_below = sum(mscr < 45))

transparent %>% filter(measure != "Varities.of.Democracy.Project") %>%
  group_by(year, country) %>%
  summarise(mscr = mean(score)) %>% group_by(year) %>%
  summarise(prop_below = sum(mscr < 45))

transparent %>% filter(country=="New Zealand", measure != "Varities.of.Democracy.Project") %>%
  ggplot(aes(x=year, y=score, colour=as.factor(measure))) +
  geom_line()

###### better or worse with new source
transparent %>% group_by(country) %>%
  summarise(mscr15 = sum(score * as.numeric(year==2015))/sum(as.numeric(year==2015)),
            mscr16 = sum(score * as.numeric(year==2016))/sum(as.numeric(year==2016)),
            mscr16old = sum(score * as.numeric(year==2016 & measure != "Varities.of.Democracy.Project"))/
              sum(as.numeric(year==2016 & measure != "Varities.of.Democracy.Project")),
            diffnew =  mscr16 - mscr15,
            diffold =  mscr16old - mscr15
            ) %>% ungroup %>% summarise(
              old_worse = sum(diffold < 0 & !is.na(diffold)),
              old_same = sum(diffold == 0 & !is.na(diffold)),
              old_better = sum(diffold > 0 & !is.na(diffold)),
              new_worse = sum(diffnew < 0 & !is.na(diffnew)),
              new_same = sum(diffnew == 0 & !is.na(diffnew)),
              new_better = sum(diffnew > 0 & !is.na(diffnew)),
              prop_worse_old = old_worse / (old_worse + old_same + old_better),
              prop_worse_new = new_worse / (new_worse + new_same + new_better)
            )
###### better or worse with new source, rounded
transparent %>% group_by(country) %>%
  summarise(mscr15 = round(sum(score * as.numeric(year==2015))/sum(as.numeric(year==2015)),0),
            mscr16 = round(sum(score * as.numeric(year==2016))/sum(as.numeric(year==2016)),0),
            mscr16old = round(sum(score * as.numeric(year==2016 & measure != "Varities.of.Democracy.Project"))/
              sum(as.numeric(year==2016 & measure != "Varities.of.Democracy.Project")),0),
            diffnew =  mscr16 - mscr15,
            diffold =  mscr16old - mscr15
  ) %>% ungroup %>% summarise(
    old_worse = sum(diffold < 0 & !is.na(diffold)),
    old_same = sum(diffold == 0 & !is.na(diffold)),
    old_better = sum(diffold > 0 & !is.na(diffold)),
    new_worse = sum(diffnew < 0 & !is.na(diffnew)),
    new_same = sum(diffnew == 0 & !is.na(diffnew)),
    new_better = sum(diffnew > 0 & !is.na(diffnew)),
    prop_worse_old = old_worse / (old_worse + old_same + old_better),
    prop_worse_new = new_worse / (new_worse + new_same + new_better)
  )

###### amount of movement, rounded data
transparent %>% group_by(country) %>%
  summarise(mscr15 = round(sum(score * as.numeric(year==2015))/sum(as.numeric(year==2015)),0),
            mscr16 = round(sum(score * as.numeric(year==2016))/sum(as.numeric(year==2016)),0),
            mscr16old = round(sum(score * as.numeric(year==2016 & measure != "Varities.of.Democracy.Project"))/
                                sum(as.numeric(year==2016 & measure != "Varities.of.Democracy.Project")),0),
            diffnew =  mscr16 - mscr15,
            diffold =  mscr16old - mscr15
  ) %>% ungroup %>% summarise(
    movenew = sum(abs(diffnew), na.rm=TRUE),
    moveold = sum(abs(diffold), na.rm=TRUE)
  )

