---
title: "Data-Exploration"
output: 
  html_document:
    toc: true
    toc_depth: 2 
---

```{r}
library(readxl)
library(tidyverse)
```

## Source Data 

Source data were provided by admissions (Adam Gross) on 6/24/22 @ 10:48AM on Box.

For now the file is too big 

```{r eval = FALSE}
email_recip <- read_excel("AIM Admissions Project/2022-Email-Recip.xlsx", 
    col_types = c("text", # Person Reference ID
                  "text", # Campaign Segment -> factor
                  "text", # Person Sex -> factor (M/F/NULL)
                  "text", # Person Race -> factor 
                  "text", # Person Hispanic (Yes) -> Bool
                  "numeric", # Person SAS High School Cluster (maybe factor)
                  "numeric", # Person SAS Neighborhood Cluster (maybe factor)
                  "text", # Address - Permanent then Mailing Postal
                  "text", # Address - Permanent then Mailing Region (US = 2-letter state code; outside, full region name)
                  "text", # Address - Permanent then Mailing Country -> factor
                  "text", # Application (No response for NA), -> Bool
                  "text", # Application Round -> factor 
                  "numeric", # Application Submitted Date 
                  "numeric", # Application Need-based Aid Interest -> Bool
                  "numeric", # Application Merit Aid Interest -> Bool
                  "numeric", # Applications Merit Aid Application -> Bool
                  "text", # Person Citizenship Status
                  "text", # Organizations Type
                  "numeric", # Person Testing Plan -> Bool
                  "numeric", # Application Academic Interests
                  "numeric", # Application Activity Interests
                  "numeric", # Application Type -> factor
                  "numeric" # Person Created Date
                 )
    )
```


Load table from RDS file: 

```{r}
  email_recip <- readRDS("AIM Admissions Project/email-recip.RDS")
```

# Utilitiy Function

```{r}
# replaces a value in a source vector, returns the results
replaceValue <- function(source, valueToReplace, replacement) {
   source[ (source == valueToReplace & !is.na(source))] <- replacement
   source
}
# takes character vector and make it a logical; changes "Yes" and "No" to "1"
# and "0"
makeBool <- function(x, trueValue = "Yes", falseValue = "No"){  
x %>%
    replaceValue(trueValue, "1") %>%
    replaceValue(falseValue, "0") %>%
    as.numeric() %>%
    as.logical()

}  

# convert columnName in-place
convertColumn <- function(table, columnName, conversionFunction) {
  
  table[[columnName]] <- conversionFunction( table[[columnName]] )
  table
}
```

# Email Recipients
```{r}
tidyEmailRecipients <- 
  email_recip                                                            %>%
  convertColumn("Campaign Segment", factor)                              %>%
  convertColumn("Person Sex", factor)                                    %>%
  convertColumn("Person Hispanic", makeBool)                             %>%
  convertColumn("Address - Permanent then Mailing Country", factor)      %>%
  convertColumn("Application",
                function(x) {makeBool(x, "Submitted", "No Application") } 
               )                                                         %>%
  convertColumn("Applications Round", factor)                            %>%
  convertColumn("Application Need-based Aid Interest", makeBool)         %>%
  convertColumn("Application Merit Aid Interest", makeBool)              %>%
  convertColumn("Applications Merit Aid Application", makeBool)          %>%
  convertColumn("Person Citizenship Status", factor)                     %>%
  convertColumn("Organizations Type", factor)                            %>%
  convertColumn("Application Type", factor)                              %>%
  convertColumn("Person Testing Plan", as.logical)
```

Encode race without parenthetical definitions: 

```{r}
tidyEmailRecipients$raceCategory <-
  gsub("\\s\\(.*?\\)", "", tidyEmailRecipients$`Person Race`)
```


The 2021 Data Set has: 

* **`r nrow(tidyEmailRecipients)`** Total Applications 
* Of which **`r filter(tidyEmailRecipients, Application) %>% nrow`** were submitted to W&L (`r round(sum(tidyEmailRecipients$Application)/nrow(tidyEmailRecipients) * 100, 2)`%)
* **`r summary(tidyEmailRecipients[['Application Type']])['app_questbridge']`** QuestBridge Applications
* **`r summary(tidyEmailRecipients[['Application Type']])['commonapp']`** Common App Applications

## Geographic Demographics

## Academic Interest 

## Race Demographics

A much larger proportion of non-applicants left `Person Race` blank, than W&L applicants:

* Total `r sum( is.na(tidyEmailRecipients['Person Race']) )` / `r nrow(tidyEmailRecipients)` = `r round( sum( is.na(tidyEmailRecipients['Person Race']) ) / nrow(tidyEmailRecipients), 3) * 100`%.
* W&L: `r sum( is.na( filter(tidyEmailRecipients, Application)['Person Race']) )` / `r nrow( filter(tidyEmailRecipients, Application))` = `r round( sum( is.na( filter(tidyEmailRecipients, Application)['Person Race']) ) / nrow( filter(tidyEmailRecipients, Application)), 3) * 100`%.


## All Applications (W&L or Not)
```{r}
tidyEmailRecipients                                                                          %>%
  filter( !is.na(`Person Race`), Application )                                               %>%
  count(raceCategory)                                                                        %>%
  arrange(desc(n))                                                                           %>%
  mutate(raceCategory = factor(raceCategory, levels = unique(raceCategory), ordered = TRUE)) %>%
  slice(1:6)                                                                                 %>%
  ggplot( aes( raceCategory, n)) + 
    geom_bar(stat = 'identity') +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
  
```


## Sex Demographics 

## QuestBridge vs. Non-QuestBridge
                                                    
##Number of Applicant's From Respective Countries

```{r}
tidyEmailRecipients$`Country` <- factor(tidyEmailRecipients$`Address - Permanent then Mailing Country`)
totals <- tidyEmailRecipients %>%
  filter( !is.na(`Address - Permanent then Mailing Country`)) %>%
  count(`Address - Permanent then Mailing Country`) %>%
  arrange(desc(n))
```


```{r}
tidyEmailRecipients%>%
  count (`Address - Permanent then Mailing Country`, Application) %>%
  mutate(Country = factor (`Address - Permanent then Mailing Country`, levels = totals$`Address - Permanent then Mailing Country`[1:10], ordered = TRUE)) %>%
  filter(Country != "United States") %>%
  ggplot (aes (Country, n, fill = Application)) +
  geom_bar(stat= 'identity')
```

```{r}
goAppProp <-
  tidyEmailRecipients %>%
  count(Country, Application)
goAppProp
  
View(goAppProp)
```

```{r}
  spread(goAppProp, Application, n)
intergoAppProp <- spread(goAppProp, Application, n)
intergoAppProp$total <- intergoAppProp[['TRUE']]+intergoAppProp[['FALSE']]
arrange(intergoAppProp, desc(total))
intergoAppProp$tpct <- intergoAppProp[['TRUE']] / intergoAppProp$total
intergoAppProp$fpct <- intergoAppProp[['FALSE']] / intergoAppProp$total  
View(intergoAppProp)
```

```{r}
finalGoAppProp <- 
  gather(intergoAppProp, Application, "Percentage", tpct:fpct)  
finalGoAppProp
```

```{r}
finalGoAppProp %>%
  filter( !is.na(Application)) %>%
  mutate(Country = factor (Country, levels = totals$`Address - Permanent then Mailing Country`[1:35], ordered = TRUE)) %>%
  filter(Country != "United States") %>%
  ggplot(aes(Country, Percentage, fill = Application )) +
    geom_bar(stat = 'identity') +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
```





###Proportions with Gender and Application 
```{r}
genderAppProp <- tidyEmailRecipients %>%
  filter( !is.na(`Application Type`)) %>%
  filter( !is.na(`Person Sex`)) %>%
  count(`Application Type`, `Person Sex`)

View(genderAppProp)
```



```{r}
interGenderAppProp <- 
  tidyEmailRecipients                     %>%
  filter( !is.na(`Application Type`))     %>%
  filter( !is.na(`Person Sex`))           %>%
  count(`Application Type`, `Person Sex`) %>%
  spread(`Person Sex`, n,)
interGenderAppProp$Sum <- rowSums(interGenderAppProp[c("F", "M")])
interGenderAppProp$Mpct <- round(100 * interGenderAppProp$M / interGenderAppProp$Sum, 2)
interGenderAppProp$Fpct <- round(100 * interGenderAppProp$F / interGenderAppProp$Sum, 2)
interGenderAppProp$tpct <- round((100 * interGenderAppProp$M / interGenderAppProp$Sum) + (100 * interGenderAppProp$F / interGenderAppProp$Sum), 2)
View(interGenderAppProp)
```

```{r}
finalinterGenderAppProp <-                                   
  gather(interGenderAppProp, key = "Person Sex", value = "Percentages", Mpct : Fpct)  
finalinterGenderAppProp
View(finalinterGenderAppProp)
```

```{r}
tidyEmailRecipients %>%
  filter( !is.na(`Person Sex`) ) %>%
  count(`Person Sex`, `Application Type`) %>%
  spread(key = `Person Sex`, value = n) %>%
  mutate(total = F + M, Fpct = F / total, Mpct = M / total) %>%
  gather(key = "sex", value = "pct", Fpct:Mpct) %>%
  ggplot(aes(`Application Type`, pct, fill = sex)) +
    geom_bar(stat = 'identity')
