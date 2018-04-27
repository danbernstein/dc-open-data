needs(tidyverse)
needs(dplyr)
needs(plotly)
needs(tidytext)
needs(wordcloud)

rawdata <- read_csv("./raw data/Enterprise_Dataset_Inventory.csv")

dim(rawdata)
str(rawdata)
summary(rawdata)
names(rawdata)

## 4 entries did not include dataset URLS, will another three did not provide reasons for their dataset classification. However, these three were all
## classified as 'open', and a closer inspection of  all the open datasets shows that most of them do not have an explanation for their classification, they just have blank space or the word "none", so we do not need to worry about these few NAs.

incomplete <- 
  rawdata %>% 
  filter(!complete.cases(.))

View(rawdata)

dim(complete)
  
peroffice <- 
  rawdata %>% 
  count(AGENCY_NAME) 

peroffice %>% 
  top_n(5, n)

## The five offices with the most entries in the EDI 
##are The Office of the Chief Technology Officer with 221 entries, DC Public Schools (168),
## District Department of Transportation (143),
## Department of Health (109),
## and the Office of Planning (70).

mean(peroffice$n)
median(peroffice$n)

## the average office has 16.7 entries, however the distribution is right skewed, 
## with six offices each having 50 or more entries, while the median value is 5.5.

plot_ly(peroffice, 
        x = ~reorder(AGENCY_NAME, -n), 
        y = ~n, type = "bar", hoverinfo = "text",
        text = ~paste0(AGENCY_NAME, ": ", n)) %>% 
  layout(yaxis = list(title = "Number of Entries"),
         xaxis = list(title = "Agency",showticklabels = F),
         title = "Composition of the Enterprise Dataset Inventory")




## DATASET_CLASSIFICATION_NAME

perclassification <- 
  rawdata %>% 
  count(DATASET_CLASSIFICATION_NAME) 

plotly_byclassification <- 
plot_ly(perclassification, 
        x = ~reorder(DATASET_CLASSIFICATION_NAME, -n), 
        y = ~n, type = "bar", hoverinfo = "text",
        text = ~paste0(DATASET_CLASSIFICATION_NAME, ": ", n)) %>% 
  layout(yaxis = list(title = "Number of Entries"),
         xaxis = list(title = "Classification Type",showticklabels = F),
         title = "Composition of the Enterprise Dataset Inventory")

api_create(plotly_byclassification, filename = "plotly_dcEDI_byclassification")

## DC Offices reported the dataset classification for each entry, ranging from Open to Restricted Confidential. 
peroffice_byclassificationtype <- 
  rawdata %>% 
  count(AGENCY_NAME, DATASET_CLASSIFICATION_NAME) %>% 
  group_by(AGENCY_NAME) %>% 
  mutate(total = sum(n),
         percent = n/total)
 

str(peroffice_byclassificationtype)

peroffice_byclassificationtype %>% 
  ncol(4)

range(peroffice_byclassificationtype$n)
## by office and classification name ranges from 1 to 141.

ggplot_byclassificationandoffice <- 
  ggplot(peroffice_byclassificationtype, aes(x = reorder(AGENCY_NAME, -total), y = n, fill = factor(DATASET_CLASSIFICATION_NAME,
  levels = c("Open", "Public Not Proactively Released","For District Government Use","Confidential", "Restricted Confidential")),
  text = AGENCY_NAME))+
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette="YlOrRd")+
  labs(x = "Agency", y = "Number of Entries", title = "Composition of the Enterprise Dataset Inventory",
       fill = "Dataset Classification Name")+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
       axis.text.x = element_blank(),
       axis.ticks.x = element_blank(),
       panel.grid.major = element_blank(),
       panel.background = element_rect(fill = 'white', colour = 'white'))


ggplotly_byclassificationandoffice <- ggplotly(p = ggplot_byclassificationandoffice, tooltip = c("y","text")) %>% 
  layout(legend = list(x = 0.1, y = 1.0, orientation = 'h'))
api_create(ggplotly_byclassificationandoffice, filename = "plotly_dcEDI_byclassificationandoffice")

## to better understand how open the various offices are, we can normalize the data, dividing the individual classification values by
## the sum. Now we can see how numerous offices entirely contribute open datasets, while others, such as the Office of the Attorney General, only have restricted confidential datasets.

ggplot_byclassificationandoffice_normalized <- 
  ggplot(peroffice_byclassificationtype, 
         aes(x = reorder(AGENCY_NAME, -total), y = percent, fill = factor(DATASET_CLASSIFICATION_NAME,
                                                                levels = c("Open", "Public Not Proactively Released","For District Government Use","Confidential", "Restricted Confidential")),
                                             text = AGENCY_NAME))+
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette="Oranges")+
  labs(x = "Agency", y = "Percent of Agency Entries", title = "Composition of the Enterprise Dataset Inventory",
       fill = "Dataset Classification Name")+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'white'))

ggplotly_byclassificationandoffice_normalized <- 
  ggplotly(p = ggplot_byclassificationandoffice_normalized, tooltip = c("y","text")) %>% 
  layout(legend = list(x = 0.1, y = 1.0, orientation = 'h'))

api_create(ggplotly_byclassificationandoffice_normalized, filename = "plotly_dcEDI_byclassificationandoffice_normalized")

## For each dataset, the DC offices reported the reason why they are classified in that fashion. The value is a 
## character string; we can see if there are any trends in why documents are not openly released. There are plenty of 
## legitimate reasons for publically-funded data to be restricted, especially those that include 
## personally identifiable information. 

## (2) look for correlation between classification and KEYWORDS
## (3) whats in DATASET_CATEGORY?

data.tidy <-
  rawdata %>% 
  filter(DATASET_CLASSIFICATION_REASON != "",
        !is.na(DATASET_CLASSIFICATION_REASON)) %>% 
  tidytext::unnest_tokens(word, DATASET_CLASSIFICATION_REASON) 

data.count <-
  data.tidy %>% 
  anti_join(stop_words) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  dplyr::group_by(DATASET_CLASSIFICATION_NAME) %>% 
  count(word) 

for (i in 1:length(unique(data.count$DATASET_CLASSIFICATION_NAME))){
  png(paste0("wordcloud_", 
              unique(data.count$DATASET_CLASSIFICATION_NAME)[i], ".png"), width=600,height=500)
    data.count %>% 
    filter(DATASET_CLASSIFICATION_NAME == unique(data.count$DATASET_CLASSIFICATION_NAME)[i]
           ) %>% 
    with(wordcloud(word, n, c(5,.3), min.freq = 10, max.words = 50, rot.per = 0.2,
                   random.color = F, colors = gray.colors(5, start = 0.9, end = 0.3)))
    dev.off()
}  
