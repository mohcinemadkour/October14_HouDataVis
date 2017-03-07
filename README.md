# October14_HouDataVis

---
title: 'Data Exploration '
author: "MM"
date: "2/8/2017"
output: html_document
---

Adding the time cuts and adding columns for patterns of 2 and 3 combined events 

```{r}
library (dplyr)
library (ggplot2)
encounter <- read.csv("encounter.csv", stringsAsFactors = FALSE)

OP2_pattern<-matrix(NA,30367,1)
encounter= data.frame(encounter,OP2_pattern)
OP3_pattern<-matrix(NA,30367,1)
encounter= data.frame(encounter,OP3_pattern)
for (i in 1:30366) {
  
  if (encounter[i,3]==1 & encounter[i+1,3]==2){
    encounter[i+1,4]='A'
  }
  else if (encounter[i,3]==1 & encounter[i+1,3]==3){
    encounter[i+1,4]='B'
  }
  else if (encounter[i,3]==2 & encounter[i+1,3]==3){
    encounter[i+1,4]='C'
  }
  else if (encounter[i,3]==3 & encounter[i+1,3]==2){
    encounter[i+1,4]='D'
  }
  
}

for (i in 1:30365) {
  
  if (encounter[i,3]==1 & encounter[i+1,3]==2 & encounter[i+2,3]==3){
    encounter[i+1,5]='P1'
  }
  else if (encounter[i,3]==1 & encounter[i+1,3]==3 & encounter[i+2,3]==2){
    encounter[i+1,5]='P2'
  }
  
}
encounter <- mutate(encounter, 
                    enc_id = as.factor(enc_id),
                    time_to_event = as.factor(time_to_event),
                    event_type_name = ifelse(as.double(event_type) == 1, " accessing a problem", ifelse(as.double(event_type) == 2, " adding a note to a problem", ifelse(as.double(event_type) == 3, " adding a task for a problem ","other operation"))),
                    time_cat = ifelse(as.double(time_to_event) < 100, "100s",ifelse(as.double(time_to_event) < 200, "200s",ifelse(as.double(time_to_event) < 300, "300s", ifelse(as.double(time_to_event) < 400, "400s",ifelse(as.double(time_to_event) < 500, "500s", ifelse(as.double(time_to_event) < 600, "600s", ifelse(as.double(time_to_event) < 700, "700s",ifelse(as.double(time_to_event) < 800, "800s", ifelse(as.double(time_to_event) < 900, "900s",ifelse(as.double(time_to_event) < 1000, "1000s",ifelse(as.double(time_to_event) < 1100, "1100s",ifelse(as.double(time_to_event) < 1200, "1200s",ifelse(as.double(time_to_event) < 1300, "1300s",ifelse(as.double(time_to_event) < 1400, "1400s","after_1400s")))))))))))))),
                    time_cat = factor(time_cat, levels = c("100s", "200s", "300s","400s", "500s", "600s", "700s","800s", "900s","1000s","1100s","1200s","1300s","1400s","after_1400s")),
                    OP2_pattern_names = ifelse(as.character(OP2_pattern) == 'A', " Adding note after accessing a problem", ifelse(as.character(OP2_pattern) == 'B', " adding task after accessing a problem", ifelse(as.character(OP2_pattern) == 'C' , " adding a task after adding a note ",ifelse(as.character(OP2_pattern) == 'D', " adding a note after adding a task ","other patterns")))),
                    OP3_pattern_names = ifelse(as.character(OP3_pattern) == 'P1', " Access-> add note -> add task", ifelse(as.character(OP3_pattern) == 'P2', " Access-> add task -> add note", "other patterns")))

                    
summary(encounter)
```

Patterns of one event:

```{r}
ggplot(encounter, aes(x = time_cat, fill = event_type_name)) + geom_bar(position = "stack") +guides(fill = guide_legend(reverse = TRUE)) + theme_bw()+ ggtitle(" patterns formed of one event")
```

Pourcentages of patterns of one event:

```{r}
ggplot(encounter, aes(x = time_cat, fill = event_type_name)) +
  geom_bar(position = "fill") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_bw() +
  labs(x="elabsed time",y = "Percentage")+
  ggtitle("Percentage of patterns formed of one operations")
```

Patterns of two event:
```{r}
ggplot(encounter, aes(x = time_cat, fill = OP2_pattern_names)) + geom_bar(position = "stack") +guides(fill = guide_legend(reverse = TRUE)) + theme_bw()+ggtitle("patterns formed of two operations")
```

Pourcentages of patterns of two events:

```{r}
ggplot(encounter, aes(x = time_cat, fill = OP2_pattern_names)) +
  geom_bar(position = "fill") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_bw() +
  labs(x="elabsed time",y = "Percentage")+
  ggtitle("Percentage of patterns formed of two operations")
```

Patterns of three event:

```{r}
ggplot(encounter, aes(x = time_cat, fill = OP3_pattern_names)) + geom_bar(position = "stack") +guides(fill = guide_legend(reverse = TRUE)) + theme_bw()+ggtitle("patterns formed of three operations")
```

Pourcentages of patterns of three events:

```{r}
ggplot(encounter, aes(x = time_cat, fill = OP3_pattern_names)) +
  geom_bar(position = "fill") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_bw() +
  labs(x="elabsed time",y = "Percentage")+
  ggtitle("Percentage of patterns formed of three operations")
```
