# Final Project: Kaggle Competition

[Final Project Kaggle page](https://www.kaggle.com/c/mas-412-final-project) is now live. 

[https://www.kaggle.com/c/mas-412-final-project](https://www.kaggle.com/c/mas-412-final-project)

## Project Description

The MAS 412 final project will be this Kaggle competition to predict the number of upvotes a New York Times article comment will receive. This estimation will be a tool that can give us a gauge on public opinion. The response variable will be `recommendations` in the training/testing `comments` file.

Submissions for the final project will be live until June 4, 2018 at 11:59 pm. 

There is also a presentation component that will happen final class period for 10 minutes each. 

-----
There has been some trouble creating submission...write commentID as a double through the tidyverse. Not sure why base R commands aren't doing the same thing.

```{r}
library(plyr)
library(dplyr)
library(tidyverse)
library(magrittr)
 
sub1 <- read_csv("~/Desktop/submission1.csv")
sub1 %<>% mutate(commentID = as.double(commentID))
write_csv(sub1,'~/Desktop/submission1-1.csv')
```
