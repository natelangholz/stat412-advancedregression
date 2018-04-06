# Packages
library(tidyverse)  # data manipulation and visualization
library(modelr)     # provides easy pipeline modeling functions
library(broom)      # helps to tidy up model outputs

# Load data 
(default <- as_tibble(ISLR::Default))


set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(default), replace = T, prob = c(0.6,0.4))
train <- default[sample, ]
test <- default[!sample, ]


model1 <- glm(default ~ balance, family = "binomial", data = train)


default %>%
  mutate(prob = ifelse(default == "Yes", 1, 0)) %>%
  ggplot(aes(balance, prob)) +
  geom_point(alpha = .15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Logistic regression model fit") +
  xlab("Balance") +
  ylab("Probability of Default")

summary(model1)

exp(coef(model1))

confint(model1)

predict(model1, data.frame(balance = c(1000, 2000)), type = "response")

model2 <- glm(default ~ student, family = "binomial", data = train)

tidy(model2)

predict(model2, data.frame(student = factor(c("Yes", "No"))), type = "response")

model3 <- glm(default ~ ., family = "binomial", data = train)
tidy(model3)

model4 <- glm(default ~ balance + income + student, family = "binomial", data = train)
tidy(model4)

caret::varImp(model3)

new.df <- tibble(balance = 1500, income = 40, student = c("Yes", "No"))
predict(model3, new.df, type = "response")

anova(model1, model3, test = "Chisq")

list(model1 = pscl::pR2(model1)["McFadden"],
     model2 = pscl::pR2(model2)["McFadden"],
     model3 = pscl::pR2(model3)["McFadden"])

model1_data <- augment(model1) %>% 
  mutate(index = 1:n())

ggplot(model1_data, aes(index, .std.resid, color = default)) + 
  geom_point(alpha = .5) +
  geom_ref_line(h = 3)

model1_data %>% 
  filter(abs(.std.resid) > 3)

plot(model1, which = 4, id.n = 5)

model1_data %>% 
  top_n(5, .cooksd)



