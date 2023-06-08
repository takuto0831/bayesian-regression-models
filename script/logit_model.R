# package
library(tidyverse)
library(mlmRev)
library(skimr)

# data
data(Contraception)
df <- Contraception

#check
glimpse(df)
skim(df)

# logit model
glm_model <- glm(use ~ urban + age + livch,
                 data = df,
                 family = "binomial") 
summary(glm_model)
coef_model <- coef(glm_model)

df$use_pred_by_func <- predict(glm_model,newdata = df, type = "response")

# predict by manual

func <- function(urban, age, livch){
  tmp<- coef_model[1] + 
    coef_model[2] * if_else(urban == 'Y',1,0) +
    coef_model[3] * age +
    case_when(livch == '0' ~ 0,
              livch == '1' ~ coef_model[4],
              livch == '2' ~ coef_model[5],
              livch == '3+' ~ coef_model[6])
  return(1 / (1 + exp(-tmp)))
}
# test
func(df[1,]$urban, df[1,]$age, df[1,]$livch)

# add column
df <- df |> 
  mutate(use_pred_by_manual = func(urban, age, livch))

# check
df |> 
  ggplot(aes(x=use_pred_by_func,y=use_pred_by_manual)) +
  geom_point() +
  theme_minimal()

