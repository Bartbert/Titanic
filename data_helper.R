addFeature_Title <- function(dt)
{
  result <- dt %>%
    mutate(Title = gsub('(.*, )|(\\..*)', '', Name))
}

addFeature_Surname <- function(dt)
{
  result <- dt %>%
    mutate(Surname =  gsub('(,.*)', '', Name))
}

addFeature_FamilySize <- function(dt)
{
  result <- dt %>%
    mutate(Fsize =  SibSp + Parch + 1)
}

addFeature_FamilyTag <- function(dt)
{
  result <- dt %>%
    mutate(FamilyTag = paste(Surname, Fsize, sep='_'))
}

addFeature_DiscretizedFamilySize <- function(dt)
{
  result <- dt %>%
    mutate(FsizeD =  cut(Fsize, 
                         breaks = c(0, 1, 4, 100), 
                         labels = c("singleton", "small", "large")))
}

addFeature_Deck <- function(dt)
{
  result <- dt %>%
    mutate(Deck =  substr(Cabin, 1, 1))
}


factorize_Features <- function(dt)
{
  result <- dt %>%
    mutate(PassengerId = factor(PassengerId),
           Pclass = factor(Pclass),
           Sex = factor(Sex),
           Embarked = factor(Embarked),
           Title = factor(Title),
           Surname = factor(Surname),
           FamilyTag = factor(FamilyTag),
           FsizeD = factor(FsizeD))
  
}

