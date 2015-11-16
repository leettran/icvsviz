df <- data.frame(
  gender = c("male","male","female","female","other","other"),
  age = c("young","old","young","old","young","old"),
  count = round(rnorm(6, 30, 5))
)

df %>% group_by(gender) %>%
  summarise(sum(count))

icvs.d.old <- icvs %>%
  rename(D.Age = D0010, D.HouseType = D0030, D.Occupation = D0050) %>%
  group_by(Country, Year, D.Age, D.HouseType, D.Occupation) %>%
  summarise(count = n())

icvs.2010.d <- icvs.2010 %>%
  rename(D.Age = D0010, D.HouseType = D0030, D.Occupation = D0050) %>%
  group_by(Country, Year, D.Age, D.HouseType, D.Occupation) %>%
  summarise(count = n())

icvs.d <- rbind(icvs.d.old, icvs.2010.d)

icvs.d %>%
  group_by(Country, Year) %>%
  summarise(count = sum(count))


df.group %>%
  group_by(.) %>%
  summarise(count = sum(count))
?group_by

df.group <- icvs.d[,c("Country","Year",perccol)]

perccol <- "D.Age"

GetPercents <- function(df, perccol) {
  df.net <- df %>%
    group_by(Country, Year) %>%
    summarise(count.net = sum(count))

  df.group <- df %>%
    group_by_(~Country, ~Year,  as.name(perccol)) %>%
    summarise(count.d = sum(count))

  df.final <- inner_join(df.group,df.net, by = c("Country","Year"))
  df.final <- df.final %>%
    mutate(Perc = count.d/count.net) %>%
    select(Country,Year,D.Age,Perc)
  
  return(df.final)
}

GetPercents(icvs.d, "D.Age")
  
icvs.d %>%
  group_by_(~Country, ~Year,  as.name(perccol)) %>%
  summarise(count.d = sum(count))  

library(lazyeval)

icvs %>%
  group_by(Country, Year, D0010) %>%
  summarise(count = n())

icvs %>%
  group_by(Country, Year) %>%
  summarise(count = n())

vignette("nse")

icvs.d %>% group_by_(~Country, ~Year,  as.name(perccol)) %>%
  summarise(count = sum(count))
                     
#,Year, as.name(perccol))

p2 <- as.name(perccol)
class(p2)

icvs$ Trim()
