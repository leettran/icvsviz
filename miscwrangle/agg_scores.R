library(lazyeval)

Clean.PoliceReporting <- function(df, df2, scorecol) {
  df.rest <- df %>% 
    select_(~Year, ~Country, scorecat = interp(~x, x = as.name(scorecol))) %>% 
    group_by(Year, Country, scorecat) %>% 
    summarise(count = n())
  
  df.2010 <- df2 %>%
    select_(~Year, ~Country, scorecat = interp(~x, x = as.name(scorecol))) %>% 
    group_by(Year, Country, scorecat) %>% 
    summarise(count = n())
  
  df.net <- data.frame(rbind(df.rest, df.2010))
  df.net <- df.net[!is.na(df.net$Country),]
  df.net$scorecat <- as.character(df.net$scorecat)
  
  df.net$scorecat[df.net$scorecat == "yes"] <- 1
  df.net$scorecat[df.net$scorecat == "no"] <- 0
  df.net$scorecat[!(df.net$scorecat %in% 0:1)] <- NA
  
  df.final <- data.frame(GetPercents(df.net[!is.na(df.net$scorecat),], "scorecat"))
  
  df.final <- df.final %>% 
    filter(scorecat == 1) %>%
    rename(Score = Perc) %>%
    mutate(Question = qtrans$Name[qtrans$Id == scorecol]) %>%
    select(Year, Country, Question, Score)
  
  return(df.final)
}

GetPercents <- function(df, perccol) {
  df.net <- df %>%
    group_by(Country, Year) %>%
    summarise(count.net = sum(count))
  
  df.group <- df %>%
    group_by_(~Country, ~Year,  interp(~x, x = as.name(perccol))) %>%
    summarise(count.d = sum(count))
  
  df.final <- inner_join(df.group,df.net, by = c("Country","Year"))
  
  df.final <- df.final %>%
    mutate(Perc = count.d/count.net) %>%
    select_(~Country,~Year,interp(~x, x = as.name(perccol)),~Perc)
  
  return(df.final)
}

C12B400 <- Clean.PoliceReporting(icvs, icvs.2010, "C11B400")
saveRDS(C12B400, "datasets/C12B400.rds")
Clean.PoliceReporting(icvs, icvs.2010, "C11B400")

qtrans <- data.frame(
  Id = c("C01B400","C02B400","C03B400","C04B400","C05B400",
         "C06B400","C09B400","C11B400","C12B400","C13B400"),
  Name = c("Car Theft","Theft from Car","Car Vandalism","Theft of Motorcycle","Bicycle Theft",
           "Burglary","Robbery","Sexual Offenses","Assault","Consumer Fraud")
)

# 1-3: C02B400 (Theft from Car), C05B400 (Bicycle Theft), 

icvs %>% select(Country, Year, C13B400) %>% distinct()
icvs %>% select(Country, Year, C13B400) %>% filter(Country == "USA") %>% table(.)

icvs %>% select(C12B400) %>% table(.)
icvs.2010 %>% select(C12B400) %>% table(.)

icvs$C02B400[icvs$C02B400 == 1] <- "yes"
icvs$C02B400[icvs$C02B400 == 2] <- "no"
icvs$C02B400[!(icvs$C02B400 %in% c("yes","no"))] <- NA
icvs.2010$C02B400[icvs.2010$C02B400 == 1] <- "yes"
icvs.2010$C02B400[icvs.2010$C02B400 == 2] <- "no"
icvs.2010$C02B400[!(icvs.2010$C02B400 %in% c("yes","no"))] <- NA

icvs$C05B400[icvs$C05B400 == 1] <- "yes"
icvs$C05B400[icvs$C05B400 == 2] <- "no"
icvs$C05B400[!(icvs$C05B400 %in% c("yes","no"))] <- NA
icvs.2010$C05B400[icvs.2010$C05B400 == 1] <- "yes"
icvs.2010$C05B400[icvs.2010$C05B400 == 2] <- "no"
icvs.2010$C05B400[!(icvs.2010$C05B400 %in% c("yes","no"))] <- NA

icvs$C12B400[icvs$C12B400 == 1] <- "yes"
icvs$C12B400[icvs$C12B400 == 2] <- "no"
icvs$C12B400[!(icvs$C12B400 %in% c("yes","no"))] <- NA
icvs.2010$C12B400[icvs.2010$C12B400 == 1] <- "yes"
icvs.2010$C12B400[icvs.2010$C05B400 == 2] <- "no"
icvs.2010$C12B400[!(icvs.2010$C12B400 %in% c("yes","no"))] <- NA

ICVS.results

names(df.net)
df.net <- df.net %>% mutate()

icvs.2010.breakin <- icvs.2010.breakin[!is.na(icvs.2010.breakin$Country),]
icvs.breakin <- data.frame(rbind(icvs.breakin, icvs.2010.breakin))
icvs.breakin$Score <- Trim(as.character(icvs.breakin$Score))

icvs.breakin %>% data.frame(.) %>% select(Year) %>% distinct()
icvs.breakin %>% data.frame(.) %>% select(Score) %>% distinct()
icvs.breakin %>% data.frame(.) %>% select(Country) %>% distinct()

icvs.breakin$Score[icvs.breakin$Score == "very likely"] <- 1
icvs.breakin$Score[icvs.breakin$Score == "likely"] <- 2
icvs.breakin$Score[icvs.breakin$Score == "not very likely"] <- 3
icvs.breakin$Score[icvs.breakin$Score %in% c("unknown","refusal")] <- NA

icvs.breakin <- icvs.breakin %>% mutate(Score = as.numeric(Score) * n..)

icvs.breakin <- icvs.breakin %>% 
  filter(!is.na(Score)) %>%
  group_by(Year, Country) %>%
  summarise(Score = sum(Score), Counts = sum(n..)) %>%
  mutate(Score = Score / Counts)

icvs.breakin <- icvs.breakin %>%
  select(Year, Country, Score) %>%
  mutate(Question = "S0040")



df.2010
df
