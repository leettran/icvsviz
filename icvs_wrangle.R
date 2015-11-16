library(foreign)
library(dplyr)

Trim <- function(vec) {
  newvec <- gsub("^\\s*", "", vec)
  gsub("\\s*$", "", newvec)
}

shiny::runApp(host="0.0.0.0",port=3168)
setwd("Documents/Learning/NYC-DSA/Project 2/policingShiny/")
icvs <- read.spss("datasets/icvs/1989 - 2005/ICVS.sav", to.data.frame = TRUE)
icvs.2010 <- read.spss("datasets/icvs/2010/ICVS2010.sav", to.data.frame = TRUE)

icvs <- readRDS("datasets/icvs/ICVS.RDS")
icvs.2010 <- readRDS("datasets/icvs/ICVS2010.RDS")

saveRDS(icvs, "datasets/icvs/ICVS.RDS")
saveRDS(icvs.2010, "datasets/icvs/ICVS2010.RDS")

# icvs.bk <- icvs
# icvs <- icvs.bk # restore

# ### WTF EXAMPLE ###
# uk.full <- icvs.breakin %>% #filter(Year == "2004", Country == "United Kingdom") %>% 
#   mutate(Score = ifelse(is.na(Score), NA, sapply(Score[!is.na(Score)], S0040)))
# uk <- icvs.breakin %>% filter(Year == "2004", Country == "United Kingdom") %>% 
#   mutate(Score = ifelse(is.na(Score), NA, sapply(Score[!is.na(Score)], S0040)))
# 
# uk
# uk.full %>% filter(Year == "2004", Country == "United Kingdom")
# ### WTF EXAMPLE END ###

## basic wrangle
icvs <- icvs %>% rename(Year = I002, Country = I005A)
icvs.2010 <- icvs.2010 %>% rename(Year = I002, Country = I005A)
icvs <- icvs %>% mutate(Country = Trim(Country))
icvs.2010 <- icvs.2010 %>% mutate(Country = Trim(Country))

icvs$Year <- as.character(icvs$Year)
icvs$Year[icvs$Year %in% c("1989")] <- "1988"
icvs$Year[icvs$Year %in% c("1993","1994")] <- "1992"
icvs$Year[icvs$Year %in% c("1995","1997","1998")] <- "1996"
icvs$Year[icvs$Year %in% c("2001","2002")] <- "2000"
icvs$Year[icvs$Year %in% c("2005","2005*")] <- "2004"

icvs.2010$Year <- as.character(icvs.2010$Year)
icvs.2010$Year[grep("2010",icvs.2010$Year)] <- "2008"


## break in chances
icvs.breakin <- icvs %>% 
  mutate(Country = Trim(Country)) %>%
  select(Year, Country, Score = S0040) %>% 
  group_by(Year, Country, Score) %>% 
  summarise(n())

icvs.2010.breakin <- icvs.2010 %>%
  mutate(Country = Trim(Country)) %>%
  select(Year, Country, Score = S0040) %>% 
  group_by(Year, Country, Score) %>% 
  summarise(n())

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

## feel safe at night
icvs.safedark <- icvs %>% 
  mutate(Country = Trim(Country)) %>%
  select(Year, Country, Score = S0020) %>% 
  group_by(Year, Country, Score) %>% 
  summarise(n())

icvs.safedark.2010 <- icvs.2010 %>% 
  mutate(Country = Trim(Country)) %>%
  select(Year, Country, Score = S0020) %>% 
  group_by(Year, Country, Score) %>% 
  summarise(n())

icvs.safedark <- data.frame(icvs.safedark)
icvs.safedark$Score <- Trim(as.character(icvs.safedark$Score))
icvs.safedark.2010 <- data.frame(icvs.safedark.2010)
icvs.safedark.2010$Score <- Trim(as.character(icvs.safedark.2010$Score))

icvs.safedark$Score[icvs.safedark$Score == "1"] <- "four" 
icvs.safedark$Score[icvs.safedark$Score == "2"] <- "three"
icvs.safedark$Score[icvs.safedark$Score == "3"] <- "two"
icvs.safedark$Score[icvs.safedark$Score == "4"] <- "one"
icvs.safedark$Score[icvs.safedark$Score == "5"] <- NA
icvs.safedark$Score[icvs.safedark$Score == "6"] <- NA

icvs.safedark$Score[icvs.safedark$Score == "four"] <- 4 
icvs.safedark$Score[icvs.safedark$Score == "three"] <- 3
icvs.safedark$Score[icvs.safedark$Score == "two"] <- 2
icvs.safedark$Score[icvs.safedark$Score == "one"] <- 1

icvs.safedark.2010$Score[icvs.safedark.2010$Score == "very safe"] <- 4
icvs.safedark.2010$Score[icvs.safedark.2010$Score == "fairly safe"] <- 3
icvs.safedark.2010$Score[icvs.safedark.2010$Score == "bit unsafe"] <- 2
icvs.safedark.2010$Score[icvs.safedark.2010$Score == "very unsafe"] <- 1
icvs.safedark.2010$Score[icvs.safedark.2010$Score == "refusal"] <- NA
icvs.safedark.2010$Score[icvs.safedark.2010$Score == "unknown"] <- NA

icvs.safedark.2010 %>% group_by(Score) %>% summarise(n())
icvs.safedark %>% group_by(Score) %>% summarise(n())

icvs.safedark <- icvs.safedark %>% mutate(Score = as.numeric(Score) * n..)
icvs.safedark.2010 <- icvs.safedark.2010 %>% mutate(Score = as.numeric(Score) * n..)

icvs.safedark.2010 <- icvs.safedark.2010[!is.na(icvs.safedark.2010$Country),]
icvs.safedark <- data.frame(rbind(icvs.safedark, icvs.safedark.2010))

icvs.safedark <- icvs.safedark %>% 
  filter(!is.na(Score)) %>%
  group_by(Year, Country) %>%
  summarise(Score = sum(Score), Counts = sum(n..)) %>%
  mutate(Score = Score / Counts)

icvs.safedark <- icvs.safedark %>%
  data.frame(icvs.safedark) %>%
  select(Year, Country, Score) %>%
  mutate(Question = "Safe At Night")

icvs.safedark.2010
icvs.2010 %>% 
  select(Country, Year, S0020) %>% 
  filter(Year == "2008") %>%
  select(Country) %>%
  distinct()

## police approval
icvs.police <- icvs %>% 
  mutate(Country = Trim(Country)) %>%
  select(Year, Country, Score = P00100)

icvs.police.2010 <- icvs.2010 %>% 
  mutate(Country = Trim(Country)) %>%
  select(Year, Country, Score = P00100)

icvs.police <- data.frame(rbind(icvs.police.2010, icvs.police))
icvs.police <- icvs.police %>% mutate(Question = "Police Approval")
icvs.police$Score <- as.numeric(icvs.police$Score)

icvs.police$Score[icvs.police$Score == "yes"] <- 1
icvs.police$Score[icvs.police$Score == "no"] <- 0
icvs.police$Score[icvs.police$Score == "unknown"] <- NA

icvs.police$Score <- as.numeric(icvs.police$Score)

icvs.police <- icvs.police %>%
  filter(!is.na(Score)) %>%
  group_by(Year, Country, Question) %>%
  summarise(Score = mean(Score)) %>%
  mutate(Score = Score - (1 - Score))

icvs.police <- as.data.frame(icvs.police)

icvs.police %>% select(Year) %>% distinct()
icvs.police %>% select(Question) %>% distinct()
icvs.police %>% select(Country) %>% distinct()

saveRDS(icvs.police, "question_agg/P00100.rds")
saveRDS(icvs.safedark, "question_agg/S0020.rds")
saveRDS(icvs.breakin, "question_agg/S0040.rds")

# icvs.usa <- icvs %>% filter(Country == "USA")
# icvs.usa.police <- icvs.usa %>% select(matches("P001|P002"))
# 
# icvs.usa.police %>% group_by(P00100) %>% summarise(count = n())
# icvs %>% group_by(Year, P00100) %>% summarise(count = n()) %>% as.data.frame(.)

