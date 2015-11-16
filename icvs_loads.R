P00100 <- readRDS("datasets/P00100.rds")
S0020 <- readRDS("datasets/S0020.rds")
S0040 <- readRDS("datasets/S0040.rds")
C01B400 <- readRDS("datasets/C01B400.rds")
C03B400 <- readRDS("datasets/C03B400.rds")
C04B400 <- readRDS("datasets/C04B400.rds")
C06B400 <- readRDS("datasets/C06B400.rds")
C09B400 <- readRDS("datasets/C09B400.rds")
C11B400 <- readRDS("datasets/C11B400.rds")
C12B400 <- readRDS("datasets/C12B400.rds")
C13B400 <- readRDS("datasets/C13B400.rds")
C05B400 <- readRDS("datasets/C05B400.rds")
C02B400 <- readRDS("datasets/C02B400.rds")

ICVS.results <- rbind(P00100, S0020, S0040, C01B400, C03B400, C04B400, C06B400, C09B400, C11B400,
                      C12B400, C02B400, C05B400, C13B400)

ICVS.results$Country[ICVS.results$Country == "Ukrain"] <- "Ukraine"
ICVS.results$Country[ICVS.results$Country == "United Kingdom"] <- "UK"
ICVS.results$Country[ICVS.results$Country == "Hong Kong (SAR China)"] <- "China"
ICVS.results <- ICVS.results[ICVS.results$Country != "England & Wales",]

ICVS.results$Country <- Trim(ICVS.results$Country)

saveRDS(ICVS.results, "datasets/ICVSresults.rds")

ICVS.results %>% select(Question) %>% distinct()
ICVS.results[ICVS.results$Question == "Assaults and Threats",]

### QA ###
# icvs %>% select(Year, Country, S0040) %>% 
#   filter(Year == "2004", Country == "United Kingdom") %>%
#   group_by(Year, Country, S0040) %>% summarise(n())
# 
# ChanceofBreakin %>% filter(Year == "2004", Country == "United Kingdom")
# 
# ((1246 * 3) + (556 * 2) + 136) / (136+556+1246)
# 
