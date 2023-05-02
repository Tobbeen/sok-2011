library(WDI)
library(tidyverse)
Sys.setlocale(locale="no_NO")

### 3.1 Data
## Lager datasett fra WDI hvor endrer variabelnavn og setter start of sluttår
df_gdp_wdi <- WDI(
  country = "all",
  indicator = c('gdppc' = "NY.GDP.PCAP.PP.KD"),
  start = 2000,
  end = 2019,
  extra = TRUE
)


df_gdp <- df_gdp_wdi %>%
  mutate(year = as.numeric(year)) %>%
  filter(iso3c != "", income != "Aggregates") %>%
  drop_na(gdppc) %>%
  group_by(country, year) %>%
  slice(which.max(gdppc)) %>%
  ungroup()

df_gdp <- df_gdp %>%
  group_by(country) %>%
  filter(year == min(year)) %>%
  select(country, gdppc0 = gdppc) %>%
  left_join(df_gdp, by = c("country")) %>%
  ungroup()

df_gdp <- df_gdp %>%
  group_by(country) %>%
  mutate(
    gdpgrowth = (log(gdppc) - lag(log(gdppc))) * 100,
    avg_gdpgrowth = mean(gdpgrowth, na.rm = TRUE)
  ) %>%
  arrange(desc(year)) %>%
  slice(1) %>%
  select(-status,
         -lastupdated,
         -capital,
         -longitude,
         -latitude,
         -lending,
         -gdpgrowth) %>%
  ungroup()

df_edu_wdi <- WDI(
  country = "all",
  indicator = c('educ' = "BAR.SCHL.15UP"),
  start = 2000,
  end = 2019,
  extra = TRUE
)

df_edu <- df_edu_wdi %>%
  drop_na(educ) %>%
  mutate(educ = as.numeric(educ)) %>%
  select(country, iso2c, iso3c, year, educ, region, income) %>%
  group_by(iso3c) %>%
  mutate(avg_educ = mean(educ)) %>%
  ungroup() %>%
  select(-year,-educ) %>%
  distinct(country, .keep_all = TRUE)

df_nsy_wdi <- WDI(
  country = "all",
  indicator = c('nsy' = "NY.ADJ.NNAT.GN.ZS"),
  start = 2000,
  end = 2015,
  extra = TRUE
)

df_nsy <- df_nsy_wdi %>%
  select(country, region, income, iso2c, iso3c, year, nsy) %>%
  arrange(iso3c, year) %>%
  drop_na(nsy, iso3c, region) %>%
  filter(region != "Aggregates") %>%
  mutate(nsy = as.numeric(nsy, na.rm = TRUE)) %>%
  group_by(country) %>%
  mutate(avg_nsy = mean(nsy, na.rm = TRUE)) %>%
  select(-year,-nsy) %>%
  distinct(country, .keep_all = TRUE) %>%
  ungroup()

df_lf_wdi <- WDI(
  country = "all",
  indicator = c('lf' = "JI.TLF.TOTL"),
  start = 2000,
  end = 2019,
  extra = TRUE
)

df_lf <- df_lf_wdi %>%
  select(country, region, income, iso3c = iso2c, year, lf) %>%
  drop_na(lf) %>%
  filter(lf != 0) %>%
  group_by(iso3c) %>%
  arrange(year) %>%
  mutate(deltaYear = year - lag(year),
         growth = log(lf) - log(lag(lf))) %>%
  drop_na(deltaYear, growth) %>%
  mutate(n = growth / deltaYear, avg_n = mean(n, na.rm = TRUE)) %>%
  filter(year == max(year)) %>%
  ungroup()

df_rest_wdi <- WDI(
  country = "all",
  indicator = c(
    'poptot' = "SP.POP.TOTL",
    'gi' = "NE.GDI.FTOT.KD.ZG",
    'gx' = "NE.EXP.GNFS.KD.ZG",
    'nry' = "NY.ADJ.DRES.GN.ZS",
    'p' = "SP.POP.GROW"
  ),
  start = 2000,
  end = 2019,
  extra = TRUE
)

df_rest <- df_rest_wdi %>%
  drop_na(iso3c, gi, gx, nry) %>%
  filter(region != "Aggregates") %>%
  select(-longitude,
         -latitude,
         -capital,
         -lending,
         -status,
         -lastupdated) %>%
  group_by(iso3c) %>%
  mutate(
    avg_p = mean(p, na.rm = TRUE),
    avg_gi = mean(gi, na.rm = TRUE),
    avg_gx = mean(gx, na.rm = TRUE),
    avg_nry = mean(nry, na.rm = TRUE)
  ) %>%
  filter(year == max(year)) %>%
  ungroup(iso3c)

df_growth <-
  df_gdp %>%
  inner_join(select(df_edu, iso3c, avg_educ), by = "iso3c") %>%
  inner_join(select(df_nsy, iso3c, avg_nsy), by = "iso3c") %>%
  inner_join(select(df_lf, iso3c, avg_n), by = "iso3c") %>%
  inner_join(select(df_rest, iso3c, poptot, avg_p, avg_gi, avg_gx, avg_nry),
             by = "iso3c") %>%
  mutate(ln_gdppc0 = log(gdppc0),
         ln_gdppc = log(gdppc),
         avg_n = avg_n * 100) %>%
  select(-year)


### 4.1 Deskriptiv statistikk

df1 <- subset(df_growth, select = c("avg_gdpgrowth", "avg_educ", "avg_nsy", "avg_p", "avg_gi", "avg_gx", "avg_nry", "avg_n"))

labs <- c("Vekstrate for BNP per innbygger %", "Antall år utdanning", "Nettosparing i %",
          "Vekstrate for befolkningen i %", "Årlig vekstrate for investeringer",
          "Vekstrate for eksport i %", "Årlig reduksjonsrate for naturressurser", "Vekstrate for arbeidskraft") 

library(vtable)
options(digits = 5)

st(df1, labels=labs,
   summ = list(
     c('notNA(x)','mean(x)','sd(x)','min(x)','max(x)'), 
     c('notNA(x)','mean(x)')
   ),
   summ.names = list(
     c('N','Gjennomsnitt','SD','Min','Maks') 
   ))



### Graf 1 av sparing og logaritme av BNP per innbygger. 
library(ggplot2)
library(scales)
plot1 <- ggplot(df_growth, aes(x = avg_nsy , y = ln_gdppc, na.rm = TRUE)) +
  xlab("Nettosparing per år") + # Beskrivelse for x-akselen
  ylab("logaritme av BNP per innbygger") + # Beskrivelse for y-akselen
  theme_minimal(base_size = 14, base_family = "Georgia") + # Tekststørrelse og font
  geom_point(aes(size = poptot, color = region), alpha = 0.6) + # Størrelse (farge) på bobblene avhenger befolkningsstørrelse (region)
  scale_size_area(guide = "none", max_size = 14) + #Ta vekk legend for befolkningsstørrelse
  theme(legend.text = element_text(size = 10,color="black"))+  # Bestem font-størrelse på legend
  scale_colour_manual(values = rainbow(9)) +# Velg farger til bobblene
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white'))
plot1

### Graf 2 av Befolkningsvekst og logaritmen av BNP per innbygger.
plot2 <- ggplot(df_growth, aes(x = avg_p , y = ln_gdppc, na.rm = TRUE)) +
  xlab("Befolkningsvekst") + # Beskrivelse for x-akselen
  ylab("logaritme av BNP per innbygger") + # Beskrivelse for y-akselen
  theme_minimal(base_size = 14, base_family = "Georgia") + # Tekststørrelse og font
  geom_point(aes(size = poptot, color = region), alpha = 0.6) + # Størrelse (farge) på bobblene avhenger befolkningsstørrelse (region)
  scale_size_area(guide = "none", max_size = 14) + #Ta vekk legend for befolkningsstørrelse
  theme(legend.text = element_text(size = 10,color="black"))+  # Bestem font-størrelse på legend
  scale_colour_manual(values = rainbow(9)) +# Velg farger til bobblene
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white'))
plot2

### Graf 3 av Humankapital (antall år utdanning) og logaritmen av BNP per innbygger
plot3 <- ggplot(df_growth, aes(x = avg_educ , y = ln_gdppc, na.rm = TRUE)) +
  xlab("Antall år utdanning (humankapital)") + # Beskrivelse for x-akselen
  ylab("logaritme av BNP per innbygger") + # Beskrivelse for y-akselen
  theme_minimal(base_size = 14, base_family = "Georgia") + # Tekststørrelse og font
  geom_point(aes(size = poptot, color = region), alpha = 0.6) + # Størrelse (farge) på bobblene avhenger befolkningsstørrelse (region)
  scale_size_area(guide = "none", max_size = 14) + #Ta vekk legend for befolkningsstørrelse
  theme(legend.text = element_text(size = 10,color="black"))+  # Bestem font-størrelse på legend
  scale_colour_manual(values = rainbow(9)) +# Velg farger til bobblene
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white'))
plot3

### Graf 4 av Sparing og årlig vekstrate i BNP per innbygger.
plot4 <- ggplot(df_growth, aes(x = avg_nsy , y = avg_gdpgrowth, na.rm = TRUE)) +
  xlab("Årlig nettosparing") + # Beskrivelse for x-akselen
  ylab("Årlig vekstrate i BNP per innbygger") + # Beskrivelse for y-akselen
  theme_minimal(base_size = 14, base_family = "Georgia") + # Tekststørrelse og font
  geom_point(aes(size = poptot, color = region), alpha = 0.6) + # Størrelse (farge) på bobblene avhenger befolkningsstørrelse (region)
  scale_size_area(guide = "none", max_size = 14) + #Ta vekk legend for befolkningsstørrelse
  theme(legend.text = element_text(size = 10,color="black"))+  # Bestem font-størrelse på legend
  scale_colour_manual(values = rainbow(9)) +# Velg farger til bobblene
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white'))
plot4

### Graf 5 av Humankapital (antall år utdanning) og årlig vekstrate i BNP per innbygger
plot5 <- ggplot(df_growth, aes(x = avg_educ , y = avg_gdpgrowth, na.rm = TRUE)) +
  xlab("Antall år utdanning") + # Beskrivelse for x-akselen
  ylab("Årlig vekstrate i BNP per innbygger") + # Beskrivelse for y-akselen
  theme_minimal(base_size = 14, base_family = "Georgia") + # Tekststørrelse og font
  geom_point(aes(size = poptot, color = region), alpha = 0.6) + # Størrelse (farge) på bobblene avhenger befolkningsstørrelse (region)
  scale_size_area(guide = "none", max_size = 14) + #Ta vekk legend for befolkningsstørrelse
  theme(legend.text = element_text(size = 10,color="black"))+  # Bestem font-størrelse på legend
  scale_colour_manual(values = rainbow(9)) +# Velg farger til bobblene
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white'))
plot5


### 4.2 Økonometrisk analyse


## Første regresjonsanalyse
library(sjPlot)
library(sjmisc)
library(sjlabelled)
model1 <- lm(avg_gdpgrowth  ~ avg_educ + avg_nry + avg_nsy + avg_p + avg_gi + avg_gx + avg_n + ln_gdppc0, data= df_growth)
tab_model(model1)



###Fjerne outliers
df <- df_growth[complete.cases( df_growth$avg_gi, df_growth$avg_n),]

Q1gi <- quantile(df$avg_gi, .25 )
Q3gi <- quantile(df$avg_gi, .75)
IQRgi <- IQR(df$avg_gi)

Q1n <- quantile(df$avg_n, .25 )
Q3n <- quantile(df$avg_n, .75)
IQRn <- IQR(df$avg_n)

no_outliers <- subset(df, df$avg_gi > (Q1gi - 1.5*IQRgi) & df$avg_gi < (Q3gi + 1.5*IQRgi) &  df$avg_n > (Q1n - 1.5*IQRn) & df$avg_n < (Q3n + 1.5*IQRn))
dim(no_outliers)

### Andre regresjonsanalyse
model2 <- lm(avg_gdpgrowth  ~ avg_educ + avg_nry + avg_nsy + avg_p + avg_gi + avg_gx + avg_n + ln_gdppc0, data= no_outliers)
tab_model(model2)

