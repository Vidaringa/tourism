library(RJSONIO)
library(WDI)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

setwd("C:/Users/vidar/Documents/Rwd/tourism")

View(WDIsearch(string = "tourism"))

#Viljum international tourism, number of arrivals þ.e. ST.INT.ARVL

tourism_data <- tbl_df(WDI(country = "all", indicator = "ST.INT.ARVL", start = 1950, end = 2015)) %>%
  arrange(country, year)
tourism_data <- rename(tourism_data, Fjoldi = ST.INT.ARVL)

#####################################################################################################
## Reiknum út vöxt á hverju ári seinustu m.v. seinustu X árin. Vil sjá hvað gerist hjá landi þar sem
## fjöldi ferðamanna hefur vaxið hratt á stuttum tíma.
## Niðurstaðan er sú að lönd sem komast í "tísku" haldast þar.

t_data <- tourism_data %>% 
  group_by(country) %>% 
  mutate(fjoldi_lag = lag(Fjoldi, n = 4L), breyting_4 = Fjoldi/fjoldi_lag-1) %>% 
  filter(!(is.na(breyting_4)),!(iso2c %in% c("7E","XL","XM","XN"))) %>% #hend út því sem eru ekki lönd
  mutate(dummy = ifelse(breyting_4 >= 0.8, year, 5000)) %>% #5000 bara til að hafa einhverja stóra tölu svo hún verði ekki valin í næstu línu
  filter(year >= min(dummy)) %>% 
  mutate(fjoldi_t0 = year - (min(year))+1, delta = Fjoldi/lag(Fjoldi)-1,
         Ice = ifelse(country == "Iceland", "Ísland", "önnur lönd")) #fjoldi_t0 er fjöldi ára frá því að viðmiðinu var náð

t_data[is.na(t_data)] <- 0 # Setja uppsafnaða aukningu sem 0 á ári 1

t_data <- t_data %>% 
        mutate(running = order_by(country, cumsum(delta)))

gg <- ggplot(t_data, aes(fjoldi_t0, running))

png(file = "tourism1.png", width = 1200, height = 900)
g_tourism + geom_point(size = 4, shape = 21) + geom_smooth(span = 0.6, size = 1.2) + 
        theme_classic() +
        labs(x = "Fjöldi ára frá því að viðmiði var náð",
             y = "Uppsöfnuð fjölgun ferðamanna")+
        theme(axis.text.x = element_text(size=20),
              axis.text.y = element_text(size=20),
              axis.title.x = element_text(size = 30),
              axis.title.y = element_text(size = 30),
              panel.grid.major.y = element_line(colour = "grey", linetype = "dotted")) + 
        scale_y_continuous(labels = percent, limits = c(-0.5,5))
dev.off()

# Uppsöfnuð breyting eftir 4 ár undir 0%
undir_0 <- t_data %>% 
        filter(fjoldi_t0 == 4, running < 0)



#######################################################################################################
## Bý til breytu sem segir til um hvort árið sem kríteríunni er náð hafi verið fyrir dot.com bóluna eða GFC

crisis <- t_data %>% 
        group_by(country) %>% 
        mutate(when = if(any(year == 2000 & fjoldi_t0 == 1)) "Dot-com" else if(any(year == 2007 & fjoldi_t0 == 1)) "Hrun 2007" else "Önnur tímabil")

# þegar aðeins eru til 6 "punktar" fyrir x ásinn þá lendir geom_smooth í veseni. Reiknum meðaltal í staðinn
# ggplot(crisis, aes(x = fjoldi_t0, y = running)) + geom_point(aes(colour=when)) + geom_smooth(aes(colour=when)) +
#        ylim(-1,2) + xlim(2,17)


# Meðaltal
crisis_avg <- crisis %>% 
        group_by(fjoldi_t0, when) %>%
        summarise(avg = median(running))

g <- ggplot(crisis_avg, aes(x = fjoldi_t0, y = avg))
g + geom_line(aes(colour = when))

#####################################################################################################
## Bý til data frame til að búa til eins mynd og sú hér að ofan nema með Íslandi inni sem sér línu ##     
#####################################################################################################

ice <- filter(crisis, country == "Iceland") %>% 
        spread(country, running) %>%
        select(fjoldi_t0, when, Iceland)

colnames(ice)[3] <- "avg"
ice$when <- "Ísland"

ice2 <- data.frame(fjoldi_t0 = 2:3, when = c("Ísland","Ísland"), avg = c(0.292, 0.715))

df_ice <- bind_rows(crisis_avg, ice, ice2)

gi <- ggplot(df_ice, aes(x = fjoldi_t0, y = avg))

png(file = "tourism2.png", width = 1200, height = 900)
gi + geom_line(aes(colour = when), size = 1.5) + theme_bw() +
        labs(x = "Fjöldi ára frá því að viðmiði var náð",
             y = "Uppsöfnuð fjölgun ferðamanna",
             colour = NULL) +
        theme(axis.text.x = element_text(size=20),
              axis.text.y = element_text(size=20),
              axis.title.x = element_text(size = 30),
              axis.title.y = element_text(size = 30),
              legend.text = element_text(size = 30))+
        scale_y_continuous(labels = percent)
dev.off()

#til að ná gögnunum á bakvið plottið:
#http://stackoverflow.com/questions/9789871/method-to-extract-stat-smooth-line-fit
#gb <- ggplot(t_data, aes(fjoldi_t0, delta)) + geom_point() + geom_smooth()
#ggplot_build(gg)$data[[2]]


#####################################################################################################
##=========================================BÆTI VIÐ RAUNGENGI======================================##
#####################################################################################################

#Leita í WDI gagnagrunninum

WDIsearch(string = "exchange rate")

# Vil fá REER

real1 <- tbl_df(WDI(country = "all", indicator = "REER", start = 1950, end = 2015))
real <- select(real1, country, REER, year) %>% 
        filter(year >= 1999 & year <= 2014) %>% 
        arrange(country, year)

t_data <- tbl_df(t_data) # nauðsynlegt svo að hægt sé að left_join-a
tour_real <- left_join(t_data, real, by = c("country" = "country","year"="year")) %>% 
        arrange(country, year)

# Reikna út breytingu á raungengi milli ára
tour_real_quantile <- tour_real %>% 
        group_by(country) %>%
        mutate(d_reer = REER/lag(REER)) %>% 
        na.omit()


##========================================================NOTA ÖLL LÖND OG RAUNGENGI=============================================##
# Hér verður viðmiðinu sleppt og öll lönd skoðuð þar sem upplýsingar um raungengi eru aðgengilegar

total_tour_real <- left_join(tourism_data, real1, by = c("country" = "country","year"="year")) %>% na.omit()

# skoða breytingu á fjölda og raungengi yfir 5 ára tímabil og plotta

total_year <- total_tour_real %>% 
        group_by(country) %>% 
        mutate(d_fjoldi = Fjoldi/lag(Fjoldi)-1, d_reer = REER/lag(REER)-1)

#total5year14 <- filter(total5year, year == 2014)
total5year14 <- total5year

## Skoða hallatölu með og án útlaga
total5year14_outl <- filter(total5year14, d_reer < 1, d_fjoldi < 1)

reg1 <- lm(total5year14$d_fjoldi~total5year14$d_reer)
reg2 <- lm(total5year14_outl$d_fjoldi~total5year14_outl$d_reer)

plot(total5year14$d_fjoldi~total5year14$d_reer)
abline(reg1, lty = "dashed")
abline(reg2)


## Loka mynd
png(file = "real_exchange.png", width = 1200, height = 900)
ggplot(total5year14_outl, aes(x = d_reer, y = d_fjoldi)) + geom_point(size = 2) +
        geom_smooth(method = "lm", lwd = 2) + theme_bw() +
        labs(x = "Breyting raungengis yfir tímabilið",
             y = "Breyting á fjölda ferðmanna yfir tímabilið") +
        theme(axis.text.x = element_text(size=20),
              axis.text.y = element_text(size=20),
              axis.title.x = element_text(size = 30),
              axis.title.y = element_text(size = 30))+
        scale_x_continuous(labels = percent)
dev.off()


#################################################################################################################
##=============================================BÆTI VIÐ PRICE LEVEL============================================##
# Hér er stuðst við öll lönd, ekki bara þau sem náðu viðmiðinu (ef það væri gert væru fáir gagnapunktar eftir)
# Farin verður svipuð leið og með raungengið að ofan nema hópnum skipt upp í 4 hópa eftir því hversu hátt
# verðlagið er. Þessi greining mun aðeins ná til OECD ríkja.

#############
## Hér set ég fjölda tímabila til að líta aftur í tímann, kóðann þarf þó að keyra aftur
#############

nbil = 5

ppp <- read.csv(file = "~/Rwd/tourism/price_level.csv")
iso <- read.csv(file = "~/Rwd/tourism/iso.csv", sep = ";")
ppp <- select(ppp, ï..LOCATION, TIME,Value)
colnames(ppp) <- c("Alpha.3.code","year","value")
ppp <- left_join(ppp, iso, by = "Alpha.3.code")
colnames(ppp)[4] <- "country"

tourism_real_price <- left_join(total_tour_real, ppp, by = c("country" = "country","year" = "year"))
tourism_real_price <- select(tourism_real_price, country, year, Fjoldi, REER, value)

# Bý til fjóra hópa eftir því hversu landið er dýrt
tourism_real_price <- tourism_real_price %>% 
        na.omit() %>% 
        group_by(year) %>% 
        mutate(Verdlag = cut(value, breaks = c(quantile(value, probs = seq(0,1, by = 0.25))),
                             labels = c("Q1","Q2","Q3","Q4"), include.lowest = TRUE))

myndagogn <- tourism_real_price %>% 
        group_by(country) %>% 
        mutate(d_fjoldi = Fjoldi/lag(Fjoldi, n = nbil)-1, d_reer = REER/lag(REER, n = nbil)-1)

# Get búið 2x2 panel með einni mynd fyrir hvern fjórðung. Eða búið til eina stóra mynd
# með skatterplotti allra fjögurra hópanna þ.e. fjórar regression línur á einni mynd.

# Ein mynd með öllum scatter-plottunum á einu grafi
ggplot(myndagogn, aes(x = d_reer, y = d_fjoldi, colour = Verdlag)) + geom_point(size = 2) +
        geom_smooth(method = "lm", lwd = 2, se = FALSE) + theme_bw() +
        labs(x = "Breyting raungengis yfir tímabilið",
             y = "Breyting á fjölda ferðmanna yfir tímabilið") +
        theme(axis.text.x = element_text(size=20),
              axis.text.y = element_text(size=20),
              axis.title.x = element_text(size = 30),
              axis.title.y = element_text(size = 30))+
        scale_x_continuous(labels = percent) +
        scale_colour_brewer(palette = "Spectral")


# 2x2 panel graf með scatter plot fyrir hvern fjórðung
library(gridExtra)

mynd1 <- ggplot(filter(myndagogn, Verdlag == "Q1"), aes(x = d_reer, y = d_fjoldi)) + geom_point(size = 2) +
        geom_smooth(method = "lm", lwd = 2)

mynd2 <- ggplot(filter(myndagogn, Verdlag == "Q2"), aes(x = d_reer, y = d_fjoldi)) + geom_point(size = 2) +
        geom_smooth(method = "lm", lwd = 2)

mynd3 <- ggplot(filter(myndagogn, Verdlag == "Q3"), aes(x = d_reer, y = d_fjoldi)) + geom_point(size = 2) +
        geom_smooth(method = "lm", lwd = 2)

mynd4 <- ggplot(filter(myndagogn, Verdlag == "Q4"), aes(x = d_reer, y = d_fjoldi)) + geom_point(size = 2) +
        geom_smooth(method = "lm", lwd = 2)

grid.arrange(mynd1, mynd2, mynd3, mynd4, nrow = 2, ncol = 2)


# Vandinn við það sem ég geri að ofan er að ég er að bera saman þróun raungengis seinustu 5 árin við
# þróun í fjölda ferðamanna seinustu 5 árin og hengi þetta við hvort verðlag í landi sé dýrt Í DAG. 
# Hér væri eflaust sniðugra að skoða mjög svipaðar myndir og að ofan nema að ekki skipta þeim upp í hópa
# m.v. verðlagið "í dag" heldur verðlagið 5 árum á undan þ.e. á þeim tíma sem ég byrja að reikna
# uppsafnaða fjölgun yfir 5 árin.

myndagogn <- myndagogn %>% 
        mutate(Verdlag_lag = lag(Verdlag, n = nbil))

mynd11 <- ggplot(filter(myndagogn, Verdlag_lag == "Q1"), aes(x = d_reer, y = d_fjoldi)) + geom_point(size = 2) +
        geom_smooth(method = "lm", lwd = 2) + theme_bw() +
        labs(x = "Breyting raungengis seinasta árið",
             y = "Breyting á fjölda ferðmanna seinasta árið",
             title = "Fyrsti fjórðungur") +
        theme(axis.text.x = element_text(size=15),
              axis.text.y = element_text(size=15),
              axis.title.x = element_text(size = 15),
              axis.title.y = element_text(size = 15))+
        scale_x_continuous(labels = percent) +
        scale_y_continuous(labels = percent)
        

mynd21 <- ggplot(filter(myndagogn, Verdlag_lag == "Q2"), aes(x = d_reer, y = d_fjoldi)) + geom_point(size = 2) +
        geom_smooth(method = "lm", lwd = 2) + theme_bw() +
        labs(x = "Breyting raungengis seinasta árið",
             y = "Breyting á fjölda ferðmanna seinasta árið",
             title = "Annar fjórðungur") +
        theme(axis.text.x = element_text(size=15),
              axis.text.y = element_text(size=15),
              axis.title.x = element_text(size = 15),
              axis.title.y = element_text(size = 15))+
        scale_x_continuous(labels = percent) +
        scale_y_continuous(labels = percent)

mynd31 <- ggplot(filter(myndagogn, Verdlag_lag == "Q3"), aes(x = d_reer, y = d_fjoldi)) + geom_point(size = 2) +
        geom_smooth(method = "lm", lwd = 2) + theme_bw() +
        labs(x = "Breyting raungengis seinasta árið",
             y = "Breyting á fjölda ferðmanna seinasta árið",
             title = "Þriðji fjórðungur") +
        theme(axis.text.x = element_text(size=15),
              axis.text.y = element_text(size=15),
              axis.title.x = element_text(size = 15),
              axis.title.y = element_text(size = 15))+
        scale_x_continuous(labels = percent) +
        scale_y_continuous(labels = percent)

mynd41 <- ggplot(filter(myndagogn, Verdlag_lag == "Q4"), aes(x = d_reer, y = d_fjoldi)) + geom_point(size = 2) +
        geom_smooth(method = "lm", lwd = 2) + theme_bw() +
        labs(x = "Breyting raungengis seinasta árið",
             y = "Breyting á fjölda ferðmanna seinasta árið",
             title = "Fjórði fjórðungur") +
        theme(axis.text.x = element_text(size=15),
              axis.text.y = element_text(size=15),
              axis.title.x = element_text(size = 15),
              axis.title.y = element_text(size = 15))+
        scale_x_continuous(labels = percent) +
        scale_y_continuous(labels = percent)

png(file = "price_realexchange_1year.png", width = 1200, height = 900)
grid.arrange(mynd11, mynd21, mynd31, mynd41, nrow = 2, ncol = 2)
dev.off()


##############################################################################################################
##============================================REGRESSION FYRIR GREIN========================================##
##############################################################################################################

reg_data <- tourism_real_price %>%
        group_by(country) %>% 
        mutate(fjolgun = Fjoldi/lag(Fjoldi)-1, raungengi = REER/lag(REER)-1)

reg_1 = plm(fjolgun ~ raungengi + value,
           data = reg_data, index = c("country","year"), model="pooling")

reg_11 = plm(fjolgun ~ raungengi,
             data = reg_data, index = c("country","year"), model="pooling")

reg_2 = plm(fjolgun ~ raungengi + value, data = reg_data, index = c("country","year"), model = "within")


stargazer(reg_1, reg_2,     
          title="Pooling vs FE", type="text", 
          column.labels=c("Pooling", "FE"), 
          df=FALSE, digits=4)
