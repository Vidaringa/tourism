library(RJSONIO)
library(WDI)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

setwd("C:/Users/vidar/Documents/Rwd/tourism")

View(WDIsearch(string = "tourism"))

#Viljum international tourism, number of arrivals �.e. ST.INT.ARVL

tourism_data <- tbl_df(WDI(country = "all", indicator = "ST.INT.ARVL", start = 1950, end = 2015)) %>%
  arrange(country, year)
tourism_data <- rename(tourism_data, Fjoldi = ST.INT.ARVL)

#####################################################################################################
## Reiknum �t v�xt � hverju �ri seinustu m.v. seinustu X �rin. Vil sj� hva� gerist hj� landi �ar sem
## fj�ldi fer�amanna hefur vaxi� hratt � stuttum t�ma.
## Ni�ursta�an er s� a� l�nd sem komast � "t�sku" haldast �ar.

t_data <- tourism_data %>% 
  group_by(country) %>% 
  mutate(fjoldi_lag = lag(Fjoldi, n = 4L), breyting_4 = Fjoldi/fjoldi_lag-1) %>% 
  filter(!(is.na(breyting_4)),!(iso2c %in% c("7E","XL","XM","XN"))) %>% #hend �t �v� sem eru ekki l�nd
  mutate(dummy = ifelse(breyting_4 >= 0.8, year, 5000)) %>% #5000 bara til a� hafa einhverja st�ra t�lu svo h�n ver�i ekki valin � n�stu l�nu
  filter(year >= min(dummy)) %>% 
  mutate(fjoldi_t0 = year - (min(year))+1, delta = Fjoldi/lag(Fjoldi)-1,
         Ice = ifelse(country == "Iceland", "�sland", "�nnur l�nd")) #fjoldi_t0 er fj�ldi �ra fr� �v� a� vi�mi�inu var n��

t_data[is.na(t_data)] <- 0 # Setja uppsafna�a aukningu sem 0 � �ri 1

t_data <- t_data %>% 
        mutate(running = order_by(country, cumsum(delta)))

gg <- ggplot(t_data, aes(fjoldi_t0, running))

png(file = "tourism1.png", width = 1200, height = 900)
g_tourism + geom_point(size = 4, shape = 21) + geom_smooth(span = 0.6, size = 1.2) + 
        theme_classic() +
        labs(x = "Fj�ldi �ra fr� �v� a� vi�mi�i var n��",
             y = "Upps�fnu� fj�lgun fer�amanna")+
        theme(axis.text.x = element_text(size=20),
              axis.text.y = element_text(size=20),
              axis.title.x = element_text(size = 30),
              axis.title.y = element_text(size = 30),
              panel.grid.major.y = element_line(colour = "grey", linetype = "dotted")) + 
        scale_y_continuous(labels = percent, limits = c(-0.5,5))
dev.off()

# Upps�fnu� breyting eftir 4 �r undir 0%
undir_0 <- t_data %>% 
        filter(fjoldi_t0 == 4, running < 0)



#######################################################################################################
## B� til breytu sem segir til um hvort �ri� sem kr�ter�unni er n�� hafi veri� fyrir dot.com b�luna e�a GFC

crisis <- t_data %>% 
        group_by(country) %>% 
        mutate(when = if(any(year == 2000 & fjoldi_t0 == 1)) "Dot-com" else if(any(year == 2007 & fjoldi_t0 == 1)) "Hrun 2007" else "�nnur t�mabil")

# �egar a�eins eru til 6 "punktar" fyrir x �sinn �� lendir geom_smooth � veseni. Reiknum me�altal � sta�inn
# ggplot(crisis, aes(x = fjoldi_t0, y = running)) + geom_point(aes(colour=when)) + geom_smooth(aes(colour=when)) +
#        ylim(-1,2) + xlim(2,17)


# Me�altal
crisis_avg <- crisis %>% 
        group_by(fjoldi_t0, when) %>%
        summarise(avg = median(running))

g <- ggplot(crisis_avg, aes(x = fjoldi_t0, y = avg))
g + geom_line(aes(colour = when))

#####################################################################################################
## B� til data frame til a� b�a til eins mynd og s� h�r a� ofan nema me� �slandi inni sem s�r l�nu ##     
#####################################################################################################

ice <- filter(crisis, country == "Iceland") %>% 
        spread(country, running) %>%
        select(fjoldi_t0, when, Iceland)

colnames(ice)[3] <- "avg"
ice$when <- "�sland"

ice2 <- data.frame(fjoldi_t0 = 2:3, when = c("�sland","�sland"), avg = c(0.292, 0.715))

df_ice <- bind_rows(crisis_avg, ice, ice2)

gi <- ggplot(df_ice, aes(x = fjoldi_t0, y = avg))

png(file = "tourism2.png", width = 1200, height = 900)
gi + geom_line(aes(colour = when), size = 1.5) + theme_bw() +
        labs(x = "Fj�ldi �ra fr� �v� a� vi�mi�i var n��",
             y = "Upps�fnu� fj�lgun fer�amanna",
             colour = NULL) +
        theme(axis.text.x = element_text(size=20),
              axis.text.y = element_text(size=20),
              axis.title.x = element_text(size = 30),
              axis.title.y = element_text(size = 30),
              legend.text = element_text(size = 30))+
        scale_y_continuous(labels = percent)
dev.off()

#til a� n� g�gnunum � bakvi� plotti�:
#http://stackoverflow.com/questions/9789871/method-to-extract-stat-smooth-line-fit
#gb <- ggplot(t_data, aes(fjoldi_t0, delta)) + geom_point() + geom_smooth()
#ggplot_build(gg)$data[[2]]


#####################################################################################################
##=========================================B�TI VI� RAUNGENGI======================================##
#####################################################################################################

#Leita � WDI gagnagrunninum

WDIsearch(string = "exchange rate")

# Vil f� REER

real1 <- tbl_df(WDI(country = "all", indicator = "REER", start = 1950, end = 2015))
real <- select(real1, country, REER, year) %>% 
        filter(year >= 1999 & year <= 2014) %>% 
        arrange(country, year)

t_data <- tbl_df(t_data) # nau�synlegt svo a� h�gt s� a� left_join-a
tour_real <- left_join(t_data, real, by = c("country" = "country","year"="year")) %>% 
        arrange(country, year)

# Reikna �t breytingu � raungengi milli �ra
tour_real_quantile <- tour_real %>% 
        group_by(country) %>%
        mutate(d_reer = REER/lag(REER)) %>% 
        na.omit()


##========================================================NOTA �LL L�ND OG RAUNGENGI=============================================##
# H�r ver�ur vi�mi�inu sleppt og �ll l�nd sko�u� �ar sem uppl�singar um raungengi eru a�gengilegar

total_tour_real <- left_join(tourism_data, real1, by = c("country" = "country","year"="year")) %>% na.omit()

# sko�a breytingu � fj�lda og raungengi yfir 5 �ra t�mabil og plotta

total_year <- total_tour_real %>% 
        group_by(country) %>% 
        mutate(d_fjoldi = Fjoldi/lag(Fjoldi)-1, d_reer = REER/lag(REER)-1)

#total5year14 <- filter(total5year, year == 2014)
total5year14 <- total5year

## Sko�a hallat�lu me� og �n �tlaga
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
        labs(x = "Breyting raungengis yfir t�mabili�",
             y = "Breyting � fj�lda fer�manna yfir t�mabili�") +
        theme(axis.text.x = element_text(size=20),
              axis.text.y = element_text(size=20),
              axis.title.x = element_text(size = 30),
              axis.title.y = element_text(size = 30))+
        scale_x_continuous(labels = percent)
dev.off()


#################################################################################################################
##=============================================B�TI VI� PRICE LEVEL============================================##
# H�r er stu�st vi� �ll l�nd, ekki bara �au sem n��u vi�mi�inu (ef �a� v�ri gert v�ru f�ir gagnapunktar eftir)
# Farin ver�ur svipu� lei� og me� raungengi� a� ofan nema h�pnum skipt upp � 4 h�pa eftir �v� hversu h�tt
# ver�lagi� er. �essi greining mun a�eins n� til OECD r�kja.

#############
## H�r set �g fj�lda t�mabila til a� l�ta aftur � t�mann, k��ann �arf �� a� keyra aftur
#############

nbil = 5

ppp <- read.csv(file = "~/Rwd/tourism/price_level.csv")
iso <- read.csv(file = "~/Rwd/tourism/iso.csv", sep = ";")
ppp <- select(ppp, �..LOCATION, TIME,Value)
colnames(ppp) <- c("Alpha.3.code","year","value")
ppp <- left_join(ppp, iso, by = "Alpha.3.code")
colnames(ppp)[4] <- "country"

tourism_real_price <- left_join(total_tour_real, ppp, by = c("country" = "country","year" = "year"))
tourism_real_price <- select(tourism_real_price, country, year, Fjoldi, REER, value)

# B� til fj�ra h�pa eftir �v� hversu landi� er d�rt
tourism_real_price <- tourism_real_price %>% 
        na.omit() %>% 
        group_by(year) %>% 
        mutate(Verdlag = cut(value, breaks = c(quantile(value, probs = seq(0,1, by = 0.25))),
                             labels = c("Q1","Q2","Q3","Q4"), include.lowest = TRUE))

myndagogn <- tourism_real_price %>% 
        group_by(country) %>% 
        mutate(d_fjoldi = Fjoldi/lag(Fjoldi, n = nbil)-1, d_reer = REER/lag(REER, n = nbil)-1)

# Get b�i� 2x2 panel me� einni mynd fyrir hvern fj�r�ung. E�a b�i� til eina st�ra mynd
# me� skatterplotti allra fj�gurra h�panna �.e. fj�rar regression l�nur � einni mynd.

# Ein mynd me� �llum scatter-plottunum � einu grafi
ggplot(myndagogn, aes(x = d_reer, y = d_fjoldi, colour = Verdlag)) + geom_point(size = 2) +
        geom_smooth(method = "lm", lwd = 2, se = FALSE) + theme_bw() +
        labs(x = "Breyting raungengis yfir t�mabili�",
             y = "Breyting � fj�lda fer�manna yfir t�mabili�") +
        theme(axis.text.x = element_text(size=20),
              axis.text.y = element_text(size=20),
              axis.title.x = element_text(size = 30),
              axis.title.y = element_text(size = 30))+
        scale_x_continuous(labels = percent) +
        scale_colour_brewer(palette = "Spectral")


# 2x2 panel graf me� scatter plot fyrir hvern fj�r�ung
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


# Vandinn vi� �a� sem �g geri a� ofan er a� �g er a� bera saman �r�un raungengis seinustu 5 �rin vi�
# �r�un � fj�lda fer�amanna seinustu 5 �rin og hengi �etta vi� hvort ver�lag � landi s� d�rt � DAG. 
# H�r v�ri eflaust sni�ugra a� sko�a mj�g svipa�ar myndir og a� ofan nema a� ekki skipta �eim upp � h�pa
# m.v. ver�lagi� "� dag" heldur ver�lagi� 5 �rum � undan �.e. � �eim t�ma sem �g byrja a� reikna
# uppsafna�a fj�lgun yfir 5 �rin.

myndagogn <- myndagogn %>% 
        mutate(Verdlag_lag = lag(Verdlag, n = nbil))

mynd11 <- ggplot(filter(myndagogn, Verdlag_lag == "Q1"), aes(x = d_reer, y = d_fjoldi)) + geom_point(size = 2) +
        geom_smooth(method = "lm", lwd = 2) + theme_bw() +
        labs(x = "Breyting raungengis seinasta �ri�",
             y = "Breyting � fj�lda fer�manna seinasta �ri�",
             title = "Fyrsti fj�r�ungur") +
        theme(axis.text.x = element_text(size=15),
              axis.text.y = element_text(size=15),
              axis.title.x = element_text(size = 15),
              axis.title.y = element_text(size = 15))+
        scale_x_continuous(labels = percent) +
        scale_y_continuous(labels = percent)
        

mynd21 <- ggplot(filter(myndagogn, Verdlag_lag == "Q2"), aes(x = d_reer, y = d_fjoldi)) + geom_point(size = 2) +
        geom_smooth(method = "lm", lwd = 2) + theme_bw() +
        labs(x = "Breyting raungengis seinasta �ri�",
             y = "Breyting � fj�lda fer�manna seinasta �ri�",
             title = "Annar fj�r�ungur") +
        theme(axis.text.x = element_text(size=15),
              axis.text.y = element_text(size=15),
              axis.title.x = element_text(size = 15),
              axis.title.y = element_text(size = 15))+
        scale_x_continuous(labels = percent) +
        scale_y_continuous(labels = percent)

mynd31 <- ggplot(filter(myndagogn, Verdlag_lag == "Q3"), aes(x = d_reer, y = d_fjoldi)) + geom_point(size = 2) +
        geom_smooth(method = "lm", lwd = 2) + theme_bw() +
        labs(x = "Breyting raungengis seinasta �ri�",
             y = "Breyting � fj�lda fer�manna seinasta �ri�",
             title = "�ri�ji fj�r�ungur") +
        theme(axis.text.x = element_text(size=15),
              axis.text.y = element_text(size=15),
              axis.title.x = element_text(size = 15),
              axis.title.y = element_text(size = 15))+
        scale_x_continuous(labels = percent) +
        scale_y_continuous(labels = percent)

mynd41 <- ggplot(filter(myndagogn, Verdlag_lag == "Q4"), aes(x = d_reer, y = d_fjoldi)) + geom_point(size = 2) +
        geom_smooth(method = "lm", lwd = 2) + theme_bw() +
        labs(x = "Breyting raungengis seinasta �ri�",
             y = "Breyting � fj�lda fer�manna seinasta �ri�",
             title = "Fj�r�i fj�r�ungur") +
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
