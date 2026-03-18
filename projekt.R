#Uzywane biblioteki:
#install.packages("mice")
#install.packages("corrplot")
library(dplyr)
library(mice)
library(psych)
library(ggplot2)
library(corrplot)
#Wstęp

#Analizowany zestaw danych dotyczy aktywności użytkowników, udzielających się w komentarzach na Reddicie.
#Szczególny nacisk jest w nim pokładany na odpowiedzi pisane przez boty. 

#Nasz zbiór zawiera kolumny:
#-comment_id - unikalny identyfikator dla każdego komentarza
#-subreddit - nazwa społeczności Reddita, do której możemy zaliczyć poszczególny komentarz
#-account_age_days - wiek konta
#-user_karma - punkty reprezentujące wiarygodność konta i jego wkład w społeczność Redditta
#-reply_delay_soconds - liczba sekund od zamierzczenia posta do poszczególnej odpowiedzi
#-sentiment_score - przedstawia emocjonalny wydźwięk danego komentarza
#-avg_word_length - średnia ilość znaków na słowo
#-contains_links - czy komentarz zawiera linki
#-is_bot_flag - czy komentarz został oznaczony jako napisany przez bota
#-bot_type_label - dzieli boty na poszczególne rodzaje
#-bot_probability - prawdopodobieństwo, że odpowiedź została napisana przez bota


bot_traffic <- read.csv("C:\\Users\\Wiktoria\\Desktop\\STUDIA\\rok 2\\semestr 1\\Analiza danych\\reddit_dead_internet_analysis_2026.csv", sep = ",")

#Czyszczenie danych

#Zaczynam od usunięcia kolumny comment_id, poniważ zawiera identyfikatory komentarzy, które nie nie wnoszą do analizy.
bot_traffic <- bot_traffic %>%
  select(-comment_id)

#Oglądam wartości w zbiorze
head(bot_traffic, 3)
tail(bot_traffic, 3)

#Struktura danych
str(bot_traffic)

#Zmieniam typ danych w kolumnach contains_links i is_bot_flag na wartości kategoryczne ze zmianą nazw wierszy
bot_traffic$is_bot_flag <- as.logical(bot_traffic$is_bot_flag)
bot_traffic$contains_links <- as.logical(bot_traffic$contains_links)
bot_traffic$is_bot_flag = factor(bot_traffic$is_bot_flag, levels = c(FALSE, TRUE), 
                            labels = c("człowiek", "bot"))
bot_traffic$contains_links = factor(bot_traffic$contains_links, levels = c(FALSE, TRUE), 
                     labels = c("nie_zawiera", "zawiera"))


#Ustawiam również kolumny bot_type_label i subreddit jako factor bez określonej kolejności
bot_traffic$bot_type_label <- as.factor(bot_traffic$bot_type_label)
bot_traffic$subreddit <- as.factor(bot_traffic$subreddit)

#Kolumna sentiment_core mówi o tonie poszczególnego kometarza (od negatywnego do pozytywnego), więc podzielę wyniki odzwierciedlające to i dodam na tej postawie nową kolumnę

bot_traffic$sentiment_category <- cut(bot_traffic$sentiment_score,
                                      breaks = c(-Inf, -0.1, 0.1, Inf),
                                      labels = c("Negatywny", "Neutralny", "Pozytywny")) 
table(bot_traffic$sentiment_category)

#Szukam braku danych i wartości odstających

summary(bot_traffic)
bot_traffic %>%
  select(account_age_days, user_karma, reply_delay_seconds, sentiment_score, avg_word_length, bot_probability) %>%
  describe()

#Nie mam braków, ale dla pewności sprawdzam jescze raz

md.pattern(bot_traffic)

#Widzę też, że wystepuja wartości odstające ale na razie zostawiam je w spokoju


#Rozkład zmiennych

#Zaczynam od zmiennych kategorycznych

#Zmienna subreddit
table(bot_traffic$subreddit)

bot_traffic %>%
  ggplot() +
  geom_bar(aes(x = subreddit), color = "darkgreen", fill = "lightgreen") + 
  theme_light() + 
  labs(title = "Społeczności Reddita",
       x = "Społeczność",
       y = "Liczba komentarzy")

#Widać że dane są rozłożone dosyc równomiernie, brak wartości odstających

#Zmienna bot_type_label
table(bot_traffic$bot_type_label)
bot_traffic %>%
  ggplot() +
  geom_bar(aes(x = bot_type_label), color = "darkblue", fill = "lightblue") + 
  theme_light() + 
  labs(title = "Rodzaje botów lub ich brak",
       x = "Rodzaj bota",
       y = "Liczba komentarzy")

#Dominuje liczba komentarzy oznaczonych jak Human, jednak z 500 danych stanowi ona niewiele więcej niż 50%, to raczej mało.

#Zmianna sentiment_category
table(bot_traffic$sentiment_category)

#Widać, że niewiele jest komentarzy nautralych, dominują skrajne odczucia.

#Zmienne logiczne

#Zmianna contains_links
table(bot_traffic$contains_links)

#Parwie 80% naszych danych nie zawiera linków

#Zmianna is_bot_flag
table(bot_traffic$is_bot_flag)


#Zmienne ilościowe

#Zmienna account_age_days

bot_traffic %>%
  ggplot() +
  geom_density(aes(x = account_age_days),
               color = "darkgreen", fill = "lightgreen") +
  theme_light() +
  labs(title = "Wiek konta w dniach",
       x = "dni")
#Rozkład wieku konta charakteruzuję się umiarkowaną skośnością prawostronną. Widać że największe zagęszczenie pbserwujemy dla konta stosunkowo młodych (do 500 dni). Od tego momentu spadki są niewielkie az do wartości 2500.

#Spójrzmy na wykres pudełkowy w poszukiwaniu potencjalnych wartości odstających

bot_traffic %>%
  ggplot() +
  geom_boxplot(aes(x = account_age_days),
               color = "darkgreen", fill = "lightgreen") +
  theme_light() +
  labs(title = "Wiek konta w dniach",
       x = "dni")

#Jak widzimy wartości odstających brak.

#Zmienna user_karma
describe(bot_traffic$user_karma)

bot_traffic %>%
  ggplot() +
  geom_density(aes(x = user_karma),
               color = "black", fill = "lightgrey") +
  theme_light() +
  labs(title = "Reputacja konta",
       x = "wartość")
#Zmienna user_karma ma dosyć symetryczny rozkład, przy czym widzimy że zdecydowanie najmniej jest uzytkowników z bardzo niską i bardzo wyską reputacją.

#Narysujmy jeszcze wykres pudełkowy w poszukiwaniu wartości odstających
bot_traffic %>%
  ggplot() +
  geom_boxplot(aes(x = user_karma),
               color = "black", fill = "lightgray") +
  theme_light() +
  labs(title = "Reputacja konta",
       x = "wartość")
#Wartości odstających brak

#Zmianna replay_delay_seconds

bot_traffic %>%
  ggplot() +
  geom_histogram(aes(x = reply_delay_seconds),
                 bins = 10,
                 color = "darkblue", fill = "grey") +
  theme_light() +
  labs(title = "Czas odpowiedzi w sekundach",
       x = "wartość")
#Zdecydowana część odpowiedzi to komentarze dodane do 10 minut po zamieszczeniu postu.

summary(bot_traffic$reply_delay_seconds)
#Patrząc dodatkowo na statystyki tej zmiennej można stwierdzić że istnieją liczne outlinery ale na razie nie usuwam tych wartości

#Zmianna avg_word_length
describe(bot_traffic$avg_word_length) 

bot_traffic %>%
  ggplot() +
  geom_density(aes(x = avg_word_length),
               color = "darkgreen", fill = "grey") +
  theme_light() +
  labs(title = "Średnia długość słów",
       x = "wartość")
#Widzimy, że rozkład jest prawie symetryczny. Największe zagęszczenie mozna zaobserwować miedzy wartościami 5.5 i 6, choć nie sądzę, że tak zmienna jest znacząca dla naszej analizy.

#Zmianna bot_probability
summary(bot_traffic$bot_probability)

bot_traffic %>%
  ggplot() +
  geom_histogram(aes(x = bot_probability),
               color = "black", fill = "purple") +
  theme_light() +
  labs(title = "Prowdopodobieństwo wystapienia bota",
       x = "wartość")
#Wykres pokrywa się z informacjami ze zmiennej bot_type_label, ponieważ przeszło połowa komentarzy okazała się być napisana przez człowieka. 
#Wartości powyżej 3.5 są naszymi outlinerami, które mogły zostać oznaczone jako boty, choć po raz kolejny odwołując się do wniosków z rozkładu zmiennej bot_type_label, 3.5 to nie jest wartość, od której swierdzamy czy dany komentarz został napisany przez bota.

#Wyznaczmy korelację między zmiennymi
cor = cor(bot_traffic[,c("account_age_days", "user_karma", "reply_delay_seconds", 
                      "sentiment_score", "avg_word_length", "bot_probability")], 
          method = "spearman")

corrplot(cor, method = "square", type = "upper")

#Z powyższej macierzy korelcji możemy wywniskować, że:
#-zmienne avg_word_length i bot_probability są ze sobą silnie skorelowane liniowo, co jest dosyć zaskakujące
#-account_age_days i reply_delay_seconds są również skorelowane liniowo choć w mniejszym stopniu
#- występuje mocna nieliniowa korelacja między czasem odpowiedzi a prawdopodobieństwem wystapienia bota
#- nieliniowe korelacje możemy również zaobserwować między zmiennymi reply_delay_seconds oraz sentiment_score i avg_word_length, user_karma i sentiment_score, tudzież account_age_days oraz avg_word_length i bot_probability

#Przeanalizujmy jeszcze zmienne kategoryczne i logiczne względem siebie

#subreddit a bot_type_label
bot_traffic %>%
  ggplot() +
  geom_bar(aes(x = subreddit, fill = bot_type_label),
           color = "white", 
           position = "dodge2") +
  theme_light() +
  labs(title = "Społeczność Reddita a botyz wyszczególnieniem na rodzaje",
       x = "subreddit",
       y = "liczba komentarzy",
       fill = "rodzaje botów")

#subreddit a is_bot_flag
bot_traffic %>%
  ggplot()+
  geom_bar(aes(x = is_bot_flag, fill = is_bot_flag),
           color = "white", 
           position = "dodge2") +
  facet_wrap(~subreddit, ncol = 3) +
  theme_light() +
  scale_fill_discrete(limits = c("bot", "człowiek")) +
  labs(title = "Społeczność Reddita a boty",
       x = "subreddit",
       y = "liczba komentarzy",
       fill = "czy bot")

#conatins_links a is_bot_flag
table(bot_traffic[,c("contains_links", "is_bot_flag")])

bot_traffic %>%
  ggplot() +
  geom_bar(aes(x = contains_links, fill = is_bot_flag),
           color = "white", 
           position = "dodge2") +
  scale_fill_discrete(limits = c("bot", "człowiek")) +
  theme_light() +
  labs(title = "Zawartość linków a boty",
       x = "czy zawira linki",
       y = "liczba komentarzy",
       fill = "czy bot")

#is_bot_flag a sentiment_category
bot_traffic %>%
  ggplot() +
  geom_bar(aes(x = sentiment_category, fill = is_bot_flag),
           color = "white", 
           position = "dodge2") +
  theme_light() +
  scale_fill_discrete(limits = c("bot", "człowiek")) +
  labs(title = "Ton komentarza a boty",
       x = "wydźwięk komentarza",
       y = "liczba komentarzy",
       fill = "czy bot")

table(bot_traffic[,c("is_bot_flag", "sentiment_category")])

#Pytania badawcze
#Spróbujmy przyjrzeć się prawdopodobieństwu wystąpienia bota i zmiennym, które z tym korelują. Od jakiej wartości system stwierdza, że to bot? W jakim stopniu inne zmienne wpływają te liczby?

#Pytanie 1: Od której wartości komentarz oznaczany jest jako nie napisany przez człowieka?

bot_traffic %>%
  ggplot() +
  geom_density(aes(x = bot_probability, fill = is_bot_flag), 
             color = "black", alpha = 0.2) +
  scale_fill_discrete(limits = c("bot", "człowiek")) +
  theme_light() +
  labs(title = "Związek między oznaczeniem jako bot a prawdopodobieństwem że to bot",
       x = "Prawdopodobieństwo wystąpienia bota",
       y = "Występowanie")
#Widzimy, że wartość prawdopodobieństwa jest mniejsza niż 0.15, by komentarz został oznaczony jako napisany przez bota. Podziałmiędzy ludźmi a botami jest bardzo wyraźny. Strefa, w której system jest niezdecydowany, czy oznaczyć dany komentarz jako bota lub człowieka, jest niewielka.

#Spórzmy jeszcze na statystyki opisowe
bot_traffic %>%
  group_by(is_bot_flag) %>%
  summarise(
    Min = min(bot_probability),
    Mediana = median(bot_probability),
    Max = max(bot_probability))

#Widzimy, że nasza wartość dzięki której system będzie wiedizał czy to bot znajduje się w przedziale 0.13 - 0.14.
#Możemy również dostrzec, że jest niewiele komentarzy, których jesteśmy połowicznie pewni, że sa wygenerowane przez boty. Dużą gestością charkteryzyje się jedank wartość około 0.2. Czy możemy stwierdzić która zmienna na to wpływa?

#Pytanie 2: Jak prezentuję się zależność miedzy umieszczeniem linku w komentarzu a prawdopodbieństwem bota?
#Widzieliśmy, że wszystkie komentarze z linkami są oznaczane jako boty. Sprwdźmy jak to się przekłada na prawdopodobieństwo.

bot_traffic %>%
  group_by(contains_links) %>%
  summarise(
    Min = min(bot_probability),
    Mediana = median(bot_probability),
    Max = max(bot_probability))

bot_traffic %>%
  ggplot() +
  geom_density(aes(x = bot_probability, fill = contains_links), 
               color = "black", alpha = 0.2) +
  scale_fill_discrete(limits = c("zawiera", "nie_zawiera")) +
  theme_light() +
  labs(title = "Związek między obecnością linku a prawdopodobieństwem że to bot",
       x = "Prawdopodobieństwo wystąpienia bota",
       y = "Występowanie")
#Wykresy gęstości zmiennych się na siebie nakładają. Nie możemy jednoznacznie stwierdzić, że tylko boty zamieszczają linki. 
#Możemy jednak stwierdzić zawartość linku znacznie podwyższa nam "górkę" w oklicach wartości 0.2.

#Pytanie 3: Jak wyglada zależność między średnią dłogością słów a prawdopodobieństwem wystapienia bota?
#Wiemy, że zmienna avg_word_length koreluje prawie współliniowo z wartością określającą prawdopodobieństwo identyfikacji bota. 
#Sprawdźmy czy możemy stwierdzić jakie średnie dłygości słów są charakterystyczne dla botów (według systemu Reddita)

bot_traffic %>%
  ggplot() +
  geom_point(aes(x = bot_probability, y = avg_word_length), size = 2, 
             color = "grey50", alpha = 0.4) +
  theme_light() +
  labs(title = "Prawdopodobieństwo wystapienia bota a średnia długość słów",
       x = "Prawdopodobieństwo wystąpienia bota",
       y = "Długosć słów")
#Widzimy, że kometarze oznaczone jako napisane przez człowieka trworzą prawie liniowy trend z niewielkimi odstępstwami - im większa średnia długość tym większe prawdopodobieństwo. Naszą szarą strefą są tutaj wartości 0d 5.5 do 6 - system jest tu niezdecydowany jak zidentifikować komentarz. 
#Odcięcie jest tu, jednak wyraźne - komentarze o średniej długości słów większej niż 5.5 mają szansę być uznane za wygenerowane przez boty.
#Spójrzmy nastepnie na naszą szarą strfę (długość słów od 5.5 do 6). Jak inne zmienne pomagają w tym przypadku identyfikować boty?

#Pytanie 4: Jak zmianna reply_delay_seconds wpływa na  parwdopodobieństwo wystąpienia bota

bot_traffic %>%
  ggplot() +
  geom_point(aes(x = bot_probability, y = reply_delay_seconds), size = 2, 
             color = "grey50", alpha = 0.4) +
  scale_y_log10() +
  theme_light() +
  labs(title = "Prawdopodobieństwo wystapienia bota a szybkość odpowiedzi",
       x = "Prawdopodobieństwo wystąpienia bota",
       y = "Szybkość odpowiedzi")
#Widzimy, że odpowiedzi do 10 sekund są oznaczane jako generowane przez boty.
#Sprawdźmy, czy ta zmienna może być decydująca, w przypadku niepewności identifikacji.

#Pytanie 5: Czy możemy stwierdzić, że wartość zmiennej reply_dalay_seconds roztrzyga kwestię wystąpienia bota (lub pomaga w tym), w przypadku gdy analiza średniej długości słów nie wystarcza?

bot_traffic %>%
  filter(avg_word_length >= 5.5 & avg_word_length <= 6.0) %>%
  group_by(is_bot_flag) %>%
  summarise(
    Liczba_przypadkow = n(),
    Srednia_delay = mean(reply_delay_seconds),
    Mediana_delay = median(reply_delay_seconds),
    Min_delay = min(reply_delay_seconds),
    Max_delay = max(reply_delay_seconds)
  )
#Widzimy, że czas reakcji jest zmienną, która nie pomoże algorytmowi stwierdzić, dany komentarz powstał za sprawą bota.
#Wynika to m.in ze średnich - prawie 30 minut dla człowieka i trochę ponad 6 sekund dla bota. Reply_delay_seconds koreluje z avg_word_length, lecz sama w sobie jest silnym czynnikiem determinującym oznaczenie komenatrza jako bot.
#Z średnią długością słow koreluje także wiek konta. Może ta zmienna jest naszym "pomocnikiem".

#Pytanie 6: Czy możemy stwierdzić, że wartość zmiennej account_age_days roztrzyga kwestię wystąpienia bota (lub pomaga w tym), w przypadku gdy analiza średniej długości słów nie wystarcza?
bot_traffic %>%
  filter(avg_word_length >= 5.5 & avg_word_length <= 6.0) %>%
  group_by(is_bot_flag) %>%
  summarise(
    Liczba_przypadkow = n(),
    Srednia_days = mean(account_age_days),
    Mediana_days = median(account_age_days),
    Min_days = min(account_age_days),
    Max_days = max(account_age_days)
  )
#Tutaj róznice już nie są aż tak znaczące. Nie obserwujemy aż tak dużej przepaści między średnimi, lecz wartości median są już bardziej zróżnicowane. Jeśli popatrzymy na wartości minimalne i maksymalne zarówno dla ludzi i botów, dostrzeżemy, że nakładają sie na siebie.
#Na podstawie tego możemy załozyć, że zmienna account_age_days jest zmienną pomocniczą, analiza średniej ilości słów zawodzi.

#Przyjrzyjmy się jeszcze cechą botów które mogły by uchodzić za człowieka (0.11 - 0.17, gdzie nasze dane z piewszego pytania się nakładają) i tym, dla których prawdopodobieństwo wynosi 35% i więcej.

#Pytanie 7: Czym cechują się boty z najniższym prawdopodobieństwem identyfikującym ich jako boty?
#Wyznaczmy statystyki
ludzkie_boty <- bot_traffic %>%
  filter(is_bot_flag == "bot") %>%
  filter(bot_probability >= 0.11 & bot_probability <= 0.17) %>%
  select(subreddit, account_age_days, reply_delay_seconds, avg_word_length, contains_links, bot_type_label)
summary(ludzkie_boty)

#Podział ze względu na rodzaje botów i elementy kluczowe w rozpoznawaniu bota
ludzkie_boty %>%
  ggplot() +
  geom_point(aes(x = bot_type_label, y = avg_word_length, color = contains_links),
             size = 2.5, alpha = 0.6) + 
  theme_light() +
  labs(title = "Średnia długość słów z zależności od rodzaju bota i obecności linków",
       x = "rodzaj bota",
       y = "średnia długość słów")
#Najliczniejszą grupą jest Engagement Farmer - ma on dużą rozpiętość średniej długości słów, w większości przypadków zawira linki (może to oznaczać, że licząc średnią ilość znaków na słowo, nie liczymy linków, poniważ spodziewałam się, że linki będą podwyższać tę wartość). Zaraz po nim jest Reprint Bot - tutaj bardziej trzymamy się szarej strefy, jeśli chodzi o wartość zmiennej avg_word_length, w większości przypadków nie zawira linków.
#Jako ostani jest AI Summarizer - prawie wcale nie zawira linków, chrakteryzuję się raczej krótszą długością słów (poza dwoma przypadkami).

#Pytanie 8: Czym cechują się boty z najwyzszym prawdopodobieństwem identyfikującym ich jako boty?
#Wyznaczmy statystyki
napewno_boty <- bot_traffic %>%
  filter(is_bot_flag == "bot") %>%
  filter(bot_probability >= 0.35) %>%
  select(subreddit, account_age_days, reply_delay_seconds, avg_word_length, contains_links, bot_type_label)
summary(napewno_boty)

napewno_boty %>%
  ggplot() +
  geom_point(aes(x = bot_type_label, y = avg_word_length, color = contains_links),
             size = 2.5, alpha = 0.6) + 
  theme_light() +
  labs(title = "Średnia długość słów z zależności od rodzaju bota i obecności linków",
       x = "rodzaj bota",
       y = "średnia długość słów")
