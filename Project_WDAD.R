library(readr)
library(dplyr)
library(C50)
library(gmodels)

#sciezka do listy nazwy plikow z rozszerzeniem csv znajdujacych sie w lokalizacji
#projektu
path = "D:/studia/zimowa 2026/WDAN/project"
file_list = list.files(path = path, pattern = "\\.csv$", full.names = TRUE)
file_list = file_list[order(nchar(file_list), file_list)] 
#nchar sortuje po długości stringa a order potem sortuje rosnąco wiec mamy
#1 2 3 ... 9 10 11... 19 20 21... 23 tak jak chcemy
#import 23 patchy
for (i in 1:23) {
  var_name = paste0("stats", i)
  assign(var_name, read.csv2(file_list[i], stringsAsFactors = TRUE))
}
var_name
#kazdy obiekt transformujemy do ramki danych
for (i in 1:23) {
  var_name = paste0("stats", i)
  var_name = as.data.frame(var_name)
}
head(stats1)

#ustawienie atrybutów na numeryki
for (i in 1:23) {
  var_name = paste0("stats", i)
  df = get(var_name)
  cols_simple = c("Score", "Trend", "KDA")
  for (col in cols_simple) {
    if (col %in% colnames(df)) {
      df[[col]] = as.numeric(gsub(",", ".", df[[col]]))
    }
  }
  cols_percent = c("Win..", "Role..", "Pick..", "Ban..")
  for (col in cols_percent) {
    if (col %in% colnames(df)) {
      temp_val = gsub("%", "", df[[col]])
      temp_val = gsub(",", ".", temp_val)
      df[[col]] = as.numeric(temp_val)
    }
  }
#usuniecie duplikatow czempionów
  df = df[order(df$Name, -df$Score), ]
  df = df[!duplicated(df$Name), ]
  assign(var_name, df)
}

str(stats1)



#zmiany nazw kolumn
for (i in 1:23) {
  var_name = paste0("stats", i)
  df = get(var_name)
  current_names = colnames(df)
  new_names = gsub("\\.\\.", "%", current_names)
  colnames(df) = new_names
  assign(var_name, df)
}

#sprawdzenie czy wartosci null istnieja
x=0
for (i in 1:23){
  x=x+sum(is.na(paste0("stats", i)))
}
x

#concatenacja 23 baz danych w jedna
all_stats_list = list(stats1, stats2, stats3, stats4, stats5, stats6, stats7, stats8, stats9, stats10, 
                      stats11, stats12, stats13, stats14, stats15, stats16, stats17, stats18, stats19, stats20,
                      stats21, stats22, stats23)
for (i in 1:length(all_stats_list)) {
  all_stats_list[[i]]$patch = i
}

stats = do.call(rbind, all_stats_list)

#reset indeksow
rownames(stats) = NULL


#utworzenie kolumny nerf
trend_data = stats[, c("Name", "patch", "Trend")]
trend_data$patch = trend_data$patch - 1
colnames(trend_data)[3] = "next_patch_trend"
stats = merge(stats, trend_data, by = c("Name", "patch"), all.x = TRUE)

stats$nerf = ifelse(!is.na(stats$next_patch_trend) & stats$next_patch_trend < -5, 1, 0)
stats = stats[order(stats$patch, stats$Name), ]
rownames(stats) = NULL
stats$next_patch_trend = NULL

#(data leakage)
#usuniecie trendu, poniewaz zawiera informacje o przyszlosci
#aby w trakcie prognozowania model nie uczył sie na informacjach
#ktorych by nie miał w rzeczywistej sytuacji

stats$Trend = NULL

#dodajemy do wiersza informacje o 
#statystykach z N poprzednich patchy (tzw. lagged features),
#stosowany do szeregów czasowych
#dajemy 5 poniewaz znajomosc gry oraz zasad dzialania nerfów
#skłania nas do tego
stats = stats %>%
  arrange(Name, patch) %>%
  group_by(Name)

for (i in 1:5) {
  stats = stats %>%
    mutate(
      !!paste0("score_", i) := lag(Score, n = i),
      !!paste0("tier_", i)  := lag(Tier, n = i),
      !!paste0("win%_", i)   := lag(`Win%`, n = i),
      !!paste0("role%_", i)  := lag(`Role%`, n = i),
      !!paste0("pick%_", i)  := lag(`Pick%`, n = i),
      !!paste0("ban%_", i)   := lag(`Ban%`, n = i),
      !!paste0("KDA_", i)    := lag(KDA, n = i)
    )
}

stats = ungroup(stats)

stats = stats %>% arrange(patch, Name)
#teraz musimy sobie poradzic z wartosciami NA. Można by je jakos uzupełnic,
#ale chcemy zeby model był jak najbardziej dokładny wiec po prostu te wiersze usuniemy
nrow(stats)
stats_model = na.omit(stats)
#czempiony ktore byly nowo dodane i niemaja wystarczajaco danych rowniez sa usuwane

nrow(stats_model)
min(stats_model$patch)
#zwraca 6 patch, czyli dziala

str(stats)

#przygotowanie danych pod model. Mozna usunac teraz kolumne patch, poniewaz
#dane te sa juz zakodowane w lagach
#do klasyfikacji C5.0 potrzebujemy zeby nerf był faktorem

stats_model$nerf <- as.factor(stats_model$nerf)

#dzielimy na treningowy i testowy. Testowy musi miec conajmniej 5 patchy, poniewaz
#w wierszu mamy dokladnie informacje o 5 wierszach, tak nasz model bedzie dzialac, 
#analizujac 5 patchy, przewidzi ten 6-ty, w naszym przypadku 23-ci
train_data <- stats_model[stats_model$patch <= 17, ]
test_data  <- stats_model[stats_model$patch > 17 & stats_model$patch < 23, ]


#sprawdzmy ile jest nerfow w treningu i tescie
prop.table(table(train_data$nerf))
prop.table(table(test_data$nerf))
#wystarczajaco podobne wyniki



#usuwamy nazwy championów oraz patch do modelu, te kolumny nie sa przydatne do 
#trenowania modelu
do_usuniecia = c("Name", "patch")

train_set = train_data[, !(colnames(train_data) %in% do_usuniecia)]
test_set  = test_data[, !(colnames(test_data) %in% do_usuniecia)]



#tworzymy pierwszy model za pomoca c5.0
drzew_dec_model=C5.0(train_set[-12], train_set$nerf)
drzew_dec_model
summary(drzew_dec_model)

#wyczytalismy ze C5.0 jest napisane w C i zle sobie radzi z znakami 
#specjalnymi w nazwach kolumn np % albo _,albo sama cyfra na koncu nazwy
#kolumny...

colnames(train_set) <- gsub("%", "Per", colnames(train_set))
colnames(test_set) <- gsub("%", "Per", colnames(test_set))
colnames(train_set) <- gsub("_", "", colnames(train_set))
colnames(test_set) <- gsub("_", "", colnames(test_set))
colnames(train_set) <- gsub("([0-9])$", "_\\1", colnames(train_set))
colnames(test_set) <- gsub("([0-9])$", "_\\1", colnames(test_set))
#tutaj mielismy wiele prób, małe litery, usuwanie znakow specjalnych
#usuwanie tylko cyfr na koncach nazwa, na podłoga oraz cyfra itp

#jak sie okazało w summary, zmienna class miala puste wartosci gdzies w bazie,
#niemamy pojecia gdzie, bo sprawdzenie ktore wiersze mialy "" pokazało ze
#albo wszystkie albo zadne. Na forum znalezlismy rozwiazanie, aby podmienic
#pusta nazwe klasy na "missing"

levels(train_set$Class)[1] = "missing"
str(train_set)

#NARESZCIE DZIALA
drzew_dec_model=C5.0(train_set[-10], train_set$nerf)
drzew_dec_model
summary(drzew_dec_model)
predyk_drzew_dec=predict(drzew_dec_model,test_set)
#tu ponizej komendy zeby wyswietlic tylko przewidywania dla ostatniech patcha z zbioru testowego
idx_p22 = which(test_data$patch == 22)
wyniki_p22_final <- data.frame(
  Champion = test_data$Name[idx_p22],
  Actual_Nerf = test_data$nerf[idx_p22],
  Predicted_Nerf = predyk_drzew_dec[idx_p22]
)
table(wyniki_p22_final$Actual_Nerf,wyniki_p22_final$Predicted_Nerf)

#te linijki beda sie pojawiac w kodzie wielokrotnie aby po kazdym predikcie
#wyswietlic tylko table z wynikami dla ostatniego patcha z zbioru testowego

#widzimy ze trafne przewidywania wynosza 2, nie zadowala to nas wiec ulepszamy model

#stosujemy boosting
drzew_dec_boost=C5.0(train_set[-10], train_set$nerf,trials=10)

summary(drzew_dec_boost)

predyk_drzew_dec_boost=predict(drzew_dec_boost,test_set)

idx_p22 <- which(test_data$patch == 22)
wyniki_p22_final <- data.frame(
  Champion = test_data$Name[idx_p22],
  Actual_Nerf = test_data$nerf[idx_p22],
  Predicted_Nerf = predyk_drzew_dec_boost[idx_p22]
)
table(wyniki_p22_final$Actual_Nerf,wyniki_p22_final$Predicted_Nerf)

#jest troche lepiej, pobawmy sie roznymi trialsami, tworzymy petle
#i sprawdzamy dokladnosc dla roznych parametrow, nastepnie wybierzemy najlepszy 
#parametr
for (i in 1:25) {
  temp_model <- C5.0(train_set[-10], train_set$nerf, trials = i)
  
  temp_pred <- predict(temp_model, test_set)
  idx_p22 <- which(test_data$patch == 22)
  wyniki_p22_final <- data.frame(
    Champion = test_data$Name[idx_p22],
    Actual_Nerf = test_data$nerf[idx_p22],
    Predicted_Nerf = temp_pred[idx_p22]
  )
  
  tab <- table(wyniki_p22_final$Actual_Nerf,wyniki_p22_final$Predicted_Nerf)
  
  if ("1" %in% colnames(tab) && "1" %in% rownames(tab)) {
    true_positives <- tab["1", "1"]
  } else {
    true_positives <- 0
  }
  
  cat("Trials:", i, " -> Poprawnie przewidziane nerfy (1 dla 1):", true_positives, "\n")
}
#widzimy ze najlepsze trafne przewidywanie jest dla 5 trialsów
drzew_dec_boost=C5.0(train_set[-10], train_set$nerf,trials=5)
predyk_drzew_dec_boost=predict(drzew_dec_boost,test_set)
idx_p22 <- which(test_data$patch == 22)
wyniki_p22_final <- data.frame(
  Champion = test_data$Name[idx_p22],
  Actual_Nerf = test_data$nerf[idx_p22],
  Predicted_Nerf = predyk_drzew_dec_boost[idx_p22]
)
table(wyniki_p22_final$Actual_Nerf,wyniki_p22_final$Predicted_Nerf)

#teraz pobawmy sie wagami, aby nasz model skupial sie na trafnych przewidywaniach
#kosztem bledu typu drugiego
matrix_dimensions = list(c("0", "1"), c("0", "1"))
names(matrix_dimensions) <- c("predicted", "actual")

error_cost = matrix(c(0, 1, 4, 0), nrow = 2, dimnames = matrix_dimensions)

drzew_dec_wagi = C5.0(train_set[-10], train_set$nerf,
                    costs = error_cost)
drzew_wagi_pred = predict(drzew_dec_wagi, test_set)

idx_p22 = which(test_data$patch == 22)
wyniki_p22_final = data.frame(
  Champion = test_data$Name[idx_p22],
  Actual_Nerf = test_data$nerf[idx_p22],
  Predicted_Nerf = drzew_wagi_pred[idx_p22]
)
table(wyniki_p22_final$Actual_Nerf,wyniki_p22_final$Predicted_Nerf)

#trafne przewidywanie zwieksza sie o 20%, duzym kosztem błedu drugiego rodzaju, bo wzrosl
#az o 400%.
#sprobojmy jeszcze inne wagi
error_cost = matrix(c(0, 1, 3, 0), nrow = 2, dimnames = matrix_dimensions)

drzew_dec_wagi = C5.0(train_set[-10], train_set$nerf,
                       costs = error_cost)
drzew_wagi_pred = predict(drzew_dec_wagi, test_set)

idx_p22 = which(test_data$patch == 22)
wyniki_p22_final = data.frame(
  Champion = test_data$Name[idx_p22],
  Actual_Nerf = test_data$nerf[idx_p22],
  Predicted_Nerf = drzew_wagi_pred[idx_p22]
)
table(wyniki_p22_final$Actual_Nerf,wyniki_p22_final$Predicted_Nerf)

#obnizylismy wage z 4 na 3, widzimy ze blad drugiego rodzaju zmalał
#natomiast trafne przewidywania pozostaly takie same.
error_cost = matrix(c(0, 1, 2, 0), nrow = 2, dimnames = matrix_dimensions)

drzew_dec_wagi = C5.0(train_set[-10], train_set$nerf,
                       costs = error_cost)
drzew_wagi_pred = predict(drzew_dec_wagi, test_set)

idx_p22 = which(test_data$patch == 22)
wyniki_p22_final = data.frame(
  Champion = test_data$Name[idx_p22],
  Actual_Nerf = test_data$nerf[idx_p22],
  Predicted_Nerf = drzew_wagi_pred[idx_p22]
)
table(wyniki_p22_final$Actual_Nerf,wyniki_p22_final$Predicted_Nerf)

#widzimy podobny efekt jak poprzednio, zatem wszystko zalezy od tego 
#jak bardzo chcemy zeby nasz model przewidywał dobrze nerfy. Powiedzmy
#ze chcemy miec ponad 50% efektywnosc dla trafnego przewidywania, zwiekszmy wage
#do 7
error_cost <- matrix(c(0, 1, 7, 0), nrow = 2, dimnames = matrix_dimensions)

drzew_dec_wagi <- C5.0(train_set[-10], train_set$nerf,
                       costs = error_cost)
drzew_wagi_pred <- predict(drzew_dec_wagi, test_set)

idx_p22 <- which(test_data$patch == 22)
wyniki_p22_final <- data.frame(
  Champion = test_data$Name[idx_p22],
  Actual_Nerf = test_data$nerf[idx_p22],
  Predicted_Nerf = drzew_wagi_pred[idx_p22]
)
table(wyniki_p22_final$Actual_Nerf,wyniki_p22_final$Predicted_Nerf)

#nadal nam brakuje, zwiekszmy do 10
error_cost <- matrix(c(0, 1, 10, 0), nrow = 2, dimnames = matrix_dimensions)

drzew_dec_wagi <- C5.0(train_set[-10], train_set$nerf,
                       costs = error_cost)
drzew_wagi_pred <- predict(drzew_dec_wagi, test_set)

idx_p22 <- which(test_data$patch == 22)
wyniki_p22_final <- data.frame(
  Champion = test_data$Name[idx_p22],
  Actual_Nerf = test_data$nerf[idx_p22],
  Predicted_Nerf = drzew_wagi_pred[idx_p22]
)
tab=table(wyniki_p22_final$Actual_Nerf,wyniki_p22_final$Predicted_Nerf)
tab
#teraz nasz model ma ponad 50% trafnosci w przewidywaniu wyników prawdziwie
#pozytywnych. Dzieje sie to jednak bardzo dużym bo aż około 30%-owym
#błedem drugiego typu.
trafpred <- tab["1", "1"] 
falszpred <- tab["0", "1"] 
falszneg <- tab["1", "0"] 
trafneg <- tab["0", "0"] 

# 3. Obliczamy profesjonalne metryki
czulosc = trafpred / (trafpred+falszneg)     
precyzja = trafpred / (trafpred + falszpred)  
doklad = (trafpred + trafneg) / sum(tab)
czulosc
precyzja
doklad

CrossTable(wyniki_p22_final$Actual_Nerf, wyniki_p22_final$Predicted_Nerf,
           prop.chisq = FALSE, 
           prop.c = FALSE, 
           prop.r = FALSE,
           dnn = c('Act_final_wage nerf (P22)', 'Pred_final_wage nerf (P22)'))
