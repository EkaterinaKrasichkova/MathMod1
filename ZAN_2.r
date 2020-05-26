#Красичкова Е.Д.ПАЭ 123 гр. Вар.9
#создайте модель множественной линейной регрессии дневных потоков 
#паров воды за период 2013 года по данным измерений методом турбулентной пульсации

library("tidyverse") 
library("readr")    
library("stringr")  
library("dplyr")     
library("ggplot2")   
getwd()
eddypro = read.csv("eddypro.csv", skip = 1, na=c("","NA","-9999","-9999.0"), comment=c("["))
#
#Блок подготовки данных
#
# Удаляем ненужную пустую первую строку
eddypro = eddypro[-1, ]
# Удаляем ненужный пустой столбец "roll"
eddypro = select(eddypro, -(roll))
# Преобразуем в факторы (factor)столбы типа char (символ)
eddypro = eddypro %>% mutate_if(is.character, factor)
#Заменим спец. символы в названии стобцов на допустимые для переменных имена
names(eddypro) = names(eddypro) %>%
  str_replace_all("[!]","_exclam_") %>%
  str_replace_all("[?]", "_quest_") %>% 
  str_replace_all("[*]", "_star_") %>% 
  str_replace_all("[+]", "_plus_") %>%
  str_replace_all("[-]", "_minus_") %>%
  str_replace_all("[@]", "_at_") %>%
  str_replace_all("[$]", "_dollar_") %>%
  str_replace_all("[#]", "_hash_") %>%
  str_replace_all("[/]", "_slash_") %>%
  str_replace_all("[%]", "__pecent_") %>%
  str_replace_all("[&]", "_amp_") %>%
  str_replace_all("[\\^]", "_power_") %>%
  str_replace_all("[()]","_")
#Возвратим столбцы таблицы в виде векторов для проверки
glimpse(eddypro)
# Удалим строки в которых содержится NA, так как они содержат неполные данные и только мешают
eddypro = drop_na(eddypro)

# Отфильтруем данные по заданию только за дневное время
eddypro = filter(eddypro, daytime ==TRUE)
# Получим таблицу, состоящую только из чисел. Будем работать с ней.
eddypro_numeric = eddypro[,sapply(eddypro, is.numeric)]
# Получим таблицу, содержащую остальные колонки
eddypro_non_numeric = eddypro[,!sapply(eddypro, is.numeric)]
#
# Блок создания модели регрессивного анализа
#
# Создадим обучающую и тестирующую непересекающиеся выборки с помощью базового функционала
row_numbers = 1:length(eddypro_numeric$h2o_flux)
teach = sample(row_numbers, floor(length(eddypro_numeric$h2o_flux)*.7))
test = row_numbers[-teach]
#Обучающая выборка
teaching_tbl = eddypro_numeric[teach,]
#Тестирующая выборка
testing_tbl = eddypro_numeric[test,]
# Создадим модель 1, добавив в нее все переменные с помощью "(.)" и используя обучающую выборку
mod1 = lm(h2o_flux~ (.) , data = teaching_tbl)
# Получим информацию о моделе и коэффициенты
summary(mod1)
# Проанализируем переменные по значимости
anova(mod1)
#Выведем графики
plot(mod1)
# Создадим модель 2б добавив в неё значимые переменные из результатов функции anova()(со значимостью до 0.01, соответственно ***,** и * пометки)
mod2 = lm(h2o_flux~ DOY + Tau + qc_Tau + rand_err_Tau + H + qc_H +  rand_err_H  + LE + qc_LE + rand_err_h2o_flux + rand_err_co2_flux + H_strg + h2o_molar_density + h2o_mole_fraction  + h2o_mixing_ratio + co2_molar_density 
          + co2_mole_fraction + co2_mixing_ratio + h2o_time_lag + sonic_temperature  + air_temperature + air_pressure + air_density + air_heat_capacity  + air_molar_volume 
          + water_vapor_density  + e + es + RH +  Tdew + u_unrot + v_unrot + w_unrot + u_rot + v_rot + w_rot + wind_dir + yaw + pitch + TKE + L
          + bowen_ratio + x_peak + x_offset  + x_offset+ x_10. +x_30.+ x_50. +x_70. + un_Tau + H_scf + un_LE + un_co2_flux 
          + un_h2o_flux + h2o_spikes + h2o.1 , data = teaching_tbl)
# Получим информацио о моделе и коэффициенты
summary(mod2)
# Проанализируем переменные по значимости
anova(mod2)
# Сравним с предыдущей моделью, не ухудшилась ли она
anova(mod2, mod1)

# Создадим модель 3, повторив отбрасывание
mod3 = lm(h2o_flux~DOY + Tau + qc_Tau +  qc_H +  rand_err_H  + qc_LE + rand_err_h2o_flux + co2_flux + H_strg + h2o_molar_density + co2_molar_density 
          + co2_time_lag+ h2o_mixing_ratio + co2_molar_density + air_pressure + u_unrot + v_unrot + w_unrot + v_rot + yaw 
          + bowen_ratio + TKE + x_peak + un_h2o_flux, data = teaching_tbl )
# Получим информацио о модели и коэффициенты
summary(mod3)
# Проанализируем переменные по значимости
anova(mod3)
# Сравним с предыдущей моделью, не ухудшилась ли она
anova(mod3, mod2)
# Выведем графики
plot(mod3)

# Проведем корреляционный анализ переменных
# Выберем из таблицы только участвующие у линейной модели переменные 
cor_teaching_tbl = select(teaching_tbl, h2o_flux, DOY, Tau, qc_Tau, qc_H, rand_err_H, qc_LE, rand_err_h2o_flux, h2o_flux,
                          H_strg, co2_molar_density, h2o_molar_density, h2o_time_lag, air_pressure,
                          u_unrot, v_unrot, w_unrot, v_rot, yaw, bowen_ratio,  x_peak, un_h2o_flux)
#Получаем таблицу коэффициентов корреляций. И подправляем модель 3, убирая из модели одну из двух коррелирующих между собой переменных (начиная от коэффициента >= 0.7)
cor_td = cor(cor_teaching_tbl) %>% as.data.frame

#
# Графики по полученной моделе
#
#Проверка модели
#Построим точки h2o_flux от h2o_flux на значениях ОБУЧАЮЩЕЙ выборки. Наложим предсказанные значения по модели 3 на ОБУЧАЮЩЕЙ выборке сверху в виде линии
#В идеале линия должна  пройти через все точки. А так как у нас график h2o_flux от самой себя, то он должен идти под 45градусов
qplot(h2o_flux , h2o_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod3, teaching_tbl)))
#Теперь сделаем тоже самое на ТЕСТИРУЮЩЕЙ выборе
qplot(h2o_flux , h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
#Так как у нас модель зависит от множества переменных, мы можем вывести много графиков зависимостей h2o_flux от учитываемых в моделе параметров
#В идеале предсказанная линия должна пройти через все точки, или как можно ближе к ним на ТЕСТИРУЮЩЕЙ выборке
#Примеры
qplot(DOY, h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
qplot(Tau, h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
qplot(h2o_flux, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
