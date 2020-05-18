A_bot <- function(hist_price, bid_ask, account_info, temp_data){
  #Подгрузка пакетов (только на 1 секунде, чтобы не делать это 31000 раз)
  #Создание временной таблицы с информацией о теукщей позиции (0 - не в позиции, 1 - long, -1 - short) и доходности по ней
  #Темплейт таблицы задаем только на 1 секунде, в которой начинаем торговать (первые 17 секунд обозреваем рынок), далее она будет нам передаваться в функцию через input()
  if (tail(hist_price$TIME, 1) == 2080){
    require(dplyr)
    require(TTR)
    temp_data = data.frame("profit" = 0, "last_position" = 0)
  }
  #Расчет индикатора и получение сигналов (для расчета slowD нужно 17 периодов, поэтому подождем 17 секунд). Если их меньше, то просто вернем пустой action и temp_data.
  if (nrow(hist_price) > 17){
    hist_price <- hist_price %>%
      select(HIGH, LOW, CLOSE)
    a <- stoch(hist_price)
    hist_price <- cbind(hist_price, a)
    rm(a)
    hist_price <- hist_price %>%
      select(-fastD) %>%
      #Если fastK пересекает slowD сверху вниз и slowD в зоне перекупленности 80%, тогда формируем сигнал на продажу. В случае, когда FastK пересекает slowD снизу вверх, а slowD находится в зоне перепроданности 20%, формируем сигнал на покупку. Иначе, ничего не делаем.
      mutate("signal" = ifelse(fastK < slowD & lag(fastK, n = 1L) > lag(slowD, n = 1L) & slowD >= 0.8, -1, ifelse(fastK > slowD & lag(fastK, n = 1L) < lag(slowD, n = 1L) & slowD <= 0.2, 1, 0)),
             #Сформируем вектор доходностей
             "returns" = c(0, log(tail(CLOSE, -1)/head(CLOSE, -1)))
      ) %>%
      #Поскольку первое значение slowD будет рассчитано только на 18 секунде, то оставим только те строчки, в которых нет строк с пропущенными значениями
      slice(18:nrow(hist_price))
    #Перенесем в таблицу temp_data результаты наших торгов по последней позиции (которые могли занять ранее)
    #В случае, если на предыдущей секунде мы вошли в длинную позицию, мы переносим значение доходности за прошедшую секунду в столбец profit и будем заносить каждую секунду до тех пор, пока не продадим его (пока last_position не сменится с 1). Аналогично с продажей (только там доходность переносим с обратным знаком)
    if (tail(temp_data$last_position, 1) == 1){
      temp_data = rbind(temp_data, data.frame("profit" = tail(hist_price$returns, 1), "last_position" = 1))
    } else {
      if (tail(temp_data$last_position, 1) == -1){
        temp_data = rbind(temp_data, data.frame("profit" = -tail(hist_price$returns, 1), "last_position" = -1))
      } else {
        temp_data = temp_data
      }
    }
    #Stop Loss & Take Profit & Интерпретируем сигналы. Stop Loss на уровне 2% убытка, Take Profit, если лучшая цена предложения на предыдущей секунде больше, чем 2/3 максимальной цены за весь прошедший период.
    #Если сигнал 1, значит покупаем по рыночной заявке, если -1, значит продаем по рыночной заявке при условии, что у нас имеется этот актив (не берем в долг у брокера)
    if ((sum(temp_data$profit) < -0.02 | tail(bid_ask$ask, 1) > 0.95 * max(hist_price$HIGH)) & account_info$assets > 0){
      action = data.frame("action" = 0, "orderno" = NA, "buysell" = "S", "price" = NA, "volume" = account_info$assets)
      temp_data = data.frame("profit" = 0, "last_position" = 0)
    } else {
      if (tail(hist_price$signal, 1) == 1){
        action = data.frame("action" = 0, "orderno" = NA, "buysell" = "B", "price" = NA, "volume" = round((0.02 * account_info$cash)/tail(bid_ask$ask, 1)))
        temp_data = data.frame("profit" = sum(temp_data$profit), "last_position" = 1)
      } else {
        if (tail(hist_price$signal, 1) == -1 & account_info$assets > 0){
          action = data.frame("action" = 0, "orderno" = NA, "buysell" = "S", "price" = NA, "volume" = account_info$assets)
          temp_data = data.frame("profit" = 0, "last_position" = 0)
        } else {
          action = data.frame("action" = NA, "orderno" = NA, "buysell" = NA, "price" = NA, "volume" = NA)
        }
      }
    }
  } else {
    action = data.frame("action" = NA, "orderno" = NA, "buysell" = NA, "price" = NA, "volume" = NA)
  }
  return(list(action = action, temp_data = temp_data))
}