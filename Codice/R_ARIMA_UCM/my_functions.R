

change = function(x){
  # Creo un ulteriore format per la data che sia più fruibile per xts.
  hour = as.numeric(x[2])
  if (hour < 10){ 
    return(paste0('0', hour, ':00:00'))
  } else {
    return(paste0(hour, ':00:00'))
  }
}

plot_seasonility = function(df, start, end, s){
  df$VALORE = as.numeric(df$VALORE)
  df[start:end,c("FullDate","VALORE")] %>%
    mutate(DATA = as_datetime(FullDate)) %>%
    as_tsibble(index = DATA) %>%
    fill_gaps() %>%
    gg_season(VALORE, period = s) +
    theme(legend.position = "none") +
    scale_y_continuous(n.breaks = 5) 
#    labs(title = paste("Analyse every", s))
}

define_regressors = function(df){
  df$Dic24 = ifelse(df$DATA == '2018-12-24' | df$DATA == '2019-12-24', 1, 0)
  df$Dic25 = ifelse(df$DATA == '2018-12-25' | df$DATA == '2019-12-25', 1, 0)
  df$Dic26 = ifelse(df$DATA == '2018-12-26' | df$DATA == '2019-12-26', 1, 0)
  # Fine anno.
  df$Gen1 = ifelse(df$DATA == '2019-01-01' | df$DATA == '2020-01-01', 1, 0)
  # Epifania
  df$Gen6 = ifelse(df$DATA == '2019-01-06' | df$DATA == '2020-01-06', 1, 0)
  # Pasqua (varia negli anni).
  df$Pasqua = ifelse(df$DATA == '2019-04-21' | df$DATA == '2020-04-12',1,0)
  df$Pasquetta = ifelse(df$DATA == '2019-04-22' | df$DATA == '2020-04-13',1,0)
  # Ferragosto.
  df$Ago15 = ifelse(df$DATA == '2019-08-15' | df$DATA == '2020-08-15', 1, 0) 
  #Immacolata.
  df$Dic8 = ifelse(df$DATA == '2018-12-08' | df$DATA == '2019-12-08', 1, 0)
  # Tutti i santi.
  df$Nov1 = ifelse(df$DATA == '2018-11-01' | df$DATA == '2019-11-01', 1, 0)
  # Festa della repubblica.
  df$Giu2 = ifelse(df$DATA == '2019-06-02' | df$DATA == '2020-06-02', 1, 0)
  # Festa dei lavoratori.
  df$Mag1 = ifelse(df$DATA == '2019-05-01' | df$DATA == '2020-05-01', 1, 0)
  # Festa della liberazione.
  df$Apr25 = ifelse(df$DATA == '2019-04-25' | df$DATA == '2020-04-25', 1, 0)
  # Prima chiusura.
  df$Covid = ifelse(df$DATA >= '2020-03-09', 1, 0)
  # Introduco il nome del giorno e creo delle variabili dummy per (sperimentare) la stagionalità settimanale.
  df$WDAY = lubridate::wday(df$DATA, label=TRUE)
  df = dummy_cols(df, select_columns = c("WDAY"))
  return(df)
}

split_train_val = function(data, tail_train, head_test){
  train = window(data, end = tail_train)
  test = window(data, start = head_test)
  # train %>% length(.) + test %>% length(.) == nrow(df)
  # Dummy settimanali.
  week_feature = c("WDAY_dom", "WDAY_lun", "WDAY_mar", "WDAY_mer", "WDAY_gio", "WDAY_ven", "WDAY_sab")
  train_w_dummy = df[1:nrow(train), week_feature]
  test_w_dummy = df[(nrow(train)+1):nrow(df), week_feature]
  
  # Festività.
  feste_feature = c("Dic24", "Dic25", "Dic26", "Gen1", "Gen6",
                    "Pasqua", "Pasquetta", "Ago15", "Dic8",
                    "Nov1", "Giu2", "Mag1", "Apr25", "Covid")
  train_feste_dummy = df[1:nrow(train), feste_feature]
  test_feste_dummy = df[(nrow(train)+1):nrow(df), feste_feature]
  
  # Check delle dimensioni. 
  return(list(train, test,
              train_w_dummy, test_w_dummy,
              train_feste_dummy, test_feste_dummy))
}

evaluation = function(test, f, name){
  appo = c()
  for (i in 1:nrow(test)) {
    appo = c(appo, as.numeric(test[i]) - as.numeric(f[i]))
  }
  me = mean(appo)
  mse = mean((appo)^2)
  rmse = sqrt(mse)  
  mae = mean(abs(appo))
  mape = (100/length(test))*sum(abs((appo)/test))
  
  evaluation = data.frame(ME = me, 
                          MSE = mse, 
                          RMSE = rmse, 
                          MAE = mae, 
                          MAPE = mape)
  evaluation = evaluation %>% `row.names<-`(c(name))
  
  return(evaluation)
}

check_arima = function(mod, test, test_reg = NULL){
  # Input:
  # Arima function & test dataset.
  # Output: 
  # grafici e caratteristiche del modello.
  print(mod %>% summary(.))
  
  
  # Analizzo le previsioni.
  cat("\n\n ***** PREVISIONI DI TUTTO IL TEST SET ***** \n")
  wind = length(test)
  f = forecast(mod, h=wind, xreg = test_reg)
  title = paste('Forecast', wind, 'hours')
  plot(f, include = wind, main = title)
  plot(f, include = wind)
  plot(f, main = title)
  
  #cat("\n\n ***** PREVISIONI DI 15 GIORNI AVANTI *****\n")
  #h_shorter = 24*10
  #f_3 = forecast(mod, h=h_shorter, xreg = test_reg)
  # title = paste('Forecast', h_shorter, 'hours')
  #plot(f_3, include = h_shorter, main = title)
  
  cat("\n\n ***** CALCOLO MAE ***** \n")
  f = data.frame(f)[,1]
  mae_test = mean(abs(as.numeric(test) - f))
  cat('MAE for test set:', mae_test)
  
  cat("\n ***** CONFRONTO TEST SET E DATI ORIGINALI ***** \n")
  plot(f, type = 'l', col = 'red', ylim = c(min(data.frame(test)[,1]), max(data.frame(test)[,1])), main = 'Overlap between validation set and prediction')
  lines(data.frame(test)[,1], type = 'l', col = 'black')
  
  cat("\n ***** CHECK RESIDUAL ***** \n ")
  mod %>% checkresiduals(.)
  ggAcf(mod$residuals, lag.max = 24*30) %>% plot(.)
  ggPacf(mod$residuals, lag.max = 24*30) %>% plot(.)
  
  return(list(accuracy(mod)[3], mae_test))
}


arima_grid_search = function(p_min = 0, d_min = 0, q_min = 0, 
                             P_min = 0, D_min = 0, Q_min = 0, 
                             p_max, d_max, q_max, 
                             P_max, D_max, Q_max, 
                             period, 
                             xreg_train, xreg_test, 
                             train, test){
  
  
  p = p_min:p_max
  d = d_min:d_max
  q = q_min:q_max
  
  P = P_min:P_max
  D = D_min:D_max
  Q = Q_min:Q_max
  
  lambda = 0
  constant = c(FALSE, TRUE)
  
  best_mae = Inf
  time = c()
  
  parameters = expand.grid(p = p, d = d, q = q, P = P, D = D, Q = Q, lambda = lambda, constant = constant)
  cat("Numero di modelli da stimare: ", parameters %>% nrow(.), "\n\n")
  parameters = data.frame(parameters)
  
  set_score = data.frame()
  
  pb <- txtProgressBar(0, nrow(parameters), style = 3)
  for (i in 1:nrow(parameters)){
    elem = parameters[i,]
    p = as.numeric(elem['p'])
    d = as.numeric(elem['d'])
    q = as.numeric(elem['q'])
    
    P = as.numeric(elem['P'])
    D = as.numeric(elem['D'])
    Q = as.numeric(elem['Q'])
    
    lambda = as.numeric(elem['lambda'])
    constant = as.numeric(elem['constant'])
    
    tryCatch({
      start.time <- Sys.time()
      mod = Arima(train, 
                  c(p, d, q), 
                  list(order = c(P,D,Q), 
                       period = period),
                  include.constant = constant,
                  lambda = lambda, 
                  xreg = xreg_train
      )
      end.time <- Sys.time()
      time.taken <- end.time - start.time
      time = c(time, time.taken)
      
      # forecast and find the best one.
      h = test %>% nrow()
      f = forecast(mod, h = h, x = xreg_test)
      
      # minimize MAE of the test set.
      mae = mean(abs(test - as.numeric(f$mean)))
      if (mae < best_mae) {
        best_model = mod
        best_mae = mae
        best_f = f
        best_param = elem 
      }
      
      cat(c("\n\n Training MAE: ", accuracy(mod)[3], 
            "Test MAE: ", mae, 
            " p: ", p, 
            " d: ", d, 
            " q: ", q, 
            " P: ", P, 
            " D: ", D, 
            " Q: ", Q, 
            " constant: ", constant,
            " lambda: ", lambda,
            "Execution time: ", time.taken, "\n")
      )
      setTxtProgressBar(pb, i)
    }, error=function(e){cat("\n ERROR : There is some problem. Skip this parameter set. \n")})
    
    if(i == nrow(parameters)){
      cat(c('\n\n', "****  BEST MODEL ****", '\n'))
      cat(c("Training MAE ", accuracy(best_model)[3], 
            "Test MAE ", best_mae, 
            " p: ", as.numeric(best_param['p']), 
            " d: ", as.numeric(best_param['d']), 
            " q: ", as.numeric(best_param['q']), 
            " P: ", as.numeric(best_param['P']), 
            " D: ", as.numeric(best_param['D']), 
            " Q: ", as.numeric(best_param['Q']), 
            " lambda: ", as.numeric(best_param['lambda']), 
            " constant: ", as.numeric(best_param['constant']), 
            "\n"))
    }
  }
  return(list(best_model, best_f, time))
}


reg_one_s = function(train, test, n, s) {
  # Input:
  #   train: train dataset
  #   test: test dataset
  #   n: numero di sinusoidi
  #   s: stagionalità
  # Output:
  #   Regressori che individuano quella stagionalità per il train e test.
  
  tempo = 1:nrow(train) # passi della serie storica.
  n = n
  s = s
  vj = 1:n
  freq = outer(tempo, vj)*2*pi/(s) # prodotto matriciale
  # per il train.
  X = cbind(cos(freq), sin(freq))
  colnames(X) = c(paste0("cos", vj), paste0("sin", vj))
  # per il test.
  wind = length(test)
  freq1 = outer((length(train)+1):(length(train)+ wind), 1:n)*2*pi/(s)
  X1 = cbind(cos(freq1), sin(freq1))
  colnames(X1) = c(paste0("cos", vj), paste0("sin", vj))
  return(list(X, X1))
}

standard_format = function(x){
  elem = format(strptime(x[2],"%H:%M:%S"),'%H') %>% as.numeric()
  # change format
  if (elem == 0){
    elem = 24 
  }
  print(elem)
}