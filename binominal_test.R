#サンプルサイズ
size <- 9
#四捨五入を実行する小数点以下の桁数
my_digits <- 5
#理論的な確率
my_prob = 0.5

#四捨五入関数
round2 <- function (n, digits) 
{
  return(floor(n * 10^digits + 0.5)/10^digits)
}

#生起確率関数
calc_mr <- function(s, digits, prob) {
  x <- 0
  loop_end <- s + 1
  #ベクトルを作成
  vec <- c(1:s)
  
  for (i in 1:loop_end) {
  
    #組み合わせの計算
    c <- factorial(s)
    dif <- s - x
    m <- factorial(x) * factorial(dif)
    pattern <- c / m
  
    #各試行の確率計算
    agr_trial <- prob ^ x
    disagr_trial <- (1 - prob) ^ (s - x)
  
    #生起確率の計算
    mr <- pattern * agr_trial * disagr_trial
    
    #ベクトルに収納
    vec <- replace(y, c(i), c(mr))
    
    x <- x + 1
  }
  
  #指定された小数点以下の桁数でベクトルの各要素を四捨五入
  rounded_vec <- round2(vec, digits)
  return(rounded_vec)
}

#二項検定(仮)
binominal_test <- function(vec, freq, siglevel) {
  #二項分布における度数の外側上下の確率
  s <- length(vec)
  dif <- s - freq
  head_vec <- head(vec, dif)
  tail_vec <- tail(vec, dif)
  target_elements <- append(head_vec, tail_vec)
  target_prob <- sum(target_elements)
  
  #二項分布における度数を基にした外側上下の確率を出力
  str_target_prob <-  as.character(target_prob)
  target_prob_message <- paste("外側上下の確率：",  str_target_prob)
  print(target_prob_message)
  
  if (target_prob < siglevel) {
    message <- "帰無仮説を棄却します (p<.05)"
    print(message)
  } 
  if (target_prob >= siglevel) {
    message <- "帰無仮説を棄却できません (p>=.05)"
    print(message)
  }
}

#生起確率を算出
mr_vec <- calc_mr(s = size, digits = my_digits, prob = my_prob)
#生起確率ベクトルを出力
mr_vec
#棒グラフにプロット
barplot(mr_vec, names.arg = c(0:9), main="二項分布", xlab="賛成数", ylab="生起確率", ylim=c(0, 0.3))
#二項検定（仮）を実施
binominal_test(mr_vec, 7, 0.05)
