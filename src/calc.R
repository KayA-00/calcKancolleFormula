
# 複数行コメントアウト : ctrl+shift+c
# 2018/03現在、実測値に最も近いとされる推定式は以下の通りである([]内は端数切捨て)。
# 夜戦CI発動率(%)=(CI項/種別係数)*100
# CI項(運50未満)=[15+運+0.75*√(Lv)]+配置補正+損傷補正+装備補正
# CI項(運50以上)=[65+√(運-50)+0.8*√(Lv)]+配置補正+損傷補正+装備補正
# 配置補正 : 旗艦：+15
# 装備補正 : 探照灯+7, 照明弾+4, 見張り員+5
# 損傷補正 : 上振れ想定なので中破は考慮しない．

torpedo <- function(luck, level, flag.ship, search.light, light.bullet, watchman){
  type.coefficient <- 122
  ci.element <- array(c("旗艦", 15, "探照灯", 7, "照明弾", 4, "見張り員", 5), dim = c(2, 4))
  if (luck < 50){
    ci <- 15 + luck + 0.75 * sqrt(level)
  }
  else {
    ci <- 65 + sqrt(luck-50) + 0.8 * sqrt(level)
  }
  ci <- floor(ci)
  judge <- c(flag.ship, search.light, light.bullet, watchman)
  if (length(judge[judge < 0]) + length(judge[judge > 1]) != 0){
    return(print("入力された値が適切ではありません\n"))
  }
  if (length(judge[judge == 0]) == 4){
    cat("デフォルト : ", ci/type.coefficient*100, "%\n")
  }
  else {
    ci.element <- ci.element[,which(judge == 1)]
    ci <- ci + sum(as.integer(ci.element[2, ]))
    p <- paste(ci.element[1, ], collapse = " + ")
    cat(p, " : ", ci/type.coefficient*100, "%\n")
  }
}

yatei <- function(level, slot.num){
  fly.rate <- floor(sqrt(3 * level)) * 4 / 100
  fly <- 1 - (1 - fly.rate) ^ slot.num
  print(fly * 100)
}

# 観測種別定数
# 主主カットイン = 150, coefficient
# 連撃 = 130

dantyaku <- function(luck, fleet.search, attack.search, flag.ship, seikuu){
  ## fleet.search : 艦隊索敵補正
  ## attack.search : 攻撃艦の装備索敵値
  if (seikuu == 1){ ## 確保
    item <- floor(floor(sqrt(luck) + 10) + 0.7 * (fleet.search + 1.6 * attack.search) + 10)
  }
  else if (seikuu == 0){ ## 優勢
    item <- floor(floor(sqrt(luck) + 10) + 0.6 * (fleet.search + 1.2 * attack.search))
  }
  else {
    return(print("制空の値が不適切です．"))
  }
  if (flag.ship == 1){
    item <- item + 15
  }
  ## 弾着率
  coefficient <- c(150, 130)
  dantyaku.rate <- ceiling(item) / coefficient[1]
  twin.attack <- ceiling(item) / coefficient[2]
  cat("弾着率 : ", dantyaku.rate, "\n")
  cat("連撃発生率 : ", twin.attack, "\n")
  cat("両方不発率 : ", (1 - dantyaku.rate) * (1 - twin.attack), "\n")
}

## 艦隊索敵値補正 及び A を求める関数 ##
calc.fleet.search <- function(search.value){
  if (length(search.value) != 6){
    return(print("6隻分の索敵値を入力してください"))
  }
  A <- sum(search.value)
  repeat{
    s <- readline("索敵値,スロット数を入力してください : ")
    if (s == "0") return( floor(sqrt(A) + 0.1 * A) )
    plane <- as.integer(unlist(strsplit(s, ","))) ## 水偵を飛行機に見立てて plane にしました．
    A <- A + plane[1] * floor(sqrt(plane[2]))
  }
}

daylight.ci <- function(){
  cfs <- calc.fleet.search(c(50, 50, 50, 50, 50, 50))
  dantyaku(18, cfs, 8, 1, 1)
}

## 戦果計算 ##
special.war.achivements <- function(x, y){
  # 以下ベクトルの要素の並び順はぜかましに準拠しています．
  EO <- sum(75, 75, 100, 150, 180, 200, 250)
  quartely <- sum(350, 400, 300, 200, 80, 330, 390)
  cat("EO : ", EO, " : Quartely : ", quartely, "\n")
  cat(sum(EO*x, quartely*y), "\n")
}

### 以下，関数実行スペース ###
daylight.ci()

torpedo(60, 175, 1, 1, 1, 1)
yatei(95, 2)
special.war.achivements(1, 1)
