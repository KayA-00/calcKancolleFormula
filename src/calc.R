getwd()
setwd("C:/Users/yamak/Documents/K/")


# �����s�R�����g�A�E�g : ctrl+shift+c
# 2018/03���݁A�����l�ɍł��߂��Ƃ���鐄�莮�͈ȉ��̒ʂ�ł���([]���͒[���؎̂�)�B
# ���CI������(%)=(CI��/��ʌW��)*100
# CI��(�^50����)=[15+�^+0.75*��(Lv)]+�z�u�␳+�����␳+�����␳
# CI��(�^50�ȏ�)=[65+��(�^-50)+0.8*��(Lv)]+�z�u�␳+�����␳+�����␳
# �z�u�␳ : ���́F+15
# �����␳ : �T�Ɠ�+7, �Ɩ��e+4, �������+5
# �����␳ : ��U��z��Ȃ̂Œ��j�͍l�����Ȃ��D

torpedo <- function(luck, level, flag.ship, search.light, light.bullet, watchman){
  type.coefficient <- 122
  ci.element <- array(c("����", 15, "�T�Ɠ�", 7, "�Ɩ��e", 4, "�������", 5), dim = c(2, 4))
  if (luck < 50){
    ci <- 15 + luck + 0.75 * sqrt(level)
  }
  else {
    ci <- 65 + sqrt(luck-50) + 0.8 * sqrt(level)
  }
  ci <- floor(ci)
  judge <- c(flag.ship, search.light, light.bullet, watchman)
  if (length(judge[judge < 0]) + length(judge[judge > 1]) != 0){
    return(print("���͂��ꂽ�l���K�؂ł͂���܂���\n"))
  }
  if (length(judge[judge == 0]) == 4){
    cat("�f�t�H���g : ", ci/type.coefficient*100, "%\n")
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

# �ϑ���ʒ萔
# ���J�b�g�C�� = 150, coefficient
# �A�� = 130

dantyaku <- function(luck, fleet.search, attack.search, flag.ship, seikuu){
  ## fleet.search : �͑����G�␳
  ## attack.search : �U���͂̑������G�l
  if (seikuu == 1){ ## �m��
    item <- floor(floor(sqrt(luck) + 10) + 0.7 * (fleet.search + 1.6 * attack.search) + 10)
  }
  else if (seikuu == 0){ ## �D��
    item <- floor(floor(sqrt(luck) + 10) + 0.6 * (fleet.search + 1.2 * attack.search))
  }
  else {
    return(print("����̒l���s�K�؂ł��D"))
  }
  if (flag.ship == 1){
    item <- item + 15
  }
  ## �e����
  coefficient <- c(150, 130)
  dantyaku.rate <- ceiling(item) / coefficient[1]
  twin.attack <- ceiling(item) / coefficient[2]
  cat("�e���� : ", dantyaku.rate, "\n")
  cat("�A�������� : ", twin.attack, "\n")
  cat("�����s���� : ", (1 - dantyaku.rate) * (1 - twin.attack), "\n")
}

## �͑����G�l�␳ �y�� A �����߂�֐� ##
calc.fleet.search <- function(search.value){
  if (length(search.value) != 6){
    return(print("6�Ǖ��̍��G�l����͂��Ă�������"))
  }
  A <- sum(search.value)
  repeat{
    s <- readline("���G�l,�X���b�g������͂��Ă������� : ")
    if (s == "0") return( floor(sqrt(A) + 0.1 * A) )
    plane <- as.integer(unlist(strsplit(s, ","))) ## ������s�@�Ɍ����Ă� plane �ɂ��܂����D
    A <- A + plane[1] * floor(sqrt(plane[2]))
  }
}

daylight.ci <- function(){
  cfs <- calc.fleet.search(c(50, 50, 50, 50, 50, 50))
  dantyaku(18, cfs, 8, 1, 1)
}

## ��ʌv�Z ##
special.war.achivements <- function(x, y){
  # �ȉ��x�N�g���̗v�f�̕��я��͂����܂��ɏ������Ă��܂��D
  EO <- sum(75, 75, 100, 150, 180, 200, 250)
  quartely <- sum(350, 400, 300, 200, 80, 330, 390)
  cat("EO : ", EO, " : Quartely : ", quartely, "\n")
  cat(sum(EO*x, quartely*y), "\n")
}

### �ȉ��C�֐����s�X�y�[�X ###
daylight.ci()

torpedo(60, 175, 1, 1, 1, 1)
yatei(95, 2)
special.war.achivements(1, 1)