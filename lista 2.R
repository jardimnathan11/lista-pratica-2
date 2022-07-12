install.packages("ivreg", dependencies = TRUE)
library(ivreg)
library(ggplot2)
library(readxl)
card <- read_excel("C:/Users/Nathan/Downloads/livros economia/estatistica/puc/econometria 1/card.xlsx")
media= c(mean(card$educ),mean(card$lwage), mean(card$exper) )
mediana= c(median(card$educ),median(card$lwage), median(card$exper))
variancia = c(var(card$educ), var(card$lwage), var(card$exper) )
minimo = c(min(card$educ),min(card$lwage), min(card$exper))
maximo = c(max(card$educ) ,max(card$lwage), max(card$exper))
estats = cbind(media, mediana, variancia, minimo, maximo)
rownames(estats) = c('educ' ,'lwage ', 'exper' )
xtable(estats, type = "latex", file = "estats.tex")
hist(card$educ , col='red', # column color
     border="black",
     prob = TRUE,  # show densities instead of frequencies
     xlab= " Amostra 1",
     main = " Educação ",xlim=c(0,20), ylim = c(0, 0.35))
hist(card$lwage , col='red', # column color
     border="black",
     prob = TRUE,  # show densities instead of frequencies
     xlab= " Amostra 1",
     main = " Log de salários",xlim=c(4,9), ylim = c(0, 0.9))

hist(card$exper , col='red', # column color
     border="black",
     prob = TRUE,  # show densities instead of frequencies
     xlab= " Amostra 1",
     main = " Experiência",xlim=c(0,20), ylim = c(0, 0.15))
###
reg1 = lm( card$lwage ~ card$educ)
summary(reg1)
stargazer(reg1)
#############
card$expersq100 = (1/100)*card$expersq
reg2 = lm( card$lwage ~ card$educ + card$exper + card$expersq100 + card$black + card$south +card$smsa )
stargazer(reg2)
reg_66 = lm( card$lwage ~ card$educ + card$exper + card$expersq100 + card$black + card$south
             + card$smsa + card$smsa66 + card$reg661 + card$reg662 + card$reg663 + card$reg664 
             + card$reg665 + card$reg666 + card$reg667 + card$reg668 )
stargazer(reg_66)

reg_parental_educ = lm( card$lwage ~ card$educ + card$exper + card$expersq100 + card$black + card$south
                        + card$smsa + card$fatheduc + card$motheduc + card$smsa66 + card$reg661 + card$reg662
                        + card$reg663 + card$reg664 
                        + card$reg665 + card$reg666 + card$reg667 + card$reg668 )
stargazer(reg_parental_educ)
reg_est = lm( card$lwage ~ card$educ + card$exper + card$expersq100 + card$black + card$south
              + card$smsa + card$fatheduc + card$motheduc + card$smsa66 + card$reg661 + card$reg662
              + card$reg663 + card$reg664 +card$momdad14 + card$sinmom14 
              + card$reg665 + card$reg666 + card$reg667 + card$reg668 )
stargazer(reg_est)
######
reg = lm( card$lwage ~ card$educ + card$iq + card$kww)
summary(reg2)

reg_kww = lm( card$lwage ~ card$educ  + card$kww)
summary(reg_kww)
stargazer(reg_kww)
reg_kww_iq = lm( card$lwage ~ card$educ  + card$kww + card$iq)
summary(reg_kww_iq)
stargazer(reg_kww_iq)
######## 9
reg_1est = lm( card$educ ~ card$nearc4  + card$black + card$south
              + card$smsa + card$fatheduc + card$motheduc + card$smsa66 + card$reg661 + card$reg662
              + card$reg663 + card$reg664 +card$momdad14 + card$sinmom14 
              + card$reg665 + card$reg666 + card$reg667 + card$reg668 )#+ card$exper + card$expersq100
summary(reg_1est)
stargazer(reg_1est)
####### 10
reg_2sls<-ivreg( card$lwage ~  card$kww + card$exper + card$expersq100 + card$black + card$south
            + card$smsa + card$fatheduc + card$motheduc + card$smsa66 + card$reg661 + card$reg662
            + card$reg663 + card$reg664 +card$momdad14 + card$sinmom14 
            + card$reg665 + card$reg666 + card$reg667 + card$reg668|card$educ |card$nearc4 )
summary(reg_2sls)
stargazer(reg_2sls)

