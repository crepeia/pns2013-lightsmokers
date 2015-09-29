# =============================
# ANALYSIS
# =============================
# Some notes - Weight - V00291

# Tip - The data is stored as data.table, which is way faster than data.frame. If you do not know how to use data.table, check this out: https://s3.amazonaws.com/assets.datacamp.com/img/blog/data+table+cheat+sheet.pdf

setwd("C:/Users/guilh/pns2013-lightsmokers")

# Load survey and data.table packages
library(survey)
library(data.table)

pnsDT <- readRDS("data/pns.rds")

# Create data.table tabaco without missing data
#tabaco <- pnsDT[ which(pnsDT$P050 != NA)
    
tabaco <- subset(pnsDT, pnsDT$P050 != " ")

tabaco$idade[18 <= tabaco$C008 & tabaco$C008 < 29]<-0
tabaco$idade[29 <= tabaco$C008 & tabaco$C008 < 59]<-1
tabaco$idade[59 <= tabaco$C008 & tabaco$C008 < 64]<-2
tabaco$idade[64 <= tabaco$C008 & tabaco$C008 < 74]<-3
tabaco$idade[tabaco$C008 >= 74]<-4




tabaco$status[tabaco$P052 == c("3")] <- 0
tabaco$status[tabaco$P05401 == c("1") & tabaco$P05402 <= 9] <- 1
tabaco$status[tabaco$P05401 == c("2", "3")] <- 1
tabaco$status[tabaco$P05401 == c("1") & tabaco$P05402 > 9] <- 2

# Survey format
fumo <- svydesign(
  id = ~1,
  data = tabaco,
  weights = ~V00291
)

# prevalencia de usuario de algum produto de tabaco
fumante_atual<- subset(pnsDT,pnsDT$P050 == c("1","2","3"))

prop.table(svytable(formula = ~fumante_atual$P050, fumo))


# Reproduce the original estimate from IBGE.

#C006 - Sexo
prop.table(svytable(formula = ~tabaco$C006+tabaco$status+tabaco$C012, fumo))

# P050 - Atualmente, o(a) Sr(a) fuma algum produto do tabaco?
prop.table(svytable(formula = ~tabaco$P050+tabaco$status, fumo ))


#svychisq(~tabaco$C006+tabaco$status, fumo)
#svychisq(~tabaco$C006+tabaco$status, fumo,statistic="adjWald")

#C012 - O informante desta parte foi:
prop.table(svytable(~tabaco$C012+tabaco$status, fumo))

#VDD004-N?vel de instrução mais elevado alcançado (pessoas de 5 anos ou mais de idade)
prop.table(svytable(~tabaco$VDD004+tabaco$status, fumo))

#N001-Em geral, como o(a) Sr(a) avalia a sua saúde?
prop.table(svytable( ~tabaco$N001+tabaco$status, fumo))


#P005-A Sra est? grávida no momento?
prop.table(svytable(~tabaco$P005+tabaco$status, fumo))

#P028-Quantos dias por semana o(a) Sr(a) costuma tomar alguma bebida alcoólica? 
prop.table(svytable(~tabaco$P028+tabaco$status, fumo))

#P029-Em geral, no dia que o(a) Sr(a) bebe, quantas doses de bebida alcoólica o(a) Sr(a) consome?
prop.table(svytable(~tabaco$P029+tabaco$status, fumo))

#P055-Quanto tempo depois de acordar o(a) Sr(a) normalmente fuma pela primeira vez?
prop.table(svytable(~tabaco$P055+tabaco$status, fumo))


#P060-Durante os últimos 12 meses, o(a) Sr(a) tentou parar de fumar?
prop.table(svytable(~tabaco$P060+tabaco$status, fumo))


#P061-Quando o(a) Sr(a) tentou parar de fumar, procurou tratamento com profissional de saúde?
prop.table(svytable(~tabaco$P061+tabaco$status, fumo))


#P072-Nos últimos 30 dias, as adverténcias nos ma?os de cigarro levaram o(a) Sr(a) a pensar em parar de fumar?
prop.table(svytable(~tabaco$P072+tabaco$status, fumo))


#Q002-Algum médico já lhe deu o diagnóstico de hipertensão arterial (press?o alta)?
prop.table(svytable(~tabaco$Q002+tabaco$status, fumo))

#Q01805-Em algum dos atendimentos para hipertensão, algum médico ou outro profissional de saúde lhe recomendou n?o fumar?
prop.table(svytable(~tabaco$Q01805+tabaco$status, fumo))

#Q030-Algum m?dico j? lhe deu o diagn?stico de diabetes?
prop.table(svytable(~tabaco$Q030+tabaco$status, fumo))


#Q04604-Em algum dos atendimentos para diabetes, algum m?dico ou outro profissional de sa?de lhe recomendou n?o fumar?
prop.table(svytable(~tabaco$Q04604+tabaco$status, fumo))


#Q060-Algum médico já lhe deu o diagn?stico de colesterol alto?
prop.table(svytable(~tabaco$Q060+tabaco$status, fumo))


#Q06205-Algum médico ou outro profissional de saúde lhe deu alguma recomendação para não fumar por causa do colesterol alto?
prop.table(svytable(~tabaco$Q06205+tabaco$status, fumo))


#Q063-Algum médico já lhe deu o diagnóstico de uma doença do coração tais como infarto, angina, insuficiência cardiaca ou outra?
prop.table(svytable(~tabaco$Q063+tabaco$status, fumo))

#Q068-Algum médico já lhe deu o diagnóstico de AVC (Acidente Vascular cerebral) ou derrame?
prop.table(svytable(~tabaco$Q068+tabaco$status, fumo))


#Q074-Algum médico já lhe deu o diagnóstico de asma (ou bronquite asm?tica)?
prop.table(svytable(~tabaco$Q074+tabaco$status, fumo))


#Q079-Algum médico já lhe deu o diagnóstico de artrite ou reumatismo?
prop.table(svytable(~tabaco$Q079+tabaco$status, fumo))


#Q084-O(a) Sr(a) tem algum problema crônico de coluna, como dor cr?nica nas costas ou no pescoço, lombalgia, dor ciática, problemas nas v?rtebras ou disco?
prop.table(svytable(~tabaco$Q084+tabaco$status, fumo))

#Q088-Algum médico já lhe deu o diagnóstico de DORT (distúrbio osteomuscular relacionado ao trabalho)?
prop.table(svytable(~tabaco$Q088+tabaco$status, fumo))

#Q092-Algum médico ou profissional de saúde mental (como psiquiatra ou psicólogo) já lhe deu o diagnóstico de depressão?
prop.table(svytable(~tabaco$Q092+tabaco$status, fumo))


#Q110-Algum m?dico ou profissional de sa?de mental (como psiquiatra ou psic?logo) já lhe deu o diagn?stico de outra doen?a mental, como esquizofrenia, transtorno bipolar, psicose ou TOC (Transtorno obsessivo compulsivo)? PS: O question?rio possui uest?es específicas para cada uma das doen?as.
prop.table(svytable(~tabaco$Q110+tabaco$status, fumo))

#Q116-Algum médico já lhe deu o diagnóstico de alguma doença no pulm?ão ou DPOC (Doen?a Pulmonar Obstrutiva Cr?nica), tais como enfisema pulmonar, bronquite crônica ou outro?
prop.table(svytable(~tabaco$Q116+tabaco$status, fumo))

#Q120-Algum médico já lhe deu algum diagnóstico de c?ncer?
prop.table(svytable(~tabaco$Q120+tabaco$status, fumo))

#Q121-No primeiro diagn?stico de câncer, que tipo de c?ncer o(a) sr(a) tem ou teve?
prop.table(svytable(~tabaco$Q121+tabaco$status, fumo))


#Q124-Algum m?dico j? lhe deu o diagn?stico de insuficiência renal crônica?
prop.table(svytable(~tabaco$Q124+tabaco$status, fumo))


#Q128-Algum médico já lhe deu o diagnóstico de outra doen?a cr?nica, f?sica ou mental, ou doen?a de longa dura??o (de mais de 6 meses de dura??o)?
prop.table(svytable(~tabaco$Q128+tabaco$status, fumo))


