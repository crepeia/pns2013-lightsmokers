# =============================
# ANALYSIS
# =============================
# Some notes - Weight - V00291

# Tip - The data is stored as data.table, which is way faster than data.frame. If you do not know how to use data.table, check this out: https://s3.amazonaws.com/assets.datacamp.com/img/blog/data+table+cheat+sheet.pdf

# Load survey and data.table packages
library(survey)
library(data.table)

pnsDT <- readRDS("data/pns.rds")

# Create data.table tabaco without missing data
#tabaco <- pnsDT[ which(pnsDT$P050 != NA)
    
tabaco <- subset(pnsDT, pnsDT$P050 != " ")

#criando subset fumantes leves, fumantes pesados e n?o fumantes
tabaco$status[tabaco$P052 == c("3") & tabaco$P050== c("3")] <- 0
tabaco$status[tabaco$P05401 == c("1") & tabaco$P05402 < 9] <- 1
tabaco$status[tabaco$P05401 == c("2", "3")] <- 1
tabaco$status[tabaco$P05401 == c("1") & tabaco$P05402 >= 9] <- 2


# Survey format
fumo <- svydesign(
  id = ~1,
  data = tabaco,
  weights = ~V00291
)


# Reproduce the original estimate from IBGE.
# P050 - Atualmente, o(a) Sr(a) fuma algum produto do tabaco?
prop.table(svytable(formula = ~tabaco$P050+tabaco$status, fumo))

#C006 - Sexo
prop.table(svytable(formula = ~tabaco$C006+tabaco$status, fumo))
svychisq(~tabaco$C006+tabaco$status, fumo)
svychisq(~tabaco$C006+tabaco$status, fumo,statistic="adjWald")

#C012 - O informante desta parte foi:
prop.table(svytable(~tabaco$C012+tabaco$status, fumo))

#VDD004-N?vel de instru??o mais elevado alcan?ado (pessoas de 5 anos ou mais de idade)
prop.table(svytable(~tabaco$VDD004+tabaco$status, fumo))

#N001-Em geral, como o(a) Sr(a) avalia a sua sa?de?
prop.table(svytable( ~tabaco$N001+tabaco$status, fumo))


#P005-A Sra est? gr?vida no momento?
prop.table(svytable(~tabaco$P005+tabaco$status, fumo))

#P028-Quantos dias por semana o(a) Sr(a) costuma tomar alguma bebida alco?lica? 
prop.table(svytable(~tabaco$P028+tabaco$status, fumo))

#P029-Em geral, no dia que o(a) Sr(a) bebe, quantas doses de bebida alco?lica o(a) Sr(a) consome?
prop.table(svytable(~tabaco$P029+tabaco$status, fumo))

#P055-Quanto tempo depois de acordar o(a) Sr(a) normalmente fuma pela primeira vez?
prop.table(svytable(~tabaco$P055+tabaco$status, fumo))


#P060-Durante os ?ltimos 12 meses, o(a) Sr(a) tentou parar de fumar?
prop.table(svytable(~tabaco$P060+tabaco$status, fumo))


#P061-Quando o(a) Sr(a) tentou parar de fumar, procurou tratamento com profissional de sa?de?
prop.table(svytable(~tabaco$P061+tabaco$status, fumo))


#P072-Nos ?ltimos 30 dias, as advert?ncias nos ma?os de cigarro levaram o(a) Sr(a) a pensar em parar de fumar?
prop.table(svytable(~tabaco$P072+tabaco$status, fumo))


#Q002-Algum m?dico j? lhe deu o diagn?stico de hipertens?o arterial (press?o alta)?
prop.table(svytable(~tabaco$Q002+tabaco$status, fumo))

#Q01805-Em algum dos atendimentos para hipertens?o, algum m?dico ou outro profissional de sa?de lhe recomendou n?o fumar?
prop.table(svytable(~tabaco$Q01805+tabaco$status, fumo))

#Q030-Algum m?dico j? lhe deu o diagn?stico de diabetes?
prop.table(svytable(~tabaco$Q030+tabaco$status, fumo))


#Q04604-Em algum dos atendimentos para diabetes, algum m?dico ou outro profissional de sa?de lhe recomendou n?o fumar?
prop.table(svytable(~tabaco$Q04604+tabaco$status, fumo))


#Q060-Algum m?dico j? lhe deu o diagn?stico de colesterol alto?
prop.table(svytable(~tabaco$Q060+tabaco$status, fumo))


#Q06205-Algum m?dico ou outro profissional de sa?de lhe deu alguma recomenda??o para n?o fumar por causa do colesterol alto?
prop.table(svytable(~tabaco$Q06205+tabaco$status, fumo))


#Q063-Algum m?dico j? lhe deu o diagn?stico de uma doen?a do cora??o tais como infarto, angina, insufici?ncia cardiaca ou outra?
prop.table(svytable(~tabaco$Q063+tabaco$status, fumo))

#Q068-Algum m?dico j? lhe deu o diagn?stico de AVC (Acidente Vascular cerebral) ou derrame?
prop.table(svytable(~tabaco$Q068+tabaco$status, fumo))


#Q074-Algum m?dico j? lhe deu o diagn?stico de asma (ou bronquite asm?tica)?
prop.table(svytable(~tabaco$Q074+tabaco$status, fumo))


#Q079-Algum m?dico j? lhe deu o diagn?stico de artrite ou reumatismo?
prop.table(svytable(~tabaco$Q079+tabaco$status, fumo))


#Q084-O(a) Sr(a) tem algum problema cr?nico de coluna, como dor cr?nica nas costas ou no pesco?o, lombalgia, dor ci?tica, problemas nas v?rtebras ou disco?
prop.table(svytable(~tabaco$Q084+tabaco$status, fumo))

#Q088-Algum m?dico j? lhe deu o diagn?stico de DORT (dist?rbio osteomuscular relacionado ao trabalho)?
prop.table(svytable(~tabaco$Q088+tabaco$status, fumo))

#Q092-Algum m?dico ou profissional de sa?de mental (como psiquiatra ou psic?logo) j? lhe deu o diagn?stico de depress?o?
prop.table(svytable(~tabaco$Q092+tabaco$status, fumo))


#Q110-Algum m?dico ou profissional de sa?de mental (como psiquiatra ou psic?logo) j? lhe deu o diagn?stico de outra doen?a mental, como esquizofrenia, transtorno bipolar, psicose ou TOC (Transtorno obsessivo compulsivo)? PS: O question?rio possui uest?es espec?ficas para cada uma das doen?as.
prop.table(svytable(~tabaco$Q110+tabaco$status, fumo))

#Q116-Algum m?dico j? lhe deu o diagn?stico de alguma doen?a no pulm?o ou DPOC (Doen?a Pulmonar Obstrutiva Cr?nica), tais como enfisema pulmonar, bronquite cr?nica ou outro?
prop.table(svytable(~tabaco$Q116+tabaco$status, fumo))

#Q120-Algum m?dico j? lhe deu algum diagn?stico de c?ncer?
prop.table(svytable(~tabaco$Q120+tabaco$status, fumo))

#Q121-No primeiro diagn?stico de c?ncer, que tipo de c?ncer o(a) sr(a) tem ou teve?
prop.table(svytable(~tabaco$Q121+tabaco$status, fumo))


#Q124-Algum m?dico j? lhe deu o diagn?stico de insufici?ncia renal cr?nica?
prop.table(svytable(~tabaco$Q124+tabaco$status, fumo))


#Q128-Algum m?dico j? lhe deu o diagn?stico de outra doen?a cr?nica, f?sica ou mental, ou doen?a de longa dura??o (de mais de 6 meses de dura??o)?
prop.table(svytable(~tabaco$Q128+tabaco$status, fumo))


