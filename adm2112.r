rm(list = ls())

# 219 aleatoire simple à partir d'une colonne
simple <- function(array){
  len <- length(array)
  degre.lib <- len - 1
  moy <- mean(array)
  sd <- sd(array)
  print(paste("Variance ",sd))
  seuil.conf <- 0.95
  seuil.conf.interv <- 1.96
  marge.err <- seuil.conf.interv*(sd/len)^(1/2)
  print(paste("Marge d'erreur ",marge.err))
  interv <- paste("La moyenne de la population se situe ",moy-marge.err," et ", moy+marge.err)
  print(interv)
}
simple2 <- function(len,moy,sd){
  degre.lib <- len - 1
  seuil.conf <- 0.95
  seuil.conf.interv <- 1.96
  marge.err <- seuil.conf.interv*(sd/len)^(1/2)
  print(paste("Marge d'erreur ",marge.err))
  interv <- paste("La moyenne de la population se situe ",moy-marge.err," et ", moy+marge.err)
  print(interv)
}
# 222 aleatoire stratifié
stratifie <- function(taille,strate.N,strate.moy,strate.sd){
  l.strate <- length(strate.N)
  strate.n <- strate.N/taille
  ech.moy <- sum(strate.N*strate.moy)/sum(strate.N)
  print(ech.moy)
  ech.sd <- sum(strate.N^2*strate.sd/strate.n*(strate.N-strate.n)/strate.N)/sum(strate.N)^2
  print(paste("Variance ",ech.sd))
  seuil.conf <- 0.95
  seuil.conf.interv <- 1.96
  marge.err <- seuil.conf.interv*ech.sd^(1/2)
  print(paste("Marge d'erreur ",marge.err))
  interv <- paste("La moyenne de la population se situe ",ech.moy-marge.err," et ", ech.moy+marge.err)
  print(interv)
}
# 224 stratifié proportion
stratifie.prop <- function(taille,strate.N,strate.prop){
  l.strate <- length(strate.N)
  strate.n <- strate.N/taille
  prop <- sum(strate.N*strate.prop)/sum(strate.N)
  ech.sd <- sum(strate.N^2*(strate.prop*(1-strate.prop))/(strate.n-1)*(strate.N-strate.n)/strate.N)/sum(strate.N)^2
  print(paste("Variance ",ech.sd))
  seuil.conf <- 0.95
  seuil.conf.interv <- 1.96
  marge.err <- seuil.conf.interv*ech.sd^(1/2)
  print(paste("Marge d'erreur ",marge.err))
  interv <- paste("L'estimation ponctuelle de la proportion est de ", prop,". La moyenne de la population se situe ",prop-marge.err," et ", prop+marge.err)
  print(interv)
}
# 279
critical.value <- function(l, c){
  ddl <- (l-1)*(c-1)
  confidence_level <- 0.95
  critical_value <- qchisq(confidence_level, ddl)
  print(critical_value)
}
adjectif <- function(x){
  if(x == 0.0){
    return("nulle")
  } else if(x < 0.09){
    return("très faible")
  } else if(x < 0.29){
    return("faible")
  } else if(x < 0.49){
    return("modérée")
  } else if(x < 0.69){
    return("forte")
  } else{
    return("très forte")
  }
}
# 295
critical.value.2 <- function(seuil, df) {
  print(qt(1 - seuil, df))
}
# 297 statistique eta pour donnée brute indépendante, indice de force/relation, aussi v de cramer 284 pour choix de réponse, t, proche de 1, très forte, modérée faible, nulle
statistique.eta <- function(t,regroup1,regroup2){
  x = sqrt((t^2/(t^2+regroup1+regroup2-2))) # -2 294
  print(x)
}
# 300 statistique eta pour donnée brute appareillée (effectuée sur les mêmes personnes), indice de force/relation, aussi v de cramer 284 pour choix de réponse, t, proche de 1, très forte, modérée faible, nulle
statistique.eta.2 <- function(t,n){
  x = sqrt((t^2/(t^2+n-1))) # -2 294
  print(x)
}
# 305 statistique f pour comp plusieurs moyennes, indice de force/relation, 0 à l'infini
statistique.f <- function(df.num,df.denom){
  df.denom <- df.denom - df.num
  df.num <- df.num - 1
  alpha <- 0.05
  x <- qf(1 - alpha, df.num, df.denom)
  print(x)
}
# 308 anova, indice de force/relation, aussi v de cramer 284 pour choix de réponse, t, proche de 1, très forte, modérée faible, nulle
anova <- function(intergroup, total){
  x <- sqrt(intergroup/total)
  print(x)
}
# 316 valeur t
t.value <- function(n, pearson){
  ddl <- 2
  n <- n - ddl
  x <- pearson / sqrt((1-(pearson)^2)/n)
  if(x < -1.96){
    print("[INFO] t.value la relation entre les deux variables est négative et significative")
  } else if(x > 1.96){
    print("[INFO] t.value la relation entre les deux variables est positive et significative")
  } else {
    print("[INFO] t.value la relation n'est pas significative")
  }
  print(x)
}

# 219 simple proportion
# prop.moy <- 0.3
# prop.rest <- 1-prop.moy
# len <- 10
# degre.lib <- len - 1
# sd <- 0.3 * 0.7 # 3 consommateur sur 10 affirment gérer leurs dépenses
# print(paste("Variance ",sd))
# seuil.conf <- 0.95
# seuil.conf.interv <- 1.96
# marge.err <- seuil.conf.interv*(prop.moy*prop.rest/degre.lib)^(1/2) #1/2 racine carrée
# print(paste("Marge d'erreur ",marge.err))
# interv <- paste("La proportion de la population se situe ",prop.moy-marge.err," et ", prop.moy+marge.err)
# print(interv)

# 225 redressé ou non
# strate.N <- c(6000,9000)
# l.strate <- length(strate.N)
# strate.n <- c(60,40)
# strate.prop <- c(0.75,0.45) # utlise le téléphone
# prop <- sum(strate.N*strate.prop)/sum(strate.N)
# prop.non.redresse <- sum(strate.n*strate.prop)/sum(strate.n)
# interv <- paste("La proportion redressé est de ", prop,". La population non redressé est de ", prop.non.redresse)
# print(interv)

# 232 estimer ech.n
# marge.err <- 0.03
# pop <- 12000
# prop <- 0.5
# seuil.conf <- 0.95
# seuil.conf.interv <- 1.96
# n <- pop*prop*prop/((pop-1)*marge.err^2/seuil.conf.interv^2+(prop*prop))
# print(n)

# simple(c(170,145,150,185,165))
# simple2(10,167.5,273.61)

# taille <- 100
# strate.N <- c(2500, 6000, 1500)
# strate.moy <- c(145,165,175)
# strate.sd <- c(625,700,725)
# stratifie(taille,strate.N,strate.moy,strate.sd)

# taille <- 100
# strate.N <- c(2500, 6000, 1500)
# strate.prop <- c(0.42,0.22,0.13) # font un budget
# stratifie.prop(taille,strate.N,strate.prop)

# critical.value(4, 6)
# print(adjectif(0.33))
# critical.value.2(0.025,257)
# statistique.eta(6.05,160,99)
# statistique.eta(-4.363, 76, 24)
# statistique.eta(4.426, 76, 24)
# statistique.eta(-3.9,15,15)
# statistique.eta.2(-2.982,259)
# statistique.eta(2.018,44,58)
# statistique.eta(-.541,160,99)


# nb.nominal <- 5
# n <- 259
# statistique.f(nb.nominal, n)
# anova(725.543,13645.861)

# nb.nominal <- 6
# n <- 100
# statistique.f(nb.nominal, n)
# adjectif(anova(116.007,239.960))
# adjectif(anova(90.565,244.240))

t.value(259, -0.207)

# library(memisc)
# library(rcompanion)
# df <- spss.system.file("G:/Mon disque/ADM2112/spiritueux_cleaned.sav")
# df <- Iconv(df,from="ISO-8859-1",to="UTF-8")
# df <- as.data.frame(as.data.set(df))
# matrix <- xtabs(~ o_interet_creme_cafe + n_sexe, data = df)
# rownames(matrix) <- NULL
# colnames(matrix) <- NULL
# print(matrix)
# print(cramerV(matrix))