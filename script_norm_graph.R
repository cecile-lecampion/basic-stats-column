### Script pour tester la normalité d'un jeu de données et faire l'analyse statisque 
# en fonction de la distribution des données ####

#### Initialisation des variables ############################################################################################
# Définition du répertoire de travail
setwd("~/PATH/TO/DIRECTORY")                 #mettre le chemin complet du repertoire de travail

# Données
# pas de colonne de n° de ligne. 
# les noms de colonnes doivent commencer par une lettre. Si vous avez des lignées avec des noms numériques type 1.1, nomer la L1.1
DATA <- "your_data_file.txt"

# Pour le box plot
# Chacune de ces variables peut prendre la valeur "" si on ne veut rien afficher
BOXPLOT_Xaxis <- "Titre de l'axe x"
BOXPLOT_Yaxis <- "Titre de l'axe y"
BOXPLOT_TITLE <- "Titre du graphique"

# Pour le Bar plot
# Chacune de ces variables peut prendre la valeur "" si on ne veut rien afficher
BAR_TITLE <- "Titre du graphique"
BAR_Yaxis <- "Titre de l'axe y"


###########################################################################################################################################
# Fonctions utilisées dans le script
##########################################################################################################################################

#=======================================================================================================================================
# Réalise le test de Student sur les données contenue dans le dataframe df
#=======================================================================================================================================
student.test <- function (df) {
  # Crée un data frame pour stocker les pvalue du test de student
  pval <- data.frame()
  # Réalise le test de student sur chaque colonne de df en utilisant la première colonne comme référence
  for (i in 2 : ncol(df)) {
    pval[i,1] <- as.data.frame(t.test(df[,1], df[,i], alternative = "two.sided")$p.value)
  }
  # Nomme colonnes et lignes du dataframe pval
  colnames(pval) <- c("pvalue")
  rownames(pval) <- colnames(df)
  pval$padjust <- p.adjust(pval$pvalue, method = "BH", n = length(pval$pvalue))
  # Renvoie le resultat du test de Student sous la forme d'un dataframe
  return(pval)
}
#=======================================================================================================================================

#=======================================================================================================================================
# Identifie la plus grande valeur du jeu de donnée pour établir la hauteur de l'axe y
#=======================================================================================================================================

maximum_y <- function () {
  ymax <- max (as.numeric(resume[4,]) + sd_df$SD)
}
#=======================================================================================================================================

#=======================================================================================================================================
# Crée un Barplot représentant les moyennes des données contenues dans df
#=======================================================================================================================================
# La fonction prend en paramètre le titre principal du graphique et le titre de l'axe y
graph.norm <- function(titre_graph, titre_axe_y, couleur = rainbow(ncol(df))) {
  ymax <- max (as.numeric(resume[4,]) + sd_df$SD)        
  # Dessine le barplot
  plot <- barplot(c(as.numeric(resume[4, ])),
                  col = couleur,
                  ylim = c(0, ymax + ymax*0.2),
                  main = titre_graph,
                  ylab = titre_axe_y,
                  names.arg = colnames(df)
  )
  # Ajout des barres d'erreurs : en vérité des fléches à double pointe (code=3) et pointes horizontales (angle = 90)
  arrows(plot[,1], as.numeric(resume[4,]) - sd_df$SD, plot[,1], as.numeric(resume[4,]) + sd_df$SD, angle = 90, code = 3)
  return(plot)
}
#=======================================================================================================================================

#=======================================================================================================================================
# Positonne les étoiles de significativité dans le cas des données normales
#=======================================================================================================================================
star_norm <- function() {
  for (i in 1 : ncol(df)) {
    x.star <- mean(plot[i, 1])      #permet de sélectionner la colonne voulu dans le plot
    y.star <- ymax*0.05 + (as.numeric(resume[4, i]) + sd_df[i, 1])    #position en y des étoiles 5% du ymax au dessus de la barre d'erreur de chaque colonne
    # Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    star = ""
    if (is.na(pval[i,2])) {                        # test le cas ou la pvalue n'est pas calculée (Na)
      star = " "
    } else if (pval[i, 2] < 0.001) {
      star = "***"
    } else if (pval[i, 2] < 0.01) {
      star = "**"
    } else if (pval[i, 2] < 0.05) {
      star = "*"
    } else if (pval[i, 2] < 0.1) {
      star = "."
    } else {
      star = " "
    }
    text(star, x = x.star, y = y.star)
  }
}
#=======================================================================================================================================

#=======================================================================================================================================
# Fait le test de Kruskal-Wallis
#=======================================================================================================================================
# Retourne la pvalue du test
test_kruskal <- function() {
  pval_test <- kruskal.test((as.list(df)))$p.value
  if (pval_test < 0.05) {
    print ("Le test de Kruskall Wallis compare les médianes, la pvalue est < 0.05 ce qui indique qu’au moins 1 des médianes est différentes des autres, on réalise un test post hoc de Dunn")
    test_dunn()
  } else {
    print ("Le test de Kruskall Wallis compare les médianes, la pvalue est > 0.05 ce qui indique qu’il n'y a pas de différence entre les médianes")
  }
  return(pval_test)
}

#=======================================================================================================================================

#=======================================================================================================================================
# Fait le test de Dunn
#=======================================================================================================================================
# retourne les pvalue dans un dataframe
test_dunn <- function() {
  # installer le package DescTools si nécessaire
  if (!requireNamespace("DescTools", quietly = TRUE))
    install.packages("DescTools")
  
  library(DescTools)
  pval <- as.data.frame(DunnTest(as.list(df), method = "BH", out.list = FALSE)[1])
  print(DunnTest(as.list(df), method = "BH"))
  return(pval)
}
#=======================================================================================================================================

#=======================================================================================================================================
# Positonne les étoiles de significativité dans le cas des données non paramétriques
#=======================================================================================================================================
star_non_param <- function() {
  for (i in 1 : (ncol(df)-1)) {
    x.star <- mean(plot[i+1, 1])     #permet de sélectionner la colonne voulu dans le plot. Le +1 permet de commencer à ajouter les étoiles sur la deuxieme colonne
    y.star <- ymax*0.05 + (as.numeric(resume[4, i+1]) + sd_df[i+1, 1])  #position en y des étoiles 5% du ymax au dessus de la barre d'erreur de chaque colonne
    # Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    star = ""
    if (is.na(pval[i,1])) {
      star = " "
    } else if (pval[i, 1] < 0.001) {
      star = "***"
    } else if (pval[i, 1] < 0.01) {
      star = "**"
    } else if (pval[i, 1] < 0.05) {
      star = "*"
    } else if (pval[i, 1] < 0.1) {
      star = "."
    } else {
      star = " "
    }
    text(star, x = x.star, y = y.star)
  }
}
######################################################################################################################################



######################################################################################################################################
# Partie principale du script

# Chargement des données
# pas de colonne de n° de ligne. 
# les noms de colonnes doivent commencer par une lettre. Si vous avez des lignées avec des noms numériques type 1.1, nomer la L1.1
df <- read.table(DATA, header = TRUE, sep = "\t")

####################################################################################################################################

# I. Statistiques descriptives
# On place dans un tableau les quantiles, la moyenne et la médiane pour toutes les colonnes du fichier de départ
resume <- as.data.frame(do.call(cbind, lapply(df, summary)))

# II. Boxplot des données
boxplot(df, 
        xlab = BOXPLOT_Xaxis, ylab = BOXPLOT_Yaxis, 
        main = BOXPLOT_TITLE, 
        col = rainbow(ncol(df)))          # rainbow(ncol(df)) peut être remplacer par un vecteur de couleur (autant que de colonne) du type c("red", "green"). 

# III. Calcul de la Déviation standard écart type/racine(nb de mesure)

sd_df <- as.data.frame(apply(df, 2, sd, na.rm = TRUE)/sqrt(nrow(df)))
colnames(sd_df) <- c("SD")

# IV. test de normalité de Shapiro sur chaque colonne 
shapiro_df <- apply(df, 2, shapiro.test)
if (sum(do.call(rbind,lapply(shapiro_df,function(v){v$p.value})) > 0.05) == ncol(df)) {
  print ("Les données de df suivent une loi normale, on réalise un test de Student")
  pval <- student.test(df)
  ymax <- maximum_y()
  plot <- graph.norm("Titre du graphique", "Titre de l'axe y")
  star_norm()
} else {
  print ("Les données de df ne suivent pas une loi normale, on réalise un test de Kruskall Wallis et un test de Dunn")
  pavl_test <- test_kruskal()
  pval <- test_dunn()
  ymax <- maximum_y()
  plot <- graph.norm("Titre du graphique", "Titre de l'axe y")
  points(x = plot, y = resume[3, ])
  star_non_param()
}

######################################################################################################################################
# R session

if (!require(devtools)) { install.packages("devtools") }

devtools::session_info()


