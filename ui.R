
options(spinner.color="red")



header=dashboardHeader(
  title="Menu",
  titleWidth = 200,
  tags$li(a(strong("Master ESA"),
            href = "https://www.master-esa.fr"),
          class = "dropdown")
)

sidebar=dashboardSidebar( 
  width = 200,
  sidebarMenu(
    id = 'sidebar',
    style = "position: relative; overflow: visible;",
    menuItem( "Présentation", tabName = 'pres', icon = icon('book')),
    
    menuItem( "Application", tabName = 'app', icon = icon('edit'),
              menuItem("Statistiques descriptives", tabName = "stats", icon = icon('chart-bar')),
              
              menuItem('Modélisation', tabName = "modele", icon = icon('layer-group'),
                       menuSubItem('Lasso', tabName = "lasso", icon = icon('circle')),
                       menuSubItem('Adaptive Lasso', tabName = "alasso", icon = icon('circle')),
                       menuSubItem('Elastic-Net', tabName = "elnet", icon = icon('circle')),
                       menuSubItem('Random Forest', tabName = "rforest", icon = icon('circle')),
                       menuSubItem('Boosting', tabName = "boost", icon = icon('circle'))
              ),
              
              menuItem('Comparaison', tabName = "compare", icon = icon("not-equal"))
    ),
    
    menuItem( "À propros", tabName = 'propos', icon = icon('user'))
  ))




body= dashboardBody(
  tabItems(
    tabItem(tabName="pres",
            img(src='image.png', width = "1100px",style="display: block; margin-left: auto; margin-right: auto;"),
            br(),
            br(),
            h2("Présentation", align="center"),
            br(),
            br(),
            p(style="text-align: justify;", "Le but de ce travail est de se focaliser sur un problème de classification. Plus précisement sur la probabilité de défaut.",
              "Dans ce contexte, les algorithmes ou méthodes de classification supervisées abordées (Lasso, Elastic-Net, Adaptive Lasso, 
            Forêts aléatoires, Boosting) doivent être mis en compétition dans le but de sélectionner celui qui présente le plus fort pouvoir de généralisation.",
              br(),
              br(),
              "Pour cela, nous allons appliquer ces algorithmes et méthodes à notre jeu de données (Kaggle : Give me some credit). Nous allons chercher à prédire le défaut de nos individus. Le défaut correspond au fait qu'un individu passe au moins 90 jours en défaut sur la période étudiée. Notre jeu de données dispose de 150 000 observations et 11 variables. 
              Dans un premier temps, nous étudions quelques statistiques descriptives ainsi que la répartition de la variable cible et celle de toutes les variables explicatives afin d’avoir une première idée de nos données.", 
              "Dans un deuxième temps, nous modélisons les cinq méthodes précédemment énoncées sur notre jeu de données et prenons en compte la matrice de confusion, l’aire sous la courbe ROC (AUC) et la précision de chaque modèle sur l’échantillon test.",
              "La dernière partie de notre travail consiste à comparer chaque méthode. Afin de ne retenir que la meilleure, nous comparons l'AUC et la précision des modèles mais également celles de l'échantillon d'apprentissage à celles de l'échantillon test afin de détecter un éventuel sur-apprentissage.")
    ),
    
    tabItem(tabName="stats", 
            h2("Statistiques descriptives", align="center"),
            br(),
            br(),
            p(style="text-align: justify;", "Observer les données brutes est une étape primordiale avant de passer à la modélisation des donneés. Pour cela, affichons le début de notre jeu de données."),
            br(),
            box( width=12, title="Affichage des premières observations", withSpinner(tableOutput("tab")),status="danger", solidHeader=T,style = "height:500;overflow-x: scroll;"),
            br(),
            p(style="text-align: justify;", "Afin de mieux comprendre le jeu de données, vous pouvez télécharger un PDF explicant les différentes variables."),
            actionButton("pdf", "Description des variables", icon=icon("mouse-pointer"), style="float:right; color: #fff; background-color: #6e5b5b; border-color: #484545"),
            br(),
            br(),
            br(),
            p(style="text-align: justify;", "Nos données comportaient quelques valeurs manquantes sur deux variables quantitatives. Celles ci ont donc été imputées par la médiane. Cette décision se base sur la proportion faible de valeurs manquantes mais également par le fait que la médiane n'est pas affectée par les outliers contraitrement à la moyenne."), 
            br(),
            br(),
            
            hr(style="border-top: 1px solid #000000;"),
            h3("Statistiques descriptives classiques", align="left"),
            br(),
            withSpinner(verbatimTextOutput("summary")),align="center",
            br(),
            p(align="justify",style="text_align:justify;", "A première vue, il ne semble pas y avoir de problème dans notre jeu de données. Notre variable cible est bien binaire. Toutes les variables explicatives sont quantitatives. Les minimums et maximums des variables ne semblent pas indiquer de valeurs aberrantes."),
            br(),
            
            hr(style="border-top: 1px solid #000000;"),
            h3("Répartition de la variable cible", align="left"),
            br(),
            fluidRow(
              splitLayout(cellWidths = c("50%", "50%"), withSpinner(plotOutput("graph1")), withSpinner(plotOutput("graph2")))),
            br(),
            p(align="justify",style="text_align:justify","Le pourcentage de défaut est très faible. Nous sommes en présence d'un évènement rare. Cela risque de poser problème lors de notre modélisation, nous effectuons donc un rééchantillonnage de nos données afin d'augmenter le pourcentage de défaut. Nous appliquons un rééchantillonnage de type SMOTE, celui-ci va d'une part augmenter le nombre d'événement en dupliquant certains individus ayant fait défaut et d'une autre part réduire les non-événements en supprimant quelques individus n’ayant pas fait défaut. Après rééchantillonnage, nous obtenons la répartition de droite. Le rééchntillonnage va poser problème lors de la construction des matrices de confusion qu'il faudra corriger."),
            br(),
            br(),
            
            h3("Répartition des variables explicatives", align="left"),
            br(),
            withSpinner(plotOutput("graph3")),
            br(),
            p(style="text_align:justify", align="justify", "La répartition des variables explicatives n'indique pas de problème. Nous pouvons donc passer à la modélisation.")),
    
    tabItem(tabName = "lasso",
            h2("Lasso", align="center"),
            br(),
            withMathJax(),
            p(style="text-align: justify;",
              "Cette méthode est adaptée lorsque le nombre de prédicteurs est plus élevé, voir beaucoup plus élevé que le nombre d'observations.",br(), 
              "Nous l'utilisons lorsqu'il y a multicolinéarité, c'est-à-dire lorsque la matrice des observations n'est pas inversible, ou lorsque nous risquons d'avoir un estimateur MCO non 
            unique et un grand risque de sur-ajustement. Nous pouvons aussi l'utiliser lorsque le nombre de prédicteurs est inférieur au nombre d'observations mais est tout de même élevé. 
            Cette méthode sert ainsi à faire une sélection de variables et à éviter le risque de sur-ajustement. ",br(),"L'hypothèse de base du Lasso est que le vecteur de paramètres est creux 
            ou éclairci, c'est-à-dire que certains paramètres sont égaux à 0, ce qui est une hypothèse raisonnable dans le cadre du nombre élevé de prédicteurs. Pour cela, nous cherchons à trouver le vecteur \\(\\beta\\) qui va minimiser \\(PRSS(\\beta)=(y-X\\beta)^T(y-X\\beta)+\\lambda\\sum_{j=1}^p|\\beta_j|\\) avec  \\(\\lambda>0\\).",br(),
              "La sélection de variables permet donc, en plus de diminuer le risque de sur-ajustement et d'améliorer la prévision, de faciliter également l'interprétation du modèle.",br(),
              "Il est important de noter que la sélection effectuée par le Lasso a une efficacité supérieure à celles qu'offrent les méthodes de sélection traditionnelles, telles que le 
            Forward le Backward, que ce soit pour l'invariance suite à la perturbation de l'échantillon d'apprentissage, ou pour l'exercice de prévision. De plus, la condition d'irreprésentabilité doit être satisfaite."),
            br(),
            withMathJax(),
            p(style="text-align: justify;", "NB: Nous faisons varier \\(\\lambda\\) entre 0 et 0.05. Il peut être supérieur à 0.05 mais avec \\(\\lambda=0.05\\) nous avons déjà la quasi-totalité des paramètres qui sont nuls."),
            tabBox(
              tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #333333}")),
              title = "Choix du paramètre", width = 14,
              sliderInput("lambda.bestlasso","Lambda", width=1400,min = 0, max = 0.05, value = 0.02505, step = 0.000001)),
            textOutput("lambdalasso"),
            br(),
            p(style="text-align: justify;", "NB: Il est possible que la matrice de confusion, l'AUC et la précision ne soient pas modifées par la valeur sélectionnée, car la modification sera trop faible pour être visible au vu des arrondis"),
            box(title="Coefficients",withSpinner(tableOutput("coeflasso")), status="danger", solidHeader=T, align="center", style = "height:500;"),
            box(title="Matrice de confusion corrigée",withSpinner(tableOutput("matconfulasso")), status="danger", solidHeader=T, align="center",style = "height:500;"),
            valueBoxOutput("acc.lasso"),
            valueBoxOutput("auc.lasso")
            
    ),
    
    tabItem(tabName = "alasso",
            h2("Adaptive Lasso", align="center"),
            br(),
            
            withMathJax(),
            p(style="text-align: justify;", 
              "Cette méthode est définie en deux étapes. Dans un premier temps, nous calculons un estimateur préliminaire de Î² pouvant être l'estimateur Ridge ou tout autre estimateur. 
            Dans un second temps, cet estimateur préliminaire est utilisé pour ajuster la pénalité imposée sur chacun des coefficients du paramètre de régression de l'Adaptive Lasso.",br(),
              "Les pondérations permettent de réduire la pénalisation des coefficients lorsque la valeur de l'estimateur de Î² est grande et de la renforcer dans le cas contraire.
            Dans le cas où l'estimateur préliminaire est asymptotiquement consistant pour l'erreur d'estimation, l'Adaptive Lasso est consistant en sélection de variables et en
            estimation, avec p fixé. Ces propriétés restent vérifiées même dans des situations où le Lasso échoue et donc où la condition d'irreprésentabilité n'est pas satisfaite.", br(), "Nous devons trouver le vecteur \\(\\beta\\) qui va minimiser \\(PRSS(\\beta)=(y-X\\beta)^T(y-X\\beta)+\\lambda\\sum_{j=1}^pw_j|\\beta_j|\\) avec \\(\\lambda>0\\) et \\(w_j\\) représente le poids des paramètres obtenu avec la regréssion Ridge."),
            br(),
            withMathJax(),
            p(style="text-align: justify;", "NB: Nous faisons varier \\(\\lambda\\) entre 0 et 1. Il peut être supérieur à 1 mais nous remarquons que la valeur de \\(\\lambda\\) a peu d'impact sur les coefficients."),
            tabBox(
              tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #333333}")),
              title = "Choix du paramètre", width = 14,
              sliderInput("lambda.bestalasso","Lambda", width=1400, min = 0, max = 1, value = 0.5, step = 0.001)),
            textOutput("lambdaalasso"),
            br(),
            p(style="text-align: justify;", "NB: Il est possible que la matrice de confusion, l'AUC et la précision ne soient pas modifées par la valeur sélectionnée, car la modification sera trop faible pour être visible au vu des arrondis"),
            box(title="Coefficients",withSpinner(tableOutput("coefalasso")),align="center",status="danger", solidHeader=T,style = "height:500;"),
            box(title="Matrice de confusion corrigée",withSpinner(tableOutput("matconfualasso")),align="center",status="danger", solidHeader=T,style = "height:500;"),
            valueBoxOutput("acc.alasso"),
            valueBoxOutput("auc.alasso")),
    
    tabItem(tabName = "elnet",
            h2("Elastic-Net", align="center"),
            br(),
            withMathJax(),
            p(style="text-align: justify;",
              "La méthode Elastic-Net combine les atouts des méthodes Ridge et Lasso. L'Elastic-net a été introduit afin de pallier deux limites du Lasso.",br(),"Premièrement, 
              le Lasso ne peut sélectionner qu'au plus n variables dans le cas où n < p, avec n le nombre d'observations, et p le nombre de prédicteurs. Deuxièmement, en présence 
              d'un groupe de variables fortement corrélées, le Lasso ne sélectionne généralement qu'une seule variable du groupe. L'idée est donc d'ajouter au Lasso une pénalité Ridge.",br(), 
              "De plus, lorsque des variables pertinentes pour notre analyse sont écartées du modèle, l'Elastic-Net permet à ces variables de devenir significatives grâce à la pénalisation Ridge.", br(),  "Nous devons trouver le vecteur \\(\\beta\\) qui va minimiser \\(PRSS(\\beta)=(y-X\\beta)^T(y-X\\beta)+\\lambda\\sum_{j=1}^p|\\beta_j|+\\alpha\\sum_{j=1}^p\\beta_j^2\\) avec \\(\\lambda>0\\) et \\(\\alpha>0\\)."),
            br(),
            
            withMathJax(),
            p(style="text-align: justify;", "NB: Nous faisons varier \\(\\lambda\\) entre 0 et 0.05 et \\(\\alpha\\) entre 0 et 1. Ils peuvent être supérieurs à ces valeurs mais nous remarquons que cela a peu d'impact sur les coefficients."),
            tabBox(
              tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #333333}")),
              tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: #333333}")),
              title = "Choix des paramètres", width = 14,
              sliderInput("lambda.bestelnet","Lambda", width=1400, min = 0, max = 0.05, value = 0.02505, step = 0.000001),
              sliderInput("alpha.bestelnet", "Alpha", width=1400, min = 0, max = 1, value = 0.5, step = 0.001)),
            textOutput("lambdaalphaelnet"),
            br(),
            box(title="Coefficients",status="danger", solidHeader=T,withSpinner(tableOutput("coefelnet")),align="center",style = "height:500;"),
            box(title="Matrice de confusion corrigée",status="danger", solidHeader=T,withSpinner(tableOutput("matconfuelnet")),align="center",style = "height:500;"),
            valueBoxOutput("acc.elnet"),
            valueBoxOutput("auc.elnet")),
    
    tabItem(tabName = "rforest",
            h2("Random Forest", align="center"),
            br(),
            
            withMathJax(),
            p(style="text-align: justify;",
              "Cet algorithme appartient à la famille des agrégations de modèles, c'est en fait un cas particulier du Bagging, appliqué aux arbres de décision de type CART.",br(),
              "Le principe des méthodes de Bagging, et donc en particulier des forêts aléatoires, est de faire la moyenne des prévisions de plusieurs modèles indépendants pour réduire 
            la variance et donc l'erreur de prévision.",br(),"Pour construire ces différents modèles, nous sélectionnons plusieurs échantillons bootstrap, c'est à dire des tirages avec remise.
            En plus du principe de Bagging, les forêts aléatoires ajoutent de l'aléa au niveau des variables. Pour chaque arbre, nous sélectionnons un échantillon bootstrap d'individus et 
            à chaque étape, la construction d'un noeud de l'arbre se fait sur un sous-ensemble de variables tirées aléatoirement, souvent égal à \\(\\sqrt{p}\\) prédicteurs.",br(),
              "Nous nous retrouvons donc avec plusieurs arbres et donc des prédictions différentes pour chaque individu. L'estimation finale se fait :",br(),
              "- Dans le cas d'une classification : nous choisissons la catégorie la plus fréquente (le mode)",br(),
              "- Dans le cas d'une régression : nous faisons la moyenne des valeurs prédites",br(),
              br(),
              "Le Random Forest est un algorithme particulièrement performant pour les problématiques de prédiction. En particulier, nous pouvons les utiliser quand nous avons un nombre de variables 
            explicatives important. En revanche, les forêts aléatoires sont une boîte noire, c'est à dire que nous connaissons seulement le résultat, les interprétations sont très difficiles.",br(), 
              "Néanmoins, il faut toujours être en mesure d'expliquer un modèle et avec cet algorithme il est possible de calculer l'importance de chaque variable pour présenter leur contribution 
            au modèle."),
            br(),
            p(style="text-align: justify;", "NB: Prendre un nombre élévé d'arbres augmente considérablement le temps d'affichage."),
            
            tabBox(
              tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: #333333}")),
              title = "Choix du paramètre", width = 14,
              sliderInput("ntree","Nombre d'arbres", width=1400,min = 0, max = 200, value = 10, step = 5)),
            textOutput("nbtree"),
            br(),
            box(title="Matrice de confusion corrigée", status="danger", solidHeader=T ,withSpinner(tableOutput("matconfurf")),align="center",style = "height:500;"),
            valueBoxOutput("acc.rf"),
            valueBoxOutput("auc.rf")),
    
    tabItem(tabName = "boost",
            h2("Boosting", align="center"),
            br(),
            p(style="text-align: justify;",
              "L'idée du Boosting est d'utiliser plusieurs méthodes que nous agrégeons ensuite pour obtenir un seul résultat.",br(), 
              "Dans  la construction des modèles, le Boosting travaille de manière séquentielle : il commence par construire un premier modèle qu'il va évaluer. 
              Puis, à partir de cette mesure, chaque individu va être pondéré en fonction de la performance de la prédiction.",br(),
              "L'objectif est de donner un poids plus important aux individus pour lesquels la valeur a été mal prédite pour la construction du modèle suivant. 
              Le fait de corriger les poids au fur et à mesure permet de mieux prédire les valeurs difficiles."),
            br(),
            p(style="text-align: justify;", "NB: Prendre un nombre élévé d'itérations augmente considérablement le temps d'affichage."),
            
            tabBox(
              tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: #333333}")),
              title = "Choix du paramètre", width = 14,
              sliderInput("mfinal","Nombre d'itérations", width=1400, min = 0, max = 200, value = 10, step = 5)),
            textOutput("nbit"),
            br(),
            box(title="Matrice de confusion corrigée", status="danger", solidHeader=T, withSpinner(tableOutput("matconfuboost")),align="center",style = "height:500;"),
            valueBoxOutput("acc.boost"),
            valueBoxOutput("auc.boost")),
    
    tabItem(tabName="compare",
            h2("Comparaison des méthodes", align="center"),
            br(),
            br(),
            p(style="text-align: justify;",
              "Nous pouvons à présent comparer chaque méthode afin de choisir laquelle convient le mieux à notre jeu de données.",br(),
              "Pour comparer au mieux les modèles, nous avons déterminé les paramètres optimaux de chacune des méthodes par validation croisée.",br(),br(),
              "Nous comparons les AUC ainsi que les précisions de chaque modèle sur les échantillons tests. Cependant, nous prêtons tout de même attention a celles des échantillons d'apprentissage afin de détecter un possible sur-apprentissage."),
            br(),
            br(),
            box(width=12, title="Comparaison", status="danger", solidHeader=T,withSpinner(tableOutput("comparaison")),align="center",style = "height:500;"),
            br(),
            p(style="text-align: justify;",
              "Le meilleur modèle en tous points est le Random Forest car c'est la méthode avec le meilleur AUC ainsi que la meilleure précision. Cependant, si nous comparons les deux critères entre les échantillons d'apprentissage et test nous remarquons un écart important. Cet écart n'est pas présent pour les autre m&thodes. Il y a donc du sur-apprentissage avec le Random Forest. Le Boosting pourrait être une bonne alternative car l'AUC et la précision sont proches de celles obtenues avec Random Forest mais d'après les résultats nous ne concluons pas à la présence de sur-apprentissage.")),
    
    
    tabItem(tabName = "propos",
            h2("Notre Equipe",align="center"),
            br(),
            br(),
            p(style="text-align: justify;",
              "Cette application a été réalisée dans le cadre d'un projet de la formation Master économétrie et statistiques appliquées par deux étudiantes de deuxième année.", br(),br(),
              "Ce projet est lié au cours Big Data Analytics (arbres de décisions, méthodes d'aggrégations et méthodes de pénalisaitons) enseigné par M. Tokpavi."),
            br(),
            br(),
            
            hr(style="border-top: 1px solid #000000;"),
            br(),
            p(style="text-align: justify;", "Pour toute question concernant cette application, n'hésitez pas à nous contacter."),
            br(),
            fluidRow(widgetUserBox(
              title = userDescription(
                title =tags$a(href='https://www.linkedin.com/in/no%C3%ABline-lepais-731040174/',
                              icon("linkedin"),
                              'Noeline LEPAIS'),
                type = 1,
                image = "https://media-exp1.licdn.com/dms/image/C4E03AQFoDhRQd8jexw/profile-displayphoto-shrink_800_800/0/1639325629962?e=1648080000&v=beta&t=8dsZaoa-qOEiR0welua5_Olo2cPdjsblUrDAW4CDh_w")
            ),
            widgetUserBox(
              title = userDescription(
                title =tags$a(href='https://www.linkedin.com/in/lou-daccord-ab668b1a3/',
                              icon("linkedin"),
                              'Lou DACCORD'),
                type = 1,
                image = "https://media-exp1.licdn.com/dms/image/C4D03AQGHt9VVHK6AOA/profile-displayphoto-shrink_800_800/0/1633191506540?e=1648080000&v=beta&t=995PS3m9PsPTtkz6gPhKViNPmEduNO4L1ntGO2fWaWk",
              ))),
            br(),
            hr(style="border-top: 1px solid #000000;"))
  )
)


ui=dashboardPage(header, sidebar, body, 
                 skin = "red" )



#site: https://alisonjollois.github.io/r-shiny/jour2-shiny.html
#https://shiny.rstudio.com/gallery/
#https://ctmm.shinyapps.io/ctmmweb/

