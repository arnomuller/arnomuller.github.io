#' Interface d'assistance à l'ACM
#' @description
#' Une fonction pour faire apparaître une fenetre Shiny permettant l'import, la selection de population, la selection de variables actives et supplementaires, une exploration sommaire de l'ACM, une aide à la classification, et une passerelle vers le package explor() de Julien Barnier.
#'
#'
#'
#'
#' @return la fonction renvoie une fenetre Shiny
#' @import shiny
#' @import shinyWidgets
#' @import shinyBS
#' @import shinyjs
#' @import shinythemes
#' @import haven
#' @import labelled
#' @import FactoMineR
#' @import factoextra
#' @import GDAtools
#' @import explor
#' @import ade4
#' @import fastcluster
#' @import ggExtra
#' @import DT
#' @import ggrepel
#' @import corrplot
#' @import ggplot2
#' @import forcats
#' @import dplyr
#' @import stringr
#' @import tibble
#' @import tidyr
#' @import readr
#' @import questionr
#' @import ggiraph
#'
#' @export


autoacm <- function() {

  jscode <- "shinyjs.closeWindow = function() { window.close(); }"
  # Pour permettre la création d'un "hub" de Shiny permettant de lancer successivement des Shiny différents
  # Dans notre cas, on supprime le Hub en faisant une passerelle directement de l'application 1 (la mienne),
  # vers la fonction explor() de Julien Barnier.


  options(shiny.maxRequestSize = 80*1024^2)
  # Pour importer des données plus importantes (ici 80Mo)

  options(scipen=999)
  # Pour supprimer la notation scientifique des tables notamment.


  # Define UI for application that draws a histogram
  ui <- fluidPage(

    # Pour créer un "Hub" de Shiny
    useShinyjs(),
    extendShinyjs(text = jscode, functions = c("closeWindow")),

    # Choisir le theme : exemple : https://rstudio.github.io/shinythemes/
    theme = shinytheme("cerulean") ,

    sidebarLayout(
      # Création du panneau latéral
      sidebarPanel(

        # On commence par créer une 1ere boite (wellPanel) pour l'import de donnée, elle s'affiche dès le lancement.
        # La base sélectionnée sera stockée dans une variable réactive data(),créee dans le script server.
        wellPanel(

          h4("Import des données :"),
          # Création de bouton CSV, SAS ou Stata, pour l'import des données
          radioButtons("datatype","Format des données à importer : ",choices = c(".csv",".sas7bdat",".dta"), selected=".csv",inline=TRUE),

          # Si les données sont en CSV
          conditionalPanel(condition="input.datatype=='.csv'",
                           # Comme d'une base à l'autre le séparateur du CSV peut changer, on propose un choix avec radioButtons() :
                           radioButtons("separator","Séparateur des colonnes :",choices = c(";",",",":"), selected=";",inline=TRUE)),

          # Si les données sont en SAS
          conditionalPanel(condition="input.datatype=='.sas7bdat'",
                           # On crée un bouton pour savoir si l'util possede un catalogue des labels
                           radioButtons("catalog","Avez-vous un catalogue de labels (format .sas7bcat) ?",
                                        choices = c("Oui","Non"), selected="Non",inline=TRUE)),


          # Une fois le fichier sélectionné, on stocke un signal appelé "target_upload" dans l'input.
          fileInput('target_upload', 'Choix de la base de donnée',
                    accept = c(
                      'text/csv',
                      'text/comma-separated-values',
                      '.csv',
                      '.sas7bdat',
                      '.dta'),
                    buttonLabel = "Parcourir...",
                    placeholder = "Pas de base sélectionnée"),

          # Si l'util a dit avoir un catalogue de format, alors on lui permet de l'importer dans le signal catalog_upload
          conditionalPanel(condition="input.catalog=='Oui' && input.datatype=='.sas7bdat'",
                           fileInput('catalog_upload', 'Choix du catalogue de label SAS (.sas7bdat)',
                                     accept = '.sas7bcat',
                                     buttonLabel = "Parcourir...",
                                     placeholder = "Pas de catalogue sélectionné")),

        ),


        # Si les données ont bien été importé, alors le  script server renvoie un signal afficher_choix_souspop == Oui
        # Dans ce cas, on fait apparaître les outils de séléction de sous-population, crée dans le script server.R
        # dans le signal : selection_variables
        conditionalPanel(condition="output.afficher_choix_souspop == 'Oui'",
                         wellPanel(
                           h4("Choix de la population"),

                           # On appelle la boucle qui permet de choisir les variables de sélection (créee dans server.R)
                           uiOutput("selection_variables")
                           ,
                           conditionalPanel(condition="output.afficher_choix_moda == 'Oui'",
                                            conditionalPanel(condition= c("input.windows=='Table'", "input.windows=='ACM'", "input.windows=='CAH'"),
                                                             h5("Filtrer les modalités d'intérêt dans les variables précédentes :"),
                                                             # On appelle la boucle qui permet de faire la selection des modalités
                                                             # Creée dans ServeurTableau.R
                                                             uiOutput("selection_modalites"))
                           ))



        )),


      # On commence le panneau principal
      mainPanel(
        # Une fois que l'util aura appuyé sur Valider pour confirmer son choix de données
        conditionalPanel(condition="output.afficher_choix_moda == 'Oui'",
                         # On divise le panneau principal en 3 fenetres
                         tabsetPanel(id = "windows",


                                     # 1er : affiche l'objet "Table" avec les variables et la sous-population
                                     tabPanel("Tableau de données", value = "Table",
                                              h4("Table"), # niveau 4 en html
                                              # On affiche la table, construite dans ServeurTableau.R
                                              DT::dataTableOutput("table"),
                                              br(), # break row, saut de ligne
                                              # On affiche un bouton pour télécharger les données
                                              column(12, align="center", id="buttons",
                                                     downloadButton('downLoadFilter',"Télécharger les données filtrées")),
                                              br()),

                                     # 2eme : On permet l'ACM.
                                     tabPanel("ACM", value = "ACM",
                                              h4("Analyse des Correspondances Multiples"),
                                              # On affiche une phrase avec le nombre de personnes dans la population
                                              uiOutput("recap_activ"),
                                              br(),
                                              h5("Tableaux croisés pour explorer la base"),
                                              # On affiche l'outil de selection de variable
                                              uiOutput("affichage_choix_table_ACM"),
                                              # On affiche le tri croisé de "cluster" avec la variable voulue
                                              fluidRow(column(8, offset = 1, uiOutput("affichage_table_ACM"))),
                                              br(),
                                              hr(),

                                              # On sélectionne les variables active et supplémentaires
                                              h5("Sélection des variables actives et supplémentaires"),
                                              uiOutput("acm_main_act"),
                                              uiOutput("acm_main_sup"),

                                              # On affiche 2 boutons
                                              helpText("Cliquer sur 'Valider' une fois votre choix de variables fait."),
                                              fluidRow(column(5, offset = 2,
                                                              # Valider l'ACM
                                                              actionButton("acmOK", "Valider", class = "btn-success"))),



                                              br(),
                                              hr(), # Barre horizontale


                                              # On affiche le bandeau de choix des représentations graphiques et tables
                                              uiOutput("affichage_choix_ACM"),

                                              # On affiche la représentation demandée
                                              uiOutput("affichage_ACM"),

                                              br(),
                                              br()),



                                     # 3eme : On fait la CAH
                                     tabPanel("Classification", value = "CAH",

                                              h4("Classification Ascendante Hiérarchique"),
                                              helpText("CAH calculée à partir des distances de Ward uniquement"),

                                              # On affiche le bandeau de choix de représentations
                                              uiOutput("affichage_choix_graph"),
                                              # On affiche la représentation demandée
                                              uiOutput("affichage_graphique"),

                                              br(),
                                              br(),
                                              br(),


                                              hr(),

                                              h5("Description bivariée de la variable 'cluster' :"),
                                              # On affiche l'outil de selection de variable
                                              uiOutput("affichage_choix_table"),
                                              # On affiche le tri croisé de "cluster" avec la variable voulue
                                              fluidRow(column(8, offset = 1, uiOutput("affichage_table"))),

                                              br(),
                                              hr(),

                                              fluidRow(h5("Vous pouvez sauvegarder la base de donnée avec une nouvelle variable 'cluster'")),

                                              # Bouton pour télécharger les données avec la variable cluster.
                                              fluidRow(column(4, offset = 1, radioButtons("datatype_sortie","Types des données en sortie : ",choices = c(".csv",".sas7bdat",".dta"), selected=".csv",inline=TRUE) ),
                                                       column(4, id="buttons",
                                                              downloadButton('downLoadCluster',"Télécharger avec Cluster"))),
                                              br(),
                                              hr(),
                                              br())
                         ))) # Fin du Main Panel
    ) # Fin sidebarLayout
  )

  # Define server logic required to draw a histogram
  server <- function(input, output, session) {


    # IMPORT DES DONNEES ----

    # data() est un objet reactif, qui est crée ou modifié à chaque fois qu'on
    # séléctionne des données dans l'application.
    data <- reactive({
      inFile <- input$target_upload
      if (is.null(inFile)) {
        return(NULL)
      }

      # Si les données sont en CSV :
      if (input$datatype == ".csv"){
        df <- read.csv(inFile$datapath, header = TRUE, sep = input$separator)
        nomcol_data <<- colnames(df)
        return(df)
      }

      # Si les données sont en SAS :
      if (input$datatype == ".sas7bdat"){

        # Si pas de catalogue de labels :
        if (input$catalog == "Non"){
          df <- as.data.frame(read_sas(inFile$datapath))
          nomcol_data <<- colnames(df)
          return(df)
        }

        # Si on a un catalogue de labels :
        if (input$catalog == "Oui"){
          # Import du fichier sas7bcat
          CataloginFile <- input$catalog_upload
          if (is.null(CataloginFile))
            return(NULL)

          # Import de la base avec les labels qui deviennent des modalités
          df <- as.data.frame(lapply(
            read_sas(inFile$datapath, catalog_file = CataloginFile$datapath),
            to_factor))
          nomcol_data <<- colnames(df)
          return(df)
        }
      } # Fin IF SAS


      if (input$datatype == ".dta")  {
        df <- as.data.frame(lapply(
          read_stata(inFile$datapath),
          to_factor))
        nomcol_data <<- colnames(df)
        return(df)
      } # Fin Stata
    }) # Fin Import

    # showModal(modalDialog("Importation de la base... Si l'opération prend du temps : rééssayer en enlevant des variables inutilisées",
    # footer=NULL))
    # removeModal()


    # CHOIX DE LA SOUS-POPULATION ----

    # Création d'un signal reactif qui indique si les données sont chargées
    output$afficher_choix_souspop <- reactive({
      if (is.null(data()) == FALSE) {
        "Oui"
      }
    })
    outputOptions(output, "afficher_choix_souspop", suspendWhenHidden=FALSE)




    # Création de PickerInput dynamique, selon le nombre de variables de sous-population
    observeEvent(input$target_upload, {

      output$selection_variables <- renderUI({
        data()
        tagList(
          fluidRow(
            column(12, selectizeInput('var_souspop',
                                      label=paste0("Sélection des variables de sous-populations"),
                                      # Choix parmis les noms de variables de data
                                      choices=c("",nomcol_data),
                                      # Plusieurs options :
                                      options = list(`actions-box` = TRUE, placeholder = 'Quelles variables pour sous-population ?'),
                                      multiple = TRUE # Si TRUE alors on peut choisir plusieurs variables.
            )))
        )})


    }) # Fin observeEvent


    # Montrer la boite de sélection des modalités (dans UI)
    output$afficher_choix_moda <- reactive({
      if (length(input$var_souspop) >= 1) { "Oui" }
    })
    outputOptions(output, "afficher_choix_moda", suspendWhenHidden=FALSE)


    # Note pour plus tard : On peut change la condition pour que la partie selection apparaisse quand toutes
    # les boites de selection de variables sont remplies :
    # ex : if (input$territoirevar != "" & input$VAR_NIV_VIOLENCE != "")



    # SUITE DU SERVER ----

    # Pour la suite, on continue le server dans des scripts différents pour chacune des onglets.

    # 1) Tableaux :

    # Quand l'utilisateur appuye sur le bouton Valider du panneau latéral :
    observeEvent(input$target_upload, {
      output$table <- DT::renderDataTable({ # Affichage de la table
        filter_data <- data()
        filter_data <<- as.data.frame(lapply(filter_data, as.character)) # On transforme en factor pour l'ACM
        filter_data <<- as.data.frame(lapply(filter_data, as.factor))

        # Options d'affichages
        datatable(filter_data,
                  options = list(lengthMenu = c(5,10,30,50),
                                 pageLength = 5,
                                 orderClasses = TRUE))

      }) # Fin renderDataTable

      # Note pour plus tard : On peut limiter le nombre de colones qui apparaissent
      # Pour ça il faut voir le script "table_choix_col.R"

    })



    # Quand l'utilisateur appuye sur le bouton Valider du panneau latéral :
    observeEvent(input$var_souspop, {


      #### SOUS-POPULATION : choix des variables ----

      # Je crée un vecteur vide, qui contiendra le nom des variables qui servent à la sous-population
      vect_var <<- input$var_souspop

      #### SOUS-POPULATION : choix des modalités ----

      # Je crée des boites qui renvoie une liste des modalités pour chaque variables séléctionnées,
      # l'utilisateur peut donc choisir celles qui l'intéresse
      output$selection_modalites <- renderUI({
        tagList(
          if(length(input$var_souspop) >= 1) { # si une variable sélectionné
            lapply((1:length(input$var_souspop)), function(i){
              pickerInput(inputId= input$var_souspop[i], # On donne comme signal le nom de la variable dans vect_var
                          label=paste0("Choix des filtres pour '",input$var_souspop[i], "'"),
                          # choices=levels(data()[,which(nomcol_data == vect_var[i])]),
                          choices=levels(as.factor(as.character(data()[,which(nomcol_data == input$var_souspop[i])]))),
                          options = list(`actions-box` = TRUE, `live-search` = TRUE, title = "Pas de filtre"),
                          multiple = TRUE)
            })
          }
        )}) # Fin renderUI




      #### CREATION DE LA BASE ----

      # Je crée la table avec seulement les individus d'intérêts
      output$table <- DT::renderDataTable({ # Affichage de la table
        data_conditionnel <- data()

        # S'il y a une variable de selection, on ne garde que les modalités sélectionnées
        if(length(input$var_souspop) >= 1) {
          for (i in c(1:length(input$var_souspop))) {
            if (is.null(input[[input$var_souspop[i]]]) == FALSE) {
              data_conditionnel <- data_conditionnel[data_conditionnel[[input$var_souspop[i]]] %in% input[[input$var_souspop[i]]],]
            }
          }
        }

        # S'il n'y a pas de sélection, on ne fait rien
        if (is.null(input$var_souspop)== T) { data_conditionnel <- data_conditionnel }


        # Creation de la base de données filtrée que l'on va pouvoir exporter par la suite
        filter_data <<- data_conditionnel # la double flèche permet de sauvegarder la base dans R en arrière-fond
        filter_data <<- as.data.frame(lapply(filter_data, as.character)) # On transforme en factor pour l'ACM
        filter_data <<- as.data.frame(lapply(filter_data, as.factor))

        # Options d'affichages
        datatable(filter_data,
                  options = list(lengthMenu = c(5,10,30,50),
                                 pageLength = 5,
                                 orderClasses = TRUE))

      }) # Fin renderDataTable

      # Note pour plus tard : On peut limiter le nombre de colones qui apparaissent
      # Pour ça il faut voir le script "table_choix_col.R"

    })



    #### BOUTONS TELECHARGEMENT DES DONNEES ----

    # On crée le bouton pour le téléchargement des données.

    output$downLoadFilter <- downloadHandler(
      filename = function() {
        paste('Filtered_data-', Sys.Date(), '.csv', sep = '')
      },
      content = function(file){
        write_csv(filter_data,file)
      }
    )


    # 2) ACM

    ### TEXTE INTRODUCTION ----

    # Une ligne de texte pour indiquer combien d'individus dans l'échantillon.
    observeEvent(input$table_rows_all ,{
      output$recap_activ <- renderPrint({
        cat("La taille de la population sélectionnée est de", length(isolate(input$table_rows_all)) ,"individus. \n")
      })

      output$affichage_choix_table_ACM <- renderUI({

        # Choix du type de table
        list_graph_ACM <- c("Effectifs univariés" = "eff_uni_ACM", "Effectif bivariés" = "eff_ACM", "% Ligne" = "pct_lign_ACM", "% Colonne" = "pct_col_ACM")

        # On crée un bandeau avec les options graphiques possibles
        tagList(
          column(12,
                 wellPanel(
                   fluidRow(
                     # Choix de la variable à croiser avec "cluster"
                     column(4,pickerInput("var_table1", "Variable 1 :", c("",nomcol_data),
                                          multiple = F,
                                          options = list(`actions-box` = TRUE, `live-search` = TRUE, title ="Variable en ligne"))),
                     # Choix de la variable à croiser avec "cluster"
                     column(4,pickerInput("var_table2", "Variable 2 :", c("",nomcol_data),
                                          multiple = F,
                                          options = list(`actions-box` = TRUE, `live-search` = TRUE, title ="Variable en colonne"))),
                     # Choix du type de tables
                     column(4, selectInput(inputId="choix_table_ACM",
                                           label="Choix du type de table : ",
                                           choices= list_graph_ACM))
                   ))))
      })



      # Effectifs univarié
      output$eff_uni_ACM <- renderTable({
        validate(need(input$var_table1,'Choisir une 1ère variable'))
        as.data.frame.matrix(with(filter_data, freq(get(input$var_table1))))
      }, include.rownames = T)


      # Effectifs
      output$eff_ACM <- renderTable({
        validate(need(input$var_table1,'Choisir une 1ère variable'),
                 need(input$var_table2,'Choisir une 2ème variable'))
        as.data.frame.matrix(with(filter_data, addmargins(table(get(input$var_table1), get(input$var_table2), useNA = "always"))))
      }, include.rownames = T)


      # Pourcentage Ligne
      output$pct_lign_ACM <- renderTable({
        validate(need(input$var_table1,'Choisir une 1ère variable'),
                 need(input$var_table2,'Choisir une 2ème variable'))
        as.data.frame.matrix(with(filter_data, lprop(table(get(input$var_table1), get(input$var_table2)))))
      }, include.rownames = T)

      # Pourcentage Colonne
      output$pct_col_ACM <- renderTable({
        validate(need(input$var_table1,'Choisir une 1ère variable'),
                 need(input$var_table2,'Choisir une 2ème variable'))
        as.data.frame.matrix(with(filter_data, cprop(table(get(input$var_table1), get(input$var_table2)))))
      }, include.rownames = T)

    })


    observeEvent(input$var_table1, {
      output$affichage_table_ACM <- renderUI({
        tableOutput(input$choix_table_ACM)
      })
    })





    ### SELECTION DES VARIABLES ACTIVES ET SUPPLEMENTAIRES ----


    # Selection des variables actives
    output$acm_main_act <- renderUI({
      data()
      tagList(
        fluidRow(
          column(7, selectizeInput('var_act',
                                   label=paste0("Choix des variables actives"),
                                   # Choix parmis les noms de variables de data
                                   choices=c("",nomcol_data),
                                   # Plusieurs options :
                                   options = list(`actions-box` = TRUE, placeholder = 'Pas de variables actives'),
                                   multiple = TRUE, # Si TRUE alors on peut choisir plusieurs variables.
                                   width = 450)))
      )})



    # Selection des variables supplémentaires

    # La sélection ne se fait que quand des variables ont ête prises en actives
    # On enlève les variables choisies en active des choix possibles en supplémentaire.

    observeEvent(input$var_act,{
      if(length(input$var_act) ==""){

        output$acm_main_sup <- renderUI({
          data()
          tagList(
            fluidRow(
              column(7,
                     selectizeInput('var_sup',
                                    label=paste0("Choix des variables supplémentaires"),
                                    choices=c("",nomcol_data),
                                    options = list(`actions-box` = TRUE, placeholder = 'Pas de variables supplémentaires'),
                                    multiple = TRUE,
                                    width = 450))))})

      }else{
        # Si des variables ont été choisies en actives :
        output$acm_main_sup <- renderUI({
          data()
          tagList(
            fluidRow(
              column(7,
                     selectizeInput('var_sup',
                                    label=paste0("Choix des variables supplémentaires"),
                                    choices = c("",nomcol_data[!(nomcol_data %in% input$var_act)]), # on enlève les actives
                                    options = list(`actions-box` = TRUE, placeholder = 'Pas de variables supplémentaires'),
                                    multiple = TRUE,
                                    width = 450))))})
      }
    })






    ########### CLIQUER SUR LE BOUTON ACM ----


    observeEvent(input$acmOK, {


      showModal(modalDialog("Le chargement de l'ACM peut prendre quelques secondes", footer=NULL))
      #showNotification("Le chargement de l'ACM peut prendre quelques secondes", duration = 15, type = "warning")


      ### CREATION DE L'OBJET FACTOMINER ----

      # On ne continue que si on a des variables actives
      validate(need(input$var_act,''))

      # Copie des variables utilisées (on pourra enlever à l'avenir)
      var_sup2 <<- input$var_sup
      var_act2 <<- input$var_act

      # On ne garde que les variables choisies en actives et supplementaires
      data_acm <<- filter_data %>%
        select(all_of(input$var_sup), all_of(input$var_act))


      # On fait l'ACM :
      if(is.null(var_sup2) == FALSE){
        # Si on a des variables supplementaires :
        # Comme on les a placé en 1er, on a juste à indiquer leurs numéros dans quali.sup
        res_mca <<- MCA(data_acm, graph = FALSE , ncp = Inf, quali.sup = c(1:length(input$var_sup)))
      } else {
        # Sans variables supplementaires
        res_mca <<- MCA(data_acm, graph = FALSE, ncp = Inf)
      }
      ############################





      ############################
      ### CREATIONS D'OBJETS POUR TABLES ET GRAPHIQUES ----

      # Pour les sauts d'inertie : ----
      variances <<- as.data.frame(round(res_mca$eig,2)) %>%
        rownames_to_column() %>%                                  # récupérer les noms de lignes (dim 1, dim 2, etc) dans une colonne distincte
        slice(1:10) %>%                                           # conserver que les infos des 10 premiers axes
        mutate(Axes = str_replace_all(rowname, "dim", "Axe")) %>% # créer une nouvelle variable à partir de rowname, qui prend les valeurs "Axe 1, Axe 2, etc" au lieu de "dim 1, dim 2, etc."
        select(-rowname) %>%                                      # on enlève cette colonne dont on n'a plus besoin
        rename(`Valeurs propres` = eigenvalue) %>%
        rename(`% de variance` = `percentage of variance`) %>%    # on renomme les autres colonnes
        rename(`% cumulé de variance` = `cumulative percentage of variance`) %>%
        mutate(Axes = fct_relevel(Axes, paste("Axe", 1:10)))      # pour que l'ordre de 1 à 10 soit bien respecté dans les graphiques


      # Pour les différents indicateurs de l'ACM : ----
      frequences <- gather(data_acm, variables, modalites) %>%       # étendre le jeu de données par variable et modalité
        # compter le nombre de couples "variable/modalité" unique (donc le nombre d'individus par modalité du jeu de données)
        count(variables, modalites) %>%
        group_by(variables) %>%
        mutate(pourcentage = round(100 * n / nrow(data_acm), 1)) %>% # calculer des pourcentages pour chaque groupe de variable
        ungroup() %>%
        select(variables, modalites, n, pourcentage) %>%
        rename(moda = modalites) %>%
        # On ajoute le nom de la variable à l'avant pour les cas de variables avec les mêmes noms de modalités
        mutate(modalites = ifelse(is.na(moda)== F, paste0(variables,"_",moda), paste0(variables,".",moda )))

      ###############
      # Modalités actives ----

      # Coordonnées (modalités actives)

      coordonnees <- as.data.frame(round(res_mca$var$coord, 2)) %>% # récupérer les coordonnées des modalités actives et arrondir à deux décimales (c'est bien suffisant)
        rename_all(tolower) %>%                                     # tout en minuscules
        rename_all(~ str_replace(., " ", "")) %>%                   # renommer les variables en supprimant les espaces : par exemple : "dim 1" devient "dim1"
        rename_all(~ str_c(., "coord", sep = "_")) %>%              # ajouter le suffixe _coord à chaque nom de variable. On obtient ainsi par exemple "dim1_coord"
        mutate(modalites = rownames(.))                             # récupérer les noms des modalités, stockées dans le nom des lignes de res_mca$var$coord

      # Contributions (modalités actives)
      contributions <- as.data.frame(round(res_mca$var$contrib, 2))  %>%
        rename_all(tolower) %>%
        rename_all(~ str_replace(., " ", "")) %>%
        rename_all(~ str_c(., "contrib", sep = "_")) %>%            # idem sauf qu'ici on obtient "dim1_contrib"
        mutate(modalites = rownames(.))

      # Cosinus carrés (modalités actives)
      cos2 <- as.data.frame(round(res_mca$var$cos2, 2)) %>%
        rename_all(tolower) %>%
        rename_all(~ str_replace(., " ", "")) %>%
        rename_all(~ str_c(., "cos2", sep = "_")) %>% # idem avec "cos2"
        mutate(modalites = rownames(.))

      # vtest (modalités actives)
      vtest <- as.data.frame(round(res_mca$var$v.test, 2)) %>%
        rename_all(tolower) %>%
        rename_all(~ str_replace(., " ", "")) %>%
        rename_all(~ str_c(., "vtest", sep = "_")) %>% # idem avec vtest
        mutate(modalites = rownames(.))


      # Compilation des indicateurs

      resultats_act <- frequences %>%
        right_join(coordonnees) %>%
        right_join(contributions) %>%
        right_join(cos2) %>%
        right_join(vtest) %>%                        # fusionner les jeux de données ; la clé de fusion (implicite) est la variable "modalites", qui est commune à tous.
        mutate(type = "Variable active") %>%         # ajout d'une colonne contenant la chaîne de caractères "Variable active" (pour pouvoir distinguer plus tard avec les variables supplémentaires)
        select(type, variables, modalites, n, pourcentage,
               contains("dim1_"), contains("dim2_"),
               contains("dim3_"), contains("dim4_")) # conserver et réorganiser les variables pertinentes axe par axe, on garde que 4 axes

      # Problème des variables avec les mêmes modalités
      meme_moda <- resultats_act %>%
        filter(is.na(variables) == T) %>%
        select(- c(variables, n, pourcentage)) %>%
        left_join(frequences, by = c("modalites"  = "moda")) %>%
        mutate(modalites = paste0(variables,"_",modalites)) %>% # On ajoute le nom de la variable devant la modalité pour éviter les noms doublons
        select( -modalites.y)

      # On enlève les modalités en doubles et on ajoute leurs corrections
      resultats_actives <<- resultats_act %>%
        filter(is.na(variables) == F) %>%
        rbind(meme_moda)


      ################
      # Modalités supplémentaires ----

      # Si on a des variables supplementaires :
      if(is.null(var_sup2) == FALSE){

        # Coordonnées (modalités supplémentaires)
        coordonnees_sup <- as.data.frame(round(res_mca$quali.sup$coord, 2)) %>% # la démarche est la même que supra, mais avec le sous-objet quali.sup qui stocke les informations sur les variables qualitatives supplémentaires
          rename_all(tolower) %>%
          rename_all(~ str_replace(., " ", "")) %>%
          rename_all(~ str_c(., "coord", sep = "_")) %>%
          mutate(modalites = rownames(.))

        # Cosinus carrés (modalités supplémentaires)
        cos2_sup <- as.data.frame(round(res_mca$quali.sup$cos2, 2)) %>%
          rename_all(tolower) %>%
          rename_all(~ str_replace(., " ", "")) %>%
          rename_all(~ str_c(., "cos2", sep = "_")) %>%
          mutate(modalites = rownames(.))

        # vtest (modalités supplémentaires)
        vtest_sup <- as.data.frame(round(res_mca$quali.sup$v.test, 2)) %>%
          rename_all(tolower) %>%
          rename_all(~ str_replace(., " ", "")) %>%
          rename_all(~ str_c(., "vtest", sep = "_")) %>%
          mutate(modalites = rownames(.))

        # Assemblage du tableau des résultats (modalités actives)

        resultats_sup <- frequences %>%
          right_join(coordonnees_sup) %>%
          right_join(cos2_sup) %>%
          right_join(vtest_sup) %>%
          mutate(type = "Variable supplémentaire") %>% # comme supra pour le tableau des résultats des modalités actives : on distingue ici le type de variable.
          select(type, variables, modalites, n, pourcentage,
                 contains("dim1_"), contains("dim2_"),
                 contains("dim3_"), contains("dim4_"))

        # Même problème de doublons
        meme_moda_sup <- resultats_sup %>%
          filter(is.na(variables) == T) %>%
          select(- c(variables, n, pourcentage)) %>%
          left_join(frequences, by = c("modalites"  = "moda")) %>%
          mutate(modalites = paste0(variables,"_",modalites)) %>%
          select( -modalites.y)

        resultats_suplem <<- resultats_sup %>%
          filter(is.na(variables) == F) %>%
          rbind(meme_moda_sup)

      } # Fin Var Supp


      ###############
      # TABLEAU COMPLET  ----

      # Si on a des var supplémentaires
      if(is.null(var_sup2) == FALSE){
        resultats_complet <<- bind_rows(resultats_actives, resultats_suplem)
      }

      # Si on a que des variables actives
      if(is.null(var_sup2) == TRUE){
        resultats_complet <<- resultats_actives
      }

      ############################




      ############################
      ### FAIRE LES GRAPHIQUES ET LES TABLES ----

      # Table de saut d'inertie
      output$variances <- DT::renderDataTable({
        datatable(variances %>% select(Axes, everything()))
      })

      # Graphique de saut d'inertie
      output$variances_graph <- renderPlot({
        ggplot(variances, aes(x = Axes)) +     # initialiser du graphique et de l'axe horizontal
          geom_bar(aes(y = `% de variance`),   # indiquer le type de graphique (barplot) et la variable à représenter sur l'axe vertical
                   stat = "identity",
                   fill = "lightgrey") +       # parce que j'aime bien le rouge
          xlab("") +                           # on enlève le label de l'axe des x, pas très utile
          ylab("% de variance") +              # renommer proprement le label de l'axe des y
          theme_minimal()                      # un des thèmes possibles dans ggplot, que j'aime bien car il est... minimaliste !
      })


      # Table de saut d'inertie, corrigé selon le critère de Benzécri
      variances_modif <<- round(modif.rate(res_mca)$modif,2)

      output$variances_Benz <- DT::renderDataTable({
        datatable(variances_modif)
      })


      # Graphique de saut d'inertie, corrigé selon le critère de Benzécri
      output$variances_Benz_graph <- renderPlot({
        ggplot(variances_modif, aes(x = reorder(row.names(variances_modif), -mrate))) + # initialiser du graphique et de l'axe horizontal
          geom_bar(aes(y = modif.rate(res_mca)$modif$mrate),                            # indiquer le type de graphique (barplot) et la variable à représenter sur l'axe vertical
                   stat = "identity",
                   fill = "red") +                                                      # choix couleur
          xlab("") +                                                                    # on enlève le label de l'axe des x, pas très utile
          ylab("% de variance") +                                                       # renommer proprement le label de l'axe des y
          theme_minimal()                                                               # un des thèmes possibles dans ggplot, que j'aime bien car il est... minimaliste !
      })


      # Graphique des variables actives

      output$plot_acm_act <- renderPlot({

        resultats_actives %>%
          mutate(modalites = substr(modalites, nchar(variables)+2, nchar(modalites))) %>%
          filter(get(paste0("dim",input$axe_X,"_contrib")) > input$seuil |
                   get(paste0("dim",input$axe_Y,"_contrib")) > input$seuil) %>%   # on part du tableau de résultat des modalités actives et on filtre uniquement celles dont la contribution dépasse le seuil pour l'un ou l'autre des deux axes (| est l'opérateur logique OU).

          ggplot(aes(x = get(paste0("dim",input$axe_X,"_coord")), y = get(paste0("dim",input$axe_Y,"_coord")), # initialisation du graphique
                     label = modalites,                                           # les labels des points sont les modalités
                     shape = variables,                                           # les formes des points dépendent des variables : à chaque variable son symbole
                     colour = variables,
                     size = 15)) +

          geom_point() +                                                          # tracer les points
          geom_text_repel(size = input$taille_label, segment.alpha = 0.5) +       # tracer les labels, en utilisant cette fonction du package ggrepel qui permet de s'assurer qu'il n'y a pas de chevauchement. segment.alpha : transparence du petit tiret qui relie si besoin le libellé au point
          coord_fixed() +                                                         # pour que les échelles des axes soient identiques

          geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") +  # ligne horizontale y = 0
          geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") +  # ligne verticale x = 0

          xlab(paste0("Axe", input$axe_X ,"(", round(variances[input$axe_X, 2], 1), " %)")) + # label de l'axe horizontal, qui intègre automatiquement le pourcentage de variance
          ylab(paste0("Axe", input$axe_Y ,"(", round(variances[input$axe_Y, 2], 1), " %)")) + # idem pour l'axe vertical

          scale_shape_manual(name="", values = 0:20) +                            # sélection des types de symboles

          guides(colour=guide_legend(title="", nrow = 2),                         # paramètres de la légende : pas de titre
                 size = "none",                                                   # Pas de legende pour la taille et la forme
                 shape = "none") +

          theme_minimal(base_size = 18) +                                         # mise en forme globale du graphique ; theme_minimal est la plus "sobre" mais d'autres sont possibles...

          theme(legend.position="bottom", legend.text = element_text(size = 20))  # pour que la légende se place sous le graphique.



      })


      # Graphique des variables actives et supplémentaires

      output$plot_acm_complet <- renderPlot({

        resultats_complet %>%
          mutate(modalites = substr(modalites, nchar(variables)+2, nchar(modalites))) %>%
          filter(get(paste0("dim",input$axe_X,"_contrib")) > input$seuil |
                   get(paste0("dim",input$axe_Y,"_contrib")) > input$seuil |
                   is.na(get(paste0("dim",input$axe_X,"_contrib"))) == T |
                   is.na(get(paste0("dim",input$axe_Y,"_contrib"))) == T) %>%

          ggplot(aes(x = get(paste0("dim",input$axe_X,"_coord")), y = get(paste0("dim",input$axe_Y,"_coord")),
                     label = modalites,
                     shape = variables,
                     colour = type,                                               # on distingue par des couleurs différentes les variables actives et supplémentaires
                     size = 15)) +

          geom_point() +
          geom_text_repel(size = input$taille_label, segment.alpha = 0.5) +
          coord_fixed() +

          geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") +
          geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") +

          xlab(paste0("Axe", input$axe_X ,"(", round(variances[input$axe_X, 2], 1), " %)")) +
          ylab(paste0("Axe", input$axe_Y ,"(", round(variances[input$axe_Y, 2], 1), " %)")) +

          scale_shape_manual(name="", values = 0:20) +
          scale_color_manual(values = c("black", "red")) +
          # scale_color_brewer(palette = "Set1") +
          # scale_color_grey() +
          # scale_color_brewer(palette = "Accent")

          guides(shape = "none",
                 colour = guide_legend(title= "Type de variable",                 # titre de la légende distinguant actives et supplémentaires
                                       title.position = "top",
                                       nrow = 2),
                 size = "none") + # toujours pas de légende pour les tailles de point

          theme_minimal(base_size = 18) +
          theme(legend.position="bottom", legend.text = element_text(size = 20))
      })


      output$plot_acm_interact <- renderGirafe({

        gg_point = resultats_complet %>%
          mutate(modalites = substr(modalites, nchar(variables)+2, nchar(modalites)),
                 tooltip = c(paste0("Modalité = ", modalites,
                                    "\n Variables = ", variables,
                                    "\n Contrib1 = ", dim1_contrib,
                                    "\n Contrib2 = ", dim2_contrib,
                                    "\n Type = ", type))) %>%
          filter(get(paste0("dim",input$axe_X,"_contrib")) > input$seuil |
                   get(paste0("dim",input$axe_Y,"_contrib")) > input$seuil |
                   is.na(get(paste0("dim",input$axe_X,"_contrib"))) == T |
                   is.na(get(paste0("dim",input$axe_Y,"_contrib"))) == T) %>%
          ggplot() +
          geom_point_interactive(aes(x = get(paste0("dim",input$axe_X,"_coord")),
                                     y = get(paste0("dim",input$axe_Y,"_coord")),
                                     color = variables,
                                     shape = variables,
                                     tooltip = tooltip,
                                     data_id = modalites)) +
          coord_fixed() +
          xlab(paste0("Axe", input$axe_X ,"(", round(variances[input$axe_X, 2], 1), " %)")) + # label de l'axe horizontal, qui intègre automatiquement le pourcentage de variance
          ylab(paste0("Axe", input$axe_Y ,"(", round(variances[input$axe_Y, 2], 1), " %)")) + # idem pour l'axe vertical
          theme_minimal()
        # + theme(legend.position="bottom", legend.text = element_text(size = 12))

        girafe(ggobj = gg_point)



      })



      # Nuage des individus

      coord_indiv <<- as.data.frame(res_mca$ind$coord) # récupérer les coordonnées des individus sur les deux premiers axes.

      output$plot_acm_ind <- renderPlot({

        ggplot(coord_indiv, aes(x = get(paste0("Dim ", input$axe_X)), y = get(paste0("Dim ", input$axe_Y)))) +  # initialisation du graphique
          geom_point(alpha = 0.6,          # alpha : permet une certaine transparence des points, pour mieux voir les superpositions. On pourrait aussi changer la taille des points avec l'argument size = 1 (par exemple).
                     colour = "#E41A1C") + # couleur hexadécimale tout à fait arbitraire... les goûts et les couleurs...
          coord_fixed() +

          geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") +
          geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") +

          xlab(paste0("Axe 1 (", round(variances[input$axe_X, 2], 1), " %)")) +
          ylab(paste0("Axe 2 (", round(variances[input$axe_Y, 2], 1), " %)")) +

          guides(colour = "none") +        # pas de légende pour le paramètre de couluer

          theme_minimal(base_size = 18)
      })




      # Tables des indicateurs

      output$resultat_acm_table <- DT::renderDataTable({
        datatable(resultats_complet %>%
                    select(type, variables, modalites, n, pourcentage,
                           contains(paste0("dim",input$axe_X,"_")), contains(paste0("dim",input$axe_Y,"_")) ))
      })


      ############################




      ############################
      ### UI : CHOIX GRAPHIQUES & PARAMETRES


      output$affichage_choix_ACM <- renderUI({

        # On crée une liste pour les différents objets qu'on va représenter
        list_acm <- list("Saut d'inertie" = "variances" ,
                         "Saut d'inertie (Benzecri)" = "variances_Benz",
                         "ACM : variables actives" = "plot_acm_act",
                         "ACM : variables actives et supplémentatires" = "plot_acm_complet",
                         "ACM : graphique interactif" = "plot_acm_interact",
                         "ACM : individus" = "plot_acm_ind",
                         "Tables des indicateurs" = "resultat_acm_table")

        # On crée un bandeau avec les choix possibles



        tagList(
          h5("Exploration de l'ACM"),
          column(12,
                 wellPanel(
                   # 1ere ligne dans le bandeau
                   fluidRow(
                     # Choix de ce qu'on montre
                     column(5, selectInput(inputId="choix_MCA",
                                           label="Choix de la représentation : ",
                                           choices= list_acm,
                                           selected = list_acm[1])),
                     # Choix de l'axe X
                     column(2, offset=1, numericInput("axe_X", "Axe X :", 1, min = 1, max = 4)),
                     # Choix de l'axe Y
                     column(2, numericInput("axe_Y", "Axe Y :", 2, min = 1, max = 4)),
                     # Choix du seuil de contribution pour la représentation
                     column(2, numericInput("seuil", "Seuil contrib.:", 100 / nrow(res_mca$var$coord) , min = 0, max = 10))),

                   # 2ème ligne dans le bandeau
                   fluidRow(
                     column(7,
                            fluidRow(helpText("Attention : Cliquer sur 'ACM avec explor' ferme l'application en cours et active la fonction explor().")),
                            fluidRow(column(4, offset=3, actionButton("explorACM", "ACM avec explor", class = "btn-danger")))),
                     column(2, offset = 1,
                            # Taille du graphique
                            sliderInput(inputId = "taille_graph",
                                        label = "Taille du graphique",
                                        min = 300, max = 3000, step = 100, value = 800)),
                     column(2,
                            # Taille des étiquette
                            sliderInput(inputId = "taille_label",
                                        label = "Taille des étiquettes",
                                        min = 2, max = 15, step = 1, value = 7)))

                 )))
      }) # Fin renderUI



    }) # Fin Bouton ACM

    removeModal()

    ############################




    ############################
    ### UI : REPRESENTATION GRAPHIQUES ET TABLES


    # Il faut choisir dans le bandeau le type de représentation
    observeEvent(input$choix_MCA, {

      # Pour saut d'inertie
      if (input$choix_MCA == "variances"){

        output$affichage_ACM <- renderUI({
          fluidRow(
            column(7,DT::dataTableOutput(input$choix_MCA)),
            column(4, offset = 1,plotOutput("variances_graph"))
          )
        })
      }
      # Pour saut d'inertie Benzécri
      else if (input$choix_MCA == "variances_Benz"){
        output$affichage_ACM <- renderUI({
          fluidRow(
            column(7,DT::dataTableOutput(input$choix_MCA)),
            column(4, offset = 1,plotOutput("variances_Benz_graph"))
          )
        })
      }
      # Pour graphique ACM sans variables supplémentaires
      else if (input$choix_MCA == "plot_acm_complet" & is.null(var_sup2) == T ){
        output$affichage_ACM <- renderUI({
          fluidRow(
            column(12, plotOutput("plot_acm_act", height = input$taille_graph, width = input$taille_graph) )
          )
        })
      }
      # Pour la table des informations sur l'ACM
      else if (input$choix_MCA == "resultat_acm_table" ){
        output$affichage_ACM <- renderUI({
          fluidRow(
            column(12, DT::dataTableOutput(input$choix_MCA))
          )
        })
      }

      # Pour graphique ACM interactif
      else if (input$choix_MCA == "plot_acm_interact"){
        output$affichage_ACM <- renderUI({
          fluidRow(
            column(12, girafeOutput("plot_acm_interact", height = input$taille_graph, width = input$taille_graph))
          )
        })
      }


      # Pour toutes les autres graphiques
      else{
        output$affichage_ACM <- renderUI({
          plotOutput(input$choix_MCA, height = input$taille_graph, width = input$taille_graph)
        })
      }


    })



    # 3) CAH

    # Quand on appuye sur le bouton Valider l'ACM dans ServeurACM, on crée aussi des
    # objets qu'on réutilise dans la partie CAH.



    observeEvent(input$acmOK, {

      showModal(modalDialog("Le chargement de l'ACM peut prendre quelques secondes", footer=NULL))


      ### CREER LES OBJETS POUR LA CLASSIFICATION

      # Plus facile de faire des classifications avec le package ade4, mais factominer est utile pour explor()

      if(is.null(var_sup2) == FALSE){
        ade4_mca <<- dudi.acm(data_acm[c((length(var_sup2)+1) : length(data_acm))], # On ne garde que les variables actives
                              scannf = FALSE, nf = Inf)
      } else {
        ade4_mca <<- dudi.acm(data_acm, scannf = FALSE, nf = Inf)
      }

      # Matrice des distances
      md <- dist.dudi(ade4_mca)
      # Dendrogramme et saut d'inertie
      arbre <<- hclust(md, method = "ward.D2")
      inertie <<- sort(arbre$height, decreasing = TRUE)



      ### CREER LES UI POUR LES GRAPHIQUES ET LES TABLES -----

      # Dendrogrammes et saut d'inertie :

      output$affichage_choix_graph <- renderUI({

        # On crée une liste des graphiques possibles
        list_graph <- c("Dendrogramme" = "dendro", "Saut d'inertie" = "inertie")
        # On crée un bandeau avec les options graphiques possibles
        tagList(
          column(12,
                 wellPanel(
                   fluidRow(
                     # Selection du graphique
                     column(5,selectInput(inputId="choix_graph",
                                          label="Choix du graphique : ",
                                          choices= list_graph)),
                     # Choix du nombre de cluster
                     column(4, numericInput("ncluster", "Nombre de 'clusters':", 2, min = 1, max = 20)),
                     # Bouton pour rafraichir le graphique si on change l'ACM
                     column(2, offset = 1, actionBttn(
                       inputId = "resetCAH",
                       label = "Rafraichir Graphique",
                       color = "primary",
                       style = "simple",
                       size = "xs"
                     ))
                   ))))
      })


      # Tableaux bi-variés :

      output$affichage_choix_table <- renderUI({

        # Choix du type de table
        list_graph <- c("Effectifs Cluster" = "eff_uni","Effectifs bivariés" = "eff", "% Ligne" = "pct_lign", "% Colonne" = "pct_col", "Sur/Sous-représentation" = "heatmap")
        # On crée un bandeau avec les options graphiques possibles
        tagList(
          column(12,
                 wellPanel(
                   fluidRow(
                     # Choix de la variable à croiser avec "cluster"
                     column(5,pickerInput("var_row", "Croiser la variable 'cluster' avec :", c("",nomcol_data),
                                          multiple = F,
                                          options = list(`actions-box` = TRUE, `live-search` = TRUE))),
                     # Choix du type de tables
                     column(4, selectInput(inputId="choix_table",
                                           label="Choix du type de table : ",
                                           choices= list_graph))
                   ))))
      })



      removeModal()

    })





    ### CREER LES GRAPHIQUES ----

    observeEvent(input$ncluster ,{
      validate(need(input$ncluster,'Choisir un nombre de cluster'))
      # On sauvegarde le nombre de cluster
      ncluster <<- input$ncluster

      # Graphiques
      if(input$ncluster < 2){ # Si moins de deux clusters, on affiche juste les graphiques normaux sans les rectangles
        # Dendrogramme
        output$dendro <- renderPlot({
          plot(arbre, labels = FALSE, main = "Choix de la partition",
               xlab = "", ylab = "", sub = "",
               axes = FALSE, hang = -1)
        })
        # Saut d'inertie
        output$inertie <- renderPlot({
          plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")
        })

      } else { # Si on a des clusters, alors on les affiches sur les graphiques
        # Dendrogramme
        output$dendro <- renderPlot({
          plot(arbre, labels = FALSE, main = "Choix de la partition",
               xlab = "", ylab = "", sub = "",
               axes = FALSE, hang = -1)
          rect.hclust(arbre, input$ncluster, border = "red")

        })
        # Saut d'inertie
        output$inertie <- renderPlot({
          plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")
          points(input$ncluster, inertie[input$ncluster], col = "red", cex = 2, lwd = 3)
        })
      }

    })


    # Quand on appuye sur reset CAH, on relance juste le code, c'est comme si on changeait le nombre de cluster
    # mais j'avais peur que certaines personnes ne le fasse pas.

    observeEvent(input$resetCAH, {
      validate(need(input$ncluster,''))
      if(input$ncluster < 2){
        output$dendro <- renderPlot({
          plot(arbre, labels = FALSE, main = "Choix de la partition",
               xlab = "", ylab = "", sub = "",
               axes = FALSE, hang = -1)
        })
        output$inertie <- renderPlot({
          plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")
        })
      } else {
        output$dendro <- renderPlot({
          plot(arbre, labels = FALSE, main = "Choix de la partition",
               xlab = "", ylab = "", sub = "",
               axes = FALSE, hang = -1)
          rect.hclust(arbre, input$ncluster, border = "red")
        })
        output$inertie <- renderPlot({
          plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")
          points(input$ncluster, inertie[input$ncluster], col = "red", cex = 2, lwd = 3)
        })
      }
    })




    # Affichage des graphiques dans l'UI :
    output$affichage_graphique <- renderUI({
      validate(need(input$ncluster,'Choisir un nombre de cluster'))
      fluidRow(
        column(8, offset = 1,
               plotOutput(input$choix_graph, height = 600)
        )
      )
    })




    ### AJOUT DE LA VARIABLE CLUSTER AUX DONNEES ----

    observeEvent(input$ncluster, {
      validate(need(input$ncluster,'Choisir un nombre de cluster'))
      filter_data$cluster <<- cutree(arbre, input$ncluster)
    })





    ### CREATION DES TABLES BIVARIEES ----

    # S'actualise a chaque fois qu'on change le nombre de cluster
    observeEvent(input$ncluster ,{
      validate(need(input$ncluster,'Choisir un nombre de cluster'))
      # Effectifs univarié
      output$eff_uni <- renderTable({
        as.data.frame.matrix(with(filter_data, freq(cluster)))
      }, include.rownames = T)


      # Effectifs
      output$eff <- renderTable({
        validate(need(input$var_row,'Choisir une variable à croiser avec "cluster"'))
        as.data.frame.matrix(with(filter_data, addmargins(table(get(input$var_row), filter_data$cluster))))
      }, include.rownames = T)

      # Pourcentage Ligne
      output$pct_lign <- renderTable({
        validate(need(input$var_row,'Choisir une variable à croiser avec "cluster"'),
                 need(filter_data$cluster,''))

        as.data.frame.matrix(with(filter_data, lprop(table(get(input$var_row), filter_data$cluster))))
      }, include.rownames = T)

      # Pourcentage Colonne
      output$pct_col <- renderTable({
        validate(need(input$var_row,'Choisir une variable à croiser avec "cluster"'),
                 need(filter_data$cluster,''))

        as.data.frame.matrix(with(filter_data, cprop(table(get(input$var_row), filter_data$cluster))))
      }, include.rownames = T)

      # Sur-representation
      output$heatmap <- renderPlot({

        validate(need(input$var_row,'Choisir une variable à croiser avec "cluster"'))

        chisq <- chisq.test(with(filter_data, table(get(input$var_row), filter_data$cluster)))
        corrplot(chisq$residuals, is.cor = FALSE)

      })


    })



    # Affichage des tables dans l'UI :

    observeEvent(input$choix_table, {
      validate(need(input$ncluster,'Choisir un nombre de cluster'))

      if (input$choix_table == "heatmap"){
        output$affichage_table <- renderUI({
          plotOutput(input$choix_table)
        })
      }
      else {
        output$affichage_table <- renderUI({
          tableOutput(input$choix_table)
        })
      }
    })






    ### TELECHARGER LES DONNES AVEC LA VARIABLE CLUSTER


    observeEvent(input$datatype_sortie, {
      validate(need(input$ncluster,'Choisir un nombre de cluster'))
      # Format CSV
      if(input$datatype_sortie == ".csv"){
        output$downLoadCluster <- downloadHandler(

          filename = function() {
            paste('Filtered_data_cluster-', Sys.Date(), '.csv', sep = '')
          },
          content = function(file){
            write_csv(filter_data,file)
          }
        )}

      # Format SAS
      if(input$datatype_sortie == ".sas7bdat"){
        output$downLoadCluster <- downloadHandler(

          filename = function() {
            paste('Filtered_data_cluster-', Sys.Date(), '.sas7bdat', sep = '')
          },
          content = function(file){
            write_sas(filter_data,file)
          }
        )}

      # Format Stata
      if(input$datatype_sortie == ".dta"){
        output$downLoadCluster <- downloadHandler(

          filename = function() {
            paste('Filtered_data_cluster-', Sys.Date(), '.dta', sep = '')
          },
          content = function(file){
            write_dta(filter_data,file)
          }
        )}

    })




    # PASSERELLE VERS EXPLOR() ----


    # Appuyer sur le bouton explorACM, stoppe l'ACM et renvoie le signal explorACM
    # Ce signal sera reçu dans la fonction VirageACM (script fonction.R), pour ouvrir explor()

    observeEvent(input$explorACM, {
      js$closeWindow(); stopApp("explorACM")
    })






  }




  appOne <- shinyApp(ui,server)
  route <- "appOne"


  while (TRUE) {
    app <- switch(
      route,
      appOne = runApp(appOne),
      explorACM = explor(res_mca)
    )
    if (is.null(app)) {  # not matched
      warning("Unknown route ", route)
      route <- "appOne"
      next
    }

    # Execute a new app and retrieve return code
    route <- print(app)
    message("New route ", route)
    # browser()
  }


}






