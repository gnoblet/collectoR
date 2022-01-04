# Load necessary functions using `box`
box::use(
  readxl[read_excel],
  dplyr[filter, select, pull, all_of],
  tidyr[drop_na, separate],
  shiny[...],
  writexl[write_xlsx],
  impactR[...]
)

# # Define UI for application that draws a histogram
ui <-
  # Application title
  # titlePanel("Suivi de collecte"),


  # Title
  navbarPage(
    title = list(
      div(
        id = "title-container",
        h2(id = "title", "collectoR"),
        h4(id = "sub-title", "Some data collection monitoring and cleaning")
      )
    ),
    theme = "custom.css",
    tabPanel(
      "A propos",
      column(
        2,
        img(src = "hex-collectoR.png", height = "200")
      ),
      column(
        8,
        # But de l'application
        div(
          class = "main-steps-container",
          div(
            class = "main-steps-title-container",
            h3("1.",
              class = "main-steps-title-number"
            ),
            h4("But de l'application",
              class = "main-steps-title"
            )
          ),
          wellPanel(
            class = "main-steps-panel",
            h5("Cette application a pour but de simplifier le suivi (en construction) ou le nettoyage des données en l'automatisant.")
          )
        ),
        # Onglets
        div(
          class = "main-steps-container",
          div(
            class = "main-steps-title-container",
            h3("2.",
              class = "main-steps-title-number"
            ),
            h4("Onglets",
              class = "main-steps-title"
            )
          ),
          wellPanel(
            class = "main-steps-panel",
            h5(tags$ul(tags$li("Pour le 'Nettoyage', il existe plusieurs pré-requis : le remplissage d'un journal de nettoyage respectant un certain format (disponible ", a("ici", href = "https://github.com/gnoblet/collectoR/blob/main/docs/BFA_guide_cleaning_log_V2.xlsx"), "), l'utilisation d'un outil Kobo (feuilles 'survey' et 'choices') avec ajout des choix de réponses recodés le cas échéant et les données brutes avec la colonne d'indentifiant unique de l'entretien renommée en 'uuid'."), tags$li("L'onglet 'Suivi' permet d'obtenir un journal de suivi de collecte, sous le même format que le journal de nettoyage à partir des données brutes, d'une liste de vérifications et de l'outil Kobo. Il permet aussi d'obtenir les réponses 'autres' et les valeurs extrêmes.")))
          )
        ),
        # Contact et remerciements
        div(
          class = "main-steps-container",
          div(
            class = "main-steps-title-container",
            h3("3.",
              class = "main-steps-title-number"
            ),
            h4("Contacts et remerciements",
              class = "main-steps-title"
            )
          ),
          wellPanel(
            class = "main-steps-panel",
            h5(tags$b("Liens"), br(), "Tout ceci est complètement amendable et perfectible, voir :", br(), tags$ul(tags$li("Contacts :", a("gnoblet@zaclys.net", href = "mailto:gnoblet@zaclys.net"), "ou", a("guillaume.noblet@reach-initiative.org", href = "mailto:guillaume.noblet@reach-initiative.org"),  "ou", a("hermann.pehan@reach-initiative.org", href = "mailto:hermann.pehan@reach-initiative.org")), tags$li("Package under the hood :", a("github.com/gnoblet/impactR", href = "https://github.com/gnoblet/impactR")))),
            h5(tags$b("Remerciements"), br(), "Remerciements aux équipes de REACH Burkina Faso qui ont testé deux pilotes des fonctions utilisées pour cette application Shiny. Merci à ",  a("marton-balazs-kovacs", href = "https://rollercoaster.shinyapps.io/tenzing/"),  " pour l'UI. Et, évidemment, remerciements à toute l'équipe du `tidyverse` et de Rstudio, aux inombrables ressources et forums.")
          )
        )
      )
    ),
    tabPanel(
      "Nettoyage",
      column(2, img(src = "hex-collectoR.png", height = "200")),
      column(
        8,
        # First step
        div(
          class = "main-steps-container",
          div(
            class = "main-steps-title-container",
            h3("1.",
              class = "main-steps-title-number"
            ),
            h4("Données brutes",
              class = "main-steps-title"
            ),
            div(
              class = "help-icon-container",
              title = "La colonne identifiant chaque entretien doit s'intituler 'uuid'.",
              icon("far fa-question-circle", lib = "font-awesome", class = "help-icon")
            )
          ),
          wellPanel(
            class = "main-steps-panel",
            h5("Choisir le fichier de données brutes issu de Kobo.", class = "main-steps-desc"),
            fileInput(
              "file_data",
              label = NULL,
              accept = c(
                ".csv",
                ".tsv",
                ".xlsx"
              ),
              buttonLabel = "Parcourir",
              placeholder = "Pas de fichier sélectionné",
              multiple = FALSE
            )
          ),
          dataTableOutput("log")
        ),
        # Second step
        div(
          class = "main-steps-container",
          div(
            class = "main-steps-title-container",
            h3("2.",
              class = "main-steps-title-number"
            ),
            h4("Outil Kobo",
              class = "main-steps-title"
            ),
            div(
              class = "help-icon-container",
              title = "S'assurer que c'est bien l'outil Kobo recodé, par exemple des réponses autres.",
              icon("far fa-question-circle", lib = "font-awesome", class = "help-icon")
            )
          ),
          wellPanel(
            class = "main-steps-panel",
            h5("Choisir le fichier de l'outil Kobo recodé. Il doit contenir les feuilles 'survey' et 'choices'.", class = "main-steps-desc"),
            fileInput(
              "file_kobo",
              label = NULL,
              accept = c(
                ".csv",
                ".tsv",
                ".xlsx"
              ),
              buttonLabel = "Parcourir",
              placeholder = "Pas de fichier sélectionné",
              multiple = FALSE
            )
          )
        ),
        # Third step
        div(
          class = "main-steps-container",
          div(
            class = "main-steps-title-container",
            h3("3.",
              class = "main-steps-title-number"
            ),
            h4("Choisir le tableur de nettoyage",
              class = "main-steps-title"
            ),
            div(
              class = "help-icon-container",
              title = "S'assurer que le tableur suit le modèle présent à [lien]",
              icon("far fa-question-circle", lib = "font-awesome", class = "help-icon")
            )
          ),
          wellPanel(
            class = "main-steps-panel",
            h5("Choisir le fichier de nettoyage. Il doit contenir une feuille 'cleaning_log'. Il peut éventuellement contenir des feuilles 'anon_cols' (pour la suppression des données identifiables),  'del_cols' (pour la suppression de variables) et une feuille 'misnamed_cols (si les variables parents-enfants'", class = "main-steps-desc"),
            fileInput(
              "file_log",
              label = NULL,
              accept = c(
                ".csv",
                ".tsv",
                ".xlsx"
              ),
              buttonLabel = "Parcourir",
              placeholder = "Pas de fichier sélectionné",
              multiple = FALSE
            )
          )
        ),
        div(
          class = "main-steps-container",
          div(
            class = "main-steps-title-container",
            h3("4.",
               class = "main-steps-title-number"
            ),
            h4("Nettoyage",
               class = "main-steps-title"
            )),
          downloadButton("dl_clean_data", 'Nettoyer et télécharger')
        )
      )
    ),
    tabPanel(
      "Suivi",
      column(2, img(src = "hex-collectoR.png", height = "200")),
      column(8, div(h5("EN CONSTRUCTION.")))
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {

  # Reading data ---------------------------
  
  data <- reactive({
    
    req(input$file_data)
    
    impactR::import_xlsx(input$file_data$datapath, sheet = 1)
  })
  
  # Reading kobo tool ---------------------------
  
  survey <- reactive({
  
    req(input$file_kobo)
    
    impactR::import_xlsx(input$file_kobo$datapath, "survey") |>
      impactR::split_survey("type") |>
      tidyr::drop_na(name) |>
      dplyr::filter(
        !(list_name %in% c(
          "begin_group",
          "end_group",
          "begin_repeat",
          "end_repeat")
          )
        )
  })
  
  choices <- reactive({
    
    req(input$file_kobo)
    
    impactR::import_xlsx(input$file_kobo$datapath, "choices")
  })

  # Reading cleaning log ---------------------------
  
  log_sheets <- reactive({ 
    
    req(input$file_log)
    
    readxl::excel_sheets(input$file_log$datapath)
  })
  
  log <- reactive({
    
    req(input$file_log, log_sheets)
    
    load <- impactR::import_xlsx(input$file_log$datapath, "cleaning_log") |> 
      impactR::log_names_fr_en()
    
    validate(check_cleaning_log(load, data()))
    
    load
  })

  anon_cols <- reactive({
    
    req(input$file_log, log_sheets)
    
    if ("anon_cols" %in% log_sheets()) {
      acols <- impactR::import_xlsx(input$file_log$datapath, "anon_cols") |>
        dplyr::pull(.data$anon_cols)
      if (length(acols) == 0) {
        return(NULL)
      } else {
        return(acols)
      }
    } else {
      return(NULL)
    }
  })

  del_cols <- reactive({
    
    req(input$file_log, log_sheets)
    
    if ("del_cols" %in% log_sheets()) {
      dcols <- impactR::import_xlsx(input$file_log$datapath, "del_cols") |>
        dplyr::pull(.data$del_cols)
      if (length(dcols) == 0) {
        return(NULL)
      } else {
        return(dcols)
      }
    } else {
      return(NULL)
    }
  })

  misnamed_cols <- reactive({
    
    req(input$file_log, log_sheets)
    
    if ("misnamed_cols" %in% log_sheets()) {
      mcols <- impactR::import_xlsx(input$file_log$datapath, "misnamed_cols") |>
      dplyr::select(.data$old_names, .data$new_names)
      if (length(mcols) == 0) {
        return(NULL)
      } else {
        return(mcols)
      }
    } else {
      return(NULL)
    }
  })
  
  
  # Cleaning ---------------------------
  
  clean_data <- reactive({
    
    req(log)
    req(data)
    req(misnamed_cols)
    req(del_cols)
    req(anon_cols)

    cdata <- data() 
    
    if (!is.null(misnamed_cols())) cdata <- cdata |>
      impactR::rename_cols(misnamed_cols()$old_names, misnamed_cols()$new_names)
    
    cols_order <- colnames(cdata)

    cdata <- cdata |>
      impactR::modify_from_log(log(), "uuid", "double") |> 
      impactR::modify_from_log(log(), "uuid", "character") |> 
      impactR::recode_other_from_log(log(), "uuid") |> 
      impactR::recode_other_parent_from_log(log(), "uuid") |> 
      impactR::remove_duplicate(log(), "uuid") |> 
      impactR::remove_from_log(log(), "uuid") |> 
      impactR::count_occ_all(survey(), choices(), "uuid") |> 
      dplyr::relocate(dplyr::all_of(cols_order))
    
    if (!is.null(del_cols())) cdata <- cdata |> dplyr::select(-dplyr::all_of(del_cols()))
    
    if (!is.null(anon_cols())) cdata <- cdata |> dplyr::select(-dplyr::all_of(anon_cols()))

    return(cdata)
  })
  
  
  output$dl_clean_data <- downloadHandler(
    filename = function() {
      paste("clean_data_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(clean_data(), file)
    }
  )

}

# Run the application
shinyApp(ui = ui, server = server)
