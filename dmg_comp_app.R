### TO DO ###

# auto hit (e.g. magic missile) for attacks
# force max damage for attacks and saves
# force crits for attacks

source(here::here("_packages_and_environments.R"))
source(here("_variables_and_functions.R"))

### Module ###

actionUI <- function(id, default_type = "attack") {
  ns <- NS(id)
  tagList(
    textInput(ns("name"), label = "Name", placeholder = "e.g. Bow shot"),
    selectInput(
      ns("actType"),
      label    = "Type",
      choices  = c("attack", "save"),
      selected = default_type
    ),
    conditionalPanel(
      "input.actType == 'attack'",
      ns = ns,
      numericInput(ns("nAtk"), label = "Num. attacks", min = 1, max = 4, value = 1)
    ),
    conditionalPanel(
      "input.actType == 'save'",
      ns = ns,
      numericInput(ns("nAtk"), label = "Num. targets", min = 1, max = 10, value = 1)
    ),
    conditionalPanel(
      "input.actType == 'attack'",
      ns = ns,
      selectInput(ns("rollType"), label = "Roll type", choices = c("normal", "advantage", "disadvantage"))
    ),
    conditionalPanel(
      "input.actType == 'save'",
      ns = ns,
      selectInput(ns("rollType"), label = "Target's roll type", choices = c("normal", "advantage", "disadvantage"))
    ),
    conditionalPanel(
      "input.actType == 'attack'",
      ns = ns,
      numericInput(ns("atkBns"), label = "Attack modifier", value = 0)
    ),
    conditionalPanel(
      "input.actType == 'save'",
      ns = ns,
      numericInput(ns("saveDC"), label = "Save DC", value = 10)
    ),
    textInput(ns("dmgDice"), label = "Damage dice", placeholder = "e.g. 2d6"),
    numericInput(ns("dmgBns"), label = "Damage bonus", value = 0),
    conditionalPanel(
      "input.actType == 'save'",
      ns = ns,
      selectInput(ns("DmgOnSave"), label = "Damage on save", choices = c("half", "none"))
    )
  )
}

actionServer <- function(id, ac.range, save.bns.range) {
  moduleServer(id, function(input, output, session) {
    reactive({
      req(input$name != "")
      req(str_detect(input$dmgDice, "^(\\d*d\\d+)( *\\+ *\\d*d\\d+)*$"))
      if (input$actType == "attack") {
        action.sim(
          name           = input$name,
          type           = input$actType,
          ac.range       = ac.range(),
          save.bns.range = save.bns.range(),
          roll.type      = input$rollType,
          atk.bns        = input$atkBns,
          dmg.dice       = input$dmgDice,
          dmg.bns        = input$dmgBns,
          n.atks         = input$nAtk
        )
      } else {
        action.sim(
          name           = input$name,
          type           = input$actType,
          ac.range       = ac.range(),
          save.bns.range = save.bns.range(),
          roll.type      = input$rollType,
          DC             = input$saveDC,
          dmg.on.save    = input$DmgOnSave,
          dmg.dice       = input$dmgDice,
          dmg.bns        = input$dmgBns,
          n.atks         = input$nAtk
        )
      }
    })
  })
}

### UI ###

ui <- page_sidebar(
  title = "Action Comparisons",
  theme = bs_theme(preset = "flatly"),
  sidebar = sidebar(
    width = 360,
    accordion(
      open = c("Settings", "Action A", "Action B"),
      multiple = TRUE,
      accordion_panel(
        "Settings",
        icon = bs_icon("sliders"),
        sliderInput("targetAC", label = "Target AC:", min = 0, max = 25, value = c(8, 18), step = 1),
        sliderInput("targetSave", label = "Target save mod:", min = -5, max = 15, value = c(0, 10), step = 1)
      ),
      accordion_panel(
        "Action A",
        icon = bs_icon("1-circle"),
        actionUI("actA", default_type = "attack")
      ),
      accordion_panel(
        "Action B",
        icon = bs_icon("2-circle"),
        actionUI("actB", default_type = "save")
      )
    )
  ),
  layout_columns(
    col_widths = c(6, 6),
    card(
      full_screen = TRUE,
      card_header("Action Choice"),
      plotOutput("gridPlot", click = "gridPlot_click", height = "560px")
    ),
    div(
      style = "display: flex; flex-direction: column; gap: 1rem; height: 100%;",
      card(
        style = "flex: 1; min-height: 0;",
        full_screen = TRUE,
        card_header(
          "Damage Distribution",
          tooltip(
            bs_icon("info-circle", title = "How to use"),
            "Click a cell in the Action Choice grid to see the damage distribution for that target."
          )
        ),
        plotOutput("distPlot", height = "100%", fill = TRUE)
      ),
      card(card_header(uiOutput("tableHeader")), tableOutput("clickTable"))
    )
  )
)

### Server ###

server <- function(input, output, session) {
  thematic::thematic_shiny()

  ac.range <- reactive(input$targetAC[1]:input$targetAC[2])
  save.bns.range <- reactive(input$targetSave[1]:input$targetSave[2])

  tblA <- actionServer("actA", ac.range, save.bns.range)
  tblB <- actionServer("actB", ac.range, save.bns.range)

  all.dmg <- reactive({
    rbind(tblA(), tblB())
  })

  all.stats <- reactive({
    rbind(
      all.dmg()[
        , .(Dmg = weighted.mean(Dmg, w = Wgt), Stat = "mean"),
        by = c("Action", "Target.AC", "Target.save")
      ],
      all.dmg()[
        , .(Dmg = weighted.median(Dmg, w = Wgt), Stat = "median"),
        by = c("Action", "Target.AC", "Target.save")
      ]
    )
  })

  expected <- reactive({
    all.dmg()[
      , .(Expected.dmg = sum(Dmg * Wgt)),
      by = c("Action", "Target.AC", "Target.save")
    ]
  })

  names.dt <- reactive({
    data.table(
      Action    = unique(expected()$Action),
      Short.lab = LETTERS[1:length(unique(expected()$Action))],
      key       = "Action"
    )
  })

  expected.dt <- reactive({
    merge(expected(), names.dt(), by = "Action")
  })
  grid.dt <- reactive({
    expected()[, .(Target.AC, Target.save)] %>% unique()
  })

  output$gridPlot <- renderPlot({
    lapply(1:nrow(grid.dt()), function(r) {
      cell <- expected.dt()[Target.AC == grid.dt()[r]$Target.AC & Target.save == grid.dt()[r]$Target.save]
      max_dmg <- max(cell$Expected.dmg)
      best <- cell[Expected.dmg == max_dmg]
      data.table(
        Target.AC    = grid.dt()[r]$Target.AC,
        Target.save  = grid.dt()[r]$Target.save,
        Expected.dmg = max_dmg,
        Short.lab    = paste(best$Short.lab, collapse = "")
      )
    }) %>%
      rbindlist() %>%
      ggplot(aes(x = Target.AC, y = Target.save, fill = Expected.dmg)) +
      geom_tile(color = "white") +
      geom_text(aes(label = Short.lab)) +
      scale_fill_distiller(name = "Expected damage", palette = "RdYlGn", direction = 1) +
      scale_x_continuous(breaks = input$targetAC[1]:input$targetAC[2]) +
      scale_y_continuous(breaks = input$targetSave[1]:input$targetSave[2]) +
      coord_equal() +
      labs(
        x = "Target AC",
        y = "Target save mod",
        caption = c(
          names.dt()[order(Short.lab), paste(Short.lab, Action, sep = ": ")],
          "\nClick on a cell to see the damage distribution."
        ) %>% paste(collapse = "\n")
      ) +
      theme(plot.caption = element_text(hjust = 0))
  })

  clickVals <- reactiveValues(AC = NA, Mod = NA)

  # Reset selection when the target grid changes
  observeEvent(
    list(input$targetAC, input$targetSave),
    {
      clickVals$AC <- NA
      clickVals$Mod <- NA
    },
    ignoreInit = TRUE
  )

  # Default to the best cell on first render
  observe({
    req(nrow(expected.dt()) > 0)
    if (is.na(clickVals$AC)) {
      best <- expected.dt()[which.max(Expected.dmg)]
      clickVals$AC <- best$Target.AC
      clickVals$Mod <- best$Target.save
    }
  })

  observeEvent(input$gridPlot_click, {
    clickVals$AC <- round(input$gridPlot_click$x, 0)
    clickVals$Mod <- round(input$gridPlot_click$y, 0)
  })

  output$distPlot <- renderPlot({
    if (is.na(clickVals$AC) || is.na(clickVals$Mod)) {
      ggplot() +
        annotate(
          "text",
          x = 0.5, y = 0.5,
          label = "Fill in both actions to see\nthe damage distribution.",
          size = 5, color = "grey60", hjust = 0.5, vjust = 0.5
        ) +
        theme_void()
    } else {
      input$gridPlot_click
      ggplot(all.dmg()[Target.AC == clickVals$AC & Target.save == clickVals$Mod]) +
        aes(x = Action, ymin = Dmg, ymax = Dmg) +
        geom_errorbar(
          data = all.stats()[Target.AC == clickVals$AC & Target.save == clickVals$Mod],
          aes(color = Stat),
          width = 0.55, linewidth = 1.2
        ) +
        scale_x_discrete(limits = names.dt()[order(Short.lab)]$Action) +
        scale_color_brewer(palette = "Dark2") +
        geom_errorbar(aes(width = {Wgt / max(Wgt)} * 0.9)) +
        scale_alpha_continuous(name = "Probability") +
        labs(
          y        = "Damage",
          title    = "Comparison for",
          subtitle = paste("Target AC:", clickVals$AC, "& Target save mod:", clickVals$Mod)
        ) +
        background_grid(major = "y", minor = "y")
    }
  })
  output$tableHeader <- renderUI({
    if (is.na(clickVals$AC) || is.na(clickVals$Mod)) {
      "Expected Damage by Action"
    } else {
      paste0(
        "Expected Damage by Action  \u2014  ",
        "Target AC: ", clickVals$AC, ", Target save mod: ", clickVals$Mod
      )
    }
  })

  output$clickTable <- renderTable(
    {
      req(!is.na(clickVals$AC), !is.na(clickVals$Mod))
      merge(
        expected()[Target.AC == clickVals$AC & Target.save == clickVals$Mod],
        names.dt()[, .(Action, Short.lab)],
        by = "Action"
      )[
        order(Expected.dmg, decreasing = TRUE),
        .(
          Action            = paste0(Short.lab, ": ", Action),
          `Expected damage` = Expected.dmg
        )
      ]
    },
    digits = 2,
    striped = TRUE,
    hover = TRUE,
    width = "100%"
  )
}

shinyApp(ui, server)
