#' @title LoginInputs
#'
#' @description Box with UserName and PassWord Inputs. Username inputId is 'UserName'. PassWord inputId is 'Password'
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id inputId
#' @param BoxStatus = 'danger'
#' @param UserNameLabel = "UserName"
#' @param PassWordLabel = 'Input Password'
#' @param UserNameChoices = Credentials[['UserName']]
#' @param BoxTitle = NULL
#' @param SolidHeader = TRUE
#' @param Collapsible = FALSE
#' @param AppWidth = AppWidth
#'
#' @noRd
LoginInputs <- function(id = 'LoginBox',
                        BoxStatus = 'danger',
                        UserNameLabel = "Select from Names",
                        PassWordLabel = 'Input Password',
                        UserNameChoices = Credentials[['UserName']],
                        BoxTitle = NULL,
                        SolidHeader = TRUE,
                        Collapsible = FALSE,
                        AppWidth = parent.frame()$AppWidth) {
  ns <- shiny::NS(id)
  shiny::tagList(
    RemixAutoML:::BlankRow(AppWidth),
    shinydashboard::box(
      title = BoxTitle,
      solidHeader = SolidHeader,
      collapsible = Collapsible,
      status = BoxStatus,
      width = AppWidth,
      shiny::uiOutput('UserName'),
      shiny::uiOutput('Password')))
}

#' @title LoginButton
#'
#' @description Action Button for Checking Credentials
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id 'Check_Credentials'
#' @param Label = 'Check Credentials',
#' @param AppWidth = AppWidth
#' @param Width = 3L
#' @param Icon = shiny::icon('chevron-right', lib = 'font-awesome')
#' @param Style = 'gradient'
#' @param Color = 'royal'
#'
#' @noRd
LoginButton <- function(id = 'LoginButton',
                        Label = 'Check Credentials',
                        AppWidth = parent.frame()$AppWidth,
                        Width = 3L,
                        Icon = shiny::icon('chevron-right', lib = 'font-awesome'),
                        Style = 'gradient',
                        Color = 'royal') {
  ns <- shiny::NS(id)
  shiny::tagList(
    RemixAutoML:::BlankRow(AppWidth),
    shiny::fluidRow(
      shiny::column(
        width = Width,
        align = 'center',
        shinyWidgets::actionBttn(
          inputId = 'Check_Credentials',
          label = Label,
          icon = Icon,
          style = Style,
          color = Color))))
}

#' @title LoadDataInputs
#'
#' @description Replica of box with inputs for local .csv data, local .Rdata, and a dropdown for External data. Local CSV inputId is 'TabularData'. Local .Rdata inputId is 'ModelObjectLoad', External csv inputId is 'blob'. External .Rdata inputId is 'rdatablob'
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id = input
#' @param AppWidth = AppWidth
#' @param LogoWidth = LogoWidth
#' @param SolidHeader = TRUE
#' @param BoxTitle = NULL
#' @param BoxStatus = 'danger'
#' @param DropdownRight = FALSE
#' @param DropDownAnimate = TRUE
#' @param DropDownStatus = 'custom'
#'
#' @noRd
LoadDataInputs <- function(id = 'ExternalData',
                           AppWidth = parent.frame()$AppWidth,
                           LogoWidth = parent.frame()$LogoWidth,
                           SolidHeader = TRUE,
                           BoxTitle = NULL,
                           BoxStatus = 'danger',
                           DropdownRight = FALSE,
                           DropDownAnimate = TRUE,
                           DropDownStatus = 'custom',
                           H3Color = 'snow')  {
  ns <- shiny::NS(id)
  shiny::tagList(
    RemixAutoML:::BlankRow(AppWidth),
    shinydashboard::box(
      title = BoxTitle,
      width = AppWidth,
      solidHeader = SolidHeader,
      status = BoxStatus,

      # Need a fluid row, otherwise shit shows up on two different rows
      RemixAutoML:::BlankRow(AppWidth),
      shiny::fluidRow(

        # Dropdown for local data
        shiny::column(
          width = 3L,
          align = 'center',
          tags$h4(tags$b('Local Data')),
          shinyWidgets::dropdown(
            right = DropdownRight, animate = DropDownAnimate, circle = FALSE, tooltip = FALSE, status = DropDownStatus, width = LogoWidth,
            tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Local Data')),
            RemixAutoML:::BlankRow(AppWidth),

            # Local data
            shiny::fluidRow(
              shiny::column(1L),
              shiny::column(
                width = 4L,
                align = 'center',
                tags$h4(tags$b('Local .csv Data')),
                shiny::uiOutput('TabularData')),
              shiny::column(2L),
              shiny::column(
                width = 4L,
                align = 'center',
                tags$h4(tags$b('Local Model Output')),
                shiny::uiOutput('ModelObjectLoad')))

            ), # end dropdown
          ), # end column

        # Dropdown for local data
        shiny::column(
          width = 3L,
          align = 'center',
          tags$h4(tags$b('Local PostGRE')),
          shinyWidgets::dropdown(
            right = DropdownRight, animate = DropDownAnimate, circle = FALSE, tooltip = FALSE, status = DropDownStatus, width = LogoWidth,
            tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Local PostGRE')),
            RemixAutoML:::BlankRow(AppWidth),
            shiny::fluidRow(
              shiny::column(1L),
              shiny::column(
                width = 4L,
                align = 'center',
                tags$h4(tags$b('Select Database')),
                shiny::uiOutput('LocalPostGRE_Database')),
              shiny::column(2L),
              shiny::column(
                width = 4L,
                align = 'center',
                tags$h4(tags$b('Select Table')),
                shiny::uiOutput('LocalPostGRE'))),

            # Add space so area underneath selected item remains inside the dropdown
            #   Otherwise, the dropdown closes prematurely
            RemixAutoML:::BlankRow(AppWidth),
            RemixAutoML:::BlankRow(AppWidth),
            RemixAutoML:::BlankRow(AppWidth),
            RemixAutoML:::BlankRow(AppWidth),
            RemixAutoML:::BlankRow(AppWidth),
            RemixAutoML:::BlankRow(AppWidth),
            RemixAutoML:::BlankRow(AppWidth)

            ) # end dropdown
          ), # end column

        # Dropdown for external data
        shiny::column(
          width = 3L,
          align = 'center',
          tags$h4(tags$b('Azure Data')),
          shinyWidgets::dropdown(
            right = DropdownRight, animate = DropDownAnimate, circle = FALSE, tooltip = FALSE, status = DropDownStatus, width = LogoWidth,
            tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Blob Data')),

            # Select data files
            RemixAutoML:::BlankRow(AppWidth),
            shiny::fluidRow(
              width = AppWidth,
              shiny::column(1L, align = 'center'),
              shiny::column(4L, align='center', shiny::uiOutput('AzureBlobStorageTabular')),
              shiny::column(2L, align = 'center'),
              shiny::column(4L, align='center', shiny::uiOutput('AzureBlobStorageRdata'))),

            # Add row
            RemixAutoML:::BlankRow(AppWidth),

            # Load button
            shiny::fluidRow(
              shiny::column(
                width = 5L,
                align = 'center',
                shinyWidgets::actionBttn(
                  inputId = 'LoadAzure',
                  label = 'Download Data'))),

            # Add space so area underneath selected item remains inside the dropdown
            #   Otherwise, the dropdown closes prematurely
            RemixAutoML:::BlankRow(AppWidth),
            RemixAutoML:::BlankRow(AppWidth),
            RemixAutoML:::BlankRow(AppWidth),
            RemixAutoML:::BlankRow(AppWidth),
            RemixAutoML:::BlankRow(AppWidth),
            RemixAutoML:::BlankRow(AppWidth),
            RemixAutoML:::BlankRow(AppWidth)

            ) # end dropdown
          ) # end column
        ), # end fluid row

      # Add rows to make space in between buttons from top to bottom more symmetrical
      RemixAutoML:::BlankRow(AppWidth),
      RemixAutoML:::BlankRow(AppWidth)

      ), # end box
    ) # end tagList
}

#' @title LoadDataButton
#'
#' @description LoadDataButton is a button for kicking off loading data
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id = input
#' @param AppWidth = AppWidth
#' @param Style = 'gradient'
#' @param Color = 'royal'
#'
#' @noRd
LoadDataButton <- function(id = 'DataButton',
                           AppWidth = parent.frame()$AppWidth,
                           Style = 'gradient',
                           Color = 'royal')  {
  ns <- shiny::NS(id)
  shiny::tagList(
    RemixAutoML:::BlankRow(AppWidth),
    shiny::fluidRow(
      shiny::column(
        width = 3L,
        align = 'center',
        shinyjs::useShinyjs(),
        shinyWidgets::actionBttn(
          inputId = 'LoadDataButton',
          label = 'Load Data',
          icon = shiny::icon('chevron-right', lib='font-awesome'),
          style = Style,
          color = Color))))
}

#' @title ShinySaveData
#'
#' @description Save data as csv to file. UI element
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id = 'SaveData_CSV_UI'
#' @param AppWidth = AppWidth
#' @param LogoWidth = LogoWidth
#' @param ButtonWidth = 3L
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'snow'
#'
#' @noRd
ShinySaveData <- function(id = 'SaveData_CSV_UI',
                          AppWidth=parent.frame()$AppWidth,
                          LogoWidth=parent.frame()$LogoWidth,
                          ButtonWidth=3L,
                          Style = 'gradient',
                          Color = 'royal',
                          Align='left',
                          DropDownRight=FALSE,
                          Animate=TRUE,
                          Status='custom',
                          H3Color = 'snow') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = ButtonWidth,
      align = Align,

      # Dropdown Button
      tags$h4(tags$b('Save Data')),
      shinyWidgets::dropdown(
        right = DropDownRight, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'SaveDataInputs',
        shiny::fluidRow(shiny::column(12L, align = 'center', tags$h4(tags$b('Save Data')))),
        RemixAutoML:::BlankRow(AppWidth),
        RemixAutoML:::BlankRow(AppWidth),
        shiny::fluidRow(

          # PostGRE DropDown
          shiny::column(
            width = 4L,
            tags$h4(tags$b('Local PostGRE')),
            shinyWidgets::dropdown(
              right = DropDownRight, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'SaveDataPostGRE',
              tags$h4(tags$b('PostGRE Table Creation')),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                shiny::column(width = 1L, align = 'center'),
                shiny::column(width = 4L, align = 'center', shiny::uiOutput('SaveData_SelectDataPostGRE'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                shiny::column(width = 1L, align = 'center'),
                shiny::column(width = 4L, align = 'center', shiny::uiOutput('SaveData_DataBaseName')),
                shiny::column(width = 4L, align = 'center', shiny::uiOutput('SaveData_TableName'))),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                shiny::column(width = 1L, align = 'center'),
                shiny::column(
                  width = 4L,
                  align = 'center',
                  shinyjs::useShinyjs(),
                  shinyWidgets::actionBttn(
                    inputId = 'PostGRE_Push', label = 'PostGRE CreateTable',
                    icon = shiny::icon('database', lib='font-awesome')))),

              # Add space so area underneath selected item remains inside the dropdown
              #   Otherwise, the dropdown closes prematurely
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth)

            ) # end postgre dropdown
          ), # end column

          # Local Data DropDown
          shiny::column(
            width = 4L,
            tags$h4(tags$b('Local Directory')),
            shinyWidgets::dropdown(
              right = DropDownRight, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'SaveDataLocal',
              tags$h4(tags$b('Local Directory .csv')),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                shiny::column(width = 1L, align = 'center'),
                shiny::column(width = 4L, align = 'center', shiny::uiOutput('SaveData_SelectData')),
                shiny::column(width = 2L, align = 'center'),
                shiny::column(width = 4L, align = 'center', tags$h4(tags$span(style=paste0('color: snow;'),'Press to Save')),
                              shiny::downloadButton(outputId = "SaveData_CSV", label = 'Save Data'))),

              # Add space so area underneath selected item remains inside the dropdown
              #   Otherwise, the dropdown closes prematurely
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth)

              ) # end dropdown
            ), # end column

          # Dropdowns on same row: otherwise they get put on diff rows
          # tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Save to Azure Blob Storage as .csv'))),
          # shiny::fluidRow(
          #   shiny::column(
          #     width = 4L,
          #     shiny::uiOutput(outputId = 'SaveData_AzureBlob'))),
        ),

        RemixAutoML:::BlankRow(AppWidth),
        RemixAutoML:::BlankRow(AppWidth)

      ) # end dropdown
    ) # end column
  ) # end tagList
}

#' @title FE_CalendarVariables
#'
#' @description Calendar Variables Dropdown
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id = 'CalendarVariables'
#' @param AppWidth = AppWidth
#' @param LogoWidth = LogoWidth
#' @param ButtonWidth = 3L
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'snow'
#'
#' @noRd
FE_DateVariables <- function(id='CalendarVariables',
                             AppWidth=parent.frame()$AppWidth,
                             LogoWidth=parent.frame()$LogoWidth,
                             ButtonWidth=3L,
                             Style = 'gradient',
                             Color = 'royal',
                             Align='left',
                             DropDownRight=FALSE,
                             Animate=TRUE,
                             Status='custom',
                             H3Color = 'snow') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = ButtonWidth,
      align=Align,

      # Dropdown Button
      tags$h4(tags$b('Date Features')),
      shinyWidgets::dropdown(
        right = DropDownRight, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth,
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Select Inputs'))),
        RemixAutoML:::BlankRow(AppWidth),

        # Dropdowns on same row: otherwise they get put on diff rows
        shiny::fluidRow(

          # Calendar Variables Selection
          shiny::column(
            width = 3L,
            tags$h4(tags$b('Calendar Features')),
            shinyWidgets::dropdown(
              right = if(DropDownRight) FALSE else TRUE, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'CalendarVariablesInputs',
              tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Calendar Features'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(6L, align = 'center', shiny::uiOutput('CalendarVariables_SelectData'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(6L, align = 'center', shiny::uiOutput('CalendarVariables_DateVariables')),
                shiny::column(6L, align = 'center', shiny::uiOutput('CalendarVariables_TimeUnits'))),

              # Blank space
              RemixAutoML:::BlankRow(AppWidth),

              # Date Variable Build Button
              shiny::fluidRow(
                shiny::column(
                  width = 3L,
                  align = 'center',
                  shinyWidgets::actionBttn(
                    inputId = 'FeatureEngineeringButton_CalendarVariables',
                    label = 'Run',
                    icon = shiny::icon('chevron-right', lib='font-awesome'),
                    style = Style,
                    color = Color))))),


          # Holiday Variables Selection
          shiny::column(
            width = 3L,
            tags$h4(tags$b('Holiday Features')),
            shinyWidgets::dropdown(
              right = if(DropDownRight) FALSE else TRUE, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'HolidayVariablesInputs',
              tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Holiday Features'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(6L, align = 'center', shiny::uiOutput('HolidayVariables_SelectData'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(6L, align = 'center', shiny::uiOutput('HolidayVariables_DateVariables')),
                shiny::column(6L, align = 'center', shiny::uiOutput('HolidayVariables_HolidayGroups')),
                shiny::column(6L, align = 'center', shiny::uiOutput('HolidayVariables_LookbackDays'))),

              # Blank space
              RemixAutoML:::BlankRow(AppWidth),

              # Holiday Variable Build Button
              shiny::fluidRow(
                shiny::column(
                  width = 3L,
                  align = 'center',
                  shinyWidgets::actionBttn(
                    inputId = 'FeatureEngineeringButton_HolidayVariables',
                    label = 'Run',
                    icon = shiny::icon('chevron-right', lib='font-awesome'),
                    style = Style,
                    color = Color))),

              # Add space so area underneath selected item remains inside the dropdown
              #   Otherwise, the dropdown closes prematurely
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth)

            ) # end dropdown
          ) # end column
        ), # end fluid row

        # Add space at bottom of box
        RemixAutoML:::BlankRow(AppWidth)

      )))
}

#' @title FE_NumericVariables
#'
#' @description Numeric Variables Dropdown
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id = 'NumericVariables'
#' @param AppWidth = AppWidth
#' @param LogoWidth = LogoWidth
#' @param ButtonWidth = 3L
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'snow'
#'
#' @noRd
FE_NumericVariables <- function(id='NumericVariables',
                                AppWidth=parent.frame()$AppWidth,
                                LogoWidth=parent.frame()$LogoWidth,
                                ButtonWidth=3L,
                                Style = 'gradient',
                                Color = 'royal',
                                Align='left',
                                DropDownRight=FALSE,
                                Animate=TRUE,
                                Status='custom',
                                H3Color = 'snow') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = ButtonWidth,
      align=Align,

      # Dropdown Button
      tags$h4(tags$b('Numeric Features')),
      shinyWidgets::dropdown(
        right = DropDownRight, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth,
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Select Inputs'))),
        RemixAutoML:::BlankRow(AppWidth),

        # Dropdowns on same row: otherwise they get put on diff rows
        shiny::fluidRow(

          # PercRank Selection
          shiny::column(
            width = 3L,
            tags$h4(tags$b('Percent Rank')),
            shinyWidgets::dropdown(
              right = DropDownRight, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'PercRankInputs',
              tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Percent Rank Variables'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(6L, align = 'center', shiny::uiOutput('PercentRank_SelectData'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(4L, align = 'center', shiny::uiOutput('PercentRank_ColNames')),
                shiny::column(4L, align = 'center', shiny::uiOutput('PercentRank_GroupVars')),
                shiny::column(4L, align = 'center', shiny::uiOutput('PercentRank_Granularity'))),

              # Blank space
              RemixAutoML:::BlankRow(AppWidth),

              # PercRank Build Button
              shiny::fluidRow(
                shiny::column(
                  width = 3L,
                  align = 'center',
                  shinyWidgets::actionBttn(
                    inputId = 'FeatureEngineeringButton_PercRank',
                    label = 'Run',
                    icon = shiny::icon('chevron-right', lib='font-awesome'),
                    style = Style,
                    color = Color))),

              # Add space so area underneath selected item remains inside the dropdown
              #   Otherwise, the dropdown closes prematurely
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth)

              ) # end dropdown
            ), # end column

          # Auto Interaction Selection
          shiny::column(
            width = 3L,
            tags$h4(tags$b('Interaction')),
            shinyWidgets::dropdown(
              right = DropDownRight, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'AutoInteractionInputs',
              tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Interaction Variables'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(6L, align = 'center', shiny::uiOutput('AutoInteraction_SelectData'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(6L, align = 'center', shiny::uiOutput('AutoInteraction_NumericVars')),
                shiny::column(6L, align = 'center', shiny::uiOutput('AutoInteraction_InteractionDepth'))),
              shiny::fluidRow(
                shiny::column(6L, align = 'center', shiny::uiOutput('AutoInteraction_Center')),
                shiny::column(6L, align = 'center', shiny::uiOutput('AutoInteraction_Scale'))),

              # Blank space
              RemixAutoML:::BlankRow(AppWidth),

              # PercRank Build Button
              shiny::fluidRow(
                shiny::column(
                  width = 3L,
                  align = 'center',
                  shinyWidgets::actionBttn(
                    inputId = 'FeatureEngineeringButton_Interaction',
                    label = 'Run',
                    icon = shiny::icon('chevron-right', lib='font-awesome'),
                    style = Style,
                    color = Color))),

              # Add space so area underneath selected item remains inside the dropdown
              #   Otherwise, the dropdown closes prematurely
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth)

            ) # end dropdown
          ), # end column

          # Transformations
          shiny::column(
            width = 3L,
            tags$h4(tags$b('Transformation')),
            shinyWidgets::dropdown(
              right = TRUE, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'TransformationInputs',
              tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Transform Variables'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(6L, align = 'center', shiny::uiOutput('AutoTransformationCreate_SelectData'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(6L, align = 'center', shiny::uiOutput('AutoTransformationCreate_ColumnNames')),
                shiny::column(6L, align = 'center', shiny::uiOutput('AutoTransformationCreate_Methods'))),

              # Blank space
              RemixAutoML:::BlankRow(AppWidth),

              # PercRank Build Button
              shiny::fluidRow(
                shiny::column(
                  width = 3L,
                  align = 'center',
                  shinyWidgets::actionBttn(
                    inputId = 'FeatureEngineeringButton_Transformations',
                    label = 'Run',
                    icon = shiny::icon('chevron-right', lib='font-awesome'),
                    style = Style,
                    color = Color)))))),

        # Add space at bottom of box
        RemixAutoML:::BlankRow(AppWidth)

      )))
}

#' @title FE_CategoricalVariables
#'
#' @description Categorical Variables Dropdown
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id = 'CategoricalVariables'
#' @param AppWidth = AppWidth
#' @param LogoWidth = LogoWidth
#' @param ButtonWidth = 3L
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'snow'
#'
#' @noRd
FE_CategoricalVariables <- function(id='CategoricalVariables',
                                    AppWidth=parent.frame()$AppWidth,
                                    LogoWidth=parent.frame()$LogoWidth,
                                    ButtonWidth=3L,
                                    Style = 'gradient',
                                    Color = 'royal',
                                    Align='left',
                                    DropDownRight=TRUE,
                                    Animate=TRUE,
                                    Status='custom',
                                    H3Color = 'snow') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = ButtonWidth,
      align=Align,

      # Dropdown Button
      tags$h4(tags$b('Categorical Features')),
      shinyWidgets::dropdown(
        right = DropDownRight, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth,
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Select Inputs'))),
        RemixAutoML:::BlankRow(AppWidth),

        # Dropdowns on same row: otherwise they get put on diff rows
        shiny::fluidRow(

          # Partial Dummies
          shiny::column(
            width = 3L,
            tags$h4(tags$b('One Hot')),
            shinyWidgets::dropdown(
              right = FALSE, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'PartialDummiesInputs',
              tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Create Partial Dummy Variables'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(6L, align = 'center', shiny::uiOutput('DummifyDT_SelectData'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(4L, align = 'center', shiny::uiOutput('DummifyDT_Cols')),
                shiny::column(4L, align = 'center', shiny::uiOutput('DummifyDT_TopN')),
                shiny::column(4L, align = 'center', shiny::uiOutput('DummifyDT_KeepBaseCols'))),

              # Blank space
              RemixAutoML:::BlankRow(AppWidth),

              # PartialDummies Build Button
              shiny::fluidRow(
                shiny::column(
                  width = 3L,
                  align = 'center',
                  shinyWidgets::actionBttn(
                    inputId = 'FeatureEngineeringButton_PartialDummies',
                    label = 'Run',
                    icon = shiny::icon('chevron-right', lib='font-awesome'),
                    style = Style,
                    color = Color))))),

          # Categorical Encoding
          shiny::column(
            width = 3L,
            tags$h4(tags$b('Char Encode')),
            shinyWidgets::dropdown(
              right = FALSE, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'CategoricalEncodingInputs',
              tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Categorical Encoding'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(6L, align = 'center', shiny::uiOutput('CategoricalEncoding_SelectData'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(4L, align = 'center', shiny::uiOutput('CategoricalEncoding_GroupVariables')),
                shiny::column(4L, align = 'center', shiny::uiOutput('CategoricalEncoding_TargetVariable')),
                shiny::column(4L, align = 'center', shiny::uiOutput('CategoricalEncoding_Method'))),

              # Blank space
              RemixAutoML:::BlankRow(AppWidth),

              # CategoricalEncoding Build Button
              shiny::fluidRow(
                shiny::column(
                  width = 3L,
                  align = 'center',
                  shinyWidgets::actionBttn(
                    inputId = 'FeatureEngineeringButton_CategoricalEncoding',
                    label = 'Run',
                    icon = shiny::icon('chevron-right', lib='font-awesome'),
                    style = Style,
                    color = Color))),

              # Add space so area underneath selected item remains inside the dropdown
              #   Otherwise, the dropdown closes prematurely
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth)

              ) # end dropdown
            ) # end column
          ), # end fluid row

        # Add space at bottom of box
        RemixAutoML:::BlankRow(AppWidth)

      )))
}

#' @title FE_WindowingVariables
#'
#' @description Windowing Variables Dropdown
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id = 'WindowingVariables'
#' @param AppWidth = AppWidth
#' @param LogoWidth = LogoWidth
#' @param ButtonWidth = 3L
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'snow'
#'
#' @noRd
FE_WindowingVariables <- function(id='WindowingVariables',
                                  AppWidth=parent.frame()$AppWidth,
                                  LogoWidth=parent.frame()$LogoWidth,
                                  ButtonWidth=3L,
                                  Style = 'gradient',
                                  Color = 'royal',
                                  Align='left',
                                  DropDownRight=FALSE,
                                  Animate=TRUE,
                                  Status='custom',
                                  H3Color = 'snow') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = ButtonWidth,
      align=Align,

      # Dropdown Button
      tags$h4(tags$b('Windowing Features')),
      shinyWidgets::dropdown(
        right = DropDownRight, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth,
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Select Inputs'))),
        RemixAutoML:::BlankRow(AppWidth),

        # Dropdowns on same row: otherwise they get put on diff rows
        shiny::fluidRow(

          # Rolling Categorical Dropdown Button
          shiny::column(
            width = 3L,
            tags$h4(tags$b('Categorical Vars')),
            shinyWidgets::dropdown(
              right = FALSE, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'AutoLagRollModeInputs',
              tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Rolling Categorical'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(6L, align = 'center', shiny::uiOutput('AutoLagRollMode_SelectData'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(4L, align = 'center', shiny::uiOutput('AutoLagRollMode_Targets')),
                shiny::column(4L, align = 'center', shiny::uiOutput('AutoLagRollMode_Lags')),
                shiny::column(4L, align = 'center', shiny::uiOutput('AutoLagRollMode_ModePeriods'))),
              shiny::fluidRow(
                shiny::column(4L, align = 'center', shiny::uiOutput('AutoLagRollMode_GroupingVars')),
                shiny::column(4L, align = 'center', shiny::uiOutput('AutoLagRollMode_SortDateName')),
                shiny::column(4L, align = 'center', shiny::uiOutput('AutoLagRollMode_WindowingLag'))),

              # Blank space
              RemixAutoML:::BlankRow(AppWidth),

              # Build Button
              shiny::fluidRow(
                shiny::column(
                  width = 3L,
                  align = 'center',
                  shinyWidgets::actionBttn(
                    inputId = 'FeatureEngineeringButton_AutoLagRollMode',
                    label = 'Run',
                    icon = shiny::icon('chevron-right', lib='font-awesome'),
                    style = Style,
                    color = Color))),

              # Add space so area underneath selected item remains inside the dropdown
              #   Otherwise, the dropdown closes prematurely
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth)

            ) # end dropdown
          ), # end column

          # Rolling Numeric
          shiny::column(
            width = 3L,
            tags$h4(tags$b('Numeric Vars')),
            shinyWidgets::dropdown(
              right = FALSE, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'AutoLagRollStatsInputs',
              tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Rolling Numeric'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(6L, align = 'center', shiny::uiOutput('AutoLagRollStats_SelectData'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(3L, align = 'center', shiny::uiOutput('AutoLagRollStats_Targets')),
                shiny::column(3L, align = 'center', shiny::uiOutput('AutoLagRollStats_GroupVars')),
                shiny::column(3L, align = 'center', shiny::uiOutput('AutoLagRollStats_DateColumn')),
                shiny::column(3L, align = 'center', shiny::uiOutput('AutoLagRollStats_RollOnLag1'))),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(3L, align = 'center', shiny::uiOutput('AutoLagRollStats_TimeUnits')),
                shiny::column(3L, align = 'center', shiny::uiOutput('AutoLagRollStats_Lags')),
                shiny::column(3L, align = 'center', shiny::uiOutput('AutoLagRollStats_MA_RollWindows')),
                shiny::column(3L, align = 'center', shiny::uiOutput('AutoLagRollStats_SD_RollWindows'))),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(3L, align = 'center', shiny::uiOutput('AutoLagRollStats_Skew_RollWindows')),
                shiny::column(3L, align = 'center', shiny::uiOutput('AutoLagRollStats_Kurt_RollWindows')),
                shiny::column(3L, align = 'center', shiny::uiOutput('AutoLagRollStats_Quantiles_Selected')),
                shiny::column(3L, align = 'center', shiny::uiOutput('AutoLagRollStats_Quantile_RollWindows'))),

              # Blank space
              RemixAutoML:::BlankRow(AppWidth),

              # AutoLagRollStats Build Button
              shiny::fluidRow(
                shiny::column(
                  width = 3L,
                  align = 'center',
                  shinyWidgets::actionBttn(
                    inputId = 'FeatureEngineeringButton_AutoLagRollStats',
                    label = 'Run',
                    icon = shiny::icon('chevron-right', lib='font-awesome'),
                    style = Style,
                    color = Color))))),

          # Diff Numeric, Character, Dates
          shiny::column(
            width = 3L,
            tags$h4(tags$b('Diff Vars')),
            shinyWidgets::dropdown(
              right = FALSE, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'AutoDiffInputs',
              tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Differencing Variables'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(6L, align = 'center', shiny::uiOutput('AutoDiffLagN_SelectData'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(4L, align = 'center', shiny::uiOutput('AutoDiffLagN_DateVariable')),
                shiny::column(4L, align = 'center', shiny::uiOutput('AutoDiffLagN_GroupVariables'))),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(4L, align = 'center', shiny::uiOutput('AutoDiffLagN_DiffVariables')),
                shiny::column(4L, align = 'center', shiny::uiOutput('AutoDiffLagN_DiffDateVariables')),
                shiny::column(4L, align = 'center', shiny::uiOutput('AutoDiffLagN_DiffGroupVariables'))),
              shiny::fluidRow(
                shiny::column(4L, align = 'center', shiny::uiOutput('AutoDiffLagN_NLag1')),
                shiny::column(4L, align = 'center', shiny::uiOutput('AutoDiffLagN_NLag2'))),

              # Blank space
              RemixAutoML:::BlankRow(AppWidth),

              # AutoDiff Build Button
              shiny::fluidRow(
                shiny::column(
                  width = 3L,
                  align = 'center',
                  shinyWidgets::actionBttn(
                    inputId = 'FeatureEngineeringButton_AutoDiff',
                    label = 'Run',
                    icon = shiny::icon('chevron-right', lib='font-awesome'),
                    style = Style,
                    color = Color))),

              # Add space so area underneath selected item remains inside the dropdown
              #   Otherwise, the dropdown closes prematurely
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth)

              ) # end dropdown
            ), # end column
          ), # end fluid row

        # Add space at bottom of box
        RemixAutoML:::BlankRow(AppWidth),
        RemixAutoML:::BlankRow(AppWidth)

        ) # end dropdown
      ) # end column
    ) # end tagList
}

#' @title FE_DataWrangling
#'
#' @description Calendar Variables Dropdown
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id = 'GeneralFeatureEngineering'
#' @param AppWidth = AppWidth
#' @param LogoWidth = LogoWidth
#' @param ButtonWidth = 3L
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'snow'
#'
#' @noRd
FE_DataWrangling <- function(id='GeneralFeatureEngineering',
                             AppWidth=parent.frame()$AppWidth,
                             LogoWidth=parent.frame()$LogoWidth,
                             ButtonWidth=3L,
                             Style = 'gradient',
                             Color = 'royal',
                             Align='left',
                             DropDownRight=FALSE,
                             Animate=TRUE,
                             Status='custom',
                             H3Color = 'snow') {
  ns <- shiny::NS(id)
  shiny::tagList(

    # General FE
    shiny::column(
      width = ButtonWidth,
      align=Align,

      # Dropdown Button
      tags$h4(tags$b('Data Wrangling')),
      shinyWidgets::dropdown(
        right = DropDownRight, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth,
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Select Inputs'))),
        RemixAutoML:::BlankRow(AppWidth),

        # Row
        shiny::fluidRow(

          # Delete Features
          shiny::column(
            width = ButtonWidth,
            align=Align,

            # Dropdown Button
            tags$h4(tags$b('Delete Features')),
            shinyWidgets::dropdown(
              right = if(DropDownRight) FALSE else TRUE, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'DeleteVariablesInputs',
              tags$h3(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Select Inputs'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(6L, align = 'center', shiny::uiOutput('DeleteVariables_SelectData')),
                shiny::column(6L, align = 'center', shiny::uiOutput('DeleteVariables'))),

              # Blank space
              RemixAutoML:::BlankRow(AppWidth),

              # Delete Features Button
              shiny::fluidRow(
                shiny::column(
                  width = 3L,
                  align = 'center',
                  shinyWidgets::actionBttn(
                    inputId = 'FeatureEngineeringButton_DeleteFeatures',
                    label = 'Run',
                    icon = shiny::icon('chevron-right', lib='font-awesome'),
                    style = Style,
                    color = Color))),

              # Add space at bottom of box
              RemixAutoML:::BlankRow(AppWidth)

            )), # end column

          # Concat Features
          shiny::column(
            width = ButtonWidth,
            align=Align,

            # Dropdown Button
            tags$h4(tags$b('Concat Features')),
            shinyWidgets::dropdown(
              right = if(DropDownRight) FALSE else TRUE, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'ConcatColumnsInputs',
              tags$h3(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Select Inputs'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(6L, align = 'center', shiny::uiOutput('ConcatColumns_SelectData')),
                shiny::column(6L, align = 'center', shiny::uiOutput('ConcatColumns'))),

              # Blank space
              RemixAutoML:::BlankRow(AppWidth),

              # Delete Features Button
              shiny::fluidRow(
                shiny::column(
                  width = 3L,
                  align = 'center',
                  shinyWidgets::actionBttn(
                    inputId = 'FeatureEngineeringButton_ConcatColumns',
                    label = 'Run',
                    icon = shiny::icon('chevron-right', lib='font-awesome'),
                    style = Style,
                    color = Color))),

              # Add space at bottom of box
              RemixAutoML:::BlankRow(AppWidth)

      ))),

      # Add space at bottom of box
      RemixAutoML:::BlankRow(AppWidth)

      ))) # tagList
}

#' @title FE_DataSets
#'
#' @description Calendar Variables Dropdown
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id = 'DataSets'
#' @param AppWidth = AppWidth
#' @param LogoWidth = LogoWidth
#' @param ButtonWidth = 3L
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'snow'
#'
#' @noRd
FE_DataSets <- function(id='DataSets',
                        AppWidth=parent.frame()$AppWidth,
                        LogoWidth=parent.frame()$LogoWidth,
                        ButtonWidth=3L,
                        Style = 'gradient',
                        Color = 'royal',
                        Align='left',
                        DropDownRight=FALSE,
                        Animate=TRUE,
                        Status='custom',
                        H3Color = 'snow') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = ButtonWidth,
      align=Align,

      # Dropdown Button
      tags$h4(tags$b('Data Sets')),
      shinyWidgets::dropdown(
        right = DropDownRight, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth,
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Select Inputs'))),
        RemixAutoML:::BlankRow(AppWidth),

        # Dropdowns on same row: otherwise they get put on diff rows
        shiny::fluidRow(

          # Model Data Prep
          shiny::column(
            width = 3L,
            tags$h4(tags$b('Type Casting')),
            shinyWidgets::dropdown(
              right = DropDownRight, up = TRUE, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'ModelDataPrepInputs',
              tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Type Conversion'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(6L, align = 'center', shiny::uiOutput('ModelDataPrep_SelectData'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(3L, align = 'center', shiny::uiOutput('ModelDataPrep_IgnoreCols')),
                shiny::column(3L, align = 'center', shiny::uiOutput('ModelDataPrep_CharToFactor')),
                shiny::column(3L, align = 'center', shiny::uiOutput('ModelDataPrep_FactorToChar')),
                shiny::column(3L, align = 'center', shiny::uiOutput('ModelDataPrep_DateToChar'))),
              shiny::fluidRow(
                shiny::column(3L, align = 'center', shiny::uiOutput('ModelDataPrep_IDateConversion')),
                shiny::column(3L, align = 'center', shiny::uiOutput('ModelDataPrep_RemoveDates')),
                shiny::column(3L, align = 'center', shiny::uiOutput('ModelDataPrep_IntToNumeric')),
                shiny::column(3L, align = 'center', shiny::uiOutput('ModelDataPrep_LogicalToBinary'))),

              # Blank space
              RemixAutoML:::BlankRow(AppWidth),

              # Build Button
              shiny::fluidRow(
                shiny::column(
                  width = 3L,
                  align = 'center',
                  shinyWidgets::actionBttn(
                    inputId = 'FeatureEngineeringButton_ModelDataPrep',
                    label = 'Run',
                    icon = shiny::icon('chevron-right', lib='font-awesome'),
                    style = Style,
                    color = Color))))),

          # Partition Data
          shiny::column(
            width = 3L,
            tags$h4(tags$b('Partition Data')),
            shinyWidgets::dropdown(
              right = DropDownRight, up = TRUE, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'AutoDataPartitionInputs',
              tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Partition Data Sets'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(6L, align = 'center', shiny::uiOutput('AutoDataPartition_SelectData'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(4L, align = 'center', shiny::uiOutput('AutoDataPartition_Ratios_Train')),
                shiny::column(4L, align = 'center', shiny::uiOutput('AutoDataPartition_Ratios_Validation')),
                shiny::column(4L, align = 'center', shiny::uiOutput('AutoDataPartition_Ratios_Test'))),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(4L, align = 'center', shiny::uiOutput('AutoDataPartition_NumDataSets')),
                shiny::column(4L, align = 'center', shiny::uiOutput('AutoDataPartition_PartitionType'))),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(4L, align = 'center', shiny::uiOutput('AutoDataPartition_StratifyCols')),
                shiny::column(4L, align = 'center', shiny::uiOutput('AutoDataPartition_DateColumn'))),

              # Blank space
              RemixAutoML:::BlankRow(AppWidth),

              # Build Button
              shiny::fluidRow(
                shiny::column(
                  width = 3L,
                  align = 'center',
                  shinyWidgets::actionBttn(
                    inputId = 'FeatureEngineeringButton_AutoDataPartition',
                    label = 'Run',
                    icon = shiny::icon('chevron-right', lib='font-awesome'),
                    style = Style,
                    color = Color)))))),

        # Add space at bottom of box
        RemixAutoML:::BlankRow(AppWidth)

      )))
}

#' @title FE_ModelBased
#'
#' @description Calendar Variables Dropdown
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id = 'DataSets'
#' @param AppWidth = AppWidth
#' @param LogoWidth = LogoWidth
#' @param ButtonWidth = 3L
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'snow'
#'
#' @noRd
FE_ModelBased <- function(id='DataSets',
                          AppWidth=parent.frame()$AppWidth,
                          LogoWidth=parent.frame()$LogoWidth,
                          ButtonWidth=3L,
                          Style = 'gradient',
                          Color = 'royal',
                          Align='left',
                          DropDownRight=FALSE,
                          Animate=TRUE,
                          Status='custom',
                          H3Color = 'snow') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = ButtonWidth,
      align=Align,

      # Dropdown Button
      tags$h4(tags$b('Model Based')),
      shinyWidgets::dropdown(
        right = DropDownRight, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth,
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Select Inputs'))),
        RemixAutoML:::BlankRow(AppWidth),

        # Dropdowns on same row: otherwise they get put on diff rows
        shiny::fluidRow(

          # Model Data Prep
          shiny::column(
            width = 3L,
            tags$h4(tags$b('H2O Word2Vec')),
            shinyWidgets::dropdown(
              inputId = 'H2O_Word2VecInputs', right = DropDownRight, up = TRUE, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth,
              tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0(''))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(6L, align = 'center', shiny::uiOutput(''))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(4L, align = 'center', shiny::uiOutput('H2O_Word2Vec_TrainData')),
                shiny::column(4L, align = 'center', shiny::uiOutput('H2O_Word2Vec_ValidationData')),
                shiny::column(4L, align = 'center', shiny::uiOutput('H2O_Word2Vec_TestData'))),
              shiny::fluidRow(
                shiny::column(4L, align = 'center', shiny::uiOutput('H2O_Word2Vec_BuildType')),
                shiny::column(4L, align = 'center', shiny::uiOutput('H2O_Word2Vec_stringCol')),
                shiny::column(4L, align = 'center', shiny::uiOutput('H2O_Word2Vec_KeepStringCol'))),
              shiny::fluidRow(
                shiny::column(3L, align = 'center', shiny::uiOutput('H2O_Word2Vec_vects')),
                shiny::column(3L, align = 'center', shiny::uiOutput('H2O_Word2Vec_MinWords')),
                shiny::column(3L, align = 'center', shiny::uiOutput('H2O_Word2Vec_WindowSize')),
                shiny::column(3L, align = 'center', shiny::uiOutput('H2O_Word2Vec_Epochs'))),

              # Blank space
              RemixAutoML:::BlankRow(AppWidth),

              # Build Button
              shiny::fluidRow(
                shiny::column(
                  width = 3L,
                  align = 'center',
                  shinyWidgets::actionBttn(
                    inputId = 'FeatureEngineeringButton_H2O_Word2Vec',
                    label = 'Run',
                    icon = shiny::icon('chevron-right', lib='font-awesome'),
                    style = Style,
                    color = Color))))),

          # H2O Clustering
          shiny::column(
            width = 3L,
            tags$h4(tags$b('')),
            shinyWidgets::dropdown(
              right = DropDownRight, up = TRUE, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'AutoDataPartitionInputs',
              tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0(''))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(6L, align = 'center', shiny::uiOutput(''))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(4L, align = 'center', shiny::uiOutput('')),
                shiny::column(4L, align = 'center', shiny::uiOutput('')),
                shiny::column(4L, align = 'center', shiny::uiOutput(''))),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(4L, align = 'center', shiny::uiOutput('')),
                shiny::column(4L, align = 'center', shiny::uiOutput(''))),
              shiny::fluidRow(
                width=AppWidth,
                shiny::column(4L, align = 'center', shiny::uiOutput('')),
                shiny::column(4L, align = 'center', shiny::uiOutput(''))),

              # Blank space
              RemixAutoML:::BlankRow(AppWidth),

              # Build Button
              shiny::fluidRow(
                shiny::column(
                  width = 3L,
                  align = 'center',
                  shinyWidgets::actionBttn(
                    inputId = '',
                    label = 'Run',
                    icon = shiny::icon('chevron-right', lib='font-awesome'),
                    style = Style,
                    color = Color)))))),

        # Add space at bottom of box
        RemixAutoML:::BlankRow(AppWidth)

      )))
}



#' @title ML_CatBoost
#'
#' @description Catboost args
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id = 'CatBoost'
#' @param AppWidth = AppWidth
#' @param LogoWidth = LogoWidth
#' @param ButtonWidth = 3L
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'snow'
#'
#' @noRd
ML_CatBoost <- function(id='CatBoostML',
                        AppWidth=parent.frame()$AppWidth,
                        LogoWidth=parent.frame()$LogoWidth,
                        ButtonWidth=3L,
                        Style = 'gradient',
                        Color = 'royal',
                        Align='left',
                        DropDownRight=FALSE,
                        Animate=TRUE,
                        Status='custom',
                        H3Color = 'snow') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = ButtonWidth,
      align=Align,

      # Dropdown Button
      tags$h4(tags$b('CatBoost')),
      shinyWidgets::dropdown(
        right = DropDownRight, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'CatBoost',
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Select Inputs'))),
        shiny::fluidRow(
          width=AppWidth,
          shiny::column(1L, align = 'center'),
          shiny::column(4L, align = 'center', shiny::uiOutput('CatBoost_TargetType')),
          shiny::column(1L, align = 'center'),
          shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_Runs'))),

        # Parameters
        RemixAutoML:::BlankRow(AppWidth),
        shiny::fluidRow(

          # MetaData Parameters
          shiny::column(
            width = 4L,
            tags$h4(tags$b('Meta Data Parameters')),
            shinyWidgets::dropdown(
              right = DropDownRight, up = TRUE, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'Cat_MetaData',
              tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('CatBoost Parameters'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_NThreads')),
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_task_type')),
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_NumGPUs'))),
              shiny::fluidRow(
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_TrainOnFull')),
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_ModelID'))),

              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth)

              )),

          # Grid Tuning Parameters
          shiny::column(
            width = 4L,
            tags$h4(tags$b('Grid Tuning Parameters')),
            shinyWidgets::dropdown(
              right = DropDownRight, up = TRUE, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'Cat_GridTuningParameters',
              tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('CatBoost Parameters'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_PassInGrid')),
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_GridTune')),
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_grid_eval_metric')),
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_MaxModelsInGrid'))),
              shiny::fluidRow(
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_MaxRunsWithoutNewWinner')),
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_MaxRunMinutes')),
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_BaselineComparison'))),

              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth)

              ))),

        # Parameters
        RemixAutoML:::BlankRow(AppWidth),
        shiny::fluidRow(

          # ML Algo Parameters
          shiny::column(
            width = 4L,
            tags$h4(tags$b('ML Parameters')),
            shinyWidgets::dropdown(
              right = DropDownRight, up = TRUE, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'Cat_MLParameters',
              tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('CatBoost Parameters'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_Trees')),
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_Depth')),
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_LearningRate')),
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_L2_Leaf_Reg'))),
              shiny::fluidRow(
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_model_size_reg')),
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_langevin')),
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_diffusion_temperature')),
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_RandomStrength'))),
              shiny::fluidRow(
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_BorderCount')),
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_RSM')),
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_BootStrapType')),
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_GrowPolicy'))),
              shiny::fluidRow(
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_feature_border_type')),
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_subsample')),
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_score_function')),
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_min_data_in_leaf'))),

              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth)

              )),

          # Data Parameters
          shiny::column(
            width = 4L,
            tags$h4(tags$b('Data Parameters')),
            shinyWidgets::dropdown(
              right = DropDownRight, up = TRUE, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'Cat_DataParameters',
              tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('CatBoost Parameters'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                shiny::column(4L, align = 'center', shiny::uiOutput('CatBoost_data')),
                shiny::column(4L, align = 'center', shiny::uiOutput('CatBoost_ValidationData')),
                shiny::column(4L, align = 'center', shiny::uiOutput('CatBoost_TestData'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                shiny::column(4L, align = 'center', shiny::uiOutput('CatBoost_TargetColumnName')),
                shiny::column(4L, align = 'center', shiny::uiOutput('CatBoost_FeatureColNames')),
                shiny::column(4L, align = 'center', shiny::uiOutput('CatBoost_PrimaryDateColumn'))),
              shiny::fluidRow(
                shiny::column(4L, align = 'center', shiny::uiOutput('CatBoost_EncodeMethod')),
                shiny::column(4L, align = 'center', shiny::uiOutput('CatBoost_WeightsColumnName')),
                shiny::column(4L, align = 'center', shiny::uiOutput('CatBoost_IDcols'))),
              shiny::fluidRow(
                shiny::column(4L, align = 'center', shiny::uiOutput('CatBoost_TransformNumericColumns')),
                shiny::column(4L, align = 'center', shiny::uiOutput('CatBoost_Methods'))),

              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth)

              )),

          # Evaluation Parameters
          shiny::column(
            width = 4L,
            tags$h4(tags$b('Evaluation Parameters')),
            shinyWidgets::dropdown(
              right = DropDownRight, up = TRUE, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'Cat_EvalParameters',
              tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('CatBoost Parameters'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_LossFunction')),
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_ClassWeights0')),
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_ClassWeights1'))),
              shiny::fluidRow(
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_EvalMetric')),
                shiny::column(3L, align = 'center', shiny::uiOutput('CatBoost_MetricPeriods'))),

              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth)

              ))),

        # Add space at bottom of box
        RemixAutoML:::BlankRow(AppWidth),

        # Build button
        RemixAutoML:::BuildModelsButton(
          id = 'MLBuildButton',
          Algo = 'CatBoost',
          AppWidth = parent.frame()$AppWidth,
          Style = 'gradient',
          Color = 'royal')) # end Catboost dropdown

      ))
}

#' @title ML_XGBoost
#'
#' @description XGBoost args
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id = 'XGBoost'
#' @param AppWidth = AppWidth
#' @param LogoWidth = LogoWidth
#' @param ButtonWidth = 3L
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'snow'
#'
#' @noRd
ML_XGBoost <- function(id='XGBoostML',
                        AppWidth=parent.frame()$AppWidth,
                        LogoWidth=parent.frame()$LogoWidth,
                        ButtonWidth=3L,
                        Style = 'gradient',
                        Color = 'royal',
                        Align='left',
                        DropDownRight=FALSE,
                        Animate=TRUE,
                        Status='custom',
                        H3Color = 'snow') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = ButtonWidth,
      align=Align,

      # Dropdown Button
      tags$h4(tags$b('XGBoost')),
      shinyWidgets::dropdown(
        right = DropDownRight, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'XGBoost',
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Select Inputs'))),
        shiny::fluidRow(
          width=AppWidth,
          shiny::column(1L, align = 'center'),
          shiny::column(4L, align = 'center', shiny::uiOutput('XGBoost_TargetType')),
          shiny::column(1L, align = 'center'),
          shiny::column(3L, align = 'center', shiny::uiOutput('XGBoost_Runs'))),

        # Parameters
        RemixAutoML:::BlankRow(AppWidth),
        shiny::fluidRow(

          # MetaData Parameters
          shiny::column(
            width = 4L,
            tags$h4(tags$b('Meta Data Parameters')),
            shinyWidgets::dropdown(
              right = DropDownRight, up = TRUE, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'XGB_MetaData',
              tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('XGBoost Parameters'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                shiny::column(3L, align = 'center', shiny::uiOutput('XGBoost_NThreads')),
                shiny::column(3L, align = 'center', shiny::uiOutput('XGBoost_TreeMethod')),
                shiny::column(3L, align = 'center', shiny::uiOutput('XGBoost_NumGPUs'))),
              shiny::fluidRow(
                shiny::column(3L, align = 'center', shiny::uiOutput('XGBoost_TrainOnFull')),
                shiny::column(3L, align = 'center', shiny::uiOutput('XGBoost_ModelID'))),

              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth)

              )),

          # Grid Tuning Parameters
          shiny::column(
            width = 4L,
            tags$h4(tags$b('Grid Tuning Parameters')),
            shinyWidgets::dropdown(
              right = DropDownRight, up = TRUE, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'XGB_GridTuningParameters',
              tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('XGBoost Parameters'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                shiny::column(3L, align = 'center', shiny::uiOutput('XGBoost_PassInGrid')),
                shiny::column(3L, align = 'center', shiny::uiOutput('XGBoost_GridTune')),
                shiny::column(3L, align = 'center', shiny::uiOutput('XGBoost_MaxModelsInGrid')),
                shiny::column(3L, align = 'center', shiny::uiOutput('XGBoost_grid_eval_metric'))),
              shiny::fluidRow(
                shiny::column(3L, align = 'center', shiny::uiOutput('XGBoost_MaxRunsWithoutNewWinner')),
                shiny::column(3L, align = 'center', shiny::uiOutput('XGBoost_MaxRunMinutes')),
                shiny::column(3L, align = 'center', shiny::uiOutput('XGBoost_BaselineComparison'))),

              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth)

              ))),

        # Parameters
        RemixAutoML:::BlankRow(AppWidth),
        shiny::fluidRow(

          # ML Algo Parameters
          shiny::column(
            width = 4L,
            tags$h4(tags$b('ML Parameters')),
            shinyWidgets::dropdown(
              right = DropDownRight, up = TRUE, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'XGB_MLParameters',
              tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('XGBoost Parameters'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                shiny::column(3L, align = 'center', shiny::uiOutput('XGBoost_Trees')),
                shiny::column(3L, align = 'center', shiny::uiOutput('XGBoost_max_depth')),
                shiny::column(3L, align = 'center', shiny::uiOutput('XGBoost_min_child_weight'))),
              shiny::fluidRow(
                shiny::column(3L, align = 'center', shiny::uiOutput('XGBoost_eta')),
                shiny::column(3L, align = 'center', shiny::uiOutput('XGBoost_subsample')),
                shiny::column(3L, align = 'center', shiny::uiOutput('XGBoost_colsample_bytree'))),

              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth)

              )),

          # Data Parameters
          shiny::column(
            width = 4L,
            tags$h4(tags$b('Data Parameters')),
            shinyWidgets::dropdown(
              right = DropDownRight, up = TRUE, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'XGB_DataParameters',
              tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('XGBoost Parameters'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                shiny::column(4L, align = 'center', shiny::uiOutput('XGBoost_data')),
                shiny::column(4L, align = 'center', shiny::uiOutput('XGBoost_ValidationData')),
                shiny::column(4L, align = 'center', shiny::uiOutput('XGBoost_TestData'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                shiny::column(4L, align = 'center', shiny::uiOutput('XGBoost_TargetColumnName')),
                shiny::column(4L, align = 'center', shiny::uiOutput('XGBoost_FeatureColNames')),
                shiny::column(4L, align = 'center', shiny::uiOutput('XGBoost_PrimaryDateColumn'))),
              shiny::fluidRow(
                shiny::column(4L, align = 'center', shiny::uiOutput('XGBoost_EncodeMethod')),
                shiny::column(4L, align = 'center', shiny::uiOutput('XGBoost_WeightsColumnName')),
                shiny::column(4L, align = 'center', shiny::uiOutput('XGBoost_IDcols'))),
              shiny::fluidRow(
                shiny::column(4L, align = 'center', shiny::uiOutput('XGBoost_TransformNumericColumns')),
                shiny::column(4L, align = 'center', shiny::uiOutput('XGBoost_Methods'))),

              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth)

              )),

          # Evaluation Parameters
          shiny::column(
            width = 4L,
            tags$h4(tags$b('Evaluation Parameters')),
            shinyWidgets::dropdown(
              right = DropDownRight, up = TRUE, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'XGB_EvalParameters',
              tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('XGBoost Parameters'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                shiny::column(4L, align = 'center', shiny::uiOutput('XGBoost_LossFunction')),
                shiny::column(4L, align = 'center', shiny::uiOutput('XGBoost_EvalMetric'))),

              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth)

              ))),

        # Add space at bottom of box
        RemixAutoML:::BlankRow(AppWidth),

        # Build button
        RemixAutoML:::BuildModelsButton(
          id = 'MLBuildButton',
          Algo = 'XGBoost',
          AppWidth = parent.frame()$AppWidth,
          Style = 'gradient',
          Color = 'royal')), # end XGboost dropdown

      # Add space at bottom of box
      RemixAutoML:::BlankRow(AppWidth),
      RemixAutoML:::BlankRow(AppWidth)

    ))
}

#' @title ML_LightGBM
#'
#' @description LightGBM args
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id = 'LightGBM'
#' @param AppWidth = AppWidth
#' @param LogoWidth = LogoWidth
#' @param ButtonWidth = 3L
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'snow'
#'
#' @noRd
ML_LightGBM <- function(id='LightGBMML',
                        AppWidth=parent.frame()$AppWidth,
                        LogoWidth=parent.frame()$LogoWidth,
                        ButtonWidth=3L,
                        Style = 'gradient',
                        Color = 'royal',
                        Align='left',
                        DropDownRight=TRUE,
                        Animate=TRUE,
                        Status='custom',
                        H3Color = 'snow') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = ButtonWidth,
      align=Align,

      # Dropdown Button
      tags$h4(tags$b('LightGBM')),
      shinyWidgets::dropdown(
        right = FALSE, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'LightGBM',
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Select Inputs'))),
        shiny::fluidRow(
          width=AppWidth,
          shiny::column(1L, align = 'center'),
          shiny::column(4L, align = 'center', shiny::uiOutput('LightGBM_TargetType')),
          shiny::column(1L, align = 'center'),
          shiny::column(3L, align = 'center', shiny::uiOutput('LightGBM_Runs'))),

        # Parameters
        RemixAutoML:::BlankRow(AppWidth),
        shiny::fluidRow(

          # MetaData Parameters
          shiny::column(
            width = 4L,
            tags$h4(tags$b('Meta Data Parameters')),
            shinyWidgets::dropdown(
              right = FALSE, up = TRUE, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'LGBM_MetaData',
              tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('LightGBM Parameters'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                shiny::column(3L, align = 'center', shiny::uiOutput('LightGBM_NThreads')),
                shiny::column(3L, align = 'center', shiny::uiOutput('LightGBM_device_type')),
                shiny::column(3L, align = 'center', shiny::uiOutput('LightGBM_NumGPUs'))),
              shiny::fluidRow(
                shiny::column(3L, align = 'center', shiny::uiOutput('LightGBM_TrainOnFull')),
                shiny::column(3L, align = 'center', shiny::uiOutput('LightGBM_ModelID'))),

              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth)

              )),

          # Grid Tuning Parameters
          shiny::column(
            width = 4L,
            tags$h4(tags$b('Grid Tuning Parameters')),
            shinyWidgets::dropdown(
              right = TRUE, up = TRUE, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'LGBM_GridTuningParameters',
              tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('LightGBM Parameters'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                shiny::column(3L, align = 'center', shiny::uiOutput('LightGBM_PassInGrid')),
                shiny::column(3L, align = 'center', shiny::uiOutput('LightGBM_GridTune')),
                shiny::column(3L, align = 'center', shiny::uiOutput('LightGBM_MaxModelsInGrid'))),
              shiny::fluidRow(
                shiny::column(3L, align = 'center', shiny::uiOutput('LightGBM_MaxRunsWithoutNewWinner')),
                shiny::column(3L, align = 'center', shiny::uiOutput('LightGBM_MaxRunMinutes')),
                shiny::column(3L, align = 'center', shiny::uiOutput('LightGBM_BaselineComparison'))),

              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth)

              ))),

        # Parameters
        RemixAutoML:::BlankRow(AppWidth),
        shiny::fluidRow(

          # ML Algo Parameters
          shiny::column(
            width = 4L,
            tags$h4(tags$b('ML Parameters')),
            shinyWidgets::dropdown(
              right = FALSE, up = TRUE, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'LGBM_MLParameters',
              tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('LightGBM Parameters'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                shiny::column(3L, align = 'center', shiny::uiOutput('LightGBM_Trees')),
                shiny::column(3L, align = 'center', shiny::uiOutput('LightGBM_max_depth')),
                shiny::column(3L, align = 'center', shiny::uiOutput('LightGBM_eta')),
                shiny::column(3L, align = 'center', shiny::uiOutput('LightGBM_lambda_l2'))),
              shiny::fluidRow(
                shiny::column(3L, align = 'center', shiny::uiOutput('LightGBM_num_leaves')),
                shiny::column(3L, align = 'center', shiny::uiOutput('LightGBM_min_data_in_leaf')),
                shiny::column(3L, align = 'center', shiny::uiOutput('LightGBM_bagging_fraction')),
                shiny::column(3L, align = 'center', shiny::uiOutput('LightGBM_lambda_l1'))),
              shiny::fluidRow(
                shiny::column(3L, align = 'center', shiny::uiOutput('LightGBM_feature_fraction')),
                shiny::column(3L, align = 'center', shiny::uiOutput('LightGBM_feature_fraction_bynode'))),

              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth)

              )),

          # Data Parameters
          shiny::column(
            width = 4L,
            tags$h4(tags$b('Data Parameters')),
            shinyWidgets::dropdown(
              right = TRUE, up = TRUE, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'LGBM_DataParameters',
              tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('LightGBM Parameters'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                shiny::column(4L, align = 'center', shiny::uiOutput('LightGBM_data')),
                shiny::column(4L, align = 'center', shiny::uiOutput('LightGBM_ValidationData')),
                shiny::column(4L, align = 'center', shiny::uiOutput('LightGBM_TestData'))),
              shiny::fluidRow(
                shiny::column(4L, align = 'center', shiny::uiOutput('LightGBM_TargetColumnName')),
                shiny::column(4L, align = 'center', shiny::uiOutput('LightGBM_FeatureColNames')),
                shiny::column(4L, align = 'center', shiny::uiOutput('LightGBM_PrimaryDateColumn'))),
              shiny::fluidRow(
                shiny::column(4L, align = 'center', shiny::uiOutput('LightGBM_EncodeMethod')),
                shiny::column(4L, align = 'center', shiny::uiOutput('LightGBM_WeightsColumnName')),
                shiny::column(4L, align = 'center', shiny::uiOutput('LightGBM_IDcols'))),
              shiny::fluidRow(
                shiny::column(6L, align = 'center', shiny::uiOutput('LightGBM_TransformNumericColumns')),
                shiny::column(4L, align = 'center', shiny::uiOutput('LightGBM_Methods'))),

              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth)

              )),

          # Evaluation Parameters
          shiny::column(
            width = 4L,
            tags$h4(tags$b('Evaluation Parameters')),
            shinyWidgets::dropdown(
              right = TRUE, up = TRUE, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = 'LGBM_EvalParameters',
              tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('LightGBM Parameters'))),
              RemixAutoML:::BlankRow(AppWidth),
              shiny::fluidRow(
                shiny::column(3L, align = 'center', shiny::uiOutput('LightGBM_LossFunction')),
                shiny::column(3L, align = 'center', shiny::uiOutput('LightGBM_ClassWeights0')),
                shiny::column(3L, align = 'center', shiny::uiOutput('LightGBM_ClassWeights1'))),
              shiny::fluidRow(
                shiny::column(3L, align = 'center', shiny::uiOutput('LightGBM_EvalMetric')),
                shiny::column(3L, align = 'center', shiny::uiOutput('LightGBM_MetricPeriods'))),

              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth),
              RemixAutoML:::BlankRow(AppWidth)

              ))),

        # Add space at bottom of box
        RemixAutoML:::BlankRow(AppWidth),

        # Build button
        RemixAutoML:::BuildModelsButton(
          id = 'MLBuildButton',
          Algo = 'LightGBM',
          AppWidth = parent.frame()$AppWidth,
          Style = 'gradient',
          Color = 'royal')), # end LightGBM dropdown

      # Add space at bottom of box
      RemixAutoML:::BlankRow(AppWidth),
      RemixAutoML:::BlankRow(AppWidth)

    ))
}

#' @title BuildModelsButton
#'
#' @description BuildModelsButton is a button for kicking off ML
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id = 'MLBuildButton'
#' @param Algo = 'CatBoost'
#' @param AppWidth = AppWidth
#' @param Style = 'gradient'
#' @param Color = 'royal'
#'
#' @noRd
BuildModelsButton <- function(id = 'MLBuildButton',
                              Algo = 'CatBoost',
                              AppWidth = parent.frame()$AppWidth,
                              Style = 'gradient',
                              Color = 'royal')  {
  ns <- shiny::NS(id)
  shiny::tagList(
    RemixAutoML:::BlankRow(AppWidth),
    shiny::fluidRow(
      shiny::column(
        width = 4L,
        shinyjs::useShinyjs(),
        align = 'center',
        shinyWidgets::actionBttn(
          inputId = paste0('BuildModels_', Algo),
          label = paste0('Build ', Algo),
          icon = shiny::icon('chevron-right', lib='font-awesome'),
          style = Style,
          color = Color))),
    RemixAutoML:::BlankRow(AppWidth))
}

#' @title PlotDropDownContents
#'
#' @description Display a plot dropdown button with a formatted input box for YVar, XVar, GroupVars, Levels1, Levels2, Levels3, FacetVar1, FacetVar2, SizeVar
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id = 'PlotDropDown'
#' @param PlotNumber = 1L
#' @param AppWidth = AppWidth
#' @param LogoWidth = LogoWidth
#' @param ButtonWidth = 3L
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = '#0088a7'
#'
#' @noRd
PlotDropDownContents <- function(id,
                                 PlotNumber = 1,
                                 AppWidth=parent.frame()$AppWidth,
                                 LogoWidth=parent.frame()$LogoWidth,
                                 ButtonWidth=3L,
                                 Align='center',
                                 DropDownRight=FALSE,
                                 Animate=TRUE,
                                 Status='custom',
                                 H3Color = '#0088a7') {

  Height <- "85px"
  if(PlotNumber > 2) DropDownRight <- TRUE
  ns <- shiny::NS(id)
  shiny::tagList(

    # Need this, otherwise buttons consume entire row and thus end up creating 4 rows
    # also get 'centering' out of it
    shiny::column(
      width = 3L,
      align = 'center',

      # Dropdown Button
      tags$h4(tags$b(paste0('Plot ', PlotNumber))),
      shinyWidgets::dropdown(
        right = DropDownRight, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = paste0('PlotDropDown', PlotNumber),

        # Select Data Box
        RemixAutoML:::BlankRow(AppWidth),
        shiny::fluidRow(
          shinydashboard::box(
            title = NULL, solidHeader = TRUE, collapsible = FALSE, status = 'warning', width = AppWidth,
            shiny::fluidRow(shiny::column(width=12L, align = 'center',tags$h3(tags$span(style=paste0('color: ', H3Color, ';'), 'Data Selection')))),
            RemixAutoML:::BlankRow(AppWidth),
            shiny::fluidRow(
              shiny::column(1L, align = 'center'),
              shiny::column(5L, align = 'center', shiny::uiOutput(paste0('Plot', PlotNumber, '_SelectData'))),
              shiny::column(1L, align = 'center'),
              shiny::column(4L, shiny::uiOutput(paste0('Plot', PlotNumber))))

          ) # end box
        ), # end fluid row

        # Tabs
        shiny::tabsetPanel(
          id = paste0('PlotTabs', PlotNumber),
          selected = 'Standard Plots',

          # Standard Plots ----
          shiny::tabPanel(
            title = "Standard Plots",
            icon = shiny::icon('code'),

            # Basic Plots
            shiny::fluidRow(
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = NULL, width = AppWidth,
                RemixAutoML:::BlankRow(AppWidth),

                # Distributions
                shiny::fluidRow(
                  shiny::column(
                    tags$h4(tags$b(paste0('Box Plot'))), width=4L, align = 'center',
                    tags$button(
                      id = paste0('BoxPlot_MenuButton', PlotNumber),
                      type = "button", class = "btn btn-default action-button",
                      tags$img(src = "https://github.com/AdrianAntico/RemixAutoML/blob/master/Images/Box.PNG?raw=true", height = Height),
                      `data-val` = shiny::restoreInput(id = paste0('BoxPlot_MenuButton', PlotNumber), default = NULL))),
                  shiny::column(
                    tags$h4(tags$b(paste0('Histogram'))), width=4L, align = 'center',
                    tags$button(
                      id = paste0('HistogramPlot_MenuButton', PlotNumber),
                      type = "button", class = "btn btn-default action-button",
                      tags$img(src = "https://github.com/AdrianAntico/RemixAutoML/blob/master/Images/Histogram.PNG?raw=true", height = Height),
                      `data-val` = shiny::restoreInput(id = paste0('HistogramPlot_MenuButton', PlotNumber), default = NULL))),
                  shiny::column(
                    tags$h4(tags$b(paste0('Violin Plot Plot'))), width=4L, align = 'center',
                    tags$button(
                      id = paste0('ViolinPlot_MenuButton', PlotNumber),
                      type = "button", class = "btn btn-default action-button",
                      tags$img(src = "https://github.com/AdrianAntico/RemixAutoML/blob/master/Images/Violin.PNG?raw=true", height = Height),
                      `data-val` = shiny::restoreInput(id = paste0('ViolinPlot_MenuButton', PlotNumber), default = NULL)))),

                RemixAutoML:::BlankRow(AppWidth),

                # Standard Plots
                shiny::fluidRow(
                  shiny::column(
                    tags$h4(tags$b(paste0('Bar Plot'))), width = 4L, align = 'center',
                    tags$button(
                      id = paste0('BarPlot_MenuButton', PlotNumber),
                      type = "button", class = "btn btn-default action-button",
                      tags$img(src = "https://github.com/AdrianAntico/RemixAutoML/blob/master/Images/Bar.PNG?raw=true", height = Height),
                      `data-val` = shiny::restoreInput(id = paste0('BarPlot_MenuButton', PlotNumber), default = NULL))),
                  shiny::column(
                    tags$h4(tags$b(paste0('Line Plot'))), width = 4L, align = 'center',
                    tags$button(
                      id = paste0('LinePlot_MenuButton', PlotNumber),
                      type = "button", class = "btn btn-default action-button",
                      tags$img(src = "https://github.com/AdrianAntico/RemixAutoML/blob/master/Images/Line.PNG?raw=true", height = Height),
                      `data-val` = shiny::restoreInput(id = paste0('LinePlot_MenuButton', PlotNumber), default = NULL))),
                  shiny::column(
                    tags$h4(tags$b(paste0('Pearson Scatter'))), width = 4L, align = 'center',
                    tags$button(
                      id = paste0('ScatterPlot_MenuButton', PlotNumber),
                      type = "button", class = "btn btn-default action-button",
                      tags$img(src = "https://github.com/AdrianAntico/RemixAutoML/blob/master/Images/Scatter.PNG?raw=true", height = Height),
                      `data-val` = shiny::restoreInput(id = paste0('ScatterPlot_MenuButton', PlotNumber), default = NULL)))),

                RemixAutoML:::BlankRow(AppWidth),

                # Additional Plots
                shiny::fluidRow(
                  shiny::column(
                    tags$h4(tags$b(paste0('Spearman Scatter'))), width = 4L, align = 'center',
                    tags$button(
                      id = paste0('CopulaPlot_MenuButton', PlotNumber),
                      type = "button", class = "btn btn-default action-button",
                      tags$img(src = "https://github.com/AdrianAntico/RemixAutoML/blob/master/Images/Copula.PNG?raw=true", height = Height),
                      `data-val` = shiny::restoreInput(id = paste0('CopulaPlot_MenuButton', PlotNumber), default = NULL))),
                  shiny::column(
                    tags$h4(tags$b(paste0('Heat Map'))), width = 4L, align = 'center',
                    tags$button(
                      id = paste0('HeatMapPlot_MenuButton', PlotNumber),
                      type = "button", class = "btn btn-default action-button",
                      tags$img(src = "https://github.com/AdrianAntico/RemixAutoML/blob/master/Images/HeatMap2.PNG?raw=true", height = Height),
                      `data-val` = shiny::restoreInput(id = paste0('HeatMapPlot_MenuButton', PlotNumber), default = NULL))),
                  shiny::column(
                    tags$h4(tags$b(paste0('Correlogram'))), width = 4L, align = 'center',
                    tags$button(
                      id = paste0('CorrelogramPlot_MenuButton', PlotNumber),
                      type = "button", class = "btn btn-default action-button",
                      tags$img(src = "https://github.com/AdrianAntico/RemixAutoML/blob/master/Images/Correlogram.PNG?raw=true", height = Height),
                      `data-val` = shiny::restoreInput(id = paste0('CorrelogramPlot_MenuButton', PlotNumber), default = NULL)))),

                RemixAutoML:::BlankRow(AppWidth),

              ) # end box
            ) # end fluid row
          ), # end tabPanel for Standard Plots

          # Model Eval Tab ----
          shiny::tabPanel(

            id = 'ModelEvalTab',

            # -- TAB REFERENCE VALUE
            # tabName = "CodePrint",
            title = "Model Eval",
            icon = shiny::icon('code'),

            # Model Eval Plots
            shiny::fluidRow(
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = NULL, width = AppWidth,
                RemixAutoML:::BlankRow(AppWidth),

                # Model Evaluation Plots
                shiny::fluidRow(
                  shiny::column(
                    tags$h4(tags$b(tags$span(style=paste0('color: snow;'), 'Residuals Histogram'))), width = 4L, align = 'center',
                    tags$button(
                      id = paste0('ResidualsHistogram_MenuButton', PlotNumber),
                      type = "button", class = "btn btn-default action-button",
                      tags$img(src = "https://github.com/AdrianAntico/RemixAutoML/blob/master/Images/ResidualsHistogram.PNG?raw=true", height = Height),
                      `data-val` = shiny::restoreInput(id = paste0('ResidualsHistogram_MenuButton', PlotNumber), default = NULL))),
                  shiny::column(
                    tags$h4(tags$b(tags$span(style=paste0('color: snow;'), 'Calibration Line'))), width = 4L, align = 'center',
                    tags$button(
                      id = paste0('CalibrationLine_MenuButton', PlotNumber),
                      type = "button", class = "btn btn-default action-button",
                      tags$img(src = "https://github.com/AdrianAntico/RemixAutoML/blob/master/Images/PartialDependenceLine.PNG?raw=true", height = Height),
                      `data-val` = shiny::restoreInput(id = paste0('CalibrationLine_MenuButton', PlotNumber), default = NULL))),
                  shiny::column(
                    tags$h4(tags$b(tags$span(style=paste0('color: snow;'), 'Calibration Box'))), width = 4L, align = 'center',
                    tags$button(
                      id = paste0('CalibrationBox_MenuButton', PlotNumber),
                      type = "button", class = "btn btn-default action-button",
                      tags$img(src = "https://github.com/AdrianAntico/RemixAutoML/blob/master/Images/PartialDependenceBox.PNG?raw=true", height = Height),
                      `data-val` = shiny::restoreInput(id = paste0('CalibrationBox_MenuButton', PlotNumber), default = NULL)))),

                RemixAutoML:::BlankRow(AppWidth),

                # Partial Dependence Plots
                shiny::fluidRow(
                  shiny::column(
                    tags$h4(tags$b(tags$span(style=paste0('color: snow;'), 'Actual v Predicted'))), width = 4L, align = 'center',
                    tags$button(
                      id = paste0("ResidualsScatterPlot_MenuButton", PlotNumber),
                      type = "button", class = "btn btn-default action-button",
                      tags$img(src = "https://github.com/AdrianAntico/RemixAutoML/blob/master/Images/Scatter.PNG?raw=true", height = Height),
                      `data-val` = shiny::restoreInput(id = paste0("ResidualsScatterPlot_MenuButton", PlotNumber), default = NULL))),
                  shiny::column(
                    tags$h4(tags$b(tags$span(style=paste0('color: snow;'), 'Partial Dependence Line'))), width = 4L, align = 'center',
                    tags$button(
                      id = paste0('PartialDependenceLine_MenuButton', PlotNumber),
                      type = "button", class = "btn btn-default action-button",
                      tags$img(src = "https://github.com/AdrianAntico/RemixAutoML/blob/master/Images/PartialDependenceLine.PNG?raw=true", height = Height),
                      `data-val` = shiny::restoreInput(id = paste0('PartialDependenceLine_MenuButton', PlotNumber), default = NULL))),
                  shiny::column(
                    tags$h4(tags$b(tags$span(style=paste0('color: snow;'), 'Partial Dependence Box'))), width = 4L, align = 'center',
                    tags$button(
                      id = paste0("PartialDependenceBox_MenuButton", PlotNumber),
                      type = "button", class = "btn btn-default action-button",
                      tags$img(src = "https://github.com/AdrianAntico/RemixAutoML/blob/master/Images/PartialDependenceBox.PNG?raw=true", height = Height),
                      `data-val` = shiny::restoreInput(id = paste0("PartialDependenceBox_MenuButton", PlotNumber), default = NULL)))),

                RemixAutoML:::BlankRow(AppWidth),

                # Var Imp
                shiny::fluidRow(
                  shiny::column(
                    tags$h4(tags$b(tags$span(style=paste0('color: snow;'), 'Variable Importance'))), width = 4L, align = 'center',
                    tags$button(
                      id = paste0("VariableImportance_MenuButton", PlotNumber),
                      type = "button", class = "btn btn-default action-button",
                      tags$img(src = "https://github.com/AdrianAntico/RemixAutoML/blob/master/Images/VariableImportance.PNG?raw=true", height = Height),
                      `data-val` = shiny::restoreInput(id = paste0("VariableImportance_MenuButton", PlotNumber), default = NULL))),
                  shiny::column(
                    tags$h4(tags$b(tags$span(style=paste0('color: snow;'), 'Shapely Importance'))), width = 4L, align = 'center',
                    tags$button(
                      id = paste0("ShapelyImportance_MenuButton", PlotNumber),
                      type = "button", class = "btn btn-default action-button",
                      tags$img(src = "https://github.com/AdrianAntico/RemixAutoML/blob/master/Images/VariableImportance.PNG?raw=true", height = Height),
                      `data-val` = shiny::restoreInput(id = paste0("ShapelyImportance_MenuButton", PlotNumber), default = NULL))),
                  shiny::column(
                    tags$h4(tags$b(tags$span(style=paste0('color: snow;'), 'Confusion Matrix'))), width = 4L, align = 'center',
                    tags$button(
                      id = paste0("ConfusionMatrix_MenuButton", PlotNumber),
                      type = "button", class = "btn btn-default action-button",
                      tags$img(src = "https://github.com/AdrianAntico/RemixAutoML/blob/master/Images/ConfusionMatrixHeatmap.PNG?raw=true", height = Height),
                      `data-val` = shiny::restoreInput(id = paste0("ConfusionMatrix_MenuButton", PlotNumber), default = NULL)))),

                RemixAutoML:::BlankRow(AppWidth),

                # Model Evaluation Classifier Plots
                shiny::fluidRow(
                  shiny::column(
                    tags$h4(tags$b(tags$span(style=paste0('color: snow;'), 'ROC Plot'))), width = 4L, align = 'center',
                    tags$button(
                      id = paste0("ROCPlot_MenuButton", PlotNumber),
                      type = "button", class = "btn btn-default action-button",
                      tags$img(src = "https://github.com/AdrianAntico/RemixAutoML/blob/master/Images/ROC.PNG?raw=true", height = Height),
                      `data-val` = shiny::restoreInput(id = paste0("ROCPlot_MenuButton", PlotNumber), default = NULL))),
                  shiny::column(
                    tags$h4(tags$b(tags$span(style=paste0('color: snow;'), 'Lift Plot'))), width = 4L, align = 'center',
                    tags$button(
                      id = paste0("LiftPlot_MenuButton", PlotNumber),
                      type = "button", class = "btn btn-default action-button",
                      tags$img(src = "https://github.com/AdrianAntico/RemixAutoML/blob/master/Images/Lift.PNG?raw=true", height = Height),
                      `data-val` = shiny::restoreInput(id = paste0("LiftPlot_MenuButton", PlotNumber), default = NULL))),
                  shiny::column(
                    tags$h4(tags$b(tags$span(style=paste0('color: snow;'), 'Gains Plot'))), width = 4L, align = 'center',
                    tags$button(
                      id = paste0("GainsPlot_MenuButton", PlotNumber),
                      type = "button", class = "btn btn-default action-button",
                      tags$img(src = "https://github.com/AdrianAntico/RemixAutoML/blob/master/Images/ROC.PNG?raw=true", height = Height),
                      `data-val` = shiny::restoreInput(id = paste0("GainsPlot_MenuButton", PlotNumber), default = NULL)))),

                RemixAutoML:::BlankRow(AppWidth),

              ) # end box
            ) # end fluid row
          ), # end tabPanel

          # Finance Tab ----
          shiny::tabPanel(

            id = 'FinanceTab',

            # -- TAB REFERENCE VALUE
            title = "Finance",
            icon = shiny::icon('code'),

            # Financial Plots
            shiny::fluidRow(
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = 'warning', width = AppWidth,
                RemixAutoML:::BlankRow(AppWidth),

                # Model Evaluation Plots
                shiny::fluidRow(
                  shiny::column(
                    tags$h4(tags$b(tags$span(style=paste0('color: snow;'), 'Candlestick Plot'))), width = 4L, align = 'center',
                    tags$button(
                      id = paste0('CandlestickPlot_MenuButton', PlotNumber),
                      type = "button", class = "btn btn-default action-button",
                      tags$img(src = "https://github.com/AdrianAntico/RemixAutoML/blob/master/Images/Candlestick.PNG?raw=true", height = Height),
                      `data-val` = shiny::restoreInput(id = paste0('CandlestickPlot_MenuButton', PlotNumber), default = NULL))),
                  shiny::column(
                    tags$h4(tags$b(tags$span(style=paste0('color: snow;'), 'OHCL Plot'))), width = 4L, align = 'center',
                    tags$button(
                      id = paste0('OHLCPlot_MenuButton', PlotNumber),
                      type = "button", class = "btn btn-default action-button",
                      tags$img(src = "https://github.com/AdrianAntico/RemixAutoML/blob/master/Images/OHLC.PNG?raw=true", height = Height),
                      `data-val` = shiny::restoreInput(id = paste0('OHLCPlot_MenuButton', PlotNumber), default = NULL)))),

                RemixAutoML:::BlankRow(AppWidth),

              ) # end box
            ) # end fluid row
          ) # end tabPanel
        ) # end tabsetPanel
      ) # end dropdown
    ) # end column
  ) # end tagList
}

#' @title DataFilters
#'
#' @description Display a dropdown button with filter variables and other filter info needed for server
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id = 'DataFilters'
#' @param PlotNumber = 1
#' @param AppWidth = AppWidth
#' @param LogoWidth = LogoWidth
#' @param H3Color = H3Color
#' @param H4Color = H4Color
#' @param Right = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#'
#' @noRd
DataFilters <- function(id='DataFiltersContents',
                        PlotNumber = 1,
                        AppWidth = parent.frame()$AppWidth,
                        LogoWidth = parent.frame()$LogoWidth,
                        H3Color = parent.frame()$H3Color,
                        H4Color = parent.frame()$H4Color,
                        Right = FALSE,
                        Animate = TRUE,
                        Status = 'custom') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 3L,
      tags$h4(tags$b(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Plot ', PlotNumber)))),
      align='center',
      shinyWidgets::dropdown(
        right = Right, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth, inputId = paste0('FilterPlotInputs', PlotNumber),
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Filters for Plot ', PlotNumber))),
        tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Subset data')),
        RemixAutoML:::BlankRow(AppWidth),
        shiny::fluidRow(
          shiny::column(3L, shiny::uiOutput(paste0('FilterVariable_', PlotNumber, '_1'))),
          shiny::column(3L, shiny::uiOutput(paste0('FilterVariable_', PlotNumber, '_2'))),
          shiny::column(3L, shiny::uiOutput(paste0('FilterVariable_', PlotNumber, '_3'))),
          shiny::column(3L, shiny::uiOutput(paste0('FilterVariable_', PlotNumber, '_4')))),
        shiny::fluidRow(
          shiny::column(3L, shiny::uiOutput(paste0('FilterLogic_', PlotNumber, '_1'))),
          shiny::column(3L, shiny::uiOutput(paste0('FilterLogic_', PlotNumber, '_2'))),
          shiny::column(3L, shiny::uiOutput(paste0('FilterLogic_', PlotNumber, '_3'))),
          shiny::column(3L, shiny::uiOutput(paste0('FilterLogic_', PlotNumber, '_4')))),
        shiny::fluidRow(
          shiny::column(3L, shiny::uiOutput(paste0('FilterValue_', PlotNumber, '_1_1'))),
          shiny::column(3L, shiny::uiOutput(paste0('FilterValue_', PlotNumber, '_2_1'))),
          shiny::column(3L, shiny::uiOutput(paste0('FilterValue_', PlotNumber, '_3_1'))),
          shiny::column(3L, shiny::uiOutput(paste0('FilterValue_', PlotNumber, '_4_1')))),
        shiny::fluidRow(
          shiny::column(3L, shiny::uiOutput(paste0('FilterValue_', PlotNumber, '_1_2'))),
          shiny::column(3L, shiny::uiOutput(paste0('FilterValue_', PlotNumber, '_2_2'))),
          shiny::column(3L, shiny::uiOutput(paste0('FilterValue_', PlotNumber, '_3_2'))),
          shiny::column(3L, shiny::uiOutput(paste0('FilterValue_', PlotNumber, '_4_2')))),

        # Add space so area underneath selected item remains inside the dropdown
        #   Otherwise, the dropdown closes prematurely
        RemixAutoML:::BlankRow(AppWidth),
        RemixAutoML:::BlankRow(AppWidth),
        RemixAutoML:::BlankRow(AppWidth),
        RemixAutoML:::BlankRow(AppWidth)

      ) # end dropdown
    ) # end column
  ) # end tagList
}

#' @title AxisLimits
#'
#' @description Display a dropdown button with axis limites
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id = 'AxisLimitsContents'
#' @param AppWidth = AppWidth
#' @param LogoWidth = LogoWidth
#' @param H3Color = H3Color
#' @param H4Color = H4Color
#' @param Right = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#'
#' @noRd
AxisLimits <- function(id = 'AxisLimitsContents',
                       AppWidth = parent.frame()$AppWidth,
                       LogoWidth = parent.frame()$LogoWidth,
                       H3Color = parent.frame()$H3Color,
                       H4Color = parent.frame()$H4Color,
                       Right = FALSE,
                       Animate = TRUE,
                       Status = 'custom') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 3L,
      tags$h4(tags$b(tags$span(style=paste0('color: ', H3Color, ';'), 'Axis Limits'))),
      shinyWidgets::dropdown(
        right = Right, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, inputId = "AxisLimitsInputs", width = LogoWidth,
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Plot Limits')),
        tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Alter the min and max limits for the Y and X Axes')),
        RemixAutoML:::BlankRow(AppWidth),
        shiny::fluidRow(
          shiny::column(3L, shiny::uiOutput('YLimMin1')),
          shiny::column(3L, shiny::uiOutput('YLimMax1')),
          shiny::column(3L, shiny::uiOutput('XLimMin1')),
          shiny::column(3L, shiny::uiOutput('XLimMax1'))),
        shiny::fluidRow(
          shiny::column(3L, shiny::uiOutput('YLimMin2')),
          shiny::column(3L, shiny::uiOutput('YLimMax2')),
          shiny::column(3L, shiny::uiOutput('XLimMin2')),
          shiny::column(3L, shiny::uiOutput('XLimMax2'))),
        shiny::fluidRow(
          shiny::column(3L, shiny::uiOutput('YLimMin3')),
          shiny::column(3L, shiny::uiOutput('YLimMax3')),
          shiny::column(3L, shiny::uiOutput('XLimMin3')),
          shiny::column(3L, shiny::uiOutput('XLimMax3'))),
        shiny::fluidRow(
          shiny::column(3L, shiny::uiOutput('YLimMin4')),
          shiny::column(3L, shiny::uiOutput('YLimMax4')),
          shiny::column(3L, shiny::uiOutput('XLimMin4')),
          shiny::column(3L, shiny::uiOutput('XLimMax4'))),

        RemixAutoML:::BlankRow(AppWidth),
        RemixAutoML:::BlankRow(AppWidth),
        RemixAutoML:::BlankRow(AppWidth),
        RemixAutoML:::BlankRow(AppWidth)

      ) # end dropdown
    ) # end column
  ) # end tagList
}

#' @title Formatting
#'
#' @description Display a dropdown button with Formatting
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id = 'AxisLimitsContents'
#' @param AppWidth = AppWidth
#' @param LogoWidth = LogoWidth
#' @param H3Color = H3Color
#' @param H4Color = H4Color
#' @param Right = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#'
#' @noRd
Formatting <- function(id = 'FormattingContents',
                       AppWidth = parent.frame()$AppWidth,
                       LogoWidth = parent.frame()$LogoWidth,
                       H3Color = parent.frame()$H3Color,
                       H4Color = parent.frame()$H4Color,
                       Right = FALSE,
                       Animate = TRUE,
                       Status = 'custom') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 3L,
      tags$h4(tags$b(tags$span(style=paste0('color: ', H3Color, ';'), 'Plot Structure'))),
      shinyWidgets::dropdown(
        right = Right, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, inputId = "PlotStructureInputs", width = LogoWidth,
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Plot Formatting')),
        tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'For color options, see Plot Colors')),
        RemixAutoML:::BlankRow(AppWidth),
        shiny::fluidRow(
          shiny::column(3L, shiny::uiOutput('AngleY1')),
          shiny::column(3L, shiny::uiOutput('AngleY2')),
          shiny::column(3L, shiny::uiOutput('AngleY3')),
          shiny::column(3L, shiny::uiOutput('AngleY4'))),
        shiny::fluidRow(
          shiny::column(3L, shiny::uiOutput('AngleX1')),
          shiny::column(3L, shiny::uiOutput('AngleX2')),
          shiny::column(3L, shiny::uiOutput('AngleX3')),
          shiny::column(3L, shiny::uiOutput('AngleX4'))),
        shiny::fluidRow(
          shiny::column(3L, shiny::uiOutput('YTicks1')),
          shiny::column(3L, shiny::uiOutput('YTicks2')),
          shiny::column(3L, shiny::uiOutput('YTicks3')),
          shiny::column(3L, shiny::uiOutput('YTicks4'))),
        shiny::fluidRow(
          shiny::column(3L, shiny::uiOutput('XTicks1')),
          shiny::column(3L, shiny::uiOutput('XTicks2')),
          shiny::column(3L, shiny::uiOutput('XTicks3')),
          shiny::column(3L, shiny::uiOutput('XTicks4'))),
        shiny::fluidRow(
          shiny::column(3L, shiny::uiOutput('TextSize1')),
          shiny::column(3L, shiny::uiOutput('TextSize2')),
          shiny::column(3L, shiny::uiOutput('TextSize3')),
          shiny::column(3L, shiny::uiOutput('TextSize4'))),
        shiny::fluidRow(
          shiny::column(3L, shiny::uiOutput('OutlierSize1')),
          shiny::column(3L, shiny::uiOutput('OutlierSize2')),
          shiny::column(3L, shiny::uiOutput('OutlierSize3')),
          shiny::column(3L, shiny::uiOutput('OutlierSize4'))),

        # Add space so area underneath selected item remains inside the dropdown
        #   Otherwise, the dropdown closes prematurely
        RemixAutoML:::BlankRow(AppWidth),
        RemixAutoML:::BlankRow(AppWidth),
        RemixAutoML:::BlankRow(AppWidth),
        RemixAutoML:::BlankRow(AppWidth),
        RemixAutoML:::BlankRow(AppWidth),
        RemixAutoML:::BlankRow(AppWidth),
        RemixAutoML:::BlankRow(AppWidth)

      ) # end dropdown
    ) # end column
  ) # end tagList
}

#' @title Coloring
#'
#' @description Display a dropdown button with Coloring
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id = 'AxisLimitsContents'
#' @param AppWidth = AppWidth
#' @param LogoWidth = LogoWidth
#' @param H3Color = H3Color
#' @param H4Color = H4Color
#' @param Right = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#'
#' @noRd
Coloring <- function(id = 'ColoringContents',
                     AppWidth = parent.frame()$AppWidth,
                     LogoWidth = parent.frame()$LogoWidth,
                     H3Color = parent.frame()$H3Color,
                     H4Color = parent.frame()$H4Color,
                     Right = FALSE,
                     Animate = TRUE,
                     Status = 'custom') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 3L,
      tags$h4(tags$b(tags$span(style=paste0('color: ', H3Color, ';'), 'Plot Colors'))),
      shinyWidgets::dropdown(
        right = Right, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, inputId = "PlotColoringInputs", width = LogoWidth,
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Plot Coloring')),
        RemixAutoML:::BlankRow(AppWidth),
        shiny::fluidRow(
          width = AppWidth,
          shiny::column(3L, shiny::uiOutput('BackGroundColor1')),
          shiny::column(3L, shiny::uiOutput('BackGroundColor2')),
          shiny::column(3L, shiny::uiOutput('BackGroundColor3')),
          shiny::column(3L, shiny::uiOutput('BackGroundColor4'))),
        shiny::fluidRow(
          width = AppWidth,
          shiny::column(3L, shiny::uiOutput('SubTitleColor1')),
          shiny::column(3L, shiny::uiOutput('SubTitleColor2')),
          shiny::column(3L, shiny::uiOutput('SubTitleColor3')),
          shiny::column(3L, shiny::uiOutput('SubTitleColor4'))),
        shiny::fluidRow(
          width = AppWidth,
          shiny::column(3L, shiny::uiOutput('ChartColor1')),
          shiny::column(3L, shiny::uiOutput('ChartColor2')),
          shiny::column(3L, shiny::uiOutput('ChartColor3')),
          shiny::column(3L, shiny::uiOutput('ChartColor4'))),
        shiny::fluidRow(
          width = AppWidth,
          shiny::column(3L, shiny::uiOutput('BorderColor1')),
          shiny::column(3L, shiny::uiOutput('BorderColor2')),
          shiny::column(3L, shiny::uiOutput('BorderColor3')),
          shiny::column(3L, shiny::uiOutput('BorderColor4'))),
        shiny::fluidRow(
          width = AppWidth,
          shiny::column(3L, shiny::uiOutput('TextColor1')),
          shiny::column(3L, shiny::uiOutput('TextColor2')),
          shiny::column(3L, shiny::uiOutput('TextColor3')),
          shiny::column(3L, shiny::uiOutput('TextColor4'))),
        shiny::fluidRow(
          width = AppWidth,
          shiny::column(3L, shiny::uiOutput('FillColor1')),
          shiny::column(3L, shiny::uiOutput('FillColor2')),
          shiny::column(3L, shiny::uiOutput('FillColor3')),
          shiny::column(3L, shiny::uiOutput('FillColor4'))),
        shiny::fluidRow(
          width = AppWidth,
          shiny::column(3L, shiny::uiOutput('GridColor1')),
          shiny::column(3L, shiny::uiOutput('GridColor2')),
          shiny::column(3L, shiny::uiOutput('GridColor3')),
          shiny::column(3L, shiny::uiOutput('GridColor4'))),

        # Add space so area underneath selected item remains inside the dropdown
        #   Otherwise, the dropdown closes prematurely
        RemixAutoML:::BlankRow(AppWidth),
        RemixAutoML:::BlankRow(AppWidth),
        RemixAutoML:::BlankRow(AppWidth),
        RemixAutoML:::BlankRow(AppWidth)

      ) # end dropdown
    ) # end column
  ) # end tagList
}

#' @title GamFitting
#'
#' @description Display a dropdown button with GamFitting
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id = 'AxisLimitsContents'
#' @param AppWidth = AppWidth
#' @param LogoWidth = LogoWidth
#' @param H3Color = H3Color
#' @param H4Color = H4Color
#' @param Right = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#'
#' @noRd
GamFitting <- function(id = 'GamFittingContents',
                     AppWidth = parent.frame()$AppWidth,
                     LogoWidth = parent.frame()$LogoWidth,
                     H3Color = parent.frame()$H3Color,
                     H4Color = parent.frame()$H4Color,
                     Right = FALSE,
                     Animate = TRUE,
                     Status = 'custom') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 3L,
      tags$h4(tags$b(tags$span(style=paste0('color: ', H3Color, ';'), 'GAM Line'))),
      shinyWidgets::dropdown(
        right = Right, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, inputId = "GamLineInputs", width = LogoWidth,
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'GAM Regression Lines')),
        tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Add a generalized additive model regression fit to the data')),
        RemixAutoML:::BlankRow(AppWidth),
        shiny::fluidRow(
          width = AppWidth,
          shiny::column(3L, shiny::uiOutput('GamFitScatter1')),
          shiny::column(3L, shiny::uiOutput('GamFitScatter2')),
          shiny::column(3L, shiny::uiOutput('GamFitScatter3')),
          shiny::column(3L, shiny::uiOutput('GamFitScatter4'))),

        # Add space so area underneath selected item remains inside the dropdown
        #   Otherwise, the dropdown closes prematurely
        RemixAutoML:::BlankRow(AppWidth),
        RemixAutoML:::BlankRow(AppWidth),
        RemixAutoML:::BlankRow(AppWidth),
        RemixAutoML:::BlankRow(AppWidth)

      ) # end dropdown
    ) # end column
  ) # end tagList
}

#' @title HistBins
#'
#' @description Display a dropdown button with HistBins
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id = 'AxisLimitsContents'
#' @param AppWidth = AppWidth
#' @param LogoWidth = LogoWidth
#' @param H3Color = H3Color
#' @param H4Color = H4Color
#' @param Right = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#'
#' @noRd
HistBins <- function(id = 'HistBinsContents',
                       AppWidth = parent.frame()$AppWidth,
                       LogoWidth = parent.frame()$LogoWidth,
                       H3Color = parent.frame()$H3Color,
                       H4Color = parent.frame()$H4Color,
                       Right = FALSE,
                       Animate = TRUE,
                       Status = 'custom') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 3L,
      tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), tags$b('Hist Bins'))),
      shinyWidgets::dropdown(
        right = Right, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, inputId = "HistBinsInputs", width = LogoWidth,
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Histogram # Bins')),
        tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Choose the number of bins for the histogram')),
        RemixAutoML:::BlankRow(AppWidth),
        shiny::fluidRow(
          width = AppWidth,
          shiny::column(3L, shiny::uiOutput('NumberBins1')),
          shiny::column(3L, shiny::uiOutput('NumberBins2')),
          shiny::column(3L, shiny::uiOutput('NumberBins3')),
          shiny::column(3L, shiny::uiOutput('NumberBins4'))),

        # Add space so area underneath selected item remains inside the dropdown
        #   Otherwise, the dropdown closes prematurely
        RemixAutoML:::BlankRow(AppWidth),
        RemixAutoML:::BlankRow(AppWidth),
        RemixAutoML:::BlankRow(AppWidth),
        RemixAutoML:::BlankRow(AppWidth)

      ) # end dropdown
    ) # end column
  ) # end tagList
}

#' @title PercentileBuckets
#'
#' @description Display a dropdown button with PercentileBuckets
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id = 'AxisLimitsContents'
#' @param AppWidth = AppWidth
#' @param LogoWidth = LogoWidth
#' @param H3Color = H3Color
#' @param H4Color = H4Color
#' @param Right = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#'
#' @noRd
PercentileBuckets <- function(id = 'PercentileBucketsContents',
                     AppWidth = parent.frame()$AppWidth,
                     LogoWidth = parent.frame()$LogoWidth,
                     H3Color = parent.frame()$H3Color,
                     H4Color = parent.frame()$H4Color,
                     Right = FALSE,
                     Animate = TRUE,
                     Status = 'custom') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 3L,
      tags$h4(tags$b(tags$span(style=paste0('color: ', H3Color, ';'), 'Eval Bins'))),
      shinyWidgets::dropdown(
        right = Right, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, inputId = "PercBinsInputs", width = LogoWidth,
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Calibration Plot Bins')),
        tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Choose the number of bins for the relevant model output plots')),
        RemixAutoML:::BlankRow(AppWidth),
        shiny::fluidRow(
          width = AppWidth,
          shiny::column(3L, shiny::uiOutput('Percentile_Buckets1')),
          shiny::column(3L, shiny::uiOutput('Percentile_Buckets2')),
          shiny::column(3L, shiny::uiOutput('Percentile_Buckets3')),
          shiny::column(3L, shiny::uiOutput('Percentile_Buckets4'))),

        # Add space so area underneath selected item remains inside the dropdown
        #   Otherwise, the dropdown closes prematurely
        RemixAutoML:::BlankRow(AppWidth),
        RemixAutoML:::BlankRow(AppWidth),
        RemixAutoML:::BlankRow(AppWidth),
        RemixAutoML:::BlankRow(AppWidth)

      ) # end dropdown
    ) # end column
  ) # end tagList
}

#' @title ShapAgg
#'
#' @description Display a dropdown button with ShapAgg
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id = 'AxisLimitsContents'
#' @param AppWidth = AppWidth
#' @param LogoWidth = LogoWidth
#' @param H3Color = H3Color
#' @param H4Color = H4Color
#' @param Right = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#'
#' @noRd
ShapAgg <- function(id = 'ShapAggContents',
                    AppWidth = parent.frame()$AppWidth,
                    LogoWidth = parent.frame()$LogoWidth,
                    H3Color = parent.frame()$H3Color,
                    H4Color = parent.frame()$H4Color,
                    Right = FALSE,
                    Animate = TRUE,
                    Status = 'custom') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 3L,
      tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), tags$b('Shapley Agg'))),
      shinyWidgets::dropdown(
        right = Right, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, inputId = "ShapAggInputs", width = LogoWidth,
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Calibration Plot Bins')),
        tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Choose the number of bins for the relevant model output plots')),
        RemixAutoML:::BlankRow(AppWidth),
        shiny::fluidRow(
          width = AppWidth,
          shiny::column(3L, shiny::uiOutput('ShapAggMethod1')),
          shiny::column(3L, shiny::uiOutput('ShapAggMethod2')),
          shiny::column(3L, shiny::uiOutput('ShapAggMethod3')),
          shiny::column(3L, shiny::uiOutput('ShapAggMethod4'))),

        # Add space so area underneath selected item remains inside the dropdown
        #   Otherwise, the dropdown closes prematurely
        RemixAutoML:::BlankRow(AppWidth),
        RemixAutoML:::BlankRow(AppWidth),
        RemixAutoML:::BlankRow(AppWidth),
        RemixAutoML:::BlankRow(AppWidth)

      ) # end dropdown
    ) # end column
  ) # end tagList
}

#' @title Plotter
#'
#' @description Replaces shiny::plotOutput and plotly::plotlyOutput
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id = 'PlotOutput'
#' @param AppWidth Width
#' @param PlotWidth width
#' @param PlotHeight height
#'
#' @noRd
Plotter <- function(id = 'PlotOutput',
                    AppWidth = 12L,
                    PlotWidth = 1100,
                    PlotHeight = 600) {
  ns <- shiny::NS(id)
  shiny::tagList(
    RemixAutoML:::BlankRow(AppWidth),
    shiny::fluidRow(
      shiny::column(
        width = AppWidth,
        align = 'center',
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(
            'TrendPlotly',
            width = PlotWidth,
            height = PlotHeight)))))
}

#' @title MarginalInputs
#'
#' @description Display a dropdown button with MarginalInputs
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id = 'MarginalContents'
#' @param AppWidth = AppWidth
#' @param LogoWidth = LogoWidth
#' @param H3Color = H3Color
#' @param H4Color = H4Color
#' @param Right = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#'
#' @noRd
MarginalInputs <- function(id='MarginalContents',
                           AppWidth=parent.frame()$AppWidth,
                           LogoWidth=parent.frame()$LogoWidth,
                           H3Color=parent.frame()$H3Color,
                           H4Color=parent.frame()$H4Color,
                           Right=TRUE,
                           Animate=TRUE,
                           Status='custom') {

  ns <- shiny::NS(id)
  shiny::tagList(

    # Marginals On / Off
    RemixAutoML:::BlankRow(AppWidth),
    shiny::column(
      width = 3L,
      tags$h4(tags$b(tags$span(style=paste0('color: ', H3Color, ';'), 'Marginal Plots'))),
      shinyWidgets::dropdown(
        right = Right, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, inputId = "MarginalOnOff", width = LogoWidth,
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Add Marginal Plots')),
        tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Add marginal plots for scatter and copula plots')),
        RemixAutoML:::BlankRow(AppWidth),
        shiny::fluidRow(
          width = AppWidth,
          shiny::column(3L, shiny::uiOutput('Marginals1')),
          shiny::column(3L, shiny::uiOutput('Marginals2')),
          shiny::column(3L, shiny::uiOutput('Marginals3')),
          shiny::column(3L, shiny::uiOutput('Marginals4'))))),

    # Marginals Plot Type
    shiny::column(
      width = 3L,
      tags$h4(tags$b(tags$span(style=paste0('color: ', H3Color, ';'), 'Marginal Plot Type'))),
      shinyWidgets::dropdown(
        right = Right, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, inputId = "MarginalPlots", width = LogoWidth,
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Marginal Plot Type')),
        tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Select from density or histogram')),
        RemixAutoML:::BlankRow(AppWidth),
        shiny::fluidRow(
          width = AppWidth,
          shiny::column(3L, shiny::uiOutput('MarginalType1')),
          shiny::column(3L, shiny::uiOutput('MarginalType2')),
          shiny::column(3L, shiny::uiOutput('MarginalType3')),
          shiny::column(3L, shiny::uiOutput('MarginalType4'))))))
}

#' @title GlobalSettingsContents
#'
#' @description Display a dropdown button for global settings
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id = 'GlobalSettings'
#' @param PlotHeight = PlotHeight
#' @param PlotWidth = PlotWidth
#' @param AppWidth = AppWidth
#' @param LogoWidth = LogoWidth
#' @param H3Color = H3Color
#' @param Right = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#'
#' @noRd
GlobalSettingsContents <- function(id='GlobalSettings',
                                   PlotHeight = Pparent.frame()$lotHeight,
                                   PlotWidth = parent.frame()$PlotWidth,
                                   AppWidth = parent.frame()$AppWidth,
                                   LogoWidth = parent.frame()$LogoWidth,
                                   H3Color = parent.frame()$H3Color,
                                   Right = FALSE,
                                   Animate = TRUE,
                                   Status = 'custom') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 3L,
      align='center',
      tags$h4(tags$b('Global Settings')),
      shinyWidgets::dropdown(
        right = Right, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth,
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Global Settings')),
        RemixAutoML:::BlankRow(AppWidth),
        shiny::fluidRow(
          width = AppWidth,
          shiny::column(
            3L,
            align='left',
            shiny::uiOutput('PlotEngine')),
          shiny::column(
            3L,
            align='left',
            shiny::uiOutput('AutoGridHorizontal'))),
        RemixAutoML:::BlankRow(AppWidth),
        shiny::fluidRow(
          width = AppWidth,
          shiny::column(
            3L,
            shiny::uiOutput('PlotHeight')),
          shiny::column(
            3L,
            shiny::uiOutput('PlotWidth'))))))
}

#' @noRd
InitalizeInputs <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      # ----

      # ----

      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      # Plot Data                            ----
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      for(i in seq_len(4L)) {
        output[[paste0("Plot", i, "_SelectData")]] <- shiny::renderUI({
          RemixAutoML:::SelectizeInput(InputID=paste0("Plot", i, "_SelectData"), Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = 'BoxPlot', Multiple = TRUE, MaxVars = 1, SelectedDefault = 'BoxPlot')
        })
        output[[paste0('Plot', i)]] <- shiny::renderUI({
          RemixAutoML:::SelectizeInput(InputID = 'Plot1', Label = 'Plot Selection', Choices = c(AvailablePlots), SelectedDefault = 'BoxPlot', Multiple = TRUE, MaxVars = 1, CloseAfterSelect = TRUE, Debug = Debug)
        })
        output[[paste0('YVar', i)]] <- shiny::renderUI({
          RemixAutoML::SelectizeInput(InputID = paste0('YVar', i), Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Y-Variable'), Choices = NULL, Multiple = TRUE, MaxVars = 1, SelectedDefault = NULL, CloseAfterSelect = TRUE)
        })
        output[[paste0('XVar', i)]] <- shiny::renderUI({
          RemixAutoML::SelectizeInput(InputID = paste0('XVar', i), Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'X-Variable'), Choices = NULL, Multiple = TRUE, MaxVars = 1, SelectedDefault = NULL, CloseAfterSelect = TRUE)
        })
        output[[paste0('ZVar', i)]] <- shiny::renderUI({
          RemixAutoML::SelectizeInput(InputID = paste0('ZVar', i), Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Z-Variable'), Choices = NULL, Multiple = TRUE, MaxVars = 1, SelectedDefault = NULL, CloseAfterSelect = TRUE)
        })
        output[[paste0('GroupVars', i)]] <- shiny::renderUI({
          RemixAutoML::SelectizeInput(InputID=paste0('GroupVars', i), Label=tags$span(style='color: snow;', 'Select Group Variables'), Choices= NULL, SelectedDefault=NULL, Multiple=TRUE, MaxVars = 3, CloseAfterSelect = FALSE)
        })
        for(j in seq_len(3L)) {
          output[[paste0('Levels_', i, '_', j)]] <- shiny::renderUI({
            RemixAutoML:::PickerInput_GetLevels2(DataExist=FALSE, InputID=paste0('Levels_', i, '_', j), InputID2=NULL, Choices=NULL, NumGroupVar=1L, Multiple=TRUE, SelectedDefault=NULL)
          })
        }
        for(j in seq_len(1L)) {
          output[[paste0('FacetVar_', i, '_', j)]] <- shiny::renderUI({
            RemixAutoML::SelectizeInput(InputID=paste0('FacetVar_', i, '_', j), Label = tags$span(style='color: snow;', 'Facet Variables'), Choices = NULL, Multiple = FALSE)
          })
        }
        output[[paste0('SizeVar', i)]] <- shiny::renderUI({
          RemixAutoML::SelectizeInput(InputID = paste0('SizeVar', i), Label = tags$span(style='color: snow;', 'Size Variable'), Choices = NULL, Multiple = FALSE)
        })
        for(j in seq_len(4L)) {
          output[[paste0('FilterVariable_', i, '_', j)]] <- shiny::renderUI({
            RemixAutoML::SelectizeInput(InputID = paste0('FilterVariable_', i, '_', j), Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Filter Variable 1'), Choices = NULL, SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
          })
        }
        for(j in seq_len(4L)) {
          output[[paste0('FilterLogic_', i, '_', j)]] <- shiny::renderUI({
            shiny::selectInput(inputId=paste0('FilterLogic_', i, '_', j), selected=NULL, label=tags$span(style='color: snow;', 'Logical Operation'), choices=c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), multiple=FALSE)
          })
        }
        for(j in seq_len(4L)) {
          for(k in seq_len(2L)) {
            output[[paste0('FilterValue_', i, '_', j, '_', k)]] <- shiny::renderUI({
              RemixAutoML::SelectizeInput(Multiple = FALSE, InputID=paste0('FilterValue_', i, '_', j, '_', k), Label=NULL, Choices=NULL, SelectedDefault=NULL)
            })
          }
        }
        output[[paste0('Plot', i)]] <- shiny::renderUI({
          RemixAutoML::SelectizeInput(InputID = paste0('Plot', i), Label = tags$span(style=paste0('color: ', AppTextColor, ';'),'Plot Type Selection'), Choices = NULL, Multiple = FALSE, SelectedDefault = NULL, CloseAfterSelect = TRUE)
        })
        output[[paste0('BarPlotAgg', i)]] <- shiny::renderUI({
          RemixAutoML::SelectizeInput(InputID = paste0('BarPlotAgg', i), Label = tags$span(style='color: snow;', 'Bar Plot Agg Method'), Choices = c('mean','median','sd'), SelectedDefault = 'mean', Multiple = FALSE, MaxVars = 1, CloseAfterSelect = TRUE, Debug = Debug)
        })
        output[[paste0('TargetLevel', i)]] <- shiny::renderUI({
          RemixAutoML::SelectizeInput(InputID = paste0('TargetLevel', i), Label = tags$span(style='color: snow;', 'Target Level'), Choices = vals, Multiple = FALSE, MaxVars = 1, CloseAfterSelect = TRUE, Debug = FALSE, SelectedDefault = NULL)
        })
        output[[paste0('YLimMin', i)]] <- shiny::renderUI({
          RemixAutoML:::TextInput(InputID = paste0('YLimMin', i), Label = tags$span(style='color: snow;', 'Y Min Limit 1'), Value = NULL, Placeholder = 'Insert a number')
        })
        output[[paste0('YLimMax', i)]] <- shiny::renderUI({
          RemixAutoML:::TextInput(InputID = paste0('YLimMax', i), Label = tags$span(style='color: snow;', 'Y Max Limit 1'), Value = NULL, Placeholder = 'Insert a number')
        })
        output[[paste0('NumberGroupsDisplay', i)]] <- shiny::renderUI({
          RemixAutoML:::NumericInput(InputID = 'NumberGroupsDisplay1', Label = tags$span(style='color: snow;', 'Dispay N Levels'), Step = 1L, Value = 5L, Min = 1L, Max = 100L)
        })
        output[[paste0('ShapAggMethod', i)]] <- shiny::renderUI({
          RemixAutoML::SelectizeInput(InputID=paste0('ShapAggMethod', i), Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Shap Agg Method 1'), Choices=c('mean','absmean','meanabs','sd','median','absmedian','medianabs'), SelectedDefault='meanabs', Multiple=FALSE, CloseAfterSelect = TRUE)
        })
        output[[paste0('Percentile_Buckets', i)]] <- shiny::renderUI({
          RemixAutoML::SelectizeInput(InputID=paste0('Percentile_Buckets', i), Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Percentile Buckets 1'), Choices=1:100, SelectedDefault=20, Multiple=FALSE, CloseAfterSelect = TRUE)
        })
        output[[paste0('GamFitScatter', i)]] <- shiny::renderUI({
          RemixAutoML::SelectizeInput(InputID=paste0('GamFitScatter', i), Label=tags$span(style=paste0('color: ',AppTextColor,';'),'Fit Gam on Scatter or Copula 1'), Choices=c('TRUE','FALSE'), SelectedDefault=FALSE, Multiple=FALSE, CloseAfterSelect = TRUE)
        })
        output[[paste0('NumberBins', i)]] <- shiny::renderUI({
          RemixAutoML:::NumericInput(InputID=paste0('NumberBins', i), Label=tags$span(style=paste0('color: ',AppTextColor,';'),'# of Bins for Histogram 1'), Min=1, Max=1000, Step=5, Value=30)
        })
        output[[paste0('YTicks', i)]] <- shiny::renderUI({
          RemixAutoML::SelectizeInput(InputID = paste0('YTicks', i), Label = tags$span(style='color: snow;', 'Y-Axis 1 Ticks'), Choices = NULL, SelectedDefault = 'Default', Multiple = TRUE, CloseAfterSelect = TRUE)
        })
        output[[paste0('XTicks', i)]] <- shiny::renderUI({
          RemixAutoML::SelectizeInput(InputID = paste0('XTicks', i), Label = tags$span(style='color: snow;', 'X-Axis 1 Ticks'), Choices = NULL, SelectedDefault = 'Default', Multiple = TRUE, CloseAfterSelect = TRUE)
        })
        output[[paste0('AngleY', i)]] <- shiny::renderUI({
          RemixAutoML:::NumericInput(InputID = paste0('AngleY', i), Label = tags$span(style='color: snow;', 'Y-axis text angle'), Step = 5, Min = 0, Max = 360, Value = 0)
        })
        output[[paste0('AngleX', i)]] <- shiny::renderUI({
          RemixAutoML:::NumericInput(InputID = paste0('AngleX', i), Label = tags$span(style='color: snow;', 'X-axis text angle'), Step = 5, Min = 0, Max = 360, Value = 90)
        })
        output[[paste0('TextSize', i)]] <- shiny::renderUI({
          RemixAutoML::SelectizeInput(InputID = paste0('TextSize', i), Label = tags$span(style='color: snow;', 'Text Size'),Choices = c(as.character(seq(1,100,1))), SelectedDefault = '14', Multiple = FALSE, CloseAfterSelect = TRUE)
        })
        output[[paste0('OutlierSize', i)]] <- shiny::renderUI({
          RemixAutoML::SelectizeInput(InputID = paste0('OutlierSize', i), Label = tags$span(style='color: snow;', 'Outlier Size'), Choices = c(seq(0.01,2,0.01)), SelectedDefault = 0.01, Multiple = FALSE, CloseAfterSelect = TRUE)
        })
        output[[paste0('LegendPosition')]] <- shiny::renderUI({
          RemixAutoML::SelectizeInput(InputID = paste0('LegendPosition'), Label = tags$span(style='color: snow;', 'Legend Position'), Choices = c('bottom','left','right','top','none'), SelectedDefault = 'bottom', Multiple = FALSE, CloseAfterSelect = TRUE)
        })
        output[[paste0('LegendBorderSize', i)]] <- shiny::renderUI({
          RemixAutoML::SelectizeInput(InputID = paste0('LegendBorderSize', i), Label = tags$span(style='color: snow;', 'Legend Border Size'), Choices = c(as.character(seq(0.01,2,0.01))), SelectedDefault = as.character(0.01), Multiple = FALSE, CloseAfterSelect = TRUE)
        })
        output[[paste0('LegendLineType', i)]] <- shiny::renderUI({
          RemixAutoML::SelectizeInput(InputID = paste0('LegendLineType', i), Label = tags$span(style='color: snow;', 'Legend Border Type'), Choices = c('solid','blank','dashed','dotdash','dotted','longlash','twodash'), SelectedDefault = 'solid', Multiple = FALSE, CloseAfterSelect = TRUE)
        })
        output[[paste0('TextColor', i)]] <- shiny::renderUI({
          RemixAutoML::SelectizeInput(InputID = paste0('TextColor', i), Label = tags$span(style='color: snow;', 'Text Color'), Choices = grDevices::colors(), SelectedDefault = 'snow', Multiple = FALSE, CloseAfterSelect = TRUE)
        })
        output[[paste0('ChartColor', i)]] <- shiny::renderUI({
          RemixAutoML::SelectizeInput(InputID = paste0('ChartColor', i), Label = tags$span(style='color: snow;', 'Chart Color'), Choices = grDevices::colors(), SelectedDefault = 'aliceblue', Multiple = FALSE, CloseAfterSelect = TRUE)
        })
        output[[paste0('GridColor', i)]] <- shiny::renderUI({
          RemixAutoML::SelectizeInput(InputID = paste0('GridColor', i), Label = tags$span(style='color: snow;', 'Grid Lines Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue1', Multiple = FALSE, CloseAfterSelect = TRUE)
        })
        output[[paste0('BackGroundColor', i)]] <- shiny::renderUI({
          RemixAutoML::SelectizeInput(InputID = paste0('BackGroundColor', i), Label = tags$span(style='color: snow;', 'Background Color'), Choices = grDevices::colors(), SelectedDefault = 'gray95', Multiple = FALSE, CloseAfterSelect = TRUE)
        })
        output[[paste0('BorderColor')]] <- shiny::renderUI({
          RemixAutoML::SelectizeInput(InputID = paste0('BorderColor'), Label = tags$span(style='color: snow;', 'Border Color'), Choices = grDevices::colors(), SelectedDefault = 'lightsteelblue4', Multiple = FALSE, CloseAfterSelect = TRUE)
        })
        output[[paste0('OutlierColor', i)]] <- shiny::renderUI({
          RemixAutoML::SelectizeInput(InputID = paste0('OutlierColor', i), Label = tags$span(style='color: snow;', 'Outlier Color'), Choices = grDevices::colors(), SelectedDefault = 'snow', Multiple = FALSE, CloseAfterSelect = TRUE)
        })
        output[[paste0('FillColor', i)]] <- shiny::renderUI({
          RemixAutoML::SelectizeInput(InputID = paste0('FillColor', i), Label = tags$span(style='color: snow;', 'BoxPlot Fill Color'), Choices = grDevices::colors(), SelectedDefault = 'gray70', Multiple = FALSE, CloseAfterSelect = TRUE)
        })
        output[[paste0('SubTitleColor', i)]] <- shiny::renderUI({
          RemixAutoML::SelectizeInput(InputID = paste0('SubTitleColor', i), Label = tags$span(style='color: snow;', 'Subtitle Color'), Choices = grDevices::colors(), SelectedDefault = 'snow', Multiple = FALSE, CloseAfterSelect = TRUE)
        })

      } # end for loop

      # ----

      # ----

      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      # Load Data                            ----
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      output$LocalPostGRE_Database <- shiny::renderUI({
        RemixAutoML:::SelectizeInput(InputID = 'LocalPostGRE_Database', Label = NULL, Choices = LocalPostGRE_DBNames, SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1L, CloseAfterSelect = FALSE, Debug = Debug)
      })

      # Local POSTGRE DB
      output$LocalPostGRE <- shiny::renderUI({
        RemixAutoML:::SelectizeInput(InputID = 'LocalPostGRE', Label = NULL, Choices = sort(x), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1L, CloseAfterSelect = FALSE, Debug = Debug)
      })

      # ----

      # ----

      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      # FE Variables                         ----
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

      # DataTable Display Initialization
      output$FE_DisplayData <- DT::renderDataTable({
        RemixAutoML::DataTable(data.table::data.table(Random1 = runif(5L), Random2 = runif(5L)))
      })

      # Delete Variables ----
      output$DeleteVariables_SelectData <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='DeleteVariables_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = NULL, Multiple = TRUE, MaxVars = 1, SelectedDefault = tryCatch({names(DataList)[1L]}, error = function(x) NULL))
      })
      output$DeleteVariables <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='DeleteVariables', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Delete Columns'), Choices = NULL, Multiple = TRUE, MaxVars = 1000)
      })

      # Concat Columns ----
      output$ConcatColumns_SelectData <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='ConcatColumns_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = NULL, Multiple = TRUE, MaxVars = 1, SelectedDefault = tryCatch({names(DataList)[1L]}, error = function(x) NULL))
      })
      output$ConcatColumns <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='ConcatColumns', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Concat Columns'), Choices = NULL, Multiple = TRUE, MaxVars = 1000)
      })

      # Date Variables ----
      output$CalendarVariables_SelectData <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='CalendarVariables_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = NULL, Multiple = TRUE, MaxVars = 1, SelectedDefault = tryCatch({names(DataList)[1L]}, error = function(x) NULL))
      })
      output$CalendarVariables_DateVariables <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='CalendarVariables_DateVariables', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Date Columns'), Choices = NULL, Multiple = TRUE, MaxVars = 100)
      })
      output$CalendarVariables_TimeUnits <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='CalendarVariables_TimeUnits', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Calendar Variables'), Choices = c('second','minute','hour','wday','mday','yday','week','isoweek','wom','month','quarter','year'), Multiple = TRUE, MaxVars = 100)
      })

      # Holiday Variables ----
      output$HolidayVariables_SelectData <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='HolidayVariables_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = NULL, Multiple = TRUE, MaxVars = 1, SelectedDefault = tryCatch({names(DataList)[1L]}, error = function(x) NULL))
      })
      output$HolidayVariables_DateVariables <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='HolidayVariables_DateVariables', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Date Variables'), Choices = NULL, Multiple = TRUE, MaxVars = 100)
      })
      output$HolidayVariables_HolidayGroups <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='HolidayVariables_HolidayGroups', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Holiday Selection'), Choices = c('USPublicHolidays','EasterGroup','ChristmasGroup','OtherEcclesticalFeasts'), Multiple = TRUE, MaxVars = 100)
      })
      output$HolidayVariables_LookbackDays <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='HolidayVariables_LookbackDays', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Lookback Days'), Choices = c(1:100), Multiple = TRUE, MaxVars = 1)
      })

      # PercRank() Parameters ----
      output$PercentRank_SelectData <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='PercentRank_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = NULL, Multiple = TRUE, MaxVars = 1, SelectedDefault = tryCatch({names(DataList)[1L]}, error = function(x) NULL))
      })
      output$PercentRank_ColNames <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='PercentRank_ColNames', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Variable Names'), Choices = NULL, Multiple = TRUE, MaxVars = 1000)
      })
      output$PercentRank_GroupVars <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='PercentRank_GroupVars', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'By Variables'), Choices = NULL, Multiple = TRUE, MaxVars = 100)
      })
      output$PercentRank_Granularity <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='PercentRank_Granularity', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Precision'), Choices = c(seq(0.001, 0.99, 0.001)), Multiple = TRUE, MaxVars = 1, SelectedDefault = 0.001)
      })

      # AutoInteraction() Parameters ----
      output$AutoInteraction_SelectData <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoInteraction_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = NULL, Multiple = TRUE, MaxVars = 1, SelectedDefault = tryCatch({names(DataList)[1L]}, error = function(x) NULL))
      })
      output$AutoInteraction_NumericVars <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoInteraction_NumericVars', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Variable Names'), Choices = NULL, Multiple = TRUE, MaxVars = 100000)
      })
      output$AutoInteraction_InteractionDepth <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoInteraction_InteractionDepth', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Interaction Depth'), Choices = c(1:10), Multiple = TRUE, MaxVars = 1, SelectedDefault = 2)
      })
      output$AutoInteraction_Scale <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoInteraction_Scale', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Scale Data'), Choices = c(FALSE,TRUE), Multiple = TRUE, MaxVars = 1, SelectedDefault = TRUE)
      })
      output$AutoInteraction_Center <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoInteraction_Center', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Center Data'), Choices = c(FALSE,TRUE), Multiple = TRUE, MaxVars = 1, SelectedDefault = TRUE)
      })

      # AutoTransformCreate() Parameters ----
      output$AutoTransformationCreate_SelectData <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoTransformationCreate_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = NULL, Multiple = TRUE, MaxVars = 1, SelectedDefault = tryCatch({names(DataList)[1L]}, error = function(x) NULL))
      })
      output$AutoTransformationCreate_ColumnNames <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoTransformationCreate_ColumnNames', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Variable Names'), Choices = NULL, Multiple = TRUE, MaxVars = 100000)
      })
      output$AutoTransformationCreate_Methods <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoTransformationCreate_Methods', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Trans Method'), Choices = c('YeoJohnson','BoxCox','Asinh','Log','LogPlus1','Sqrt','Asin','Logit'), Multiple = TRUE, MaxVars = 100)
      })

      # DummifyDT() Parameters ----
      output$DummifyDT_SelectData <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='DummifyDT_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = NULL, Multiple = TRUE, MaxVars = 1, SelectedDefault = tryCatch({names(DataList)[1L]}, error = function(x) NULL))
      })
      output$DummifyDT_Cols <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='DummifyDT_Cols', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Variable Names'), Choices = NULL, Multiple = TRUE, MaxVars = 100000)
      })
      output$DummifyDT_TopN <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='DummifyDT_TopN', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'TopN'), Choices = 1:100, Multiple = FALSE, MaxVars = 100)
      })
      output$DummifyDT_KeepBaseCols <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='DummifyDT_KeepBaseCols', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Keep Base Cols'), Choices = c(TRUE, FALSE), Multiple = TRUE, MaxVars = 1)
      })

      # CategoricalEncoding() Parameters ----
      output$CategoricalEncoding_SelectData <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='CategoricalEncoding_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = NULL, Multiple = TRUE, MaxVars = 1, SelectedDefault = tryCatch({names(DataList)[1L]}, error = function(x) NULL))
      })
      output$CategoricalEncoding_GroupVariables <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='CategoricalEncoding_GroupVariables', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Variable Names'), Choices = NULL, Multiple = TRUE, MaxVars = 100000)
      })
      output$CategoricalEncoding_TargetVariable <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='CategoricalEncoding_TargetVariable', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Target Name'), Choices = NULL, Multiple = FALSE, MaxVars = 100000)
      })
      output$CategoricalEncoding_Method <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='CategoricalEncoding_Method', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Encoding Method'), Choices = c('credibility','target_encoding','m_estimator','woe','poly_encode','backward_difference','helmert'), Multiple = FALSE, MaxVars = 100000)
      })

      # AutoLagRollMode() Parameters
      output$AutoLagRollMode_SelectData <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoLagRollMode_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = NULL, Multiple = TRUE, MaxVars = 1, SelectedDefault = tryCatch({names(DataList)[1L]}, error = function(x) NULL))
      })
      output$AutoLagRollMode_Lags <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoLagRollMode_Lags', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Lags'), Choices = 1:100, Multiple = TRUE, MaxVars = 1000)
      })
      output$AutoLagRollMode_ModePeriods <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoLagRollMode_ModePeriods', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Window Sizes'), Choices = 1:100, Multiple = TRUE, MaxVars = 1000)
      })
      output$AutoLagRollMode_Targets <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoLagRollMode_Targets', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Target Variables'), Choices = NULL, Multiple = TRUE, MaxVars = 1000)
      })
      output$AutoLagRollMode_GroupingVars <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoLagRollMode_GroupingVars', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'By-Variables'), Choices = NULL, Multiple = TRUE, MaxVars = 100)
      })
      output$AutoLagRollMode_SortDateName <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoLagRollMode_SortDateName', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Date Variable'), Choices = NULL, Multiple = TRUE, MaxVars = 1)
      })
      output$AutoLagRollMode_WindowingLag <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoLagRollMode_WindowingLag', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Window Lag'), Choices = c(FALSE,TRUE), Multiple = TRUE, MaxVars = 1, SelectedDefault = TRUE)
      })

      # AutoLagRollStats() Parameters ----
      output$AutoLagRollStats_SelectData <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoLagRollStats_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = NULL, Multiple = TRUE, MaxVars = 1, SelectedDefault = tryCatch({names(DataList)[1L]}, error = function(x) NULL))
      })
      output$AutoLagRollStats_Targets <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoLagRollStats_Targets', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Target Variables'), Choices = NULL, Multiple = TRUE, MaxVars = 1000)
      })
      output$AutoLagRollStats_GroupVars <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoLagRollStats_GroupVars', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'By-Variables'), Choices = NULL, Multiple = TRUE, MaxVars = 100)
      })
      output$AutoLagRollStats_DateColumn <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoLagRollStats_DateColumn', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Date Variable'), Choices = NULL, Multiple = TRUE, MaxVars = 1)
      })
      output$AutoLagRollStats_TimeUnits <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoLagRollStats_TimeUnits', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Base Aggregation'), Choices = c('raw','hour','day','week','month','quarter','year'), Multiple = TRUE, MaxVars = 100000)
      })
      output$AutoLagRollStats_RollOnLag1 <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoLagRollStats_RollOnLag1', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Window Lag'), Choices = c(FALSE,TRUE), Multiple = FALSE, MaxVars = 100000, SelectedDefault = TRUE)
      })
      output$AutoLagRollStats_Lags <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoLagRollStats_Lags', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Window Lag Start'), Choices = c(1:100), Multiple = FALSE, MaxVars = 100000)
      })
      output$AutoLagRollStats_MA_RollWindows <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoLagRollStats_MA_RollWindows', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Moving Average'), Choices = c(1:100), Multiple = FALSE, MaxVars = 100000)
      })
      output$AutoLagRollStats_SD_RollWindows <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoLagRollStats_SD_RollWindows', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Rolling StDev'), Choices = c(1:100), Multiple = FALSE, MaxVars = 100000)
      })
      output$AutoLagRollStats_Skew_RollWindows <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoLagRollStats_Skew_RollWindows', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Rolling Skew'), Choices = c(1:100), Multiple = FALSE, MaxVars = 100000)
      })
      output$AutoLagRollStats_Kurt_RollWindows <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoLagRollStats_Kurt_RollWindows', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Rolling Kurt'), Choices = c(1:100), Multiple = FALSE, MaxVars = 100000)
      })
      output$AutoLagRollStats_Quantile_RollWindows <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoLagRollStats_Quantile_RollWindows', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Rolling Percentile'), Choices = c(1:100), Multiple = FALSE, MaxVars = 100000)
      })
      output$AutoLagRollStats_Quantiles_Selected <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoLagRollStats_Quantiles_Selected', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Select Percentiles'), Choices = c('q5','q10','q15','q20','q25','q30','q35','q40','q45','q50','q55','q60','q65','q70','q75','q80','q85','q90','q95'), Multiple = TRUE, MaxVars = 100000)
      })

      # AutoDiffLagN() Parameters ----
      output$AutoDiffLagN_SelectData <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoDiffLagN_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = NULL, Multiple = TRUE, MaxVars = 1, SelectedDefault = tryCatch({names(DataList)[1L]}, error = function(x) NULL))
      })
      output$AutoDiffLagN_DateVariable <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoDiffLagN_DateVariable', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Date Variable'), Choices = NULL, Multiple = FALSE, MaxVars = 100000)
      })
      output$AutoDiffLagN_GroupVariables <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoDiffLagN_GroupVariables', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'By-Variables'), Choices = NULL, Multiple = TRUE, MaxVars = 100000)
      })
      output$AutoDiffLagN_DiffVariables <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoDiffLagN_DiffVariables', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Numeric Diff Variables'), Choices = NULL, Multiple = TRUE, MaxVars = 100000)
      })
      output$AutoDiffLagN_DiffDateVariables <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoDiffLagN_DiffDateVariables', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Date Diff Variables'), Choices = NULL, Multiple = FALSE, MaxVars = 100000)
      })
      output$AutoDiffLagN_DiffGroupVariables <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoDiffLagN_DiffGroupVariables', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Group Diff Variables'), Choices = NULL, Multiple = TRUE, MaxVars = 100000)
      })
      output$AutoDiffLagN_NLag1 <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoDiffLagN_NLag1', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Base Period'), Choices = c(0:100), Multiple = TRUE, MaxVars = 1)
      })
      output$AutoDiffLagN_NLag2 <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoDiffLagN_NLag2', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Lookback Period'), Choices = c(1:100), Multiple = TRUE, MaxVars = 1)
      })

      # Type Conversion ----
      output$ModelDataPrep_SelectData <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='ModelDataPrep_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = NULL, Multiple = TRUE, MaxVars = 1, SelectedDefault = tryCatch({names(DataList)[1L]}, error = function(x) NULL))
      })
      output$ModelDataPrep_IgnoreCols <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='ModelDataPrep_IgnoreCols', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Skip Columns'), Choices = NULL, Multiple = TRUE, MaxVars = 1000)
      })
      output$ModelDataPrep_CharToFactor <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='ModelDataPrep_CharToFactor', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Char to Factor'), Choices = c(FALSE,TRUE), Multiple = TRUE, MaxVars = 1, SelectedDefault = FALSE)
      })
      output$ModelDataPrep_FactorToChar <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='ModelDataPrep_FactorToChar', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Factor to Char'), Choices = c(FALSE,TRUE), Multiple = TRUE, MaxVars = 1, SelectedDefault = FALSE)
      })
      output$ModelDataPrep_DateToChar <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='ModelDataPrep_DateToChar', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Date to Char'), Choices = c(FALSE,TRUE), Multiple = TRUE, MaxVars = 1, SelectedDefault = FALSE)
      })
      output$ModelDataPrep_IDateConversion <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='ModelDataPrep_IDateConversion', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'IDate to Date'), Choices = c(FALSE,TRUE), Multiple = TRUE, MaxVars = 1, SelectedDefault = FALSE)
      })
      output$ModelDataPrep_RemoveDates <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='ModelDataPrep_RemoveDates', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Remove Dates'), Choices = c(FALSE,TRUE), Multiple = TRUE, MaxVars = 1, SelectedDefault = FALSE)
      })
      output$ModelDataPrep_IntToNumeric <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='ModelDataPrep_IntToNumeric', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Integer to Numeric'), Choices = c(FALSE,TRUE), Multiple = TRUE, MaxVars = 1, SelectedDefault = FALSE)
      })
      output$ModelDataPrep_LogicalToBinary <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='ModelDataPrep_LogicalToBinary', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Logical to Binary'), Choices = c(FALSE,TRUE), Multiple = TRUE, MaxVars = 1, SelectedDefault = FALSE)
      })
      output$ModelDataPrep_MissFactor <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='ModelDataPrep_MissFactor', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Character Impute'), Choices = c('0','missing','NULL'), Multiple = TRUE, MaxVars = 1, SelectedDefault = '0')
      })
      output$ModelDataPrep_MissNum <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='ModelDataPrep_MissNum', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Numeric Impute'), Choices = c(-1, 0, -999, 999, 0.001, -0.001), Multiple = TRUE, MaxVars = 1, SelectedDefault = -1)
      })

      # Data Partition ----
      output$AutoDataPartition_SelectData <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoDataPartition_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = NULL, Multiple = TRUE, MaxVars = 1, SelectedDefault = tryCatch({names(DataList)[1L]}, error = function(x) NULL))
      })
      output$AutoDataPartition_NumDataSets <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoDataPartition_NumDataSets', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = c(2,3), Multiple = TRUE, MaxVars = 1, SelectedDefault = 3)
      })
      output$AutoDataPartition_Ratios_Train <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoDataPartition_Ratios_Train', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Train %'), Choices = c(seq(0.50, 0.95, 0.05)), Multiple = TRUE, MaxVars = 1, SelectedDefault = c(0.70))
      })
      output$AutoDataPartition_Ratios_Validation <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoDataPartition_Ratios_Validation', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Validation %'), Choices = c(seq(0.50, 0.95, 0.05)), Multiple = TRUE, MaxVars = 1, SelectedDefault = c(0.20))
      })
      output$AutoDataPartition_Ratios_Test <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoDataPartition_Ratios_Test', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Test %'), Choices = c(seq(0.0, 0.50, 0.05)), Multiple = TRUE, MaxVars = 1, SelectedDefault = c(0.10))
      })
      output$AutoDataPartition_PartitionType <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoDataPartition_PartitionType', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Partition Type'), Choices = c('random', 'time'), Multiple = TRUE, MaxVars = 1, SelectedDefault = 'random')
      })
      output$AutoDataPartition_StratifyColumnNames <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoDataPartition_StratifyColumnNames', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Numeric Impute'), Choices = NULL, Multiple = TRUE, MaxVars = 10, SelectedDefault = NULL)
      })
      output$AutoDataPartition_TimeColumnName <- shiny::renderUI({
        RemixAutoML::SelectizeInput(InputID='AutoDataPartition_TimeColumnName', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Numeric Impute'), Choices = NULL, Multiple = TRUE, MaxVars = 1, SelectedDefault = NULL)
      })

      # ----

      # ----

      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
      # Plotting MetaData                    ----
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

      print('Plotting Initialization')

      # Intialize Plot ----
      output$TrendPlotly <- plotly::renderPlotly({
        if(!exists('PlotCollectionList')) plotly::ggplotly(RemixAutoML:::BlankPlot())
      })

      # Define plot output type: if any FacetVars are not null then use ggplot2, othwerwise plotly ----
      output$PlotEngine <- shiny::renderUI({
        shiny::checkboxGroupInput(inputId = "PlotEngine", label = tags$span(style='color: snow;', 'Plot Engine'),choices = list("plotly" = 1, "ggplot2" = 2), selected = 1)
      })

      # Auto SCaling of Plot Grid: doubles the size in the event of more than 1 plot ----
      # output$AutoGridHorizontal <-  shiny::renderUI({
      #   shiny::checkboxGroupInput(inputId = "AutoGridHorizontal", label = tags$span(style='color: snow;', 'Auto Grid Scale'),choices = list("On" = 1, "Off" = 2), selected = 1)
      # })

      # Dragula for PlotType ----
      output$PlotTypeDragula <- shiny::renderUI({
        dragheight <- '40px'
        tags$style("#display {background: #2d2d2d; border: 10px solid #000000; border-radius: 5px; font-size: 2em; color: white; height: 100px; min-width:200px; text-align: center; padding: 1em; display:table-cell; vertical-align:middle; } #drag-elements { display: block; background-color: #dfdfdf; border-radius: 5px; min-height: 50px; margin: 0 auto; padding: 2em; } #drag-elements > div { text-align: center; float: left; padding: 1em; margin: 0 1em 1em 0; box-shadow: 1px 1px 1px rgba(0, 0, 0, 0.3); border-radius: 100px; border: 2px solid #ececec; background: #F7F7F7; transition: all .5s ease; } #drag-elements > div:active { -webkit-animation: wiggle 0.3s 0s infinite ease-in; animation: wiggle 0.3s 0s infinite ease-in; opacity: .6; border: 2px solid #000; } #drag-elements > div:hover { border: 2px solid gray; background-color: #e5e5e5; } #drop-target { border: 2px dashed #D9D9D9; border-radius: 5px; min-height: 50px; margin: 0 auto; margin-top: 10px; padding: 2em; display: block; text-align: center; } #drop-target > div { transition: all .5s; text-align: center; float: left; padding: 1em; margin: 0 1em 1em 0; box-shadow: 1px 1px 1px rgba(0, 0, 0, 0.3); border-radius: 5px; border: 2px solid skyblue; background: #F7F7F7; transition: all .5s ease; } #drop-target > div:active { -webkit-animation: wiggle 0.3s 0s infinite ease-in; animation: wiggle 0.3s 0s infinite ease-in; opacity: .6; border: 2px solid #000; }")
        esquisse::dragulaInput(
          height = dragheight,
          replace = FALSE,
          inputId = 'PlotTypeDragula',
          label = 'Drag and Drop Plot Types',
          sourceLabel = 'Plots',
          targetsLabels = c('TopLeft', 'LowerLeft', 'TopRight', 'LowerRight'),
          boxStyle = shiny::tags$style(HTML(
            "
        /* drag source */
        #container-drag-source {
        	position: relative;
        	padding: 5px 0 5px 0;
        	border-color: #cfcef0;
        	border-radius: 5px;
        	border-style: solid;
        	border-width: 10px;
        	overflow: auto;
        	overflow-x: hidden;
        	font-size: 12px;
        }
        ")),
          choices = c('Plot1', 'Plot2', 'Plot3', 'Plot4'))
      })


      # Global Setting ----
      output$PlotWidth <- shiny::renderUI({
        RemixAutoML:::NumericInput(InputID = "PlotWidth", Label=tags$span(style='color: snow;', 'Plot Width'), Step = 50, Min = 500, Max = 3500, Value = 1600)
      })
      output$PlotHeight <- shiny::renderUI({
        RemixAutoML:::NumericInput(InputID = "PlotHeight", Label=tags$span(style='color: snow;', 'Plot Height'), Step = 25, Min = 300, Max = 3500, Value = 500)
      })

      # Other values ----
      output$SampleSize <- shiny::renderUI({
        RemixAutoML:::NumericInput(InputID = 'SampleSize', Label = tags$span(style='color: snow;', 'Sample size for plotting'), Step = 50000, Min = 0, Max = 1000000, Value = 100000)
      })

      # Turn off usage ----
      # InitalizeInputs <<- FALSE

    })
}

#' @noRd
DeleteVariablesServer <- function(id) {
  ns <- NS(id)
  shiny::observeEvent(input$DeleteVariablesInputs, {
    print('Delete Variables Inputs Dropdown')
    output$DeleteVariables_SelectData <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='DeleteVariables_SelectData', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Choose data set'), Choices = names(DataList), Multiple = TRUE, MaxVars = 1, SelectedDefault = names(DataList)[1L], CloseAfterSelect = FALSE)
    })
    dt <- shiny::reactive({shiny::req(tryCatch({DataList[[input$DeleteVariables_SelectData]]}, error = function(x) DataList[[1L]]))})
    output$DeleteVariables <- shiny::renderUI({
      RemixAutoML::SelectizeInput(InputID='DeleteVariables', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Delete Columns'), Choices = names(dt()), Multiple = TRUE, MaxVars = 1000, CloseAfterSelect = FALSE)
    })
  })
}

# Leftovers

# Button to disable / enable Data Load Page
# shinyjs::addCssClass(selector = "a[data-value='LoadDataPage']", class = "inactiveLink")
# shinyjs::addCssClass(selector = "a[data-value='Plotter']", class = "inactiveLink")
# shinyjs::addCssClass(selector = "a[data-value='FeatureEngineering']", class = "inactiveLink")
# shinyjs::addCssClass(selector = "a[data-value='MachineLearning']", class = "inactiveLink")
# shinyjs::addCssClass(selector = "a[data-value='CodePrint']", class = "inactiveLink")

# Login
# shiny::observeEvent(input$Check_Credentials, {
#   if(UserName() %in% Credentials$UserName && Password() %in% Credentials[UserName == eval(UserName())]$Password) {
#     shinyjs::removeCssClass(selector = "a[data-value='LoadDataPage']", class = "inactiveLink")
#     shinyjs::removeCssClass(selector = "a[data-value='Plotter']", class = "inactiveLink")
#     shinyjs::removeCssClass(selector = "a[data-value='FeatureEngineering']", class = "inactiveLink")
#     shinyjs::removeCssClass(selector = "a[data-value='MachineLearning']", class = "inactiveLink")
#     shinyjs::removeCssClass(selector = "a[data-value='CodePrint']", class = "inactiveLink")
#     shinyWidgets::sendSweetAlert(session, title = NULL, text = 'Success', type = NULL, btn_labels = "success", btn_colors = "green", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
#   } else {
#     shinyjs::addCssClass(selector = "a[data-value='LoadDataPage']", class = "inactiveLink")
#     shinyjs::addCssClass(selector = "a[data-value='Plotter']", class = "inactiveLink")
#     shinyjs::addCssClass(selector = "a[data-value='FeatureEngineering']", class = "inactiveLink")
#     shinyjs::addCssClass(selector = "a[data-value='MachineLearning']", class = "inactiveLink")
#     shinyjs::addCssClass(selector = "a[data-value='CodePrint']", class = "inactiveLink")
#     shinyWidgets::sendSweetAlert(session, title = NULL, text = 'Username and / or password is incorrect', type = NULL, btn_labels = "error", btn_colors = "red", html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
#   }
# })

# shiny::observeEvent(input$sidebar, {
#   if(input$sidebar == 'Login')
#     shiny::updateTabsetPanel(session, inputId = "Login1", selected = "Login")
# })


