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
                        AppWidth = AppWidth) {
  ns <- shiny::NS(id)
  shiny::tagList(
    RemixAutoML::BlankRow(AppWidth),
    shinydashboard::box(
      title = BoxTitle,
      solidHeader = SolidHeader,
      collapsible = Collapsible,
      status = BoxStatus,
      width = AppWidth,
      shiny::selectInput(
        inputId = 'UserName',
        label =  UserNameLabel,
        choices = UserNameChoices,
        selected = 'UserName'),
      shiny::textInput(
        inputId = "Password",
        label =  PassWordLabel,
        value = 'Password')))
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
                        AppWidth = AppWidth,
                        Width = 3L,
                        Icon = shiny::icon('chevron-right', lib = 'font-awesome'),
                        Style = 'gradient',
                        Color = 'royal') {
  ns <- shiny::NS(id)
  shiny::tagList(
    RemixAutoML::BlankRow(AppWidth),
    shiny::fluidRow(
      shiny::column(
        width = Width,
        shinyWidgets::actionBttn(
          inputId = 'Check_Credentials',
          label = Label,
          icon = Icon,
          style = Style,
          color = Color))))
}

#' @title LoadDataInputs
#'
#' @description Replica of box with inputs for local .csv data, local .Rdata, and a dropdown for External data. Local CSV inputId is 'DataLoad'. Local .Rdata inputId is 'ModelObjectLoad', External csv inputId is 'blob'. External .Rdata inputId is 'rdatablob'
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
#' @param CSV_h4 = 'Local .csv Data'
#' @param DropdownRight = FALSE
#' @param DropDownAnimate = TRUE
#' @param DropDownStatus = 'custom'
#'
#' @noRd
LoadDataInputs <- function(id = 'ExternalData',
                           AppWidth = AppWidth,
                           LogoWidth = LogoWidth,
                           SolidHeader = TRUE,
                           BoxTitle = NULL,
                           BoxStatus = 'danger',
                           CSV_h4 = 'Local .csv Data',
                           DropdownRight = FALSE,
                           DropDownAnimate = TRUE,
                           DropDownStatus = 'custom',
                           H3Color = 'blue')  {
  ns <- shiny::NS(id)
  shiny::tagList(
    RemixAutoML::BlankRow(AppWidth),
    shinydashboard::box(
      title = BoxTitle,
      width = AppWidth,
      solidHeader = SolidHeader,
      status = BoxStatus,

      # Local CSV
      RemixAutoML::BlankRow(AppWidth),
      tags$h4(tags$b(CSV_h4)),
      shiny::uiOutput('DataLoad'),

      # Local .Rdata
      RemixAutoML::BlankRow(AppWidth),
      tags$h4(tags$b('Local .Rdata Model Output List')),
      shiny::uiOutput('ModelObjectLoad'),

      # Dropdown for external data
      RemixAutoML::BlankRow(AppWidth),
      tags$h4(tags$b('External Data Loading')),
      shinyWidgets::dropdown(
        right = DropdownRight,
        animate = DropDownAnimate,
        circle = FALSE,
        tooltip = FALSE,
        status = DropDownStatus,
        width = LogoWidth,
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'External Data')),
        RemixAutoML::BlankRow(AppWidth),
        shiny::fluidRow(
          width = AppWidth,
          shiny::column(
            3L,
            align='right',
            shiny::uiOutput('blob'))),
        RemixAutoML::BlankRow(AppWidth),
        shiny::fluidRow(
          width = AppWidth,
          shiny::column(
            3L,
            shiny::uiOutput('rdatablob'))),
        RemixAutoML::BlankRow(AppWidth),

        # Load button
        shinyWidgets::actionBttn(
          inputId = 'LoadAzure',
          label = 'Download Data')),

      # Add row
      RemixAutoML::BlankRow(AppWidth),
      RemixAutoML::BlankRow(AppWidth)))
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
                           AppWidth = AppWidth,
                           Style = 'gradient',
                           Color = 'royal')  {
  ns <- shiny::NS(id)
  shiny::tagList(
    RemixAutoML::BlankRow(AppWidth),
    shiny::fluidRow(
      shiny::column(
        width = 3L,
        shinyjs::useShinyjs(),
        shinyWidgets::actionBttn(
          inputId = 'LoadDataButton',
          label = 'Load Data',
          icon = shiny::icon('chevron-right', lib='font-awesome'),
          style = Style,
          color = Color))))
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
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'blue'
#'
#' @noRd
FE_DateVariables <- function(id='CalendarVariables',
                             AppWidth=AppWidth,
                             LogoWidth=LogoWidth,
                             ButtonWidth=3L,
                             Align='left',
                             DropDownRight=FALSE,
                             Animate=TRUE,
                             Status='custom',
                             H3Color = 'blue') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = ButtonWidth,
      align=Align,

      # Dropdown Button
      tags$h4(tags$b('Date Variables')),
      shinyWidgets::dropdown(
        right = DropDownRight, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth,
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Select Inputs'))),
        RemixAutoML::BlankRow(AppWidth),

        # Calendar Variables Selection
        tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Calendar Variables'))),
        shiny::fluidRow(
          width=AppWidth,
          shiny::column(6L, align = 'center', shiny::uiOutput('CalendarVariables_DateVariables')),
          shiny::column(6L, align = 'center', shiny::uiOutput('CalendarVariables_TimeUnits'))),


        # Holiday Variables Selection
        RemixAutoML::BlankRow(AppWidth),
        tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Holiday Variables'))),
        shiny::fluidRow(
          width=AppWidth,
          shiny::column(6L, align = 'center', shiny::uiOutput('HolidayVariables_DateVariables')),
          shiny::column(6L, align = 'center', shiny::uiOutput('HolidayVariables_HolidayGroups')),
          shiny::column(6L, align = 'center', shiny::uiOutput('HolidayVariables_LookbackDays')))
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
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'blue'
#'
#' @noRd
FE_NumericVariables <- function(id='NumericVariables',
                                AppWidth=AppWidth,
                                LogoWidth=LogoWidth,
                                ButtonWidth=3L,
                                Align='left',
                                DropDownRight=FALSE,
                                Animate=TRUE,
                                Status='custom',
                                H3Color = 'blue') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = ButtonWidth,
      align=Align,

      # Dropdown Button
      tags$h4(tags$b('Date Variables')),
      shinyWidgets::dropdown(
        right = DropDownRight, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth,
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Select Inputs'))),
        RemixAutoML::BlankRow(AppWidth),

        # PercRank Selection
        tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Percent Rank Variables'))),
        shiny::fluidRow(
          width=AppWidth,
          shiny::column(6L, align = 'center', shiny::uiOutput('PercentRank_ColNames')),
          shiny::column(6L, align = 'center', shiny::uiOutput('PercentRank_GroupVars')),
          shiny::column(6L, align = 'center', shiny::uiOutput('PercentRank_Granularity'))),

        # Auto Interaction Selection
        RemixAutoML::BlankRow(AppWidth),
        tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Interaction Variables'))),
        shiny::fluidRow(
          width=AppWidth,
          shiny::column(6L, align = 'center', shiny::uiOutput('AutoInteraction_NumericVars')),
          shiny::column(6L, align = 'center', shiny::uiOutput('AutoInteraction_InteractionDepth')),
          shiny::column(6L, align = 'center', shiny::uiOutput('AutoInteraction_Center')),
          shiny::column(6L, align = 'center', shiny::uiOutput('AutoInteraction_Scale'))),

        # Transformations
        RemixAutoML::BlankRow(AppWidth),
        tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Transform Variables'))),
        shiny::fluidRow(
          width=AppWidth,
          shiny::column(6L, align = 'center', shiny::uiOutput('AutoInteraction_ColumnNames')),
          shiny::column(6L, align = 'center', shiny::uiOutput('AutoInteraction_Methods')))
      )))
}

#' @title FeatureEngineeringButton
#'
#' @description FeatureEngineeringButton is a button for kicking off feature engineering
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
FeatureEngineeringButton <- function(id = 'FEButton',
                                     AppWidth = AppWidth,
                                     Style = 'gradient',
                                     Color = 'royal') {
  ns <- shiny::NS(id)
  shiny::tagList(
    RemixAutoML::BlankRow(AppWidth),
    shiny::fluidRow(
      shiny::column(
        width = 3L,
        shinyjs::useShinyjs(),
        shinyWidgets::actionBttn(
          inputId = 'FeatureEngineeringButton',
          label = 'Build Features',
          icon = shiny::icon('chevron-right', lib='font-awesome'),
          style = Style,
          color = Color))))
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
#' @param H3Color = 'blue'
#'
#' @noRd
PlotDropDownContents <- function(id = 'PlotDropDown',
                                 PlotNumber = 1,
                                 AppWidth=AppWidth,
                                 LogoWidth=LogoWidth,
                                 ButtonWidth=3L,
                                 Align='center',
                                 DropDownRight=FALSE,
                                 Animate=TRUE,
                                 Status='custom',
                                 H3Color = 'blue') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = ButtonWidth,
      align=Align,

      # Dropdown Button
      tags$h4(tags$b(paste0('Plot ', PlotNumber))),
      shinyWidgets::dropdown(
        right = DropDownRight, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, width = LogoWidth,
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Core Variables for Plot ', PlotNumber))),
        RemixAutoML::BlankRow(AppWidth),

        # PlotType Selection
        shiny::fluidRow(
          width=AppWidth,
          shiny::column(6L, shiny::uiOutput(paste0('Plot', PlotNumber)))),
        shiny::fluidRow(
          width=AppWidth,
          shiny::column(6L, shiny::uiOutput(paste0('YVar', PlotNumber))),
          shiny::column(6L, shiny::uiOutput(paste0('XVar', PlotNumber)))),
        shiny::fluidRow(
          width=AppWidth,
          shiny::column(6L, shiny::uiOutput(paste0('FacetVar_', PlotNumber, '_1'))),
          shiny::column(6L, shiny::uiOutput(paste0('FacetVar_', PlotNumber, '_2')))),
        shiny::fluidRow(
          width=AppWidth,
          shiny::column(6L, shiny::uiOutput(paste0('GroupVars', PlotNumber))),
          shiny::column(6L, shiny::uiOutput(paste0('SizeVar', PlotNumber)))),

        # Group-Levels
        shiny::fluidRow(
          width=AppWidth,
          shiny::column(4L, shiny::conditionalPanel(width = 3L, condition = "length(input['GroupVars1']) >= 1", shiny::uiOutput(paste0('Levels_', PlotNumber, '_1')))),
          shiny::column(4L, shiny::conditionalPanel(width = 3L, condition = "length(input['GroupVars2']) >= 2", shiny::uiOutput(paste0('Levels_', PlotNumber, '_2')))),
          shiny::column(4L, shiny::conditionalPanel(width = 3L, condition = "length(input['GroupVars3']) >= 3", shiny::uiOutput(paste0('Levels_', PlotNumber, '_3'))))))))
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
                                   PlotHeight = PlotHeight,
                                   PlotWidth = PlotWidth,
                                   AppWidth = AppWidth,
                                   LogoWidth = LogoWidth,
                                   H3Color = H3Color,
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
        RemixAutoML::BlankRow(AppWidth),
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
        RemixAutoML::BlankRow(AppWidth),
        shiny::fluidRow(
          width = AppWidth,
          shiny::column(
            3L,
            shiny::uiOutput('PlotHeight')),
          shiny::column(
            3L,
            shiny::uiOutput('PlotWidth'))))))
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
                        AppWidth = AppWidth,
                        LogoWidth = LogoWidth,
                        H3Color = H3Color,
                        H4Color = H4Color,
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
        right = Right, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, inputId = paste0('Filters-P', PlotNumber), width = LogoWidth,
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'), paste0('Filters for Plot ', PlotNumber))),
        tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Subset data')),
        RemixAutoML::BlankRow(AppWidth),
        shiny::fluidRow(
          width = 12L,
          shiny::column(3L, shiny::uiOutput(paste0('FilterVariable_', PlotNumber, '_1'))),
          shiny::column(3L, shiny::uiOutput(paste0('FilterVariable_', PlotNumber, '_2'))),
          shiny::column(3L, shiny::uiOutput(paste0('FilterVariable_', PlotNumber, '_3'))),
          shiny::column(3L, shiny::uiOutput(paste0('FilterVariable_', PlotNumber, '_4')))),
        shiny::fluidRow(
          width = 12L,
          shiny::column(3L, shiny::uiOutput(paste0('FilterLogic_', PlotNumber, '_1'))),
          shiny::column(3L, shiny::uiOutput(paste0('FilterLogic_', PlotNumber, '_2'))),
          shiny::column(3L, shiny::uiOutput(paste0('FilterLogic_', PlotNumber, '_3'))),
          shiny::column(3L, shiny::uiOutput(paste0('FilterLogic_', PlotNumber, '_4')))),
        shiny::fluidRow(
          width = 12L,
          shiny::column(3L, shiny::uiOutput(paste0('FilterValue_', PlotNumber, '_1_1'))),
          shiny::column(3L, shiny::uiOutput(paste0('FilterValue_', PlotNumber, '_2_1'))),
          shiny::column(3L, shiny::uiOutput(paste0('FilterValue_', PlotNumber, '_3_1'))),
          shiny::column(3L, shiny::uiOutput(paste0('FilterValue_', PlotNumber, '_4_1')))),
        shiny::fluidRow(
          width = 12L,
          shiny::column(3L, shiny::uiOutput(paste0('FilterValue_', PlotNumber, '_1_2'))),
          shiny::column(3L, shiny::uiOutput(paste0('FilterValue_', PlotNumber, '_2_2'))),
          shiny::column(3L, shiny::uiOutput(paste0('FilterValue_', PlotNumber, '_3_2'))),
          shiny::column(3L, shiny::uiOutput(paste0('FilterValue_', PlotNumber, '_4_2')))))))
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
                       AppWidth = AppWidth,
                       LogoWidth = LogoWidth,
                       H3Color = H3Color,
                       H4Color = H4Color,
                       Right = FALSE,
                       Animate = TRUE,
                       Status = 'custom') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 3L,
      tags$h4(tags$b(tags$span(style=paste0('color: ', H3Color, ';'), 'Axis Limits'))),
      shinyWidgets::dropdown(
        right = Right, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, inputId = "Axis-Limits", width = LogoWidth,
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Plot Limits')),
        tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Alter the min and max limits for the Y and X Axes')),
        RemixAutoML::BlankRow(AppWidth),
        shiny::fluidRow(
          width = AppWidth,
          shiny::column(3L, shiny::uiOutput('YLimMin1')),
          shiny::column(3L, shiny::uiOutput('YLimMax1')),
          shiny::column(3L, shiny::uiOutput('XLimMin1')),
          shiny::column(3L, shiny::uiOutput('XLimMax1'))),
        shiny::fluidRow(
          width = AppWidth,
          shiny::column(3L, shiny::uiOutput('YLimMin2')),
          shiny::column(3L, shiny::uiOutput('YLimMax2')),
          shiny::column(3L, shiny::uiOutput('XLimMin2')),
          shiny::column(3L, shiny::uiOutput('XLimMax2'))),
        shiny::fluidRow(
          width = AppWidth,
          shiny::column(3L, shiny::uiOutput('YLimMin3')),
          shiny::column(3L, shiny::uiOutput('YLimMax3')),
          shiny::column(3L, shiny::uiOutput('XLimMin3')),
          shiny::column(3L, shiny::uiOutput('XLimMax3'))),
        shiny::fluidRow(
          width = AppWidth,
          shiny::column(3L, shiny::uiOutput('YLimMin4')),
          shiny::column(3L, shiny::uiOutput('YLimMax4')),
          shiny::column(3L, shiny::uiOutput('XLimMin4')),
          shiny::column(3L, shiny::uiOutput('XLimMax4'))))))
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
                       AppWidth = AppWidth,
                       LogoWidth = LogoWidth,
                       H3Color = H3Color,
                       H4Color = H4Color,
                       Right = FALSE,
                       Animate = TRUE,
                       Status = 'custom') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 3L,
      tags$h4(tags$b(tags$span(style=paste0('color: ', H3Color, ';'), 'Plot Structure'))),
      shinyWidgets::dropdown(
        right = Right, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, inputId = "Plot-Structure", width = LogoWidth,
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Plot Formatting')),
        tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'For color options, see Plot Colors')),
        RemixAutoML::BlankRow(AppWidth),
        shiny::fluidRow(
          width = AppWidth,
          shiny::column(3L, shiny::uiOutput('SampleSize1')),
          shiny::column(3L, shiny::uiOutput('SampleSize2')),
          shiny::column(3L, shiny::uiOutput('SampleSize3')),
          shiny::column(3L, shiny::uiOutput('SampleSize4'))),
        shiny::fluidRow(
          width = AppWidth,
          shiny::column(3L, shiny::uiOutput('AngleY1')),
          shiny::column(3L, shiny::uiOutput('AngleY2')),
          shiny::column(3L, shiny::uiOutput('AngleY3')),
          shiny::column(3L, shiny::uiOutput('AngleY4'))),
        shiny::fluidRow(
          width = AppWidth,
          shiny::column(3L, shiny::uiOutput('AngleX1')),
          shiny::column(3L, shiny::uiOutput('AngleX2')),
          shiny::column(3L, shiny::uiOutput('AngleX3')),
          shiny::column(3L, shiny::uiOutput('AngleX4'))),
        shiny::fluidRow(
          width = AppWidth,
          shiny::column(3L, shiny::uiOutput('YTicks1')),
          shiny::column(3L, shiny::uiOutput('YTicks2')),
          shiny::column(3L, shiny::uiOutput('YTicks3')),
          shiny::column(3L, shiny::uiOutput('YTicks4'))),
        shiny::fluidRow(
          width = AppWidth,
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
        shiny::fluidRow(
          shiny::column(3L, shiny::uiOutput('LegendPosition1')),
          shiny::column(3L, shiny::uiOutput('LegendPosition2')),
          shiny::column(3L, shiny::uiOutput('LegendPosition3')),
          shiny::column(3L, shiny::uiOutput('LegendPosition4'))),
        shiny::fluidRow(
          shiny::column(3L, shiny::uiOutput('LegendLineType1')),
          shiny::column(3L, shiny::uiOutput('LegendLineType2')),
          shiny::column(3L, shiny::uiOutput('LegendLineType3')),
          shiny::column(3L, shiny::uiOutput('LegendLineType4'))))))
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
                     AppWidth = AppWidth,
                     LogoWidth = LogoWidth,
                     H3Color = H3Color,
                     H4Color = H4Color,
                     Right = FALSE,
                     Animate = TRUE,
                     Status = 'custom') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 3L,
      tags$h4(tags$b(tags$span(style=paste0('color: ', H3Color, ';'), 'Plot Colors'))),
      shinyWidgets::dropdown(
        right = Right, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, inputId = "Plot-Colors", width = LogoWidth,
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Plot Coloring')),
        RemixAutoML::BlankRow(AppWidth),
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
          shiny::column(3L, shiny::uiOutput('OutlierColor1')),
          shiny::column(3L, shiny::uiOutput('OutlierColor2')),
          shiny::column(3L, shiny::uiOutput('OutlierColor3')),
          shiny::column(3L, shiny::uiOutput('OutlierColor4'))),
        shiny::fluidRow(
          width = AppWidth,
          shiny::column(3L, shiny::uiOutput('GridColor1')),
          shiny::column(3L, shiny::uiOutput('GridColor2')),
          shiny::column(3L, shiny::uiOutput('GridColor3')),
          shiny::column(3L, shiny::uiOutput('GridColor4'))))))
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
                     AppWidth = AppWidth,
                     LogoWidth = LogoWidth,
                     H3Color = H3Color,
                     H4Color = H4Color,
                     Right = FALSE,
                     Animate = TRUE,
                     Status = 'custom') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 3L,
      tags$h4(tags$b(tags$span(style=paste0('color: ', H3Color, ';'), 'GAM Line'))),
      shinyWidgets::dropdown(
        right = Right, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, inputId = "GamRegressionLine", width = LogoWidth,
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'GAM Regression Lines')),
        tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Add a generalized additive model regression fit to the data')),
        RemixAutoML::BlankRow(AppWidth),
        shiny::fluidRow(
          width = AppWidth,
          shiny::column(3L, shiny::uiOutput('GamFitScatter1')),
          shiny::column(3L, shiny::uiOutput('GamFitScatter2')),
          shiny::column(3L, shiny::uiOutput('GamFitScatter3')),
          shiny::column(3L, shiny::uiOutput('GamFitScatter4'))))))
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
                       AppWidth = AppWidth,
                       LogoWidth = LogoWidth,
                       H3Color = H3Color,
                       H4Color = H4Color,
                       Right = FALSE,
                       Animate = TRUE,
                       Status = 'custom') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 3L,
      tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), tags$b('Hist Bins'))),
      shinyWidgets::dropdown(
        right = Right, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, inputId = "Histograms-Bins", width = LogoWidth,
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Histogram # Bins')),
        tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Choose the number of bins for the histogram')),
        RemixAutoML::BlankRow(AppWidth),
        shiny::fluidRow(
          width = AppWidth,
          shiny::column(3L, shiny::uiOutput('NumberBins1')),
          shiny::column(3L, shiny::uiOutput('NumberBins2')),
          shiny::column(3L, shiny::uiOutput('NumberBins3')),
          shiny::column(3L, shiny::uiOutput('NumberBins4'))))))
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
                     AppWidth = AppWidth,
                     LogoWidth = LogoWidth,
                     H3Color = H3Color,
                     H4Color = H4Color,
                     Right = FALSE,
                     Animate = TRUE,
                     Status = 'custom') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 3L,
      tags$h4(tags$b(tags$span(style=paste0('color: ', H3Color, ';'), 'Eval Bins'))),
      shinyWidgets::dropdown(
        right = Right, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, inputId = "Percentile-Bins", width = LogoWidth,
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Calibration Plot Bins')),
        tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Choose the number of bins for the relevant model output plots')),
        RemixAutoML::BlankRow(AppWidth),
        shiny::fluidRow(
          width = AppWidth,
          shiny::column(3L, shiny::uiOutput('Percentile_Buckets1')),
          shiny::column(3L, shiny::uiOutput('Percentile_Buckets2')),
          shiny::column(3L, shiny::uiOutput('Percentile_Buckets3')),
          shiny::column(3L, shiny::uiOutput('Percentile_Buckets4'))))))
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
                              AppWidth = AppWidth,
                              LogoWidth = LogoWidth,
                              H3Color = H3Color,
                              H4Color = H4Color,
                              Right = FALSE,
                              Animate = TRUE,
                              Status = 'custom') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 3L,
      tags$h4(tags$span(style=paste0('color: ', H3Color, ';'), tags$b('Shapley Agg'))),
      shinyWidgets::dropdown(
        right = Right, animate = Animate, circle = FALSE, tooltip = FALSE, status = Status, inputId = "Shapely-Agg", width = LogoWidth,
        tags$h3(tags$span(style=paste0('color: ', H3Color, ';'),'Calibration Plot Bins')),
        tags$h4(tags$span(style=paste0('color: ', H4Color, ';'),'Choose the number of bins for the relevant model output plots')),
        RemixAutoML::BlankRow(AppWidth),
        shiny::fluidRow(
          width = AppWidth,
          shiny::column(3L, shiny::uiOutput('ShapAggMethod1')),
          shiny::column(3L, shiny::uiOutput('ShapAggMethod2')),
          shiny::column(3L, shiny::uiOutput('ShapAggMethod3')),
          shiny::column(3L, shiny::uiOutput('ShapAggMethod4'))))))
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
Plotter <- function(id = 'PlotOutput', AppWidth = 12L, PlotWidth = 1100, PlotHeight = 600) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::mainPanel(
      shiny::conditionalPanel(
        condition =  "input.PlotEngine == 'plotly'",
        shiny::column(width = AppWidth, plotly::plotlyOutput("TrendPlotly", width = PlotWidth, height = PlotHeight))
      ),
      shiny::conditionalPanel(
        condition = "input.PlotEngine == 'ggplot2'",
        shiny::column(width = AppWidth, shiny::plotOutput("Trendggplot2"))
      )))
}










