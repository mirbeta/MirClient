object IssueListGridForm: TIssueListGridForm
  Left = 271
  Top = 149
  BorderStyle = bsNone
  Caption = 'IssueListGridForm'
  ClientHeight = 418
  ClientWidth = 672
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 439
    Top = 24
    Height = 394
    Align = alRight
  end
  object lbDesciption: TLabel
    Left = 0
    Top = 0
    Width = 672
    Height = 24
    Align = alTop
    AutoSize = False
    Color = 12937777
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindow
    Font.Height = -15
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object cxGrid: TcxGrid
    Left = 0
    Top = 24
    Width = 439
    Height = 394
    Align = alClient
    TabOrder = 0
    RootLevelOptions.DetailTabsPosition = dtpLeft
    RootLevelStyles.OnGetTabStyle = cxGridRootLevelStylesGetTabStyle
    OnActiveTabChanged = cxGridActiveTabChanged
    OnFocusedViewChanged = cxGridFocusedViewChanged
    object tvItems: TcxGridDBTableView
      DataController.DataSource = dmMain.dsItems
      DataController.DetailKeyFieldNames = 'PROJECTID'
      DataController.KeyFieldNames = 'ID'
      DataController.MasterKeyFieldNames = 'ID'
      DataController.Summary.DefaultGroupSummaryItems = <
        item
          Kind = skCount
          Position = spFooter
          Column = tvItemsNAME
        end>
      DataController.Summary.FooterSummaryItems = <
        item
          Kind = skCount
          Column = tvItemsNAME
        end>
      DataController.Summary.SummaryGroups = <>
      OptionsBehavior.IncSearch = True
      OptionsView.ColumnAutoWidth = True
      OptionsView.Footer = True
      OptionsView.GroupFooters = gfVisibleWhenExpanded
      Preview.Column = tvItemsDESCRIPTION
      Preview.RightIndent = 45
      Preview.Visible = True
      Styles.OnGetContentStyle = tvItemsStylesGetContentStyle
      Styles.StyleSheet = dmMain.ssTableStyles
      object tvItemsNAME: TcxGridDBColumn
        Caption = 'Subject'
        DataBinding.FieldName = 'NAME'
        Width = 100
      end
      object tvItemsPROJECTID: TcxGridDBColumn
        Caption = 'Project'
        DataBinding.FieldName = 'PROJECTID'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.DropDownListStyle = lsFixedList
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'NAME'
          end>
        Properties.ListSource = dmMain.dsProjects
        Visible = False
        SortIndex = 0
        SortOrder = soAscending
      end
      object tvItemsTYPE: TcxGridDBColumn
        Caption = 'Type'
        DataBinding.FieldName = 'TYPE'
        RepositoryItem = dmMain.edrepItemType
        MinWidth = 50
      end
      object tvItemsOWNERID: TcxGridDBColumn
        Caption = 'Owner'
        DataBinding.FieldName = 'OWNERID'
        RepositoryItem = dmMain.edrepUserLookup
        Visible = False
      end
      object tvItemsCREATEDDATE: TcxGridDBColumn
        Caption = 'CreatedDate'
        DataBinding.FieldName = 'CREATEDDATE'
        PropertiesClassName = 'TcxDateEditProperties'
        Properties.SaveTime = False
        Visible = False
      end
      object tvItemsSTATUS: TcxGridDBColumn
        Caption = 'Status'
        DataBinding.FieldName = 'STATUS'
        RepositoryItem = dmMain.edrepItemStatus
      end
      object tvItemsPRIORITY: TcxGridDBColumn
        Caption = 'Priority'
        DataBinding.FieldName = 'PRIORITY'
        RepositoryItem = dmMain.edrepItemPriority
        MinWidth = 50
      end
      object tvItemsCREATORID: TcxGridDBColumn
        Caption = 'Creator'
        DataBinding.FieldName = 'CREATORID'
        RepositoryItem = dmMain.edrepUserLookup
        Visible = False
      end
      object tvItemsLASTMODIFIEDDATE: TcxGridDBColumn
        Caption = 'LastModifiedDate'
        DataBinding.FieldName = 'LASTMODIFIEDDATE'
        PropertiesClassName = 'TcxDateEditProperties'
        Visible = False
      end
      object tvItemsFIXEDDATE: TcxGridDBColumn
        Caption = 'FixedDate'
        DataBinding.FieldName = 'FIXEDDATE'
        PropertiesClassName = 'TcxDateEditProperties'
        Visible = False
      end
      object tvItemsRESOLUTION: TcxGridDBColumn
        Caption = 'Resolution'
        DataBinding.FieldName = 'RESOLUTION'
        PropertiesClassName = 'TcxBlobEditProperties'
        Visible = False
      end
      object tvItemsDESCRIPTION: TcxGridDBColumn
        Caption = 'Description'
        DataBinding.FieldName = 'DESCRIPTION'
      end
    end
    object tvProjects: TcxGridDBTableView
      DataController.DataSource = dmMain.dsProjects
      DataController.KeyFieldNames = 'ID'
      DataController.Summary.DefaultGroupSummaryItems = <
        item
          Kind = skCount
          Column = tvProjectsNAME
        end>
      DataController.Summary.FooterSummaryItems = <
        item
          Kind = skCount
          Column = tvProjectsNAME
        end>
      DataController.Summary.SummaryGroups = <>
      OptionsView.ColumnAutoWidth = True
      OptionsView.Footer = True
      Styles.StyleSheet = dmMain.ssTableStyles
      object tvProjectsNAME: TcxGridDBColumn
        Caption = 'Name'
        DataBinding.FieldName = 'NAME'
        Width = 100
      end
      object tvProjectsMANAGERID: TcxGridDBColumn
        Caption = 'Manager'
        DataBinding.FieldName = 'MANAGERID'
        RepositoryItem = dmMain.edrepUserLookup
      end
    end
    object tvUsers: TcxGridDBTableView
      DataController.DataSource = dmMain.dsUsers
      DataController.DetailKeyFieldNames = 'DEPARTMENTID'
      DataController.KeyFieldNames = 'ID'
      DataController.MasterKeyFieldNames = 'ID'
      DataController.Summary.DefaultGroupSummaryItems = <
        item
          Kind = skCount
          Position = spFooter
          Column = tvUsersFNAME
        end>
      DataController.Summary.FooterSummaryItems = <
        item
          Kind = skCount
          Column = tvUsersFNAME
        end>
      DataController.Summary.SummaryGroups = <>
      OptionsView.ColumnAutoWidth = True
      OptionsView.Footer = True
      OptionsView.GroupFooters = gfVisibleWhenExpanded
      Styles.StyleSheet = dmMain.ssTableStyles
      object tvUsersFNAME: TcxGridDBColumn
        Caption = 'Forename'
        DataBinding.FieldName = 'FNAME'
        Width = 40
      end
      object tvUsersMNAME: TcxGridDBColumn
        Caption = 'Middlename'
        DataBinding.FieldName = 'MNAME'
        Width = 30
      end
      object tvUsersLNAME: TcxGridDBColumn
        Caption = 'Surname'
        DataBinding.FieldName = 'LNAME'
        Width = 40
      end
      object tvUsersEMAIL: TcxGridDBColumn
        Caption = 'E-mail'
        DataBinding.FieldName = 'EMAIL'
        Width = 40
      end
      object tvUsersPHONE: TcxGridDBColumn
        Caption = 'Phone'
        DataBinding.FieldName = 'PHONE'
        Width = 40
      end
      object tvUsersDEPARTMENTID: TcxGridDBColumn
        Caption = 'Department'
        DataBinding.FieldName = 'DEPARTMENTID'
        RepositoryItem = dmMain.edrepDepartmentName
        SortIndex = 0
        SortOrder = soAscending
      end
    end
    object tvTeams: TcxGridDBTableView
      DataController.DataSource = dmMain.dsTeam
      DataController.Summary.DefaultGroupSummaryItems = <
        item
          Kind = skCount
          Column = tvTeamsUSERID
        end>
      DataController.Summary.FooterSummaryItems = <
        item
          Kind = skCount
          Column = tvTeamsUSERID
        end>
      DataController.Summary.SummaryGroups = <>
      OptionsView.ColumnAutoWidth = True
      OptionsView.Footer = True
      Styles.StyleSheet = dmMain.ssTableStyles
      object tvTeamsPROJECTID: TcxGridDBColumn
        Caption = 'Project'
        DataBinding.FieldName = 'PROJECTID'
        RepositoryItem = dmMain.edrepProjectName
        Visible = False
        GroupIndex = 0
        SortIndex = 0
        SortOrder = soAscending
      end
      object tvTeamsUSERID: TcxGridDBColumn
        Caption = 'User'
        DataBinding.FieldName = 'USERID'
        RepositoryItem = dmMain.edrepUserLookup
        Width = 300
      end
      object tvTeamsFUNCTION: TcxGridDBColumn
        Caption = 'Function'
        DataBinding.FieldName = 'FUNCTION'
      end
    end
    object tvDepartments: TcxGridDBTableView
      DataController.DataSource = dmMain.dsDepartments
      DataController.KeyFieldNames = 'ID'
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <
        item
          Kind = skCount
          Column = tvDepartmentsNAME
        end>
      DataController.Summary.SummaryGroups = <>
      OptionsView.ColumnAutoWidth = True
      OptionsView.Footer = True
      Styles.StyleSheet = dmMain.ssTableStyles
      object tvDepartmentsNAME: TcxGridDBColumn
        Caption = 'Name'
        DataBinding.FieldName = 'NAME'
      end
    end
    object btnSchedule: TcxGridDBBandedTableView
      DataController.DataSource = dmMain.dsScheduler
      DataController.Summary.DefaultGroupSummaryItems = <
        item
          Kind = skSum
          Position = spFooter
          Column = btnScheduleFRIDAY
        end
        item
          Kind = skSum
          Position = spFooter
          Column = btnScheduleMONDAY
        end
        item
          Kind = skSum
          Position = spFooter
          Column = btnScheduleSATURDAY
        end
        item
          Kind = skSum
          Position = spFooter
          Column = btnScheduleSUNDAY
        end
        item
          Kind = skSum
          Position = spFooter
          Column = btnScheduleTHURSDAY
        end
        item
          Kind = skSum
          Position = spFooter
          Column = btnScheduleTUESDAY
        end
        item
          Kind = skSum
          Position = spFooter
          Column = btnScheduleWEDNESDAY
        end
        item
          Kind = skAverage
          Position = spFooter
          Column = btnScheduleRowAvg
        end
        item
          Kind = skSum
          Position = spFooter
          Column = btnScheduleRowSum
        end>
      DataController.Summary.FooterSummaryItems = <
        item
          Kind = skSum
          Column = btnScheduleFRIDAY
        end
        item
          Kind = skSum
          Column = btnScheduleMONDAY
        end
        item
          Kind = skSum
          Column = btnScheduleSATURDAY
        end
        item
          Kind = skSum
          Column = btnScheduleSUNDAY
        end
        item
          Kind = skSum
          Column = btnScheduleTHURSDAY
        end
        item
          Kind = skSum
          Column = btnScheduleTUESDAY
        end
        item
          Kind = skSum
          Column = btnScheduleWEDNESDAY
        end
        item
          Kind = skAverage
          Column = btnScheduleRowAvg
        end
        item
          Kind = skSum
          Column = btnScheduleRowSum
        end>
      DataController.Summary.SummaryGroups = <>
      OptionsView.Footer = True
      OptionsView.GroupFooters = gfVisibleWhenExpanded
      Styles.Selection = dmMain.stSelected
      Styles.Footer = dmMain.stLightYellow
      Styles.BandHeader = dmMain.stSelected
      Bands = <
        item
          Caption = 'User'
          FixedKind = fkLeft
          Options.Moving = False
          Styles.Content = dmMain.stLightBlue
          Width = 150
        end
        item
          Caption = 'Days'
        end
        item
          Caption = 'Totals'
          FixedKind = fkRight
          Options.Moving = False
          Styles.Content = dmMain.stLightBlue
          Width = 200
        end>
      object btnScheduleID: TcxGridDBBandedColumn
        DataBinding.FieldName = 'ID'
        Visible = False
        Position.BandIndex = 0
        Position.ColIndex = 2
        Position.RowIndex = 0
      end
      object btnSchedulePROJECTID: TcxGridDBBandedColumn
        Caption = 'Project Name'
        DataBinding.FieldName = 'PROJECTID'
        RepositoryItem = dmMain.edrepProjectName
        Visible = False
        GroupIndex = 0
        SortIndex = 0
        SortOrder = soAscending
        Position.BandIndex = 0
        Position.ColIndex = 1
        Position.RowIndex = 0
      end
      object btnScheduleUSERID: TcxGridDBBandedColumn
        Caption = 'Full Name'
        DataBinding.FieldName = 'USERID'
        RepositoryItem = dmMain.edrepUserFullName
        Position.BandIndex = 0
        Position.ColIndex = 0
        Position.RowIndex = 0
      end
      object btnScheduleSUNDAY: TcxGridDBBandedColumn
        Caption = 'Sunday'
        DataBinding.FieldName = 'SUNDAY'
        Position.BandIndex = 1
        Position.ColIndex = 0
        Position.RowIndex = 0
      end
      object btnScheduleMONDAY: TcxGridDBBandedColumn
        Caption = 'Monday'
        DataBinding.FieldName = 'MONDAY'
        Position.BandIndex = 1
        Position.ColIndex = 1
        Position.RowIndex = 0
      end
      object btnScheduleTUESDAY: TcxGridDBBandedColumn
        Caption = 'Tuesday'
        DataBinding.FieldName = 'TUESDAY'
        Position.BandIndex = 1
        Position.ColIndex = 2
        Position.RowIndex = 0
      end
      object btnScheduleWEDNESDAY: TcxGridDBBandedColumn
        Caption = 'Wednesday'
        DataBinding.FieldName = 'WEDNESDAY'
        Position.BandIndex = 1
        Position.ColIndex = 3
        Position.RowIndex = 0
      end
      object btnScheduleTHURSDAY: TcxGridDBBandedColumn
        Caption = 'Thursday'
        DataBinding.FieldName = 'THURSDAY'
        Position.BandIndex = 1
        Position.ColIndex = 4
        Position.RowIndex = 0
      end
      object btnScheduleFRIDAY: TcxGridDBBandedColumn
        Caption = 'Friday'
        DataBinding.FieldName = 'FRIDAY'
        Position.BandIndex = 1
        Position.ColIndex = 5
        Position.RowIndex = 0
      end
      object btnScheduleSATURDAY: TcxGridDBBandedColumn
        Caption = 'Saturday'
        DataBinding.FieldName = 'SATURDAY'
        Position.BandIndex = 1
        Position.ColIndex = 6
        Position.RowIndex = 0
      end
      object btnScheduleRowSum: TcxGridDBBandedColumn
        Caption = 'SUM'
        DataBinding.FieldName = 'RowSum'
        Position.BandIndex = 2
        Position.ColIndex = 0
        Position.RowIndex = 0
      end
      object btnScheduleRowAvg: TcxGridDBBandedColumn
        Caption = 'AVG'
        DataBinding.FieldName = 'RowAvg'
        Position.BandIndex = 2
        Position.ColIndex = 1
        Position.RowIndex = 0
      end
    end
    object lvProjects: TcxGridLevel
      Tag = 1
      Caption = 'Projects'
      GridView = tvProjects
      MaxDetailHeight = 450
      object lvProjectItems: TcxGridLevel
        GridView = tvItems
      end
    end
    object lvItems: TcxGridLevel
      Tag = 2
      Caption = 'Project Items'
    end
    object lvDepartments: TcxGridLevel
      Tag = 3
      Caption = 'Departments'
      GridView = tvDepartments
      object lvDepartmentUsers: TcxGridLevel
        GridView = tvUsers
      end
    end
    object lvTeam: TcxGridLevel
      Tag = 4
      Caption = 'Project Teams'
      GridView = tvTeams
    end
    object lvUsers: TcxGridLevel
      Tag = 5
      Caption = 'Users'
    end
    object lvSchedule: TcxGridLevel
      Tag = 6
      Caption = 'Schedule'
      GridView = btnSchedule
    end
  end
  object pnlForm: TPanel
    Left = 442
    Top = 24
    Width = 230
    Height = 394
    Align = alRight
    BevelOuter = bvLowered
    Constraints.MinWidth = 230
    TabOrder = 1
  end
  object cxGridPopupMenu1: TcxGridPopupMenu
    Grid = cxGrid
    PopupMenus = <>
    Left = 144
    Top = 72
  end
end
