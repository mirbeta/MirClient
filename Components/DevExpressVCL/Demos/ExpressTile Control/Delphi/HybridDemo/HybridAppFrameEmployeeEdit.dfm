inherited frmEmployeeEdit: TfrmEmployeeEdit
  Width = 1088
  Height = 704
  inherited dxLayoutControl1: TdxLayoutControl
    Width = 1088
    Height = 584
    object edPhoto: TcxDBImage [0]
      Left = 59
      Top = 17
      DataBinding.DataField = 'Picture'
      DataBinding.DataSource = DM.dsEmployees
      Properties.GraphicClassName = 'TdxSmartImage'
      Style.HotTrack = True
      TabOrder = 0
      Height = 302
      Width = 245
    end
    object edFirstName: TcxDBTextEdit [1]
      Left = 409
      Top = 17
      DataBinding.DataField = 'FirstName'
      DataBinding.DataSource = DM.dsEmployees
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 1
      Width = 138
    end
    object edLastName: TcxDBTextEdit [2]
      Left = 409
      Top = 56
      DataBinding.DataField = 'LastName'
      DataBinding.DataSource = DM.dsEmployees
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 2
      Width = 138
    end
    object edPrefix: TcxDBLookupComboBox [3]
      Left = 409
      Top = 95
      DataBinding.DataField = 'Prefix'
      DataBinding.DataSource = DM.dsEmployees
      ParentFont = False
      ParentShowHint = False
      Properties.Alignment.Horz = taLeftJustify
      Properties.KeyFieldNames = 'Prefix_ID'
      Properties.ListColumns = <
        item
          FieldName = 'Prefix_Name'
        end>
      Properties.ListOptions.ShowHeader = False
      Properties.ListSource = DM.dsPrefixSpr
      ShowHint = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 3
      Width = 138
    end
    object edTitle: TcxDBTextEdit [4]
      Left = 409
      Top = 134
      DataBinding.DataField = 'Title'
      DataBinding.DataSource = DM.dsEmployees
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 4
      Width = 138
    end
    object edAddress: TcxDBTextEdit [5]
      Left = 409
      Top = 173
      DataBinding.DataField = 'Address_Line'
      DataBinding.DataSource = DM.dsEmployees
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 5
      Width = 138
    end
    object edCity: TcxDBTextEdit [6]
      Left = 409
      Top = 212
      DataBinding.DataField = 'Address_City'
      DataBinding.DataSource = DM.dsEmployees
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 6
      Width = 138
    end
    object edState: TcxDBLookupComboBox [7]
      Left = 409
      Top = 251
      DataBinding.DataField = 'Address_State'
      DataBinding.DataSource = DM.dsEmployees
      ParentFont = False
      Properties.Alignment.Horz = taLeftJustify
      Properties.DropDownSizeable = True
      Properties.KeyFieldNames = 'ID'
      Properties.ListColumns = <
        item
          FieldName = 'LongName'
        end>
      Properties.ListOptions.ShowHeader = False
      Properties.ListSource = DM.dsStatesSpr
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 7
      Width = 138
    end
    object edZipCode: TcxDBTextEdit [8]
      Left = 409
      Top = 290
      DataBinding.DataField = 'Address_ZipCode'
      DataBinding.DataSource = DM.dsEmployees
      ParentFont = False
      Properties.Alignment.Horz = taLeftJustify
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 8
      Width = 138
    end
    object edHomePhone: TcxDBTextEdit [9]
      Left = 696
      Top = 17
      DataBinding.DataField = 'HomePhone'
      DataBinding.DataSource = DM.dsEmployees
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 9
      Width = 162
    end
    object edMobilePhone: TcxDBTextEdit [10]
      Left = 696
      Top = 56
      DataBinding.DataField = 'MobilePhone'
      DataBinding.DataSource = DM.dsEmployees
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 10
      Width = 162
    end
    object edEmail: TcxDBTextEdit [11]
      Left = 696
      Top = 95
      DataBinding.DataField = 'Email'
      DataBinding.DataSource = DM.dsEmployees
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 11
      Width = 162
    end
    object edSkype: TcxDBTextEdit [12]
      Left = 696
      Top = 134
      DataBinding.DataField = 'Skype'
      DataBinding.DataSource = DM.dsEmployees
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 12
      Width = 162
    end
    object edDepartment: TcxDBLookupComboBox [13]
      Left = 696
      Top = 173
      DataBinding.DataField = 'Department'
      DataBinding.DataSource = DM.dsEmployees
      ParentFont = False
      Properties.KeyFieldNames = 'Department_ID'
      Properties.ListColumns = <
        item
          FieldName = 'Department_Name'
        end>
      Properties.ListOptions.ShowHeader = False
      Properties.ListSource = DM.dtsDepartmentSpr
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 13
      Width = 162
    end
    object edStatus: TcxDBLookupComboBox [14]
      Left = 696
      Top = 212
      DataBinding.DataField = 'Status'
      DataBinding.DataSource = DM.dsEmployees
      ParentFont = False
      Properties.KeyFieldNames = 'Status_ID'
      Properties.ListColumns = <
        item
          FieldName = 'Status_Name'
        end>
      Properties.ListOptions.ShowHeader = False
      Properties.ListSource = DM.dsStatusSpr
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 14
      Width = 162
    end
    object edHireDate: TcxDBDateEdit [15]
      Left = 696
      Top = 251
      DataBinding.DataField = 'HireDate'
      DataBinding.DataSource = DM.dsEmployees
      ParentFont = False
      Properties.DateButtons = [btnClear]
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 15
      Width = 162
    end
    object edBirthDate: TcxDBDateEdit [16]
      Left = 696
      Top = 290
      DataBinding.DataField = 'BirthDate'
      DataBinding.DataSource = DM.dsEmployees
      ParentFont = False
      Properties.DateButtons = [btnClear]
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 16
      Width = 162
    end
    object edProfile: TcxDBRichEdit [17]
      Left = 879
      Top = 17
      DataBinding.DataField = 'PersonalProfile'
      DataBinding.DataSource = DM.dsEmployees
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -16
      Style.Font.Name = 'Segoe UI'
      Style.Font.Style = []
      Style.HotTrack = False
      Style.IsFontAssigned = True
      TabOrder = 17
      Height = 302
      Width = 192
    end
    object cxGridTasks: TcxGrid [18]
      Left = 76
      Top = 379
      Width = 978
      Height = 171
      TabOrder = 18
      object gvTasks: TcxGridDBTableView
        Navigator.Buttons.CustomButtons = <>
        DataController.DataSource = DM.dsEmployeesTasks
        DataController.KeyFieldNames = 'Id'
        DataController.Options = [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoSortByDisplayText]
        DataController.Summary.DefaultGroupSummaryItems = <>
        DataController.Summary.FooterSummaryItems = <>
        DataController.Summary.SummaryGroups = <>
        OptionsCustomize.ColumnGrouping = False
        OptionsSelection.CellSelect = False
        OptionsView.ShowEditButtons = gsebForFocusedRecord
        OptionsView.CellAutoHeight = True
        OptionsView.ColumnAutoWidth = True
        OptionsView.DataRowHeight = 40
        OptionsView.GroupByBox = False
        OptionsView.HeaderFilterButtonShowMode = fbmSmartTag
        Styles.Content = DM.cxStyle1
        Styles.Header = DM.cxStyle2
        Styles.Selection = DM.cxStyle3
        object colTask_AssignedTo: TcxGridDBColumn
          Caption = 'Assigned To'
          DataBinding.FieldName = 'AssignedEmployeeId'
          PropertiesClassName = 'TcxLookupComboBoxProperties'
          Properties.KeyFieldNames = 'Id'
          Properties.ListColumns = <
            item
              FieldName = 'FullName'
            end>
          Properties.ListSource = DM.dsEmployeesHelper
          OnGetFilterValues = colTasksTask_SubjectGetFilterValues
          HeaderAlignmentHorz = taCenter
          Options.ShowEditButtons = isebNever
          Width = 142
        end
        object colTask_OwnedBy: TcxGridDBColumn
          Caption = 'Owned By'
          DataBinding.FieldName = 'OwnerId'
          PropertiesClassName = 'TcxLookupComboBoxProperties'
          Properties.KeyFieldNames = 'Id'
          Properties.ListColumns = <
            item
              FieldName = 'FullName'
            end>
          Properties.ListSource = DM.dsEmployeesHelper
          OnGetFilterValues = colTasksTask_SubjectGetFilterValues
          HeaderAlignmentHorz = taCenter
          Options.ShowEditButtons = isebNever
          Width = 143
        end
        object colTasksTask_Subject: TcxGridDBColumn
          DataBinding.FieldName = 'Subject'
          OnGetFilterValues = colTasksTask_SubjectGetFilterValues
          HeaderAlignmentHorz = taCenter
          Width = 465
        end
        object colTasksTask_Priority: TcxGridDBColumn
          DataBinding.FieldName = 'Priority'
          PropertiesClassName = 'TcxImageComboBoxProperties'
          Properties.Alignment.Horz = taCenter
          Properties.DropDownRows = 4
          Properties.Items = <
            item
              ImageIndex = 0
              Value = '0'
            end
            item
              ImageIndex = 1
              Value = 1
            end
            item
              ImageIndex = 2
              Value = 2
            end
            item
              ImageIndex = 3
              Value = 3
            end>
          Properties.ShowDescriptions = False
          OnGetFilterValues = colTasksTask_SubjectGetFilterValues
          HeaderAlignmentHorz = taCenter
          Options.ShowEditButtons = isebNever
          Width = 83
        end
        object colTasksTask_Due_Date: TcxGridDBColumn
          Caption = 'Due Date'
          DataBinding.FieldName = 'DueDate'
          PropertiesClassName = 'TcxTextEditProperties'
          Properties.Alignment.Horz = taCenter
          OnGetFilterValues = colTasksTask_SubjectGetFilterValues
          HeaderAlignmentHorz = taCenter
          Width = 92
        end
        object colTasks_Complete: TcxGridDBColumn
          Caption = '% Complete'
          DataBinding.FieldName = 'Completion'
          PropertiesClassName = 'TcxProgressBarProperties'
          Properties.BeginColor = 10258176
          Properties.EndColor = clNavy
          Properties.OverloadBeginColor = clWhite
          Properties.OverloadEndColor = clWhite
          Properties.PeakValue = 100.000000000000000000
          OnGetFilterValues = colTasksTask_SubjectGetFilterValues
          HeaderAlignmentHorz = taCenter
          Width = 177
        end
      end
      object gvEmployees: TcxGridDBTableView
        Navigator.Buttons.CustomButtons = <>
        DataController.DataSource = DM.dsTaskEmployes
        DataController.DetailKeyFieldNames = 'EmployeeTask_Id'
        DataController.KeyFieldNames = 'EmployeeTask_Id;Employee_Id'
        DataController.MasterKeyFieldNames = 'Id'
        DataController.Options = [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoSortByDisplayText]
        DataController.Summary.DefaultGroupSummaryItems = <>
        DataController.Summary.FooterSummaryItems = <>
        DataController.Summary.SummaryGroups = <>
        OptionsCustomize.ColumnGrouping = False
        OptionsSelection.CellSelect = False
        OptionsView.ShowEditButtons = gsebForFocusedRecord
        OptionsView.ColumnAutoWidth = True
        OptionsView.DataRowHeight = 40
        OptionsView.GroupByBox = False
        OptionsView.HeaderFilterButtonShowMode = fbmSmartTag
        Styles.Content = DM.cxStyle1
        Styles.Header = DM.cxStyle2
        Styles.Selection = DM.cxStyle3
        object gvEmployeesFullName: TcxGridDBColumn
          Caption = 'Full Name'
          DataBinding.FieldName = 'FullName'
          OnGetFilterValues = colTasksTask_SubjectGetFilterValues
          HeaderAlignmentHorz = taCenter
          Options.ShowEditButtons = isebNever
          Width = 150
        end
        object gvEmployeesDepartment: TcxGridDBColumn
          DataBinding.FieldName = 'Department'
          PropertiesClassName = 'TcxLookupComboBoxProperties'
          Properties.KeyFieldNames = 'Department_ID'
          Properties.ListColumns = <
            item
              FieldName = 'Department_Name'
            end>
          Properties.ListSource = DM.dtsDepartmentSpr
          OnGetFilterValues = colTasksTask_SubjectGetFilterValues
          HeaderAlignmentHorz = taCenter
          Options.ShowEditButtons = isebNever
          Width = 150
        end
        object gvEmployeesTitle: TcxGridDBColumn
          DataBinding.FieldName = 'Title'
          OnGetFilterValues = colTasksTask_SubjectGetFilterValues
          HeaderAlignmentHorz = taCenter
          Options.ShowEditButtons = isebNever
          Width = 150
        end
        object gvEmployeesStatus: TcxGridDBColumn
          DataBinding.FieldName = 'Status'
          PropertiesClassName = 'TcxLookupComboBoxProperties'
          Properties.Alignment.Horz = taCenter
          Properties.KeyFieldNames = 'Status_ID'
          Properties.ListColumns = <
            item
              FieldName = 'Status_Name'
            end>
          Properties.ListSource = DM.dsStatusSpr
          OnGetFilterValues = colTasksTask_SubjectGetFilterValues
          HeaderAlignmentHorz = taCenter
          Options.ShowEditButtons = isebNever
          Width = 100
        end
        object gvEmployeesPersonalProfile: TcxGridDBColumn
          Caption = 'Personal Profile'
          DataBinding.FieldName = 'PersonalProfile'
          OnGetFilterValues = colTasksTask_SubjectGetFilterValues
          HeaderAlignmentHorz = taCenter
          Options.ShowEditButtons = isebNever
          Width = 500
        end
      end
      object lvTasks: TcxGridLevel
        GridView = gvTasks
        Options.DetailTabsPosition = dtpTop
        Styles.Tab = DM.cxStyle1
        Styles.TabsBackground = DM.cxStyle5
        object lvEmployees: TcxGridLevel
          Caption = 'Assigned Employees'
          GridView = gvEmployees
        end
      end
    end
    object cxGridNotes: TcxGrid [19]
      Left = 10000
      Top = 10000
      Width = 970
      Height = 167
      TabOrder = 19
      Visible = False
      object gvNotes: TcxGridDBTableView
        Navigator.Buttons.CustomButtons = <>
        DataController.DataSource = DM.dsEvaluations
        DataController.Options = [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoSortByDisplayText]
        DataController.Summary.DefaultGroupSummaryItems = <>
        DataController.Summary.FooterSummaryItems = <>
        DataController.Summary.SummaryGroups = <>
        OptionsCustomize.ColumnGrouping = False
        OptionsSelection.CellSelect = False
        OptionsView.ShowEditButtons = gsebForFocusedRecord
        OptionsView.ColumnAutoWidth = True
        OptionsView.DataRowHeight = 60
        OptionsView.GroupByBox = False
        OptionsView.HeaderFilterButtonShowMode = fbmSmartTag
        Styles.Content = DM.cxStyle1
        Styles.Header = DM.cxStyle2
        Styles.Selection = DM.cxStyle3
        object colNotesCreated_On: TcxGridDBColumn
          Caption = 'Created On'
          DataBinding.FieldName = 'CreatedOn'
          PropertiesClassName = 'TcxTextEditProperties'
          Properties.Alignment.Horz = taLeftJustify
          OnGetFilterValues = colTasksTask_SubjectGetFilterValues
          HeaderAlignmentHorz = taCenter
          Width = 134
        end
        object colNotesCreated_By: TcxGridDBColumn
          Caption = 'Created By'
          DataBinding.FieldName = 'CreatedById'
          PropertiesClassName = 'TcxLookupComboBoxProperties'
          Properties.KeyFieldNames = 'Id'
          Properties.ListColumns = <
            item
              FieldName = 'FullName'
            end>
          Properties.ListSource = DM.dsEmployeesHelper
          OnGetFilterValues = colTasksTask_SubjectGetFilterValues
          HeaderAlignmentHorz = taCenter
          Options.ShowEditButtons = isebNever
          Width = 187
        end
        object colNotesSubject: TcxGridDBColumn
          Caption = 'Subject'
          DataBinding.FieldName = 'NoteCalc'
          PropertiesClassName = 'TcxRichEditProperties'
          OnGetFilterValues = colTasksTask_SubjectGetFilterValues
          HeaderAlignmentHorz = taCenter
          Options.Filtering = False
          Width = 783
        end
      end
      object cxGridNotesLevel1: TcxGridLevel
        GridView = gvNotes
      end
    end
    inherited dxLayoutGroup2: TdxLayoutGroup
      ItemIndex = 1
      ScrollOptions.Vertical = smAuto
    end
    object dxLayoutGroup5: TdxLayoutGroup
      Parent = dxLayoutGroup2
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutGroup5
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Text = 'cxDBImage1'
      CaptionOptions.Visible = False
      Control = edPhoto
      ControlOptions.OriginalHeight = 227
      ControlOptions.OriginalWidth = 245
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup7: TdxLayoutGroup
      Parent = dxLayoutGroup5
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object liFirstName: TdxLayoutItem
      Parent = dxLayoutGroup7
      CaptionOptions.Text = 'FIRST NAME'
      Control = edFirstName
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 138
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liLastName: TdxLayoutItem
      Parent = dxLayoutGroup7
      CaptionOptions.Text = 'LAST NAME'
      Control = edLastName
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 129
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object liPrefix: TdxLayoutItem
      Parent = dxLayoutGroup7
      CaptionOptions.Text = 'PREFIX'
      Control = edPrefix
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 129
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object liTitle: TdxLayoutItem
      Parent = dxLayoutGroup7
      CaptionOptions.Text = 'TITLE'
      Control = edTitle
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 129
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object liAddress: TdxLayoutItem
      Parent = dxLayoutGroup7
      CaptionOptions.Text = 'ADDRESS'
      Control = edAddress
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 129
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object liCity: TdxLayoutItem
      Parent = dxLayoutGroup7
      CaptionOptions.Text = 'CITY'
      Control = edCity
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 129
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object liState: TdxLayoutItem
      Parent = dxLayoutGroup7
      CaptionOptions.Text = 'STATE'
      Control = edState
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object liZipCode: TdxLayoutItem
      Parent = dxLayoutGroup7
      CaptionOptions.Text = 'ZIP CODE'
      Control = edZipCode
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 105
      ControlOptions.ShowBorder = False
      Index = 7
    end
    object dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem
      Parent = dxLayoutGroup5
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      Index = 2
    end
    object liHomePhone: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      CaptionOptions.Text = 'HOME PHONE'
      Control = edHomePhone
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 162
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liMobilePhone: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      CaptionOptions.Text = 'MOBILE PHONE'
      Control = edMobilePhone
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 162
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutGroup5
      AlignHorz = ahLeft
      AlignVert = avClient
      Index = 3
      AutoCreated = True
    end
    object liEmail: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      CaptionOptions.Text = 'EMAIL'
      Control = edEmail
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 162
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object liSkype: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      CaptionOptions.Text = 'SKYPE'
      Control = edSkype
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 154
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object liDepartment: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      CaptionOptions.Text = 'DEPARTMENT'
      Control = edDepartment
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 154
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object liStatus: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      CaptionOptions.Text = 'STATUS'
      Control = edStatus
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 162
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object liHireDate: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      CaptionOptions.Text = 'HIRE DATE'
      Control = edHireDate
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 162
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object liDOB: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      CaptionOptions.Text = 'DOB'
      Control = edBirthDate
      ControlOptions.OriginalHeight = 29
      ControlOptions.OriginalWidth = 162
      ControlOptions.ShowBorder = False
      Index = 7
    end
    object dxLayoutSplitterItem1: TdxLayoutSplitterItem
      Parent = dxLayoutGroup5
      CaptionOptions.Text = 'Splitter'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 4
    end
    object dxLayoutItem22: TdxLayoutItem
      Parent = dxLayoutGroup5
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'cxDBRichEdit1'
      CaptionOptions.Visible = False
      Control = edProfile
      ControlOptions.OriginalHeight = 99
      ControlOptions.OriginalWidth = 917
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutGroup6: TdxLayoutGroup
      Parent = dxLayoutGroup2
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      SizeOptions.Height = 214
      SizeOptions.Width = 1144
      ButtonOptions.Buttons = <>
      LayoutDirection = ldTabbed
      ShowBorder = False
      Index = 1
    end
    object lgTasks: TdxLayoutGroup
      Parent = dxLayoutGroup6
      AlignVert = avClient
      CaptionOptions.Text = 'TASKS'
      ButtonOptions.Buttons = <>
      Index = 0
    end
    object dxLayoutItem23: TdxLayoutItem
      Parent = lgTasks
      AlignVert = avClient
      CaptionOptions.Text = 'cxGrid1'
      CaptionOptions.Visible = False
      Control = cxGridTasks
      ControlOptions.MinHeight = 150
      ControlOptions.OriginalHeight = 200
      ControlOptions.OriginalWidth = 250
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lgNotes: TdxLayoutGroup
      Parent = dxLayoutGroup6
      CaptionOptions.Text = 'NOTES'
      ButtonOptions.Buttons = <>
      Index = 1
    end
    object dxLayoutItem24: TdxLayoutItem
      Parent = lgNotes
      AlignVert = avClient
      CaptionOptions.Text = 'cxGrid1'
      CaptionOptions.Visible = False
      Control = cxGridNotes
      ControlOptions.OriginalHeight = 200
      ControlOptions.OriginalWidth = 250
      ControlOptions.ShowBorder = False
      Index = 0
    end
  end
  inherited cxGroupBox1: TcxGroupBox
    Top = 584
    Width = 1088
    inherited dxLayoutControl2: TdxLayoutControl
      Width = 1084
      object btnSave: TcxButton [0]
        Left = 452
        Top = 17
        Width = 85
        Height = 82
        Caption = 'Save'
        OptionsImage.ImageIndex = 25
        OptionsImage.Images = DM.ilButtons
        OptionsImage.Layout = blGlyphTop
        TabOrder = 0
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        OnClick = btnSaveClick
      end
      object btnCancel: TcxButton [1]
        Left = 547
        Top = 17
        Width = 85
        Height = 82
        Caption = 'Cancel'
        OptionsImage.ImageIndex = 21
        OptionsImage.Images = DM.ilButtons
        OptionsImage.Layout = blGlyphTop
        TabOrder = 1
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        OnClick = btnCancelClick
      end
      object dxLayoutItem1: TdxLayoutItem
        Parent = dxLayoutGroup4
        AlignHorz = ahLeft
        AlignVert = avClient
        CaptionOptions.Visible = False
        Control = btnSave
        ControlOptions.OriginalHeight = 80
        ControlOptions.OriginalWidth = 85
        ControlOptions.ShowBorder = False
        Index = 0
      end
      object dxLayoutItem4: TdxLayoutItem
        Parent = dxLayoutGroup4
        AlignVert = avClient
        CaptionOptions.Visible = False
        Control = btnCancel
        ControlOptions.OriginalHeight = 80
        ControlOptions.OriginalWidth = 85
        ControlOptions.ShowBorder = False
        Index = 1
      end
    end
  end
end
