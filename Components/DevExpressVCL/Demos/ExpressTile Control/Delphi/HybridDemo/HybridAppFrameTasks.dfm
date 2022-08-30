inherited frmTasks: TfrmTasks
  inherited dxLayoutControl1: TdxLayoutControl
    object tcTasksState: TdxTileControl [0]
      Left = 59
      Top = 17
      Width = 309
      Height = 413
      Align = alNone
      Images = ilTasks
      OptionsBehavior.ItemCheckMode = tcicmNone
      OptionsBehavior.ItemHotTrackHighlightColor = 1069596864
      OptionsBehavior.ItemHotTrackMode = tcihtmHighlight
      OptionsBehavior.ItemMoving = False
      OptionsBehavior.ItemOuterFrameColor = clSilver
      OptionsBehavior.ItemPressAnimation = False
      OptionsBehavior.ScrollMode = smScrollButtons
      OptionsView.CenterContentHorz = True
      OptionsView.FixedIndentHorz = True
      OptionsView.FixedIndentVert = True
      OptionsView.GroupBlockMaxColumnCount = 1
      OptionsView.GroupLayout = glVertical
      OptionsView.GroupMaxRowCount = 1000
      OptionsView.IndentHorz = 50
      OptionsView.IndentVert = 5
      OptionsView.ItemHeight = 85
      OptionsView.ItemWidth = 200
      TabOrder = 0
      Title.Font.Charset = DEFAULT_CHARSET
      Title.Font.Color = clGray
      Title.Font.Height = -16
      Title.Font.Name = 'Segoe UI'
      Title.Font.Style = []
      Title.Text = ' TASKS'
      OnItemFocusChange = tcTasksStateItemFocusChange
      object tcTasksStateGroup: TdxTileControlGroup
        Index = 0
      end
      object tiAllTasks: TdxTileControlItem
        Glyph.Align = oaTopLeft
        Glyph.ImageIndex = 0
        GroupIndex = 0
        IndexInGroup = 0
        Style.GradientBeginColor = clWhite
        Text1.AssignedValues = []
        Text2.Align = oaTopRight
        Text2.AssignedValues = [avFont]
        Text2.Font.Charset = DEFAULT_CHARSET
        Text2.Font.Color = clSilver
        Text2.Font.Height = -40
        Text2.Font.Name = 'Segoe UI'
        Text2.Font.Style = []
        Text2.IndentVert = 0
        Text2.Value = '0'
        Text3.AssignedValues = [avTextColor, avFont]
        Text3.Font.Charset = DEFAULT_CHARSET
        Text3.Font.Color = 536870912
        Text3.Font.Height = -16
        Text3.Font.Name = 'Segoe UI'
        Text3.Font.Style = []
        Text3.Value = 'All Tasks'
        Text3.TextColor = clGray
        Text4.AssignedValues = []
      end
      object tiInProgress: TdxTileControlItem
        Tag = 2
        Glyph.Align = oaTopLeft
        Glyph.ImageIndex = 1
        GroupIndex = 0
        IndexInGroup = 3
        Style.GradientBeginColor = clWhite
        Text1.AssignedValues = []
        Text2.Align = oaTopRight
        Text2.AssignedValues = [avFont]
        Text2.Font.Charset = DEFAULT_CHARSET
        Text2.Font.Color = clSilver
        Text2.Font.Height = -40
        Text2.Font.Name = 'Segoe UI'
        Text2.Font.Style = []
        Text2.IndentVert = 0
        Text2.Value = '0'
        Text3.AssignedValues = [avTextColor, avFont]
        Text3.Font.Charset = DEFAULT_CHARSET
        Text3.Font.Color = 536870912
        Text3.Font.Height = -16
        Text3.Font.Name = 'Segoe UI'
        Text3.Font.Style = []
        Text3.Value = 'In Progress'
        Text3.TextColor = clGray
        Text4.AssignedValues = []
      end
      object tiNotStarted: TdxTileControlItem
        Glyph.Align = oaTopLeft
        Glyph.ImageIndex = 2
        GroupIndex = 0
        IndexInGroup = 1
        Style.GradientBeginColor = clWhite
        Text1.AssignedValues = []
        Text2.Align = oaTopRight
        Text2.AssignedValues = [avFont]
        Text2.Font.Charset = DEFAULT_CHARSET
        Text2.Font.Color = clSilver
        Text2.Font.Height = -40
        Text2.Font.Name = 'Segoe UI'
        Text2.Font.Style = []
        Text2.IndentVert = 0
        Text2.Value = '0'
        Text3.AssignedValues = [avTextColor, avFont]
        Text3.Font.Charset = DEFAULT_CHARSET
        Text3.Font.Color = 536870912
        Text3.Font.Height = -16
        Text3.Font.Name = 'Segoe UI'
        Text3.Font.Style = []
        Text3.Value = 'Not Started'
        Text3.TextColor = clGray
        Text4.AssignedValues = []
      end
      object tiNeedAssist: TdxTileControlItem
        Tag = 3
        Glyph.Align = oaTopLeft
        Glyph.ImageIndex = 7
        GroupIndex = 0
        IndexInGroup = 4
        Style.GradientBeginColor = clWhite
        Text1.AssignedValues = []
        Text2.Align = oaTopRight
        Text2.AssignedValues = [avFont]
        Text2.Font.Charset = DEFAULT_CHARSET
        Text2.Font.Color = clSilver
        Text2.Font.Height = -40
        Text2.Font.Name = 'Segoe UI'
        Text2.Font.Style = []
        Text2.IndentVert = 0
        Text2.Value = '0'
        Text3.AssignedValues = [avTextColor, avFont]
        Text3.Font.Charset = DEFAULT_CHARSET
        Text3.Font.Color = 536870912
        Text3.Font.Height = -16
        Text3.Font.Name = 'Segoe UI'
        Text3.Font.Style = []
        Text3.Value = 'Need Assistance'
        Text3.TextColor = clGray
        Text4.AssignedValues = []
      end
      object tiDeferred: TdxTileControlItem
        Tag = 4
        Glyph.Align = oaTopLeft
        Glyph.ImageIndex = 3
        GroupIndex = 0
        IndexInGroup = 5
        Style.GradientBeginColor = clWhite
        Text1.AssignedValues = []
        Text2.Align = oaTopRight
        Text2.AssignedValues = [avFont]
        Text2.Font.Charset = DEFAULT_CHARSET
        Text2.Font.Color = clSilver
        Text2.Font.Height = -40
        Text2.Font.Name = 'Segoe UI'
        Text2.Font.Style = []
        Text2.IndentVert = 0
        Text2.Value = '0'
        Text3.AssignedValues = [avTextColor, avFont]
        Text3.Font.Charset = DEFAULT_CHARSET
        Text3.Font.Color = 536870912
        Text3.Font.Height = -16
        Text3.Font.Name = 'Segoe UI'
        Text3.Font.Style = []
        Text3.Value = 'Deferred'
        Text3.TextColor = clGray
        Text4.AssignedValues = []
      end
      object tiCompleted: TdxTileControlItem
        Tag = 1
        Glyph.Align = oaTopLeft
        Glyph.ImageIndex = 4
        GroupIndex = 0
        IndexInGroup = 2
        Style.GradientBeginColor = clWhite
        Text1.AssignedValues = []
        Text2.Align = oaTopRight
        Text2.AssignedValues = [avFont]
        Text2.Font.Charset = DEFAULT_CHARSET
        Text2.Font.Color = clSilver
        Text2.Font.Height = -40
        Text2.Font.Name = 'Segoe UI'
        Text2.Font.Style = []
        Text2.IndentVert = 0
        Text2.Value = '0'
        Text3.AssignedValues = [avTextColor, avFont]
        Text3.Font.Charset = DEFAULT_CHARSET
        Text3.Font.Color = 536870912
        Text3.Font.Height = -16
        Text3.Font.Name = 'Segoe UI'
        Text3.Font.Style = []
        Text3.Value = 'Completed'
        Text3.TextColor = clGray
        Text4.AssignedValues = []
      end
      object tiHighPriority: TdxTileControlItem
        Tag = 6
        Glyph.Align = oaTopLeft
        Glyph.ImageIndex = 5
        GroupIndex = 0
        IndexInGroup = 6
        Style.GradientBeginColor = clWhite
        Text1.AssignedValues = []
        Text2.Align = oaTopRight
        Text2.AssignedValues = [avFont]
        Text2.Font.Charset = DEFAULT_CHARSET
        Text2.Font.Color = clSilver
        Text2.Font.Height = -40
        Text2.Font.Name = 'Segoe UI'
        Text2.Font.Style = []
        Text2.IndentVert = 0
        Text2.Value = '0'
        Text3.AssignedValues = [avTextColor, avFont]
        Text3.Font.Charset = DEFAULT_CHARSET
        Text3.Font.Color = 536870912
        Text3.Font.Height = -16
        Text3.Font.Name = 'Segoe UI'
        Text3.Font.Style = []
        Text3.Value = 'High Priority'
        Text3.TextColor = clGray
        Text4.AssignedValues = []
      end
      object tiUrgent: TdxTileControlItem
        Tag = 7
        Glyph.Align = oaTopLeft
        Glyph.ImageIndex = 6
        GroupIndex = 0
        IndexInGroup = 7
        Style.GradientBeginColor = clWhite
        Text1.AssignedValues = []
        Text2.Align = oaTopRight
        Text2.AssignedValues = [avFont]
        Text2.Font.Charset = DEFAULT_CHARSET
        Text2.Font.Color = clSilver
        Text2.Font.Height = -40
        Text2.Font.Name = 'Segoe UI'
        Text2.Font.Style = []
        Text2.IndentVert = 0
        Text2.Value = '0'
        Text3.AssignedValues = [avTextColor, avFont]
        Text3.Font.Charset = DEFAULT_CHARSET
        Text3.Font.Color = 536870912
        Text3.Font.Height = -16
        Text3.Font.Name = 'Segoe UI'
        Text3.Font.Style = []
        Text3.Value = 'Urgent'
        Text3.TextColor = clGray
        Text4.AssignedValues = []
      end
    end
    object cxGridTasks: TcxGrid [1]
      Left = 378
      Top = 17
      Width = 490
      Height = 413
      TabOrder = 1
      LookAndFeel.NativeStyle = False
      object gvTasks: TcxGridDBTableView
        OnKeyDown = gvTasksKeyDown
        Navigator.Buttons.CustomButtons = <>
        FindPanel.DisplayMode = fpdmAlways
        FindPanel.Position = fppBottom
        DataController.DataSource = DM.dsTasks
        DataController.KeyFieldNames = 'Id'
        DataController.Options = [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoSortByDisplayText]
        DataController.Summary.DefaultGroupSummaryItems = <>
        DataController.Summary.FooterSummaryItems = <
          item
            Kind = skCount
            OnGetText = gvTasksTcxGridDBDataControllerTcxDataSummaryFooterSummaryItems0GetText
            FieldName = 'Id'
            Column = colTasks_Complete
          end>
        DataController.Summary.SummaryGroups = <>
        OptionsCustomize.ColumnGrouping = False
        OptionsData.CancelOnExit = False
        OptionsData.DeletingConfirmation = False
        OptionsData.Editing = False
        OptionsData.Inserting = False
        OptionsSelection.CellSelect = False
        OptionsView.ShowEditButtons = gsebForFocusedRecord
        OptionsView.CellAutoHeight = True
        OptionsView.ColumnAutoWidth = True
        OptionsView.DataRowHeight = 40
        OptionsView.Footer = True
        OptionsView.FooterAutoHeight = True
        OptionsView.GroupByBox = False
        OptionsView.HeaderFilterButtonShowMode = fbmSmartTag
        Styles.Content = DM.cxStyle1
        Styles.Footer = DM.cxStyle1
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
          OnGetFilterValues = colTasks_SubjectGetFilterValues
          HeaderAlignmentHorz = taCenter
          Options.ShowEditButtons = isebNever
          Width = 47
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
          OnGetFilterValues = colTasks_SubjectGetFilterValues
          HeaderAlignmentHorz = taCenter
          Options.ShowEditButtons = isebNever
          Width = 123
        end
        object colTasks_Subject: TcxGridDBColumn
          DataBinding.FieldName = 'Subject'
          OnGetFilterValues = colTasks_SubjectGetFilterValues
          HeaderAlignmentHorz = taCenter
          Width = 145
        end
        object colTasks_Priority: TcxGridDBColumn
          DataBinding.FieldName = 'Priority'
          PropertiesClassName = 'TcxImageComboBoxProperties'
          Properties.Alignment.Horz = taCenter
          Properties.DropDownRows = 4
          Properties.Images = DM.ilPriority
          Properties.Items = <
            item
              Description = 'Low'
              ImageIndex = 0
              Value = 0
            end
            item
              Description = 'Normal'
              ImageIndex = 1
              Value = 1
            end
            item
              Description = 'High'
              ImageIndex = 2
              Value = 2
            end
            item
              Description = 'Urgent'
              ImageIndex = 3
              Value = 3
            end>
          Properties.ShowDescriptions = False
          OnGetFilterValues = colTasks_SubjectGetFilterValues
          HeaderAlignmentHorz = taCenter
          Options.ShowEditButtons = isebNever
          Width = 37
        end
        object colTasks_Due_Date: TcxGridDBColumn
          Caption = 'Due Date'
          DataBinding.FieldName = 'DueDate'
          PropertiesClassName = 'TcxTextEditProperties'
          Properties.Alignment.Horz = taCenter
          OnGetFilterValues = colTasks_SubjectGetFilterValues
          HeaderAlignmentHorz = taCenter
          SortIndex = 0
          SortOrder = soDescending
          Width = 41
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
          OnGetFilterValues = colTasks_SubjectGetFilterValues
          HeaderAlignmentHorz = taCenter
          Width = 78
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
        OptionsData.CancelOnExit = False
        OptionsData.Deleting = False
        OptionsData.DeletingConfirmation = False
        OptionsData.Editing = False
        OptionsData.Inserting = False
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
          OnGetFilterValues = colTasks_SubjectGetFilterValues
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
          OnGetFilterValues = colTasks_SubjectGetFilterValues
          HeaderAlignmentHorz = taCenter
          Options.ShowEditButtons = isebNever
          Width = 150
        end
        object gvEmployeesTitle: TcxGridDBColumn
          DataBinding.FieldName = 'Title'
          OnGetFilterValues = colTasks_SubjectGetFilterValues
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
          OnGetFilterValues = colTasks_SubjectGetFilterValues
          HeaderAlignmentHorz = taCenter
          Options.ShowEditButtons = isebNever
          Width = 100
        end
        object gvEmployeesPersonalProfile: TcxGridDBColumn
          Caption = 'Personal Profile'
          DataBinding.FieldName = 'PersonalProfile'
          OnGetFilterValues = colTasks_SubjectGetFilterValues
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
    inherited dxLayoutGroup2: TdxLayoutGroup
      LayoutDirection = ldHorizontal
      Index = 0
    end
    inherited lgBackButton: TdxLayoutGroup
      Parent = dxLayoutGroup2
      Visible = False
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Text = 'dxTileControl1'
      CaptionOptions.Visible = False
      Control = tcTasksState
      ControlOptions.OriginalHeight = 488
      ControlOptions.OriginalWidth = 309
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahClient
      AlignVert = avClient
      Control = cxGridTasks
      ControlOptions.OriginalHeight = 470
      ControlOptions.OriginalWidth = 594
      ControlOptions.ShowBorder = False
      Index = 2
    end
  end
  inherited cxGroupBox1: TcxGroupBox
    inherited dxLayoutControl2: TdxLayoutControl
      object btnEdit: TcxButton [0]
        Left = 295
        Top = 17
        Width = 85
        Height = 82
        Caption = 'Edit'
        OptionsImage.ImageIndex = 22
        OptionsImage.Images = DM.ilButtons
        OptionsImage.Layout = blGlyphTop
        SpeedButtonOptions.Flat = True
        TabOrder = 0
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        OnClick = btnEditClick
      end
      object btnDelete: TcxButton [1]
        Left = 390
        Top = 17
        Width = 85
        Height = 82
        Caption = 'Delete'
        OptionsImage.ImageIndex = 20
        OptionsImage.Images = DM.ilButtons
        OptionsImage.Layout = blGlyphTop
        SpeedButtonOptions.Flat = True
        TabOrder = 1
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        OnClick = btnDeleteClick
      end
      object btnPrint: TcxButton [2]
        Left = 501
        Top = 17
        Width = 85
        Height = 80
        Caption = 'Print'
        OptionsImage.ImageIndex = 18
        OptionsImage.Images = DM.ilButtons
        OptionsImage.Layout = blGlyphTop
        SpeedButtonOptions.Flat = True
        TabOrder = 2
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        OnClick = btnPrintClick
      end
      object dxLayoutItem1: TdxLayoutItem
        Parent = dxLayoutGroup4
        AlignVert = avClient
        CaptionOptions.Visible = False
        Control = btnEdit
        ControlOptions.OriginalHeight = 80
        ControlOptions.OriginalWidth = 85
        ControlOptions.ShowBorder = False
        Index = 0
      end
      object dxLayoutItem7: TdxLayoutItem
        Parent = dxLayoutGroup4
        AlignVert = avClient
        CaptionOptions.Visible = False
        Control = btnDelete
        ControlOptions.OriginalHeight = 80
        ControlOptions.OriginalWidth = 85
        ControlOptions.ShowBorder = False
        Index = 1
      end
      object dxLayoutSeparatorItem2: TdxLayoutSeparatorItem
        Parent = dxLayoutGroup4
        CaptionOptions.Text = 'Separator'
        Index = 2
      end
      object dxLayoutItem8: TdxLayoutItem
        Parent = dxLayoutGroup4
        AlignVert = avClient
        CaptionOptions.Visible = False
        Control = btnPrint
        ControlOptions.OriginalHeight = 80
        ControlOptions.OriginalWidth = 85
        ControlOptions.ShowBorder = False
        Index = 3
      end
    end
  end
  object ilTasks: TcxImageList
    SourceDPI = 96
    Height = 48
    Width = 48
    FormatVersion = 1
    DesignInfo = 4194568
    ImageInfo = <
      item
        Image.Data = {
          36240000424D3624000000000000360000002800000030000000300000000100
          2000000000000024000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000404040B75959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF0909091B404040B7595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF404040B70909091B595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF404040B700000000595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF00000000595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF00000000595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF00000000595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF00000000595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF585858FC595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF00000000595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF00000000595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF00000000595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF00000000595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF00000000595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF00000000595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF2424246622222260585858FC595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF00000000595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF00000000595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF00000000595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF24242466000000000000000022222260585858FC595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF00000000595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF00000000595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF00000000595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF242424660000000000000000000000000000000022222260585858FC5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF00000000595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF00000000595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF00000000595959FF595959FF595959FF595959FF595959FF595959FF2424
          2466000000000000000000000000000000000000000000000000222222605858
          58FC595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF00000000595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF00000000595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF00000000595959FF595959FF595959FF595959FF595959FF242424660000
          00000000000000000000242424662727276F0000000000000000000000002222
          2260585858FC595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF00000000595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF00000000595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF00000000595959FF595959FF595959FF595959FF595959FF2F2F2F870000
          00000000000024242466595959FF595959FF2727276F00000000000000000000
          000022222260585858FC595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF00000000595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF00000000595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF00000000595959FF595959FF595959FF595959FF595959FF595959FF2F2F
          2F8725252569595959FF595959FF595959FF595959FF2727276F000000000000
          00000000000022222260585858FC595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF00000000595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF00000000595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF00000000595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF2727276F0000
          0000000000000000000022222260585858FC595959FF595959FF595959FF5959
          59FF595959FF595959FF00000000595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF00000000595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF00000000595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF2727
          276F00000000000000000000000022222260585858FC595959FF595959FF5959
          59FF595959FF595959FF00000000595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF00000000595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF00000000595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF2727276F00000000000000000000000022222260585858FC595959FF5959
          59FF595959FF595959FF00000000595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF00000000595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF00000000595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF2727276F00000000000000002D2D2D81595959FF595959FF5959
          59FF595959FF595959FF00000000595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF2727276F2D2D2D81595959FF595959FF595959FF5959
          59FF595959FF595959FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF00000000595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF00000000595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF00000000595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF00000000595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF00000000595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF00000000595959FF5959
          59FF595959FF19191948000000000000000019191948595959FF595959FF5959
          59FF00000000595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF00000000595959FF595959FF595959FF191919480000
          00000000000019191948595959FF595959FF595959FF00000000595959FF5959
          59FF595959FF00000000595959FF595959FF00000000595959FF595959FF5959
          59FF000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000595959FF595959FF595959FF000000005959
          59FF595959FF00000000595959FF595959FF595959FF00000000424242BD5959
          59FF595959FF00000000595959FF595959FF00000000595959FF595959FF5959
          59FF00000000595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF00000000595959FF595959FF595959FF000000005959
          59FF595959FF00000000595959FF595959FF424242BD00000000000000000000
          00000000000000000000595959FF595959FF0000000000000000000000000000
          000000000000595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF00000000000000000000000000000000000000005959
          59FF595959FF0000000000000000000000000000000000000000000000000000
          00000000000000000000595959FF595959FF0000000000000000000000000000
          000000000000595959FF595959FF595959FF595959FF404040B7080808180808
          0818404040B7595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF404040B70808081808080818404040B7595959FF5959
          59FF595959FF595959FF00000000000000000000000000000000000000005959
          59FF595959FF0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000595959FF595959FF595959FF595959FF0909091B404040B74040
          40B70909091B595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF0909091B404040B7404040B70909091B595959FF5959
          59FF595959FF595959FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000595959FF595959FF595959FF595959FF00000000595959FF5959
          59FF00000000595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF00000000595959FF595959FF00000000595959FF5959
          59FF595959FF595959FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000424242BD595959FF595959FF595959FF00000000595959FF5959
          59FF00000000595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF00000000595959FF595959FF00000000595959FF5959
          59FF595959FF424242BD00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000595959FF5959
          59FF000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000595959FF595959FF00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000424242BD4242
          42BD000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000424242BD424242BD00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36240000424D3624000000000000360000002800000030000000300000000100
          2000000000000024000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000003C3C3CB7545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF3C3C3CB7000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000545454FF545454FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000545454FF545454FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000545454FF545454FF000000005454
          54FF545454FF545454FF545454FF545454FF545454FF00000000545454FF5454
          54FF545454FF545454FF545454FF545454FF0000000000000000000000000000
          000000000000000000000000000000000000545454FF545454FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000545454FF545454FF000000005454
          54FF545454FF545454FF545454FF545454FF545454FF00000000545454FF5454
          54FF545454FF545454FF545454FF545454FF0000000000000000000000000000
          000000000000000000000000000000000000545454FF545454FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000545454FF545454FF000000005454
          54FF545454FF545454FF545454FF545454FF545454FF00000000545454FF5454
          54FF545454FF545454FF545454FF545454FF0000000000000000000000000000
          000000000000000000000000000000000000545454FF545454FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000545454FF545454FF000000005454
          54FF545454FF545454FF545454FF545454FF545454FF00000000545454FF5454
          54FF545454FF545454FF545454FF545454FF0000000000000000000000000000
          000000000000000000000000000000000000545454FF545454FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000545454FF545454FF000000005454
          54FF545454FF545454FF545454FF545454FF545454FF00000000545454FF5454
          54FF545454FF545454FF545454FF545454FF0000000000000000000000000000
          000000000000000000000000000000000000545454FF545454FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000545454FF545454FF000000005454
          54FF545454FF545454FF545454FF545454FF545454FF00000000545454FF5454
          54FF545454FF545454FF545454FF545454FF0000000000000000000000000000
          000000000000000000000000000000000000545454FF545454FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000545454FF545454FF000000005454
          54FF545454FF545454FF545454FF545454FF545454FF00000000545454FF5454
          54FF545454FF545454FF545454FF545454FF0000000000000000000000000000
          000000000000000000000000000000000000545454FF545454FF000000000000
          0000181818484C4C4CE7545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF0000000000000000545454FF545454FF000000005454
          54FF545454FF545454FF545454FF545454FF545454FF00000000545454FF5454
          54FF545454FF545454FF545454FF545454FF0000000000000000000000000000
          000000000000000000000000000000000000545454FF545454FF000000000000
          00004B4B4BE4545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF0000000000000000545454FF545454FF000000005454
          54FF545454FF545454FF545454FF545454FF545454FF00000000545454FF5454
          54FF545454FF545454FF545454FF545454FF0000000000000000000000000000
          000000000000000000000000000000000000545454FF545454FF000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF0000000000000000545454FF545454FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000545454FF545454FF000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF0000000000000000545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000003E3E3EBD545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF3E3E3EBD000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF08080818000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF3F3F3FC0121212360000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF2A2A2A7E000000002A2A
          2A7E545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF2A2A2A7E00000000000000000000
          00002A2A2A7E545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF2A2A2A7E0000000000000000000000000000
          0000000000002A2A2A7E545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF2A2A2A7E0000000000000000000000000A0A0A1F0000
          000000000000000000002A2A2A7E545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF2A2A2A7E0000000000000000000000002A2A2A81545454FF2A2A
          2A810000000000000000000000002A2A2A7E545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF0000000000000000000000002A2A2A81545454FF545454FF5454
          54FF2A2A2A810000000000000000000000002A2A2A7E545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF2A2A2A81000000002A2A2A81545454FF545454FF545454FF5454
          54FF545454FF2A2A2A810000000000000000000000002A2A2A7E545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF2A2A2A810000000000000000000000002A2A2A7E5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF2A2A2A810000000000000000000000002A2A
          2A7E545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF2A2A2A8100000000000000000000
          00002A2A2A7E545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF2A2A2A81000000000000
          000000000000545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF2A2A2A810000
          00002A2A2A81545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF3C3C3CB7080808180000
          0000080808183C3C3CB7545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF3C3C3CB70808081800000000080808183C3C3CB7545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF0909091B3C3C3CB75454
          54FF3C3C3CB70909091B545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF0909091B3C3C3CB7545454FF3C3C3CB70909091B545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00004C4C4CE7545454FF545454FF545454FF545454FF00000000545454FF5454
          54FF545454FF00000000545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF00000000545454FF545454FF545454FF00000000545454FF545454FF5454
          54FF545454FF4C4C4CE700000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00001515153F424242C9545454FF545454FF545454FF00000000545454FF5454
          54FF545454FF00000000545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF00000000545454FF545454FF545454FF00000000545454FF545454FF5454
          54FF424242C91515153F00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000545454FF5454
          54FF545454FF0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000545454FF545454FF545454FF0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000545454FF5454
          54FF545454FF0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000545454FF545454FF545454FF0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000003E3E3EBD5454
          54FF3E3E3EBD0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000003E3E3EBD545454FF3E3E3EBD0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36240000424D3624000000000000360000002800000030000000300000000100
          2000000000000024000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000909091B2424246C3C3C
          3CB7434343CC545454FF545454FF434343CC3C3C3CB72424246C0909091B0000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000909091B3333339C535353FC545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF535353FC3333
          339C0909091B0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000001A1A1A4E505050F3545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF505050F31A1A1A4E00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000002525256F545454FF545454FF545454FF545454FF3D3D3DBA2222
          226611111133000000000000000011111133222222663D3D3DBA545454FF5454
          54FF545454FF545454FF2525256F000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00001A1A1A4E545454FF545454FF545454FF484848DB1414143C000000000000
          00000000000000000000000000000000000000000000000000001414143C4848
          48DB545454FF545454FF545454FF1A1A1A4E0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000909
          091B505050F3545454FF545454FF404040C30606061200000000000000000000
          0000000000000000000000000000000000000000000000000000000000000606
          06123F3F3FC0545454FF545454FF505050F30909091B00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000003232
          3299545454FF545454FF484848DB0505050F0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000505050F484848DB545454FF545454FF3232329900000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000000B0B0B215353
          53FC545454FF545454FF1515153F000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000001515153F545454FF545454FF535353FC0B0B0B21000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000282828785454
          54FF545454FF3E3E3EBD00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000003E3E3EBD545454FF545454FF28282878000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000003E3E3EBD5454
          54FF545454FF2323236900000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000023232369545454FF545454FF3D3D3DBA000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000004B4B4BE45454
          54FF545454FF0E0E0E2A00000000000000000000000000000000000000000000
          0000181818484C4C4CE74C4C4CE7181818480000000000000000000000000000
          000000000000000000000C0C0C24545454FF545454FF4B4B4BE4000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000525252F95454
          54FF545454FF0505050F00000000000000000000000000000000000000000000
          00004B4B4BE4545454FF545454FF4B4B4BE40000000000000000000000000000
          0000000000000000000002020206545454FF545454FF525252F9000000000000
          0000181818484C4C4CE7545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF0202020600000000525252F95454
          54FF545454FF0202020600000000000000000000000000000000000000000000
          00004C4C4CE7545454FF545454FF4C4C4CE70000000000000000000000000000
          0000000000000000000002020206545454FF545454FF525252F9000000000000
          00004B4B4BE4545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF07070715000000004B4B4BE45454
          54FF545454FF0C0C0C2400000000000000000000000000000000000000000000
          00001515153F545454FF545454FF1515153F0000000000000000000000000000
          000000000000000000000C0C0C24545454FF545454FF4B4B4BE4000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF1515153F000000003E3E3EBD5454
          54FF545454FF2323236900000000000000000000000000000000000000000000
          000000000000545454FF545454FF000000000000000000000000000000000000
          0000000000000000000023232369545454FF545454FF3D3D3DBA000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF2828287800000000282828785454
          54FF545454FF3E3E3EBD00000000000000000000000000000000000000000000
          000000000000545454FF545454FF000000000000000000000000000000000000
          000000000000000000003E3E3EBD545454FF545454FF28282878000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF3F3F3FC0000000000A0A0A1E5353
          53FC545454FF545454FF1515153F000000000000000000000000000000000000
          000000000000545454FF545454FF000000000000000000000000000000000000
          0000000000001515153F545454FF545454FF535353FC0A0A0A1E000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF0D0D0D27000000003232
          3299545454FF545454FF484848DB060606120000000000000000000000000000
          000000000000545454FF545454FF000000000000000000000000000000000000
          000006060612484848DB545454FF545454FF3232329900000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF383838AB000000000909
          091B4F4F4FF0545454FF545454FF404040C30606061200000000000000000000
          000000000000545454FF545454FF000000000000000000000000000000000606
          0612404040C3545454FF545454FF4F4F4FF00909091B00000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF2A2A2A7E000000002A2A
          2A7E545454FF545454FF545454FF545454FF545454FF545454FF1414143C0000
          00001A1A1A4E535353FC545454FF545454FF484848DB1515153F000000000000
          000000000000545454FF545454FF0000000000000000000000001515153F4848
          48DB545454FF545454FF535353FC1A1A1A4E0000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF2A2A2A7E00000000000000000000
          00002A2A2A7E545454FF545454FF545454FF545454FF545454FF494949DE0707
          0715000000002525256F545454FF545454FF545454FF545454FF3D3D3DBA2222
          226611111133010101030000000011111133222222663D3D3DBA545454FF5454
          54FF545454FF545454FF2525256F000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF2A2A2A7E0000000000000000000000000000
          0000000000002A2A2A7E545454FF545454FF545454FF545454FF545454FF3F3F
          3FC0020202060000000018181848505050F3545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF515151F64E4E4EEE4C4C4CE7181818480000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF2A2A2A7E0000000000000000000000000A0A0A1F0000
          000000000000000000002A2A2A7E545454FF545454FF545454FF545454FF5454
          54FF3F3F3FC006060612000000000808081832323299535353FC545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF535353FC3232
          32994C4C4CE7545454FF545454FF4B4B4BE40000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF2A2A2A7E0000000000000000000000002A2A2A81545454FF2A2A
          2A810000000000000000000000002A2A2A7E545454FF545454FF545454FF5454
          54FF545454FF4A4A4AE11313133900000000000000000909091B2424246C3C3C
          3CB7545454FF545454FF545454FF545454FF3C3C3CB72424246C0909091B0000
          00004C4C4CE7545454FF545454FF4C4C4CE70000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF0000000000000000000000002A2A2A81545454FF545454FF5454
          54FF2A2A2A810000000000000000000000002A2A2A7E545454FF545454FF5454
          54FF545454FF545454FF545454FF393939AE0D0D0D2700000000000000000000
          0000545454FF545454FF545454FF545454FF0000000000000000000000000000
          00001515153F424242C9424242C91515153F0000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF2A2A2A81000000002A2A2A81545454FF545454FF545454FF5454
          54FF545454FF2A2A2A810000000000000000000000002A2A2A7E545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF00000000000000003C3C
          3CB7545454FF545454FF545454FF545454FF3C3C3CB700000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF2A2A2A810000000000000000000000002A2A2A7E5454
          54FF545454FF545454FF545454FF545454FF545454FF00000000000000005454
          54FF545454FF545454FF545454FF545454FF545454FF00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF2A2A2A810000000000000000000000002A2A
          2A7E545454FF545454FF545454FF545454FF545454FF00000000000000003E3E
          3EBD545454FF545454FF545454FF545454FF3E3E3EBD00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF2A2A2A8100000000000000000000
          00002A2A2A7E545454FF545454FF545454FF545454FF08080818000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF2A2A2A81000000000000
          000000000000545454FF545454FF545454FF545454FF3F3F3FC0121212360000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF2A2A2A810000
          00002A2A2A81545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF3C3C3CB7080808180000
          0000080808183C3C3CB7545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF3C3C3CB70808081800000000080808183C3C3CB7545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF0909091B3C3C3CB75454
          54FF3C3C3CB70909091B545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF0909091B3C3C3CB7545454FF3C3C3CB70909091B545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00004C4C4CE7545454FF545454FF545454FF545454FF00000000545454FF5454
          54FF545454FF00000000545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF00000000545454FF545454FF545454FF00000000545454FF545454FF5454
          54FF545454FF4C4C4CE700000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00001515153F424242C9545454FF545454FF545454FF00000000545454FF5454
          54FF545454FF00000000545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF00000000545454FF545454FF545454FF00000000545454FF545454FF5454
          54FF424242C91515153F00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000545454FF5454
          54FF545454FF0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000545454FF545454FF545454FF0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000545454FF5454
          54FF545454FF0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000545454FF545454FF545454FF0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000003E3E3EBD5454
          54FF3E3E3EBD0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000003E3E3EBD545454FF3E3E3EBD0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36240000424D3624000000000000360000002800000030000000300000000100
          2000000000000024000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000909091B2424246C3C3C
          3CB7434343CC545454FF545454FF0000000000000000545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF2A2A2A7E00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000909091B3333339C535353FC545454FF5454
          54FF545454FF545454FF545454FF0000000000000000545454FF545454FF5454
          54FF545454FF545454FF545454FF2A2A2A7E0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000001A1A1A4E505050F3545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF0000000000000000545454FF545454FF5454
          54FF545454FF545454FF2A2A2A7E000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000002525256F545454FF545454FF545454FF545454FF3D3D3DBA2222
          22661111113300000000000000000000000000000000545454FF545454FF5454
          54FF545454FF545454FF2525256F000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00001A1A1A4E545454FF545454FF545454FF484848DB1414143C000000000000
          00000000000000000000000000000000000000000000545454FF545454FF5454
          54FF545454FF545454FF545454FF1A1A1A4E0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000909
          091B505050F3545454FF545454FF404040C30606061200000000000000000000
          00000000000000000000000000000000000000000000545454FF545454FF2C2C
          2C873F3F3FC0545454FF545454FF505050F30909091B00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000003232
          3299545454FF545454FF484848DB0505050F0000000000000000000000000000
          00000000000000000000000000000000000000000000545454FF2A2A2A7E0000
          00000505050F484848DB545454FF545454FF3232329900000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000000B0B0B215353
          53FC545454FF545454FF1515153F000000000000000000000000000000000000
          000000000000000000000000000000000000000000002A2A2A7E000000000000
          0000000000001515153F545454FF545454FF535353FC0B0B0B21000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000282828785454
          54FF545454FF3E3E3EBD00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000003E3E3EBD545454FF545454FF28282878000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000003E3E3EBD5454
          54FF545454FF2323236900000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000023232369545454FF545454FF3D3D3DBA000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000004B4B4BE45454
          54FF545454FF0E0E0E2A00000000000000000000000000000000000000000000
          000019191948515151E9515151E9191919480000000000000000000000000000
          000000000000000000000C0C0C24545454FF545454FF4B4B4BE4000000000000
          0000181818484C4C4CE7545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF0202020600000000525252F95454
          54FF545454FF0505050F00000000000000000000000000000000000000000000
          0000505050E6595959FF595959FF505050E60000000000000000000000000000
          0000000000000000000002020206545454FF545454FF525252F9000000000000
          00004B4B4BE4545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF0202020600000000525252F95454
          54FF545454FF0202020600000000000000000000000000000000000000000707
          0715585858FB595959FF595959FF585858FB0707071500000000000000000000
          0000000000000000000002020206545454FF545454FF525252F9000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF07070715000000004B4B4BE45454
          54FF545454FF0C0C0C2400000000000000000000000000000000070707154949
          49D2595959FF565656F5565656F5595959FF494949D207070715000000000000
          000000000000000000000C0C0C24545454FF545454FF4B4B4BE4000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF1515153F000000003E3E3EBD5454
          54FF545454FF2323236900000000000000000000000007070715494949D25959
          59FF484848CF0707071507070715484848CF595959FF494949D2070707150000
          0000000000000000000023232369545454FF545454FF3D3D3DBA000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF2828287800000000282828785454
          54FF545454FF3E3E3EBD000000000000000000000000494949D2595959FF4848
          48CF07070715000000000000000007070715484848CF595959FF494949D20707
          071500000000000000003E3E3EBD545454FF545454FF28282878000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF3F3F3FC0000000000A0A0A1E5353
          53FC545454FF545454FF1515153F0000000000000000595959FF484848CF0707
          07150000000000000000000000000000000007070715484848CF595959FF4949
          49D2000000001515153F545454FF545454FF535353FC0A0A0A1E000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF0D0D0D27000000003232
          3299545454FF545454FF484848DB060606120000000000000000000000000000
          0000000000000000000000000000000000000000000007070715484848CF5959
          59FF06060612484848DB545454FF545454FF3232329900000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF2A2A2A7E000000002A2A
          2A7E545454FF545454FF545454FF545454FF545454FF383838AB000000000909
          091B4F4F4FF0545454FF545454FF404040C30606061200000000000000000000
          0000000000000000000000000000000000000000000000000000000000000606
          0612404040C3545454FF545454FF4F4F4FF00909091B00000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF2A2A2A7E00000000000000000000
          00002A2A2A7E545454FF545454FF545454FF545454FF545454FF1414143C0000
          00001A1A1A4E535353FC545454FF545454FF484848DB1515153F000000000000
          00000000000000000000000000000000000000000000000000001515153F4848
          48DB545454FF545454FF535353FC1A1A1A4E0000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF2A2A2A7E0000000000000000000000000000
          0000000000002A2A2A7E545454FF545454FF545454FF545454FF494949DE0707
          0715000000002525256F545454FF545454FF545454FF545454FF3D3D3DBA2222
          226611111133010101030000000011111133222222663D3D3DBA545454FF5454
          54FF545454FF545454FF2525256F000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF2A2A2A7E0000000000000000000000000A0A0A1F0000
          000000000000000000002A2A2A7E545454FF545454FF545454FF545454FF3F3F
          3FC0020202060000000018181848505050F3545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF505050F31818184800000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF2A2A2A7E0000000000000000000000002A2A2A81545454FF2A2A
          2A810000000000000000000000002A2A2A7E545454FF545454FF545454FF5454
          54FF3F3F3FC006060612000000000808081832323299535353FC545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF535353FC3232
          3299080808180000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF0000000000000000000000002A2A2A81545454FF545454FF5454
          54FF2A2A2A810000000000000000000000002A2A2A7E545454FF545454FF5454
          54FF545454FF4A4A4AE11313133900000000000000000909091B2424246C3C3C
          3CB7434343CC535353FC535353FC434343CC3C3C3CB72424246C0909091B0000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF2A2A2A81000000002A2A2A81545454FF545454FF545454FF5454
          54FF545454FF2A2A2A810000000000000000000000002A2A2A7E545454FF5454
          54FF545454FF545454FF545454FF393939AE0D0D0D2700000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF2A2A2A810000000000000000000000002A2A2A7E5454
          54FF545454FF545454FF545454FF545454FF545454FF454545D22A2A2A811414
          143C111111330101010300000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF2A2A2A810000000000000000000000002A2A
          2A7E545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF2A2A2A8100000000000000000000
          00002A2A2A7E545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF2A2A2A81000000000000
          000000000000545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF2A2A2A810000
          00002A2A2A81545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF3C3C3CB7080808180000
          0000080808183C3C3CB7545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF3C3C3CB70808081800000000080808183C3C3CB7545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF0909091B3C3C3CB75454
          54FF3C3C3CB70909091B545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF0909091B3C3C3CB7545454FF3C3C3CB70909091B545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00004C4C4CE7545454FF545454FF545454FF545454FF00000000545454FF5454
          54FF545454FF00000000545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF00000000545454FF545454FF545454FF00000000545454FF545454FF5454
          54FF545454FF4C4C4CE700000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00001515153F424242C9545454FF545454FF545454FF00000000545454FF5454
          54FF545454FF00000000545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF00000000545454FF545454FF545454FF00000000545454FF545454FF5454
          54FF424242C91515153F00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000545454FF5454
          54FF545454FF0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000545454FF545454FF545454FF0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000545454FF5454
          54FF545454FF0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000545454FF545454FF545454FF0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000003E3E3EBD5454
          54FF3E3E3EBD0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000003E3E3EBD545454FF3E3E3EBD0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36240000424D3624000000000000360000002800000030000000300000000100
          2000000000000024000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000003C3C3CB7545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF3C3C3CB7000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000545454FF545454FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000545454FF545454FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000545454FF545454FF000000005454
          54FF545454FF545454FF545454FF545454FF545454FF00000000545454FF5454
          54FF545454FF545454FF545454FF545454FF00000000545454FF545454FF5454
          54FF545454FF545454FF545454FF00000000545454FF545454FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000545454FF545454FF000000005454
          54FF545454FF545454FF545454FF545454FF545454FF00000000545454FF5454
          54FF545454FF545454FF545454FF545454FF00000000545454FF545454FF5454
          54FF545454FF545454FF545454FF00000000545454FF545454FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000545454FF545454FF000000005454
          54FF545454FF545454FF545454FF545454FF545454FF00000000545454FF5454
          54FF545454FF545454FF545454FF545454FF00000000545454FF545454FF5454
          54FF545454FF545454FF545454FF00000000545454FF545454FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000545454FF545454FF000000005454
          54FF545454FF545454FF545454FF545454FF545454FF00000000545454FF5454
          54FF545454FF545454FF545454FF545454FF00000000545454FF545454FF5454
          54FF545454FF545454FF545454FF00000000545454FF545454FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000545454FF545454FF000000005454
          54FF545454FF545454FF545454FF545454FF545454FF00000000545454FF5454
          54FF545454FF545454FF545454FF545454FF00000000545454FF545454FF5454
          54FF545454FF545454FF545454FF00000000545454FF545454FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000545454FF545454FF000000005454
          54FF545454FF545454FF545454FF545454FF545454FF00000000545454FF5454
          54FF545454FF545454FF545454FF545454FF00000000545454FF545454FF5454
          54FF545454FF545454FF545454FF00000000545454FF545454FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000545454FF545454FF000000005454
          54FF545454FF545454FF545454FF545454FF545454FF00000000545454FF5454
          54FF545454FF545454FF545454FF545454FF00000000545454FF545454FF5454
          54FF545454FF545454FF545454FF00000000545454FF545454FF000000000000
          0000181818484C4C4CE7545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF0000000000000000545454FF545454FF000000005454
          54FF545454FF545454FF545454FF545454FF545454FF00000000545454FF5454
          54FF545454FF545454FF545454FF545454FF00000000545454FF545454FF5454
          54FF545454FF545454FF545454FF00000000545454FF545454FF000000000000
          00004B4B4BE4545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF0000000000000000545454FF545454FF000000005454
          54FF545454FF545454FF545454FF545454FF545454FF00000000545454FF5454
          54FF545454FF545454FF545454FF545454FF00000000545454FF545454FF5454
          54FF545454FF545454FF545454FF00000000545454FF545454FF000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF0000000000000000545454FF545454FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000545454FF545454FF000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF0000000000000000545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000003E3E3EBD545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF3E3E3EBD000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF08080818000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF3F3F3FC0121212360000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF2A2A2A7E000000002A2A
          2A7E545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF2A2A2A7E00000000000000000000
          00002A2A2A7E545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF2A2A2A7E0000000000000000000000000000
          0000000000002A2A2A7E545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF2A2A2A7E0000000000000000000000000A0A0A1F0000
          000000000000000000002A2A2A7E545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF2A2A2A7E0000000000000000000000002A2A2A81545454FF2A2A
          2A810000000000000000000000002A2A2A7E545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF0000000000000000000000002A2A2A81545454FF545454FF5454
          54FF2A2A2A810000000000000000000000002A2A2A7E545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF2A2A2A81000000002A2A2A81545454FF545454FF545454FF5454
          54FF545454FF2A2A2A810000000000000000000000002A2A2A7E545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF2A2A2A810000000000000000000000002A2A2A7E5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF2A2A2A810000000000000000000000002A2A
          2A7E545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF2A2A2A8100000000000000000000
          00002A2A2A7E545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF2A2A2A81000000000000
          000000000000545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF2A2A2A810000
          00002A2A2A81545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF3C3C3CB7080808180000
          0000080808183C3C3CB7545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF3C3C3CB70808081800000000080808183C3C3CB7545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF0909091B3C3C3CB75454
          54FF3C3C3CB70909091B545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF0909091B3C3C3CB7545454FF3C3C3CB70909091B545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00004C4C4CE7545454FF545454FF545454FF545454FF00000000545454FF5454
          54FF545454FF00000000545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF00000000545454FF545454FF545454FF00000000545454FF545454FF5454
          54FF545454FF4C4C4CE700000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00001515153F424242C9545454FF545454FF545454FF00000000545454FF5454
          54FF545454FF00000000545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF00000000545454FF545454FF545454FF00000000545454FF545454FF5454
          54FF424242C91515153F00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000545454FF5454
          54FF545454FF0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000545454FF545454FF545454FF0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000545454FF5454
          54FF545454FF0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000545454FF545454FF545454FF0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000003E3E3EBD5454
          54FF3E3E3EBD0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000003E3E3EBD545454FF3E3E3EBD0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36240000424D3624000000000000360000002800000030000000300000000100
          2000000000000024000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000404040B75959
          59FF595959FF595959FF595959FF595959FF595959FF404040B7000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000181818484C4C4CE7545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF3434349F010101030000
          00002C2C2C7E595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF2C2C2C7E0000000000000000000000000000
          00004B4B4BE4545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF3434349F0101
          0103000000002C2C2C7E595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF2C2C2C7E000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF3434
          349F01010103000000002C2C2C7E595959FF595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF595959FF2C2C2C7E00000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF3434349F01010103000000002C2C2C7E595959FF595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
          59FF2C2C2C7E0000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF3434349F01010103000000002C2C2C7E595959FF595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF2C2C
          2C7E000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF3434349F01010103000000002C2C2C7E595959FF5959
          59FF595959FF595959FF595959FF595959FF595959FF595959FF2C2C2C7E0000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF3434349F01010103000000002C2C2C7E5959
          59FF595959FF595959FF595959FF595959FF595959FF2C2C2C7E000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF2A2A2A7E000000002A2A
          2A7E545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF3434349F01010103000000002C2C
          2C7E595959FF595959FF595959FF595959FF2C2C2C7E00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF2A2A2A7E00000000000000000000
          00002A2A2A7E545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF3434349F010101030000
          00002C2C2C7E595959FF595959FF2C2C2C7E0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF2A2A2A7E0000000000000000000000000000
          0000000000002A2A2A7E545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF3434349F0101
          0103000000002C2C2C7E2C2C2C7E000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF2A2A2A7E0000000000000000000000000A0A0A1F0000
          000000000000000000002A2A2A7E545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF3434
          349F010101030000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF2A2A2A7E0000000000000000000000002A2A2A81545454FF2A2A
          2A810000000000000000000000002A2A2A7E545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF3434349F0101010300000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF0000000000000000000000002A2A2A81545454FF545454FF5454
          54FF2A2A2A810000000000000000000000002A2A2A7E545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF3434349F00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF2A2A2A81000000002A2A2A81545454FF545454FF545454FF5454
          54FF545454FF2A2A2A810000000000000000000000002A2A2A7E545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF2A2A2A810000000000000000000000002A2A2A7E5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF2A2A2A810000000000000000000000002A2A
          2A7E545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF2A2A2A8100000000000000000000
          00002A2A2A7E545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF2A2A2A81000000000000
          000000000000545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF2A2A2A810000
          00002A2A2A81545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF3C3C3CB7080808180000
          0000080808183C3C3CB7545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF3C3C3CB70808081800000000080808183C3C3CB7545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF0909091B3C3C3CB75454
          54FF3C3C3CB70909091B545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF0909091B3C3C3CB7545454FF3C3C3CB70909091B545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00004C4C4CE7545454FF545454FF545454FF545454FF00000000545454FF5454
          54FF545454FF00000000545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF00000000545454FF545454FF545454FF00000000545454FF545454FF5454
          54FF545454FF4C4C4CE700000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00001515153F424242C9545454FF545454FF545454FF00000000545454FF5454
          54FF545454FF00000000545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF00000000545454FF545454FF545454FF00000000545454FF545454FF5454
          54FF424242C91515153F00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000545454FF5454
          54FF545454FF0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000545454FF545454FF545454FF0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000545454FF5454
          54FF545454FF0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000545454FF545454FF545454FF0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000003E3E3EBD5454
          54FF3E3E3EBD0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000003E3E3EBD545454FF3E3E3EBD0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36240000424D3624000000000000360000002800000030000000300000000100
          2000000000000024000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000002B2B2B84535353FC545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF535353FC2828287800000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000535353FC545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF505050F300000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000003E3E3EBD545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF3A3A3AB100000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000010101030535353FC545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF3C3C3CB7080808180808
          08183C3C3CB7545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF525252F90C0C0C2400000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000031313196545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF0909091B000000000000
          00000909091B545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF2F2F2F900000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000060606124F4F4FF0545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF08080818000000000000
          000008080818545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF4E4E4EED060606120000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000028282878545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF3F3F3FC0121212361212
          12363F3F3FC0545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF23232369000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000002020206484848DB545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF474747D801010103000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000001919194B545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF1919194B00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000404040C3545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF181818481818
          1848545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF3B3B3BB40000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000010101030535353FC5454
          54FF545454FF545454FF545454FF545454FF545454FF4E4E4EED000000000000
          00004E4E4EED545454FF545454FF545454FF545454FF545454FF545454FF5353
          53FC0D0D0D270000000000000000000000000000000000000000000000000000
          0000181818484C4C4CE7545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF2B2B2B8400000000313131965454
          54FF545454FF545454FF545454FF545454FF545454FF414141C6000000000000
          0000414141C6545454FF545454FF545454FF545454FF545454FF545454FF3131
          3196000000000000000000000000000000000000000000000000000000000000
          00004B4B4BE4545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF505050F30909091B070707155050
          50F3545454FF545454FF545454FF545454FF545454FF353535A2000000000000
          0000353535A2545454FF545454FF545454FF545454FF545454FF4E4E4EED0606
          0612000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF363636A5000000002828
          2878545454FF545454FF545454FF545454FF545454FF2A2A2A81000000000000
          00002929297B545454FF545454FF545454FF545454FF545454FF2525256F0000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF1414143C0202
          0206484848DB545454FF545454FF545454FF545454FF1F1F1F5D000000000000
          00001F1F1F5D545454FF545454FF545454FF545454FF484848DB020202060000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF404040C30000
          00001A1A1A4E545454FF545454FF545454FF545454FF13131339000000000000
          000013131339545454FF545454FF545454FF545454FF1919194B000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF1E1E
          1E5A00000000404040C3545454FF545454FF545454FF06060612000000000000
          000006060612545454FF545454FF545454FF3D3D3DBA00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF4B4B
          4BE40404040C10101030535353FC545454FF545454FF16161642000000000000
          000016161642545454FF545454FF535353FC1010103000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF2A2A2A7E000000002A2A
          2A7E545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF2929297B0000000031313196545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF313131960000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF2A2A2A7E00000000000000000000
          00002A2A2A7E545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF505050F30909091B0909091B505050F3545454FF545454FF545454FF5454
          54FF545454FF545454FF505050F3060606120000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF2A2A2A7E0000000000000000000000000000
          0000000000002A2A2A7E545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF363636A50000000028282878545454FF545454FF545454FF5454
          54FF545454FF545454FF28282878000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF2A2A2A7E0000000000000000000000000A0A0A1F0000
          000000000000000000002A2A2A7E545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF1111113302020206484848DB545454FF545454FF5454
          54FF545454FF484848DB02020206000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF2A2A2A7E0000000000000000000000002A2A2A81545454FF2A2A
          2A810000000000000000000000002A2A2A7E545454FF545454FF545454FF5454
          54FF545454FF545454FF404040C3000000001C1C1C54545454FF545454FF5454
          54FF545454FF1B1B1B5100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF0000000000000000000000002A2A2A81545454FF545454FF5454
          54FF2A2A2A810000000000000000000000002A2A2A7E545454FF545454FF5454
          54FF545454FF545454FF545454FF1E1E1E5A00000000404040C3545454FF5454
          54FF404040C30000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF2A2A2A81000000002A2A2A81545454FF545454FF545454FF5454
          54FF545454FF2A2A2A810000000000000000000000002A2A2A7E545454FF5454
          54FF545454FF545454FF545454FF4B4B4BE4020202060C0C0C24484848DB4747
          47D80C0C0C240202020600000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF2A2A2A810000000000000000000000002A2A2A7E5454
          54FF545454FF545454FF545454FF545454FF2A2A2A7E00000000000000000000
          0000000000002A2A2A7E00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF2A2A2A810000000000000000000000002A2A
          2A7E545454FF545454FF545454FF545454FF535353FC2929297B0A0A0A1E0909
          091B28282878535353FC00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF2A2A2A8100000000000000000000
          00002A2A2A7E545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF2A2A2A81000000000000
          000000000000545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF2A2A2A810000
          00002A2A2A81545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF3C3C3CB7080808180000
          0000080808183C3C3CB7545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF3C3C3CB70808081800000000080808183C3C3CB7545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000545454FF545454FF545454FF545454FF545454FF0909091B3C3C3CB75454
          54FF3C3C3CB70909091B545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF0909091B3C3C3CB7545454FF3C3C3CB70909091B545454FF545454FF5454
          54FF545454FF545454FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00004C4C4CE7545454FF545454FF545454FF545454FF00000000545454FF5454
          54FF545454FF00000000545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF00000000545454FF545454FF545454FF00000000545454FF545454FF5454
          54FF545454FF4C4C4CE700000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00001515153F424242C9545454FF545454FF545454FF00000000545454FF5454
          54FF545454FF00000000545454FF545454FF545454FF545454FF545454FF5454
          54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
          54FF00000000545454FF545454FF545454FF00000000545454FF545454FF5454
          54FF424242C91515153F00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000545454FF5454
          54FF545454FF0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000545454FF545454FF545454FF0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000545454FF5454
          54FF545454FF0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000545454FF545454FF545454FF0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000003E3E3EBD5454
          54FF3E3E3EBD0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000003E3E3EBD545454FF3E3E3EBD0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36240000424D3624000000000000360000002800000030000000300000000100
          2000000000000024000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000303030E1212124E202020882B2B2BB8343434DE3A3A3AF63A3A3AF63434
          34DE2B2B2BB8202020881212124E0303030E0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000050505141919
          196C2D2D2DC03C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF2D2D2DC01919196C05050514000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000011111147292929AF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF292929AF111111470000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000017171760333333D73C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF333333D71717
          1760000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000017171760363636E53C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3636
          36E5171717600000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000011111147333333D73C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF333333D71111114700000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000005050514292929AF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF0000000000000000000000000000
          00003C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF292929AF05050514000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000001919196C3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF0000000000000000000000000000
          00003C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF1919196C000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000303030E2D2D2DC03C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF2D2D2DC00303030E0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00001212124E3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF1212124E0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000202020883C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF0000000000000000000000000000
          00003C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF202020880000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00002B2B2BB83C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF0404041100000000000000000000
          0000323232D43C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF2B2B2BB80000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000343434DE3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF2C2C2CBB00000000000000000000
          000008080822383838EE3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF343434DE0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00003A3A3AF63C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF2C2C2CBB000000000000
          00000000000008080822383838EE3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF3A3A3AF60000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00003A3A3AF63C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF303030CC0000
          0000000000000000000014141455383838EE3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF3A3A3AF60000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000343434DE3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF2424
          2499000000000000000000000000202020883C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF343434DE0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00002B2B2BB83C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3838
          38EE000000000000000000000000080808223C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF2B2B2BB80000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000202020883C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF000000000000000000000000242424993C3C3CFF3C3C3CFF3C3C3CFF3838
          38EE000000000000000000000000080808223C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF202020880000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00001212124E3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF04040411000000000000000004040411343434DD3C3C3CFF3C3C3CFF2020
          2088000000000000000000000000202020883C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF1212124E0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000303030E2D2D2DC03C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF2C2C2CBB0000000000000000000000000000000014141455141414550000
          0000000000000000000008080822383838EE3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF2D2D2DC00303030E0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000001919196C3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF2020208800000000000000000000000000000000000000000000
          00000000000004040411303030CC3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF1919196C000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000005050514292929AF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF2C2C2CBB181818660000000000000000000000000404
          0411202020882F2F2FC73C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF292929AF05050514000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000011111147333333D73C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF333333D71111114700000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000017171760363636E53C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3636
          36E5171717600000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000017171760333333D73C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF333333D71717
          1760000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000011111147292929AF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF292929AF111111470000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000050505141919
          196C2D2D2DC03C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C
          3CFF3C3C3CFF3C3C3CFF3C3C3CFF2D2D2DC01919196C05050514000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000303030E1212124E202020882B2B2BB8343434DE3A3A3AF63A3A3AF63434
          34DE2B2B2BB8202020881212124E0303030E0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end>
  end
end
