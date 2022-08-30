object dxSpreadSheetFindAndReplaceDialogForm: TdxSpreadSheetFindAndReplaceDialogForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  ClientHeight = 412
  ClientWidth = 514
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 514
    Height = 225
    Align = alTop
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    HighlightRoot = False
    object btnFindNext: TcxButton
      Left = 322
      Top = 190
      Width = 80
      Height = 25
      Caption = 'Find Next'
      Default = True
      TabOrder = 18
      OnClick = btnFindNextClick
    end
    object btnClose: TcxButton
      Left = 424
      Top = 190
      Width = 80
      Height = 25
      Cancel = True
      Caption = 'Close'
      TabOrder = 19
      OnClick = btnCloseClick
    end
    object btnFindAll: TcxButton
      Left = 241
      Top = 190
      Width = 75
      Height = 25
      Caption = 'Find All'
      TabOrder = 17
      OnClick = btnFindAllClick
    end
    object btnReplaceAll: TcxButton
      Left = 62
      Top = 190
      Width = 75
      Height = 25
      Caption = 'Replace All'
      TabOrder = 15
      OnClick = btnReplaceAllClick
    end
    object btnReplace: TcxButton
      Left = 143
      Top = 190
      Width = 76
      Height = 25
      Caption = 'Replace'
      TabOrder = 16
      OnClick = btnReplaceClick
    end
    object cmbWhatReplace: TcxComboBox
      Left = 10000
      Top = 10000
      Properties.OnChange = cmbFindWhatPropertiesChange
      Style.HotTrack = False
      TabOrder = 7
      Visible = False
      Width = 382
    end
    object cmbWithReplace: TcxComboBox
      Left = 10000
      Top = 10000
      Style.HotTrack = False
      TabOrder = 8
      Visible = False
      Width = 382
    end
    object chkMatchCaseReplace: TcxCheckBox
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Match case'
      Properties.OnChange = chkFindMatchCasePropertiesChange
      Style.HotTrack = False
      TabOrder = 12
      Transparent = True
      Visible = False
      Height = 21
      Width = 175
    end
    object btnOptionsReplace: TcxButton
      Left = 10000
      Top = 10000
      Width = 75
      Height = 25
      Caption = 'Options >>'
      TabOrder = 14
      Visible = False
      OnClick = btnOptionsFindClick
    end
    object cmbWithinRangeReplace: TcxComboBox
      Left = 10000
      Top = 10000
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cmbFindWithinRangePropertiesChange
      Style.HotTrack = False
      TabOrder = 9
      Visible = False
      Width = 120
    end
    object cmbSearchModeReplace: TcxComboBox
      Left = 10000
      Top = 10000
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cmbFindSearchModePropertiesChange
      Style.HotTrack = False
      TabOrder = 10
      Visible = False
      Width = 120
    end
    object cmbLookInReplace: TcxComboBox
      Left = 10000
      Top = 10000
      Properties.DropDownListStyle = lsFixedList
      Style.HotTrack = False
      TabOrder = 11
      Visible = False
      Width = 120
    end
    object chkEntireCellReplace: TcxCheckBox
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Match entire cell contents'
      Properties.OnChange = chkFindEntireCellPropertiesChange
      Style.HotTrack = False
      TabOrder = 13
      Transparent = True
      Visible = False
      Height = 21
      Width = 175
    end
    object cmbWhatFind: TcxComboBox
      Left = 85
      Top = 44
      Properties.OnChange = cmbFindWhatPropertiesChange
      Style.HotTrack = False
      TabOrder = 0
      Width = 408
    end
    object btnOptionsFind: TcxButton
      Left = 418
      Top = 98
      Width = 75
      Height = 25
      Caption = 'Options >>'
      TabOrder = 6
      OnClick = btnOptionsFindClick
    end
    object cmbWithinRangeFind: TcxComboBox
      Left = 85
      Top = 98
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cmbFindWithinRangePropertiesChange
      Style.HotTrack = False
      TabOrder = 1
      Width = 120
    end
    object chkMatchCaseFind: TcxCheckBox
      Left = 211
      Top = 98
      AutoSize = False
      Caption = 'Match case'
      Properties.OnChange = chkFindMatchCasePropertiesChange
      Style.HotTrack = False
      TabOrder = 4
      Transparent = True
      Height = 21
      Width = 169
    end
    object chkEntireCellFind: TcxCheckBox
      Left = 211
      Top = 125
      AutoSize = False
      Caption = 'Match entire cell contents'
      Properties.OnChange = chkFindEntireCellPropertiesChange
      Style.HotTrack = False
      TabOrder = 5
      Transparent = True
      Height = 21
      Width = 169
    end
    object cmbSearchModeFind: TcxComboBox
      Left = 85
      Top = 125
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cmbFindSearchModePropertiesChange
      Style.HotTrack = False
      TabOrder = 2
      Width = 120
    end
    object cmbLookInFind: TcxComboBox
      Left = 85
      Top = 152
      Properties.DropDownListStyle = lsFixedList
      Style.HotTrack = False
      TabOrder = 3
      Width = 120
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahParentManaged
      AlignVert = avTop
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object lcMainItem1: TdxLayoutItem
      Parent = lcMainGroup14
      AlignHorz = ahClient
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      CaptionOptions.Layout = clTop
      Control = btnFindNext
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 80
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lcMainItem2: TdxLayoutItem
      Parent = lcMainGroup14
      CaptionOptions.Visible = False
      Control = btnClose
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 80
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object lcMainGroup14: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object lcMainContentGroup: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldTabbed
      ShowBorder = False
      TabbedOptions.ShowFrame = True
      OnTabChanged = lcMainContentGroupTabChanged
      Index = 0
    end
    object lcgTabReplace: TdxLayoutGroup
      Parent = lcMainContentGroup
      CaptionOptions.Text = 'Replace'
      ButtonOptions.Buttons = <>
      Index = 1
    end
    object lcMainSpaceItem1: TdxLayoutEmptySpaceItem
      Parent = lcMainGroup14
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      Index = 4
    end
    object lcMainItem3: TdxLayoutItem
      Parent = lcMainGroup14
      AlignHorz = ahClient
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnFindAll
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lcMainSpaceItem2: TdxLayoutEmptySpaceItem
      Parent = lcMainGroup14
      AlignHorz = ahClient
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      Index = 1
    end
    object lcgReplaceButtons: TdxLayoutGroup
      Parent = lcMainGroup14
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object lcMainItem5: TdxLayoutItem
      Parent = lcgReplaceButtons
      AlignHorz = ahClient
      CaptionOptions.Text = 'cxButton3'
      CaptionOptions.Visible = False
      Control = btnReplaceAll
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainItem4: TdxLayoutItem
      Parent = lcgReplaceButtons
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnReplace
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 76
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciWhatReplace: TdxLayoutItem
      Parent = lcgTabReplace
      AlignHorz = ahClient
      CaptionOptions.AlignHorz = taRightJustify
      CaptionOptions.Text = 'Find what:'
      Control = cmbWhatReplace
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 370
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciWithReplace: TdxLayoutItem
      Parent = lcgTabReplace
      AlignHorz = ahClient
      CaptionOptions.Text = 'Replace with:'
      Control = cmbWithReplace
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 370
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcgGroupOptionsReplace: TdxLayoutGroup
      Parent = lcMainGroup3
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      Visible = False
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 0
    end
    object lciMatchCaseReplace: TdxLayoutItem
      Parent = lcMainGroup5
      AlignHorz = ahClient
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = chkMatchCaseReplace
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 163
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainGroup2: TdxLayoutAutoCreatedGroup
      Parent = lcgGroupOptionsReplace
      LayoutDirection = ldHorizontal
      Index = 0
      AutoCreated = True
    end
    object lcMainItem6: TdxLayoutItem
      Parent = lcMainGroup3
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnOptionsReplace
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainGroup3: TdxLayoutAutoCreatedGroup
      Parent = lcgTabReplace
      LayoutDirection = ldHorizontal
      Index = 2
      AutoCreated = True
    end
    object lciWithinRangeReplace: TdxLayoutItem
      Parent = lcMainGroup4
      AlignHorz = ahClient
      CaptionOptions.AlignHorz = taRightJustify
      CaptionOptions.Text = 'Within:'
      Control = cmbWithinRangeReplace
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 120
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciSearchModeReplace: TdxLayoutItem
      Parent = lcMainGroup4
      CaptionOptions.AlignHorz = taRightJustify
      CaptionOptions.Text = 'Search:'
      Control = cmbSearchModeReplace
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 120
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainGroup4: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup2
      AlignHorz = ahLeft
      Index = 0
      AutoCreated = True
    end
    object lciLookInReplace: TdxLayoutItem
      Parent = lcMainGroup4
      CaptionOptions.AlignHorz = taRightJustify
      CaptionOptions.Text = 'Look in:'
      Control = cmbLookInReplace
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 120
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lciEntireCellReplace: TdxLayoutItem
      Parent = lcMainGroup5
      AlignHorz = ahClient
      CaptionOptions.Text = 'cxCheckBox2'
      CaptionOptions.Visible = False
      Control = chkEntireCellReplace
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 163
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainGroup5: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup2
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object lcgTabFind: TdxLayoutGroup
      Parent = lcMainContentGroup
      CaptionOptions.Text = 'Find'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      Index = 0
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = lcgTabFind
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lciWhatFind: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.AlignHorz = taRightJustify
      CaptionOptions.Text = 'Find what:'
      Padding.AssignedValues = [lpavLeft, lpavRight]
      Control = cmbWhatFind
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 376
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcFindSpaceItem: TdxLayoutEmptySpaceItem
      Parent = dxLayoutGroup1
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.Height = 21
      SizeOptions.Width = 10
      Index = 1
    end
    object lcMainGroup8: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutGroup1
      LayoutDirection = ldHorizontal
      Index = 2
      AutoCreated = True
    end
    object lcgGroupOptionsFind: TdxLayoutGroup
      Parent = lcMainGroup8
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object lcMainItem7: TdxLayoutItem
      Parent = lcMainGroup8
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnOptionsFind
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lcFindHorzAlignSpaceItem: TdxLayoutEmptySpaceItem
      Parent = lcgTabFind
      AlignVert = avClient
      CaptionOptions.Text = 'Empty Space Item'
      Index = 0
    end
    object lciWithinRangeFind: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.AlignHorz = taRightJustify
      CaptionOptions.Text = 'Within:'
      Control = cmbWithinRangeFind
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 120
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainGroup11: TdxLayoutAutoCreatedGroup
      Parent = lcgGroupOptionsFind
      AlignHorz = ahLeft
      AlignVert = avTop
      Index = 1
      AutoCreated = True
    end
    object lciMatchCaseFind: TdxLayoutItem
      Parent = lcMainGroup11
      AlignHorz = ahClient
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = chkMatchCaseFind
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 169
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciEntireCellFind: TdxLayoutItem
      Parent = lcMainGroup11
      AlignHorz = ahClient
      CaptionOptions.Text = 'cxCheckBox2'
      CaptionOptions.Visible = False
      Control = chkEntireCellFind
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 169
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciSearchModeFind: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.AlignHorz = taRightJustify
      CaptionOptions.Text = 'Search:'
      Control = cmbSearchModeFind
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 120
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup
      Parent = lcgGroupOptionsFind
      AlignHorz = ahLeft
      AlignVert = avTop
      Index = 0
      AutoCreated = True
    end
    object lciLookInFind: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.AlignHorz = taRightJustify
      CaptionOptions.Text = 'Look in:'
      Control = cmbLookInFind
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 120
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lgOptionsHeightAdjusting: TdxLayoutGroup
      Parent = lcMainGroup8
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object liOptionsHeightAdjustingItem1: TdxLayoutEmptySpaceItem
      Parent = lgOptionsHeightAdjusting
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.Height = 10
      SizeOptions.Width = 1
      Index = 0
    end
    object liOptionsHeightAdjustingItem2: TdxLayoutEmptySpaceItem
      Parent = lgOptionsHeightAdjusting
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.Height = 10
      SizeOptions.Width = 1
      Index = 1
    end
    object liOptionsHeightAdjustingItem3: TdxLayoutEmptySpaceItem
      Parent = lgOptionsHeightAdjusting
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.Height = 10
      SizeOptions.Width = 1
      Index = 2
    end
  end
  object mcFindAllResults: TcxMCListBox
    Left = 0
    Top = 225
    Width = 514
    Height = 170
    Align = alClient
    ColumnLineColor = clWhite
    Delimiter = #9
    HeaderSections = <
      item
        AllowClick = True
        Text = 'Sheet'
        Width = 95
      end
      item
        AllowClick = True
        Text = 'Name'
        Width = 95
      end
      item
        AllowClick = True
        Text = 'Cell'
        Width = 96
      end
      item
        AllowClick = True
        Text = 'Value'
        Width = 96
      end
      item
        AllowClick = True
        Text = 'Formula'
        Width = 96
      end>
    OverflowEmptyColumn = False
    ShowColumnLines = False
    TabOrder = 1
    OnClick = mcFindAllResultsClick
  end
  object lbFoundCellsInfo: TcxLabel
    Left = 0
    Top = 395
    Align = alBottom
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 368
    Top = 16
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
