inherited dxRichEditSearchTextDialogForm: TdxRichEditSearchTextDialogForm
  Caption = 'Find and Replace'
  ClientHeight = 245
  ClientWidth = 523
  Position = poDefault
  PixelsPerInch = 96
  TextHeight = 13
  inherited dxLayoutControl1: TdxLayoutControl
    Width = 523
    Height = 245
    HighlightRoot = False
    object edFndSearchString: TcxMRUEdit [0]
      Left = 66
      Top = 44
      Properties.OnButtonClick = edSearchStringPropertiesButtonClick
      Properties.OnChange = edSearchStringPropertiesChange
      Properties.OnNewLookupDisplayText = SearchStringAddingMRUItem
      Style.HotTrack = False
      TabOrder = 0
      Width = 425
    end
    object cmbFndFindDirection: TcxComboBox [1]
      Left = 66
      Top = 71
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cmbFndFindDirectionPropertiesChange
      Style.HotTrack = False
      TabOrder = 1
      Width = 75
    end
    object cbFndMatchCase: TcxCheckBox [2]
      Left = 62
      Top = 98
      Action = aMatchCase
      Style.HotTrack = False
      TabOrder = 2
      Transparent = True
    end
    object cbFndFindWholeWord: TcxCheckBox [3]
      Left = 62
      Top = 125
      Action = aFindWholeWord
      Style.HotTrack = False
      TabOrder = 3
      Transparent = True
    end
    object chbFndRegex: TcxCheckBox [4]
      Left = 62
      Top = 152
      Action = aRegex
      Style.HotTrack = False
      TabOrder = 4
      Transparent = True
    end
    object edRplSearchString: TcxMRUEdit [5]
      Left = 10000
      Top = 10000
      Properties.OnButtonClick = edSearchStringPropertiesButtonClick
      Properties.OnChange = edSearchStringPropertiesChange
      Properties.OnNewLookupDisplayText = SearchStringAddingMRUItem
      Style.HotTrack = False
      TabOrder = 5
      Visible = False
      Width = 170
    end
    object edRplReplaceString: TcxMRUEdit [6]
      Left = 10000
      Top = 10000
      Properties.ShowEllipsis = False
      Properties.OnButtonClick = edRplReplaceStringPropertiesButtonClick
      Properties.OnChange = edRplReplaceStringPropertiesChange
      Properties.OnNewLookupDisplayText = ReplaceStringAddingMRUItem
      Style.HotTrack = False
      TabOrder = 6
      Visible = False
      Width = 170
    end
    object cmbRplFindDirection: TcxComboBox [7]
      Left = 10000
      Top = 10000
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cmbRplFindDirectionPropertiesChange
      Style.HotTrack = False
      TabOrder = 7
      Visible = False
      Width = 75
    end
    object cbRplMatchCase: TcxCheckBox [8]
      Left = 10000
      Top = 10000
      Action = aMatchCase
      Style.HotTrack = False
      TabOrder = 8
      Transparent = True
      Visible = False
    end
    object cbRplFindWholeWord: TcxCheckBox [9]
      Left = 10000
      Top = 10000
      Action = aFindWholeWord
      Style.HotTrack = False
      TabOrder = 9
      Transparent = True
      Visible = False
    end
    object cbRplRegex: TcxCheckBox [10]
      Left = 10000
      Top = 10000
      Action = aRegex
      Style.HotTrack = False
      TabOrder = 10
      Transparent = True
      Visible = False
    end
    object btnReplaceNext: TcxButton [11]
      Left = 79
      Top = 193
      Width = 102
      Height = 25
      Action = aReplaceNext
      TabOrder = 11
    end
    object btnReplaceAll: TcxButton [12]
      Left = 187
      Top = 193
      Width = 102
      Height = 25
      Action = aReplaceAll
      TabOrder = 12
    end
    object btnFindNext: TcxButton [13]
      Left = 295
      Top = 193
      Width = 102
      Height = 25
      Action = aFindNext
      Default = True
      TabOrder = 13
    end
    object btnCancel: TcxButton [14]
      Left = 403
      Top = 193
      Width = 102
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 14
      OnClick = btnCancelClick
    end
    inherited dxLayoutControl1Group_Root: TdxLayoutGroup
      Index = -1
    end
    object lcgTabControl: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldTabbed
      ShowBorder = False
      OnTabChanged = lcgTabControlTabChanged
      Index = 0
    end
    object lcgFind: TdxLayoutGroup
      Parent = lcgTabControl
      CaptionOptions.Text = 'Fin&d'
      ButtonOptions.Buttons = <>
      Index = 0
    end
    object lciFndSearchString: TdxLayoutItem
      Parent = lcgFind
      CaptionOptions.Text = 'Fi&nd:'
      Control = edFndSearchString
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcilFndDirection: TdxLayoutItem
      Parent = lcgFind
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Search&:'
      Control = cmbFndFindDirection
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Group2: TdxLayoutGroup
      Parent = lcgFind
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 38
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 2
    end
    object dxLayoutControl1Item5: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      CaptionOptions.Visible = False
      Control = cbFndMatchCase
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item6: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      CaptionOptions.Visible = False
      Control = cbFndFindWholeWord
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Item7: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      CaptionOptions.Visible = False
      Control = chbFndRegex
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lcgReplace: TdxLayoutGroup
      Tag = 1
      Parent = lcgTabControl
      CaptionOptions.Text = 'Re&place'
      ButtonOptions.Buttons = <>
      Index = 1
    end
    object dxLayoutControl1Group3: TdxLayoutGroup
      Parent = lcgReplace
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object lciRplSearchString: TdxLayoutItem
      Parent = dxLayoutControl1Group3
      CaptionOptions.Text = 'Fi&nd:'
      Control = edRplSearchString
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciRplReplaceString: TdxLayoutItem
      Parent = dxLayoutControl1Group3
      AlignHorz = ahRight
      CaptionOptions.Text = 'replace w&ith:'
      Control = edRplReplaceString
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciRplDirection: TdxLayoutItem
      Parent = lcgReplace
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Search&:'
      Control = cmbRplFindDirection
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Group4: TdxLayoutGroup
      Parent = lcgReplace
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 38
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 2
    end
    object dxLayoutControl1Item8: TdxLayoutItem
      Parent = dxLayoutControl1Group4
      CaptionOptions.Visible = False
      Control = cbRplMatchCase
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item9: TdxLayoutItem
      Parent = dxLayoutControl1Group4
      CaptionOptions.Visible = False
      Control = cbRplFindWholeWord
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Item11: TdxLayoutItem
      Parent = dxLayoutControl1Group4
      CaptionOptions.Visible = False
      Control = cbRplRegex
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutControl1Group1: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahRight
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object lciReplaceNext: TdxLayoutItem
      Parent = dxLayoutControl1Group1
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = btnReplaceNext
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciReplaceAll: TdxLayoutItem
      Parent = dxLayoutControl1Group1
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = btnReplaceAll
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Item3: TdxLayoutItem
      Parent = dxLayoutControl1Group1
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = btnFindNext
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutControl1Item4: TdxLayoutItem
      Parent = dxLayoutControl1Group1
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.ShowBorder = False
      Index = 3
    end
  end
  object alActions: TActionList
    Left = 464
    Top = 65528
    object aReplaceNext: TAction
      Caption = '&Replace'
      OnExecute = aReplaceNextExecute
    end
    object aReplaceAll: TAction
      Caption = 'Replace &All'
      OnExecute = aReplaceAllExecute
    end
    object aFindNext: TAction
      Caption = '&Find Next'
      OnExecute = aFindNextExecute
    end
    object aMatchCase: TAction
      AutoCheck = True
      Caption = 'Matc&h case'
      OnExecute = aMatchCaseExecute
    end
    object aFindWholeWord: TAction
      AutoCheck = True
      Caption = 'Find whole words onl&y'
      OnExecute = aFindWholeWordExecute
    end
    object aRegex: TAction
      AutoCheck = True
      Caption = 'Re&gular expression'
      OnExecute = aRegexExecute
    end
  end
  object pmSearchRegex: TPopupMenu
    Left = 344
    Top = 65528
  end
  object pmReplaceRegex: TPopupMenu
    Left = 408
    Top = 65528
  end
end
