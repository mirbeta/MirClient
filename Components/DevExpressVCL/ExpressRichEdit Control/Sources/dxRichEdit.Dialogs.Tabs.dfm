inherited dxRichEditTabsDialogForm: TdxRichEditTabsDialogForm
  Caption = 'Tabs'
  ClientHeight = 356
  ClientWidth = 382
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited dxLayoutControl1: TdxLayoutControl
    Width = 382
    Height = 356
    object btnOk: TcxButton [0]
      Left = 216
      Top = 315
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 19
    end
    object btnCancel: TcxButton [1]
      Left = 297
      Top = 315
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 20
    end
    object teTabStopPosition: TcxTextEdit [2]
      Left = 10
      Top = 28
      Properties.ValidationOptions = [evoRaiseException, evoShowErrorIcon]
      Properties.OnChange = teTabStopPositionPropertiesChange
      Properties.OnValidate = teTabStopPositionPropertiesValidate
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 0
      Width = 183
    end
    object lbTabStopPosition: TcxListBox [3]
      Left = 10
      Top = 47
      Width = 183
      Height = 74
      ItemHeight = 13
      Style.TransparentBorder = False
      TabOrder = 1
      OnClick = TabStopPositions
    end
    object seDefaultTabStops: TdxMeasurementUnitEdit [4]
      Left = 199
      Top = 28
      Properties.OnChange = seDefaultTabStopsPropertiesChange
      TabOrder = 2
      Width = 173
    end
    object lblTabStopsToBeCleared: TcxLabel [5]
      Left = 199
      Top = 55
      Caption = 'Tab stops to be cleared:'
      Style.HotTrack = False
      Transparent = True
    end
    object lblRemoveTabStops: TcxLabel [6]
      Left = 215
      Top = 78
      AutoSize = False
      Style.HotTrack = False
      Properties.WordWrap = True
      Transparent = True
      Height = 45
      Width = 157
    end
    object btnSet: TcxButton [7]
      Left = 135
      Top = 284
      Width = 75
      Height = 25
      Caption = '&Set'
      TabOrder = 16
      OnClick = btnSetClick
    end
    object btnClear: TcxButton [8]
      Left = 216
      Top = 284
      Width = 75
      Height = 25
      Caption = 'Cl&ear'
      TabOrder = 17
      OnClick = btnClearClick
    end
    object btnClearAll: TcxButton [9]
      Left = 297
      Top = 284
      Width = 75
      Height = 25
      Caption = 'Clear &All'
      TabOrder = 18
      OnClick = btnClearAllClick
    end
    object rbLeft: TcxRadioButton [10]
      Left = 18
      Top = 149
      Width = 113
      Height = 17
      Caption = '&Left'
      Checked = True
      TabOrder = 5
      TabStop = True
      OnClick = AlignmentOrLeaderSelected
      GroupIndex = 1
      Transparent = True
    end
    object rbDecimal: TcxRadioButton [11]
      Left = 18
      Top = 172
      Width = 113
      Height = 17
      Caption = '&Decimal'
      TabOrder = 8
      OnClick = AlignmentOrLeaderSelected
      GroupIndex = 1
      Transparent = True
    end
    object rbCenter: TcxRadioButton [12]
      Left = 137
      Top = 149
      Width = 113
      Height = 17
      Caption = '&Center'
      TabOrder = 6
      OnClick = AlignmentOrLeaderSelected
      GroupIndex = 1
      Transparent = True
    end
    object rbHyphens: TcxRadioButton [13]
      Left = 18
      Top = 238
      Width = 113
      Height = 17
      Caption = '&Hyphens'
      TabOrder = 12
      OnClick = AlignmentOrLeaderSelected
      GroupIndex = 2
      Transparent = True
    end
    object rbRight: TcxRadioButton [14]
      Left = 256
      Top = 149
      Width = 113
      Height = 17
      Caption = '&Right'
      TabOrder = 7
      OnClick = AlignmentOrLeaderSelected
      GroupIndex = 1
      Transparent = True
    end
    object rbNone: TcxRadioButton [15]
      Left = 18
      Top = 215
      Width = 113
      Height = 17
      Caption = '(&None)'
      Checked = True
      TabOrder = 9
      TabStop = True
      OnClick = AlignmentOrLeaderSelected
      GroupIndex = 2
      Transparent = True
    end
    object rbDots: TcxRadioButton [16]
      Left = 137
      Top = 215
      Width = 113
      Height = 17
      Caption = 'D&ots'
      TabOrder = 10
      OnClick = AlignmentOrLeaderSelected
      GroupIndex = 2
      Transparent = True
    end
    object rbMiddleDots: TcxRadioButton [17]
      Left = 256
      Top = 215
      Width = 113
      Height = 17
      Caption = '&MiddleDots'
      TabOrder = 11
      OnClick = AlignmentOrLeaderSelected
      GroupIndex = 2
      Transparent = True
    end
    object rbUnderline: TcxRadioButton [18]
      Left = 137
      Top = 238
      Width = 113
      Height = 17
      Caption = '&Underline'
      TabOrder = 13
      OnClick = AlignmentOrLeaderSelected
      GroupIndex = 2
      Transparent = True
    end
    object rbThickLine: TcxRadioButton [19]
      Left = 256
      Top = 238
      Width = 113
      Height = 17
      Caption = 'Th&ickLine'
      TabOrder = 14
      OnClick = AlignmentOrLeaderSelected
      GroupIndex = 2
      Transparent = True
    end
    object rbEqualSign: TcxRadioButton [20]
      Left = 18
      Top = 261
      Width = 354
      Height = 17
      Caption = 'E&qualSign'
      TabOrder = 15
      OnClick = AlignmentOrLeaderSelected
      GroupIndex = 2
      Transparent = True
    end
    inherited dxLayoutControl1Group_Root: TdxLayoutGroup
      CaptionOptions.Visible = False
    end
    object lcMainGroup_Root: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item1: TdxLayoutItem
      Parent = dxLayoutControl1Group1
      AlignHorz = ahClient
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item2: TdxLayoutItem
      Parent = dxLayoutControl1Group1
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Group1: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahRight
      LayoutDirection = ldHorizontal
      Index = 2
      AutoCreated = True
    end
    object dxLayoutControl1Group2: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Group3: TdxLayoutGroup
      Parent = dxLayoutControl1Group2
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      LayoutLookAndFeel = dxLayoutCxLookAndFeel2
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Group4: TdxLayoutGroup
      Parent = dxLayoutControl1Group2
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lciTabStopPosition: TdxLayoutItem
      Parent = dxLayoutControl1Group3
      CaptionOptions.Text = '&Tab stop position:'
      CaptionOptions.Layout = clTop
      Control = teTabStopPosition
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 183
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item3: TdxLayoutItem
      Parent = dxLayoutControl1Group3
      CaptionOptions.Text = 'cxListBox1'
      CaptionOptions.Visible = False
      CaptionOptions.Layout = clTop
      Control = lbTabStopPosition
      ControlOptions.OriginalHeight = 74
      ControlOptions.OriginalWidth = 183
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciDefaultTabStops: TdxLayoutItem
      Parent = dxLayoutControl1Group4
      CaptionOptions.Text = 'De&fault tab stops:'
      CaptionOptions.Layout = clTop
      Control = seDefaultTabStops
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 173
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item6: TdxLayoutItem
      Parent = dxLayoutControl1Group4
      CaptionOptions.Text = 'cxLabel1'
      CaptionOptions.Visible = False
      CaptionOptions.Layout = clTop
      Control = lblTabStopsToBeCleared
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Item7: TdxLayoutItem
      Parent = dxLayoutControl1Group4
      AlignVert = avClient
      CaptionOptions.Text = 'cxLabel2'
      CaptionOptions.Visible = False
      CaptionOptions.Layout = clTop
      Offsets.Left = 16
      Control = lblRemoveTabStops
      ControlOptions.OriginalHeight = 45
      ControlOptions.OriginalWidth = 157
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutControl1Item13: TdxLayoutItem
      Parent = dxLayoutControl1Group5
      AlignHorz = ahClient
      CaptionOptions.Text = 'cxButton4'
      CaptionOptions.Visible = False
      Control = btnSet
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item12: TdxLayoutItem
      Parent = dxLayoutControl1Group5
      CaptionOptions.Text = 'cxButton3'
      CaptionOptions.Visible = False
      Control = btnClear
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Group5: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahRight
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutControl1Item14: TdxLayoutItem
      Parent = dxLayoutControl1Group5
      CaptionOptions.Text = 'cxButton5'
      CaptionOptions.Visible = False
      Control = btnClearAll
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutControl1Group6: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 8
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 2
    end
    object dxLayoutControl1Item4: TdxLayoutItem
      Parent = dxLayoutControl1Group8
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxRadioButton1'
      CaptionOptions.Visible = False
      Control = rbLeft
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 113
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item16: TdxLayoutItem
      Parent = dxLayoutControl1Group6
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxRadioButton4'
      CaptionOptions.Visible = False
      Control = rbDecimal
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 113
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Item5: TdxLayoutItem
      Parent = dxLayoutControl1Group8
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = rbCenter
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 113
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Group8: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group6
      AlignHorz = ahLeft
      LayoutDirection = ldHorizontal
      Index = 0
      AutoCreated = True
    end
    object dxLayoutControl1Group9: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 8
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 4
    end
    object dxLayoutControl1Item19: TdxLayoutItem
      Parent = dxLayoutControl1Group10
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxRadioButton4'
      CaptionOptions.Visible = False
      Control = rbHyphens
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 113
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item15: TdxLayoutItem
      Parent = dxLayoutControl1Group8
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxRadioButton3'
      CaptionOptions.Visible = False
      Control = rbRight
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 113
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutControl1Item9: TdxLayoutItem
      Parent = dxLayoutControl1Group7
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxRadioButton1'
      CaptionOptions.Visible = False
      Control = rbNone
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 113
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item17: TdxLayoutItem
      Parent = dxLayoutControl1Group7
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxRadioButton2'
      CaptionOptions.Visible = False
      Control = rbDots
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 113
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Group7: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group9
      LayoutDirection = ldHorizontal
      Index = 0
      AutoCreated = True
    end
    object dxLayoutControl1Item18: TdxLayoutItem
      Parent = dxLayoutControl1Group7
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxRadioButton3'
      CaptionOptions.Visible = False
      Control = rbMiddleDots
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 113
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutControl1Item20: TdxLayoutItem
      Parent = dxLayoutControl1Group10
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxRadioButton5'
      CaptionOptions.Visible = False
      Control = rbUnderline
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 113
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Group10: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group9
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutControl1Item21: TdxLayoutItem
      Parent = dxLayoutControl1Group10
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxRadioButton6'
      CaptionOptions.Visible = False
      Control = rbThickLine
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 113
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutControl1Item22: TdxLayoutItem
      Parent = dxLayoutControl1Group9
      CaptionOptions.Text = 'cxRadioButton7'
      CaptionOptions.Visible = False
      Control = rbEqualSign
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 354
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lblAlignment: TdxLayoutSeparatorItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Alignment'
      CaptionOptions.Visible = True
      Index = 1
    end
    object lblLeader: TdxLayoutSeparatorItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Leader'
      CaptionOptions.Visible = True
      Index = 3
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 8
    Top = 56
    inherited dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
    object dxLayoutCxLookAndFeel2: TdxLayoutCxLookAndFeel
      Offsets.ItemOffset = 0
      PixelsPerInch = 96
    end
  end
end
