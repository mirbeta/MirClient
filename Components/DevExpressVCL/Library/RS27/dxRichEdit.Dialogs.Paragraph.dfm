inherited dxRichEditParagraphDialogForm: TdxRichEditParagraphDialogForm
  ClientHeight = 345
  ClientWidth = 497
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited dxLayoutControl1: TdxLayoutControl
    Width = 497
    Height = 345
    object edtAlignment: TcxComboBox [0]
      Left = 104
      Top = 64
      Properties.DropDownListStyle = lsFixedList
      Style.HotTrack = False
      TabOrder = 0
      Width = 121
    end
    object ccbOutlineLevel: TcxComboBox [1]
      Left = 104
      Top = 91
      Properties.DropDownListStyle = lsFixedList
      Properties.DropDownRows = 10
      Style.HotTrack = False
      TabOrder = 1
      Width = 121
    end
    object seLeftIndent: TdxMeasurementUnitEdit [2]
      Left = 104
      Top = 138
      TabOrder = 2
      Width = 121
    end
    object seRightIndent: TdxMeasurementUnitEdit [3]
      Left = 104
      Top = 165
      TabOrder = 3
      Width = 121
    end
    object ccbFirstLineIndentType: TcxComboBox [4]
      Left = 231
      Top = 165
      Properties.DropDownListStyle = lsFixedList
      Style.HotTrack = False
      TabOrder = 4
      Width = 121
    end
    object seFirstLineIndent: TdxMeasurementUnitEdit [5]
      Left = 358
      Top = 165
      TabOrder = 5
      Width = 94
    end
    object seSpacingBefore: TdxMeasurementUnitEdit [6]
      Left = 104
      Top = 212
      TabOrder = 6
      Width = 121
    end
    object seSpacingAfter: TdxMeasurementUnitEdit [7]
      Left = 104
      Top = 239
      TabOrder = 7
      Width = 121
    end
    object ccbLineSpacing: TcxComboBox [8]
      Left = 231
      Top = 239
      Properties.DropDownListStyle = lsFixedList
      Style.HotTrack = False
      TabOrder = 8
      Width = 121
    end
    object seAtSpacing: TdxMeasurementUnitEdit [9]
      Left = 358
      Top = 239
      TabOrder = 9
      Width = 94
    end
    object cbContextualSpacing: TcxCheckBox [10]
      Left = 36
      Top = 266
      Caption = 'Don'#39't add spa&ce between paragraphs of the same style'
      Style.HotTrack = False
      TabOrder = 10
      Transparent = True
    end
    object cbKeepLinesTogether: TcxCheckBox [11]
      Left = 10000
      Top = 10000
      Caption = '&Keep lines together'
      Style.HotTrack = False
      TabOrder = 11
      Transparent = True
      Visible = False
    end
    object cbPageBreakBefore: TcxCheckBox [12]
      Left = 10000
      Top = 10000
      Caption = 'Page &break before'
      Style.HotTrack = False
      TabOrder = 12
      Transparent = True
      Visible = False
    end
    object btnTabs: TcxButton [13]
      Left = 10
      Top = 303
      Width = 95
      Height = 25
      Caption = '&Tabs...'
      TabOrder = 13
      OnClick = btnTabsClick
    end
    object btnOK: TcxButton [14]
      Left = 306
      Top = 303
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      TabOrder = 14
      OnClick = btnOKClick
    end
    object btnCancel: TcxButton [15]
      Left = 387
      Top = 303
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 15
    end
    object dxLayoutControl1Group1: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldTabbed
      ShowBorder = False
      Index = 0
    end
    object lcgIndentsAndSpacing: TdxLayoutGroup
      Parent = dxLayoutControl1Group1
      CaptionOptions.Text = '&Indents and Spacing'
      ButtonOptions.Buttons = <>
      ItemIndex = 5
      Index = 0
    end
    object dxLayoutControl1Group3: TdxLayoutGroup
      Parent = lcgIndentsAndSpacing
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lciAlignment: TdxLayoutItem
      Parent = dxLayoutControl1Group3
      CaptionOptions.Text = 'Ali&gnment:'
      Control = edtAlignment
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciOutlineLevel: TdxLayoutItem
      Parent = dxLayoutControl1Group3
      CaptionOptions.Text = '&Outline level:'
      Control = ccbOutlineLevel
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Group4: TdxLayoutGroup
      Parent = lcgIndentsAndSpacing
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 3
    end
    object dxLayoutControl1Group5: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group4
      AlignHorz = ahLeft
      Index = 0
    end
    object lciLeft: TdxLayoutItem
      Parent = dxLayoutControl1Group5
      AlignHorz = ahLeft
      CaptionOptions.Text = '&Left:'
      Control = seLeftIndent
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciRight: TdxLayoutItem
      Parent = dxLayoutControl1Group5
      CaptionOptions.Text = '&Right:'
      Control = seRightIndent
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciSpecial: TdxLayoutItem
      Parent = dxLayoutControl1Group4
      AlignVert = avBottom
      CaptionOptions.Text = '&Special:'
      CaptionOptions.Layout = clTop
      Control = ccbFirstLineIndentType
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciBy: TdxLayoutItem
      Parent = dxLayoutControl1Group4
      AlignVert = avBottom
      CaptionOptions.Text = 'B&y:'
      CaptionOptions.Layout = clTop
      Control = seFirstLineIndent
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 94
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutControl1Group6: TdxLayoutGroup
      Parent = lcgIndentsAndSpacing
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 5
    end
    object dxLayoutControl1Group7: TdxLayoutGroup
      Parent = dxLayoutControl1Group6
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Group8: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group7
      AlignHorz = ahLeft
      Index = 0
    end
    object lciBefore: TdxLayoutItem
      Parent = dxLayoutControl1Group8
      AlignHorz = ahLeft
      CaptionOptions.Text = '&Before:'
      Control = seSpacingBefore
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciAfter: TdxLayoutItem
      Parent = dxLayoutControl1Group8
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Aft&er:'
      Control = seSpacingAfter
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciLineSpacing: TdxLayoutItem
      Parent = dxLayoutControl1Group7
      AlignHorz = ahLeft
      AlignVert = avBottom
      CaptionOptions.Text = 'Li&ne Spacing:'
      CaptionOptions.Layout = clTop
      Control = ccbLineSpacing
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciAtSpacing: TdxLayoutItem
      Parent = dxLayoutControl1Group7
      AlignVert = avBottom
      CaptionOptions.Text = 'At:'
      CaptionOptions.Layout = clTop
      Control = seAtSpacing
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 94
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutControl1Item14: TdxLayoutItem
      Parent = dxLayoutControl1Group6
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = cbContextualSpacing
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcgLineAndPageBreaks: TdxLayoutGroup
      Parent = dxLayoutControl1Group1
      CaptionOptions.Text = 'Line and &Page Breaks'
      ButtonOptions.Buttons = <>
      Index = 1
    end
    object dxLayoutControl1Group9: TdxLayoutGroup
      Parent = lcgLineAndPageBreaks
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Item11: TdxLayoutItem
      Parent = dxLayoutControl1Group9
      AlignHorz = ahClient
      CaptionOptions.Text = 'cxCheckBox2'
      CaptionOptions.Visible = False
      Control = cbKeepLinesTogether
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item17: TdxLayoutItem
      Parent = dxLayoutControl1Group9
      CaptionOptions.Text = 'cxCheckBox3'
      CaptionOptions.Visible = False
      Control = cbPageBreakBefore
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Group10: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object lciTabs: TdxLayoutItem
      Parent = dxLayoutControl1Group10
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnTabs
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 95
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Group2: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group10
      AlignHorz = ahRight
      LayoutDirection = ldHorizontal
      Index = 1
    end
    object dxLayoutControl1Item2: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnOK
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item1: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lbGeneral: TdxLayoutSeparatorItem
      Parent = lcgIndentsAndSpacing
      CaptionOptions.Text = 'General'
      CaptionOptions.Visible = True
      Index = 0
    end
    object lbIndentation: TdxLayoutSeparatorItem
      Parent = lcgIndentsAndSpacing
      CaptionOptions.Text = 'Indentation'
      CaptionOptions.Visible = True
      Index = 2
    end
    object lbSpacing: TdxLayoutSeparatorItem
      Parent = lcgIndentsAndSpacing
      CaptionOptions.Text = 'Spacing'
      CaptionOptions.Visible = True
      Index = 4
    end
    object lbPagination: TdxLayoutSeparatorItem
      Parent = lcgLineAndPageBreaks
      CaptionOptions.Text = 'Pagination'
      CaptionOptions.Visible = True
      Index = 0
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    inherited dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
