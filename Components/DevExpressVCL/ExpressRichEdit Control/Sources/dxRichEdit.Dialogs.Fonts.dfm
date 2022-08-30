inherited dxRichEditFontDialogForm: TdxRichEditFontDialogForm
  Caption = 'Font'
  ClientHeight = 433
  ClientWidth = 417
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  inherited dxLayoutControl1: TdxLayoutControl
    Width = 417
    Height = 433
    object cmbFontColor: TdxColorEdit [0]
      Left = 10
      Top = 135
      Style.HotTrack = False
      Style.TransparentBorder = True
      TabOrder = 6
      Width = 136
    end
    object cmbUnderlineStyle: TcxComboBox [1]
      Left = 152
      Top = 135
      Properties.DropDownListStyle = lsFixedList
      Properties.ItemHeight = 15
      Style.HotTrack = False
      Style.TransparentBorder = True
      TabOrder = 7
      Width = 106
    end
    object cmbUnderlineColor: TdxColorEdit [2]
      Left = 264
      Top = 135
      Enabled = False
      Style.HotTrack = False
      Style.TransparentBorder = True
      TabOrder = 8
      Width = 136
    end
    object cbStrikethrough: TcxCheckBox [3]
      Left = 20
      Top = 182
      Caption = 'Stri&kethrough'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 9
      Transparent = True
    end
    object cbDoubleStrikethrough: TcxCheckBox [4]
      Left = 20
      Top = 205
      Caption = 'Double strikethrou&gh'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 10
      Transparent = True
    end
    object cbUnderlineWordsOnly: TcxCheckBox [5]
      Left = 20
      Top = 228
      Caption = '&Underline words only'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 11
      Transparent = True
    end
    object cbSuperscript: TcxCheckBox [6]
      Left = 152
      Top = 182
      Caption = 'Su&perscript'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 12
      Transparent = True
    end
    object cbSubscript: TcxCheckBox [7]
      Left = 152
      Top = 205
      Caption = 'Su&bscript'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 13
      Transparent = True
    end
    object cbAllCaps: TcxCheckBox [8]
      Left = 279
      Top = 182
      Caption = '&All caps'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 14
      Transparent = True
    end
    object cbHidden: TcxCheckBox [9]
      Left = 279
      Top = 205
      Caption = '&Hidden'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 15
      Transparent = True
    end
    object btnCancel: TcxButton [10]
      Left = 325
      Top = 381
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 18
    end
    object btnOk: TcxButton [11]
      Left = 244
      Top = 381
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 17
    end
    object cmbFontName: TcxTextEdit [12]
      Left = 10
      Top = -5
      AutoSize = False
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 0
      Height = 21
      Width = 191
    end
    object cmbFontSize: TcxTextEdit [13]
      Left = 334
      Top = -5
      AutoSize = False
      Properties.ValidationOptions = [evoRaiseException, evoShowErrorIcon, evoAllowLoseFocus]
      Style.TransparentBorder = False
      TabOrder = 4
      Height = 21
      Width = 66
    end
    object cmbFontStyle: TcxTextEdit [14]
      Left = 207
      Top = -5
      AutoSize = False
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Height = 21
      Width = 121
    end
    object lbFontName: TcxListBox [15]
      Left = 10
      Top = 16
      Width = 191
      Height = 95
      TabStop = False
      ItemHeight = 13
      Style.TransparentBorder = False
      TabOrder = 1
    end
    object lbFontStyle: TcxListBox [16]
      Left = 207
      Top = 16
      Width = 121
      Height = 95
      TabStop = False
      ItemHeight = 13
      Style.TransparentBorder = False
      TabOrder = 3
    end
    object lbFontSize: TcxListBox [17]
      Left = 334
      Top = 16
      Width = 66
      Height = 95
      TabStop = False
      ItemHeight = 13
      Items.Strings = (
        '8'
        '9'
        '10'
        '11'
        '12'
        '14'
        '16'
        '18'
        '20'
        '22'
        '24'
        '26'
        '28'
        '36'
        '48'
        '72')
      Style.TransparentBorder = False
      TabOrder = 5
    end
    object srePreview: TdxSimpleRichEditControl [18]
      Left = 11
      Top = 272
      Width = 388
      Height = 64
      BorderStyle = cxcbsNone
      TabOrder = 16
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
    object dxLayoutControl1Group1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Group3: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object lciFontColor: TdxLayoutItem
      Parent = dxLayoutControl1Group3
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Font Color:'
      CaptionOptions.Layout = clTop
      Control = cmbFontColor
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 136
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciUnderlineStyle: TdxLayoutItem
      Parent = dxLayoutControl1Group3
      AlignVert = avTop
      CaptionOptions.Text = 'Underline style:'
      CaptionOptions.Layout = clTop
      Control = cmbUnderlineStyle
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 106
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciUnderlineColor: TdxLayoutItem
      Parent = dxLayoutControl1Group3
      AlignVert = avTop
      CaptionOptions.Text = 'Underline color:'
      CaptionOptions.Layout = clTop
      Control = cmbUnderlineColor
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 136
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 2
    end
    object dxLayoutControl1Group4: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 3
    end
    object dxLayoutControl1Group5: TdxLayoutGroup
      Parent = dxLayoutControl1Group4
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 10
      SizeOptions.Width = 84
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item4: TdxLayoutItem
      Parent = dxLayoutControl1Group5
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = cbStrikethrough
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item5: TdxLayoutItem
      Parent = dxLayoutControl1Group5
      CaptionOptions.Text = 'cxCheckBox2'
      CaptionOptions.Visible = False
      Control = cbDoubleStrikethrough
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Item6: TdxLayoutItem
      Parent = dxLayoutControl1Group5
      CaptionOptions.Text = 'cxCheckBox3'
      CaptionOptions.Visible = False
      Control = cbUnderlineWordsOnly
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutControl1Group6: TdxLayoutGroup
      Parent = dxLayoutControl1Group4
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      SizeOptions.Width = 84
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Group7: TdxLayoutGroup
      Parent = dxLayoutControl1Group4
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      SizeOptions.Width = 84
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 2
    end
    object dxLayoutControl1Item7: TdxLayoutItem
      Parent = dxLayoutControl1Group6
      CaptionOptions.Text = 'cxCheckBox4'
      CaptionOptions.Visible = False
      Control = cbSuperscript
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item8: TdxLayoutItem
      Parent = dxLayoutControl1Group6
      CaptionOptions.Text = 'cxCheckBox5'
      CaptionOptions.Visible = False
      Control = cbSubscript
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Item9: TdxLayoutItem
      Parent = dxLayoutControl1Group7
      CaptionOptions.Text = 'cxCheckBox6'
      CaptionOptions.Visible = False
      Control = cbAllCaps
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item10: TdxLayoutItem
      Parent = dxLayoutControl1Group7
      CaptionOptions.Text = 'cxCheckBox7'
      CaptionOptions.Visible = False
      Control = cbHidden
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Item3: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      CaptionOptions.Text = 'Button2'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Group2: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahRight
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutControl1Item2: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      CaptionOptions.Text = 'Button1'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lblEffects: TdxLayoutSeparatorItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Effects'
      CaptionOptions.Visible = True
      Index = 2
    end
    object dxLayoutControl1Group8: TdxLayoutGroup
      Parent = dxLayoutControl1Group1
      AlignVert = avBottom
      CaptionOptions.Text = 'New Group'
      LayoutLookAndFeel = dxLayoutCxLookAndFeel2
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Group9: TdxLayoutGroup
      Parent = dxLayoutControl1Group1
      AlignVert = avBottom
      CaptionOptions.Text = 'New Group'
      LayoutLookAndFeel = dxLayoutCxLookAndFeel2
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Group10: TdxLayoutGroup
      Parent = dxLayoutControl1Group1
      AlignVert = avBottom
      CaptionOptions.Text = 'New Group'
      LayoutLookAndFeel = dxLayoutCxLookAndFeel2
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 2
    end
    object lciFontName: TdxLayoutItem
      Parent = dxLayoutControl1Group8
      CaptionOptions.Text = 'Font:'
      CaptionOptions.Layout = clTop
      Control = cmbFontName
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 191
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciFontSize: TdxLayoutItem
      Parent = dxLayoutControl1Group10
      AlignVert = avTop
      CaptionOptions.Text = 'Size:'
      CaptionOptions.Layout = clTop
      Control = cmbFontSize
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 66
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciFontStyle: TdxLayoutItem
      Parent = dxLayoutControl1Group9
      AlignVert = avTop
      CaptionOptions.Text = 'Font style:'
      CaptionOptions.Layout = clTop
      Control = cmbFontStyle
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item1: TdxLayoutItem
      Parent = dxLayoutControl1Group8
      CaptionOptions.Text = 'cxListBox1'
      CaptionOptions.Visible = False
      CaptionOptions.Layout = clTop
      Control = lbFontName
      ControlOptions.OriginalHeight = 95
      ControlOptions.OriginalWidth = 191
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Item11: TdxLayoutItem
      Parent = dxLayoutControl1Group9
      CaptionOptions.Text = 'cxListBox2'
      CaptionOptions.Visible = False
      CaptionOptions.Layout = clTop
      Control = lbFontStyle
      ControlOptions.OriginalHeight = 95
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Item12: TdxLayoutItem
      Parent = dxLayoutControl1Group10
      CaptionOptions.Text = 'cxListBox3'
      CaptionOptions.Visible = False
      CaptionOptions.Layout = clTop
      Control = lbFontSize
      ControlOptions.OriginalHeight = 95
      ControlOptions.OriginalWidth = 66
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lblPreview: TdxLayoutSeparatorItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Preview'
      CaptionOptions.Visible = True
      Index = 4
    end
    object dxLayoutControl1Item13: TdxLayoutItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'dxSimpleRichEditControl1'
      CaptionOptions.Visible = False
      Control = srePreview
      ControlOptions.OriginalHeight = 64
      ControlOptions.OriginalWidth = 388
      Enabled = False
      Index = 5
    end
    object liFontNameWarning: TdxLayoutLabeledItem
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      CaptionOptions.AlignVert = tavTop
      CaptionOptions.VisibleElements = [cveText]
      CaptionOptions.WordWrap = True
      SizeOptions.Height = 32
      Index = 6
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    object dxLayoutCxLookAndFeel2: TdxLayoutCxLookAndFeel
      Offsets.ItemOffset = 0
    end
  end
end
