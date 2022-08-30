inherited dxRichEditEditStyleDialogForm: TdxRichEditEditStyleDialogForm
  Caption = 'Modify Style'
  ClientHeight = 452
  ClientWidth = 524
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  inherited dxLayoutControl1: TdxLayoutControl
    Width = 524
    Height = 452
    object edtName: TcxTextEdit [0]
      Left = 164
      Top = 77
      Style.HotTrack = False
      TabOrder = 1
      Width = 350
    end
    object cmbParent: TcxComboBox [1]
      Left = 164
      Top = 104
	  Properties.DropDownListStyle = lsFixedList
      Style.HotTrack = False
      TabOrder = 2
      Width = 350
    end
    object cmbNextStyle: TcxComboBox [2]
      Left = 164
      Top = 131
	  Properties.DropDownListStyle = lsFixedList
      Style.HotTrack = False
      TabOrder = 3
      Width = 350
    end
    object cmbCurrentStyle: TcxComboBox [3]
      Left = 164
      Top = 30
	  Properties.DropDownListStyle = lsFixedList
      Style.HotTrack = False
      TabOrder = 0
      Width = 350
    end
    object cmbFontEdit: TdxRichEditFontNameComboBox [4]
      Left = 10
      Top = 179
      Properties.FontPreview.Visible = False
      Properties.ItemHeight = 24
      Properties.OnChange = FontEditPropertiesChange
      Style.HotTrack = False
      TabOrder = 4
      Width = 121
    end
    object cmbFontSize: TcxComboBox [5]
      Left = 137
      Top = 179
      Style.HotTrack = False
      TabOrder = 5
      Width = 38
    end
    object cmbFontColor: TdxColorEdit [6]
      Left = 283
      Top = 179
      Properties.OnChange = FontColorPropertiesChange
      Style.HotTrack = False
      TabOrder = 9
      Width = 121
    end
    object btnToggleParagraphAlignmentLeft: TcxButton [7]
      Left = 10
      Top = 220
      Width = 25
      Height = 25
      Action = aToggleParagraphAlignmentLeft
      PaintStyle = bpsGlyph
      ParentShowHint = False
      ShowHint = True
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 10
    end
    object btnToggleParagraphAlignmentCenter: TcxButton [8]
      Left = 35
      Top = 220
      Width = 25
      Height = 25
      Action = aToggleParagraphAlignmentCenter
      PaintStyle = bpsGlyph
      ParentShowHint = False
      ShowHint = True
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 11
    end
    object btnToggleParagraphAlignmentRight: TcxButton [9]
      Left = 60
      Top = 220
      Width = 25
      Height = 25
      Action = aToggleParagraphAlignmentRight
      PaintStyle = bpsGlyph
      ParentShowHint = False
      ShowHint = True
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 12
    end
    object btnToggleParagraphAlignmentJustify: TcxButton [10]
      Left = 85
      Top = 220
      Width = 25
      Height = 25
      Action = aToggleParagraphAlignmentJustify
      PaintStyle = bpsGlyph
      ParentShowHint = False
      ShowHint = True
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 13
    end
    object btnSingleParagraphSpacing: TcxButton [11]
      Left = 116
      Top = 220
      Width = 25
      Height = 25
      Action = aSetSingleParagraphSpacing
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 14
    end
    object btnIncreaseParagraphSpacing: TcxButton [12]
      Left = 197
      Top = 220
      Width = 25
      Height = 25
      Action = aSpacingIncrease
      PaintStyle = bpsGlyph
      ParentShowHint = False
      ShowHint = True
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 17
    end
    object btnDecreaseParagraphSpacing: TcxButton [13]
      Left = 222
      Top = 220
      Width = 25
      Height = 25
      Action = aSpacingDecrease
      PaintStyle = bpsGlyph
      ParentShowHint = False
      ShowHint = True
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 18
    end
    object btnDecrementIndent: TcxButton [14]
      Left = 253
      Top = 220
      Width = 25
      Height = 25
      Action = aDecrementIndent
      PaintStyle = bpsGlyph
      ParentShowHint = False
      ShowHint = True
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 19
    end
    object btnIncrementIndent: TcxButton [15]
      Left = 278
      Top = 220
      Width = 25
      Height = 25
      Action = aIncrementIndent
      PaintStyle = bpsGlyph
      ParentShowHint = False
      ShowHint = True
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 20
    end
    object PreviewRichEditControl: TdxRichEditControl [16]
      Left = 10
      Top = 251
      Width = 504
      Height = 149
      ActiveViewType = Simple
      Enabled = False
      Options.HorizontalRuler.Visibility = Hidden
      Options.HorizontalScrollbar.Visibility = Hidden
      Options.VerticalRuler.Visibility = Hidden
      Options.VerticalScrollbar.Visibility = Hidden
      TabOrder = 21
    end
    object btnFormat: TcxButton [17]
      Left = 10
      Top = 406
      Width = 75
      Height = 25
      Caption = 'F&ormat'
      DropDownMenu = pmFormat
      Kind = cxbkOfficeDropDown
      TabOrder = 22
    end
    object btnOk: TcxButton [18]
      Left = 358
      Top = 406
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 23
    end
    object btnCancel: TcxButton [19]
      Left = 439
      Top = 406
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 24
    end
    object btnToggleFontBold: TcxButton [20]
      Left = 193
      Top = 178
      Width = 24
      Height = 24
      Action = aToggleFontBold
      LookAndFeel.Kind = lfStandard
      PaintStyle = bpsGlyph
      ParentShowHint = False
      ShowHint = True
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 6
      TabStop = False
    end
    object btnToggleFontItalic: TcxButton [21]
      Left = 217
      Top = 178
      Width = 24
      Height = 24
      Action = aToggleFontItalic
      LookAndFeel.Kind = lfStandard
      PaintStyle = bpsGlyph
      ParentShowHint = False
      ShowHint = True
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 7
      TabStop = False
    end
    object btnToggleFontUnderline: TcxButton [22]
      Left = 241
      Top = 178
      Width = 24
      Height = 24
      Action = aToggleFontUnderline
      LookAndFeel.Kind = lfStandard
      PaintStyle = bpsGlyph
      ParentShowHint = False
      ShowHint = True
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 8
      TabStop = False
    end
    object btnSesquialteralParagraphSpacing: TcxButton [23]
      Left = 141
      Top = 220
      Width = 25
      Height = 25
      Action = aSetSesquialteralParagraphSpacing
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 15
    end
    object btnDoubleParagraphSpacing: TcxButton [24]
      Left = 166
      Top = 220
      Width = 25
      Height = 25
      Action = aSetDoubleParagraphSpacing
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 16
    end
    inherited dxLayoutControl1Group_Root: TdxLayoutGroup
      Index = -1
    end
    object lcMainGroup_Root: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 0
    end
    object lblSelectedStyle: TdxLayoutSeparatorItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Select Style'
      CaptionOptions.Visible = True
      Index = 0
    end
    object dxLayoutControl1Group1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 3
    end
    object lblProperties: TdxLayoutSeparatorItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Properties'
      CaptionOptions.Visible = True
      Index = 2
    end
    object lcilName: TdxLayoutItem
      Parent = dxLayoutControl1Group1
      CaptionOptions.Text = '&Name:'
      Control = edtName
      ControlOptions.AlignHorz = ahRight
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcilStyleBasedOn: TdxLayoutItem
      Parent = dxLayoutControl1Group1
      CaptionOptions.Text = 'Style &based on:'
      Control = cmbParent
      ControlOptions.AlignHorz = ahRight
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciStyleForFollowingParagraph: TdxLayoutItem
      Parent = dxLayoutControl1Group1
      CaptionOptions.Text = '&Style for following paragraph:'
      Control = cmbNextStyle
      ControlOptions.AlignHorz = ahRight
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lblFormatting: TdxLayoutSeparatorItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Formatting'
      CaptionOptions.Visible = True
      Index = 4
    end
    object lciCurrentStyle: TdxLayoutItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Current style:'
      Control = cmbCurrentStyle
      ControlOptions.AlignHorz = ahRight
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Item14: TdxLayoutSeparatorItem
      Parent = lcMainGroup_Root
      LayoutLookAndFeel = dxLayoutBarLookAndFeel
      Index = 6
    end
    object lcgBarFontFormatting: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 5
    end
    object dxLayoutControl1Item8: TdxLayoutItem
      Parent = lcgBarFontFormatting
      AlignHorz = ahLeft
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = cmbFontEdit
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item9: TdxLayoutItem
      Parent = lcgBarFontFormatting
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = cmbFontSize
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Item10: TdxLayoutItem
      Parent = lcgBarFontFormatting
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = cmbFontColor
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutControl1Group2: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      LayoutLookAndFeel = dxLayoutBarLookAndFeel
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 7
    end
    object dxLayoutControl1Item15: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      CaptionOptions.Text = 'cxButton4'
      CaptionOptions.Visible = False
      Control = btnToggleParagraphAlignmentLeft
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item16: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      CaptionOptions.Text = 'cxButton5'
      CaptionOptions.Visible = False
      Control = btnToggleParagraphAlignmentCenter
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Item17: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      CaptionOptions.Text = 'cxButton6'
      CaptionOptions.Visible = False
      Control = btnToggleParagraphAlignmentRight
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutControl1Item18: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      CaptionOptions.Text = 'cxButton7'
      CaptionOptions.Visible = False
      Control = btnToggleParagraphAlignmentJustify
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutControl1Item19: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      CaptionOptions.Text = 'cxButton8'
      CaptionOptions.Visible = False
      Control = btnSingleParagraphSpacing
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutControl1Item20: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      CaptionOptions.Text = 'cxButton9'
      CaptionOptions.Visible = False
      Control = btnIncreaseParagraphSpacing
      ControlOptions.ShowBorder = False
      Index = 9
    end
    object dxLayoutControl1Item21: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      CaptionOptions.Text = 'cxButton10'
      CaptionOptions.Visible = False
      Control = btnDecreaseParagraphSpacing
      ControlOptions.ShowBorder = False
      Index = 10
    end
    object dxLayoutControl1Item22: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      CaptionOptions.Text = 'cxButton11'
      CaptionOptions.Visible = False
      Control = btnDecrementIndent
      ControlOptions.ShowBorder = False
      Index = 12
    end
    object dxLayoutControl1Item23: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      CaptionOptions.Text = 'cxButton12'
      CaptionOptions.Visible = False
      Control = btnIncrementIndent
      ControlOptions.ShowBorder = False
      Index = 13
    end
    object dxLayoutControl1Item24: TdxLayoutItem
      Parent = lcMainGroup_Root
      CaptionOptions.Visible = False
      Control = PreviewRichEditControl
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 8
    end
    object dxLayoutControl1Item25: TdxLayoutItem
      Parent = dxLayoutControl1Group4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxButton13'
      CaptionOptions.Visible = False
      Control = btnFormat
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item26: TdxLayoutItem
      Parent = dxLayoutControl1Group4
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton14'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Group4: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group_Root
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutControl1Item27: TdxLayoutItem
      Parent = dxLayoutControl1Group4
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton15'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutControl1Group3: TdxLayoutGroup
      Parent = lcgBarFontFormatting
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      LayoutLookAndFeel = dxLayoutBarLookAndFeel
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 3
    end
    object dxLayoutControl1Item12: TdxLayoutItem
      Parent = dxLayoutControl1Group3
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnToggleFontBold
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item13: TdxLayoutItem
      Parent = dxLayoutControl1Group3
      CaptionOptions.Text = 'cxButton3'
      CaptionOptions.Visible = False
      Control = btnToggleFontItalic
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Item11: TdxLayoutItem
      Parent = dxLayoutControl1Group3
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnToggleFontUnderline
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = dxLayoutControl1Group2
      CaptionOptions.Text = 'Separator'
      Index = 4
    end
    object dxLayoutSeparatorItem2: TdxLayoutSeparatorItem
      Parent = dxLayoutControl1Group2
      CaptionOptions.Text = 'Separator'
      Index = 8
    end
    object dxLayoutSeparatorItem3: TdxLayoutSeparatorItem
      Parent = dxLayoutControl1Group2
      CaptionOptions.Text = 'Separator'
      Index = 11
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnSesquialteralParagraphSpacing
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnDoubleParagraphSpacing
      ControlOptions.ShowBorder = False
      Index = 7
    end
    object dxLayoutSeparatorItem4: TdxLayoutSeparatorItem
      Parent = lcgBarFontFormatting
      CaptionOptions.Text = 'Separator'
      Index = 2
    end
    object dxLayoutSeparatorItem5: TdxLayoutSeparatorItem
      Parent = lcgBarFontFormatting
      CaptionOptions.Text = 'Separator'
      Index = 4
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 64
    Top = 344
    object dxLayoutBarLookAndFeel: TdxLayoutStandardLookAndFeel
      Offsets.ItemOffset = 0
    end
  end
  object pmFormat: TPopupMenu
    Left = 168
    Top = 344
    object miFontDialog: TMenuItem
      Action = aFontDialog
    end
    object miParagraphDialog: TMenuItem
      Action = aParagraphDialog
    end
    object miTabsDialog: TMenuItem
      Action = aTabsDialog
    end
  end
  object ilActions: TcxImageList
    FormatVersion = 1
    DesignInfo = 22544720
    ImageInfo = <
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000343434EE3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF2020
          20BB060606550000000000000000000000000000000000000000000000000000
          00000000000000000000343434EE3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF0202023300000000000000000000000000000000000000000000
          00000000000000000000343434EE3B3B3BFF0606065500000000010101223434
          34EE3B3B3BFF0D0D0D7700000000000000000000000000000000000000000000
          00000000000000000000343434EE3B3B3BFF0606065500000000010101223434
          34EE3B3B3BFF0D0D0D7700000000000000000000000000000000000000000000
          00000000000000000000343434EE3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF262626CC0000000000000000000000000000000000000000000000000000
          00000000000000000000343434EE3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF2020
          20BB000000110000000000000000000000000000000000000000000000000000
          00000000000000000000343434EE3B3B3BFF06060655000000000D0D0D773B3B
          3BFF262626CC0000000000000000000000000000000000000000000000000000
          00000000000000000000343434EE3B3B3BFF06060655000000000D0D0D773B3B
          3BFF3B3B3BFF0202023300000000000000000000000000000000000000000000
          00000000000000000000343434EE3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF2C2C2CDD0000000000000000000000000000000000000000000000000000
          00000000000000000000343434EE3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF2020
          20BB020202330000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          7E000000424D7E000000000000003E0000002800000010000000100000000100
          010000000000400000000000000000000000020000000000000000000000FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000262626CC3B3B3BFF111111880000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000111111883B3B3BFF262626CC0000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000090909663B3B3BFF3B3B3BFF0000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000020202333B3B3BFF3B3B3BFF0202
          0233000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000343434EE3B3B3BFF0D0D
          0D77000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000262626CC3B3B3BFF1111
          1188000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000111111883B3B3BFF2626
          26CC000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000090909663B3B3BFF3B3B
          3BFF000000110000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000020202333B3B3BFF3B3B
          3BFF020202330000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000343434EE3B3B
          3BFF0D0D0D770000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000262626CC3B3B
          3BFF151515990000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          7E000000424D7E000000000000003E0000002800000010000000100000000100
          010000000000400000000000000000000000020000000000000000000000FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000003B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF3B3B3BFF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000003030340151515992E2E2EE02E2E2EE01515
          1599030303400000000000000000000000000000000000000000000000000000
          0000000000000000000003030340262626CC3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF262626CC0303034000000000000000000000000000000000000000000000
          00000000000000000000151515993B3B3BFF1A1A1AAA0303033F0303033F1A1A
          1AAA3B3B3BFF1515159900000000000000000000000000000000000000000000
          000000000000000000002E2E2EE03B3B3BFF0303033F00000000000000000303
          033F3B3B3BFF2E2E2EE000000000000000000000000000000000000000000000
          000000000000000000003B3B3BFF3B3B3BFF0000000000000000000000000000
          00003B3B3BFF3B3B3BFF00000000000000000000000000000000000000000000
          000000000000000000003B3B3BFF3B3B3BFF0000000000000000000000000000
          00003B3B3BFF3B3B3BFF00000000000000000000000000000000000000000000
          000000000000000000003B3B3BFF3B3B3BFF0000000000000000000000000000
          00003B3B3BFF3B3B3BFF00000000000000000000000000000000000000000000
          000000000000000000003B3B3BFF3B3B3BFF0000000000000000000000000000
          00003B3B3BFF3B3B3BFF00000000000000000000000000000000000000000000
          000000000000000000003B3B3BFF3B3B3BFF0000000000000000000000000000
          00003B3B3BFF3B3B3BFF00000000000000000000000000000000000000000000
          000000000000000000003B3B3BFF3B3B3BFF0000000000000000000000000000
          00003B3B3BFF3B3B3BFF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          7E000000424D7E000000000000003E0000002800000010000000100000000100
          010000000000400000000000000000000000020000000000000000000000FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00003B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00003B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00003B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00003B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00003B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          7E000000424D7E000000000000003E0000002800000010000000100000000100
          010000000000400000000000000000000000020000000000000000000000FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00003B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000003B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00003B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000003B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00003B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          7E000000424D7E000000000000003E0000002800000010000000100000000100
          010000000000400000000000000000000000020000000000000000000000FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00003B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000003B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00003B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000003B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00003B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          7E000000424D7E000000000000003E0000002800000010000000100000000100
          010000000000400000000000000000000000020000000000000000000000FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00003B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00003B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00003B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00003B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00003B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          7E000000424D7E000000000000003E0000002800000010000000100000000100
          010000000000400000000000000000000000020000000000000000000000FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00083B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00083B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000F0F0F81000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000003B3B3BFF0F0F0F810000000000000000000000003B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF000000003B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF0F0F0F8100000000000000000000
          0000000000000000000000000000000000000000000000000000000000003B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF0E0E0E7E000000003B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF000000003B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF0E0E0E7E00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000003B3B3BFF0E0E0E7E0000000000000000000000003B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF000000000000
          000000000000000000000E0E0E7E000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00043B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00023B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          7E000000424D7E000000000000003E0000002800000010000000100000000100
          010000000000400000000000000000000000020000000000000000000000FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00083B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00083B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000F0F0F81000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000003B3B3BFF0F0F0F810000000000000000000000003B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF000000003B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF0F0F0F8100000000000000000000
          0000000000000000000000000000000000000000000000000000000000003B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF0E0E0E7E000000003B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF000000003B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF0E0E0E7E00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000003B3B3BFF0E0E0E7E0000000000000000000000003B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF000000000000
          000000000000000000000E0E0E7E000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00043B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00023B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          7E000000424D7E000000000000003E0000002800000010000000100000000100
          010000000000400000000000000000000000020000000000000000000000FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00083B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00083B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000F0F0F8100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000F0F0F813B3B3BFF00000000000000000000000000000000000000003B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF000000000F0F
          0F813B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF00000000000000000000
          00000000000000000000000000000000000000000000000000000E0E0E7E3B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF00000000000000003B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF000000000E0E
          0E7E3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000E0E0E7E3B3B3BFF00000000000000000000000000000000000000003B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF000000000000
          0000000000000E0E0E7E00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00043B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00023B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B
          3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF3B3B3BFF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          7E000000424D7E000000000000003E0000002800000010000000100000000100
          010000000000400000000000000000000000020000000000000000000000FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000522B117B0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000002A16
          093FAF642FFF2C17094200000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000007E43
          1BBDBD7B49FF7D431ABD000000000000000089898989BFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBF8989898900000000000000002A16093FB167
          32FFC48858FFB06631FF2C17094200000000BFBFBFBF5C473DFF422E25FF422D
          24FF412D24FF412D25FF291A14FFBFBFBFBF00000000000000007F431BBDAB5B
          26FFAA5B25FFAA5A24FF7E431BBD000000008E8E8E8EBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBF8E8E8E8E0000000000000000000000000000
          0000AB5C25FF00000000000000000000000089898989BFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF89898989000000000000
          0000AC5D26FF000000000000000000000000BFBFBFBF614B3FFF463227FF4631
          27FF463026FF453026FF453026FF443026FF2B1C16FFBFBFBFBF000000000000
          0000000000000000000000000000000000008E8E8E8EBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF8E8E8E8E000000000000
          00000000000000000000000000000000000089898989BFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBF898989890000000000000000000000000000
          0000AD5E28FF000000000000000000000000BFBFBFBF645042FF4B3529FF4A35
          29FF4B3429FF4A3429FF2F1E18FFBFBFBFBF0000000000000000000000000000
          0000AE5F29FF0000000000000000000000008E8E8E8EBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBF8E8E8E8E0000000000000000824820BDAF61
          2AFFAF602AFFAE6029FF81461EBD0000000089898989BFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF898989892B180B3FB86F
          3AFFD09869FFB86F3AFF2D190B4200000000BFBFBFBF695345FF4F392CFF4F39
          2CFF4F392CFF4E382CFF4E372BFF4D372BFF322119FFBFBFBFBF000000008249
          21BDC88B5AFF824920BD00000000000000008E8E8E8EBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF8E8E8E8E000000002B18
          0B3FB8703BFF2E190B4200000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00005530167B0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000A95923FF0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000A95923FF0000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000007E431BBDA959
          24FFA95923FFA95923FF7D421ABD0000000089898989BFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBF8989898900000000000000002A16093FB065
          31FFC48656FFB06530FF2C17094200000000BFBFBFBF5C473DFF422E25FF422D
          24FF412D24FF412D25FF291A14FFBFBFBFBF0000000000000000000000007E43
          1BBDBD7C4BFF7E431BBD00000000000000008E8E8E8EBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBF8E8E8E8E0000000000000000000000002A16
          093FB16732FF2C170942000000000000000089898989BFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF89898989000000000000
          0000522C127B000000000000000000000000BFBFBFBF614B3FFF463227FF4631
          27FF463026FF453026FF453026FF443026FF2B1C16FFBFBFBFBF000000000000
          0000000000000000000000000000000000008E8E8E8EBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF8E8E8E8E000000000000
          00000000000000000000000000000000000089898989BFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBF898989890000000000000000000000000000
          0000532D137B000000000000000000000000BFBFBFBF645042FF4B3529FF4A35
          29FF4B3429FF4A3429FF2F1E18FFBFBFBFBF0000000000000000000000002B18
          0A3FB66C38FF2D180B4200000000000000008E8E8E8EBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBF8E8E8E8E0000000000000000000000008248
          1FBDC68957FF81471EBD000000000000000089898989BFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF898989892B180B3FB86F
          3AFFD09869FFB86F3AFF2D190B4200000000BFBFBFBF695345FF4F392CFF4F39
          2CFF4F392CFF4E382CFF4E372BFF4D372BFF322119FFBFBFBFBF8A5734BDB973
          43FFB7703EFFB56D3AFF854F28BD000000008E8E8E8EBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF8E8E8E8E000000000000
          0000BA7545FF0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000BD7C4EFF0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00}
        MaskColor = clWhite
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00}
        MaskColor = clWhite
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00}
        MaskColor = clWhite
      end>
  end
  object alActions: TActionList
    Images = ilActions
    Left = 416
    Top = 344
    object aToggleFontBold: TAction
      AutoCheck = True
      Caption = '&Bold'
      Hint = 'Bold'
      ImageIndex = 0
    end
    object aToggleFontItalic: TAction
      AutoCheck = True
      Caption = '&Italic'
      Hint = 'Italic'
      ImageIndex = 1
    end
    object aToggleFontUnderline: TAction
      AutoCheck = True
      Caption = '&Underline'
      Hint = 'Underline'
      ImageIndex = 2
    end
    object aToggleParagraphAlignmentLeft: TAction
      AutoCheck = True
      Caption = 'Align Text &Left'
      GroupIndex = 1
      Hint = 'Align Left'
      ImageIndex = 3
    end
    object aToggleParagraphAlignmentCenter: TAction
      Tag = 2
      AutoCheck = True
      Caption = '&Center'
      GroupIndex = 1
      Hint = 'Center'
      ImageIndex = 4
    end
    object aToggleParagraphAlignmentRight: TAction
      Tag = 1
      AutoCheck = True
      Caption = 'Align Text &Right'
      GroupIndex = 1
      Hint = 'Align Right'
      ImageIndex = 5
    end
    object aToggleParagraphAlignmentJustify: TAction
      Tag = 3
      AutoCheck = True
      Caption = '&Justify'
      GroupIndex = 1
      Hint = 'Justify'
      ImageIndex = 6
    end
    object aSetSingleParagraphSpacing: TAction
      AutoCheck = True
      GroupIndex = 2
      ImageIndex = 12
    end
    object aSetDoubleParagraphSpacing: TAction
      Tag = 2
      AutoCheck = True
      GroupIndex = 2
      ImageIndex = 14
    end
    object aSetSesquialteralParagraphSpacing: TAction
      Tag = 1
      AutoCheck = True
      GroupIndex = 2
      ImageIndex = 13
    end
    object aIncrementIndent: TAction
      Hint = 'Increase Indent'
      ImageIndex = 8
      OnExecute = aIncrementIndentExecute
    end
    object aDecrementIndent: TAction
      Hint = 'Decrease Indent'
      ImageIndex = 9
      OnExecute = aDecrementIndentExecute
    end
    object aFontDialog: TAction
      Caption = 'Font...'
      OnExecute = aFontDialogExecute
    end
    object aParagraphDialog: TAction
      Caption = 'Paragraph...'
      OnExecute = aParagraphDialogExecute
    end
    object aTabsDialog: TAction
      Caption = 'Tabs...'
      OnExecute = aTabsDialogExecute
    end
    object aSpacingDecrease: TAction
      Caption = 'aSpacingDecrease'
      ImageIndex = 11
      OnExecute = aSpacingDecreaseExecute
    end
    object aSpacingIncrease: TAction
      Caption = 'aSpacingIncrease'
      ImageIndex = 10
      OnExecute = aSpacingIncreaseExecute
    end
  end
end
