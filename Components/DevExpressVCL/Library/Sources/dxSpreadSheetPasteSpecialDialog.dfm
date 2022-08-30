object dxSpreadSheetPasteSpecialDialogForm: TdxSpreadSheetPasteSpecialDialogForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  ClientHeight = 320
  ClientWidth = 320
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 320
    Height = 320
    Align = alClient
    TabOrder = 0
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object btnCancel: TcxButton
      Left = 235
      Top = 285
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'btnCancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnOk: TcxButton
      Left = 154
      Top = 285
      Width = 75
      Height = 25
      Caption = 'btnOk'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object lbFormats: TcxListBox
      Left = 10
      Top = 245
      Width = 300
      Height = 34
      ItemHeight = 13
      TabOrder = 11
      OnDblClick = lbFormatsDblClick
    end
    object cbValues: TcxCheckBox
      Left = 10
      Top = 31
      Caption = 'cbValues'
      Properties.OnChange = PasteOptionsChanged
      State = cbsChecked
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Transparent = True
      Width = 300
    end
    object cbFormulas: TcxCheckBox
      Left = 30
      Top = 54
      Caption = 'cbFormulas'
      Properties.OnChange = PasteOptionsChanged
      State = cbsChecked
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Transparent = True
      Width = 280
    end
    object lbPasteOptions: TcxLabel
      Left = 10
      Top = 10
      AutoSize = False
      Caption = 'lbPasteOptions'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Height = 15
      Width = 300
    end
    object cbSkipBlanks: TcxCheckBox
      Left = 10
      Top = 204
      Caption = 'cbSkipBlanks'
      Properties.OnChange = PasteOptionsChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 10
      Transparent = True
      Width = 300
    end
    object cbComments: TcxCheckBox
      Left = 10
      Top = 77
      Caption = 'cbComments'
      Properties.OnChange = PasteOptionsChanged
      State = cbsChecked
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 5
      Transparent = True
      Width = 300
    end
    object cbStyles: TcxCheckBox
      Left = 10
      Top = 100
      Caption = 'cbStyles'
      Properties.OnChange = PasteOptionsChanged
      State = cbsChecked
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Transparent = True
      Width = 300
    end
    object cbColumnWidths: TcxCheckBox
      Left = 10
      Top = 169
      Caption = 'cbColumnWidths'
      Properties.OnChange = PasteOptionsChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 9
      Transparent = True
      Width = 300
    end
    object rbAll: TcxRadioButton
      Left = 30
      Top = 123
      Width = 280
      Height = 17
      Caption = 'rbAll'
      Checked = True
      TabOrder = 7
      TabStop = True
      Transparent = True
    end
    object rbNumberFormatting: TcxRadioButton
      Left = 30
      Top = 146
      Width = 280
      Height = 17
      Caption = 'rbNumberFormatting'
      TabOrder = 8
      Transparent = True
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      ShowBorder = False
      Index = -1
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignVert = avClient
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      AlignVert = avBottom
      LayoutDirection = ldHorizontal
      Index = 0
      AutoCreated = True
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lgPasteOptions: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lgPasteFormats: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 2
    end
    object liPasteFormat: TdxLayoutItem
      Parent = lgPasteFormats
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Paste As:'
      CaptionOptions.Layout = clTop
      Control = lbFormats
      ControlOptions.OriginalHeight = 97
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = lgPasteOptions
      CaptionOptions.Visible = False
      Control = cbValues
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = lgPasteOptions
      CaptionOptions.Visible = False
      Offsets.Left = 20
      Control = cbFormulas
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = lgPasteOptions
      CaptionOptions.Visible = False
      Control = lbPasteOptions
      ControlOptions.OriginalHeight = 15
      ControlOptions.OriginalWidth = 72
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = lgPasteOptions
      CaptionOptions.Visible = False
      Control = cbSkipBlanks
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 9
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lgPasteOptions
      CaptionOptions.Text = 'Separator'
      Index = 8
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = lgPasteOptions
      CaptionOptions.Visible = False
      Control = cbComments
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = lgPasteOptions
      CaptionOptions.Visible = False
      Control = cbStyles
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = lgPasteOptions
      CaptionOptions.Visible = False
      Control = cbColumnWidths
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 7
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = lgPasteOptions
      CaptionOptions.Visible = False
      Offsets.Left = 20
      Control = rbAll
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 113
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = lgPasteOptions
      CaptionOptions.Visible = False
      Offsets.Left = 20
      Control = rbNumberFormatting
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 219
      ControlOptions.ShowBorder = False
      Index = 6
    end
  end
  object dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList
    Left = 8
    Top = 400
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
