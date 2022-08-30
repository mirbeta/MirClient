object fmSpellCheckerAutoCorrectOptionsForm: TfmSpellCheckerAutoCorrectOptionsForm
  Left = 311
  Top = 75
  AutoSize = True
  BorderStyle = bsDialog
  ClientHeight = 473
  ClientWidth = 460
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
    Width = 460
    Height = 473
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object cbCorrectInitialCaps: TcxCheckBox
      Left = 22
      Top = 28
      Caption = 'cbCorrectInitialCaps'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 0
      Transparent = True
    end
    object cbCorrectSentenceCaps: TcxCheckBox
      Left = 22
      Top = 51
      Caption = 'cbCorrectSentenceCaps'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 1
      Transparent = True
    end
    object cbCorrectCapsLock: TcxCheckBox
      Left = 22
      Top = 74
      Caption = 'cbCorrectCapsLock'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Transparent = True
    end
    object btnExceptions: TcxButton
      Left = 22
      Top = 97
      Width = 85
      Height = 23
      Caption = 'btnExceptions'
      TabOrder = 4
      OnClick = btnExceptionsClick
    end
    object cbReplaceTextAsYouType: TcxCheckBox
      Left = 22
      Top = 156
      Caption = 'cbReplaceTextAsYouType'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 5
      Transparent = True
    end
    object teWith: TcxTextEdit
      Left = 111
      Top = 197
      Properties.OnChange = teReplacePropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 7
      Width = 339
    end
    object btnOperation: TcxButton
      Left = 298
      Top = 349
      Width = 73
      Height = 23
      Caption = 'btnOperation'
      TabOrder = 9
      OnClick = btnOperationClick
    end
    object btnDelete: TcxButton
      Left = 377
      Top = 349
      Width = 73
      Height = 23
      Cancel = True
      Caption = 'btnDelete'
      TabOrder = 10
      OnClick = btnDeleteClick
    end
    object cbAutomaticallyUseSuggestions: TcxCheckBox
      Left = 22
      Top = 378
      Caption = 'cbAutomaticallyUseSuggestions'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 11
      Transparent = True
    end
    object btnCancel: TcxButton
      Left = 389
      Top = 413
      Width = 73
      Height = 23
      Cancel = True
      Caption = 'btnCancel'
      ModalResult = 2
      TabOrder = 13
    end
    object cbDisableCapsLock: TcxCheckBox
      Left = 340
      Top = 74
      Caption = 'cbDisableCapsLock'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Transparent = True
    end
    object teReplace: TcxTextEdit
      Left = 22
      Top = 197
      Properties.OnChange = teReplacePropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Width = 83
    end
    object lvReplacements: TcxListView
      Left = 22
      Top = 222
      Width = 428
      Height = 121
      ColumnClick = False
      Columns = <
        item
          MaxWidth = 81
          MinWidth = 81
          Width = 81
        end
        item
          AutoSize = True
        end>
      HideSelection = False
      IconOptions.WrapText = False
      ReadOnly = True
      RowSelect = True
      ShowColumnHeaders = False
      SortType = stText
      TabOrder = 8
      ViewStyle = vsReport
      OnClick = lvReplacementsClick
    end
    object btnOk: TcxButton
      Left = 310
      Top = 413
      Width = 73
      Height = 23
      Caption = 'btnOk'
      Default = True
      ModalResult = 1
      TabOrder = 12
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 2
      ShowBorder = False
      Index = -1
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cbCorrectInitialCaps'
      CaptionOptions.Visible = False
      Control = cbCorrectInitialCaps
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 117
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cbCorrectSentenceCaps'
      CaptionOptions.Visible = False
      Control = cbCorrectSentenceCaps
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 136
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cbCorrectCapsLock'
      CaptionOptions.Visible = False
      Control = cbCorrectCapsLock
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 112
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahLeft
      AlignVert = avBottom
      CaptionOptions.Text = 'btnExceptions'
      CaptionOptions.Visible = False
      Control = btnExceptions
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutGroup5
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'cbReplaceTextAsYouType'
      CaptionOptions.Visible = False
      Control = cbReplaceTextAsYouType
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 145
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lbWith: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'lbWith'
      CaptionOptions.Layout = clTop
      Control = teWith
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 339
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = dxLayoutGroup6
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnOperation'
      CaptionOptions.Visible = False
      Control = btnOperation
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 73
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = dxLayoutGroup6
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnDelete'
      CaptionOptions.Visible = False
      Control = btnDelete
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 73
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem12: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahLeft
      AlignVert = avBottom
      CaptionOptions.Text = 'cbAutomaticallyUseSuggestions'
      CaptionOptions.Visible = False
      Control = cbAutomaticallyUseSuggestions
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 171
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem14: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'btnCancel'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 73
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahRight
      CaptionOptions.Text = 'cbDisableCapsLock'
      CaptionOptions.Visible = False
      Control = cbDisableCapsLock
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 110
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lbReplace: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'lbReplace'
      CaptionOptions.Layout = clTop
      Control = teReplace
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 83
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object gbCapitalize: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      CaptionOptions.Text = 'gbCapitalize'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      Index = 0
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = gbCapitalize
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Hidden Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 2
      ShowBorder = False
      Index = 0
    end
    object gbReplaceTextAsYouType: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      CaptionOptions.Text = 'gbReplaceTextAsYouType'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      Index = 1
    end
    object dxLayoutGroup4: TdxLayoutGroup
      Parent = gbReplaceTextAsYouType
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Hidden Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup5: TdxLayoutGroup
      Parent = dxLayoutGroup4
      AlignHorz = ahClient
      CaptionOptions.Text = 'Hidden Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup6: TdxLayoutGroup
      Parent = dxLayoutGroup4
      AlignHorz = ahRight
      CaptionOptions.Text = 'Hidden Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutGroup2
      LayoutDirection = ldHorizontal
      Index = 2
      AutoCreated = True
    end
    object dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutGroup5
      AlignVert = avTop
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutGroup5
      Control = lvReplacements
      ControlOptions.OriginalHeight = 121
      ControlOptions.OriginalWidth = 244
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem13: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahRight
      AlignVert = avClient
      CaptionOptions.Text = 'btnOk'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 73
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup_Root
      AlignVert = avBottom
      LayoutDirection = ldHorizontal
      Index = 2
      AutoCreated = True
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 416
    Top = 24
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
