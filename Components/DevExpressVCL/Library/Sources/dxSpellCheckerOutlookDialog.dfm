object fmSpellCheckerOutlookForm: TfmSpellCheckerOutlookForm
  Left = -603
  Top = 512
  AutoSize = True
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'fmSpellCheckerOutlookForm'
  ClientHeight = 209
  ClientWidth = 465
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    000001002000000000004004000000000000000000000000000000000000FFFF
    FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000050000
    001200000011000000050000000000000000000000000000000000000000FFFF
    FF00FFFFFF00FFFFFF000000000013C56B0011C36900000000070000002C0981
    44A9087B4199000000180BBB62000BBA610009B9600009B85F0008B75E00FFFF
    FF00FFFFFF00FFFFFF000000000015C86D0000000008000000330C8D4BCC10C1
    67FF0EC066FF0000003D000000050CBC62000BBB61000ABA610009B95F00FFFF
    FF00FFFFFF00FFFFFF00000000000000000900000038109C55DB13C66BFF12C4
    6AFF10C268FF09753FB2000000190DBE65000DBD64000BBC62000ABB6100FFFF
    FF00FFFFFF00FFFFFF000000000A0000003D15AC5FE617CB70FF16C96EFF14C7
    6CFF12C56BFF12C369FF0000003F000000050EBF65000DBE64000DBC6300FFFF
    FF00FFFFFF00FFFFFF00000000201ABE6AEC1BD175FF1ACF73FF18CD71FF094F
    2BB115C46BFE13C66BFF0B7D43B80000001A0FC167000FC066000DBE6400FFFF
    FF00FFFFFF00FFFFFF000000001A179856BF1ED578FF1CCF74FE0C5C33890000
    00360F8549BC16C96EFF14C76DFF000000400000000610C268000FC16700FFFF
    FF00FFFFFF00FFFFFF00000000060000002D189E5BB814884D9D0000001F0000
    00080000003716B262EE17CB6FFF0E8549BE0000001B12C56A0011C368000000
    0000000000170000004F00000062000000530000003F0000005B000000650000
    004F000000260B59327F19CD72FE17CC71FF000000420000000613C66B000000
    00170000006EFFFFFFFFFFFFFFFFFFFFFFFF0000008FFFFFFFFFFFFFFFFFFFFF
    FFFF0000007000000037149352C91BD073FF12914FC50000001C15C96E000000
    003AFFFFFFFF000000B4000000CCFFFFFFFF000000ADFFFFFFFF0000009E0000
    008FFFFFFFFF000000530000003F1DD377FF1ACA71FB00000042000000070000
    00170000006EFFFFFFFFFFFFFFFFFFFFFFFF000000ADFFFFFFFF0000009E0000
    008FFFFFFFFF0000004F000000140E64398E1ED478FF149856C70000001D0000
    00000000002E0000008F000000C1FFFFFFFF0000009EFFFFFFFFFFFFFFFFFFFF
    FFFF0000006E00000017000000030000002719A25CD01FD377FD0000003C0000
    00000000003AFFFFFFFFFFFFFFFF0000006E00000074FFFFFFFF0000009E0000
    004F0000001725DE800025DD7F00000000070000003F21D97CFF158F528D0000
    0000000000170000004F0000004F000000170000004FFFFFFFFF0000004F28E2
    840028E1830027E0820025DF810024DD80000000000F0F5C34450000001B0000
    000000000000000000000000000000000000000000170000003A000000170000
    000000000000000000000000000000000000000000010000000400000001FE1F
    FFFFFC1FFFFFF80FFFFFF00FFFFFE007FFFFE007FFFFE003FFFFE003FFFF8001
    FFFF0001FFFF0000FFFF0000FFFF8000FFFF8030FFFF80F8FFFFF8F8FFFF}
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 465
    Height = 209
    TabOrder = 0
    AutoSize = True
    object teMisspelledWord: TcxTextEdit
      Left = 81
      Top = 10
      ParentColor = True
      Properties.ReadOnly = True
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 0
      Width = 360
    end
    object teSuggestion: TcxTextEdit
      Left = 81
      Top = 35
      Properties.OnChange = teSuggestionPropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 1
      Width = 360
    end
    object lbxSuggestions: TcxListBox
      Left = 81
      Top = 60
      Width = 178
      Height = 95
      ItemHeight = 13
      Style.TransparentBorder = False
      TabOrder = 2
      OnClick = lbxSuggestionsClick
    end
    object btnIgnore: TcxButton
      Left = 265
      Top = 60
      Width = 85
      Height = 23
      Caption = 'btnIgnore'
      TabOrder = 3
      OnClick = btnIgnoreClick
    end
    object btnChange: TcxButton
      Left = 265
      Top = 89
      Width = 85
      Height = 23
      Caption = 'btnChange'
      TabOrder = 4
      OnClick = btnChangeClick
    end
    object btnAdd: TcxButton
      Left = 265
      Top = 118
      Width = 85
      Height = 23
      Caption = 'btnAdd'
      TabOrder = 5
      OnClick = btnAddClick
    end
    object btnIgnoreAll: TcxButton
      Left = 356
      Top = 60
      Width = 85
      Height = 23
      Caption = 'btnIgnoreAll'
      TabOrder = 6
      OnClick = btnIgnoreAllClick
    end
    object btnChangeAll: TcxButton
      Left = 356
      Top = 89
      Width = 85
      Height = 23
      Caption = 'btnChangeAll'
      TabOrder = 7
      OnClick = btnChangeAllClick
    end
    object btnSuggest: TcxButton
      Left = 356
      Top = 118
      Width = 85
      Height = 23
      Caption = 'btnSuggest'
      TabOrder = 8
      OnClick = btnSuggestClick
    end
    object btnOptions: TcxButton
      Left = 174
      Top = 161
      Width = 85
      Height = 23
      Caption = 'btnOptions'
      TabOrder = 9
      OnClick = btnOptionsClick
    end
    object btnUndoLast: TcxButton
      Left = 265
      Top = 161
      Width = 85
      Height = 23
      Caption = 'btnUndoLast'
      TabOrder = 10
      OnClick = btnUndoLastClick
    end
    object btnCancel: TcxButton
      Left = 356
      Top = 161
      Width = 85
      Height = 23
      Cancel = True
      Caption = 'btnCancel'
      ModalResult = 2
      TabOrder = 11
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahLeft
      AlignVert = avTop
      LayoutLookAndFeel = dxLayoutCxLookAndFeel1
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 2
      ShowBorder = False
      Index = -1
    end
    object lbMisspelled: TdxLayoutItem
      Parent = lcMainGroup_Root
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'asd'
      Control = teMisspelledWord
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 360
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lbChangeTo: TdxLayoutItem
      Parent = lcMainGroup_Root
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'lbChangeTo'
      Control = teSuggestion
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 360
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object lbSuggestions: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahClient
      CaptionOptions.AlignVert = tavTop
      CaptionOptions.Text = 'lbSuggestions'
      Control = lbxSuggestions
      ControlOptions.OriginalHeight = 95
      ControlOptions.OriginalWidth = 174
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup3: TdxLayoutGroup
      Parent = dxLayoutGroup2
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnIgnore'
      CaptionOptions.Visible = False
      Control = btnIgnore
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnChange'
      CaptionOptions.Visible = False
      Control = btnChange
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnAdd'
      CaptionOptions.Visible = False
      Control = btnAdd
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutGroup4: TdxLayoutGroup
      Parent = dxLayoutGroup2
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 2
      ShowBorder = False
      Index = 2
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnIgnoreAll'
      CaptionOptions.Visible = False
      Control = btnIgnoreAll
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnChangeAll'
      CaptionOptions.Visible = False
      Control = btnChangeAll
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnSuggest'
      CaptionOptions.Visible = False
      Control = btnSuggest
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutGroup5: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 3
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = dxLayoutGroup5
      CaptionOptions.Text = 'btnOptions'
      CaptionOptions.Visible = False
      Control = btnOptions
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = dxLayoutGroup5
      CaptionOptions.Text = 'btnUndoLast'
      CaptionOptions.Visible = False
      Control = btnUndoLast
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem12: TdxLayoutItem
      Parent = dxLayoutGroup5
      CaptionOptions.Text = 'btnCancel'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 2
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 8
    Top = 8
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
