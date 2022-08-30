object dxfmDefinePrintStyles: TdxfmDefinePrintStyles
  Left = 414
  Top = 232
  BorderStyle = bsDialog
  Caption = 'Define Print Styles'
  ClientHeight = 250
  ClientWidth = 300
  Color = clBtnFace
  Constraints.MinHeight = 220
  Constraints.MinWidth = 290
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 300
    Height = 250
    Align = alClient
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object lbxPrintStyles: TcxListBox
      Left = 10
      Top = 28
      Width = 189
      Height = 212
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 34
      ListStyle = lbOwnerDrawFixed
      PopupMenu = pmPrintStyles
      Style.TransparentBorder = False
      TabOrder = 0
      OnClick = lbxPrintStylesClick
      OnDblClick = EditClick
      OnDrawItem = lbxPrintStylesDrawItem
    end
    object btnEdit: TcxButton
      Left = 205
      Top = 28
      Width = 85
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '&Edit...'
      TabOrder = 1
      OnClick = EditClick
    end
    object btnCopy: TcxButton
      Left = 205
      Top = 57
      Width = 85
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '&Copy...'
      TabOrder = 2
      OnClick = CopyClick
    end
    object btnReset: TcxButton
      Left = 205
      Top = 86
      Width = 85
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '&Reset...'
      TabOrder = 3
      OnClick = ResetClick
    end
    object btnClose: TcxButton
      Left = 205
      Top = 188
      Width = 85
      Height = 23
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Close'
      Default = True
      ModalResult = 1
      TabOrder = 4
    end
    object btnHelp: TcxButton
      Left = 205
      Top = 217
      Width = 85
      Height = 23
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = '&Help'
      ModalResult = 1
      TabOrder = 5
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = -1
    end
    object lblPrintStyles: TdxLayoutItem
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Print &Styles:'
      CaptionOptions.Layout = clTop
      Control = lbxPrintStyles
      ControlOptions.OriginalHeight = 165
      ControlOptions.OriginalWidth = 200
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      AlignVert = avClient
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 4
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      CaptionOptions.Text = ' '
      CaptionOptions.Layout = clTop
      Control = btnEdit
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      CaptionOptions.Text = 'btnCopy'
      CaptionOptions.Visible = False
      Control = btnCopy
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      CaptionOptions.Text = 'btnReset'
      CaptionOptions.Visible = False
      Control = btnReset
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object liClose: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      AlignVert = avBottom
      CaptionOptions.Visible = False
      Control = btnClose
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object liHelp: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      AlignVert = avBottom
      CaptionOptions.Text = 'btnHelp'
      CaptionOptions.Visible = False
      Control = btnHelp
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 4
    end
  end
  object pmPrintStyles: TPopupMenu
    Images = ilPrintStyles
    OnPopup = pmPrintStylesPopup
    Left = 26
    Top = 120
    object miEdit: TMenuItem
      Caption = '&Edit ...'
      Default = True
      ImageIndex = 15
      ShortCut = 16397
      OnClick = EditClick
    end
    object miLine1: TMenuItem
      Caption = '-'
    end
    object miCopy: TMenuItem
      Caption = '&Copy ...'
      ImageIndex = 2
      ShortCut = 45
      OnClick = CopyClick
    end
    object miReset: TMenuItem
      Caption = '&Reset ...'
      OnClick = ResetClick
    end
    object miClear: TMenuItem
      Caption = 'C&lear ...'
      OnClick = ClearClick
    end
  end
  object ilPrintStyles: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 7864376
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 88
    Top = 120
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
