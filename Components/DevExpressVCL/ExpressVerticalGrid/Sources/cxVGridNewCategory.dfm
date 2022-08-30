object fmCreateCategory: TfmCreateCategory
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  ClientHeight = 88
  ClientWidth = 271
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbCaption: TLabel
    Left = 11
    Top = 13
    Width = 3
    Height = 13
    FocusControl = edCaption
  end
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 271
    Height = 88
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object edCaption: TcxTextEdit
      Left = 10
      Top = 10
      Properties.OnChange = edCaptionPropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 0
      Width = 250
    end
    object btOK: TcxButton
      Left = 104
      Top = 49
      Width = 75
      Height = 24
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
    object btCancel: TcxButton
      Left = 185
      Top = 49
      Width = 75
      Height = 24
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahLeft
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 0
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahLeft
      AlignVert = avTop
      Control = edCaption
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 250
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      AlignVert = avTop
      Index = 1
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup2
      CaptionOptions.Text = 'btOK'
      CaptionOptions.Visible = False
      Control = btOK
      ControlOptions.OriginalHeight = 24
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup2
      CaptionOptions.Text = 'btCancel'
      CaptionOptions.Visible = False
      Control = btCancel
      ControlOptions.OriginalHeight = 24
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 8
    Top = 48
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      LookAndFeel.Kind = lfUltraFlat
      LookAndFeel.NativeStyle = True
      PixelsPerInch = 96
    end
  end
end
