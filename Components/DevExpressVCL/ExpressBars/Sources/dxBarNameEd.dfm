object dxBarNameEd: TdxBarNameEd
  Left = 199
  Top = 196
  AutoSize = True
  BorderIcons = []
  BorderStyle = bsDialog
  ClientHeight = 129
  ClientWidth = 297
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 297
    Height = 129
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object BOK: TcxButton
      Left = 59
      Top = 55
      Width = 73
      Height = 23
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
    object BCancel: TcxButton
      Left = 138
      Top = 55
      Width = 73
      Height = 23
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
    end
    object EName: TcxTextEdit
      Left = 10
      Top = 28
      Properties.OnChange = ENameChange
      Style.HotTrack = False
      TabOrder = 0
      Width = 251
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
      AlignHorz = ahCenter
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup1
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = BOK
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 73
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup1
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = BCancel
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 73
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object LName: TdxLayoutItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Toolbar:'
      CaptionOptions.Layout = clTop
      Control = EName
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 251
      ControlOptions.ShowBorder = False
      Index = 0
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 8
    Top = 40
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
    end
  end
end
