object fmSpellCheckerAutoCorrectExceptionsForm: TfmSpellCheckerAutoCorrectExceptionsForm
  Left = 412
  Top = 178
  BorderStyle = bsDialog
  ClientHeight = 421
  ClientWidth = 318
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 318
    Height = 421
    Align = alClient
    TabOrder = 0
    AutoSize = True
    object btnOk: TcxButton
      Left = 156
      Top = 388
      Width = 73
      Height = 23
      Anchors = [akRight, akBottom]
      Caption = 'btnOk'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnClose: TcxButton
      Left = 235
      Top = 388
      Width = 73
      Height = 23
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'btnClose'
      ModalResult = 2
      TabOrder = 1
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      ShowBorder = False
      Index = 0
    end
    object lgFirstLetter: TdxLayoutGroup
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'gbFirstLetter'
      ButtonOptions.Buttons = <>
      Index = 0
    end
    object lgInitialCaps: TdxLayoutGroup
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'gbInitialCaps'
      ButtonOptions.Buttons = <>
      Index = 1
    end
    object dxLayoutGroup4: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'btnOk'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 73
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'btnClose'
      CaptionOptions.Visible = False
      Control = btnClose
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 73
      ControlOptions.ShowBorder = False
      Index = 1
    end
  end
end
