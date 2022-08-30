object dxfmColorPalette: TdxfmColorPalette
  Left = 410
  Top = 280
  AutoSize = True
  BorderStyle = bsDialog
  ClientHeight = 201
  ClientWidth = 161
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object dxLayoutControl1: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 161
    Height = 201
    TabOrder = 0
    AutoSize = True
    object cgColors: TdxColorGallery
      Left = 10
      Top = 41
      Width = 132
      Height = 84
      ColorPalette = cpExtended
      TabOrder = 0
      Transparent = True
      OnItemClick = cgColorsItemClick
    end
    object btnNoFill: TcxButton
      Left = 10
      Top = 10
      Width = 132
      Height = 25
      Caption = 'No Fill'
      SpeedButtonOptions.GroupIndex = 1
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.AllowAllUp = True
      SpeedButtonOptions.Down = True
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 1
      OnClick = ButtonClick
    end
    object btnFillEffects: TcxButton
      Left = 10
      Top = 143
      Width = 132
      Height = 25
      Caption = '&Fill Effects ...'
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 2
      OnClick = btnFillEffectsClick
    end
    object btnMoreColors: TcxButton
      Left = 10
      Top = 174
      Width = 132
      Height = 25
      Caption = '&More Colors ...'
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 3
      OnClick = btnMoreColorsClick
    end
    object dxLayoutControl1Group_Root: TdxLayoutGroup
      AlignHorz = ahLeft
      AlignVert = avTop
      SizeOptions.AssignedValues = [sovSizableHorz]
      SizeOptions.SizableHorz = False
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      ShowBorder = False
      Index = -1
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahLeft
      AlignVert = avBottom
      Control = cgColors
      ControlOptions.OriginalHeight = 84
      ControlOptions.OriginalWidth = 132
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahClient
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnNoFill
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 50
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutControl1Group_Root
      AlignVert = avBottom
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnFillEffects
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutControl1Group_Root
      AlignVert = avBottom
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnMoreColors
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = dxLayoutControl1Group_Root
      AlignVert = avBottom
      CaptionOptions.Text = 'Separator'
      Index = 1
    end
  end
end
