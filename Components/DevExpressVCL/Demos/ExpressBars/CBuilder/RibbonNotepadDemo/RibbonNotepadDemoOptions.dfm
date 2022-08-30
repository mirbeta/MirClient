object RibbonDemoOptionsForm: TRibbonDemoOptionsForm
  Left = 417
  Top = 292
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Options'
  ClientHeight = 175
  ClientWidth = 387
  Color = clBtnFace
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object gbPanel: TdxBevel
    Left = 8
    Top = 8
    Width = 371
    Height = 128
    Anchors = [akLeft, akTop, akRight]
    Shape = dxbsFrame
  end
  object lblColorScheme: TcxLabel
    Left = 22
    Top = 51
    Caption = 'Color scheme:'
    Style.TransparentBorder = False
    Transparent = True
  end
  object lblRibbonStyle: TcxLabel
    Left = 22
    Top = 24
    Caption = 'Ribbon style:'
    Style.TransparentBorder = False
    Transparent = True
  end
  object lblScreenTipStyle: TcxLabel
    Left = 22
    Top = 105
    Caption = 'ScreenTip style:'
    Style.TransparentBorder = False
    Transparent = True
  end
  object lblColorSchemeAccent: TcxLabel
    Left = 22
    Top = 78
    Caption = 'Color scheme accent:'
    Style.TransparentBorder = False
    Transparent = True
  end
  object btnOk: TcxButton
    Left = 221
    Top = 142
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TcxButton
    Left = 304
    Top = 142
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object cbRibbonStyle: TcxComboBox
    Left = 131
    Top = 21
    Anchors = [akLeft, akTop, akRight]
    Properties.DropDownListStyle = lsFixedList
    Properties.OnChange = cbRibbonStyleSelect
    TabOrder = 3
    Width = 237
  end
  object cbColorScheme: TcxComboBox
    Left = 131
    Top = 48
    Anchors = [akLeft, akTop, akRight]
    Properties.DropDownListStyle = lsFixedList
    TabOrder = 2
    Width = 237
  end
  object cbColorSchemeAccent: TcxComboBox
    Left = 131
    Top = 75
    Anchors = [akLeft, akTop, akRight]
    Properties.DropDownListStyle = lsFixedList
    TabOrder = 5
    Width = 237
  end
  object cbScreenTipStyle: TcxComboBox
    Left = 131
    Top = 102
    Anchors = [akLeft, akTop, akRight]
    Properties.DropDownListStyle = lsFixedList
    Properties.Items.Strings = (
      'Show feature descriptions in ScreenTips'
      'Don'#39't show feature descriptions in ScreenTips'
      'Don'#39't show ScreenTips')
    TabOrder = 4
    Width = 237
  end
end
