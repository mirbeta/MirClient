object ImageViewerDemoResizeImageForm: TImageViewerDemoResizeImageForm
  Left = 691
  Top = 402
  BorderStyle = bsDialog
  Caption = 'Resize by'
  ClientHeight = 213
  ClientWidth = 213
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object cxLabel1: TcxLabel
    Left = 20
    Top = 8
    Caption = 'Percentage:'
    Transparent = True
  end
  object seScale: TcxSpinEdit
    Left = 90
    Top = 8
    Properties.MaxValue = 10000.000000000000000000
    Properties.MinValue = 10.000000000000000000
    Properties.SpinButtons.ShowFastButtons = True
    Properties.ValueType = vtFloat
    TabOrder = 1
    Value = 75.000000000000000000
    Width = 87
  end
  object cgbPixels: TdxCheckGroupBox
    Left = 12
    Top = 48
    Caption = 'Pixels'
    CheckBox.Checked = False
    Properties.OnChange = cgbPixelsPropertiesChange
    Style.LookAndFeel.Kind = lfFlat
    Style.LookAndFeel.NativeStyle = True
    StyleDisabled.LookAndFeel.Kind = lfFlat
    StyleDisabled.LookAndFeel.NativeStyle = True
    StyleFocused.LookAndFeel.Kind = lfFlat
    StyleFocused.LookAndFeel.NativeStyle = True
    StyleHot.LookAndFeel.Kind = lfFlat
    StyleHot.LookAndFeel.NativeStyle = True
    TabOrder = 2
    Height = 113
    Width = 189
    object cxLabel2: TcxLabel
      Left = 16
      Top = 24
      Caption = 'Horizontal:'
      Transparent = True
    end
    object cxLabel3: TcxLabel
      Left = 16
      Top = 48
      Caption = 'Vertical:'
      Transparent = True
    end
    object teWidth: TcxTextEdit
      Left = 78
      Top = 24
      Properties.OnChange = teWidthPropertiesChange
      Properties.OnValidate = teHorizontalPropertiesValidate
      TabOrder = 2
      Text = '32'
      Width = 59
    end
    object teHeight: TcxTextEdit
      Left = 78
      Top = 48
      Properties.OnChange = teHeightPropertiesChange
      TabOrder = 3
      Text = '32'
      Width = 59
    end
    object cbAspectRatio: TcxCheckBox
      Left = 14
      Top = 80
      Caption = 'Maintain aspect ratio'
      Properties.OnChange = cbAspectRatioPropertiesChange
      State = cbsChecked
      Style.LookAndFeel.NativeStyle = True
      StyleDisabled.LookAndFeel.NativeStyle = True
      StyleFocused.LookAndFeel.NativeStyle = True
      StyleHot.LookAndFeel.NativeStyle = True
      TabOrder = 4
      Width = 137
    end
  end
  object btnApply: TcxButton
    Left = 29
    Top = 176
    Width = 72
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object cxButton1: TcxButton
    Left = 117
    Top = 176
    Width = 72
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
end
