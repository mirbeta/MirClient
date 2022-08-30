object DllForm: TDllForm
  Left = 369
  Top = 334
  BorderStyle = bsDialog
  Caption = 'DllForm'
  ClientHeight = 81
  ClientWidth = 392
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    392
    81)
  PixelsPerInch = 96
  TextHeight = 12
  object EditTest: TEdit
    Left = 16
    Top = 16
    Width = 359
    Height = 20
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'EditTest'
  end
  object BitBtnTest: TBitBtn
    Left = 287
    Top = 46
    Width = 82
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 1
    Kind = bkOK
  end
end
