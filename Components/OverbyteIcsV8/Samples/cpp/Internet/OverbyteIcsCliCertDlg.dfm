object ClientCertDlg: TClientCertDlg
  Left = 183
  Top = 205
  Caption = 'Client Certificate Requested'
  ClientHeight = 201
  ClientWidth = 357
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 6
    Top = 6
    Width = 334
    Height = 13
    Caption = 
      'Server requested a client certificate, which one do you want to ' +
      'send?'
  end
  object CertListBox: TListBox
    Left = 6
    Top = 26
    Width = 335
    Height = 133
    ItemHeight = 13
    TabOrder = 0
  end
  object OKButton: TButton
    Left = 266
    Top = 166
    Width = 75
    Height = 21
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
  end
  object CancelButton: TButton
    Left = 6
    Top = 166
    Width = 129
    Height = 21
    Caption = 'Don'#39't send a certificate'
    ModalResult = 2
    TabOrder = 2
  end
end
