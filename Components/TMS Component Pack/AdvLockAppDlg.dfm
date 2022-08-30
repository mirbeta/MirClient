object AdvUnLockForm: TAdvUnLockForm
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Enter the password to reopen'
  ClientHeight = 112
  ClientWidth = 285
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lbl_usr: TLabel
    Left = 8
    Top = 11
    Width = 52
    Height = 13
    Caption = 'Username:'
  end
  object lbl_pwd: TLabel
    Left = 8
    Top = 40
    Width = 50
    Height = 13
    Caption = 'Password:'
  end
  object lb_Username: TLabel
    Left = 80
    Top = 11
    Width = 7
    Height = 13
    Caption = 'A'
  end
  object me_Password: TMaskEdit
    Left = 80
    Top = 37
    Width = 197
    Height = 21
    PasswordChar = '*'
    TabOrder = 0
  end
  object Button1: TButton
    Left = 121
    Top = 79
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 202
    Top = 79
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
