object dlg_wmie_Login: Tdlg_wmie_Login
  Left = 0
  Top = 0
  Caption = 'Connect to Namespace'
  ClientHeight = 190
  ClientWidth = 364
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    364
    190)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 40
    Top = 22
    Width = 43
    Height = 13
    Alignment = taRightJustify
    Caption = 'Machine:'
  end
  object Label2: TLabel
    Left = 57
    Top = 47
    Width = 26
    Height = 13
    Alignment = taRightJustify
    Caption = 'User:'
  end
  object Label3: TLabel
    Left = 33
    Top = 71
    Width = 50
    Height = 13
    Alignment = taRightJustify
    Caption = 'Password:'
  end
  object Label4: TLabel
    Left = 56
    Top = 102
    Width = 27
    Height = 13
    Alignment = taRightJustify
    Caption = 'Root:'
  end
  object Bevel1: TBevel
    Left = 11
    Top = 139
    Width = 341
    Height = 3
    Anchors = [akLeft, akRight, akBottom]
    Shape = bsTopLine
    ExplicitWidth = 280
  end
  object eMachine: TEdit
    Left = 89
    Top = 19
    Width = 261
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object eUser: TEdit
    Left = 89
    Top = 44
    Width = 261
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object ePwd: TEdit
    Left = 89
    Top = 68
    Width = 261
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    PasswordChar = '*'
    TabOrder = 2
  end
  object cbRoot: TComboBox
    Left = 89
    Top = 99
    Width = 261
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    DropDownCount = 20
    TabOrder = 3
    Text = 'root\CIMV2'
    Items.Strings = (
      'root\SECURITY'
      'root\SecurityCenter'
      'root\SecurityCenter2'
      'root\WMI'
      'root\WMI\ms_409'
      'root\CIMV2'
      'root\CIMV2\ms_409'
      'root\CIMV2\Applications'
      'root\CIMV2\Applications\MicrosoftACT'
      'root\CIMV2\Applications\MicrosoftIE'
      'root\Microsoft'
      'root\Microsoft\HomeNet'
      'root\DEFAULT'
      'root\DEFAULT\ms_409'
      'root\directory'
      'root\directory\LDAP'
      'root\directory\LDAP\ms_409'
      'root\subscription'
      'root\subscription\ms_409'
      'root\MSAPPS11')
  end
  object bOK: TButton
    Left = 196
    Top = 154
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object bCancel: TButton
    Left = 277
    Top = 154
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
end
