object wndMain: TwndMain
  Left = 0
  Top = 0
  ActiveControl = bOpen
  BorderStyle = bsDialog
  Caption = 'Certificate Information'
  ClientHeight = 314
  ClientWidth = 693
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    693
    314)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 31
    Top = 19
    Width = 20
    Height = 13
    Alignment = taRightJustify
    Caption = 'File:'
  end
  object Image: TImage
    Left = 19
    Top = 53
    Width = 32
    Height = 32
  end
  object eFile: TEdit
    Left = 60
    Top = 16
    Width = 529
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ParentColor = True
    ReadOnly = True
    TabOrder = 0
  end
  object bOpen: TButton
    Left = 595
    Top = 14
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Anchors = [akTop, akRight]
    Caption = 'Open...'
    TabOrder = 1
    OnClick = bOpenClick
  end
  object Memo: TMemo
    Left = 60
    Top = 53
    Width = 610
    Height = 215
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
  object bView: TButton
    Left = 548
    Top = 278
    Width = 122
    Height = 25
    Cursor = crHandPoint
    Anchors = [akTop, akRight]
    Caption = 'View certificate...'
    Enabled = False
    TabOrder = 4
    OnClick = bViewClick
  end
  object bProps: TButton
    Left = 420
    Top = 278
    Width = 122
    Height = 25
    Cursor = crHandPoint
    Anchors = [akTop, akRight]
    Caption = 'File properties...'
    Enabled = False
    TabOrder = 3
    OnClick = bPropsClick
  end
  object od: TOpenDialog
    Filter = 
      'Supported files|*.exe;*.dll;*.sys;*.ocx;*.msi;*.pem;*.crt;*.cer;' +
      '*.p7b;*.p7c;*.der;*.pfx;*.p12|Portable executables|*.exe;*.dll;*' +
      '.sys;*.ocx;*.msi;|Certificates|*.pem;*.crt;*.cer;*.p7b;*.p7c;*.d' +
      'er;*.pfx;*.p12|All files|*.*'
    Left = 305
    Top = 123
  end
end
