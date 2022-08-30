object Form1: TForm1
  Left = 337
  Top = 265
  Caption = 'MSIC DLL Demo'
  ClientHeight = 584
  ClientWidth = 903
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 903
    Height = 52
    Align = alTop
    BevelEdges = []
    BevelOuter = bvNone
    BorderWidth = 10
    Color = clWhite
    TabOrder = 0
    DesignSize = (
      903
      52)
    object Icon: TImage
      Left = 9
      Top = 10
      Width = 32
      Height = 32
      AutoSize = True
      Center = True
    end
    object Label1: TLabel
      Left = 53
      Top = 15
      Width = 161
      Height = 19
      Caption = 'System Information'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Button1: TButton
      Left = 821
      Top = 15
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Save'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object WebBrowser: TWebBrowser
    Left = 0
    Top = 52
    Width = 903
    Height = 532
    Align = alClient
    TabOrder = 1
    ExplicitLeft = 340
    ExplicitTop = 154
    ExplicitWidth = 300
    ExplicitHeight = 150
    ControlData = {
      4C000000545D0000FC3600000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
end
