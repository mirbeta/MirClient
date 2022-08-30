object frmWebBrowser: TfrmWebBrowser
  Left = 491
  Top = 166
  BorderStyle = bsNone
  Caption = 'BLUE MIR2'
  ClientHeight = 393
  ClientWidth = 632
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object Panel1: TPanel
    Left = 0
    Top = 371
    Width = 632
    Height = 22
    Align = alBottom
    BevelOuter = bvNone
    Color = clWindowText
    TabOrder = 0
    object sbtnWebBrowserClose: TSpeedButton
      Left = 528
      Top = 0
      Width = 104
      Height = 22
      Align = alRight
      Caption = #20851' '#38381
      Flat = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindow
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = sbtnWebBrowserCloseClick
    end
    object SpeedButton1: TSpeedButton
      Left = 364
      Top = 0
      Width = 82
      Height = 22
      Align = alRight
      Caption = #21518#36864
      Flat = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindow
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButton1Click
      ExplicitLeft = 368
    end
    object SpeedButton2: TSpeedButton
      Left = 446
      Top = 0
      Width = 82
      Height = 22
      Align = alRight
      Caption = #21069#36827
      Flat = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindow
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButton2Click
      ExplicitLeft = 456
    end
  end
  object WebBrowser: TWebBrowser
    Left = 0
    Top = 0
    Width = 632
    Height = 371
    Align = alClient
    TabOrder = 1
    OnNewWindow2 = WebBrowserNewWindow2
    OnWindowClosing = WebBrowserWindowClosing
    ExplicitHeight = 354
    ControlData = {
      4C00000052410000582600000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
end
