object Form1: TForm1
  Left = 262
  Top = 320
  Caption = 'TEmbeddedWB & MSHTMLEvents'
  ClientHeight = 558
  ClientWidth = 815
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object EmbeddedWB1: TEmbeddedWB
    Left = 0
    Top = 105
    Width = 815
    Height = 453
    Align = alClient
    TabOrder = 0
    Silent = False
    OnDownloadComplete = EmbeddedWB1DownloadComplete
    OnBeforeNavigate2 = EmbeddedWB1BeforeNavigate2
    DisableCtrlShortcuts = 'N'
    UserInterfaceOptions = [EnablesFormsAutoComplete, EnableThemes]
    About = ' EmbeddedWB http://bsalsa.com/'
    HTMLCode.Strings = (
      'http://ccms/SSOWeb/UserLogin.aspx?ServiceId=1')
    PrintOptions.HTMLHeader.Strings = (
      '<HTML></HTML>')
    PrintOptions.Orientation = poPortrait
    ExplicitWidth = 823
    ExplicitHeight = 457
    ControlData = {
      4C00000017590000EA3700000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 815
    Height = 105
    Align = alTop
    TabOrder = 1
    DesignSize = (
      815
      105)
    object Label1: TLabel
      Left = 19
      Top = 14
      Width = 95
      Height = 13
      Caption = '> Click on any URL!'
      Transparent = True
    end
    object Memo1: TMemo
      Left = 184
      Top = 8
      Width = 624
      Height = 89
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
  end
end
