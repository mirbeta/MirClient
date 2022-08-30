object Form1: TForm1
  Left = 245
  Top = 214
  Caption = 'TEmbeddedWB - HTMLListener Demo'
  ClientHeight = 546
  ClientWidth = 731
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object EmbeddedWB1: TEmbeddedWB
    Left = 0
    Top = 65
    Width = 731
    Height = 481
    Align = alClient
    TabOrder = 0
    Silent = False
    OnBeforeNavigate2 = EmbeddedWB1BeforeNavigate2
    OnDocumentComplete = EmbeddedWB1DocumentComplete
    DisableCtrlShortcuts = 'N'
    UserInterfaceOptions = [EnablesFormsAutoComplete, EnableThemes]
    About = ' EmbeddedWB http://bsalsa.com/'
    PrintOptions.HTMLHeader.Strings = (
      '<HTML></HTML>')
    PrintOptions.Orientation = poPortrait
    ExplicitWidth = 739
    ExplicitHeight = 485
    ControlData = {
      4C0000003E4B00009B3400000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 731
    Height = 65
    Align = alTop
    TabOrder = 1
    DesignSize = (
      731
      65)
    object lblClickedOnElement: TLabel
      Left = 14
      Top = 8
      Width = 97
      Height = 13
      Caption = '[Clicked on Element]'
    end
    object lblElementUndertheMouse: TLabel
      Left = 14
      Top = 32
      Width = 127
      Height = 13
      Caption = '[Element under the Mouse]'
    end
    object btnEnableDisable: TButton
      Left = 595
      Top = 16
      Width = 115
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Disable Events'
      TabOrder = 0
      OnClick = btnEnableDisableClick
    end
  end
  object HtmlListener1: THtmlListener
    Handlers = <
      item
        EventID = eiOnClick
        OnHandle = HtmlListener1HandlersOnClickHandle
      end
      item
        EventID = eiOnSelect
      end
      item
        EventID = eiOnMouseMove
        OnHandle = HtmlListener1HandlersOnMouseMoveHandle
      end
      item
        EventID = eiUnknown
      end>
    SinkKind = skDocument
    Left = 528
    Top = 12
  end
end
