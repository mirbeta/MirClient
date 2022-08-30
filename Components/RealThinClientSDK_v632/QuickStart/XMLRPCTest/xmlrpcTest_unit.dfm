object Form1: TForm1
  Left = 230
  Top = 128
  Width = 841
  Height = 597
  Caption = 'JSON, RTC and XML-RPC parser/generator testing'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object Splitter1: TSplitter
    Left = 401
    Top = 0
    Width = 4
    Height = 527
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 401
    Height = 527
    Align = alLeft
    TabOrder = 0
    object Panel4: TPanel
      Left = 1
      Top = 1
      Width = 399
      Height = 92
      Align = alTop
      TabOrder = 1
      object Label1: TLabel
        Left = 45
        Top = 69
        Width = 314
        Height = 16
        Caption = 'Enter JSON, RTC, or XML-RPC string here for parsing'
        Layout = tlCenter
      end
      object btnRTC: TSpeedButton
        Left = 135
        Top = 29
        Width = 98
        Height = 30
        AllowAllUp = True
        Caption = 'from RTC ->'
        OnClick = btnRTCClick
      end
      object btnJSON: TSpeedButton
        Left = 27
        Top = 29
        Width = 102
        Height = 30
        AllowAllUp = True
        Caption = 'from JSON ->'
        OnClick = btnJSONClick
      end
      object btnXML: TSpeedButton
        Left = 242
        Top = 29
        Width = 123
        Height = 30
        AllowAllUp = True
        Caption = 'from XML-RPC ->'
        OnClick = btnXMLClick
      end
      object btnClr: TSpeedButton
        Left = 4
        Top = 64
        Width = 31
        Height = 25
        Caption = 'C'
        OnClick = btnClrClick
      end
      object btnOTF: TCheckBox
        Left = 48
        Top = 8
        Width = 301
        Height = 17
        Caption = 'On-the-fly input parsing and output generation'
        TabOrder = 0
        OnClick = btnOTFClick
      end
    end
    object eXMLSrc: TMemo
      Left = 1
      Top = 93
      Width = 399
      Height = 433
      Align = alClient
      ScrollBars = ssBoth
      TabOrder = 0
      OnChange = eXMLSrcChange
    end
  end
  object Panel1: TPanel
    Left = 405
    Top = 0
    Width = 418
    Height = 527
    Align = alClient
    TabOrder = 1
    object Panel5: TPanel
      Left = 1
      Top = 1
      Width = 416
      Height = 32
      Align = alTop
      Caption = 'New JSON, RTC, and XML-RPC output will be generated here'
      TabOrder = 0
    end
    object eXMLDest: TMemo
      Left = 1
      Top = 33
      Width = 416
      Height = 493
      Align = alClient
      ScrollBars = ssBoth
      TabOrder = 1
    end
  end
  object MainMenu1: TMainMenu
    Left = 128
    Top = 208
    object JSON1: TMenuItem
      Caption = 'JSON Parser'
      object mParseDate: TMenuItem
        AutoCheck = True
        Caption = '"\/Date(...)\/" -> rtc_DateTime'
        OnClick = mParseDateClick
      end
      object mParseError: TMenuItem
        AutoCheck = True
        Caption = '"\/error\/..." -> rtc_Exception'
        OnClick = mParseErrorClick
      end
      object mParseBase64: TMenuItem
        AutoCheck = True
        Caption = '"\/base64\/..." -> rtc_ByteStream'
        OnClick = mParseBase64Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object mParseMethod: TMenuItem
        AutoCheck = True
        Caption = '{"\/method"...} -> rtc_Function'
        OnClick = mParseMethodClick
      end
      object mParseDataSet: TMenuItem
        AutoCheck = True
        Caption = '{"\/dsfields"...} -> rtc_DataSet'
        OnClick = mParseDataSetClick
      end
    end
    object N1: TMenuItem
      Caption = '|'
      Enabled = False
    end
    object JSONGenerator1: TMenuItem
      Caption = 'JSON Generator'
      object mGenDateTime: TMenuItem
        AutoCheck = True
        Caption = 'rtc_DateTime -> "\/Date(...)\/"'
        OnClick = mGenDateTimeClick
      end
      object mGenError: TMenuItem
        AutoCheck = True
        Caption = 'rtc_Exception -> "\/error\/..."'
        OnClick = mGenErrorClick
      end
      object mGenBase64: TMenuItem
        AutoCheck = True
        Caption = 'rtc_ByteStream -> "\/base64\/..."'
        OnClick = mGenBase64Click
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mGenMethod: TMenuItem
        AutoCheck = True
        Caption = 'rtc_Function -> {"\/method"...}'
        OnClick = mGenMethodClick
      end
      object mGenSlash: TMenuItem
        AutoCheck = True
        Caption = '"/" -> "\/"'
        Checked = True
        OnClick = mGenSlashClick
      end
    end
  end
end
