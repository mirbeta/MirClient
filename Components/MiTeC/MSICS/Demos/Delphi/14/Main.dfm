object wnd_cputest_Main: Twnd_cputest_Main
  Left = 336
  Top = 193
  Caption = 'CPU Test'
  ClientHeight = 524
  ClientWidth = 742
  Color = clBtnFace
  Font.Charset = EASTEUROPE_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 483
    Width = 742
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 1
    DesignSize = (
      742
      41)
    object Button1: TButton
      Left = 661
      Top = 8
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Close'
      TabOrder = 1
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 578
      Top = 8
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Anchors = [akTop, akRight]
      Caption = 'Run Test'
      Default = True
      TabOrder = 0
      OnClick = Button2Click
    end
    object cbxSMBIOS: TCheckBox
      Left = 4
      Top = 3
      Width = 97
      Height = 17
      Cursor = crHandPoint
      Caption = 'SMBIOS'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object cbxWMI: TCheckBox
      Left = 4
      Top = 20
      Width = 97
      Height = 17
      Cursor = crHandPoint
      Caption = 'WMI'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object cbxSave: TCheckBox
      Left = 107
      Top = 3
      Width = 97
      Height = 17
      Cursor = crHandPoint
      Caption = 'Save SIS file'
      TabOrder = 4
    end
    object cbxXML: TCheckBox
      Left = 107
      Top = 20
      Width = 97
      Height = 17
      Cursor = crHandPoint
      Caption = 'Save XML file'
      TabOrder = 5
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 742
    Height = 483
    Align = alClient
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 0
    object Box: TMemo
      Left = 0
      Top = 0
      Width = 742
      Height = 483
      Hint = 'Information Box'
      Align = alClient
      Color = clBlack
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clLime
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      Lines.Strings = (
        ''
        ''
        ''
        ''
        ''
        ''
        '')
      ParentFont = False
      ParentShowHint = False
      ReadOnly = True
      ScrollBars = ssBoth
      ShowHint = False
      TabOrder = 0
      WantReturns = False
    end
  end
end
