object wnd_msicstest_Main: Twnd_msicstest_Main
  Left = 295
  Top = 285
  Caption = 'MSICS Test'
  ClientHeight = 608
  ClientWidth = 630
  Color = clBtnFace
  Font.Charset = EASTEUROPE_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    630
    608)
  PixelsPerInch = 96
  TextHeight = 13
  object bClose: TButton
    Left = 541
    Top = 575
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    TabOrder = 8
    OnClick = bCloseClick
  end
  object Box: TMemo
    Left = 14
    Top = 286
    Width = 602
    Height = 281
    Hint = 'Information Box'
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ReadOnly = True
    ScrollBars = ssBoth
    ShowHint = True
    TabOrder = 4
  end
  object bRun: TButton
    Left = 13
    Top = 575
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Anchors = [akLeft, akBottom]
    Caption = 'Run Test'
    TabOrder = 5
    OnClick = bRunClick
  end
  object cbxReport: TCheckBox
    Left = 13
    Top = 250
    Width = 432
    Height = 17
    Cursor = crHandPoint
    Caption = 'Save data to System Information File'
    TabOrder = 3
  end
  object GroupBox1: TGroupBox
    Left = 13
    Top = 1
    Width = 601
    Height = 154
    Anchors = [akLeft, akTop, akRight]
    Caption = '[Tested objects]'
    TabOrder = 0
    object clb: TCheckListBox
      Left = 2
      Top = 15
      Width = 597
      Height = 137
      Cursor = crHandPoint
      Align = alClient
      Anchors = [akLeft, akTop, akBottom]
      BorderStyle = bsNone
      Color = clBtnFace
      Columns = 4
      ItemHeight = 13
      Items.Strings = (
        'ActiveDirectory'
        'APM'
        'ASPI32'
        'BIOS'
        'Bluetooth'
        'CPU'
        'Devices'
        'DirectX'
        'Disk'
        'Display'
        'Engines'
        'EventLog'
        'Firewall'
        'Internet'
        'LocaleInfo'
        'Machine'
        'Media'
        'Memory'
        'Monitor'
        'MSProduct'
        'Network'
        'Network Credentials'
        'OS'
        'Printers'
        'ProcessList'
        'Security'
        'SMBIOS'
        'Software'
        'Startup'
        'Storage'
        'TCPIP'
        'TimeZone'
        'USB'
        'USBHistory'
        'WIFI'
        'Winsock'
        'WLANC')
      PopupMenu = PopupMenu1
      Sorted = True
      TabOrder = 0
    end
  end
  object GroupBox2: TGroupBox
    Left = 13
    Top = 191
    Width = 601
    Height = 46
    Anchors = [akLeft, akTop, akRight]
    Caption = '[ProcessList detection range]'
    TabOrder = 2
    object clbPL: TCheckListBox
      Left = 20
      Top = 18
      Width = 538
      Height = 13
      Cursor = crHandPoint
      BorderStyle = bsNone
      Color = clBtnFace
      Columns = 5
      IntegralHeight = True
      ItemHeight = 13
      Items.Strings = (
        'Processes'
        'Services')
      TabOrder = 0
    end
  end
  object cbxSI: TCheckBox
    Left = 13
    Top = 164
    Width = 163
    Height = 17
    Cursor = crHandPoint
    Caption = 'Test SystemInfo component'
    TabOrder = 1
  end
  object bShutdown: TButton
    Left = 223
    Top = 575
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Anchors = [akLeft, akBottom]
    Caption = 'Power Off'
    TabOrder = 6
    OnClick = bShutdownClick
  end
  object bRunAsAdmin: TButton
    Left = 307
    Top = 575
    Width = 126
    Height = 25
    Cursor = crHandPoint
    Anchors = [akLeft, akBottom]
    Caption = 'Elevate privileges'
    ElevationRequired = True
    TabOrder = 7
    OnClick = bRunAsAdminClick
  end
  object PopupMenu1: TPopupMenu
    Left = 516
    Top = 57
    object Checkall1: TMenuItem
      Tag = 1
      Caption = 'Check all'
      OnClick = Checkall1Click
    end
    object Uncheckall1: TMenuItem
      Caption = 'Uncheck all'
      OnClick = Checkall1Click
    end
    object Inverse1: TMenuItem
      Caption = 'Inverse'
      OnClick = Inverse1Click
    end
  end
end
