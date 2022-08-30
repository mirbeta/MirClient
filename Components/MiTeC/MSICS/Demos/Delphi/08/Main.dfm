object wnd_dm_Main: Twnd_dm_Main
  Left = 0
  Top = 0
  Anchors = [akLeft, akBottom]
  Caption = 'Device Monitor'
  ClientHeight = 644
  ClientWidth = 892
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
    892
    644)
  PixelsPerInch = 96
  TextHeight = 13
  object Box: TListBox
    Left = 11
    Top = 8
    Width = 870
    Height = 494
    Style = lbOwnerDrawVariable
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 17
    TabOrder = 0
    OnClick = BoxClick
    OnDblClick = BoxDblClick
    OnDrawItem = BoxDrawItem
  end
  object Button1: TButton
    Left = 806
    Top = 608
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    Default = True
    TabOrder = 5
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 11
    Top = 608
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Anchors = [akLeft, akBottom]
    Caption = 'Save...'
    TabOrder = 1
    OnClick = Button2Click
  end
  object cbxAll: TCheckBox
    Left = 370
    Top = 612
    Width = 130
    Height = 17
    Cursor = crHandPoint
    Anchors = [akLeft, akBottom]
    Caption = 'Capture all messages'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object Button3: TButton
    Left = 92
    Top = 608
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Anchors = [akLeft, akBottom]
    Caption = 'Clear'
    TabOrder = 2
    OnClick = Button3Click
  end
  object cbxACtive: TCheckBox
    Left = 194
    Top = 612
    Width = 55
    Height = 17
    Cursor = crHandPoint
    Anchors = [akLeft, akBottom]
    Caption = 'Active'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = cbxACtiveClick
  end
  object Memo: TMemo
    Left = 11
    Top = 508
    Width = 870
    Height = 89
    Anchors = [akLeft, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 6
  end
  object DeviceMonitor: TMiTeC_DeviceMonitor
    Active = True
    CatchBluetooth = False
    OnMessage = DeviceMonitorMessage
    OnDeviceConnect = DeviceMonitorDeviceConnect
    OnDeviceDisconect = DeviceMonitorDeviceDisconect
    OnVolumeConnect = DeviceMonitorVolumeConnect
    OnVolumeDisconnect = DeviceMonitorVolumeDisconnect
    Left = 174
    Top = 148
  end
  object sd: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 241
    Top = 146
  end
  object ilSystem: TImageList
    ColorDepth = cd32Bit
    Left = 161
    Top = 79
  end
end
