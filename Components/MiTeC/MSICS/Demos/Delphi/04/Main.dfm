object Form1: TForm1
  Left = 401
  Top = 349
  Caption = 'USB Devices'
  ClientHeight = 644
  ClientWidth = 829
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 385
    Top = 0
    Height = 596
    ResizeStyle = rsUpdate
    ExplicitLeft = 313
    ExplicitTop = 10
    ExplicitHeight = 517
  end
  object Tree: TTreeView
    AlignWithMargins = True
    Left = 10
    Top = 10
    Width = 375
    Height = 576
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 0
    Margins.Bottom = 10
    Align = alLeft
    HideSelection = False
    Images = ilSystem
    Indent = 19
    ParentShowHint = False
    ReadOnly = True
    ShowHint = True
    TabOrder = 0
    OnChange = TreeChange
    OnCustomDrawItem = TreeCustomDrawItem
    OnDeletion = TreeDeletion
  end
  object Panel1: TPanel
    Left = 0
    Top = 596
    Width = 829
    Height = 48
    Align = alBottom
    BevelEdges = [beTop]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      829
      46)
    object bRefresh: TButton
      Left = 10
      Top = 12
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Caption = 'Refresh'
      TabOrder = 0
      OnClick = bRefreshClick
    end
    object bSave: TButton
      Left = 97
      Top = 12
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Caption = 'Save...'
      TabOrder = 1
      OnClick = cmSave
    end
    object bRemove: TButton
      Left = 195
      Top = 12
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Caption = 'Eject'
      Enabled = False
      TabOrder = 2
      OnClick = cmRemove
    end
    object cbxAuto: TCheckBox
      Left = 335
      Top = 16
      Width = 97
      Height = 17
      Cursor = crHandPoint
      Caption = 'Auto Refresh'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object bClose: TButton
      Left = 743
      Top = 12
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Close'
      Default = True
      TabOrder = 4
      OnClick = bCloseClick
    end
  end
  object List: TListView
    AlignWithMargins = True
    Left = 388
    Top = 10
    Width = 431
    Height = 576
    Margins.Left = 0
    Margins.Top = 10
    Margins.Right = 10
    Margins.Bottom = 10
    Align = alClient
    Columns = <
      item
        Caption = 'Property'
        Width = 150
      end
      item
        Caption = 'Value'
        Width = 250
      end>
    ColumnClick = False
    ReadOnly = True
    RowSelect = True
    ParentColor = True
    TabOrder = 2
    ViewStyle = vsReport
  end
  object sd: TSaveDialog
    Filter = 'MiTeC System Information Storage files|*.sis|All files|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 278
    Top = 204
  end
  object USB: TMiTeC_USB
    Left = 113
    Top = 103
  end
  object DeviceMonitor: TMiTeC_DeviceMonitor
    Active = True
    CatchBluetooth = False
    OnDeviceConnect = DeviceMonitorDeviceConnect
    OnDeviceDisconect = DeviceMonitorDeviceConnect
    OnVolumeConnect = DeviceMonitorVolumeConnect
    OnVolumeDisconnect = DeviceMonitorVolumeConnect
    Left = 174
    Top = 148
  end
  object ilSystem: TImageList
    ColorDepth = cd32Bit
    Left = 243
    Top = 423
  end
end
