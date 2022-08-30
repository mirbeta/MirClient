object wndMain: TwndMain
  Left = 0
  Top = 0
  Caption = 'DriveContent'
  ClientHeight = 280
  ClientWidth = 606
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
  object cbDrive: TComboBox
    Left = 8
    Top = 8
    Width = 93
    Height = 21
    Cursor = crHandPoint
    Style = csDropDownList
    TabOrder = 0
    OnChange = cbDriveChange
  end
  object bScan: TButton
    Left = 107
    Top = 6
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Caption = 'Scan'
    Enabled = False
    TabOrder = 1
    OnClick = bScanClick
  end
  object sb: TStatusBar
    Left = 0
    Top = 261
    Width = 606
    Height = 19
    Panels = <
      item
        Alignment = taCenter
        Width = 100
      end
      item
        Width = 50
      end>
    ExplicitLeft = 204
    ExplicitTop = 257
    ExplicitWidth = 0
  end
end
