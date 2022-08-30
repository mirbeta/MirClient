object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Wi-Fi Known Networks'
  ClientHeight = 375
  ClientWidth = 894
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
    894
    375)
  PixelsPerInch = 96
  TextHeight = 13
  object List: TListView
    Left = 8
    Top = 8
    Width = 878
    Height = 322
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'SSID'
        Width = 150
      end
      item
        Caption = 'Key'
        Width = 150
      end
      item
        Caption = 'Authentication'
        Width = 90
      end
      item
        Caption = 'Encryption'
        Width = 70
      end
      item
        Caption = 'Connection'
        Width = 70
      end
      item
        Caption = 'Adapter name'
        Width = 100
      end
      item
        Caption = 'IP Address'
        Width = 100
      end
      item
        Caption = 'Timestamp'
        Width = 120
      end>
    ColumnClick = False
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = ListDblClick
  end
  object Button2: TButton
    Left = 811
    Top = 342
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 342
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Anchors = [akLeft, akBottom]
    Caption = 'Save...'
    TabOrder = 2
    OnClick = Button3Click
  end
  object sd: TSaveDialog
    DefaultExt = 'sif'
    Filter = 'System Information files|*.sif|All files|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 378
    Top = 182
  end
end
