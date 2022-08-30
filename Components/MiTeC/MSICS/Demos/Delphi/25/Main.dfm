object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'USB History'
  ClientHeight = 500
  ClientWidth = 602
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
    602
    500)
  PixelsPerInch = 96
  TextHeight = 13
  object lv: TListView
    Left = 3
    Top = 3
    Width = 602
    Height = 467
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Device'
        Width = 200
      end
      item
        Caption = 'Serial number'
        Width = 150
      end
      item
        Caption = 'Last seen'
        Width = 120
      end
      item
        Caption = 'Class'
        Width = 100
      end>
    ColumnClick = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnCompare = lvCompare
  end
  object Button3: TButton
    Left = 3
    Top = 470
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Anchors = [akLeft, akBottom]
    Caption = 'Save...'
    TabOrder = 1
    OnClick = Button3Click
  end
  object Button2: TButton
    Left = 522
    Top = 470
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    TabOrder = 2
    OnClick = Button2Click
  end
  object sd: TSaveDialog
    DefaultExt = 'sif'
    Filter = 'System Information files|*.sif|All files|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 378
    Top = 182
  end
end
