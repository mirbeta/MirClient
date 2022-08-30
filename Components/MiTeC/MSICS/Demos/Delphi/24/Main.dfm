object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Network Credentials'
  ClientHeight = 426
  ClientWidth = 640
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    640
    426)
  PixelsPerInch = 96
  TextHeight = 13
  object List: TListView
    Left = 8
    Top = 13
    Width = 624
    Height = 369
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Type'
        Width = 100
      end
      item
        Caption = 'Timestamp'
        Width = 120
      end
      item
        Caption = 'Target'
        Width = 100
      end
      item
        Caption = 'Username'
        Width = 150
      end
      item
        Caption = 'Password'
        Width = 150
      end>
    ColumnClick = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
  end
  object Button1: TButton
    Left = 8
    Top = 393
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Anchors = [akLeft, akBottom]
    Caption = 'Save...'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 557
    Top = 393
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
    Left = 377
    Top = 182
  end
end
