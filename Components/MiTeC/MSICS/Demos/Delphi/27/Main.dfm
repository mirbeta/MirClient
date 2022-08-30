object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Windows Firewall'
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
    Top = 47
    Width = 878
    Height = 283
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Rule'
        Width = 150
      end
      item
        Caption = 'Description'
        Width = 150
      end
      item
        Caption = 'Application Name'
        Width = 120
      end
      item
        Caption = 'Service Name'
        Width = 120
      end
      item
        Caption = 'Protocol'
      end
      item
        Caption = 'Local Ports'
        Width = 100
      end
      item
        Caption = 'Remote Ports'
        Width = 100
      end
      item
        Caption = 'Local Addresses'
        Width = 120
      end
      item
        Caption = 'Remote Addresses'
        Width = 100
      end
      item
        Caption = 'ICMP types and codes'
        Width = 100
      end
      item
        Caption = 'Direction'
      end
      item
        Caption = 'Enabled'
      end
      item
        Caption = 'Edge'
      end
      item
        Caption = 'Action'
        Width = 150
      end
      item
        Caption = 'Grouping'
        Width = 100
      end
      item
        Caption = 'Interface types'
        Width = 100
      end>
    ColumnClick = False
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
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
  object cbxDomain: TCheckBox
    Left = 8
    Top = 15
    Width = 97
    Height = 17
    Caption = 'Domain Profile'
    Enabled = False
    TabOrder = 3
  end
  object cbxPublic: TCheckBox
    Left = 140
    Top = 15
    Width = 97
    Height = 17
    Caption = 'Public Profile'
    Enabled = False
    TabOrder = 4
  end
  object cbxPrivate: TCheckBox
    Left = 278
    Top = 15
    Width = 97
    Height = 17
    Caption = 'Private Profile'
    Enabled = False
    TabOrder = 5
  end
  object sd: TSaveDialog
    DefaultExt = 'sif'
    Filter = 'System Information files|*.sif|All files|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 378
    Top = 182
  end
end
