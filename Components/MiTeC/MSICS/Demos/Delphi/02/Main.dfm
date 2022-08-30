object wndMain: TwndMain
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'StreamCodeProcedure demo'
  ClientHeight = 252
  ClientWidth = 645
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
    645
    252)
  PixelsPerInch = 96
  TextHeight = 13
  object lRes: TLabel
    Left = 22
    Top = 223
    Width = 3
    Height = 13
  end
  object lTS: TLabel
    Left = 618
    Top = 73
    Width = 3
    Height = 13
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
  end
  object Label1: TLabel
    Left = 339
    Top = 223
    Width = 282
    Height = 13
    Cursor = crHandPoint
    Caption = 'Use MiTeC Structure Storage Viewer to browse created file'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = Label1Click
  end
  object bSave: TButton
    Left = 22
    Top = 61
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Caption = 'Save'
    TabOrder = 2
    OnClick = bSaveClick
  end
  object cbxZLIB: TCheckBox
    Left = 22
    Top = 37
    Width = 84
    Height = 17
    Caption = 'Compressed'
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
  object bLoad: TButton
    Left = 103
    Top = 61
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Caption = 'Load'
    Enabled = False
    TabOrder = 3
    OnClick = bLoadClick
  end
  object List: TListView
    Left = 22
    Top = 95
    Width = 599
    Height = 114
    Columns = <
      item
        Width = 200
      end
      item
        Width = 350
      end>
    ColumnClick = False
    GridLines = True
    ReadOnly = True
    RowSelect = True
    ShowColumnHeaders = False
    TabOrder = 4
    ViewStyle = vsReport
  end
  object cbxCrypt: TCheckBox
    Left = 22
    Top = 14
    Width = 82
    Height = 17
    Caption = 'Encrypted'
    Checked = True
    State = cbChecked
    TabOrder = 0
  end
  object SI: TMiTeC_SystemInfo
    OnWriteHeader = SIWriteHeader
    OnReadHeader = SIReadHeader
    Left = 247
    Top = 46
  end
end
