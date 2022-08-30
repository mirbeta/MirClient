object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Software detection'
  ClientHeight = 558
  ClientWidth = 763
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
  object Panel1: TPanel
    Left = 0
    Top = 517
    Width = 763
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 0
    DesignSize = (
      763
      41)
    object bSave: TButton
      Left = 3
      Top = 8
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Caption = 'Save...'
      TabOrder = 0
      OnClick = bSaveClick
    end
    object bLoad: TButton
      Left = 84
      Top = 8
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Caption = 'Load...'
      TabOrder = 1
      OnClick = bLoadClick
    end
    object Button1: TButton
      Left = 684
      Top = 9
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Anchors = [akTop, akRight]
      Caption = 'Refresh'
      TabOrder = 2
      OnClick = Button1Click
    end
  end
  object pc: TPageControl
    Left = 0
    Top = 0
    Width = 763
    Height = 517
    Cursor = crHandPoint
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Installed software'
      object lv: TListView
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 749
        Height = 483
        Align = alClient
        Columns = <
          item
            Caption = 'Name'
            Width = 250
          end
          item
            Caption = 'Version'
            Width = 100
          end
          item
            Caption = 'Company'
            Width = 100
          end
          item
            Caption = 'Install Date'
            Width = 120
          end
          item
            Caption = 'Uninstall'
            Width = 300
          end
          item
            Caption = 'Help Link'
            Width = 100
          end
          item
            Caption = 'About Link'
            Width = 100
          end
          item
            Caption = 'Info Link'
            Width = 100
          end
          item
            Caption = 'Update Link'
            Width = 100
          end>
        ColumnClick = False
        ReadOnly = True
        RowSelect = True
        SortType = stText
        TabOrder = 0
        ViewStyle = vsReport
        ExplicitLeft = 186
        ExplicitTop = 13
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'MS Products'
      ImageIndex = 1
      object lvMSP: TListView
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 749
        Height = 483
        Align = alClient
        Columns = <
          item
            Caption = 'Product Name'
            Width = 250
          end
          item
            Caption = 'Product ID'
            Width = 200
          end
          item
            Caption = 'Product Key'
            Width = 200
          end
          item
            Caption = 'Registry'
            Width = 500
          end>
        ColumnClick = False
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        SortType = stText
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
  end
  object od: TOpenDialog
    DefaultExt = 'sis'
    Filter = 'SIS files|*.sis|All files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 307
    Top = 201
  end
  object sd: TSaveDialog
    DefaultExt = 'sis'
    Filter = 'SIS files|*.sis|All files|*.*'
    InitialDir = 'sd'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 402
    Top = 196
  end
end
