object dlg_db_Detail: Tdlg_db_Detail
  Left = 326
  Top = 330
  Caption = 'Device Properties'
  ClientHeight = 651
  ClientWidth = 673
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonPanel: TPanel
    Left = 0
    Top = 609
    Width = 673
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Panel: TPanel
      Left = 583
      Top = 0
      Width = 90
      Height = 42
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object bOK: TButton
        Left = 4
        Top = 6
        Width = 75
        Height = 25
        Cursor = crHandPoint
        Cancel = True
        Caption = 'OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
      end
    end
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 673
    Height = 609
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 10
    Caption = ' '
    TabOrder = 1
    DesignSize = (
      673
      609)
    object Icon: TImage
      Left = 10
      Top = 7
      Width = 32
      Height = 32
    end
    object eName: TEdit
      Left = 54
      Top = 14
      Width = 606
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = True
      ParentFont = False
      ReadOnly = True
      TabOrder = 0
    end
    object lv: TListView
      AlignWithMargins = True
      Left = 13
      Top = 50
      Width = 647
      Height = 546
      Margins.Top = 40
      Align = alClient
      Columns = <
        item
          Caption = 'Variable'
          Width = 150
        end
        item
          AutoSize = True
          Caption = 'Value'
        end>
      ColumnClick = False
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      ParentColor = True
      ParentShowHint = False
      ShowColumnHeaders = False
      ShowHint = True
      TabOrder = 1
      ViewStyle = vsReport
      OnAdvancedCustomDrawItem = lvAdvancedCustomDrawItem
      OnAdvancedCustomDrawSubItem = lvAdvancedCustomDrawSubItem
      OnDblClick = lvDblClick
      ExplicitHeight = 449
    end
  end
end
