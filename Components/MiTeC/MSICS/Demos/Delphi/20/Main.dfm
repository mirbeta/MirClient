object wndMain: TwndMain
  Left = 0
  Top = 0
  Caption = 'Network Connections'
  ClientHeight = 534
  ClientWidth = 933
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
  object sb: TStatusBar
    Left = 0
    Top = 515
    Width = 933
    Height = 19
    Panels = <
      item
        Alignment = taCenter
        Width = 100
      end
      item
        Width = 500
      end
      item
        Width = 50
      end>
  end
  object List: TListView
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 927
    Height = 509
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvRaised
    BevelKind = bkFlat
    BorderStyle = bsNone
    Columns = <
      item
        Caption = 'Process'
        Width = 200
      end
      item
        Caption = 'Local address'
        Width = 200
      end
      item
        Alignment = taRightJustify
        Caption = 'Local port'
        Width = 75
      end
      item
        Caption = 'Remote address'
        Width = 200
      end
      item
        Alignment = taRightJustify
        Caption = 'Remote port'
        Width = 75
      end
      item
        Caption = 'Protocol'
      end
      item
        Caption = 'Status'
        Width = 100
      end>
    ColumnClick = False
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
  end
end
