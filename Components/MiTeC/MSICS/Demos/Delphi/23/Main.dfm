object wndMain: TwndMain
  Left = 0
  Top = 0
  Caption = 'Task Scheduler'
  ClientHeight = 404
  ClientWidth = 740
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
  object List: TListView
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 734
    Height = 398
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvRaised
    BevelKind = bkFlat
    BorderStyle = bsNone
    Columns = <
      item
        Caption = 'Name'
        Width = 300
      end
      item
        Caption = 'CommandLine'
        Width = 300
      end
      item
        Caption = 'Author'
        Width = 100
      end>
    ColumnClick = False
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
  end
end
