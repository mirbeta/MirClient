object wndMain: TwndMain
  Left = 0
  Top = 0
  Caption = 'Process Monitor and CPU Usage Test'
  ClientHeight = 406
  ClientWidth = 885
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
    885
    406)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 8
    Top = 40
    Width = 357
    Height = 255
    Anchors = [akLeft, akTop, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
  object bStart: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Caption = 'Start'
    TabOrder = 1
    OnClick = bStartClick
  end
  object sb: TStatusBar
    Left = 0
    Top = 387
    Width = 885
    Height = 19
    Panels = <
      item
        Alignment = taCenter
        Width = 100
      end
      item
        Alignment = taCenter
        Width = 100
      end
      item
        Alignment = taCenter
        Width = 100
      end
      item
        Alignment = taCenter
        Width = 120
      end
      item
        Alignment = taCenter
        Width = 100
      end
      item
        Width = 100
      end>
  end
  object List: TListView
    AlignWithMargins = True
    Left = 374
    Top = 40
    Width = 497
    Height = 336
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelInner = bvLowered
    BevelOuter = bvRaised
    BevelKind = bkFlat
    BorderStyle = bsNone
    Columns = <
      item
        Caption = 'Thread'
      end
      item
        Caption = 'Created'
        Width = 120
      end
      item
        Caption = 'State'
        Width = 150
      end
      item
        Alignment = taRightJustify
        Caption = 'CPU'
      end
      item
        Alignment = taCenter
        Caption = 'Priority'
      end
      item
        Caption = 'Start address'
        Width = 150
      end
      item
        Caption = 'Debug info'
        Width = 500
      end>
    ColumnClick = False
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 3
    ViewStyle = vsReport
  end
  object CPUPanel: TPanel
    Left = 8
    Top = 301
    Width = 50
    Height = 75
    Anchors = [akLeft, akBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 4
    object CPUGauge: TGauge
      Left = 0
      Top = 0
      Width = 46
      Height = 59
      Align = alClient
      BorderStyle = bsNone
      ForeColor = clLime
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Kind = gkVerticalBar
      ParentFont = False
      Progress = 0
      ExplicitLeft = -12
      ExplicitTop = -6
    end
    object CPUPanelTitle: TPanel
      Left = 0
      Top = 59
      Width = 46
      Height = 12
      Align = alBottom
      BevelEdges = [beTop]
      BevelKind = bkTile
      BevelOuter = bvNone
      Caption = 'CPU Usage'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -8
      Font.Name = 'Small Fonts'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 0
    end
  end
  object Panel3: TPanel
    Left = 64
    Top = 301
    Width = 50
    Height = 75
    Anchors = [akLeft, akBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 5
    object MemGauge: TGauge
      Left = 0
      Top = 0
      Width = 46
      Height = 59
      Align = alClient
      BorderStyle = bsNone
      ForeColor = clAqua
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Kind = gkVerticalBar
      ParentFont = False
      Progress = 0
      ExplicitLeft = -1
      ExplicitTop = 12
      ExplicitWidth = 36
      ExplicitHeight = 38
    end
    object Panel4: TPanel
      Left = 0
      Top = 59
      Width = 46
      Height = 12
      Align = alBottom
      BevelEdges = [beTop]
      BevelKind = bkTile
      BevelOuter = bvNone
      Caption = 'Mem Load'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -8
      Font.Name = 'Small Fonts'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 0
    end
  end
  object CoreList: TListView
    AlignWithMargins = True
    Left = 120
    Top = 302
    Width = 245
    Height = 74
    Anchors = [akLeft, akBottom]
    BevelInner = bvLowered
    BevelOuter = bvRaised
    BevelKind = bkFlat
    BorderStyle = bsNone
    Columns = <
      item
        Caption = 'Core'
      end
      item
        Alignment = taRightJustify
        Caption = 'Max Freq'
        Width = 80
      end
      item
        Alignment = taRightJustify
        Caption = 'Current Freq'
        Width = 80
      end>
    ColumnClick = False
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 6
    ViewStyle = vsReport
  end
end
