object wndMain: TwndMain
  Left = 0
  Top = 0
  Caption = 'Process Monitor'
  ClientHeight = 603
  ClientWidth = 1008
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
    Top = 584
    Width = 1008
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
  object InfoPanel: TPanel
    AlignWithMargins = True
    Left = 454
    Top = 3
    Width = 551
    Height = 578
    Margins.Left = 0
    Align = alClient
    BevelKind = bkFlat
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 1
    Visible = False
    DesignSize = (
      547
      574)
    object lPri: TLabel
      Left = 9
      Top = 106
      Width = 38
      Height = 13
      Caption = 'Priority:'
    end
    object lCT: TLabel
      Left = 9
      Top = 61
      Width = 43
      Height = 13
      Caption = 'Created:'
    end
    object lMem: TLabel
      Left = 9
      Top = 130
      Width = 42
      Height = 13
      Caption = 'Memory:'
    end
    object lCPU: TLabel
      Left = 9
      Top = 84
      Width = 57
      Height = 13
      Caption = 'CPU Usage:'
    end
    object lIORead: TLabel
      Left = 9
      Top = 153
      Width = 70
      Height = 13
      Caption = 'IO Read Rate:'
    end
    object lIOWrite: TLabel
      Left = 9
      Top = 177
      Width = 71
      Height = 13
      Caption = 'IO Write Rate:'
    end
    object lPub: TLabel
      Left = 9
      Top = 36
      Width = 47
      Height = 13
      Caption = 'Publisher:'
      OnClick = lPubClick
    end
    object lSigner: TLabel
      Left = 62
      Top = 36
      Width = 3
      Height = 13
      OnClick = lPubClick
    end
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 547
      Height = 23
      Align = alTop
      BevelEdges = [beBottom]
      BevelKind = bkFlat
      BevelOuter = bvNone
      Color = 16118770
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 0
      object lTitle: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 541
        Height = 15
        Align = alClient
        AutoSize = False
        Caption = '<no process selected>'
        EllipsisPosition = epPathEllipsis
        Layout = tlCenter
        ExplicitWidth = 111
        ExplicitHeight = 13
      end
    end
    object CPUPanel: TPanel
      Left = 489
      Top = 34
      Width = 50
      Height = 75
      Anchors = [akTop, akRight]
      BevelKind = bkTile
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 1
      object CPUGauge: TGauge
        Left = 0
        Top = 0
        Width = 46
        Height = 59
        Align = alClient
        BackColor = clBlack
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
        ShowText = False
        ExplicitLeft = -1
        ExplicitTop = 12
        ExplicitWidth = 36
        ExplicitHeight = 38
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
        Caption = 'CPU'
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
    object pc: TPageControl
      Left = 9
      Top = 203
      Width = 534
      Height = 364
      Cursor = crHandPoint
      ActivePage = tsHandles
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 2
      object tsThreads: TTabSheet
        Caption = 'Threads'
        object ThreadList: TListView
          Left = 0
          Top = 0
          Width = 526
          Height = 336
          Align = alClient
          BevelInner = bvLowered
          BevelOuter = bvRaised
          BevelKind = bkFlat
          BorderStyle = bsNone
          Columns = <
            item
              Caption = 'Thread'
            end
            item
              Caption = 'State'
              Width = 200
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
            end>
          ColumnClick = False
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          TabOrder = 0
          ViewStyle = vsReport
        end
      end
      object tsModules: TTabSheet
        Caption = 'Modules'
        ImageIndex = 1
        object ModList: TListView
          Left = 0
          Top = 0
          Width = 526
          Height = 336
          Align = alClient
          BevelInner = bvLowered
          BevelOuter = bvRaised
          BevelKind = bkFlat
          BorderStyle = bsNone
          Columns = <
            item
              Caption = 'Name'
              Width = 200
            end
            item
              Alignment = taRightJustify
              Caption = 'Base address'
              Width = 100
            end
            item
              Alignment = taRightJustify
              Caption = 'Size'
              Width = 100
            end
            item
              Caption = 'Description'
              Width = 200
            end>
          ColumnClick = False
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          TabOrder = 0
          ViewStyle = vsReport
        end
      end
      object tsHandles: TTabSheet
        Caption = 'Handles'
        ImageIndex = 2
        object HandleList: TListView
          Left = 0
          Top = 0
          Width = 526
          Height = 336
          Align = alClient
          BevelInner = bvLowered
          BevelOuter = bvRaised
          BevelKind = bkFlat
          BorderStyle = bsNone
          Columns = <
            item
              Caption = 'Handle'
              Width = 100
            end
            item
              Caption = 'Type'
              Width = 100
            end
            item
              Caption = 'Name'
              Width = 300
            end>
          ColumnClick = False
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          TabOrder = 0
          ViewStyle = vsReport
        end
      end
    end
  end
  object ListPanel: TPanel
    Left = 0
    Top = 0
    Width = 454
    Height = 584
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 2
    object List: TListView
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 448
      Height = 554
      Align = alClient
      BevelInner = bvLowered
      BevelOuter = bvRaised
      BevelKind = bkFlat
      BorderStyle = bsNone
      Columns = <
        item
          Caption = 'PID'
        end
        item
          Alignment = taRightJustify
          Caption = 'Session'
        end
        item
          Alignment = taRightJustify
          Caption = 'Bits'
          Width = 40
        end
        item
          Alignment = taRightJustify
          Caption = 'CPU'
        end
        item
          Alignment = taRightJustify
          Caption = 'Memory'
          Width = 75
        end
        item
          Caption = 'Name'
          Width = 175
        end
        item
          Caption = 'Username'
          Width = 200
        end>
      ColumnClick = False
      DoubleBuffered = True
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      ParentDoubleBuffered = False
      TabOrder = 0
      ViewStyle = vsReport
      OnSelectItem = ListSelectItem
    end
    object eSearch: TEdit
      AlignWithMargins = True
      Left = 3
      Top = 560
      Width = 448
      Height = 21
      Hint = 'Find process by name'
      Margins.Top = 0
      Align = alBottom
      BevelKind = bkFlat
      BorderStyle = bsNone
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnKeyUp = eSearchKeyUp
    end
  end
end
