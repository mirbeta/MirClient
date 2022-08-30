object wndMain: TwndMain
  Left = 0
  Top = 0
  Caption = 'Handles'
  ClientHeight = 465
  ClientWidth = 833
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
  object Label1: TLabel
    Left = 0
    Top = 124
    Width = 833
    Height = 300
    Align = alClient
    Alignment = taCenter
    Caption = 'Refreshing...please wait'
    Layout = tlCenter
    ExplicitTop = 0
    ExplicitWidth = 118
    ExplicitHeight = 13
  end
  object ButtonPanel: TPanel
    Left = 0
    Top = 424
    Width = 833
    Height = 41
    Align = alBottom
    BevelEdges = [beTop]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 732
    DesignSize = (
      833
      39)
    object bOK: TButton
      Left = 747
      Top = 6
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'OK'
      TabOrder = 0
      OnClick = bOKClick
      ExplicitLeft = 646
    end
    object bRefresh: TButton
      Left = 12
      Top = 6
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Caption = 'Refresh'
      TabOrder = 1
      OnClick = bRefreshClick
    end
  end
  object List: TListView
    AlignWithMargins = True
    Left = 3
    Top = 127
    Width = 827
    Height = 294
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvRaised
    BevelKind = bkFlat
    BorderStyle = bsNone
    Columns = <
      item
        Caption = 'PID'
        Width = 75
      end
      item
        Caption = 'Processname'
        Width = 150
      end
      item
        Caption = 'Handle'
        Width = 75
      end
      item
        Caption = 'Type'
        Width = 100
      end
      item
        Caption = 'Filename'
        Width = 400
      end>
    ColumnClick = False
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    ExplicitWidth = 791
  end
  object clb: TCheckListBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 827
    Height = 118
    Cursor = crHandPoint
    Align = alTop
    Columns = 4
    ItemHeight = 13
    Items.Strings = (
      'TYPE'
      'DIRECTORY'
      'SYMBOLIC_LINK'
      'TOKEN'
      'PROCESS'
      'THREAD'
      'JOB'
      'DEBUG_OBJECT'
      'EVENT'
      'EVENT_PAIR'
      'MUTANT'
      'CALLBACK'
      'SEMAPHORE'
      'TIMER'
      'PROFILE'
      'KEYED_EVENT'
      'WINDOWS_STATION'
      'DESKTOP'
      'SECTION'
      'KEY'
      'PORT'
      'WAITABLE_PORT'
      'ADAPTER'
      'CONTROLLER'
      'DEVICE'
      'DRIVER'
      'IOCOMPLETION'
      'FILE'
      'WMIGUID')
    ParentColor = True
    TabOrder = 2
    ExplicitWidth = 791
  end
end
