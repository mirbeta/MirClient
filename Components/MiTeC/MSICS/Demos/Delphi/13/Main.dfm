object wnd_db_Main: Twnd_db_Main
  Left = 0
  Top = 0
  Caption = 'Device Browser'
  ClientHeight = 554
  ClientWidth = 658
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
  object pc: TPageControl
    Left = 0
    Top = 0
    Width = 658
    Height = 554
    Cursor = crHandPoint
    ActivePage = tsDevTree
    Align = alClient
    HotTrack = True
    TabOrder = 0
    object tsDevTree: TTabSheet
      Caption = ' Windows Devices '
      DesignSize = (
        650
        526)
      object Tree: TTreeView
        Left = 10
        Top = 10
        Width = 630
        Height = 475
        Anchors = [akLeft, akTop, akRight, akBottom]
        HideSelection = False
        Images = ilSystem
        Indent = 19
        ReadOnly = True
        TabOrder = 0
        OnChange = TreeChange
        OnDblClick = TreeDblClick
        OnDeletion = TreeDeletion
      end
      object bProps: TButton
        Left = 540
        Top = 494
        Width = 100
        Height = 25
        Cursor = crHandPoint
        Anchors = [akRight, akBottom]
        Caption = 'Properties...'
        Enabled = False
        TabOrder = 3
        OnClick = cmProps
      end
      object bRes: TButton
        Left = 438
        Top = 494
        Width = 100
        Height = 25
        Cursor = crHandPoint
        Anchors = [akRight, akBottom]
        Caption = 'Resources...'
        Enabled = False
        TabOrder = 2
        OnClick = cmRes
      end
      object bRefresh: TButton
        Left = 10
        Top = 494
        Width = 100
        Height = 25
        Cursor = crHandPoint
        Anchors = [akLeft, akBottom]
        Caption = 'Refresh'
        TabOrder = 1
        OnClick = cmRefresh
      end
    end
    object tsDevRes: TTabSheet
      Caption = ' Device Resources '
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        650
        526)
      object ResList: TListView
        Left = 10
        Top = 10
        Width = 630
        Height = 506
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Resource'
            Width = 175
          end
          item
            Caption = 'Share'
            Width = 100
          end
          item
            Caption = 'Device'
            Width = 230
          end>
        ColumnClick = False
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        SmallImages = ilSystem
        SortType = stText
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
  end
  object ilSystem: TImageList
    ColorDepth = cd32Bit
    Left = 161
    Top = 79
  end
end
