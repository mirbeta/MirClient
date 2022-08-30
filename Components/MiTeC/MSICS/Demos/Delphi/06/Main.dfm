object wnd_Main: Twnd_Main
  Left = 296
  Top = 324
  Caption = 'MiTeC Process Viewer'
  ClientHeight = 517
  ClientWidth = 729
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    729
    517)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 729
    Height = 2
    Align = alTop
    Shape = bsTopLine
  end
  object pc: TPageControl
    Left = 11
    Top = 12
    Width = 706
    Height = 492
    Cursor = crHandPoint
    ActivePage = tsSvc
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    OnChange = pcChange
    object tsPrc: TTabSheet
      Caption = ' Processes '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        698
        464)
      object PrcList: TListView
        Left = 8
        Top = 7
        Width = 682
        Height = 413
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Name'
            Width = 200
          end
          item
            Alignment = taRightJustify
            Caption = 'PID'
            Width = 70
          end
          item
            Alignment = taRightJustify
            Caption = 'CPU Time'
            Width = 100
          end
          item
            Alignment = taRightJustify
            Caption = 'Mem Usage'
            Width = 100
          end
          item
            Caption = 'Command line'
            Width = 500
          end>
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnAdvancedCustomDrawItem = ListAdvancedCustomDrawItem
        OnAdvancedCustomDrawSubItem = ListAdvancedCustomDrawSubItem
        OnColumnClick = ListColumnClick
        OnCompare = ListCompare
        OnDblClick = cmPrcDetails
      end
      object Button2: TButton
        Left = 615
        Top = 430
        Width = 75
        Height = 25
        Cursor = crHandPoint
        Anchors = [akRight, akBottom]
        Caption = 'Details...'
        TabOrder = 1
        OnClick = cmPrcDetails
      end
    end
    object tsSvc: TTabSheet
      Caption = ' Services '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ImageIndex = 2
      ParentFont = False
      DesignSize = (
        698
        464)
      object SvcList: TListView
        Left = 7
        Top = 42
        Width = 682
        Height = 382
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Display Name'
            Width = 300
          end
          item
            Caption = 'Type'
            Width = 100
          end
          item
            Caption = 'Status'
            Width = 80
          end
          item
            Caption = 'Startup Type'
            Width = 75
          end
          item
            Caption = 'Account'
            Width = 200
          end>
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnAdvancedCustomDrawItem = ListAdvancedCustomDrawItem
        OnAdvancedCustomDrawSubItem = ListAdvancedCustomDrawSubItem
        OnColumnClick = ListColumnClick
        OnCompare = ListCompare
        OnDblClick = cmSvcDetails
      end
      object Button5: TButton
        Left = 615
        Top = 430
        Width = 75
        Height = 25
        Cursor = crHandPoint
        Anchors = [akRight, akBottom]
        Caption = 'Details...'
        TabOrder = 1
        OnClick = cmSvcDetails
      end
      object cbSvc: TComboBox
        Left = 7
        Top = 11
        Width = 163
        Height = 21
        Cursor = crHandPoint
        Style = csDropDownList
        ItemIndex = 2
        TabOrder = 2
        Text = 'All'
        OnChange = cbSvcChange
        Items.Strings = (
          'Services'
          'Drivers'
          'All')
      end
    end
    object tsDrv: TTabSheet
      Caption = ' Drivers '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ImageIndex = 1
      ParentFont = False
      TabVisible = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        698
        464)
      object DrvList: TListView
        Left = 8
        Top = 7
        Width = 682
        Height = 413
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Name'
            Width = 150
          end
          item
            Caption = 'Base Address'
            Width = 150
          end
          item
            Alignment = taRightJustify
            Caption = 'Load Cnt'
            Width = 60
          end
          item
            Caption = 'Description'
            Width = 500
          end>
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnAdvancedCustomDrawItem = ListAdvancedCustomDrawItem
        OnAdvancedCustomDrawSubItem = ListAdvancedCustomDrawSubItem
        OnColumnClick = ListColumnClick
        OnCompare = ListCompare
        OnDblClick = cmDrvDetails
      end
      object Button6: TButton
        Left = 615
        Top = 430
        Width = 75
        Height = 25
        Cursor = crHandPoint
        Anchors = [akRight, akBottom]
        Caption = 'Details...'
        TabOrder = 1
        OnClick = cmDrvDetails
      end
    end
  end
  object MainMenu: TMainMenu
    Left = 78
    Top = 158
    object File1: TMenuItem
      Caption = 'File'
      object mmRefresh: TMenuItem
        Caption = 'Refresh'
        ShortCut = 116
        OnClick = cmRefresh
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mmExit: TMenuItem
        Caption = 'Exit'
        OnClick = cmExit
      end
    end
    object mmProcess: TMenuItem
      Caption = 'Process'
      object mmPrcProps: TMenuItem
        Caption = 'Properties...'
      end
      object mmPrcDetails: TMenuItem
        Caption = 'Details...'
        OnClick = cmPrcDetails
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mmPrcKill: TMenuItem
        Caption = 'Kill'
        OnClick = cmPrcKill
      end
    end
    object mmService: TMenuItem
      Caption = 'Service'
      Visible = False
      object mmSvcProps: TMenuItem
        Caption = 'Properties...'
      end
      object mSvcDetails: TMenuItem
        Caption = 'Details...'
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object mmSvcStart: TMenuItem
        Caption = 'Start'
        OnClick = cmSvcStart
      end
      object mmSvcStop: TMenuItem
        Caption = 'Stop'
        OnClick = cmSvcStop
      end
      object mmSvcPause: TMenuItem
        Caption = 'Pause'
        OnClick = cmSvcPause
      end
      object mmSvcResume: TMenuItem
        Caption = 'Resume'
        OnClick = cmSvcResume
      end
    end
    object mmDriver: TMenuItem
      Caption = 'Driver'
      Visible = False
      object mmDrvProps: TMenuItem
        Caption = 'Properties...'
      end
    end
    object mmAbout: TMenuItem
      Caption = 'About'
      OnClick = mmAboutClick
    end
  end
end
