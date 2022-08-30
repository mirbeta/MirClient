object dlg_PrcDetails: Tdlg_PrcDetails
  Left = 333
  Top = 447
  Caption = 'Process Details'
  ClientHeight = 526
  ClientWidth = 661
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    661
    526)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 12
    Top = 473
    Width = 637
    Height = 6
    Anchors = [akLeft, akRight, akBottom]
    Shape = bsTopLine
    ExplicitWidth = 566
  end
  object imgIcon: TImage
    Left = 14
    Top = 11
    Width = 32
    Height = 32
    Center = True
  end
  object bClose: TButton
    Left = 574
    Top = 487
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object pc: TPageControl
    Left = 12
    Top = 52
    Width = 637
    Height = 407
    Cursor = crHandPoint
    ActivePage = tsEnv
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    MultiLine = True
    ParentFont = False
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = ' General '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ImageIndex = 1
      ParentFont = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        629
        379)
      object GenList: TListView
        Left = 11
        Top = 14
        Width = 606
        Height = 353
        Anchors = [akLeft, akTop, akRight, akBottom]
        Color = clBtnFace
        Columns = <
          item
            Width = 125
          end
          item
            Width = 450
          end>
        ColumnClick = False
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        ParentShowHint = False
        ShowColumnHeaders = False
        ShowHint = True
        TabOrder = 0
        ViewStyle = vsReport
        OnAdvancedCustomDrawItem = GenListAdvancedCustomDrawItem
        OnAdvancedCustomDrawSubItem = GenListAdvancedCustomDrawSubItem
      end
    end
    object TabSheet4: TTabSheet
      Caption = ' Performance '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ImageIndex = 3
      ParentFont = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        629
        379)
      object CntList: TListView
        Left = 12
        Top = 12
        Width = 606
        Height = 353
        Anchors = [akLeft, akTop, akRight, akBottom]
        Color = clBtnFace
        Columns = <
          item
            Caption = 'Name'
            Width = 350
          end
          item
            Alignment = taRightJustify
            Caption = 'Current'
            Width = 100
          end
          item
            Alignment = taRightJustify
            Caption = 'Peak'
            Width = 100
          end>
        ColumnClick = False
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        ViewStyle = vsReport
        OnAdvancedCustomDrawItem = GenListAdvancedCustomDrawItem
        OnAdvancedCustomDrawSubItem = GenListAdvancedCustomDrawSubItem
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Modules'
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
        629
        379)
      object ModList: TListView
        Left = 11
        Top = 12
        Width = 606
        Height = 357
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Name'
            Width = 125
          end
          item
            Alignment = taRightJustify
            Caption = 'Size [B]'
            Width = 75
          end
          item
            Alignment = taRightJustify
            Caption = 'Base Address'
            Width = 120
          end
          item
            Caption = 'Description'
            Width = 300
          end>
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        ViewStyle = vsReport
        OnAdvancedCustomDrawItem = ListAdvancedCustomDrawItem
        OnAdvancedCustomDrawSubItem = ListAdvancedCustomDrawSubItem
        OnColumnClick = ListColumnClick
        OnCompare = ListCompare
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Threads'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ImageIndex = 4
      ParentFont = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        629
        379)
      object ThdList: TListView
        Left = 11
        Top = 12
        Width = 606
        Height = 353
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'ID'
          end
          item
            Caption = 'Start Address'
            Width = 80
          end
          item
            Alignment = taRightJustify
            Caption = 'Base Pri'
            Width = 55
          end
          item
            Alignment = taRightJustify
            Caption = 'Dyn Pri'
          end
          item
            Caption = 'State'
            Width = 100
          end
          item
            Alignment = taRightJustify
            Caption = 'CntxSwitch Cnt'
            Width = 85
          end
          item
            Caption = 'Kernel Time'
            Width = 70
          end
          item
            Caption = 'User Time'
            Width = 70
          end>
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        ViewStyle = vsReport
        OnAdvancedCustomDrawItem = ListAdvancedCustomDrawItem
        OnAdvancedCustomDrawSubItem = ListAdvancedCustomDrawSubItem
        OnColumnClick = ListColumnClick
        OnCompare = ListCompare
      end
    end
    object tsEnv: TTabSheet
      Caption = 'Environment'
      ImageIndex = 8
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        629
        379)
      object EnvBox: TListBox
        Left = 14
        Top = 14
        Width = 599
        Height = 345
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object TabSheet6: TTabSheet
      Caption = ' Handles '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ImageIndex = 5
      ParentFont = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        629
        379)
      object HList: TListView
        Left = 12
        Top = 12
        Width = 606
        Height = 353
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Handle'
          end
          item
            Caption = 'Type'
            Width = 75
          end
          item
            Caption = 'Access'
            Width = 80
          end
          item
            Caption = 'Name'
            Width = 350
          end>
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        ViewStyle = vsReport
        OnAdvancedCustomDrawItem = ListAdvancedCustomDrawItem
        OnAdvancedCustomDrawSubItem = ListAdvancedCustomDrawSubItem
        OnColumnClick = ListColumnClick
        OnCompare = ListCompare
      end
    end
    object TabSheet7: TTabSheet
      Caption = ' Windows '
      ImageIndex = 6
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        629
        379)
      object WinTree: TTreeView
        Left = 11
        Top = 12
        Width = 606
        Height = 312
        Anchors = [akLeft, akTop, akRight, akBottom]
        HideSelection = False
        Images = ImageList
        Indent = 19
        ReadOnly = True
        TabOrder = 0
        OnDblClick = cmWinDetails
        OnDeletion = WinTreeDeletion
      end
      object Button5: TButton
        Left = 542
        Top = 338
        Width = 75
        Height = 25
        Cursor = crHandPoint
        Anchors = [akRight, akBottom]
        Caption = 'Details...'
        TabOrder = 1
        OnClick = cmWinDetails
      end
    end
    object TabSheet8: TTabSheet
      Caption = ' Security '
      ImageIndex = 7
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        629
        379)
      object SecList: TListView
        Left = 10
        Top = 11
        Width = 606
        Height = 353
        Anchors = [akLeft, akTop, akRight, akBottom]
        Color = clBtnFace
        Columns = <
          item
            Width = 450
          end
          item
            Width = 115
          end>
        ColumnClick = False
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        ParentShowHint = False
        ShowColumnHeaders = False
        ShowHint = True
        TabOrder = 0
        ViewStyle = vsReport
        OnAdvancedCustomDrawItem = GenListAdvancedCustomDrawItem
        OnAdvancedCustomDrawSubItem = GenListAdvancedCustomDrawSubItem
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Child Processes'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ImageIndex = 2
      ParentFont = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        629
        379)
      object CPList: TListView
        Left = 12
        Top = 12
        Width = 606
        Height = 357
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'PID'
            Width = 125
          end
          item
            Caption = 'Name'
            Width = 450
          end>
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        ViewStyle = vsReport
        OnAdvancedCustomDrawItem = ListAdvancedCustomDrawItem
        OnAdvancedCustomDrawSubItem = ListAdvancedCustomDrawSubItem
        OnColumnClick = ListColumnClick
        OnCompare = ListCompare
      end
    end
  end
  object eName: TEdit
    Left = 54
    Top = 16
    Width = 593
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
    TabOrder = 2
  end
  object ImageList: TImageList
    Left = 108
    Top = 472
    Bitmap = {
      494C010102000500040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009C9C9C009C9C9C009C9C
      9C009C9C9C009C9C9C009C9C9C009C9C9C009C9C9C009C9C9C009C9C9C009C9C
      9C009C9C9C009C9C9C009C9C9C000000000000000000FF310000FF310000FF31
      0000FF310000FF310000FF310000FF310000FF310000FF310000FF310000FF31
      0000FF310000FF310000FF310000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009C9C9C00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000009C9C9C000000000000000000FF310000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF310000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009C9C9C00000000009C9C
      9C00000000009C9C9C009C9C9C00000000009C9C9C009C9C9C00000000009C9C
      9C009C9C9C00000000009C9C9C000000000000000000FF310000FFFFFF009C9C
      9C00FFFFFF009C9C9C009C9C9C00FFFFFF009C9C9C009C9C9C00FFFFFF009C9C
      9C009C9C9C00FFFFFF00FF310000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009C9C9C00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000009C9C9C000000000000000000FF310000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF310000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009C9C9C00000000009C9C
      9C00000000009C9C9C009C9C9C00000000009C9C9C009C9C9C00000000009C9C
      9C009C9C9C00000000009C9C9C000000000000000000FF310000FFFFFF009C9C
      9C00FFFFFF009C9C9C009C9C9C00FFFFFF009C9C9C009C9C9C00FFFFFF009C9C
      9C009C9C9C00FFFFFF00FF310000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009C9C9C00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000009C9C9C000000000000000000FF310000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF310000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009C9C9C00000000009C9C
      9C009C9C9C009C9C9C009C9C9C009C9C9C009C9C9C009C9C9C009C9C9C009C9C
      9C009C9C9C00000000009C9C9C000000000000000000FF310000FFFFFF00CE9C
      6300CE9C6300CE9C6300CE9C6300CE9C6300CE9C6300CE9C6300CE9C6300CE9C
      6300CE9C6300FFFFFF00FF310000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009C9C9C00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000009C9C9C000000000000000000FF310000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF310000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009C9C9C00000000000000
      0000000000009C9C9C009C9C9C00000000009C9C9C009C9C9C00000000009C9C
      9C009C9C9C00000000009C9C9C000000000000000000FF310000FFFFFF00FFFF
      FF00FFFFFF009C9C9C009C9C9C00FFFFFF009C9C9C009C9C9C00FFFFFF009C9C
      9C009C9C9C00FFFFFF00FF310000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009C9C9C00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000009C9C9C000000000000000000FF310000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF310000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009C9C9C00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000009C9C9C000000000000000000FF310000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF310000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009C9C9C009C9C9C009C9C
      9C009C9C9C009C9C9C009C9C9C009C9C9C009C9C9C009C9C9C009C9C9C009C9C
      9C009C9C9C009C9C9C009C9C9C000000000000000000FF310000FF310000FF31
      0000FF310000FF310000FF310000FF310000FF310000FF310000FF310000FF31
      0000FF310000FF310000FF310000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009C9C9C009C9C9C009C9C
      9C009C9C9C009C9C9C009C9C9C009C9C9C009C9C9C009C9C9C009C9C9C009C9C
      9C009C9C9C009C9C9C009C9C9C000000000000000000FF310000FF310000FF31
      0000FF310000FF310000FF310000FF310000FF310000FF310000FF310000FF31
      0000FF310000FF310000FF310000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CECECE009C9C9C009C9C
      9C009C9C9C009C9C9C009C9C9C009C9C9C009C9C9C009C9C9C009C9C9C009C9C
      9C009C9C9C009C9C9C00CECECE000000000000000000FFCE9C00FF310000FF31
      0000FF310000FF310000FF310000FF310000FF310000FF310000FF310000FF31
      0000FF310000FF310000FFCE9C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFF000000008001800100000000
      BFFD800100000000A925800100000000BFFD800100000000A925800100000000
      BFFD800100000000A005800100000000BFFD800100000000B925800100000000
      BFFD800100000000BFFD80010000000080018001000000008001800100000000
      8001800100000000FFFFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
end
