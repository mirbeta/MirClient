object wnd_msi_Summary: Twnd_msi_Summary
  Left = 567
  Top = 300
  Caption = 'Summary Report'
  ClientHeight = 542
  ClientWidth = 784
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 501
    Width = 784
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 1
    DesignSize = (
      784
      41)
    object Button1: TButton
      Left = 707
      Top = 8
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
    end
    object bSave: TButton
      Left = 10
      Top = 8
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Caption = 'Save...'
      TabOrder = 1
      OnClick = bSaveClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 45
    Width = 784
    Height = 456
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 10
    Caption = ' '
    TabOrder = 0
    object pc: TPageControl
      Left = 10
      Top = 10
      Width = 764
      Height = 436
      Cursor = crHandPoint
      ActivePage = TabSheet1
      Align = alClient
      TabOrder = 0
      object TabSheet1: TTabSheet
        Caption = ' Hardware '
        object Panel3: TPanel
          Left = 0
          Top = 0
          Width = 756
          Height = 408
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 10
          Caption = ' '
          TabOrder = 0
          object HWList: TListView
            Left = 10
            Top = 10
            Width = 736
            Height = 388
            Align = alClient
            Columns = <
              item
                Caption = 'Machine'
                Width = 125
              end
              item
                Alignment = taRightJustify
                Caption = 'CPU Count'
              end
              item
                Caption = 'CPU Vendor'
                Width = 75
              end
              item
                Caption = 'CPU Name'
                Width = 150
              end
              item
                Alignment = taRightJustify
                Caption = 'CPU Freq [MHz]'
                Width = 70
              end
              item
                Alignment = taRightJustify
                Caption = 'Memory [MB]'
                Width = 65
              end
              item
                Alignment = taRightJustify
                Caption = 'HDD Count'
              end
              item
                Alignment = taRightJustify
                Caption = 'Capacity [GB]'
                Width = 70
              end
              item
                Alignment = taRightJustify
                Caption = 'CDROM Count'
                Width = 65
              end
              item
                Alignment = taRightJustify
                Caption = 'DVD Count'
                Width = 65
              end
              item
                Alignment = taRightJustify
                Caption = 'Tape Count'
                Width = 65
              end
              item
                Caption = 'System'
                Width = 200
              end
              item
                Caption = 'Mainboard'
                Width = 200
              end
              item
                Caption = 'Graphic Adapter'
                Width = 200
              end
              item
                Caption = 'Operating System'
                Width = 250
              end
              item
                Caption = 'Machine GUID'
                Width = 150
              end
              item
                Caption = 'Product Key'
                Width = 150
              end>
            HideSelection = False
            ReadOnly = True
            RowSelect = True
            TabOrder = 0
            ViewStyle = vsReport
            OnAdvancedCustomDrawItem = HWListAdvancedCustomDrawItem
            OnAdvancedCustomDrawSubItem = HWListAdvancedCustomDrawSubItem
            OnColumnClick = HWListColumnClick
            OnCompare = HWListCompare
          end
        end
      end
      object TabSheet2: TTabSheet
        Caption = ' Software '
        ImageIndex = 1
        object Panel4: TPanel
          Left = 0
          Top = 0
          Width = 756
          Height = 408
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 10
          Caption = ' '
          TabOrder = 0
          object SWList: TListView
            Left = 10
            Top = 10
            Width = 736
            Height = 388
            Align = alClient
            Columns = <
              item
                Caption = 'Product'
                Width = 350
              end
              item
                Alignment = taRightJustify
                Caption = 'Count'
              end
              item
                Caption = 'Location'
                Width = 300
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
      object TabSheet3: TTabSheet
        Caption = ' OS '
        ImageIndex = 2
        object Panel5: TPanel
          Left = 0
          Top = 0
          Width = 756
          Height = 408
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 10
          Caption = ' '
          TabOrder = 0
          object OSList: TListView
            Left = 10
            Top = 10
            Width = 736
            Height = 388
            Align = alClient
            Columns = <
              item
                Caption = 'Name'
                Width = 300
              end
              item
                Alignment = taRightJustify
                Caption = 'Count'
              end
              item
                Caption = 'Location'
                Width = 350
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
      object TabSheet4: TTabSheet
        Caption = ' Network '
        ImageIndex = 3
        object Panel6: TPanel
          Left = 0
          Top = 0
          Width = 756
          Height = 408
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 10
          Caption = ' '
          TabOrder = 0
          object NetList: TListView
            Left = 10
            Top = 10
            Width = 736
            Height = 388
            Align = alClient
            Columns = <
              item
                Caption = 'Machine'
                Width = 100
              end
              item
                Caption = 'IP Address'
                Width = 300
              end
              item
                Caption = 'MAC Address'
                Width = 300
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
    end
  end
  object Header: TPanel
    Left = 0
    Top = 0
    Width = 784
    Height = 45
    Align = alTop
    BevelOuter = bvLowered
    Caption = ' '
    Color = 13399629
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    object OverviewIcon: TImage
      Left = 10
      Top = 5
      Width = 32
      Height = 32
      AutoSize = True
      Center = True
      Picture.Data = {
        055449636F6E0000010001002020200000000000A81000001600000028000000
        2000000040000000010020000000000000100000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000B0753BFFB0753BFDB0753BFDB0753BFDB0753BFEB0753BFE
        0000000000000000B0753BFEB0753BFEB0753BFDB0753BFDB0753BFEB0753BFE
        0000000000000000B0753BFEB0753BFDB0753BFDB0753BFDB0753BFEB0753BFF
        0000000000000000B0753BFFB0753BFDB0753BFDB0753BFDB0753BFEB0753BFE
        0000000000000000B0753BFFB0753BFDB0753BFDB0753BFDB0753BFEB0753BFE
        0000000000000000B0753BFEB0753BFEB0753BFDB0753BFDB0753BFEB0753BFE
        0000000000000000B0753BFEB0753BFDB0753BFDB0753BFDB0753BFEB0753BFF
        0000000000000000B0753BFFB0753BFDB0753BFDB0753BFDB0753BFEB0753BFE
        0000000000000000B0753BFFB0753BFDB0753BFDB0753BFDB0753BFEB0753BFE
        0000000000000000B0753BFEB0753BFEB0753BFDB0753BFDB0753BFEB0753BFE
        0000000000000000B0753BFEB0753BFDB0753BFDB0753BFDB0753BFEB0753BFF
        0000000000000000B0753BFFB0753BFDB0753BFDB0753BFDB0753BFEB0753BFE
        0000000000000000B0753BFFB0753BFDB0753BFDB0753BFDB0753BFEB0753BFE
        0000000000000000B0753BFEB0753BFEB0753BFDB0753BFDB0753BFEB0753BFE
        0000000000000000B0753BFEB0753BFDB0753BFDB0753BFDB0753BFEB0753BFF
        0000000000000000B0753BFFB0753BFDB0753BFDB0753BFDB0753BFEB0753BFE
        0000000000000000B0753BFFB0753BFDB0753BFDB0753BFDB0753BFEB0753BFE
        0000000000000000B0753BFEB0753BFEB0753BFDB0753BFDB0753BFEB0753BFE
        0000000000000000B0753BFEB0753BFDB0753BFDB0753BFDB0753BFEB0753BFF
        0000000000000000B0753BFFB0753BFDB0753BFDB0753BFDB0753BFEB0753BFE
        0000000000000000B0753BFEB0753BFEB0753BFDB0753BFDB0753BFDB0753BFE
        0000000000000000B0753BFEB1753BFEB0753BFDB0753BFDB0753BFDB0753BFE
        0000000000000000B0753BFEB0753BFDB0753BFDB0753BFDB0753BFDB0753BFE
        0000000000000000B0753BFFB0753BFDB0753BFDB0753BFDB0753BFEB0753BFE
        0000000000000000B0753BFEB0753BFDB0753BFDB0753BFDB0753BFDB0753BFD
        0000000000000000B0753BFEB0753BFEB0753BFDB0753BFDB0753BFDB0753BFD
        0000000000000000B0753BFEB0753BFDB0753BFDB0753BFDB0753BFDB0753BFE
        0000000000000000B0753BFFB0753BFDB0753BFDB0753BFDB0753BFEB0753BFE
        0000000000000000B0753BFEB0753BFDAF743AFCB0753BFDAF743AFCAF743AFD
        0000000000000000B0753BFDB0753BFDAF743AFCAF743AFCB0753BFDAF743AFD
        0000000000000000B0753BFDB0753BFDB0753BFDAF743AFCAF743AFCB0753BFD
        0000000000000000B0753BFEB0753BFEB0753BFDB0753BFDB0753BFDB0753BFE
        0000000000000000B0753BFEB0753BFDB0753BFDB0753BFDB0753BFDB0753BFD
        0000000000000000B0753BFEB0753BFEB0753BFDB0753BFDB0753BFDB0753BFD
        0000000000000000B0753BFEB0753BFDB0753BFDB0753BFDB0753BFDB0753BFE
        0000000000000000B0753BFEB0753BFDB0753BFDB0753BFDB0753BFDB0753BFD
        0000000000000000B0753BFEB0753BFDB0753BFDB0753BFDB0753BFDB0753BFD
        0000000000000000B0753BFEB0753BFEB0753BFDB0753BFDB0753BFDB0753BFD
        0000000000000000B0753BFEB0753BFDB0753BFDB0753BFDB0753BFDB0753BFE
        0000000000000000B0753BFEB0753BFDAF743AFCB0753BFDAF743AFCAF743AFD
        0000000000000000B0753BFEB0753BFDB0753BFDB0753BFDB0753BFDB0753BFD
        0000000000000000B0753BFEB0753BFEB0753BFDB0753BFDB0753BFDB0753BFD
        0000000000000000B0753BFEB0753BFDB0753BFDB0753BFDB0753BFDB0753BFE
        0000000000000000B0753BFEB0753BFDB0753BFDB0753BFDB0753BFDB0753BFD
        0000000000000000B0753BFEB0753BFDB0753BFDB0753BFDB0753BFDB0753BFD
        0000000000000000B0753BFEB0753BFEB0753BFDB0753BFDB0753BFDB0753BFD
        0000000000000000B0753BFEB0753BFDB0753BFDB0753BFDB0753BFDB0753BFE
        0000000000000000B0753BFEB0753BFDB0753BFDB0753BFDB0753BFDB0753BFD
        0000000000000000AD743C99B0753BFFB0753BFFB0753BFFB0753BFFAD743CA1
        0000000000000000B0753BFEB0753BFEB0753BFDB0753BFDB0753BFDB0753BFD
        0000000000000000B0753BFEB0753BFDB0753BFDB0753BFDB0753BFDB0753BFE
        0000000000000000B0753BFEB0753BFDB0753BFDB0753BFDB0753BFDB0753BFD
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000B0753BFEB0753BFDB0753BFDB0753BFDB0753BFDB0753BFD
        0000000000000000B0753BFEB0753BFDB0753BFEB0753BFDB0753BFDB0753BFE
        0000000000000000B0753BFEB0753BFDB0753BFDB0753BFDB0753BFDB0753BFD
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000B0753BFEB0753BFDB0753BFDB0753BFDB0753BFDB0753BFD
        0000000000000000B0753BFEB0753BFEB0753BFEB0753BFEB0753BFEB0753BFF
        0000000000000000AD743C99B0753BFFB0753BFFB0753BFFB0753BFFAD743CA1
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000B0753BFEB0753BFDB0753BFDB0753BFDB0753BFDB0753BFD
        0000000000000000B0753BFEB0753BFEB0753BFEB0753BFEB0753BFEB0753BFF
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000B0753BFEB0753BFDB0753BFDB0753BFDB0753BFDB0753BFD
        0000000000000000B0753BFEB0753BFEB0753BFEB0753BFEB0753BFEB0753BFF
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000B0753BFEB0753BFDB0753BFDB0753BFDB0753BFDB0753BFD
        0000000000000000B0753BFEB0753BFEB0753BFEB0753BFEB0753BFEB0753BFF
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000616161FE616161FE00000000616161FE616161FE00000000616161FE
        616161FE00000000616161FE616161FE00000000616161FE616161FE00000000
        616161FE616161FE00000000616161FE616161FE00000000616161FE616161FE
        00000000616161FE616161FE00000000616161FE616161FE00000000616161FE
        616161FE00000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000000000003F55C1FE3F55C1FE3F55C1FD3F55C1FD3F55C1FD3F55C1FE
        00000000000000003F55C1FE3F55C1FD3F55C1FD3F55C1FD3F55C1FD3F55C1FE
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000000000003F55C1FE3F55C1FE3F55C1FD3F55C1FD3F55C1FD3F55C1FD
        00000000000000003F55C1FE3F55C1FE3F55C1FE3F55C1FE3F55C1FE3F55C1FF
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000000000003F55C1FE3F55C1FE3F55C1FD3F55C1FD3F55C1FD3F55C1FD
        00000000000000003F55C1FE3F55C1FE3F55C1FE3F55C1FE3F55C1FE3F55C1FF
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000000000003F55C1FE3F55C1FE3F55C1FD3F55C1FD3F55C1FE3F55C1FE
        00000000000000003F55C1CF3F55C1FE3F55C1FD3F55C1FD3F55C1F93F55C1CB
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000000000003F55C1FE3F55C1FE3F55C1FD3F55C1FD3F55C1FD3F55C1FD
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000000000003F55C1FE3F55C1FE3F55C1FD3F55C1FD3F55C1FD3F55C1FE
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000000000003F55C1FE3F55C1FE3F55C1FD3F55C1FD3F55C1FD3F55C1FE
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000000000003F55C1FE3F55C1FE3F55C1FD3F55C1FD3F55C1FE3F55C1FE
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000000000003F55C1CC3F55C1FE3F55C1FD3F55C1FD3F55C1FA3F55C1CE
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000FFFFFFFF818181818181818181818181818181818181818181818181
        81818181818181818181818181818181818181818181818181818181FF818181
        FF818181FF8181FFFF8181FFFF8181FFFFFFFFFF24924924FFFFFFFFFF8181FF
        FF8181FFFF8181FFFF8181FFFF81FFFFFF81FFFFFF81FFFFFF81FFFFFF81FFFF
        FFFFFFFF}
    end
    object lMachine: TLabel
      Left = 48
      Top = 5
      Width = 109
      Height = 16
      Caption = 'Summary Report'
      Color = 13882323
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      Transparent = True
      Layout = tlCenter
    end
    object lCount: TLabel
      Left = 57
      Top = 24
      Width = 14
      Height = 13
      Caption = 'OS'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
  end
  object sd: TSaveDialog
    Filter = 'CSV files|*.csv|All files|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 209
    Top = 280
  end
end
