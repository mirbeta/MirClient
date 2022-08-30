object wndDetails: TwndDetails
  Left = 328
  Top = 273
  Caption = 'SMBIOS Details'
  ClientHeight = 494
  ClientWidth = 702
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
  object Panel2: TPanel
    Left = 0
    Top = 457
    Width = 702
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Panel3: TPanel
      Left = 536
      Top = 0
      Width = 166
      Height = 37
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object bOK: TButton
        Left = 80
        Top = 4
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
  object Panel25: TPanel
    Left = 0
    Top = 0
    Width = 702
    Height = 457
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 10
    Caption = ' '
    TabOrder = 1
    DesignSize = (
      702
      457)
    object Label60: TLabel
      Left = 22
      Top = 15
      Width = 35
      Height = 13
      Alignment = taRightJustify
      Caption = 'Version'
    end
    object Bevel7: TBevel
      Left = 15
      Top = 36
      Width = 673
      Height = 5
      Anchors = [akLeft, akTop, akRight]
      Shape = bsTopLine
    end
    object Label66: TLabel
      Left = 501
      Top = 15
      Width = 106
      Height = 13
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      Caption = 'Structure Table Count'
    end
    object eSMVer: TEdit
      Left = 66
      Top = 11
      Width = 95
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 0
    end
    object pcSM: TPageControl
      Left = 10
      Top = 46
      Width = 682
      Height = 401
      Cursor = crHandPoint
      ActivePage = tsBIOS
      Align = alBottom
      Anchors = [akLeft, akTop, akRight, akBottom]
      HotTrack = True
      MultiLine = True
      TabOrder = 1
      object tsBIOS: TTabSheet
        Caption = ' BIOS '
        ImageIndex = 9
        object GroupBox21: TGroupBox
          Left = 0
          Top = 0
          Width = 674
          Height = 337
          Align = alClient
          TabOrder = 0
          DesignSize = (
            674
            337)
          object Label3: TLabel
            Left = 51
            Top = 22
            Width = 34
            Height = 13
            Alignment = taRightJustify
            Caption = 'Vendor'
          end
          object Label4: TLabel
            Left = 50
            Top = 49
            Width = 35
            Height = 13
            Alignment = taRightJustify
            Caption = 'Version'
          end
          object Label5: TLabel
            Left = 62
            Top = 77
            Width = 23
            Height = 13
            Alignment = taRightJustify
            Caption = 'Date'
          end
          object Label6: TLabel
            Left = 66
            Top = 104
            Width = 19
            Height = 13
            Alignment = taRightJustify
            Caption = 'Size'
          end
          object Label53: TLabel
            Left = 190
            Top = 105
            Width = 12
            Height = 13
            Caption = 'KB'
          end
          object Label8: TLabel
            Left = 12
            Top = 143
            Width = 73
            Height = 13
            Alignment = taRightJustify
            Caption = 'System Version'
          end
          object Label9: TLabel
            Left = 215
            Top = 143
            Width = 185
            Height = 13
            Alignment = taRightJustify
            Caption = 'Embedded Controller Firmware Version'
          end
          object eBIOSVendor: TEdit
            Left = 95
            Top = 18
            Width = 276
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 0
          end
          object eBIOSVer: TEdit
            Left = 95
            Top = 45
            Width = 90
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 1
          end
          object eBIOSDate: TEdit
            Left = 95
            Top = 73
            Width = 90
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 2
          end
          object eBIOSSize: TEdit
            Left = 95
            Top = 100
            Width = 90
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 3
          end
          object eBIOSSV: TEdit
            Left = 95
            Top = 139
            Width = 90
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 4
          end
          object eBIOSECFV: TEdit
            Left = 410
            Top = 139
            Width = 90
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 5
          end
          object clbBIOS: TCheckListBox
            Left = 13
            Top = 176
            Width = 643
            Height = 148
            OnClickCheck = clbBIOSClickCheck
            Anchors = [akLeft, akTop, akRight, akBottom]
            Color = clBtnFace
            Columns = 3
            ItemHeight = 13
            Items.Strings = (
              'ISA'
              'MCA'
              'EISA'
              'PCI'
              'PCMCIA'
              'Plug-and-Play'
              'APM'
              'Flash BIOS'
              'BIOS Shadow'
              'VL-VESA'
              'ESCD'
              'Boot from CD'
              'Selectable Boot'
              'BIOS ROM Socketed'
              'Boot from PC Card'
              'EDD'
              'NEC PC-98'
              'ACPI'
              'USB Legacy'
              'AGP'
              'I2O Boot'
              'LS-120 Boot'
              'ATAPI ZIP Drive Boot'
              'IEE1394 Boot'
              'Smart Battery'
              'BIOS Boot Specification'
              'Function key-initiated Network Service Boot'
              'Targeted Content Distribution'
              'UEFI Specification'
              'Virtual Machine')
            TabOrder = 6
          end
        end
      end
      object tsSMSystem: TTabSheet
        Caption = ' System '
        object GroupBox4: TGroupBox
          Left = 0
          Top = 0
          Width = 674
          Height = 337
          Align = alClient
          TabOrder = 0
          object Label61: TLabel
            Left = 78
            Top = 24
            Width = 28
            Height = 13
            Alignment = taRightJustify
            Caption = 'Model'
          end
          object Label62: TLabel
            Left = 41
            Top = 51
            Width = 65
            Height = 13
            Alignment = taRightJustify
            Caption = 'Manufacturer'
          end
          object lSysVer: TLabel
            Left = 71
            Top = 78
            Width = 35
            Height = 13
            Alignment = taRightJustify
            Caption = 'Version'
          end
          object lSysSer: TLabel
            Left = 40
            Top = 104
            Width = 66
            Height = 13
            Alignment = taRightJustify
            Caption = 'Serial Number'
          end
          object lSysID: TLabel
            Left = 12
            Top = 131
            Width = 94
            Height = 13
            Alignment = taRightJustify
            Caption = 'Universal Unique ID'
          end
          object eSysMod: TEdit
            Left = 116
            Top = 20
            Width = 200
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 0
          end
          object eSysMan: TEdit
            Left = 116
            Top = 47
            Width = 200
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 1
          end
          object eSysVer: TEdit
            Left = 116
            Top = 74
            Width = 200
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 2
          end
          object eSysSer: TEdit
            Left = 116
            Top = 100
            Width = 200
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 3
          end
          object eSysID: TEdit
            Left = 116
            Top = 127
            Width = 200
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 4
          end
        end
      end
      object tsSMMB: TTabSheet
        Caption = ' MainBoard'
        ImageIndex = 1
        object GroupBox15: TGroupBox
          Left = 0
          Top = 0
          Width = 674
          Height = 337
          Align = alClient
          TabOrder = 0
          object Label67: TLabel
            Left = 78
            Top = 24
            Width = 28
            Height = 13
            Alignment = taRightJustify
            Caption = 'Model'
          end
          object Label68: TLabel
            Left = 41
            Top = 51
            Width = 65
            Height = 13
            Alignment = taRightJustify
            Caption = 'Manufacturer'
          end
          object Label70: TLabel
            Left = 71
            Top = 78
            Width = 35
            Height = 13
            Alignment = taRightJustify
            Caption = 'Version'
          end
          object Label71: TLabel
            Left = 40
            Top = 104
            Width = 66
            Height = 13
            Alignment = taRightJustify
            Caption = 'Serial Number'
          end
          object Label2: TLabel
            Left = 58
            Top = 130
            Width = 48
            Height = 13
            Alignment = taRightJustify
            Caption = 'Asset Tag'
          end
          object Label7: TLabel
            Left = 14
            Top = 155
            Width = 92
            Height = 13
            Alignment = taRightJustify
            Caption = 'Location In Chassis'
          end
          object eMBMod: TEdit
            Left = 116
            Top = 20
            Width = 200
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 0
          end
          object eMBMan: TEdit
            Left = 116
            Top = 47
            Width = 200
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 1
          end
          object eMBVer: TEdit
            Left = 116
            Top = 74
            Width = 200
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 2
          end
          object eMBSer: TEdit
            Left = 116
            Top = 100
            Width = 200
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 3
          end
          object eMBAT: TEdit
            Left = 116
            Top = 126
            Width = 200
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 4
          end
          object eMBLIC: TEdit
            Left = 116
            Top = 151
            Width = 200
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 5
          end
        end
      end
      object tsSMCH: TTabSheet
        Caption = ' Chassis '
        ImageIndex = 2
        object GroupBox18: TGroupBox
          Left = 0
          Top = 0
          Width = 674
          Height = 337
          Align = alClient
          TabOrder = 0
          object Label78: TLabel
            Left = 78
            Top = 24
            Width = 28
            Height = 13
            Alignment = taRightJustify
            Caption = 'Model'
          end
          object Label79: TLabel
            Left = 41
            Top = 51
            Width = 65
            Height = 13
            Alignment = taRightJustify
            Caption = 'Manufacturer'
          end
          object Label80: TLabel
            Left = 71
            Top = 78
            Width = 35
            Height = 13
            Alignment = taRightJustify
            Caption = 'Version'
          end
          object Label81: TLabel
            Left = 40
            Top = 104
            Width = 66
            Height = 13
            Alignment = taRightJustify
            Caption = 'Serial Number'
          end
          object Label1: TLabel
            Left = 58
            Top = 130
            Width = 48
            Height = 13
            Alignment = taRightJustify
            Caption = 'Asset Tag'
          end
          object eCHMod: TEdit
            Left = 116
            Top = 20
            Width = 200
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 0
          end
          object eCHMan: TEdit
            Left = 116
            Top = 47
            Width = 200
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 1
          end
          object eCHVer: TEdit
            Left = 116
            Top = 74
            Width = 200
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 2
          end
          object eCHSer: TEdit
            Left = 116
            Top = 100
            Width = 200
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 3
          end
          object eCHAT: TEdit
            Left = 116
            Top = 126
            Width = 200
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 4
          end
        end
      end
      object tsSMMC: TTabSheet
        Caption = ' Memory Controller '
        ImageIndex = 10
        object GroupBox22: TGroupBox
          Left = 0
          Top = 0
          Width = 674
          Height = 337
          Align = alClient
          TabOrder = 0
          object Label103: TLabel
            Left = 6
            Top = 24
            Width = 100
            Height = 13
            Alignment = taRightJustify
            Caption = 'Interleave (Cur/Sup)'
          end
          object Label104: TLabel
            Left = 18
            Top = 51
            Width = 88
            Height = 13
            Alignment = taRightJustify
            Caption = 'Supported Speeds'
          end
          object Label105: TLabel
            Left = 24
            Top = 78
            Width = 82
            Height = 13
            Alignment = taRightJustify
            Caption = 'Supported Types'
          end
          object Label106: TLabel
            Left = 12
            Top = 104
            Width = 94
            Height = 13
            Alignment = taRightJustify
            Caption = 'Supported Voltages'
          end
          object Label107: TLabel
            Left = 19
            Top = 131
            Width = 87
            Height = 13
            Alignment = taRightJustify
            Caption = 'Maximum Slot Size'
          end
          object Label108: TLabel
            Left = 222
            Top = 131
            Width = 14
            Height = 13
            Caption = 'MB'
          end
          object Label109: TLabel
            Left = 30
            Top = 158
            Width = 76
            Height = 13
            Alignment = taRightJustify
            Caption = 'Number of Slots'
          end
          object eMCI: TEdit
            Left = 116
            Top = 20
            Width = 200
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 0
          end
          object eMCSS: TEdit
            Left = 116
            Top = 47
            Width = 200
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 1
          end
          object eMCST: TEdit
            Left = 116
            Top = 74
            Width = 200
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 2
          end
          object eMCSV: TEdit
            Left = 116
            Top = 100
            Width = 200
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 3
          end
          object eMCMS: TEdit
            Left = 116
            Top = 127
            Width = 100
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 4
          end
          object eMCSC: TEdit
            Left = 116
            Top = 154
            Width = 100
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 5
          end
        end
      end
      object tsSMCPU: TTabSheet
        Caption = ' Processors '
        ImageIndex = 3
        object Panel33: TPanel
          Left = 0
          Top = 0
          Width = 674
          Height = 337
          Align = alClient
          BevelInner = bvLowered
          BevelOuter = bvNone
          BorderWidth = 3
          Caption = 'Panel7'
          TabOrder = 0
          object lvProcs: TListView
            Left = 4
            Top = 4
            Width = 666
            Height = 329
            Align = alClient
            BorderStyle = bsNone
            Columns = <
              item
                Caption = 'Vendor'
                Width = 100
              end
              item
                Caption = 'Vendor ID'
                Width = 100
              end
              item
                Caption = 'Socket'
                Width = 75
              end
              item
                Caption = 'Upgrade Intf'
                Width = 75
              end
              item
                Alignment = taRightJustify
                Caption = 'Voltage'
              end
              item
                Alignment = taRightJustify
                Caption = 'Frequency'
                Width = 75
              end
              item
                Alignment = taRightJustify
                Caption = 'External Clock'
                Width = 80
              end
              item
                Caption = 'Serial Number'
                Width = 100
              end
              item
                Caption = 'Asset Tag'
                Width = 100
              end
              item
                Caption = 'Part Number'
                Width = 100
              end
              item
                Alignment = taRightJustify
                Caption = 'Core Count'
                Width = 75
              end
              item
                Alignment = taRightJustify
                Caption = 'Thread Count'
                Width = 75
              end>
            ColumnClick = False
            FlatScrollBars = True
            HideSelection = False
            ReadOnly = True
            RowSelect = True
            TabOrder = 0
            ViewStyle = vsReport
          end
        end
      end
      object tsSMCaches: TTabSheet
        Caption = ' Caches '
        ImageIndex = 8
        object Panel32: TPanel
          Left = 0
          Top = 0
          Width = 674
          Height = 337
          Align = alClient
          BevelInner = bvLowered
          BevelOuter = bvNone
          BorderWidth = 3
          Caption = 'Panel7'
          TabOrder = 0
          object lvCache: TListView
            Left = 4
            Top = 4
            Width = 666
            Height = 329
            Align = alClient
            BorderStyle = bsNone
            Columns = <
              item
                Caption = 'Socket'
                Width = 100
              end
              item
                Caption = 'Type'
                Width = 75
              end
              item
                Caption = 'Associativity'
                Width = 100
              end
              item
                Caption = 'SRAM'
                Width = 100
              end
              item
                Alignment = taRightJustify
                Caption = 'Size'
                Width = 60
              end
              item
                Alignment = taRightJustify
                Caption = 'Max Size'
                Width = 60
              end
              item
                Alignment = taRightJustify
                Caption = 'Speed'
              end>
            ColumnClick = False
            FlatScrollBars = True
            HideSelection = False
            ReadOnly = True
            RowSelect = True
            TabOrder = 0
            ViewStyle = vsReport
          end
        end
      end
      object tsSMMem: TTabSheet
        Caption = ' Memory Modules '
        ImageIndex = 4
        object Panel26: TPanel
          Left = 0
          Top = 0
          Width = 674
          Height = 337
          Align = alClient
          BevelInner = bvLowered
          BevelOuter = bvNone
          BorderWidth = 3
          Caption = 'Panel7'
          TabOrder = 0
          object lvMem: TListView
            Left = 4
            Top = 4
            Width = 666
            Height = 329
            Align = alClient
            BorderStyle = bsNone
            Columns = <
              item
                Caption = 'Bank'
                Width = 100
              end
              item
                Caption = 'Form Factor'
                Width = 100
              end
              item
                Caption = 'Type'
                Width = 100
              end
              item
                Caption = 'Device'
                Width = 100
              end
              item
                Alignment = taRightJustify
                Caption = 'Size'
              end
              item
                Alignment = taRightJustify
                Caption = 'Speed'
              end>
            ColumnClick = False
            FlatScrollBars = True
            HideSelection = False
            ReadOnly = True
            RowSelect = True
            TabOrder = 0
            ViewStyle = vsReport
          end
        end
      end
      object tsMemDev: TTabSheet
        Caption = ' Memory Devices '
        ImageIndex = 13
        object Panel40: TPanel
          Left = 0
          Top = 0
          Width = 674
          Height = 337
          Align = alClient
          BevelInner = bvLowered
          BevelOuter = bvNone
          BorderWidth = 3
          Caption = 'Panel7'
          TabOrder = 0
          object lvMemDev: TListView
            Left = 4
            Top = 4
            Width = 666
            Height = 329
            Align = alClient
            BorderStyle = bsNone
            Columns = <
              item
                Caption = 'Device Locator'
                Width = 85
              end
              item
                Caption = 'Bank Locator'
                Width = 80
              end
              item
                Caption = 'Device'
                Width = 75
              end
              item
                Caption = 'Details'
                Width = 100
              end
              item
                Caption = 'Form Factor'
                Width = 100
              end
              item
                Alignment = taRightJustify
                Caption = 'Size'
                Width = 60
              end
              item
                Alignment = taRightJustify
                Caption = 'Speed'
                Width = 75
              end
              item
                Alignment = taRightJustify
                Caption = 'Max Speed'
                Width = 75
              end
              item
                Alignment = taRightJustify
                Caption = 'Total Width'
                Width = 70
              end
              item
                Alignment = taRightJustify
                Caption = 'Data Width'
                Width = 70
              end
              item
                Caption = 'Manufacturer'
                Width = 100
              end
              item
                Caption = 'Serial Number'
                Width = 100
              end
              item
                Caption = 'Asset Tag'
                Width = 75
              end
              item
                Caption = 'Part Number'
                Width = 75
              end
              item
                Caption = 'Technology'
                Width = 100
              end>
            ColumnClick = False
            FlatScrollBars = True
            HideSelection = False
            ReadOnly = True
            RowSelect = True
            TabOrder = 0
            ViewStyle = vsReport
          end
        end
      end
      object tsSMPort: TTabSheet
        Caption = ' Ports '
        ImageIndex = 5
        object Panel27: TPanel
          Left = 0
          Top = 0
          Width = 674
          Height = 337
          Align = alClient
          BevelInner = bvLowered
          BevelOuter = bvNone
          BorderWidth = 3
          Caption = 'Panel7'
          TabOrder = 0
          object lvPort: TListView
            Left = 4
            Top = 4
            Width = 666
            Height = 329
            Align = alClient
            BorderStyle = bsNone
            Columns = <
              item
                Caption = 'Type'
                Width = 100
              end
              item
                Caption = 'Internal Designator'
                Width = 150
              end
              item
                Caption = 'Internal Connector'
                Width = 150
              end
              item
                Caption = 'External Designator'
                Width = 150
              end
              item
                Caption = 'External Connector'
                Width = 150
              end>
            ColumnClick = False
            FlatScrollBars = True
            HideSelection = False
            ReadOnly = True
            RowSelect = True
            TabOrder = 0
            ViewStyle = vsReport
          end
        end
      end
      object tsSMSlot: TTabSheet
        Caption = ' Slots '
        ImageIndex = 6
        object Panel28: TPanel
          Left = 0
          Top = 0
          Width = 674
          Height = 337
          Align = alClient
          BevelInner = bvLowered
          BevelOuter = bvNone
          BorderWidth = 3
          Caption = 'Panel7'
          TabOrder = 0
          object lvSlot: TListView
            Left = 4
            Top = 4
            Width = 666
            Height = 329
            Align = alClient
            BorderStyle = bsNone
            Columns = <
              item
                Caption = 'Type'
                Width = 150
              end
              item
                Caption = 'Data Bus'
                Width = 100
              end
              item
                Caption = 'Current Usage'
                Width = 100
              end
              item
                Caption = 'Length'
              end
              item
                Alignment = taRightJustify
                Caption = 'Bus'
              end
              item
                Alignment = taRightJustify
                Caption = 'Device'
                Width = 60
              end
              item
                Alignment = taRightJustify
                Caption = 'Function'
                Width = 60
              end>
            ColumnClick = False
            FlatScrollBars = True
            HideSelection = False
            ReadOnly = True
            RowSelect = True
            TabOrder = 0
            ViewStyle = vsReport
          end
        end
      end
      object tsOBD: TTabSheet
        Caption = ' On-Board Devices'
        object Panel1: TPanel
          Left = 0
          Top = 0
          Width = 674
          Height = 337
          Align = alClient
          BevelInner = bvLowered
          BevelOuter = bvNone
          BorderWidth = 3
          Caption = 'Panel7'
          TabOrder = 0
          object lvOBD: TListView
            Left = 4
            Top = 4
            Width = 666
            Height = 329
            Align = alClient
            BorderStyle = bsNone
            Columns = <
              item
                Caption = 'Device'
                Width = 100
              end
              item
                Caption = 'Type'
                Width = 100
              end
              item
                Caption = 'Status'
                Width = 75
              end>
            ColumnClick = False
            FlatScrollBars = True
            HideSelection = False
            ReadOnly = True
            RowSelect = True
            TabOrder = 0
            ViewStyle = vsReport
          end
        end
      end
      object tsPMA: TTabSheet
        Caption = 'Physical Memory Array'
        ImageIndex = 17
        object Label10: TLabel
          Left = 115
          Top = 20
          Width = 44
          Height = 13
          Alignment = taRightJustify
          Caption = 'Location:'
        end
        object Label11: TLabel
          Left = 137
          Top = 47
          Width = 22
          Height = 13
          Alignment = taRightJustify
          Caption = 'Use:'
        end
        object Label12: TLabel
          Left = 37
          Top = 74
          Width = 122
          Height = 13
          Alignment = taRightJustify
          Caption = 'Memory Error Correction:'
        end
        object Label14: TLabel
          Left = 66
          Top = 101
          Width = 93
          Height = 13
          Alignment = taRightJustify
          Caption = 'Maximum Capacity:'
        end
        object Label15: TLabel
          Left = 275
          Top = 101
          Width = 12
          Height = 13
          Caption = 'KB'
        end
        object Label16: TLabel
          Left = 24
          Top = 128
          Width = 135
          Height = 13
          Alignment = taRightJustify
          Caption = 'Number of Memory Devices:'
        end
        object ePMALoc: TEdit
          Left = 169
          Top = 16
          Width = 200
          Height = 21
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 0
        end
        object ePMAUse: TEdit
          Left = 169
          Top = 43
          Width = 200
          Height = 21
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 1
        end
        object ePMAECT: TEdit
          Left = 169
          Top = 70
          Width = 200
          Height = 21
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 2
        end
        object ePMAMC: TEdit
          Left = 169
          Top = 97
          Width = 100
          Height = 21
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 3
        end
        object ePMADN: TEdit
          Left = 169
          Top = 124
          Width = 100
          Height = 21
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 4
        end
      end
      object tsVP: TTabSheet
        Caption = ' Voltage Probes '
        ImageIndex = 11
        object Panel6: TPanel
          Left = 0
          Top = 0
          Width = 674
          Height = 337
          Align = alClient
          BevelInner = bvLowered
          BevelOuter = bvNone
          BorderWidth = 3
          Caption = 'Panel7'
          TabOrder = 0
          object lvVP: TListView
            Left = 4
            Top = 4
            Width = 666
            Height = 329
            Align = alClient
            BorderStyle = bsNone
            Columns = <
              item
                Caption = 'Description'
                Width = 100
              end
              item
                Caption = 'Location'
                Width = 100
              end
              item
                Caption = 'Status'
                Width = 75
              end
              item
                Alignment = taRightJustify
                Caption = 'Nominal'
              end
              item
                Alignment = taCenter
                Caption = 'Range'
                Width = 100
              end
              item
                Alignment = taRightJustify
                Caption = 'Resolution'
                Width = 75
              end
              item
                Alignment = taRightJustify
                Caption = 'Tolerance'
                Width = 75
              end
              item
                Alignment = taRightJustify
                Caption = 'Accuracy'
                Width = 75
              end>
            ColumnClick = False
            FlatScrollBars = True
            HideSelection = False
            ReadOnly = True
            RowSelect = True
            TabOrder = 0
            ViewStyle = vsReport
          end
        end
      end
      object tsTP: TTabSheet
        Caption = ' Temperature Probes '
        ImageIndex = 12
        object Panel4: TPanel
          Left = 0
          Top = 0
          Width = 674
          Height = 337
          Align = alClient
          BevelInner = bvLowered
          BevelOuter = bvNone
          BorderWidth = 3
          Caption = 'Panel7'
          TabOrder = 0
          object lvTP: TListView
            Left = 4
            Top = 4
            Width = 666
            Height = 329
            Align = alClient
            BorderStyle = bsNone
            Columns = <
              item
                Caption = 'Description'
                Width = 100
              end
              item
                Caption = 'Location'
                Width = 100
              end
              item
                Caption = 'Status'
                Width = 75
              end
              item
                Alignment = taRightJustify
                Caption = 'Nominal'
              end
              item
                Alignment = taCenter
                Caption = 'Range'
                Width = 100
              end
              item
                Alignment = taRightJustify
                Caption = 'Resolution'
                Width = 75
              end
              item
                Alignment = taRightJustify
                Caption = 'Tolerance'
                Width = 75
              end
              item
                Alignment = taRightJustify
                Caption = 'Accuracy'
                Width = 75
              end>
            ColumnClick = False
            FlatScrollBars = True
            HideSelection = False
            ReadOnly = True
            RowSelect = True
            TabOrder = 0
            ViewStyle = vsReport
          end
        end
      end
      object tsCP: TTabSheet
        Caption = ' Electrical Current Probes '
        ImageIndex = 14
        object Panel7: TPanel
          Left = 0
          Top = 0
          Width = 674
          Height = 337
          Align = alClient
          BevelInner = bvLowered
          BevelOuter = bvNone
          BorderWidth = 3
          Caption = 'Panel7'
          TabOrder = 0
          object lvCP: TListView
            Left = 4
            Top = 4
            Width = 666
            Height = 329
            Align = alClient
            BorderStyle = bsNone
            Columns = <
              item
                Caption = 'Description'
                Width = 100
              end
              item
                Caption = 'Location'
                Width = 100
              end
              item
                Caption = 'Status'
                Width = 75
              end
              item
                Alignment = taRightJustify
                Caption = 'Nominal'
              end
              item
                Alignment = taCenter
                Caption = 'Range'
                Width = 100
              end
              item
                Alignment = taRightJustify
                Caption = 'Resolution'
                Width = 75
              end
              item
                Alignment = taRightJustify
                Caption = 'Tolerance'
                Width = 75
              end
              item
                Alignment = taRightJustify
                Caption = 'Accuracy'
                Width = 75
              end>
            ColumnClick = False
            FlatScrollBars = True
            HideSelection = False
            ReadOnly = True
            RowSelect = True
            TabOrder = 0
            ViewStyle = vsReport
          end
        end
      end
      object tsCD: TTabSheet
        Caption = ' Cooling Devices '
        ImageIndex = 14
        object Panel5: TPanel
          Left = 0
          Top = 0
          Width = 674
          Height = 337
          Align = alClient
          BevelInner = bvLowered
          BevelOuter = bvNone
          BorderWidth = 3
          Caption = 'Panel7'
          TabOrder = 0
          object lvCD: TListView
            Left = 4
            Top = 4
            Width = 666
            Height = 329
            Align = alClient
            BorderStyle = bsNone
            Columns = <
              item
                Caption = 'Description'
                Width = 150
              end
              item
                Caption = 'Type'
                Width = 150
              end
              item
                Caption = 'Status'
                Width = 100
              end
              item
                Alignment = taRightJustify
                Caption = 'Nominal Speed'
                Width = 100
              end
              item
                Alignment = taRightJustify
                Caption = 'Group Unit'
                Width = 70
              end>
            ColumnClick = False
            FlatScrollBars = True
            HideSelection = False
            ReadOnly = True
            RowSelect = True
            TabOrder = 0
            ViewStyle = vsReport
          end
        end
      end
      object tsOBDX: TTabSheet
        Caption = 'On-Board Devices Extended'
        ImageIndex = 18
        object lvOBDX: TListView
          Left = 0
          Top = 0
          Width = 674
          Height = 337
          Align = alClient
          BorderStyle = bsNone
          Columns = <
            item
              Caption = 'Device'
              Width = 200
            end
            item
              Caption = 'Type'
              Width = 100
            end
            item
              Caption = 'Status'
              Width = 75
            end
            item
              Alignment = taRightJustify
              Caption = 'Instance'
            end
            item
              Alignment = taRightJustify
              Caption = 'Segment Group'
            end
            item
              Alignment = taRightJustify
              Caption = 'Bus'
            end
            item
              Alignment = taRightJustify
              Caption = 'Device'
            end
            item
              Alignment = taRightJustify
              Caption = 'Function'
            end>
          ColumnClick = False
          FlatScrollBars = True
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          TabOrder = 0
          ViewStyle = vsReport
        end
      end
      object tsSPS: TTabSheet
        Caption = 'System Power Supply'
        ImageIndex = 19
        object Label13: TLabel
          Left = 123
          Top = 39
          Width = 44
          Height = 13
          Alignment = taRightJustify
          Caption = 'Location:'
        end
        object Label17: TLabel
          Left = 79
          Top = 12
          Width = 88
          Height = 13
          Alignment = taRightJustify
          Caption = 'Power Unit Group:'
        end
        object Label18: TLabel
          Left = 101
          Top = 66
          Width = 66
          Height = 13
          Alignment = taRightJustify
          Caption = 'Device Name:'
        end
        object Label19: TLabel
          Left = 41
          Top = 228
          Width = 126
          Height = 13
          Alignment = taRightJustify
          Caption = 'Maximum Power Capacity:'
        end
        object Label20: TLabel
          Left = 283
          Top = 228
          Width = 18
          Height = 13
          Caption = 'mW'
        end
        object Label21: TLabel
          Left = 98
          Top = 93
          Width = 69
          Height = 13
          Alignment = taRightJustify
          Caption = 'Manufacturer:'
        end
        object Label22: TLabel
          Left = 97
          Top = 120
          Width = 70
          Height = 13
          Alignment = taRightJustify
          Caption = 'Serial Number:'
        end
        object Label23: TLabel
          Left = 75
          Top = 147
          Width = 92
          Height = 13
          Alignment = taRightJustify
          Caption = 'Asset Tag Number:'
        end
        object Label24: TLabel
          Left = 72
          Top = 174
          Width = 95
          Height = 13
          Alignment = taRightJustify
          Caption = 'Model Part Number:'
        end
        object Label25: TLabel
          Left = 95
          Top = 201
          Width = 72
          Height = 13
          Alignment = taRightJustify
          Caption = 'Revision Level:'
        end
        object eSPSLoc: TEdit
          Left = 177
          Top = 35
          Width = 200
          Height = 21
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 0
        end
        object eSPSPUG: TEdit
          Left = 177
          Top = 8
          Width = 44
          Height = 21
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 1
        end
        object eSPSDN: TEdit
          Left = 177
          Top = 62
          Width = 200
          Height = 21
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 2
        end
        object eSPSMPC: TEdit
          Left = 177
          Top = 224
          Width = 100
          Height = 21
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 8
        end
        object eSPSM: TEdit
          Left = 177
          Top = 89
          Width = 200
          Height = 21
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 3
        end
        object eSPSSN: TEdit
          Left = 177
          Top = 116
          Width = 200
          Height = 21
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 4
        end
        object eSPSATN: TEdit
          Left = 177
          Top = 143
          Width = 200
          Height = 21
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 5
        end
        object eSPSMPN: TEdit
          Left = 177
          Top = 170
          Width = 200
          Height = 21
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 6
        end
        object eSPSRL: TEdit
          Left = 177
          Top = 197
          Width = 200
          Height = 21
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 7
        end
      end
      object tsPB: TTabSheet
        Caption = 'Portable Battery'
        ImageIndex = 21
        object lvPB: TListView
          Left = 0
          Top = 0
          Width = 674
          Height = 337
          Align = alClient
          BorderStyle = bsNone
          Columns = <
            item
              Caption = 'Location'
              Width = 100
            end
            item
              Caption = 'Manufacturer'
              Width = 100
            end
            item
              Caption = 'Manufacturer date'
              Width = 100
            end
            item
              Caption = 'Serial number'
              Width = 100
            end
            item
              Caption = 'Device name'
              Width = 100
            end
            item
              Caption = 'Chemistry'
              Width = 100
            end
            item
              Alignment = taRightJustify
              Caption = 'Capacity'
              Width = 75
            end
            item
              Alignment = taRightJustify
              Caption = 'Voltage'
              Width = 75
            end>
          ColumnClick = False
          FlatScrollBars = True
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          TabOrder = 0
          ViewStyle = vsReport
        end
      end
      object tsTPM: TTabSheet
        Caption = 'TPM Devices'
        ImageIndex = 20
        object lvTPM: TListView
          Left = 0
          Top = 0
          Width = 674
          Height = 337
          Align = alClient
          BorderStyle = bsNone
          Columns = <
            item
              Caption = 'VendorID'
              Width = 75
            end
            item
              Caption = 'Spec Version'
              Width = 75
            end
            item
              Caption = 'Description'
              Width = 300
            end>
          ColumnClick = False
          FlatScrollBars = True
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          TabOrder = 0
          ViewStyle = vsReport
        end
      end
      object tsSMTables: TTabSheet
        Caption = ' Available Tables '
        ImageIndex = 7
        object Panel29: TPanel
          Left = 0
          Top = 0
          Width = 674
          Height = 337
          Align = alClient
          BevelInner = bvLowered
          BevelOuter = bvNone
          BorderWidth = 3
          Caption = 'Panel7'
          TabOrder = 0
          object lvTables: TListView
            Left = 4
            Top = 4
            Width = 666
            Height = 329
            Align = alClient
            BorderStyle = bsNone
            Columns = <
              item
                Caption = 'Table Type'
                Width = 215
              end
              item
                Alignment = taRightJustify
                Caption = 'Length'
                Width = 45
              end
              item
                Alignment = taRightJustify
                Caption = 'Handle'
                Width = 45
              end>
            ColumnClick = False
            FlatScrollBars = True
            HideSelection = False
            ReadOnly = True
            RowSelect = True
            TabOrder = 0
            ViewStyle = vsReport
          end
        end
      end
    end
    object eSMTables: TEdit
      Left = 616
      Top = 11
      Width = 66
      Height = 21
      Anchors = [akTop, akRight]
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 2
    end
  end
end
