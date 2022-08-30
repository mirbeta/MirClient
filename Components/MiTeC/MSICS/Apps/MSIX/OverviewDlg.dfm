object dlg_msi_Overview: Tdlg_msi_Overview
  Left = 714
  Top = 285
  Caption = 'MiTeC System Information'
  ClientHeight = 644
  ClientWidth = 710
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label82: TLabel
    Left = 14
    Top = 101
    Width = 33
    Height = 13
    Caption = 'Market'
  end
  object Label87: TLabel
    Left = 14
    Top = 123
    Width = 36
    Height = 13
    Caption = 'Generic'
  end
  object Panel1: TPanel
    Left = 0
    Top = 607
    Width = 710
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Panel2: TPanel
      Left = 544
      Top = 0
      Width = 166
      Height = 37
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object bRefresh: TButton
        Left = 4
        Top = 4
        Width = 75
        Height = 25
        Cursor = crHandPoint
        Caption = 'Refresh'
        TabOrder = 0
        OnClick = cmRefresh
      end
      object bOK: TButton
        Left = 85
        Top = 4
        Width = 75
        Height = 25
        Cursor = crHandPoint
        Cancel = True
        Caption = 'OK'
        Default = True
        ModalResult = 1
        TabOrder = 1
        OnClick = cmClose
      end
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 710
    Height = 607
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 5
    TabOrder = 1
    object pc: TPageControl
      Left = 5
      Top = 5
      Width = 700
      Height = 597
      Cursor = crHandPoint
      ActivePage = tsWksta
      Align = alClient
      MultiLine = True
      TabOrder = 0
      object tsWksta: TTabSheet
        Caption = ' Machine '
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          692
          533)
        object pNumLock: TPanel
          Left = 442
          Top = 509
          Width = 77
          Height = 20
          Anchors = [akRight, akBottom]
          BevelKind = bkFlat
          BevelOuter = bvNone
          Caption = 'Num Lock'
          TabOrder = 0
        end
        object pCapsLock: TPanel
          Left = 524
          Top = 509
          Width = 77
          Height = 20
          Anchors = [akRight, akBottom]
          BevelKind = bkFlat
          BevelOuter = bvNone
          Caption = 'Caps Lock'
          TabOrder = 1
        end
        object pScrollLock: TPanel
          Left = 606
          Top = 509
          Width = 77
          Height = 20
          Anchors = [akRight, akBottom]
          BevelKind = bkFlat
          BevelOuter = bvNone
          Caption = 'Scroll Lock'
          TabOrder = 2
        end
        object eMachine: TEdit
          Left = 16
          Top = 8
          Width = 668
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          ReadOnly = True
          TabOrder = 3
        end
        object pcMachine: TPageControl
          Left = 11
          Top = 35
          Width = 677
          Height = 463
          Cursor = crHandPoint
          ActivePage = tsMachineSMBIOS
          Anchors = [akLeft, akTop, akRight, akBottom]
          TabOrder = 4
          object tsMachineGeneral: TTabSheet
            Caption = ' General '
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            DesignSize = (
              669
              435)
            object gbMID: TGroupBox
              Left = 13
              Top = 3
              Width = 643
              Height = 116
              Anchors = [akLeft, akTop, akRight]
              Caption = ' Identification '
              TabOrder = 0
              DesignSize = (
                643
                116)
              object Label1: TLabel
                Left = 70
                Top = 19
                Width = 68
                Height = 13
                Alignment = taRightJustify
                Caption = 'Machine name'
              end
              object Label2: TLabel
                Left = 87
                Top = 66
                Width = 51
                Height = 13
                Alignment = taRightJustify
                Caption = 'User name'
              end
              object icoComputer: TImage
                Left = 14
                Top = 24
                Width = 32
                Height = 32
                AutoSize = True
                Center = True
                Transparent = True
              end
              object lJoinName: TLabel
                Left = 103
                Top = 43
                Width = 35
                Height = 13
                Alignment = taRightJustify
                Caption = 'Domain'
              end
              object Label115: TLabel
                Left = 102
                Top = 90
                Width = 36
                Height = 13
                Alignment = taRightJustify
                Caption = 'Session'
              end
              object eWksta: TEdit
                Left = 148
                Top = 15
                Width = 478
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                Color = clBtnFace
                ReadOnly = True
                TabOrder = 0
              end
              object eUser: TEdit
                Left = 148
                Top = 62
                Width = 478
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                Color = clBtnFace
                ReadOnly = True
                TabOrder = 2
              end
              object eDomain: TEdit
                Left = 148
                Top = 39
                Width = 478
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                Color = clBtnFace
                ReadOnly = True
                TabOrder = 1
              end
              object eSes: TEdit
                Left = 148
                Top = 86
                Width = 478
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                Color = clBtnFace
                ReadOnly = True
                TabOrder = 3
              end
            end
            object GroupBox2: TGroupBox
              Left = 13
              Top = 125
              Width = 643
              Height = 220
              Anchors = [akLeft, akTop, akRight, akBottom]
              Caption = ' BIOS information '
              TabOrder = 1
              DesignSize = (
                643
                220)
              object Label97: TLabel
                Left = 8
                Top = 22
                Width = 47
                Height = 13
                Alignment = taRightJustify
                Caption = 'Copyright'
              end
              object Label98: TLabel
                Left = 28
                Top = 44
                Width = 27
                Height = 13
                Alignment = taRightJustify
                Caption = 'Name'
              end
              object Label99: TLabel
                Left = 514
                Top = 44
                Width = 23
                Height = 13
                Alignment = taRightJustify
                Anchors = [akTop, akRight]
                Caption = 'Date'
                ExplicitLeft = 462
              end
              object Label100: TLabel
                Left = 16
                Top = 69
                Width = 39
                Height = 13
                Alignment = taRightJustify
                Caption = 'Ext Info'
              end
              object eSBCopy: TEdit
                Left = 63
                Top = 18
                Width = 568
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                Color = clBtnFace
                ReadOnly = True
                TabOrder = 0
              end
              object eSBName: TEdit
                Left = 63
                Top = 40
                Width = 440
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                Color = clBtnFace
                ReadOnly = True
                TabOrder = 1
              end
              object eSBDate: TEdit
                Left = 541
                Top = 40
                Width = 90
                Height = 21
                Anchors = [akTop, akRight]
                Color = clBtnFace
                ReadOnly = True
                TabOrder = 2
              end
              object eSBExt: TEdit
                Left = 63
                Top = 63
                Width = 568
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                Color = clBtnFace
                ReadOnly = True
                TabOrder = 3
              end
              object BIOSList: TListView
                Left = 11
                Top = 95
                Width = 620
                Height = 115
                Anchors = [akLeft, akTop, akRight, akBottom]
                Columns = <
                  item
                    Caption = 'Property'
                    Width = 200
                  end
                  item
                    Caption = 'Value'
                    Width = 310
                  end>
                ColumnClick = False
                ReadOnly = True
                RowSelect = True
                SortType = stText
                TabOrder = 4
                ViewStyle = vsReport
              end
            end
            object GroupBox3: TGroupBox
              Left = 13
              Top = 351
              Width = 643
              Height = 75
              Anchors = [akLeft, akRight, akBottom]
              Caption = '  Boot information '
              TabOrder = 2
              DesignSize = (
                643
                75)
              object Label7: TLabel
                Left = 51
                Top = 23
                Width = 45
                Height = 13
                Alignment = taRightJustify
                Caption = 'Last boot'
              end
              object Label8: TLabel
                Left = 22
                Top = 49
                Width = 74
                Height = 13
                Alignment = taRightJustify
                Caption = 'System Up time'
              end
              object Label48: TLabel
                Left = 299
                Top = 23
                Width = 70
                Height = 13
                Alignment = taRightJustify
                Caption = 'Last shutdown'
              end
              object eLastBoot: TEdit
                Left = 106
                Top = 19
                Width = 184
                Height = 21
                Color = clBtnFace
                ReadOnly = True
                TabOrder = 0
              end
              object eSysTime: TEdit
                Left = 106
                Top = 45
                Width = 518
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                Color = clBtnFace
                ReadOnly = True
                TabOrder = 2
              end
              object eLastShut: TEdit
                Left = 379
                Top = 19
                Width = 184
                Height = 21
                Color = clBtnFace
                ReadOnly = True
                TabOrder = 1
              end
            end
          end
          object tsMachineSMBIOS: TTabSheet
            Caption = 'SM BIOS'
            ImageIndex = 1
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object Panel25: TPanel
              Left = 0
              Top = 0
              Width = 669
              Height = 435
              Align = alClient
              BevelOuter = bvNone
              BorderWidth = 5
              Caption = ' '
              TabOrder = 0
              DesignSize = (
                669
                435)
              object Label60: TLabel
                Left = 7
                Top = 15
                Width = 35
                Height = 13
                Alignment = taRightJustify
                Caption = 'Version'
              end
              object Bevel7: TBevel
                Left = 0
                Top = 35
                Width = 665
                Height = 5
                Anchors = [akLeft, akTop, akRight]
                Shape = bsTopLine
                ExplicitWidth = 613
              end
              object Label66: TLabel
                Left = 478
                Top = 15
                Width = 106
                Height = 13
                Alignment = taRightJustify
                Anchors = [akTop, akRight]
                Caption = 'Structure Table Count'
                ExplicitLeft = 464
              end
              object eSMVer: TEdit
                Left = 51
                Top = 11
                Width = 100
                Height = 21
                Color = clBtnFace
                ReadOnly = True
                TabOrder = 0
              end
              object pcSM: TPageControl
                Left = 5
                Top = 46
                Width = 659
                Height = 384
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
                  ExplicitLeft = 0
                  ExplicitTop = 0
                  ExplicitWidth = 0
                  ExplicitHeight = 0
                  object GroupBox21: TGroupBox
                    Left = 0
                    Top = 0
                    Width = 651
                    Height = 302
                    Align = alClient
                    TabOrder = 0
                    DesignSize = (
                      651
                      302)
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
                      Left = 189
                      Top = 104
                      Width = 12
                      Height = 13
                      Caption = 'KB'
                    end
                    object Label117: TLabel
                      Left = 12
                      Top = 143
                      Width = 73
                      Height = 13
                      Alignment = taRightJustify
                      Caption = 'System Version'
                    end
                    object Label118: TLabel
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
                      Width = 250
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
                      Left = 10
                      Top = 173
                      Width = 627
                      Height = 120
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
                  ExplicitLeft = 0
                  ExplicitTop = 0
                  ExplicitWidth = 0
                  ExplicitHeight = 0
                  object GroupBox4: TGroupBox
                    Left = 0
                    Top = 0
                    Width = 651
                    Height = 302
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
                  ExplicitLeft = 0
                  ExplicitTop = 0
                  ExplicitWidth = 0
                  ExplicitHeight = 0
                  object GroupBox15: TGroupBox
                    Left = 0
                    Top = 0
                    Width = 651
                    Height = 302
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
                    object Label84: TLabel
                      Left = 58
                      Top = 130
                      Width = 48
                      Height = 13
                      Alignment = taRightJustify
                      Caption = 'Asset Tag'
                    end
                    object Label85: TLabel
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
                  ExplicitLeft = 0
                  ExplicitTop = 0
                  ExplicitWidth = 0
                  ExplicitHeight = 0
                  object GroupBox18: TGroupBox
                    Left = 0
                    Top = 0
                    Width = 651
                    Height = 302
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
                    object Label83: TLabel
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
                  ExplicitLeft = 0
                  ExplicitTop = 0
                  ExplicitWidth = 0
                  ExplicitHeight = 0
                  object GroupBox22: TGroupBox
                    Left = 0
                    Top = 0
                    Width = 651
                    Height = 302
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
                  ExplicitLeft = 0
                  ExplicitTop = 0
                  ExplicitWidth = 0
                  ExplicitHeight = 0
                  object lvProcs: TListView
                    Left = 0
                    Top = 0
                    Width = 651
                    Height = 302
                    Align = alClient
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
                        Width = 70
                      end
                      item
                        Alignment = taRightJustify
                        Caption = 'Thread Count'
                        Width = 70
                      end>
                    ColumnClick = False
                    HideSelection = False
                    ReadOnly = True
                    RowSelect = True
                    TabOrder = 0
                    ViewStyle = vsReport
                  end
                end
                object tsSMCaches: TTabSheet
                  Caption = ' Caches '
                  ImageIndex = 8
                  ExplicitLeft = 0
                  ExplicitTop = 0
                  ExplicitWidth = 0
                  ExplicitHeight = 0
                  object lvCache: TListView
                    Left = 0
                    Top = 0
                    Width = 651
                    Height = 302
                    Align = alClient
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
                    HideSelection = False
                    ReadOnly = True
                    RowSelect = True
                    TabOrder = 0
                    ViewStyle = vsReport
                  end
                end
                object tsPMA: TTabSheet
                  Caption = 'Physical Memory Array'
                  ImageIndex = 17
                  ExplicitLeft = 0
                  ExplicitTop = 0
                  ExplicitWidth = 0
                  ExplicitHeight = 0
                  object Label122: TLabel
                    Left = 115
                    Top = 20
                    Width = 44
                    Height = 13
                    Alignment = taRightJustify
                    Caption = 'Location:'
                  end
                  object Label123: TLabel
                    Left = 137
                    Top = 47
                    Width = 22
                    Height = 13
                    Alignment = taRightJustify
                    Caption = 'Use:'
                  end
                  object Label124: TLabel
                    Left = 37
                    Top = 74
                    Width = 122
                    Height = 13
                    Alignment = taRightJustify
                    Caption = 'Memory Error Correction:'
                  end
                  object Label125: TLabel
                    Left = 66
                    Top = 101
                    Width = 93
                    Height = 13
                    Alignment = taRightJustify
                    Caption = 'Maximum Capacity:'
                  end
                  object Label126: TLabel
                    Left = 275
                    Top = 101
                    Width = 12
                    Height = 13
                    Caption = 'KB'
                  end
                  object Label127: TLabel
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
                object tsSMMem: TTabSheet
                  Caption = ' Memory Modules '
                  ImageIndex = 4
                  ExplicitLeft = 0
                  ExplicitTop = 0
                  ExplicitWidth = 0
                  ExplicitHeight = 0
                  object lvMem: TListView
                    Left = 0
                    Top = 0
                    Width = 651
                    Height = 302
                    Align = alClient
                    Columns = <
                      item
                        Caption = 'Bank'
                        Width = 150
                      end
                      item
                        Caption = 'Type'
                        Width = 150
                      end
                      item
                        Alignment = taRightJustify
                        Caption = 'Size'
                        Width = 60
                      end
                      item
                        Alignment = taRightJustify
                        Caption = 'Speed'
                      end>
                    ColumnClick = False
                    HideSelection = False
                    ReadOnly = True
                    RowSelect = True
                    TabOrder = 0
                    ViewStyle = vsReport
                  end
                end
                object tsMemDev: TTabSheet
                  Caption = ' Memory Devices '
                  ImageIndex = 12
                  ExplicitLeft = 0
                  ExplicitTop = 0
                  ExplicitWidth = 0
                  ExplicitHeight = 0
                  object lvMemDev: TListView
                    Left = 0
                    Top = 0
                    Width = 651
                    Height = 302
                    Align = alClient
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
                    HideSelection = False
                    ReadOnly = True
                    RowSelect = True
                    TabOrder = 0
                    ViewStyle = vsReport
                  end
                end
                object tsSMPort: TTabSheet
                  Caption = ' Ports '
                  ImageIndex = 5
                  ExplicitLeft = 0
                  ExplicitTop = 0
                  ExplicitWidth = 0
                  ExplicitHeight = 0
                  object lvPort: TListView
                    Left = 0
                    Top = 0
                    Width = 651
                    Height = 302
                    Align = alClient
                    Columns = <
                      item
                        Caption = 'Type'
                        Width = 100
                      end
                      item
                        Caption = 'Internal Designator'
                        Width = 100
                      end
                      item
                        Caption = 'Internal Connector'
                        Width = 100
                      end
                      item
                        Caption = 'External Designator'
                        Width = 100
                      end
                      item
                        Caption = 'External Connector'
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
                object tsSMSlot: TTabSheet
                  Caption = ' Slots '
                  ImageIndex = 6
                  ExplicitLeft = 0
                  ExplicitTop = 0
                  ExplicitWidth = 0
                  ExplicitHeight = 0
                  object lvSlot: TListView
                    Left = 0
                    Top = 0
                    Width = 651
                    Height = 302
                    Align = alClient
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
                        Caption = 'Bus Number'
                        Width = 70
                      end
                      item
                        Alignment = taRightJustify
                        Caption = 'Device Number'
                        Width = 80
                      end
                      item
                        Alignment = taRightJustify
                        Caption = 'Function Number'
                        Width = 80
                      end>
                    ColumnClick = False
                    HideSelection = False
                    ReadOnly = True
                    RowSelect = True
                    TabOrder = 0
                    ViewStyle = vsReport
                  end
                end
                object tsOBD: TTabSheet
                  Caption = ' On-Board Devices'
                  ImageIndex = 11
                  ExplicitLeft = 0
                  ExplicitTop = 0
                  ExplicitWidth = 0
                  ExplicitHeight = 0
                  object lvOBD: TListView
                    Left = 0
                    Top = 0
                    Width = 651
                    Height = 302
                    Align = alClient
                    Columns = <
                      item
                        Caption = 'Device'
                        Width = 300
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
                    HideSelection = False
                    ReadOnly = True
                    RowSelect = True
                    TabOrder = 0
                    ViewStyle = vsReport
                  end
                end
                object tsVP: TTabSheet
                  Caption = ' Voltage Probes '
                  ImageIndex = 13
                  ExplicitLeft = 0
                  ExplicitTop = 0
                  ExplicitWidth = 0
                  ExplicitHeight = 0
                  object lvVP: TListView
                    Left = 0
                    Top = 0
                    Width = 651
                    Height = 302
                    Align = alClient
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
                object tsTP: TTabSheet
                  Caption = ' Temperature Probes '
                  ImageIndex = 14
                  ExplicitLeft = 0
                  ExplicitTop = 0
                  ExplicitWidth = 0
                  ExplicitHeight = 0
                  object lvTP: TListView
                    Left = 0
                    Top = 0
                    Width = 651
                    Height = 302
                    Align = alClient
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
                object tsCP: TTabSheet
                  Caption = ' Electrical Current Probes '
                  ImageIndex = 15
                  ExplicitLeft = 0
                  ExplicitTop = 0
                  ExplicitWidth = 0
                  ExplicitHeight = 0
                  object lvCP: TListView
                    Left = 0
                    Top = 0
                    Width = 651
                    Height = 302
                    Align = alClient
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
                object tsCD: TTabSheet
                  Caption = ' Cooling Devices '
                  ImageIndex = 16
                  ExplicitLeft = 0
                  ExplicitTop = 0
                  ExplicitWidth = 0
                  ExplicitHeight = 0
                  object lvCD: TListView
                    Left = 0
                    Top = 0
                    Width = 651
                    Height = 302
                    Align = alClient
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
                object tsOBDX: TTabSheet
                  Caption = 'On-Board Devices Extended'
                  ImageIndex = -1
                  ExplicitLeft = 0
                  ExplicitTop = 0
                  ExplicitWidth = 0
                  ExplicitHeight = 0
                  object lvOBDX: TListView
                    Left = 0
                    Top = 0
                    Width = 651
                    Height = 302
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
                  ExplicitLeft = 0
                  ExplicitTop = 0
                  ExplicitWidth = 0
                  ExplicitHeight = 0
                  object Label128: TLabel
                    Left = 123
                    Top = 39
                    Width = 44
                    Height = 13
                    Alignment = taRightJustify
                    Caption = 'Location:'
                  end
                  object Label129: TLabel
                    Left = 79
                    Top = 12
                    Width = 88
                    Height = 13
                    Alignment = taRightJustify
                    Caption = 'Power Unit Group:'
                  end
                  object Label130: TLabel
                    Left = 101
                    Top = 66
                    Width = 66
                    Height = 13
                    Alignment = taRightJustify
                    Caption = 'Device Name:'
                  end
                  object Label131: TLabel
                    Left = 41
                    Top = 228
                    Width = 126
                    Height = 13
                    Alignment = taRightJustify
                    Caption = 'Maximum Power Capacity:'
                  end
                  object Label132: TLabel
                    Left = 283
                    Top = 228
                    Width = 18
                    Height = 13
                    Caption = 'mW'
                  end
                  object Label133: TLabel
                    Left = 98
                    Top = 93
                    Width = 69
                    Height = 13
                    Alignment = taRightJustify
                    Caption = 'Manufacturer:'
                  end
                  object Label134: TLabel
                    Left = 97
                    Top = 120
                    Width = 70
                    Height = 13
                    Alignment = taRightJustify
                    Caption = 'Serial Number:'
                  end
                  object Label135: TLabel
                    Left = 75
                    Top = 147
                    Width = 92
                    Height = 13
                    Alignment = taRightJustify
                    Caption = 'Asset Tag Number:'
                  end
                  object Label136: TLabel
                    Left = 72
                    Top = 174
                    Width = 95
                    Height = 13
                    Alignment = taRightJustify
                    Caption = 'Model Part Number:'
                  end
                  object Label137: TLabel
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
                    TabOrder = 3
                  end
                  object eSPSM: TEdit
                    Left = 177
                    Top = 89
                    Width = 200
                    Height = 21
                    Color = clBtnFace
                    ReadOnly = True
                    TabOrder = 4
                  end
                  object eSPSSN: TEdit
                    Left = 177
                    Top = 116
                    Width = 200
                    Height = 21
                    Color = clBtnFace
                    ReadOnly = True
                    TabOrder = 5
                  end
                  object eSPSATN: TEdit
                    Left = 177
                    Top = 143
                    Width = 200
                    Height = 21
                    Color = clBtnFace
                    ReadOnly = True
                    TabOrder = 6
                  end
                  object eSPSMPN: TEdit
                    Left = 177
                    Top = 170
                    Width = 200
                    Height = 21
                    Color = clBtnFace
                    ReadOnly = True
                    TabOrder = 7
                  end
                  object eSPSRL: TEdit
                    Left = 177
                    Top = 197
                    Width = 200
                    Height = 21
                    Color = clBtnFace
                    ReadOnly = True
                    TabOrder = 8
                  end
                end
                object tsPB: TTabSheet
                  Caption = 'Portable Batteries'
                  ImageIndex = 21
                  ExplicitLeft = 0
                  ExplicitTop = 0
                  ExplicitWidth = 0
                  ExplicitHeight = 0
                  object lvPB: TListView
                    Left = 0
                    Top = 0
                    Width = 651
                    Height = 302
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
                  ExplicitLeft = 0
                  ExplicitTop = 0
                  ExplicitWidth = 0
                  ExplicitHeight = 0
                  object lvTPM: TListView
                    Left = 0
                    Top = 0
                    Width = 651
                    Height = 302
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
                  ExplicitLeft = 0
                  ExplicitTop = 0
                  ExplicitWidth = 0
                  ExplicitHeight = 0
                  object lvTables: TListView
                    Left = 0
                    Top = 0
                    Width = 651
                    Height = 302
                    Align = alClient
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
                    HideSelection = False
                    ReadOnly = True
                    RowSelect = True
                    TabOrder = 0
                    ViewStyle = vsReport
                  end
                end
              end
              object eSMTables: TEdit
                Left = 593
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
        end
      end
      object tsOS: TTabSheet
        Caption = ' Operating System'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          692
          533)
        object Image2: TImage
          Left = 20
          Top = 13
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
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            0000000000000000B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
            B1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFFB1763CFF
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
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            00000000FFFFFFFFFFFFFFFFFFFFFFFF80018001800180018001800180018001
            80018001800180018001800180018001800180018001800180018001FFFFFFFF
            FFFFFFFF80018001800180018001800180018001800180018001800180018001
            80018001800180018001800180018001FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFF}
          Transparent = True
        end
        object lEdition: TLabel
          Left = 82
          Top = 13
          Width = 148
          Height = 13
          Caption = 'Windows Millenium Edition'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Bevel1: TBevel
          Left = 9
          Top = 93
          Width = 673
          Height = 3
          Anchors = [akLeft, akTop, akRight]
          Shape = bsTopLine
          ExplicitWidth = 621
        end
        object Bevel2: TBevel
          Left = 10
          Top = 53
          Width = 673
          Height = 3
          Anchors = [akLeft, akTop, akRight]
          Shape = bsTopLine
          ExplicitWidth = 621
        end
        object lVersion: TLabel
          Left = 90
          Top = 32
          Width = 58
          Height = 13
          Caption = 'Version: 1.0'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label116: TLabel
          Left = 32
          Top = 68
          Width = 71
          Height = 13
          Alignment = taRightJustify
          Caption = 'Machine GUID:'
        end
        object GenuineIcon: TImage
          Left = 601
          Top = 0
          Width = 88
          Height = 47
          Anchors = [akTop, akRight]
          AutoSize = True
          Center = True
          Picture.Data = {
            0A544A504547496D6167653D0B0000FFD8FFE000104A46494600010101004800
            480000FFDB0043000302020302020303030304030304050805050404050A0707
            06080C0A0C0C0B0A0B0B0D0E12100D0E110E0B0B1016101113141515150C0F17
            1816141812141514FFDB00430103040405040509050509140D0B0D1414141414
            1414141414141414141414141414141414141414141414141414141414141414
            14141414141414141414141414FFC0001108002F005803012200021101031101
            FFC4001F0000010501010101010100000000000000000102030405060708090A
            0BFFC400B5100002010303020403050504040000017D01020300041105122131
            410613516107227114328191A1082342B1C11552D1F02433627282090A161718
            191A25262728292A3435363738393A434445464748494A535455565758595A63
            6465666768696A737475767778797A838485868788898A92939495969798999A
            A2A3A4A5A6A7A8A9AAB2B3B4B5B6B7B8B9BAC2C3C4C5C6C7C8C9CAD2D3D4D5D6
            D7D8D9DAE1E2E3E4E5E6E7E8E9EAF1F2F3F4F5F6F7F8F9FAFFC4001F01000301
            01010101010101010000000000000102030405060708090A0BFFC400B5110002
            0102040403040705040400010277000102031104052131061241510761711322
            328108144291A1B1C109233352F0156272D10A162434E125F11718191A262728
            292A35363738393A434445464748494A535455565758595A636465666768696A
            737475767778797A82838485868788898A92939495969798999AA2A3A4A5A6A7
            A8A9AAB2B3B4B5B6B7B8B9BAC2C3C4C5C6C7C8C9CAD2D3D4D5D6D7D8D9DAE2E3
            E4E5E6E7E8E9EAF2F3F4F5F6F7F8F9FAFFDA000C03010002110311003F00E3FC
            35E3AB7FECBB6F3638659DD77CB2C88ACEEE4E59998E492492493DCD767A778D
            AD988FDD4007B46BFE15F3DFC3CD135AF1AEB5A7E87A058CFAAEAB76C560B4B7
            C6E6C64B124F0AAA064B12001FAFA8EB1F0AFC67E04D124D675AD2961D192E23
            B3FED1B5BD86EA0799C90A91B46C779DC369C7420838AFD4A1EC60D464D26CFC
            D2BC6B4D3945368F558BC556F2C5FEA6DCFF00DB25FF000A827F135B807FD1E0
            FF00BF4BFE15CEDF7C39F16F847409F58D674892CAC6D5125BC1F698A4B8B247
            1F234F02B178836460B01D474AD9B9F835E3967B98FF00E11F97CE86DFED7E41
            BA844B343B43178537EE94004676838C81D78AEA53C2DAFCE8F239316A7CBC8C
            A771E2AB6C7FA883FEFD2FF8565DD78B2D42E7ECF6F8FF00AE2A4FFE83F4AE7F
            54D0F50B3D2343D526B764B1D715DF4D9BCC5C5CAA388DC819CAE1980E40AB1A
            D783B56F0BF877C796FAB784D64BFD16EAC6DEEF58935108DA2B48432A08D491
            319432F4CEDCF3D2AA7EC229352BDFFCEC7A18795696F1B7F571B77E31B519FF
            0046B53FF6C13FC2B1EEFC6F683ADB5A0EDFEA13FC38AD0D5FF675F8936516AA
            F3786258DF4D80DCCF6DF6B80CEF08018C91441F748801E4A8E3A727A723E1BF
            823E39F883A0C7AC681A30BCD3EE5DE3B2925BD82DDEFDD07CEB6C923AB4C57B
            ED07A71E95C951E15A6F9D5BD4F629FB7BAF749752F1ED9DA45BE6B5B68832EF
            4325BAA861CF425706A6F1BEBD69E17BD7825B4B38F1712C07CC8631F32246D8
            FBBD7F79CF5EC3DEB0FC2BE33D4E112694B9D39ACA095D9AE224B9B48BC95624
            DCDBDC2B84C15DA4A153BB1906B274BBCF15DB4BABEA7E226D557ED16F2EBBE5
            4D6F17DAAF5C95592485E5522321581765190AA303A1AF2561ABCABF354B7B3B
            3D9EEDB56FB927F7975F1B4A9D1E5A77E74D5D3F46B7F569FC8CFF0017F8CED2
            EBC3FA8412595B279B6CED1936CA8C0E0957538C8208C861DC515E7FE3FF0012
            4FE2BBA6B9920F24C709841F3E59E4900E7749239F98F6E02803000A2B0AB414
            6568A3B68D6A928DE7A33E80FD94FC5DA2681E25F13E9FADEB5178617C4BE1AB
            DD0ACB5EB96D9158DCC84142EE3EE06C6DDDF4F6AF5BF02697A6FC1AF8012497
            3AD68DE22FECCF887A25D5F58786E7FB5DAC010025164C047959577955CE30A1
            8E49C7C9FF000F7C5F2F837584D4A2D2F47D67F732C1258EB9662EAD6547E1B2
            991F37030410457A0F887E2CEB1E37D2B48D124B3D17C39E19D3AE45C5AE83E1
            DB0FB1D9ACCE42B4EE37334926D24658F03A735BD7C34AACF9969176BDFCBB13
            1AF1A30B4B7574BE7DCFA23E2778C61D367F88DE21F0CCFF000AEE2C3C576D73
            6E2FB4E9E67D735086E08FDD3C064C89812092E02AECC8C74AE8EDBC61E1D7FD
            ACF44F139D62CA5D174FB6B2B66D54CF98A055B074923DFD87987E6038DC79E6
            B9EB7F863E11B7F8C6753834054F01B6A23C3FFD8CB72491ABF9FF0066F20B67
            7942A3ED58249DBD6B8BF0FE8B1A784B4FD7355BC75D2A0D1A0BA9974BB18DAE
            5DA6D426B68948760AC73192D33E0050AB827159E1E185B59BE96DBBAFCD58E4
            C5BC65B9D2EA9EFDBFCEE74976346F17FC2AF86F9F15689A34FE179AFADF53B7
            D5AE4C5388A6BCF3A39E18C02D382ABC2AF39239EB57FE31F8D3C3DAD69DFB44
            AD86B5617675ED73439B4AF2A653F6D48845E698F1F7C26D3923A60D72F27877
            4DFECCB6BCB8D4AFB7268337892EA2834F46682D52668552325C6E9998293D15
            14B16638AA9AC7816D2DEC3529AD7509EFEED74DB5D5B4FD36382186E9ECE688
            C8669D5DC61232BB5C425D8021F1B6BABD96153B4AA3B2DB4FEF27FA1CD4EAE2
            E5EF2A6AEF777F2B7EA76ADE3AF0DB7EDBFAB78C24D7EC4E806CE5B54D69EE01
            8021D2C208FCCCF4F378DBD377B9AC8D3BC5BA4F89BC05F09F54D0E2F85A9ACF
            8434986CAED7C77772DA5EE993C521712C1875578DB87F90127D4E70297867E1
            6F87A4F88569617D7D7DA9D9691E28B3D075BB6974F58A29E6991DD3C83BF2D0
            968886DD862BB59410D5E77AF69DE13B9F0A785BC512C3AA4D7BAC6BDA925D5B
            2D9C16B0FD9A029858D11F1115F30602E7258E70066B0AB4B0B52718C5BD125B
            7A9EA51AB5E306E496ADBDCE0F5EF1BDDEA3E2BF126B7716F617771ABEA33EA3
            2FD9E32B6D24DBCB232AB7261DD890467A9D99C818AE6F4DF196A3A34D752492
            4B7F24D29BB0F3C9B9D2EBFE7B027D412AEBD1D5B07A02347548A26B999E08DA
            2819C98D2439655C9C026B9BBC8300FB57BEE1CB04974478D2A54EACE5292BB7
            B983ACC9A7C9A34C89606DF50323B19A39B10AA1248458F19181800EEED451A9
            47FB8978E361E3F0A2BCBAB1B4B53D8A4D25627B2936AAF38EBFCEBA5D32EB6B
            291DB07F1AE7B57D32EBC31AB5E693A9426DF50B199EDEE220CAE15D58860181
            C1191D6ACD85FAAE3391F857542A45C56A8E4AD4E5AE87A9E97E24D524BC5BB7
            D62FDAEC5E1D484ED70C5FED854A9B9CFF00CF5DA76EEEB8E2BB0F0E6BFA968D
            2413E9FAAEA3673DBDB35B4325B5C10F1C3BCB9897FD92C4B6DE7E624E335E4D
            A66AB10DBF39FC8D761A4EB36E7EF3E41EA0835D91F62F7B1E1D7F6E9DD367B2
            D9E8DA85B5CE9B7B67AFCB15D4A20B3D3F5381A660D2CDE63081244CE1431904
            8C4615D986D278A6EAFA1CBA8DB6A56561E26D5AF25D56E6DAD2EECEE3CD3FDA
            D7125819DBCC73EBB444117E42368E738AE534EF1497772DAADEABCB0FD9DC89
            E41BA2FF009E7C71B7BE2A4935886D89F27509E13BE393F772483E78C6226FF7
            93F84F55ED593A32A8EFCE97DC650C44295A3184BCCDED4FC2DE29BCBCB132F8
            A6EEE5B4D8DEE74B98DC4AF1DB9588ED31B9C796CDB64894FCCDFB99001B4572
            BAE782F50D23C217F07FC24D07F61D9DC349FD99E6B470DC5D47144D26C46EB2
            2ACC80301F3907A0506A9BEB42D91A3B6D4EEEDE378C42C914F2A83182C427FB
            B97738FF006DBFBC738D7BABB7D9AE211A85C79170FE64F16F62B330000671DC
            8000FA01E82A952A9195F9E36F43BE35E9497C12BF9B394BE833C9E87D7AD73D
            7D0F0715D45F5D40C4FCE4FE06B9FBD9A1E7079FA57554942DF122E9A976397D
            4A1FDC4BD3EE37F2A2AE4F01BF956DADD4C93CC7CA8D381963C753D3AD15E262
            2A4232B367B3462E51B9FFD9}
          ExplicitLeft = 540
        end
        object bNTSpec: TButton
          Left = 580
          Top = 62
          Width = 100
          Height = 25
          Cursor = crHandPoint
          Anchors = [akTop, akRight]
          Caption = 'Installed Suites...'
          TabOrder = 0
          OnClick = cmNTSpec
        end
        object pcOS: TPageControl
          Left = 10
          Top = 104
          Width = 671
          Height = 423
          Cursor = crHandPoint
          ActivePage = tsOSWEI
          Anchors = [akLeft, akTop, akRight, akBottom]
          TabOrder = 1
          object tsGeneral: TTabSheet
            Caption = ' General '
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object lvOS: TListView
              Left = 0
              Top = 0
              Width = 663
              Height = 395
              Align = alClient
              Columns = <
                item
                  Caption = 'Parameter'
                  Width = 150
                end
                item
                  Caption = 'Value'
                  Width = 360
                end>
              ColumnClick = False
              ReadOnly = True
              RowSelect = True
              TabOrder = 0
              ViewStyle = vsReport
            end
          end
          object tsFolders: TTabSheet
            Caption = ' Folders '
            ImageIndex = 1
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object FolderList: TListView
              Left = 0
              Top = 0
              Width = 663
              Height = 395
              Align = alClient
              Columns = <
                item
                  Caption = 'System name'
                  Width = 100
                end
                item
                  Caption = 'Location'
                  Width = 410
                end>
              ColumnClick = False
              ReadOnly = True
              RowSelect = True
              SortType = stText
              TabOrder = 0
              ViewStyle = vsReport
            end
          end
          object tsInternet: TTabSheet
            Caption = ' Internet '
            ImageIndex = 2
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            DesignSize = (
              663
              395)
            object Label43: TLabel
              Left = 47
              Top = 20
              Width = 81
              Height = 13
              Alignment = taRightJustify
              Caption = 'Default Browser:'
            end
            object Label44: TLabel
              Left = 43
              Top = 69
              Width = 85
              Height = 13
              Alignment = taRightJustify
              Caption = 'Connection Type:'
            end
            object Label45: TLabel
              Left = 61
              Top = 91
              Width = 67
              Height = 13
              Alignment = taRightJustify
              Caption = 'Proxy Server:'
            end
            object Label46: TLabel
              Left = 38
              Top = 44
              Width = 90
              Height = 13
              Alignment = taRightJustify
              Caption = 'Default Mail Client:'
            end
            object eBrowser: TEdit
              Left = 138
              Top = 16
              Width = 508
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              Color = clBtnFace
              ReadOnly = True
              TabOrder = 0
            end
            object eCT: TEdit
              Left = 138
              Top = 65
              Width = 508
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              Color = clBtnFace
              ReadOnly = True
              TabOrder = 1
            end
            object email: TEdit
              Left = 138
              Top = 40
              Width = 508
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              Color = clBtnFace
              ReadOnly = True
              TabOrder = 2
            end
            object mProxy: TMemo
              Left = 138
              Top = 91
              Width = 508
              Height = 89
              Anchors = [akLeft, akTop, akRight]
              ParentColor = True
              TabOrder = 3
              WordWrap = False
            end
          end
          object tsLocale: TTabSheet
            Caption = ' Locale '
            ImageIndex = 3
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object LocaleList: TListView
              Left = 0
              Top = 0
              Width = 663
              Height = 395
              Align = alClient
              Columns = <
                item
                  Caption = 'Parameter'
                  Width = 150
                end
                item
                  Caption = 'Value'
                  Width = 360
                end>
              ColumnClick = False
              ReadOnly = True
              RowSelect = True
              TabOrder = 0
              ViewStyle = vsReport
            end
          end
          object TabSheet1: TTabSheet
            Caption = ' Updates '
            ImageIndex = 4
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object UpdList: TListView
              Left = 0
              Top = 0
              Width = 663
              Height = 395
              Align = alClient
              Columns = <
                item
                  Caption = 'ID'
                  Width = 75
                end
                item
                  Caption = 'Install Date'
                  Width = 100
                end
                item
                  Caption = 'Installed By'
                  Width = 75
                end
                item
                  Caption = 'Description'
                  Width = 260
                end>
              ColumnClick = False
              ReadOnly = True
              RowSelect = True
              SortType = stText
              TabOrder = 0
              ViewStyle = vsReport
            end
          end
          object TabSheet2: TTabSheet
            Caption = 'Environment'
            ImageIndex = 5
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object EnvList: TListView
              Left = 0
              Top = 0
              Width = 663
              Height = 395
              Align = alClient
              Columns = <
                item
                  Caption = 'Variable'
                  Width = 150
                end
                item
                  Caption = 'Value'
                  Width = 360
                end>
              ColumnClick = False
              ReadOnly = True
              RowSelect = True
              SortType = stText
              TabOrder = 0
              ViewStyle = vsReport
            end
          end
          object tsPC: TTabSheet
            Caption = 'Parental Controls'
            ImageIndex = 6
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object PCList: TListView
              Left = 0
              Top = 0
              Width = 663
              Height = 395
              Align = alClient
              Columns = <
                item
                  Caption = 'Account'
                  Width = 400
                end
                item
                  Caption = 'Status'
                  Width = 100
                end>
              ColumnClick = False
              ReadOnly = True
              RowSelect = True
              SortType = stText
              TabOrder = 0
              ViewStyle = vsReport
            end
          end
          object TabSheet3: TTabSheet
            Caption = ' Server Flags '
            ImageIndex = 7
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object clbOSFlags: TCheckListBox
              Left = 0
              Top = 0
              Width = 663
              Height = 395
              Cursor = crHandPoint
              Align = alClient
              Columns = 2
              ItemHeight = 13
              Items.Strings = (
                'WORKSTATION'
                'SERVER'
                'SQLSERVER'
                'DOMAIN_CTRL'
                'DOMAIN_BAKCTRL'
                'TIME_SOURCE'
                'AFP'
                'NOVELL'
                'DOMAIN_MEMBER'
                'PRINTQ_SERVER'
                'DIALIN_SERVER'
                'XENIX_SERVER'
                'NT'
                'WFW'
                'SERVER_MFPN'
                'SERVER_NT'
                'POTENTIAL_BROWSER'
                'BACKUP_BROWSER'
                'MASTER_BROWSER'
                'DOMAIN_MASTER'
                'SERVER_OSF'
                'SERVER_VMS'
                'WINDOWS'
                'DFS'
                'CLUSTER_NT'
                'TERMINALSERVER'
                'CLUSTER_VS_NT'
                'DCE'
                'ALTERNATE_XPORT'
                'LOCAL_LIST_ONLY'
                'DOMAIN_ENUM')
              ParentColor = True
              TabOrder = 0
            end
          end
          object tsOSWEI: TTabSheet
            Caption = 'Windows Experience Index'
            ImageIndex = 8
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object lSystemScore: TLabel
              Left = 61
              Top = 18
              Width = 90
              Height = 13
              Alignment = taRightJustify
              Caption = 'System score: 0'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object lCpuScore: TLabel
              Left = 62
              Top = 45
              Width = 89
              Height = 13
              Alignment = taRightJustify
              Caption = 'Processor score: 0'
            end
            object lMemoryScore: TLabel
              Left = 71
              Top = 66
              Width = 80
              Height = 13
              Alignment = taRightJustify
              Caption = 'Memory score: 0'
            end
            object lGraphicsScore: TLabel
              Left = 68
              Top = 87
              Width = 83
              Height = 13
              Alignment = taRightJustify
              Caption = 'Graphics score: 0'
            end
            object lGamingScore: TLabel
              Left = 74
              Top = 108
              Width = 77
              Height = 13
              Alignment = taRightJustify
              Caption = 'Gaming score: 0'
            end
            object lDiskScore: TLabel
              Left = 21
              Top = 129
              Width = 130
              Height = 13
              Alignment = taRightJustify
              Caption = 'Primary hard drive score: 0'
            end
          end
        end
        object eGUID: TEdit
          Left = 109
          Top = 64
          Width = 401
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 2
        end
      end
      object tsCPU: TTabSheet
        Caption = ' CPU '
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          692
          533)
        object icoCPU: TImage
          Left = 19
          Top = 10
          Width = 37
          Height = 36
          AutoSize = True
          Center = True
          ParentShowHint = False
          ShowHint = True
          Transparent = True
        end
        object pcCPU: TPageControl
          Left = 0
          Top = 53
          Width = 692
          Height = 480
          Cursor = crHandPoint
          ActivePage = tsID
          Anchors = [akLeft, akTop, akRight, akBottom]
          TabOrder = 0
          object tsID: TTabSheet
            Caption = ' Identification '
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            DesignSize = (
              684
              452)
            object GroupBox5: TGroupBox
              Left = 7
              Top = 6
              Width = 668
              Height = 434
              Anchors = [akLeft, akTop, akRight, akBottom]
              ParentBackground = False
              TabOrder = 0
              object lGeneric: TLabel
                Left = 14
                Top = 175
                Width = 36
                Height = 13
                Caption = 'Generic'
              end
              object lSerial: TLabel
                Left = 16
                Top = 243
                Width = 26
                Height = 13
                Caption = 'Serial'
              end
              object lVendor: TLabel
                Left = 16
                Top = 221
                Width = 34
                Height = 13
                Caption = 'Vendor'
              end
              object lMarket: TLabel
                Left = 14
                Top = 153
                Width = 33
                Height = 13
                Caption = 'Market'
              end
              object lTech: TLabel
                Left = 17
                Top = 322
                Width = 55
                Height = 13
                Caption = 'Technology'
              end
              object lFreq: TLabel
                Left = 16
                Top = 200
                Width = 30
                Height = 13
                Caption = 'Speed'
              end
              object lRevision: TLabel
                Left = 17
                Top = 301
                Width = 40
                Height = 13
                Caption = 'Revision'
              end
              object lCPU: TLabel
                Left = 14
                Top = 18
                Width = 22
                Height = 13
                Caption = 'CPU'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = [fsBold]
                ParentFont = False
              end
              object lCodename: TLabel
                Left = 17
                Top = 279
                Width = 52
                Height = 13
                Caption = 'CodeName'
              end
              object lArch: TLabel
                Left = 14
                Top = 41
                Width = 59
                Height = 13
                Caption = 'Architecture'
              end
              object lCPP: TLabel
                Left = 14
                Top = 62
                Width = 19
                Height = 13
                Caption = 'CPP'
              end
              object lLPC: TLabel
                Left = 14
                Top = 82
                Width = 18
                Height = 13
                Caption = 'LPC'
              end
              object lCC: TLabel
                Left = 14
                Top = 102
                Width = 14
                Height = 13
                Caption = 'CC'
              end
              object lTHC: TLabel
                Left = 14
                Top = 123
                Width = 20
                Height = 13
                Caption = 'THC'
              end
            end
          end
          object tsCache: TTabSheet
            Caption = ' Cache '
            ImageIndex = 2
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            DesignSize = (
              684
              452)
            object GroupBox20: TGroupBox
              Left = 13
              Top = 9
              Width = 658
              Height = 433
              Anchors = [akLeft, akTop, akRight, akBottom]
              ParentBackground = False
              TabOrder = 0
              object Panel34: TPanel
                Left = 2
                Top = 15
                Width = 654
                Height = 416
                Align = alClient
                BevelOuter = bvNone
                BorderWidth = 5
                TabOrder = 0
                object CacheBox: TMemo
                  Left = 5
                  Top = 5
                  Width = 644
                  Height = 406
                  Align = alClient
                  BorderStyle = bsNone
                  ParentColor = True
                  ReadOnly = True
                  TabOrder = 0
                end
              end
            end
          end
          object tsFeatures: TTabSheet
            Caption = ' Features '
            ImageIndex = 1
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            DesignSize = (
              684
              452)
            object tc: TTabControl
              Left = 7
              Top = 8
              Width = 670
              Height = 437
              Cursor = crHandPoint
              Anchors = [akLeft, akTop, akRight, akBottom]
              TabOrder = 0
              Tabs.Strings = (
                'Standard-1 '
                'Standard-2'
                'Extended-1 '
                'Extended-2'
                'Power Management')
              TabIndex = 0
              OnChange = tcChange
              DesignSize = (
                670
                437)
              object GroupBox6: TGroupBox
                Left = 13
                Top = 28
                Width = 645
                Height = 395
                Anchors = [akLeft, akTop, akRight, akBottom]
                ParentBackground = False
                TabOrder = 0
                object Panel4: TPanel
                  Left = 2
                  Top = 15
                  Width = 641
                  Height = 378
                  Align = alClient
                  BevelOuter = bvNone
                  BorderWidth = 5
                  TabOrder = 0
                  object clbFS: TCheckListBox
                    Left = 5
                    Top = 5
                    Width = 631
                    Height = 364
                    OnClickCheck = clbClickCheck
                    Align = alClient
                    BorderStyle = bsNone
                    IntegralHeight = True
                    ItemHeight = 13
                    ParentColor = True
                    TabOrder = 0
                  end
                end
              end
            end
          end
        end
        object cbCPU: TComboBox
          Left = 66
          Top = 17
          Width = 614
          Height = 21
          Cursor = crHandPoint
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
          OnChange = cbCPUChange
        end
      end
      object tsMemory: TTabSheet
        Caption = ' Memory '
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          692
          533)
        object GroupBox7: TGroupBox
          Left = 8
          Top = 11
          Width = 648
          Height = 193
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          object icoMemory: TImage
            Left = 18
            Top = 24
            Width = 32
            Height = 32
            AutoSize = True
            Center = True
            ParentShowHint = False
            ShowHint = True
            Transparent = True
          end
          object Label16: TLabel
            Left = 75
            Top = 21
            Width = 65
            Height = 13
            Alignment = taRightJustify
            Caption = 'Physical Total'
          end
          object Label17: TLabel
            Left = 77
            Top = 47
            Width = 63
            Height = 13
            Alignment = taRightJustify
            Caption = 'Physical Free'
          end
          object Label18: TLabel
            Left = 254
            Top = 21
            Width = 27
            Height = 13
            Caption = 'bytes'
          end
          object Label19: TLabel
            Left = 254
            Top = 47
            Width = 27
            Height = 13
            Caption = 'bytes'
          end
          object Label20: TLabel
            Left = 70
            Top = 79
            Width = 70
            Height = 13
            Alignment = taRightJustify
            Caption = 'Page File Total'
          end
          object Label21: TLabel
            Left = 72
            Top = 105
            Width = 68
            Height = 13
            Alignment = taRightJustify
            Caption = 'Page File Free'
          end
          object Label22: TLabel
            Left = 254
            Top = 79
            Width = 27
            Height = 13
            Caption = 'bytes'
          end
          object Label23: TLabel
            Left = 254
            Top = 105
            Width = 27
            Height = 13
            Caption = 'bytes'
          end
          object Label24: TLabel
            Left = 83
            Top = 137
            Width = 57
            Height = 13
            Alignment = taRightJustify
            Caption = 'Virtual Total'
          end
          object Label25: TLabel
            Left = 85
            Top = 163
            Width = 55
            Height = 13
            Alignment = taRightJustify
            Caption = 'Virtual Free'
          end
          object Label26: TLabel
            Left = 254
            Top = 137
            Width = 27
            Height = 13
            Caption = 'bytes'
          end
          object Label27: TLabel
            Left = 254
            Top = 163
            Width = 27
            Height = 13
            Caption = 'bytes'
          end
          object ePT: TEdit
            Left = 150
            Top = 17
            Width = 100
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 0
          end
          object ePF: TEdit
            Left = 150
            Top = 43
            Width = 100
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 1
          end
          object eFT: TEdit
            Left = 150
            Top = 75
            Width = 100
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 2
          end
          object eFF: TEdit
            Left = 150
            Top = 101
            Width = 100
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 3
          end
          object eVT: TEdit
            Left = 150
            Top = 133
            Width = 100
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 4
          end
          object eVF: TEdit
            Left = 150
            Top = 159
            Width = 100
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 5
          end
        end
        object GroupBox9: TGroupBox
          Left = 8
          Top = 213
          Width = 648
          Height = 99
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
          object Label28: TLabel
            Left = 43
            Top = 24
            Width = 101
            Height = 13
            Alignment = taRightJustify
            Caption = 'Allocation granularity'
          end
          object Label29: TLabel
            Left = 20
            Top = 48
            Width = 124
            Height = 13
            Alignment = taRightJustify
            Caption = 'Application address range'
          end
          object Label30: TLabel
            Left = 99
            Top = 72
            Width = 45
            Height = 13
            Alignment = taRightJustify
            Caption = 'Page size'
          end
          object Label31: TLabel
            Left = 258
            Top = 72
            Width = 27
            Height = 13
            Caption = 'bytes'
          end
          object Label32: TLabel
            Left = 258
            Top = 23
            Width = 27
            Height = 13
            Caption = 'bytes'
          end
          object eAG: TEdit
            Left = 154
            Top = 20
            Width = 100
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 0
          end
          object eAppAddr: TEdit
            Left = 154
            Top = 44
            Width = 133
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 1
          end
          object ePS: TEdit
            Left = 154
            Top = 68
            Width = 100
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 2
          end
        end
        object gbMemUsage: TGroupBox
          Left = 8
          Top = 326
          Width = 634
          Height = 170
          TabOrder = 2
          object Label13: TLabel
            Left = 25
            Top = 26
            Width = 46
            Height = 13
            Caption = 'Utilization'
          end
          object Label50: TLabel
            Left = 29
            Top = 70
            Width = 63
            Height = 13
            Caption = 'Physical Free'
          end
          object Label49: TLabel
            Left = 148
            Top = 70
            Width = 68
            Height = 13
            Caption = 'Page File Free'
          end
          object Label51: TLabel
            Left = 269
            Top = 69
            Width = 55
            Height = 13
            Caption = 'Virtual Free'
          end
          object gMemory: TProgressBar
            Left = 26
            Top = 45
            Width = 298
            Height = 18
            Hint = 'Memory utilization'
            ParentShowHint = False
            Step = 1
            ShowHint = True
            TabOrder = 0
          end
          object gPhys: TProgressBar
            Left = 27
            Top = 87
            Width = 55
            Height = 66
            Hint = 'Memory utilization'
            Orientation = pbVertical
            ParentShowHint = False
            Smooth = True
            Step = 1
            ShowHint = True
            TabOrder = 1
          end
          object gPage: TProgressBar
            Left = 148
            Top = 87
            Width = 55
            Height = 66
            Hint = 'Memory utilization'
            Orientation = pbVertical
            ParentShowHint = False
            Smooth = True
            Step = 1
            ShowHint = True
            TabOrder = 2
          end
          object gVirt: TProgressBar
            Left = 269
            Top = 87
            Width = 55
            Height = 66
            Hint = 'Memory utilization'
            Orientation = pbVertical
            ParentShowHint = False
            Smooth = True
            Step = 1
            ShowHint = True
            TabOrder = 3
          end
        end
      end
      object tsDisplay: TTabSheet
        Caption = ' Display '
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          692
          533)
        object pcDisplay: TPageControl
          Left = 6
          Top = 11
          Width = 680
          Height = 512
          Cursor = crHandPoint
          ActivePage = tsDisplay2
          Anchors = [akLeft, akTop, akRight, akBottom]
          TabOrder = 0
          object tsDisplay1: TTabSheet
            Caption = ' Adapters '
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            DesignSize = (
              672
              484)
            object icoVideo: TImage
              Left = 9
              Top = 7
              Width = 32
              Height = 32
              AutoSize = True
              Center = True
              Transparent = True
            end
            object Label33: TLabel
              Left = 64
              Top = 16
              Width = 39
              Height = 13
              Alignment = taRightJustify
              Caption = 'Adapter'
            end
            object GroupBox10: TGroupBox
              Left = 7
              Top = 48
              Width = 658
              Height = 429
              Anchors = [akLeft, akTop, akRight, akBottom]
              Caption = ' Properties '
              TabOrder = 0
              DesignSize = (
                658
                429)
              object Label34: TLabel
                Left = 39
                Top = 27
                Width = 36
                Height = 13
                Alignment = taRightJustify
                Caption = 'Chipset'
              end
              object Label35: TLabel
                Left = 54
                Top = 54
                Width = 21
                Height = 13
                Alignment = taRightJustify
                Caption = 'DAC'
              end
              object Label36: TLabel
                Left = 37
                Top = 82
                Width = 38
                Height = 13
                Alignment = taRightJustify
                Caption = 'Memory'
              end
              object Label37: TLabel
                Left = 602
                Top = 82
                Width = 27
                Height = 13
                Anchors = [akTop, akRight]
                Caption = 'bytes'
                ExplicitLeft = 588
              end
              object Label65: TLabel
                Left = 51
                Top = 109
                Width = 24
                Height = 13
                Alignment = taRightJustify
                Caption = 'BIOS'
              end
              object eChip: TEdit
                Left = 85
                Top = 23
                Width = 543
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                Color = clBtnFace
                ReadOnly = True
                TabOrder = 0
              end
              object eDAC: TEdit
                Left = 85
                Top = 50
                Width = 543
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                Color = clBtnFace
                ReadOnly = True
                TabOrder = 1
              end
              object eMem: TEdit
                Left = 85
                Top = 78
                Width = 510
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                Color = clBtnFace
                ReadOnly = True
                TabOrder = 2
              end
              object eBIOSString: TEdit
                Left = 85
                Top = 105
                Width = 543
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                Color = clBtnFace
                ReadOnly = True
                TabOrder = 3
              end
            end
            object cbDisplay: TComboBox
              Left = 109
              Top = 13
              Width = 553
              Height = 21
              Cursor = crHandPoint
              Style = csDropDownList
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 1
              OnChange = cbDisplayChange
            end
          end
          object tsDisplay2: TTabSheet
            Caption = ' Capabilities '
            ImageIndex = 2
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object Panel35: TPanel
              Left = 0
              Top = 0
              Width = 672
              Height = 484
              Align = alClient
              BevelOuter = bvNone
              BorderWidth = 10
              Caption = ' '
              TabOrder = 0
              object lvDisp: TListView
                Left = 10
                Top = 10
                Width = 530
                Height = 384
                Align = alClient
                Columns = <
                  item
                    Caption = 'Property'
                    Width = 110
                  end
                  item
                    Caption = 'Value'
                    Width = 100
                  end>
                ColumnClick = False
                ReadOnly = True
                RowSelect = True
                TabOrder = 0
                ViewStyle = vsReport
              end
              object GroupBox11: TPanel
                Left = 540
                Top = 10
                Width = 122
                Height = 384
                Align = alRight
                BevelOuter = bvNone
                TabOrder = 1
                object bCurve: TButton
                  Left = 6
                  Top = 5
                  Width = 110
                  Height = 25
                  Cursor = crHandPoint
                  Caption = 'Curve...'
                  TabOrder = 0
                  OnClick = cmCaps
                end
                object bLine: TButton
                  Tag = 1
                  Left = 6
                  Top = 35
                  Width = 110
                  Height = 25
                  Cursor = crHandPoint
                  Caption = 'Line...'
                  TabOrder = 1
                  OnClick = cmCaps
                end
                object bPoly: TButton
                  Tag = 2
                  Left = 6
                  Top = 65
                  Width = 110
                  Height = 25
                  Cursor = crHandPoint
                  Caption = 'Polygon...'
                  TabOrder = 2
                  OnClick = cmCaps
                end
                object bRaster: TButton
                  Tag = 3
                  Left = 6
                  Top = 95
                  Width = 110
                  Height = 25
                  Cursor = crHandPoint
                  Caption = 'Raster...'
                  TabOrder = 3
                  OnClick = cmCaps
                end
                object bText: TButton
                  Tag = 4
                  Left = 6
                  Top = 125
                  Width = 110
                  Height = 25
                  Cursor = crHandPoint
                  Caption = 'Text...'
                  TabOrder = 4
                  OnClick = cmCaps
                end
                object Button1: TButton
                  Tag = 6
                  Left = 6
                  Top = 186
                  Width = 110
                  Height = 25
                  Cursor = crHandPoint
                  Caption = 'Color Management...'
                  TabOrder = 5
                  OnClick = cmCaps
                end
                object Button2: TButton
                  Tag = 5
                  Left = 6
                  Top = 155
                  Width = 110
                  Height = 25
                  Cursor = crHandPoint
                  Caption = 'Shade Blend...'
                  TabOrder = 6
                  OnClick = cmCaps
                end
                object bModes: TButton
                  Left = 6
                  Top = 227
                  Width = 110
                  Height = 25
                  Cursor = crHandPoint
                  Caption = 'Video Modes...'
                  TabOrder = 7
                  OnClick = cmModes
                end
              end
              object GroupBox12: TGroupBox
                Left = 10
                Top = 394
                Width = 652
                Height = 80
                Align = alBottom
                Caption = ' BIOS '
                TabOrder = 2
                DesignSize = (
                  652
                  80)
                object Label101: TLabel
                  Left = 16
                  Top = 24
                  Width = 35
                  Height = 13
                  Alignment = taRightJustify
                  Caption = 'Version'
                end
                object Label102: TLabel
                  Left = 28
                  Top = 50
                  Width = 23
                  Height = 13
                  Alignment = taRightJustify
                  Caption = 'Date'
                end
                object eBIOS: TEdit
                  Left = 60
                  Top = 20
                  Width = 562
                  Height = 21
                  Anchors = [akLeft, akTop, akRight]
                  Color = clBtnFace
                  ReadOnly = True
                  TabOrder = 0
                end
                object edate: TEdit
                  Left = 60
                  Top = 46
                  Width = 562
                  Height = 21
                  Anchors = [akLeft, akTop, akRight]
                  Color = clBtnFace
                  ReadOnly = True
                  TabOrder = 1
                end
              end
            end
          end
        end
      end
      object tsMonitor: TTabSheet
        Caption = ' Monitor '
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          692
          533)
        object icoMonitor: TImage
          Left = 20
          Top = 14
          Width = 32
          Height = 32
          AutoSize = True
          Center = True
          Transparent = True
        end
        object cbMon: TComboBox
          Left = 65
          Top = 20
          Width = 609
          Height = 21
          Cursor = crHandPoint
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          OnChange = cbMonChange
        end
        object GroupBox23: TGroupBox
          Left = 17
          Top = 62
          Width = 659
          Height = 450
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = ' Monitor properties '
          TabOrder = 1
          DesignSize = (
            659
            450)
          object Label52: TLabel
            Left = 56
            Top = 28
            Width = 27
            Height = 13
            Alignment = taRightJustify
            Caption = 'Name'
          end
          object Label86: TLabel
            Left = 28
            Top = 167
            Width = 55
            Height = 13
            Alignment = taRightJustify
            Caption = 'Display size'
          end
          object Label90: TLabel
            Left = 18
            Top = 335
            Width = 65
            Height = 13
            Alignment = taRightJustify
            Caption = 'Serial number'
          end
          object Label88: TLabel
            Left = 18
            Top = 111
            Width = 65
            Height = 13
            Alignment = taRightJustify
            Caption = 'Manufacturer'
          end
          object Label89: TLabel
            Left = 48
            Top = 279
            Width = 35
            Height = 13
            Alignment = taRightJustify
            Caption = 'Gamma'
          end
          object Label96: TLabel
            Left = 60
            Top = 307
            Width = 23
            Height = 13
            Alignment = taRightJustify
            Caption = 'Date'
          end
          object Label110: TLabel
            Left = 21
            Top = 363
            Width = 62
            Height = 13
            Alignment = taRightJustify
            Caption = 'EDID version'
          end
          object Label111: TLabel
            Left = 55
            Top = 139
            Width = 28
            Height = 13
            Alignment = taRightJustify
            Caption = 'Model'
          end
          object Label112: TLabel
            Left = 7
            Top = 83
            Width = 76
            Height = 13
            Alignment = taRightJustify
            Caption = 'Product number'
          end
          object Label114: TLabel
            Left = 33
            Top = 195
            Width = 50
            Height = 13
            Alignment = taRightJustify
            Caption = 'Resolution'
          end
          object Label119: TLabel
            Left = 30
            Top = 55
            Width = 53
            Height = 13
            Alignment = taRightJustify
            Caption = 'Description'
          end
          object Label120: TLabel
            Left = 32
            Top = 223
            Width = 51
            Height = 13
            Alignment = taRightJustify
            Caption = 'Work Area'
          end
          object Label121: TLabel
            Left = 66
            Top = 251
            Width = 17
            Height = 13
            Alignment = taRightJustify
            Caption = 'DPI'
          end
          object eMonName: TEdit
            Left = 91
            Top = 24
            Width = 543
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 0
          end
          object eMonSize: TEdit
            Left = 91
            Top = 163
            Width = 543
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 5
          end
          object eMonSN: TEdit
            Left = 91
            Top = 331
            Width = 543
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 11
          end
          object eMonMan: TEdit
            Left = 91
            Top = 107
            Width = 543
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 3
          end
          object eGamma: TEdit
            Left = 91
            Top = 275
            Width = 543
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 9
          end
          object eMonDate: TEdit
            Left = 91
            Top = 303
            Width = 543
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 10
          end
          object eEDID: TEdit
            Left = 91
            Top = 359
            Width = 543
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 12
          end
          object eMonMod: TEdit
            Left = 91
            Top = 135
            Width = 543
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 4
          end
          object eMonPN: TEdit
            Left = 91
            Top = 79
            Width = 543
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 2
          end
          object eMonRes: TEdit
            Left = 91
            Top = 191
            Width = 543
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 6
          end
          object cbxMonPrim: TCheckBox
            Left = 91
            Top = 390
            Width = 97
            Height = 17
            Caption = 'Primary'
            TabOrder = 13
            OnClick = cbxClick
          end
          object eMonDesc: TEdit
            Left = 91
            Top = 51
            Width = 543
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 1
          end
          object eWorkArea: TEdit
            Left = 91
            Top = 219
            Width = 543
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 7
          end
          object eDPI: TEdit
            Left = 91
            Top = 247
            Width = 543
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 8
          end
        end
      end
      object tsAPM: TTabSheet
        Caption = ' APM '
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Panel7: TPanel
          Left = 0
          Top = 0
          Width = 692
          Height = 49
          Align = alTop
          BevelOuter = bvNone
          Caption = ' '
          ParentBackground = False
          TabOrder = 0
          DesignSize = (
            692
            49)
          object icoMachine: TImage
            Left = 7
            Top = 8
            Width = 32
            Height = 32
            AutoSize = True
            Center = True
          end
          object eAC: TLabel
            Left = 49
            Top = 14
            Width = 3
            Height = 13
            Anchors = [akLeft, akTop, akRight]
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Shell Dlg 2'
            Font.Style = [fsBold]
            ParentFont = False
          end
        end
        object pcAPM: TPageControl
          Left = 0
          Top = 49
          Width = 692
          Height = 484
          Cursor = crHandPoint
          ActivePage = tsProc
          Align = alClient
          TabOrder = 1
          object tsBat: TTabSheet
            Caption = ' Battery '
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            DesignSize = (
              684
              456)
            object icoBattery: TImage
              Left = 7
              Top = 8
              Width = 32
              Height = 32
              AutoSize = True
              Center = True
            end
            object Label41: TLabel
              Left = 32
              Top = 159
              Width = 74
              Height = 13
              Alignment = taRightJustify
              Caption = 'Estimated time:'
            end
            object Label39: TLabel
              Left = 34
              Top = 132
              Width = 72
              Height = 13
              Alignment = taRightJustify
              Caption = 'Charge status:'
            end
            object Label9: TLabel
              Left = 60
              Top = 213
              Width = 46
              Height = 13
              Alignment = taRightJustify
              Caption = 'Capacity:'
            end
            object Label10: TLabel
              Left = 53
              Top = 240
              Width = 53
              Height = 13
              Alignment = taRightJustify
              Caption = 'Remaining:'
            end
            object Label11: TLabel
              Left = 16
              Top = 267
              Width = 90
              Height = 13
              Alignment = taRightJustify
              Caption = '(Dis)charging rate:'
            end
            object Label15: TLabel
              Left = 54
              Top = 78
              Width = 52
              Height = 13
              Alignment = taRightJustify
              Caption = 'Chemistry:'
            end
            object Label38: TLabel
              Left = 37
              Top = 52
              Width = 69
              Height = 13
              Alignment = taRightJustify
              Caption = 'Manufacturer:'
            end
            object Label40: TLabel
              Left = 66
              Top = 186
              Width = 40
              Height = 13
              Alignment = taRightJustify
              Caption = 'Voltage:'
            end
            object Label63: TLabel
              Left = 55
              Top = 105
              Width = 51
              Height = 13
              Alignment = taRightJustify
              Caption = 'Unique ID:'
            end
            object eBatLife: TEdit
              Left = 112
              Top = 157
              Width = 547
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              Color = clBtnFace
              ReadOnly = True
              TabOrder = 5
            end
            object pbBat: TProgressBar
              Left = 11
              Top = 430
              Width = 657
              Height = 15
              Anchors = [akLeft, akRight, akBottom]
              Step = 1
              TabOrder = 10
            end
            object eBat: TEdit
              Left = 112
              Top = 130
              Width = 547
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              Color = clBtnFace
              ReadOnly = True
              TabOrder = 4
            end
            object eBatCap: TEdit
              Left = 112
              Top = 210
              Width = 547
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              Color = clBtnFace
              ReadOnly = True
              TabOrder = 7
            end
            object eBatRemain: TEdit
              Left = 112
              Top = 237
              Width = 547
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              Color = clBtnFace
              ReadOnly = True
              TabOrder = 8
            end
            object eBatRate: TEdit
              Left = 112
              Top = 264
              Width = 547
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              Color = clBtnFace
              ReadOnly = True
              TabOrder = 9
            end
            object eBatChem: TEdit
              Left = 112
              Top = 75
              Width = 547
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              Color = clBtnFace
              ReadOnly = True
              TabOrder = 2
            end
            object eBatMan: TEdit
              Left = 112
              Top = 49
              Width = 547
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              Color = clBtnFace
              ReadOnly = True
              TabOrder = 1
            end
            object eBatVolt: TEdit
              Left = 112
              Top = 183
              Width = 547
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              Color = clBtnFace
              ReadOnly = True
              TabOrder = 6
            end
            object eBatID: TEdit
              Left = 112
              Top = 102
              Width = 547
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              Color = clBtnFace
              ReadOnly = True
              TabOrder = 3
            end
            object cbBat: TComboBox
              Left = 49
              Top = 14
              Width = 617
              Height = 21
              Cursor = crHandPoint
              Style = csDropDownList
              Anchors = [akLeft, akTop, akRight]
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Shell Dlg 2'
              Font.Style = []
              ParentFont = False
              TabOrder = 0
              OnChange = cbBatChange
            end
          end
          object tsProc: TTabSheet
            Caption = ' CPU '
            ImageIndex = 1
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            DesignSize = (
              684
              456)
            object icoProc: TImage
              Left = 7
              Top = 8
              Width = 32
              Height = 32
              AutoSize = True
              Center = True
            end
            object Label72: TLabel
              Left = 23
              Top = 80
              Width = 93
              Height = 13
              Alignment = taRightJustify
              Caption = 'Current frequency:'
            end
            object Label91: TLabel
              Left = 40
              Top = 53
              Width = 76
              Height = 13
              Alignment = taRightJustify
              Caption = 'Max frequency:'
            end
            object Label92: TLabel
              Left = 42
              Top = 108
              Width = 76
              Height = 13
              Alignment = taRightJustify
              Caption = 'Frequency limit:'
            end
            object Label93: TLabel
              Left = 45
              Top = 133
              Width = 71
              Height = 13
              Alignment = taRightJustify
              Caption = 'Max idle state:'
            end
            object Label94: TLabel
              Left = 28
              Top = 159
              Width = 88
              Height = 13
              Alignment = taRightJustify
              Caption = 'Current idle state:'
            end
            object cbProc: TComboBox
              Left = 49
              Top = 14
              Width = 617
              Height = 21
              Cursor = crHandPoint
              Style = csDropDownList
              Anchors = [akLeft, akTop, akRight]
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Shell Dlg 2'
              Font.Style = []
              ParentFont = False
              TabOrder = 0
              OnChange = cbProcChange
            end
            object eFreq: TEdit
              Left = 125
              Top = 77
              Width = 537
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              Color = clBtnFace
              ReadOnly = True
              TabOrder = 1
            end
            object eMaxFreq: TEdit
              Left = 125
              Top = 50
              Width = 537
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              Color = clBtnFace
              ReadOnly = True
              TabOrder = 2
            end
            object eFreqLimit: TEdit
              Left = 125
              Top = 103
              Width = 537
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              Color = clBtnFace
              ReadOnly = True
              TabOrder = 3
            end
            object eMaxIdle: TEdit
              Left = 125
              Top = 130
              Width = 537
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              Color = clBtnFace
              ReadOnly = True
              TabOrder = 4
            end
            object eIdle: TEdit
              Left = 125
              Top = 156
              Width = 537
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              Color = clBtnFace
              ReadOnly = True
              TabOrder = 5
            end
            object pbCPU: TProgressBar
              Left = 11
              Top = 430
              Width = 657
              Height = 15
              Anchors = [akLeft, akRight, akBottom]
              Step = 1
              TabOrder = 6
            end
          end
          object TabSheet6: TTabSheet
            Caption = ' System '
            ImageIndex = 2
            TabVisible = False
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            DesignSize = (
              684
              456)
            object Image12: TImage
              Left = 7
              Top = 8
              Width = 32
              Height = 32
              AutoSize = True
              Center = True
              Picture.Data = {
                055449636F6E0000010001002020000001002000A81000001600000028000000
                2000000040000000010020000000000080100000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                00000000000000000000000056504E0B635C5904000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                00000000000000000000000000000000000000000000000000000000746E6D02
                5A5654214E4746655B5252B2AEA3A0E55C5452C44B45413F6A615B0100000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                00000000000000000000000000000000726E6E0855525135514A4A81746C6CCA
                BCB2B0F7E3D9D2FFEFE4D8FFE7D8C6FFE5D6C2FFA59A91FA3A34339657504A10
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000065616114514E4E505A53539C999191DED5CCCCF7F6F4F4FFFBF8F4FF
                F2EBE3FFEBE1D6FFE7DACCFFE3D4C2FFDFCEB9FFE2CBB0FFD7C6AFFF67615DD9
                2826254100000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000807C7C035B585824615D5D6B
                716B6BB6BAB3B3EEE4DEDEF6E7E6E6F1D6D6D6EEB9B9B9F9E2E1E0FFF6F1EDFF
                F0E9E1FFEBE1D6FFE7DBCCFFE6D7C4FFD7C7B3FFAEA08EFF998D81FF716C68FF
                484747F91616168A1D1C1C0B0000000000000000000000000000000000000000
                00000000000000007672720B5552523B69656587938D8DCED3CCCCF9EFEAEAFF
                E9E8E8FBC7C7C7F9B3B3B3FE979797FF7D7D7DFF727272FF646565FFA9A7A6FF
                ECE5DEFFBEB7AFFF928982FF625752FF423431FF422F2BFF3E2B29FF443B3AFF
                3C3736FF413E3EFF171717CD100F0F3100000000000000000000000000000000
                00000000A7A1A140AEA8A8E2DED6D6FEE5DDDDFFE3DDDDFFE8E3E3FFEEEAEAFF
                F3F0F0FFD6D5D5FF919191FF757575FF878787FF858585FF807D7DFF6D6765FF
                4A403EFF473936FF3C2D2BFF463531FF402E2AFF3C2A26FF412D28FF3C2826FF
                423938FF3D3837FF393534FF1A1A1AF305050549060606020000000000000000
                00000000C6C0C051D6CDCDECD7CECEFFDDD6D6FFE2DBDBFFE6E1E1FFEBE6E6FF
                EBE7E7FFCFCCCCFFA8A6A6FF767272FF534D4DFF4F4746FF423837FF4A3E3CFF
                4A3C3AFF433532FF493A36FF44332FFF3F2E2BFF422F2BFF392725FF423231FF
                4B3B38FF5D4B42FF3B3635FF353130EF020202700000003A0202020600000000
                0000000000000000DED7D70CE1DADA74DED6D6E6D5CFCFFFA4A1A2FF7E7D7EFF
                5D5C5CFF555253FF4A4646FF554F4FFF4D4646FF473E3DFF4E4342FF473B39FF
                473A37FF493A37FF423432FF504341FF584E4DFF5B5352FF676261FF605B5BFF
                675644FF5D4937EC23201F900B0A0A330D0B0B220C0A0A1C0908081010101001
                00000000000000000000000000000000D4CECE08B5ACAC73988F8FE56F6A6BFF
                585556FF504C4DFF544E4FFF4E4747FF4C4443FF4D4442FF4B413FFF5A5150FF
                625B5AFF686262FF6A6565FF615D5DFF585454FF534D4DFF4C4645F43B3737C7
                2D2B2B973633317536333193333231B72D2E2EDD3837368E0000000000000000
                0000000000000000000000000000000000000000000000009E9999089D96966D
                988F8FE2706A6AFF575151FF625B5BFF5B5454FF666060FF736C6CFF6B6363FF
                5D5656FF434242FF363636FF343535FF363737FF403F3FFC424242F6404141FE
                434546FF605D5BFF816358FFA1654EFFC27A5CFFBDBCBBD70000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                8A8383059991915D8F8686A97E7474AF726969AC6A605F9C5C5453914F4A4AC0
                4C4A4AEC494A4AFE4F4F50FF4E4F50FF5E5F5DFF7D6357FF996149FFBA633EFF
                D05C2DFFCB3E0AFFC83B02FFCC4303FFCB470BFFCBCBCBF4B18E7D0300000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000006E625F125A545366535251BD565757FA666969FF
                7F7069FF926757FFA86046FFC15B34FFC54313FFC43A07FFC73F08FFCB4309FF
                C83F08FFC63A07FFC93F08FFCF4A11FFD14B10FFCFC2BCFFBD95822100000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000A695911483807EF7957063FFA46551FFB65B3FFFBA431EFF
                B3320CFFB4360FFFB83A12FFBC3D14FFC14217FFC54619FFCA4E20FFCD5020FF
                CD4C1DFFCC491CFFD04D1BFFD24D1BFFD24B17FFD3AFA1FFB893834B00000000
                0000000000000000000000000000000000000000000000000000000000000000
                000000000000000000000000CBA69CE5AB2805FFAD3212FFAD3313FFAC3314FF
                AE3414FFB33A18FFB93F1CFFBE421EFFC2431EFFC4451EFFCB522CFFD05B34FF
                D05631FFD25630FFD65B31FFD85A2EFFD9582AFFD2977FFFBDA0937100000000
                0000000000000000000000000000000000000000000000000000000000000000
                000000000000000000000000D0B9B4B9AF3513FFB34022FFB24022FFB13E21FF
                B13B1DFFB43A1BFFB93C1CFFBD3E1CFFC13E1CFFC5421EFFD15D3AFFD76946FF
                D86644FFDB6944FFDF6E46FFDF6B42FFE06A3EFFD28466FFBFAFA79C00000000
                0000000000000000000000000000000000000000000000000000000000000000
                000000000000000000000000D2BDB78FB24628FFB84D31FFB74D32FFB84C31FF
                B64225FFB94123FFBE4324FFC24525FFC64625FFCC4B29FFD96D4DFFDE7959FF
                DF7757FFE27D5AFFE47E59FFE57B55FFE67A52FFD97B56FFBBB8B6C400000000
                0000000000000000000000000000000000000000000000000000000000000000
                000000000000000000000000D2B5AE64B55D45FFBD5A40FFBE5C42FFBD553BFF
                B9462BFFBF482BFFC34A2DFFC84C2DFFCD4E2DFFD35534FFE07E61FFE4886BFF
                E5896CFFE88F6FFFE98D6BFFEA8B68FFEA8A65FFE58057FFBCBAB9EA00000000
                0000000000000000000000000000000000000000000000000000000000000000
                000000000000000000000000D1AAA038B97866FFC1654EFFC46A52FFBE573EFF
                BE4C32FFC34F34FFC85135FFCD5335FFD35637FFD96243FFE69076FFE8977DFF
                EB9C81FFEC9D81FFED9A7CFFEE9979FFEE9876FFED8E67FFC7BAB4FFB5998C17
                0000000000000000000000000000000000000000000000000000000000000000
                000000000000000000000000D0A89E10C09387FDC56F59FFC9725CFFBE553DFF
                C2533AFFC7563CFFCD593DFFD45C3FFFDA5E40FFE27154FFECA28BFFEFA790FF
                F0AA92FFF0A98FFFF0A98CFFF0A889FFEFA786FFEEA07BFFD5BAAEFFAE95893E
                0000000000000000000000000000000000000000000000000000000000000000
                00000000000000000000000000000000CAAFA7E1C87660FFC8705BFFC25740FF
                C95B41FFD05E43FFD86145FFDF6648FFE56747FFED8569FFF2B39EFFF4B7A3FF
                F3B59FFFF1B39BFFF1B49AFFF1B397FFF0B294FFF0AD8CFFD9B4A0FFB09D9466
                0000000000000000000000000000000000000000000000000000000000000000
                00000000000000000000000000000000C9B5AFB5C97863FFC66752FFC75E46FF
                CF6249FFD6664BFFDD6A4FFFE46D51FFEA6E4FFFF29C84FFF5C3B2FFF5C3B1FF
                F3BEAAFFF2BBA6FFF2BCA5FFF2BCA4FFF1BBA0FFF0B79AFFD9AA91FFAEA6A291
                0000000000000000000000000000000000000000000000000000000000000000
                00000000000000000000000000000000D0C0BC8BC1715DFFC5624CFFCB634DFF
                D36850FFDA6D54FFE27259FFE77358FFEE785CFFF6B5A4FFF7CFC1FFF6CBBBFF
                F4C7B5FFF3C4B1FFF2C2ADFFF2C2AEFFF1C1ABFFF1BFA5FFDEAA8DFFAFADACB8
                0000000000000000000000000000000000000000000000000000000000000000
                00000000000000000000000000000000D2BAB560B86F5EFFC6634FFFCD6953FF
                D66E58FFDD755FFFE47860FFEB7A60FFF38870FFF8CABDFFF8D7CBFFF6D2C4FF
                F5CFBFFFF4CBBAFFF3C7B4FFF2C3AEFFF2C1ACFFF0BBA2FFE0A384FFADACABE2
                AEA39D0200000000000000000000000000000000000000000000000000000000
                00000000000000000000000000000000D1B0A934B68073FFC86753FFD06E5AFF
                D87662FFE07B66FFE77F68FFEE826AFFF69782FFFAD3C8FFF9D4C8FFF3C9BAFF
                E8BFAEFFE0BAABFFDABAADFFD5BCB2FFD2C3BDFFC8C2C1F6B9BBBCD9ACABABB2
                A29A950B00000000000000000000000000000000000000000000000000000000
                00000000000000000000000000000000D4B8B10DBA948AFCCA6C59FFD47765FF
                D97D6BFFDA8271FFDB8D7DFFDE9E90FFE0B5ACFFDECCC8FFD5CBC8F0C8C5C4D1
                CDCAC9AAC5C1C089CFC5C064C0B4AF42C1AFA81FBEACA5030000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000C9BCB9DCC7A6A0FFCCB9B5EC
                CBC5C4CBD6CAC8A6D4C6C482D8C1BC5FD2B9B33AD1B8B21AD7C0BA0200000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000D4BCB720CCB3AE14CCA79F01
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
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                00000000FFFFFFFFFFFFCFFFFFFC03FFFFE001FFFF0000FFF000003F8000001F
                000000070000000380000001E0000007F8000007FE000003FFC00003FF800003
                FFC00003FFC00003FFC00003FFC00003FFC00001FFC00001FFE00001FFE00001
                FFE00001FFE00000FFE00000FFE00007FFF003FFFFF1FFFFFFFFFFFFFFFFFFFF
                FFFFFFFF}
            end
            object eAPM: TEdit
              Left = 49
              Top = 14
              Width = 637
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              BorderStyle = bsNone
              Color = clBtnFace
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Shell Dlg 2'
              Font.Style = [fsBold]
              ParentFont = False
              ReadOnly = True
              TabOrder = 0
            end
            object APMPanel: TPanel
              Left = 5
              Top = 46
              Width = 690
              Height = 408
              Anchors = [akLeft, akTop, akRight, akBottom]
              BevelOuter = bvNone
              Caption = ' '
              Enabled = False
              TabOrder = 1
              DesignSize = (
                690
                408)
              object Label95: TLabel
                Left = 15
                Top = 232
                Width = 75
                Height = 13
                Alignment = taRightJustify
                Caption = 'Last sleep time:'
              end
              object Label113: TLabel
                Left = 15
                Top = 259
                Width = 75
                Height = 13
                Alignment = taRightJustify
                Caption = 'Last wake time:'
              end
              object cbxUPS: TCheckBox
                Left = 16
                Top = 8
                Width = 169
                Height = 17
                Caption = 'UPS'
                TabOrder = 0
              end
              object cbxTC: TCheckBox
                Left = 16
                Top = 27
                Width = 169
                Height = 17
                Caption = 'Thermal control'
                TabOrder = 1
              end
              object cbxVD: TCheckBox
                Left = 16
                Top = 46
                Width = 169
                Height = 17
                Caption = 'Video dimming'
                TabOrder = 2
              end
              object cbxPT: TCheckBox
                Left = 16
                Top = 65
                Width = 169
                Height = 17
                Caption = 'Processor throttling'
                TabOrder = 3
              end
              object cbxDSD: TCheckBox
                Left = 16
                Top = 85
                Width = 281
                Height = 17
                Caption = 'Allowing the removal of power to fixed disk devices'
                TabOrder = 4
              end
              object cbxPB: TCheckBox
                Left = 16
                Top = 128
                Width = 169
                Height = 17
                Caption = 'Power button'
                TabOrder = 5
              end
              object cbxSB: TCheckBox
                Left = 16
                Top = 147
                Width = 169
                Height = 17
                Caption = 'Sleep button'
                TabOrder = 6
              end
              object cbxLS: TCheckBox
                Left = 16
                Top = 166
                Width = 169
                Height = 17
                Caption = 'Lid switch'
                TabOrder = 7
              end
              object cbxSysBat: TCheckBox
                Left = 16
                Top = 197
                Width = 281
                Height = 17
                Caption = 'System batteries'
                TabOrder = 8
              end
              object eLST: TEdit
                Left = 99
                Top = 229
                Width = 559
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                Color = clBtnFace
                ReadOnly = True
                TabOrder = 9
              end
              object eLWT: TEdit
                Left = 99
                Top = 256
                Width = 559
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                Color = clBtnFace
                ReadOnly = True
                TabOrder = 10
              end
            end
          end
        end
      end
      object tsMedia: TTabSheet
        Caption = ' Media '
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          692
          533)
        object Label42: TLabel
          Left = 13
          Top = 9
          Width = 83
          Height = 13
          Caption = 'Available Devices'
        end
        object Label14: TLabel
          Left = 13
          Top = 345
          Width = 70
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'Sound Devices'
        end
        object lvMedia: TListView
          Left = 13
          Top = 28
          Width = 667
          Height = 297
          Anchors = [akLeft, akTop, akRight, akBottom]
          Columns = <
            item
              Width = 500
            end>
          ColumnClick = False
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          ShowColumnHeaders = False
          ShowWorkAreas = True
          SmallImages = ilSystem
          TabOrder = 0
          ViewStyle = vsReport
        end
        object lvSound: TListView
          Left = 13
          Top = 363
          Width = 667
          Height = 156
          Anchors = [akLeft, akRight, akBottom]
          Columns = <
            item
              Caption = 'Driver'
              Width = 225
            end
            item
              Caption = 'Device'
              Width = 100
            end>
          ColumnClick = False
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          SmallImages = ilSystem
          TabOrder = 1
          ViewStyle = vsReport
        end
      end
      object tsNetwork: TTabSheet
        Caption = ' Network '
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Panel6: TPanel
          Left = 0
          Top = 0
          Width = 692
          Height = 533
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 10
          Caption = ' '
          TabOrder = 0
          object pcNET: TPageControl
            Left = 10
            Top = 10
            Width = 672
            Height = 513
            Cursor = crHandPoint
            ActivePage = tsNetGeneral
            Align = alClient
            TabOrder = 0
            object tsNetGeneral: TTabSheet
              Caption = ' General '
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
              DesignSize = (
                664
                485)
              object Label47: TLabel
                Left = 11
                Top = 9
                Width = 86
                Height = 13
                Caption = 'Network adapters'
              end
              object bProto: TButton
                Left = 11
                Top = 448
                Width = 75
                Height = 25
                Cursor = crHandPoint
                Anchors = [akLeft, akBottom]
                Caption = 'Protocols...'
                TabOrder = 1
                OnClick = cmProto
              end
              object bServ: TButton
                Left = 97
                Top = 448
                Width = 75
                Height = 25
                Cursor = crHandPoint
                Anchors = [akLeft, akBottom]
                Caption = 'Services...'
                TabOrder = 2
                OnClick = cmServ
              end
              object bCli: TButton
                Left = 183
                Top = 448
                Width = 75
                Height = 25
                Cursor = crHandPoint
                Anchors = [akLeft, akBottom]
                Caption = 'Clients...'
                TabOrder = 3
                OnClick = cmCli
              end
              object lvIntf: TListView
                Left = 11
                Top = 28
                Width = 644
                Height = 414
                Anchors = [akLeft, akTop, akRight, akBottom]
                Columns = <
                  item
                    Caption = 'Interface'
                    Width = 300
                  end
                  item
                    Caption = 'IP Address'
                    Width = 100
                  end
                  item
                    Caption = 'Mask'
                    Width = 100
                  end
                  item
                    Caption = 'MAC Address'
                    Width = 120
                  end
                  item
                    Caption = 'Type'
                    Width = 150
                  end
                  item
                    Caption = 'Operational'
                    Width = 100
                  end
                  item
                    Caption = 'Admin'
                  end
                  item
                    Alignment = taRightJustify
                    Caption = 'Speed [Mbps]'
                    Width = 80
                  end
                  item
                    Alignment = taRightJustify
                    Caption = 'MTU [B]'
                  end>
                ColumnClick = False
                HideSelection = False
                ReadOnly = True
                RowSelect = True
                SmallImages = ilWIFI
                TabOrder = 0
                ViewStyle = vsReport
                OnChange = lvIntfChange
                OnDblClick = lvIntfDblClick
              end
              object bIntfDetail: TButton
                Left = 581
                Top = 448
                Width = 75
                Height = 25
                Cursor = crHandPoint
                Anchors = [akRight, akBottom]
                Caption = 'Details...'
                Enabled = False
                TabOrder = 4
                OnClick = bIntfDetailClick
              end
            end
            object tsNetWinsock: TTabSheet
              Caption = ' Winsock '
              ImageIndex = 1
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
              DesignSize = (
                664
                485)
              object Label57: TLabel
                Left = 55
                Top = 18
                Width = 53
                Height = 13
                Alignment = taRightJustify
                Caption = 'Description'
              end
              object Label58: TLabel
                Left = 73
                Top = 45
                Width = 35
                Height = 13
                Alignment = taRightJustify
                Caption = 'Version'
              end
              object Label59: TLabel
                Left = 77
                Top = 73
                Width = 31
                Height = 13
                Alignment = taRightJustify
                Caption = 'Status'
              end
              object icoNet: TImage
                Left = 10
                Top = 10
                Width = 32
                Height = 32
                AutoSize = True
                Center = True
              end
              object eWSDesc: TEdit
                Left = 117
                Top = 14
                Width = 518
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                Color = clBtnFace
                ReadOnly = True
                TabOrder = 0
              end
              object eWSVer: TEdit
                Left = 117
                Top = 41
                Width = 362
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                Color = clBtnFace
                ReadOnly = True
                TabOrder = 1
              end
              object eWSStat: TEdit
                Left = 117
                Top = 68
                Width = 518
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                Color = clBtnFace
                ReadOnly = True
                TabOrder = 2
              end
            end
            object tsNetEnv: TTabSheet
              Caption = 'Environment'
              ImageIndex = 3
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
              DesignSize = (
                664
                485)
              object pcNetEnv: TPageControl
                Left = 9
                Top = 9
                Width = 646
                Height = 467
                Cursor = crHandPoint
                ActivePage = tsNEConns
                Anchors = [akLeft, akTop, akRight, akBottom]
                TabOrder = 0
                object tsNEConns: TTabSheet
                  Caption = 'Connections'
                  ExplicitLeft = 0
                  ExplicitTop = 0
                  ExplicitWidth = 0
                  ExplicitHeight = 0
                  object lvNEConns: TListView
                    Left = 0
                    Top = 0
                    Width = 638
                    Height = 439
                    Align = alClient
                    Columns = <
                      item
                        Caption = 'Name'
                        Width = 100
                      end
                      item
                        Caption = 'User'
                        Width = 75
                      end
                      item
                        Caption = 'Type'
                      end
                      item
                        Alignment = taRightJustify
                        Caption = 'Files'
                      end
                      item
                        Alignment = taRightJustify
                        Caption = 'Users'
                      end>
                    ColumnClick = False
                    HideSelection = False
                    ReadOnly = True
                    RowSelect = True
                    TabOrder = 0
                    ViewStyle = vsReport
                  end
                end
                object tsNEShares: TTabSheet
                  Caption = 'Shares'
                  ImageIndex = 2
                  ExplicitLeft = 0
                  ExplicitTop = 0
                  ExplicitWidth = 0
                  ExplicitHeight = 0
                  object lvNEShares: TListView
                    Left = 0
                    Top = 0
                    Width = 638
                    Height = 439
                    Align = alClient
                    Columns = <
                      item
                        Caption = 'Name'
                        Width = 100
                      end
                      item
                        Caption = 'Path'
                        Width = 150
                      end
                      item
                        Caption = 'Type'
                      end>
                    ColumnClick = False
                    HideSelection = False
                    ReadOnly = True
                    RowSelect = True
                    TabOrder = 0
                    ViewStyle = vsReport
                  end
                end
                object tsNEFiles: TTabSheet
                  Caption = 'Open files'
                  ImageIndex = 1
                  ExplicitLeft = 0
                  ExplicitTop = 0
                  ExplicitWidth = 0
                  ExplicitHeight = 0
                  object lvNEFiles: TListView
                    Left = 0
                    Top = 0
                    Width = 638
                    Height = 439
                    Align = alClient
                    Columns = <
                      item
                        Caption = 'Name'
                        Width = 200
                      end
                      item
                        Caption = 'User'
                      end
                      item
                        Caption = 'Access'
                      end>
                    ColumnClick = False
                    HideSelection = False
                    ReadOnly = True
                    RowSelect = True
                    TabOrder = 0
                    ViewStyle = vsReport
                  end
                end
                object tsNESessions: TTabSheet
                  Caption = 'Sessions'
                  ImageIndex = 3
                  ExplicitLeft = 0
                  ExplicitTop = 0
                  ExplicitWidth = 0
                  ExplicitHeight = 0
                  object lvNESessions: TListView
                    Left = 0
                    Top = 0
                    Width = 638
                    Height = 439
                    Align = alClient
                    Columns = <
                      item
                        Caption = 'Name'
                        Width = 100
                      end
                      item
                        Caption = 'User'
                        Width = 100
                      end
                      item
                        Alignment = taRightJustify
                        Caption = 'Files'
                        Width = 40
                      end
                      item
                        Caption = 'Type'
                        Width = 100
                      end
                      item
                        Caption = 'Transport'
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
              end
            end
            object TabSheet9: TTabSheet
              Caption = 'Credentials'
              ImageIndex = 4
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
              object lvNetCreds: TListView
                Left = 0
                Top = 0
                Width = 664
                Height = 485
                Align = alClient
                Columns = <
                  item
                    Caption = 'Type'
                    Width = 100
                  end
                  item
                    Caption = 'Timestamp'
                    Width = 120
                  end
                  item
                    Caption = 'Target'
                    Width = 100
                  end
                  item
                    Caption = 'Username'
                    Width = 150
                  end
                  item
                    Caption = 'Password'
                    Width = 150
                  end>
                ColumnClick = False
                ReadOnly = True
                RowSelect = True
                TabOrder = 0
                ViewStyle = vsReport
              end
            end
          end
        end
      end
      object tsDevices: TTabSheet
        Caption = ' Devices '
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Panel36: TPanel
          Left = 0
          Top = 0
          Width = 692
          Height = 533
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 10
          Caption = ' '
          TabOrder = 0
          object pcDev: TPageControl
            Left = 10
            Top = 10
            Width = 672
            Height = 513
            Cursor = crHandPoint
            ActivePage = tsDevTree
            Align = alClient
            HotTrack = True
            TabOrder = 0
            object tsDevTree: TTabSheet
              Caption = ' Windows Devices '
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
              DesignSize = (
                664
                485)
              object Tree: TTreeView
                Left = 10
                Top = 10
                Width = 644
                Height = 424
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
                Left = 554
                Top = 446
                Width = 100
                Height = 25
                Cursor = crHandPoint
                Anchors = [akRight, akBottom]
                Caption = 'Properties...'
                TabOrder = 1
                OnClick = cmProps
              end
              object bRes: TButton
                Left = 10
                Top = 446
                Width = 100
                Height = 25
                Cursor = crHandPoint
                Anchors = [akLeft, akBottom]
                Caption = 'Resources...'
                TabOrder = 2
                OnClick = cmRes
              end
            end
            object tsDevRes: TTabSheet
              Caption = ' Device Resources '
              ImageIndex = 1
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
              object ResList: TListView
                Left = 0
                Top = 0
                Width = 664
                Height = 485
                Align = alClient
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
        end
      end
      object tsPrinters: TTabSheet
        Caption = ' Printers '
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          692
          533)
        object bPrint: TButton
          Left = 583
          Top = 500
          Width = 100
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = 'Properties...'
          TabOrder = 0
          OnClick = cmPrintSetup
        end
        object lvPrinter: TListView
          Left = 14
          Top = 16
          Width = 669
          Height = 472
          Anchors = [akLeft, akTop, akRight, akBottom]
          Columns = <
            item
              Caption = 'Name'
              Width = 190
            end
            item
              Caption = 'Driver'
              Width = 190
            end
            item
              Caption = 'Port'
              Width = 150
            end>
          ColumnClick = False
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          SmallImages = ilPrinter
          TabOrder = 1
          ViewStyle = vsReport
        end
      end
      object tsStorage: TTabSheet
        Caption = ' Storage '
        ImageIndex = 16
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Panel10: TPanel
          Left = 0
          Top = 0
          Width = 692
          Height = 533
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 10
          Caption = ' '
          TabOrder = 0
          object Splitter1: TSplitter
            Left = 310
            Top = 10
            Height = 513
            ExplicitLeft = 293
            ExplicitTop = 27
          end
          object StorageTree: TTreeView
            Left = 10
            Top = 10
            Width = 300
            Height = 513
            Align = alLeft
            BorderWidth = 5
            HideSelection = False
            Images = ilSystem
            Indent = 19
            ParentShowHint = False
            ReadOnly = True
            ShowHint = True
            TabOrder = 0
            OnChange = StorageTreeChange
            OnCustomDrawItem = StorageTreeCustomDrawItem
            OnDeletion = TreeDeletion
          end
          object lvStorage: TListView
            Left = 313
            Top = 10
            Width = 369
            Height = 513
            Align = alClient
            Columns = <
              item
                Caption = 'Property'
                Width = 150
              end
              item
                Caption = 'Value'
                Width = 180
              end>
            ColumnClick = False
            ReadOnly = True
            RowSelect = True
            ParentColor = True
            TabOrder = 1
            ViewStyle = vsReport
          end
        end
      end
      object tsUSB: TTabSheet
        Caption = ' USB '
        ImageIndex = 17
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object pcUSB: TPageControl
          Left = 0
          Top = 0
          Width = 692
          Height = 533
          Cursor = crHandPoint
          ActivePage = tsUSBConnections
          Align = alClient
          TabOrder = 0
          object tsUSBConnections: TTabSheet
            Caption = 'Connections'
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object Panel9: TPanel
              Left = 0
              Top = 0
              Width = 684
              Height = 505
              Align = alClient
              BevelOuter = bvNone
              BorderWidth = 10
              Caption = ' '
              TabOrder = 0
              object Splitter2: TSplitter
                Left = 310
                Top = 10
                Height = 485
                ExplicitLeft = 293
                ExplicitTop = 27
                ExplicitHeight = 513
              end
              object lvUSB: TListView
                Left = 313
                Top = 10
                Width = 361
                Height = 485
                Align = alClient
                Columns = <
                  item
                    Caption = 'Property'
                    Width = 150
                  end
                  item
                    Caption = 'Value'
                    Width = 180
                  end>
                ColumnClick = False
                ReadOnly = True
                RowSelect = True
                ParentColor = True
                TabOrder = 1
                ViewStyle = vsReport
              end
              object USBTree: TTreeView
                Left = 10
                Top = 10
                Width = 300
                Height = 485
                Align = alLeft
                HideSelection = False
                Images = ilSystem
                Indent = 19
                ParentShowHint = False
                ReadOnly = True
                ShowHint = True
                TabOrder = 0
                OnChange = USBTreeChange
                OnCustomDrawItem = USBTreeCustomDrawItem
                OnDeletion = TreeDeletion
              end
            end
          end
          object tsUSBHistory: TTabSheet
            Caption = 'History'
            ImageIndex = 1
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object lvUSBH: TListView
              Left = 0
              Top = 0
              Width = 684
              Height = 505
              Align = alClient
              Columns = <
                item
                  Caption = 'Device'
                  Width = 200
                end
                item
                  Caption = 'Serial number'
                  Width = 150
                end
                item
                  Caption = 'Last seen'
                  Width = 120
                end
                item
                  Caption = 'Class'
                  Width = 100
                end>
              ColumnClick = False
              ReadOnly = True
              RowSelect = True
              TabOrder = 0
              ViewStyle = vsReport
              OnCompare = lvUSBHCompare
            end
          end
        end
      end
      object tsEngines: TTabSheet
        Caption = ' Engines '
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object pcEng: TPageControl
          Left = 0
          Top = 0
          Width = 692
          Height = 533
          Cursor = crHandPoint
          ActivePage = tsASPI
          Align = alClient
          TabOrder = 0
          object tsEng: TTabSheet
            Caption = ' Engines '
            ImageIndex = 2
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object Panel5: TPanel
              Left = 0
              Top = 0
              Width = 684
              Height = 505
              Align = alClient
              BevelOuter = bvNone
              BorderWidth = 10
              Caption = ' '
              TabOrder = 0
              object lvEng: TListView
                Left = 10
                Top = 10
                Width = 664
                Height = 485
                Align = alClient
                Columns = <
                  item
                    Caption = 'Name'
                    Width = 300
                  end
                  item
                    Caption = 'Version'
                    Width = 120
                  end>
                ColumnClick = False
                HideSelection = False
                ReadOnly = True
                RowSelect = True
                SmallImages = ilEng
                TabOrder = 0
                ViewStyle = vsReport
              end
            end
          end
          object tsDirectX: TTabSheet
            Caption = ' DirectX '
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object Panel19: TPanel
              Left = 0
              Top = 0
              Width = 684
              Height = 505
              Align = alClient
              BevelOuter = bvNone
              BorderWidth = 10
              TabOrder = 0
              object lDirectX: TLabel
                Left = 10
                Top = 10
                Width = 664
                Height = 19
                Align = alTop
                AutoSize = False
                Caption = 'lASPI'
                Layout = tlCenter
                ExplicitWidth = 612
              end
              object lvDirectX: TListView
                Left = 10
                Top = 29
                Width = 664
                Height = 466
                Align = alClient
                Columns = <
                  item
                    Caption = 'Driver'
                    Width = 370
                  end
                  item
                    Caption = 'Device'
                    Width = 150
                  end>
                ColumnClick = False
                HideSelection = False
                ReadOnly = True
                RowSelect = True
                SmallImages = ilEng
                TabOrder = 0
                ViewStyle = vsReport
              end
            end
          end
          object tsASPI: TTabSheet
            Caption = ' Windows ASPI 32 '
            ImageIndex = 1
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object Panel22: TPanel
              Left = 0
              Top = 0
              Width = 684
              Height = 505
              Align = alClient
              BevelOuter = bvNone
              BorderWidth = 10
              TabOrder = 0
              object lASPI: TLabel
                Left = 10
                Top = 10
                Width = 664
                Height = 19
                Align = alTop
                AutoSize = False
                Caption = 'lASPI'
                Layout = tlCenter
                ExplicitWidth = 612
              end
              object lvASPI: TListView
                Left = 10
                Top = 29
                Width = 664
                Height = 466
                Align = alClient
                Columns = <
                  item
                    Caption = 'Product'
                    Width = 150
                  end
                  item
                    Caption = 'Revision'
                    Width = 55
                  end
                  item
                    Caption = 'Type'
                    Width = 75
                  end
                  item
                    Caption = 'Adapter'
                  end
                  item
                    Caption = 'ID'
                    Width = 30
                  end
                  item
                    Caption = 'LUN'
                    Width = 35
                  end
                  item
                    Caption = 'Extra'
                    Width = 100
                  end>
                ColumnClick = False
                HideSelection = False
                ReadOnly = True
                RowSelect = True
                SmallImages = ilEng
                TabOrder = 0
                ViewStyle = vsReport
              end
            end
          end
        end
      end
      object tsDrives: TTabSheet
        Caption = ' Drives '
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          692
          533)
        object Label54: TLabel
          Left = 20
          Top = 28
          Width = 25
          Height = 13
          Caption = 'Drive'
        end
        object cbDrive: TComboBox
          Left = 58
          Top = 24
          Width = 623
          Height = 21
          Cursor = crHandPoint
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          OnChange = cbDriveChange
        end
        object GroupBox19: TGroupBox
          Left = 12
          Top = 56
          Width = 669
          Height = 464
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = ' Properties '
          TabOrder = 1
          DesignSize = (
            669
            464)
          object imgDrive: TImage
            Left = 20
            Top = 20
            Width = 32
            Height = 32
            AutoSize = True
            Center = True
          end
          object lDriveType: TLabel
            Left = 64
            Top = 32
            Width = 61
            Height = 13
            Caption = 'lDriveType'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label55: TLabel
            Left = 81
            Top = 68
            Width = 21
            Height = 13
            Alignment = taRightJustify
            Caption = 'UNC'
          end
          object Label56: TLabel
            Left = 37
            Top = 95
            Width = 65
            Height = 13
            Alignment = taRightJustify
            Caption = 'Serial number'
          end
          object Bevel3: TBevel
            Left = 8
            Top = 120
            Width = 653
            Height = 3
            Anchors = [akLeft, akTop, akRight]
            Shape = bsTopLine
            ExplicitWidth = 601
          end
          object lCapacity: TLabel
            Left = 14
            Top = 134
            Width = 52
            Height = 13
            Caption = 'lCapacity'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object lFree: TLabel
            Left = 14
            Top = 158
            Width = 28
            Height = 13
            Caption = 'lFree'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object lBPS: TLabel
            Left = 523
            Top = 134
            Width = 20
            Height = 13
            Anchors = [akTop, akRight]
            Caption = 'lBPS'
            ExplicitLeft = 509
          end
          object lSPC: TLabel
            Left = 523
            Top = 155
            Width = 21
            Height = 13
            Anchors = [akTop, akRight]
            Caption = 'lSPC'
            ExplicitLeft = 509
          end
          object lTC: TLabel
            Left = 523
            Top = 175
            Width = 15
            Height = 13
            Anchors = [akTop, akRight]
            Caption = 'lTC'
            ExplicitLeft = 509
          end
          object lFC: TLabel
            Left = 523
            Top = 196
            Width = 15
            Height = 13
            Anchors = [akTop, akRight]
            Caption = 'lFC'
            ExplicitLeft = 471
          end
          object clbFlags: TCheckListBox
            Left = 11
            Top = 218
            Width = 647
            Height = 199
            OnClickCheck = clbClickCheck
            Anchors = [akLeft, akTop, akRight, akBottom]
            Columns = 2
            IntegralHeight = True
            ItemHeight = 13
            ParentColor = True
            TabOrder = 2
          end
          object eUNC: TEdit
            Left = 112
            Top = 64
            Width = 518
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 0
          end
          object eDSN: TEdit
            Left = 112
            Top = 91
            Width = 518
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 1
          end
          object gDisk: TProgressBar
            Left = 14
            Top = 185
            Width = 497
            Height = 15
            Anchors = [akLeft, akTop, akRight]
            Step = 1
            TabOrder = 3
          end
        end
      end
      object tsTZ: TTabSheet
        Caption = 'Time Zone'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          692
          533)
        object Panel12: TPanel
          Left = 7
          Top = 36
          Width = 678
          Height = 326
          Anchors = [akLeft, akTop, akRight, akBottom]
          BevelOuter = bvLowered
          Caption = ' '
          Color = clNavy
          TabOrder = 0
          object Image10: TImage
            Left = 1
            Top = 1
            Width = 356
            Height = 184
            Align = alClient
            AutoSize = True
            Center = True
            Picture.Data = {
              07544269746D617016040100424D160401000000000036040000280000006401
              0000B80000000100080000000000E0FF00000000000000000000000100000000
              000000000000000080000080000000808000800000008000800080800000C0C0
              C000C0DCC000F0CAA60004040400080808000C0C0C0011111100161616001C1C
              1C002222220029292900555555004D4D4D004242420039393900807CFF005050
              FF009300D600FFECCC00C6D6EF00D6E7E70090A9AD0000003300000066000000
              99000000CC00003300000033330000336600003399000033CC000033FF000066
              00000066330000666600006699000066CC000066FF0000990000009933000099
              6600009999000099CC000099FF0000CC000000CC330000CC660000CC990000CC
              CC0000CCFF0000FF660000FF990000FFCC003300000033003300330066003300
              99003300CC003300FF00333300003333330033336600333399003333CC003333
              FF00336600003366330033666600336699003366CC003366FF00339900003399
              330033996600339999003399CC003399FF0033CC000033CC330033CC660033CC
              990033CCCC0033CCFF0033FF330033FF660033FF990033FFCC0033FFFF006600
              00006600330066006600660099006600CC006600FF0066330000663333006633
              6600663399006633CC006633FF00666600006666330066666600666699006666
              CC00669900006699330066996600669999006699CC006699FF0066CC000066CC
              330066CC990066CCCC0066CCFF0066FF000066FF330066FF990066FFCC00CC00
              FF00FF00CC009999000099339900990099009900CC0099000000993333009900
              66009933CC009900FF00996600009966330099336600996699009966CC009933
              FF009999330099996600999999009999CC009999FF0099CC000099CC330066CC
              660099CC990099CCCC0099CCFF0099FF000099FF330099CC660099FF990099FF
              CC0099FFFF00CC00000099003300CC006600CC009900CC00CC0099330000CC33
              3300CC336600CC339900CC33CC00CC33FF00CC660000CC66330099666600CC66
              9900CC66CC009966FF00CC990000CC993300CC996600CC999900CC99CC00CC99
              FF00CCCC0000CCCC3300CCCC6600CCCC9900CCCCCC00CCCCFF00CCFF0000CCFF
              330099FF6600CCFF9900CCFFCC00CCFFFF00CC003300FF006600FF009900CC33
              0000FF333300FF336600FF339900FF33CC00FF33FF00FF660000FF663300CC66
              6600FF669900FF66CC00CC66FF00FF990000FF993300FF996600FF999900FF99
              CC00FF99FF00FFCC0000FFCC3300FFCC6600FFCC9900FFCCCC00FFCCFF00FFFF
              3300CCFF6600FFFF9900FFFFCC006666FF0066FF660066FFFF00FF666600FF66
              FF00FFFF66002100A5005F5F5F00777777008686860096969600CBCBCB00B2B2
              B200D7D7D700DDDDDD00E3E3E300EAEAEA00F1F1F100F8F8F800F0FBFF00A4A0
              A000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
              FF00040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040402020404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040402020202
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040202
              0202020202040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404020202020202040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040402020404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404020202020202040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040402020204040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404020202020202040404040404020404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040204040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040202020202040404040404020202020204040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040202020202020404040404040402020202
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040402020202020202040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040402
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404020202020202020204
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404020204040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040202020202
              0202020204040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040402
              0202020202020202040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404020202040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040202020202020202020404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040202020204040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040402020202020202040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040402020202020404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404020202020202020204040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040202020202040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040402020202020202020404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404020202
              0202040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040202020202020204040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404020204040404040404040404040404040404040404
              0404020202020404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040402020202020202
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404020202020404040404040404040404040404
              0404040404040404020202040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404020202
              0202020202040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404020202040404040404040404040404
              0404040404040404040404040202020402040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040402
              0202020202020202020204040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040402040404040404040404
              0404040404040404040404040404040404040402020404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040402020202020202020202020404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040402020202040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404020202020202020202020202040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040202
              0404040404040404040404040404040404040404040404040404020202020204
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040202020202020202020202020202040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040204
              0402020204020404040404040404040404040404040404040404040404040402
              0202020404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040402020202020202020202020202020204
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0402020202020202020202040404040404040404040404040404040404040404
              0404020202040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040202020202020202020202
              0202020204040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040402020202020202020202020404040404040404040404040404040404
              0404040404020202040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040402020202020202
              0202020202020202040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404020204040404040404040404040404
              0404040404020202020202020202020202020202040404040404040404040404
              0404040404040404040202040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040202
              0202020202020202020202020404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404020202020404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404020202020404040404040404
              0404040404040404040402020202020202020202020202020404040404040404
              0404040404040404040404040402040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404020202020202020202020202020202040204040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040402020202020202
              0202040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040402020202020202
              0404040404040404040404040204040202020202020202020202020204040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040202020202020202020202020202020202020404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404020202
              0202020202020202040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404020202
              0202020202020202020404040404040202020402020202020202020202020202
              0204040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040402020202020202020202020202020202020204040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0402020202020202020202020202040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0402020202020202020202020204040404020202020202020202020202020202
              0202020202040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404020202020202020202020202020202020202
              0202040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040402020202020202020202020202020404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404020202020202020202020202020202020202020202020202020202
              0202020202020202020204040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040202020202020202020202020202
              0202020202020204040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040402020202020202020202020202020204040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040202020202020202020202020202020202020202020202
              0202020202020202020202020202040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040402020202020202020202
              0202020202020202020202040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404020202020202020202020202020202020404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040202020202020202020202020202020202020202
              0202020202020202020202020202020202020204040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404020202020202
              0202020202020202020202020202020204040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040202020202020202020202020202020202
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040202020202020202020202020202020202
              0202020202020202020202020202020202020202020202040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040202
              0202020202020202020202020202020202020202020404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040202020202020202020202020202
              0202020204040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040402020202020202020202020202
              0202020202020202020202020202020202020202020202020202020404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404020202020202020202020202020202020202020202020204040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040402020202020202020202
              0202020202020202040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040402020202020202020202
              0202020202020202020202020202020202020202020202020202020202020404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404020204040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040202020202020202020202020202020202020202020202040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404020202020202
              0202020202020202020202020204040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040402020202040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040402020202020202020202020202020202020202020202
              0202020204040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040202
              0202020202020202020202020202020202020404040404040404040402020404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040402
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040202040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040202020202020202020202020202020202
              0202020202020202040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404020202020202020202020202020202020202020202040404040404040202
              0202020404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040202020202020202020202020202020202020202020202020202020202
              0202020202020202040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040402040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040402020202020202020202020202
              0202020202020202020202020202040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040202020202020202020202020202020202020202020404040404
              0404020202020204040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404020202020202020202020202020202020202020202020202
              0202020202020202020202040404040404040404040404040402020204040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040202020204040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404020202020202020202
              0202020202020202020202020202020202020202040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040202020202020202020202020202020202020202020204
              0404040404040202020202040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040402020202020202020202020202020202020202
              0202020202020202020202020202020404040404040404040404040404020204
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040202020204040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040202020202
              0202020202020202020202020202020202020202020202020404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040402020202020202020202020202020202020202
              0202040404040404040402020202020204040404040202040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040202020202020202020202020202
              0202020202020202020202020202020202020404040404040404040404040404
              0202040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404020404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040402
              0202020202020202020202020202020202020202020202020202020202040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404020202020202020202020202020202
              0202020202020204040404040404040202020202040404040402020402040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040202020202020202
              0202020202020202020202020202020202020202020404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040202020202020202020202020202020202020202020202020202020202
              0202040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404020202020202020202020202
              0202020202020202020202020404040404040202020202020404040404040404
              0204040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040202
              0202020202020202020202020202020202020202020202040404040404040404
              0404040404040404040404040404040404040404040202020204040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040402020202020202020202020202020202020202020202020202
              0202020202020404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040202020202020202
              0202020202020202020202020202020202020404040402020202020202040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040202020202020202020202020202020202020202020202020404040404
              0404040404040404040404040404040404040404040404040402020204040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040202020202020202020202020202020202020202020202
              0202020202020202020404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040202020202
              0202020202020202020202020202020202020202020202040404040202020202
              0204040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040202020202020202020202020202020402020202020204
              0404040404040404040404040404040404040404020404040404040404040204
              0204040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404020202020202020202020202020202020202020202
              0202020202020202020202020204040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040402
              0202020202020202020202020202020202020202020202020202020204040402
              0202020202040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040402020202020202020202020202020204040202
              0202020404040404040404040404040404040404040402020204040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404020202020202020202020202020202020202
              0202020202020202020202020202020202020404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040202020202020202020202020202020202020202020202020202020202
              0404040402020202020404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040202020202020202020202020404
              0404020202020404040404040404040404040404040404040404040204040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404020202020202020202020202020202
              0202020202020202020202020202020202020202020204040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404020202020202020202020202020202020202020202020202
              0202020204040404040402020202040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404020202020202
              0204040404040202020404040404040404040404040404040404040404040404
              0404040404040404040404040404040204040404040404020204040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040202020202020202020202
              0202020202020202020202020202020202020202020202020202040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040202020202020202020202020202020202020202
              0202020202020202040404040404040202020404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040402
              0202020202040404040402020204040404040404040404040404040404040404
              0404040404040404040404040404040404040202040404040404020202020404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040202020202020202
              0202020202020202020202020202020202020202020202020202020202020404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404020202020202020202020202020202
              0202020202020202020202020404040404040402040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040402020204040404040404040204040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040402
              0202040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040402020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040202020202020202020202
              0202020202020202020202020202020204040202040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040402020204040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040402
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040202020202020202
              0202020202020202020202020202020202020202040402020404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404020404
              0202020402020204040404040404040404040404040404040404040202020404
              0404040404040404020202040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040402020202
              0202020202020202020202020202020202020202020202040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404020202020202
              0402020202020202040402040404040404040404040404040402020204040202
              0204040404040404040202020204040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0202020202020202020202020202020202020202020202020202040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040402020202
              0202020204020404040402020204020204040402020404040404040202020202
              0202020204040404040404040202020204040404040404040404040404040404
              0404040404040204040404040404020204040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040402020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040202020202020202020202020202020202020202020202020202020404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040402
              0202020202040404040404040404040404040404040404020404020204040402
              0202020202020202040404040404040204040404040404040404040404040404
              0404040404040404040202040404040404020202020404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040402020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020204040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404020202020202020202020202020202020202020202020202
              0202040404040404040404040404040202040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404020202020404040404040404040404040404040404040404040404040202
              0204020202020202020202020402020202040202040404040404040404040404
              0404040404040404040404040202040404040404040402020204040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040402020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404020202020202020202020202020202020202020202
              0202020202020404040404040404040404040402020204040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404020202040404040404040404040404020202040404040404040204
              0404040202020202020202020202020404040202020204040404040404040404
              0404040404040404040404040404040402020404040404040404020202040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404020202020202020202020202020202020202
              0202020202020202020202040404040404040404040404040202020404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040402020202020404040404040202020204040202020404040202
              0402020204020202020202020202020404040404040404020204040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040402020202020202
              0202020202020202020202020202020202020202020202020202020202020404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404020202020202020202020202020202
              0202020202020202020202020202020204040404040404040404040404020204
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040402020202020204040404020202020202040202020202
              0404040404020204040202020202020202040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040202040404040404040402020202
              0202020202020202020202020202020202020202020202020202020202020202
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404020202020202020202020202
              0202020202020202020202020202020202020202040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404020202020202040404020202020202020404
              0202020204040402020404040202020204020202040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040402020404040404040404
              0202020202020202020202020202020202020202020202020202020202020202
              0202020404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404020202020202020202
              0202020202020202020202020202020202020202020202020204040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404020202020202040404040202020202
              0202040402020202040404040404040202020204040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040202040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040402020204040404
              0404040404020202020202020202020202020202020202020202020202020202
              0202040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040402020202
              0202020202020202020202020202020202020202020202020202020202020404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404020202020404040404040402
              0202020202020202040202020204040404040404020204040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040402020404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040202
              0404040404040404040402020202020202020202020202020202020202020202
              0202020202020404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0402020202020202020202020202020202020202020202020202020202020202
              0202020204040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040202020404020204
              0404040202020202020202020204040202020404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404020204040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404020404040404040404040404020202020202020202020202020202020202
              0202020202020202020204040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040402020202020202020202020202020202020202020202020202020202
              0202020202020202040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040202020404
              0202020404040404020202020202020204040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040402020202020202020202020202
              0202020202020202020202020202040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040402020202020202020202020202020202020202020202020202
              0202020202020202020202020202040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040202
              0202040402020404040404040402020202020204040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404020404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404020202020202020202
              0202020202020202020202020202020202020404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040402020202020202020202020202020202020202020202
              0202020202020202020202020202020202020404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0402020202020402020404040404040404040202020202020404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0202040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040202020202
              0202020202020202020202020202020202020202020404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040402
              0202020202020204040404040202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202040404040404040404
              0404040404040404040404040404040404040404040402040404040404040404
              0404040402020204040202020404040404040404040404020202020204040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040402020404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040402
              0202020202020202020202020202020202020204040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0202020202020202020202020202040202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020204040404
              0404040404040404040404040404040404040404040404040402020204040404
              0404040404040404040404040402020404040404040404040404040404020204
              0404040404020202040404040404020404040404040404040404040404040404
              0404040404040402020204040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040202020202020202020202020202020202020204040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0204040404040404040404040404040404040404040404040404040404020202
              0204040404040404040404040404040402020204040404040404040404040404
              0404040404040404020202020404040404040202040404040404040404040404
              0404040404040404040404040204040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0402020202020402020202020202020202020202020202020202040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202040404040404040404040404040404040404040404040404040404
              0402020204040404040404040404040404040402020204040404040404040404
              0404040404040204040404020202020404040404040402020204040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404020202020202020202020202020202020202020202020202020404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040402020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020204040404040404040404040404040404040404040404
              0404020204040204040404040404040404040404040404020202040404040202
              0404040404040404040404020404040404040404040404040404040202040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040402020204040404020404020202020202020202020202020202
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040402020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202040404040404040404040404040404040404
              0404040404020202020404040404040404040404040402040404020202020404
              0402020204040404040404040404040202040204020404040404040404040404
              0204040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404020202040404040404040404020202020202020404
              0404040402040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202040202020204040404040404040404040404
              0404040404040404040202020202020404040404040404040402020404040402
              0202040402020202020204040404040404040404020404020402020404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404020202040404040404040404040404020202
              0404040404040404020404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020404040404040404040404040404040404
              0404040404040404040404040202020202020204040404040404040404040202
              0404040202020404020202020202020404040404040404040404020402040404
              0404040404040404040404040404040404040204040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404020202020202020204040404040404040404
              0404040404040404040404040204040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020402020404040404040404040404
              0404040404040404040404040404040402020202020202040404040404040404
              0402020204040402020202020202020202020204040404040404040404040402
              0404040404040404040404040404040404040404040402040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040402020202020202020202040404040404
              0404040404040404040404040404040402040404040404040404040404040404
              0404040404040404040404040404040404040404040204040404040402020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202040402020202040404040404
              0404040404040404040404040404040404040404020202020202040404040404
              0404040404040202040404020202020202020202020204040404040404040404
              0404020204040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404020402020202020202020202020404
              0404040404040404040404040404040404040402020404040404040404040404
              0404040404040404040404040404040404040404040404040202040404040404
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020402020202020202
              0204040404040404040404040404040404040404040404020202020202020404
              0404040404040404040404040204040202020202020202020202040404040404
              0404040404020204040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404020202020202020204040402
              0204040404040404040404040404040404040404040204020404040404040404
              0404040404040404040404040404040404040404040404040404040404020404
              0404040402020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020404020202
              0202020202020202040404040404040404040404040404040404040202020202
              0202020204040404040404040404040402020202020202020202020202040404
              0404040404040404020202040404040404040404040404040404040404040404
              0404020404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040402020202020202020202
              0202040404040404040404040204040404040404040402040402020404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020404
              0402020202020202020202020202020404040404040404040404040404040202
              0202020202020202020404040404040404040404020202020202020202020202
              0404040404040404040404040202020404040404040404040404040404040404
              0404040404040204040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040202020202020202
              0202020202020404040404040404020202020402020202020204020204040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020402020202020202020202020202020204040404040404040404040404
              0404020202020202020202020202020404040404040404020202020202020202
              0202020204040202040404040404040404020404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040402020404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404020202020202
              0202020404040202020204040404040404040404040404040202020204040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040402020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020402020202020202020202020202020202020404040404040404
              0404040404040202020202020202020202020204040404040404020202020202
              0202020202020202020202020204040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040402020404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404020202
              0202020202020404040404020202020404040404040404020204040402020204
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040402020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020204020202020202020202020202020202020202040404
              0404040404040404020402020202020202020202020202020404040404020202
              0202020202020202020202020204020204040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040402020202040402020204040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0402020202020202020404040404040402020204040402020202020202020404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020204020202020202020202020202020202020202
              0204040404040404040404020202020202020202020202020202020202020404
              0402020202020202020202020202020202020402020204040404040404020404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040402020402020204020404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404020202020202020204040404040404040404040402020202020204
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020204040202020202020202020202020202
              0202020202020404040404040404040202020202020202020202020202020202
              0202040404040202020202020202020202020202020202020202020202040404
              0402040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404020202020404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040402020202020202020204040404040404040404040404040202
              0202040404040204040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020204040202020202020202020202
              0202020202020202020404040404040404040202020202020202020202020202
              0202020202020404040402020202020202020202020202020202020202020202
              0202020404020204040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404020202040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040402020402020202020202020202040404040404040404040404
              0404040404040404020202040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040402020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020204040402020202020202
              0202020202020404020202020404040404040404040202020202020202020202
              0202020202020202020204040404020202020202020202020202020202020202
              0202020202020202040202020404040404040404040404040404040404040402
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040202040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040402020402020202020202020202020204040404040404
              0404040404040402020404040202020404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202040402020202
              0202020202020202020204040402020404040204020202020202020202020202
              0202020202020202020202020202020404020202020202020202020202020202
              0202020202020202020202020204020204040404020404040404040404040404
              0404020202040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404020204040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040402020204020202020202020202020202040404
              0404040404040404040404020204040402020404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040402020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020402
              0202020202020202020202020202040404040204040202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202040404040404020202040404040404
              0404040404040402020404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404020204020202020202020202020202
              0204040404040404040404040404040202020402020404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0204040202020202020202020202020202040202020404040202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020204040404040402040404
              0404040404040404040402020404040404040404040404040404040404040404
              0404040404040404040404040404040404040404020202040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040402020204020202020202020202
              0202020202020404040404040404040404040202020404020404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202040202020202020202020202020202040202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020404040404
              0404020404040404040404040404020404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040202020404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404020204020202020202
              0202020202020202020202040404020404040404040402020204040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020402020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020204
              0404040404040404040404040404040404040204040404040404040404040404
              0404040404040404040404040404040404040404040404040404040202040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404020202020202
              0202020202020202020202020202020204020202040404020202020202040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020204020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020404040404040404040404040404040404040204040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404020202
              0402020202020202020202020202020202020202020202020202020202020202
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020204040404040404040204040404040404040402020404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0402020202020202020202020202020202020202020202020202020202020202
              0202020202040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040402020202020202020202020202020202020202020202
              0202020204040202020202020202040202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202040404040404040202020404040404040402020404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040402020202020202020202020202020202020202020202020202020202
              0202020202020202020404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404020202020202020202020202020202020202
              0202020202020404040404020202040402040404040404040402020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020404040404040402020202020202040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040402020202020202020202020202020202020202020202020202
              0202020202020202020202020202020404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040402020202020202020202020202
              0202020202020404040404040404040202040404040404040402020404020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020404040404040404040204040202
              0202040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040202020202020202
              0202020202020202020204040404040404040404040404020204040402020202
              0404020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020204040404040202020404
              0202020202020204040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040202
              0202020202020202020202020202020404040404040404040404040202040404
              0402040202040202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020204040404040404
              0202040402020202020202020204040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040402020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040202020202020202020202020404040404040404040404040202
              0204040202040404020402020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020204
              0404040202020204040404020202020202020404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040402020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0204040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404020204040404040404040404040404
              0404040404040202020404040202020202040202020204020202040404040404
              0202040204040202020202020202020202020202020202020202020202020404
              0404020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020404040402020202040404040404040202020204040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404020202040404040404040404
              0404040404040404040202020202040404040404040404040404040202040404
              0404040402020204040202020202020202020202020202020202020202020202
              0202040404020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020404040404040402020202040404040404040404020202040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020204040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404020204040404040404
              0404040404040404040404040202020202020204040404040404040404040404
              0204020204040404020202020402020202020202020202020202020202020202
              0202020202020204040202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202040404020404020202020404040404040404040402020204
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040202020202020202020402020404040404
              0204040404040202040404020202020202020202020202020202020202020202
              0202020202020202020204040202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020404040404040404040404
              0202020404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0402020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040402020202020202020204040404
              0404040202040404040402020204020202020202020204020202020202020202
              0202020202020202020202020202040402020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020404040404
              0404040404020404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404020202020202020202
              0202040404040402020404040202020204040202020202020202020404020202
              0202020202020202020202020202020202020402020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0404040404040404040202020404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040402020202020202020202020202020202020202020202
              0202020202020202020202020202040202020202040202020202020202020204
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040202020202
              0202020202020404040404040204040202020204040202020202020202020404
              0404040202020404040404040202020202020202020204020202020202020204
              0402020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020204040404040402020202040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404020202020202020202020202020202020202
              0202020202020202020202020202020202020402020202020202020202020202
              0202020404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040402
              0202020202020202020202020402040404040202020204020202020202020202
              0202020404040404040404040404040202020202020202020202040402020202
              0202020404020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020404040404040202020204040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040202020202020202020202020202
              0202020202020202020202020202020202020202020204020202040202020404
              0402020202020204040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040402020202040202020202020202020204040202020204020202020202
              0202020202020202040404040404040404040202020202020202020202020204
              0402020202020202040202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202040404040402020204040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040402020202020202020202
              0202020202020202020202020202020202020202020202020202040202020404
              0202020202020202020202020404040404040404040404040204020404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404020202020202020202020202020204020202
              0202020202020202020202020204040402020404040202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020204040404020204040402
              0202040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404020202020202
              0202020202020202020202020202020202020202020202020202020202020204
              0404020402020202020202020202020204040404040404040202020202020204
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040202020202020202020202020202
              0202020202020202020202020202020202020402020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202040404040404
              0404040402020202040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202040404020202020202020202020202020202040204040404040404020202
              0202020404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020204
              0404040404040404040202020204040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0402020202020202020202020202020202020202020202020202020202020202
              0202020202040404020202020202020202020202020202020202020404040404
              0402020202020404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020204040404040404020202020404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404020202020202020202020202020202020202020202020202020202
              0202020202020202040404020202020202020202020202020202020402020202
              0204040404040202020204040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202040404040404040404040202040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040402020204020202020202020202020202020202020202020202
              0202020202020202020202020204020202020202020202020202020202020202
              0402020202040204040404020204040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020204040404040404040404020404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020404040404020204040402020404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020204040404040404
              0404040404020404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040402020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0402020202020202020202020202020202020202020202020404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040202040402020202020204040202020202020202020202020202
              0404040402020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020204040404
              0404040404040404040204040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040204040202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202040202020202020202020202020202020202020202020202020404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040202020204020202020202040402020202020202020202
              0204040404040404020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020204
              0404040404040404040404040402020404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202040402020202020202020202020202020202020202020202
              0204040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040402020202040402020202020404040202020202
              0202020204040404040404040202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020204040404040404040404040404020202040404040404040404040404
              0404040204040404040404040404040404040404040404020204040404040404
              0404040404040404040404040404040404040404040404040402020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202040404020202020202020202020202020202020202
              0202020202040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404020202020404020202020204040404
              0402020202020202040404040404040402020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202040402040402040404040404040404040404020202020204040404040404
              0404040404040404040404040404040404040404040404040404040402040202
              0404040404040404040404040404040404040404040404040404040404020402
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020404020202020202020202020202020202
              0202020202020202040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040402020202040402020204
              0404040404040402020202020404040404040404020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020204040404040404040404040404040404040404040202020202020404
              0404040402040404040404040404040404040404040404040404040404040404
              0404020202040204040404040404040404040404040404040404040404040404
              0404020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020204040402020202020202020202
              0202020202020202020202040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040202020402
              0202040404040404040404020202040204040404040404020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020404040404040404040404040404040404040402020202
              0202040404040202040404040404040404040404040404040404040404040404
              0404040404040402020202020204040404040404040404040404040404040404
              0404040404020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020204040404040404020202020202
              0202020202020202020202020404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404020202040404040404040404040202040404040404040404040202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020404040404040404040404040404040404
              0202020202020204040404040404040404040404040404040404040404040404
              0404040404040404040404040202020202040404020204040404040404040404
              0404040404040402020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020404040402020402
              0202020202020202020202020202020204040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404020202040404040404040404040202020204020202020404040402
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202040404040404040404040404
              0404040404020202020202020404040404040404040404040404040404040404
              0404040404040404040404040404040404040402020202040402040404040404
              0404040404040404040202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202040404040404
              0404040402020202020202020202020202020202040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040202020404040404040404040402020204020202020202
              0404040202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202040204040404
              0404040404040404040202020202020204040404040404040404040404040404
              0404040404040404040404040404040404040404040404040202020204020404
              0404040404040404040404040402020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020204040404040404
              0404040404040402020202020202020202020202020202040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040204020204040404040404040404040202040202
              0202020204040404020402020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0204040404040404040404040404020202020204040404040404040404040404
              0404040404040404040404040404040404040404040404040402020202020202
              0204040404040404040404040404040402020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202040404
              0404040404040404040402020202020202020202020202020202020404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040402040404020404040404040404020204
              0404020202020202020404040404040202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020402020404040202020404040402020202020404040404040404
              0404040404040404040404040404040404040404040404040404040404020202
              0202020202040202040404040404040404040402020202020202020202020202
              0202020202020202020202020202020404020202020202020202020202020204
              0404040404040404040404040404020202020202020202020204020202020404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040204040404040404
              0402020202020202020202020204040404020404040202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202040202020202040404040202020202040404
              0404040404040404040404040404040404040404040404040404040404040202
              0202020202020202020202020202040404020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020404040404040404040404040404040402020202020202020204040404
              0204040404040404040404040404040404040404020404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040402020202020202020202020202020404020202020404020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020404040404020202
              0202040402020404040404040404040404040404040404040404040404040404
              0402020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202040404040404040404040404040404020202020202020202
              0404040402040404040404040404040404040404040402020202040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404020202020202020202020202020404020202020202040402
              0204040202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020404
              0202040202020202020202020404040404040404040404040404040404040404
              0404040404020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020404040404040404040404040404040202020202
              0202040404040404040404040404040404040404040404040202020202020404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040202020202020202020202020404040202020202
              0202020202040202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020204040202020202020202020404040404040404040404040404
              0404040404040404020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202040202
              0202020202020202020202020202020202040404040404040202020404020402
              0202020202040404040204040404040404040404040404040404040202020202
              0202040404040404040404040404040404040404040404040404040404040404
              0404040202040404040404040404040402020202020202020202020202040402
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202040404040404
              0404040404040404040404040402020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202040404040404020204
              0402040404040404040404040202020404040404040404040404040404040402
              0202020202020404040404040404040404040404040404040404040404040404
              0404040404040404020404040404040404040404040202020202020202020202
              0204040402020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0404040404040404040404040404040404020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020404040402
              0404040202040404040404040402020202020402040404040404040404040404
              0404020202020202020202040404040404040404040404040404040404040404
              0202040404040404040404040404040404040404040404040404020202020202
              0202020202020404040202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020404040404040404040404040404040404040202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020404020204040202020202020202020404040404040404
              0404040404020202020202020202020204040404040404040404040404040404
              0404020202020202020404040404040404040404040404040404040404040404
              0202020202020202020202020404040202020202020202020202020404020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020404040404040404040404040404040404040202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020404020202020202020202020202020204040404
              0404040404040404040202020202020202020202040404040404040404040404
              0404040404020202020202020202040404040404040404040404040404040404
              0404040404040202020202020202020204040402020202020202020202020404
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020404040202020202040202040404040202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020404040402020202020202020202020204
              0204040404040404040404040202020202020202020202020204040204040404
              0404040404040404020202020202020202020204040404040404040404040404
              0404040404040404040402020202020202020202040402020202020202020202
              0202040404040402020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202040402
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020404020202020202020202020202020202020202
              0202020202020202020202020202020204040404040404040404020202020202
              0204040202020204040404040404040202020202020202020202020202020202
              0204040404040404040404040202020202020202020204040404040404040404
              0404040404040404040404040404040202020202020202020202020202020202
              0202020202040404040404020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0204040402020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020204020402020202020202020202020202
              0202020202020202020202020202020202020202020204040404040404040202
              0202020202020202020202020404040404040402020202020202020202020202
              0202020202020404040404040404040404020404040402020204040404040404
              0404040404040404040404040404040404040404020202020202020202020202
              0202020202020202040404020202020404040202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020204040404040404020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020204040402
              0202040202020202020202020204020404040404040402020202020202020202
              0202020202020202020202040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040202020202020202
              0202020202020202020202020202020202020202040402020204020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020204040404040404040202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020204
              0404040202020204020202020202020202040404040404040404020202020202
              0202020202020202020202020202020204020404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404020202
              0202020202020202020202020202020202020202020202020404020202040402
              0202020202020202020202020202020202020202020202040202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020404040404040404040402020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020402020404040402020204020202020202020402020202020202020404
              0202020204040404020202020202020202020202040404040404040404040402
              0202020202020202020202020202020202020202020202040202040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0202020202020202020202020202020202020202020202020202040404040402
              0204040402020202020202020202020202020202020204020202020404020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020404040404040404040404020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020402020202020204040204020202040402020202040202020202
              0204040402020202040404040402020202020202020404040404040404040404
              0404040404040202020202020202020202020202020202020202020202020204
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040202020202020202020202020202020202020202020204040404
              0404040404040404040402020204040202040202020202040404020202020204
              0402020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202040404040404040404040404040404040402020202020202
              0202020202020202020202020202020202020202040402020202020202020202
              0202020202020404040202020202020202020204020202020404020202040202
              0202020404040404020404040404040402020202020202020202040404040404
              0404040404040202020202020202020202020202020202020202020202020202
              0202020204040404040404040404040404040404040404040404040404040404
              0404040404040404040402020202020202020202020202020202020202040204
              0404040404040404040404040404040404040404040402020202020404020202
              0202020404020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020402020202
              0204020202020202020204040404040404040404040404040404040404040404
              0402020202020202020202020202020202020204040404040404040404040202
              0402020404040404040404040402020202020202020202020202020202040402
              0202020202020404040402020404040202020202020202020202020202020404
              0404040404040404040402020202020202020202020202020202020202020202
              0202020202020202040404040404040404040404040404040404040404040404
              0404040404040404040404040404040402020202020202020202020202040404
              0404040404040404040404040404040404040404040404040402020202040404
              0202020202020204020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020404
              0404040404040402040404040404040404040404040404040404040404040404
              0404040404040202020202020202020202020404040404040404040404040404
              0404040404040404040404040404040402020202020202020202020202020202
              0404040404020202020204040202020402020402020202020202020202020202
              0204040404040404040404040404020202020202020202020202020202020202
              0202020202020202020202020204040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404020202020202020202
              0204040404040404040404040404040404040404040404040404020204020204
              0404040402020202020202040202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0404040404040404040404040404040404040402020404040404040404040404
              0404040404040404040404040402020204020204040404040404040404040404
              0404040404040404040404040404040402020202020202020202020202020202
              0202020404040404040202020204040402020204020402020202020202020202
              0202020404040404040404040404040404040404020202020202020202020202
              0202020202020202020202020202020202020404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040402
              0204020204040404040404040404040404040404040404040404040202020204
              0404040404040404040202020202040202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202040202020202020202020202020202040404
              0404040404040404040404040404040404040404040402020202040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040202020202020202020202020202
              0202020202040404040202020404020204040404020202040404020202020202
              0202020202040404040404040404040404040404040202020202020202020202
              0202020202020202020202020202020202020202020204040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040402
              0202020404040404040404040404020202020202020202020202040402020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020404040404040404020202020202020202020404
              0404040404040404040404040404040404040404040404040404040204040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404020202020202040202020202
              0202020202020202020404020202020202040404040404020202020404020202
              0202020202020204040404040404040404040404040404040202020202020202
              0202020202020202020202020202020202020202020202020202040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040202020404040404040404040404040402020202040202020202040402
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020204040404040404040402020204040204
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040402020202020204
              0402020202020202020202020204040202020202020404020202040402020202
              0202020202020202020204040404040404040404040404040404040402020202
              0202020202020202020202020202020202020202020202020202020202020404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040402020202040404040404040404040404040202040402020204
              0404020202020202020202020202020202020202020202020202020204020202
              0202020202020202020204020202020202020404040404040404040404040402
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404020202
              0202020202040404040404040404020202040402020202020404020202020204
              0402020404020202020202020204040404040404040404040404040404040404
              0202020202020202020202020202020202020202020202020202020202020202
              0202020404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040202020204040404040404040404040402040404
              0404040404040202020202020202020202020202020202020202020202020202
              0404040202020202020202020404040402020204040404040404040404040404
              0402020204040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0402020202020202020204040404040404040402040404020202020202040402
              0202020404040404040402020402020204040404040404040404040404040404
              0404040402020202020202020202020202020202020202020202020202020202
              0202020202020202040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404020202020404040404040404040404
              0404040404040404040402020204020202020202020202020202020202020202
              0202020204020404020204040404040404040404040404040404040404040404
              0404040404040204040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040202020202020204040404040404040404040404040404040204
              0404040404040404040402020404040404040404040404040404040404040404
              0404040404040404020202020202020202020202020202020202020202020202
              0202020202020202020202020204040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040402020202040404040404
              0404040404040404040404040404040404040404020202020202020202020202
              0202020202020202020202020404040404040404040404040404040404040404
              0404040404040402020404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040402020202020204040404040402
              0202040404020202040202020202020402020202040404040404040404040404
              0404040404040404040404020202020202020202020202020202020202020202
              0202020202020202020202020202020204040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404020202020204
              0404040404040404040404040404040404040404040404040202020202020202
              0202020202020202020202020202020202020404040404040404040404040404
              0404040404040404040402020202020404040402020204040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040202020202020202020202
              0404020202020202040202040202020202020204020202020204040404040404
              0404040404040404040404040404020202020202020202020202020202020202
              0202020202020202020202020202020202020202020404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0202020202020404040404040404040404040404040404040404040404020202
              0202020202020202020202020202020202020202020202020404040404040404
              0404040404040404040404040402020202020202040404020202040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040202020204040404020202020202
              0202020204020202020202020404040402020202040404020402020404040404
              0404040404040404040202040404040402020202020202020202020202020202
              0202020202020202020202020202020202020202020202020404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404020202020202040404040404040404040404040404040404040404
              0404040202020202020202020202020202020202020202020202020204040404
              0404040404040404040404040404040404040202020202040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404020202020204040402
              0204040202020204020202020202020204020202020202020402020204040202
              0202040404040404040404040202020202020402020202020202020202020202
              0202020202020202020202020202020202020202020202020202020204040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404020204020202020204040404040404040404040404
              0404040404040404040402020402040402020202020202020202020202040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040202020202
              0404040404040404040404040404040404040204040402020404020402020204
              0202020202020204040404040404020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202040404040404040404040404040404040404040404040404040404040404
              0404040204040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040402020202040404040404040404
              0404040404040404040404040404040404040404040402020202020202040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0402020202040404040202040404040404020404040404040404020404040404
              0402020402020202020202040404040404040402020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020404040404040404040404040404040404040404040404040404
              0404040404040202020404040202040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040202040404040404
              0404040404040404040404040404040404040404040404040404020202020204
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040402020202020204040404020402020204040202020202
              0402020204040204020202020202020202040404020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020404040404040404040404040404040404040404040404
              0404040404040404020202020202040202020404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0402040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040202020202040404040402020202020202
              0202020402020202020402040202020202020202020204040202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020204040404040404040404040404040404040404
              0404040404040404040404040402020202020402020404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404020202020404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040402020404040402020202
              0202040202040404040202020202040402020202020202020202040404020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202040404040404040404040404040404
              0404040404040404040404040404020204020202020202040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0402020204040202020202040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040402
              0202020202040404040404040202020202020404020202020202020202020404
              0404040202020202020202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020404040404040404040404
              0404040404040404040404040404040404040202020202020202040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404020202020404040202040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040402020202040402020404040202020202020204040202020202020202
              0202020204040404040202020202020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020404040404
              0404040404040404040404040404040404040404040202020202020202040404
              0202020204040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040402020404020202020204040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040402020204040202020202020404040404020202
              0202020202020202020202040404020202020202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0404040404040404040404040404040404040404040404040404020404040402
              0402020202020202020204040404040404040404040404040404040404020202
              0404020204040202040402040404040404040404040404040404040404040404
              0404040404040404040202020404020204040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040204040402020202040402020202
              0404040404040202020202020202020204040202020202020202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202040404040404040404040404040404040404040404040404040404
              0404040404020202040202020404040404040404040404040404040404040402
              0202020202040404020404020402020204040404040404040404040404040404
              0404040404040404040404040402020202040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040202040402
              0202020402020202020202020202020202020202020204040202020202020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020202020202040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040402020202020404040402020204040204020202040404040404040404
              0404040404040404040404040404040404040402020202040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404020202020202020202020202020202020202020202020202020404020202
              0202020202020202020202020202020202020202020202020202020202020202
              0202020202020404020202020202020404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040202020404040402020404040404
              0404040404040404040404040404040404040404040404040204040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404020202020202020202020202020202020202020202020202
              0404040202020202020202020202020202020202020202020202020202020202
              0202020202020204020202040404020202020404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404020204040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040402020202020202020202020202020202020202
              0202020202040404040404040202020202020202020202020202020202020202
              0202020202020202020202020202020404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040402020202020202020202
              0202020202020202040404040404040404040404040404020202020202020202
              0202020202020202020202020202020202020204040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040402020202
              0202020202020202020202040404040404040404040404040404040404040402
              0202020202020202020202020202020202020202020204040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040402020202020202020202020202020202040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404020202020202020202020202040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404040404040404040404040404040404040404040404040404040404040404
              0404}
            Stretch = True
          end
        end
        object gbStd: TGroupBox
          Left = 11
          Top = 369
          Width = 669
          Height = 62
          Anchors = [akLeft, akRight, akBottom]
          Caption = ' Standard Time '
          TabOrder = 1
          object Label12: TLabel
            Left = 12
            Top = 29
            Width = 50
            Height = 13
            Alignment = taRightJustify
            Caption = 'Start Date'
          end
          object Label73: TLabel
            Left = 236
            Top = 29
            Width = 19
            Height = 13
            Alignment = taRightJustify
            Caption = 'Bias'
          end
          object Label74: TLabel
            Left = 321
            Top = 29
            Width = 20
            Height = 13
            Caption = 'min.'
          end
          object eStdStart: TEdit
            Left = 72
            Top = 25
            Width = 146
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 0
          end
          object eStdBias: TEdit
            Left = 260
            Top = 25
            Width = 53
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 1
          end
        end
        object eTZ: TEdit
          Left = 8
          Top = 9
          Width = 675
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 2
        end
        object gbDay: TGroupBox
          Left = 11
          Top = 437
          Width = 669
          Height = 62
          Anchors = [akLeft, akRight, akBottom]
          Caption = ' Daylight Time '
          TabOrder = 3
          object Label75: TLabel
            Left = 12
            Top = 29
            Width = 50
            Height = 13
            Alignment = taRightJustify
            Caption = 'Start Date'
          end
          object Label76: TLabel
            Left = 236
            Top = 29
            Width = 19
            Height = 13
            Alignment = taRightJustify
            Caption = 'Bias'
          end
          object Label77: TLabel
            Left = 321
            Top = 29
            Width = 20
            Height = 13
            Caption = 'min.'
          end
          object edayStart: TEdit
            Left = 72
            Top = 25
            Width = 146
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 0
          end
          object eDayBias: TEdit
            Left = 260
            Top = 25
            Width = 53
            Height = 21
            Color = clBtnFace
            ReadOnly = True
            TabOrder = 1
          end
        end
      end
      object tsStartup: TTabSheet
        Caption = ' Startup '
        ImageIndex = 15
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          692
          533)
        object tcStartup: TTabControl
          Left = 13
          Top = 10
          Width = 667
          Height = 517
          Cursor = crHandPoint
          Anchors = [akLeft, akTop, akRight, akBottom]
          MultiLine = True
          TabOrder = 0
          Tabs.Strings = (
            ' Startup folders '
            ' Registry '
            ' INI files '
            ' Browser Helper Objects '
            ' Task Scheduler ')
          TabIndex = 0
          OnChange = tcStartupChange
          object lvStartup: TListView
            Left = 4
            Top = 24
            Width = 659
            Height = 489
            Align = alClient
            Columns = <
              item
                Caption = 'Name'
                Width = 150
              end
              item
                Caption = 'Location'
                Width = 150
              end
              item
                Caption = 'Command line'
                Width = 300
              end>
            ColumnClick = False
            ReadOnly = True
            RowSelect = True
            TabOrder = 0
            ViewStyle = vsReport
          end
        end
      end
      object tsSoftware: TTabSheet
        Caption = ' Software '
        ImageIndex = 16
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          692
          533)
        object pcSW: TPageControl
          Left = 8
          Top = 6
          Width = 675
          Height = 521
          Cursor = crHandPoint
          ActivePage = tsSWMSP
          Anchors = [akLeft, akTop, akRight, akBottom]
          TabOrder = 0
          object tsSWApp: TTabSheet
            Caption = ' Applications '
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object lvSW: TListView
              Left = 0
              Top = 0
              Width = 667
              Height = 493
              Align = alClient
              Columns = <
                item
                  Caption = 'Product Name'
                  Width = 350
                end
                item
                  Caption = 'Version'
                  Width = 70
                end
                item
                  Caption = 'Date'
                  Width = 80
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
          object tsSWMSP: TTabSheet
            Caption = 'MS Products'
            ImageIndex = 1
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object lvMSP: TListView
              Left = 0
              Top = 0
              Width = 667
              Height = 493
              Align = alClient
              Columns = <
                item
                  Caption = 'Product Name'
                  Width = 250
                end
                item
                  Caption = 'Product ID'
                  Width = 160
                end
                item
                  Caption = 'Product Key'
                  Width = 250
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
      object tsProcesses: TTabSheet
        Caption = ' Processes '
        ImageIndex = 18
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object ProcList: TListView
          Left = 0
          Top = 0
          Width = 692
          Height = 533
          Align = alClient
          Columns = <
            item
              Caption = 'Name'
              Width = 100
            end
            item
              Alignment = taRightJustify
              Caption = 'PID'
              Width = 70
            end
            item
              Caption = 'Type'
              Width = 40
            end
            item
              Alignment = taRightJustify
              Caption = 'CPU Time'
              Width = 75
            end
            item
              Alignment = taRightJustify
              Caption = 'Mem Usage'
              Width = 90
            end
            item
              Caption = 'Image Name'
              Width = 200
            end>
          ColumnClick = False
          ReadOnly = True
          RowSelect = True
          TabOrder = 0
          ViewStyle = vsReport
        end
      end
      object tsDrivers: TTabSheet
        Caption = ' Drivers '
        ImageIndex = 19
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object DrvList: TListView
          Left = 0
          Top = 0
          Width = 692
          Height = 533
          Align = alClient
          Columns = <
            item
              Caption = 'Name'
              Width = 150
            end
            item
              Alignment = taRightJustify
              Caption = 'Load Cnt'
              Width = 60
            end
            item
              Caption = 'Location'
              Width = 320
            end>
          ColumnClick = False
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          TabOrder = 0
          ViewStyle = vsReport
        end
      end
      object tsServices: TTabSheet
        Caption = ' Services '
        ImageIndex = 20
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object SvcList: TListView
          Left = 0
          Top = 0
          Width = 692
          Height = 533
          Align = alClient
          Columns = <
            item
              Caption = 'Display Name'
              Width = 220
            end
            item
              Caption = 'Typ'
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
              Caption = 'Image Name'
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
      object tsBT: TTabSheet
        Caption = 'Bluetooth'
        ImageIndex = 22
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lvBT: TListView
          Left = 0
          Top = 0
          Width = 692
          Height = 533
          Align = alClient
          Columns = <
            item
              Caption = 'Device'
              Width = 150
            end
            item
              Caption = 'Address'
              Width = 150
            end
            item
              Caption = 'Last Used'
              Width = 120
            end
            item
              Caption = 'Last Seen'
              Width = 120
            end
            item
              Caption = 'Authenticated'
            end
            item
              Caption = 'Remembered'
            end
            item
              Caption = 'Connected'
            end>
          ColumnClick = False
          ReadOnly = True
          RowSelect = True
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          ViewStyle = vsReport
        end
      end
      object tsEL: TTabSheet
        Caption = 'Event Log'
        ImageIndex = 23
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Bevel4: TBevel
          Left = 0
          Top = 21
          Width = 692
          Height = 9
          Align = alTop
          Shape = bsSpacer
          ExplicitWidth = 678
        end
        object cbEL: TComboBox
          Left = 0
          Top = 0
          Width = 692
          Height = 21
          Cursor = crHandPoint
          Align = alTop
          Style = csDropDownList
          TabOrder = 0
          OnChange = cbELChange
        end
        object lvEL: TListView
          Left = 0
          Top = 30
          Width = 692
          Height = 414
          Align = alClient
          Columns = <
            item
              Caption = '   Timestamp'
              Width = 150
            end
            item
              Caption = 'Source'
              Width = 100
            end
            item
              Caption = 'Category'
              Width = 100
            end
            item
              Alignment = taRightJustify
              Caption = 'Event'
            end
            item
              Caption = 'User'
              Width = 100
            end
            item
              Caption = 'Machine'
              Width = 100
            end>
          ColumnClick = False
          ReadOnly = True
          RowSelect = True
          SmallImages = ilEvents
          TabOrder = 1
          ViewStyle = vsReport
          OnSelectItem = lvELSelectItem
        end
        object mEL: TMemo
          Left = 0
          Top = 444
          Width = 692
          Height = 89
          Align = alBottom
          ScrollBars = ssVertical
          TabOrder = 2
        end
      end
      object tsAD: TTabSheet
        Caption = 'Active Directory'
        ImageIndex = 24
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object tvAD: TTreeView
          Left = 0
          Top = 0
          Width = 258
          Height = 513
          Align = alLeft
          Images = ilAD
          Indent = 19
          ReadOnly = True
          RowSelect = True
          SortType = stText
          TabOrder = 0
          OnChange = tvADChange
          OnDeletion = tvADDeletion
        end
        object lvAD: TListView
          Left = 258
          Top = 0
          Width = 434
          Height = 513
          Align = alClient
          Columns = <
            item
              Caption = 'Property'
              Width = 150
            end
            item
              Caption = 'Value'
              Width = 400
            end>
          ColumnClick = False
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          SortType = stText
          TabOrder = 1
          ViewStyle = vsReport
        end
        object pAD: TPanel
          Left = 0
          Top = 513
          Width = 692
          Height = 20
          Align = alBottom
          Alignment = taLeftJustify
          BevelOuter = bvNone
          TabOrder = 2
        end
      end
      object tsSC: TTabSheet
        Caption = ' Security '
        ImageIndex = 24
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object pcSec: TPageControl
          Left = 0
          Top = 0
          Width = 692
          Height = 533
          Cursor = crHandPoint
          ActivePage = TabSheet7
          Align = alClient
          TabOrder = 0
          object TabSheet7: TTabSheet
            Caption = 'Software'
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object lvAS: TListView
              Left = 0
              Top = 151
              Width = 684
              Height = 115
              Align = alTop
              Columns = <
                item
                  Caption = 'Product'
                  Width = 200
                end
                item
                  Caption = 'Path'
                  Width = 500
                end>
              ColumnClick = False
              ReadOnly = True
              RowSelect = True
              TabOrder = 0
              ViewStyle = vsReport
            end
            object lvAV: TListView
              Left = 0
              Top = 18
              Width = 684
              Height = 115
              Align = alTop
              Columns = <
                item
                  Caption = 'Product'
                  Width = 200
                end
                item
                  Caption = 'Path'
                  Width = 500
                end>
              ColumnClick = False
              ReadOnly = True
              RowSelect = True
              TabOrder = 1
              ViewStyle = vsReport
            end
            object lvFW: TListView
              Left = 0
              Top = 284
              Width = 684
              Height = 115
              Align = alTop
              Columns = <
                item
                  Caption = 'Product'
                  Width = 200
                end
                item
                  Caption = 'Path'
                  Width = 500
                end>
              ColumnClick = False
              ReadOnly = True
              RowSelect = True
              TabOrder = 2
              ViewStyle = vsReport
            end
            object pAS: TPanel
              Left = 0
              Top = 133
              Width = 684
              Height = 18
              Align = alTop
              Alignment = taLeftJustify
              BevelOuter = bvNone
              Caption = ' AntiSpyware'
              Color = clGray
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindow
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
              TabOrder = 3
            end
            object pAV: TPanel
              Left = 0
              Top = 0
              Width = 684
              Height = 18
              Align = alTop
              Alignment = taLeftJustify
              BevelOuter = bvNone
              Caption = ' AntiViruses'
              Color = clGray
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindow
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
              TabOrder = 4
            end
            object pFW: TPanel
              Left = 0
              Top = 266
              Width = 684
              Height = 18
              Align = alTop
              Alignment = taLeftJustify
              BevelOuter = bvNone
              Caption = ' Firewalls'
              Color = clGray
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindow
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
              TabOrder = 5
            end
          end
          object TabSheet8: TTabSheet
            Caption = 'Windows Firewall'
            ImageIndex = 1
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object lvWFW: TListView
              Left = 0
              Top = 28
              Width = 684
              Height = 477
              Align = alClient
              Columns = <
                item
                  Caption = 'Rule'
                  Width = 150
                end
                item
                  Caption = 'Description'
                  Width = 150
                end
                item
                  Caption = 'Application Name'
                  Width = 120
                end
                item
                  Caption = 'Service Name'
                  Width = 120
                end
                item
                  Caption = 'Protocol'
                end
                item
                  Caption = 'Local Ports'
                  Width = 100
                end
                item
                  Caption = 'Remote Ports'
                  Width = 100
                end
                item
                  Caption = 'Local Addresses'
                  Width = 120
                end
                item
                  Caption = 'Remote Addresses'
                  Width = 100
                end
                item
                  Caption = 'ICMP types and codes'
                  Width = 100
                end
                item
                  Caption = 'Direction'
                end
                item
                  Caption = 'Enabled'
                end
                item
                  Caption = 'Edge'
                end
                item
                  Caption = 'Action'
                  Width = 150
                end
                item
                  Caption = 'Grouping'
                  Width = 100
                end
                item
                  Caption = 'Interface types'
                  Width = 100
                end>
              ColumnClick = False
              HideSelection = False
              ReadOnly = True
              RowSelect = True
              TabOrder = 0
              ViewStyle = vsReport
            end
            object Panel8: TPanel
              Left = 0
              Top = 0
              Width = 684
              Height = 28
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 1
              object cbxDomain: TCheckBox
                Left = 10
                Top = 6
                Width = 97
                Height = 17
                Caption = 'Domain Profile'
                Enabled = False
                TabOrder = 0
              end
              object cbxPublic: TCheckBox
                Left = 142
                Top = 6
                Width = 97
                Height = 17
                Caption = 'Public Profile'
                Enabled = False
                TabOrder = 1
              end
              object cbxPrivate: TCheckBox
                Left = 280
                Top = 6
                Width = 97
                Height = 17
                Caption = 'Private Profile'
                Enabled = False
                TabOrder = 2
              end
            end
          end
        end
      end
      object tsWIFI: TTabSheet
        Caption = 'Wi-Fi'
        ImageIndex = 25
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object pcWIFI: TPageControl
          Left = 0
          Top = 0
          Width = 692
          Height = 533
          Cursor = crHandPoint
          ActivePage = TabSheet4
          Align = alClient
          TabOrder = 0
          object TabSheet4: TTabSheet
            Caption = 'Available'
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object lvWIFI: TListView
              Left = 0
              Top = 0
              Width = 684
              Height = 505
              Align = alClient
              Columns = <
                item
                  Caption = '    SSID'
                  Width = 200
                end
                item
                  Alignment = taRightJustify
                  Caption = 'Signal'
                end
                item
                  Caption = 'Authentication'
                  Width = 100
                end
                item
                  Caption = 'Security'
                  Width = 75
                end
                item
                  Caption = 'PHY Type'
                  Width = 100
                end
                item
                  Caption = 'BSS Type'
                  Width = 80
                end
                item
                  Caption = 'MAC Address'
                  Width = 120
                end
                item
                  Alignment = taRightJustify
                  Caption = 'RSSI'
                  Width = 75
                end
                item
                  Alignment = taRightJustify
                  Caption = 'Channel Freq'
                  Width = 100
                end
                item
                  Alignment = taRightJustify
                  Caption = 'Channel #'
                end
                item
                  Alignment = taRightJustify
                  Caption = 'Max Speed'
                  Width = 75
                end
                item
                  Caption = 'Interface'
                  Width = 250
                end>
              ColumnClick = False
              ReadOnly = True
              RowSelect = True
              SmallImages = ilWIFI
              SortType = stText
              TabOrder = 0
              ViewStyle = vsReport
            end
          end
          object TabSheet5: TTabSheet
            Caption = 'Known'
            ImageIndex = 1
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object lvWLANC: TListView
              Left = 0
              Top = 0
              Width = 684
              Height = 505
              Align = alClient
              Columns = <
                item
                  Caption = 'SSID'
                  Width = 150
                end
                item
                  Caption = 'Key'
                  Width = 150
                end
                item
                  Caption = 'Authentication'
                  Width = 90
                end
                item
                  Caption = 'Encryption'
                  Width = 70
                end
                item
                  Caption = 'Connection'
                  Width = 70
                end
                item
                  Caption = 'Adapter name'
                  Width = 100
                end
                item
                  Caption = 'IP Address'
                  Width = 100
                end
                item
                  Caption = 'Timestamp'
                  Width = 120
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
    end
  end
  object psd: TPrinterSetupDialog
    Left = 136
    Top = 372
  end
  object ilEng: TImageList
    ColorDepth = cd32Bit
    Left = 218
    Top = 366
    Bitmap = {
      494C010103000500040010001000FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000D0D0D1C676767FF676767FF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000090C0E1D283036803C464BC5465158E647535BE73E484ECA2B353B890C10
      1326000000000000000000000000000000000000000000000000000000000000
      0000000000000000000030303030EFEFEFEFEFEFEFEF30303030000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000303
      030700000000000000002828285B676767FF676767FF17171735000000000000
      0000282828520000000000000000000000000000000000000000000101023055
      6C7C4D5D66F84A4A4AFF4A4A4AFF4A4A4AFF4A4A4AFF4A4A4AFF4A4A4AFF4B51
      54FC365D758F0204050600000000000000000000000000000000000000000000
      00000000000030303030EFEFEFEF858585FF858585FFEFEFEFEF303030300000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000006B6B6BFF6767
      67FF1E1E1E4810101028676767F9676767FF676767FF616161EE0F0F0F254B4B
      4BB6676767FF6A6A6AFF00000000000000000000000000010102407595A365B7
      EAFF5D99BEFF4A4A4AFF4A4A4AFF4A4A4AFF4A4A4AFF4A4A4AFF4A4A4AFF5782
      9CFF65B7EAFF4984A9B902050607000000000000000000000000000000000000
      000030303030EFEFEFEF858585FF494949FF484848FF858585FFEFEFEFEF3030
      3030000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000002626264F676767FF6767
      67FF676767FF676767FF676767FF676767FF676767FF676767FF676767FF6767
      67FF676767FF676767FF03030308000000000000000030566F7965B7EAFF65B7
      EAFF65B7EAFF526D7EFF4A4A4AFF4A4A4AFF4A4A4AFF4A4A4AFF4E5C64FF64B4
      E6FF65B7EAFF65B7EAFF3A6A8794000000000000000000000000000000003030
      3030EFEFEFEF858585FF4A4A4AFF4C4C4CFF4C4C4CFF484848FF858585FFEFEF
      EFEF303030300000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004A4A4AB46767
      67FF676767FF676767FF676767FF676767FF676767FF676767FF676767FF6767
      67FF676767FF1D1D1D4900000000000000000911161962B0E1F665B7EAFF65B7
      EAFF65B7EAFF62AAD8FF4B4F51FF4A4A4AFF4A4A4AFF4A4A4BFF5E9EC5FF65B7
      EAFF65B7EAFF65B7EAFF64B5E8FD12212B2F000000000000000030303030EFEF
      EFEF868686FF4C4C4CFF4E4E4EFFDADADAFFDADADAFF4C4C4CFF484848FF8585
      85FFEFEFEFEF3030303000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000101010276767
      67FF676767FF676767FF676767FF545454D0545454CF676767FF676767FF6767
      67FF676767FF0808081400000000000000003158707B65B7EAFF65B7EAFF65B7
      EAFF65B7EAFF65B7EAFF5885A0FF4A4A4AFF4A4A4AFF55768BFF65B7EAFF65B7
      EAFF65B7EAFF65B7EAFF65B7EAFF3B6B89960000000030303030EFEFEFEF8787
      87FF505050FF4F4F4FFFDADADAFF9F9F9F9F9F9F9F9FDADADAFF4C4C4CFF4444
      44FF848484FFEFEFEFEF30303030000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001414142D616161EC6767
      67FF676767FF676767FF00000000000000000000000000000000676767FF6767
      67FF676767FF646464F42C2C2C650F0F0F214C89AFBF65B7EAFF65B7EAFF65B7
      EAFF65B7EAFF65B7EAFF64B4E6FF4E5B63FF4C5459FF63B0E0FF65B7EAFF65B7
      EAFF65B7EAFF65B7EAFF65B7EAFF569BC7D930303030EFEFEFEF888888FF5353
      53FF525252FFDBDBDBFF9F9F9F9F00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000676767FF676767FF676767FF6767
      67FF676767FF575757D7000000000000000000000000000000005C5C5CE36767
      67FF676767FF676767FF676767FF676767FF579FCBDF65B7EAFF65B7EAFF65B7
      EAFF65B7EAFF65B7EAFF65B7EAFF5E9BC1FF5C93B5FF65B7EAFF65B7EAFF65B7
      EAFF65B7EAFF65B7EAFF65B7EAFF63B3E5FAEFEFEFEF8A8A8AFF585858FF5555
      55FFDBDBDBFF9F9F9F9F0000000000000000FFFFFFFF474747FF494949FF4848
      48FF474747FF474747FF434343FFFFFFFFFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000676767FF676767FF676767FF6767
      67FF676767FF5A5A5ADF00000000000000000000000000000000606060ED6767
      67FF676767FF676767FF676767FF676767FF454F57E14A4A4AFF4A4A4AFF4A4A
      4AFF4A4A4AFF4A4A4AFF4A4A4AFF506572FF526C7DFF4A4C4EFF4A4C4EFF4A4C
      4EFF4A4C4EFF4A4C4EFF4A4C4EFF4C575EFBEFEFEFEF8B8B8BFF5B5B5BFF5757
      57FFDBDBDBFF9F9F9F9F0000000000000000FFFFFFFF4D4D4DFF4D4D4DFF4C4C
      4CFF4B4B4BFF4A4A4AFF464646FFFFFFFFFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000E0E0E1F2A2A2A61656565F56767
      67FF676767FF676767FF0606060F00000000000000000C0C0C1C676767FF6767
      67FF676767FF5B5B5BE015151532000000003B454BC14A4A4AFF4A4A4AFF4A4A
      4AFF4A4A4AFF4A4A4AFF4A4A4AFF5E9CC2FF5FA0C9FF4A4A4AFF4A4A4AFF4A4A
      4AFF4A4A4AFF4A4A4AFF4A4A4AFF435058DB30303030EFEFEFEF8C8C8CFF5C5C
      5CFF585858FFDCDCDCFF9F9F9F9F00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000505050D6767
      67FF676767FF676767FF676767FF626262F3636363F6676767FF676767FF6767
      67FF676767FF0404040B0000000000000000283239804A4A4AFF4A4A4AFF4A4A
      4AFF4A4A4AFF4A4A4AFF506572FF65B7EAFF65B7EAFF526A7AFF4A4A4AFF4A4A
      4AFF4A4A4AFF4A4A4AFF4A4A4AFF303C439A0000000030303030EFEFEFEF8D8D
      8DFF5C5C5CFF595959FFDCDCDCFF9F9F9F9F9F9F9F9FDCDCDCFF535353FF4A4A
      4AFF888888FFEFEFEFEF30303030000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000001B1B1B416767
      67FF676767FF676767FF676767FF676767FF676767FF676767FF676767FF6767
      67FF676767FF4F4F4FC10000000000000000090D0F1D494F52F84A4A4AFF4A4A
      4AFF4A4A4AFF4A4A4AFF5E9CC3FF65B7EAFF65B7EAFF5FA1CAFF4A4A4AFF4A4A
      4AFF4A4A4AFF4A4A4AFF4A4C4EFE10151933000000000000000030303030EFEF
      EFEF8E8E8EFF5C5C5CFF5A5A5AFFDCDCDCFFDCDCDCFF565656FF515151FF8B8B
      8BFFEFEFEFEF3030303000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000001010102686868FF6767
      67FF676767FF676767FF676767FF676767FF676767FF676767FF676767FF6767
      67FF676767FF676767FF2A2A2A59000000000000000029353D824A4A4AFF4A4A
      4AFF4A4A4AFF516673FF65B7EAFF65B7EAFF65B7EAFF65B7EAFF526B7BFF4A4A
      4AFF4A4A4AFF4A4A4AFF313E479B000000000000000000000000000000003030
      3030EFEFEFEF8F8F8FFF5D5D5DFF5B5B5BFF595959FF555555FF8D8D8DFFEFEF
      EFEF303030300000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000006A6A6AFF6767
      67FF4E4E4EC003030308595959D9676767FF676767FF606060EC000000001B1B
      1B42676767FF696969FF0000000000000000000000000001010337464FAD4A4A
      4AFF4A4A4AFF5E9DC4FF65B7EAFF65B7EAFF65B7EAFF65B7EAFF5FA2CCFF4A4A
      4AFF4A4A4AFF3C4A52C202030409000000000000000000000000000000000000
      000030303030EFEFEFEF909090FF5D5D5DFF5B5B5BFF8F8F8FFFEFEFEFEF3030
      3030000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000002727
      275400000000000000001414142D676767FF676767FF2E2E2E6B000000000000
      0000010101030000000000000000000000000000000000000000010101032B39
      4186516B7CFA65B7EAFF65B7EAFF65B7EAFF65B7EAFF65B7EAFF65B7EAFF536F
      82FD303D44980203040900000000000000000000000000000000000000000000
      00000000000030303030EFEFEFEF919191FF919191FFEFEFEFEF303030300000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000676767FF676767FF11111126000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000D161C2436627D8A5294BDCE60ADDDF160AEDFF35498C2D439698593111E
      252E000000000000000000000000000000000000000000000000000000000000
      0000000000000000000030303030EFEFEFEFEFEFEFEF30303030000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FC7FF00FFC3F0000EC37C003F81F0000
      C0038001F00F000080018001E0070000C0030000C0030000C003000080010000
      83C000000100000003C000000300000003C00000030000000181000001000000
      C003000080010000C0030000C003000080018001E0070000C0238001F00F0000
      EC37C003F81F0000FE3FF00FFC3F000000000000000000000000000000000000
      000000000000}
  end
  object ilEvents: TImageList
    Left = 276
    Top = 392
    Bitmap = {
      494C010105000900040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005153D5FF0A15B0FF9E9E
      DBFFF6F9FCFF2C34BDFF1E2ABEFF83BAE5FF1CA9D2FF0A9CCCFF179AC9FF309D
      C7FF57AFD2FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000002D2EBBFF2C72FFFF1433
      D4FF1B33C4FF174EFFFF155CFFFF273AC6FF82F7FFFF00D9FFFF00B5E9FF00AE
      E2FF0197CBFF44A6CEFF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009F9FE8FF1225C5FF2C67
      FFFF255DFFFF205BFFFF1220BBFF86DFF3FF15E9FFFF00D3FFFF00C3F6FF00B6
      E9FF00ACDFFF088CBFFF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BDBDE4FF1022
      BFFF2D66FFFF1C49F4FF6178CDFF76FDFFFF00E7FFFF00D3FFFF00C3F6FF00B6
      E9FF00AADDFF098DBEFF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000002D2DAFFF3F7D
      FFFF1C3FDDFF2961FFFF1123C5FF86DFF3FF15E9FFFF00D3FFFF00C3F6FF00B6
      E9FF00AADDFF0B8FC0FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000161EC2FF396D
      FFFF6C82D0FF363BB8FF2B6DFFFF1F2DC0FF88F6FFFF00D5FFFF00C4F9FF00B8
      ECFF00ABDEFF0B90C1FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000004141
      C3FF9BE4F5FF97D1F0FF2731BEFF98B6EDFF64CEECFF14B3E0FF0AAADAFF019F
      D3FF0099CEFF0B93C5FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00003DE6FFFF659DB3FFA5B0B8FF8AE1EFFF5BECFFFF4AE7FFFF3AE6FFFF28BA
      E0FF176E87FF0C87B8FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A9B8B9FF70706EFF37D8EBFF3BCAEBFF32C8EBFF1FC6EBFF84A6
      B2FF5B524FFF49AFD7FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000E6DEDEFF7F716EFF00000000000000000000000000000000B5B7
      B9FF6F6966FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000F3F3F3FF726C6AFF00000000000000000000000000000000C1BD
      BBFF73706FFF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000F5F5F5FF939090FF8C8785FF0000000000000000000000009696
      96FF989898FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E3E3E3FF8E8B8AFF797473FF7B7776FF848180FF9898
      98FFD6D6D6FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000DBDBDBFFBEBEBEFFA9A9A9FFA6A6A6FFD8D8
      D8FF000000000000000000000000000000000000000000000000000000000000
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
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484D6004A4ABD004239B5004A4ABD008484D600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009C3100009C310000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006B6B
      C6000000AD000008BD000010CE000010CE000010CE000008BD000000AD006B6B
      C600000000000000000000000000000000000000000063CEFF00319CCE00319C
      CE00319CCE00319CCE00319CCE00319CCE00319CCE00319CCE00319CCE00319C
      CE00319CCE0063CEFF0000000000000000000000000000000000000000000000
      00000000000000000000000000009C310000FFFFFF009C310000000000000000
      00000000000000000000000000000000000000000000000000000550BDFF25A3
      D2FF0CA6CEFF019ACBFF0896CBFF91CCE1FF58A879FF248324FF166F09FF1771
      1EFF2D8B6CFF00000000000000000000000000000000000000003129AD000008
      BD000010CE000010CE000010CE000010CE000010CE000010CE000010CE000008
      BD003129AD0000000000000000000000000063CEFF00319CCE0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF00319CCE0063CEFF00000000000000000000000000000000000000
      000000000000000000009C310000FFFFFF00FFFFFF009C310000000000000000
      000000000000000000000000000000000000000000000F92D0FFABFFFFFF4CFF
      FFFF00FFFFFF04EEFFFF8CEAF9FF2B9E3FFF009A00FF00A207FF009700FF0E89
      00FF2E7100FF197531FF0000000000000000000000004A4ABD001018C6008484
      E7002129D6000010CE000010CE000010CE000010CE00636BDE008484E7001018
      CE000008BD004A4ABD000000000000000000319CCE0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF00319CCE0000316300319CCE0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF00319CCE0000000000000000000000000000000000CE9C
      63009C3100009C3100009C310000FFFFFF00FFFFFF009C3100009C310000CE9C
      630000000000000000000000000000000000000000001DB7DEFFC6FFFFFF43FF
      FFFF00FCFFFF4DEEFFFF5ABE8AFF0DA71AFF2CB53FFFFDFBFFFF6FD488FF0098
      00FF009500FF307000FF83B483FF00000000000000000000AD009494E700FFFF
      FF00FFFFFF00636BDE000010CE001018CE00C6C6F700FFFFFF00DEE7FF008484
      E7000010CE000000AD000000000000000000319CCE0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000316300003163000031630000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF00319CCE000000000000000000000000009C310000CE9C
      6300FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00CE9C
      63009C3100000000000000000000000000000000000022BEE1FFC5FFFFFF43FF
      FFFF00FCFFFF89F4FFFF36AC4CFF2EB33CFFF5ECF4FFFFF6FFFFFFFFFFFF6FD5
      88FF009A00FF128800FF368126FF000000005A5AC6000010CE000010CE001018
      CE00C6C6F700FFFFFF00636BDE00D6D6F700EFF7FF00525ADE000010CE000010
      CE000010CE000008C6005A5ABD0000000000319CCE0063CEFF0000FFFF0000FF
      FF0000FFFF0000FFFF00319CCE0000316300319CCE0000FFFF0000FFFF0000FF
      FF0000FFFF0063CEFF00319CCE0000000000000000009C310000FFFFFF00FFFF
      FF00FFFFFF009C3100009C3100009C3100009C3100009C310000FFFFFF00FFFF
      FF00FFFFFF009C31000000000000000000000000000022C1E3FFC5FFFFFF43FF
      FFFF00FCFFFF9CF6FFFF36AE45FF5DB964FFC0D6BCFF03AD21FF9DDBA7FFFFFF
      FFFF6DD487FF009500FF1F750AFF000000002121BD000018E7000018D6000010
      CE000010CE00B5B5EF00FFFFFF00EFF7FF003139D6000010CE000010CE000010
      CE000010CE000010CE002121AD000000000063CEFF00319CCE0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF00319CCE0063CEFF0000000000CE9C6300CE9C6300FFFFFF00FFFF
      FF00FFFFFF00FFFFFF009C3100009C3100009C310000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00CE9C6300CE9C6300000000000000000021C4E5FFCAFFFFFF46FF
      FFFF00FFFFFF89F6FFFF41B65CFF43CD6DFF19BB42FF25C04FFF0BB22EFF9DD9
      A5FFFFFFFFFF67CF7CFF308828FF000000000008BD000021EF000018E7000018
      DE000018DE00B5B5EF00FFFFFF00DEE7FF001018CE000010CE000010CE000010
      CE000010CE000010CE000000A5000000000000000000319CCE0063CEFF0000FF
      FF0000FFFF0000FFFF0063CEFF000031630063CEFF0000FFFF0000FFFF0000FF
      FF0063CEFF00319CCE0000000000000000009C310000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF009C3100009C3100009C310000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF009C310000000000000000000017C2E7FF45EFFFFF1ACB
      EFFF19B5DEFF5DCCEBFF60BF88FF72DD96FF35CE6AFF2EC75EFF25BE4BFF09AF
      27FF8BD094FF4EBB59FF86BC86FF000000000008C6000029FF000021F7000021
      EF00737BEF00FFFFFF00848CE700EFF7FF00D6D6F7001018CE000010CE000010
      CE000010CE000010CE000000A500000000000000000063CEFF00319CCE0000FF
      FF0000FFFF0000FFFF00319CCE0000316300319CCE0000FFFF0000FFFF0000FF
      FF00319CCE0063CEFF0000000000000000009C310000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF009C3100009C3100009C310000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF009C31000000000000000000003DE6FFFF508FA8FF4059
      68FF51D3E7FF5DECFFFFABF1FAFF47BE61FF71DD96FF4AD37AFF29C252FF1CB7
      3BFF0DAA22FF03842FFF00000000000000004242CE001039FF000029FF002142
      FF00F7F7FF00C6C6F7000018DE00525AE700FFFFFF00D6D6F7003139D6000010
      CE000010CE000010CE004239B500000000000000000000000000319CCE0063CE
      FF0000FFFF0000FFFF0031639C000031630031639C0000FFFF0000FFFF0063CE
      FF00319CCE000000000000000000000000009C310000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF009C3100009C3100009C3100009C310000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF009C310000000000000000000000000000A9B8B9FF6E6E
      6CFF37D8EBFF3BCAEBFF38CAECFF98E3F1FF76BE80FF4AB256FF37B149FF31A2
      40FF31692AFF000000000000000000000000000000002942EF00214AFF00738C
      FF00FFFFFF003952FF000021EF000018E700636BEF00EFF7FF00FFFFFF003139
      D6000010CE000008BD008484D60000000000000000000000000063CEFF00319C
      CE0000FFFF0000FFFF0000316300003163000031630000FFFF0000FFFF00319C
      CE0063CEFF000000000000000000000000009C310000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF009C310000000000000000000000000000E6DEDEFF7F71
      6EFF00000000000000000000000000000000C9CCCDFFBCB9B7FF000000000000
      000000000000000000000000000000000000000000002129D6005A6BFF006B84
      FF00DEDEFF000021F7000021F7000021F7000018E7001029E700394AE7000010
      CE000010CE001010A5000000000000000000000000000000000000000000319C
      CE0063CEFF0000FFFF0000316300003163000031630000FFFF0063CEFF00319C
      CE00000000000000000000000000000000009C310000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00CE9C63009C310000CE9C6300FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF009C310000000000000000000000000000F3F3F3FF726C
      6AFF00000000000000000000000000000000C1BDBBFF73706FFF000000000000
      00000000000000000000000000000000000000000000000000002939DE006B7B
      FF005A6BFF00294AFF001039FF000029FF000021F7000021EF000018E7000018
      DE000000AD009494D600000000000000000000000000000000000000000063CE
      FF00319CCE0000FFFF00319CCE0000316300319CCE0000FFFF00319CCE0063CE
      FF0000000000000000000000000000000000CE9C6300CE9C6300FFFFFF00FFFF
      FF00FFFFFF00FFFFFF009C3100009C3100009C310000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00CE9C6300CE9C6300000000000000000000000000F5F5F5FF9390
      90FF8C8785FF000000000000000000000000969696FF989898FF000000000000
      0000000000000000000000000000000000000000000000000000000000002939
      DE006B7BFF006373FF00395AFF002142FF000029FF000029FF000018EF000008
      BD009494DE000000000000000000000000000000000000000000000000000000
      0000319CCE0063CEFF0000FFFF0000FFFF0000FFFF0063CEFF00319CCE000000
      000000000000000000000000000000000000000000009C310000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00CE9C63009C310000CE9C6300FFFFFF00FFFFFF00FFFF
      FF00FFFFFF009C3100000000000000000000000000000000000000000000E3E3
      E3FF8E8B8AFF797473FF787473FF83807FFF979797FF7F7C7BFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00004A52DE002931DE003142DE002131DE001029DE000010CE00524AC6000000
      0000000000000000000000000000000000000000000000000000000000000000
      000063CEFF00319CCE00319CCE00319CCE00319CCE00319CCE0063CEFF000000
      00000000000000000000000000000000000000000000000000009C310000CE9C
      6300FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00CE9C
      63009C3100000000000000000000000000000000000000000000000000000000
      0000DBDBDBFFBEBEBEFFA9A9A9FF9F9F9FFFAEAEAEFF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000063CEFF0063CEFF0063CEFF0000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CE9C
      63009C3100009C3100009C3100009C3100009C3100009C3100009C310000CE9C
      6300000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF00FFFF0000000000008007000000000000
      80030000000000008003000000000000C003000000000000C003000000000000
      C003000000000000E003000000000000F003000000000000F803000000000000
      F9E7000000000000F9E7000000000000F8E7000000000000FC07000000000000
      FE0F000000000000FFFF000000000000F83FFFFFFF3FFFFFE00F8003FE3FC007
      C0070001FC3F800380030001E00F800180030001C00780010001000180038001
      0001000100018001000180030001800100018003000180030001C0070001C007
      8001C0070001CF3F8003E00F0001CF3FC003E00F0001C73FE007F01F8003E03F
      F01FF01FC007F07FFFFFFC7FE00FFFFF00000000000000000000000000000000
      000000000000}
  end
  object ilAD: TImageList
    Left = 353
    Top = 295
    Bitmap = {
      494C010105000900040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A9B5A90057845800276829001E631F00246625005481
      5500B8BEB8000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000316F33001782250026B1480024AD45001DA13C001472
      1C00276627000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B8B8C300000000002166230032C557003EE26F0030C95A00136B17001892
      30000A510A000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008989B100272895000A0A
      8500101089000A0A8500114E1F0031BD500048EC7700166D1A0017621600147B
      2000155B15000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000202193000E21CF000F30
      FB000A26F9000A1CD9000A2167000B4F21000E4558000A34620018496A001057
      28004D7E4D000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000018188F001C3CED001E44
      FC000F29E5000C0D8C000A2D6D000A3563002F82B80049AEEC002A78AE000B39
      5E00323236007A5C5A00BEBBBB00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000232C93001049A7001845
      C0000E0F8F002F2D9A000A3462000D3E6F0057B3EB004CA2D90051B5F0003185
      B900173A5300763D1E00724F4D00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000115587000A7BB5000A53
      87000A346200184378000A3462000B3C6C000B3968000D4070000A3765000B3B
      6A000A3561008F4A1400633A3700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000115788000AB0EC000A55
      890085CBF8003D8ABB000A3563000A5589000A70A7000A6EA5000A5F94000A47
      78000D3662007A3D19006E4B4700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000D54860017AEE0000B55
      87004991BB00418DBC00286FA0000A3767000A689E000A74AC000A588C000A37
      6500123B64004233370099878700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000C53860023ABD4000D5D
      8F000F6294000E6293000A5487000A4373000D3258000A3462000A3B69000A58
      8C000A578B000D3660008593A200000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000245E8A00289BC20061ED
      FC0061EDFC004FE5FC0025A9D100145B8B000A0A0A000A0C0D001A84B2001C96
      CC000A83BC001562940020456D00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A3B1BA000E5889003CA6
      C20055D0E40033ADD000105C8E00889FB1000A0A0A000A0A0A000A0A0A000B0B
      0B000C1B21000D34490011305100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A4B1BB005181
      A1002A6994003C779B0090A5B40000000000111213000E0E0E001F1F1F002222
      2200131313000A0A0A002B303500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000848484000F101000171717003535
      35001B1B1B0010101100A9A9A900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000093939300353535002121
      210044444400ABABAB0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C5C4
      C400A29494007C5E5900663F39005127270051272700582E2C00724F4D00A79A
      9A00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000D0997EFFAF5430FFA25D3BFF9B3E13FFA54E34FFC69384FF0000
      0000000000000000000000000000000000000000000000000000F2F8FCFF148B
      D2FF148BD2FF148BD2FF148BD2FF148BD2FF148BD2FF148BD2FF148BD2FF148B
      D2FF148BD2FF148BD2FF148BD2FFFEFEFEFF000000000000000000000000876E
      6B0069392A009A561E00B7681A00D0761100CA6D0A00B15E0D00894716005E31
      2B0088706F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000A8A8A8FF676767FF393939FF5A5A5AFFABAB
      ABFF000000000000000000000000000000000000000000000000000000000000
      0000B85512FFAE9992FFAB724FFFA87862FFB7C8D3FFA36545FFA16F57FF9E52
      35FF0000000000000000000000000000000000000000148BD2FF148BD2FFC8FE
      FFFFCCFFFFFFCCFFFFFFCCFFFFFFCCFFFFFFCCFFFFFFCCFFFFFFCCFFFFFFCCFF
      FFFFCCFFFFFFCCFFFFFFC9FEFEFF148BD2FF0000000000000000000000007451
      4C00AE682900E08F3300DF8D3000D9862400D2791500C96C0A00B5610D009950
      1300552C2A00C4C2C20000000000000000000000000000000000000000000000
      00000000000000000000888888FF909090FF939393FF9B9B9BFF595959FF5E5E
      5EFF2B2B2BFF8E8E8EFF0000000000000000000000000000000000000000C172
      30FFBD8556FFB7B1B4FFBAC0C7FFB26124FFAE7E62FFAE744DFF9C2E00FF9C4D
      27FF9D573EFF000000000000000000000000148BD2FFBEFBFDFF148BD2FFCCFF
      FFFF3CDBFFFF3ADBFFFF35DAFFFF2FD8FFFF2AD7FFFF25D6FFFF21D4FFFF1CD3
      FFFF17D2FFFF09CDFFFFCCFFFFFF148BD2FF0000000000000000000000006E47
      4300C07B3C00ECA15100E99F4C00E2933900D37F21006B39270083461B00A758
      0F00562C2A00C1BFBF0000000000000000000000000000000000A0A0A0FF6C6C
      6CFF4D4D4DFF303030FF4D4D4DFF8F8F8FFFE1E1E1FFEEEEEEFFFFFFFFFFC5C5
      C5FF545454FF181818FF3B3B3BFF000000000000000000000000CF853FFFC5AA
      8EFFC69462FFC27F42FFC16C20FFBB5100FFB88764FFB69887FFBED0DBFFA347
      15FFB9CED6FF9F4E2DFF0000000000000000148BD2FFCCFFFFFF148BD2FFCCFF
      FFFF51E2FFFF51E2FFFF4CDFFFFF47DFFFFF42DEFFFF3DDBFFFF37DBFFFF34DA
      FFFF2FD7FFFF22D4FFFFCCFFFFFF148BD2FF000000000000000000000000704B
      4700C17F4900F6B37200F2AC6400E2984600703D2D006F453F00703C2600AB5A
      0E00542A2900000000000000000000000000000000009F9F9FFF898989FF6C6C
      6CFF414141FF3D3D3DFF373737FF1D1D1DFF5D5D5DFF696969FF6B6B6BFF7171
      71FFAEAEAEFF626262FF909090FF0000000000000000EBC298FFCF791FFFCF6F
      0CFFCE9A63FFC9A88BFFC6CBD0FFC6A07DFFC3A589FFBDA595FFC1C7CAFFB390
      7BFFAE8A76FFAEA19CFFD0A398FF00000000148BD2FFCCFFFFFF148BD2FFCCFF
      FFFF67E7FFFF66E6FFFF61E6FFFF5CE5FFFF57E2FFFF52E2FFFF4DE1FFFF47DE
      FFFF44DEFFFF38DAFFFFCCFFFFFF148BD2FF000000000000000000000000846A
      6900995F4000F6B26F00EEA96200774535008D6A6800E4D4CA005E302900964E
      1300633C390000000000000000000000000000000000000000008F8F8FFF6565
      65FF525252FF141414FF5E5E5EFF9D9D9DFF000000000000000000000000A9A9
      A9FF9F9F9FFF00000000000000000000000000000000E19D58FFCFE3F9FFD39C
      62FFD2D1D2FFD0C9C1FFCDC0B4FFCDD0D2FFCA7F37FFC5CDD4FFC2B5ABFFBCA7
      98FFA43B00FFA2552EFFAF654BFF00000000148BD2FFCCFFFFFF148BD2FFCCFF
      FFFF7DEDFFFF79EDFFFF76EAFFFF71E9FFFF6CE9FFFF67E6FFFF62E5FFFF5DE5
      FFFF57E4FFFF4EE0FFFFCCFFFFFF148BD2FF000000000000000000000000B7B1
      B1005B322F00A97748002B4256000B3562000A346200305D8400392B3700713B
      23008C7675000000000000000000000000000000000000000000000000009696
      96FF585858FF222222FF00000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000DEA060FFD5E4F5FFD9A9
      78FFD8CFC5FFD7DFE5FFD5C5B4FFD2CBC2FFCE6E0AFFC9792DFFC0620FFFBE92
      72FFAD5319FFC3DEEEFFA65E3BFF00000000148BD2FFCCFFFFFF148BD2FFCCFF
      FFFF93F3FFFF8EF1FFFF8BF1FFFF86F0FFFF81EDFFFF7CEDFFFF77ECFFFF72E9
      FFFF6DE8FFFF63E6FFFFCCFFFFFF148BD2FF0000000000000000000000000000
      00009C9799000E37620024699E0042A3E0003F9FDB002670A6000F3963002631
      4900BFBDBD0000000000000000000000000000000000B7B7B7FF555657FF3D41
      46FF373B40FF2F3338FF3E4043FF45484BFF515455FF757575FF000000000000
      00000000000000000000000000000000000000000000E3A86DFFDBEDFFFFDFBF
      9FFFE1AB73FFDEA162FFDC8B37FFD8AA7AFFD38A40FFD2D2D3FFCEB9A4FFCBDD
      EAFFB46A34FFC1CDD3FFAA5F39FF00000000148BD2FFCCFFFFFF148BD2FFCCFF
      FFFFA8F8FFFFA4F8FFFF9EF7FFFF9BF4FFFF96F4FFFF91F3FFFF8CF0FFFF87F0
      FFFF82EFFFFF79EDFFFFC3FEFFFF148BD2FF000000000000000000000000BBBE
      C00017416E001F6399005AC0FC005AC1FC0056BFFC004EB8F600378FCA00163F
      68008594A400000000000000000000000000000000009A9B9BFF7B7774FFC48A
      4FFFBA8C5AFFB38B61FFAC8964FFA48565FFA2886DFF404244FF000000000000
      00000000000000000000000000000000000000000000EEB782FFE7AF75FFE6BA
      8EFFE8AC6DFFE4EFF9FFE4D9CDFFDFE8F0FFDABDA1FFD5B087FFD1BBA6FFCFDD
      E9FFB96F3AFFC5D7E2FFB86C48FF00000000148BD2FFCCFFFFFF148BD2FFCCFF
      FFFFC2FFFFFFBFFFFFFFBBFFFFFFB5FEFFFFB1FDFFFFACFBFFFFA6FAFFFFA1F9
      FFFF9CF6FFFF93F4FFFFB7FFFFFF148BD2FF0000000000000000000000007286
      9B000A3462002B73A90068C5FC0069C6FC0062C4FC005AC0FC004DB6F4002D74
      A8003153770000000000000000000000000000000000959696FF85786AFFEA97
      3DFFE39845FFE49743FFE2933DFFE08B2FFFE3923EFF50565BFF000000000000
      00000000000000000000000000000000000000000000F7DCBDFFE9DCD2FFE9F6
      FFFFECB67BFFEBF0F4FFEAE7E5FFE7DED4FFE2E8EDFFD69857FFD8C7B6FFCCB3
      9BFFB85708FFAF5A20FFD7A992FF00000000148BD2FFCCFFFFFF00B6F1FF148B
      D2FF148BD2FF148BD2FF148BD2FF148BD2FF148BD2FF148BD2FF148BD2FF148B
      D2FF148BD2FF148BD2FF148BD2FFFCFDFEFF0000000000000000000000004161
      83000A3C6C00114574001B5687001045740010457400185487002670A6002D7F
      B8000F39650000000000000000000000000000000000929495FF8D7B66FFECA6
      5AFFE6A75FFFE5A65FFFE3A157FFE29848FFDD9E59FF5B5F63FF000000000000
      0000000000000000000000000000000000000000000000000000FBCB9AFFEFF8
      FFFFF1E7DBFFEFCAA3FFEFFBFFFFE9C9A8FFDFB386FFD56F0AFFD17F2CFFCDA7
      83FFBF7436FFBF7747FF0000000000000000148BD2FFCCFFFFFF00CBFFFF00CB
      FFFF00CBFFFF00CBFFFF00CCFFFF00CBFFFF4FE3FFFF4FE3FFFF4FE3FFFF4FE3
      FFFF4FE3FFFF4FE3FFFF148BD2FF000000000000000000000000000000003C5E
      7F000A4F83000B4677000A5286000A5D92000A588C000A4A7C000A4273000B39
      6800133C6800000000000000000000000000000000008F9193FF988066FFF3B8
      70FFEDB978FFECB878FFE9B16CFFE8A55BFFD8A369FF686B6EFF000000000000
      000000000000000000000000000000000000000000000000000000000000FFDF
      BCFFF8E2CEFFEFA75FFFEAAA69FFE7BF96FFE7D1BDFFD9934BFFDBD9D4FFCFB8
      A4FFC05907FF000000000000000000000000148BD2FFCCFFFFFF00C8FFFF00CA
      FFFF00CAFFFF00CAFFFF00C5FCFF4FE3FFFF148BD2FF148BD2FF148BD2FF148B
      D2FF148BD2FF148BD2FFFDFEFEFF000000000000000000000000000000006178
      92000A4A7C000A72A9000A77AE000A73AB000A699F000A5E93000A5286000A4A
      7C0018406B00000000000000000000000000000000008C8E90FFA58968FFF9C6
      86FFF7CB92FFF6C98FFFF0BF7FFFEFB169FFCDA16FFF787A7DFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FBC48CFFF9F5F0FFEAB37DFFEDEFF1FFE9EFF4FFD9934DFFDAC8B5FFCE72
      1DFFF4CBADFF000000000000000000000000148BD2FFC0FDFFFF4FE3FFFF4FE3
      FFFF4FE3FFFF4FE3FFFF4FE3FFFF148BD2FF0000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000A9AF
      B700103A67000A6FA6000A85BE000A78B0000A6CA2000A6095000A5487000D3D
      6C006C809700000000000000000000000000000000008C8F91FFA78963FFF3C8
      8FFFF2D2A3FFEFCD9DFFE8BF88FFE6B274FFB89771FF8B8C8EFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000F4CEA8FFE9AE71FFDE8A34FFDDA265FFDB8E40FFE3AB76FF0000
      000000000000000000000000000000000000FBFDFEFF148BD2FF148BD2FF148B
      D2FF148BD2FF148BD2FF148BD2FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00006F849900113E6B000A5D92000A6DA3000A699F000A5286000F3F6D003556
      7A00C3C5C60000000000000000000000000000000000939494FF626467FF5B5E
      62FF585C60FF56595EFF575B5EFF565A5EFF5D5F62FFB8B8B8FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009BA5B0003C5D7F00123B68000A3462002349710074879B000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF00FC07000000000000FC07000000000000
      F407000000000000800700000000000080070000000000008001000000000000
      8001000000000000800100000000000080010000000000008001000000000000
      800100000000000080010000000000008001000000000000C101000000000000
      FF01000000000000FF83000000000000FFFFFFFFE00FFFFFF81FC000E007FE0F
      F00F8000E003FC03E0070000E003C001C0030000E007800180010000E007C0E7
      80010000E007E3FF80010000F007803F80010000E007803F80010000E007803F
      80010000E007803FC0030001E007803FE0070001E007803FF00700FFE007803F
      F81F01FFF007803FFFFFFFFFF81FFFFF00000000000000000000000000000000
      000000000000}
  end
  object ilWIFI: TImageList
    ColorDepth = cd32Bit
    Left = 415
    Top = 375
    Bitmap = {
      494C010104000900040010001000FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
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
      00001C1C1B1E606A5280798F58C27F9D53E37F9E53E2788E59C05E68517D1919
      181B0000000000000000000000000000000000000000000000005CB1E5F959B2
      E9FF59B2E9FF59B2E9FF58B1E9FF56B0E7FF56B0E7FF59B2E9FF59B2E9FF59B2
      E9FF59B2E9FF59B2E9FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000001A1A191C6571548D789055C67890
      55C66571548D1A1A191C00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000002020203606A
      528181A64BF981A849FF81A849FF81A849FF81A849FF81A849FF81A849FF81A5
      4DF75C65507900000001000000000000000000000000000000005BB2EAFE59B2
      E9FF59B2E9FF58B1E9FF58B1E7FF73BEECFF72BDECFF56B0E7FF59B2E9FF59B2
      E9FF59B2E9FF59B2E9FF00000000000000005CB1E5F959B2E9FF59B2E9FF59B2
      E9FF58B1E9FF5EA0C8DA01010102464B40557D9C50E27DA645FF7DA645FF7DA6
      45FF7DA645FF7D9C50E2464B4055000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000202020372845AAC81A8
      49FF81A849FF81A849FF81A849FF81A849FF81A849FF81A849FF81A849FF81A8
      49FF81A849FF6F7F59A3000000010000000000000000000000005BB2EAFE59B2
      E9FF59B2E9FF58B1E7FF58B1E7FFFFFFFFFFFFFFFFFF4EADE6FF58B1E9FF59B2
      E9FF59B2E9FF59B2E9FF00000000000000005BB2EAFE59B2E9FF59B2E9FF58B1
      E9FF58B1E7FF353A3C3E1A1A191C7D9C50E27DA645FF749F38FFFFFFFFFF749F
      38FF7BA242FF719E36FF7D9C50E21A1A191C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000616B538381A849FF81A8
      49FF81A849FF81A849FFA6C180FFAEC68BFF81A849FF81A849FF81A849FF81A8
      49FF81A849FF81A849FF5C6550790000000000000000000000005BB2EAFE59B2
      E9FF59B2E9FF59B2E9FF4CABE6FFF8FCFDFFEAF4FCFF4DADE6FF59B2E9FF59B2
      E9FF59B2E9FF59B2E9FF00000000000000005BB2EAFE59B2E9FF59B2E9FF58B1
      E7FF609FC5D70B0B0B0B6571548D7DA645FF7DA645FFFFFFFFFFFFFFFFFFFFFF
      FFFF749F38FF7BA644FF7DA645FF6571548D0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001E1F1D2181A64BF981A849FF81A8
      49FF81A849FFA6C181FFFCFCFAFFFDFDFCFFACC588FF81A849FF81A849FF81A8
      49FF81A849FF81A849FF80A54DF61718171900000000000000005BB2EAFE59B2
      E9FF59B2E9FF59B2E9FF4DADE6FFD2E9F8FFC4E3F6FF4EADE6FF59B2E9FF59B2
      E9FF59B2E9FF59B2E9FF00000000000000005BB2EAFE59B2E9FF59B2E9FF59B2
      E9FF589BC5D702020203789055C67DA645FFFFFFFFFFFFFFFFFFBDD2A0FFFFFF
      FFFFFFFFFFFF749F38FF7DA645FF789055C60000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000626D538581A849FF81A849FF81A8
      49FFA8C281FFFCFDFAFFFFFFFFFFFFFFFFFFFDFDFCFFACC589FF81A849FF81A8
      49FF81A849FF81A849FF81A849FF5D66517B00000000000000005BB2EAFE59B2
      E9FF59B2E9FF57B1E7FF67B8E9FFFFFFFFFFFFFFFEFF5BB2E7FF58B1E7FF59B2
      E9FF59B2E9FF59B2E9FF00000000000000005BB2EAFE59B2E9FF59B2E9FF59B2
      E9FF599DC5D702020203789055C67DA645FFFFFFFFFFBDD2A0FF749F38FFBDD2
      A0FFFFFFFFFFFFFFFFFF7DA645FF789055C60000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B9257C981A849FF81A849FFA8C2
      82FFFCFDFBFFFFFFFFFFEDF2E5FFEFF3E8FFFFFFFFFFFDFDFCFFADC589FF81A8
      49FF81A849FF81A849FF81A849FF788E58BF00000000000000005BB2EAFE59B2
      E9FF59B2E9FF56B0E7FF71BDEBFFFFFFFFFFFFFFFFFF64B6E9FF57B1E7FF59B2
      E9FF59B2E9FF59B2E9FF00000000000000005BB2EAFE59B2E9FF59B2E9FF57B1
      E7FF6CA4C7D70C0C0C0C6571548D7DA645FF749F38FF749F38FF749F38FF749F
      38FFBDD2A0FFFFFFFFFF7DA645FF6571548D0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007FA051EA81A849FF8BAE57FFF9FB
      F6FFFFFFFFFFEDF2E5FF8FB15CFF90B25EFFEEF3E8FFFFFFFFFFFDFDFCFFADC5
      8AFF81A849FF81A849FF81A849FF7E9C54E000000000000000005AB3EBFF59B2
      E9FF59B2E9FF59B2E9FF53AFE7FF81C3EDFF77BFECFF54B0E7FF59B2E9FF59B2
      E9FF59B2E9FF59B2E9FF00000000000000005BB2EAFE59B2E9FF59B2E9FF56B0
      E7FF71BDEBFF404040401A1A191C7D9C50E27DA645FF7DA645FF7DA645FF7DA6
      45FF7DA645FFC1D4A6FF7D9C50E21A1A191C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000080A150EB81A849FF81A849FFA6C1
      81FFEAF0E1FF8FB15DFF81A849FF81A849FF90B15EFFEEF3E8FFFFFFFFFFFDFD
      FCFFAEC68BFF81A849FF81A849FF7F9D53E100000000000000005AB3EBFF59B2
      E9FF59B2E9FF59B2E9FF59B2E9FF55B0E7FF56B0E7FF59B2E9FF59B2E9FF59B2
      E9FF5AB1E6FC59B2E9FF00000000000000005AB3EBFF59B2E9FF59B2E9FF59B2
      E9FF53AFE7FF7EAFCEDC01010102464B40557D9C50E27DA645FF7DA645FF7DA6
      45FF7DA645FF7D9C50E2464B4055000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B9457CC81A849FF81A849FF81A8
      49FF82A94BFF81A849FF81A849FF81A849FF81A849FF90B15EFFEEF3E6FFFFFF
      FFFFFDFEFDFFADC589FF81A849FF798F59C300000000000000000000000059B2
      E9FF6293B2BF0000000000000000000000000000000000000000000000006295
      B5C259B2E9FF0000000000000000000000005AB3EBFF59B2E9FF59B2E9FF59B2
      E9FF59B2E9FF55B0E7FF5E8298A3010101021A1A191C6571548D789055C67890
      55C66571548D1A1A191C00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000006570558B81A849FF81A849FF81A8
      49FF81A849FF81A849FF81A849FF81A849FF81A849FF81A849FF8FB15EFFEEF3
      E6FFF6F9F3FF9AB96CFF81A849FF616B538200000000000000000000000059B2
      E9FF6398B9C40000000000000000000000000000000000000000000000006399
      BBC659B2E9FF0000000000000000000000000000000059B2E9FF6398B9C40000
      0000000000000000000000000000000000000000000004040405000000010000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002324222780A74AFC81A849FF81A8
      49FF81A849FF81A849FF81A849FF81A849FF81A849FF81A849FF81A849FF8FB1
      5DFF9BB96DFF81A849FF81A64BF91D1D1C1F00000000000000000000000059B2
      E9FF6299BCCB000000000000000000000000000000000000000000000000619C
      BFCD59B2E9FF0000000000000000000000000000000059B2E9FF6299BCCB0000
      000000000000000000000000000000000000000000006189A1AC61A0C7D70000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000006772558E81A849FF81A8
      49FF81A849FF81A849FF81A849FF81A849FF81A849FF81A849FF81A849FF81A8
      49FF81A849FF81A849FF626C5384000000000000000000000000000000005DAB
      DBEF59B2E9FF00000000000000000000000000000000000000000000000059B2
      E9FF5DACDDF1000000000000000000000000000000005DABDBEF59B2E9FF0000
      0000000000000000000000000000000000000000000059B2E9FF5DACDDF10000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000005050506778B59BA81A8
      49FF81A849FF81A849FF81A849FF81A849FF81A849FF81A849FF81A849FF81A8
      49FF81A849FF748659B103030304000000000000000000000000000000004C5B
      646859B2E9FF60A5D0E20606060700000000000000000303030460A4CEE059B2
      E9FF4E5D676B000000000000000000000000000000004C5B646859B2E9FF60A5
      D0E20606060700000000000000000303030460A4CEE059B2E9FF4E5D676B0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000050505066976
      579381A84BFD81A849FF81A849FF81A849FF81A849FF81A849FF81A849FF80A7
      4AFC6671558C0303030400000000000000000000000000000000000000000000
      0000618BA5B059B2E9FF59B2E9FF6299BCCB6299BBC959B2E9FF59B2E9FF628E
      AAB6000000000000000000000000000000000000000000000000618BA5B059B2
      E9FF59B2E9FF6299BCCB6299BBC959B2E9FF59B2E9FF628EAAB6000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00002829262C687556927D9856D580A44EF481A44EF37D9756D36772558E2425
      2328000000000000000000000000000000000000000000000000000000000000
      00000000000041494F52629CBFCE59B2E9FF59B2E9FF619DC2D2444E55580000
      0000000000000000000000000000000000000000000000000000000000004149
      4F52629CBFCE59B2E9FF59B2E9FF619DC2D2444E555800000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFFF00FC003FF03FFFFC003C0030001
      FFFF8001C0030000FFFF8001C0030000FFFF0000C0030000FFFF0000C0030000
      FFFF0000C0030000FFFF0000C0030000FFFF0000C0030001FFFF0000E7E70003
      FFFF0000E7E79F9FFFFF0000E7E79F9FFFFF8001E7E79F9FFFFF8001E187861F
      FFFFC003F00FC03FFFFFF00FF81FE07F00000000000000000000000000000000
      000000000000}
  end
  object ilSystem: TImageList
    ColorDepth = cd32Bit
    Left = 185
    Top = 425
  end
  object ilPrinter: TImageList
    ColorDepth = cd32Bit
    Left = 245
    Top = 292
    Bitmap = {
      494C010104000900040010001000FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
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
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000565656B8777777FE777777FF777777FF777777FF777777FF777777FF5858
      58BC000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000669822FA000000010000
      00000000000000000000000000000000000085AD49FF85AD49FF85AD49FF85AD
      49FF555C4B6C767676FF767676FF767676FF767676FF767676FF767676FF555C
      4B6C85AD49FF85AD49FF85AD49FF85AD49FF0000000000000000000000000000
      00000000000000000000000000000000000000000000669822FA000000010000
      0000000000000000000000000000000000000000000000000000000000000000
      0000777777FEFEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7878
      78FF000000020000000000000000000000000000000000000000000000000000
      0000717171B8767676FB4949495B0000000065991DFF65991DFF65991DFF0000
      00000000000000000000000000000000000085AD49FF85AD49FF85AD49FF85AD
      49FF555C4B6C767676FF767676FF767676FF767676FF767676FF767676FF555C
      4B6C85AD49FF85AD49FF85AD49FF85AD49FF0000000000000000000000000000
      00000000000000000000000000000000000065991DFF65991DFF65991DFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000777777FFFFFFFFFFD9BB9EFFD9BB9EFFD9BB9EFFD9BB9EFFFFFFFFFF7878
      78FF000000020000000000000000000000000000000000000000000000000000
      0000767676FB5D5D5D5E0000000065991DFF65991DFF65991DFF65991DFF6599
      1DFF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000767676FF767676FF00000000000000000000
      000000000000000000000000000000000000A4A4A4FFA4A4A4FFA4A4A4FFA4A4
      A4FF545454624B4B4B5E0000000065991DFF65991DFF65991DFF65991DFF6599
      1DFF000000003535353A00000000000000000000000000000000000000000000
      0000777777FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7878
      78FF000000020000000000000000000000000000000000000000000000000000
      0000646464910000000065991DFF65991DFF65991DFF0000000069932EE86599
      1DFF65991DFF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000555C4B6C555C4B6C00000000000000000000
      000000000000000000000000000000000000A4A4A4FFA4A4A4FFA4A4A4FFA4A4
      A4FF333333380000000065991DFF65991DFF65991DFF0000000069932EE86599
      1DFF65991DFF000000000000000000000000676767DD777777FF777777FF7777
      77FF777777FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7878
      78FF777777FF777777FF777777FF606060CE777777DD767676FF767676FF7676
      76FF6A6A6A9E0000000065991DFF65991DFF000000001E1E1E1E000000006796
      26F365991DFF65991DFF00000000000000000000000000000000000000000000
      0000717171B8777777FE767676FF767676FF767676FF767676FF767676FF7272
      72BC000000000000000000000000000000000000000000000000000000000000
      0000000000000000000065991DFF65991DFF0000000000000000000000006796
      26F365991DFF65991DFF0000000000000000777777FF00000000000000017777
      77FF777777FF777777FF777777FF777777FF777777FF777777FF777777FF7777
      77FF777777FF777777FF777777FF777777FF767676FF00000000000000017676
      76FF777777FE50505067000000000B0B0B0C3D3D3D48767676F7505050660000
      0000679723F965991DFF65991DFF000000000000000000000000000000000000
      0000767676FFFEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7777
      77FF050505060000000000000000000000000000000000000000000000000000
      00000000000000000000000000000B0B0B0C0000000000000000000000000000
      0000679723F965991DFF65991DFF00000000777777FF777777FF777777FF7777
      77FF777777FF777777FF777777FF777777FF777777FF777777FF777777FF7777
      77FF777777FF777777FF777777FF777777FF767676FF767676FF767676FF7676
      76FF767676FF777777FE6464648F5D5D5D7E767676F7767676FF777777FE4D4D
      4D620000000065991FFD65991DFF669822FA777777DD767676FF767676FF7676
      76FF767676FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7777
      77FF767676FF767676FF767676FF757575CE0000000000000000000000007171
      71B8767676FF777777FE6464648F5D5D5D7E767676F7767676FF717171BB0000
      00000000000065991FFD65991DFF669822FA777777FF777777FF777777FF7777
      77FF777777FF777777FF777777FF777777FF777777FF777777FF777777FF7777
      77FF777777FF777777FF777777FF777777FF767676FF767676FF767676FF7676
      76FF767676FF767676FF767676FF767676FF767676FF767676FF767676FF7676
      76FD4C4C4C600000000065991DFF08080809767676FF00000000000000017676
      76FF767676FF767676FF767676FF767676FF767676FF767676FF767676FF7676
      76FF767676FF767676FF767676FF767676FF0000000000000000000000007676
      76FFFEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF777777FF0000
      0000000000000000000065991DFF08080809777777FF777777FFD7D7D7FFFEFE
      FEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFEFEFEFFD9D9D9FF777777FF777777FF767676FF767676FFD7D7D7FFFEFE
      FEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFE
      FEFFD6D6D6FC4B4B4B5E0000000000000000767676FF767676FF767676FF7676
      76FF767676FF767676FF767676FF767676FF767676FF767676FF767676FF7676
      76FF767676FF767676FF767676FF767676FF777777DD767676FF767676FF7676
      76FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF777777FF7676
      76FF777777FC3F3F3F4B0000000000000000777777FF777777FFFCFCFCFFBC89
      55FFB3773DFFB3773DFFB3773DFFB3773DFFB3773DFFB3773DFFB3773DFFB377
      3DFFBD8956FFFEFEFEFF777777FF777777FF767676FF767676FFFCFCFCFFBC89
      54FFB3763CFFB3763CFFB3763CFFB3763CFFB3763CFFB3763CFFB3763CFFBD89
      55FFFEFEFEFF777777FE757575CC00000000767676FF767676FFD7D7D7FFFEFE
      FEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFEFEFEFFD9D9D9FF767676FF767676FF767676FF00000000000000017676
      76FF767676FF767676FF767676FF767676FF767676FF767676FF767676FF7676
      76FF767676FF777777FE0000000000000000777777FF777777FFFBFBFAFFB377
      3DFFB3773DFFB3773DFFB3773DFFB3773DFFB3773DFFB3773DFFB3773DFFB377
      3DFFB3773DFFFDFDFCFF777777FF777777FF767676FF767676FFFBFBFAFFB376
      3CFFB3763CFFB3763CFFB3763CFFB3763CFFB3763CFFB3763CFFB3763CFFB376
      3CFFFDFDFCFF767676FF767676FF00000000767676FF767676FFFCFCFCFFBC89
      54FFB3763CFFB3763CFFB3763CFFB3763CFFB3763CFFB3763CFFB3763CFFB376
      3CFFBD8955FFFEFEFEFF767676FF767676FF767676FF767676FF767676FF7676
      76FF767676FF767676FF767676FF767676FF767676FF767676FF767676FF7676
      76FF767676FF767676FF0000000000000000696969E1777777FFFBFBFAFFB377
      3DFFB3773DFFB3773DFFB3773DFFB3773DFFB3773DFFB3773DFFB3773DFFB377
      3DFFB3773DFFFDFDFCFF777777FF636363D4777777E1767676FFFBFBFAFFB376
      3CFFB3763CFFB3763CFFB3763CFFB3763CFFB3763CFFB3763CFFB3763CFFB376
      3CFFFDFDFCFF767676FF767676D400000000767676FF767676FFFCFCFCFFBC89
      54FFB3763CFFB3763CFFB3763CFFB3763CFFB3763CFFB3763CFFB3763CFFB376
      3CFFBD8955FFFEFEFEFF767676FF767676FF767676FF767676FFD7D7D7FFFEFE
      FEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFEFFD9D9
      D9FF767676FF767676FF0000000000000000000000000000000002010103B377
      3DFFB3773DFFB3773DFFB3773DFFB3773DFFB3773DFFB3773DFFB3773DFFB377
      3DFFB3773DFF020101030000000000000000000000000000000002020203B376
      3CFFB3763CFFB3763CFFB3763CFFB3763CFFB3763CFFB3763CFFB3763CFFB376
      3CFF02020203000000000000000000000000777777E1767676FFFBFBFAFFB376
      3CFFB3763CFFB3763CFFB3763CFFB3763CFFB3763CFFB3763CFFB3763CFFB376
      3CFFB3763CFFFDFDFCFF767676FF767676D4767676FF767676FFFCFCFCFFBC89
      54FFB3763CFFB3763CFFB3763CFFB3763CFFB3763CFFB3763CFFBD8955FFFEFE
      FEFF767676FF767676FF00000000000000000000000000000000000000000101
      0103797979FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFEFF7676
      76FD000000000000000000000000000000000000000000000000000000000202
      0203787878FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFEFF767676FD0000
      0000000000000000000000000000000000000000000000000000020202030000
      0000787878FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFEFF7676
      76FD00000000020202030000000000000000777777E1767676FFFBFBFAFFB376
      3CFFB3763CFFB3763CFFB3763CFFB3763CFFB3763CFFB3763CFFB3763CFFFDFD
      FCFF767676FF767676D400000000000000000000000000000000000000000101
      0103797979FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFEFF7676
      76FD000000000000000000000000000000000000000000000000000000000202
      0203787878FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFEFF767676FD0000
      0000000000000000000000000000000000000000000000000000000000000202
      0203787878FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFEFF7676
      76FD000000000000000000000000000000000000000000000000020202030202
      0203787878FFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFEFF767676FF000000000202
      0203000000000000000000000000000000000000000000000000000000000000
      0000585858BD777777FF777777FF777777FF777777FF777777FF777777FE5656
      56B8000000000000000000000000000000000000000000000000000000000000
      0000727272BD767676FF767676FF767676FF767676FF777777FE717171B80000
      0000000000000000000000000000000000000000000000000000000000000000
      0000727272BD767676FF767676FF767676FF767676FF767676FF777777FE7171
      71B8000000000000000000000000000000000000000000000000000000000000
      0000727272BD767676FF767676FF767676FF767676FF717171B8000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000F00FFF9F0000FF9FF007F11F0000FF1F
      F007F20FFE7F020BF007F447FE7F0447000004A3F00FFCE340004211F007FEF1
      000000080000E018000000044000E01C00000003000000030000000100004003
      00000001000000030000000100000003C003C00700000003E00FE01FD00B0003
      E00FE01FE00FC02FF00FF01FF00FF03F00000000000000000000000000000000
      000000000000}
  end
end
