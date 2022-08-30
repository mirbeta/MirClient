object frmGalleryDesigner: TfrmGalleryDesigner
  Left = 186
  Top = 118
  BorderIcons = [biSystemMenu]
  Caption = 'frmRibbonGalleryDesigner'
  ClientHeight = 351
  ClientWidth = 243
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object tvGalleryItems: TTreeView
    Left = 0
    Top = 30
    Width = 243
    Height = 321
    Align = alClient
    DragMode = dmAutomatic
    HideSelection = False
    Indent = 19
    MultiSelect = True
    MultiSelectStyle = [msControlSelect, msShiftSelect]
    PopupMenu = pmGalleryItems
    StateImages = ilGroupState
    TabOrder = 0
    OnChange = tvGalleryItemsChange
    OnDragDrop = tvGalleryItemsDragDrop
    OnDragOver = tvGalleryItemsDragOver
    OnEdited = tvGalleryItemsEdited
    OnEditing = tvGalleryItemsEditing
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 243
    Height = 30
    BorderWidth = 1
    ButtonWidth = 24
    Caption = 'ToolBar1'
    DisabledImages = ilHelper
    EdgeBorders = [ebTop, ebBottom]
    Images = ilButtons
    Indent = 4
    TabOrder = 1
    object tbAddGroup: TToolButton
      Left = 4
      Top = 0
      Action = actAddGroup
      ParentShowHint = False
      ShowHint = True
    end
    object tbAddGroupItem: TToolButton
      Left = 28
      Top = 0
      Action = actAddGroupItem
      ParentShowHint = False
      ShowHint = True
    end
    object tbDelete: TToolButton
      Left = 52
      Top = 0
      Action = actRemove
      ParentShowHint = False
      ShowHint = True
    end
    object tbSeparator: TToolButton
      Left = 76
      Top = 0
      Width = 8
      Caption = 'tbSeparator'
      ImageIndex = 2
      Style = tbsSeparator
    end
    object tbMoveNodeUp: TToolButton
      Left = 84
      Top = 0
      Action = actMoveNodeUp
      ParentShowHint = False
      ShowHint = True
    end
    object tbMoveNodeDown: TToolButton
      Left = 108
      Top = 0
      Action = actMoveNodeDown
      ParentShowHint = False
      ShowHint = True
    end
  end
  object ilButtons: TcxImageList
    FormatVersion = 1
    DesignInfo = 6291544
    ImageInfo = <
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00004D0F00004D0F00004D0F00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00D0BAA400AC845D008E5C28008D5C2800A6825D00CBB8A400FF00
          FF00FF00FF0000550F0040D0770000550F00FF00FF00FF00FF00FF00FF00FF00
          FF00C2A48500A3662C00C8895200E9A97500EAAC7600C88B560082522500005E
          1000005D1000005D0F0041D17900005D1000005E1000015D1000FF00FF00D3BD
          A500A6662A00D58E5800EA9B5F00EE995700F09B5900F0A06400A97349000166
          100043D47E0043D47E0044D47D0043D47E0043D47E0000661000FF00FF00B58B
          5E00BF794000DD864A00E1864600E38B4C00E68D4C00E58A4B00BD744200016F
          1100016F10000170110047D78100016F1000016F1100016F1000FF00FF00A067
          2A00CA7A4300D3763A00D97C3E00DC7F4100DB834400DC814100CE753B00AB69
          3A004E2D0C000176110048DB860001771100FF00FF00FF00FF00FF00FF00A168
          2900C4693000D1753E00DC895200E4956100E5966100DF8B5400D57B4200C56E
          34006F421500017C1100017C1100017C1100FF00FF00FF00FF00FF00FF00B88C
          5E00B15D2000DF9D7600EAB99900EBB99900EBB99A00EABA9A00E09F7900AC5B
          22008D562100BC7F5200AA764D0083552B00CDBAA800FF00FF00FF00FF00D5BE
          A500A2591800DFB29600F2D9CB00F2D9CB00F2D9CB00F2D9CB00DFB396009854
          1700AB693200DE8A4F00E3915600BE7E4B00AA876300FF00FF00FF00FF00FF00
          FF00C8A78500B57D4800D8B19400EFDACD00EFDACD00D7B09400B17948009C5C
          2500CC7E4600DB854700DA804600D0824D0091613100FF00FF00FF00FF00FF00
          FF00FF00FF00D5BEA500B88D5F00945714009556160096581E00AA683500D58D
          5E00E3976400E08E5900D7804900C8743C0093623100FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00AC855C00A55B2600D2977300E3B49600EAB9
          9B00EBBB9D00EBBB9D00E1A37D00B0622B00AF896300FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00D7C1A800A6602100E0B59A00F2DACC00F2DA
          CC00F2DACC00F3DACC00E0B59A009D5A2000D1BDA800FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00CAAA8A00B9824F00DAB49800F0DC
          CF00F0DCCF00D9B39800B47F4F00C4A78900FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00D7C0A800BB916500A56D
          3100A36D3200B7906400D4BFA800FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00004D0F00004D0F00004D0F00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF0000550F0040D0770000550F00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00CFD9D600B7AC9600A2856000936939007A572F00005E1000005D
          1000005D0F0041D17900005D1000005E1000015D1000FF00FF00FF00FF00FF00
          FF00C7CAC000A5886500AD6F3600C87F4100DC8B4900AD6D39000166100043D4
          7E0043D47E0044D47D0043D47E0043D47E0000661000FF00FF00FF00FF00D0D9
          D600A6886400B7753800E08C4900E08C4900E08C4A00BA743D00016F1100016F
          10000170110047D78100016F1000016F1100016F1000FF00FF00FF00FF00B8AB
          9500AB6B3100DB854500DB864500DB854400DA864400D1804200B66F39008753
          2A000176110048DB860001771100FF00FF00FF00FF00FF00FF00FF00FF00A684
          5C00BF733600D67F4000D57F4000D67F4000D67F4000D57F3F00D67F4000B16A
          3400017C1100017C1100017C1100FF00FF00FF00FF00FF00FF00FF00FF009A68
          3400CA753800CF783A00D7834800DE8C5400E3935D00E3935D00DE8C5400CD7D
          4500AD6430009E5C2C0080562B00FF00FF00FF00FF00FF00FF00FF00FF009B67
          3300C46F3200D7875100E9A67800E9A67800E9A57800E9A67900E9A67800E9A5
          7700D7875100C46F33009B673300FF00FF00FF00FF00FF00FF00FF00FF00A983
          5A00B5662A00E6AC8600EFBD9B00EFBD9B00EFBC9A00EFBD9B00EEBC9B00EFBC
          9B00E6AC8500B5652A00AA835A00FF00FF00FF00FF00FF00FF00FF00FF00BCAA
          9200A75E2300EABFA300F5D6C100F6D6C100F5D6C100F6D6C100F5D6C100F5D6
          C100EABFA300A75E2300BCAB9200FF00FF00FF00FF00FF00FF00FF00FF00D1D9
          D500AE875E00B97B4B00FAECE300FBEDE400FBEDE400FBEDE300FBEDE400FAEC
          E300B97B4A00AE865E00D1D9D500FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00CBC9BE00AF865D00B06F3A00CFA38100EFDFD200EFDED200CFA38100AF6F
          3A00AF865C00CBC9BE00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00D2D9D400BFAA9000AF825600A3662C00A3652C00AF825600BFAA
          9000D1D9D400FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00CCCCDD00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00CBCBDB00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF007777AC0017167100ACACCA00FF00FF00FF00FF00FF00FF00FF00
          FF00ABAAC800111067007372A400FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF007877AE0015157300071EC90017167100ACACCA00FF00FF00FF00FF00ABAB
          C80012126A00061BC7000E0D65007372A400FF00FF00FF00FF00FF00FF00CDCD
          DF001D1B7A000B28CE000625DC00071EC90015177C00ACACCA00ACABC9001214
          7700061BC700041DD800061BC50013136800CBCBDB00FF00FF00FF00FF00FF00
          FF00AEAECD001D1C7A000A28CE000525DC00071ECA0014167A0013157900061B
          C700041DD800061BC60015136B00ABAAC700FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00AEAECD001B1E84000A28CE000625DC00061ECB00061CCA00041D
          D800061BC60014167700ABABC800FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00AEAECD00191D83000929CF000627DD000420DA00061C
          C90015177A00ACABC900FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00AEAECE00191D84000C2ED400072CE0000625DC00071E
          CC0014167A00ACACCA00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00AFAFCF001D2188000E3CDA000B39E7006379E3006175E0000625
          DC00071ECA0015177B00ACABCA00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00B0B0D000201F81001348DF000F49EF006580E7001A1E8400191D83006175
          DF000625DC00071ECA0017167200ACACCA00FF00FF00FF00FF00FF00FF00D9D9
          E700212186004E7BEB001356F4006789EA001E238900AEAECE00AEAECD001B1E
          84006275DF000625DC00071EC90017167100D7D7E400FF00FF00FF00FF00FF00
          FF009999C6001F1E850088A7F10021268E00AFAFCF00FF00FF00FF00FF00AEAE
          CD001B1D84006175DF00161573009594BD00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF009999C60022299200B0AFD000FF00FF00FF00FF00FF00FF00FF00
          FF00AEAECD001B1E84009595BE00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00D9D9E700FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00D8D8E500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00AE7D4A00904B04008F4A0300AC7B
          4600FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00924D0800C4864F00B6621D008F4A
          0200FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00934E0A00E3A77700C9712C00904C
          0400FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF0094500B00E8AE7D00CC773100914C
          0500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF0095510D00EAB38100CF7D3600924D
          0700FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF0096520F00EDB98600D2853B00934E
          0800FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00CAA889009A571700995615009854130097531100E69E5200D58A40009350
          0B00934E0800914D0600904C0400C4A27E00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00CAA88900B4773C00E4B48100ECB47900E7A15500D68E4500D58C
          4100D3863D00AB692700C5A38000FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00CAA88900B77B3F00EFBF8C00ECB47900E7A15500E69F
          5400B06F2E00C6A48200FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00CAA88900B77B3F00EFBF8C00ECB57900B172
          3100C7A58400FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00CAA88900B77A3F00B4773B00C8A6
          8600FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CAA88900C9A88800FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00C4A37F00C4A27E00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00C6A38200A9642300A3571200C4A2
          7F00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00C7A58400AD6A2A00D07A3300C9712C00A357
          1200C4A27F00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00C8A68600B06F2F00DF8E4500DD884000D17A3400CA73
          2D00A4591200C4A37F00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00C9A88800B4773B00EEBC8800ECB88500EAB38100DE894100D27C
          3400DBA17100AA692A00C4A37F00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00CAA88A009B5819009A5817009956150098561300EDB98600E19348009551
          0D0094500B00934E0900914D0700C4A37F00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF009A571600EEBD8A00D58A40009653
          1000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF009B571900EFBF8C00D68E45009854
          1300FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF009C591A00EFBF8C00D79146009A56
          1500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF009D5A1C00EFBF8C00DB9C58009A58
          1800FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF009D5A1C00D6A06A00CE9962009B58
          1900FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00B283590097561B0097561B00B283
          5800FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end>
  end
  object pmGalleryItems: TPopupMenu
    Images = ilButtons
    OwnerDraw = True
    OnPopup = pmGalleryItemsPopup
    Left = 169
    Top = 102
    object pmiAddGroup: TMenuItem
      Action = actAddGroup
    end
    object pmiAddGroupItem: TMenuItem
      Action = actAddGroupItem
    end
    object pmiDelete: TMenuItem
      Action = actRemove
    end
  end
  object pmDragDrop: TPopupMenu
    OnPopup = pmDragDropPopup
    Left = 177
    Top = 38
    object ppmiCopy: TMenuItem
      Caption = '&Copy'
      OnClick = ppmiCopyClick
    end
    object ppmiMove: TMenuItem
      Caption = '&Move'
      OnClick = ppmiMoveClick
    end
  end
  object ilGroupState: TcxImageList
    FormatVersion = 1
    DesignInfo = 2162781
    ImageInfo = <
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000808000008080000080800000808000008080000080800000808000008080
          0000808000008080000080800000808000008080000000000000000000008080
          0000808000008080000080800000808000008080000080800000808000008080
          0000808000008080000080800000808000008080000000000000000000008080
          0000808000008080000080800000808000008080000080800000808000008080
          0000808000008080000080800000808000008080000000000000000000008080
          0000808000008080000080800000808000008080000080800000808000008080
          0000808000008080000080800000808000008080000000000000000000008080
          0000808000008080000080800000808000008080000080800000808000008080
          0000808000008080000080800000808000008080000000000000000000008080
          0000808000008080000080800000808000008080000080800000808000008080
          0000808000008080000080800000808000008080000000000000000000008080
          0000808000008080000080800000808000008080000080800000808000008080
          0000808000008080000080800000808000008080000000000000000000008080
          0000808000008080000080800000808000008080000080800000808000008080
          0000808000008080000080800000808000008080000000000000000000008080
          0000808000008080000080800000808000008080000080800000808000008080
          0000808000008080000080800000808000008080000000000000000000008080
          0000808000008080000080800000808000008080000080800000808000008080
          0000808000008080000080800000808000008080000000000000000000008080
          0000808000008080000080800000808000008080000080800000808000008080
          0000808000008080000080800000808000008080000000000000000000008080
          0000808000008080000080800000808000008080000080800000808000008080
          0000808000008080000080800000808000008080000000000000000000008080
          0000808000008080000080800000808000008080000080800000808000008080
          0000808000008080000080800000808000008080000000000000000000008080
          0000808000008080000080800000808000008080000080800000808000008080
          0000808000008080000080800000808000008080000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        MaskColor = clBlack
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00BFBFBF00878787005C5C5C005B5B5B0084848400BCBCBC00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00A7A7A700676767008D8D8D00AFAFAF00B0B0B0008F8F8F0064646400A3A3
          A300FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00C1C1
          C1006868680096969600A4A4A400A2A2A200A4A4A400AAAAAA009A9A9A006262
          6200BCBCBC00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF008C8C
          8C007F7F7F009393930093939300979797009999990098989800999999007F7F
          7F0084848400FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF006565
          650086868600868686008B8B8B008E8E8E008F8F8F008E8E8E008B8B8B008A8A
          8A00494949007E7E7E00BFBFBF00FF00FF00FF00FF00FF00FF00FF00FF006666
          66007A7A7A008787870097979700A2A2A200A3A3A300999999008B8B8B007C7C
          7C004F4F4F00868686006A6A6A00A6A6A600FF00FF00FF00FF00FF00FF008E8E
          8E0068686800AAAAAA00C1C1C100C2C2C200C2C2C200C2C2C200ACACAC006767
          67005B5B5B00A2A2A2009D9D9D0068686800BFBFBF00FF00FF00FF00FF00C2C2
          C2005D5D5D00BABABA00DEDEDE00DEDEDE00DEDEDE00DEDEDE00BABABA005757
          57006E6E6E00969696009C9C9C008484840089898900FF00FF00FF00FF00FF00
          FF00AAAAAA007E7E7E00B6B6B600DEDEDE00DEDEDE00B5B5B5007C7C7C006060
          60008989890091919100909090008E8E8E0062626200FF00FF00FF00FF00FF00
          FF00FF00FF00C2C2C2008E8E8E0054545400555555005A5A5A006F6F6F009999
          9900A3A3A3009C9C9C00909090008282820062626200FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF008686860065656500A2A2A200BCBCBC00C2C2
          C200C4C4C400C4C4C400AFAFAF006D6D6D008B8B8B00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00C4C4C40063636300BDBDBD00DFDFDF00DFDF
          DF00DFDFDF00DFDFDF00BDBDBD005E5E5E00C2C2C200FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00AEAEAE0084848400B9B9B900DFDF
          DF00DFDFDF00B8B8B80081818100AAAAAA00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00C4C4C400929292006C6C
          6C006B6B6B0090909000C3C3C300FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF009FBEA500538C5E001B6629001B64290053895E009FBCA500FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF007DAA8500167326002F943F0044B0540044AF54002F923F00166D26007DA5
          8500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF009FC1
          A4001677260032A5440029AF3D0018A92E0018A92E0029AF3D0032A34400166D
          26009FBCA500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005394
          5D00259035001BA32D000F9D22000F9E22000F9D23000F9E23001BA22E00258B
          350053895E00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF001B74
          2800229530000D931D000D931C000D911C000D931C000D911C000D911C002293
          2F000D5214009B7B5B00CDBAA700FF00FF00FF00FF00FF00FF00FF00FF001B76
          280011861C001B942C0034AA4D0048BD680048BD680034AB4D001C952C001182
          1C0012581900B9845400A0693500BCA38900FF00FF00FF00FF00FF00FF005398
          5D000772110064B774008DD1A0008DD2A0008DD2A0008DD1A00065B77400076C
          11003C612400E2996300D99762009D663400CDBAA800FF00FF00FF00FF009FC5
          A400046C11008AC09100C4E3CA00C4E3CA00C4E3CA00C4E3CA0089BF91000461
          11007B6C3200DE8A4F00E3915600BE7E4B00AA876300FF00FF00FF00FF00FF00
          FF007DB18400398B430088BA8D00C7E0C900C7DFCA0088B98D00398544005762
          2500CB7E4600DB854700DA804600D0824D0091613100FF00FF00FF00FF00FF00
          FF00FF00FF009FC4A50053985D000E6514001063140035601E00766D3500D48D
          5E00E3976400E08E5900D7804900C8743C0093623100FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00AC855C00A55B2600D2977300E3B49600EAB9
          9B00EBBB9D00EBBB9D00E1A37D00B0622B00AF896300FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00D7C1A800A6602100E0B59A00F2DACC00F2DA
          CC00F2DACC00F3DACC00E0B59A009D5A2000D1BDA800FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00CAAA8A00B9824F00DAB49800F0DC
          CF00F0DCCF00D9B39800B47F4F00C4A78900FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00D7C0A800BB916500A56D
          3100A36D3200B7906400D4BFA800FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end>
  end
  object alMain: TActionList
    Images = ilButtons
    Left = 33
    Top = 33
    object actAddGroup: TAction
      Caption = 'Add &'
      Hint = 'Add Group'
      ImageIndex = 0
      ShortCut = 32839
      OnExecute = actAddGroupExecute
    end
    object actRemove: TAction
      Caption = '&Remove'
      Hint = 'Remove'
      ImageIndex = 2
      ShortCut = 46
      OnExecute = actRemoveExecute
    end
    object actAddGroupItem: TAction
      Caption = 'Add'
      Hint = 'Add Group Item'
      ImageIndex = 1
      ShortCut = 32841
      OnExecute = actAddGroupItemExecute
    end
    object actMoveNodeUp: TAction
      Caption = 'Move Node Up'
      Hint = 'Move Node Up'
      ImageIndex = 3
      ShortCut = 16422
      OnExecute = actMoveNodeUpExecute
    end
    object actMoveNodeDown: TAction
      Caption = 'Move Node Down'
      Hint = 'Move Node Down'
      ImageIndex = 4
      ShortCut = 16424
      OnExecute = actMoveNodeDownExecute
    end
    object actClose: TAction
      ShortCut = 27
      OnExecute = actCloseExecute
    end
    object actSelectAll: TAction
      Caption = 'actSelectAll'
      ShortCut = 16449
      OnExecute = actSelectAllExecute
    end
  end
  object ilHelper: TcxImageList
    FormatVersion = 1
    DesignInfo = 9961560
  end
end