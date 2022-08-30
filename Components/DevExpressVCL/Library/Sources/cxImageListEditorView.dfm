object cxImageListEditorForm: TcxImageListEditorForm
  Left = 327
  Top = 272
  Anchors = [akLeft, akTop, akRight, akBottom]
  BorderIcons = [biSystemMenu]
  Caption = 'cxImageListEditorForm'
  ClientHeight = 386
  ClientWidth = 582
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlClient: TPanel
    AlignWithMargins = True
    Left = 8
    Top = 8
    Width = 566
    Height = 330
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 0
    object gbImages: TGroupBox
      AlignWithMargins = True
      Left = 0
      Top = 0
      Width = 345
      Height = 330
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 4
      Margins.Bottom = 0
      Align = alClient
      Caption = 'Images'
      TabOrder = 0
      object lvImages: TListView
        AlignWithMargins = True
        Left = 10
        Top = 18
        Width = 325
        Height = 274
        Hint = 'Add image'
        Margins.Left = 8
        Margins.Right = 8
        Align = alClient
        Columns = <>
        Constraints.MinHeight = 150
        Constraints.MinWidth = 228
        DragMode = dmAutomatic
        HideSelection = False
        IconOptions.AutoArrange = True
        MultiSelect = True
        ReadOnly = True
        PopupMenu = pmCommands
        ShowColumnHeaders = False
        TabOrder = 0
        OnEndDrag = lvImagesEndDrag
        OnDragOver = lvImagesDragOver
        OnKeyDown = lvImagesKeyDown
        OnStartDrag = lvImagesStartDrag
      end
      object pnlBottomBar: TPanel
        AlignWithMargins = True
        Left = 10
        Top = 298
        Width = 325
        Height = 22
        Margins.Left = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alBottom
        BevelOuter = bvNone
        FullRepaint = False
        TabOrder = 1
        object pnlToolBarSubstrate: TPanel
          Left = 0
          Top = 0
          Width = 201
          Height = 22
          Align = alLeft
          BevelOuter = bvNone
          TabOrder = 0
          object tbCommands: TToolBar
            Left = 0
            Top = 0
            Width = 201
            Height = 22
            Align = alClient
            Caption = 'tbCommands'
            Images = imglSmall
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            object tbbAdd: TToolButton
              Left = 0
              Top = 0
              Action = actAdd
              DropdownMenu = pmAdd
            end
            object tbbReplace: TToolButton
              Left = 23
              Top = 0
              Action = actReplace
              DropdownMenu = pmReplace
            end
            object tbbDelete: TToolButton
              Left = 46
              Top = 0
              Action = actDelete
            end
            object tbbClear: TToolButton
              Left = 69
              Top = 0
              Action = actClear
            end
            object tbbExport: TToolButton
              Left = 92
              Top = 0
              Action = actExport
              DropdownMenu = pmExport
            end
            object tbbImport: TToolButton
              Left = 115
              Top = 0
              Action = actImport
              DropdownMenu = pmImageLists
            end
            object tbbConvertTo32bit: TToolButton
              Left = 138
              Top = 0
              Action = actConvertTo32bit
            end
            object tbbSeparator: TToolButton
              Left = 161
              Top = 0
              Width = 8
              Caption = 'tbbSeparator'
              ImageIndex = 7
              Style = tbsSeparator
            end
            object tbbShowImageTypes: TToolButton
              Left = 169
              Top = 0
              Hint = 'Show image types'
              ImageIndex = 7
              Style = tbsCheck
              OnClick = tbbShowImageTypesClick
            end
          end
        end
        object cbImagesSize: TComboBox
          Left = 255
          Top = 0
          Width = 70
          Height = 21
          Align = alRight
          Style = csDropDownList
          TabOrder = 1
          OnChange = cbImagesSizeChange
          Items.Strings = (
            'Custom...'
            '16x16'
            '24x24'
            '32x32'
            '48x48'
            '64x64'
            '128x128')
        end
      end
    end
    object gbSelectedImage: TGroupBox
      AlignWithMargins = True
      Left = 353
      Top = 0
      Width = 213
      Height = 330
      Margins.Left = 4
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alRight
      Caption = 'Selected Image'
      TabOrder = 1
      object pbPreview: TPaintBox
        AlignWithMargins = True
        Left = 10
        Top = 18
        Width = 193
        Height = 193
        Margins.Left = 8
        Margins.Right = 8
        Align = alTop
        OnMouseDown = pbPreviewMouseDown
        OnMouseMove = pbPreviewMouseMove
        OnMouseUp = pbPreviewMouseUp
        OnPaint = pbPreviewPaint
      end
      object lbTransparentColor: TLabel
        AlignWithMargins = True
        Left = 10
        Top = 242
        Width = 193
        Height = 13
        Margins.Left = 8
        Margins.Top = 0
        Margins.Right = 8
        Align = alBottom
        Caption = 'Transparent Color:'
      end
      object lbBackgroundFill: TLabel
        AlignWithMargins = True
        Left = 10
        Top = 282
        Width = 193
        Height = 13
        Margins.Left = 8
        Margins.Top = 0
        Margins.Right = 8
        Align = alBottom
        Caption = 'Background Fill:'
      end
      object cbTransparentColor: TComboBox
        AlignWithMargins = True
        Left = 10
        Top = 258
        Width = 193
        Height = 21
        Margins.Left = 8
        Margins.Top = 0
        Margins.Right = 8
        Align = alBottom
        TabOrder = 1
        OnChange = cbTransparentColorChange
        OnExit = cbTransparentColorExit
      end
      object cbBackgroundFill: TComboBox
        AlignWithMargins = True
        Left = 10
        Top = 298
        Width = 193
        Height = 22
        Margins.Left = 8
        Margins.Top = 0
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alBottom
        Style = csOwnerDrawVariable
        TabOrder = 2
        OnChange = cbBackgroundFillChange
        OnExit = cbBackgroundFillExit
      end
      object cbGridlines: TCheckBox
        AlignWithMargins = True
        Left = 10
        Top = 222
        Width = 193
        Height = 17
        Margins.Left = 8
        Margins.Right = 8
        Align = alBottom
        Caption = 'Show Gridlines'
        TabOrder = 0
        OnClick = cbGridlinesClick
      end
    end
  end
  object pnlBottom: TPanel
    AlignWithMargins = True
    Left = 8
    Top = 346
    Width = 566
    Height = 32
    Margins.Left = 8
    Margins.Top = 0
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alBottom
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 1
    object lblManifestWarning: TLabel
      AlignWithMargins = True
      Left = 38
      Top = 3
      Width = 246
      Height = 26
      Margins.Left = 6
      Align = alLeft
      AutoSize = False
      Caption = 'Manifest Warning'
      Transparent = True
      Visible = False
      WordWrap = True
    end
    object imgWarning: TImage
      Left = 0
      Top = 0
      Width = 32
      Height = 32
      Align = alLeft
      AutoSize = True
      Picture.Data = {
        0B546478504E47496D61676589504E470D0A1A0A0000000D4948445200000020
        000000200806000000737A7AF40000001974455874536F667477617265004164
        6F626520496D616765526561647971C9653C0000001B744558745469746C6500
        5761726E696E673B4E6F74696669636174696F6E3BB6E779860000049E494441
        54785EED94794CD37718C6B7653AC45DC9FE59B66CF18F89324427E31045C298
        3B5C9685C503C7C47901634303988906141430A3188E5105B103741C010B4839
        46E528542BB08A481139047518CA515A282D679167DFF797C589A2CE49354B6C
        F2499E3CEFFBE57979DFB4CF0178AA3CFBFC7F91C539BD517BCC793F6377E961
        07D3279DFF82FCA8F3F1F63307D1F6FB3E30CD27EF49853F5FC27358511BEF3C
        61D05D8541D70AD2E451CDE8E9D1DE8B4D6BE29C9A6ECAE230AE9673749C8DC1
        1F7CA766BECF92B946FFEFCF45AD8CB822DC09C38002633D950437C415A10F58
        2DCAA85B1087D9DB55473B1A467A6A31D625C178AF8C60BA02C34A19586DA238
        D4DED12843446EB5303DCB7368BC2189C1586F3546FB5B202911119C1E65435C
        2FE3E15CE4CA56BEF7E297677CF592F0E5118A746F18FA9B31DA2D43B22001CB
        9C3610A4396F4C5909459A37586FEC8C6EA120C8D64E7A68B961B8A71EC31D45
        185156209E1F035BC7F51C094762396FF88608FA0E0958EF447EA0CD47333244
        88ABD99CB290658D6D67781853D561A82D13237F1623EFD409583BAC25489347
        35364401AE1685A1FC807D1B7FFBA2571E7BF5C58136E11793B6C1A0BB097D4B
        06230D43EDB9904BF360B5FC6B82347954E37A86D9301793B7431C64F3783F50
        423F2BEB9240DBF1A1CE3AB6DE22E81A5338F44D69E86C966289DD570469F2FE
        A9B7A4437F5D0CF6F656B6BFD5C7FFE914BBBE986752F4D3878AE6BC83185337
        41DB701C5A45E26D46BBE5B05AF625417A4A8D7A752D9968CEDD8FE2DDD6D762
        DDCD5F7DE4D5E7F92E0DAF3EB2915BFDE0E51468EB8E4C61E466253EFD7C0D41
        9ABCA9D41F83BE35073547DD21F2B38A7FA4536478595A17FA2E1D1F6815B375
        0AA1A90AC7C085280CD4C6DE467FF53442020308D277D6A897DE3042A1A91380
        FDADC92CEFC5ABFED5297C57BD6B72FAC7258A4B277762A4B31AAA727FA84A7D
        197EE89304402DDD078D2C14DA4B020CB517328AA0AD17904735EAE17AB93765
        FED09C0F47FD096F887C3EB811ED6AF6DA43579FE9B92854CA7361E1552C280C
        BDE21FEE415D1984868A9370FBD6036E6E1EA4C99BB69706E997C7401AE98253
        5E96890F3C45CA26F3A5D99E96E3AADA340CD6A7A0A7D0635A066AE3E1E3E589
        F7AD5613A4C9BB6FBFAAD41F7D3571C8F1B49C4CDD6CF1D9B4A7D8E5FCCEECCC
        2D1675F2444F76D30274177D8F2ED177D3A23E1F81D0BD3B306F813DC7C13D3B
        C8A3DAF4E46F814AB21717051E38B57551C76197F75EBF67F5A91B1786941F58
        CDC20BD157190C65AEDB7DE9CADF064D431612780104A7BB0AB63DF88D6833D4
        E70EA12CD819E9EEE682BB4FF16286BB797B47F92F186C4845DFD970287336A0
        53B8EEFE64BBA2B76C0F419ABC87D29DEF816BA2BDA02C9639EBCE016627BB9A
        955445AD43BFFC283435B150571D865AF63387A63A8AF307EA92A055FC86C1C6
        2CE89A73A16BCAC1E01521B49733A0AD3F89FE0BF1E0DECA78505504A3B73400
        3D625F74177841797A1394D96E90867D82A4F566C52CF3A53B0798B573C55B8E
        BFAE9DDF94FECD8249068CC4A460CDFC86EDB66FDADCBD01BAC75CC6DB8C850C
        0B23B1F0EF0CD3E9BE8E64CC66CC312226943125FCD907C053E52F6A6A3577F3
        F307D50000000049454E44AE426082}
      Visible = False
    end
    object btnApply: TButton
      AlignWithMargins = True
      Left = 491
      Top = 4
      Width = 75
      Height = 25
      Margins.Left = 6
      Margins.Top = 4
      Margins.Right = 0
      Action = actApply
      Align = alRight
      TabOrder = 2
    end
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 410
      Top = 4
      Width = 75
      Height = 25
      Margins.Left = 6
      Margins.Top = 4
      Margins.Right = 0
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnOK: TButton
      AlignWithMargins = True
      Left = 329
      Top = 4
      Width = 75
      Height = 25
      Margins.Left = 6
      Margins.Top = 4
      Margins.Right = 0
      Action = actOK
      Align = alRight
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
  object pmImageLists: TPopupMenu
    Images = imglSmall
    Left = 376
    Top = 32
  end
  object pmCommands: TPopupMenu
    Images = imglSmall
    Left = 24
    Top = 32
    object miAddFromFile: TMenuItem
      Action = actAddFromFile
    end
    object miAddFromDXGallery: TMenuItem
      Action = actAddFromDXGallery
    end
    object miReplaceFromDisk: TMenuItem
      Action = actReplaceFromFile
      Caption = 'Replace From Disk'
    end
    object miReplaceFromDevExpressGallery: TMenuItem
      Action = actReplaceFromDXGallery
      Caption = 'Replace From DevExpress Gallery'
    end
    object miDelete: TMenuItem
      Action = actDelete
    end
    object miClear: TMenuItem
      Action = actClear
    end
    object miExport: TMenuItem
      Action = actExport
      object AsBitmap1: TMenuItem
        Action = actExportAsBitmap
      end
      object AsPNG1: TMenuItem
        Action = actExportAsPNG
      end
      object asSVG2: TMenuItem
        Action = actExportAsSVG
      end
    end
    object miImport: TMenuItem
      Action = actImport
    end
    object miConvertTo32bit: TMenuItem
      Action = actConvertTo32bit
    end
  end
  object imglSmall: TcxImageList
    SourceDPI = 96
    CompressData = True
    FormatVersion = 1
    DesignInfo = 19923176
    ImageInfo = <
      item
        Image.Data = {
          910200005844424D020004023604850000424D36040000000000003600000028
          0000000210000000830100200000000000000400001400000000814E4E4E0002
          4D4D4D00024B4B4B008148484800024747470081454545000343434300040000
          00008151515100026BCE7D008569CF7F0068CF800068D0810067D1830067D184
          000265D286008264D38800454545000400000000835454540064D388002FC562
          00022EC76500022DC86900022ECA6D008330CB700064D9950047474700040000
          00008C5757570068D9960041CF7A004CD181005CD68C0072DB9C0085E1A900A1
          E7BB00BDEDCD00E4F1DB00F2F5E6004B4B4B0004000000008559595900D1EFD4
          00E4F1DB00F6F1DD00F9F2DF0002FDF3E10081FDF4E20002FFF4E60082FFF8EE
          004D4D4D000400000000825C5C5C00FFF6EA0002FFF3E30082FFF3E100FFF2DF
          0002FFF1DE0002FFF1DD00810353190002034F18000300000000835E5E5E00FF
          F0DA00FFEBCE0002FFEACC0083FFE9CA00FFE9C800FFE8C70002FFE7C4008302
          591B000FCD69000353190003000000008361616100FFE8C700FFDFB30002FFDE
          B00088FFDDAE00FFDDAD00FFDCAB00FFDBAA00FFDAA600025E1D000FCD690002
          561A0003000000008364646400FFDEB000FFDDAE0002FFDDAD008BFFDCAB00FF
          DBAA0003752800037025000269220002641F0027D87C00025E1D0002591B0002
          561A0003531900026666660081646464000263636300026161610081037C2C00
          026FF0B0008353E89C003CE08A0027D87C00020FCD69008102561A0007000000
          00020387330087037C2C000375280053E89C000269220002641F00025E1D0002
          591B000A0000000083037C2C006FF0B000037025000D00000000830387330087
          F6C100037C2C000D000000008104943A0002038733001300000000}
        Mask.Data = {
          690000005844424D02000100817E030083424D7E0700813E0300812803008110
          03008110030083010001050081400B0081020B0003FF810002FF0300810F0300
          810F0300810F0300810F0300810F0300810703008107030081070A0081FE0300
          82FFC7020082FFC7020082FFC7020002FF0200}
      end
      item
        Image.Data = {
          0E0300005844424D020004023604850000424D36040000000000003600000028
          000000021000000083010020000000000000040000110000000081A559060002
          00000000027A7A7A000377777700037575750081737373000270707000810000
          000003A559060082CCB592007D7D7D00038FDA9D00818EDBA000028DDCA10003
          8CDDA4008173737300020000000085A5590600D3CFC200A55906007D7D7D008B
          DEA5000262D389000262D58C000361D79000828CE3AF00757575000400000000
          8CA5590600818181008CE3AF0072DB9C0078DCA00085E1A90095E4B400A1E7BB
          00BDEDCD00D1EFD400EDF6E7007777770005000000008381818100E4F1DB00ED
          F6E70002F9F5E600814E4E4E00024D4D4D00024B4B4B00814848480002474747
          00814545450002434343008285858500FFF8EE0003FFF6EA008151515100026B
          CE7D008569CF7F0068CF800068D0810067D1830067D184000265D28600834545
          450085858500FFF3E30003FFF0DA00835454540064D388002FC56200022EC765
          00022DC86900022ECA6D008564D995004848480089898900FFEDD300FFE7C600
          02FFE7C4008C5757570068D9960041CF7A004CD181005CD68C0072DB9C0085E1
          A900A1E7BB00BDEDCD00EDF6E7004B4B4B008A8A8A0004FFE6C2008559595900
          D1EFD400E4F1DB00F6F1DD00F9F2DF0002FDF3E10085FDF4E200FFF4E600FFF8
          EE004E4E4E008D8D8D00038A8A8A0083898989005C5C5C00FFF6EA0002FFF3E3
          0082FFF3E100FFF2DF0002FFF1DE0083FFF1DD00FFF4E6005151510005000000
          00835E5E5E00FFF0DA00FFEBCE0002FFEACC0087FFE9CA00FFE9C800FFE8C700
          FFE7C400FFEDD30054545400C6731B0004000000008361616100FFE8C700FFDF
          B30002FFDEB00089FFDDAE00FFDDAD00FFDCAB00FFDBAA00FFE4BB0057575700
          C9761D00DAD4C600C6731B0002000000008364646400FFDEB000FFDDAE0002FF
          DDAD0081FFDCAB0002FFDBAA0002FFDAA6008659595900D8C3A600CB781E00C9
          761D00C6731B0000000000026666660081646464000263636300026161610003
          5E5E5E00815C5C5C00020000000081CB781E000D00000000}
        Mask.Data = {
          570000005844424D02000100817E030083424D7E0700813E0300812803008110
          03008110030083010001050081400B0081020B0003FF8300FFFB030081100300
          81180300811E0300811F1A0081F8030081780300811803008108030082DFFF02
          00}
      end
      item
        Image.Data = {
          BA0200005844424D020004023604850000424D36040000000000003600000028
          0000000210000000830100200000000000000400001400000000814E4E4E0002
          4D4D4D00024B4B4B008148484800024747470081454545000343434300040000
          00008151515100026BCE7D008569CF7F0068CF800068D0810067D1830067D184
          000265D286008264D38800454545000400000000835454540064D388002FC562
          00022EC76500022DC86900022ECA6D008330CB700064D9950047474700040000
          00008C5757570068D9960041CF7A004CD181005CD68C0072DB9C0085E1A900A1
          E7BB00BDEDCD00E4F1DB00F2F5E6004B4B4B0004000000008559595900D1EFD4
          00E4F1DB00F6F1DD00F9F2DF0002FDF3E10081FDF4E20002FFF4E60082FFF8EE
          004D4D4D00020000000084969FD300000000005C5C5C00FFF6EA0002FFF3E300
          84FFF3E100FFF2DF00EFE2DC00FFF1DE0002FFF1DD0089FFF3E3004E4E4E0000
          000000969FD300929BD100000000005E5E5E00FFF0DA00FFEBCE0002FFEACC00
          83FFE9CA00B7A8CA00E7D3C60002FFE7C40084FFEDD30053535300C9D5EB005B
          60BD0002000000008361616100FFE8C700FFDFB30002FFDEB00089FFDDAE00C4
          ABB6008B78B700FADAAB00FFDAA600FFE4BB00535353006469CA00969FD30002
          000000008364646400FFDEB000FFDDAE0002FFDDAD0082FFDBAA00FADAAB0002
          8B78B70002FFDAA600822A2A94006469CA000300000000026666660081646464
          00026363630002616161008159596600042A2A94000C0000000084AEB8E5000A
          0BCC000505C9007C84D9000A0000000083C9D5EB007D84E4002C2FD700026D73
          DD00851011CA001F21CD005D62D6008991DA00C9D5EB000600000000837D84E4
          00383AD6009BA4E300020000000085AEB8E5004E52D9001011CA000505C50047
          4BD300060000000002C9D5EB00050000000083B5C0E7007D84E4008B93E20010
          00000000}
        Mask.Data = {
          710000005844424D02000100817E030083424D7E0700813E0300812803008110
          03008110030083010001050081400B0081020B0003FF810002FF0300810F0300
          810F0300810F0300810F0300810D030081090300810303008103030081070300
          810F020082FF0F020081FC030082FC60020082FCF8020002FF0200}
      end
      item
        Image.Data = {
          230200005844424D020004023604850000424D36040000000000003600000028
          0000000210000000830100200000000000000400001100000000815B60BD000E
          000000008299A2D3005B60BD00020000000081C9D5EB000B00000000824B4FB4
          0099A2D3000200000000818991DA000A00000000828A92CF004B4FB400030000
          0000827C84D900A4ADE400080000000083B5C0E7002C2EAF00929BD100030000
          000082AEB8E5005D62D6000800000000025B60BD000500000000826D73DD005D
          62D6000600000000837178CF002C2EAF00AEB8E5000600000000823F42D20064
          6AD8000400000000837C84D9001515BC008991DA000700000000839BA4E3001F
          21CD00585CD6000200000000837C84D9001515BC007177D40009000000008685
          8DE1001011CA00383AD600474BD3000B0BC500646AD8000B0000000085646AD8
          000A0BCC000505C9002E30CE00A4ADE4000A0000000088858DE1002C2FD7000A
          0BCC001A1BCD000505C9001A1BCD005D62D6009BA4E300060000000088A4ADE4
          004E52D9000808D5003033D5008B93E200A4ADE4004E52D9000505C900020505
          C600862E30CE005D62D600838ADB00A4ADE400000000005D63E200020808D500
          825E63DF00B5C0E700030000000084858DE100383AD6000505C9000505C60002
          0505C500850505C0007C84D9007D84E4005D63E200959EE70007000000008685
          8DE100474BD3001011CA000505C6000505C5001F21C7000C0000000084B5C0E7
          00858DE1005D62D6009BA4E300}
        Mask.Data = {
          7C0000005844424D02000100817E030083424D7E0700813E0300812803008110
          03008110030083010001050081400B0081020B0003FF8300FFFB020082FFF302
          00827FF30200827FE70200823FC70200823FCF0200829F8F020082CF1F020082
          C63F020082E07F020082F07F020082E01F0200828001020081070300821FC002
          0082FFF00200}
      end
      item
        Image.Data = {
          F30200005844424D020004023604850000424D36040000000000003600000028
          000000021000000083010020000000000000040000150000000083C5A67B00B2
          6C3400CA8B580002242424000319191900030F0F0F0083BA774300A1551A00B7
          8E5D00020000000085BA774300EFCA9100EBC588003434340032323200022C2C
          2C00812424240002E2B97A008419191900E2B97A00DAA65600A1551A00020000
          00008EC17E4800F2CE9600EFCA910045454500414141003C3C3C003737370034
          343400E5BD7D00E4BA7C0024242400E2B97A00DAA65600A1551A000200000000
          8EC6834E00F4D19D00F2CE960059595900535353004E4E4E0048484800454545
          00E7BD8300E5BD7D0037373700E2B97A00DAA65600A85E240002000000008ECA
          8B5800F6D5A500F4D19D006B6B6B0066666600616161005C5C5C005757570053
          5353004D4D4D0048484800E5BD7D00DAA65600A85E2400020000000084CA8B58
          00FADAAB00F6D5A500F4D19D0002EFCA910003EBC5880003E7BD830082E1AE5F
          00A85E2400020000000083D1915D00FBDDB100F2CE960002F3C8860083EDC07A
          00ECBC7200E9B86D0002E6B5690002E3B1640082E1AE5F00B26C340002000000
          0082D99B6800FFE4BB0002E3AF790002E3B2750002E4AF6E0004E2AD680082E3
          B16400B26C3400020000000083D99B6800FFE4BB00E3AF790002FCFCFC0002FA
          FAFA0002F8F8F80002F6F6F60083E2AD6800E6B56900B26C3400020000000084
          DFA47100FFE6C200DFA47100FEFEFE0004D6D6D60002D2D2D20084F8F8F800E0
          A96A00E6B56900BA774300020000000083DFA47100FFECC900DFA4710002FFFF
          FF0002FEFEFE0002FCFCFC0002FAFAFA0083E0A96A00E9B86D00BA7743000200
          00000084E4A77500FFEFCF00D99B6800FFFFFF0004D9D9D90002D6D6D60084FC
          FCFC00E0A96A00ECBC7200C17E4800020000000083E4A77500FFF4D400DFAE85
          0007FFFFFF0084FCFCFC00E7BD8300EFCA9100C6834E00020000000081E2CFB0
          0002E4A7750008D9D9D90083D1915D00CA8B5800CCB592001100000000}
        Mask.Data = {
          7C0000005844424D02000100817E030083424D7E0700813E0300812803008110
          03008110030083010001050081400B0081020B0003FF810002FF020082800102
          0082800102008280010200828001020082800102008280010200828001020082
          8001020082800102008280010200828001020082800102008280010200828001
          020002FF0200}
      end
      item
        Image.Data = {
          2F0200005844424D020004023604850000424D36040000000000003600000028
          00000002100000008301002000000000000004000017000000008C6666660061
          6161005C5C5C0057575700515151004D4D4D0047474700414141003C3C3C0037
          373700343434002C2C2C000400000000826B6B6B00E3E3E30009FFFFFF008132
          3232000400000000826B6B6B00E3E3E30009FFFFFF0081343434000400000000
          8270707000E3E3E30002FFFFFF0081BF611B0006FFFFFF008137373700040000
          00008273737300E3E3E30002FFFFFF0002C5631B0005FFFFFF00813C3C3C0002
          0000000086E07A2E00DD762A00DA722600D6712600D46C2200D16B1F0002CB66
          1C0081C5631B0004FFFFFF00813C3C3C00020000000081E8833A0002E37E3400
          87E07A2E00DD762A00DA722600D6712600D16B1F00CE691F00CB661C0003FFFF
          FF008141414100020000000002EE8D460002E8833A0002E37E340083E07A2E00
          DD762A00D671260004FFFFFF00814545450004000000008281818100E3E3E300
          02FFFFFF0082E8833A00E37E340005FFFFFF0081484848000400000000828181
          8100E3E3E30002FFFFFF0081EE8D460006FFFFFF00814B4B4B00040000000082
          85858500E3E3E30009FFFFFF00814E4E4E0004000000008289898900E3E3E300
          09FFFFFF0081535353000400000000818A8A8A000AE3E3E30081575757000400
          0000008C8D8D8D008989890085858500818181007D7D7D007777770073737300
          7070700066666600646464005E5E5E00595959001100000000}
        Mask.Data = {
          7C0000005844424D02000100817E030083424D7E0700813E0300812803008110
          03008110030083010001050081400B0081020B0003FF810002FF020082E00102
          0082E001020082E001020082E001020082E00102008280010200828001020082
          8001020082E001020082E001020082E001020082E001020082E001020082E001
          020002FF0200}
      end
      item
        Image.Data = {
          070300005844424D020004023604850000424D36040000000000003600000028
          000000021000000083010020000000000000040000040000000010FF00FF0002
          4E4E4E00834D4D4D004B4B4B004A4A4A00024949490083484848004747470045
          45450002444444008943434300424242004141410040404000505050006ACE7C
          006ACE7D0069CF7D0069CF7F000AFFFFFF0086434343005353530065D1830030
          C25A002EC35B002DC25C0002FFFFFF00020505C70082BABAF000FFFFFF000305
          05C70087FFFFFF00464646005555550062D48B002EC766002EC8670030C86A00
          03FFFFFF0085DADAF7000505C700FFFFFF000505C700DADAF70002FFFFFF0086
          48484800585858005FD78F002ECA6E0032CB71003ACE760003FFFFFF008D0505
          C700BABAF000FFFFFF00BABAF0000505C700BABAF000FFFFFF004B4B4B005A5A
          5A0098E3B30084DDA2008CDDA50094E0AA0003FFFFFF0082DADAF7000505C700
          02FFFFFF0089DADAF7000505C700FFFFFF004D4D4D005D5D5D00E2F2DC00ECF1
          DA00F3F1DB00F7F1DD0002FFFFFF00020505C70082BABAF000FFFFFF00020505
          C70085BABAF000FFFFFF00505050005F5F5F00FDF7EC0002FFF6E90081FFF5E9
          000AFFFFFF00845353530061616100FFF5E900FFF2E10002FFF2DF000AFFFFFF
          00855656560064646400FFF2E000FFEED400FFEDD20002FFEDD10082FFECD100
          FFECD00002FFEBCE0081FFEBCD0002FFEACC0086FFEACB00FFEFD80058585800
          66666600FFEDD200FFE6C30002FFE5C10083FFE5C000FFE4C000FFE4BF0002FF
          E4BD0089FFE3BC00FFE2BB00FFE2BA00FFE2B900FFE9CA005A5A5A0068686800
          FFE8C600FFDFB30002FFDEB1008EFFDEB000FFDDAF00FFDDAE00FFDDAD00FFDD
          AC00FFDCAB00FFDBAB00FFDBA900FFDBA800FFE4BC005D5D5D006B6B6B00FFE2
          B900FFE1B90002FFE0B70084FFE1B600FFE0B600FFE0B500FFE0B40002FFE0B3
          0087FFDEB300FFDEB100FFDEB000FFDEAF005F5F5F006D6D6D006C6C6C00026B
          6B6B00816A6A6A00026969690002686868008566666600656565006666660065
          65650064646400026262620010FF00FF00}
        MaskColor = clFuchsia
      end
      item
        Image.Data = {
          D80300005844424D020004023604850000424D36040000000000003600000028
          0000000210000000830100200000000000000400000400000000840000000200
          0000090000000E0000000F020000001003000000110200000012830000001100
          00000C00000003020000000089000000087B5043C0AB705CFFAB6F5AFFAB705C
          FFAA6F5BFFAA6E59FFA96F5AFFAA6D59FF02AA6C59FF83A96C58FF794D3FC300
          00000B02000000008E0000000CAD735FFFFDFBF9FFFBF5F2FFFAF5F1FFFAF4F0
          FFFAF4EFFFFAF2EEFFFAF1EDFFF8F1ECFFF8F0EBFFF8F0EAFFD5B6ADFF000000
          110200000000850000000CB07762FFFDFBFAFFF7EDE6FF6462BAFF0217169CFF
          8216159BFF14159AFF03141499FFEB131498FF131398FF0C0D65AD0000000000
          00000BB07966FFFDFBFBFFF8EEE8FF2121A5FFFAF6F6FF17169CFFF9F5F3FF15
          159BFFF8F2F1FF141499FF5D5BB4FFF7EFEDFFF6EEEBFF131396FF0000000000
          00000AB37C69FFFEFCFBFFF8F0EAFF2222A7FFFBF7F6FF17179DFFF9F5F4FF15
          159CFFF8F4F2FF15149AFFF7F2EFFF141499FFF6F0ECFF131398FF0000000000
          000009B67F6CFFFEFDFCFFF9F0EBFF2324A8FFFBF8F7FF18179DFFFAF6F5FF16
          169CFFF9F5F2FF15149AFFF8F3F0FF141499FF141497FF131496FF0000000000
          000009B98371FFFEFDFCFFFAF3EEFF2524A8FFFCF8F8FF18179EFFFBF7F6FFFA
          F6F5FFFAF5F3FF15159BFFF8F3F1FF14149AFF141499FF141497FF0000000000
          000008BC8877FFFEFEFDFFFBF4EFFF2624A9FFFBFAF9FF18189EFFFAF8F7FF60
          5FBAFFFAF7F5FF15169BFF5E5DB7FFF8F3F1FFF8F2F0FF141497FF0000000000
          000007BF8C7AFFFEFEFDFFFBF6F1FF6A69C1FF2624A9FF2424A8FF2223A7FF21
          21A6FF2020A4FF1E1EA4FF1D1CA2FF1C1CA0FF1A1A9FFF11116BAD0000000000
          000006C18F7FFFFEFEFEFFFAF6F3FFFAF5F3FFFBF6F2FFFBF5F1FF02FBF5F0FF
          85FAF4EFFFFAF4EEFFFDF9F8FFDEC3BAFF0000000B02000000008400000006C4
          9382FFFFFEFEFFFBF7F4FF02FBF6F4FF02FCF6F3FF86FCF4F2FFFBF5F1FFFBF5
          F0FFFDFBF9FFBF8C7BFF0000000B02000000008500000005C79985FFFFFEFEFF
          FCF8F7FFFCF8F6FF02FCF7F5FF02FBF6F4FF85FCF6F3FFFCF6F2FFFDFCFAFFC2
          8F7FFF0000000A02000000008200000004C99A89FF02FFFFFEFF02FFFEFEFF02
          FEFEFEFF02FEFEFDFF02FEFDFDFF82C49382FF00000008020000000082000000
          02977567C002CA9C8BFF8AC99C8AFFC99B89FFC99B8AFFCA9A88FFC89A88FFC9
          9987FFC79887FFC89886FF927163C20000000502000000008300000001000000
          0200000003020000000403000000050400000006820000000500000001020000
          0000}
      end>
  end
  object spdSave: TSavePictureDialog
    DefaultExt = '*.bmp'
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Left = 233
    Top = 344
  end
  object actlCommands: TActionList
    Images = imglSmall
    Left = 200
    Top = 344
    object actAdd: TAction
      Caption = '&Add'
      Hint = 'Add image'
      ImageIndex = 0
      OnExecute = actAddExecute
    end
    object actAddFromFile: TAction
      Caption = 'Load From Disk'
      ImageIndex = 0
      ShortCut = 45
      OnExecute = actAddFromFileExecute
    end
    object actAddFromDXGallery: TAction
      Caption = 'Load From DevExpress Icon Library'
      ImageIndex = 0
      ShortCut = 32813
      OnExecute = actAddFromDXGalleryExecute
    end
    object actInsert: TAction
      Caption = 'Insert'
      ShortCut = 8237
      OnExecute = actInsertExecute
    end
    object actReplace: TAction
      Caption = '&Replace'
      Hint = 'Replace image'
      ImageIndex = 1
      OnExecute = actReplaceExecute
    end
    object actReplaceFromFile: TAction
      Caption = 'Load From Disk'
      ImageIndex = 1
      ShortCut = 16466
      OnExecute = actReplaceFromFileExecute
    end
    object actReplaceFromDXGallery: TAction
      Caption = 'Load From DevExpress Icon Library'
      ImageIndex = 1
      ShortCut = 49234
      OnExecute = actReplaceFromDXGalleryExecute
    end
    object actDelete: TAction
      Caption = '&Delete'
      Hint = 'Delete image'
      ImageIndex = 2
      ShortCut = 46
      OnExecute = actDeleteExecute
    end
    object actClear: TAction
      Caption = '&Clear'
      Hint = 'Clear'
      ImageIndex = 3
      ShortCut = 16430
      OnExecute = actClearExecute
    end
    object actImport: TAction
      Caption = '&Import'
      Hint = 'Import images'
      ImageIndex = 5
      OnExecute = actImportExecute
    end
    object actApply: TAction
      Caption = 'Apply'
      OnExecute = actApplyExecute
    end
    object actOK: TAction
      Caption = 'OK'
      OnExecute = actOKExecute
    end
    object actExportAsBitmap: TAction
      Caption = 'as &Bitmap...'
      Hint = 'Export as bitmap'
      ImageIndex = 4
      ShortCut = 16450
      OnExecute = actExportAsBitmapExecute
    end
    object actExportAsPNG: TAction
      Caption = 'as &PNG...'
      Hint = 'Export as PNG'
      ImageIndex = 4
      ShortCut = 16464
      OnExecute = actExportAsPNGExecute
    end
    object actExportAsSVG: TAction
      Caption = 'as &SVG...'
      ImageIndex = 4
      ShortCut = 16455
      OnExecute = actExportAsSVGExecute
      OnUpdate = actExportAsSVGUpdate
    end
    object actExport: TAction
      Caption = '&Export'
      Hint = 'Export images'
      ImageIndex = 4
      OnExecute = actExportExecute
    end
    object actConvertTo32bit: TAction
      Caption = 'C&onvert to 32-bit'
      Hint = 'Convert all images to 32-bit'
      ImageIndex = 6
      OnExecute = actConvertTo32bitExecute
    end
  end
  object opdOpen: TOpenPictureDialog
    Filter = 
      'All (*.bmp, *.ico)|*.bmp;*.ico|Bitmaps (*.bmp)|*.bmp|Icons (*.ic' +
      'o)|*.ico'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 262
    Top = 344
  end
  object pmExport: TPopupMenu
    Images = imglSmall
    Left = 128
    Top = 264
    object AsBitmap2: TMenuItem
      Action = actExportAsBitmap
      Caption = 'Export as &Bitmap...'
    end
    object AsPNG2: TMenuItem
      Action = actExportAsPNG
      Caption = 'Export as &PNG...'
    end
    object asSVG1: TMenuItem
      Action = actExportAsSVG
      Caption = 'Export as &SVG...'
    end
  end
  object pmAdd: TPopupMenu
    Images = imglSmall
    Left = 16
    Top = 264
    object AddFromFile1: TMenuItem
      Action = actAddFromFile
    end
    object AddFromDXGallery1: TMenuItem
      Action = actAddFromDXGallery
    end
  end
  object pmReplace: TPopupMenu
    Images = imglSmall
    Left = 40
    Top = 264
    object ReplaceFromFile1: TMenuItem
      Action = actReplaceFromFile
    end
    object ReplaceFromDXGallery1: TMenuItem
      Action = actReplaceFromDXGallery
    end
  end
end