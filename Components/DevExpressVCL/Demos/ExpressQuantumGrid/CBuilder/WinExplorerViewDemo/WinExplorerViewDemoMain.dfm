inherited frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'ExpressQuantumGrid WinExplorerView Demo'
  ClientHeight = 623
  ClientWidth = 908
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 908
    Height = 32
    Caption = 
      'This demo illustrates the Win Explorer View, which is inspired b' +
      'y Microsoft Windows Explorer. With this View, you can display re' +
      'cords with captions, descriptions, images, and check boxes using' +
      ' one of the seven built-in display modes.'
  end
  inherited sbMain: TStatusBar
    Top = 604
    Width = 908
  end
  object Grid: TcxGrid [2]
    Left = 0
    Top = 107
    Width = 908
    Height = 497
    Align = alClient
    TabOrder = 1
    object WinExplorerView: TcxGridDBWinExplorerView
      Navigator.Buttons.CustomButtons = <>
      FindPanel.DisplayMode = fpdmManual
      ActiveDisplayMode = dmExtraLargeImages
      DataController.DataSource = dmGridCars.dsModels
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      ItemSet.CheckBoxItem = WinExplorerViewInStock
      ItemSet.DescriptionItem = WinExplorerViewDescription
      ItemSet.ExtraLargeImageItem = WinExplorerViewPhoto
      ItemSet.MediumImageItem = WinExplorerViewImage
      ItemSet.TextItem = WinExplorerViewName
      OptionsData.Inserting = False
      OptionsView.CellEndEllipsis = True
      OptionsView.FocusRect = False
      OptionsView.ShowExpandButtons = True
      DisplayModes.ExtraLargeImages.ImageSize.Height = 192
      DisplayModes.LargeImages.ImageSize.Height = 72
      object WinExplorerViewTrademark: TcxGridDBWinExplorerViewItem
        DataBinding.FieldName = 'Trademark'
      end
      object WinExplorerViewName: TcxGridDBWinExplorerViewItem
        Tag = -1
        DataBinding.FieldName = 'Name'
      end
      object WinExplorerViewCategory: TcxGridDBWinExplorerViewItem
        Tag = 1
        DataBinding.FieldName = 'Category'
      end
      object WinExplorerViewBodyStyle: TcxGridDBWinExplorerViewItem
        Tag = 3
        DataBinding.FieldName = 'BodyStyle'
      end
      object WinExplorerViewTransmissionTypeName: TcxGridDBWinExplorerViewItem
        Tag = 2
        DataBinding.FieldName = 'TransmissionTypeName'
      end
      object WinExplorerViewDescription: TcxGridDBWinExplorerViewItem
        Tag = -1
        DataBinding.FieldName = 'Description'
      end
      object WinExplorerViewPhoto: TcxGridDBWinExplorerViewItem
        Tag = -1
        DataBinding.FieldName = 'Photo'
        RepositoryItem = dmGridCars.EditRepositoryImage
        Options.Editing = False
      end
      object WinExplorerViewInStock: TcxGridDBWinExplorerViewItem
        Tag = -1
        DataBinding.FieldName = 'InStock'
      end
      object WinExplorerViewImage: TcxGridDBWinExplorerViewItem
        Tag = -1
        DataBinding.FieldName = 'Image'
        RepositoryItem = dmGridCars.EditRepositoryImage
        Options.Editing = False
      end
    end
    object Level: TcxGridLevel
      GridView = WinExplorerView
    end
  end
  object GroupBox: TcxGroupBox [3]
    Left = 0
    Top = 32
    Align = alTop
    PanelStyle.Active = True
    PanelStyle.OfficeBackgroundKind = pobkGradient
    TabOrder = 2
    Height = 75
    Width = 908
    object gcDisplayModes: TdxGalleryControl
      Left = 4
      Top = 5
      Width = 397
      Height = 65
      OptionsBehavior.ItemCheckMode = icmSingleRadio
      OptionsView.ContentOffsetItems.All = -1
      OptionsView.ContentOffsetItems.Left = 6
      OptionsView.ContentOffsetItems.Top = 2
      OptionsView.ContentOffsetItems.Right = 6
      OptionsView.ContentOffsetItems.Bottom = 2
      OptionsView.Item.Image.ShowFrame = False
      OptionsView.Item.Text.AlignHorz = taLeftJustify
      OptionsView.Item.Text.AlignVert = vaCenter
      OptionsView.Item.Text.Position = posRight
      TabOrder = 0
      OnItemClick = gcDisplayModesItemClick
      object gcgGroup: TdxGalleryControlGroup
        Caption = 'Group'
        ShowCaption = False
        object gciExtraLargeIcons: TdxGalleryControlItem
          Tag = 1
          Caption = 'Extra large images'
          Checked = True
          Glyph.Data = {
            89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
            61000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
            00097048597300000EC100000EC101B8916BED0000021C49444154384FA5935B
            48936118803FCA42AC8B6E8AE84028129416458B24DAE8E08F3397A766336BE6
            42DD21C803466405AEC3868108068998A4A3186A6A9A49695A2323940E0CBB28
            31A2721179918829C1FA9FB6FF26FEE822D60B0FBC37CF73F37DAF00FE0BF17A
            FC03034F5E4644D815E125D23957D3FC3B90D43489B6E513BA968F7F3089CE13
            407BEB1BBAAE3976F40549689D571C5560AF37486AB74C46E70C596D5FC86A0F
            70B02D80B1EB2BC6FE9F64F74E636C1EC3D0F08CCCC651C551055CD5E514B95A
            29F44E60BEFF9DFCEE290AEE4E63190C52E4BE8E292D9963FB25CC9244A97E9F
            E2A80297EF5C606A601B0F7B6C38875E50F67896D21199F2FA5EAC7A1D05A60C
            F2CD36EA0C761EE5DA1447157074CCD0D2749192DB3EEC1E3FD7868671BD9171
            9EAEA2A2C84CB1AD8493A75CCC3DB7F2CE5DA538AAC08D9BAF28387F8FB486B7
            384E749063F5621EFC81252F87B43D5AB6EF34E17B60409675F8478F288E2A50
            5DE14193524F7AE93009EB0B898D4921BA629CD53A43E8B70885D94B02F990C0
            59B944715481C662371BB7DA29AF1D61F7E674A4659BD872760CA94F6655485E
            BB220AFFCA2864BD20552C551C5560226E036B120F60AD6CA543C4532BA28975
            D491EB0F3DED511BC51AC195C38B907D82CC94C58AA30ACC87AA57C52E7A240B
            9FC572DAC53A34E9F1E49D4924BB2C996C298EE30931BCCF5C484DD202C55105
            221925103E88F012095DFD4FFF7EA2FF0EE2178316E79B3E740A640000000049
            454E44AE426082}
        end
        object gciLargeIcons: TdxGalleryControlItem
          Tag = 2
          Caption = 'Large images'
          Glyph.Data = {
            89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
            61000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
            00097048597300000EC100000EC101B8916BED0000017149444154384FA592C9
            2B84611C80FD0F4E6E6E722009074A2E0A27392025CB8121E5A6EC49344CD671
            B165296599061911D234494663672453DEB115524324CBCCE3FD24129F7C393C
            EFE1E9F77BDECBCF0FF8173F4A2DBC3D4ED7310B4BEB9A50763E028AF07ABD9A
            A8681EF81E08AADA21DCE0264C2F886915441A04293D828E55817957D0E470D3
            68DB560F944DB9191350B70A7A3B54DBA0F31076E5D093C4720B25D663F5C096
            C7C39D14B73ED891C30BE7307C024BF7B0FF0297D2CF38857AA064A01F8BCB89
            574AB9F7F6B3121392BD77B77672A41E88CA4B25C35848C3DC38EB57373CF89E
            F1F85EB890816BC9A31C769D9EA907029282094C0B21BC389E84DA1CB2FBCA48
            1FACA5607884029385C14D3B6B075BBF04127584E6D611A1AB27B6B495B8CA16
            920D46529A0CE4F4765034314AB579483DE09FD54548F934D135B324E9AD64B4
            2D93D9BE427EF706F5A6038616CFE8343BD4035AF81250CE52115A989C5FFE0C
            FC871FE5DFC1EF1540DF2D0D974393950000000049454E44AE426082}
        end
        object gciMediumIcons: TdxGalleryControlItem
          Tag = 4
          Caption = 'Medium images'
          Glyph.Data = {
            89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
            61000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
            00097048597300000EC100000EC101B8916BED0000018049444154384FA5913D
            4842511886BFA1D0CDA108A2ADA668E88782A05C84A0A021441A82C241088282
            0822332D907EC08A8B9281495948045122A8648551F6830E95640E4E0A6DB5B4
            042DBD790F1E1BCE8D205FF8EEB9E7F9CE7DCEB91C4A67F3388BDF2BD65FBD42
            88E4C96F995BF315DFC4147B3F82FD17407A00A6AE81C72F864A82234737FC01
            0F7CAF803FC3902838CF25D928A7F87D4990A9254856423817C4F21843A2803A
            08C3AE4948310F966211C6B8209177C298A841BF57838B10FB7751A0D65A503F
            BA81B6E92DE8EC3B8C7181146BC5F85503B4EB1AA4B206C6048152B840292581
            7C1DF244A902A7378A5C2EB9C704659F800B561B09AE26828508E9A767C6B860
            F1F004B6BB0FCCDF7EC2B4E0644C10044C84E804618554B8DC1B648C0B0E826E
            7853EF70479218EAED614C101C17760EA95588EB2B10B511635C30608EA2BDCF
            8E66D3262A0BEBE408826DF32C76678C787354C3D052C518178C0C5A61A9EB82
            DE19864ED7C99820500A1728A52428FB1AE5C7FF0BF40D18562B9EF95E444800
            00000049454E44AE426082}
        end
        object gciSmallIcons: TdxGalleryControlItem
          Tag = 5
          Caption = 'Small images'
          Glyph.Data = {
            89504E470D0A1A0A0000000D4948445200000010000000100803000000282D0F
            53000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
            005D504C5445FFFFFF7C8DA0BBC3CED4DAE18B0900B2170091D0F1423D33AFBA
            C5385F9D008F5E10C9B56A778174D1D48DC6C69BB3837BC915D46F37944200A2
            902E82A0FFB7AE568BAAFF51A800889C73D4FDFFE5FFFFB5785646857080B8B2
            9443001F4FB34D000000097048597300000EC100000EC101B8916BED00000062
            4944415428538D8C470A80401443A7EAD87B2FF73FA6219B3F820BB37881109E
            B20689A08C427404A3AA1A35CD80EB38E405AA6D00DF739037613512045FD2A4
            44D13C8C1CB21445F3B2729037F147BAED289ADDC1E1BC5034FB9B83BC89B754
            870782A902F43D166B550000000049454E44AE426082}
        end
        object gciList: TdxGalleryControlItem
          Tag = 3
          Caption = 'List'
          Glyph.Data = {
            89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
            61000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
            00097048597300000EC100000EC101B8916BED000000AA49444154384F63F8FF
            FF3F45184C9CBF7AFBFFE63D47FF6FDC75F8FFF24D7BFF1F3C7111288C50844F
            1E4C80249141DF9CD5200A6E003E793001321904029B63C174E3C4C5FF83532A
            C01808F0CA830D00390B195476CD05517017E093071317AEDD063B0B64324872
            DD8E237005D8E40F9EA45518DC1002717187013679B001C3280CF433F783695C
            61804D1E6C00C561004ADBC87E5CBB1D6C23DC007CF27045E462AC82C4E3FF0C
            007D3C1E4CCFC8C1070000000049454E44AE426082}
        end
        object gciTiles: TdxGalleryControlItem
          Tag = 6
          Caption = 'Tiles'
          Glyph.Data = {
            89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
            61000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
            00097048597300000EC100000EC101B8916BED0000011D49444154384F63B872
            EBE1FFDD87CFE1C420792060C08519408AF0819ADE05200AAB6610861BB0ECC9
            FFFF13CEFFFF5F7CE4FFFF0BFFFEFFDFFAF60F581C6640486AE57F6C186EC09E
            07A7C0340800F5FFFF056112EF02065386FFB1930BFF4FD837EB7FDBBE6DFFD7
            5C811888CB05203110861BC0615BFD5F297DEA7FA3B2D9FF9D9AE7FF4F9CBB0E
            2C4EB40B7001820680A209A408175EBFEB28501D76CD203C88D2C1DA6E9BFF4B
            D6CFFABFE0F5FFFF4BAEFDFF5FB71D12913003D0630186E1065C9364F83FA196
            E1FFD6071BFFB767FDFFEF6479122C4EB40B4E3E9CF43FE1A4D87F9F39FCFFF7
            6F79F87FF1AA7960715C2E00898130DC8009FB0CFFE71E52FE6FDBC7FFFFE2AD
            90FF0FB3D5C1E244BB001720680065E9E03F03002996171868316E7A00000000
            49454E44AE426082}
        end
        object gciContent: TdxGalleryControlItem
          Caption = 'Content'
          Glyph.Data = {
            89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
            61000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
            00097048597300000EC100000EC101B8916BED000000C249444154384F63F8FF
            FF3FC3955B0FFFEF3E7C0E0583C440728430980069400735BD0B401443486AE5
            7F5C18248F6240C9C52BFF7B8F1D03B3610610C260026600437FDC7F81935BC1
            6C9801B1DD17FFE3C2207914039001492E0005184803325EBFEB28500ABB2664
            0C26A81688132F7CFC9F55DE0866C30C2084C104CC008BF8B9FF9D6C8DC16C98
            01D8020F8641F228062003925C307802F1CADFFFFF9F82596406A25B6BC1FF86
            1D9BC06C9801D8020F8641F228062003925C407E20FE67000089AE0829921F61
            E90000000049454E44AE426082}
        end
      end
    end
    object cbSortBy: TcxComboBox
      Left = 611
      Top = 3
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'Ascending'
        'Descending'
        'None')
      Properties.OnEditValueChanged = cbSortByPropertiesEditValueChanged
      TabOrder = 1
      Text = 'None'
      Width = 121
    end
    object lbSortBy: TcxLabel
      Left = 554
      Top = 5
      Caption = 'Sort Order:'
      Transparent = True
    end
    object lbGroupBy: TcxLabel
      Left = 554
      Top = 29
      Caption = 'Group By:'
      Transparent = True
    end
    object cbGroupBy: TcxComboBox
      Left = 611
      Top = 27
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'Trademark'
        'Category'
        'Transmission type'
        'Body style'
        'None')
      Properties.OnEditValueChanged = cbGroupByPropertiesEditValueChanged
      TabOrder = 4
      Text = 'None'
      Width = 121
    end
    object cbHotTrack: TcxCheckBox
      Left = 413
      Top = 1
      Caption = 'Hot-track'
      Properties.OnEditValueChanged = cbHotTrackPropertiesEditValueChanged
      State = cbsChecked
      TabOrder = 5
      Transparent = True
    end
    object cbMultiSelect: TcxCheckBox
      Left = 413
      Top = 18
      Caption = 'Multi Select'
      Properties.OnEditValueChanged = cbMultiSelectPropertiesEditValueChanged
      TabOrder = 6
      Transparent = True
    end
    object cbShowCheckBoxes: TcxCheckBox
      Left = 413
      Top = 36
      Caption = 'Show Item Check Boxes'
      Properties.OnEditValueChanged = cbShowCheckBoxesPropertiesEditValueChanged
      TabOrder = 7
      Transparent = True
    end
    object cbShowExpandButtons: TcxCheckBox
      Left = 413
      Top = 53
      Caption = 'Show Expand Buttons'
      Properties.OnEditValueChanged = cbShowExpandButtonPropertiesEditValueChanged
      State = cbsChecked
      TabOrder = 8
      Transparent = True
    end
  end
  inherited mmMain: TMainMenu
    Left = 168
    Top = 144
  end
  inherited StyleRepository: TcxStyleRepository
    Top = 144
    PixelsPerInch = 96
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
    inherited GridCardViewStyleSheetDevExpress: TcxGridCardViewStyleSheet
      BuiltIn = True
    end
  end
  inherited cxLookAndFeelController1: TcxLookAndFeelController
    Top = 144
  end
end
