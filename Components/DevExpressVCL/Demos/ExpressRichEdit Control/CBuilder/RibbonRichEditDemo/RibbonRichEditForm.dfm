inherited frmRibbonRichEditForm: TfrmRibbonRichEditForm
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited Ribbon: TdxRibbon [0]
    ApplicationButton.Menu = rbvBackstageView
    Contexts = <
      item
        Caption = 'Selection Tools'
        Color = 13468115
      end
      item
        Caption = 'Header & Footer Tools'
        Color = clGreen
      end
      item
        Caption = 'Table Tools'
        Color = clYellow
      end
      item
        Caption = 'Picture Tools'
        Color = clFuchsia
      end
      item
        Caption = 'Application Options'
        Color = clMaroon
        Visible = True
      end>
    inherited rtFile: TdxRibbonTab
      Index = 0
    end
    inherited tabHome: TdxRibbonTab
      Index = 1
    end
    inherited rtInsert: TdxRibbonTab
      Index = 2
    end
    inherited rtPageLayout: TdxRibbonTab
      Index = 3
    end
    inherited rtReferences: TdxRibbonTab
      Index = 4
    end
    inherited rtMailings: TdxRibbonTab
      Index = 5
    end
    inherited rtReview: TdxRibbonTab
      Index = 6
    end
    inherited rtView: TdxRibbonTab
      Index = 7
    end
    inherited rtHeaderAndFooterTools: TdxRibbonTab
      Index = 8
      ContextIndex = 1
    end
    inherited rtTableToolsLayout: TdxRibbonTab
      Index = 9
      ContextIndex = 2
    end
    inherited rtTableToolsDesign: TdxRibbonTab
      Index = 10
      ContextIndex = 2
    end
    inherited rtPictureTools: TdxRibbonTab
      Index = 11
      ContextIndex = 3
    end
    inherited rtHelp: TdxRibbonTab [12]
      Visible = True
      Index = 12
      ContextIndex = 4
    end
    inherited rtAppearance: TdxRibbonTab [13]
      Visible = True
      Index = 13
    end
  end
  inherited rsbStatusBar: TdxRibbonStatusBar [1]
  end
  object rbvBackstageView: TdxRibbonBackstageView [2]
    Left = 8
    Top = 165
    Width = 841
    Height = 400
    Buttons = <
      item
        Item = bbNew
      end
      item
        Item = bbOptions
        Position = mbpAfterTabs
      end
      item
        Item = bbExit
        Position = mbpAfterTabs
      end>
    Ribbon = Ribbon
    OnPopup = rbvBackstageViewPopup
    OnTabChanged = rbvBackstageViewTabChanged
    object bvtsOpen: TdxRibbonBackstageViewTabSheet
      Left = 132
      Top = 0
      Active = True
      Caption = 'Open'
      SizeOptions.MinWidth = 700
      object bvlSpacer1: TBevel
        Left = 667
        Top = 60
        Width = 42
        Height = 314
        Align = alRight
        Shape = bsSpacer
      end
      object bvSpacer2: TBevel
        Left = 0
        Top = 60
        Width = 42
        Height = 314
        Align = alLeft
        Shape = bsSpacer
      end
      object bvSpacer7: TBevel
        Left = 0
        Top = 374
        Width = 709
        Height = 26
        Align = alBottom
        Shape = bsSpacer
      end
      object gbBackstageViewTabCaption: TcxGroupBox
        Left = 0
        Top = 0
        Align = alTop
        PanelStyle.Active = True
        Style.BorderStyle = ebsNone
        Style.LookAndFeel.NativeStyle = True
        Style.TransparentBorder = False
        StyleDisabled.LookAndFeel.NativeStyle = True
        StyleFocused.LookAndFeel.NativeStyle = True
        StyleHot.LookAndFeel.NativeStyle = True
        TabOrder = 0
        Transparent = True
        Height = 60
        Width = 709
        object bvSpacer4: TBevel
          Left = 667
          Top = 0
          Width = 42
          Height = 60
          Align = alRight
          Shape = bsSpacer
        end
        object bvSpacer3: TBevel
          Left = 0
          Top = 0
          Width = 42
          Height = 60
          Align = alLeft
          Shape = bsSpacer
        end
        object lbbvTabCaption2010: TcxLabel
          Left = 42
          Top = 0
          Align = alClient
          AutoSize = False
          Caption = 'Support'
          ParentFont = False
          Style.Font.Charset = DEFAULT_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -20
          Style.Font.Name = 'Tahoma'
          Style.Font.Style = [fsBold]
          Style.IsFontAssigned = True
          Transparent = True
          Height = 60
          Width = 625
        end
        object lbbvTabCaption2013: TcxLabel
          Left = 42
          Top = 0
          Align = alClient
          Caption = 'Open'
          ParentFont = False
          Style.Font.Charset = DEFAULT_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -35
          Style.Font.Name = 'Tahoma'
          Style.Font.Style = []
          Style.TransparentBorder = False
          Style.IsFontAssigned = True
          Transparent = True
        end
      end
      object gbLocationsMain: TcxGroupBox
        Left = 42
        Top = 60
        Align = alClient
        PanelStyle.Active = True
        Style.BorderStyle = ebsNone
        Style.LookAndFeel.NativeStyle = True
        Style.TransparentBorder = False
        StyleDisabled.LookAndFeel.NativeStyle = True
        StyleFocused.LookAndFeel.NativeStyle = True
        StyleHot.LookAndFeel.NativeStyle = True
        TabOrder = 1
        Transparent = True
        Height = 314
        Width = 625
        object gbLocationsPane: TcxGroupBox
          Left = 0
          Top = 0
          Align = alLeft
          PanelStyle.Active = True
          Style.BorderStyle = ebsNone
          Style.LookAndFeel.NativeStyle = True
          Style.TransparentBorder = False
          StyleDisabled.LookAndFeel.NativeStyle = True
          StyleFocused.LookAndFeel.NativeStyle = True
          StyleHot.LookAndFeel.NativeStyle = True
          TabOrder = 0
          Transparent = True
          Height = 314
          Width = 337
          object bvSpacer5: TBevel
            Left = 305
            Top = 0
            Width = 32
            Height = 314
            Align = alRight
            Shape = bsSpacer
          end
          object dxBevel1: TdxBevel
            Left = 299
            Top = 0
            Width = 6
            Height = 314
            Align = alRight
            AutoSize = True
            Shape = dxbsLineLeft
          end
          object bvgcLocations: TdxRibbonBackstageViewGalleryControl
            Left = 0
            Top = 0
            Width = 299
            Height = 314
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Tahoma'
            Font.Style = []
            BorderStyle = cxcbsNone
            OptionsBehavior.ItemCheckMode = icmSingleRadio
            OptionsView.ColumnAutoWidth = True
            OptionsView.ColumnCount = 1
            OptionsView.ContentOffset.All = 0
            OptionsView.ContentOffsetGroups.All = -1
            OptionsView.ContentOffsetGroups.Left = 0
            OptionsView.ContentOffsetGroups.Top = 0
            OptionsView.ContentOffsetGroups.Right = 0
            OptionsView.ContentOffsetGroups.Bottom = 5
            OptionsView.ContentOffsetItems.All = -1
            OptionsView.ContentOffsetItems.Left = 8
            OptionsView.ContentOffsetItems.Top = 12
            OptionsView.ContentOffsetItems.Right = 8
            OptionsView.ContentOffsetItems.Bottom = 12
            OptionsView.Item.Image.ShowFrame = False
            OptionsView.Item.Text.AlignHorz = taLeftJustify
            OptionsView.Item.Text.AlignVert = vaCenter
            OptionsView.Item.Text.Position = posRight
            Ribbon = Ribbon
            TabOrder = 0
            OnItemClick = bvgcLocationsItemClick
            object bvgcLocationsRecentDocumentsGroup: TdxRibbonBackstageViewGalleryGroup
              ShowCaption = False
              object bvgcLocationsRecentDocumentsItem: TdxRibbonBackstageViewGalleryItem
                Caption = 'Recent Documents'
                ImageIndex = 29
              end
            end
            object bvgcLocationsGroup1: TdxRibbonBackstageViewGalleryGroup
              ShowCaption = False
              object bvgcLocationsComputerItem: TdxRibbonBackstageViewGalleryItem
                Caption = 'Computer'
                ImageIndex = 28
              end
            end
          end
        end
        object gbRecentPathsPane: TcxScrollBox
          Left = 337
          Top = 0
          Width = 288
          Height = 314
          Align = alClient
          BorderStyle = cxcbsNone
          TabOrder = 1
          Transparent = True
          object bvSpacer6: TBevel
            Left = 0
            Top = 28
            Width = 288
            Height = 16
            Align = alTop
            Shape = bsSpacer
          end
          object bvgcRecentPaths: TdxRibbonBackstageViewGalleryControl
            Left = 0
            Top = 151
            Width = 288
            Height = 63
            Align = alTop
            AutoSizeMode = asAutoHeight
            BorderStyle = cxcbsNone
            Images = ilSmallImages
            OptionsBehavior.ItemShowHint = True
            OptionsView.ColumnAutoWidth = True
            OptionsView.ColumnCount = 1
            OptionsView.Item.Image.ShowFrame = False
            OptionsView.Item.Text.AlignHorz = taLeftJustify
            OptionsView.Item.Text.AlignVert = vaCenter
            OptionsView.Item.Text.Position = posRight
            OptionsView.Item.PinMode = bgipmTag
            Ribbon = Ribbon
            TabOrder = 0
            OnItemClick = bvgcRecentPathsItemClick
            object bvgcRecentPathsGroup: TdxRibbonBackstageViewGalleryGroup
              ShowCaption = False
            end
          end
          object gbRecentPathsPaneBottom: TcxGroupBox
            Left = 0
            Top = 214
            Align = alTop
            PanelStyle.Active = True
            Style.BorderStyle = ebsNone
            Style.LookAndFeel.NativeStyle = True
            Style.TransparentBorder = False
            StyleDisabled.LookAndFeel.NativeStyle = True
            StyleFocused.LookAndFeel.NativeStyle = True
            StyleHot.LookAndFeel.NativeStyle = True
            TabOrder = 1
            Transparent = True
            Height = 95
            Width = 288
            object btnBrowsePath: TcxButton
              Left = 0
              Top = 9
              Width = 86
              Height = 86
              Caption = 'Browse'
              OptionsImage.ImageIndex = 1
              OptionsImage.Layout = blGlyphTop
              TabOrder = 0
              OnClick = btnBrowsePathClick
            end
          end
          object gbRecentPathsPaneCurrentFolder: TcxGroupBox
            Left = 0
            Top = 44
            Align = alTop
            PanelStyle.Active = True
            Style.BorderStyle = ebsNone
            Style.LookAndFeel.NativeStyle = True
            Style.TransparentBorder = False
            StyleDisabled.LookAndFeel.NativeStyle = True
            StyleFocused.LookAndFeel.NativeStyle = True
            StyleHot.LookAndFeel.NativeStyle = True
            TabOrder = 2
            Transparent = True
            Height = 85
            Width = 288
            object lbCurrentFolder: TcxLabel
              Left = 0
              Top = 0
              Align = alTop
              Caption = 'Current Folder'
              ParentFont = False
              Style.Font.Charset = DEFAULT_CHARSET
              Style.Font.Color = clWindowText
              Style.Font.Height = -15
              Style.Font.Name = 'Tahoma'
              Style.Font.Style = []
              Style.IsFontAssigned = True
              Transparent = True
            end
            object bvgcCurrentFolder: TdxRibbonBackstageViewGalleryControl
              Left = 0
              Top = 22
              Width = 288
              Height = 63
              Align = alClient
              BorderStyle = cxcbsNone
              OptionsBehavior.ItemShowHint = True
              OptionsView.ColumnAutoWidth = True
              OptionsView.ColumnCount = 1
              OptionsView.Item.Image.ShowFrame = False
              OptionsView.Item.Text.AlignHorz = taLeftJustify
              OptionsView.Item.Text.AlignVert = vaCenter
              OptionsView.Item.Text.Position = posRight
              Ribbon = Ribbon
              TabOrder = 1
              OnItemClick = bvgcRecentPathsItemClick
            end
          end
          object lbComputer: TcxLabel
            Left = 0
            Top = 0
            Align = alTop
            Caption = 'Computer'
            ParentFont = False
            Style.Font.Charset = DEFAULT_CHARSET
            Style.Font.Color = clWindowText
            Style.Font.Height = -20
            Style.Font.Name = 'Tahoma'
            Style.Font.Style = []
            Style.TransparentBorder = True
            Style.IsFontAssigned = True
            Transparent = True
          end
          object lbRecentFolders: TcxLabel
            Left = 0
            Top = 129
            Align = alTop
            Caption = 'Recent Folders'
            ParentFont = False
            Style.Font.Charset = DEFAULT_CHARSET
            Style.Font.Color = clWindowText
            Style.Font.Height = -15
            Style.Font.Name = 'Tahoma'
            Style.Font.Style = []
            Style.IsFontAssigned = True
            Transparent = True
          end
        end
        object gbRecentDocumentsPane: TcxScrollBox
          Left = 337
          Top = 0
          Width = 288
          Height = 314
          Align = alClient
          BorderStyle = cxcbsNone
          TabOrder = 2
          Transparent = True
          object bvSpacer8: TBevel
            Left = 0
            Top = 28
            Width = 288
            Height = 16
            Align = alTop
            Shape = bsSpacer
          end
          object lbRecentDocuments: TcxLabel
            Left = 0
            Top = 0
            Align = alTop
            Caption = 'Recent Documents'
            ParentFont = False
            Style.Font.Charset = DEFAULT_CHARSET
            Style.Font.Color = clWindowText
            Style.Font.Height = -20
            Style.Font.Name = 'Tahoma'
            Style.Font.Style = []
            Style.TransparentBorder = True
            Style.IsFontAssigned = True
            Transparent = True
          end
          object bvgcRecentDocuments: TdxRibbonBackstageViewGalleryControl
            Left = 0
            Top = 44
            Width = 288
            Height = 270
            Align = alClient
            BorderStyle = cxcbsNone
            Images = ilSmallImages
            OptionsBehavior.ItemShowHint = True
            OptionsView.ColumnAutoWidth = True
            OptionsView.ColumnCount = 1
            OptionsView.Item.Image.ShowFrame = False
            OptionsView.Item.Text.AlignHorz = taLeftJustify
            OptionsView.Item.Text.AlignVert = vaCenter
            OptionsView.Item.Text.Position = posRight
            OptionsView.Item.PinMode = bgipmTag
            Ribbon = Ribbon
            TabOrder = 1
            OnItemClick = bvgcRecentDocumentsItemClick
          end
        end
      end
    end
    object bvtsSaveAs: TdxRibbonBackstageViewTabSheet
      Left = 132
      Top = 0
      Caption = 'Save As'
      SizeOptions.MinWidth = 700
      object Bevel1: TBevel
        Left = 0
        Top = 0
        Width = 42
        Height = 374
        Align = alLeft
        Shape = bsSpacer
      end
      object Bevel2: TBevel
        Left = 667
        Top = 0
        Width = 42
        Height = 374
        Align = alRight
        Shape = bsSpacer
      end
      object Bevel3: TBevel
        Left = 0
        Top = 374
        Width = 709
        Height = 26
        Align = alBottom
        Shape = bsSpacer
      end
    end
    object bvtsAbout: TdxRibbonBackstageViewTabSheet
      Left = 132
      Top = 0
      Caption = 'About this Demo'
      object Bevel4: TBevel
        Left = 0
        Top = 0
        Width = 42
        Height = 374
        Align = alLeft
        Shape = bsSpacer
      end
      object Bevel5: TBevel
        Left = 667
        Top = 0
        Width = 42
        Height = 374
        Align = alRight
        Shape = bsSpacer
      end
      object Bevel6: TBevel
        Left = 0
        Top = 374
        Width = 709
        Height = 26
        Align = alBottom
        Shape = bsSpacer
      end
      object meAbout: TcxMemo
        Left = 42
        Top = 0
        Align = alClient
        ParentFont = False
        Properties.ScrollBars = ssVertical
        Style.BorderStyle = ebsNone
        Style.TransparentBorder = False
        StyleFocused.BorderStyle = ebsNone
        StyleHot.BorderStyle = ebsNone
        TabOrder = 0
        Height = 374
        Width = 625
      end
    end
    object bvtsHelp: TdxRibbonBackstageViewTabSheet
      Left = 132
      Top = 0
      Caption = 'Help'
      SizeOptions.MinWidth = 470
      object Bevel7: TBevel
        Left = 0
        Top = 0
        Width = 42
        Height = 374
        Align = alLeft
        Shape = bsSpacer
      end
      object Bevel8: TBevel
        Left = 0
        Top = 374
        Width = 709
        Height = 26
        Align = alBottom
        Shape = bsSpacer
      end
      object Bevel9: TBevel
        Left = 667
        Top = 0
        Width = 42
        Height = 374
        Align = alRight
        Shape = bsSpacer
      end
      object gbHelpContent: TcxGroupBox
        Left = 42
        Top = 0
        Align = alClient
        PanelStyle.Active = True
        Style.BorderStyle = ebsNone
        Style.LookAndFeel.NativeStyle = True
        StyleDisabled.LookAndFeel.NativeStyle = True
        StyleFocused.LookAndFeel.NativeStyle = True
        StyleHot.LookAndFeel.NativeStyle = True
        TabOrder = 0
        Transparent = True
        Height = 374
        Width = 625
        object Image1: TImage
          Left = 0
          Top = 182
          Width = 8
          Height = 7
          Center = True
          Picture.Data = {
            07544269746D6170F6000000424DF60000000000000036000000280000000800
            0000080000000100180000000000C00000000000000000000000000000000000
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            797979414141414141797979FFFFFFFFFFFFFFFFFF7979792927272927272927
            27292727797979FFFFFFFFFFFF474544312E2D292625292625312E2D474544FF
            FFFFFFFFFF4F4E4E4241414241414241414241414F4E4EFFFFFFFFFFFF989898
            444343444343444343444343989898FFFFFFFFFFFFFFFFFF9C9C9C5959595959
            599C9C9CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFF}
          Transparent = True
        end
        object imDot1: TImage
          Left = 0
          Top = 41
          Width = 8
          Height = 7
          Center = True
          Picture.Data = {
            07544269746D6170F6000000424DF60000000000000036000000280000000800
            0000080000000100180000000000C00000000000000000000000000000000000
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            797979414141414141797979FFFFFFFFFFFFFFFFFF7979792927272927272927
            27292727797979FFFFFFFFFFFF474544312E2D292625292625312E2D474544FF
            FFFFFFFFFF4F4E4E4241414241414241414241414F4E4EFFFFFFFFFFFF989898
            444343444343444343444343989898FFFFFFFFFFFFFFFFFF9C9C9C5959595959
            599C9C9CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFF}
          Transparent = True
        end
        object imDot2: TImage
          Left = 0
          Top = 59
          Width = 8
          Height = 8
          Center = True
          Picture.Data = {
            07544269746D6170F6000000424DF60000000000000036000000280000000800
            0000080000000100180000000000C00000000000000000000000000000000000
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            797979414141414141797979FFFFFFFFFFFFFFFFFF7979792927272927272927
            27292727797979FFFFFFFFFFFF474544312E2D292625292625312E2D474544FF
            FFFFFFFFFF4F4E4E4241414241414241414241414F4E4EFFFFFFFFFFFF989898
            444343444343444343444343989898FFFFFFFFFFFFFFFFFF9C9C9C5959595959
            599C9C9CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFF}
          Transparent = True
        end
        object imDot3: TImage
          Left = 0
          Top = 78
          Width = 8
          Height = 7
          Center = True
          Picture.Data = {
            07544269746D6170F6000000424DF60000000000000036000000280000000800
            0000080000000100180000000000C00000000000000000000000000000000000
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            797979414141414141797979FFFFFFFFFFFFFFFFFF7979792927272927272927
            27292727797979FFFFFFFFFFFF474544312E2D292625292625312E2D474544FF
            FFFFFFFFFF4F4E4E4241414241414241414241414F4E4EFFFFFFFFFFFF989898
            444343444343444343444343989898FFFFFFFFFFFFFFFFFF9C9C9C5959595959
            599C9C9CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFF}
          Transparent = True
        end
        object imDot4: TImage
          Left = 0
          Top = 163
          Width = 8
          Height = 8
          Center = True
          Picture.Data = {
            07544269746D6170F6000000424DF60000000000000036000000280000000800
            0000080000000100180000000000C00000000000000000000000000000000000
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            797979414141414141797979FFFFFFFFFFFFFFFFFF7979792927272927272927
            27292727797979FFFFFFFFFFFF474544312E2D292625292625312E2D474544FF
            FFFFFFFFFF4F4E4E4241414241414241414241414F4E4EFFFFFFFFFFFF989898
            444343444343444343444343989898FFFFFFFFFFFFFFFFFF9C9C9C5959595959
            599C9C9CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFF}
          Transparent = True
        end
        object imDot5: TImage
          Left = 0
          Top = 201
          Width = 8
          Height = 7
          Center = True
          Picture.Data = {
            07544269746D6170F6000000424DF60000000000000036000000280000000800
            0000080000000100180000000000C00000000000000000000000000000000000
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            797979414141414141797979FFFFFFFFFFFFFFFFFF7979792927272927272927
            27292727797979FFFFFFFFFFFF474544312E2D292625292625312E2D474544FF
            FFFFFFFFFF4F4E4E4241414241414241414241414F4E4EFFFFFFFFFFFF989898
            444343444343444343444343989898FFFFFFFFFFFFFFFFFF9C9C9C5959595959
            599C9C9CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFF}
          Transparent = True
        end
        object imDot6: TImage
          Left = 0
          Top = 145
          Width = 8
          Height = 7
          Center = True
          Picture.Data = {
            07544269746D6170F6000000424DF60000000000000036000000280000000800
            0000080000000100180000000000C00000000000000000000000000000000000
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            797979414141414141797979FFFFFFFFFFFFFFFFFF7979792927272927272927
            27292727797979FFFFFFFFFFFF474544312E2D292625292625312E2D474544FF
            FFFFFFFFFF4F4E4E4241414241414241414241414F4E4EFFFFFFFFFFFF989898
            444343444343444343444343989898FFFFFFFFFFFFFFFFFF9C9C9C5959595959
            599C9C9CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFF}
          Transparent = True
        end
        object imLogo: TImage
          Left = 419
          Top = 2
          Width = 204
          Height = 370
          Align = alRight
          AutoSize = True
          Picture.Data = {
            0B546478504E47496D61676589504E470D0A1A0A0000000D49484452000000CC
            00000026080600000088FD74160000000467414D410000B18F0BFC6105000000
            1974455874536F6674776172650041646F626520496D616765526561647971C9
            653C00000A1349444154785EED5B3D6B1D4714D54F50CA74224D5A5769ED2285
            712508EEAD26950B77EA8282AB40104EA94E0117492A7509A8511343C0853081
            54212218E42A1F60B01D7F6CCE1966963B77CFEC97DE7BBC67E6C2D1EECCBDF3
            B13BF7CCDC997DDA6A9AA6A2A26224646645458586CCACA8A8D090991515151A
            5B2FBEFCF03AF00BD0AC086CEBBAEA4C45C5BA8384B934CEBC2A5CAACE5454AC
            3B4818E5D04B87EF4845C52660230873EBD6AD6DE046095B1B2678FE6DE0C644
            ECC4E26B2D6AFCDE27AC84302FBFF9A479F5ED67CDEB9FBE685E9F7DDDBCFDE3
            67B4AD3B6401325C039A9138071E006BEF58782724807C573D3888C5D75AD438
            BE4F5838615E7EF571F3DFF77B2D31DEBDF817ED48911DB280F3730551E41802
            89B31DC770ED04EFA9126643B110C2BC3AFA34AC1E6F2F7F459DA34576C8024E
            3F973004579CB5240DDE5925CC8662366118669124EFFEFE13F5CC12D9210B38
            BC22CC1970607002789B84B3388E6B25787F8A3017C0590FEEC4E255268AF2AD
            B9984C18EE45DEFCF623CA8E97274F9E340F1F3E6CEEDFBFDFECEDED056786C8
            0E59C04E11A633D3226F07281167ED1C0DEF5111662356904D14E55B7DC058FC
            1EC7E410781CF37099401812256ED607E5F9F3E7CDE9E969208870E00088ECAC
            05EC46112609748A341751DD2BB04B27714B0FE3F03ED79E30E80FFB78A50314
            94E769E0B5982C4AB4637BB3DE7D2C5B6C47F916ED59AEA0FB083805F6018ECD
            E7BC5247A51FB80C53883244120B48A7A31EB09B4A183AFD3FC63661379A6482
            7CDA33ACBB887609ACE318C81C06E95D8021A185EC0FF2EF383B221C81E3BDCE
            260CEC8E011FAE759E0F79F79C0D11565B5E5DFE8398BF0B9C03B65F0C158B7D
            83CED61342605C4900A68BCF166D1E00FF449B04A6F98CBDE481FE0060DF6C59
            82FDCFDA4BFE847C3EDF49B4B3603D6C7327DA59C2A4D5E62FEAA8F4850378DA
            F5E6FC3BD8F4CBB367CF42B875FBF66DEB70838064E45080DD24C250A0A7A30F
            96411E8FAC15B92CA86F672EDC33F4EBD8447526C82741BC6D7002BCDFAB1046
            95CD5651A4E98CCA1153FB7436ABA373D3616C9E071DB1E3C4CE86E0CCEDDBCE
            9E0D693AAEB7F1A0BEB36A208FCFE649ED918D097D097943CF478415075712E6
            07E008E0EAF238A63F500F1C8E857B8E83839028878787DE214603D2218807EC
            E61086AB802F936DFE911E439604DAB58E827B9EBE799BCEC00A9B93A8E23B9F
            4D180A6CD52CD996E7BDD311ED5E0EF75E3FE4BC09C7B18A56848D9AF56DDF48
            28AF2F81756524455A3DBB47FBAE29487345557619BCFF29640F3C6655E1FEE4
            E8E8C83BC3644064872C603787306A15F084F14E4F523084627B0CD1AC8E68DB
            C4FD3DA723EE457510A41569ADC3CA5502C8C21B8B583408D23B807772A639FB
            AAD5C597E70C6FF516A91F251265BFAC107A054B184F28B64387E63B5144B765
            F9DC5ECFD586CF4330C463FDD978C43C5BC6B6C9D095759C281FF4681F98DF52
            86BEA3708F3235F42A01223B6401BBC984A18832ADC3E09EC4F0FA6C8540DA93
            A65DE2713F8690FC70EA6DDA9912EF5B11A617B1682BC893CE05A8D934DB8B21
            5D6ABF7534DC97429F6C9511FA0496A533D291D3DECDF78D8E9BBD7BA4E9F4D6
            A60D3771AF9E4D85897E55F265EE785F43DEB6CF530895912C7D2118C3AFFDFD
            7DEF0057024476C80276CB208C3F49EB8419C853A4B07B99CE695C540541DAAF
            603E44B8326128C85733A7CFEBBC2FE4A9F633D25390A766F46C7F20F444F6BC
            4998EFEC5488A742B640785C7B43CD928832A35613852D9E82F591E5D1A3470B
            5B552C20B24316B09B1392F1E4CB97B184F17B17AE066CC7C3DA10ED4914EED5
            2A15F4B8AAF6B341C5802D8A3043F5903C6A0656E5B2302609F2D52A63574BAF
            23E471B4B02301D8170F6FD777BA487043DFD94726814E85977CAECE4A3304FE
            29CA22F62A25403A9DF1809D72DC21C2A8328130B82A671E8BB65DDCAB7AC2D1
            2CAE6AFFE2430435F0832BA71294EBDB0467FB8D24CC77767DB63E44CA6C854E
            7EF742FE94CDBE473B59E15E1138817BAF0E7190A756A6044E2ABBCAFF14F8A7
            23DCD82F3A04F380743AE301BB3984519BF2E4CCAABEB1C8DA45DA8765E731DF
            EF5F54D8B148C2941CB1F8C116BA298451CED647984E6847417E697518037B60
            C230D1879D1661431FCD83D097903774AC7CECFD4F817F322159EEDEBD6B077C
            2980743AE301BB398451DF3F52B87415C2641F0791566119571EBF7FC9CA5130
            388B248C5A0112647C8FFC4D234CD637A47920D1470092A60D0B933F218F8710
            2A3C4B180CD1F8A79555918580743AE301BB498481AE4488362412BAD21EC623
            0BAB98067C5DB4B369F9511303B310C2A0CC509833650FD3213605F90C73BC6D
            EB8C4257228C3A4028ED613C3ACF40413EEB641D8A04EDFBB43E857C928DA76D
            6A953AB7B60AFC1364956421209DCE78C0CE3B20211D0BF97460FF1317220B89
            90F69BFE4EC8345650D687657E7593756360E8047EB0E610C63BB372824EBDC8
            53ED87B0D50AF2D4379DA1533249180A74BEAED9EFDE0AEAE1C4E1EB6EFBA17C
            0B7A796CEEED3CF867E564215207FA00BB5184411EEDD4177892233BB1415AFD
            7446CEAE4382722A2CB328CDDA57260CECE5F7164091267B0748ABF6E970FE70
            428563FE88DCEBFB08A30E288AA75B5304F5F8C9A39730046C3ACFE76D3CF867
            E91B7C85D4813EC04E1186B378FA5F18421125A113C3234FD5499048D4F11B0C
            7F3AC37B1E20C86F0A14E8FA4EDD643846C1C02887A5A373D04B685700DCAB99
            3F3808AE8A4863BF03B10F2C4F7D698FE0F7135EDF479852BBDC875147C273B5
            E07DF8F1682C1A24EAF9DCC13E66339FB69D3AA39A7AAE247C1E7E440D1F2871
            651B7E72B9F03EE8B1C51F4E8AC15E3A7C4714605772EE31901B5E0A74EA4B7C
            1FE48698029DFA9702A2186E60604A8ED387D679703F74D4AB5619AB9FD33ED1
            21C3181B2BD08FF92D9845BB02E15E4D062584E7C595938BD22B1C283FB4D85A
            C647C931F01D5180DD1CC2701F5374F024B02939BA82FCA84781AE149615C33C
            0CCC6CC2E0AA36FA7E26568E657F62A2DAF72B960767697580E0ED86084307E6
            8AE9CB95608F94878E8613ECEA329664E1F9941F5A70C0D5602F1DBE230AB01B
            4B18EE5518524DDA8BC09ECEAE0E0A2CA82FD60B9D0ACB8AE11805037315C228
            67EB4C10C853AB4C203EAEAA7D3A56E9A3201DB57452E56D7B099304760CA354
            1F2DA86FDF3DEE878E85699F451648F359FBDA4921DEB8DF9289C15E097C474A
            806DDA4F24705F41474F69F9338C29823AD806EBB47BA3D175C3CEF7B17723CB
            C10138905310EA14F97235453EE37D6F6BEBF08E934218EAB8192648A2DE7710
            ED2D266DE2690FB09DD426C17A8AED42E7CB9048C576E947D0F37D700F93B5E3
            FD6D081CECA78074EA25E2A9EA4C0955162B74144012E67D14E5537341C2DC04
            2E8D332F1B6CEBA6EA4C0955162B2487234B25CC48C8CC754395C50AC9E1C852
            0933123273DD5065B1427238B254C28C84CC5C375459AC901C8E2C9530232133
            D70D55162B20074F99ECAF088849A75B9B24CAA7E6A1D9FA1FC90723B9691093
            440000000049454E44AE426082}
          Transparent = True
        end
        object lblSupport: TcxLabel
          Left = 0
          Top = 7
          Caption = 'Support'
          ParentFont = False
          Style.Font.Charset = DEFAULT_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -15
          Style.Font.Name = 'Tahoma'
          Style.Font.Style = []
          Style.IsFontAssigned = True
          Transparent = True
        end
        object cxLabel1: TcxLabel
          Left = 0
          Top = 111
          Caption = 'Links'
          ParentFont = False
          Style.Font.Charset = DEFAULT_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -15
          Style.Font.Name = 'Tahoma'
          Style.Font.Style = []
          Style.IsFontAssigned = True
          Transparent = True
        end
        object lblClientCenter: TcxLabel
          Left = 13
          Top = 197
          Cursor = crHandPoint
          Caption = 'DevExpress Client Center'
          ParentFont = False
          Style.Font.Charset = DEFAULT_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -11
          Style.Font.Name = 'Tahoma'
          Style.Font.Style = [fsUnderline]
          Style.TransparentBorder = False
          Style.IsFontAssigned = True
          Transparent = True
        end
        object lblDownloads: TcxLabel
          Left = 13
          Top = 178
          Cursor = crHandPoint
          Caption = 'DevExpress Downloads'
          ParentFont = False
          Style.Font.Charset = DEFAULT_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -11
          Style.Font.Name = 'Tahoma'
          Style.Font.Style = [fsUnderline]
          Style.TransparentBorder = False
          Style.IsFontAssigned = True
          Transparent = True
        end
        object lblDXonWeb: TcxLabel
          Left = 13
          Top = 141
          Cursor = crHandPoint
          Caption = 'DevExpress on the WEB'
          ParentFont = False
          Style.Font.Charset = DEFAULT_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -11
          Style.Font.Name = 'Tahoma'
          Style.Font.Style = [fsUnderline]
          Style.TransparentBorder = False
          Style.IsFontAssigned = True
          Transparent = True
        end
        object lblHelpBars: TcxLabel
          Left = 13
          Top = 37
          Cursor = crHandPoint
          Caption = 'ExpressBars Help'
          ParentFont = False
          Style.Font.Charset = DEFAULT_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -11
          Style.Font.Name = 'Tahoma'
          Style.Font.Style = [fsUnderline]
          Style.TransparentBorder = False
          Style.IsFontAssigned = True
          Transparent = True
        end
        object lblHelpDocking: TcxLabel
          Left = 13
          Top = 56
          Cursor = crHandPoint
          Caption = 'ExpressDocking Library Help'
          ParentFont = False
          Style.Font.Charset = DEFAULT_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -11
          Style.Font.Name = 'Tahoma'
          Style.Font.Style = [fsUnderline]
          Style.TransparentBorder = False
          Style.IsFontAssigned = True
          Transparent = True
        end
        object lblProducts: TcxLabel
          Left = 13
          Top = 160
          Cursor = crHandPoint
          Caption = 'DevExpress Products'
          ParentFont = False
          Style.Font.Charset = DEFAULT_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -11
          Style.Font.Name = 'Tahoma'
          Style.Font.Style = [fsUnderline]
          Style.TransparentBorder = False
          Style.IsFontAssigned = True
          Transparent = True
        end
        object lblSupportCenter: TcxLabel
          Left = 13
          Top = 74
          Cursor = crHandPoint
          Caption = 'DevExpress Support Center'
          ParentFont = False
          Style.Font.Charset = DEFAULT_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -11
          Style.Font.Name = 'Tahoma'
          Style.Font.Style = [fsUnderline]
          Style.TransparentBorder = False
          Style.IsFontAssigned = True
          Transparent = True
        end
      end
    end
  end
  inherited tbZoom: TdxZoomTrackBar [3]
  end
  inherited RichEditControl: TdxRichEditControl [4]
    OnDocumentLoaded = RichEditControlDocumentLoaded
  end
  inherited bmBarManager: TdxBarManager
    DockControlHeights = (
      0
      0
      0
      0)
    inherited dxbStatusBarToolbar2: TdxBar
      FloatClientHeight = 216
    end
    inherited dxbHelp: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited dxbLinks: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bmbFileCommon: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bmbInsertPages: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bmbInsertTables: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bmbInserIllustrations: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bmbInsertLinks: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bmbInsertHeaderAndFooter: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bmbInsertText: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bmbInsertSymbols: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bmbPageLayoutPageSetup: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bmbPageLayoutPageBackground: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bmbReferencesTableOfContents: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bmbReferencesCaptions: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bmbMailingsMailMerge: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bmbReviewProofing: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bmbReviewProtect: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bmbViewDocumentViews: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bmbViewShow: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bmbViewZoom: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bmbHFTNavigation: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bmbHFTOptions: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bmbHFTClose: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bmbTableToolsTable: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bmbTableToolsRowsAndColumns: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bmbTableToolsMerge: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bmbTableToolsCellSize: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bmbTableToolsTableStyleOptions: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bmbTableToolsTableStyles: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bmbTableToolsBordersShadings: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bmbPictureToolsShapeStyles: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bmbPictureToolsArrange: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bmbTableToolsAlignment: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bmbPrint: TdxBar
      DockedDockingStyle = dsNone
    end
    inherited bbRibbonForm: TdxBarLargeButton
      ImageIndex = 12
    end
    inherited bbApplicationButton: TdxBarLargeButton
      ImageIndex = 13
    end
    inherited bbQATVisible: TdxBarLargeButton
      ImageIndex = 14
    end
    inherited bbOptions: TdxBarButton
      OnClick = bbOptionsClick
    end
    inherited bbDXOnWeb: TdxBarLargeButton
      ImageIndex = 2
    end
    inherited bbFontColor: TdxBarLargeButton
      ImageIndex = -1
    end
  end
  inherited PrinterEngine: TdxPSEngineController
    Active = True
  end
  inherited Printer: TdxComponentPrinter
    inherited RichEditPrinterLink: TdxRichEditControlReportLink
      BuiltInReportLink = True
    end
  end
end
