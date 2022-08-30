object dxFlowChartDesigner: TdxFlowChartDesigner
  Left = 0
  Top = 0
  Caption = 'dxFlowChart Editor'
  ClientHeight = 840
  ClientWidth = 1276
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 600
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object rRibbon: TdxRibbon
    Left = 0
    Top = 0
    Width = 1276
    Height = 165
    ApplicationButton.Menu = bamRibbonApplicationButtonMenu
    BarManager = bmManager
    Style = rs2019
    ColorSchemeName = 'White'
    QuickAccessToolbar.Toolbar = rbQuickAccessUndoRedo
    SupportNonClientDrawing = True
    Contexts = <>
    TabOrder = 0
    TabStop = False
    object rtHome: TdxRibbonTab
      Active = True
      Caption = 'Home'
      Groups = <
        item
          Restriction = rtgrNoCollapse
          CanCollapse = False
          ToolbarName = 'rbHomeClipboard'
        end
        item
          Restriction = rtgrNoCollapse
          CanCollapse = False
          ToolbarName = 'rbHomeEdititng'
        end
        item
          ToolbarName = 'rbHomeFont'
        end
        item
          ToolbarName = 'rbHomeParagraph'
        end
        item
          ToolbarName = 'rbHomeTools'
        end
        item
          ToolbarName = 'rbHomeShapeStyles'
        end
        item
          ToolbarName = 'rbHomeArrange'
        end
        item
          ToolbarName = 'rbHomeDesigner'
        end>
      Index = 0
    end
    object rtDesign: TdxRibbonTab
      Caption = 'Design'
      Groups = <
        item
          ToolbarName = 'bDesignOptions'
        end
        item
          ToolbarName = 'bLayout'
        end>
      Index = 1
    end
    object rtView: TdxRibbonTab
      Caption = 'View'
      Groups = <
        item
          ToolbarName = 'rbViewGridLines'
        end
        item
          ToolbarName = 'rbViewAntialiasing'
        end
        item
          ToolbarName = 'rbViewZoom'
        end>
      Index = 2
    end
  end
  object dsShapes: TdxDockSite
    Left = 0
    Top = 165
    Width = 357
    Height = 675
    Align = alLeft
    AutoSize = True
    DockingType = 5
    OriginalWidth = 290
    OriginalHeight = 504
    object dxLayoutDockSite1: TdxLayoutDockSite
      Left = 0
      Top = 0
      Width = 357
      Height = 675
      DockingType = 0
      OriginalWidth = 357
      OriginalHeight = 200
    end
    object dpShapes: TdxDockPanel
      Left = 0
      Top = 0
      Width = 357
      Height = 675
      AllowFloating = False
      AutoHide = False
      Caption = 'Shapes'
      CaptionButtons = [cbMaximize, cbHide]
      CustomCaptionButtons.Buttons = <>
      TabsProperties.CustomButtons.Buttons = <>
      DockingType = 0
      OriginalWidth = 357
      OriginalHeight = 140
      object gcShapes: TdxGalleryControl
        Left = 0
        Top = 54
        Width = 353
        Height = 597
        Align = alClient
        BorderStyle = cxcbsNone
        OptionsBehavior.ItemCheckMode = icmSingleRadio
        OptionsView.ColumnAutoWidth = True
        OptionsView.Item.Image.ShowFrame = False
        OptionsView.Item.Image.Size.Height = 34
        OptionsView.Item.Image.Size.Width = 34
        OptionsView.Item.Text.AlignHorz = taLeftJustify
        OptionsView.Item.Text.AlignVert = vaCenter
        OptionsView.Item.Text.Position = posRight
        TabOrder = 0
        object gcgShapes: TdxGalleryControlGroup
          Caption = 'Shapes'
          ShowCaption = False
        end
      end
      object gcStencils: TdxGalleryControl
        Left = 0
        Top = 27
        Width = 353
        Height = 27
        Align = alTop
        AutoSizeMode = asAutoHeight
        BorderStyle = cxcbsNone
        OptionsBehavior.ItemCheckMode = icmSingleRadio
        OptionsView.ColumnAutoWidth = True
        OptionsView.ColumnCount = 1
        OptionsView.Item.Image.ShowFrame = False
        OptionsView.Item.Text.AlignHorz = taLeftJustify
        OptionsView.Item.Text.AlignVert = vaCenter
        OptionsView.Item.Text.Position = posLeft
        TabOrder = 1
        OnItemClick = gcStencilsItemClick
        object gcgStencils: TdxGalleryControlGroup
          ShowCaption = False
        end
      end
      object gcMoreShapes: TdxGalleryControl
        Left = 0
        Top = 0
        Width = 353
        Height = 27
        Align = alTop
        AutoSizeMode = asAutoHeight
        BorderStyle = cxcbsNone
        OptionsView.ColumnAutoWidth = True
        OptionsView.Item.Image.ShowFrame = False
        OptionsView.Item.Text.AlignHorz = taLeftJustify
        OptionsView.Item.Text.AlignVert = vaCenter
        OptionsView.Item.Text.Position = posLeft
        TabOrder = 2
        OnItemClick = gcMoreShapesItemClick
        OnMouseMove = gcMoreShapesMouseMove
        object gcgMoreShapes: TdxGalleryControlGroup
          ShowCaption = False
          object gciMoreShapes: TdxGalleryControlItem
            Caption = 'More Shapes'
            ActionIndex = nil
          end
        end
      end
      object imgMoreShapesArrow: TcxImage
        Left = 74
        Top = 7
        TabStop = False
        Picture.Data = {
          0D546478536D617274496D6167653C3F786D6C2076657273696F6E3D22312E30
          2220656E636F64696E673D225554462D38223F3E0D0A3C737667207665727369
          6F6E3D22312E31222069643D22D0A1D0BBD0BED0B95F312220786D6C6E733D22
          687474703A2F2F7777772E77332E6F72672F323030302F7376672220786D6C6E
          733A786C696E6B3D22687474703A2F2F7777772E77332E6F72672F313939392F
          786C696E6B2220783D223070782220793D22307078222076696577426F783D22
          30203020333220333222207374796C653D22656E61626C652D6261636B67726F
          756E643A6E6577203020302033322033323B2220786D6C3A73706163653D2270
          72657365727665223E262331333B262331303B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B3C
          7374796C6520747970653D22746578742F637373223E2E426C75657B66696C6C
          3A233131373744373B7D3C2F7374796C653E0D0A3C672069643D22D0A1D0BBD0
          BED0B95F32223E0D0A09093C706F6C79676F6E20636C6173733D22426C756522
          20706F696E74733D2231302C362032322C31362031302C3236202623393B222F
          3E0D0A093C2F673E0D0A3C2F7376673E0D0A}
        Properties.GraphicClassName = 'TdxSmartImage'
        Properties.GraphicTransparency = gtTransparent
        Properties.PopupMenuLayout.MenuItems = []
        Properties.ReadOnly = True
        Properties.ShowFocusRect = False
        Style.BorderStyle = ebsNone
        Style.HotTrack = False
        Style.TransparentBorder = False
        TabOrder = 3
        Transparent = True
        Visible = False
        Height = 13
        Width = 13
      end
    end
  end
  object dsProperties: TdxDockSite
    Left = 998
    Top = 165
    Width = 278
    Height = 675
    Align = alRight
    AutoSize = True
    DockingType = 5
    OriginalWidth = 300
    OriginalHeight = 504
    object dxLayoutDockSite2: TdxLayoutDockSite
      Left = 0
      Top = 0
      Width = 278
      Height = 675
      DockingType = 0
      OriginalWidth = 278
      OriginalHeight = 200
    end
    object dpProperties: TdxDockPanel
      Left = 0
      Top = 0
      Width = 278
      Height = 675
      AllowDock = []
      AllowDockClients = []
      AllowFloating = False
      AutoHide = False
      Caption = 'Properties'
      CaptionButtons = [cbMaximize, cbHide]
      CustomCaptionButtons.Buttons = <>
      TabsProperties.CustomButtons.Buttons = <>
      TabsProperties.NavigatorPosition = npLeftTop
      DockingType = 0
      OriginalWidth = 278
      OriginalHeight = 140
      object lcProperties: TdxLayoutControl
        Left = 0
        Top = 0
        Width = 274
        Height = 651
        Align = alClient
        TabOrder = 0
        LayoutLookAndFeel = dxLayoutCxLookAndFeel1
        object icbEndArrow: TcxImageComboBox
          Left = 67
          Top = 482
          AutoSize = False
          Properties.Images = ilEndArrowTypes
          Properties.Items = <>
          Properties.OnEditValueChanged = ConnectionPropertiesChanged
          Style.HotTrack = False
          Style.TransparentBorder = False
          TabOrder = 14
          Height = 23
          Width = 173
        end
        object seLineThickness: TcxSpinEdit
          Left = 180
          Top = 91
          AutoSize = False
          Properties.ImmediatePost = True
          Properties.MaxValue = 50.000000000000000000
          Properties.MinValue = 1.000000000000000000
          Properties.OnEditValueChanged = seLineThicknessPropertiesEditValueChanged
          Style.HotTrack = False
          Style.TransparentBorder = False
          TabOrder = 2
          Value = 1
          Height = 23
          Width = 60
        end
        object seDArrowWidth: TcxSpinEdit
          Left = 67
          Top = 540
          AutoSize = False
          Properties.ImmediatePost = True
          Properties.MinValue = 1.000000000000000000
          Properties.UseNullString = True
          Properties.OnEditValueChanged = DestArrowDimensionChanged
          Style.HotTrack = False
          Style.TransparentBorder = False
          TabOrder = 16
          Value = 1
          Height = 23
          Width = 65
        end
        object seSArrowWidth: TcxSpinEdit
          Left = 67
          Top = 392
          AutoSize = False
          Properties.ImmediatePost = True
          Properties.MinValue = 1.000000000000000000
          Properties.UseNullString = True
          Properties.OnEditValueChanged = SourceArrowDimensionChanged
          Style.HotTrack = False
          Style.TransparentBorder = False
          TabOrder = 11
          Value = 1
          Height = 23
          Width = 65
        end
        object icbBeginArrow: TcxImageComboBox
          Left = 67
          Top = 334
          AutoSize = False
          Properties.Images = ilBeginArrowTypes
          Properties.Items = <>
          Properties.OnEditValueChanged = ConnectionPropertiesChanged
          Style.HotTrack = False
          Style.TransparentBorder = False
          TabOrder = 9
          Height = 23
          Width = 173
        end
        object seSArrowHeight: TcxSpinEdit
          Left = 174
          Top = 392
          AutoSize = False
          Properties.ImmediatePost = True
          Properties.MinValue = 1.000000000000000000
          Properties.UseNullString = True
          Properties.OnEditValueChanged = SourceArrowDimensionChanged
          Style.HotTrack = False
          Style.TransparentBorder = False
          TabOrder = 12
          Value = 1
          Height = 23
          Width = 66
        end
        object seDArrowHeight: TcxSpinEdit
          Left = 174
          Top = 540
          AutoSize = False
          Properties.ImmediatePost = True
          Properties.MinValue = 1.000000000000000000
          Properties.UseNullString = True
          Properties.OnEditValueChanged = DestArrowDimensionChanged
          Style.HotTrack = False
          Style.TransparentBorder = False
          TabOrder = 17
          Value = 1
          Height = 23
          Width = 66
        end
        object cbSArrowSize: TcxComboBox
          Left = 67
          Top = 363
          AutoSize = False
          Properties.DropDownListStyle = lsFixedList
          Properties.OnEditValueChanged = cbSArrowSizePropertiesEditValueChanged
          Style.HotTrack = False
          Style.TransparentBorder = False
          TabOrder = 10
          Height = 23
          Width = 173
        end
        object cbDArrowSize: TcxComboBox
          Left = 67
          Top = 511
          AutoSize = False
          Properties.DropDownListStyle = lsFixedList
          Properties.OnEditValueChanged = cbDArrowSizePropertiesEditValueChanged
          Style.HotTrack = False
          Style.TransparentBorder = False
          TabOrder = 15
          Height = 23
          Width = 173
        end
        object btnLineColor: TcxButton
          Left = 64
          Top = 91
          Width = 25
          Height = 25
          DropDownMenu = ppmLineColor
          Kind = cxbkDropDown
          OptionsImage.ImageIndex = 46
          OptionsImage.Images = ilSmallIcons
          SpeedButtonOptions.Transparent = True
          TabOrder = 1
        end
        object btnSourceArrowColor: TcxButton
          Left = 67
          Top = 421
          Width = 25
          Height = 25
          DropDownMenu = ppmSourceArrowColor
          Kind = cxbkDropDown
          OptionsImage.ImageIndex = 47
          OptionsImage.Images = ilSmallIcons
          SpeedButtonOptions.Transparent = True
          TabOrder = 13
        end
        object btnDestArrowColor: TcxButton
          Left = 67
          Top = 569
          Width = 25
          Height = 25
          DropDownMenu = ppmDestArrowColor
          Kind = cxbkDropDown
          OptionsImage.ImageIndex = 47
          OptionsImage.Images = ilSmallIcons
          SpeedButtonOptions.Transparent = True
          TabOrder = 18
        end
        object btnObjectBkColor: TcxButton
          Left = 55
          Top = 195
          Width = 25
          Height = 25
          DropDownMenu = ppmObjectBkColor
          Kind = cxbkDropDown
          OptionsImage.ImageIndex = 47
          OptionsImage.Images = ilSmallIcons
          SpeedButtonOptions.Transparent = True
          TabOrder = 4
        end
        object mText: TcxMemo
          Left = 55
          Top = 30
          Properties.WantReturns = False
          Properties.OnEditValueChanged = mTextPropertiesEditValueChanged
          Style.HotTrack = False
          Style.TransparentBorder = False
          TabOrder = 0
          OnKeyPress = mTextKeyPress
          Height = 37
          Width = 197
        end
        object seShapeHeight: TcxSpinEdit
          Left = 192
          Top = 226
          AutoSize = False
          Properties.ImmediatePost = True
          Properties.MinValue = 1.000000000000000000
          Properties.UseNullString = True
          Properties.OnEditValueChanged = seShapeHeightPropertiesEditValueChanged
          Style.HotTrack = False
          Style.TransparentBorder = False
          TabOrder = 7
          Value = 1
          Height = 23
          Width = 60
        end
        object seAngle: TcxSpinEdit
          Left = 192
          Top = 195
          AutoSize = False
          Properties.ImmediatePost = True
          Properties.MaxValue = 359.000000000000000000
          Properties.MinValue = -359.000000000000000000
          Properties.UseNullString = True
          Properties.OnEditValueChanged = seAnglePropertiesEditValueChanged
          Style.HotTrack = False
          Style.TransparentBorder = False
          TabOrder = 5
          Height = 23
          Width = 60
        end
        object seShapeWidth: TcxSpinEdit
          Left = 55
          Top = 226
          AutoSize = False
          Properties.ImmediatePost = True
          Properties.MinValue = 1.000000000000000000
          Properties.UseNullString = True
          Properties.OnEditValueChanged = seShapeWidthPropertiesEditValueChanged
          Style.HotTrack = False
          Style.TransparentBorder = False
          TabOrder = 6
          Value = 1
          Height = 23
          Width = 60
        end
        object icbConnectionType: TcxImageComboBox
          Left = 55
          Top = 287
          AutoSize = False
          Properties.Images = ilSmallIcons
          Properties.Items = <>
          Properties.OnEditValueChanged = ConnectionPropertiesChanged
          Style.HotTrack = False
          Style.TransparentBorder = False
          TabOrder = 8
          Height = 23
          Width = 197
        end
        object icbLineStyle: TcxImageComboBox
          Left = 67
          Top = 122
          AutoSize = False
          Properties.Images = ilLineStyles
          Properties.Items = <>
          Properties.OnEditValueChanged = icbLineStylePropertiesEditValueChanged
          Style.HotTrack = False
          Style.TransparentBorder = False
          TabOrder = 3
          Height = 23
          Width = 173
        end
        object lcPropertiesGroup_Root: TdxLayoutGroup
          AlignHorz = ahClient
          AlignVert = avTop
          SizeOptions.AssignedValues = [sovSizableHorz]
          SizeOptions.SizableHorz = False
          ButtonOptions.Buttons = <>
          Hidden = True
          ItemIndex = 1
          ShowBorder = False
          Index = -1
        end
        object lgConnections: TdxLayoutGroup
          Parent = lcPropertiesGroup_Root
          CaptionOptions.Text = 'Connection'
          AllowRemove = False
          ButtonOptions.Buttons = <>
          ButtonOptions.ShowExpandButton = True
          ItemIndex = 1
          Index = 2
        end
        object dxLayoutGroup2: TdxLayoutGroup
          Parent = lgConnections
          AlignHorz = ahLeft
          AlignVert = avTop
          CaptionOptions.Text = 'Hidden Group'
          SizeOptions.AssignedValues = [sovSizableHorz]
          SizeOptions.SizableHorz = True
          SizeOptions.Width = 139
          ButtonOptions.Buttons = <>
          Hidden = True
          LayoutDirection = ldHorizontal
          ShowBorder = False
          Index = 0
        end
        object dxLayoutGroup3: TdxLayoutGroup
          Parent = lgConnections
          AlignVert = avTop
          CaptionOptions.Text = 'Hidden Group'
          ButtonOptions.Buttons = <>
          Hidden = True
          ShowBorder = False
          Index = 1
        end
        object lgSource: TdxLayoutGroup
          Parent = dxLayoutGroup3
          AlignHorz = ahClient
          AlignVert = avTop
          CaptionOptions.Text = ' Source Arrow'
          SizeOptions.AssignedValues = [sovSizableHorz]
          SizeOptions.SizableHorz = True
          SizeOptions.Width = 229
          ButtonOptions.Buttons = <>
          ItemIndex = 3
          Index = 1
        end
        object lgTarget: TdxLayoutGroup
          Parent = dxLayoutGroup3
          AlignHorz = ahClient
          AlignVert = avTop
          CaptionOptions.Text = ' Destination Arrow'
          SizeOptions.AssignedValues = [sovSizableHorz]
          SizeOptions.SizableHorz = True
          SizeOptions.Width = 174
          AllowRemove = False
          ButtonOptions.Buttons = <>
          ItemIndex = 3
          Index = 2
        end
        object liStrokeThickness: TdxLayoutItem
          Parent = dxLayoutAutoCreatedGroup2
          AlignHorz = ahRight
          AlignVert = avTop
          CaptionOptions.AlignHorz = taRightJustify
          CaptionOptions.Text = 'Thickness'
          CaptionOptions.Width = 60
          Control = seLineThickness
          ControlOptions.AutoControlAreaAlignment = False
          ControlOptions.OriginalHeight = 23
          ControlOptions.OriginalWidth = 60
          ControlOptions.ShowBorder = False
          Index = 1
        end
        object liDArrowSize: TdxLayoutItem
          Parent = dxLayoutAutoCreatedGroup5
          AlignHorz = ahClient
          AlignVert = avTop
          CaptionOptions.Text = 'Width'
          SizeOptions.AssignedValues = [sovSizableHorz]
          SizeOptions.SizableHorz = True
          SizeOptions.Width = 83
          Control = seDArrowWidth
          ControlOptions.OriginalHeight = 23
          ControlOptions.OriginalWidth = 50
          ControlOptions.ShowBorder = False
          Index = 0
        end
        object liSArrowWidth: TdxLayoutItem
          Parent = dxLayoutAutoCreatedGroup1
          AlignHorz = ahClient
          AlignVert = avTop
          CaptionOptions.Text = 'Width'
          SizeOptions.AssignedValues = [sovSizableHorz]
          SizeOptions.SizableHorz = True
          SizeOptions.Width = 83
          Control = seSArrowWidth
          ControlOptions.OriginalHeight = 23
          ControlOptions.OriginalWidth = 50
          ControlOptions.ShowBorder = False
          Index = 0
        end
        object liSArrowStyle: TdxLayoutItem
          Parent = lgSource
          AlignHorz = ahClient
          AlignVert = avTop
          CaptionOptions.Text = 'Style'
          Control = icbBeginArrow
          ControlOptions.OriginalHeight = 23
          ControlOptions.OriginalWidth = 48
          ControlOptions.ShowBorder = False
          Index = 0
        end
        object dxLayoutItem1: TdxLayoutItem
          Parent = lgTarget
          AlignHorz = ahClient
          AlignVert = avTop
          CaptionOptions.Text = 'Style'
          SizeOptions.AssignedValues = [sovSizableHorz]
          SizeOptions.SizableHorz = True
          Control = icbEndArrow
          ControlOptions.OriginalHeight = 23
          ControlOptions.OriginalWidth = 172
          ControlOptions.ShowBorder = False
          Index = 0
        end
        object lgObjects: TdxLayoutGroup
          Parent = lcPropertiesGroup_Root
          CaptionOptions.Text = 'Shape'
          ButtonOptions.Buttons = <>
          ButtonOptions.ShowExpandButton = True
          ItemIndex = 1
          Index = 1
        end
        object liSArrowHeight: TdxLayoutItem
          Parent = dxLayoutAutoCreatedGroup1
          AlignHorz = ahClient
          AlignVert = avTop
          CaptionOptions.Text = 'Height'
          Control = seSArrowHeight
          ControlOptions.OriginalHeight = 23
          ControlOptions.OriginalWidth = 50
          ControlOptions.ShowBorder = False
          Index = 1
        end
        object dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
          Parent = lgSource
          AlignVert = avTop
          LayoutDirection = ldHorizontal
          Index = 2
        end
        object dxLayoutItem3: TdxLayoutItem
          Parent = dxLayoutAutoCreatedGroup5
          AlignHorz = ahClient
          AlignVert = avTop
          CaptionOptions.Text = 'Height'
          Control = seDArrowHeight
          ControlOptions.OriginalHeight = 23
          ControlOptions.OriginalWidth = 50
          ControlOptions.ShowBorder = False
          Index = 1
        end
        object liSArrowSize: TdxLayoutItem
          Parent = lgSource
          AlignHorz = ahClient
          AlignVert = avTop
          CaptionOptions.Text = 'Size'
          Control = cbSArrowSize
          ControlOptions.OriginalHeight = 23
          ControlOptions.OriginalWidth = 121
          ControlOptions.ShowBorder = False
          Index = 1
        end
        object dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup
          Parent = lgTarget
          LayoutDirection = ldHorizontal
          Index = 2
        end
        object dxLayoutItem4: TdxLayoutItem
          Parent = lgTarget
          AlignHorz = ahClient
          AlignVert = avTop
          CaptionOptions.Text = 'Size'
          SizeOptions.AssignedValues = [sovSizableHorz]
          SizeOptions.SizableHorz = True
          Control = cbDArrowSize
          ControlOptions.OriginalHeight = 23
          ControlOptions.OriginalWidth = 172
          ControlOptions.ShowBorder = False
          Index = 1
        end
        object dxLayoutItem5: TdxLayoutItem
          Parent = dxLayoutAutoCreatedGroup2
          AlignVert = avTop
          CaptionOptions.Text = 'Color'
          Control = btnLineColor
          ControlOptions.AutoControlAreaAlignment = False
          ControlOptions.OriginalHeight = 25
          ControlOptions.OriginalWidth = 25
          ControlOptions.ShowBorder = False
          Index = 0
        end
        object dxLayoutItem6: TdxLayoutItem
          Parent = lgSource
          AlignHorz = ahLeft
          AlignVert = avTop
          CaptionOptions.Text = 'Fill'
          Control = btnSourceArrowColor
          ControlOptions.OriginalHeight = 25
          ControlOptions.OriginalWidth = 25
          ControlOptions.ShowBorder = False
          Index = 3
        end
        object dxLayoutItem7: TdxLayoutItem
          Parent = lgTarget
          AlignHorz = ahLeft
          AlignVert = avTop
          CaptionOptions.Text = 'Fill'
          Control = btnDestArrowColor
          ControlOptions.OriginalHeight = 25
          ControlOptions.OriginalWidth = 25
          ControlOptions.ShowBorder = False
          Index = 3
        end
        object dxLayoutItem8: TdxLayoutItem
          Parent = dxLayoutAutoCreatedGroup3
          AlignHorz = ahLeft
          AlignVert = avTop
          CaptionOptions.Text = 'Fill'
          Control = btnObjectBkColor
          ControlOptions.OriginalHeight = 25
          ControlOptions.OriginalWidth = 25
          ControlOptions.ShowBorder = False
          Index = 0
        end
        object lgLine: TdxLayoutGroup
          Parent = lgCommon
          CaptionOptions.Text = 'Line'
          ButtonOptions.Buttons = <>
          ItemIndex = 1
          Index = 1
        end
        object dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup
          Parent = lgLine
          AlignVert = avTop
          LayoutDirection = ldHorizontal
          Index = 0
        end
        object dxLayoutItem2: TdxLayoutItem
          Parent = lgCommon
          CaptionOptions.Text = 'Text'
          Control = mText
          ControlOptions.OriginalHeight = 37
          ControlOptions.OriginalWidth = 256
          ControlOptions.ShowBorder = False
          Index = 0
        end
        object dxLayoutItem9: TdxLayoutItem
          Parent = dxLayoutAutoCreatedGroup6
          AlignHorz = ahRight
          AlignVert = avTop
          CaptionOptions.Text = 'Height'
          Control = seShapeHeight
          ControlOptions.OriginalHeight = 23
          ControlOptions.OriginalWidth = 60
          ControlOptions.ShowBorder = False
          Index = 1
        end
        object dxLayoutItem10: TdxLayoutItem
          Parent = dxLayoutAutoCreatedGroup3
          AlignHorz = ahRight
          AlignVert = avTop
          CaptionOptions.Text = 'Angle'
          Control = seAngle
          ControlOptions.OriginalHeight = 23
          ControlOptions.OriginalWidth = 60
          ControlOptions.ShowBorder = False
          Index = 1
        end
        object dxLayoutItem11: TdxLayoutItem
          Parent = dxLayoutAutoCreatedGroup6
          AlignHorz = ahLeft
          CaptionOptions.Text = 'Width'
          Control = seShapeWidth
          ControlOptions.OriginalHeight = 23
          ControlOptions.OriginalWidth = 60
          ControlOptions.ShowBorder = False
          Index = 0
        end
        object dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup
          Parent = lgObjects
          AlignHorz = ahClient
          AlignVert = avTop
          LayoutDirection = ldHorizontal
          Index = 1
        end
        object dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup
          Parent = lgObjects
          AlignHorz = ahClient
          AlignVert = avTop
          LayoutDirection = ldHorizontal
          Index = 0
        end
        object lgCommon: TdxLayoutGroup
          Parent = lcPropertiesGroup_Root
          AlignHorz = ahClient
          AlignVert = avTop
          CaptionOptions.Text = 'Common'
          ButtonOptions.Buttons = <>
          ButtonOptions.ShowExpandButton = True
          ItemIndex = 1
          Index = 0
        end
        object liConnectionType: TdxLayoutItem
          Parent = dxLayoutGroup3
          AlignHorz = ahClient
          AlignVert = avTop
          CaptionOptions.Text = 'Type'
          Control = icbConnectionType
          ControlOptions.OriginalHeight = 23
          ControlOptions.OriginalWidth = 121
          ControlOptions.ShowBorder = False
          Index = 0
        end
        object liLineStyle: TdxLayoutItem
          Parent = lgLine
          CaptionOptions.Text = 'Style'
          Control = icbLineStyle
          ControlOptions.OriginalHeight = 23
          ControlOptions.OriginalWidth = 121
          ControlOptions.ShowBorder = False
          Index = 1
        end
      end
    end
  end
  object bmManager: TdxBarManager
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    CanCustomize = False
    Categories.Strings = (
      'File'
      'Edit'
      'View'
      'Unions'
      'Options'
      'Help'
      'ChartPopupMenu')
    Categories.ItemsVisibles = (
      2
      2
      2
      2
      2
      2
      2)
    Categories.Visibles = (
      True
      True
      True
      True
      True
      True
      True)
    ImageOptions.Images = ilSmallIcons
    ImageOptions.LargeImages = ilLargeIcons
    ImageOptions.StretchGlyphs = False
    PopupMenuLinks = <>
    Style = bmsUseLookAndFeel
    UseSystemFont = True
    Left = 320
    Top = 192
    PixelsPerInch = 96
    object rbHomeClipboard: TdxBar
      Caption = 'Clipboard'
      CaptionButtons = <>
      DockedLeft = 0
      DockedTop = 0
      FloatLeft = 669
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'miPaste'
        end
        item
          Position = ipBeginsNewColumn
          ViewLevels = [ivlLargeControlOnly, ivlSmallIconWithText, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'miCut'
        end
        item
          ViewLevels = [ivlLargeControlOnly, ivlSmallIconWithText, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'miCopy'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object rbHomeArrange: TdxBar
      Caption = 'Arrange'
      CaptionButtons = <>
      DockedLeft = 840
      DockedTop = 0
      FloatLeft = 669
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      Glyph.SourceDPI = 96
      Glyph.Data = {
        3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
        462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
        617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
        2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
        77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
        22307078222076696577426F783D2230203020333220333222207374796C653D
        22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
        3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
        303B2623393B093C7374796C6520747970653D22746578742F6373732220786D
        6C3A73706163653D227072657365727665223E2E7374307B6F7061636974793A
        302E363B7D262331333B262331303B2623393B2E7374317B66696C6C3A234646
        464646463B7D262331333B262331303B2623393B2E7374327B66696C6C3A2333
        37374242363B7D262331333B262331303B2623393B2E7374337B6F7061636974
        793A302E373B7D3C2F7374796C653E0D0A093C6720636C6173733D2273743022
        3E0D0A09093C7265637420783D2231382220793D223138222077696474683D22
        313422206865696768743D223134222F3E0D0A09093C72656374207769647468
        3D22313422206865696768743D223134222F3E0D0A093C2F673E0D0A093C673E
        0D0A09093C7265637420783D2232302220793D2232302220636C6173733D2273
        7431222077696474683D22313022206865696768743D223130222F3E0D0A0909
        3C7265637420783D22322220793D22322220636C6173733D2273743122207769
        6474683D22313022206865696768743D223130222F3E0D0A093C2F673E0D0A09
        3C7265637420783D22362220793D22362220636C6173733D2273743222207769
        6474683D22323022206865696768743D223230222F3E0D0A093C6720636C6173
        733D22737433223E0D0A09093C7265637420783D22382220793D22382220636C
        6173733D22737431222077696474683D22313622206865696768743D22313622
        2F3E0D0A093C2F673E0D0A3C2F7376673E0D0A}
      ItemLinks = <
        item
          Position = ipBeginsNewColumn
          ViewLevels = [ivlLargeControlOnly, ivlSmallIconWithText, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'miBringToFront'
        end
        item
          ViewLevels = [ivlLargeControlOnly, ivlSmallIconWithText, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'miSendToBack'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object rbViewZoom: TdxBar
      Caption = 'Zoom'
      CaptionButtons = <>
      DockedLeft = 164
      DockedTop = 0
      FloatLeft = 669
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'miZoomIn'
        end
        item
          Visible = True
          ItemName = 'miZoomOut'
        end
        item
          Position = ipBeginsNewColumn
          ViewLevels = [ivlLargeControlOnly, ivlSmallIconWithText, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'miFit'
        end
        item
          ViewLevels = [ivlLargeControlOnly, ivlSmallIconWithText, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'miActualSize'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object rbViewAntialiasing: TdxBar
      Caption = 'Antialiasing'
      CaptionButtons = <>
      DockedLeft = 64
      DockedTop = 0
      FloatLeft = 669
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'miAntialiasing'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object rbHomeEdititng: TdxBar
      Caption = 'Editing'
      CaptionButtons = <>
      DockedLeft = 121
      DockedTop = 0
      FloatLeft = 669
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          ViewLevels = [ivlLargeControlOnly, ivlSmallIconWithText, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'miSelectAll'
        end
        item
          ViewLevels = [ivlLargeControlOnly, ivlSmallIconWithText, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'miClearSelection'
        end
        item
          ViewLevels = [ivlLargeControlOnly, ivlSmallIconWithText, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'miDelete'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object rbViewGridLines: TdxBar
      Caption = 'Grid'
      CaptionButtons = <>
      DockedLeft = 0
      DockedTop = 0
      FloatLeft = 669
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'miGridLines'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object rbHomeDesigner: TdxBar
      Caption = 'Designer'
      CaptionButtons = <>
      DockedDockingStyle = dsTop
      DockedLeft = 923
      DockedTop = 0
      FloatLeft = 669
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      Glyph.SourceDPI = 96
      Glyph.Data = {
        3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D227574
        662D38223F3E0D0A3C212D2D2047656E657261746F723A2041646F626520496C
        6C7573747261746F722031392E312E302C20535647204578706F727420506C75
        672D496E202E205356472056657273696F6E3A20362E3030204275696C642030
        2920202D2D3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
        617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
        2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
        77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
        22307078220D0A092076696577426F783D223020302033322033322220656E61
        626C652D6261636B67726F756E643D226E657720302030203332203332222078
        6D6C3A73706163653D227072657365727665223E0D0A3C706F6C79676F6E2066
        696C6C3D22233335364142372220706F696E74733D2231302C33322031302C32
        382031342C333220222F3E0D0A3C706F6C79676F6E2066696C6C3D2223383038
        3038302220706F696E74733D22382C323820342C323820342C342033302C3420
        33302C362032382C382032362C313020362C313020362C32362031302C323620
        222F3E0D0A3C706F6C79676F6E2066696C6C3D22233830383038302220706F69
        6E74733D22322C323420302C323420302C302032382C302032382C3220322C32
        20222F3E0D0A3C706F6C79676F6E2066696C6C3D22233830383038302220706F
        696E74733D2232302C32382032322C32362032382C32362032382C3230203330
        2C31382033302C323820222F3E0D0A3C673E0D0A090D0A09093C726563742078
        3D2231372E322220793D2231332E3522207472616E73666F726D3D226D617472
        6978282D302E37303731202D302E3730373120302E37303731202D302E373037
        312031382E353835382035312E3639383529222066696C6C3D22233335364142
        37222077696474683D22352E3722206865696768743D223137222F3E0D0A093C
        706174682066696C6C3D22233335364142372220643D224D33322C31346C2D34
        2D346C2D322C3263302C302C342C342C342C344C33322C31347A222F3E0D0A3C
        2F673E0D0A3C706F6C79676F6E2066696C6C3D22234535373432382220706F69
        6E74733D2231322C31342031322C32322031342C32322031362C32302031342C
        32302031342C31362032302C31362032322C313420222F3E0D0A3C673E0D0A09
        3C7265637420783D22362220793D223134222066696C6C3D2223413041304130
        222077696474683D223622206865696768743D2232222F3E0D0A093C72656374
        20783D2231322220793D223130222066696C6C3D222341304130413022207769
        6474683D223222206865696768743D2234222F3E0D0A093C706F6C79676F6E20
        66696C6C3D22234130413041302220706F696E74733D2231322C32342031342C
        32322031322C32322009222F3E0D0A093C7265637420783D2232302220793D22
        3130222066696C6C3D2223413041304130222077696474683D22322220686569
        6768743D2234222F3E0D0A093C7265637420783D22362220793D223230222066
        696C6C3D2223413041304130222077696474683D223622206865696768743D22
        32222F3E0D0A3C2F673E0D0A3C2F7376673E0D0A}
      ItemLinks = <
        item
          Visible = True
          ItemName = 'miCloseAndApplyChanges'
        end>
      OneOnRow = False
      Row = 1
      UseOwnFont = False
      Visible = False
      WholeRow = False
    end
    object rbHomeFont: TdxBar
      Caption = 'Font'
      CaptionButtons = <>
      DockedLeft = 248
      DockedTop = 0
      FloatLeft = 758
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      Glyph.SourceDPI = 96
      Glyph.Data = {
        3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
        462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D2246
        6F6E742220786D6C6E733D22687474703A2F2F7777772E77332E6F72672F3230
        30302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F7777772E
        77332E6F72672F313939392F786C696E6B2220783D223070782220793D223070
        78222076696577426F783D2230203020333220333222207374796C653D22656E
        61626C652D6261636B67726F756E643A6E6577203020302033322033323B2220
        786D6C3A73706163653D227072657365727665223E262331333B262331303B09
        3C7374796C6520747970653D22746578742F637373223E2E426C75657B66696C
        6C3A233131373744373B7D3C2F7374796C653E0D0A093C7061746820636C6173
        733D22426C75652220643D224D32312E372C32384832364C31372E332C34682D
        302E34682D332E39682D302E344C342C323868342E336C322E322D3668392E31
        4C32312E372C32387A204D31312E392C31384C31352C392E346C332E312C382E
        364831312E397A222F3E0D0A3C2F7376673E0D0A}
      ItemLinks = <
        item
          UserDefine = [udWidth]
          UserWidth = 137
          ViewLevels = [ivlLargeIconWithText, ivlLargeControlOnly, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'bcbFontName'
        end
        item
          Position = ipContinuesRow
          UserDefine = [udWidth]
          UserWidth = 48
          ViewLevels = [ivlLargeIconWithText, ivlLargeControlOnly, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'bcbFontSize'
        end
        item
          ButtonGroup = bgpStart
          Position = ipContinuesRow
          ViewLevels = [ivlLargeControlOnly, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'bbFontSizeInc'
        end
        item
          ButtonGroup = bgpMember
          Position = ipContinuesRow
          ViewLevels = [ivlLargeControlOnly, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'bbFontSizeDec'
        end
        item
          ButtonGroup = bgpStart
          ViewLevels = [ivlLargeControlOnly, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'bbFontBold'
        end
        item
          ButtonGroup = bgpMember
          Position = ipContinuesRow
          ViewLevels = [ivlLargeControlOnly, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'bbFontItalic'
        end
        item
          ButtonGroup = bgpMember
          Position = ipContinuesRow
          ViewLevels = [ivlLargeControlOnly, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'bbFontUnderline'
        end
        item
          ButtonGroup = bgpMember
          Position = ipContinuesRow
          ViewLevels = [ivlLargeControlOnly, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'bbFontStrikeout'
        end
        item
          Position = ipContinuesRow
          ViewLevels = [ivlLargeControlOnly, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'bbFontColor'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object rbHomeShapeStyles: TdxBar
      Caption = 'Shape Styles'
      CaptionButtons = <>
      DockedLeft = 713
      DockedTop = 0
      FloatLeft = 976
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      Glyph.SourceDPI = 96
      Glyph.Data = {
        3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
        462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
        617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
        2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
        77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
        22307078222076696577426F783D223020302033322033322220656E61626C65
        2D6261636B67726F756E643D226E6577203020302033322033322220786D6C3A
        73706163653D227072657365727665223E262331333B262331303B093C706174
        68206F7061636974793D22302E382220656E61626C652D6261636B67726F756E
        643D226E6577202020202220643D224D32392E372C34632D302E332C302D302E
        352C302E312D302E382C302E334C31372C313263302E352D302E352C312E392C
        302E332C332E332C312E3720202623393B63312E342C312E342C322E322C322E
        382C312E372C332E336C382E372D31302E394333302E392C352E392C33312C35
        2E362C33312C352E334333312C342E362C33302E342C342C32392E372C347A22
        2F3E0D0A093C70617468206F7061636974793D22302E342220656E61626C652D
        6261636B67726F756E643D226E6577202020202220643D224D31372C31326C2D
        332E312C366C312C332E314C32322C313763302E352D302E352D302E332D312E
        392D312E372D332E334331382E392C31322E332C31372E352C31312E352C3137
        2C31327A222F3E0D0A093C706174682066696C6C3D2223333337434238222064
        3D224D302C32372E3763302C302C31312E372C312E372C31362E372D332E3663
        342E332D342E342C302E372D372E372C302E372D372E37732D332E352D332E31
        2D372E322D302E3353372E332C32342E382C302C32372E377A222F3E0D0A3C2F
        7376673E0D0A}
      ItemLinks = <
        item
          ViewLevels = [ivlLargeControlOnly, ivlSmallIconWithText, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'bbBackgroundColor'
        end
        item
          ViewLevels = [ivlLargeControlOnly, ivlSmallIconWithText, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'bbStrokeColor'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object rbHomeParagraph: TdxBar
      Caption = 'Paragraph'
      CaptionButtons = <>
      DockedLeft = 506
      DockedTop = 0
      FloatLeft = 1068
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      Glyph.SourceDPI = 96
      Glyph.Data = {
        3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
        462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
        617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
        2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
        77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
        22307078222076696577426F783D223020302033322033322220656E61626C65
        2D6261636B67726F756E643D226E6577203020302033322033322220786D6C3A
        73706163653D227072657365727665223E262331333B262331303B093C67206F
        7061636974793D22302E36223E0D0A09093C7265637420783D22362220793D22
        38222066696C6C3D2223303130313031222077696474683D2232302220686569
        6768743D2232222F3E0D0A09093C7265637420783D2231302220793D22313222
        2066696C6C3D2223303130313031222077696474683D22313222206865696768
        743D2232222F3E0D0A09093C7265637420783D22362220793D22313622206669
        6C6C3D2223303130313031222077696474683D22323022206865696768743D22
        32222F3E0D0A09093C7265637420783D2231302220793D223230222066696C6C
        3D2223303130313031222077696474683D22313222206865696768743D223222
        2F3E0D0A09093C7265637420783D22362220793D223234222066696C6C3D2223
        303130313031222077696474683D22323022206865696768743D2232222F3E0D
        0A093C2F673E0D0A3C2F7376673E0D0A}
      ItemLinks = <
        item
          ButtonGroup = bgpStart
          ViewLevels = [ivlLargeControlOnly, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'bbTextAlignTop'
        end
        item
          ButtonGroup = bgpMember
          Position = ipContinuesRow
          ViewLevels = [ivlLargeControlOnly, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'bbTextAlignMiddle'
        end
        item
          ButtonGroup = bgpMember
          Position = ipContinuesRow
          ViewLevels = [ivlLargeControlOnly, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'bbTextAlignBottom'
        end
        item
          ButtonGroup = bgpStart
          ViewLevels = [ivlLargeControlOnly, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'bbTextAlignLeft'
        end
        item
          ButtonGroup = bgpMember
          Position = ipContinuesRow
          ViewLevels = [ivlLargeControlOnly, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'bbTextAlignCenter'
        end
        item
          ButtonGroup = bgpMember
          Position = ipContinuesRow
          ViewLevels = [ivlLargeControlOnly, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'bbTextAlignRight'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object rbQuickAccessUndoRedo: TdxBar
      Caption = 'UndoRedo'
      CaptionButtons = <>
      DockedLeft = 0
      DockedTop = 0
      FloatLeft = 1120
      FloatTop = 8
      FloatClientWidth = 51
      FloatClientHeight = 54
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bbUndo'
        end
        item
          Visible = True
          ItemName = 'bbRedo'
        end>
      OneOnRow = True
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object rbHomeTools: TdxBar
      Caption = 'Tools'
      CaptionButtons = <>
      DockedLeft = 600
      DockedTop = 0
      FloatLeft = 1120
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      Glyph.SourceDPI = 96
      Glyph.Data = {
        3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
        462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
        617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
        2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
        77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
        22307078222076696577426F783D223020302033322033322220656E61626C65
        2D6261636B67726F756E643D226E6577203020302033322033322220786D6C3A
        73706163653D227072657365727665223E262331333B262331303B093C672069
        643D224C617965725F32222F3E0D0A093C706F6C79676F6E2066696C6C3D2223
        4646464646462220706F696E74733D2231342E312C32312E3220382E352C3236
        2E3820382E352C332E312032362E372C31392E352031392C31392E352032342E
        332C32382E382031392E322C33312E3320222F3E0D0A093C706174682066696C
        6C3D22234646464646462220643D224D392C342E324C32352E342C3139682D35
        2E35682D312E376C302E382C312E356C342E362C382E316C2D342E322C322E31
        6C2D342E362D392E316C2D302E362D312E326C2D312C314C392C32352E365634
        2E32204D382C327632366C362D366C352C31306C362D3320202623393B6C2D35
        2E312D394832384C382C324C382C327A222F3E0D0A093C70617468206F706163
        6974793D22302E362220643D224D392C342E324C32352E342C3139682D352E35
        682D312E376C302E382C312E356C342E362C382E316C2D342E322C322E316C2D
        342E362D392E316C2D302E362D312E326C2D312C314C392C32352E3656342E32
        204D382C327632366C362D366C352C31306C362D3320202623393B6C2D352E31
        2D394832384C382C324C382C327A222F3E0D0A3C2F7376673E0D0A}
      ItemLinks = <
        item
          ViewLevels = [ivlLargeControlOnly, ivlSmallIconWithText, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'bbPointerTool'
        end
        item
          ViewLevels = [ivlLargeControlOnly, ivlSmallIconWithText, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'bbConnector'
        end
        item
          ViewLevels = [ivlLargeControlOnly, ivlSmallIconWithText, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'bbStandardShape'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object bDesignOptions: TdxBar
      Caption = 'Options'
      CaptionButtons = <>
      DockedLeft = 0
      DockedTop = 0
      FloatLeft = 1120
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bbSnapToGrid'
        end
        item
          Visible = True
          ItemName = 'bbUseAdvancedShapesOnly'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object bLayout: TdxBar
      Caption = 'Layout'
      CaptionButtons = <>
      DockedLeft = 167
      DockedTop = 0
      FloatLeft = 1120
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bsiConnectors'
        end
        item
          Visible = True
          ItemName = 'dxBarLargeButton1'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object miNew: TdxBarLargeButton
      Action = acNew
      Category = 0
      Description = 'Create a new diagram'
      ScreenTip = stNew
      SyncImageIndex = False
      ImageIndex = 17
    end
    object miOpen: TdxBarLargeButton
      Action = acOpen
      Category = 0
      Description = 'Open a diagram file'
      ScreenTip = stOpen
    end
    object miSave: TdxBarLargeButton
      Action = acSave
      Category = 0
      Description = 'Save this diagram'
      ScreenTip = stSave
    end
    object miSaveAs: TdxBarLargeButton
      Action = acSaveAs
      Category = 0
      Description = 'Save this diagram with a different name'
      ScreenTip = stSaveAs
      SyncImageIndex = False
      ImageIndex = 1
    end
    object bcbFontName: TcxBarEditItem
      Caption = 'Font'
      Category = 0
      Hint = 'Font'
      ScreenTip = stFontName
      Visible = ivAlways
      PropertiesClassName = 'TcxFontNameComboBoxProperties'
      Properties.FontPreview.ShowButtons = False
      Properties.OnEditValueChanged = bcbFontNamePropertiesEditValueChanged
      Properties.OnLoadFontComplete = bcbFontNamePropertiesLoadFontComplete
    end
    object bcbFontSize: TcxBarEditItem
      Caption = 'Font Size'
      Category = 0
      Hint = 'Font Size'
      ScreenTip = stFontSize
      Visible = ivAlways
      PropertiesClassName = 'TcxComboBoxProperties'
      Properties.DropDownListStyle = lsEditFixedList
      Properties.Items.Strings = (
        '8'
        '9'
        '10'
        '11'
        '12'
        '14'
        '16'
        '18'
        '20'
        '22'
        '24'
        '26'
        '28'
        '36'
        '48'
        '72')
      Properties.OnEditValueChanged = bcbFontSizePropertiesEditValueChanged
    end
    object bbFontSizeInc: TdxBarLargeButton
      Action = acFontSizeInc
      Category = 0
      ScreenTip = stFontSizeInc
    end
    object bbFontSizeDec: TdxBarLargeButton
      Action = acFontSizeDec
      Category = 0
      ScreenTip = stFontSizeDec
    end
    object bbFontBold: TdxBarLargeButton
      Action = acFontBold
      Category = 0
      ScreenTip = stFontBold
      ButtonStyle = bsChecked
    end
    object bbFontItalic: TdxBarLargeButton
      Action = acFontItalic
      Category = 0
      ScreenTip = stFontItalic
      ButtonStyle = bsChecked
    end
    object bbFontUnderline: TdxBarLargeButton
      Action = acFontUnderline
      Category = 0
      ScreenTip = stUnderline
      ButtonStyle = bsChecked
    end
    object bbFontStrikeout: TdxBarLargeButton
      Action = acFontStrikeout
      Category = 0
      ScreenTip = stStrikeout
      ButtonStyle = bsChecked
    end
    object bbTextAlignTop: TdxBarLargeButton
      Action = acTextAlignTop
      Category = 0
      ScreenTip = stTextAlignTop
      ButtonStyle = bsChecked
      GroupIndex = 1
    end
    object bbTextAlignMiddle: TdxBarLargeButton
      Action = acTextAlignMiddle
      Category = 0
      ScreenTip = stTextAlignMiddle
      ButtonStyle = bsChecked
      GroupIndex = 1
    end
    object bbTextAlignBottom: TdxBarLargeButton
      Action = acTextAlignBottom
      Category = 0
      ScreenTip = stTextAlignBottom
      ButtonStyle = bsChecked
      GroupIndex = 1
    end
    object bbTextAlignLeft: TdxBarLargeButton
      Action = acTextAlignLeft
      Category = 0
      ScreenTip = stTextAlignLeft
      ButtonStyle = bsChecked
      GroupIndex = 2
    end
    object bbTextAlignCenter: TdxBarLargeButton
      Action = acTextAlignCenter
      Category = 0
      ScreenTip = stTextAlignCenter
      ButtonStyle = bsChecked
      GroupIndex = 2
    end
    object bbTextAlignRight: TdxBarLargeButton
      Action = acTextAlignRight
      Category = 0
      ScreenTip = stTextAlignRight
      ButtonStyle = bsChecked
      GroupIndex = 2
    end
    object bbFakeTextVertAlign: TdxBarLargeButton
      Action = acFakeTextVertAlign
      Caption = 'Fake Text Horz Align'
      Category = 0
      ButtonStyle = bsChecked
      GroupIndex = 1
    end
    object bbFakeTextHorzAlign: TdxBarLargeButton
      Action = acFakeTextHorzAlign
      Caption = 'Fake Text Vertical Align'
      Category = 0
      ButtonStyle = bsChecked
      GroupIndex = 2
    end
    object bbPointerTool: TdxBarLargeButton
      Action = acPointerTool
      Category = 0
      ScreenTip = stPointerTool
      ButtonStyle = bsChecked
      GroupIndex = 3
    end
    object bbConnector: TdxBarLargeButton
      Action = acConnector
      Category = 0
      ScreenTip = stConnector
      ButtonStyle = bsChecked
      GroupIndex = 3
    end
    object bbStandardShape: TdxBarLargeButton
      Action = acStandardShape
      Category = 0
      ButtonStyle = bsCheckedDropDown
      GroupIndex = 3
      DropDownMenu = bpmStandardShape
    end
    object bbRedo: TdxBarLargeButton
      Action = acRedo
      Category = 0
      ScreenTip = stRedo
    end
    object bbSnapToGrid: TdxBarLargeButton
      Action = acSnapToGrid
      Category = 0
      ScreenTip = stSnapToGrid
      ButtonStyle = bsChecked
    end
    object bbRightAngle: TdxBarLargeButton
      Action = acRightAngleHorizontal
      Category = 0
    end
    object bbCurved: TdxBarLargeButton
      Action = acCurved
      Category = 0
    end
    object bbStraight: TdxBarLargeButton
      Action = acStraight
      Category = 0
    end
    object bbRightAngleVertical: TdxBarLargeButton
      Action = acRightAngleVertical
      Category = 0
    end
    object bsiConnectors: TdxBarSubItem
      Caption = 'Connectors'
      Category = 0
      Hint = 'Connectors'
      ScreenTip = stConnector
      Visible = ivAlways
      ImageIndex = 40
      LargeImageIndex = 40
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bbRightAngle'
        end
        item
          Visible = True
          ItemName = 'bbRightAngleVertical'
        end
        item
          Visible = True
          ItemName = 'bbCurved'
        end
        item
          Visible = True
          ItemName = 'bbStraight'
        end>
    end
    object bbFontColor: TdxBarLargeButton
      Action = acFontColor
      Category = 0
      ScreenTip = stFontColor
      ButtonStyle = bsDropDown
      DropDownMenu = ppmFontColor
    end
    object rgiFontColor: TdxRibbonGalleryItem
      Caption = 'Font Color Gallery'
      Category = 0
      Visible = ivAlways
      GalleryFilter.Categories = <>
      GalleryInMenuOptions.CollapsedInSubmenu = False
      GalleryInMenuOptions.DropDownGalleryResizing = gsrNone
      ItemLinks = <>
    end
    object rgiColorTheme: TdxRibbonGalleryItem
      Caption = 'Font Color Theme'
      Category = 0
      Visible = ivNever
      GalleryFilter.Categories = <>
      ItemLinks = <>
    end
    object bbBackgroundColor: TdxBarLargeButton
      Action = acBackgroundColor
      Category = 0
      ScreenTip = stBackgroundColor
      ButtonStyle = bsDropDown
      DropDownMenu = ppmBackgroundColor
    end
    object rgiBackgroundColor: TdxRibbonGalleryItem
      Caption = 'New Gallery'
      Category = 0
      Visible = ivAlways
      GalleryFilter.Categories = <>
      GalleryInMenuOptions.CollapsedInSubmenu = False
      GalleryInMenuOptions.DropDownGalleryResizing = gsrNone
      ItemLinks = <>
    end
    object rgiBackgroundThemsColor: TdxRibbonGalleryItem
      Caption = 'New Gallery'
      Category = 0
      Visible = ivNever
      GalleryFilter.Categories = <>
      ItemLinks = <>
    end
    object bbStrokeColor: TdxBarLargeButton
      Action = acStrokeColor
      Category = 0
      ScreenTip = stStrokeColor
      ButtonStyle = bsDropDown
      DropDownMenu = ppmStrokeColor
    end
    object rgiStrokeColor: TdxRibbonGalleryItem
      Caption = 'New Gallery'
      Category = 0
      Visible = ivAlways
      GalleryFilter.Categories = <>
      GalleryInMenuOptions.CollapsedInSubmenu = False
      GalleryInMenuOptions.DropDownGalleryResizing = gsrNone
      ItemLinks = <>
    end
    object rgiStrokeColorTheme: TdxRibbonGalleryItem
      Caption = 'New Gallery'
      Category = 0
      Visible = ivNever
      GalleryFilter.Categories = <>
      ItemLinks = <>
    end
    object rgiObjectBackgroundColor: TdxRibbonGalleryItem
      Caption = 'New Gallery'
      Category = 0
      Visible = ivAlways
      GalleryFilter.Categories = <>
      GalleryInMenuOptions.CollapsedInSubmenu = False
      GalleryInMenuOptions.DropDownGalleryResizing = gsrNone
      ItemLinks = <>
    end
    object rgiObjectBackgroundThemsColor: TdxRibbonGalleryItem
      Caption = 'New Gallery'
      Category = 0
      Visible = ivNever
      GalleryFilter.Categories = <>
      ItemLinks = <>
    end
    object rgiLineColor: TdxRibbonGalleryItem
      Caption = 'New Gallery'
      Category = 0
      Visible = ivAlways
      GalleryFilter.Categories = <>
      GalleryInMenuOptions.CollapsedInSubmenu = False
      GalleryInMenuOptions.DropDownGalleryResizing = gsrNone
      ItemLinks = <>
    end
    object rgiLineColorTheme: TdxRibbonGalleryItem
      Caption = 'New Gallery'
      Category = 0
      Visible = ivNever
      GalleryFilter.Categories = <>
      ItemLinks = <>
    end
    object rgiSourceArrowColor: TdxRibbonGalleryItem
      Caption = 'New Gallery'
      Category = 0
      Visible = ivAlways
      GalleryFilter.Categories = <>
      GalleryInMenuOptions.CollapsedInSubmenu = False
      GalleryInMenuOptions.DropDownGalleryResizing = gsrNone
      ItemLinks = <>
    end
    object rgiSourceArrowColorTheme: TdxRibbonGalleryItem
      Caption = 'New Gallery'
      Category = 0
      Visible = ivNever
      GalleryFilter.Categories = <>
      ItemLinks = <>
    end
    object rgiDestArrowColor: TdxRibbonGalleryItem
      Caption = 'New Gallery'
      Category = 0
      Visible = ivAlways
      GalleryFilter.Categories = <>
      GalleryInMenuOptions.CollapsedInSubmenu = False
      GalleryInMenuOptions.DropDownGalleryResizing = gsrNone
      ItemLinks = <>
    end
    object rgiDestArrowColorTheme: TdxRibbonGalleryItem
      Caption = 'New Gallery'
      Category = 0
      Visible = ivNever
      GalleryFilter.Categories = <>
      ItemLinks = <>
    end
    object bbUseAdvancedShapesOnly: TdxBarLargeButton
      Action = acUseAdvancedShapesOnly
      Category = 0
      ButtonStyle = bsChecked
    end
    object dxBarLargeButton1: TdxBarLargeButton
      Action = acApplyLayeredLayout
      Category = 0
      ScreenTip = stApplyLayeredLayout
    end
    object bbUndo: TdxBarLargeButton
      Action = acUndo
      Category = 1
      ScreenTip = stUndo
      SyncImageIndex = False
      ImageIndex = 8
    end
    object miCut: TdxBarLargeButton
      Action = acCut
      Category = 1
      ScreenTip = stCut
    end
    object miCopy: TdxBarLargeButton
      Action = acCopy
      Category = 1
      ScreenTip = stCopy
    end
    object miPaste: TdxBarLargeButton
      Action = acPaste
      Category = 1
      ScreenTip = stPaste
    end
    object miDelete: TdxBarLargeButton
      Action = acDelete
      Category = 1
    end
    object miSelectAll: TdxBarLargeButton
      Action = acSelectAll
      Category = 1
    end
    object miClearSelection: TdxBarLargeButton
      Action = acClearSelection
      Category = 1
    end
    object miBringToFront: TdxBarLargeButton
      Action = acBringToFront
      Category = 1
      ScreenTip = stBringToFront
    end
    object miSendToBack: TdxBarLargeButton
      Action = acSendToBack
      Category = 1
      ScreenTip = stSendToBack
    end
    object miCloseAndApplyChanges: TdxBarLargeButton
      Action = acCloseAndApplyChanges
      Category = 1
    end
    object miAntialiasing: TdxBarLargeButton
      Action = acAntialiasing
      Category = 2
      ButtonStyle = bsChecked
    end
    object miZoomIn: TdxBarLargeButton
      Action = acZoomIn
      Category = 2
    end
    object miZoomOut: TdxBarLargeButton
      Action = acZoomOut
      Category = 2
    end
    object miFit: TdxBarLargeButton
      Action = acFit
      Category = 2
      ButtonStyle = bsChecked
    end
    object miActualSize: TdxBarLargeButton
      Action = acActualSize
      Category = 2
    end
    object miGridLines: TdxBarLargeButton
      Action = acGridLines
      Category = 2
      ScreenTip = stGrid
      ButtonStyle = bsChecked
    end
    object miNewUnion: TdxBarLargeButton
      Action = acNewUnion
      Category = 3
    end
    object miAddToUnion: TdxBarLargeButton
      Action = acAddToUnion
      Category = 3
    end
    object miRemoveFromUnion: TdxBarLargeButton
      Action = acRemoveFromUnion
      Category = 3
    end
    object miClearUnion: TdxBarLargeButton
      Action = acClearUnion
      Category = 3
    end
    object miClearAllUnions: TdxBarLargeButton
      Action = acClearAllUnions
      Category = 3
    end
    object miContents: TdxBarLargeButton
      Action = acContents
      Category = 5
    end
    object miProperties: TdxBarLargeButton
      Action = acProperties
      Category = 6
    end
    object miRemovePoint: TdxBarLargeButton
      Action = acRemovePoint
      Category = 6
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'flc'
    Filter = 'Flow Chart|*.flc'
    Left = 198
    Top = 200
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'flc'
    Filter = 'Flow Chart|*.flc'
    Left = 230
    Top = 200
  end
  object bpmChart: TdxBarPopupMenu
    BarManager = bmManager
    ItemLinks = <
      item
        Visible = True
        ItemName = 'miProperties'
      end
      item
        Visible = True
        ItemName = 'miCut'
      end
      item
        Visible = True
        ItemName = 'miCopy'
      end
      item
        Visible = True
        ItemName = 'miPaste'
      end
      item
        Visible = True
        ItemName = 'miDelete'
      end
      item
        Visible = True
        ItemName = 'miRemovePoint'
      end
      item
        BeginGroup = True
        Visible = True
        ItemName = 'miSelectAll'
      end
      item
        Visible = True
        ItemName = 'miClearSelection'
      end
      item
        BeginGroup = True
        Visible = True
        ItemName = 'miBringToFront'
      end
      item
        Visible = True
        ItemName = 'miSendToBack'
      end
      item
        BeginGroup = True
        Visible = True
        ItemName = 'miNewUnion'
      end
      item
        Visible = True
        ItemName = 'miAddToUnion'
      end
      item
        Visible = True
        ItemName = 'miRemoveFromUnion'
      end
      item
        Visible = True
        ItemName = 'miClearUnion'
      end
      item
        Visible = True
        ItemName = 'miClearAllUnions'
      end>
    UseOwnFont = False
    Left = 136
    Top = 200
    PixelsPerInch = 96
  end
  object bamRibbonApplicationButtonMenu: TdxBarApplicationMenu
    BarManager = bmManager
    Buttons = <>
    ExtraPane.Items = <>
    ExtraPane.Visible = False
    ItemLinks = <
      item
        Visible = True
        ItemName = 'miNew'
      end
      item
        Visible = True
        ItemName = 'miOpen'
      end
      item
        Visible = True
        ItemName = 'miSave'
      end
      item
        Visible = True
        ItemName = 'miSaveAs'
      end
      item
        Visible = True
        ItemName = 'miContents'
      end>
    UseOwnFont = False
    Left = 112
    Top = 384
    PixelsPerInch = 96
  end
  object alActions: TActionList
    Left = 400
    Top = 312
    object acNew: TAction
      Caption = 'New'
      Hint = 'New'
      ImageIndex = 17
      ShortCut = 16462
      OnExecute = acNewExecute
    end
    object acOpen: TAction
      Caption = 'Open'
      Hint = 'Open'
      ImageIndex = 0
      ShortCut = 16463
      OnExecute = acOpenExecute
    end
    object acSave: TAction
      Caption = 'Save'
      Hint = 'Save'
      ImageIndex = 18
      ShortCut = 16467
      OnExecute = acSaveExecute
    end
    object acSaveAs: TAction
      Caption = 'Save As'
      Hint = 'Save As'
      ImageIndex = 1
      ShortCut = 24659
      OnExecute = acSaveAsExecute
    end
    object acContents: TAction
      Caption = 'Contents'
      ImageIndex = 2
      ShortCut = 112
      OnExecute = acContentsExecute
    end
    object acPaste: TAction
      Caption = 'Paste'
      Hint = 'Paste'
      ImageIndex = 3
      OnExecute = acPasteExecute
      OnUpdate = acUpdate
    end
    object acCut: TAction
      Caption = 'Cut'
      Hint = 'Cut'
      ImageIndex = 4
      OnExecute = acCutExecute
      OnUpdate = acUpdate
    end
    object acCopy: TAction
      Caption = 'Copy'
      Hint = 'Copy'
      ImageIndex = 5
      OnExecute = acCopyExecute
      OnUpdate = acUpdate
    end
    object acSelectAll: TAction
      Caption = 'Select All'
      ImageIndex = 6
      OnExecute = acSelectAllExecute
      OnUpdate = acUpdate
    end
    object acClearSelection: TAction
      Caption = 'Clear Selection'
      ImageIndex = 7
      OnExecute = acClearSelectionExecute
    end
    object acUndo: TAction
      Caption = 'Undo'
      Hint = 'Undo'
      ImageIndex = 8
      ShortCut = 16474
      OnExecute = acUndoExecute
    end
    object acDelete: TAction
      Caption = 'Delete'
      ImageIndex = 9
      OnExecute = acDeleteExecute
      OnUpdate = acUpdate
    end
    object acBringToFront: TAction
      Caption = 'Bring To Front'
      Hint = 'Bring To Front'
      ImageIndex = 10
      ShortCut = 24646
      OnExecute = acBringToFrontExecute
    end
    object acSendToBack: TAction
      Caption = 'Send To Back'
      Hint = 'Send To Back'
      ImageIndex = 11
      ShortCut = 24642
      OnExecute = acSendToBackExecute
    end
    object acZoomIn: TAction
      Caption = 'Zoom In'
      ImageIndex = 12
      ShortCut = 16491
      OnExecute = acZoomInExecute
    end
    object acZoomOut: TAction
      Caption = 'Zoom Out'
      ImageIndex = 13
      ShortCut = 16493
      OnExecute = acZoomOutExecute
    end
    object acFit: TAction
      AutoCheck = True
      Caption = 'Fit'
      ImageIndex = 14
      ShortCut = 24663
      OnExecute = acFitExecute
    end
    object acActualSize: TAction
      Caption = 'Actual Size'
      ImageIndex = 15
      OnExecute = acActualSizeExecute
    end
    object acAntialiasing: TAction
      AutoCheck = True
      Caption = 'Antialiasing'
      Checked = True
      ImageIndex = 16
      OnExecute = acAntialiasingExecute
    end
    object acNewUnion: TAction
      Caption = 'New Union'
      OnExecute = acNewUnionExecute
    end
    object acAddToUnion: TAction
      Caption = 'Add To Union'
      OnExecute = acAddToUnionExecute
    end
    object acRemoveFromUnion: TAction
      Caption = 'Remove From Union'
      OnExecute = acRemoveFromUnionExecute
    end
    object acClearUnion: TAction
      Caption = 'Clear Union'
      OnExecute = acClearUnionExecute
    end
    object acClearAllUnions: TAction
      Caption = 'Clear All Unions'
      OnExecute = acClearAllUnionsExecute
    end
    object acProperties: TAction
      Caption = 'Properties'
      OnExecute = acPropertiesExecute
    end
    object acRemovePoint: TAction
      Caption = 'Remove Point'
      OnExecute = acRemovePointExecute
    end
    object acGridLines: TAction
      AutoCheck = True
      Caption = 'Grid'
      Checked = True
      Hint = 'Grid'
      ImageIndex = 19
      OnExecute = acGridLinesExecute
    end
    object acCloseAndApplyChanges: TAction
      Caption = 'Close And Apply Changes'
      ImageIndex = 20
      Visible = False
      OnExecute = acCloseAndApplyChangesExecute
    end
    object acFontSizeInc: TAction
      Caption = 'Increase Font Size'
      Hint = 'Increase Font Size'
      ImageIndex = 21
      ShortCut = 24766
      OnExecute = acFontSizeIncExecute
    end
    object acFontSizeDec: TAction
      Caption = 'Decrease Font Size'
      Hint = 'Decrease Font Size'
      ImageIndex = 22
      ShortCut = 24764
      OnExecute = acFontSizeDecExecute
    end
    object acFontBold: TAction
      AutoCheck = True
      Caption = 'Bold'
      Hint = 'Bold'
      ImageIndex = 23
      ShortCut = 16450
      OnExecute = acFontBoldExecute
    end
    object acFontItalic: TAction
      AutoCheck = True
      Caption = 'Italic'
      Hint = 'Italic'
      ImageIndex = 24
      ShortCut = 16457
      OnExecute = acFontItalicExecute
    end
    object acFontUnderline: TAction
      AutoCheck = True
      Caption = 'Underline'
      Hint = 'Underline'
      ImageIndex = 25
      ShortCut = 16469
      OnExecute = acFontUnderlineExecute
    end
    object acFontStrikeout: TAction
      AutoCheck = True
      Caption = 'Strikethrough'
      Hint = 'Strikethrough'
      ImageIndex = 26
      OnExecute = acFontStrikeoutExecute
    end
    object acTextAlignTop: TAction
      AutoCheck = True
      Caption = 'Align Top'
      GroupIndex = 1
      Hint = 'Align Top'
      ImageIndex = 30
      ShortCut = 24660
      OnExecute = acTextVertAlignExecute
    end
    object acTextAlignMiddle: TAction
      AutoCheck = True
      Caption = 'Align Middle'
      GroupIndex = 1
      Hint = 'Align Middle'
      ImageIndex = 31
      ShortCut = 24653
      OnExecute = acTextVertAlignExecute
    end
    object acTextAlignBottom: TAction
      AutoCheck = True
      Caption = 'Align Bottom'
      GroupIndex = 1
      Hint = 'Align Bottom'
      ImageIndex = 32
      ShortCut = 24662
      OnExecute = acTextVertAlignExecute
    end
    object acTextAlignLeft: TAction
      AutoCheck = True
      Caption = 'Align Left'
      GroupIndex = 2
      Hint = 'Align Left'
      ImageIndex = 33
      ShortCut = 24652
      OnExecute = acTextHorzAlignExecute
    end
    object acTextAlignCenter: TAction
      AutoCheck = True
      Caption = 'Align Center'
      GroupIndex = 2
      Hint = 'Align Center'
      ImageIndex = 34
      ShortCut = 24643
      OnExecute = acTextHorzAlignExecute
    end
    object acTextAlignRight: TAction
      AutoCheck = True
      Caption = 'Align Right'
      GroupIndex = 2
      Hint = 'Align Right'
      ImageIndex = 35
      ShortCut = 24658
      OnExecute = acTextHorzAlignExecute
    end
    object acFakeTextVertAlign: TAction
      AutoCheck = True
      GroupIndex = 1
      Visible = False
    end
    object acFakeTextHorzAlign: TAction
      AutoCheck = True
      GroupIndex = 2
      Visible = False
    end
    object acPointerTool: TAction
      AutoCheck = True
      Caption = 'Pointer Tool'
      GroupIndex = 3
      Hint = 'Pointer Tool'
      ImageIndex = 36
      ShortCut = 16433
      OnExecute = acPointerToolExecute
    end
    object acConnector: TAction
      AutoCheck = True
      Caption = 'Connector'
      GroupIndex = 3
      Hint = 'Connector'
      ImageIndex = 37
      ShortCut = 16434
      OnExecute = acConnectorExecute
    end
    object acStandardShape: TAction
      AutoCheck = True
      Caption = 'Shape'
      GroupIndex = 3
      OnExecute = acStandardShapeExecute
    end
    object acRedo: TAction
      Caption = 'Redo'
      Hint = 'Redo'
      ImageIndex = 38
      ShortCut = 16473
      OnExecute = acRedoExecute
    end
    object acSnapToGrid: TAction
      AutoCheck = True
      Caption = 'Snap To Grid'
      Hint = 'Snap To Grid'
      ImageIndex = 39
      OnExecute = acSnapToGridExecute
    end
    object acRightAngleHorizontal: TAction
      Caption = 'Right Angle Horizontal'
      ImageIndex = 41
      OnExecute = acRightAngleHorizontalExecute
    end
    object acRightAngleVertical: TAction
      Caption = 'Right Angle Vertical'
      ImageIndex = 42
      OnExecute = acRightAngleVerticalExecute
    end
    object acCurved: TAction
      Caption = 'Curved'
      ImageIndex = 43
      OnExecute = acCurvedExecute
    end
    object acStraight: TAction
      Caption = 'Straight'
      ImageIndex = 44
      OnExecute = acStraightExecute
    end
    object acFontColor: TAction
      Caption = 'Font Color'
      Hint = 'Font Color'
      ImageIndex = 27
      OnExecute = acFontColorExecute
    end
    object acBackgroundColor: TAction
      Caption = 'Background'
      Hint = 'Background'
      ImageIndex = 28
      OnExecute = acBackgroundColorExecute
    end
    object acStrokeColor: TAction
      Caption = 'Stroke'
      Hint = 'Stroke'
      ImageIndex = 29
      OnExecute = acStrokeColorExecute
    end
    object acUseAdvancedShapesOnly: TAction
      AutoCheck = True
      Caption = 'Use Advanced Shapes Only'
      Checked = True
      Hint = 'Use Advanced Shapes Only'
      ImageIndex = 45
      Visible = False
      OnExecute = acUseAdvancedShapesOnlyExecute
    end
    object acApplyLayeredLayout: TAction
      Caption = 'Apply Layered Layout'
      Hint = 'Apply Layered Layout'
      ImageIndex = 48
      OnExecute = acApplyLayeredLayoutExecute
    end
  end
  object bpmStensils: TdxBarPopupMenu
    BarManager = bmManager
    ItemLinks = <>
    UseOwnFont = False
    Left = 120
    Top = 272
    PixelsPerInch = 96
  end
  object dmShapes: TdxDockingManager
    Color = clBtnFace
    DefaultHorizContainerSiteProperties.CustomCaptionButtons.Buttons = <>
    DefaultHorizContainerSiteProperties.Dockable = True
    DefaultHorizContainerSiteProperties.ImageIndex = -1
    DefaultVertContainerSiteProperties.CustomCaptionButtons.Buttons = <>
    DefaultVertContainerSiteProperties.Dockable = True
    DefaultVertContainerSiteProperties.ImageIndex = -1
    DefaultTabContainerSiteProperties.CustomCaptionButtons.Buttons = <>
    DefaultTabContainerSiteProperties.Dockable = True
    DefaultTabContainerSiteProperties.ImageIndex = -1
    DefaultTabContainerSiteProperties.TabsProperties.CustomButtons.Buttons = <>
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 512
    Top = 184
    PixelsPerInch = 96
  end
  object ilLargeIcons: TcxImageList
    SourceDPI = 96
    Height = 32
    Width = 32
    FormatVersion = 1
    Left = 400
    Top = 192
    Bitmap = {
      494C010131002C01580120002000FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      000000000000360000002800000080000000A001000001002000000000000040
      0300000000000000000000000000000000000000000000000000000000390000
      003D000000000000000000000000000000000000000000000000000000000000
      0000000000000000004F0000000000000000B67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000004F00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000001A000000930000
      00940000001C0000000000000000000000000000000000000000000000000000
      000000000000000000990000004F00000000B67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF000000000000004F0000009900000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000060000007F000000990000
      0099000000820000000800000000000000000000009900000099000000990000
      00990000009900000099000000990000004FB67A36FFB67A36FFFFFFFFFFFFFF
      FFFFB67A36FFB67A36FF0000004F000000990000009900000099000000990000
      0099000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000006000000099000000990000
      0099000000990000006300000000000000000000009900000099000000990000
      00990000009900000099000000990000004AB67A36FFB67A36FFFFFFFFFFFFFF
      FFFFB67A36FFB67A36FF0000004A000000990000009900000099000000990000
      0099000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000009900000099000000000000
      000000000000000000990000004A00000000B67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF000000000000004A0000009900000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000009900000099000000000000
      0000000000000000004A0000000000000000B67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000004A00000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000009900000099000000000000
      00000000000000000000000000000000000000000000000000000000004F0000
      004F000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000009900000099000000000000
      000000000000000000000000000000000000000000000000004F000000990000
      00990000004F0000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000009900000099000000000000
      0000000000000000000000000000000000000000004F00000099000000990000
      0099000000990000004F00000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      009900000000000000000000004C0000004C0000004C0000004C0000004C0000
      004C000000000000000000000000000000000000000000000000000000990000
      00990000000000000000000000000000000000000000000000000000004C0000
      004C0000004C0000004C0000004C0000004C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      009900000000000000000000004C0000004C0000004C0000004C0000004C0000
      004C000000000000000000000000000000000000000000000000000000990000
      00990000000000000000000000000000000000000000000000000000004C0000
      004C0000004C0000004C0000004C0000004C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      009900000000000000000000004C0000004CFFFFFFFFFFFFFFFF0000004C0000
      004C000000000000000000000000000000000000000000000000000000990000
      00990000000000000000000000000000000000000000000000000000004C0000
      004CFFFFFFFFFFFFFFFF0000004C0000004C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      009900000000000000000000004C0000004CFFFFFFFFFFFFFFFF0000004C0000
      004C000000000000000000000000000000000000000000000000000000990000
      00990000000000000000000000000000000000000000000000000000004C0000
      004CFFFFFFFFFFFFFFFF0000004C0000004C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      009900000000000000000000004C0000004C0000004C0000004C0000004C0000
      004C000000000000000000000000000000000000000000000000000000990000
      00990000000000000000000000000000000000000000000000000000004C0000
      004C0000004C0000004C0000004C0000004C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      009900000000000000000000004C0000004C0000004C0000004C0000004C0000
      004C000000000000000000000000000000000000000000000000000000990000
      00990000000000000000000000000000000000000000000000000000004C0000
      004C0000004C0000004C0000004C0000004C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000004F0000004F000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      00000000004F0000004F00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      00990000000000000000000000000000004F00000099000000990000004F0000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      004F00000099000000990000004F000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      009900000000000000000000004F000000990000009900000099000000990000
      004F000000000000000000000000000000000000000000000000000000990000
      00990000000000000000000000000000000000000000000000000000004F0000
      00990000009900000099000000990000004F0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000009900000099000000000000
      00000000000000000000000000000000000089AE4CFF89AE4CFF89AE4CFF89AE
      4CFF89AE4CFF89AE4CFF00000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000009900000099000000000000
      00000000000000000000000000000000000089AE4CFF89AE4CFF89AE4CFF89AE
      4CFF89AE4CFF89AE4CFF00000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000009900000099000000990000
      00990000009900000099000000990000009989AE4CFF89AE4CFFFFFFFFFFFFFF
      FFFF89AE4CFF89AE4CFF00000099000000990000009900000099000000990000
      0099000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000009900000099000000990000
      00990000009900000099000000990000009989AE4CFF89AE4CFFFFFFFFFFFFFF
      FFFF89AE4CFF89AE4CFF00000099000000990000009900000099000000990000
      0099000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000089AE4CFF89AE4CFF89AE4CFF89AE
      4CFF89AE4CFF89AE4CFF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000089AE4CFF89AE4CFF89AE4CFF89AE
      4CFF89AE4CFF89AE4CFF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000322131390D4141496D8000004260000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000003221515A3E11B1BD1FF1B1BD1FF1616A9E50000
      0426000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000003221515A3E11B1BD1FF1B1BD1FF1B1BD1FF1B1BD1FF1616
      A9E5000004260000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000003221515A3E11B1BD1FF1B1BD1FF1B1BD1FF1B1BD1FF1B1BD1FF1B1B
      D1FF1616A9E50000042600000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      03221515A3E11B1BD1FF1B1BD1FF1B1BD1FF1B1BD1FF1B1BD1FF1B1BD1FF1B1B
      D1FF1B1BD1FF1616A9E500000426000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000003221515
      A3E11B1BD1FF1B1BD1FF1B1BD1FF1B1BD1FF1B1BD1FF1B1BD1FF1B1BD1FF1B1B
      D1FF1B1BD1FF1B1BD1FF1616A9E5000004260000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FF00000000000000000000000000000000000000000000000012128BD01B1B
      D1FF1B1BD1FF1B1BD1FF1B1BD1FF1B1BD1FF1B1BD1FF1B1BD1FF1B1BD1FF1B1B
      D1FF1B1BD1FF1B1BD1FF1B1BD1FF141496D80000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FF000000000000000000000000000000000000000000000000111185CB1B1B
      D1FF1B1BD1FF1B1BD1FF1B1BD1FF1B1BD1FF1B1BD1FF1B1BD1FF1B1BD1FF1B1B
      D1FF1B1BD1FF1B1BD1FF1B1BD1FF12128FD30000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFE9D7C3FFE9D7C3FFB67A36FFB67A
      36FF0000000000000000000000000000000000000000000000000000021A1414
      97D91B1BD1FF1B1BD1FF1B1BD1FF1B1BD1FF1B1BD1FF1B1BD1FF1B1BD1FF1B1B
      D1FF1B1BD1FF1B1BD1FF14149DDD0000021E0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF000000000000
      0000D77610FFD77610FFD77610FF321C037C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFE9D7C3FFE9D7C3FFB67A36FFB67A
      36FF000000000000000000000000000000000000000000000000000000000000
      021A141497D91B1BD1FF1B1BD1FF1B1BD1FF1B1BD1FF1B1BD1FF1B1BD1FF1B1B
      D1FF1B1BD1FF14149DDD0000021E000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF000000000000
      0000D77610FFD77610FF321C037C00000000391F0484391F0484000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000200000058B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FF000000000000000000000000000000000000000000000000000000000000
      00000000021A141497D91B1BD1FF1B1BD1FF1B1BD1FF1B1BD1FF1B1BD1FF1B1B
      D1FF14149DDD0000021E00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      0000D77610FF321C037C00000000391F0484D77610FFD77610FF391F04840000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00070000006C00000099B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000021A141497D91B1BD1FF1B1BD1FF1B1BD1FF1B1BD1FF1414
      9DDD0000021E0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      0000321C037C00000000391F0484D77610FFD77610FFD77610FFD77610FF391F
      0484000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000B0000
      0004000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000110000
      007C000000990000009900000097000000410000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000021A141497D91B1BD1FF1B1BD1FF14149DDD0000
      021E000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      000000000000391F0484D77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FF391F04840000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000070707434D4D4DD2717171FF6969
      69F62929299A0000000D00000000000000000000000000000000000000000000
      0000000000000000000000000000000000106A3A08B3D27410FC8F4E0BD00603
      002D000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000001E000000890000
      009900000099000000920000002D000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000021A111185CB12128ACF0000021E0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      000000000000321C037CD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FF391F048400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000B0B0B516D6D6DFA717171FF717171FF7171
      71FF717171FF434343C50000000E000000000000000000000000000000000000
      000000000000000000000000000049280595D77610FFD77610FFD77610FF8F4E
      0BD0000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000002E00000092000000990000
      0099000000880000001D00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      00000000000000000000321C037CD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FF391F0484000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000B0B0B516D6D6DFA717171FF717171FF717171FF7171
      71FF717171FF717171FF434343C50000000E0000000000000000000000000000
      000000000000000000000000000081470AC6D77610FFD77610FFD77610FFD776
      10FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000420000009700000099000000990000
      007B000000100000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000321C037CD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FF391F04840000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000B0B0B516D6D6DFA717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF434343C50000000E00000000000000000000
      000000000000000000000000000092500BD2D77610FFD77610FFD77610FFC069
      0FF1000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000002000000580000009900000099000000990000006B0000
      0007000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000001001B0A2D018A196F02D7229802FC229802FC197102D90A2F
      018D0002001D00000000000000000000000000000000000000000F7DB3D614B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF13A2E9F40000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000321C037CD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FF391F048400000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000B0B0B516D6D6DFA717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF434343C50000000E000000000000
      0000000000000000000000000000B2610DE8D77610FFD77610FFD77610FF8146
      0AC5000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000070000006C00000099000000990000009900000057000000020000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000413005A1F8E02F3229C02FF229C02FF229C02FF229C02FF229C02FF229C
      02FF209002F50416006000000000000000000000000000000000063A549314B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF0A577DB30000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000321C037CD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FF391F0484000000000000
      0000000000000000000000000000000000000000000000000000000000000B0B
      0B516D6D6DFA717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF434343C50000000E0000
      000000000000000000000000000FD57610FED77610FFD77610FFD77610FF331C
      037D000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00110000007C0000009900000099000000970000004100000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000413
      0059229A02FE229C02FF229C02FF229C02FF229C02FF229C02FF229C02FF229C
      02FF229C02FF229B02FE041600600000000000000000000000000001021A13A5
      EDF614B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF00070A350000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000321C037CD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FF391F04840000
      00000000000000000000000000000000000000000000000000000606063C6D6D
      6DFA717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF434343C50000
      000E00000000000000000E080143D77610FFD77610FFD77610FFCD7010F90201
      001C000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000001E0000
      00890000009900000099000000920000002D0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000100191F8C
      02F2229C02FF229C02FF229C02FF229C02FF229C02FF229C02FF229C02FF229C
      02FF229C02FF229C02FF209002F50002001E0000000000000000000000000637
      508F14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF0A577DB3000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000321C
      037CD77610FFD77610FFD77610FFD77610FFD77610FFD77610FF321C037C0000
      00000000000000000000000000000000000000000000000000003D3D3DBB7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF3C3C
      3CBA00000000000000003E220489D77610FFD77610FFD77610FF3C2104870000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000002E000000920000
      009900000099000000880000001D000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000092A0185229C
      02FF229C02FF229C02FF229C02FF229C02FF229C02FF229C02FF229C02FF229C
      02FF229C02FF229C02FF229C02FF0A2F018D0000000000000000000000000001
      021913A5EDF614B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14AFFDFE00060932000000000000000000000000717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF383838B4000000070000
      0000321C037CD77610FFD77610FFD77610FFD77610FF321C037C00000000391F
      04843F23048B00000000000000000000000000000000000000005F5F5FEA7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF6D6D6DFA1616
      16710000000C0000000A111111656B6B6BF8717171FF717171FF676767F40606
      063F0000000000000004AA5D0DE3D77610FFD77610FF81470AC6000000070000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000004200000097000000990000
      00990000007B0000001000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000176902D1229C
      02FF229C02FF229C02FF229C02FF229C02FF229C02FF229C02FF229C02FF229C
      02FF229C02FF229C02FF229C02FF197102D90000000000000000000000000000
      00000637508F14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF095378AF00000000000000000000000000000000717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF383838B40000
      000700000000321C037CD77610FFD77610FF321C037C00000000391F0484D776
      10FFD77610FF3C210487000000000000000000000000000000004B4B4BD07171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF1414146D0000
      000000000000000000000000000008080846676767F4676767F40606063F0000
      00000000000020110264D77610FFD77610FF80460AC500000011000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FF000000990000
      006B000000070000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000208E02F4229C
      02FF229C02FF229C02FF229C02FF229C02FF229C02FF229C02FF229C02FF229C
      02FF229C02FF229C02FF229C02FF229802FC0000000000000000000000000000
      00000001021813A3EBF514B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14AFFDFE0006093100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000321C037C321C037C00000000391F0484D77610FFD776
      10FFD77610FFBE690FF000000000000000000000000000000000111111657171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF000000120000
      0000434343C44B4B4BD000000017000000000606063C0606063F000000000000
      000003010021B9650EEDCB7010F8391F04840000000700000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FF000000570000
      0002000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000208E02F3229C
      02FF229C02FF229C02FF229C02FF229C02FF229C02FF229C02FF229C02FF229C
      02FF229C02FF229C02FF229C02FF229802FC0000000000000000000000000000
      00000000000006344C8B14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF095378AF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000391F0484D77610FFD77610FFD776
      10FFC56C0FF40D07003F00000000000000000000000000000000000000002525
      2592717171FF717171FF717171FF717171FF717171FF717171FF000000130000
      0000424242C3717171FF4E4E4ED4000000170000000000000000000000020905
      0035562F06A12F1A037802010019000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B67A36FFB67A36FFE9D7C3FFE9D7C3FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000166601CF229C
      02FF229C02FF229C02FF229C02FF229C02FF229C02FF229C02FF229C02FF229C
      02FF229C02FF229C02FF229C02FF196F02D70000000000000000000000000000
      0000000000000001021713A3EBF514B1FFFF14B1FFFF14B1FFFF14B1FFFF14AF
      FDFE0006082F0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002C180374D77610FFD77610FFC56C
      0FF40D07003F0000000000000000000000000000000000000000000000000000
      00012929299A717171FF717171FF717171FF717171FF717171FF1313136A0000
      000000000011484848CB717171FF4E4E4ED40000001700000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B67A36FFB67A36FFE9D7C3FFE9D7C3FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000009280182229C
      02FF229C02FF229C02FF229C02FF229C02FF229C02FF229C02FF229C02FF229C
      02FF229C02FF229C02FF229C02FF0A2D018A0000000000000000000000000000
      0000000000000000000006344C8B14B1FFFF14B1FFFF14B1FFFF14B1FFFF094F
      72AB000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000002C180374B2610DE80D07
      003F000000000000000000000000000000000000000000000000000000000000
      0000000000022D2D2DA1717171FF717171FF717171FF717171FF696969F60808
      08450000000000000011484848CB717171FF4E4E4ED400000017000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000100161E8A
      02F0229C02FF229C02FF229C02FF229C02FF229C02FF229C02FF229C02FF229C
      02FF229C02FF229C02FF1F8E02F30001001B0000000000000000000000000000
      000000000000000000000001011612A0E7F314B1FFFF14B1FFFF14AFFDFE0005
      082E000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000003303030A7717171FF717171FF717171FF717171FF6969
      69F6080808450000000000000011484848CB717171FF4E4E4ED4000000170000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000310
      0053229902FD229C02FF229C02FF229C02FF229C02FF229C02FF229C02FF229C
      02FF229C02FF229A02FE0413005A000000000000000000000000000000000000
      00000000000000000000000000000531478714B1FFFF14B1FFFF094F72AB0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000004343434AD717171FF717171FF717171FF6767
      67F40606063F000000000000000000000011484848CB717171FF4E4E4ED40000
      0017000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000031000531E8A02F0229C02FF229C02FF229C02FF229C02FF229C02FF229C
      02FF1F8C02F20413005900000000000000000000000000000000000000000000
      00000000000000000000000000000001011412A0E7F314ADFBFD0005072C0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000005373737B3717171FF676767F40606
      063F0000000000000000000000000000000000000011484848CB717171FF4343
      43C5000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000001001609280183166601CF1F8E02F41F8E02F4176901D1092A
      0185000100180000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000052D4282084768A3000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000007353535AE0606063F0000
      00000000000000000000000000000000000000000000000000113B3B3BB82727
      2796000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000100000002000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF0000000000000000B67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF0000000000000000B67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B67A36FFB67A36FFE9D7C3FFE9D7
      C3FFB67A36FFB67A36FF0000000000000000B67A36FFB67A36FFE9D7C3FFE9D7
      C3FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFE9D7C3FFE9D7C3FFB67A36FFB67A
      36FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B67A36FFB67A36FFE9D7C3FFE9D7
      C3FFB67A36FFB67A36FF0000000000000000B67A36FFB67A36FFE9D7C3FFE9D7
      C3FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFE9D7C3FFE9D7C3FFB67A36FFB67A
      36FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF0000000000000000B67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FF00000000000000000000000000000000B67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF0000000000000000B67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FF000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFE9D7C3FFE9D7C3FFB67A36FFB67A
      36FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      00990000009900000099B67A36FFB67A36FFE9D7C3FFE9D7C3FFB67A36FFB67A
      36FF000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000050000005C0000009200000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      00990000009900000099B67A36FFB67A36FFE9D7C3FFE9D7C3FFB67A36FFB67A
      36FF000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFE9D7C3FFE9D7C3FFB67A36FFB67A
      36FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      00990000009900000099B67A36FFB67A36FFE9D7C3FFE9D7C3FFB67A36FFB67A
      36FF000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000005A000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      00990000009900000099B67A36FFB67A36FFE9D7C3FFE9D7C3FFB67A36FFB67A
      36FF000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000990000
      00990000000000000000000000050000005C00000092000000930000005F0000
      0007000000000000000000000000000000000000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FF000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000008E000000990000002500000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FF000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000990000
      009900000000000000000000005A000000990000009900000099000000990000
      005F000000000000000000000000000000000000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FF000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000990000
      009900000000000000000000008E000000990000002500000021000000990000
      0093000000000000000000000000000000000000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000099000000990000000000000000000000990000
      0099000000000000000000000000000000000000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000099000000990000000000000000000000990000
      0099000000000000000000000000000000000000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000099000000990000000000000000000000990000
      0099000000000000000000000000000000000000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000008E000000990000002A00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000099000000990000000000000000000000990000
      0099000000000000000000000000000000000000009900000099000000990000
      0099000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000057000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000930000005F00000007000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000099000000990000000000000000000000990000
      0099000000000000000000000000000000000000009900000099000000990000
      0099000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000004000000570000008E00000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      009900000099000000990000009900000099000000990000005F000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000099000000990000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000210000009900000093000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      00990000000000000000000000000000000000000000000000000000008E0000
      00990000002A0000002600000099000000920000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000570000
      00990000009900000099000000990000005C0000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000040000
      00570000008E0000008E0000005A000000050000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      0000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      0000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000260000009900000092000000000000
      000000000000000000000000000000000000B67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      000000000000000000000000000000000000B67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FF0000000000000000000000000000
      0000B67A36FFB67A36FFE9D7C3FFE9D7C3FFB67A36FFB67A36FF000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B67A36FFB67A36FFE9D7C3FFE9D7C3FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B67A36FFB67A36FFE9D7C3FFE9D7C3FFB67A36FFB67A36FF000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      009900000099000000990000009900000099000000990000005C000000000000
      000000000000000000000000000000000000B67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      000000000000000000000000000000000000B67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FF0000000000000000000000000000
      0000B67A36FFB67A36FFE9D7C3FFE9D7C3FFB67A36FFB67A36FF000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B67A36FFB67A36FFE9D7C3FFE9D7C3FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B67A36FFB67A36FFE9D7C3FFE9D7C3FFB67A36FFB67A36FF000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      00990000009900000099000000990000008E0000005A00000005000000000000
      000000000000000000000000000000000000B67A36FFB67A36FFE9D7C3FFE9D7
      C3FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      000000000000000000000000000000000000B67A36FFB67A36FFE9D7C3FFE9D7
      C3FFB67A36FFB67A36FF00000000000000000000000000000000B67A36FFB67A
      36FFE9D7C3FFE9D7C3FFB67A36FFB67A36FF0000000000000000000000000000
      0000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B67A36FFB67A36FFE9D7C3FFE9D7
      C3FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      000000000000000000000000000000000000B67A36FFB67A36FFE9D7C3FFE9D7
      C3FFB67A36FFB67A36FF00000000000000000000000000000000B67A36FFB67A
      36FFE9D7C3FFE9D7C3FFB67A36FFB67A36FF0000000000000000000000000000
      0000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      000000000000000000000000000000000000B67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      000000000000000000000000000000000000B67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000200000
      0063000000220000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000120C05526B48
      20C4AE7434F9AE7534FA6F4A21C7140E06560000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000610000
      0081000000810000006300000022000000000000000000000000000000000000
      000000000000000000000000000000000000000000003524108AB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FF3B28129200000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000020000000819A9A
      9ADA6A6A6AC50C0C0C8C00000081000000630000002200000000000000000000
      000000000000000000000000000000000000110B054FB67A36FFB67A36FFBC86
      46FFD6B58FFFD7B690FFBD8748FFB67A36FFB67A36FF150E0657000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000612C2C2CA3FFFF
      FFFFFFFFFFFFF1F1F1FA6C6C6CC6000000810000005E00000000000000000000
      00000000000000000000000000000000000066441EBFB67A36FFBB8545FFF7F0
      E8FFFFFFFFFFFFFFFFFFF8F2EBFFBD8748FFB67A36FF6F4A21C7000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000004300000000000000000000
      0000000000000000000000000000000000000000002000000081B3B3B3E4FFFF
      FFFFFFFFFFFFFFFFFFFF747474CA0000007F0000001600000000000000000000
      000000000000000000000000000000000000A46E31F2B67A36FFD4B28AFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFD7B690FFB67A36FFAE7534FA000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000008100000043000000000000
      00000000000000000000000000000000000000000061343434A8FFFFFFFFFFFF
      FFFFFFFFFFFFEDEDEDF90C0C0C8C0000004C0000000000000000000000000000
      000000000000000000000000000000000000A26D30F1B67A36FFD4B189FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFD6B58FFFB67A36FFAE7434F9000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000008100000081000000430000
      00000000000000000000000000000000002001010182C0C0C0E9FFFFFFFFFFFF
      FFFFFFFFFFFF606060C00000007A0000000B0000000000000000000000000000
      00000000000000000000000000000000000063421EBCB67A36FFBB8443FFF5EE
      E5FFFFFFFFFFFFFFFFFFF7F0E8FFBC8646FFB67A36FF6B4820C4000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000C00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000009900000099FFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000081262626A0030303850000
      0043000000000000000000000000000000613C3C3CADFFFFFFFFFFFFFFFFFFFF
      FFFFDFDFDFF4060606870000003B000000000000000000000000000000000000
      0000000000000000000000000000000000000F0A044AB67A36FFB67A36FFBB84
      43FFD4B189FFD4B28AFFBB8545FFB67A36FFB67A36FF120C0552000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000C07003E00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000009900000099FFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000081606060C0A0A0A0DD0404
      048600000043000000000000002002020283CACACAEDFFFFFFFFFFFFFFFFFFFF
      FFFF4D4D4DB70000007000000003000000000000000000000000000000000000
      000000000000000000000000000000000000000000002F200E83B67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FF3524108A00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004526059100000006000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000006C3C08B503010020000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000009900000099FFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000081606060C0FFFFFFFFA3A3
      A3DE050505870000004300000061454545B3FFFFFFFFFFFFFFFFFFFFFFFFCECE
      CEEE030303840000002A00000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000F0A044A6342
      1EBCA87032F9A97232FA66441EBF110B054E0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003D22048949280595000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D77610FFC66D0FF52D1903760000
      0005000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000009900000099FFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000081606060C0FFFFFFFFFFFF
      FFFFA8A8A8E00505058704040485D3D3D3F0FFFFFFFFFFFFFFFFFFFFFFFF3C3C
      3CAD000000620000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      000000000000120C05526B4820C4AE7434F9AE7534FA6F4A21C7140E06560000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000120A014BD77610FF482705940000
      0005000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D77610FFD77610FFD77610FF904F
      0BD10A0500380000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000009900000099FFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000081606060C0FFFFFFFFFFFF
      FFFFFFFFFFFFAEAEAEE25C5C5CBEFFFFFFFFFFFFFFFFFFFFFFFFBABABAE70101
      0181000000190000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      00003524108AB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FF3B28
      1292000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000AC56C0FF4D77610FF894B
      0BCC0B06003A0000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D77610FFD77610FFD77610FFD776
      10FFD37410FD492805950000000F000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000009900000099FFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000081606060C0FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF2C2C2CA40000
      0081000000810000008100000081000000810000008100000081000000800000
      003A000000000000000000000000000000000000000000000000000000000000
      000000000099000000990000000000000000000000000000000000000000110B
      054FB67A36FFB67A36FFBC8646FFD6B58FFFD7B690FFBD8748FFB67A36FFB67A
      36FF150E06570000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000562F06A1D77610FFD776
      10FFD77610FF794209C01B0F025C0000000F0000000000000000000000000000
      000000000000000000000000000000000000D77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFAF600DE6180D02560000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000009900000099FFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000081606060C0FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFC2C2C2EA0F0F0F8F0000007E0000002E0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000006644
      1EBFB67A36FFBB8545FFF7F0E8FFFFFFFFFFFFFFFFFFF8F2EBFFBD8748FFB67A
      36FF6F4A21C70000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000009050036D77610FFD776
      10FFD77610FFD77610FFD77610FFCF7210FA81470AC645260591211202650E08
      014305030029010000160000000B00000008D77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FF6C3C08B503010020000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000009900000099FFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000081606060C0FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFABABABE10909098A0000007B00000023000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000099000000990000009900000099000000990000009900000099A972
      32FAB67A36FFD4B28AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD7B690FFB67A
      36FFB07635FD0000009900000099000000990000009900000099000000990000
      00990000000000000000000000000000000000000000000000006C3B08B5D776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFC66D0FF52D1903760000
      0005000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000009900000099FFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000081606060C0FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF939393D704040485000000750000001A00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000099000000990000009900000099000000990000009900000099A871
      32FAB67A36FFD4B189FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD6B58FFFB67A
      36FFB07635FD0000009900000099000000990000009900000099000000990000
      009900000000000000000000000000000000000000000000000004020026C86E
      0FF6D77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FF904F
      0BD10A0500380000000000000000000000000000000000000000000000000000
      000000000000000000000000009900000099FFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000081606060C0FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF797979CC010101830000006F000000120000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006342
      1EBCB67A36FFBB8443FFF5EEE5FFFFFFFFFFFFFFFFFFF7F0E8FFBC8646FFB67A
      36FF6B4820C40000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000002414
      0269D77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD37410FD492805950000000F000000000000000000000000000000000000
      000000000000000000000000009900000099FFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000081606060C0FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5F5F
      5FC000000081000000670000000B000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000F0A
      044AB67A36FFB67A36FFBB8443FFD4B189FFD4B28AFFBB8545FFB67A36FFB67A
      36FF120C05520000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      00004225058ED77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFAF600DE6180D02560000000000000000000000000000
      000000000000000000000000009900000099FFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000081606060C0FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAFAFAFD474747B30000
      00810000005E0000000600000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00002F200E83B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FF3524
      108A000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      000000000001391F0484D57610FED77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFA75C0DE1140A014E0000000000000000000000000000
      000000000000000000000000009900000099FFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000081606060C0FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF2F2F2FB333333A8000000810000
      0053000000030000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000F0A044A63421EBCA26D30F1A46E31F266441EBF110B054E0000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      00000000000000000000130A014CAB5D0DE3D77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD17310FC4124048D0000000C000000000000000000000000000000000000
      000000000000000000000000009900000099FFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000081606060C0FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFE4E4E4F62424249E00000081000000470000
      0001000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000120C05526B4820C4B07635FDB076
      35FD6F4A21C7140E065600000000000000000000000000000000000000000000
      00000000000000000000000000000000000B2C180375A45A0DDFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FF874A
      0BCA080400310000000000000000000000000000000000000000000000000000
      000000000000000000000000009900000099FFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000081606060C0FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFD5D5D5F118181896000000800000003A000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000003524108AB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF3B281292000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000020A060039331C
      037D673908B197530BD6BA660EEDC96E0FF7D77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFC16A0FF22715036E0000
      0003000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000009900000099FFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000081606060C0FFFFFFFFFFFF
      FFFFFFFFFFFFC2C2C2EA0F0F0F8F0000007E0000002E00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000110B054FB67A36FFB67A36FFBC8646FFD6B58FFFD7B6
      90FFBD8748FFB67A36FFB67A36FF150E06570000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FF633608AD0201001B000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000081606060C0FFFFFFFFFFFF
      FFFFABABABE10909098A0000007B000000230000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000066441EBFB67A36FFBB8545FFF7F0E8FFFFFFFFFFFFFF
      FFFFF8F2EBFFBD8748FFB67A36FF6F4A21C70000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFA75C0DE1140A014E0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000081606060C0FFFFFFFF9393
      93D704040485000000750000001A000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A46E31F2B67A36FFD4B28AFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFD7B690FFB67A36FFAE7534FA0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D77610FFD77610FFD77610FFD776
      10FFD17310FC4124048D0000000C000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000081606060C0797979CC0101
      01830000006F0000001200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A26D30F1B67A36FFD4B189FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFD6B58FFFB67A36FFAE7434F90000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D77610FFD77610FFD77610FF874A
      0BCA080400310000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000008113131392000000810000
      00670000000B0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000063421EBCB67A36FFBB8443FFF5EEE5FFFFFFFFFFFFFF
      FFFFF7F0E8FFBC8646FFB67A36FF6B4820C40000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D77610FFC16A0FF22715036E0000
      0003000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000081000000810000005E0000
      0006000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000F0A044AB67A36FFB67A36FFBB8443FFD4B189FFD4B2
      8AFFBB8545FFB67A36FFB67A36FF120C05520000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000633608AD0201001B000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000008100000053000000030000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000002F200E83B67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF3524108A000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFB67A36FFB67A36FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB67A36FFB67A
      36FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000004700000001000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000F0A044A63421EBCA26D30F1A46E
      31F266441EBF110B054E00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFB67A36FFB67A36FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB67A36FFB67A
      36FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000E000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000E000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000E000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000E000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000E1015
      095A35441EA05E7835D4000000000000000000000000000000005F7A35D53847
      20A414190B620001001400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000020201202F3C1B977DA046F489AE
      4CFF89AE4CFF89AE4CFF0000000000000000000000000000000089AE4CFF89AE
      4CFF89AE4CFF83A74AFA3C4C21A9040602310000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000330000
      0033000000330000003300000033000000330000003300000033000000000000
      0000000000660000006600000066000000660000006600000066000000660000
      0066000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000006232D138281A448F889AE4CFF89AE4CFF89AE
      4CFF89AE4CFF89AE4CFF0000000000000000000000000000000089AE4CFF89AE
      4CFF89AE4CFF89AE4CFF89AE4CFF87AC4CFE34421D9E00000011000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000330000
      0033000000330000003300000033000000330000003300000033000000000000
      0000000000660000006600000066000000660000006600000066000000660000
      0066000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000000000000000000000000000000000000000000
      00000000000000000000212B127F89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE
      4CFF89AE4CFF89AE4CFF0000000000000000000000000000000089AE4CFF89AE
      4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF232D1483000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000330000
      0033000000330000003300000033000000330000003300000033000000000000
      0000000000660000006600000066000000660000006600000066000000660000
      0066000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000000000000000000000000000000000000000000
      00000000000000000000000000043B4A21A889AE4CFF89AE4CFF89AE4CFF89AE
      4CFF89AE4CFF89AE4CFF0000000000000000000000000000000089AE4CFF89AE
      4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF34431D9F00000002000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000330000
      0033000000330000003300000033000000330000003300000033000000000000
      0000000000660000006600000066000000660000006600000066000000660000
      0066000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000043B4A21A889AE4CFF89AE4CFF89AE
      4CFF89AE4CFF89AE4CFF0000000000000000000000000000000089AE4CFF89AE
      4CFF89AE4CFF89AE4CFF89AE4CFF37461FA30000000300000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000330000
      0033000000330000003300000033000000330000003300000033000000000000
      0000000000660000006600000066000000660000006600000066000000660000
      0066000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000000000000000000000000000000000000030D11
      075100000004000000000000000000000000000000043B4A21A889AE4CFF83A6
      48F9394920A60E110852000000000000000000000000000000000D10074F3846
      1FA381A448F889AE4CFF3B4A21A8000000040000000000000000000000000000
      00030D1107510000000500000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000330000
      0033000000330000003300000033000000330000003300000033000000000000
      0000000000660000006600000066000000660000006600000066000000660000
      0066000000000000000000000099000000990000009900000099000000990000
      00990000009900000099000000000000000000000000000000000C0F064C313E
      1B99181F0D6C00000004000000000000000000000000000000041E27117A0202
      011F000000000000000000000000000000000000000000000000000000000000
      00000102001C1F28117B0000000400000000000000000000000000000003161C
      0C68313E1B990D11075100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000330000
      0033000000330000003300000033000000330000003300000033000000000000
      0000000000660000006600000066000000660000006600000066000000660000
      0066000000000000000000000099000000990000009900000099000000990000
      00990000009900000099000000000000000000000000000000122D3A1A94313E
      1B99313E1B99181F0D6C00000004000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000003161C0C68313E
      1B99313E1B992E3B1A9500010015000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000330000
      0033000000330000003300000033000000330000003300000033000000000000
      0000000000660000006600000066000000660000006600000066000000660000
      0066000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000000000000000000000010140958313E1B99313E
      1B99313E1B99313E1B99181F0D6C000000040000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000003161C0C68313E1B99313E
      1B99313E1B99313E1B9912160A5D000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000062C381891313E1B99313E
      1B99313E1B99313E1B99313E1B990C10074F0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000B0E064A313E1B99313E1B99313E
      1B99313E1B99313E1B992D391994000000090000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000005060233313E1B99313E1B99313E
      1B99313E1B99313E1B992E3B1A96000100140000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000112E3A1A95313E1B99313E
      1B99313E1B99313E1B99313E1B99060803380000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000212B127F212B
      127F212B127F212B127F212B127F212B127F212B127F212B127F000000000000
      00004C612BBF4C612BBF4C612BBF4C612BBF4C612BBF4C612BBF4C612BBF4C61
      2BBF000000000000000089AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE
      4CFF89AE4CFF89AE4CFF000000000000000011160A5C313E1B99313E1B99313E
      1B99313E1B99313E1B99161C0C67000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000013190B62313E1B99313E
      1B99313E1B99313E1B99313E1B9913190A610000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000212B127F212B
      127F212B127F212B127F212B127F212B127F212B127F212B127F000000000000
      00004C612BBF4C612BBF4C612BBF4C612BBF4C612BBF4C612BBF4C612BBF4C61
      2BBF000000000000000089AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE
      4CFF89AE4CFF89AE4CFF00000000000000001F28117B313E1B99313E1B99313E
      1B99313E1B99313E1B9905070334000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000004060230313E1B99313E
      1B99313E1B99313E1B99313E1B99222B13800000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000212B127F212B
      127F212B127F212B127F212B127F212B127F212B127F212B127F000000000000
      00004C612BBF4C612BBF4C612BBF4C612BBF4C612BBF4C612BBF4C612BBF4C61
      2BBF000000000000000089AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE
      4CFF89AE4CFF89AE4CFF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000212B127F212B
      127F212B127F212B127F212B127F212B127F212B127F212B127F000000000000
      00004C612BBF4C612BBF4C612BBF4C612BBF4C612BBF4C612BBF4C612BBF4C61
      2BBF000000000000000089AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE
      4CFF89AE4CFF89AE4CFF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000212B127F212B
      127F212B127F212B127F212B127F212B127F212B127F212B127F000000000000
      00004C612BBF4C612BBF4C612BBF4C612BBF4C612BBF4C612BBF4C612BBF4C61
      2BBF000000000000000089AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE
      4CFF89AE4CFF89AE4CFF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000212B127F212B
      127F212B127F212B127F212B127F212B127F212B127F212B127F000000000000
      00004C612BBF4C612BBF4C612BBF4C612BBF4C612BBF4C612BBF4C612BBF4C61
      2BBF000000000000000089AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE
      4CFF89AE4CFF89AE4CFF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000212B127F212B
      127F212B127F212B127F212B127F212B127F212B127F212B127F000000000000
      00004C612BBF4C612BBF4C612BBF4C612BBF4C612BBF4C612BBF4C612BBF4C61
      2BBF000000000000000089AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE
      4CFF89AE4CFF89AE4CFF0000000000000000291C0C7A412C1399412C1399412C
      1399412C1399412C139908050236000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000006040131412C1399412C
      1399412C1399412C1399412C13992D1E0D7F0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000212B127F212B
      127F212B127F212B127F212B127F212B127F212B127F212B127F000000000000
      00004C612BBF4C612BBF4C612BBF4C612BBF4C612BBF4C612BBF4C612BBF4C61
      2BBF000000000000000089AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE
      4CFF89AE4CFF89AE4CFF0000000000000000170F075B412C1399412C1399412C
      1399412C1399412C13991E140969000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001B120864412C1399412C
      1399412C1399412C1399412C1399191107600000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000006040131412C1399412C1399412C
      1399412C1399412C13993F2B1397010000160000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000133D291295412C1399412C
      1399412C1399412C1399412C1399080502360000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000639271190412C1399412C
      1399412C1399412C1399412C1399100B054E0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000E090449412C1399412C1399412C
      1399412C1399412C13993B281292000000080000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000002D1E0D7F2D1E
      0D7F2D1E0D7F2D1E0D7F2D1E0D7F2D1E0D7F2D1E0D7F2D1E0D7F000000000000
      000066441EBF66441EBF66441EBF66441EBF66441EBF66441EBF66441EBF6644
      1EBF0000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF000000000000000000000000140E0656412C1399412C
      1399412C1399412C13991E140968000000030000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000021C130865412C1399412C
      1399412C1399412C1399170F075B000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      00000000000000000000000000000000000000000000000000002D1E0D7F2D1E
      0D7F2D1E0D7F2D1E0D7F2D1E0D7F2D1E0D7F2D1E0D7F2D1E0D7F000000000000
      000066441EBF66441EBF66441EBF66441EBF66441EBF66441EBF66441EBF6644
      1EBF0000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF000000000000000000000000000000103C281293412C
      1399412C13991E14096800000003000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000021C130865412C
      1399412C13993D29129500000013000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      00000000000000000000000000000000000000000000000000002D1E0D7F2D1E
      0D7F2D1E0D7F2D1E0D7F2D1E0D7F2D1E0D7F2D1E0D7F2D1E0D7F000000000000
      000066441EBF66441EBF66441EBF66441EBF66441EBF66441EBF66441EBF6644
      1EBF0000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF000000000000000000000000000000000E090449412C
      13991E14096800000003000000000000000000000000000000042E1F0E820302
      0125000000000000000000000000000000000000000000000000000000000000
      00000302002230200E8300000007000000000000000000000000000000021C13
      0865412C1399110B054E00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000002D1E0D7F2D1E
      0D7F2D1E0D7F2D1E0D7F2D1E0D7F2D1E0D7F2D1E0D7F2D1E0D7F000000000000
      000066441EBF66441EBF66441EBF66441EBF66441EBF66441EBF66441EBF6644
      1EBF0000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF0000000000000000000000000000000000000003100B
      054D00000003000000000000000000000000000000044E3517A8B67A36FFB277
      35FC553919AF160F065A00000000000000000000000000000000150E06575137
      18ABAF7634FAB67A36FF5B3D1BB4000000070000000000000000000000000000
      0002100A044C0000000400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000002D1E0D7F2D1E
      0D7F2D1E0D7F2D1E0D7F2D1E0D7F2D1E0D7F2D1E0D7F2D1E0D7F000000000000
      000066441EBF66441EBF66441EBF66441EBF66441EBF66441EBF66441EBF6644
      1EBF0000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      000000000000000000000000000000000003493116A3B67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FF5B3D1BB40000000700000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000002D1E0D7F2D1E
      0D7F2D1E0D7F2D1E0D7F2D1E0D7F2D1E0D7F2D1E0D7F2D1E0D7F000000000000
      000066441EBF66441EBF66441EBF66441EBF66441EBF66441EBF66441EBF6644
      1EBF0000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      0000000000000000000000000002462F159FB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FF5B3D1BB400000007000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000002D1E0D7F2D1E
      0D7F2D1E0D7F2D1E0D7F2D1E0D7F2D1E0D7F2D1E0D7F2D1E0D7F000000000000
      000066441EBF66441EBF66441EBF66441EBF66441EBF66441EBF66441EBF6644
      1EBF0000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      000000000000000000002B1D0D7DB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FF33220F87000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000002D1E0D7F2D1E
      0D7F2D1E0D7F2D1E0D7F2D1E0D7F2D1E0D7F2D1E0D7F2D1E0D7F000000000000
      000066441EBF66441EBF66441EBF66441EBF66441EBF66441EBF66441EBF6644
      1EBF0000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      000000000000000000000000000C3A271191B07634FBB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFAA7233F72C1E0D7F00000006000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000004020127442E149DA87032F5B67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFA36E31F23C2812930201001D0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000E150E
      0657422C139A754E23CC00000000000000000000000000000000754F23CD422C
      139A130D05540000000B00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000350000003500000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000009101B
      4A992A46BEF42B46C0F5111D509E0000000B0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0035000000660000006600000035000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000101A47962E4C
      D0FF2E4CD0FF2E4CD0FF2E4CD0FF111D509E0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000350000
      0066000000660000006600000066000000350000000000000000000000000000
      00000000000000000000000000000000000000000000000000002842B5EE2E4C
      D0FF2E4CD0FF2E4CD0FF2E4CD0FF2B46C0F50000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000035000000660000
      0066000000660000006600000066000000590000002700000000000000000000
      00000000000000000000000000000000000000000000000000002943B6EF2E4C
      D0FF2E4CD0FF2E4CD0FF2E4CD0FF2A46BEF40000000000000000000000000000
      000000000000000000000000000000000000000000001919197A717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF323232AA000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000B0909094A2121218B3B3B
      3BB94F4F4FD55C5C5CE75B5B5BE54D4D4DD3363636B01919197A020202280000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000003500000066000000660000
      00660000006600000066000000590000004C0000004C00000027000000000000
      000000000000000000000000000000000000000000000000000014225CAA2E4C
      D0FF2E4CD0FF2E4CD0FF2E4CD0FF162566B20000000000000000000000000000
      0000000000000000000000000000000000000000000008080847717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF434343C4000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000616161EC717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FE2B2B
      2B9E0000000D0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000350000006600000066000000660000
      006600000066000000590000004C0000004C0000004C0000004C000000270000
      0000000000000000000000000000000000000000000000000000060B1E622E4C
      D0FF2E4CD0FF2E4CD0FF2E4CD0FF080D246A0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000575757DF717171FF717171FF3B3B3BB90000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000101
      011E1E1E1E84474747CA656565F1717171FF6F6F6FFD5E5E5EE83C3C3CBB1515
      156F000000100000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF464646C80000000800000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000035000000660000006600000066000000660000
      0066000000590000004C0000004C0000004C0000004C0000004C0000004C0000
      00270000000000000000000000000000000000000000000000000000021B2E4C
      CEFE2E4CD0FF2E4CD0FF2E4CD0FF000103220000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000323232A9717171FF717171FF626262EE0000000100000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000131313686A6A
      6AF7717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF5F5F5FEA0A0A0A4D00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF626262EE252525920707
      07430000000F00000000000000030101011E101010615B5B5BE5717171FF7171
      71FF717171FF1818187700000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000003500000066000000660000006600000066000000660000
      00590000004C0000004C0000004C0000004C0000004C0000004C0000004C0000
      004C000000270000000000000000000000000000000000000000000000001F33
      8CD12E4CD0FF2E4CD0FF213796D9000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000017171774717171FF717171FF717171FF0202022600000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000010101061717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF6D6D6DFB09090949000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001C1C1C800000000A000000000000
      000000000000000000000000000000000000000000000303032E6F6F6FFD7171
      71FF717171FF4D4D4DD300000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000350000006600000066000000660000006600000066000000590000
      004C0000004C0000004C0000004C0000004C0000004C0000004C0000004C0000
      004C0000004C0000002700000000000000000000000000000000000000000D15
      3B892E4CD0FF2E4CD0FF0F184291000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000606063E717171FF717171FF717171FF0E0E0E5C00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000013646464F0717171FF7171
      71FF717171FF353535AF05050538000000060000000202020229252525927171
      71FE717171FF717171FF5C5C5CE50000000A0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004D4D4DD27171
      71FF717171FF6E6E6EFC00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00350000006600000066000000660000006600000066000000590000004C0000
      004C0000004C0000004C0000004C0000004C0000004C0000004C0000004C0000
      004C0000004C0000004C00000027000000000000000000000000000000000305
      0D412E4CD0FF2E4CD0FF03061149000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000B6F6F6FFC717171FF717171FF2424249100000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000015151570717171FF717171FF7171
      71FF2A2A2A9D0000000100000000000000000000000000000000000000001919
      1979717171FF717171FF717171FF101010620000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000025B5B5BE57171
      71FF717171FF696969F600000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000350000
      006600000066000000660000006600000066000000590000004C0000004C0000
      004C0000004C0000004C0000004C0000004C0000004C0000004C0000004C0000
      004C0000004C0000004C0000004C000000270000000000000000000000000000
      00062A45BBF22B47C2F600000009000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000004C4C4CD1717171FF717171FF454545C700000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000393939B5717171FF717171FF6767
      67F4000000110000000000000000000000000000000000000000000000000000
      00055B5B5BE5717171FF717171FF353535AE0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000016161671717171FF7171
      71FF717171FF393939B500000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000035000000660000
      0066000000660000006600000066000000590000004C0000004C0000004C0000
      004C0000004C0000004C0000004C0000004C0000004C0000004C0000004C0000
      004C0000004C0000004C0000004C0000004C0000002700000000000000000000
      0000152462AF18276BB700000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000002929299B717171FF717171FF6A6A6AF700000006000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000575757E0717171FF717171FF3636
      36B0000000000000000000000000000000000000000000000000000000000000
      00002A2A2A9B717171FF717171FF575757DF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000101011A2D2D2DA1717171FF717171FF7171
      71FF6F6F6FFD0505053A00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000003500000066000000660000
      00660000006600000066000000590000004C0000004C0000004C0000004C0000
      004C0000004C0000004C0000004C0000004C0000004C0000004C0000004C0000
      004C0000004C0000004C0000004C0000004C0000004C00000027000000000000
      0000070C2167090E276F00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000011111166717171FF717171FF717171FF04040434000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000686868F5717171FF717171FF1D1D
      1D82000000000000000000000000000000000000000000000000000000000000
      000016161673717171FF717171FF6B6B6BF90000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000101011D24242490696969F6717171FF717171FF717171FF7171
      71FF181818770000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000003100000066000000660000
      006600000066000000590000004C0000004C0000004C0000004C0000004C0000
      004C0000004C0000004C0000004C0000004C0000004C0000004C0000004C0000
      004C0000004C0000004C0000004C0000004C0000004C0000004C000000270000
      00000001031F0101042700000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000003030330717171FF717171FF717171FF1313136A000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000006F6F6FFD717171FF717171FF1515
      156F000000000000000000000000000000000000000000000000000000000000
      000011111163717171FF717171FF717171FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000005C5C5CE75C5C5CE75C5C5CE75C5C5CE75C5C5CE75C5C
      5CE75C5C5CE76D6D6DFB717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000031000000660000
      0066000000590000004C0000004C0000004C0000004C0000004C0000004C0000
      004C0000004C0000004C020202526E6E6EB8ECECECF7EEEEEEF8747474BB0303
      03540000004C0000004C0000004C0000004C0000004C0000004C0000004C0000
      0027000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000004686868F5717171FF717171FF2C2C2C9F000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF1212
      1268000000000000000000000000000000000000000000000000000000000000
      000010101060717171FF717171FF717171FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000310000
      00590000004C0000004C0000004C0000004C0000004C0000004C0000004C0000
      004C0000004C0000004C6A6A6AB5FFFFFFFF969696FF969696FFFFFFFFFF7474
      74BB0000004C0000004C0000004C0000004C0000004C0000004C0000004C0000
      0025000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000424242C3717171FF717171FF4F4F4FD5000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF1212
      1268000000000000000000000000000000000000000000000000000000000000
      000010101060717171FF717171FF717171FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000001010118010101180101011824242490717171FF7171
      71FF717171FF717171FF717171FF454545C70B0B0B5100000001000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00250000004C0000004C0000004C0000004C0000004C0000004C0000004C0000
      004C0000004C0000004CE2E2E2F3969696FF323232FF323232FF969696FFEEEE
      EEF80000004C0000004C0000004C0000004C0000004C0000004C000000250000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002222228D717171FF717171FF6F6F6FFD0000000E0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF1212
      1268000000000000000000000000000000000000000000000000000000000000
      000010101060717171FF717171FF717171FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000007070742717171FE717171FF7171
      71FF717171FF484848CB0606063F000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000250000004C0000004C0000004C0000004C0000004C0000004C0000
      004C0000004C0000004C808080F4323232FF323232FF323232FF9C9C9CFFECEC
      ECF70000004C0000004C0000004C0000004C0000004C00000025000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000D0D0D58717171FF717171FF717171FF070707420000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF1212
      1268000000000000000000000000000000000000000000000000000000000000
      000010101060717171FF717171FF717171FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003E3E3EBD717171FF717171FF7171
      71FF252525920000000200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000250000004C0000004C0000004C0000004C0000004C0000
      004C0000004C000000961B1B1BF0323232FF323232FF9C9C9CFFFFFFFFFF6E6E
      6EB80000004C0000004C0000004C0000004C0000002500000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000001010121717171FF717171FF717171FF181818780000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF1212
      1268000000000000000000000000000000000000000000000000000000000000
      000010101060717171FF717171FF717171FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000656565F1717171FF717171FF6262
      62EE000000060000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000250000004C0000004C0000004C0000004C0000
      004C00000096000000DB000000DC1B1B1BF0858585F3E1E1E1F36A6A6AB50202
      02520000004C0000004C0000004C000000250000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000005F5F5FEA717171FF717171FF353535AE0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF1212
      1268000000000000000000000000000000000000000000000000000000000000
      000010101060717171FF717171FF717171FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000686868F6717171FF717171FF5050
      50D6000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000250000004C0000004C0000004C0000
      0096000000DB000000DB000000DB000000910000004C0000004C0000004C0000
      004C0000004C0000004C00000025000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000393939B5717171FF717171FF5A5A5AE30000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF1212
      1268000000000000000000000000000000000000000000000000000000000000
      000010101060717171FF717171FF717171FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004D4D4DD2717171FF717171FF6F6F
      6FFC040404330000000000000000000000000000000000000000000000000000
      0000000000090000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000250000004C000000960000
      00DB000000DB000000DB000000910000004C0000004C0000004C0000004C0000
      004C0000004C0000002500000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000001C1C1C7F717171FF717171FF717171FF0101
      011A000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF1212
      1268000000000000000000000000000000000000000000000000000000000000
      000010101060717171FF717171FF717171FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001515156E717171FF717171FF7171
      71FF636363EE1111116600000010000000000000000000000002020202251515
      1570575757DF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000007F000000DB0000
      00DB000000DB000000910000004C0000004C0000004C0000004C0000004C0000
      004C000000250000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000009090949717171FF717171FF717171FF0B0B
      0B50000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF1212
      1268000000000000000000000000000000000000000000000000000000000000
      000010101060717171FF717171FF717171FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000002353535AF717171FF7171
      71FF717171FF717171FF707070FE626262ED606060EC6D6D6DFA717171FF7171
      71FF717171FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000069000000CC000000D30000
      00DB000000910000004C0000004C0000004C0000004C0000004C0000004C0000
      0025000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000008080847717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF3A3A3AB800000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF1212
      1268000000000000000000000000000000000000000000000000000000000000
      000010101060717171FF717171FF717171FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000072929299B7171
      71FE717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000069000000CC000000CC000000CC0000
      00790000004C0000004C0000004C0000004C0000004C0000004C000000250000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000014717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF606060EB00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF1212
      1268000000000000000000000000000000000000000000000000000000000000
      000010101060717171FF717171FF717171FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000303
      032E2222228C424242C45C5C5CE6696969F66A6A6AF7636363EF505050D63333
      33AC111111640000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000069000000CC000000CC000000CC000000630000
      0000000000250000004C0000004C0000004C0000004C00000025000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000069000000CC000000CC000000CC00000063000000000000
      000000000000000000250000004C0000004C0000002500000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000063000000CC000000CC0000006300000000000000000000
      0000000000000000000000000025000000250000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000063000000630000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000089AE4CFF89AE
      4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE
      4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000089AE4CFF89AE
      4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE
      4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000099000000990000009900000099000000990000
      009900000099000000990000009900000099000000990000009989AE4CFF89AE
      4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE
      4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF0000000000000000000000630000
      0081000000810000007D00000009000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000006600000081000000810000006B0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000630000
      0081000000810000007D00000009000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000006600000081000000810000006B0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000099000000990000009900000099000000990000
      009900000099000000990000009900000099000000990000009989AE4CFF89AE
      4CFF89AE4CFF89AE4CFF89AE4CFFC6D8A9FFC6D8A9FF89AE4CFF89AE4CFF89AE
      4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF0000000000000000000000310000
      0081000000810000008100000036000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0019000000810000008100000081000000390000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000310000
      0081000000810000008100000036000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0019000000810000008100000081000000390000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000089AE4CFF89AE
      4CFF89AE4CFF89AE4CFFC6D8A9FFFFFFFFFFFFFFFFFFC6D8A9FF89AE4CFF89AE
      4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF0000000000000000000000060000
      007B000000810000008100000068000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      004F00000081000000810000007E0000000B0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000060000
      007B000000810000008100000068000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      004F00000081000000810000007E0000000B0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000089AE4CFF89AE
      4CFF89AE4CFFC6D8A9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC6D8A9FF89AE
      4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF0000000000000000000000000000
      004F000000810000008100000081000000180000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000080000
      007C000000810000008100000058000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      004F000000810000008100000081000000180000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000080000
      007C000000810000008100000058000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000089AE4CFF89AE
      4CFFC6D8A9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC6D8
      A9FF89AE4CFF89AE4CFF89AE4CFF89AE4CFF0000000000000000000000000000
      001E000000810000008100000081000000490000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000380000
      0081000000810000008100000026000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      001E000000810000008100000081000000490000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000380000
      0081000000810000008100000026000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF6D6D6DFA555555DE3232
      32AA0D0D0D590000000600000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000089AE4CFF89AE
      4CFFC2D5A3FFFFFFFFFFFFFFFFFFC2D5A3FFC2D5A3FFFFFFFFFFFFFFFFFFFFFF
      FFFFC6D8A9FF89AE4CFF89AE4CFF89AE4CFF0000000000000000000000000000
      00000000006E0000008100000081000000770000000400000000000000000000
      00000000000000000000000000000000000000000000000000010000006D0000
      0081000000810000007400000002000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000006E0000008100000081000000770000000400000000000000000000
      00000000000000000000000000000000000000000000000000010000006D0000
      0081000000810000007400000002000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF555555DD07070740000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000004C0000004C000000000000
      00000000004C0000004C0000004C0000004C000000000000000089AE4CFF89AE
      4CFF89AE4CFFC2D5A3FFC2D5A3FF89AE4CFF89AE4CFFC2D5A3FFFFFFFFFFFFFF
      FFFFFFFFFFFFC6D8A9FF89AE4CFF89AE4CFF0000000000000000000000000000
      00000000003C0000008100000081000000810000003C00000020000000200000
      0020000000200000002000000020000000200000002000000034000000810000
      0081000000810000004400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000003C0000008100000081000000810000003C00000020000000200000
      0020000000200000002000000020000000200000002000000034000000810000
      0081000000810000004400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF6D6D6DFA070707440000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000004C0000004C000000000000
      00000000004C0000004C0000004C0000004C000000000000000089AE4CFF89AE
      4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFFC2D5A3FFFFFF
      FFFFFFFFFFFFC2D5A3FF89AE4CFF89AE4CFF0000000000000000000000000000
      00000000000C0000007F00000081000000810000008100000081000000810000
      0081000000810000008100000081000000810000008100000081000000810000
      0081000000810000001300000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000C0000007F00000081000000810000008100000081000000810000
      0081000000810000008100000081000000810000008100000081000000810000
      0081000000810000001300000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF7171
      71FF717171FF2828289728282897282828972F2F2FA65A5A5AE3717171FF7171
      71FF717171FF717171FF717171FF5A5A5AE40000000900000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000089AE4CFF89AE
      4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFFC2D5
      A3FFC2D5A3FF89AE4CFF89AE4CFF89AE4CFF0000000000000000000000000000
      0000000000000000005A00000081000000810000008100000081000000810000
      0081000000810000008100000081000000810000008100000081000000810000
      0081000000630000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000005A00000081000000810000008100000081000000810000
      0081000000810000008100000081000000810000008100000081000000810000
      0081000000630000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF7171
      71FF717171FF00000000000000000000000000000000000000041C1C1C807171
      71FF717171FF717171FF717171FF717171FF0C0C0C5600000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000089AE4CFF89AE
      4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE
      4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF0000000000000000000000000000
      0000000000000000002900000081000000810000008100000054000000180000
      0018000000180000001800000018000000180000004D00000081000000810000
      0081000000310000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000002900000081000000810000008100000054000000180000
      0018000000180000001800000018000000180000004D00000081000000810000
      0081000000310000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF7171
      71FF717171FF0000000000000000000000000000000000000000000000004A4A
      4ACE717171FF717171FF717171FF717171FF2222228C00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000004C0000004C000000000000
      00000000004C0000004C0000004C0000004C000000000000000089AE4CFF89AE
      4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE
      4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF0000000000000000000000000000
      0000000000000000000300000076000000810000008100000070000000000000
      0000000000000000000000000000000000000000006700000081000000810000
      007B000000060000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000300000076000000810000008100000070000000000000
      0000000000000000000000000000000000000000006700000081000000810000
      007B000000060000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF7171
      71FF717171FF0000000000000000000000000000000000000000000000002929
      2999717171FF717171FF717171FF717171FF2A2A2A9B00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000004C0000004C000000000000
      00000000004C0000004C0000004C0000004C000000000000000089AE4CFF89AE
      4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF89AE
      4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF0000000000000000000000000000
      0000000000000000000000000047000000810000008100000081000000170000
      00000000000000000000000000000000000F0000008100000081000000810000
      004F000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000047000000810000008100000081000000170000
      00000000000000000000000000000000000F0000008100000081000000810000
      004F000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF7171
      71FF717171FF0000000000000000000000000000000000000000000000003D3D
      3DBB717171FF717171FF717171FF717171FF1D1D1D8200000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000015000000810000008100000081000000420000
      0000000000000000000000000000000000390000008100000081000000810000
      001E000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000015000000810000008100000081000000420000
      0000000000000000000000000000000000390000008100000081000000810000
      001E000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF7171
      71FF717171FF0000000000000000000000000000000000000000131313687171
      71FE717171FF717171FF717171FF717171FE0404043100000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000006500000081000000810000006B0000
      00000000000000000000000000000000006300000081000000810000006D0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000006500000081000000810000006B0000
      00000000000000000000000000000000006300000081000000810000006D0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF7171
      71FF717171FF1C1C1C801C1C1C801D1D1D82252525924B4B4BD0717171FF7171
      71FF717171FF717171FF717171FF282828980000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000004C0000004C000000000000
      00000000004C0000004C0000004C0000004C0000004C0000004C0000004C0000
      004C0000004C0000004C0000004C0000004C0000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000330000008100000081000000810000
      001300000000000000000000000C0000008000000081000000810000003C0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000330000008100000081000000810000
      001300000000000000000000000C0000008000000081000000810000003C0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF6D6D6DFA20202089000000040000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000004C0000004C000000000000
      00000000004C0000004C0000004C0000004C0000004C0000004C0000004C0000
      004C0000004C0000004C0000004C0000004C0000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000070000007C00000081000000810000
      003D00000000000000000000003400000081000000810000007F0000000C0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000070000007C00000081000000810000
      003D00000000000000000000003400000081000000810000007F0000000C0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF5353
      53DA181818760101011F00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000005100000081000000810000
      006600000000000000000000005E00000081000000810000005A000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000005100000081000000810000
      006600000000000000000000005E00000081000000810000005A000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF474747CA0505053B00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000002000000081000000810000
      00810000000F000000090000007F000000810000008100000028000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000002000000081000000810000
      00810000000F000000090000007F000000810000008100000028000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF7171
      71FF717171FF01010118010101180101011A0606063E393939B6717171FF7171
      71FF717171FF6D6D6DFB0D0D0D56000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000004C0000004C000000000000
      00000000004C0000004C0000004C0000004C0000004C0000004C0000004C0000
      004C0000004C0000004C0000004C0000004C0000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000010000006F000000810000
      0081000000370000002F00000081000000810000007600000003000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000010000006F000000810000
      0081000000370000002F00000081000000810000007600000003000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF7171
      71FF717171FF00000000000000000000000000000000000000044A4A4ACE7171
      71FF717171FF717171FF696969F6010101220000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000004C0000004C000000000000
      00000000004C0000004C0000004C0000004C0000004C0000004C0000004C0000
      004C0000004C0000004C0000004C0000004C0000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000003E000000810000
      0081000000620000005A00000081000000810000004700000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000003E000000810000
      0081000000620000005A00000081000000810000004700000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF7171
      71FF717171FF00000000000000000000000000000000000000001E1E1E857171
      71FF717171FF717171FF717171FF202020880000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000E000000800000
      0081000000800000007D00000081000000810000001500000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000E000000800000
      0081000000800000007D00000081000000810000001500000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF7171
      71FF717171FF00000000000000000000000000000000000000002929299A7171
      71FF717171FF717171FF717171FF373737B20000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000005D0000
      0081000000810000008100000081000000650000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000005D0000
      0081000000810000008100000081000000650000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF7171
      71FF717171FF000000000000000000000000000000050F0F0F5F6B6B6BF87171
      71FF717171FF717171FF717171FF303030A70000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000004C0000004C000000000000
      00000000004C0000004C0000004C0000004C0000004C0000004C0000004C0000
      004C0000004C0000004C0000004C0000004C0000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000002A0000
      0081000000810000008100000081000000330000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000002A0000
      0081000000810000008100000081000000330000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF7171
      71FF717171FF4B4B4BCF4B4B4BCF505050D6686868F5717171FF717171FF7171
      71FF717171FF717171FF717171FF111111650000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000004C0000004C000000000000
      00000000004C0000004C0000004C0000004C0000004C0000004C0000004C0000
      004C0000004C0000004C0000004C0000004C0000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000030000
      007700000081000000810000007C000000070000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000030000
      007700000081000000810000007C000000070000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF4A4A4ACE000000070000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000271B0C78B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FF271B0C780000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000033220F8833220F880000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF373737B300000015000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000271B0C78B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF271B0C78000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000033220F88B67A36FFB67A36FF3322
      0F88000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF6A6A6AF75E5E5EE9454545C82222
      228E050505360000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000271B0C78B67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FF271B0C7800000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000033220F88B67A36FFB67A36FFB67A36FFB67A
      36FF33220F880000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000271B0C78B67A36FFB67A36FFB67A36FFB67A
      36FF271B0C780000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000033220F88B67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FF33220F8800000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000271B0C78B67A36FFB67A36FF271B
      0C78000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000033220F88B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF33220F88000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000271B0C78271B0C780000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000033220F88B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FF33220F880000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000003232
      32FF323232FF323232FF323232FF323232FF323232FF323232FF323232FF3232
      32FF323232FF323232FF323232FF323232FF323232FF323232FF323232FF3232
      32FF323232FF323232FF323232FF323232FF323232FF323232FF323232FF3232
      32FF323232FF323232FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000003232
      32FF323232FF323232FF323232FF323232FF323232FF323232FF323232FF3232
      32FF323232FF323232FF323232FF323232FF323232FF323232FF323232FF3232
      32FF323232FF323232FF323232FF323232FF323232FF323232FF323232FF3232
      32FF323232FF323232FF0000000000000000717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF00000000000000000000000000000000000000000000
      0000484848CC717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF4E4E4ED4000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000003232
      32FF323232FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000323232FF323232FF0000000000000000717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF00000000000000000000000000000000000000000000
      0000717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000003232
      32FF323232FF0000000000000000000000000000000000000000000000000000
      0000323232FF0000000000000000000000000000000000000000000000000000
      0000323232FF0000000000000000000000000000000000000000000000000000
      0000323232FF323232FF0000000000000000717171FF717171FFFFFFFFFFFFFF
      FFFF717171FF717171FFFFFFFFFFFFFFFFFF717171FF717171FFFFFFFFFFFFFF
      FFFF717171FF717171FFFFFFFFFFFFFFFFFF717171FF717171FFFFFFFFFFFFFF
      FFFF717171FF717171FFFFFFFFFFFFFFFFFF717171FF717171FFFFFFFFFFFFFF
      FFFF717171FF717171FF00000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000003232
      32FF323232FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000323232FF323232FF0000000000000000717171FF717171FFFFFFFFFFFFFF
      FFFF717171FF717171FFFFFFFFFFFFFFFFFF717171FF717171FFFFFFFFFFFFFF
      FFFF717171FF717171FFFFFFFFFFFFFFFFFF717171FF717171FFFFFFFFFFFFFF
      FFFF717171FF717171FFFFFFFFFFFFFFFFFF717171FF717171FFFFFFFFFFFFFF
      FFFF717171FF717171FF00000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000003232
      32FF323232FF0000000000000000000000000000000000000000000000000000
      0000323232FF0000000000000000000000000000000000000000000000000000
      0000323232FF0000000000000000000000000000000000000000000000000000
      0000323232FF323232FF0000000000000000717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF00000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000003232
      32FF323232FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000323232FF323232FF0000000000000000717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF00000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000003232
      32FF323232FF00000000323232FF00000000323232FF00000000323232FF0000
      0000323232FF00000000323232FF00000000323232FF00000000323232FF0000
      0000323232FF00000000323232FF00000000323232FF00000000323232FF0000
      0000323232FF323232FF0000000000000000717171FF717171FFFFFFFFFFFFFF
      FFFF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FFB6B6B6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF717171FF717171FF00000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000003232
      32FF323232FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000323232FF323232FF0000000000000000717171FF717171FFFFFFFFFFFFFF
      FFFF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FFB6B6B6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF717171FF717171FF00000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000003232
      32FF323232FF0000000000000000000000000000000000000000000000000000
      0000323232FF0000000000000000000000000000000000000000000000000000
      0000323232FF0000000000000000000000000000000000000000000000000000
      0000323232FF323232FF0000000000000000717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FFBBBBBBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF717171FF717171FF00000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000003232
      32FF323232FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000323232FF323232FF0000000000000000717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FFBBBBBBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF717171FF717171FF00000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000003232
      32FF323232FF0000000000000000000000000000000000000000000000000000
      0000323232FF0000000000000000000000000000000000000000000000000000
      0000323232FF0000000000000000000000000000000000000000000000000000
      0000323232FF323232FF0000000000000000717171FF717171FFFFFFFFFFFFFF
      FFFF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FFBBBBBBFFBBBBBBFF717171FF717171FF717171FF717171FF7171
      71FF717171FFBBBBBBFFFFFFFFFFFFFFFFFFFFFFFFFFB6B6B6FFB6B6B6FFFFFF
      FFFF717171FF717171FF00000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000003232
      32FF323232FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000323232FF323232FF0000000000000000717171FF717171FFFFFFFFFFFFFF
      FFFF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FFBBBBBBFFFFFFFFFFFFFFFFFFBBBBBBFF717171FF717171FF717171FF7171
      71FFBBBBBBFFFFFFFFFFFFFFFFFFFFFFFFFFB6B6B6FF717171FF717171FFB6B6
      B6FF717171FF717171FF00000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000003232
      32FF323232FF00000000323232FF00000000323232FF00000000323232FF0000
      0000323232FF00000000323232FF00000000323232FF00000000323232FF0000
      0000323232FF00000000323232FF00000000323232FF00000000323232FF0000
      0000323232FF323232FF0000000000000000717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FFBBBB
      BBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBBBBBBFF717171FF717171FFBBBB
      BBFFFFFFFFFFFFFFFFFFFFFFFFFFB6B6B6FF717171FF717171FF717171FF7171
      71FF717171FF717171FF00000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000003232
      32FF323232FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000323232FF323232FF0000000000000000717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FFBBBBBBFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBBBBBBFFBBBBBBFFFFFF
      FFFFFFFFFFFFFFFFFFFFB6B6B6FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF00000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000003232
      32FF323232FF0000000000000000000000000000000000000000000000000000
      0000323232FF0000000000000000000000000000000000000000000000000000
      0000323232FF0000000000000000000000000000000000000000000000000000
      0000323232FF323232FF0000000000000000717171FF717171FFFFFFFFFFFFFF
      FFFF717171FF717171FF717171FF717171FF717171FFBBBBBBFFFFFFFFFFFFFF
      FFFFFFFFFFFFB6B6B6FFB6B6B6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFB6B6B6FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF00000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000003232
      32FF323232FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000323232FF323232FF0000000000000000717171FF717171FFFFFFFFFFFFFF
      FFFF717171FF717171FF717171FF717171FFBBBBBBFFFFFFFFFFFFFFFFFFFFFF
      FFFFB6B6B6FF717171FF717171FFB6B6B6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFB6B6B6FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF00000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000003232
      32FF323232FF0000000000000000000000000000000000000000000000000000
      0000323232FF0000000000000000000000000000000000000000000000000000
      0000323232FF0000000000000000000000000000000000000000000000000000
      0000323232FF323232FF0000000000000000717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FFBBBBBBFFFFFFFFFFFFFFFFFFFFFFFFFFB6B6
      B6FF717171FF717171FF717171FF717171FFB6B6B6FFFFFFFFFFFFFFFFFFB6B6
      B6FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF00000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF7C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C
      7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C
      7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB2B67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000003232
      32FF323232FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000323232FF323232FF0000000000000000717171FF717171FF717171FF7171
      71FF717171FF717171FFBBBBBBFFFFFFFFFFFFFFFFFFFFFFFFFFB6B6B6FF7171
      71FF717171FF717171FF717171FF717171FF717171FFB6B6B6FFB6B6B6FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF00000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF7C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C
      7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C
      7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB2B67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000003232
      32FF323232FF00000000323232FF00000000323232FF00000000323232FF0000
      0000323232FF00000000323232FF00000000323232FF00000000323232FF0000
      0000323232FF00000000323232FF00000000323232FF00000000323232FF0000
      0000323232FF323232FF0000000000000000717171FF717171FFFFFFFFFFFFFF
      FFFF717171FF717171FFB6B6B6FFFFFFFFFFFFFFFFFFB6B6B6FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF00000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000484848CC7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF7C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C
      7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C
      7CB2B67A36FFB67A36FF7C7C7CB27C7C7CB2B67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000003232
      32FF323232FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000323232FF323232FF0000000000000000717171FF717171FFFFFFFFFFFFFF
      FFFF717171FF717171FF717171FFB6B6B6FFB6B6B6FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF00000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF1A1A1A7C000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF7C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C
      7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C
      7CB2B67A36FFB67A36FF7C7C7CB27C7C7CB2B67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000003232
      32FF323232FF0000000000000000000000000000000000000000000000000000
      0000323232FF0000000000000000000000000000000000000000000000000000
      0000323232FF0000000000000000000000000000000000000000000000000000
      0000323232FF323232FF0000000000000000717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF00000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF717171FF717171FF717171FF717171FF1A1A1A7C00000000000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF7C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C
      7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C
      7CB2B67A36FFB67A36FF7C7C7CB27C7C7CB2B67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000003232
      32FF323232FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000323232FF323232FF0000000000000000717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF00000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF717171FF717171FF717171FF1A1A1A7C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF7C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C
      7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C
      7CB2B67A36FFB67A36FF7C7C7CB27C7C7CB2B67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000003232
      32FF323232FF0000000000000000000000000000000000000000000000000000
      0000323232FF0000000000000000000000000000000000000000000000000000
      0000323232FF0000000000000000000000000000000000000000000000000000
      0000323232FF323232FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF717171FF717171FF1A1A1A7C000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF7C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C
      7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C
      7CB2B67A36FFB67A36FF7C7C7CB27C7C7CB2B67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FF2A1D0D7C00000000000000000000000000000000000000003232
      32FF323232FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000323232FF323232FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF717171FF1A1A1A7C00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF7C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C
      7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C
      7CB2B67A36FFB67A36FF7C7C7CB27C7C7CB2B67A36FFB67A36FFB67A36FFB67A
      36FF2A1D0D7C0000000000000000000000000000000000000000000000003232
      32FF323232FF323232FF323232FF323232FF323232FF323232FF323232FF3232
      32FF323232FF323232FF323232FF323232FF323232FF323232FF323232FF3232
      32FF323232FF323232FF323232FF323232FF323232FF323232FF323232FF3232
      32FF323232FF323232FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF1A1A1A7C0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF7C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C
      7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C
      7CB2B67A36FFB67A36FF7C7C7CB27C7C7CB2B67A36FFB67A36FFB67A36FF2A1D
      0D7C000000000000000000000000000000000000000000000000000000003232
      32FF323232FF323232FF323232FF323232FF323232FF323232FF323232FF3232
      32FF323232FF323232FF323232FF323232FF323232FF323232FF323232FF3232
      32FF323232FF323232FF323232FF323232FF323232FF323232FF323232FF3232
      32FF323232FF323232FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000434343C4717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF1A1A
      1A7C000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FFB67A36FF7C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C
      7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C
      7CB2B67A36FFB67A36FF7C7C7CB27C7C7CB2B67A36FFB67A36FF2A1D0D7C0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D77610FFD77610FF00000000000000000000000B573007A3C76E0FF6C96E
      0FF75B3107A60000000E00000000000000000000000B573007A3C76E0FF6C96E
      0FF75B3107A60000000E00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D77610FFD77610FF00000000000000003E22048AD77610FF6C3B08B56839
      08B1D77610FF4626059200000000000000003E22048AD77610FF6C3B08B56839
      08B1D77610FF4626059200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000810000
      0081000000810000008100000081000000810000008100000081000000810000
      0081000000810000008100000081000000810000008100000081000000810000
      0081000000810000008100000081000000810000008100000081000000810000
      008100000081000000810000000000000000000000001E1E1E841E1E1E840000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D77610FFD77610FF00000000000000009B550BD9D77610FF000000090000
      0005D77610FFA85C0DE200000000000000009B550BD9D77610FF000000090000
      0005D77610FFA85C0DE200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000120000
      0052000000020000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000120000
      0052000000020000000000000000000000000000000000000000000000810000
      0081000000810000008100000081000000810000008100000081000000810000
      0081000000810000008100000081000000810000008100000081000000810000
      0081000000810000008100000081000000810000008100000081000000810000
      0081000000810000008100000000000000001E1E1E84717171FF717171FF1E1E
      1E84000000000000000000000000000000000000000000000000000000000000
      0000D77610FFD77610FF0000000000000000C76D0FF6D77610FF000000000000
      0000D77610FFD37410FD0000000000000000C76D0FF6D77610FF000000000000
      0000D77610FFD37410FD00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000012000000850000
      0099000000520000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000012000000850000
      0099000000520000000000000000000000000000000000000000000000810000
      0081FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFE9D8C4FFE7D5BFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000810000008100000000000000001A1A1A7C717171FF717171FF7171
      71FF1E1E1E8400000000000000000000000000000000000000005A3107A50000
      000DD77610FFD77610FF0000000000000000C66D0FF5D77610FF000000000000
      0000D77610FFD37410FD0000000000000000C66D0FF5D77610FF000000000000
      0000D77610FFD37410FD00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000001000000084000000990000
      0085000000120000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000001000000084000000990000
      0085000000120000000000000000000000000000000000000000000000810000
      0081FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFDFBF9FFC08D51FFBF8B4EFFFCFAF7FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF00000081000000810000000000000000000000001A1A1A7C717171FF7171
      71FF717171FF1E1E1E8400000000000000000000000000000000D77610FF9B55
      0BD8D77610FFD77610FF000000000000000098530BD7D77610FF0000000A0000
      0006D77610FFA45A0DDF000000000000000098530BD7D77610FF0000000A0000
      0006D77610FFA45A0DDF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000100000008200000099000000840000
      0012000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000100000008200000099000000840000
      0012000000000000000000000000000000000000000000000000000000810000
      0081FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFDBBD9CFFB67A36FFB67A36FFD9BA97FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF0000008100000081000000000000000000000000000000001A1A1A7C7171
      71FF717171FF717171FF1E1E1E84000000000000000000000000321C037CD776
      10FFD77610FFD77610FF0000000000000000371E0482D77610FF744008BC703D
      08B8D77610FF3E22048A0000000000000000371E0482D77610FF744008BC703D
      08B8D77610FF3E22048A00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000E000000810000009900000082000000100000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000E000000810000009900000082000000100000
      0000000000000000000000000000000000000000000000000000000000810000
      0081FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFF5EEE5FFB9803DFFB67A36FFB67A36FFB87E3BFFF4EBE1FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000810000008100000000000000000000000000000000000000001A1A
      1A7C717171FF717171FF717171FF1E1E1E84000000000000000000000000321C
      037CD77610FFD77610FF0000000000000000000000084C2A0598BA670EEEBC67
      0EEF522C069D0000000B0000000000000000000000084C2A0598BA670EEEBC67
      0EEF522C069D0000000B00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000010000
      002F0000006500000087000000980000009800000086000000620000002A0000
      0000000000000000000E0000007F000000990000008100000010000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000010000
      002F0000006500000087000000980000009800000086000000620000002A0000
      0000000000000000000E0000007F000000990000008100000010000000000000
      0000000000000000000000000000000000000000000000000000000000810000
      0081FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFCCA373FFB67A36FFB67A36FFB67A36FFB67A36FFCAA06EFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000810000008100000000000000000000000000000000000000000000
      00001A1A1A7C717171FF717171FF717171FF1E1E1E8400000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000001B0000007A0000
      0099000000990000009900000099000000990000009900000099000000990000
      0076000000230000007F000000990000007F0000000E00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000001B0000007A0000
      0099000000990000009900000099000000990000009900000099000000990000
      0076000000230000007F000000990000007F0000000E00000000000000000000
      0000000000000000000000000000000000000000000000000000000000810000
      0081FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFB67A36FFB67A36FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000810000008100000000000000000000000000000000000000000000
      0000000000001A1A1A7C717171FF717171FF717171FF1E1E1E84000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000002900000093000000990000
      0087000000450000001800000002000000020000001700000043000000860000
      009900000099000000990000007F0000000E0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000002900000093000000990000
      0087000000450000001800000002000000020000001700000043000000860000
      009900000099000000990000007F0000000E0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000810000
      0081FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFB67A36FFB67A36FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000810000008100000000000000000000000000000000000000000000
      000000000000000000001A1A1A7C717171FF717171FF717171FF1E1E1E840000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000001A0000009200000099000000600000
      0006000000000000000000000000000000000000000000000000000000050000
      005C000000990000009900000025000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000001A0000009200000099000000600000
      0006000000000000000000000000000000000000000000000000000000050000
      005C000000990000009900000025000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000810000
      0081FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFB67A36FFB67A36FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000810000008100000000000000000000000000000000000000000000
      00000000000000000000000000001A1A1A7C717171FF717171FF717171FF1E1E
      1E84000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000001000000790000009900000060000000000000
      0000000000000000000000000099000000990000000000000000000000000000
      00000000005C000000990000007B000000010000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000001000000790000009900000060000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000005C000000990000007B000000010000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000810000
      0081FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000810000008100000000000000000000000000000000000000000000
      0000000000000000000000000000000000001A1A1A7C717171FF717171FF7171
      71FF1E1E1E840000000000000002140A014E5D3307A8A75C0DE1D37410FDD374
      10FDAA5D0DE35F3407AA150C0151000000020000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000002C000000990000008900000007000000000000
      0000000000000000000000000099000000990000000000000000000000000000
      0000000000050000008600000099000000310000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000002C000000990000008900000007000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000050000008600000099000000310000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000810000
      0081FFFFFFFFFFFFFFFFFFFFFFFFF5EDE3FFCBA170FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFCAA06EFFF4EBE1FFFFFFFFFFFFFFFFFFFFFF
      FFFF000000810000008100000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001A1A1A7C717171FF7171
      71FF717171FF31271D96894B0BCCD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FF8F4E0BD00804003100000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000061000000990000004800000000000000000000
      0000000000000000000000000099000000990000000000000000000000000000
      0000000000000000004300000099000000670000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000061000000990000004800000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000004300000099000000670000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000810000
      0081FFFFFFFFFCFAF8FFDABC99FFB87E3CFFB67A36FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFB67A36FFB87E3BFFD9BA97FFFCFAF7FFFFFF
      FFFF000000810000008100000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000001A1A1A7C7171
      71FF8D7257FFD37614FFD77610FFA85C0DE22B18037305020027000000040000
      00030402002629160370A45A0DDFD77610FFC86E0FF6120A014B000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000083000000990000001B00000000000000000000
      0000000000000000000000000099000000990000000000000000000000000000
      0000000000000000001700000099000000880000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000083000000990000001B00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000001700000099000000880000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000810000
      0081E8D6C1FFBF8C4FFFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFBF8B4EFFE7D5
      BFFF000000810000008100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000002D24
      1A8FD37614FFD77610FF532D069F0000000A0000000000000000000000000000
      00000000000000000000000000094D2A0599D77610FFC86E0FF6080400320000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000093000000990000000700000000000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000000000000200000099000000980000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000093000000990000000700000000000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000000000000200000099000000980000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000810000
      0081EBDAC7FFC18F54FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFC08D52FFEAD9
      C5FF000000810000008100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000018649
      0AC9D77610FF552F06A100000001000000000000000000000000D77610FFD776
      10FF000000000000000000000000000000004D2A0599D77610FF8F4E0BD00000
      0002000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000093000000990000000700000000000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000000000000200000099000000980000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000093000000990000000700000000000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000000000000200000099000000980000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000810000
      0081FFFFFFFFFDFCFAFFDCC09FFFB9803EFFB67A36FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFB67A36FFB9803DFFDBBE9DFFFDFBF9FFFFFF
      FFFF000000810000008100000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000011090149D776
      10FFAB5E0DE40000000B00000000000000000000000000000000D77610FFD776
      10FF0000000000000000000000000000000000000009A45A0DDFD77610FF150C
      0151000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000082000000990000001C00000000000000000000
      0000000000000000000000000099000000990000000000000000000000000000
      0000000000000000001800000099000000870000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000082000000990000001C00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000001800000099000000870000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000810000
      0081FFFFFFFFFFFFFFFFFFFFFFFFF6EFE7FFCEA677FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFCDA474FFF6EEE6FFFFFFFFFFFFFFFFFFFFFF
      FFFF000000810000008100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000573006A2D776
      10FF2F1A03780000000000000000000000000000000000000000D77610FFD776
      10FF000000000000000000000000000000000000000029160370D77610FF5F34
      07AA000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000060000000990000004A00000000000000000000
      0000000000000000000000000099000000990000000000000000000000000000
      0000000000000000004500000099000000650000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000060000000990000004A00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000004500000099000000650000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000810000
      0081FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000810000008100000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009E570BDBD776
      10FF0603002E0000000000000000000000000000000000000000D77610FFD776
      10FF000000000000000000000000000000000000000004020026D77610FFAA5D
      0DE3000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000002A000000990000008A00000008000000000000
      0000000000000000000000000099000000990000000000000000000000000000
      00000000000600000087000000990000002F0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000002A000000990000008A00000008000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000600000087000000990000002F0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000810000
      0081FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFB67A36FFB67A36FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000810000008100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C76D0FF5D776
      10FF0000000B00000000D77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FF0000000000000003D77610FFD374
      10FD000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000760000009900000064000000010000
      0000000000000000000000000099000000990000000000000000000000000000
      000000000060000000990000007A000000010000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000760000009900000064000000010000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000060000000990000007A000000010000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000810000
      0081FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFB67A36FFB67A36FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000810000008100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C66D0FF5D776
      10FF0000000C00000000D77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FF0000000000000004D77610FFD374
      10FD000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000180000009100000099000000640000
      0008000000000000000000000000000000000000000000000000000000070000
      006000000099000000930000001B000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000180000009100000099000000640000
      0008000000000000000000000000000000000000000000000000000000070000
      006000000099000000930000001B000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000810000
      0081FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFB67A36FFB67A36FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000810000008100000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009B550BD9D776
      10FF070400300000000000000000000000000000000000000000D77610FFD776
      10FF000000000000000000000000000000000000000005020028D77610FFA75C
      0DE1000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000002600000091000000990000
      008A0000004A0000001C00000007000000070000001B00000048000000890000
      0099000000920000002900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000002600000091000000990000
      008A0000004A0000001C00000007000000070000001B00000048000000890000
      0099000000920000002900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000810000
      0081FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFCEA677FFB67A36FFB67A36FFB67A36FFB67A36FFCCA272FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000810000008100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000542E069FD776
      10FF311B037B0000000000000000000000000000000000000000D77610FFD776
      10FF00000000000000000000000000000000000000002B180373D77610FF5C32
      07A7000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000018000000760000
      0099000000990000009900000099000000990000009900000099000000990000
      00790000001A0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000018000000760000
      0099000000990000009900000099000000990000009900000099000000990000
      00790000001A0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000810000
      0081FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFF6EFE7FFB9803EFFB67A36FFB67A36FFB87E3CFFF5EDE4FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000810000008100000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000010090146D776
      10FFAF610DE70000000D00000000000000000000000000000000D77610FFD776
      10FF000000000000000000000000000000000000000AA85C0DE2D77610FF140A
      014E000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      002A0000006000000082000000930000009300000083000000610000002C0000
      0001000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      002A0000006000000082000000930000009300000083000000610000002C0000
      0001000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000810000
      0081FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFDCC09FFFB67A36FFB67A36FFDABD9BFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000810000008100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000018046
      0AC5D77610FF5C3207A700000002000000000000000000000000D77610FFD776
      10FF00000000000000000000000000000001532D069FD77610FF894B0BCC0000
      0002000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000810000
      0081FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFDFCFAFFC18F54FFC08C50FFFDFBF8FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000810000008100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000502
      0028C16A0FF2D77610FF5B3207A70000000D0000000000000000000000000000
      000000000000000000000000000B552F06A1D77610FFC56C0FF40603002E0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000810000
      0081FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFEBDAC7FFE9D7C3FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000810000008100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000D07003FC16A0FF2D77610FFAF610DE7311B037B0704002F0000000B0000
      000B0603002E2F1A0378AB5E0DE4D77610FFC56C0FF40F080145000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000810000
      0081000000810000008100000081000000810000008100000081000000810000
      0081000000810000008100000081000000810000008100000081000000810000
      0081000000810000008100000081000000810000008100000081000000810000
      0081000000810000008100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000502002880460AC5D77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FF86490AC90603002C00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000810000
      0081000000810000008100000081000000810000008100000081000000810000
      0081000000810000008100000081000000810000008100000081000000810000
      0081000000810000008100000081000000810000008100000081000000810000
      0081000000810000008100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000110090146552E06A09B550BD9C66D0FF5C76D
      0FF59E570CDB573006A211090149000000010000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000C1437840C1437840000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000C1437840C143784000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000C1437842E4CD0FF2E4CD0FF0C14378400000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000C1437842E4CD0FF2E4CD0FF0C1437840000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000C0000000000000000000000000000
      00000C1437842E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF0C143784000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000C1437842E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF0C14
      3784000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000099000000990000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FF000000990000
      0099FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000C07003E0000000000000000000000000000
      00000B11307C2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF0C1437840000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000C1437842E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF0B11
      307C000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000099000000990000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FF000000990000
      0099FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000201001E683908B10000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000004472705930000000000000000000000000000
      0000000000000B11307C2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF0C14
      3784000000000000000000000000000000000000000000000000000000000000
      00000C1437842E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF0B11307C0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFB67A36FFB67A36FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000099000000990000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FF000000990000
      0099FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000042A170372C36B0FF3D77610FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000004124048D452605910000000000000000000000000000
      000000000000000000000B11307C2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4C
      D0FF0C1437840000000000000000000000000000000000000000000000000C14
      37842E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF0B11307C000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFB67A36FFB67A36FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000099000000990000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FF000000990000
      0099FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000905
      00358C4D0BCED77610FFD77610FFD77610FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000044124048DD77610FF160C01530000000000000000000000000000
      00000000000000000000000000000B11307C2E4CD0FF2E4CD0FF2E4CD0FF2E4C
      D0FF2E4CD0FF0C143784000000000000000000000000000000000C1437842E4C
      D0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF0B11307C00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFB67A36FFB67A36FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000099000000990000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FF000000990000
      0099FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000099000000990000000000000000000000000000
      000000000000000000000000000000000000000000000000000E45260591D274
      10FCD77610FFD77610FFD77610FFD77610FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000905
      003684480AC8D77610FFCB700FF80000000E0000000000000000000000000000
      0000000000000000000000000000000000000B11307C2E4CD0FF2E4CD0FF2E4C
      D0FF2E4CD0FF2E4CD0FF0C14378400000000000000000C1437842E4CD0FF2E4C
      D0FF2E4CD0FF2E4CD0FF2E4CD0FF0B11307C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFB67A36FFB67A36FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000099000000990000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FF000000990000
      0099FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000099000000990000000000000000000000000000
      000000000000000000000000000000000000160C0152AB5E0DE4D77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FF0000000000000000000000000000
      0000000000000000000000000000000000000000000E1A0E0259754009BCD776
      10FFD77610FFD77610FF5F3407A9000000000000000000000000000000000000
      000000000000000000000000000000000000000000000B11307C2E4CD0FF2E4C
      D0FF2E4CD0FF2E4CD0FF2E4CD0FF0C1437840C1437842E4CD0FF2E4CD0FF2E4C
      D0FF2E4CD0FF2E4CD0FF0B11307C000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFB67A36FFB67A36FF000000990000
      0099000000990000009900000099000000990000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FF000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000000000000000000000000000
      000000000000000000000201001E683908B1D77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FF000000080000000B010000160502
      00280E070142201202644325058F7F4609C4CE7110FAD77610FFD77610FFD776
      10FFD77610FFD77610FF0C07003E000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000B11307C2E4C
      D0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4C
      D0FF2E4CD0FF0B11307C00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFB67A36FFB67A36FF000000990000
      0099000000990000009900000099000000990000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FF000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000099000000990000000000000000000000000000
      0000000000042A170372C36B0FF3D77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FF764009BD00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000B11
      307C2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4C
      D0FF0B11307C0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000905
      00358C4D0BCED77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFCD7010F90603002C00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000B11307C2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF0B11
      307C000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFB67A36FFB67A36FF000000000000
      000000000000000000000000000000000000000000000000000E45260591D274
      10FCD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FF291603700000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000C1437842E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF0C14
      3784000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFB67A36FFB67A36FF000000000000
      000000000000000000000000000000000000160C0152AB5E0DE4D77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FF4A29
      0596000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000C14
      37842E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4C
      D0FF0C1437840000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B67A36FFB67A36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000001109014AA45A0DDFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FF3F23048A0000
      0002000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000C1437842E4C
      D0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4C
      D0FF2E4CD0FF0C14378400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009900000099000000990000
      00990000009900000099B67A36FFB67A36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      00990000009900000099E9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFB67A36FFB67A36FF000000000000
      000000000000000000000000000000000000000000000000000B3E220489CF72
      10FBD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFAF600DE6150C0151000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000C1437842E4CD0FF2E4C
      D0FF2E4CD0FF2E4CD0FF2E4CD0FF0B11307C0B11307C2E4CD0FF2E4CD0FF2E4C
      D0FF2E4CD0FF2E4CD0FF0C143784000000000000000000000000000000000000
      0000000000000000000000000000000000000000009900000099000000990000
      00990000009900000099B67A36FFB67A36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      00990000009900000099E9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000703
      002E83480AC7D77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFA85C0DE22F1A03780000000D00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000C1437842E4CD0FF2E4CD0FF2E4C
      D0FF2E4CD0FF2E4CD0FF0B11307C00000000000000000B11307C2E4CD0FF2E4C
      D0FF2E4CD0FF2E4CD0FF2E4CD0FF0C1437840000000000000000000000000000
      0000000000000000000000000000000000000000009900000099FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFB67A36FFB67A36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000009900000099FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF0000009900000099E9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000022414026ABE690FF0D77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFC96E0FF7BA670EEE98530BD76A3A
      08B3351D037F0B06003C00000002000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000C1437842E4CD0FF2E4CD0FF2E4CD0FF2E4C
      D0FF2E4CD0FF0B11307C000000000000000000000000000000000B11307C2E4C
      D0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF0C14378400000000000000000000
      0000000000000000000000000000000000000000009900000099FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFB67A36FFB67A36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000009900000099FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF0000009900000099E9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000020100195F3407A9D77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000C1437842E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4C
      D0FF0B11307C0000000000000000000000000000000000000000000000000B11
      307C2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF0C143784000000000000
      0000000000000000000000000000000000000000009900000099FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFB67A36FFB67A36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000009900000099FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF0000009900000099E9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001109014AA45A0DDFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000C1437842E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF0B11
      307C000000000000000000000000000000000000000000000000000000000000
      00000B11307C2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF0C1437840000
      0000000000000000000000000000000000000000009900000099FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFB67A36FFB67A36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000009900000099FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF0000009900000099E9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000B3E220489CF72
      10FBD77610FFD77610FFD77610FFD77610FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000C1437842E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF0B11307C0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000B11307C2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF0C14
      3784000000000000000000000000000000000000009900000099FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000009900000099FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF0000009900000099B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000703
      002E83480AC7D77610FFD77610FFD77610FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000B11307C2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF0B11307C000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000B11307C2E4CD0FF2E4CD0FF2E4CD0FF2E4CD0FF0B11
      307C000000000000000000000000000000000000009900000099FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000009900000099FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF0000009900000099B67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000022414026ABE690FF0D77610FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000B11307C2E4CD0FF2E4CD0FF0B11307C00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000B11307C2E4CD0FF2E4CD0FF0B11307C0000
      0000000000000000000000000000000000000000009900000099FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009900000099FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000020100195F3407A90000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000B11307C0B11307C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000B11307C0B11307C000000000000
      0000000000000000000000000000000000000000009900000099FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009900000099FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009900000099FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009900000099FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009900000099FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009900000099FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009900000099000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000D0D0D57444444C66D6D6DFA6D6D6DFA454545C80E0E0E5B000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000D0D0D57444444C66D6D6DFA6D6D6DFA454545C80E0E0E5B000000000000
      0000000000000000000000000000000000000000000000000000484848CC7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF4E4E4ED400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000F82BCDB14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF0F89C6E1000000000000
      0000000000000000000000000000000000000000000000000000000000002525
      2592717171FF717171FF717171FF717171FF717171FF717171FF2929299A0000
      0000000000000000000000000000000000000000000000000000000000002525
      2592717171FF717171FF717171FF717171FF717171FF717171FF2929299A0000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000007415E9B14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF084665A1000000000000
      00000000000000000000000000000000000000000000000000000C0C0C547171
      71FF696969F6121212660000000B0000000A10101061676767F4717171FF0E0E
      0E5C0000000000000000000000000000000000000000000000000C0C0C547171
      71FF696969F6121212660000000B0000000A10101061676767F4717171FF0E0E
      0E5C000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF717171FF717171FF717171FF717171FF000000000000
      0000717171FF717171FF717171FF717171FF0000000000000000717171FF7171
      71FF717171FF717171FF0000000000000000717171FF717171FF717171FF7171
      71FF717171FF717171FF00000000000000000000000000000000000000000000
      000000000000000000000217215C14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF031A2562000000000000
      0000000000000000000000000000000000000000000000000000404040C17171
      71FF131313690000000000000000000000000000000010101061717171FF4545
      45C8000000000000000000000000000000000000000000000000404040C17171
      71FF131313690000000000000000000000000000000010101061717171FF4545
      45C8000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF717171FF717171FF717171FF717171FF000000000000
      0000717171FF717171FF717171FF717171FF0000000000000000717171FF7171
      71FF717171FF717171FF0000000000000000717171FF717171FF717171FF7171
      71FF717171FF717171FF00000000000000000000000000000000000000000000
      000000000000000000000002031C14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF00030422000000000000
      0000000000000000000000000000000000000000000000000000666666F37171
      71FF00000012000000000000000000000000000000000000000A717171FF6D6D
      6DFA000000000000000000000000000000000000000000000000666666F37171
      71FF00000012000000000000000000000000000000000000000A717171FF6D6D
      6DFA000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      00000000000000000000000000000F82BCDB14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF0F89C6E100000000000000000000
      0000000000000000000000000000000000000000000000000000666666F27171
      71FF00000013000000000000000000000000000000000000000B717171FF6D6D
      6DFA000000000000000000000000000000000000000000000000666666F27171
      71FF00000013000000000000000000000000000000000000000B717171FF6C6C
      6CFA000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      000000000000000000000000000007415E9B14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF084665A100000000000000000000
      00000000000000000000000000000000000000000000000000003F3F3FBE7171
      71FF1515156E0000000000000000000000000000000011111166717171FF4646
      46C8000000000000000000000000000000000000000000000000404040C07171
      71FF1515156E0000000000000000000000000000000011111166717171FF4444
      44C6000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF717171FF717171FF717171FF717171FF717171FF4E4E
      4ED4000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF0000000000000000D77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FF000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      00000000000000000000000000000217215C14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF031A256200000000000000000000
      00000000000000000000000000000000000000000000000000000A0A0A4F7171
      71FF6B6B6BF81515156E000000130000001213131369696969F6717171FF2F2F
      2FA60000000000000000000000000000000000000000000000002B2B2B9E7171
      71FF6B6B6BF81515156E000000130000001213131369696969F6717171FF0D0D
      0D57000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF0000000000000000D77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FF000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      00000000000000000000000000000002031C14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF0003042200000000000000000000
      0000000000000000000000000000000000000000000000000000000000002020
      2089717171FF717171FF717171FF717171FF717171FF717171FF717171FF3737
      37B200000000000000000000000000000000000000001E1E1E84717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF252525920000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000A0A0A4F3F3F3FBF666666F2666666F2484848CC717171FF696969F50101
      0120000000000000000000000000000000001E1E1E84717171FF717171FF7171
      71FF717171FF484848CC656565F1666666F2404040C10B0B0B53000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000017171774191919790000
      000007070742717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF1A1A1A7C0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF0000000000000000D77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FF000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      000000000000000000000000000000000000626262EE717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF6A6A6AF70000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000010000
      0001464646C8717171FF717171FF717171FF717171FF717171FF717171FF1A1A
      1A7C000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF0000000000000000D77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FF000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      000000000000000000000000000000000000323232AA717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF373737B20000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000B0B
      0B51717171FF717171FF040404330303032B717171FF717171FF1A1A1A7C0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF0000000000000000D77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FF000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      00000000000000000000000000000000000002020223606060EC717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF636363EF020202280000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000034F4F
      4FD5717171FF717171FF0606063B04040433717171FF1A1A1A7C000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF0000000000000000D77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FF000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000002020223323232AA6363
      63EE717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF646464F0343434AD02020227000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000101010607171
      71FF717171FF717171FF717171FF717171FF1A1A1A7C00000000000000020404
      0434000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000007575757E07171
      71FF717171FF717171FF717171FF1A1A1A7C00000000000000032C2C2C9F5E5E
      5EE80000000B0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001515156F717171FF7171
      71FF717171FF717171FF1A1A1A7C0000000000000004303030A7717171FF7171
      71FF1A1A1A7A0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      00000000000000000000484848CC717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF00000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF0000000000000000D77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FF000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000D5E5E5EE9717171FF7171
      71FF717171FF1A1A1A7C00000000000000000B0B0B50717171FF717171FF7171
      71FF636363EF0000001200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF1A1A1A7C00000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF0000000000000000D77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FF000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001B1B1B7E717171FF717171FF7171
      71FF1A1A1A7C0000000000000000000000000000000017171774717171FF7171
      71FF717171FF2020208900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF717171FF717171FF717171FF7171
      71FF1A1A1A7C0000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF0000000000000000D77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FF000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000014656565F1717171FF717171FF1A1A
      1A7C000000000000000000000000000000000000000000000000171717747171
      71FF717171FF696969F60101011A000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF717171FF717171FF717171FF1A1A
      1A7C000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF0000000000000000D77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FF000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000002222228D717171FF717171FF1A1A1A7C0000
      0000000000000000000000000000000000000000000000000000000000001717
      1774717171FF717171FF28282897000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF717171FF717171FF1A1A1A7C0000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000101011D6A6A6AF7717171FF1A1A1A7C000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000017171774717171FF6D6D6DFA020202240000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF717171FF1A1A1A7C000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002A2A2A9C717171FF1A1A1A7C00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000017171774717171FF2F2F2FA50000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF1A1A1A7C00000000000000000000
      0000484848CC717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF0000000000000000D77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FF000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000020202286D6D6DFB1A1A1A7C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000171717746F6F6FFD0303032F00000000000000000000
      0000000000000000000000000000000000000000000000000000434343C47171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF1A1A1A7C0000000000000000000000000000
      0000717171FF717171FF717171FF717171FF717171FF717171FF717171FF1A1A
      1A7C000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF0000000000000000D77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FF000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000323232AB1A1A1A7C000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000017171774373737B300000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF717171FF717171FF717171FF717171FF1A1A1A7C0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000040404341A1A1A7B00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000171717740505053B000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF717171FF717171FF717171FF1A1A1A7C000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000050505370000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000005050537000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF717171FF717171FF1A1A1A7C00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF717171FF717171FF717171FF717171FF000000000000
      0000717171FF717171FF717171FF717171FF0000000000000000717171FF7171
      71FF717171FF717171FF0000000000000000717171FF717171FF717171FF7171
      71FF717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000595959E3717171FF717171FF606060EB000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF717171FF1A1A1A7C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF717171FF717171FF717171FF717171FF000000000000
      0000717171FF717171FF717171FF717171FF0000000000000000717171FF7171
      71FF717171FF717171FF0000000000000000717171FF717171FF717171FF7171
      71FF717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000909094A595959E35B5B5BE50B0B0B50000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF1A1A1A7C000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000434343C4717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF1A1A1A7C00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000391F048400000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D77610FF391F0484000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D77610FFD77610FF391F04840000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000484848CC717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF4E4E4ED400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D77610FFD77610FFD77610FF391F
      0484000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000484848CC7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF4E4E4ED4000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D77610FFD77610FFD77610FFD776
      10FF391F04840000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000D71A3CC14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF0000000000000000717171FF717171FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D77610FFD77610FFD77610FFD776
      10FFD77610FF391F048400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000014B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF0000000000000000717171FF717171FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF000000000000000000000000000000000000000E0000
      00000B628CBD14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF0D77ABD10000
      0112000000000000000000000000000000000000000000000000717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000000000000502
      0028683908B2C96E0FF7D77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFCB7010F86D3C
      08B60603002C0000000000000000000000000000000000000000000000000000
      000014B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF0000000000000000717171FF717171FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF00000000000000000000000000000000031D2A680000
      0000010D134714B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF042B
      3F7F000000000000000000000000000000000000000000000000717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF000000000000
      000000000000000000000000000000000000000000000000000005020027BC67
      0EEFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFC06A0FF10603002C00000000000000000000000000000000000000000000
      000014B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF0000000000000000717171FF717171FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF000000000000000000000000000000000A587FB40000
      000B000000010D73A6CE14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF1194
      D6EA0000000A0000000000000000000000000000000000000000717171FF7171
      71FF717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000633608ADD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FF6C3C08B500000000000000000000000000000000000000000000
      000014B1FFFF14B1FFFF14B1FFFF14B1FFFF0000000000000000000000000000
      00000000000000000000717171FF717171FF0000000000000000717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF000000000000
      0000717171FF717171FF000000000000000000000000000000000B638FBF0215
      1E590000000002141E5814B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF031E2C6B0000000000000000000000000000000000000000717171FF7171
      71FF717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000BE690FF0D776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFCB700FF800000000000000000000000000000000000000000000
      000014B1FFFF14B1FFFF14B1FFFF14B1FFFF0000000000000000000000000000
      00000000000000000000717171FF717171FF0000000000000000717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF000000000000
      0000717171FF717171FF000000000000000000000000000000000B638FBF0A4F
      72AB00000005000000030F84BDDC14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF0F84BFDD0000000400000000000000000000000000000000717171FF7171
      71FF717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000D77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFF6E1CAFFF8E5D2FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FF00000000000000000000000000000000000000000000
      000014B1FFFF14B1FFFF14B1FFFF14B1FFFF0000000000000000000000000000
      00000000000000000000717171FF717171FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF000000000000000000000000000000000B638FBF0B63
      8FBF010E144800000000031D2A6814B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF02141E5800000000000000000000000000000000717171FF7171
      71FF717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000D77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFF5DDC3FFF6E1CAFFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FF00000000000000000000000000000000000000000000
      000014B1FFFF14B1FFFF14B1FFFF14B1FFFF0000000000000000000000000000
      00000000000000000000717171FF717171FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF000000000000000000000000000000000B638FBF0B63
      8FBF0844639F00000001000000091192D3E814B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF0D72A4CD00000000000000000000000000000000717171FF7171
      71FF717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FF00000000000000000000000000000000000000000000
      000014B1FFFF14B1FFFF14B1FFFF14B1FFFF0000000000000000000000000000
      00000000000000000000717171FF717171FF0000000000000000717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF000000000000
      0000717171FF717171FF000000000000000000000000000000000B638FBF0B63
      8FBF0B638FBF01080C39000000000427397914B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF010C1244000000000000000000000000717171FF7171
      71FF717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFDB862AFFDC882EFFD87813FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FF00000000000000000000000000000000000000000000
      000014B1FFFF14B1FFFF14B1FFFF14B1FFFF0000000000000000000000000000
      00000000000000000000717171FF717171FF0000000000000000717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF000000000000
      0000717171FF717171FF000000000000000000000000000000000B638FBF0B63
      8FBF0B638FBF063851900000000000000110129EE3F114B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF0A5E87BA000000000000000000000000717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF0000000000000000484848CC7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF4E4E4ED40000000000000000D77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFFBF2E9FFFFFFFFFFD87A17FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FF00000000000000000000000000000000000000000000
      000014B1FFFF14B1FFFF14B1FFFF14B1FFFF0000000000000000000000000000
      00000000000000000000717171FF717171FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF000000000000000000000000000000000B638FBF0B63
      8FBF0B638FBF0B638FBF000406290000000005334A8A14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF000609320000000000000000717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF0000000000000000717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF0000000000000000D77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFF9EBDBFFFFFFFFFFE9B47BFFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FF00000000000000000000000000000000000000000000
      000014B1FFFF14B1FFFF14B1FFFF14B1FFFF0000000000000000000000000000
      00000000000000000000717171FF717171FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF000000000000000000000000000000000B638FBF0B63
      8FBF0B638FBF0B638FBF052C4080000000000001021A13A7F1F814B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF094B6DA70000000000000000717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF0000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF0000000000000000D77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFE5A763FFFFFEFEFFFFFFFFFFEDC091FFD776
      11FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FF00000000000000000000000000000000000000000000
      000014B1FFFF14B1FFFF14B1FFFF14B1FFFF0000000000000000000000000000
      00000000000000000000717171FF717171FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF000000000000000000000000000000000B638FBF0B63
      8FBF0B638FBF0B638FBF0B628CBD0002021B0000000007415E9B14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF1192D3E80000000000000000717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF0000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF0000000000000000D77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFE5A765FFFEFBF8FFFFFFFFFFECBE
      8DFFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FF00000000000000000000000000000000000000000000
      000014B1FFFF14B1FFFF14B1FFFF14B1FFFF0000000000000000000000000000
      00000000000000000000717171FF717171FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF000000000000000000000000000000000B638FBF0B63
      8FBF0B638FBF0B638FBF0B638FBF042231700000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF0000000000000000D77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFE3A159FFFFFFFFFFFFFD
      FCFFDC882EFFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FF00000000000000000000000000000000000000000000
      000014B1FFFF14B1FFFF14B1FFFF14B1FFFF0000000000000000000000000000
      00000000000000000000717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF000000000000000000000000000000000B638FBF0B63
      8FBF0B638FBF0B638FBF0B638FBF0B5F89BB010B114200000005000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF717171FF717171FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF0000000000000000D77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFF5DD
      C3FFFBF3E9FFEAB984FFD77610FFD77610FFD77610FFD87916FFFFFFFFFFFFFF
      FFFFE3A057FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FF00000000000000000000000000000000000000000000
      000014B1FFFF14B1FFFF14B1FFFF14B1FFFF0000000000000000000000000000
      00000000000000000000434343C4717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF484848CC000000000000000000000000000000000B638FBF0B63
      8FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B63
      8FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B63
      8FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF717171FF717171FF0000000000000000717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF0000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF0000000000000000D77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFF5DC
      C1FFFFFFFFFFFCF5EEFFE09647FFD77610FFD87B18FFEDC193FFFFFFFFFFFEFD
      FBFFDC862BFFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FF00000000000000000000000000000000000000000000
      000014B1FFFF14B1FFFF14B1FFFF14B1FFFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000B638FBF0B63
      8FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B63
      8FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B63
      8FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF717171FF717171FF0000000000000000717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF0000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF0000000000000000D77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFDF90
      3CFFFDF8F2FFFFFFFFFFFFFFFFFFFCF6EFFFFEFCF9FFFFFFFFFFFFFFFFFFEAB8
      82FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FF00000000000000000000000000000000000000000000
      000014B1FFFF14B1FFFF14B1FFFF14B1FFFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000B638FBF0B63
      8FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B63
      8FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B63
      8FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF717171FF717171FF0000000000000000717171FF717171FF000000000000
      0000717171FF717171FF717171FF717171FF0000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF0000000000000000D77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFDE8D37FFF2D2B0FFFBF3EAFFFEFBF8FFFCF6EFFFF5DCC2FFE4A45EFFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FF00000000000000000000000000000000000000000000
      000014B1FFFF14B1FFFF14B1FFFF14B1FFFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000014B1FFFF14B1FFFF14B1FFFF14B1FFFF000000000000
      00000000000000000000000000000000000000000000000000000B638FBF0B63
      8FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B63
      8FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B63
      8FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF073F5C99000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF717171FF717171FF0000000000000000717171FF717171FF000000000000
      0000717171FF717171FF717171FF717171FF0000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF0000000000000000D77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FF00000000000000000000000000000000000000000000
      000014B1FFFF14B1FFFF14B1FFFF14B1FFFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000014B1FFFF14B1FFFF14B1FFFF14B1FFFF000000000000
      00000000000000000000000000000000000000000000000000000B638FBF0B63
      8FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B63
      8FBF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF717171FF717171FF0000000000000000717171FF717171FF000000000000
      0000717171FF717171FF717171FF717171FF0000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF0000000000000000BB670EEED776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFC96E0FF700000000000000000000000000000000000000000000
      000014B1FFFF14B1FFFF14B1FFFF14B1FFFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000014B1FFFF14B1FFFF14B1FFFF14B1FFFF000000000000
      00000000000000000000000000000000000000000000000000000B638FBF0B63
      8FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B63
      8FBF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF717171FF717171FF0000000000000000717171FF717171FF000000000000
      0000717171FF717171FF717171FF717171FF0000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF00000000000000005F3407AAD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FF683908B200000000000000000000000000000000000000000000
      000014B1FFFF14B1FFFF14B1FFFF14B1FFFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000014B1FFFF14B1FFFF14B1FFFF14B1FFFF000000000000
      00000000000000000000000000000000000000000000000000000B638FBF0B63
      8FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B63
      8FBF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF717171FF717171FF0000000000000000717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF0000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF000000000000000004020023B865
      0EECD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFBC670EEF0502002800000000000000000000000000000000000000000000
      000014B1FFFF14B1FFFF14B1FFFF14B1FFFF0000000000000000484848CC7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF4E4E
      4ED4000000000000000014B1FFFF14B1FFFF14B1FFFF14B1FFFF000000000000
      0000000000000000000000000000000000000000000000000000073A54930B63
      8FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF073F
      5C99000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000434343C47171
      71FF717171FF717171FF0000000000000000717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF0000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF0000000000000000000000000402
      00235F3407AABB670EEED77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFBE690FF06336
      08AD050200270000000000000000000000000000000000000000000000000000
      000014B1FFFF14B1FFFF14B1FFFF14B1FFFF0000000000000000717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF000000000000000014B1FFFF14B1FFFF14B1FFFF14B1FFFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000014B1FFFF14B1FFFF14B1FFFF14B1FFFF0000000000000000717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF000000000000000014B1FFFF14B1FFFF14B1FFFF14B1FFFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF717171FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000C6896C414B1FFFF14B1FFFF14B1FFFF0000000000000000717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF000000000000000014B1FFFF14B1FFFF14B1FFFF0D77ABD1000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF717171FF717171FF717171FF717171FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000434343C47171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF484848CC0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000434343C4717171FF717171FF717171FF717171FF484848CC000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000080000000A00100000100010000000000001A00000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000}
    DesignInfo = 12583312
    ImageInfo = <
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224F
          70656E2220786D6C6E733D22687474703A2F2F7777772E77332E6F72672F3230
          30302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F7777772E
          77332E6F72672F313939392F786C696E6B2220783D223070782220793D223070
          78222076696577426F783D2230203020333220333222207374796C653D22656E
          61626C652D6261636B67726F756E643A6E6577203020302033322033323B2220
          786D6C3A73706163653D227072657365727665223E262331333B262331303B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B3C7374796C6520747970653D22
          746578742F6373732220786D6C3A73706163653D227072657365727665223E2E
          59656C6C6F777B66696C6C3A234646423131353B7D262331333B262331303B26
          23393B2E7374307B6F7061636974793A302E37353B7D3C2F7374796C653E0D0A
          3C6720636C6173733D22737430223E0D0A09093C7061746820636C6173733D22
          59656C6C6F772220643D224D322E322C32352E326C352E352D313263302E332D
          302E372C312D312E322C312E382D312E32483236563963302D302E362D302E34
          2D312D312D31483132563563302D302E362D302E342D312D312D31483343322E
          342C342C322C342E342C322C3576323020202623393B2623393B63302C302E32
          2C302C302E332C302E312C302E3443322E312C32352E332C322E322C32352E33
          2C322E322C32352E327A222F3E0D0A093C2F673E0D0A3C7061746820636C6173
          733D2259656C6C6F772220643D224D33312E332C313448392E364C342C323668
          32312E3863302E352C302C312E312D302E332C312E332D302E374C33322C3134
          2E374333322E312C31342E332C33312E382C31342C33312E332C31347A222F3E
          0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D2253
          6176652220786D6C6E733D22687474703A2F2F7777772E77332E6F72672F3230
          30302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F7777772E
          77332E6F72672F313939392F786C696E6B2220783D223070782220793D223070
          78222076696577426F783D2230203020333220333222207374796C653D22656E
          61626C652D6261636B67726F756E643A6E6577203020302033322033323B2220
          786D6C3A73706163653D227072657365727665223E262331333B262331303B3C
          7374796C6520747970653D22746578742F637373223E2E426C61636B7B66696C
          6C3A233732373237323B7D3C2F7374796C653E0D0A3C7061746820636C617373
          3D22426C61636B2220643D224D33312C30483139632D302E362C302D312C302E
          342D312C3176313663302C302E362C302E342C312C312C3168313263302E362C
          302C312D302E342C312D3156314333322C302E342C33312E362C302C33312C30
          7A204D33302C313648323056326831305631367A222F3E0D0A3C706174682063
          6C6173733D22426C61636B2220643D224D32322C323076344836762D36683130
          762D3448365634483343322E342C342C322C342E342C322C3576323263302C30
          2E362C302E342C312C312C3168323263302E362C302C312D302E342C312D3176
          2D374832327A204D31362C3448387638683856347A20202623393B204D31322C
          3130682D32563668325631307A222F3E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D22D0
          A1D0BBD0BED0B95F312220786D6C6E733D22687474703A2F2F7777772E77332E
          6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
          2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
          20793D22307078222076696577426F783D223020302033322033322220737479
          6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203332
          2033323B2220786D6C3A73706163653D227072657365727665223E262331333B
          262331303B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B3C7374796C6520
          747970653D22746578742F6373732220786D6C3A73706163653D227072657365
          727665223E2E426C75657B66696C6C3A233131373744373B7D262331333B2623
          31303B2623393B2E57686974657B66696C6C3A234646464646463B7D3C2F7374
          796C653E0D0A3C672069643D22D0A1D0BBD0BED0B95F32223E0D0A09093C7061
          746820636C6173733D22426C75652220643D224D362C3468323063322E322C30
          2C342C312E382C342C3476313463302C322E322D312E382C342D342C34483134
          6C2D362C36762D364836632D322E322C302D342D312E382D342D34563843322C
          352E382C332E382C342C362C347A222F3E0D0A09093C7061746820636C617373
          3D2257686974652220643D224D31372E312C313763302E312D302E332C302E32
          2D302E352C302E372D302E3963312D302E382C312E372D312E352C322D327330
          2E352D312C302E352D312E3563302D312D302E342D312E382D312E332D322E36
          632D302E382D302E372D322D312D332E342D3120202623393B2623393B632D31
          2E332C302D322E342C302E322D332E332C31632D302E382C302E372D312E332C
          312E362D312E332C322E376C322E332C302E3363302E322D302E372C302E342D
          312E332C302E392D312E3663302E342D302E342C302E392D302E352C312E362D
          302E3563302E372C302C312E322C302E322C312E362C302E3520202623393B26
          23393B73302E362C302E382C302E362C312E3263302C302E332D302E312C302E
          372D302E332C302E39632D302E312C302E322D302E362C302E362D312E332C31
          2E32632D302E372C302E362D302E392C302E392D312E322C312E34632D302E32
          2C302E352D302E312C322D302E312C32683220202623393B2623393B4331372C
          31382C31362E392C31372E332C31372E312C31377A222F3E0D0A09093C636972
          636C6520636C6173733D225768697465222063783D223136222063793D223231
          2220723D2231222F3E0D0A093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D2250
          617374652220786D6C6E733D22687474703A2F2F7777772E77332E6F72672F32
          3030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F777777
          2E77332E6F72672F313939392F786C696E6B2220783D223070782220793D2230
          7078222076696577426F783D2230203020333220333222207374796C653D2265
          6E61626C652D6261636B67726F756E643A6E6577203020302033322033323B22
          20786D6C3A73706163653D227072657365727665223E262331333B262331303B
          3C7374796C6520747970653D22746578742F6373732220786D6C3A7370616365
          3D227072657365727665223E2E426C61636B7B66696C6C3A233732373237323B
          7D262331333B262331303B2623393B2E59656C6C6F777B66696C6C3A23464642
          3131353B7D3C2F7374796C653E0D0A3C7061746820636C6173733D2259656C6C
          6F772220643D224D31322C323476344835632D302E362C302D312D302E342D31
          2D31563363302D302E362C302E342D312C312D3168337632324831327A204D32
          352C32682D337638683456322E384332362C322E342C32352E362C322C32352C
          327A222F3E0D0A3C7061746820636C6173733D22426C61636B2220643D224D32
          392C3132483135632D302E362C302D312C302E342D312C3176313663302C302E
          362C302E342C312C312C3168313463302E362C302C312D302E342C312D315631
          334333302C31322E342C32392E362C31322C32392C31327A204D32382C323848
          313656313420202623393B6831325632387A204D32362C3230682D38762D3268
          385632307A204D32362C3234682D38762D3268385632347A222F3E0D0A3C7061
          746820636C6173733D22426C61636B2220643D224D31382C32563163302D302E
          362D302E342D312D312D31682D34632D302E362C302D312C302E342D312C3176
          31682D32763363302C302E362C302E342C312C312C31683863302E362C302C31
          2D302E342C312D3156324831387A222F3E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D2243
          75742220786D6C6E733D22687474703A2F2F7777772E77332E6F72672F323030
          302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F7777772E77
          332E6F72672F313939392F786C696E6B2220783D223070782220793D22307078
          222076696577426F783D2230203020333220333222207374796C653D22656E61
          626C652D6261636B67726F756E643A6E6577203020302033322033323B222078
          6D6C3A73706163653D227072657365727665223E262331333B262331303B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B3C7374796C6520747970653D2274
          6578742F637373223E2E426C61636B7B66696C6C3A233732373237323B7D3C2F
          7374796C653E0D0A3C7061746820636C6173733D22426C61636B2220643D224D
          31362E342C31322E364C32362C336C2D362E362C31322E354C31362E342C3132
          2E367A204D31312E342C32322E3663302E342C302E372C302E362C312E352C30
          2E362C322E3463302C322E382D322E322C352D352C35632D322E382C302D352D
          322E322D352D3520202623393B73322E322D352C352D3563302E362C302C312E
          322C302E312C312E372C302E336C312E342D312E344C31322C32324C31312E34
          2C32322E367A204D31302C323563302D312E372D312E332D332D332D33632D31
          2E372C302D332C312E332D332C3373312E332C332C332C3320202623393B4338
          2E372C32382C31302C32362E372C31302C32357A204D32382C323563302C322E
          382D322E322C352D352C35632D322E382C302D352D322E322D352D3563302D30
          2E392C302E322D312E372C302E362D322E344C31362C32306C2D332C304C342C
          336C31372E332C31372E3320202623393B63302E352D302E322C312E312D302E
          332C312E372D302E334332352E382C32302C32382C32322E322C32382C32357A
          204D31362C313763302D302E362D302E342D312D312D31632D302E362C302D31
          2C302E342D312C3173302E342C312C312C314331352E362C31382C31362C3137
          2E362C31362C31377A204D32362C323520202623393B63302D312E372D312E33
          2D332D332D33632D312E372C302D332C312E332D332C3373312E332C332C332C
          334332342E372C32382C32362C32362E372C32362C32357A222F3E0D0A3C2F73
          76673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D2243
          6F70792220786D6C6E733D22687474703A2F2F7777772E77332E6F72672F3230
          30302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F7777772E
          77332E6F72672F313939392F786C696E6B2220783D223070782220793D223070
          78222076696577426F783D2230203020333220333222207374796C653D22656E
          61626C652D6261636B67726F756E643A6E6577203020302033322033323B2220
          786D6C3A73706163653D227072657365727665223E262331333B262331303B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B3C7374796C6520747970653D22
          746578742F637373223E2E426C61636B7B66696C6C3A233732373237323B7D3C
          2F7374796C653E0D0A3C7061746820636C6173733D22426C61636B2220643D22
          4D32312C30483943382E342C302C382C302E342C382C317635483343322E342C
          362C322C362E342C322C3776323263302C302E362C302E342C312C312C316831
          3863302E362C302C312D302E342C312D31762D35683563302E362C302C312D30
          2E342C312D31563720202623393B4C32312C307A204D32302C32384834563868
          3468326834763563302C302E362C302E342C312C312C31683576347634763256
          32387A204D32362C3132763130682D34762D396C2D372D37682D355632683468
          367634763163302C302E362C302E342C312C312C3168355631327A222F3E0D0A
          3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D22D0
          A1D0BBD0BED0B95F322220786D6C6E733D22687474703A2F2F7777772E77332E
          6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
          2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
          20793D22307078222076696577426F783D223020302033322033322220737479
          6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203332
          2033323B2220786D6C3A73706163653D227072657365727665223E262331333B
          262331303B3C7374796C6520747970653D22746578742F6373732220786D6C3A
          73706163653D227072657365727665223E2E426C75657B66696C6C3A23313137
          3744373B7D262331333B262331303B2623393B2E426C61636B7B66696C6C3A23
          3732373237323B7D3C2F7374796C653E0D0A3C7265637420783D22382220793D
          22362220636C6173733D22426C7565222077696474683D223138222068656967
          68743D223138222F3E0D0A3C673E0D0A09093C673E0D0A0909093C706F6C7967
          6F6E20636C6173733D22426C61636B2220706F696E74733D22362C323220342C
          323220342C32382031302C32382031302C323620362C3236202623393B262339
          3B222F3E0D0A0909093C706F6C79676F6E20636C6173733D22426C61636B2220
          706F696E74733D22342C3820362C3820362C342031302C342031302C3220342C
          32202623393B2623393B222F3E0D0A0909093C706F6C79676F6E20636C617373
          3D22426C61636B2220706F696E74733D2232382C32362032342C32362032342C
          32382033302C32382033302C32322032382C3232202623393B2623393B222F3E
          0D0A0909093C706F6C79676F6E20636C6173733D22426C61636B2220706F696E
          74733D2232342C322032342C342032382C342032382C382033302C382033302C
          32202623393B2623393B222F3E0D0A0909093C7265637420783D22342220793D
          2231302220636C6173733D22426C61636B222077696474683D22322220686569
          6768743D2234222F3E0D0A0909093C7265637420783D22342220793D22313622
          20636C6173733D22426C61636B222077696474683D223222206865696768743D
          2234222F3E0D0A0909093C7265637420783D2231322220793D22322220636C61
          73733D22426C61636B222077696474683D223422206865696768743D2232222F
          3E0D0A0909093C7265637420783D2231382220793D22322220636C6173733D22
          426C61636B222077696474683D223422206865696768743D2232222F3E0D0A09
          09093C7265637420783D2232382220793D2231302220636C6173733D22426C61
          636B222077696474683D223222206865696768743D2234222F3E0D0A0909093C
          7265637420783D2232382220793D2231362220636C6173733D22426C61636B22
          2077696474683D223222206865696768743D2234222F3E0D0A0909093C726563
          7420783D2231322220793D2232362220636C6173733D22426C61636B22207769
          6474683D223422206865696768743D2232222F3E0D0A0909093C726563742078
          3D2231382220793D2232362220636C6173733D22426C61636B22207769647468
          3D223422206865696768743D2232222F3E0D0A09093C2F673E0D0A09093C673E
          0D0A0909093C706F6C79676F6E20636C6173733D22426C61636B2220706F696E
          74733D22362C323220342C323220342C32382031302C32382031302C32362036
          2C3236202623393B2623393B222F3E0D0A0909093C706F6C79676F6E20636C61
          73733D22426C61636B2220706F696E74733D22342C3820362C3820362C342031
          302C342031302C3220342C32202623393B2623393B222F3E0D0A0909093C706F
          6C79676F6E20636C6173733D22426C61636B2220706F696E74733D2232382C32
          362032342C32362032342C32382033302C32382033302C32322032382C323220
          2623393B2623393B222F3E0D0A0909093C706F6C79676F6E20636C6173733D22
          426C61636B2220706F696E74733D2232342C322032342C342032382C34203238
          2C382033302C382033302C32202623393B2623393B222F3E0D0A0909093C7265
          637420783D22342220793D2231302220636C6173733D22426C61636B22207769
          6474683D223222206865696768743D2234222F3E0D0A0909093C726563742078
          3D22342220793D2231362220636C6173733D22426C61636B222077696474683D
          223222206865696768743D2234222F3E0D0A0909093C7265637420783D223132
          2220793D22322220636C6173733D22426C61636B222077696474683D22342220
          6865696768743D2232222F3E0D0A0909093C7265637420783D2231382220793D
          22322220636C6173733D22426C61636B222077696474683D2234222068656967
          68743D2232222F3E0D0A0909093C7265637420783D2232382220793D22313022
          20636C6173733D22426C61636B222077696474683D223222206865696768743D
          2234222F3E0D0A0909093C7265637420783D2232382220793D2231362220636C
          6173733D22426C61636B222077696474683D223222206865696768743D223422
          2F3E0D0A0909093C7265637420783D2231322220793D2232362220636C617373
          3D22426C61636B222077696474683D223422206865696768743D2232222F3E0D
          0A0909093C7265637420783D2231382220793D2232362220636C6173733D2242
          6C61636B222077696474683D223422206865696768743D2232222F3E0D0A0909
          3C2F673E0D0A093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D22D0
          A1D0BBD0BED0B95F322220786D6C6E733D22687474703A2F2F7777772E77332E
          6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
          2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
          20793D22307078222076696577426F783D223020302033322033322220737479
          6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203332
          2033323B2220786D6C3A73706163653D227072657365727665223E262331333B
          262331303B3C7374796C6520747970653D22746578742F6373732220786D6C3A
          73706163653D227072657365727665223E2E426C61636B7B66696C6C3A233732
          373237323B7D262331333B262331303B2623393B2E59656C6C6F777B66696C6C
          3A234646423131353B7D3C2F7374796C653E0D0A3C706F6C79676F6E20636C61
          73733D2259656C6C6F772220706F696E74733D22382C323220362C3330203236
          2C33302032342C323220222F3E0D0A3C7061746820636C6173733D22426C6163
          6B2220643D224D32302C3136682D32563463302D312E312D302E392D322D322D
          32732D322C302E392D322C32763132682D32632D322E322C302D342C312E382D
          342C346831364332342C31372E382C32322E322C31362C32302C31367A222F3E
          0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D2255
          6E646F2220786D6C6E733D22687474703A2F2F7777772E77332E6F72672F3230
          30302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F7777772E
          77332E6F72672F313939392F786C696E6B2220783D223070782220793D223070
          78222076696577426F783D2230203020333220333222207374796C653D22656E
          61626C652D6261636B67726F756E643A6E6577203020302033322033323B2220
          786D6C3A73706163653D227072657365727665223E262331333B262331303B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B3C7374796C6520747970653D22
          746578742F637373223E2E426C75657B66696C6C3A233131373744373B7D3C2F
          7374796C653E0D0A3C7061746820636C6173733D22426C75652220643D224D33
          322C323663302C302C302D382D31362D3876364C302C31344C31362C34763643
          33322C31302C33322C32362C33322C32367A222F3E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B3C7374796C6520747970
          653D22746578742F637373223E2E5265647B66696C6C3A234430344432463B7D
          3C2F7374796C653E0D0A3C706F6C79676F6E20636C6173733D22526564222070
          6F696E74733D2232352C342031362C313320372C3420342C372031332C313620
          342C323520372C32382031362C31392032352C32382032382C32352031392C31
          362032382C3720222F3E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B3C7374796C6520747970653D22746578742F6373732220786D6C3A737061
          63653D227072657365727665223E2E7374307B6F7061636974793A302E363B7D
          262331333B262331303B2623393B2E7374317B66696C6C3A234646464646463B
          7D262331333B262331303B2623393B2E7374327B66696C6C3A23333737424236
          3B7D262331333B262331303B2623393B2E7374337B6F7061636974793A302E37
          3B7D3C2F7374796C653E0D0A3C6720636C6173733D22737430223E0D0A09093C
          7265637420783D2231382220793D223138222077696474683D22313422206865
          696768743D223134222F3E0D0A09093C726563742077696474683D2231342220
          6865696768743D223134222F3E0D0A093C2F673E0D0A3C673E0D0A09093C7265
          637420783D2232302220793D2232302220636C6173733D227374312220776964
          74683D22313022206865696768743D223130222F3E0D0A09093C726563742078
          3D22322220793D22322220636C6173733D22737431222077696474683D223130
          22206865696768743D223130222F3E0D0A093C2F673E0D0A3C7265637420783D
          22362220793D22362220636C6173733D22737432222077696474683D22323022
          206865696768743D223230222F3E0D0A3C6720636C6173733D22737433223E0D
          0A09093C7265637420783D22382220793D22382220636C6173733D2273743122
          2077696474683D22313622206865696768743D223136222F3E0D0A093C2F673E
          0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B3C7374796C6520747970653D22746578742F6373732220786D6C3A737061
          63653D227072657365727665223E2E7374307B66696C6C3A233337374242363B
          7D262331333B262331303B2623393B2E7374317B6F7061636974793A302E363B
          7D262331333B262331303B2623393B2E7374327B66696C6C3A23464646464646
          3B7D262331333B262331303B2623393B2E7374337B6F7061636974793A302E37
          3B7D3C2F7374796C653E0D0A3C706F6C79676F6E20636C6173733D2273743022
          20706F696E74733D2231382C31382032362C31382032362C362031342C362031
          342C313420362C313420362C32362031382C323620222F3E0D0A3C6720636C61
          73733D22737431223E0D0A09093C7265637420783D2231382220793D22313822
          2077696474683D22313422206865696768743D223134222F3E0D0A09093C7265
          63742077696474683D22313422206865696768743D223134222F3E0D0A093C2F
          673E0D0A3C673E0D0A09093C7265637420783D2232302220793D223230222063
          6C6173733D22737432222077696474683D22313022206865696768743D223130
          222F3E0D0A09093C7265637420783D22322220793D22322220636C6173733D22
          737432222077696474683D22313022206865696768743D223130222F3E0D0A09
          3C2F673E0D0A3C6720636C6173733D22737433223E0D0A09093C706F6C79676F
          6E20636C6173733D227374322220706F696E74733D2231342C382031342C3134
          20382C313420382C32342031382C32342031382C31382032342C31382032342C
          38202623393B222F3E0D0A093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B3C7374796C6520747970
          653D22746578742F6373732220786D6C3A73706163653D227072657365727665
          223E2E7374307B6F7061636974793A302E363B7D262331333B262331303B2623
          393B2E7374317B66696C6C2D72756C653A6576656E6F64643B636C69702D7275
          6C653A6576656E6F64643B7D3C2F7374796C653E0D0A3C6720636C6173733D22
          737430223E0D0A09093C7061746820636C6173733D227374312220643D224D32
          382E382C32372E336C2D312E352C312E354C32302E362C3232632D312E352C31
          2E322D332E352C322D352E362C32632D352C302D392D342D392D3973342D392C
          392D3973392C342C392C3963302C322E312D302E372C342E312D322C352E3620
          202623393B2623393B4C32382E382C32372E337A204D31352C38632D332E392C
          302D372C332E312D372C3773332E312C372C372C3773372D332E312C372D3753
          31382E392C382C31352C387A204D31362C3230682D32762D34682D34762D3268
          34762D346832763468347632682D345632307A222F3E0D0A093C2F673E0D0A3C
          2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B3C7374796C6520747970
          653D22746578742F6373732220786D6C3A73706163653D227072657365727665
          223E2E7374307B6F7061636974793A302E363B7D262331333B262331303B2623
          393B2E7374317B66696C6C2D72756C653A6576656E6F64643B636C69702D7275
          6C653A6576656E6F64643B7D3C2F7374796C653E0D0A3C6720636C6173733D22
          737430223E0D0A09093C7061746820636C6173733D227374312220643D224D32
          382E382C32372E336C2D312E352C312E354C32302E362C3232632D312E352C31
          2E322D332E352C322D352E362C32632D352C302D392D342D392D3973342D392C
          392D3973392C342C392C3963302C322E312D302E372C342E312D322C352E3620
          202623393B2623393B4C32382E382C32372E337A204D31352C38632D332E392C
          302D372C332E312D372C3773332E312C372C372C3773372D332E312C372D3753
          31382E392C382C31352C387A204D31362C3136682D326C302C30682D34762D32
          68346C302C3068326C302C30683476324831364C31362C31367A222F3E0D0A09
          3C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D22D0
          A1D0BBD0BED0B95F312220786D6C6E733D22687474703A2F2F7777772E77332E
          6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
          2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
          20793D22307078222076696577426F783D223020302033322033322220737479
          6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203332
          2033323B2220786D6C3A73706163653D227072657365727665223E262331333B
          262331303B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B3C7374796C6520
          747970653D22746578742F6373732220786D6C3A73706163653D227072657365
          727665223E2E7374307B6F7061636974793A302E333B7D262331333B26233130
          3B2623393B2E7374317B66696C6C3A234646464646463B7D262331333B262331
          303B2623393B2E7374327B66696C6C3A233337374242363B7D3C2F7374796C65
          3E0D0A3C6720636C6173733D22737430223E0D0A09093C673E0D0A0909093C72
          65637420783D22322220793D2232222077696474683D22323822206865696768
          743D223238222F3E0D0A09093C2F673E0D0A09093C673E0D0A0909093C726563
          7420783D22322220793D2232222077696474683D22323822206865696768743D
          223238222F3E0D0A09093C2F673E0D0A093C2F673E0D0A3C673E0D0A09093C70
          61746820636C6173733D227374312220643D224D342C34763234683234563448
          347A222F3E0D0A09093C7061746820636C6173733D227374322220643D224D32
          332C3139762D32682D33762D326833762D326C352C334C32332C31397A222F3E
          0D0A09093C7061746820636C6173733D227374322220643D224D392C3139762D
          326833762D324839762D326C2D352C334C392C31397A222F3E0D0A09093C7061
          746820636C6173733D227374322220643D224D31332C32336832762D33683276
          3368326C2D332C354C31332C32337A222F3E0D0A09093C7061746820636C6173
          733D227374322220643D224D31362C346C2D332C35683276336832563968324C
          31362C347A222F3E0D0A093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D22D0
          A1D0BBD0BED0B95F312220786D6C6E733D22687474703A2F2F7777772E77332E
          6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
          2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
          20793D22307078222076696577426F783D223020302033322033322220737479
          6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203332
          2033323B2220786D6C3A73706163653D227072657365727665223E262331333B
          262331303B3C7374796C6520747970653D22746578742F6373732220786D6C3A
          73706163653D227072657365727665223E2E426C75657B66696C6C3A23313137
          3744373B7D262331333B262331303B2623393B2E426C61636B7B66696C6C3A23
          3732373237323B7D3C2F7374796C653E0D0A3C7061746820636C6173733D2242
          6C61636B2220643D224D31312C31374C302C32386C322C326C31312D31316C31
          2D316C2D322D324C31312C31377A222F3E0D0A3C673E0D0A09093C673E0D0A09
          09093C7061746820636C6173733D22426C75652220643D224D31392C32632D35
          2C302D392C342D392C3973342C392C392C3973392D342C392D395332342C322C
          31392C327A204D31392C3138632D332E392C302D372D332E312D372D3763302D
          332E392C332E312D372C372D3773372C332E312C372C3720202623393B262339
          3B2623393B4332362C31342E392C32322E392C31382C31392C31387A222F3E0D
          0A09093C2F673E0D0A093C2F673E0D0A3C706F6C79676F6E20636C6173733D22
          426C75652220706F696E74733D2231302C32362031302C32382031322C32362E
          352031322C33322031342C33322031342C32342031322C323420222F3E0D0A3C
          7061746820636C6173733D22426C75652220643D224D32392E312C32342E3843
          32382E362C32342E332C32372E392C32342C32372C3234632D302E392C302D31
          2E362C302E332D322E312C302E384332342E332C32352E352C32342C32362E35
          2C32342C323863302C312E352C302E332C322E352C302E382C332E3120202623
          393B63302E352C302E362C312E332C302E392C322E322C302E3963302E392C30
          2C312E362D302E332C322E312D302E3863302E362D302E362C302E392D312E37
          2C302E392D332E324333302C32362E352C32392E372C32352E352C32392E312C
          32342E387A204D32382C32392E3563302C302E352D302E352C312D312C312020
          2623393B732D312D302E352D312D31762D3363302D302E352C302E352D312C31
          2D3173312C302E352C312C315632392E357A222F3E0D0A3C7061746820636C61
          73733D22426C75652220643D224D32312E312C32342E384332302E362C32342E
          332C31392E392C32342C31392C3234632D302E392C302D312E362C302E332D32
          2E312C302E384331362E332C32352E352C31362C32362E352C31362C32386330
          2C312E352C302E332C322E352C302E382C332E3120202623393B63302E352C30
          2E362C312E332C302E392C322E322C302E3963302E392C302C312E362D302E33
          2C322E312D302E3863302E362D302E362C302E392D312E372C302E392D332E32
          4332322C32362E352C32312E372C32352E352C32312E312C32342E387A204D32
          302C32392E3563302C302E352D302E352C312D312C3120202623393B732D312D
          302E352D312D31762D3363302D302E352C302E352D312C312D3173312C302E35
          2C312C315632392E357A222F3E0D0A3C706F6C79676F6E20636C6173733D2242
          6C75652220706F696E74733D2232342C31302032302C31302032302C36203138
          2C362031382C31302031342C31302031342C31322031382C31322031382C3136
          2032302C31362032302C31322032342C313220222F3E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D22D0
          A1D0BBD0BED0B95F312220786D6C6E733D22687474703A2F2F7777772E77332E
          6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
          2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
          20793D22307078222076696577426F783D223020302033322033322220737479
          6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203332
          2033323B2220786D6C3A73706163653D227072657365727665223E262331333B
          262331303B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B3C7374796C6520
          747970653D22746578742F6373732220786D6C3A73706163653D227072657365
          727665223E2E426C61636B7B66696C6C3A233732373237323B7D262331333B26
          2331303B2623393B2E57686974657B66696C6C3A234646464646463B7D3C2F73
          74796C653E0D0A3C672069643D22D0A1D0BBD0BED0B95F32223E0D0A09093C70
          61746820636C6173733D22426C61636B2220643D224D33302C36763232483056
          364833307A222F3E0D0A09093C7061746820636C6173733D2257686974652220
          643D224D32382C31367636682D366C322D326C2D362D366C2D342C346C2D382D
          386C322D326C362C366C342D346C382C384C32382C31367A222F3E0D0A09093C
          7265637420783D22322220793D22382220636C6173733D225768697465222077
          696474683D223222206865696768743D2232222F3E0D0A09093C726563742078
          3D22322220793D2231322220636C6173733D225768697465222077696474683D
          223222206865696768743D2232222F3E0D0A09093C7265637420783D22322220
          793D2231362220636C6173733D225768697465222077696474683D2232222068
          65696768743D2232222F3E0D0A09093C7265637420783D22322220793D223230
          2220636C6173733D225768697465222077696474683D22322220686569676874
          3D2232222F3E0D0A09093C7265637420783D22322220793D2232342220636C61
          73733D225768697465222077696474683D223222206865696768743D2232222F
          3E0D0A09093C7265637420783D22362220793D2232342220636C6173733D2257
          68697465222077696474683D223222206865696768743D2232222F3E0D0A0909
          3C7265637420783D2231302220793D2232342220636C6173733D225768697465
          222077696474683D223222206865696768743D2232222F3E0D0A09093C726563
          7420783D2231342220793D2232342220636C6173733D22576869746522207769
          6474683D223222206865696768743D2232222F3E0D0A09093C7265637420783D
          2231382220793D2232342220636C6173733D225768697465222077696474683D
          223222206865696768743D2232222F3E0D0A09093C7265637420783D22323222
          20793D2232342220636C6173733D225768697465222077696474683D22322220
          6865696768743D2232222F3E0D0A09093C7265637420783D2232362220793D22
          32342220636C6173733D225768697465222077696474683D2232222068656967
          68743D2232222F3E0D0A093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224E
          65772220786D6C6E733D22687474703A2F2F7777772E77332E6F72672F323030
          302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F7777772E77
          332E6F72672F313939392F786C696E6B2220783D223070782220793D22307078
          222076696577426F783D2230203020333220333222207374796C653D22656E61
          626C652D6261636B67726F756E643A6E6577203020302033322033323B222078
          6D6C3A73706163653D227072657365727665223E262331333B262331303B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B3C7374796C6520747970653D2274
          6578742F637373223E2E426C61636B7B66696C6C3A233732373237323B7D3C2F
          7374796C653E0D0A3C7061746820636C6173733D22426C61636B2220643D224D
          31392C32483543342E342C322C342C322E342C342C3376323463302C302E362C
          302E342C312C312C3168323063302E362C302C312D302E342C312D3156394C31
          392C327A204D32342C323648365634683132763563302C302E362C302E342C31
          2C312C31683520202623393B5632367A222F3E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B3C7374796C6520747970653D22746578742F6373732220786D6C3A737061
          63653D227072657365727665223E2E57686974657B66696C6C3A234646464646
          463B7D262331333B262331303B2623393B2E426C75657B66696C6C3A23333737
          4242363B7D3C2F7374796C653E0D0A3C7265637420636C6173733D2257686974
          652220783D22362220793D2232222077696474683D2231382220686569676874
          3D22313022206F7061636974793D22302E37222F3E0D0A3C7265637420636C61
          73733D22426C75652220783D2232302220793D2232222077696474683D223222
          206865696768743D2238222F3E0D0A3C7061746820636C6173733D22426C7565
          2220643D224D32362C32682D3276313048365632483276323868323856364C32
          362C327A204D32362C323848365631346832305632387A222F3E0D0A3C2F7376
          673E0D0A}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F40000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C0000001E744558745469746C650056696577477269646C696E65
          733B477269646C696E65733B4DE0041B0000004849444154785EED97390A0040
          0803FD53FEFFB66CBDB578C11441C4260CB148D81ED5B7604092BBB4DE405429
          6D207BBF43000215732501320001BE800C40802F200310A098504E1F5D681D9E
          08AB2B410000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D22D0
          A1D0BBD0BED0B95F312220786D6C6E733D22687474703A2F2F7777772E77332E
          6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
          2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
          20793D22307078222076696577426F783D223020302033322033322220737479
          6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203332
          2033323B2220786D6C3A73706163653D227072657365727665223E262331333B
          262331303B3C7374796C6520747970653D22746578742F6373732220786D6C3A
          73706163653D227072657365727665223E2E477265656E7B66696C6C3A233444
          414538393B7D262331333B262331303B2623393B2E57686974657B66696C6C3A
          234646464646463B7D262331333B262331303B2623393B2E7374307B6F706163
          6974793A302E363B7D262331333B262331303B2623393B2E7374317B6F706163
          6974793A302E333B7D3C2F7374796C653E0D0A3C672069643D22D0A1D0BBD0BE
          D0B95F322220636C6173733D22737430223E0D0A09093C7061746820636C6173
          733D22426C61636B2220643D224D342C32763238683234563248347A204D3236
          2C3238483656346832305632387A222F3E0D0A093C2F673E0D0A3C672069643D
          22D0A1D0BBD0BED0B95F332220636C6173733D22737431223E0D0A09093C7265
          637420783D2231322220793D22362220636C6173733D22426C61636B22207769
          6474683D22313222206865696768743D2232222F3E0D0A09093C726563742078
          3D2231322220793D2231302220636C6173733D22426C61636B22207769647468
          3D22313222206865696768743D2232222F3E0D0A09093C7265637420783D2231
          322220793D2231342220636C6173733D22426C61636B222077696474683D2231
          3222206865696768743D2232222F3E0D0A09093C7265637420783D2231322220
          793D2231382220636C6173733D22426C61636B222077696474683D2234222068
          65696768743D2232222F3E0D0A09093C7265637420783D2231322220793D2232
          322220636C6173733D22426C61636B222077696474683D223422206865696768
          743D2232222F3E0D0A09093C7265637420783D22382220793D22362220636C61
          73733D22426C61636B222077696474683D223222206865696768743D2232222F
          3E0D0A09093C7265637420783D22382220793D2231302220636C6173733D2242
          6C61636B222077696474683D223222206865696768743D2232222F3E0D0A0909
          3C7265637420783D22382220793D2231342220636C6173733D22426C61636B22
          2077696474683D223222206865696768743D2232222F3E0D0A09093C72656374
          20783D22382220793D2231382220636C6173733D22426C61636B222077696474
          683D223222206865696768743D2232222F3E0D0A09093C7265637420783D2238
          2220793D2232322220636C6173733D22426C61636B222077696474683D223222
          206865696768743D2232222F3E0D0A093C2F673E0D0A3C7265637420783D2231
          382220793D2231382220636C6173733D22477265656E222077696474683D2231
          3422206865696768743D223134222F3E0D0A3C706F6C79676F6E20636C617373
          3D2257686974652220706F696E74733D2232322C32332032342C32352032382C
          32312033302C32332032342C32392032302C323520222F3E0D0A3C2F7376673E
          0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D22D0
          A1D0BBD0BED0B95F312220786D6C6E733D22687474703A2F2F7777772E77332E
          6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
          2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
          20793D22307078222076696577426F783D223020302031362031362220737479
          6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203136
          2031363B2220786D6C3A73706163653D227072657365727665223E262331333B
          262331303B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B3C7374796C6520
          747970653D22746578742F6373732220786D6C3A73706163653D227072657365
          727665223E2E7374307B66696C6C3A233337374242363B7D262331333B262331
          303B2623393B2E7374317B6F7061636974793A302E333B7D3C2F7374796C653E
          0D0A3C673E0D0A09093C673E0D0A0909093C706F6C79676F6E20636C6173733D
          227374302220706F696E74733D2231302C332031332C302031362C3320262339
          3B2623393B222F3E0D0A09093C2F673E0D0A093C2F673E0D0A3C6720636C6173
          733D22737431223E0D0A09093C673E0D0A0909093C7061746820643D224D3132
          2C3135682D312E386C2D312E342D332E3448342E324C322E392C313548314C35
          2E362C3368312E384C31322C31357A204D382E332C31302E344C362E352C342E
          386C2D312E382C352E3648382E337A222F3E0D0A09093C2F673E0D0A09093C67
          3E0D0A0909093C7061746820643D224D31322C3135682D312E386C2D312E342D
          332E3448342E324C322E392C313548314C352E362C3368312E384C31322C3135
          7A204D382E332C31302E344C362E352C342E386C2D312E382C352E3648382E33
          7A222F3E0D0A09093C2F673E0D0A093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D22D0
          A1D0BBD0BED0B95F312220786D6C6E733D22687474703A2F2F7777772E77332E
          6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
          2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
          20793D22307078222076696577426F783D223020302031362031362220737479
          6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203136
          2031363B2220786D6C3A73706163653D227072657365727665223E262331333B
          262331303B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B3C7374796C6520
          747970653D22746578742F6373732220786D6C3A73706163653D227072657365
          727665223E2E7374307B6F7061636974793A302E333B7D262331333B26233130
          3B2623393B2E7374317B66696C6C3A233337374242363B7D3C2F7374796C653E
          0D0A3C6720636C6173733D22737430223E0D0A09093C673E0D0A0909093C7061
          746820643D224D31322C3135682D312E386C2D312E342D332E3448342E324C32
          2E392C313548314C352E362C3368312E384C31322C31357A204D382E332C3130
          2E344C362E352C342E386C2D312E382C352E3648382E337A222F3E0D0A09093C
          2F673E0D0A09093C673E0D0A0909093C7061746820636C6173733D22426C6163
          6B2220643D224D31322C3135682D312E386C2D312E342D332E3448342E324C32
          2E392C313548314C352E362C3368312E384C31322C31357A204D382E332C3130
          2E344C362E352C342E386C2D312E382C352E3648382E337A222F3E0D0A09093C
          2F673E0D0A093C2F673E0D0A3C673E0D0A09093C706F6C79676F6E20636C6173
          733D227374312220706F696E74733D2231302C302031332C332031362C302026
          23393B222F3E0D0A093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D2242
          6F6C642220786D6C6E733D22687474703A2F2F7777772E77332E6F72672F3230
          30302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F7777772E
          77332E6F72672F313939392F786C696E6B2220783D223070782220793D223070
          78222076696577426F783D2230203020333220333222207374796C653D22656E
          61626C652D6261636B67726F756E643A6E6577203020302033322033323B2220
          786D6C3A73706163653D227072657365727665223E262331333B262331303B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B3C7374796C6520747970653D22
          746578742F637373223E2E426C61636B7B66696C6C3A233732373237323B7D3C
          2F7374796C653E0D0A3C7061746820636C6173733D22426C61636B2220643D22
          4D382C32365634683863322E352C302C342E332C302E352C352E372C312E3463
          312E332C302E392C322C322E322C322C332E3863302C312E322D302E342C322E
          322D312E322C332E31632D302E382C302E392D312E382C312E352D332E312C31
          2E3976302E3120202623393B63312E362C302E322C322E382C302E382C332E38
          2C312E3763302E392C312C312E342C322E312C312E342C332E3563302C322D30
          2E372C332E362D322E322C342E384332302E392C32352E342C31392C32362C31
          362E352C323648387A204D31332C372E3776352E3268322E3220202623393B63
          312C302C312E382D302E322C322E342D302E3763302E362D302E352C302E392D
          312E322C302E392D3263302D312E362D312E322D322E342D332E362D322E3448
          31337A204D31332C31362E3676352E3868322E3763312E312C302C322D302E33
          2C322E372D302E3863302E362D302E352C312D312E332C312D322E3220202623
          393B63302D302E392D302E332D312E362D312D322E31632D302E362D302E352D
          312E352D302E382D322E372D302E384831337A222F3E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D2249
          74616C69632220786D6C6E733D22687474703A2F2F7777772E77332E6F72672F
          323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F7777
          772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D22
          307078222076696577426F783D2230203020333220333222207374796C653D22
          656E61626C652D6261636B67726F756E643A6E6577203020302033322033323B
          2220786D6C3A73706163653D227072657365727665223E262331333B26233130
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B3C7374796C652074797065
          3D22746578742F637373223E2E426C61636B7B66696C6C3A233732373237323B
          7D3C2F7374796C653E0D0A3C706F6C79676F6E20636C6173733D22426C61636B
          2220706F696E74733D2232312E362C362032322C342031342C342031332E362C
          362031352E382C362031322C323420392E382C323420392E342C32362031372E
          362C32362031372E382C32342031352E362C32342031392E342C3620222F3E0D
          0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D2255
          6E6465726C696E652220786D6C6E733D22687474703A2F2F7777772E77332E6F
          72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F
          2F7777772E77332E6F72672F313939392F786C696E6B2220783D223070782220
          793D22307078222076696577426F783D2230203020333220333222207374796C
          653D22656E61626C652D6261636B67726F756E643A6E65772030203020333220
          33323B2220786D6C3A73706163653D227072657365727665223E262331333B26
          2331303B2623393B2623393B2623393B2623393B2623393B2623393B2623393B
          2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B
          2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B
          2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B
          2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B
          2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B
          2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B
          2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B
          2623393B2623393B2623393B2623393B2623393B2623393B3C7374796C652074
          7970653D22746578742F637373223E2E426C61636B7B66696C6C3A2337323732
          37323B7D3C2F7374796C653E0D0A3C7061746820636C6173733D22426C61636B
          2220643D224D382C31352E37563468332E3476313163302C342C312E362C362C
          342E372C3663332C302C342E352D312E392C342E352D352E3856344832347631
          312E3463302C352E372D322E372C382E362D382E322C382E3620202623393B43
          31302E362C32342C382C32312E322C382C31352E377A204D362C323876326832
          30762D3248367A222F3E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D2253
          7472696B656F75742220786D6C6E733D22687474703A2F2F7777772E77332E6F
          72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F
          2F7777772E77332E6F72672F313939392F786C696E6B2220783D223070782220
          793D22307078222076696577426F783D2230203020333220333222207374796C
          653D22656E61626C652D6261636B67726F756E643A6E65772030203020333220
          33323B2220786D6C3A73706163653D227072657365727665223E262331333B26
          2331303B2623393B2623393B2623393B2623393B2623393B2623393B2623393B
          2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B
          2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B
          2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B
          2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B
          2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B
          2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B
          2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B
          2623393B2623393B2623393B2623393B2623393B2623393B3C7374796C652074
          7970653D22746578742F637373223E2E426C61636B7B66696C6C3A2337323732
          37323B7D3C2F7374796C653E0D0A3C7061746820636C6173733D22426C61636B
          2220643D224D32342C3134682D362E39632D302E322D302E312D302E342D302E
          322D302E372D302E34632D302E372D302E332D312E342D302E362D322D302E39
          632D302E362D302E332D312D302E362D312E342D302E38632D302E342D302E33
          2D302E372D302E362D302E392D3120202623393B632D302E322D302E332D302E
          332D302E382D302E332D312E3263302D302E352C302E312D302E392C302E342D
          312E3363302E332D302E342C302E362D302E362C312D302E3963302E342D302E
          322C302E392D302E342C312E342D302E3563302E352D302E312C312E312D302E
          312C312E362D302E3120202623393B63322C302C332E352C302E342C342E382C
          312E3356342E38632D312D302E352D322E372D302E382D352D302E38632D312C
          302D322C302E312D332C302E33632D312C302E322D312E382C302E362D322E36
          2C312E3143392E382C352E392C392E322C362E352C382E372C372E3243382E32
          2C382C382C382E382C382C392E3820202623393B63302C302E382C302E312C31
          2E352C302E342C322E3163302E332C302E362C302E362C312E312C312E312C31
          2E3663302E312C302E312C302E332C302E332C302E352C302E34483676326837
          2E3163302E312C302C302E312C302E312C302E322C302E3163302E372C302E33
          2C312E332C302E362C312E392C302E3920202623393B63302E362C302E332C31
          2E312C302E362C312E362C302E3963302E342C302E332C302E382C302E362C31
          2C3163302E322C302E342C302E342C302E382C302E342C312E3363302C302E39
          2D302E342C312E372D312E312C322E32632D302E372C302E352D312E392C302E
          372D332E342C302E3720202623393B632D302E342C302D302E392C302D312E34
          2D302E31632D302E352D302E312D312D302E322D312E362D302E34632D302E35
          2D302E322D312D302E342D312E352D302E36632D302E352D302E322D302E392D
          302E352D312E322D302E3876332E3663302E332C302E322C302E372C302E342C
          312E322C302E3520202623393B63302E352C302E312C312C302E332C312E362C
          302E3463302E362C302E312C312E312C302E322C312E362C302E3263302E352C
          302E312C312C302E312C312E342C302E3163312E312C302C322E312D302E312C
          332E312D302E3363312D302E322C312E392D302E352C322E362D312020262339
          3B63302E382D302E352C312E342D312E312C312E382D312E3863302E342D302E
          372C302E372D312E372C302E372D322E3763302D302E382D302E322D312E352D
          302E352D322E31632D302E332D302E362D302E372D312E322D312E322D312E37
          632D302E312D302E312D302E322D302E322D302E332D302E3368345631347A22
          2F3E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B3C7374796C6520747970653D22746578742F6373732220786D6C3A737061
          63653D227072657365727665223E2E7374307B6F7061636974793A302E333B7D
          262331333B262331303B2623393B2E7374317B66696C6C3A234646464646463B
          7D262331333B262331303B2623393B2E7374327B66696C6C3A23443034443246
          3B7D262331333B262331303B2623393B2E7374337B6F7061636974793A302E38
          3B7D262331333B262331303B2623393B2E7374347B6F7061636974793A302E34
          3B7D3C2F7374796C653E0D0A3C6720636C6173733D22737430223E0D0A09093C
          706F6C79676F6E20706F696E74733D22332C31342031352C322032382C313520
          31362C3237202623393B222F3E0D0A093C2F673E0D0A3C7061746820636C6173
          733D227374312220643D224D31372C313063312E372C302C332C312E332C332C
          33732D312E332C332D332C33732D332D312E332D332D335331352E332C31302C
          31372C31307A222F3E0D0A3C7061746820636C6173733D227374322220643D22
          4D33322C32362E364C32392C31366C2D332C31302E3663302C302E312C302C30
          2E322C302C302E3463302C312E372C312E332C332C332C3373332D312E332C33
          2D334333322C32362E392C33322C32362E382C33322C32362E367A222F3E0D0A
          3C6720636C6173733D22737433223E0D0A09093C706F6C79676F6E20706F696E
          74733D22352C3320372C312031392C31332031372C3135202623393B222F3E0D
          0A093C2F673E0D0A3C6720636C6173733D22737434223E0D0A09093C706F6C79
          676F6E20706F696E74733D22302C313720332C31342031362C32372031332C33
          30202623393B222F3E0D0A093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B3C7374796C6520747970653D22746578742F6373732220786D6C3A737061
          63653D227072657365727665223E2E477265656E7B66696C6C3A233444414538
          393B7D262331333B262331303B2623393B2E426C75657B66696C6C3A23333737
          4242363B7D262331333B262331303B2623393B2E7374307B6F7061636974793A
          302E363B7D262331333B262331303B2623393B2E7374317B6F7061636974793A
          302E343B7D262331333B262331303B2623393B2E7374327B6F7061636974793A
          302E37353B7D262331333B262331303B2623393B2E7374337B6F706163697479
          3A302E323B7D262331333B262331303B2623393B2E7374347B6F706163697479
          3A302E353B7D3C2F7374796C653E0D0A3C7265637420783D2232322220793D22
          322220636C6173733D22426C7565222077696474683D22382220686569676874
          3D2238222F3E0D0A3C7265637420783D2232322220793D2231322220636C6173
          733D22477265656E222077696474683D223822206865696768743D2238222F3E
          0D0A3C6720636C6173733D22737430223E0D0A09093C7265637420783D223232
          2220793D2232322220636C6173733D22426C61636B222077696474683D223822
          206865696768743D2238222F3E0D0A093C2F673E0D0A3C6720636C6173733D22
          737431223E0D0A09093C7265637420783D2231322220793D2232322220636C61
          73733D22426C61636B222077696474683D223822206865696768743D2238222F
          3E0D0A093C2F673E0D0A3C6720636C6173733D22737432223E0D0A09093C7265
          637420783D2231322220793D2231322220636C6173733D22477265656E222077
          696474683D223822206865696768743D2238222F3E0D0A093C2F673E0D0A3C67
          20636C6173733D22737432223E0D0A09093C7265637420783D2231322220793D
          22322220636C6173733D22426C7565222077696474683D223822206865696768
          743D2238222F3E0D0A093C2F673E0D0A3C6720636C6173733D22737433223E0D
          0A09093C7265637420783D22322220793D2232322220636C6173733D22426C61
          636B222077696474683D223822206865696768743D2238222F3E0D0A093C2F67
          3E0D0A3C6720636C6173733D22737434223E0D0A09093C7265637420783D2232
          2220793D2231322220636C6173733D22477265656E222077696474683D223822
          206865696768743D2238222F3E0D0A093C2F673E0D0A3C6720636C6173733D22
          737434223E0D0A09093C7265637420783D22322220793D22322220636C617373
          3D22426C7565222077696474683D223822206865696768743D2238222F3E0D0A
          093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B3C7374796C6520747970
          653D22746578742F6373732220786D6C3A73706163653D227072657365727665
          223E2E426C75657B66696C6C3A233337374242363B7D262331333B262331303B
          2623393B2E477265656E7B66696C6C3A233444414538393B7D262331333B2623
          31303B2623393B2E7374307B6F7061636974793A302E363B7D3C2F7374796C65
          3E0D0A3C673E0D0A09093C673E0D0A0909093C7061746820636C6173733D2247
          7265656E2220643D224D362E322C32382E3663322E322C312E372C342E392C32
          2E392C372E382C332E33762D362E31632D312E332D302E332D322E352D302E38
          2D332E352D312E354C362E322C32382E367A222F3E0D0A0909093C7061746820
          636C6173733D22477265656E2220643D224D32312E352C32342E33632D312C30
          2E372D322E322C312E322D332E352C312E3576362E3163322E392D302E342C35
          2E362D312E352C372E382D332E324C32312E352C32342E337A222F3E0D0A0909
          3C2F673E0D0A09093C673E0D0A0909093C7061746820636C6173733D22477265
          656E2220643D224D362E322C32382E3663322E322C312E372C342E392C322E39
          2C372E382C332E33762D362E31632D312E332D302E332D322E352D302E382D33
          2E352D312E354C362E322C32382E367A222F3E0D0A0909093C7061746820636C
          6173733D22477265656E2220643D224D32312E352C32342E33632D312C302E37
          2D322E322C312E322D332E352C312E3576362E3163322E392D302E342C352E36
          2D312E352C372E382D332E324C32312E352C32342E337A222F3E0D0A09093C2F
          673E0D0A093C2F673E0D0A3C673E0D0A09093C7061746820636C6173733D2242
          6C75652220643D224D31382C302E3176362E3163312E332C302E332C322E352C
          302E382C332E352C312E356C342E332D342E334332332E362C312E372C32302E
          392C302E352C31382C302E317A222F3E0D0A09093C7061746820636C6173733D
          22426C75652220643D224D31302E352C372E3763312D302E372C322E322D312E
          322C332E352D312E3556302E31632D322E392C302E342D352E362C312E352D37
          2E382C332E324C31302E352C372E377A222F3E0D0A093C2F673E0D0A3C672063
          6C6173733D22737430223E0D0A09093C7061746820636C6173733D22426C7565
          2220643D224D302E312C313468362E3163302E332D312E332C302E382D322E35
          2C312E352D332E354C332E342C362E3243312E372C382E342C302E352C31312E
          312C302E312C31347A222F3E0D0A09093C7061746820636C6173733D22426C75
          652220643D224D32382E362C362E326C2D342E332C342E3363302E372C312C31
          2E322C322E322C312E352C332E3568362E314333312E352C31312E312C33302E
          332C382E342C32382E362C362E327A222F3E0D0A093C2F673E0D0A3C6720636C
          6173733D22737430223E0D0A09093C7061746820636C6173733D22477265656E
          2220643D224D302E312C313863302E342C322E392C312E362C352E362C332E33
          2C372E386C342E332D342E33632D302E372D312D312E322D322E322D312E352D
          332E3548302E317A222F3E0D0A09093C7061746820636C6173733D2247726565
          6E2220643D224D32352E382C3138632D302E332C312E332D302E382C322E352D
          312E352C332E356C342E332C342E3363312E372D322E322C322E392D342E392C
          332E332D372E384832352E387A222F3E0D0A093C2F673E0D0A3C2F7376673E0D
          0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B3C7374796C6520747970653D22746578742F63737322
          3E2E7374307B6F7061636974793A302E363B7D3C2F7374796C653E0D0A3C6720
          636C6173733D22737430223E0D0A09093C7265637420783D22362220793D2232
          2220636C6173733D22426C61636B222077696474683D22323022206865696768
          743D2232222F3E0D0A09093C7265637420783D2231302220793D22362220636C
          6173733D22426C61636B222077696474683D22313222206865696768743D2232
          222F3E0D0A09093C7265637420783D22362220793D2231302220636C6173733D
          22426C61636B222077696474683D22323022206865696768743D2232222F3E0D
          0A093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B3C7374796C6520747970653D22746578742F63737322
          3E2E7374307B6F7061636974793A302E363B7D3C2F7374796C653E0D0A3C6720
          636C6173733D22737430223E0D0A09093C7265637420783D22362220793D2238
          2220636C6173733D22426C61636B222077696474683D22323022206865696768
          743D2232222F3E0D0A09093C7265637420783D2231302220793D223132222063
          6C6173733D22426C61636B222077696474683D22313222206865696768743D22
          32222F3E0D0A09093C7265637420783D22362220793D2231362220636C617373
          3D22426C61636B222077696474683D22323022206865696768743D2232222F3E
          0D0A093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B3C7374796C6520747970653D22746578742F63737322
          3E2E7374307B6F7061636974793A302E363B7D3C2F7374796C653E0D0A3C6720
          636C6173733D22737430223E0D0A09093C7265637420783D22362220793D2231
          362220636C6173733D22426C61636B222077696474683D223230222068656967
          68743D2232222F3E0D0A09093C7265637420783D2231302220793D2232302220
          636C6173733D22426C61636B222077696474683D22313222206865696768743D
          2232222F3E0D0A09093C7265637420783D22362220793D2232342220636C6173
          733D22426C61636B222077696474683D22323022206865696768743D2232222F
          3E0D0A093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B3C7374796C6520747970653D22746578742F63737322
          3E2E7374307B6F7061636974793A302E363B7D3C2F7374796C653E0D0A3C6720
          636C6173733D22737430223E0D0A09093C673E0D0A0909093C7265637420783D
          22362220793D22382220636C6173733D22426C61636B222077696474683D2232
          3022206865696768743D2232222F3E0D0A0909093C7265637420783D22362220
          793D2231322220636C6173733D22426C61636B222077696474683D2231362220
          6865696768743D2232222F3E0D0A0909093C7265637420783D22362220793D22
          31362220636C6173733D22426C61636B222077696474683D2232302220686569
          6768743D2232222F3E0D0A0909093C7265637420783D22362220793D22323022
          20636C6173733D22426C61636B222077696474683D2231362220686569676874
          3D2232222F3E0D0A0909093C7265637420783D22362220793D2232342220636C
          6173733D22426C61636B222077696474683D22323022206865696768743D2232
          222F3E0D0A09093C2F673E0D0A093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B3C7374796C6520747970653D22746578742F637373223E2E7374
          307B6F7061636974793A302E363B7D3C2F7374796C653E0D0A3C6720636C6173
          733D22737430223E0D0A09093C7265637420783D22362220793D22382220636C
          6173733D22426C61636B222077696474683D22323022206865696768743D2232
          222F3E0D0A09093C7265637420783D2231302220793D2231322220636C617373
          3D22426C61636B222077696474683D22313222206865696768743D2232222F3E
          0D0A09093C7265637420783D22362220793D2231362220636C6173733D22426C
          61636B222077696474683D22323022206865696768743D2232222F3E0D0A0909
          3C7265637420783D2231302220793D2232302220636C6173733D22426C61636B
          222077696474683D22313222206865696768743D2232222F3E0D0A09093C7265
          637420783D22362220793D2232342220636C6173733D22426C61636B22207769
          6474683D22323022206865696768743D2232222F3E0D0A093C2F673E0D0A3C2F
          7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B3C7374796C6520747970653D22746578742F63737322
          3E2E7374307B6F7061636974793A302E363B7D3C2F7374796C653E0D0A3C6720
          636C6173733D22737430223E0D0A09093C7265637420783D22362220793D2238
          2220636C6173733D22426C61636B222077696474683D22323022206865696768
          743D2232222F3E0D0A09093C7265637420783D22392E392220793D2231322220
          636C6173733D22426C61636B222077696474683D2231362E3122206865696768
          743D2232222F3E0D0A09093C7265637420783D22362220793D2231362220636C
          6173733D22426C61636B222077696474683D22323022206865696768743D2232
          222F3E0D0A09093C7265637420783D22392E392220793D2232302220636C6173
          733D22426C61636B222077696474683D2231362E3122206865696768743D2232
          222F3E0D0A09093C7265637420783D22362220793D2232342220636C6173733D
          22426C61636B222077696474683D22323022206865696768743D2232222F3E0D
          0A093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B3C7374796C6520747970653D22746578742F6373732220786D6C3A737061
          63653D227072657365727665223E2E7374307B6F7061636974793A302E333B7D
          262331333B262331303B2623393B2E7374317B66696C6C3A234646464646463B
          7D3C2F7374796C653E0D0A3C6720636C6173733D22737430223E0D0A09093C67
          3E0D0A0909093C7061746820643D224D32382C32304C382C327632366C362D36
          6C352C31306C362D336C2D352E312D394832387A222F3E0D0A09093C2F673E0D
          0A09093C673E0D0A0909093C7061746820643D224D32382C32304C382C327632
          366C362D366C352C31306C362D336C2D352E312D394832387A222F3E0D0A0909
          3C2F673E0D0A093C2F673E0D0A3C706F6C79676F6E20636C6173733D22737431
          2220706F696E74733D2231382C31392032342E352C313920392E352C352E3520
          392E352C32342E372031342E332C32302031392E352C33302032332C32382E33
          20222F3E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B3C7374796C6520747970653D22746578742F6373732220786D6C3A737061
          63653D227072657365727665223E2E57686974657B66696C6C3A234646464646
          463B7D262331333B262331303B2623393B2E426C75657B66696C6C3A23333737
          4242363B7D262331333B262331303B2623393B2E7374307B6F7061636974793A
          302E363B7D3C2F7374796C653E0D0A3C6720636C6173733D22737430223E0D0A
          09093C706F6C79676F6E20636C6173733D22426C61636B2220706F696E74733D
          2232382C362032382C313820362C313820362C323420342C323420342C313620
          32362C31362032362C36202623393B222F3E0D0A093C2F673E0D0A3C673E0D0A
          09093C636972636C6520636C6173733D22426C7565222063783D223136222063
          793D2231372220723D2235222F3E0D0A09093C636972636C6520636C6173733D
          22426C7565222063783D223237222063793D22372220723D2235222F3E0D0A09
          093C636972636C6520636C6173733D22426C7565222063783D2235222063793D
          2232372220723D2235222F3E0D0A093C2F673E0D0A3C673E0D0A09093C636972
          636C6520636C6173733D225768697465222063783D223136222063793D223137
          2220723D22322E35222F3E0D0A09093C636972636C6520636C6173733D225768
          697465222063783D223237222063793D22372220723D22322E35222F3E0D0A09
          093C636972636C6520636C6173733D225768697465222063783D223522206379
          3D2232372220723D22322E35222F3E0D0A093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D2252
          65646F2220786D6C6E733D22687474703A2F2F7777772E77332E6F72672F3230
          30302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F7777772E
          77332E6F72672F313939392F786C696E6B2220783D223070782220793D223070
          78222076696577426F783D2230203020333220333222207374796C653D22656E
          61626C652D6261636B67726F756E643A6E6577203020302033322033323B2220
          786D6C3A73706163653D227072657365727665223E262331333B262331303B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B3C7374796C65207479
          70653D22746578742F637373223E2E426C75657B66696C6C3A23313137374437
          3B7D3C2F7374796C653E0D0A3C7061746820636C6173733D22426C7565222064
          3D224D31362C313056346C31362C31304C31362C3234762D3643302C31382C30
          2C32362C302C323653302C31302C31362C31307A222F3E0D0A3C2F7376673E0D
          0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B3C7374796C6520747970653D22746578742F6373732220786D6C3A737061
          63653D227072657365727665223E2E57686974657B66696C6C3A234646464646
          463B7D262331333B262331303B2623393B2E426C75657B66696C6C3A23333737
          4242363B7D262331333B262331303B2623393B2E7374307B6F7061636974793A
          302E363B7D3C2F7374796C653E0D0A3C6720636C6173733D22737430223E0D0A
          09093C7265637420783D22362220793D22382220636C6173733D22426C61636B
          222077696474683D22323022206865696768743D223230222F3E0D0A093C2F67
          3E0D0A3C7265637420783D22362220636C6173733D22426C7565222077696474
          683D22323022206865696768743D2236222F3E0D0A3C673E0D0A09093C726563
          7420783D22382220793D2231302220636C6173733D2257686974652220776964
          74683D22313622206865696768743D223136222F3E0D0A09093C726563742078
          3D22382220793D22322220636C6173733D225768697465222077696474683D22
          3422206865696768743D2232222F3E0D0A09093C7265637420783D2231342220
          793D22322220636C6173733D225768697465222077696474683D223422206865
          696768743D2232222F3E0D0A09093C7265637420783D2232302220793D223222
          20636C6173733D225768697465222077696474683D223422206865696768743D
          2232222F3E0D0A093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D22D0
          A1D0BBD0BED0B95F312220786D6C6E733D22687474703A2F2F7777772E77332E
          6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
          2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
          20793D22307078222076696577426F783D223020302033322033322220737479
          6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203332
          2033323B2220786D6C3A73706163653D227072657365727665223E262331333B
          262331303B3C7374796C6520747970653D22746578742F6373732220786D6C3A
          73706163653D227072657365727665223E2E426C75657B66696C6C3A23333737
          4242363B7D262331333B262331303B2623393B2E57686974657B66696C6C3A23
          4646464646463B7D262331333B262331303B2623393B2E7374307B6F70616369
          74793A302E363B7D262331333B262331303B2623393B2E7374317B6F70616369
          74793A302E373B7D3C2F7374796C653E0D0A3C672069643D22D0A1D0BBD0BED0
          B95F322220636C6173733D22737430223E0D0A09093C7061746820636C617373
          3D22426C61636B2220643D224D31382C3576313463302C302E352D302E352C31
          2D312C316C302C30632D302E352C302D312D302E352D312D31762D3663302D31
          2E372D312E332D332D332D336C302C30632D312E372C302D332C312E332D332C
          33763131683256313320202623393B2623393B63302D302E352C302E352D312C
          312D316C302C3063302E352C302C312C302E352C312C31763663302C312E372C
          312E332C332C332C336C302C3063312E372C302C332D312E332C332D33563548
          31387A222F3E0D0A09093C7265637420783D22322220793D22382220636C6173
          733D22426C61636B222077696474683D223222206865696768743D223136222F
          3E0D0A09093C706F6C79676F6E20706F696E74733D2232382C382032382C3134
          2032342C31342032342C32342032362C32342032362C31362033302C31362033
          302C38202623393B222F3E0D0A093C2F673E0D0A3C7265637420793D22322220
          636C6173733D22426C7565222077696474683D223622206865696768743D2236
          222F3E0D0A3C6720636C6173733D22737431223E0D0A09093C7061746820636C
          6173733D2257686974652220643D224D322C3476326832563448327A222F3E0D
          0A093C2F673E0D0A3C7265637420783D2231362220793D22322220636C617373
          3D22426C7565222077696474683D223622206865696768743D2236222F3E0D0A
          3C6720636C6173733D22737431223E0D0A09093C7061746820636C6173733D22
          57686974652220643D224D31382C347632683256344831387A222F3E0D0A093C
          2F673E0D0A3C7265637420783D2232362220793D22322220636C6173733D2242
          6C7565222077696474683D223622206865696768743D2236222F3E0D0A3C6720
          636C6173733D22737431223E0D0A09093C7061746820636C6173733D22576869
          74652220643D224D32382C347632683256344832387A222F3E0D0A093C2F673E
          0D0A3C7265637420783D2232322220793D2232342220636C6173733D22426C75
          65222077696474683D223622206865696768743D2236222F3E0D0A3C6720636C
          6173733D22737431223E0D0A09093C7061746820636C6173733D225768697465
          2220643D224D32342C323676326832762D324832347A222F3E0D0A093C2F673E
          0D0A3C7265637420783D22382220793D2232342220636C6173733D22426C7565
          222077696474683D223622206865696768743D2236222F3E0D0A3C6720636C61
          73733D22737431223E0D0A09093C7061746820636C6173733D22576869746522
          20643D224D31302C323676326832762D324831307A222F3E0D0A093C2F673E0D
          0A3C7265637420793D2232342220636C6173733D22426C756522207769647468
          3D223622206865696768743D2236222F3E0D0A3C6720636C6173733D22737431
          223E0D0A09093C7061746820636C6173733D2257686974652220643D224D322C
          323676326832762D3248327A222F3E0D0A093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D22D0
          A1D0BBD0BED0B95F312220786D6C6E733D22687474703A2F2F7777772E77332E
          6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
          2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
          20793D22307078222076696577426F783D223020302033322033322220737479
          6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203332
          2033323B2220786D6C3A73706163653D227072657365727665223E262331333B
          262331303B3C7374796C6520747970653D22746578742F6373732220786D6C3A
          73706163653D227072657365727665223E2E426C75657B66696C6C3A23333737
          4242363B7D262331333B262331303B2623393B2E57686974657B66696C6C3A23
          4646464646463B7D262331333B262331303B2623393B2E7374307B6F70616369
          74793A302E363B7D262331333B262331303B2623393B2E7374317B6F70616369
          74793A302E373B7D3C2F7374796C653E0D0A3C672069643D22D181D0BBD0BED0
          B95F322220636C6173733D22737430223E0D0A09093C706F6C79676F6E20636C
          6173733D22426C61636B2220706F696E74733D2231302C362032362C36203236
          2C32302032342C32302032342C382031302C382031302C36202623393B222F3E
          0D0A093C2F673E0D0A3C7265637420783D22342220793D22342220636C617373
          3D22426C7565222077696474683D223622206865696768743D2236222F3E0D0A
          3C6720636C6173733D22737431223E0D0A09093C7061746820636C6173733D22
          57686974652220643D224D362C3676326832563648367A222F3E0D0A093C2F67
          3E0D0A3C7265637420783D2232322220793D2232302220636C6173733D22426C
          7565222077696474683D223622206865696768743D2236222F3E0D0A3C672063
          6C6173733D22737431223E0D0A09093C7061746820636C6173733D2257686974
          652220643D224D32342C323276326832762D324832347A222F3E0D0A093C2F67
          3E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D22D0
          A1D0BBD0BED0B95F312220786D6C6E733D22687474703A2F2F7777772E77332E
          6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
          2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
          20793D22307078222076696577426F783D223020302033322033322220737479
          6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203332
          2033323B2220786D6C3A73706163653D227072657365727665223E262331333B
          262331303B3C7374796C6520747970653D22746578742F6373732220786D6C3A
          73706163653D227072657365727665223E2E426C75657B66696C6C3A23333737
          4242363B7D262331333B262331303B2623393B2E57686974657B66696C6C3A23
          4646464646463B7D262331333B262331303B2623393B2E7374307B6F70616369
          74793A302E363B7D262331333B262331303B2623393B2E7374317B6F70616369
          74793A302E373B7D3C2F7374796C653E0D0A3C672069643D22D181D0BBD0BED0
          B95F322220636C6173733D22737430223E0D0A09093C706F6C79676F6E20636C
          6173733D22426C61636B2220706F696E74733D22362C3820382C382C20382C32
          322032322C32322032322C323420362C323420362C38202623393B222F3E0D0A
          093C2F673E0D0A3C7265637420783D22342220793D22342220636C6173733D22
          426C7565222077696474683D223622206865696768743D2236222F3E0D0A3C67
          20636C6173733D22737431223E0D0A09093C7061746820636C6173733D225768
          6974652220643D224D362C3676326832563648367A222F3E0D0A093C2F673E0D
          0A3C7265637420783D2232322220793D2232302220636C6173733D22426C7565
          222077696474683D223622206865696768743D2236222F3E0D0A3C6720636C61
          73733D22737431223E0D0A09093C7061746820636C6173733D22576869746522
          20643D224D32342C323276326832762D324832347A222F3E0D0A093C2F673E0D
          0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D22D0
          A1D0BBD0BED0B95F312220786D6C6E733D22687474703A2F2F7777772E77332E
          6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
          2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
          20793D22307078222076696577426F783D223020302033322033322220737479
          6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203332
          2033323B2220786D6C3A73706163653D227072657365727665223E262331333B
          262331303B3C7374796C6520747970653D22746578742F6373732220786D6C3A
          73706163653D227072657365727665223E2E426C75657B66696C6C3A23333737
          4242363B7D262331333B262331303B2623393B2E57686974657B66696C6C3A23
          4646464646463B7D262331333B262331303B2623393B2E7374307B6F70616369
          74793A302E363B7D262331333B262331303B2623393B2E7374317B6F70616369
          74793A302E373B7D3C2F7374796C653E0D0A3C672069643D22D0A1D0BBD0BED0
          B95F322220636C6173733D22737430223E0D0A09093C7061746820636C617373
          3D22426C61636B2220643D224D32322C32324839632D302E352C302D312D302E
          352D312D31762D3463302D302E352C302E352D312C312D3168313463312E372C
          302C332D312E332C332D33563963302D312E372D312E332D332D332D33483876
          3268313563302E352C302C312C302E352C312C3120202623393B2623393B7634
          63302C302E352D302E352C312D312C314839632D312E372C302D332C312E332D
          332C33763463302C312E372C312E332C332C332C336831335632327A222F3E0D
          0A093C2F673E0D0A3C7265637420783D22342220793D22342220636C6173733D
          22426C7565222077696474683D223622206865696768743D2236222F3E0D0A3C
          6720636C6173733D22737431223E0D0A09093C7061746820636C6173733D2257
          686974652220643D224D362C3676326832563648367A222F3E0D0A093C2F673E
          0D0A3C7265637420783D2232322220793D2232302220636C6173733D22426C75
          65222077696474683D223622206865696768743D2236222F3E0D0A3C6720636C
          6173733D22737431223E0D0A09093C7061746820636C6173733D225768697465
          2220643D224D32342C323276326832762D324832347A222F3E0D0A093C2F673E
          0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D22D0
          A1D0BBD0BED0B95F312220786D6C6E733D22687474703A2F2F7777772E77332E
          6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
          2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
          20793D22307078222076696577426F783D223020302033322033322220737479
          6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203332
          2033323B2220786D6C3A73706163653D227072657365727665223E262331333B
          262331303B3C7374796C6520747970653D22746578742F6373732220786D6C3A
          73706163653D227072657365727665223E2E426C75657B66696C6C3A23333737
          4242363B7D262331333B262331303B2623393B2E57686974657B66696C6C3A23
          4646464646463B7D262331333B262331303B2623393B2E7374307B6F70616369
          74793A302E363B7D262331333B262331303B2623393B2E7374317B6F70616369
          74793A302E373B7D3C2F7374796C653E0D0A3C672069643D22D0A1D0BBD0BED0
          B95F322220636C6173733D22737430223E0D0A09093C706F6C79676F6E20706F
          696E74733D2231302C382032342C32302032322C323220382C3130202623393B
          222F3E0D0A093C2F673E0D0A3C7265637420783D22342220793D22342220636C
          6173733D22426C7565222077696474683D223622206865696768743D2236222F
          3E0D0A3C6720636C6173733D22737431223E0D0A09093C7061746820636C6173
          733D2257686974652220643D224D362C3676326832563648367A222F3E0D0A09
          3C2F673E0D0A3C7265637420783D2232322220793D2232302220636C6173733D
          22426C7565222077696474683D223622206865696768743D2236222F3E0D0A3C
          6720636C6173733D22737431223E0D0A09093C7061746820636C6173733D2257
          686974652220643D224D32342C323276326832762D324832347A222F3E0D0A09
          3C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D225F
          7833335F5369676E732220786D6C6E733D22687474703A2F2F7777772E77332E
          6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
          2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
          20793D22307078222076696577426F783D223020302033322033322220737479
          6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203332
          2033323B2220786D6C3A73706163653D227072657365727665223E262331333B
          262331303B3C7374796C6520747970653D22746578742F6373732220786D6C3A
          73706163653D227072657365727665223E2E59656C6C6F777B66696C6C3A2346
          46423131353B7D262331333B262331303B2623393B2E5265647B66696C6C3A23
          4431314331433B7D262331333B262331303B2623393B2E477265656E7B66696C
          6C3A233033394332333B7D3C2F7374796C653E0D0A3C636972636C6520636C61
          73733D22477265656E222063783D2239222063793D22392220723D2237222F3E
          0D0A3C7061746820636C6173733D2259656C6C6F772220643D224D32342E352C
          322E3463302E332D302E362C302E382D302E362C312E312C306C362E332C3132
          2E3563302E332C302E362C302E312C312E312D302E352C312E314831382E3763
          2D302E362C302D302E382D302E352D302E352D312E314C32342E352C322E347A
          222F3E0D0A3C7061746820636C6173733D225265642220643D224D322E342C32
          352E39632D302E352D302E352D302E352D312E332C302D312E386C352E372D35
          2E3763302E352D302E352C312E332D302E352C312E382C306C352E372C352E37
          63302E352C302E352C302E352C312E332C302C312E386C2D352E372C352E3720
          202623393B632D302E352C302E352D312E332C302E352D312E382C304C322E34
          2C32352E397A222F3E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B3C7374796C6520747970653D22746578742F6373732220786D6C3A737061
          63653D227072657365727665223E2E426C75657B66696C6C3A23313137374437
          3B7D262331333B262331303B2623393B2E426C61636B7B66696C6C3A23373237
          3237323B7D3C2F7374796C653E0D0A3C706F6C79676F6E20636C6173733D2242
          6C61636B2220706F696E74733D2231372E322C31322031392E322C313020322C
          313020322C32342031302C32342031302C323220342C323220342C313220222F
          3E0D0A3C7061746820636C6173733D22426C75652220643D224D32372C31334C
          31372C32336C2D342D344C32332C394C32372C31337A204D32382C31326C312E
          372D312E3763302E342D302E342C302E342D312C302D312E334C32372C362E33
          632D302E342D302E342D312D302E342D312E332C304C32342C384C32382C3132
          7A20202623393B204D31322C3230763468344C31322C32307A222F3E0D0A3C2F
          7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D2253
          686164696E672220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B3C7374796C6520747970653D22746578742F6373732220786D6C3A737061
          63653D227072657365727665223E2E426C61636B7B66696C6C3A233732373237
          323B7D262331333B262331303B2623393B2E426C75657B66696C6C3A23313137
          3744373B7D3C2F7374796C653E0D0A3C7061746820636C6173733D22426C7565
          2220643D224D31382E342C3863322E362C302C342E382C342E332C342E382C39
          2E3663302C312E332C312E312C322E342C322E342C322E3463312E332C302C32
          2E342D312E312C322E342D322E344332382C31322E332C32332E372C382C3138
          2E342C387A222F3E0D0A3C7061746820636C6173733D22426C61636B2220643D
          224D31372C392E336C332C334C31332E332C3139632D312E342C312E342D332E
          372C312E342D352E312C306C2D352E312D352E31632D312E342D312E342D312E
          342D332E372C302D352E314C392E372C326C332C336C2D312E392C312E392020
          2623393B4331302E332C372E342C31302C382E322C31302C3963302C302E382C
          302E332C312E362C302E392C322E3163302E362C302E362C312E332C302E392C
          322E312C302E3973312E362D302E332C322E312D302E394C31372C392E337A20
          4D31332E372C392E376C352E392D352E3920202623393B63302E342D302E342C
          302E342D312C302D312E346C302C30632D302E342D302E342D312D302E342D31
          2E342C306C2D352E392C352E39632D302E342C302E342D302E342C312C302C31
          2E346C302C304331322E372C31302E312C31332E332C31302E312C31332E372C
          392E377A222F3E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D22D0
          A1D0BBD0BED0B95F312220786D6C6E733D22687474703A2F2F7777772E77332E
          6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
          2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
          20793D22307078222076696577426F783D223020302033322033322220737479
          6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203332
          2033323B2220786D6C3A73706163653D227072657365727665223E262331333B
          262331303B3C7374796C6520747970653D22746578742F6373732220786D6C3A
          73706163653D227072657365727665223E2E7374307B6F7061636974793A302E
          363B7D262331333B262331303B2623393B2E7374317B66696C6C3A2333373742
          42363B7D262331333B262331303B2623393B2E7374327B66696C6C3A23344441
          4538393B7D262331333B262331303B2623393B2E7374337B6F7061636974793A
          302E333B7D262331333B262331303B2623393B2E7374347B66696C6C3A234646
          464646463B7D3C2F7374796C653E0D0A3C6720636C6173733D22737430223E0D
          0A09093C673E0D0A0909093C706F6C79676F6E20706F696E74733D2231332C32
          382031302C32382031302C323020382C323020382C33302031332C3330203133
          2C33322031362C32392031332C3236202623393B2623393B222F3E0D0A090909
          3C706F6C79676F6E20706F696E74733D2231322C31312031302C31312031302C
          362031362C362031362C3420382C3420382C313120362C313120392C31342026
          23393B2623393B222F3E0D0A0909093C706F6C79676F6E20706F696E74733D22
          32322C32332032302C32332032302C382031382C382031382C32332031362C32
          332031392C3236202623393B2623393B222F3E0D0A0909093C706F6C79676F6E
          20706F696E74733D22342C3220322C3220322C323820302C323820332C333220
          362C323820342C3238202623393B2623393B222F3E0D0A0909093C706F6C7967
          6F6E20706F696E74733D2232382C32382032352C32382032352C32362032322C
          32392032352C33322032352C33302033302C33302033302C32302032382C3230
          202623393B2623393B222F3E0D0A0909093C706F6C79676F6E20706F696E7473
          3D2232382C31312032362C31312032392C31342033322C31312033302C313120
          33302C342032322C342032322C362032382C36202623393B2623393B222F3E0D
          0A0909093C7265637420783D2231382220793D2234222077696474683D223222
          206865696768743D2232222F3E0D0A0909093C7265637420783D22382220793D
          223136222077696474683D223222206865696768743D2232222F3E0D0A090909
          3C7265637420783D2231382220793D223238222077696474683D223222206865
          696768743D2232222F3E0D0A0909093C7265637420783D2232382220793D2231
          36222077696474683D223222206865696768743D2232222F3E0D0A09093C2F67
          3E0D0A093C2F673E0D0A3C7061746820636C6173733D227374312220643D224D
          31362C323676366836762D364831367A222F3E0D0A3C7061746820636C617373
          3D227374322220643D224D31362C327636683656324831367A222F3E0D0A3C67
          20636C6173733D22737433223E0D0A09093C673E0D0A0909093C726563742078
          3D2232362220793D223134222077696474683D223622206865696768743D2236
          222F3E0D0A0909093C7265637420783D22362220793D22313422207769647468
          3D223622206865696768743D2236222F3E0D0A09093C2F673E0D0A093C2F673E
          0D0A3C7061746820636C6173733D227374342220643D224D32382C3138762D32
          683276324832387A204D31382C323868327632682D325632387A204D31382C34
          68327632682D3256347A204D382C31366832763248385631367A222F3E0D0A3C
          2F7376673E0D0A}
      end>
  end
  object ilSmallIcons: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    Left = 400
    Top = 256
    Bitmap = {
      494C010131002C01580110001000FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      000000000000360000002800000040000000D0000000010020000000000000D0
      0000000000000000000000000000000000000000000600000067000000070000
      000000000000000000000000003A00000014B67A36FFB67A36FFB67A36FF0000
      00140000003A0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000006000000099000000610000
      000000000099000000990000009900000073B67A36FFFFFFFFFFB67A36FF0000
      0073000000990000009900000099000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000099000000000000
      000000000099000000000000003900000013B67A36FFB67A36FFB67A36FF0000
      0013000000390000000000000099000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000099000000000000
      0000000000990000000000000000000000000000001400000074000000140000
      0000000000000000000000000099000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000099000000000000
      0000000000990000000000000000000000000000003A000000990000003A0000
      0000000000000000000000000099000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000099000000000000
      0000000000990000000000000000000000000000000000000099000000000000
      0000000000000000000000000099000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000099000000000000
      004C0000004C0000004C00000000000000000000000000000099000000000000
      0000000000000000004C0000004C0000004C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000099000000000000
      004CFFFFFFFF0000004C00000000000000000000000000000099000000000000
      0000000000000000004CFFFFFFFF0000004C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000099000000000000
      004C0000004C0000004C00000000000000000000000000000099000000000000
      0000000000000000004C0000004C0000004C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000099000000000000
      0014000000740000001400000000000000000000000000000099000000000000
      0000000000000000001400000074000000140000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000099000000000000
      003A000000990000003A00000000000000000000000000000099000000000000
      0000000000000000003A000000990000003A0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000099000000000000
      0000000000990000000000000000000000000000000000000099000000000000
      0000000000000000000000000099000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000099000000000000
      00000000009900000000000000000000000089AE4CFF89AE4CFF89AE4CFF0000
      0000000000000000000000000099000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000099000000000000
      00000000009900000099000000990000009989AE4CFFFFFFFFFF89AE4CFF0000
      0099000000990000009900000099000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000099000000000000
      00000000000000000000000000000000000089AE4CFF89AE4CFF89AE4CFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000202
      11491717B1EB0202124C00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000020211491A1A
      C5F81B1BD1FF1A1AC6F80202124C000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000020211491A1AC5F81B1B
      D1FF1B1BD1FF1B1BD1FF1A1AC6F80202124C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B67A
      36FFB67A36FFB67A36FF0000000000000000000000001616AAE61B1BD1FF1B1B
      D1FF1B1BD1FF1B1BD1FF1B1BD1FF1717B0EA0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B67A
      36FFE9D7C3FFB67A36FF00000000000000000000000001010E431919C1F51B1B
      D1FF1B1BD1FF1B1BD1FF1A1AC3F702020F460000000000000000000000000000
      00000000000000000000000000000000000000000000717171FF717171FF7171
      71FF717171FF00000000D77610FF341D037E0E07014200000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000200000058B67A
      36FFB67A36FFB67A36FF0000000000000000000000000000000001010E431919
      C1F51B1BD1FF1A1AC3F702020F46000000000000000000000000000000000000
      00000000000000000000000000000000000000000000717171FF000000000000
      00000000000000000000341D037E371E0482D77610FF371E0482000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000070000006C000000970000
      0041000000000000000000000000000000000000000000000000000000000101
      0E431616AAE602020F4600000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000717171FF000000000000
      000000000000000000000D070140D77610FFD77610FFD77610FF371E04820000
      0000000000000000000000000000000000000000000000000000000000000000
      0014424242C36F6F6FFD2929299B000000030000000000000000000000000503
      0029B6640EEB6A3A08B300000000000000000000000000000000000000000000
      0000000000000000000000000000000000110000007C000000920000002D0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000717171FF000000000000
      0000000000000000000000000000341D037ED77610FFD77610FFD77610FF371E
      0482000000000000000000000000000000000000000000000000000000144B4B
      4BD1717171FF717171FF717171FF2F2F2FA60000000300000000000000002212
      0266D77610FFD07210FB00000000000000000000000000000000000000000000
      000000000000000000000000001D00000089000000880000001D000000000000
      00000000000000000000000000000000000000000000000000000413005A1970
      02D8229A02FD197102D90414005D00000000000000000F81BADA14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF1194D4E900000000717171FF000000000000
      000000000000000000000000000000000000341D037ED77610FFD77610FFD776
      10FF371E048200000000000000000000000000000000000000144B4B4BD17171
      71FF717171FF717171FF717171FF717171FF2F2F2FA60000000300000000331C
      037DD77610FF8F4E0BD000000000000000000000000000000000000000000000
      0000000000000000002E000000920000007C0000001000000000000000000000
      0000000000000000000000000000000000000000000004120059229C02FF229C
      02FF229C02FF229C02FF229C02FF0414005D00000000031D2A6814B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF04283A7A00000000717171FF000000000000
      00000000000000000000000000000000000000000000341D037ED77610FFD776
      10FFD77610FF0D0701400000000000000000000000003D3D3DBC717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF2E2E2EA3000000006839
      08B2D77610FF2313026700000000000000000000000000000000000000000000
      000000000042000000970000006B000000070000000000000000000000000000
      00000000000000000000000000000000000000000000186C02D5229C02FF229C
      02FF229C02FF229C02FF229C02FF197102D900000000000000060F89C6E114B1
      FFFF14B1FFFF14B1FFFF1196D8EB0000000D00000000717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF616161EC04040431341D037ED776
      10FF341D037E371E0482391F04840000000000000000626262EE717171FF7171
      71FF717171FF1818187600000006131313696C6C6CF90E0E0E5C0201001ACB70
      10F85B3207A70000000200000000000000000000000000000000B67A36FFB67A
      36FFB67A36FF0000005700000002000000000000000000000000000000000000
      00000000000000000000000000000000000000000000219502F9229C02FF229C
      02FF229C02FF229C02FF229C02FF229A02FD0000000000000000031C286614B1
      FFFF14B1FFFF14B1FFFF04263878000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000C07
      003E371E0482D77610FF84490AC800000000000000001B1B1B7D717171FF7171
      71FF717171FF00000009505050D6070707400101011F0000000E3E22048A2112
      0265000000020000000000000000000000000000000000000000B67A36FFE9D7
      C3FFB67A36FF0000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000186C02D4229C02FF229C
      02FF229C02FF229C02FF229C02FF197002D80000000000000000000000060F87
      C3DF14B1FFFF1194D6EA0000000C000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000311B037A81470AC6000000100000000000000000000000002323238F7171
      71FF717171FF131313690505053B5C5C5CE70707074000000000000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000004110056229B02FE229C
      02FF229C02FF229C02FF229C02FF0413005A000000000000000000000000031A
      276414B1FFFF0425367600000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000012727
      2796717171FF6D6D6DFA010101210505053B5C5C5CE707070740000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000004110056186C
      01D4219502F9186C02D504120059000000000000000000000000000000000000
      00050C6A98C50000000B00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00012929299A0E0E0E5C00000000000000000505053B434343C4000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000010000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B67A36FFB67A36FFB67A36FF0000
      0000B67A36FFB67A36FFB67A36FF00000000000000000000000000000000B67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B67A36FFE9D7C3FFB67A36FF0000
      0000B67A36FFE9D7C3FFB67A36FF00000000000000000000000000000000B67A
      36FFE9D7C3FFB67A36FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B67A36FFB67A36FFB67A36FF0000
      0000B67A36FFB67A36FFB67A36FF00000000000000000000000000000000B67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B67A
      36FFB67A36FFB67A36FF00000000000000000000000000000099000000000000
      0000000000000000009900000000000000000000000000000000000000000000
      0000000000990000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B67A
      36FFE9D7C3FFB67A36FF00000000000000000000000000000000000000000000
      009900000099000000990000009900000099000000990000009900000099B67A
      36FFE9D7C3FFB67A36FF00000000000000000000000000000000000000000000
      005500000097000000990000009900000099000000990000009900000099B67A
      36FFE9D7C3FFB67A36FF00000000000000000000000000000099000000000000
      0000000000000000009900000000000000550000009600000057000000000000
      0000000000990000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      009900000000000000000000000000000000000000000000000000000000B67A
      36FFB67A36FFB67A36FF00000000000000000000000000000000000000000000
      009600000009000000000000000000000000000000000000000000000000B67A
      36FFB67A36FFB67A36FF00000000000000000000000000000099000000000000
      0000000000000000009900000000000000960000001200000098000000000000
      0000000000990000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000000000000000000000000000000000000000000000000000
      0099000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0099000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000099000000000000
      0000000000000000009900000000000000990000000000000099000000000000
      0000000000990000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000000000000000000000000000000000000000000000000000
      0099000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00960000000B0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000099000000000000
      0000000000000000009900000000000000990000000000000099000000000000
      0000000000990000009900000099000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000000000000000000000000000000000000000000000000000
      0099000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0053000000960000009900000099000000990000009900000099000000990000
      0098000000570000000000000000000000000000000000000099000000000000
      0000000000000000009600000014000000970000000000000099000000000000
      0000000000000000000000000099000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000000000000000000000000000000000000000000000000000
      0099000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0008000000980000000000000000000000000000000000000099000000000000
      0000000000000000005300000094000000550000000000000099000000000000
      0000000000000000000000000099000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000000000000000000000000000000000000000000000000000
      0099000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000990000000000000000000000000000000000000099000000000000
      0000000000000000000000000000000000000000000000000099000000000000
      0000000000000000000000000099000000000000000000000000B67A36FFB67A
      36FFB67A36FF0000000000000000000000000000000000000000000000000000
      0000000000990000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FF0000000000000000000000000000000000000000000000000000
      000900000097000000000000000000000000B67A36FFB67A36FFB67A36FF0000
      000000000000000000000000000000000000B67A36FFB67A36FFB67A36FF0000
      000000000000B67A36FFB67A36FFB67A36FF0000000000000000B67A36FFE9D7
      C3FFB67A36FF0000009900000099000000990000009900000099000000990000
      0099000000990000000000000000000000000000000000000000B67A36FFE9D7
      C3FFB67A36FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFE9D7
      C3FFB67A36FF0000009900000099000000990000009900000099000000990000
      009600000055000000000000000000000000B67A36FFE9D7C3FFB67A36FF0000
      000000000000000000000000000000000000B67A36FFE9D7C3FFB67A36FF0000
      000000000000B67A36FFE9D7C3FFB67A36FF0000000000000000B67A36FFB67A
      36FFB67A36FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B67A36FFB67A
      36FFB67A36FF0000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B67A36FFB67A36FFB67A36FF0000
      000000000000000000000000000000000000B67A36FFB67A36FFB67A36FF0000
      000000000000B67A36FFB67A36FFB67A36FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000059000000420000
      000900000000000000000000000000000000030200236D4920C5B27835FC6F4A
      21C7030201250000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000205E5E5EBF888888D31313
      138B000000200000000000000000000000006A471FC3C99D6AFFEBDAC7FFCA9F
      6CFF6F4A21C70000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000420000000000000000000000000808086BEBEBEBF8FAFAFAFD1616
      168800000005000000000000000000000000AC7433F8EAD8C4FFFFFFFFFFEBDA
      C7FFB27835FC0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000000000000000000000000000000000000000000000000000
      0000080808890000004200000000000000206C6C6CC6FFFFFFFF7F7F7FCF0000
      00300000000000000000000000000000000068461FC1C89C68FFEAD8C4FFC99D
      6AFF6D4921C50000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000100001200000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0099FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000990000000000000000000000000000000000000000000000000000
      0000282828A1828282D0000000430B0B0B6EF1F1F1FAF2F2F2FB0D0D0D750000
      0001000000000000000000000000000000000202002168461FC1AF7635FC6A47
      1FC3030200230000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002715026D00000000000000000000
      000000000000000000000000000000000000683908B20301001F000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0099FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000990000000000000000000000000000000000000000000000000000
      0000282828A1FFFFFFFF878787D27B7B7BCDFFFFFFFF6A6A6AC50000001F0000
      0000000000000000000000000000000000000000000000000000000000990000
      00000000000000000000271A0C779F6B30EFA16C30F0291C0C7A000000000000
      000000000000000000000000000000000000462605924D2A05990000000F0000
      000000000000000000000000000000000000D77610FFC36C0FF32B1703730000
      0004000000000000000000000000000000000000000000000000000000000000
      0099FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000990000000000000000000000000000000000000000000000000000
      0000282828A1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF727272C9606060C05555
      55BB030303840000003A00000000000000000000000000000000000000990000
      0000000000000C080344B77C3AFFE2CAAFFFE2CBB1FFB87D3BFF0E0904480000
      00000000000000000000000000000000000009050036D77610FFBD680EEF4D2A
      0599180D02560503002A0000001000000005D77610FFD77610FFD77610FF8C4D
      0BCE090500350000000000000000000000000000000000000000000000000000
      0099FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000990000000000000000000000000000000000000000000000000000
      0000282828A1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE1E1E1F52020
      20990000002E0000000000000000000000000000000000000000000000990000
      009900000099432E14C9C5965FFFFFFFFFFFFFFFFFFFC69862FF463015CB0000
      009900000099000000990000000000000000000000006B3B08B4D77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD27410FC462605920000000E000000000000000000000000000000000000
      0099FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000990000000000000000000000000000000000000000000000000000
      0000282828A1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD1D1D1EF1515158E0000
      0023000000000000000000000000000000000000000000000000000000000000
      0000000000000C080342B77C39FFE1C8ACFFE1C9AEFFB77D3AFF0D0904460000
      000000000000000000990000000000000000000000000201001AAA5D0DE3D776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFAB5E0DE4160C01530000000000000000000000000000
      0099FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000990000000000000000000000000000000000000000000000000000
      0000282828A1FFFFFFFFFFFFFFFFFFFFFFFFBDBDBDE80D0D0D820000001A0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000024180B739B672EEB9B692FEC261A0B76000000000000
      000000000000000000990000000000000000000000000000000003010021884B
      0BCBD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFA85C0DE2140B014F0000000000000000000000000000
      0099FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000990000000000000000000000000000000000000000000000000000
      0000282828A1FFFFFFFFFFFFFFFFA6A6A6DF0606067600000012000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000302
      00236D4920C5B37935FE6F4A21C7030201250000000000000000000000000000
      0003170D0155633608ADA75C0DE1CD7010F9D77610FFD77610FFD77610FFD776
      10FFD17310FC4224058E0000000C000000000000000000000000000000000000
      0099FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF000000990000000000000000000000000000000000000000000000000000
      0000282828A1FFFFFFFF8E8E8ED50202026B0000000B00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006A47
      1FC3C99D6AFFEBDAC7FFCA9F6CFF6F4A21C70000000000000000000000000000
      000000000000000000000000000000000000D77610FFD77610FFD77610FF884B
      0BCB080400320000000000000000000000000000000000000000000000000000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000000000000000000000000000000000000000000000000000
      0000282828A1737373C90000005F000000060000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000AC74
      33F8EAD8C4FFFFFFFFFFEBDAC7FFB27835FC0000000000000000000000000000
      000000000000000000000000000000000000D77610FFC16A0FF22816036F0000
      0003000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000040404850000005300000003000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006846
      1FC1C89C68FFEAD8C4FFC99D6AFF6D4921C50000000000000000000000000000
      000000000000000000000000000000000000643708AE0201001C000000000000
      000000000000000000000000000000000000000000000000000000000000B67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FF0000000000000000000000000000000000000000000000000000
      0000000000470000000100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000202
      002168461FC1AC7434F86A471FC3030200230000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B67A
      36FFFFFFFFFFFFFFFFFFB67A36FFFFFFFFFFFFFFFFFFB67A36FFFFFFFFFFFFFF
      FFFFB67A36FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000B67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000000000000000000000000000000000000000000000000000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000000000000000000000000000000000000000000000000000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000000000000000000000000000000000000000000000000000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000009900000099000000990000009900000099000000990000
      0000000000000000000000000000000000000000000000000000000000000000
      0099000000990000009900000099000000990000009900000099000000990000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000009900000099000000990000009900000099000000990000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000070000009900000099000000990000009900000099000000990000
      0099000000990000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000000000000000000000000000000000000000000000000000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000000000000000000000000000000000000000000000000000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000000000000000000000000000000000000000000000000000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0099000000990000009900000099000000990000009900000099000000990000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000009900000099000000990000009900000099000000990000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000070000009900000099000000990000009900000099000000990000
      0099000000990000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000000000000000000000000000000000000000000000000000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000000000000000000000000000000000000000000000000000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000405022E2F3C1A97658239DD000000000000000067833ADE33401C9C0607
      0336000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000033000000330000
      0033000000330000000000000066000000660000006600000066000000000000
      009900000099000000990000009900000000000000000000000000000000232C
      138287AB4BFD89AE4CFF89AE4CFF000000000000000089AE4CFF89AE4CFF89AE
      4CFF2934168C0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000033000000330000
      0033000000330000000000000066000000660000006600000066000000000000
      0099000000990000009900000099000000000000000000000000000000000405
      022C71913FE989AE4CFF89AE4CFF000000000000000089AE4CFF89AE4CFF7190
      3FE8030401290000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000033000000330000
      0033000000330000000000000066000000660000006600000066000000000000
      009900000099000000990000009900000000000000000C10074E0102011D0000
      00000405022C384720A4070A043E00000000000000000709043D38471FA40405
      022C000000000102001C0D100750000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000033000000330000
      0033000000330000000000000066000000660000006600000066000000000000
      0099000000990000009900000099000000000101001A303D1B982A35178E0102
      011D000000000000000000000000000000000000000000000000000000000000
      00000102001C2934178D303D1B980102011D0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000010140959313E1B99313E1B99151A
      0B65000000000000000000000000000000000000000000000000000000000000
      000014190B62313E1B99313E1B991116095B0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000212B127F212B127F212B
      127F212B127F000000004C612BBF4C612BBF4C612BBF4C612BBF0000000089AE
      4CFF89AE4CFF89AE4CFF89AE4CFF00000000232D1382313E1B99313E1B990303
      0127000000000000000000000000000000000000000000000000000000000000
      000002030124313E1B99313E1B99242E14850000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000212B127F212B127F212B
      127F212B127F000000004C612BBF4C612BBF4C612BBF4C612BBF0000000089AE
      4CFF89AE4CFF89AE4CFF89AE4CFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0099000000990000009900000099000000990000009900000099000000990000
      00990000009900000000000000000000000000000000212B127F212B127F212B
      127F212B127F000000004C612BBF4C612BBF4C612BBF4C612BBF0000000089AE
      4CFF89AE4CFF89AE4CFF89AE4CFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000212B127F212B127F212B
      127F212B127F000000004C612BBF4C612BBF4C612BBF4C612BBF0000000089AE
      4CFF89AE4CFF89AE4CFF89AE4CFF000000002E1F0E82412C1399412C13990402
      0128000000000000000000000000000000000000000000000000000000000000
      000003020125412C1399412C139930210E840000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000009900000099000000990000009900000099000000990000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000150E0658412C1399412C13991C13
      0865000000000000000000000000000000000000000000000000000000000000
      00001B120863412C1399412C1399160F075A0000000000000000000000000000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000002D1E0D7F2D1E0D7F2D1E
      0D7F2D1E0D7F0000000066441EBF66441EBF66441EBF66441EBF00000000B67A
      36FFB67A36FFB67A36FFB67A36FF00000000010100193F2B13973725108D0201
      001C000000000000000000000000000000000000000000000000000000000000
      00000101001A3625108C402B13980201001B0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0099000000990000009900000099000000990000009900000099000000990000
      009900000099000000000000000000000000000000002D1E0D7F2D1E0D7F2D1E
      0D7F2D1E0D7F0000000066441EBF66441EBF66441EBF66441EBF00000000B67A
      36FFB67A36FFB67A36FFB67A36FF0000000000000000100A044C0201001C0000
      00000503012C4E3517A80C08034200000000000000000B0703414E3517A80604
      0131000000000101001A100B054E000000000000000000000000000000000000
      0000000000000000009900000099000000990000009900000099000000990000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000002D1E0D7F2D1E0D7F2D1E
      0D7F2D1E0D7F0000000066441EBF66441EBF66441EBF66441EBF00000000B67A
      36FFB67A36FFB67A36FFB67A36FF000000000000000000000000000000000403
      012997652DE8B67A36FFB67A36FF0000000000000000B67A36FFB67A36FF9C69
      2FEC060401310000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000002D1E0D7F2D1E0D7F2D1E
      0D7F2D1E0D7F0000000066441EBF66441EBF66441EBF66441EBF00000000B67A
      36FFB67A36FFB67A36FFB67A36FF000000000000000000000000000000003222
      0F86B47A36FEB67A36FFB67A36FF0000000000000000B67A36FFB67A36FFB278
      36FD2F200E830000000000000000000000000000000000000000000000000000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000060402313E2A1296845927D90000000000000000845927D93D2912940503
      012C000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000D0000004D0000000D0000000000000000000000000000
      0000000000000E17408E2C49C8FA0F1943920000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000D0000005A00000066000000560000000A00000000000000000000
      0000000000002B48C2F72E4CD0FF2C49C8FA0000000000000000000000000000
      000004040430717171FF717171FF717171FF535353DB00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000242424904B4B4BD0636363EF626262ED474747CA16161671000000030000
      0000000000000000000000000000000000000000000000000000000000000000
      000D0000005A00000066000000630000004F000000430000000A000000000000
      0000000000001B2C79C22E4CD0FF1C2E7EC60000000000000000000000000000
      00000000000000000000595959E25E5E5EE90000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000F0F0F5F4D4D4DD36D6D6DFB6B6B6BF9474747CA0B0B0B520000
      0000000000000000000000000000000000000000000000000000000000000000
      00006D6D6DFB393939B51E1E1E83202020884B4B4BD1717171FF252525920000
      00000000000000000000000000000000000000000000000000000000000D0000
      005A00000066000000630000004F0000004C0000004C000000430000000A0000
      0000000000000B11307B2E4CD0FF0B12337F0000000000000000000000000000
      00000000000000000000333333AC717171FF0101012000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000D0D0D59717171FF606060EB2323238F2121218A5A5A5AE3717171FE0A0A
      0A4E000000000000000000000000000000000000000000000000000000000000
      0000010101220000000000000000000000000000000B676767F3676767F30000
      000000000000000000000000000000000000000000000000000D0000005A0000
      0066000000630000004F0000004C0000004C0000004C0000004C000000430000
      000A00000000010208332E4CD0FF020309370000000000000000000000000000
      0000000000000000000018181876717171FF0D0D0D5700000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000464646C86F6F6FFC0303032C0000000000000000010101206B6B6BF84343
      43C3000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000101011D6B6B6BF85F5F5FEA0000
      0000000000000000000000000000000000000000000D0000005A000000660000
      00630000004F0000004C0000004C0000004C0000004C0000004C0000004C0000
      00430000000A00000001203590D4000000020000000000000000000000000000
      0000000000000000000007070740717171FF2222228C00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000686868F5484848CC00000000000000000000000000000000424242C36969
      69F6000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000007121212685C5C5CE7717171FF1414146B0000
      0000000000000000000000000000000000000000004D00000066000000630000
      004F0000004C0000004C0000004C0000004C0000004C0000004C0000004C0000
      004C000000430000000A03051047000000000000000000000000000000000000
      000000000000000000000000000D6F6F6FFC414141C200000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FE393939B500000000000000000000000000000000353535B07171
      71FF000000000000000000000000000000000000000000000000000000006767
      67F3676767F3676767F36B6B6BF8717171FF717171FF717171FF717171FF7171
      71FF000000000000000000000000000000000000000C000000560000004F0000
      004C0000004C0000004C0000004C626262B0C2C2C2FB666666B20000004C0000
      004C0000004C0000003900000000000000000000000000000000000000000000
      00000000000000000000000000004E4E4ED3676767F400000004000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF373737B300000000000000000000000000000000353535AF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      000C1A1A1A7A717171FF666666F21D1D1D810000001400000000000000000000
      0000000000000000000000000000000000000000000000000009000000420000
      004C0000004C0000004C0000004C8C8C8CF9323232FFC4C4C4FB0000004C0000
      004C000000420000000900000000000000000000000000000000000000000000
      00000000000000000000000000002A2A2A9D717171FF0303032F000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF373737B300000000000000000000000000000000353535AF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      0000606060EB6D6D6DFB02020227000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000090000
      00420000004C0000004C000000951A1A1AEF8E8E8EF9616161AF0000004C0000
      0042000000090000000000000000000000000000000000000000000000000000
      000000000000000000000000000012121268717171FF11111165000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF373737B300000000000000000000000000000000353535AF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      0000656565F1676767F40000000D000000000000000000000000000000020000
      0000000000000000000000000000000000000000000000000000000000000000
      00090000004200000095000000DB000000920000004C0000004C000000420000
      0009000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000004040432717171FF2929299A000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF373737B300000000000000000000000000000000353535AF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      000020202088717171FF4E4E4ED41C1C1C7F1919197A2F2F2FA5181818780000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000006D000000D9000000920000004C0000004C00000042000000090000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000017717171FF717171FF717171FF5E5E5EE80000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF373737B300000000000000000000000000000000353535AF7171
      71FF000000000000000000000000000000000000000000000000000000000000
      000000000002161616724E4E4ED3696969F66B6B6BF9575757E00D0D0D590000
      0000000000000000000000000000000000000000000000000000000000000000
      0068000000CC0000006A000000420000004C0000004200000009000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000330000
      00CC000000640000000000000009000000380000000900000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0031000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000089AE4CFF89AE4CFF89AE
      4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0099000000990000009900000099000000990000009989AE4CFF89AE4CFF98B8
      63FF98B863FF89AE4CFF89AE4CFF89AE4CFF0000000000000066000000800000
      00100000000000000000000000000000000000000000000000060000007A0000
      006A000000000000000000000000000000000000000000000066000000800000
      00100000000000000000000000000000000000000000000000060000007A0000
      006A000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0000000000000000000000000000000000000000000089AE4CFF98B863FFF1F5
      E9FFF1F5E9FF98B863FF89AE4CFF89AE4CFF0000000000000034000000810000
      0040000000000000000000000000000000000000000000000034000000810000
      0038000000000000000000000000000000000000000000000034000000810000
      0040000000000000000000000000000000000000000000000034000000810000
      0038000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000990000
      0000000000000000000000000000000000000000000089AE4CFFE2EBD3FFF0F5
      E8FFF0F5E8FFF1F5E9FF98B863FF89AE4CFF00000000000000080000007C0000
      007100000001000000000000000000000000000000000000006A0000007E0000
      000A0000000000000000000000000000000000000000000000080000007C0000
      007100000001000000000000000000000000000000000000006A0000007E0000
      000A000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF717171FF717171FF707070FE585858E12323238F0000
      0010000000000000000000000000000000000000000000000000000000990000
      00000000004C000000000000004C0000004C0000000089AE4CFF97B862FF97B8
      62FF97B862FFF0F5E8FFE2EBD3FF89AE4CFF0000000000000000000000520000
      0081000000580000005100000051000000510000005600000081000000560000
      0000000000000000000000000000000000000000000000000000000000520000
      0081000000580000005100000051000000510000005600000081000000560000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF5B5B5BE5484848CB595959E2717171FF717171FF4545
      45C8000000020000000000000000000000000000000000000000000000990000
      0000000000000000000000000000000000000000000089AE4CFF89AE4CFF89AE
      4CFF89AE4CFF97B862FF97B862FF89AE4CFF0000000000000000000000200000
      0081000000760000004D0000004D0000004D0000007400000081000000250000
      0000000000000000000000000000000000000000000000000000000000200000
      0081000000760000004D0000004D0000004D0000007400000081000000250000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF1C1C1C80000000000000000125252593717171FF7171
      71FF050505390000000000000000000000000000000000000000000000990000
      00000000004C000000000000004C0000004C0000000089AE4CFF89AE4CFF89AE
      4CFF89AE4CFF89AE4CFF89AE4CFF89AE4CFF0000000000000000000000010000
      00700000007D0000000600000000000000040000007A00000073000000010000
      0000000000000000000000000000000000000000000000000000000000010000
      00700000007D0000000600000000000000040000007A00000073000000010000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF1C1C1C8000000000000000000C0C0C56717171FF7171
      71FF090909480000000000000000000000000000000000000000000000990000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000009900000000000000000000000000000000000000000000
      003F000000810000002B00000000000000270000008100000043000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      003F000000810000002B00000000000000270000008100000043000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF2C2C2CA0070707400D0D0D59525252D9717171FF5B5B
      5BE50000000C0000000000000000000000000000000000000000000000990000
      00000000004C000000000000004C0000004C0000004C0000004C0000004C0000
      004C000000000000009900000000000000000000000000000000000000000000
      000E000000800000005500000000000000510000008100000012000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000E000000800000005500000000000000510000008100000012000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF717171FF717171FF717171FF696969F62E2E2EA30202
      0223000000000000000000000000000000000000000000000000000000990000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000009900000000000000000000000000000000000000000000
      00000000005D0000007A00000006000000780000006100000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000005D0000007A00000006000000780000006100000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF434343C52222228C3E3E3EBD717171FF404040C00000
      0016000000000000000000000000000000000000000000000000000000990000
      00000000004C000000000000004C0000004C0000004C0000004C0000004C0000
      004C000000000000009900000000000000000000000000000000000000000000
      00000000002B0000008100000048000000810000002F00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000002B0000008100000048000000810000002F00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF1C1C1C8000000000000000014E4E4ED4717171FF3030
      30A7000000000000000000000000000000000000000000000000000000990000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000009900000000000000000000000000000000000000000000
      00000000000300000078000000800000007A0000000500000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000300000078000000800000007A0000000500000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF1C1C1C8000000000010101195A5A5AE4717171FF5050
      50D6000000000000000000000000000000000000000000000000000000990000
      00000000004C000000000000004C0000004C0000004C0000004C0000004C0000
      004C000000000000009900000000000000000000000000000000000000000000
      00000000000000000049000000810000004E0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000049000000810000004E0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF717171FF676767F35E5E5EE96F6F6FFC717171FF717171FF2323
      238E000000000000000000000000000000000000000000000000000000990000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000009900000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000002A1D0D7CB67A
      36FFB67A36FFB67A36FFB67A36FF2A1D0D7C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000030200E8430200E8400000000000000000000000000000000000000000000
      0000717171FF717171FF717171FF717171FF6B6B6BF84F4F4FD51A1A1A7A0000
      0005000000000000000000000000000000000000000000000000000000990000
      0099000000990000009900000099000000990000009900000099000000990000
      0099000000990000009900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000002A1D
      0D7CB67A36FFB67A36FF2A1D0D7C000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003020
      0E84B67A36FFB67A36FF30200E84000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00002A1D0D7C2A1D0D7C00000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000030200E84B67A
      36FFB67A36FFB67A36FFB67A36FF30200E840000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000323232FF323232FF3232
      32FF323232FF323232FF323232FF323232FF323232FF323232FF323232FF3232
      32FF323232FF323232FF323232FF323232FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FF0000000000000000323232FF323232FF3232
      32FF323232FF323232FF323232FF323232FF323232FF323232FF323232FF3232
      32FF323232FF323232FF323232FF323232FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF000000000000000000000000666666F27171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF676767F400000000000000000000000000000000B67A36FFB67A36FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B67A36FFB67A36FF0000000000000000323232FF323232FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000323232FF323232FF717171FFFFFFFFFF717171FFFFFF
      FFFF717171FFFFFFFFFF717171FFFFFFFFFF717171FFFFFFFFFF717171FFFFFF
      FFFF717171FFFFFFFFFF717171FF000000000000000000000000717171FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF00000000000000000000000000000000B67A36FFB67A36FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B67A36FFB67A36FF0000000000000000323232FF323232FF0000
      000000000000000000000000000000000000323232FF00000000000000000000
      00000000000000000000323232FF323232FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF000000000000000000000000717171FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF00000000000000000000000000000000B67A36FFB67A36FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B67A36FFB67A36FF0000000000000000323232FF323232FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000323232FF323232FF717171FFFFFFFFFF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FFB7B7
      B7FFFFFFFFFFFFFFFFFF717171FF000000000000000000000000717171FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF00000000000000000000000000000000B67A36FFB67A36FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B67A36FFB67A36FF0000000000000000323232FF323232FF0000
      0000323232FF00000000323232FF00000000323232FF00000000323232FF0000
      0000323232FF00000000323232FF323232FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FFBABA
      BAFFFFFFFFFFFFFFFFFF717171FF000000000000000000000000717171FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF00000000000000000000000000000000B67A36FFB67A36FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B67A36FFB67A36FF0000000000000000323232FF323232FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000323232FF323232FF717171FFFFFFFFFF717171FF7171
      71FF717171FF717171FFBABABAFFBABABAFF717171FF717171FFBABABAFFFFFF
      FFFFB7B7B7FFB7B7B7FF717171FF000000000000000000000000717171FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF00000000000000000000000000000000B67A36FFB67A36FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B67A36FFB67A36FF0000000000000000323232FF323232FF0000
      000000000000000000000000000000000000323232FF00000000000000000000
      00000000000000000000323232FF323232FF717171FF717171FF717171FF7171
      71FF717171FFBABABAFFFFFFFFFFFFFFFFFFBABABAFFBABABAFFFFFFFFFFB7B7
      B7FF717171FF717171FF717171FF000000000000000000000000717171FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF00000000000000000000000000000000B67A36FFB67A36FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B67A36FFB67A36FF0000000000000000323232FF323232FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000323232FF323232FF717171FFFFFFFFFF717171FF7171
      71FFBABABAFFFFFFFFFFB7B7B7FFB7B7B7FFFFFFFFFFFFFFFFFFB7B7B7FF7171
      71FF717171FF717171FF717171FF000000000000000000000000717171FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF00000000000000000000000000000000B67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFB67A36FFB67A36FF0000000000000000323232FF323232FF0000
      0000323232FF00000000323232FF00000000323232FF00000000323232FF0000
      0000323232FF00000000323232FF323232FF717171FF717171FF717171FFBABA
      BAFFFFFFFFFFB7B7B7FF717171FF717171FFB7B7B7FFB7B7B7FF717171FF7171
      71FF717171FF717171FF717171FF000000000000000000000000717171FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000717171FF00000000000000000000000000000000B67A36FFB67A36FF7C7C
      7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C
      7CB2B67A36FFB67A36FFB67A36FF0000000000000000323232FF323232FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000323232FF323232FF717171FFFFFFFFFF717171FFB7B7
      B7FFB7B7B7FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF000000000000000000000000717171FF0000
      00000000000000000000000000000000000000000000666666F2717171FF7171
      71FF555555DE00000000000000000000000000000000B67A36FFB67A36FF7C7C
      7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB2B67A36FF7C7C
      7CB2B67A36FFB67A36FFB67A36FF0000000000000000323232FF323232FF0000
      000000000000000000000000000000000000323232FF00000000000000000000
      00000000000000000000323232FF323232FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF717171FF717171FF000000000000000000000000717171FF0000
      00000000000000000000000000000000000000000000717171FF717171FF5555
      55DE0101011F00000000000000000000000000000000B67A36FFB67A36FF7C7C
      7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB2B67A36FF7C7C
      7CB2B67A36FFB67A36FFB67A36FF0000000000000000323232FF323232FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000323232FF323232FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000717171FF0000
      00000000000000000000000000000000000000000000717171FF555555DE0101
      011F0000000000000000000000000000000000000000B67A36FFB67A36FF7C7C
      7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB2B67A36FF7C7C
      7CB2B67A36FFB67A36FF2C1E0D7E0000000000000000323232FF323232FF3232
      32FF323232FF323232FF323232FF323232FF323232FF323232FF323232FF3232
      32FF323232FF323232FF323232FF323232FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000646464F07171
      71FF717171FF717171FF717171FF717171FF717171FF555555DE0101011F0000
      00000000000000000000000000000000000000000000B67A36FFB67A36FF7C7C
      7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB27C7C7CB2B67A36FF7C7C
      7CB2B67A36FF2C1E0D7E000000000000000000000000323232FF323232FF3232
      32FF323232FF323232FF323232FF323232FF323232FF323232FF323232FF3232
      32FF323232FF323232FF323232FF323232FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000D77610FF000000004224048E95520BD5452605910000
      00004224048E95520BD545260591000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000001900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000001900000000000000000000000000000081000000810000
      0081000000810000008100000081000000810000008100000081000000810000
      0081000000810000008100000081000000001D1D1D821D1D1D82000000000000
      00000000000000000000D77610FF00000000C36B0FF300000003C96E0FF70000
      0000C36B0FF300000003C96E0FF7000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000290000008F00000019000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000290000008F00000019000000000000000000000081FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFE9D8C3FFE8D6C1FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000081000000001B1B1B7E717171FF1D1D1D820000
      000000000000573006A2D77610FF00000000C16A0FF200000004C86E0FF60000
      0000C16A0FF200000004C86E0FF6000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00280000008E0000002900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00280000008E0000002900000000000000000000000000000081FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFDFBF9FFC08D51FFBF8C4FFFFCFAF8FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF0000008100000000000000001B1B1B7E717171FF1D1D
      1D8200000000341D037ED77610FF000000003D22048895510BD44023048C0000
      00003D22048895510BD44023048C000000000000000000000000000000000000
      000000000007000000510000008700000098000000870000004E0000002C0000
      008C000000280000000000000000000000000000000000000000000000000000
      000000000007000000510000008700000098000000870000004E0000002C0000
      008C000000280000000000000000000000000000000000000081FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFF2E8DCFFC99C68FFC99C68FFF2E7DBFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF000000810000000000000000000000001B1B1B7E7171
      71FF1D1D1D820000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00070000007A0000006200000017000000010000001700000060000000990000
      002C000000000000000000000000000000000000000000000000000000000000
      00070000007A0000006200000017000000010000001700000060000000990000
      002C000000000000000000000000000000000000000000000081FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFDBBD9BFFDBBD9BFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000081000000000000000000000000000000001B1B
      1B7E717171FF1D1D1D8200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0050000000620000000000000000000000990000000000000000000000600000
      0052000000000000000000000000000000000000000000000000000000000000
      0050000000620000000000000000000000000000000000000000000000600000
      0052000000000000000000000000000000000000000000000081FFFFFFFFFCFA
      F8FFF2E8DCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF2E7
      DBFFFCFAF8FFFFFFFFFF00000081000000000000000000000000000000000000
      00001B1B1B7E717171FF211F1D863B210487A85C0DE2D57610FEAA5D0DE33D22
      04890000000C0000000000000000000000000000000000000000000000000000
      0086000000190000000000000000000000990000000000000000000000170000
      0088000000000000000000000000000000000000000000000000000000000000
      0086000000190000000000000000000000000000000000000000000000170000
      0088000000000000000000000000000000000000000000000081E9D7C2FFC08C
      50FFC99C68FFDBBD9BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDBBD9BFFC99C
      68FFBF8C4FFFE8D6C1FF00000081000000000000000000000000000000000000
      0000000000001F1D1B82C27524FF573007A3050200270000000204020026552E
      06A08C4D0BCE0000000C00000000000000000000000000000000000000000000
      0096000000030000009900000099000000990000009900000099000000010000
      0099000000000000000000000000000000000000000000000000000000000000
      0096000000030000009900000099000000990000009900000099000000010000
      0099000000000000000000000000000000000000000000000081EAD9C5FFC08E
      52FFC99C68FFDBBD9BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDBBD9BFFC99C
      68FFC08D51FFE9D8C4FF00000081000000000000000000000000000000000000
      0000000000003A200485593007A40000000000000000D77610FF000000000000
      0000552E06A03D22048800000000000000000000000000000000000000000000
      00850000001A0000000000000000000000990000000000000000000000170000
      0087000000000000000000000000000000000000000000000000000000000000
      00850000001A0000000000000000000000000000000000000000000000170000
      0087000000000000000000000000000000000000000000000081FFFFFFFFFDFB
      F9FFF3E9DDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF3E8
      DDFFFDFBF9FFFFFFFFFF00000081000000000000000000000000000000000000
      000000000000A45A0DDF0503002A0000000000000000D77610FF000000000000
      000004020026AA5D0DE300000000000000000000000000000000000000000000
      004E000000640000000000000000000000990000000000000000000000620000
      0051000000000000000000000000000000000000000000000000000000000000
      004E000000640000000000000000000000000000000000000000000000620000
      0051000000000000000000000000000000000000000000000081FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFDBBD9BFFDBBD9BFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000081000000000000000000000000000000000000
      000000000000CF7210FA00000006D77610FFD77610FFD77610FFD77610FFD776
      10FF00000002D57510FE00000000000000000000000000000000000000000000
      000600000078000000640000001A0000000300000019000000620000007A0000
      0007000000000000000000000000000000000000000000000000000000000000
      000600000078000000640000001A0000000300000019000000620000007A0000
      0007000000000000000000000000000000000000000000000081FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFF3E9DDFFC99C68FFC99C68FFF2E8DCFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000081000000000000000000000000000000000000
      000000000000A3590DDE0603002B0000000000000000D77610FF000000000000
      000005020027A85C0DE200000000000000000000000000000000000000000000
      0000000000060000004E00000085000000960000008600000050000000070000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000060000004E00000085000000960000008600000050000000070000
      0000000000000000000000000000000000000000000000000081FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFDFBF9FFC08E52FFC08C51FFFDFBF8FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000081000000000000000000000000000000000000
      000000000000381F04835B3207A60000000000000000D77610FF000000000000
      0000573007A33B21048700000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000081FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFEAD9C5FFE9D7C3FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000081000000000000000000000000000000000000
      0000000000000000000A84490AC85B3207A60603002B00000006050300295930
      07A4884B0BCB0000000B00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000081000000810000
      0081000000810000008100000081000000810000008100000081000000810000
      0081000000810000008100000081000000000000000000000000000000000000
      000000000000000000000000000A381F0483A3590DDECF7210FAA45A0DDF3A20
      04850000000B0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000099000000990000
      0099000000990000009900000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000099000000990000
      0099000000990000009900000099000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000099FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000099FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000103211B2B
      77C1000103210000000000000000000000000000000000000000000000000001
      03211B2B77C10001032100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000099FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000099FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000100001200000000000000001A2B75BF2E4C
      D0FF233BA1E0000103210000000000000000000000000000000000010321233B
      A1E02E4CD0FF1A2B75BF0000000000000000000000000000000000000000B67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FFFFFFFFFFFFFFFFFF00000099000000000000000000000000B67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FF00000099FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000990000000000000000000000000000
      000000000000000000000201001D673808B00000000000000000000000000000
      00000000000000000000000000002715026D00000000000000000001031F233A
      9DDE2E4CD0FF233BA1E000010321000000000000000000010321233BA1E02E4C
      D0FF233A9DDE0001031F0000000000000000000000000000000000000000B67A
      36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFB67A36FFFFFFFFFFFFFFFFFF00000099000000000000000000000000B67A
      36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FF00000099FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000990000000000000000000000000000
      00000000000329170371C36B0FF3D77610FF0000000000000000000000000000
      0000000000000000000E49290596492805950000000000000000000000000001
      031F233A9DDE2E4CD0FF233BA1E00001032100010321233BA1E02E4CD0FF233A
      9DDE0001031F000000000000000000000000000000000000000000000000B67A
      36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFB67A36FFFFFFFFFFFFFFFFFF00000099000000000000000000000000B67A
      36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FF00000099FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000990000000000000000000000000804
      00348B4C0BCDD77610FFD77610FFD77610FF000000050000001005030029170D
      01554C2A0598BB670EEED77610FF0B06003A0000000000000000000000000000
      00000001031F233A9DDE2E4CD0FF233BA1E0233BA1E02E4CD0FF233A9DDE0001
      031F00000000000000000000000000000000000000000000000000000000B67A
      36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFB67A36FF000000990000009900000099000000000000000000000000B67A
      36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FF00000099000000990000
      009900000099000000990000009900000099000000000000000D44250590D174
      10FCD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FF703D08B8000000000000000000000000000000000000
      0000000000000001031F233A9DDE2E4CD0FF2E4CD0FF233A9DDE0001031F0000
      000000000000000000000000000000000000000000000000000000000000B67A
      36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFB67A36FF000000000000000000000000000000000000000000000000B67A
      36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFB67A36FF000000000000000000000000150C0151AA5D0DE3D77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFAD5F0DE50201001C000000000000000000000000000000000000
      00000000000000010321233BA1E02E4CD0FF2E4CD0FF233BA1E0000103210000
      000000000000000000000000000000000000000000000000000000000000B67A
      36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFB67A36FF000000000000000000000000000000000000000000000000B67A
      36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFB67A36FF000000000000000000000000130A014DA65B0DE0D77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FF8B4C0BCD0402002300000000000000000000000000000000000000000000
      000000010321233BA1E02E4CD0FF233A9DDE233A9DDE2E4CD0FF233BA1E00001
      032100000000000000000000000000000000000000990000009900000099B67A
      36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFB67A36FF0000000000000000000000000000009900000099000000990000
      0099000000990000009900000099E9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFB67A36FF000000000000000000000000000000000000000C4024048CD072
      10FBD77610FFD77610FFD77610FFD77610FFCD7010F9A85C0DE2643708AE180D
      0257000000030000000000000000000000000000000000000000000000000001
      0321233BA1E02E4CD0FF233A9DDE0001031F0001031F233A9DDE2E4CD0FF233B
      A1E00001032100000000000000000000000000000099FFFFFFFFFFFFFFFFB67A
      36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFB67A36FF00000000000000000000000000000099FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000099E9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFB67A36FF0000000000000000000000000000000000000000000000000704
      0031864A0ACAD77610FFD77610FFD77610FF0000000000000000000000000000
      000000000000000000000000000000000000000000000000000000010321233B
      A1E02E4CD0FF233A9DDE0001031F00000000000000000001031F233A9DDE2E4C
      D0FF233BA1E000010321000000000000000000000099FFFFFFFFFFFFFFFFB67A
      36FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFB67A36FF00000000000000000000000000000099FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000099E9D7C3FFE9D7C3FFE9D7C3FFE9D7C3FFE9D7
      C3FFB67A36FF0000000000000000000000000000000000000000000000000000
      0000000000032715026DC06A0FF1D77610FF0000000000000000000000000000
      00000000000000000000000000000000000000000000000000001A2B75BF2E4C
      D0FF233A9DDE0001031F000000000000000000000000000000000001031F233A
      9DDE2E4CD0FF1A2B75BF000000000000000000000099FFFFFFFFFFFFFFFFB67A
      36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FF00000000000000000000000000000099FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000099B67A36FFB67A36FFB67A36FFB67A36FFB67A
      36FFB67A36FF0000000000000000000000000000000000000000000000000000
      000000000000000000000201001B623508AC0000000000000000000000000000
      00000000000000000000000000000000000000000000000000000001031F192A
      73BD0001031F0000000000000000000000000000000000000000000000000001
      031F192A73BD0001031F000000000000000000000099FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000099000000000000000000000000000000000000
      00000000000000000000000000000000000000000099FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000099000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000099FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000099000000000000000000000000000000000000
      00000000000000000000000000000000000000000099FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF00000099000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000009900000099000000990000
      0099000000990000009900000099000000000000000000000000000000000000
      0000000000000000000000000000000000000000009900000099000000990000
      0099000000990000009900000099000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000002020225454545C76F6F
      6FFC464646C80202022700000000000000000000000002020225454545C76F6F
      6FFC464646C802020227000000000000000000000000666666F2717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF676767F40000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000F85
      BFDD14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF0F89C4E000000000000000000000000000000000434343C4161616710000
      00051414146D464646C8000000000000000000000000434343C4161616710000
      00051414146D464646C8000000000000000000000000717171FF000000000000
      0000000000000000000000000000000000000000000000000000717171FF0000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF717171FF00000000717171FF717171FF00000000717171FF717171FF0000
      0000717171FF717171FF717171FF000000000000000000000000000000000742
      609D14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF084564A0000000000000000000000000000000006B6B6BF9000000090000
      0000000000056F6F6FFD0000000000000000000000006B6B6BF9000000090000
      0000000000056F6F6FFC000000000000000000000000717171FF000000000000
      0000000000000000000000000000000000000000000000000000717171FF0000
      0000000000000000000000000000000000000000000000000000717171FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF000000000000000000000000000000000217
      225E14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF0319246100000000000000000000000000000000424242C3171717750000
      000916161671535353DB000000000000000000000000505050D7171717750000
      000916161671444444C6000000000000000000000000717171FF000000000000
      0000000000000000000000000000000000000000000000000000717171FF7171
      71FF717171FF676767F400000000000000000000000000000000717171FF0000
      0000D77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FF00000000717171FF000000000000000000000000000000000002
      031E14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF000204210000000000000000000000000000000001010122424242C36B6B
      6BF8666666F2363636B200000000000000001D1D1D82717171FF666666F26B6B
      6BF8434343C502020225000000000000000000000000717171FF000000000000
      0000000000000000000000000000000000000000000000000000717171FF0000
      000000000000717171FF00000000000000000000000000000000000000000000
      0000D77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000101011D0101011F414141C2717171FF717171FF555555DE0101011F0000
      00000000000000000000000000000000000000000000717171FF000000000000
      0000000000000000000000000000000000000000000000000000717171FF0000
      000000000000717171FF00000000000000000000000000000000717171FF0000
      0000D77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FF00000000717171FF000000000000000000000000000000000000
      00005C5C5CE5717171FF717171FF717171FF717171FF717171FF717171FF5F5F
      5FEA000000000000000000000000000000000000000000000000000000000000
      0000000000000909094A717171FF04040433555555DE0101011F000000000000
      00000000000000000000000000000000000000000000717171FF000000000000
      0000000000000000000000000000000000000000000000000000717171FF0000
      000000000000717171FF00000000000000000000000000000000717171FF0000
      0000D77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FF00000000717171FF000000000000000000000000000000000000
      00000A0A0A4C5C5C5CE6717171FF717171FF717171FF717171FF5C5C5CE70A0A
      0A4F000000000000000000000000000000000000000000000000000000000000
      0000000000024B4B4BCF717171FF555555DE010101201515156F000000030000
      00000000000000000000000000000000000000000000717171FF000000000000
      0000000000000000000000000000000000000000000000000000717171FF0000
      000000000000717171FF00000000000000000000000000000000000000000000
      0000D77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000717171FF717171FF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000D0D0D59717171FF555555DE0101011F1B1B1B7E717171FF0F0F0F5F0000
      00000000000000000000000000000000000000000000717171FF000000000000
      0000000000000000000000000000666666F2717171FF717171FF555555DE0000
      000000000000717171FF00000000000000000000000000000000717171FF0000
      0000D77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FF00000000717171FF000000000000000000000000000000000000
      0000000000000000000000000000717171FF717171FF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0005535353DB555555DE0101011F000000000101011D545454DC575757DF0000
      00070000000000000000000000000000000000000000717171FF000000000000
      0000000000000000000000000000717171FF717171FF555555DE0101011F0000
      000000000000717171FF00000000000000000000000000000000717171FF0000
      0000D77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FF00000000717171FF000000000000000000000000000000000000
      0000000000000000000000000000717171FF717171FF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000001313
      1368555555DE0101011F0000000000000000000000000101011D545454DC1414
      146D0000000000000000000000000000000000000000717171FF000000000000
      0000000000000000000000000000717171FF555555DE0101011F000000000000
      000000000000717171FF00000000000000000000000000000000000000000000
      0000D77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000717171FF717171FF00000000000000000000
      00000000000000000000000000000000000000000000000000000000000A4343
      43C40101011F00000000000000000000000000000000000000000101011D4343
      43C50000000C00000000000000000000000000000000646464F0717171FF7171
      71FF717171FF717171FF717171FF555555DE0101011F00000000666666F27171
      71FF717171FF555555DE00000000000000000000000000000000717171FF0000
      0000D77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FF00000000717171FF000000000000000000000000000000000000
      0000000000000000000000000000717171FF717171FF00000000000000000000
      00000000000000000000000000000000000000000000000000000D0D0D570101
      011F000000000000000000000000000000000000000000000000000000000101
      011D0D0D0D590000000000000000000000000000000000000000000000000000
      0000717171FF0000000000000000000000000000000000000000717171FF7171
      71FF555555DE0101011F00000000000000000000000000000000717171FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000717171FF000000000000000000000000000000000000
      0000000000000000000000000000717171FF717171FF00000000000000000000
      00000000000000000000000000000000000000000000000000000000000E0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000E0000000000000000000000000000000000000000000000000000
      0000717171FF0000000000000000000000000000000000000000717171FF5555
      55DE0101011F0000000000000000000000000000000000000000717171FF7171
      71FF717171FF00000000717171FF717171FF00000000717171FF717171FF0000
      0000717171FF717171FF717171FF000000000000000000000000000000000000
      0000000000000000000000000000424242C3454545C800000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000646464F0717171FF717171FF717171FF717171FF717171FF555555DE0101
      011F000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000371E04820000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D77610FF371E048200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000666666F2717171FF717171FF717171FF7171
      71FF717171FF717171FF676767F4000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000666666F2717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF676767F40000000000000000000000000000000000000000000000000000
      0000D77610FFD77610FF371E0482000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000129FE5F214B1
      FFFF14B1FFFF14B1FFFF00000000717171FF0000000000000000000000000000
      00000000000000000000717171FF00000000000000000002031E0C6590C014B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF073E5A98000000000000000000000000717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF717171FF717171FF717171FF7171
      71FF717171FF00000000000000000000000000000000140B014FB4630EEAD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFB6640EEB160C015200000000000000000000000014B1FFFF14B1
      FFFF14B1FFFF14B1FFFF00000000717171FF0000000000000000000000000000
      00000000000000000000717171FF000000000000000004253676010E154A14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14A9F5FA0002031D0000000000000000717171FF717171FF0000
      0000000000000000000000000000000000000000000000000000000000007171
      71FF717171FF00000000000000000000000000000000B0610DE7D77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFB6640EEB00000000000000000000000014B1FFFF14B1
      FFFF000000000000000000000000717171FF00000000717171FF717171FF7171
      71FF717171FF00000000717171FF00000000000000000B5F87BA000101140D77
      ABD114B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF06364F8E0000000000000000717171FF717171FF0000
      0000000000000000000000000000000000000000000000000000000000007171
      71FF717171FF00000000000000000000000000000000D77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFE6AB6BFFE7AD6EFFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FF00000000000000000000000014B1FFFF14B1
      FFFF000000000000000000000000717171FF0000000000000000000000000000
      00000000000000000000717171FF00000000000000000B638FBF031C28660216
      205B14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF12A0E7F30000011100000000717171FF717171FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFD87A17FFD87B18FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FF00000000000000000000000014B1FFFF14B1
      FFFF000000000000000000000000717171FF00000000717171FF717171FF7171
      71FF717171FF00000000717171FF00000000000000000B638FBF0A577DB30000
      000E0F86C1DE14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF04283B7B00000000717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF00000000666666F2717171FF7171
      71FF717171FF717171FF717171FF676767F400000000D77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFE9B379FFF0CBA5FFD77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FF00000000000000000000000014B1FFFF14B1
      FFFF000000000000000000000000717171FF0000000000000000000000000000
      00000000000000000000717171FF00000000000000000B638FBF0B638FBF0214
      1C56031F2D6C14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1FFFF14B1
      FFFF14B1FFFF14B1FFFF14B1FFFF108CCAE300000000717171FF717171FF7171
      71FF717171FF717171FF717171FF717171FF00000000717171FF000000000000
      0000000000000000000000000000717171FF00000000D77610FFD77610FFD776
      10FFD77610FFD77610FFD77610FFDB8325FFF8E8D7FFECBD8CFFD77610FFD776
      10FFD77610FFD77610FFD77610FF00000000000000000000000014B1FFFF14B1
      FFFF000000000000000000000000717171FF0000000000000000000000000000
      00000000000000000000717171FF00000000000000000B638FBF0B638FBF094F
      71AA000001120000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000717171FF717171FF0000
      00000000000000000000000000000000000000000000717171FF000000000000
      0000000000000000000000000000717171FF00000000D77610FFD77610FFD776
      10FFD77610FFDE903CFFE5A663FFD77610FFDA8224FFFFFFFEFFDB8529FFD776
      10FFD77610FFD77610FFD77610FF00000000000000000000000014B1FFFF14B1
      FFFF000000000000000000000000646464F0717171FF717171FF717171FF7171
      71FF717171FF717171FF666666F200000000000000000B638FBF0B638FBF0B63
      8FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B63
      8FBF0B638FBF00000000000000000000000000000000717171FF717171FF0000
      0000717171FF717171FF717171FF717171FF00000000717171FF000000000000
      0000000000000000000000000000717171FF00000000D77610FFD77610FFD776
      10FFD77610FFE09747FFFEFBF8FFEDC192FFF1CEA9FFFAEDDFFFD87A17FFD776
      10FFD77610FFD77610FFD77610FF00000000000000000000000014B1FFFF14B1
      FFFF000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000B638FBF0B638FBF0B63
      8FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B638FBF0B63
      8FBF0A5980B500000000000000000000000000000000717171FF717171FF0000
      0000717171FF00000000717171FF717171FF00000000717171FF000000000000
      0000000000000000000000000000717171FF00000000D77610FFD77610FFD776
      10FFD77610FFD77610FFDF9342FFEAB781FFE8B074FFDA8224FFD77610FFD776
      10FFD77610FFD77610FFD77610FF00000000000000000000000014B1FFFF14B1
      FFFF0000000000000000000000000000000000000000000000000000000014B1
      FFFF14B1FFFF000000000000000000000000000000000B638FBF0B638FBF0B63
      8FBF0B638FBF0B638FBF00000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000717171FF717171FF0000
      0000717171FF00000000717171FF717171FF00000000717171FF000000000000
      0000000000000000000000000000717171FF00000000AE5F0DE5D77610FFD776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFD77610FFB4630EEA00000000000000000000000014B1FFFF14B1
      FFFF0000000000000000000000000000000000000000000000000000000014B1
      FFFF14B1FFFF000000000000000000000000000000000A577FB40B638FBF0B63
      8FBF0B638FBF0A5980B500000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000646464F0717171FF0000
      0000717171FF717171FF717171FF717171FF00000000717171FF000000000000
      0000000000000000000000000000717171FF00000000130A014CAF600DE6D776
      10FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD77610FFD776
      10FFD77610FFB0610DE7140B014F00000000000000000000000014B1FFFF14B1
      FFFF00000000666666F2717171FF717171FF717171FF676767F40000000014B1
      FFFF14B1FFFF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000717171FF000000000000
      0000000000000000000000000000717171FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000129DE1F014B1
      FFFF00000000717171FF717171FF717171FF717171FF717171FF0000000014B1
      FFFF12A1E7F30000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000646464F0717171FF7171
      71FF717171FF717171FF717171FF666666F20000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000646464F0717171FF666666F200000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000D00000000100010000000000800600000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000}
    DesignInfo = 16777616
    ImageInfo = <
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224F
          70656E2220786D6C6E733D22687474703A2F2F7777772E77332E6F72672F3230
          30302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F7777772E
          77332E6F72672F313939392F786C696E6B2220783D223070782220793D223070
          78222076696577426F783D2230203020333220333222207374796C653D22656E
          61626C652D6261636B67726F756E643A6E6577203020302033322033323B2220
          786D6C3A73706163653D227072657365727665223E262331333B262331303B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B3C7374796C6520747970653D22
          746578742F6373732220786D6C3A73706163653D227072657365727665223E2E
          59656C6C6F777B66696C6C3A234646423131353B7D262331333B262331303B26
          23393B2E7374307B6F7061636974793A302E37353B7D3C2F7374796C653E0D0A
          3C6720636C6173733D22737430223E0D0A09093C7061746820636C6173733D22
          59656C6C6F772220643D224D322E322C32352E326C352E352D313263302E332D
          302E372C312D312E322C312E382D312E32483236563963302D302E362D302E34
          2D312D312D31483132563563302D302E362D302E342D312D312D31483343322E
          342C342C322C342E342C322C3576323020202623393B2623393B63302C302E32
          2C302C302E332C302E312C302E3443322E312C32352E332C322E322C32352E33
          2C322E322C32352E327A222F3E0D0A093C2F673E0D0A3C7061746820636C6173
          733D2259656C6C6F772220643D224D33312E332C313448392E364C342C323668
          32312E3863302E352C302C312E312D302E332C312E332D302E374C33322C3134
          2E374333322E312C31342E332C33312E382C31342C33312E332C31347A222F3E
          0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D2253
          6176652220786D6C6E733D22687474703A2F2F7777772E77332E6F72672F3230
          30302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F7777772E
          77332E6F72672F313939392F786C696E6B2220783D223070782220793D223070
          78222076696577426F783D2230203020333220333222207374796C653D22656E
          61626C652D6261636B67726F756E643A6E6577203020302033322033323B2220
          786D6C3A73706163653D227072657365727665223E262331333B262331303B3C
          7374796C6520747970653D22746578742F637373223E2E426C61636B7B66696C
          6C3A233732373237323B7D3C2F7374796C653E0D0A3C7061746820636C617373
          3D22426C61636B2220643D224D33312C30483139632D302E362C302D312C302E
          342D312C3176313663302C302E362C302E342C312C312C3168313263302E362C
          302C312D302E342C312D3156314333322C302E342C33312E362C302C33312C30
          7A204D33302C313648323056326831305631367A222F3E0D0A3C706174682063
          6C6173733D22426C61636B2220643D224D32322C323076344836762D36683130
          762D3448365634483343322E342C342C322C342E342C322C3576323263302C30
          2E362C302E342C312C312C3168323263302E362C302C312D302E342C312D3176
          2D374832327A204D31362C3448387638683856347A20202623393B204D31322C
          3130682D32563668325631307A222F3E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D22D0
          A1D0BBD0BED0B95F312220786D6C6E733D22687474703A2F2F7777772E77332E
          6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
          2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
          20793D22307078222076696577426F783D223020302033322033322220737479
          6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203332
          2033323B2220786D6C3A73706163653D227072657365727665223E262331333B
          262331303B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B3C7374796C6520
          747970653D22746578742F6373732220786D6C3A73706163653D227072657365
          727665223E2E426C75657B66696C6C3A233131373744373B7D262331333B2623
          31303B2623393B2E57686974657B66696C6C3A234646464646463B7D3C2F7374
          796C653E0D0A3C672069643D22D0A1D0BBD0BED0B95F32223E0D0A09093C7061
          746820636C6173733D22426C75652220643D224D362C3468323063322E322C30
          2C342C312E382C342C3476313463302C322E322D312E382C342D342C34483134
          6C2D362C36762D364836632D322E322C302D342D312E382D342D34563843322C
          352E382C332E382C342C362C347A222F3E0D0A09093C7061746820636C617373
          3D2257686974652220643D224D31372E312C313763302E312D302E332C302E32
          2D302E352C302E372D302E3963312D302E382C312E372D312E352C322D327330
          2E352D312C302E352D312E3563302D312D302E342D312E382D312E332D322E36
          632D302E382D302E372D322D312D332E342D3120202623393B2623393B632D31
          2E332C302D322E342C302E322D332E332C31632D302E382C302E372D312E332C
          312E362D312E332C322E376C322E332C302E3363302E322D302E372C302E342D
          312E332C302E392D312E3663302E342D302E342C302E392D302E352C312E362D
          302E3563302E372C302C312E322C302E322C312E362C302E3520202623393B26
          23393B73302E362C302E382C302E362C312E3263302C302E332D302E312C302E
          372D302E332C302E39632D302E312C302E322D302E362C302E362D312E332C31
          2E32632D302E372C302E362D302E392C302E392D312E322C312E34632D302E32
          2C302E352D302E312C322D302E312C32683220202623393B2623393B4331372C
          31382C31362E392C31372E332C31372E312C31377A222F3E0D0A09093C636972
          636C6520636C6173733D225768697465222063783D223136222063793D223231
          2220723D2231222F3E0D0A093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D2250
          617374652220786D6C6E733D22687474703A2F2F7777772E77332E6F72672F32
          3030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F777777
          2E77332E6F72672F313939392F786C696E6B2220783D223070782220793D2230
          7078222076696577426F783D2230203020333220333222207374796C653D2265
          6E61626C652D6261636B67726F756E643A6E6577203020302033322033323B22
          20786D6C3A73706163653D227072657365727665223E262331333B262331303B
          3C7374796C6520747970653D22746578742F6373732220786D6C3A7370616365
          3D227072657365727665223E2E426C61636B7B66696C6C3A233732373237323B
          7D262331333B262331303B2623393B2E59656C6C6F777B66696C6C3A23464642
          3131353B7D3C2F7374796C653E0D0A3C7061746820636C6173733D2259656C6C
          6F772220643D224D31322C323476344835632D302E362C302D312D302E342D31
          2D31563363302D302E362C302E342D312C312D3168337632324831327A204D32
          352C32682D337638683456322E384332362C322E342C32352E362C322C32352C
          327A222F3E0D0A3C7061746820636C6173733D22426C61636B2220643D224D32
          392C3132483135632D302E362C302D312C302E342D312C3176313663302C302E
          362C302E342C312C312C3168313463302E362C302C312D302E342C312D315631
          334333302C31322E342C32392E362C31322C32392C31327A204D32382C323848
          313656313420202623393B6831325632387A204D32362C3230682D38762D3268
          385632307A204D32362C3234682D38762D3268385632347A222F3E0D0A3C7061
          746820636C6173733D22426C61636B2220643D224D31382C32563163302D302E
          362D302E342D312D312D31682D34632D302E362C302D312C302E342D312C3176
          31682D32763363302C302E362C302E342C312C312C31683863302E362C302C31
          2D302E342C312D3156324831387A222F3E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D2243
          75742220786D6C6E733D22687474703A2F2F7777772E77332E6F72672F323030
          302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F7777772E77
          332E6F72672F313939392F786C696E6B2220783D223070782220793D22307078
          222076696577426F783D2230203020333220333222207374796C653D22656E61
          626C652D6261636B67726F756E643A6E6577203020302033322033323B222078
          6D6C3A73706163653D227072657365727665223E262331333B262331303B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B3C7374796C6520747970653D2274
          6578742F637373223E2E426C61636B7B66696C6C3A233732373237323B7D3C2F
          7374796C653E0D0A3C7061746820636C6173733D22426C61636B2220643D224D
          31362E342C31322E364C32362C336C2D362E362C31322E354C31362E342C3132
          2E367A204D31312E342C32322E3663302E342C302E372C302E362C312E352C30
          2E362C322E3463302C322E382D322E322C352D352C35632D322E382C302D352D
          322E322D352D3520202623393B73322E322D352C352D3563302E362C302C312E
          322C302E312C312E372C302E336C312E342D312E344C31322C32324C31312E34
          2C32322E367A204D31302C323563302D312E372D312E332D332D332D33632D31
          2E372C302D332C312E332D332C3373312E332C332C332C3320202623393B4338
          2E372C32382C31302C32362E372C31302C32357A204D32382C323563302C322E
          382D322E322C352D352C35632D322E382C302D352D322E322D352D3563302D30
          2E392C302E322D312E372C302E362D322E344C31362C32306C2D332C304C342C
          336C31372E332C31372E3320202623393B63302E352D302E322C312E312D302E
          332C312E372D302E334332352E382C32302C32382C32322E322C32382C32357A
          204D31362C313763302D302E362D302E342D312D312D31632D302E362C302D31
          2C302E342D312C3173302E342C312C312C314331352E362C31382C31362C3137
          2E362C31362C31377A204D32362C323520202623393B63302D312E372D312E33
          2D332D332D33632D312E372C302D332C312E332D332C3373312E332C332C332C
          334332342E372C32382C32362C32362E372C32362C32357A222F3E0D0A3C2F73
          76673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D2243
          6F70792220786D6C6E733D22687474703A2F2F7777772E77332E6F72672F3230
          30302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F7777772E
          77332E6F72672F313939392F786C696E6B2220783D223070782220793D223070
          78222076696577426F783D2230203020333220333222207374796C653D22656E
          61626C652D6261636B67726F756E643A6E6577203020302033322033323B2220
          786D6C3A73706163653D227072657365727665223E262331333B262331303B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B3C7374796C6520747970653D22
          746578742F637373223E2E426C61636B7B66696C6C3A233732373237323B7D3C
          2F7374796C653E0D0A3C7061746820636C6173733D22426C61636B2220643D22
          4D32312C30483943382E342C302C382C302E342C382C317635483343322E342C
          362C322C362E342C322C3776323263302C302E362C302E342C312C312C316831
          3863302E362C302C312D302E342C312D31762D35683563302E362C302C312D30
          2E342C312D31563720202623393B4C32312C307A204D32302C32384834563868
          3468326834763563302C302E362C302E342C312C312C31683576347634763256
          32387A204D32362C3132763130682D34762D396C2D372D37682D355632683468
          367634763163302C302E362C302E342C312C312C3168355631327A222F3E0D0A
          3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D22D0
          A1D0BBD0BED0B95F322220786D6C6E733D22687474703A2F2F7777772E77332E
          6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
          2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
          20793D22307078222076696577426F783D223020302033322033322220737479
          6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203332
          2033323B2220786D6C3A73706163653D227072657365727665223E262331333B
          262331303B3C7374796C6520747970653D22746578742F6373732220786D6C3A
          73706163653D227072657365727665223E2E426C75657B66696C6C3A23313137
          3744373B7D262331333B262331303B2623393B2E426C61636B7B66696C6C3A23
          3732373237323B7D3C2F7374796C653E0D0A3C7265637420783D22382220793D
          22362220636C6173733D22426C7565222077696474683D223138222068656967
          68743D223138222F3E0D0A3C673E0D0A09093C673E0D0A0909093C706F6C7967
          6F6E20636C6173733D22426C61636B2220706F696E74733D22362C323220342C
          323220342C32382031302C32382031302C323620362C3236202623393B262339
          3B222F3E0D0A0909093C706F6C79676F6E20636C6173733D22426C61636B2220
          706F696E74733D22342C3820362C3820362C342031302C342031302C3220342C
          32202623393B2623393B222F3E0D0A0909093C706F6C79676F6E20636C617373
          3D22426C61636B2220706F696E74733D2232382C32362032342C32362032342C
          32382033302C32382033302C32322032382C3232202623393B2623393B222F3E
          0D0A0909093C706F6C79676F6E20636C6173733D22426C61636B2220706F696E
          74733D2232342C322032342C342032382C342032382C382033302C382033302C
          32202623393B2623393B222F3E0D0A0909093C7265637420783D22342220793D
          2231302220636C6173733D22426C61636B222077696474683D22322220686569
          6768743D2234222F3E0D0A0909093C7265637420783D22342220793D22313622
          20636C6173733D22426C61636B222077696474683D223222206865696768743D
          2234222F3E0D0A0909093C7265637420783D2231322220793D22322220636C61
          73733D22426C61636B222077696474683D223422206865696768743D2232222F
          3E0D0A0909093C7265637420783D2231382220793D22322220636C6173733D22
          426C61636B222077696474683D223422206865696768743D2232222F3E0D0A09
          09093C7265637420783D2232382220793D2231302220636C6173733D22426C61
          636B222077696474683D223222206865696768743D2234222F3E0D0A0909093C
          7265637420783D2232382220793D2231362220636C6173733D22426C61636B22
          2077696474683D223222206865696768743D2234222F3E0D0A0909093C726563
          7420783D2231322220793D2232362220636C6173733D22426C61636B22207769
          6474683D223422206865696768743D2232222F3E0D0A0909093C726563742078
          3D2231382220793D2232362220636C6173733D22426C61636B22207769647468
          3D223422206865696768743D2232222F3E0D0A09093C2F673E0D0A09093C673E
          0D0A0909093C706F6C79676F6E20636C6173733D22426C61636B2220706F696E
          74733D22362C323220342C323220342C32382031302C32382031302C32362036
          2C3236202623393B2623393B222F3E0D0A0909093C706F6C79676F6E20636C61
          73733D22426C61636B2220706F696E74733D22342C3820362C3820362C342031
          302C342031302C3220342C32202623393B2623393B222F3E0D0A0909093C706F
          6C79676F6E20636C6173733D22426C61636B2220706F696E74733D2232382C32
          362032342C32362032342C32382033302C32382033302C32322032382C323220
          2623393B2623393B222F3E0D0A0909093C706F6C79676F6E20636C6173733D22
          426C61636B2220706F696E74733D2232342C322032342C342032382C34203238
          2C382033302C382033302C32202623393B2623393B222F3E0D0A0909093C7265
          637420783D22342220793D2231302220636C6173733D22426C61636B22207769
          6474683D223222206865696768743D2234222F3E0D0A0909093C726563742078
          3D22342220793D2231362220636C6173733D22426C61636B222077696474683D
          223222206865696768743D2234222F3E0D0A0909093C7265637420783D223132
          2220793D22322220636C6173733D22426C61636B222077696474683D22342220
          6865696768743D2232222F3E0D0A0909093C7265637420783D2231382220793D
          22322220636C6173733D22426C61636B222077696474683D2234222068656967
          68743D2232222F3E0D0A0909093C7265637420783D2232382220793D22313022
          20636C6173733D22426C61636B222077696474683D223222206865696768743D
          2234222F3E0D0A0909093C7265637420783D2232382220793D2231362220636C
          6173733D22426C61636B222077696474683D223222206865696768743D223422
          2F3E0D0A0909093C7265637420783D2231322220793D2232362220636C617373
          3D22426C61636B222077696474683D223422206865696768743D2232222F3E0D
          0A0909093C7265637420783D2231382220793D2232362220636C6173733D2242
          6C61636B222077696474683D223422206865696768743D2232222F3E0D0A0909
          3C2F673E0D0A093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D22D0
          A1D0BBD0BED0B95F322220786D6C6E733D22687474703A2F2F7777772E77332E
          6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
          2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
          20793D22307078222076696577426F783D223020302033322033322220737479
          6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203332
          2033323B2220786D6C3A73706163653D227072657365727665223E262331333B
          262331303B3C7374796C6520747970653D22746578742F6373732220786D6C3A
          73706163653D227072657365727665223E2E426C61636B7B66696C6C3A233732
          373237323B7D262331333B262331303B2623393B2E59656C6C6F777B66696C6C
          3A234646423131353B7D3C2F7374796C653E0D0A3C706F6C79676F6E20636C61
          73733D2259656C6C6F772220706F696E74733D22382C323220362C3330203236
          2C33302032342C323220222F3E0D0A3C7061746820636C6173733D22426C6163
          6B2220643D224D32302C3136682D32563463302D312E312D302E392D322D322D
          32732D322C302E392D322C32763132682D32632D322E322C302D342C312E382D
          342C346831364332342C31372E382C32322E322C31362C32302C31367A222F3E
          0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D2255
          6E646F2220786D6C6E733D22687474703A2F2F7777772E77332E6F72672F3230
          30302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F7777772E
          77332E6F72672F313939392F786C696E6B2220783D223070782220793D223070
          78222076696577426F783D2230203020333220333222207374796C653D22656E
          61626C652D6261636B67726F756E643A6E6577203020302033322033323B2220
          786D6C3A73706163653D227072657365727665223E262331333B262331303B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B3C7374796C6520747970653D22
          746578742F637373223E2E426C75657B66696C6C3A233131373744373B7D3C2F
          7374796C653E0D0A3C7061746820636C6173733D22426C75652220643D224D33
          322C323663302C302C302D382D31362D3876364C302C31344C31362C34763643
          33322C31302C33322C32362C33322C32367A222F3E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B3C7374796C6520747970
          653D22746578742F637373223E2E5265647B66696C6C3A234430344432463B7D
          3C2F7374796C653E0D0A3C706F6C79676F6E20636C6173733D22526564222070
          6F696E74733D2232352C342031362C313320372C3420342C372031332C313620
          342C323520372C32382031362C31392032352C32382032382C32352031392C31
          362032382C3720222F3E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B3C7374796C6520747970653D22746578742F6373732220786D6C3A737061
          63653D227072657365727665223E2E7374307B6F7061636974793A302E363B7D
          262331333B262331303B2623393B2E7374317B66696C6C3A234646464646463B
          7D262331333B262331303B2623393B2E7374327B66696C6C3A23333737424236
          3B7D262331333B262331303B2623393B2E7374337B6F7061636974793A302E37
          3B7D3C2F7374796C653E0D0A3C6720636C6173733D22737430223E0D0A09093C
          7265637420783D2231382220793D223138222077696474683D22313422206865
          696768743D223134222F3E0D0A09093C726563742077696474683D2231342220
          6865696768743D223134222F3E0D0A093C2F673E0D0A3C673E0D0A09093C7265
          637420783D2232302220793D2232302220636C6173733D227374312220776964
          74683D22313022206865696768743D223130222F3E0D0A09093C726563742078
          3D22322220793D22322220636C6173733D22737431222077696474683D223130
          22206865696768743D223130222F3E0D0A093C2F673E0D0A3C7265637420783D
          22362220793D22362220636C6173733D22737432222077696474683D22323022
          206865696768743D223230222F3E0D0A3C6720636C6173733D22737433223E0D
          0A09093C7265637420783D22382220793D22382220636C6173733D2273743122
          2077696474683D22313622206865696768743D223136222F3E0D0A093C2F673E
          0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B3C7374796C6520747970653D22746578742F6373732220786D6C3A737061
          63653D227072657365727665223E2E7374307B66696C6C3A233337374242363B
          7D262331333B262331303B2623393B2E7374317B6F7061636974793A302E363B
          7D262331333B262331303B2623393B2E7374327B66696C6C3A23464646464646
          3B7D262331333B262331303B2623393B2E7374337B6F7061636974793A302E37
          3B7D3C2F7374796C653E0D0A3C706F6C79676F6E20636C6173733D2273743022
          20706F696E74733D2231382C31382032362C31382032362C362031342C362031
          342C313420362C313420362C32362031382C323620222F3E0D0A3C6720636C61
          73733D22737431223E0D0A09093C7265637420783D2231382220793D22313822
          2077696474683D22313422206865696768743D223134222F3E0D0A09093C7265
          63742077696474683D22313422206865696768743D223134222F3E0D0A093C2F
          673E0D0A3C673E0D0A09093C7265637420783D2232302220793D223230222063
          6C6173733D22737432222077696474683D22313022206865696768743D223130
          222F3E0D0A09093C7265637420783D22322220793D22322220636C6173733D22
          737432222077696474683D22313022206865696768743D223130222F3E0D0A09
          3C2F673E0D0A3C6720636C6173733D22737433223E0D0A09093C706F6C79676F
          6E20636C6173733D227374322220706F696E74733D2231342C382031342C3134
          20382C313420382C32342031382C32342031382C31382032342C31382032342C
          38202623393B222F3E0D0A093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B3C7374796C6520747970
          653D22746578742F6373732220786D6C3A73706163653D227072657365727665
          223E2E7374307B6F7061636974793A302E363B7D262331333B262331303B2623
          393B2E7374317B66696C6C2D72756C653A6576656E6F64643B636C69702D7275
          6C653A6576656E6F64643B7D3C2F7374796C653E0D0A3C6720636C6173733D22
          737430223E0D0A09093C7061746820636C6173733D227374312220643D224D32
          382E382C32372E336C2D312E352C312E354C32302E362C3232632D312E352C31
          2E322D332E352C322D352E362C32632D352C302D392D342D392D3973342D392C
          392D3973392C342C392C3963302C322E312D302E372C342E312D322C352E3620
          202623393B2623393B4C32382E382C32372E337A204D31352C38632D332E392C
          302D372C332E312D372C3773332E312C372C372C3773372D332E312C372D3753
          31382E392C382C31352C387A204D31362C3230682D32762D34682D34762D3268
          34762D346832763468347632682D345632307A222F3E0D0A093C2F673E0D0A3C
          2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B3C7374796C6520747970
          653D22746578742F6373732220786D6C3A73706163653D227072657365727665
          223E2E7374307B6F7061636974793A302E363B7D262331333B262331303B2623
          393B2E7374317B66696C6C2D72756C653A6576656E6F64643B636C69702D7275
          6C653A6576656E6F64643B7D3C2F7374796C653E0D0A3C6720636C6173733D22
          737430223E0D0A09093C7061746820636C6173733D227374312220643D224D32
          382E382C32372E336C2D312E352C312E354C32302E362C3232632D312E352C31
          2E322D332E352C322D352E362C32632D352C302D392D342D392D3973342D392C
          392D3973392C342C392C3963302C322E312D302E372C342E312D322C352E3620
          202623393B2623393B4C32382E382C32372E337A204D31352C38632D332E392C
          302D372C332E312D372C3773332E312C372C372C3773372D332E312C372D3753
          31382E392C382C31352C387A204D31362C3136682D326C302C30682D34762D32
          68346C302C3068326C302C30683476324831364C31362C31367A222F3E0D0A09
          3C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D22D0
          A1D0BBD0BED0B95F312220786D6C6E733D22687474703A2F2F7777772E77332E
          6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
          2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
          20793D22307078222076696577426F783D223020302033322033322220737479
          6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203332
          2033323B2220786D6C3A73706163653D227072657365727665223E262331333B
          262331303B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B3C7374796C6520
          747970653D22746578742F6373732220786D6C3A73706163653D227072657365
          727665223E2E7374307B6F7061636974793A302E333B7D262331333B26233130
          3B2623393B2E7374317B66696C6C3A234646464646463B7D262331333B262331
          303B2623393B2E7374327B66696C6C3A233337374242363B7D3C2F7374796C65
          3E0D0A3C6720636C6173733D22737430223E0D0A09093C673E0D0A0909093C72
          65637420783D22322220793D2232222077696474683D22323822206865696768
          743D223238222F3E0D0A09093C2F673E0D0A09093C673E0D0A0909093C726563
          7420783D22322220793D2232222077696474683D22323822206865696768743D
          223238222F3E0D0A09093C2F673E0D0A093C2F673E0D0A3C673E0D0A09093C70
          61746820636C6173733D227374312220643D224D342C34763234683234563448
          347A222F3E0D0A09093C7061746820636C6173733D227374322220643D224D32
          332C3139762D32682D33762D326833762D326C352C334C32332C31397A222F3E
          0D0A09093C7061746820636C6173733D227374322220643D224D392C3139762D
          326833762D324839762D326C2D352C334C392C31397A222F3E0D0A09093C7061
          746820636C6173733D227374322220643D224D31332C32336832762D33683276
          3368326C2D332C354C31332C32337A222F3E0D0A09093C7061746820636C6173
          733D227374322220643D224D31362C346C2D332C35683276336832563968324C
          31362C347A222F3E0D0A093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D22D0
          A1D0BBD0BED0B95F312220786D6C6E733D22687474703A2F2F7777772E77332E
          6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
          2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
          20793D22307078222076696577426F783D223020302033322033322220737479
          6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203332
          2033323B2220786D6C3A73706163653D227072657365727665223E262331333B
          262331303B3C7374796C6520747970653D22746578742F6373732220786D6C3A
          73706163653D227072657365727665223E2E426C75657B66696C6C3A23313137
          3744373B7D262331333B262331303B2623393B2E426C61636B7B66696C6C3A23
          3732373237323B7D3C2F7374796C653E0D0A3C7061746820636C6173733D2242
          6C61636B2220643D224D31312C31374C302C32386C322C326C31312D31316C31
          2D316C2D322D324C31312C31377A222F3E0D0A3C673E0D0A09093C673E0D0A09
          09093C7061746820636C6173733D22426C75652220643D224D31392C32632D35
          2C302D392C342D392C3973342C392C392C3973392D342C392D395332342C322C
          31392C327A204D31392C3138632D332E392C302D372D332E312D372D3763302D
          332E392C332E312D372C372D3773372C332E312C372C3720202623393B262339
          3B2623393B4332362C31342E392C32322E392C31382C31392C31387A222F3E0D
          0A09093C2F673E0D0A093C2F673E0D0A3C706F6C79676F6E20636C6173733D22
          426C75652220706F696E74733D2231302C32362031302C32382031322C32362E
          352031322C33322031342C33322031342C32342031322C323420222F3E0D0A3C
          7061746820636C6173733D22426C75652220643D224D32392E312C32342E3843
          32382E362C32342E332C32372E392C32342C32372C3234632D302E392C302D31
          2E362C302E332D322E312C302E384332342E332C32352E352C32342C32362E35
          2C32342C323863302C312E352C302E332C322E352C302E382C332E3120202623
          393B63302E352C302E362C312E332C302E392C322E322C302E3963302E392C30
          2C312E362D302E332C322E312D302E3863302E362D302E362C302E392D312E37
          2C302E392D332E324333302C32362E352C32392E372C32352E352C32392E312C
          32342E387A204D32382C32392E3563302C302E352D302E352C312D312C312020
          2623393B732D312D302E352D312D31762D3363302D302E352C302E352D312C31
          2D3173312C302E352C312C315632392E357A222F3E0D0A3C7061746820636C61
          73733D22426C75652220643D224D32312E312C32342E384332302E362C32342E
          332C31392E392C32342C31392C3234632D302E392C302D312E362C302E332D32
          2E312C302E384331362E332C32352E352C31362C32362E352C31362C32386330
          2C312E352C302E332C322E352C302E382C332E3120202623393B63302E352C30
          2E362C312E332C302E392C322E322C302E3963302E392C302C312E362D302E33
          2C322E312D302E3863302E362D302E362C302E392D312E372C302E392D332E32
          4332322C32362E352C32312E372C32352E352C32312E312C32342E387A204D32
          302C32392E3563302C302E352D302E352C312D312C3120202623393B732D312D
          302E352D312D31762D3363302D302E352C302E352D312C312D3173312C302E35
          2C312C315632392E357A222F3E0D0A3C706F6C79676F6E20636C6173733D2242
          6C75652220706F696E74733D2232342C31302032302C31302032302C36203138
          2C362031382C31302031342C31302031342C31322031382C31322031382C3136
          2032302C31362032302C31322032342C313220222F3E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D22D0
          A1D0BBD0BED0B95F312220786D6C6E733D22687474703A2F2F7777772E77332E
          6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
          2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
          20793D22307078222076696577426F783D223020302033322033322220737479
          6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203332
          2033323B2220786D6C3A73706163653D227072657365727665223E262331333B
          262331303B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B3C7374796C6520
          747970653D22746578742F6373732220786D6C3A73706163653D227072657365
          727665223E2E426C61636B7B66696C6C3A233732373237323B7D262331333B26
          2331303B2623393B2E57686974657B66696C6C3A234646464646463B7D3C2F73
          74796C653E0D0A3C672069643D22D0A1D0BBD0BED0B95F32223E0D0A09093C70
          61746820636C6173733D22426C61636B2220643D224D33302C36763232483056
          364833307A222F3E0D0A09093C7061746820636C6173733D2257686974652220
          643D224D32382C31367636682D366C322D326C2D362D366C2D342C346C2D382D
          386C322D326C362C366C342D346C382C384C32382C31367A222F3E0D0A09093C
          7265637420783D22322220793D22382220636C6173733D225768697465222077
          696474683D223222206865696768743D2232222F3E0D0A09093C726563742078
          3D22322220793D2231322220636C6173733D225768697465222077696474683D
          223222206865696768743D2232222F3E0D0A09093C7265637420783D22322220
          793D2231362220636C6173733D225768697465222077696474683D2232222068
          65696768743D2232222F3E0D0A09093C7265637420783D22322220793D223230
          2220636C6173733D225768697465222077696474683D22322220686569676874
          3D2232222F3E0D0A09093C7265637420783D22322220793D2232342220636C61
          73733D225768697465222077696474683D223222206865696768743D2232222F
          3E0D0A09093C7265637420783D22362220793D2232342220636C6173733D2257
          68697465222077696474683D223222206865696768743D2232222F3E0D0A0909
          3C7265637420783D2231302220793D2232342220636C6173733D225768697465
          222077696474683D223222206865696768743D2232222F3E0D0A09093C726563
          7420783D2231342220793D2232342220636C6173733D22576869746522207769
          6474683D223222206865696768743D2232222F3E0D0A09093C7265637420783D
          2231382220793D2232342220636C6173733D225768697465222077696474683D
          223222206865696768743D2232222F3E0D0A09093C7265637420783D22323222
          20793D2232342220636C6173733D225768697465222077696474683D22322220
          6865696768743D2232222F3E0D0A09093C7265637420783D2232362220793D22
          32342220636C6173733D225768697465222077696474683D2232222068656967
          68743D2232222F3E0D0A093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224E
          65772220786D6C6E733D22687474703A2F2F7777772E77332E6F72672F323030
          302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F7777772E77
          332E6F72672F313939392F786C696E6B2220783D223070782220793D22307078
          222076696577426F783D2230203020333220333222207374796C653D22656E61
          626C652D6261636B67726F756E643A6E6577203020302033322033323B222078
          6D6C3A73706163653D227072657365727665223E262331333B262331303B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B3C7374796C6520747970653D2274
          6578742F637373223E2E426C61636B7B66696C6C3A233732373237323B7D3C2F
          7374796C653E0D0A3C7061746820636C6173733D22426C61636B2220643D224D
          31392C32483543342E342C322C342C322E342C342C3376323463302C302E362C
          302E342C312C312C3168323063302E362C302C312D302E342C312D3156394C31
          392C327A204D32342C323648365634683132763563302C302E362C302E342C31
          2C312C31683520202623393B5632367A222F3E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B3C7374796C6520747970653D22746578742F6373732220786D6C3A737061
          63653D227072657365727665223E2E57686974657B66696C6C3A234646464646
          463B7D262331333B262331303B2623393B2E426C75657B66696C6C3A23333737
          4242363B7D3C2F7374796C653E0D0A3C7265637420636C6173733D2257686974
          652220783D22362220793D2232222077696474683D2231382220686569676874
          3D22313022206F7061636974793D22302E37222F3E0D0A3C7265637420636C61
          73733D22426C75652220783D2232302220793D2232222077696474683D223222
          206865696768743D2238222F3E0D0A3C7061746820636C6173733D22426C7565
          2220643D224D32362C32682D3276313048365632483276323868323856364C32
          362C327A204D32362C323848365631346832305632387A222F3E0D0A3C2F7376
          673E0D0A}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C0000001E744558745469746C650056696577477269646C696E65
          733B477269646C696E65733B4DE0041B0000003849444154785EED934B0A0040
          0842BB93F73F9B03B30FA40F41B478062D44048D648A2F0018A5DCC0546403EF
          DF9740BDAB3AB80EE6C7F400C6B257B7F0ED20460000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D22D0
          A1D0BBD0BED0B95F312220786D6C6E733D22687474703A2F2F7777772E77332E
          6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
          2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
          20793D22307078222076696577426F783D223020302033322033322220737479
          6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203332
          2033323B2220786D6C3A73706163653D227072657365727665223E262331333B
          262331303B3C7374796C6520747970653D22746578742F6373732220786D6C3A
          73706163653D227072657365727665223E2E477265656E7B66696C6C3A233444
          414538393B7D262331333B262331303B2623393B2E57686974657B66696C6C3A
          234646464646463B7D262331333B262331303B2623393B2E7374307B6F706163
          6974793A302E363B7D262331333B262331303B2623393B2E7374317B6F706163
          6974793A302E333B7D3C2F7374796C653E0D0A3C672069643D22D0A1D0BBD0BE
          D0B95F322220636C6173733D22737430223E0D0A09093C7061746820636C6173
          733D22426C61636B2220643D224D342C32763238683234563248347A204D3236
          2C3238483656346832305632387A222F3E0D0A093C2F673E0D0A3C672069643D
          22D0A1D0BBD0BED0B95F332220636C6173733D22737431223E0D0A09093C7265
          637420783D2231322220793D22362220636C6173733D22426C61636B22207769
          6474683D22313222206865696768743D2232222F3E0D0A09093C726563742078
          3D2231322220793D2231302220636C6173733D22426C61636B22207769647468
          3D22313222206865696768743D2232222F3E0D0A09093C7265637420783D2231
          322220793D2231342220636C6173733D22426C61636B222077696474683D2231
          3222206865696768743D2232222F3E0D0A09093C7265637420783D2231322220
          793D2231382220636C6173733D22426C61636B222077696474683D2234222068
          65696768743D2232222F3E0D0A09093C7265637420783D2231322220793D2232
          322220636C6173733D22426C61636B222077696474683D223422206865696768
          743D2232222F3E0D0A09093C7265637420783D22382220793D22362220636C61
          73733D22426C61636B222077696474683D223222206865696768743D2232222F
          3E0D0A09093C7265637420783D22382220793D2231302220636C6173733D2242
          6C61636B222077696474683D223222206865696768743D2232222F3E0D0A0909
          3C7265637420783D22382220793D2231342220636C6173733D22426C61636B22
          2077696474683D223222206865696768743D2232222F3E0D0A09093C72656374
          20783D22382220793D2231382220636C6173733D22426C61636B222077696474
          683D223222206865696768743D2232222F3E0D0A09093C7265637420783D2238
          2220793D2232322220636C6173733D22426C61636B222077696474683D223222
          206865696768743D2232222F3E0D0A093C2F673E0D0A3C7265637420783D2231
          382220793D2231382220636C6173733D22477265656E222077696474683D2231
          3422206865696768743D223134222F3E0D0A3C706F6C79676F6E20636C617373
          3D2257686974652220706F696E74733D2232322C32332032342C32352032382C
          32312033302C32332032342C32392032302C323520222F3E0D0A3C2F7376673E
          0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D22D0
          A1D0BBD0BED0B95F312220786D6C6E733D22687474703A2F2F7777772E77332E
          6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
          2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
          20793D22307078222076696577426F783D223020302031362031362220737479
          6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203136
          2031363B2220786D6C3A73706163653D227072657365727665223E262331333B
          262331303B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B3C7374796C6520
          747970653D22746578742F6373732220786D6C3A73706163653D227072657365
          727665223E2E7374307B66696C6C3A233337374242363B7D262331333B262331
          303B2623393B2E7374317B6F7061636974793A302E333B7D3C2F7374796C653E
          0D0A3C673E0D0A09093C673E0D0A0909093C706F6C79676F6E20636C6173733D
          227374302220706F696E74733D2231302C332031332C302031362C3320262339
          3B2623393B222F3E0D0A09093C2F673E0D0A093C2F673E0D0A3C6720636C6173
          733D22737431223E0D0A09093C673E0D0A0909093C7061746820643D224D3132
          2C3135682D312E386C2D312E342D332E3448342E324C322E392C313548314C35
          2E362C3368312E384C31322C31357A204D382E332C31302E344C362E352C342E
          386C2D312E382C352E3648382E337A222F3E0D0A09093C2F673E0D0A09093C67
          3E0D0A0909093C7061746820643D224D31322C3135682D312E386C2D312E342D
          332E3448342E324C322E392C313548314C352E362C3368312E384C31322C3135
          7A204D382E332C31302E344C362E352C342E386C2D312E382C352E3648382E33
          7A222F3E0D0A09093C2F673E0D0A093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D22D0
          A1D0BBD0BED0B95F312220786D6C6E733D22687474703A2F2F7777772E77332E
          6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
          2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
          20793D22307078222076696577426F783D223020302031362031362220737479
          6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203136
          2031363B2220786D6C3A73706163653D227072657365727665223E262331333B
          262331303B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B3C7374796C6520
          747970653D22746578742F6373732220786D6C3A73706163653D227072657365
          727665223E2E7374307B6F7061636974793A302E333B7D262331333B26233130
          3B2623393B2E7374317B66696C6C3A233337374242363B7D3C2F7374796C653E
          0D0A3C6720636C6173733D22737430223E0D0A09093C673E0D0A0909093C7061
          746820643D224D31322C3135682D312E386C2D312E342D332E3448342E324C32
          2E392C313548314C352E362C3368312E384C31322C31357A204D382E332C3130
          2E344C362E352C342E386C2D312E382C352E3648382E337A222F3E0D0A09093C
          2F673E0D0A09093C673E0D0A0909093C7061746820636C6173733D22426C6163
          6B2220643D224D31322C3135682D312E386C2D312E342D332E3448342E324C32
          2E392C313548314C352E362C3368312E384C31322C31357A204D382E332C3130
          2E344C362E352C342E386C2D312E382C352E3648382E337A222F3E0D0A09093C
          2F673E0D0A093C2F673E0D0A3C673E0D0A09093C706F6C79676F6E20636C6173
          733D227374312220706F696E74733D2231302C302031332C332031362C302026
          23393B222F3E0D0A093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D2242
          6F6C642220786D6C6E733D22687474703A2F2F7777772E77332E6F72672F3230
          30302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F7777772E
          77332E6F72672F313939392F786C696E6B2220783D223070782220793D223070
          78222076696577426F783D2230203020333220333222207374796C653D22656E
          61626C652D6261636B67726F756E643A6E6577203020302033322033323B2220
          786D6C3A73706163653D227072657365727665223E262331333B262331303B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B3C7374796C6520747970653D22
          746578742F637373223E2E426C61636B7B66696C6C3A233732373237323B7D3C
          2F7374796C653E0D0A3C7061746820636C6173733D22426C61636B2220643D22
          4D382C32365634683863322E352C302C342E332C302E352C352E372C312E3463
          312E332C302E392C322C322E322C322C332E3863302C312E322D302E342C322E
          322D312E322C332E31632D302E382C302E392D312E382C312E352D332E312C31
          2E3976302E3120202623393B63312E362C302E322C322E382C302E382C332E38
          2C312E3763302E392C312C312E342C322E312C312E342C332E3563302C322D30
          2E372C332E362D322E322C342E384332302E392C32352E342C31392C32362C31
          362E352C323648387A204D31332C372E3776352E3268322E3220202623393B63
          312C302C312E382D302E322C322E342D302E3763302E362D302E352C302E392D
          312E322C302E392D3263302D312E362D312E322D322E342D332E362D322E3448
          31337A204D31332C31362E3676352E3868322E3763312E312C302C322D302E33
          2C322E372D302E3863302E362D302E352C312D312E332C312D322E3220202623
          393B63302D302E392D302E332D312E362D312D322E31632D302E362D302E352D
          312E352D302E382D322E372D302E384831337A222F3E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D2249
          74616C69632220786D6C6E733D22687474703A2F2F7777772E77332E6F72672F
          323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F7777
          772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D22
          307078222076696577426F783D2230203020333220333222207374796C653D22
          656E61626C652D6261636B67726F756E643A6E6577203020302033322033323B
          2220786D6C3A73706163653D227072657365727665223E262331333B26233130
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B2623393B2623393B262339
          3B2623393B2623393B2623393B2623393B2623393B3C7374796C652074797065
          3D22746578742F637373223E2E426C61636B7B66696C6C3A233732373237323B
          7D3C2F7374796C653E0D0A3C706F6C79676F6E20636C6173733D22426C61636B
          2220706F696E74733D2232312E362C362032322C342031342C342031332E362C
          362031352E382C362031322C323420392E382C323420392E342C32362031372E
          362C32362031372E382C32342031352E362C32342031392E342C3620222F3E0D
          0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D2255
          6E6465726C696E652220786D6C6E733D22687474703A2F2F7777772E77332E6F
          72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F
          2F7777772E77332E6F72672F313939392F786C696E6B2220783D223070782220
          793D22307078222076696577426F783D2230203020333220333222207374796C
          653D22656E61626C652D6261636B67726F756E643A6E65772030203020333220
          33323B2220786D6C3A73706163653D227072657365727665223E262331333B26
          2331303B2623393B2623393B2623393B2623393B2623393B2623393B2623393B
          2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B
          2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B
          2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B
          2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B
          2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B
          2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B
          2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B
          2623393B2623393B2623393B2623393B2623393B2623393B3C7374796C652074
          7970653D22746578742F637373223E2E426C61636B7B66696C6C3A2337323732
          37323B7D3C2F7374796C653E0D0A3C7061746820636C6173733D22426C61636B
          2220643D224D382C31352E37563468332E3476313163302C342C312E362C362C
          342E372C3663332C302C342E352D312E392C342E352D352E3856344832347631
          312E3463302C352E372D322E372C382E362D382E322C382E3620202623393B43
          31302E362C32342C382C32312E322C382C31352E377A204D362C323876326832
          30762D3248367A222F3E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D2253
          7472696B656F75742220786D6C6E733D22687474703A2F2F7777772E77332E6F
          72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F
          2F7777772E77332E6F72672F313939392F786C696E6B2220783D223070782220
          793D22307078222076696577426F783D2230203020333220333222207374796C
          653D22656E61626C652D6261636B67726F756E643A6E65772030203020333220
          33323B2220786D6C3A73706163653D227072657365727665223E262331333B26
          2331303B2623393B2623393B2623393B2623393B2623393B2623393B2623393B
          2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B
          2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B
          2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B
          2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B
          2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B
          2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B
          2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B
          2623393B2623393B2623393B2623393B2623393B2623393B3C7374796C652074
          7970653D22746578742F637373223E2E426C61636B7B66696C6C3A2337323732
          37323B7D3C2F7374796C653E0D0A3C7061746820636C6173733D22426C61636B
          2220643D224D32342C3134682D362E39632D302E322D302E312D302E342D302E
          322D302E372D302E34632D302E372D302E332D312E342D302E362D322D302E39
          632D302E362D302E332D312D302E362D312E342D302E38632D302E342D302E33
          2D302E372D302E362D302E392D3120202623393B632D302E322D302E332D302E
          332D302E382D302E332D312E3263302D302E352C302E312D302E392C302E342D
          312E3363302E332D302E342C302E362D302E362C312D302E3963302E342D302E
          322C302E392D302E342C312E342D302E3563302E352D302E312C312E312D302E
          312C312E362D302E3120202623393B63322C302C332E352C302E342C342E382C
          312E3356342E38632D312D302E352D322E372D302E382D352D302E38632D312C
          302D322C302E312D332C302E33632D312C302E322D312E382C302E362D322E36
          2C312E3143392E382C352E392C392E322C362E352C382E372C372E3243382E32
          2C382C382C382E382C382C392E3820202623393B63302C302E382C302E312C31
          2E352C302E342C322E3163302E332C302E362C302E362C312E312C312E312C31
          2E3663302E312C302E312C302E332C302E332C302E352C302E34483676326837
          2E3163302E312C302C302E312C302E312C302E322C302E3163302E372C302E33
          2C312E332C302E362C312E392C302E3920202623393B63302E362C302E332C31
          2E312C302E362C312E362C302E3963302E342C302E332C302E382C302E362C31
          2C3163302E322C302E342C302E342C302E382C302E342C312E3363302C302E39
          2D302E342C312E372D312E312C322E32632D302E372C302E352D312E392C302E
          372D332E342C302E3720202623393B632D302E342C302D302E392C302D312E34
          2D302E31632D302E352D302E312D312D302E322D312E362D302E34632D302E35
          2D302E322D312D302E342D312E352D302E36632D302E352D302E322D302E392D
          302E352D312E322D302E3876332E3663302E332C302E322C302E372C302E342C
          312E322C302E3520202623393B63302E352C302E312C312C302E332C312E362C
          302E3463302E362C302E312C312E312C302E322C312E362C302E3263302E352C
          302E312C312C302E312C312E342C302E3163312E312C302C322E312D302E312C
          332E312D302E3363312D302E322C312E392D302E352C322E362D312020262339
          3B63302E382D302E352C312E342D312E312C312E382D312E3863302E342D302E
          372C302E372D312E372C302E372D322E3763302D302E382D302E322D312E352D
          302E352D322E31632D302E332D302E362D302E372D312E322D312E322D312E37
          632D302E312D302E312D302E322D302E322D302E332D302E3368345631347A22
          2F3E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B3C7374796C6520747970653D22746578742F6373732220786D6C3A737061
          63653D227072657365727665223E2E7374307B6F7061636974793A302E333B7D
          262331333B262331303B2623393B2E7374317B66696C6C3A234646464646463B
          7D262331333B262331303B2623393B2E7374327B66696C6C3A23443034443246
          3B7D262331333B262331303B2623393B2E7374337B6F7061636974793A302E38
          3B7D262331333B262331303B2623393B2E7374347B6F7061636974793A302E34
          3B7D3C2F7374796C653E0D0A3C6720636C6173733D22737430223E0D0A09093C
          706F6C79676F6E20706F696E74733D22332C31342031352C322032382C313520
          31362C3237202623393B222F3E0D0A093C2F673E0D0A3C7061746820636C6173
          733D227374312220643D224D31372C313063312E372C302C332C312E332C332C
          33732D312E332C332D332C33732D332D312E332D332D335331352E332C31302C
          31372C31307A222F3E0D0A3C7061746820636C6173733D227374322220643D22
          4D33322C32362E364C32392C31366C2D332C31302E3663302C302E312C302C30
          2E322C302C302E3463302C312E372C312E332C332C332C3373332D312E332C33
          2D334333322C32362E392C33322C32362E382C33322C32362E367A222F3E0D0A
          3C6720636C6173733D22737433223E0D0A09093C706F6C79676F6E20706F696E
          74733D22352C3320372C312031392C31332031372C3135202623393B222F3E0D
          0A093C2F673E0D0A3C6720636C6173733D22737434223E0D0A09093C706F6C79
          676F6E20706F696E74733D22302C313720332C31342031362C32372031332C33
          30202623393B222F3E0D0A093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B3C7374796C6520747970653D22746578742F6373732220786D6C3A737061
          63653D227072657365727665223E2E477265656E7B66696C6C3A233444414538
          393B7D262331333B262331303B2623393B2E426C75657B66696C6C3A23333737
          4242363B7D262331333B262331303B2623393B2E7374307B6F7061636974793A
          302E363B7D262331333B262331303B2623393B2E7374317B6F7061636974793A
          302E343B7D262331333B262331303B2623393B2E7374327B6F7061636974793A
          302E37353B7D262331333B262331303B2623393B2E7374337B6F706163697479
          3A302E323B7D262331333B262331303B2623393B2E7374347B6F706163697479
          3A302E353B7D3C2F7374796C653E0D0A3C7265637420783D2232322220793D22
          322220636C6173733D22426C7565222077696474683D22382220686569676874
          3D2238222F3E0D0A3C7265637420783D2232322220793D2231322220636C6173
          733D22477265656E222077696474683D223822206865696768743D2238222F3E
          0D0A3C6720636C6173733D22737430223E0D0A09093C7265637420783D223232
          2220793D2232322220636C6173733D22426C61636B222077696474683D223822
          206865696768743D2238222F3E0D0A093C2F673E0D0A3C6720636C6173733D22
          737431223E0D0A09093C7265637420783D2231322220793D2232322220636C61
          73733D22426C61636B222077696474683D223822206865696768743D2238222F
          3E0D0A093C2F673E0D0A3C6720636C6173733D22737432223E0D0A09093C7265
          637420783D2231322220793D2231322220636C6173733D22477265656E222077
          696474683D223822206865696768743D2238222F3E0D0A093C2F673E0D0A3C67
          20636C6173733D22737432223E0D0A09093C7265637420783D2231322220793D
          22322220636C6173733D22426C7565222077696474683D223822206865696768
          743D2238222F3E0D0A093C2F673E0D0A3C6720636C6173733D22737433223E0D
          0A09093C7265637420783D22322220793D2232322220636C6173733D22426C61
          636B222077696474683D223822206865696768743D2238222F3E0D0A093C2F67
          3E0D0A3C6720636C6173733D22737434223E0D0A09093C7265637420783D2232
          2220793D2231322220636C6173733D22477265656E222077696474683D223822
          206865696768743D2238222F3E0D0A093C2F673E0D0A3C6720636C6173733D22
          737434223E0D0A09093C7265637420783D22322220793D22322220636C617373
          3D22426C7565222077696474683D223822206865696768743D2238222F3E0D0A
          093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B3C7374796C6520747970
          653D22746578742F6373732220786D6C3A73706163653D227072657365727665
          223E2E426C75657B66696C6C3A233337374242363B7D262331333B262331303B
          2623393B2E477265656E7B66696C6C3A233444414538393B7D262331333B2623
          31303B2623393B2E7374307B6F7061636974793A302E363B7D3C2F7374796C65
          3E0D0A3C673E0D0A09093C673E0D0A0909093C7061746820636C6173733D2247
          7265656E2220643D224D362E322C32382E3663322E322C312E372C342E392C32
          2E392C372E382C332E33762D362E31632D312E332D302E332D322E352D302E38
          2D332E352D312E354C362E322C32382E367A222F3E0D0A0909093C7061746820
          636C6173733D22477265656E2220643D224D32312E352C32342E33632D312C30
          2E372D322E322C312E322D332E352C312E3576362E3163322E392D302E342C35
          2E362D312E352C372E382D332E324C32312E352C32342E337A222F3E0D0A0909
          3C2F673E0D0A09093C673E0D0A0909093C7061746820636C6173733D22477265
          656E2220643D224D362E322C32382E3663322E322C312E372C342E392C322E39
          2C372E382C332E33762D362E31632D312E332D302E332D322E352D302E382D33
          2E352D312E354C362E322C32382E367A222F3E0D0A0909093C7061746820636C
          6173733D22477265656E2220643D224D32312E352C32342E33632D312C302E37
          2D322E322C312E322D332E352C312E3576362E3163322E392D302E342C352E36
          2D312E352C372E382D332E324C32312E352C32342E337A222F3E0D0A09093C2F
          673E0D0A093C2F673E0D0A3C673E0D0A09093C7061746820636C6173733D2242
          6C75652220643D224D31382C302E3176362E3163312E332C302E332C322E352C
          302E382C332E352C312E356C342E332D342E334332332E362C312E372C32302E
          392C302E352C31382C302E317A222F3E0D0A09093C7061746820636C6173733D
          22426C75652220643D224D31302E352C372E3763312D302E372C322E322D312E
          322C332E352D312E3556302E31632D322E392C302E342D352E362C312E352D37
          2E382C332E324C31302E352C372E377A222F3E0D0A093C2F673E0D0A3C672063
          6C6173733D22737430223E0D0A09093C7061746820636C6173733D22426C7565
          2220643D224D302E312C313468362E3163302E332D312E332C302E382D322E35
          2C312E352D332E354C332E342C362E3243312E372C382E342C302E352C31312E
          312C302E312C31347A222F3E0D0A09093C7061746820636C6173733D22426C75
          652220643D224D32382E362C362E326C2D342E332C342E3363302E372C312C31
          2E322C322E322C312E352C332E3568362E314333312E352C31312E312C33302E
          332C382E342C32382E362C362E327A222F3E0D0A093C2F673E0D0A3C6720636C
          6173733D22737430223E0D0A09093C7061746820636C6173733D22477265656E
          2220643D224D302E312C313863302E342C322E392C312E362C352E362C332E33
          2C372E386C342E332D342E33632D302E372D312D312E322D322E322D312E352D
          332E3548302E317A222F3E0D0A09093C7061746820636C6173733D2247726565
          6E2220643D224D32352E382C3138632D302E332C312E332D302E382C322E352D
          312E352C332E356C342E332C342E3363312E372D322E322C322E392D342E392C
          332E332D372E384832352E387A222F3E0D0A093C2F673E0D0A3C2F7376673E0D
          0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B3C7374796C6520747970653D22746578742F63737322
          3E2E7374307B6F7061636974793A302E363B7D3C2F7374796C653E0D0A3C6720
          636C6173733D22737430223E0D0A09093C7265637420783D22362220793D2232
          2220636C6173733D22426C61636B222077696474683D22323022206865696768
          743D2232222F3E0D0A09093C7265637420783D2231302220793D22362220636C
          6173733D22426C61636B222077696474683D22313222206865696768743D2232
          222F3E0D0A09093C7265637420783D22362220793D2231302220636C6173733D
          22426C61636B222077696474683D22323022206865696768743D2232222F3E0D
          0A093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B3C7374796C6520747970653D22746578742F63737322
          3E2E7374307B6F7061636974793A302E363B7D3C2F7374796C653E0D0A3C6720
          636C6173733D22737430223E0D0A09093C7265637420783D22362220793D2238
          2220636C6173733D22426C61636B222077696474683D22323022206865696768
          743D2232222F3E0D0A09093C7265637420783D2231302220793D223132222063
          6C6173733D22426C61636B222077696474683D22313222206865696768743D22
          32222F3E0D0A09093C7265637420783D22362220793D2231362220636C617373
          3D22426C61636B222077696474683D22323022206865696768743D2232222F3E
          0D0A093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B3C7374796C6520747970653D22746578742F63737322
          3E2E7374307B6F7061636974793A302E363B7D3C2F7374796C653E0D0A3C6720
          636C6173733D22737430223E0D0A09093C7265637420783D22362220793D2231
          362220636C6173733D22426C61636B222077696474683D223230222068656967
          68743D2232222F3E0D0A09093C7265637420783D2231302220793D2232302220
          636C6173733D22426C61636B222077696474683D22313222206865696768743D
          2232222F3E0D0A09093C7265637420783D22362220793D2232342220636C6173
          733D22426C61636B222077696474683D22323022206865696768743D2232222F
          3E0D0A093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B3C7374796C6520747970653D22746578742F63737322
          3E2E7374307B6F7061636974793A302E363B7D3C2F7374796C653E0D0A3C6720
          636C6173733D22737430223E0D0A09093C673E0D0A0909093C7265637420783D
          22362220793D22382220636C6173733D22426C61636B222077696474683D2232
          3022206865696768743D2232222F3E0D0A0909093C7265637420783D22362220
          793D2231322220636C6173733D22426C61636B222077696474683D2231362220
          6865696768743D2232222F3E0D0A0909093C7265637420783D22362220793D22
          31362220636C6173733D22426C61636B222077696474683D2232302220686569
          6768743D2232222F3E0D0A0909093C7265637420783D22362220793D22323022
          20636C6173733D22426C61636B222077696474683D2231362220686569676874
          3D2232222F3E0D0A0909093C7265637420783D22362220793D2232342220636C
          6173733D22426C61636B222077696474683D22323022206865696768743D2232
          222F3E0D0A09093C2F673E0D0A093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B3C7374796C6520747970653D22746578742F637373223E2E7374
          307B6F7061636974793A302E363B7D3C2F7374796C653E0D0A3C6720636C6173
          733D22737430223E0D0A09093C7265637420783D22362220793D22382220636C
          6173733D22426C61636B222077696474683D22323022206865696768743D2232
          222F3E0D0A09093C7265637420783D2231302220793D2231322220636C617373
          3D22426C61636B222077696474683D22313222206865696768743D2232222F3E
          0D0A09093C7265637420783D22362220793D2231362220636C6173733D22426C
          61636B222077696474683D22323022206865696768743D2232222F3E0D0A0909
          3C7265637420783D2231302220793D2232302220636C6173733D22426C61636B
          222077696474683D22313222206865696768743D2232222F3E0D0A09093C7265
          637420783D22362220793D2232342220636C6173733D22426C61636B22207769
          6474683D22323022206865696768743D2232222F3E0D0A093C2F673E0D0A3C2F
          7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B3C7374796C6520747970653D22746578742F63737322
          3E2E7374307B6F7061636974793A302E363B7D3C2F7374796C653E0D0A3C6720
          636C6173733D22737430223E0D0A09093C7265637420783D22362220793D2238
          2220636C6173733D22426C61636B222077696474683D22323022206865696768
          743D2232222F3E0D0A09093C7265637420783D22392E392220793D2231322220
          636C6173733D22426C61636B222077696474683D2231362E3122206865696768
          743D2232222F3E0D0A09093C7265637420783D22362220793D2231362220636C
          6173733D22426C61636B222077696474683D22323022206865696768743D2232
          222F3E0D0A09093C7265637420783D22392E392220793D2232302220636C6173
          733D22426C61636B222077696474683D2231362E3122206865696768743D2232
          222F3E0D0A09093C7265637420783D22362220793D2232342220636C6173733D
          22426C61636B222077696474683D22323022206865696768743D2232222F3E0D
          0A093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B3C7374796C6520747970653D22746578742F6373732220786D6C3A737061
          63653D227072657365727665223E2E7374307B6F7061636974793A302E333B7D
          262331333B262331303B2623393B2E7374317B66696C6C3A234646464646463B
          7D3C2F7374796C653E0D0A3C6720636C6173733D22737430223E0D0A09093C67
          3E0D0A0909093C7061746820643D224D32382C32304C382C327632366C362D36
          6C352C31306C362D336C2D352E312D394832387A222F3E0D0A09093C2F673E0D
          0A09093C673E0D0A0909093C7061746820643D224D32382C32304C382C327632
          366C362D366C352C31306C362D336C2D352E312D394832387A222F3E0D0A0909
          3C2F673E0D0A093C2F673E0D0A3C706F6C79676F6E20636C6173733D22737431
          2220706F696E74733D2231382C31392032342E352C313920392E352C352E3520
          392E352C32342E372031342E332C32302031392E352C33302032332C32382E33
          20222F3E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B2623
          393B3C7374796C6520747970653D22746578742F6373732220786D6C3A737061
          63653D227072657365727665223E2E57686974657B66696C6C3A234646464646
          463B7D262331333B262331303B2623393B2E426C75657B66696C6C3A23333737
          4242363B7D262331333B262331303B2623393B2E7374307B6F7061636974793A
          302E363B7D3C2F7374796C653E0D0A3C6720636C6173733D22737430223E0D0A
          09093C706F6C79676F6E20636C6173733D22426C61636B2220706F696E74733D
          2232382C362032382C313820362C313820362C323420342C323420342C313620
          32362C31362032362C36202623393B222F3E0D0A093C2F673E0D0A3C673E0D0A
          09093C636972636C6520636C6173733D22426C7565222063783D223136222063
          793D2231372220723D2235222F3E0D0A09093C636972636C6520636C6173733D
          22426C7565222063783D223237222063793D22372220723D2235222F3E0D0A09
          093C636972636C6520636C6173733D22426C7565222063783D2235222063793D
          2232372220723D2235222F3E0D0A093C2F673E0D0A3C673E0D0A09093C636972
          636C6520636C6173733D225768697465222063783D223136222063793D223137
          2220723D22322E35222F3E0D0A09093C636972636C6520636C6173733D225768
          697465222063783D223237222063793D22372220723D22322E35222F3E0D0A09
          093C636972636C6520636C6173733D225768697465222063783D223522206379
          3D2232372220723D22322E35222F3E0D0A093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D2252
          65646F2220786D6C6E733D22687474703A2F2F7777772E77332E6F72672F3230
          30302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F7777772E
          77332E6F72672F313939392F786C696E6B2220783D223070782220793D223070
          78222076696577426F783D2230203020333220333222207374796C653D22656E
          61626C652D6261636B67726F756E643A6E6577203020302033322033323B2220
          786D6C3A73706163653D227072657365727665223E262331333B262331303B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B2623393B2623393B26
          23393B2623393B2623393B2623393B2623393B2623393B3C7374796C65207479
          70653D22746578742F637373223E2E426C75657B66696C6C3A23313137374437
          3B7D3C2F7374796C653E0D0A3C7061746820636C6173733D22426C7565222064
          3D224D31362C313056346C31362C31304C31362C3234762D3643302C31382C30
          2C32362C302C323653302C31302C31362C31307A222F3E0D0A3C2F7376673E0D
          0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B3C7374796C6520747970653D22746578742F6373732220786D6C3A737061
          63653D227072657365727665223E2E57686974657B66696C6C3A234646464646
          463B7D262331333B262331303B2623393B2E426C75657B66696C6C3A23333737
          4242363B7D262331333B262331303B2623393B2E7374307B6F7061636974793A
          302E363B7D3C2F7374796C653E0D0A3C6720636C6173733D22737430223E0D0A
          09093C7265637420783D22362220793D22382220636C6173733D22426C61636B
          222077696474683D22323022206865696768743D223230222F3E0D0A093C2F67
          3E0D0A3C7265637420783D22362220636C6173733D22426C7565222077696474
          683D22323022206865696768743D2236222F3E0D0A3C673E0D0A09093C726563
          7420783D22382220793D2231302220636C6173733D2257686974652220776964
          74683D22313622206865696768743D223136222F3E0D0A09093C726563742078
          3D22382220793D22322220636C6173733D225768697465222077696474683D22
          3422206865696768743D2232222F3E0D0A09093C7265637420783D2231342220
          793D22322220636C6173733D225768697465222077696474683D223422206865
          696768743D2232222F3E0D0A09093C7265637420783D2232302220793D223222
          20636C6173733D225768697465222077696474683D223422206865696768743D
          2232222F3E0D0A093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D22D0
          A1D0BBD0BED0B95F312220786D6C6E733D22687474703A2F2F7777772E77332E
          6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
          2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
          20793D22307078222076696577426F783D223020302033322033322220737479
          6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203332
          2033323B2220786D6C3A73706163653D227072657365727665223E262331333B
          262331303B3C7374796C6520747970653D22746578742F6373732220786D6C3A
          73706163653D227072657365727665223E2E426C75657B66696C6C3A23333737
          4242363B7D262331333B262331303B2623393B2E57686974657B66696C6C3A23
          4646464646463B7D262331333B262331303B2623393B2E7374307B6F70616369
          74793A302E363B7D262331333B262331303B2623393B2E7374317B6F70616369
          74793A302E373B7D3C2F7374796C653E0D0A3C672069643D22D0A1D0BBD0BED0
          B95F322220636C6173733D22737430223E0D0A09093C7061746820636C617373
          3D22426C61636B2220643D224D31382C3576313463302C302E352D302E352C31
          2D312C316C302C30632D302E352C302D312D302E352D312D31762D3663302D31
          2E372D312E332D332D332D336C302C30632D312E372C302D332C312E332D332C
          33763131683256313320202623393B2623393B63302D302E352C302E352D312C
          312D316C302C3063302E352C302C312C302E352C312C31763663302C312E372C
          312E332C332C332C336C302C3063312E372C302C332D312E332C332D33563548
          31387A222F3E0D0A09093C7265637420783D22322220793D22382220636C6173
          733D22426C61636B222077696474683D223222206865696768743D223136222F
          3E0D0A09093C706F6C79676F6E20706F696E74733D2232382C382032382C3134
          2032342C31342032342C32342032362C32342032362C31362033302C31362033
          302C38202623393B222F3E0D0A093C2F673E0D0A3C7265637420793D22322220
          636C6173733D22426C7565222077696474683D223622206865696768743D2236
          222F3E0D0A3C6720636C6173733D22737431223E0D0A09093C7061746820636C
          6173733D2257686974652220643D224D322C3476326832563448327A222F3E0D
          0A093C2F673E0D0A3C7265637420783D2231362220793D22322220636C617373
          3D22426C7565222077696474683D223622206865696768743D2236222F3E0D0A
          3C6720636C6173733D22737431223E0D0A09093C7061746820636C6173733D22
          57686974652220643D224D31382C347632683256344831387A222F3E0D0A093C
          2F673E0D0A3C7265637420783D2232362220793D22322220636C6173733D2242
          6C7565222077696474683D223622206865696768743D2236222F3E0D0A3C6720
          636C6173733D22737431223E0D0A09093C7061746820636C6173733D22576869
          74652220643D224D32382C347632683256344832387A222F3E0D0A093C2F673E
          0D0A3C7265637420783D2232322220793D2232342220636C6173733D22426C75
          65222077696474683D223622206865696768743D2236222F3E0D0A3C6720636C
          6173733D22737431223E0D0A09093C7061746820636C6173733D225768697465
          2220643D224D32342C323676326832762D324832347A222F3E0D0A093C2F673E
          0D0A3C7265637420783D22382220793D2232342220636C6173733D22426C7565
          222077696474683D223622206865696768743D2236222F3E0D0A3C6720636C61
          73733D22737431223E0D0A09093C7061746820636C6173733D22576869746522
          20643D224D31302C323676326832762D324831307A222F3E0D0A093C2F673E0D
          0A3C7265637420793D2232342220636C6173733D22426C756522207769647468
          3D223622206865696768743D2236222F3E0D0A3C6720636C6173733D22737431
          223E0D0A09093C7061746820636C6173733D2257686974652220643D224D322C
          323676326832762D3248327A222F3E0D0A093C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D22D0
          A1D0BBD0BED0B95F312220786D6C6E733D22687474703A2F2F7777772E77332E
          6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
          2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
          20793D22307078222076696577426F783D223020302033322033322220737479
          6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203332
          2033323B2220786D6C3A73706163653D227072657365727665223E262331333B
          262331303B3C7374796C6520747970653D22746578742F6373732220786D6C3A
          73706163653D227072657365727665223E2E426C75657B66696C6C3A23333737
          4242363B7D262331333B262331303B2623393B2E57686974657B66696C6C3A23
          4646464646463B7D262331333B262331303B2623393B2E7374307B6F70616369
          74793A302E363B7D262331333B262331303B2623393B2E7374317B6F70616369
          74793A302E373B7D3C2F7374796C653E0D0A3C672069643D22D181D0BBD0BED0
          B95F322220636C6173733D22737430223E0D0A09093C706F6C79676F6E20636C
          6173733D22426C61636B2220706F696E74733D2231302C362032362C36203236
          2C32302032342C32302032342C382031302C382031302C36202623393B222F3E
          0D0A093C2F673E0D0A3C7265637420783D22342220793D22342220636C617373
          3D22426C7565222077696474683D223622206865696768743D2236222F3E0D0A
          3C6720636C6173733D22737431223E0D0A09093C7061746820636C6173733D22
          57686974652220643D224D362C3676326832563648367A222F3E0D0A093C2F67
          3E0D0A3C7265637420783D2232322220793D2232302220636C6173733D22426C
          7565222077696474683D223622206865696768743D2236222F3E0D0A3C672063
          6C6173733D22737431223E0D0A09093C7061746820636C6173733D2257686974
          652220643D224D32342C323276326832762D324832347A222F3E0D0A093C2F67
          3E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D22D0
          A1D0BBD0BED0B95F312220786D6C6E733D22687474703A2F2F7777772E77332E
          6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
          2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
          20793D22307078222076696577426F783D223020302033322033322220737479
          6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203332
          2033323B2220786D6C3A73706163653D227072657365727665223E262331333B
          262331303B3C7374796C6520747970653D22746578742F6373732220786D6C3A
          73706163653D227072657365727665223E2E426C75657B66696C6C3A23333737
          4242363B7D262331333B262331303B2623393B2E57686974657B66696C6C3A23
          4646464646463B7D262331333B262331303B2623393B2E7374307B6F70616369
          74793A302E363B7D262331333B262331303B2623393B2E7374317B6F70616369
          74793A302E373B7D3C2F7374796C653E0D0A3C672069643D22D181D0BBD0BED0
          B95F322220636C6173733D22737430223E0D0A09093C706F6C79676F6E20636C
          6173733D22426C61636B2220706F696E74733D22362C3820382C382C20382C32
          322032322C32322032322C323420362C323420362C38202623393B222F3E0D0A
          093C2F673E0D0A3C7265637420783D22342220793D22342220636C6173733D22
          426C7565222077696474683D223622206865696768743D2236222F3E0D0A3C67
          20636C6173733D22737431223E0D0A09093C7061746820636C6173733D225768
          6974652220643D224D362C3676326832563648367A222F3E0D0A093C2F673E0D
          0A3C7265637420783D2232322220793D2232302220636C6173733D22426C7565
          222077696474683D223622206865696768743D2236222F3E0D0A3C6720636C61
          73733D22737431223E0D0A09093C7061746820636C6173733D22576869746522
          20643D224D32342C323276326832762D324832347A222F3E0D0A093C2F673E0D
          0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D22D0
          A1D0BBD0BED0B95F312220786D6C6E733D22687474703A2F2F7777772E77332E
          6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
          2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
          20793D22307078222076696577426F783D223020302033322033322220737479
          6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203332
          2033323B2220786D6C3A73706163653D227072657365727665223E262331333B
          262331303B3C7374796C6520747970653D22746578742F6373732220786D6C3A
          73706163653D227072657365727665223E2E426C75657B66696C6C3A23333737
          4242363B7D262331333B262331303B2623393B2E57686974657B66696C6C3A23
          4646464646463B7D262331333B262331303B2623393B2E7374307B6F70616369
          74793A302E363B7D262331333B262331303B2623393B2E7374317B6F70616369
          74793A302E373B7D3C2F7374796C653E0D0A3C672069643D22D0A1D0BBD0BED0
          B95F322220636C6173733D22737430223E0D0A09093C7061746820636C617373
          3D22426C61636B2220643D224D32322C32324839632D302E352C302D312D302E
          352D312D31762D3463302D302E352C302E352D312C312D3168313463312E372C
          302C332D312E332C332D33563963302D312E372D312E332D332D332D33483876
          3268313563302E352C302C312C302E352C312C3120202623393B2623393B7634
          63302C302E352D302E352C312D312C314839632D312E372C302D332C312E332D
          332C33763463302C312E372C312E332C332C332C336831335632327A222F3E0D
          0A093C2F673E0D0A3C7265637420783D22342220793D22342220636C6173733D
          22426C7565222077696474683D223622206865696768743D2236222F3E0D0A3C
          6720636C6173733D22737431223E0D0A09093C7061746820636C6173733D2257
          686974652220643D224D362C3676326832563648367A222F3E0D0A093C2F673E
          0D0A3C7265637420783D2232322220793D2232302220636C6173733D22426C75
          65222077696474683D223622206865696768743D2236222F3E0D0A3C6720636C
          6173733D22737431223E0D0A09093C7061746820636C6173733D225768697465
          2220643D224D32342C323276326832762D324832347A222F3E0D0A093C2F673E
          0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D22D0
          A1D0BBD0BED0B95F312220786D6C6E733D22687474703A2F2F7777772E77332E
          6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
          2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
          20793D22307078222076696577426F783D223020302033322033322220737479
          6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203332
          2033323B2220786D6C3A73706163653D227072657365727665223E262331333B
          262331303B3C7374796C6520747970653D22746578742F6373732220786D6C3A
          73706163653D227072657365727665223E2E426C75657B66696C6C3A23333737
          4242363B7D262331333B262331303B2623393B2E57686974657B66696C6C3A23
          4646464646463B7D262331333B262331303B2623393B2E7374307B6F70616369
          74793A302E363B7D262331333B262331303B2623393B2E7374317B6F70616369
          74793A302E373B7D3C2F7374796C653E0D0A3C672069643D22D0A1D0BBD0BED0
          B95F322220636C6173733D22737430223E0D0A09093C706F6C79676F6E20706F
          696E74733D2231302C382032342C32302032322C323220382C3130202623393B
          222F3E0D0A093C2F673E0D0A3C7265637420783D22342220793D22342220636C
          6173733D22426C7565222077696474683D223622206865696768743D2236222F
          3E0D0A3C6720636C6173733D22737431223E0D0A09093C7061746820636C6173
          733D2257686974652220643D224D362C3676326832563648367A222F3E0D0A09
          3C2F673E0D0A3C7265637420783D2232322220793D2232302220636C6173733D
          22426C7565222077696474683D223622206865696768743D2236222F3E0D0A3C
          6720636C6173733D22737431223E0D0A09093C7061746820636C6173733D2257
          686974652220643D224D32342C323276326832762D324832347A222F3E0D0A09
          3C2F673E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D225F
          7833335F5369676E732220786D6C6E733D22687474703A2F2F7777772E77332E
          6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
          2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
          20793D22307078222076696577426F783D223020302033322033322220737479
          6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203332
          2033323B2220786D6C3A73706163653D227072657365727665223E262331333B
          262331303B3C7374796C6520747970653D22746578742F6373732220786D6C3A
          73706163653D227072657365727665223E2E59656C6C6F777B66696C6C3A2346
          46423131353B7D262331333B262331303B2623393B2E5265647B66696C6C3A23
          4431314331433B7D262331333B262331303B2623393B2E477265656E7B66696C
          6C3A233033394332333B7D3C2F7374796C653E0D0A3C636972636C6520636C61
          73733D22477265656E222063783D2239222063793D22392220723D2237222F3E
          0D0A3C7061746820636C6173733D2259656C6C6F772220643D224D32342E352C
          322E3463302E332D302E362C302E382D302E362C312E312C306C362E332C3132
          2E3563302E332C302E362C302E312C312E312D302E352C312E314831382E3763
          2D302E362C302D302E382D302E352D302E352D312E314C32342E352C322E347A
          222F3E0D0A3C7061746820636C6173733D225265642220643D224D322E342C32
          352E39632D302E352D302E352D302E352D312E332C302D312E386C352E372D35
          2E3763302E352D302E352C312E332D302E352C312E382C306C352E372C352E37
          63302E352C302E352C302E352C312E332C302C312E386C2D352E372C352E3720
          202623393B632D302E352C302E352D312E332C302E352D312E382C304C322E34
          2C32352E397A222F3E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D224C
          617965725F312220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B3C7374796C6520747970653D22746578742F6373732220786D6C3A737061
          63653D227072657365727665223E2E426C75657B66696C6C3A23313137374437
          3B7D262331333B262331303B2623393B2E426C61636B7B66696C6C3A23373237
          3237323B7D3C2F7374796C653E0D0A3C706F6C79676F6E20636C6173733D2242
          6C61636B2220706F696E74733D2231372E322C31322031392E322C313020322C
          313020322C32342031302C32342031302C323220342C323220342C313220222F
          3E0D0A3C7061746820636C6173733D22426C75652220643D224D32372C31334C
          31372C32336C2D342D344C32332C394C32372C31337A204D32382C31326C312E
          372D312E3763302E342D302E342C302E342D312C302D312E334C32372C362E33
          632D302E342D302E342D312D302E342D312E332C304C32342C384C32382C3132
          7A20202623393B204D31322C3230763468344C31322C32307A222F3E0D0A3C2F
          7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D2253
          686164696E672220786D6C6E733D22687474703A2F2F7777772E77332E6F7267
          2F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A2F2F77
          77772E77332E6F72672F313939392F786C696E6B2220783D223070782220793D
          22307078222076696577426F783D2230203020333220333222207374796C653D
          22656E61626C652D6261636B67726F756E643A6E657720302030203332203332
          3B2220786D6C3A73706163653D227072657365727665223E262331333B262331
          303B3C7374796C6520747970653D22746578742F6373732220786D6C3A737061
          63653D227072657365727665223E2E426C61636B7B66696C6C3A233732373237
          323B7D262331333B262331303B2623393B2E426C75657B66696C6C3A23313137
          3744373B7D3C2F7374796C653E0D0A3C7061746820636C6173733D22426C7565
          2220643D224D31382E342C3863322E362C302C342E382C342E332C342E382C39
          2E3663302C312E332C312E312C322E342C322E342C322E3463312E332C302C32
          2E342D312E312C322E342D322E344332382C31322E332C32332E372C382C3138
          2E342C387A222F3E0D0A3C7061746820636C6173733D22426C61636B2220643D
          224D31372C392E336C332C334C31332E332C3139632D312E342C312E342D332E
          372C312E342D352E312C306C2D352E312D352E31632D312E342D312E342D312E
          342D332E372C302D352E314C392E372C326C332C336C2D312E392C312E392020
          2623393B4331302E332C372E342C31302C382E322C31302C3963302C302E382C
          302E332C312E362C302E392C322E3163302E362C302E362C312E332C302E392C
          322E312C302E3973312E362D302E332C322E312D302E394C31372C392E337A20
          4D31332E372C392E376C352E392D352E3920202623393B63302E342D302E342C
          302E342D312C302D312E346C302C30632D302E342D302E342D312D302E342D31
          2E342C306C2D352E392C352E39632D302E342C302E342D302E342C312C302C31
          2E346C302C304331322E372C31302E312C31332E332C31302E312C31332E372C
          392E377A222F3E0D0A3C2F7376673E0D0A}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          3C3F786D6C2076657273696F6E3D22312E302220656E636F64696E673D225554
          462D38223F3E0D0A3C7376672076657273696F6E3D22312E31222069643D22D0
          A1D0BBD0BED0B95F312220786D6C6E733D22687474703A2F2F7777772E77332E
          6F72672F323030302F7376672220786D6C6E733A786C696E6B3D22687474703A
          2F2F7777772E77332E6F72672F313939392F786C696E6B2220783D2230707822
          20793D22307078222076696577426F783D223020302033322033322220737479
          6C653D22656E61626C652D6261636B67726F756E643A6E657720302030203332
          2033323B2220786D6C3A73706163653D227072657365727665223E262331333B
          262331303B3C7374796C6520747970653D22746578742F6373732220786D6C3A
          73706163653D227072657365727665223E2E7374307B6F7061636974793A302E
          363B7D262331333B262331303B2623393B2E7374317B66696C6C3A2333373742
          42363B7D262331333B262331303B2623393B2E7374327B66696C6C3A23344441
          4538393B7D262331333B262331303B2623393B2E7374337B6F7061636974793A
          302E333B7D262331333B262331303B2623393B2E7374347B66696C6C3A234646
          464646463B7D3C2F7374796C653E0D0A3C6720636C6173733D22737430223E0D
          0A09093C673E0D0A0909093C706F6C79676F6E20706F696E74733D2231332C32
          382031302C32382031302C323020382C323020382C33302031332C3330203133
          2C33322031362C32392031332C3236202623393B2623393B222F3E0D0A090909
          3C706F6C79676F6E20706F696E74733D2231322C31312031302C31312031302C
          362031362C362031362C3420382C3420382C313120362C313120392C31342026
          23393B2623393B222F3E0D0A0909093C706F6C79676F6E20706F696E74733D22
          32322C32332032302C32332032302C382031382C382031382C32332031362C32
          332031392C3236202623393B2623393B222F3E0D0A0909093C706F6C79676F6E
          20706F696E74733D22342C3220322C3220322C323820302C323820332C333220
          362C323820342C3238202623393B2623393B222F3E0D0A0909093C706F6C7967
          6F6E20706F696E74733D2232382C32382032352C32382032352C32362032322C
          32392032352C33322032352C33302033302C33302033302C32302032382C3230
          202623393B2623393B222F3E0D0A0909093C706F6C79676F6E20706F696E7473
          3D2232382C31312032362C31312032392C31342033322C31312033302C313120
          33302C342032322C342032322C362032382C36202623393B2623393B222F3E0D
          0A0909093C7265637420783D2231382220793D2234222077696474683D223222
          206865696768743D2232222F3E0D0A0909093C7265637420783D22382220793D
          223136222077696474683D223222206865696768743D2232222F3E0D0A090909
          3C7265637420783D2231382220793D223238222077696474683D223222206865
          696768743D2232222F3E0D0A0909093C7265637420783D2232382220793D2231
          36222077696474683D223222206865696768743D2232222F3E0D0A09093C2F67
          3E0D0A093C2F673E0D0A3C7061746820636C6173733D227374312220643D224D
          31362C323676366836762D364831367A222F3E0D0A3C7061746820636C617373
          3D227374322220643D224D31362C327636683656324831367A222F3E0D0A3C67
          20636C6173733D22737433223E0D0A09093C673E0D0A0909093C726563742078
          3D2232362220793D223134222077696474683D223622206865696768743D2236
          222F3E0D0A0909093C7265637420783D22362220793D22313422207769647468
          3D223622206865696768743D2236222F3E0D0A09093C2F673E0D0A093C2F673E
          0D0A3C7061746820636C6173733D227374342220643D224D32382C3138762D32
          683276324832387A204D31382C323868327632682D325632387A204D31382C34
          68327632682D3256347A204D382C31366832763248385631367A222F3E0D0A3C
          2F7376673E0D0A}
      end>
  end
  object bpmStandardShape: TdxBarPopupMenu
    BarManager = bmManager
    ItemLinks = <>
    UseOwnFont = False
    Left = 400
    Top = 408
    PixelsPerInch = 96
  end
  object bpmConnectors: TdxBarPopupMenu
    BarManager = bmManager
    ItemLinks = <
      item
        Visible = True
        ItemName = 'bbRightAngle'
      end
      item
        Visible = True
        ItemName = 'bbRightAngleVertical'
      end
      item
        Visible = True
        ItemName = 'bbCurved'
      end
      item
        Visible = True
        ItemName = 'bbStraight'
      end>
    UseOwnFont = False
    Left = 120
    Top = 328
    PixelsPerInch = 96
  end
  object strScreenTips: TdxBarScreenTipRepository
    Left = 536
    Top = 360
    PixelsPerInch = 96
    object stPaste: TdxScreenTip
      Description.Text = 'Paste the contents of the Clipboard.'
      UseHintAsHeader = True
    end
    object stCut: TdxScreenTip
      Header.Text = 'stCut'
      Description.Text = 
        'Remove the selection and put it on the Clipboard so you can past' +
        'e it somewhere else.'
      UseHintAsHeader = True
    end
    object stCopy: TdxScreenTip
      Header.Text = 'stCopy'
      Description.Text = 
        'Put a copy of the selection on the Clipboard so you can paste it' +
        ' somewhere else.'
      UseHintAsHeader = True
    end
    object stFontName: TdxScreenTip
      Header.Text = 'stFontName'
      Description.Text = 'Pick a new font for your text.'#13#10
      UseHintAsHeader = True
    end
    object stFontSize: TdxScreenTip
      Header.Text = 'stFontSize'
      Description.Text = 'Change the size of your text.'
      UseHintAsHeader = True
    end
    object stFontSizeInc: TdxScreenTip
      Header.Text = 'stFontSizeInc'
      Description.Text = 'Make your text a bit bigger.'#13#10
      UseHintAsHeader = True
    end
    object stFontSizeDec: TdxScreenTip
      Header.Text = 'stFontSizeDec'
      Description.Text = 'Make your text a bit smaller.'
      UseHintAsHeader = True
    end
    object stFontBold: TdxScreenTip
      Header.Text = 'stFontBold'
      Description.Text = 'Make your text bold.'#13#10
      UseHintAsHeader = True
    end
    object stFontItalic: TdxScreenTip
      Header.Text = 'stFontItalic'
      Description.Text = 'Italicize your text.'#13#10
      UseHintAsHeader = True
    end
    object stUnderline: TdxScreenTip
      Header.Text = 'stUnderline'
      Description.Text = 'Underline your text.'#13#10
      UseHintAsHeader = True
    end
    object stStrikeout: TdxScreenTip
      Header.Text = 'stStrikeout'
      Description.Text = 'Cross something out by drawing a line through it.'#13#10
      UseHintAsHeader = True
    end
    object stFontColor: TdxScreenTip
      Header.Text = 'stFontColor'
      Description.Text = 'Change the color of your text.'
      UseHintAsHeader = True
    end
    object stTextAlignTop: TdxScreenTip
      Header.Text = 'stTextAlignTop'
      Description.Text = 'Align text to the top of the text block.'
      UseHintAsHeader = True
    end
    object stTextAlignMiddle: TdxScreenTip
      Header.Text = 'stTextAlignMiddle'
      Description.Text = 
        'Align text so that it is centered between the top and bottom of ' +
        'the text block.'
      UseHintAsHeader = True
    end
    object stTextAlignBottom: TdxScreenTip
      Header.Text = 'stTextAlignBottom'
      Description.Text = 'Align text to the bottom of the text block.'
      UseHintAsHeader = True
    end
    object stTextAlignLeft: TdxScreenTip
      Header.Text = 'stTextAlignLeft'
      Description.Text = 'Align your content to the left.'
      UseHintAsHeader = True
    end
    object stTextAlignCenter: TdxScreenTip
      Header.Text = 'stTextAlignCenter'
      Description.Text = 'Center your content.'
      UseHintAsHeader = True
    end
    object stTextAlignRight: TdxScreenTip
      Header.Text = 'stTextAlignRight'
      Description.Text = 'Align your content to the right.'
      UseHintAsHeader = True
    end
    object stPointerTool: TdxScreenTip
      Header.Text = 'stPointerTool'
      Description.Text = 'Select, move, and resize objects.'
      UseHintAsHeader = True
    end
    object stConnector: TdxScreenTip
      Header.Text = 'stConnector'
      Description.Text = 'Draw connectors between objects.'
      UseHintAsHeader = True
    end
    object stBackgroundColor: TdxScreenTip
      Header.Text = 'stBackgroundColor'
      Description.Text = 'Change the background color.'
      UseHintAsHeader = True
    end
    object stStrokeColor: TdxScreenTip
      Header.Text = 'stStrokeColor'
      Description.Text = 'Change the stroke color.'
      UseHintAsHeader = True
    end
    object stBringToFront: TdxScreenTip
      Header.Text = 'stBringToFront'
      Description.Text = 'Bring the selected object in front of all other objects.'
      UseHintAsHeader = True
    end
    object stSendToBack: TdxScreenTip
      Header.Text = 'stSendToBack'
      Description.Text = 'Send the selected object behind all other objects.'
      UseHintAsHeader = True
    end
    object stSnapToGrid: TdxScreenTip
      Header.Text = 'stSnapToGrid'
      Description.Text = 'Position diagram items to the closest intersection of the grid.'
      UseHintAsHeader = True
    end
    object stConnectors: TdxScreenTip
      Header.Text = 'stConnectors'
      Description.Text = 'Change the appearance of connectors in the diagram.'
      UseHintAsHeader = True
    end
    object stGrid: TdxScreenTip
      Header.Text = 'stGrid'
      Description.Text = 
        'The gridlines make it easy for you to align objects with text, o' +
        'ther objects, or a particular spot.'
      UseHintAsHeader = True
    end
    object stUndo: TdxScreenTip
      Header.Text = 'stUndo'
      Description.Text = 'Undo the last operation.'
      UseHintAsHeader = True
    end
    object stRedo: TdxScreenTip
      Header.Text = 'stRedo'
      Description.Text = 'Redo the last operation.'
      UseHintAsHeader = True
    end
    object stNew: TdxScreenTip
      Header.Text = 'stNew'
      Description.Text = 'Create a new diagram'
      UseHintAsHeader = True
    end
    object stOpen: TdxScreenTip
      Header.Text = 'stOpen'
      Description.Text = 'Open a diagram file'
      UseHintAsHeader = True
    end
    object stSave: TdxScreenTip
      Header.Text = 'stSave'
      Description.Text = 'Save this diagram'
      UseHintAsHeader = True
    end
    object stSaveAs: TdxScreenTip
      Header.Text = 'stSaveAs'
      Description.Text = 'Save this diagram with a different name'
      UseHintAsHeader = True
    end
    object stApplyLayeredLayout: TdxScreenTip
      Header.Text = 'stApplyLayeredLayout'
      Description.Text = 'Rearranges all chart elements with the layered layout algorithm'
      UseHintAsHeader = True
    end
  end
  object ppmFontColor: TdxBarPopupMenu
    BarManager = bmManager
    ItemLinks = <>
    UseOwnFont = False
    Left = 648
    Top = 224
    PixelsPerInch = 96
  end
  object ppmBackgroundColor: TdxBarPopupMenu
    BarManager = bmManager
    ItemLinks = <>
    UseOwnFont = False
    Left = 648
    Top = 280
    PixelsPerInch = 96
  end
  object ppmStrokeColor: TdxBarPopupMenu
    BarManager = bmManager
    ItemLinks = <>
    UseOwnFont = False
    Left = 648
    Top = 328
    PixelsPerInch = 96
  end
  object ilBeginArrowTypes: TcxImageList
    SourceDPI = 96
    Height = 20
    Width = 48
    FormatVersion = 1
    DesignInfo = 12583652
  end
  object ilEndArrowTypes: TcxImageList
    SourceDPI = 96
    Height = 20
    Width = 48
    FormatVersion = 1
    DesignInfo = 12845880
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 268
    Top = 34
  end
  object ColorDialog: TdxColorDialog
    Options.ColorPicker.ShowHSLEditors = False
    Options.ColorPicker.DefaultVisible = True
    Left = 236
    Top = 34
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
  object ppmLineColor: TdxBarPopupMenu
    BarManager = bmManager
    ItemLinks = <>
    UseOwnFont = False
    Left = 648
    Top = 176
    PixelsPerInch = 96
  end
  object ppmSourceArrowColor: TdxBarPopupMenu
    BarManager = bmManager
    ItemLinks = <>
    UseOwnFont = False
    Left = 648
    Top = 384
    PixelsPerInch = 96
  end
  object ppmDestArrowColor: TdxBarPopupMenu
    BarManager = bmManager
    ItemLinks = <>
    UseOwnFont = False
    Left = 648
    Top = 432
    PixelsPerInch = 96
  end
  object ppmObjectBkColor: TdxBarPopupMenu
    BarManager = bmManager
    ItemLinks = <>
    UseOwnFont = False
    Left = 648
    Top = 480
    PixelsPerInch = 96
  end
  object ilLineStyles: TcxImageList
    SourceDPI = 96
    Height = 18
    Width = 58
    FormatVersion = 1
    DesignInfo = 33554832
    ImageInfo = <
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D494844520000003A00000012080200000085265A
          240000000467414D410000B18F0BFC6105000000097048597300000EC200000E
          C20115284A800000001874455874536F667477617265007061696E742E6E6574
          20342E312E36FD4E09E80000003449444154484BEDCE810900300CC3B0FEFF74
          06652F181AB02ED0A48A5D925D925D5273774EFAB9D5DCBDCF2EC92EC92EA9AA
          9B3C65FF059626DA755E0000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D494844520000003A00000012080200000085265A
          240000000467414D410000B18F0BFC6105000000097048597300000EC200000E
          C20115284A800000001874455874536F667477617265007061696E742E6E6574
          20342E312E36FD4E09E80000004649444154484BEDCEB10900300C0341EFBFB4
          03E68B144E298840570A155F6D25B94AC9554AAE92736EBDF1184C1B1E8369C3
          63306D785C9C73FF975CA5E42A2557C92AB7FB00C5BB59428231658700000000
          49454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D494844520000003A00000012080200000085265A
          240000000467414D410000B18F0BFC6105000000097048597300000EC200000E
          C20115284A800000001874455874536F667477617265007061696E742E6E6574
          20342E312E36FD4E09E80000003E49444154484BEDCE310A0040080341FFFFE9
          1C48CA6B170C641A519B1D45692EA9B9A4E6929273677959E72E9EEBF3BE76F1
          0CD15C527349CD2545E54A0F4DB6C4D68AC0ED680000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D494844520000003A00000012080200000085265A
          240000000467414D410000B18F0BFC6105000000097048597300000EC200000E
          C20115284A800000001874455874536F667477617265007061696E742E6E6574
          20342E312E36FD4E09E80000005049444154484BEDCE310A00300C42D1DEFFD2
          2904875074141AF08DE2F04FAD925CA7E43A25D76979EED1F0686AA1F0686AA1
          F018DE0947068FA6160A8FA6160A8F814C3F4BAE53729D92EBB42AB7EA02199E
          7724DDBF48450000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D494844520000003A00000012080200000085265A
          240000000467414D410000B18F0BFC6105000000097048597300000EC200000E
          C20115284A800000001874455874536F667477617265007061696E742E6E6574
          20342E312E36FD4E09E80000004E49444154484BEDCE310A00300C42D1DCFFD2
          29A4D2A1A4E02234E01BC5E1478EE25C25E72A395769786EBCE151F8A5854761
          96E35EF7B58547E197161E85598E7EFD9673959CABE45CA551B9990B55369506
          3938EDB10000000049454E44AE426082}
      end>
  end
end
