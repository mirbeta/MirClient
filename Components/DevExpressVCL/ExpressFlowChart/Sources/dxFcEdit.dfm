object FChartEditor: TFChartEditor
  Left = 354
  Top = 105
  Caption = 'dxFlowChart Editor'
  ClientHeight = 534
  ClientWidth = 677
  Color = clBtnFace
  Constraints.MinHeight = 350
  Constraints.MinWidth = 350
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object bvlSpace1: TBevel
    Left = 0
    Top = 10
    Width = 10
    Height = 474
    Align = alLeft
    Shape = bsSpacer
  end
  object bvlSpace2: TBevel
    Left = 667
    Top = 10
    Width = 10
    Height = 474
    Align = alRight
    Shape = bsSpacer
  end
  object bvlSpace3: TBevel
    Left = 0
    Top = 0
    Width = 677
    Height = 10
    Align = alTop
    Shape = bsSpacer
  end
  object bvlSpace4: TBevel
    Left = 55
    Top = 10
    Width = 8
    Height = 474
    Align = alLeft
    Shape = bsSpacer
  end
  object Panel1: TPanel
    Left = 10
    Top = 10
    Width = 45
    Height = 474
    Align = alLeft
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 0
    object Bevel1: TBevel
      Left = 0
      Top = 37
      Width = 45
      Height = 2
      Align = alTop
      Shape = bsTopLine
    end
    object Bevel2: TBevel
      Left = 0
      Top = 121
      Width = 45
      Height = 2
      Align = alTop
      Shape = bsTopLine
    end
    object Bevel4: TBevel
      Left = 0
      Top = 225
      Width = 45
      Height = 2
      Align = alTop
      Shape = bsTopLine
    end
    object Bevel3: TBevel
      Left = 0
      Top = 472
      Width = 45
      Height = 2
      Align = alBottom
      Shape = bsBottomLine
    end
    object Panel4: TPanel
      Left = 0
      Top = 0
      Width = 45
      Height = 37
      Align = alTop
      BevelOuter = bvNone
      FullRepaint = False
      TabOrder = 0
      object Label1: TLabel
        Left = 1
        Top = 2
        Width = 33
        Height = 13
        Caption = 'Create'
        Transparent = True
      end
      object btnCreateConnect: TcxButton
        Left = 22
        Top = 16
        Width = 19
        Height = 19
        Hint = 'Connection'
        LookAndFeel.NativeStyle = True
        OptionsImage.ImageIndex = 76
        OptionsImage.Images = ilSmallImages
        ParentShowHint = False
        ShowHint = True
        SpeedButtonOptions.GroupIndex = 1
        SpeedButtonOptions.CanBeFocused = False
        SpeedButtonOptions.AllowAllUp = True
        SpeedButtonOptions.Flat = True
        SpeedButtonOptions.Transparent = True
        TabOrder = 0
        OnClick = btnCreateConnectClick
      end
      object btnCreateObject: TcxButton
        Left = 2
        Top = 16
        Width = 19
        Height = 19
        Hint = 'Object'
        LookAndFeel.NativeStyle = True
        OptionsImage.ImageIndex = 0
        OptionsImage.Images = ilSmallImages
        ParentShowHint = False
        ShowHint = True
        SpeedButtonOptions.GroupIndex = 1
        SpeedButtonOptions.CanBeFocused = False
        SpeedButtonOptions.AllowAllUp = True
        SpeedButtonOptions.Flat = True
        SpeedButtonOptions.Transparent = True
        TabOrder = 1
        OnClick = btnCreateObjectClick
      end
    end
    object pObject: TPanel
      Left = 0
      Top = 39
      Width = 45
      Height = 82
      Align = alTop
      BevelOuter = bvNone
      FullRepaint = False
      TabOrder = 1
      object Label2: TLabel
        Left = 1
        Top = 4
        Width = 32
        Height = 13
        Caption = 'Object'
        Transparent = True
      end
      object sbShape: TcxButton
        Tag = 1
        Left = 2
        Top = 20
        Width = 19
        Height = 19
        Hint = 'Shape Style'
        LookAndFeel.NativeStyle = True
        OptionsImage.ImageIndex = 0
        OptionsImage.Images = ilSmallImages
        ParentShowHint = False
        ShowHint = True
        SpeedButtonOptions.CanBeFocused = False
        SpeedButtonOptions.Flat = True
        SpeedButtonOptions.Transparent = True
        TabOrder = 0
        OnClick = sbShapeClick
      end
      object sbLine: TcxButton
        Tag = 1
        Left = 22
        Top = 20
        Width = 19
        Height = 19
        Hint = 'Shape Line Width'
        LookAndFeel.NativeStyle = True
        OptionsImage.ImageIndex = 9
        OptionsImage.Images = ilSmallImages
        ParentShowHint = False
        ShowHint = True
        SpeedButtonOptions.CanBeFocused = False
        SpeedButtonOptions.Flat = True
        SpeedButtonOptions.Transparent = True
        TabOrder = 1
        OnClick = sbShapeClick
      end
      object sbTextPosition: TcxButton
        Tag = 4
        Left = 2
        Top = 40
        Width = 19
        Height = 19
        Hint = 'Text Layout'
        LookAndFeel.NativeStyle = True
        OptionsImage.ImageIndex = 61
        OptionsImage.Images = ilSmallImages
        ParentShowHint = False
        ShowHint = True
        SpeedButtonOptions.CanBeFocused = False
        SpeedButtonOptions.Flat = True
        SpeedButtonOptions.Transparent = True
        TabOrder = 2
        OnClick = sbShapeClick
      end
      object sbObjectFont: TcxButton
        Left = 22
        Top = 40
        Width = 19
        Height = 19
        Hint = 'Text Font'
        LookAndFeel.NativeStyle = True
        OptionsImage.ImageIndex = 77
        OptionsImage.Images = ilSmallImages
        ParentShowHint = False
        ShowHint = True
        SpeedButtonOptions.CanBeFocused = False
        SpeedButtonOptions.Flat = True
        SpeedButtonOptions.Transparent = True
        TabOrder = 3
        OnClick = sbObjectFontClick
      end
      object sbImagePosition: TcxButton
        Left = 2
        Top = 60
        Width = 19
        Height = 19
        Hint = 'Image Layout'
        LookAndFeel.NativeStyle = True
        OptionsImage.ImageIndex = 66
        OptionsImage.Images = ilSmallImages
        ParentShowHint = False
        ShowHint = True
        SpeedButtonOptions.CanBeFocused = False
        SpeedButtonOptions.Flat = True
        SpeedButtonOptions.Transparent = True
        TabOrder = 4
        OnClick = sbShapeClick
      end
    end
    object pConnect: TPanel
      Left = 0
      Top = 123
      Width = 45
      Height = 102
      Align = alTop
      BevelOuter = bvNone
      FullRepaint = False
      TabOrder = 2
      object Label5: TLabel
        Left = 1
        Top = 4
        Width = 40
        Height = 13
        Caption = 'Connect'
        Transparent = True
      end
      object sbStyle: TcxButton
        Tag = 1
        Left = 2
        Top = 20
        Width = 19
        Height = 19
        Hint = 'Line Style'
        LookAndFeel.NativeStyle = True
        OptionsImage.ImageIndex = 19
        OptionsImage.Images = ilSmallImages
        ParentShowHint = False
        ShowHint = True
        SpeedButtonOptions.CanBeFocused = False
        SpeedButtonOptions.Flat = True
        SpeedButtonOptions.Transparent = True
        TabOrder = 0
        OnClick = sbShapeClick
      end
      object sbSourceArrow: TcxButton
        Left = 2
        Top = 40
        Width = 19
        Height = 19
        Hint = 'Source Arrow'
        LookAndFeel.NativeStyle = True
        OptionsImage.ImageIndex = 75
        OptionsImage.Images = ilSmallImages
        ParentShowHint = False
        ShowHint = True
        SpeedButtonOptions.CanBeFocused = False
        SpeedButtonOptions.Flat = True
        SpeedButtonOptions.Transparent = True
        TabOrder = 1
        OnClick = sbShapeClick
      end
      object sbDestArrow: TcxButton
        Left = 22
        Top = 40
        Width = 19
        Height = 19
        Hint = 'Destination Arrow'
        LookAndFeel.NativeStyle = True
        OptionsImage.ImageIndex = 75
        OptionsImage.Images = ilSmallImages
        ParentShowHint = False
        ShowHint = True
        SpeedButtonOptions.CanBeFocused = False
        SpeedButtonOptions.Flat = True
        SpeedButtonOptions.Transparent = True
        TabOrder = 2
        OnClick = sbShapeClick
      end
      object sbSArrowSize: TcxButton
        Tag = 1
        Left = 2
        Top = 60
        Width = 19
        Height = 19
        Hint = 'Source Arrow Size'
        LookAndFeel.NativeStyle = True
        OptionsImage.ImageIndex = 31
        OptionsImage.Images = ilSmallImages
        ParentShowHint = False
        ShowHint = True
        SpeedButtonOptions.CanBeFocused = False
        SpeedButtonOptions.Flat = True
        SpeedButtonOptions.Transparent = True
        TabOrder = 3
        OnClick = sbShapeClick
      end
      object sbDArrowSize: TcxButton
        Tag = 1
        Left = 22
        Top = 60
        Width = 19
        Height = 19
        Hint = 'Destination Arrow Size'
        LookAndFeel.NativeStyle = True
        OptionsImage.ImageIndex = 36
        OptionsImage.Images = ilSmallImages
        ParentShowHint = False
        ShowHint = True
        SpeedButtonOptions.CanBeFocused = False
        SpeedButtonOptions.Flat = True
        SpeedButtonOptions.Transparent = True
        TabOrder = 4
        OnClick = sbShapeClick
      end
      object sbSPoint: TcxButton
        Left = 2
        Top = 80
        Width = 19
        Height = 19
        Hint = 'Linked point of Source object'
        LookAndFeel.NativeStyle = True
        OptionsImage.ImageIndex = 41
        OptionsImage.Images = ilSmallImages
        ParentShowHint = False
        ShowHint = True
        SpeedButtonOptions.CanBeFocused = False
        SpeedButtonOptions.Flat = True
        SpeedButtonOptions.Transparent = True
        TabOrder = 5
        OnClick = sbShapeClick
      end
      object sbDPoint: TcxButton
        Left = 22
        Top = 80
        Width = 19
        Height = 19
        Hint = 'Linked point of Destination object'
        LookAndFeel.NativeStyle = True
        OptionsImage.ImageIndex = 41
        OptionsImage.Images = ilSmallImages
        ParentShowHint = False
        ShowHint = True
        SpeedButtonOptions.CanBeFocused = False
        SpeedButtonOptions.Flat = True
        SpeedButtonOptions.Transparent = True
        TabOrder = 6
        OnClick = sbShapeClick
      end
      object sbConnectFont: TcxButton
        Left = 22
        Top = 20
        Width = 19
        Height = 19
        Hint = 'Text Font'
        LookAndFeel.NativeStyle = True
        OptionsImage.ImageIndex = 77
        OptionsImage.Images = ilSmallImages
        ParentShowHint = False
        ShowHint = True
        SpeedButtonOptions.CanBeFocused = False
        SpeedButtonOptions.Flat = True
        SpeedButtonOptions.Transparent = True
        TabOrder = 7
        OnClick = sbConnectFontClick
      end
    end
    object Panel6: TPanel
      Left = 0
      Top = 227
      Width = 45
      Height = 30
      Align = alTop
      BevelOuter = bvNone
      FullRepaint = False
      TabOrder = 3
      object sbZoom: TcxButton
        Left = 2
        Top = 4
        Width = 19
        Height = 19
        Hint = 'Zoom'
        LookAndFeel.NativeStyle = True
        OptionsImage.ImageIndex = 78
        OptionsImage.Images = ilSmallImages
        ParentShowHint = False
        ShowHint = True
        SpeedButtonOptions.GroupIndex = 2
        SpeedButtonOptions.CanBeFocused = False
        SpeedButtonOptions.AllowAllUp = True
        SpeedButtonOptions.Flat = True
        SpeedButtonOptions.Transparent = True
        TabOrder = 0
        OnClick = sbZoomClick
      end
      object sbFit: TcxButton
        Left = 22
        Top = 4
        Width = 19
        Height = 19
        Hint = 'Fit'
        LookAndFeel.NativeStyle = True
        OptionsImage.ImageIndex = 79
        OptionsImage.Images = ilSmallImages
        ParentShowHint = False
        ShowHint = True
        SpeedButtonOptions.GroupIndex = 3
        SpeedButtonOptions.CanBeFocused = False
        SpeedButtonOptions.AllowAllUp = True
        SpeedButtonOptions.Flat = True
        SpeedButtonOptions.Transparent = True
        TabOrder = 1
        OnClick = sbFitClick
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 484
    Width = 677
    Height = 50
    Align = alBottom
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 1
    object plPalette: TPanel
      Left = 15
      Top = 6
      Width = 183
      Height = 33
      BevelOuter = bvNone
      TabOrder = 2
      object pWhite: TPanel
        Left = 48
        Top = 17
        Width = 16
        Height = 16
        BevelOuter = bvLowered
        Color = clWhite
        TabOrder = 0
        OnMouseDown = pBlackMouseDown
      end
      object pBlack: TPanel
        Left = 48
        Top = 0
        Width = 16
        Height = 16
        BevelOuter = bvLowered
        Color = clBlack
        TabOrder = 1
        OnMouseDown = pBlackMouseDown
      end
      object pGray: TPanel
        Left = 65
        Top = 0
        Width = 16
        Height = 16
        BevelOuter = bvLowered
        Color = clGray
        TabOrder = 2
        OnMouseDown = pBlackMouseDown
      end
      object Panel5: TPanel
        Left = 65
        Top = 17
        Width = 16
        Height = 16
        BevelOuter = bvLowered
        Color = clSilver
        TabOrder = 3
        OnMouseDown = pBlackMouseDown
      end
      object pRed: TPanel
        Left = 82
        Top = 17
        Width = 16
        Height = 16
        BevelOuter = bvLowered
        Color = clRed
        TabOrder = 4
        OnMouseDown = pBlackMouseDown
      end
      object pMarron: TPanel
        Left = 82
        Top = 0
        Width = 16
        Height = 16
        BevelOuter = bvLowered
        Color = clMaroon
        TabOrder = 5
        OnMouseDown = pBlackMouseDown
      end
      object pOlive: TPanel
        Left = 99
        Top = 0
        Width = 16
        Height = 16
        BevelOuter = bvLowered
        Color = clOlive
        TabOrder = 6
        OnMouseDown = pBlackMouseDown
      end
      object pYellow: TPanel
        Left = 99
        Top = 17
        Width = 16
        Height = 16
        BevelOuter = bvLowered
        Color = clYellow
        TabOrder = 7
        OnMouseDown = pBlackMouseDown
      end
      object pLime: TPanel
        Left = 116
        Top = 17
        Width = 16
        Height = 16
        BevelOuter = bvLowered
        Color = clLime
        TabOrder = 8
        OnMouseDown = pBlackMouseDown
      end
      object pGreen: TPanel
        Left = 116
        Top = 0
        Width = 16
        Height = 16
        BevelOuter = bvLowered
        Color = clGreen
        TabOrder = 9
        OnMouseDown = pBlackMouseDown
      end
      object pAqua: TPanel
        Left = 133
        Top = 17
        Width = 16
        Height = 16
        BevelOuter = bvLowered
        Color = clAqua
        TabOrder = 10
        OnMouseDown = pBlackMouseDown
      end
      object pTeal: TPanel
        Left = 133
        Top = 0
        Width = 16
        Height = 16
        BevelOuter = bvLowered
        Color = clTeal
        TabOrder = 11
        OnMouseDown = pBlackMouseDown
      end
      object pNavy: TPanel
        Left = 150
        Top = 0
        Width = 16
        Height = 16
        BevelOuter = bvLowered
        Color = clNavy
        TabOrder = 12
        OnMouseDown = pBlackMouseDown
      end
      object pBlue: TPanel
        Left = 150
        Top = 17
        Width = 16
        Height = 16
        BevelOuter = bvLowered
        Color = clBlue
        TabOrder = 13
        OnMouseDown = pBlackMouseDown
      end
      object pFuchsia: TPanel
        Left = 167
        Top = 17
        Width = 16
        Height = 16
        BevelOuter = bvLowered
        Color = clFuchsia
        TabOrder = 14
        OnMouseDown = pBlackMouseDown
      end
      object pPurple: TPanel
        Left = 167
        Top = 0
        Width = 16
        Height = 16
        BevelOuter = bvLowered
        Color = clPurple
        TabOrder = 15
        OnMouseDown = pBlackMouseDown
      end
      object pBk: TPanel
        Left = 1
        Top = 0
        Width = 33
        Height = 33
        BevelOuter = bvLowered
        ParentColor = True
        TabOrder = 16
        object pBkColor: TPanel
          Tag = 2
          Left = 13
          Top = 13
          Width = 16
          Height = 16
          BevelOuter = bvLowered
          Color = clWhite
          TabOrder = 1
          OnClick = pColorClick
          OnDblClick = pColorDblClick
        end
        object pColor: TPanel
          Tag = 1
          Left = 4
          Top = 4
          Width = 16
          Height = 16
          BevelOuter = bvLowered
          Color = clBlack
          TabOrder = 0
          OnClick = pColorClick
          OnDblClick = pColorDblClick
        end
      end
    end
    object btnOK: TcxButton
      Left = 511
      Top = 16
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TcxButton
      Left = 592
      Top = 16
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object Chart: TdxFlowChart
    Left = 63
    Top = 10
    Width = 604
    Height = 474
    Align = alClient
    OnChange = ChartChange
    OnSelected = ChartSelected
    OnSelection = ChartSelection
    Options = [fcoCanDelete, fcoCanDrag, fcoCanSelect, fcoMultiSelect, fcoHideSelection, fcoDelOnClick]
    OnDblClick = ChartDblClick
    OnKeyDown = ChartKeyDown
    OnKeyUp = ChartKeyUp
    OnMouseDown = ChartMouseDown
    OnMouseMove = ChartMouseMove
    OnMouseUp = ChartMouseUp
  end
  object MainMenu: TMainMenu
    Images = ilLargeImages
    Left = 64
    Top = 16
    object miFile: TMenuItem
      Caption = '&File'
      object miOpen: TMenuItem
        Caption = '&Open'
        OnClick = miOpenClick
      end
      object miSaveAs: TMenuItem
        Caption = 'Save &As ...'
        OnClick = miSaveAsClick
      end
    end
    object miEdit: TMenuItem
      Caption = '&Edit'
      object miUndo: TMenuItem
        Caption = '&Undo'
        Enabled = False
        ShortCut = 16474
        OnClick = miUndoClick
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object miCut: TMenuItem
        Caption = 'Cu&t'
        Enabled = False
        ShortCut = 16472
        OnClick = iCutClick
      end
      object miCopy: TMenuItem
        Caption = '&Copy'
        Enabled = False
        ShortCut = 16451
        OnClick = iCopyClick
      end
      object miPaste: TMenuItem
        Caption = '&Paste'
        Enabled = False
        ShortCut = 16470
        OnClick = iPasteClick
      end
      object miDelete: TMenuItem
        Caption = '&Delete'
        Enabled = False
        ShortCut = 46
        OnClick = iDeleteClick
      end
      object N6: TMenuItem
        Caption = '-'
        Enabled = False
      end
      object miSelectAll: TMenuItem
        Caption = 'Se&lect All'
        ShortCut = 16449
        OnClick = iSelectAllClick
      end
      object miClearSelection: TMenuItem
        Caption = 'Cl&ear Selection'
        Enabled = False
        OnClick = iClearSelectionClick
      end
      object N7: TMenuItem
        Caption = '-'
        Enabled = False
      end
      object miBringToFront: TMenuItem
        Caption = 'Bring To &Front'
        Enabled = False
        OnClick = iBringToFrontClick
      end
      object miSendToBack: TMenuItem
        Caption = 'Send To &Back'
        Enabled = False
        OnClick = iSendToBackClick
      end
    end
    object View1: TMenuItem
      Caption = '&View'
      object miAntialiasing: TMenuItem
        AutoCheck = True
        Caption = '&Antialiasing'
        OnClick = miAntialiasingClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object miZoomIn: TMenuItem
        Caption = 'Zoom &In'
        ShortCut = 16457
        OnClick = miZoomInClick
      end
      object miZoomOut: TMenuItem
        Caption = 'Zoom &Out'
        ShortCut = 16469
        OnClick = miZoomOutClick
      end
      object miFit: TMenuItem
        Caption = '&Fit'
        ShortCut = 16454
        OnClick = miFitClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object miActualSize: TMenuItem
        Caption = '&Actual Size'
        ShortCut = 16465
        OnClick = miActualSizeClick
      end
    end
    object miUnions: TMenuItem
      Caption = '&Unions'
      object miNewUnion: TMenuItem
        Caption = 'New Union'
        Enabled = False
        OnClick = iNewUnionClick
      end
      object miAddToUnion: TMenuItem
        Caption = 'Add To Union'
        Enabled = False
        OnClick = iAddToUnionClick
      end
      object miRemoveFromUnion: TMenuItem
        Caption = 'Remove From Union'
        Enabled = False
        OnClick = iRemoveFromUnionClick
      end
      object miClearUnion: TMenuItem
        Caption = 'Clear Union'
        Enabled = False
        OnClick = iClearUnionClick
      end
      object miClearAllUnions: TMenuItem
        Caption = 'Clear All Unions'
        OnClick = iClearAllUnionsClick
      end
    end
    object miOptions: TMenuItem
      Caption = '&Options'
      object miDynamicMoving: TMenuItem
        AutoCheck = True
        Caption = 'Dynamic &Moving'
        OnClick = miDynamicMovingClick
      end
      object miDynamicSizing: TMenuItem
        AutoCheck = True
        Caption = 'Dynamic &Sizing'
        OnClick = miDynamicSizingClick
      end
    end
    object miHelp: TMenuItem
      Caption = '&Help'
      object Contents1: TMenuItem
        Caption = '&Contents'
        ShortCut = 112
        OnClick = Contents1Click
      end
    end
  end
  object ShapePopupMenu: TPopupMenu
    Images = ilSmallImages
    Left = 96
    Top = 17
    object iNone: TMenuItem
      Caption = 'None'
      OnClick = iRectangleClick
    end
    object iRectangle: TMenuItem
      Tag = 1
      Caption = 'Rectangle'
      OnClick = iRectangleClick
    end
    object iEllipse: TMenuItem
      Tag = 2
      Caption = 'Ellipse'
      OnClick = iRectangleClick
    end
    object iRoundRect: TMenuItem
      Tag = 3
      Caption = 'Round Rect'
      OnClick = iRectangleClick
    end
    object iDiamond: TMenuItem
      Tag = 4
      Caption = 'Diamond'
      OnClick = iRectangleClick
    end
    object iNorthTriangle: TMenuItem
      Tag = 5
      Caption = 'North Triangle'
      OnClick = iRectangleClick
    end
    object itSouthTriangle: TMenuItem
      Tag = 6
      Caption = 'South Triangle'
      OnClick = iRectangleClick
    end
    object itEastTriangle: TMenuItem
      Tag = 7
      Caption = 'East Triangle'
      OnClick = iRectangleClick
    end
    object itWestTriangle: TMenuItem
      Tag = 8
      Caption = 'West Triangle'
      OnClick = iRectangleClick
    end
    object itHexagon: TMenuItem
      Tag = 9
      Caption = 'Hexagon'
      OnClick = iRectangleClick
    end
  end
  object LinePopupMenu: TPopupMenu
    Images = ilSmallImages
    Left = 128
    Top = 17
    object i1p: TMenuItem
      Tag = 1
      Caption = '1 pixel'
      OnClick = iRectangleClick
    end
    object i2p: TMenuItem
      Tag = 2
      Caption = '2 pixels'
      OnClick = iRectangleClick
    end
    object i3p: TMenuItem
      Tag = 3
      Caption = '3 pixels'
      OnClick = iRectangleClick
    end
    object i4p: TMenuItem
      Tag = 4
      Caption = '4 pixels'
      OnClick = iRectangleClick
    end
    object i5p: TMenuItem
      Tag = 5
      Caption = '5 pixels'
      OnClick = iRectangleClick
    end
    object i6p: TMenuItem
      Tag = 6
      Caption = '6 pixels'
      OnClick = iRectangleClick
    end
    object i7p: TMenuItem
      Tag = 7
      Caption = '7 pixels'
      OnClick = iRectangleClick
    end
    object i8p: TMenuItem
      Tag = 8
      Caption = '8 pixels'
      OnClick = iRectangleClick
    end
    object i9p: TMenuItem
      Tag = 9
      Caption = '9 pixels'
      OnClick = iRectangleClick
    end
    object i10p: TMenuItem
      Tag = 10
      Caption = '10 pixels'
      OnClick = iRectangleClick
    end
  end
  object StylePopupMenu: TPopupMenu
    Images = ilSmallImages
    Left = 160
    Top = 17
    object iStraight: TMenuItem
      Tag = 1
      Caption = 'Straight'
      OnClick = iRectangleClick
    end
    object iCurved: TMenuItem
      Tag = 2
      Caption = 'Curved'
      OnClick = iRectangleClick
    end
    object iRectHorizontal: TMenuItem
      Tag = 3
      Caption = 'Rect Horizontal'
      OnClick = iRectangleClick
    end
    object iRectVertical: TMenuItem
      Tag = 4
      Caption = 'Rect Vertical'
      OnClick = iRectangleClick
    end
  end
  object SourceArrowPopupMenu: TPopupMenu
    Images = ilSmallImages
    Left = 192
    Top = 17
    object iSNone: TMenuItem
      Caption = 'None'
      OnClick = iRectangleClick
    end
    object iSArrow: TMenuItem
      Tag = 1
      Caption = 'Arrow'
      OnClick = iRectangleClick
    end
    object iSOvalArrow: TMenuItem
      Tag = 2
      Caption = 'Ellipse Arrow'
      OnClick = iRectangleClick
    end
    object iSRectArrow: TMenuItem
      Tag = 3
      Caption = 'Rect Arrow'
      OnClick = iRectangleClick
    end
  end
  object DestArrowPopupMenu: TPopupMenu
    Images = ilSmallImages
    Left = 224
    Top = 17
    object iDNone: TMenuItem
      Caption = 'None'
      OnClick = iRectangleClick
    end
    object iDArrow: TMenuItem
      Tag = 1
      Caption = 'Arrow'
      OnClick = iRectangleClick
    end
    object iDOvalArrow: TMenuItem
      Tag = 2
      Caption = 'Ellipse Arrow'
      OnClick = iRectangleClick
    end
    object iDRectArrow: TMenuItem
      Tag = 3
      Caption = 'Rect Arrow'
      OnClick = iRectangleClick
    end
  end
  object SArrowSizePopupMenu: TPopupMenu
    Images = ilSmallImages
    Left = 256
    Top = 17
    object iS10: TMenuItem
      Tag = 1
      Caption = '10 x 10'
      OnClick = iRectangleClick
    end
    object iS20: TMenuItem
      Tag = 2
      Caption = '15 x 15'
      OnClick = iRectangleClick
    end
    object iS30: TMenuItem
      Tag = 3
      Caption = '20 x 20'
      OnClick = iRectangleClick
    end
    object iS40: TMenuItem
      Tag = 4
      Caption = '25 x 25'
      OnClick = iRectangleClick
    end
    object iS50: TMenuItem
      Tag = 5
      Caption = '30 x 30'
      OnClick = iRectangleClick
    end
  end
  object DArrowSizePopupMenu: TPopupMenu
    Images = ilSmallImages
    Left = 288
    Top = 17
    object iD10: TMenuItem
      Tag = 1
      Caption = '10 x 10'
      OnClick = iRectangleClick
    end
    object iD20: TMenuItem
      Tag = 2
      Caption = '15 x 15'
      OnClick = iRectangleClick
    end
    object iD30: TMenuItem
      Tag = 3
      Caption = '20 x 20'
      OnClick = iRectangleClick
    end
    object iD40: TMenuItem
      Tag = 4
      Caption = '25 x 25'
      OnClick = iRectangleClick
    end
    object iD50: TMenuItem
      Tag = 5
      Caption = '30 x 30'
      OnClick = iRectangleClick
    end
  end
  object SPointPopupMenu: TPopupMenu
    Images = ilSmallImages
    Left = 320
    Top = 17
    object iSP1: TMenuItem
      Caption = 'Point 1'
      OnClick = iRectangleClick
    end
    object iSP2: TMenuItem
      Tag = 1
      Caption = 'Point 2'
      OnClick = iRectangleClick
    end
    object iSP3: TMenuItem
      Tag = 2
      Caption = 'Point 3'
      OnClick = iRectangleClick
    end
    object iSP4: TMenuItem
      Tag = 3
      Caption = 'Point 4'
      OnClick = iRectangleClick
    end
    object iSP5: TMenuItem
      Tag = 4
      Caption = 'Point 5'
      OnClick = iRectangleClick
    end
    object iSP6: TMenuItem
      Tag = 5
      Caption = 'Point 6'
      OnClick = iRectangleClick
    end
    object iSP7: TMenuItem
      Tag = 6
      Caption = 'Point 7'
      OnClick = iRectangleClick
    end
    object iSP8: TMenuItem
      Tag = 7
      Caption = 'Point 8'
      OnClick = iRectangleClick
    end
    object iSP9: TMenuItem
      Tag = 8
      Caption = 'Point 9'
      OnClick = iRectangleClick
    end
    object iSP10: TMenuItem
      Tag = 9
      Caption = 'Point 10'
      OnClick = iRectangleClick
    end
    object iSP11: TMenuItem
      Tag = 10
      Caption = 'Point 11'
      OnClick = iRectangleClick
    end
    object iSP12: TMenuItem
      Tag = 11
      Caption = 'Point 12'
      OnClick = iRectangleClick
    end
    object iSP13: TMenuItem
      Tag = 12
      Caption = 'Point 13'
      OnClick = iRectangleClick
    end
    object iSP14: TMenuItem
      Tag = 13
      Caption = 'Point 14'
      OnClick = iRectangleClick
    end
    object iSP15: TMenuItem
      Tag = 14
      Caption = 'Point 15'
      OnClick = iRectangleClick
    end
    object iSP16: TMenuItem
      Tag = 15
      Caption = 'Point 16'
      OnClick = iRectangleClick
    end
  end
  object DPointPopupMenu: TPopupMenu
    Images = ilSmallImages
    Left = 352
    Top = 17
    object iDP1: TMenuItem
      Caption = 'Point 1'
      OnClick = iRectangleClick
    end
    object iDP2: TMenuItem
      Tag = 1
      Caption = 'Point 2'
      OnClick = iRectangleClick
    end
    object iDP3: TMenuItem
      Tag = 2
      Caption = 'Point 3'
      OnClick = iRectangleClick
    end
    object iDP4: TMenuItem
      Tag = 3
      Caption = 'Point 4'
      OnClick = iRectangleClick
    end
    object iDP5: TMenuItem
      Tag = 4
      Caption = 'Point 5'
      OnClick = iRectangleClick
    end
    object iDP6: TMenuItem
      Tag = 5
      Caption = 'Point 6'
      OnClick = iRectangleClick
    end
    object iDP7: TMenuItem
      Tag = 6
      Caption = 'Point 7'
      OnClick = iRectangleClick
    end
    object iDP8: TMenuItem
      Tag = 7
      Caption = 'Point 8'
      OnClick = iRectangleClick
    end
    object iDP9: TMenuItem
      Tag = 8
      Caption = 'Point 9'
      OnClick = iRectangleClick
    end
    object iDP10: TMenuItem
      Tag = 9
      Caption = 'Point 10'
      OnClick = iRectangleClick
    end
    object iDP11: TMenuItem
      Tag = 10
      Caption = 'Point 11'
      OnClick = iRectangleClick
    end
    object iDP12: TMenuItem
      Tag = 11
      Caption = 'Point 12'
      OnClick = iRectangleClick
    end
    object iDP13: TMenuItem
      Tag = 12
      Caption = 'Point 13'
      OnClick = iRectangleClick
    end
    object iDP14: TMenuItem
      Tag = 13
      Caption = 'Point 14'
      OnClick = iRectangleClick
    end
    object iDP15: TMenuItem
      Tag = 14
      Caption = 'Point 15'
      OnClick = iRectangleClick
    end
    object iDP16: TMenuItem
      Tag = 15
      Caption = 'Point 16'
      OnClick = iRectangleClick
    end
  end
  object TextPositionPopupMenu: TPopupMenu
    Images = ilSmallImages
    Left = 386
    Top = 17
    object iTextTopLeft: TMenuItem
      Caption = 'Top-Left'
      OnClick = iRectangleClick
    end
    object iTextTop: TMenuItem
      Tag = 1
      Caption = 'Top'
      OnClick = iRectangleClick
    end
    object iTextTopRight: TMenuItem
      Tag = 2
      Caption = 'Top-Right'
      OnClick = iRectangleClick
    end
    object iTextLeft: TMenuItem
      Tag = 3
      Caption = 'Left'
      OnClick = iRectangleClick
    end
    object iTextCenter: TMenuItem
      Tag = 4
      Caption = 'Center'
      OnClick = iRectangleClick
    end
    object iTextRight: TMenuItem
      Tag = 5
      Caption = 'Right'
      OnClick = iRectangleClick
    end
    object iTextBottomLeft: TMenuItem
      Tag = 6
      Caption = 'Bottom-Left'
      OnClick = iRectangleClick
    end
    object iTextBottom: TMenuItem
      Tag = 7
      Caption = 'Bottom'
      OnClick = iRectangleClick
    end
    object iTextBottomRight: TMenuItem
      Tag = 8
      Caption = 'Bottom-Right'
      OnClick = iRectangleClick
    end
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 142
    Top = 208
  end
  object ImagePositionPopupMenu: TPopupMenu
    Images = ilSmallImages
    Left = 416
    Top = 17
    object iImageTopLeft: TMenuItem
      Caption = 'Top-Left'
      OnClick = iRectangleClick
    end
    object iImageTop: TMenuItem
      Tag = 1
      Caption = 'Top'
      OnClick = iRectangleClick
    end
    object iImageTopRight: TMenuItem
      Tag = 2
      Caption = 'Top-Right'
      OnClick = iRectangleClick
    end
    object iImageLeft: TMenuItem
      Tag = 3
      Caption = 'Left'
      OnClick = iRectangleClick
    end
    object iImageCenter: TMenuItem
      Tag = 4
      Caption = 'Center'
      OnClick = iRectangleClick
    end
    object iImageRight: TMenuItem
      Tag = 5
      Caption = 'Right'
      OnClick = iRectangleClick
    end
    object iImageBottomLeft: TMenuItem
      Tag = 6
      Caption = 'Bottom-Left'
      OnClick = iRectangleClick
    end
    object iImageBottom: TMenuItem
      Tag = 7
      Caption = 'Bottom'
      OnClick = iRectangleClick
    end
    object iImageBottomRight: TMenuItem
      Tag = 8
      Caption = 'Bottom-Right'
      OnClick = iRectangleClick
    end
  end
  object ChartPopupMenu: TPopupMenu
    Images = ilLargeImages
    OnPopup = ChartPopupMenuPopup
    Left = 448
    Top = 17
    object iEdit: TMenuItem
      Caption = '&Properties'
      OnClick = iEditClick
    end
    object N10: TMenuItem
      Caption = '-'
    end
    object iCut: TMenuItem
      Caption = 'Cu&t'
      Enabled = False
      ShortCut = 16472
      OnClick = iCutClick
    end
    object iCopy: TMenuItem
      Caption = '&Copy'
      Enabled = False
      ShortCut = 16451
      OnClick = iCopyClick
    end
    object iPaste: TMenuItem
      Caption = '&Paste'
      Enabled = False
      ShortCut = 16470
      OnClick = iPasteClick
    end
    object iDelete: TMenuItem
      Caption = '&Delete'
      Enabled = False
      ShortCut = 46
      OnClick = iDeleteClick
    end
    object iRemovePoint: TMenuItem
      Caption = '&Remove Point'
      Enabled = False
      OnClick = iRemovePointClick
    end
    object N8: TMenuItem
      Caption = '-'
    end
    object iSelectAll: TMenuItem
      Caption = 'Se&lect All'
      ShortCut = 16449
      OnClick = iSelectAllClick
    end
    object iClearSelection: TMenuItem
      Caption = 'Cl&ear Selection'
      Enabled = False
      OnClick = iClearSelectionClick
    end
    object N9: TMenuItem
      Caption = '-'
    end
    object iBringToFront: TMenuItem
      Caption = 'Bring To &Front'
      Enabled = False
      OnClick = iBringToFrontClick
    end
    object iSendToBack: TMenuItem
      Caption = 'Send To &Back'
      Enabled = False
      OnClick = iSendToBackClick
    end
    object N12: TMenuItem
      Caption = '-'
    end
    object iNewUnion: TMenuItem
      Caption = 'New Union'
      Enabled = False
      OnClick = iNewUnionClick
    end
    object iAddToUnion: TMenuItem
      Caption = 'Add To Union'
      Enabled = False
      OnClick = iAddToUnionClick
    end
    object iRemoveFromUnion: TMenuItem
      Caption = 'Remove From Union'
      Enabled = False
      OnClick = iRemoveFromUnionClick
    end
    object iClearUnion: TMenuItem
      Caption = 'Clear Union'
      Enabled = False
      OnClick = iClearUnionClick
    end
    object iClearAllUnions: TMenuItem
      Caption = 'Clear All Unions'
      Enabled = False
      OnClick = iClearAllUnionsClick
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
  object ilLargeImages: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 3670088
    ImageInfo = <
      item
        ImageClass = 'TBitmap'
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00000000000000000000000000000000000000000000000000FFFF
          FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF000000000000000000000000000000000000000000FFFF
          FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000000000000000FFFF
          FF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF0000000000000000000000000000000000000000000000000000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF0000000000000000000000000000000000000000000000000000000000FFFF
          FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          7E000000424D7E000000000000003E0000002800000010000000100000000100
          010000000000400000000000000000000000020000000000000000000000FFFF
          FF00FFFF0000FFFF0000001F0000000F00000007000000030000000100000000
          0000001F0000001F0000001F00008FF10000FFF90000FF750000FF8F0000FFFF
          0000}
      end
      item
        ImageClass = 'TBitmap'
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
          0000FFFFFF00FFFFFF0000000000FFFFFF000000000000000000000000000000
          0000FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
          0000FFFFFF00FFFFFF0000000000FFFFFF000000000000000000000000000000
          0000FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
          0000FFFFFF00FFFFFF0000000000FFFFFF000000000000000000000000000000
          0000FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
          0000000000000000000000000000FFFFFF000000000000000000000000000000
          0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
          0000FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
          00000000000000000000FFFFFF00FFFFFF000000000000000000000000000000
          0000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF0000000000FFFFFF000000000000000000000000000000
          0000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF0000000000FFFFFF000000000000000000000000000000
          0000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF0000000000FFFFFF000000000000000000000000000000
          0000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF0000000000FFFFFF000000000000000000000000000000
          0000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
          0000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF0000000000FFFFFF000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          7E000000424D7E000000000000003E0000002800000010000000100000000100
          010000000000400000000000000000000000020000000000000000000000FFFF
          FF00FFFF0000C001000080010000800100008001000080010000800100008001
          000080010000800100008001000080010000800100008001000080010000FFFF
          0000}
      end
      item
        ImageClass = 'TBitmap'
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          7E000000424D7E000000000000003E0000002800000010000000100000000100
          010000000000400000000000000000000000020000000000000000000000FFFF
          FF00FFFF0000FFFF0000FFFF0000FFFF0000FFF70000C1F70000C3FB0000C7FB
          0000CBFB0000DCF70000FF0F0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
          0000}
      end
      item
        ImageClass = 'TBitmap'
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000800000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          7E000000424D7E000000000000003E0000002800000010000000100000000100
          010000000000400000000000000000000000020000000000000000000000FFFF
          FF00FFFF0000F3FF0000ED9F0000ED6F0000ED6F0000F16F0000FD1F0000FC7F
          0000FEFF0000FC7F0000FD7F0000F93F0000FBBF0000FBBF0000FBBF0000FFFF
          0000}
      end
      item
        ImageClass = 'TBitmap'
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
          00000000000000000000000000000000000000000000FFFFFF00000000000000
          0000000000000000000000000000FFFFFF000000000000000000000000000000
          00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00000000000000
          0000000000000000000000000000FFFFFF00000000000000000000000000FFFF
          FF000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00000000000000
          0000FFFFFF00000000000000000000000000000000000000000000000000FFFF
          FF000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
          FF00FFFFFF0000000000FFFFFF0000000000000000000000000000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00000000000000000000000000000000000000000000000000FFFF
          FF000000000000000000FFFFFF00000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF0000000000000000000000
          000000000000000000000000000000000000000000000000000000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          7E000000424D7E000000000000003E0000002800000010000000100000000100
          010000000000400000000000000000000000020000000000000000000000FFFF
          FF00FFFF0000FFFF0000FC010000FC010000FC01000000010000000100000001
          0000000100000003000000070000000F000000FF000001FF000003FF0000FFFF
          0000}
      end
      item
        ImageClass = 'TBitmap'
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
          0000FFFFFF0000000000FFFFFF000000000000000000FFFFFF00000000000000
          000000000000000000000000000000000000FFFFFF000000000000000000FFFF
          FF0000000000FFFFFF0000000000FFFFFF0000000000FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
          0000FFFFFF0000000000FFFFFF000000000000000000FFFFFF00000000000000
          000000000000FFFFFF000000000000000000000000000000000000000000FFFF
          FF0000000000FFFFFF0000000000FFFFFF0000000000FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF0000000000FFFFFF000000000000000000000000000000
          0000FFFFFF0000000000FFFFFF000000000000000000FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF000000000000000000000000000000000000000000FFFF
          FF0000000000FFFFFF0000000000FFFFFF000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000FFFFFF0000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
          0000FFFFFF0000000000FFFFFF0000000000000000000000000000000000FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          000000000000FFFFFF0000000000000000000000000000000000000000000000
          0000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF000000000000000000FFFFFF0000000000000000000000000000000000FFFF
          FF0000000000FFFFFF0000000000FFFFFF000000000000000000FFFFFF000000
          000000000000FFFFFF0000000000000000000000000000000000000000000000
          000000000000000000000000000000000000FFFFFF00FFFFFF00000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          7E000000424D7E000000000000003E0000002800000010000000100000000100
          010000000000400000000000000000000000020000000000000000000000FFFF
          FF00FFFF0000FC00000080000000000000000000000000000000000100000003
          0000000300000003000000030000000300000003000080070000F87F0000FFFF
          0000}
      end
      item
        ImageClass = 'TBitmap'
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          7E000000424D7E000000000000003E0000002800000010000000100000000100
          010000000000400000000000000000000000020000000000000000000000FFFF
          FF00FFFF0000DFFB00008FFF000087F70000C7EF0000E3CF0000F19F0000F83F
          0000FC7F0000F83F0000F19F0000C3CF000087E700008FFB0000FFFF0000FFFF
          0000}
      end
      item
        ImageClass = 'TBitmap'
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          7E000000424D7E000000000000003E0000002800000010000000100000000100
          010000000000400000000000000000000000020000000000000000000000FFFF
          FF00FFFF0000F8F80000F8000000F8F80000FDFD00001D1D0000001D00001D1D
          0000BDBD0000BDBD0000B8B80000B8000000B8B800001F1F0000001F00001F1F
          0000}
      end
      item
        ImageClass = 'TBitmap'
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
          0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
          0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
          0000000000000000000000000000FFFFFF00FFFFFF0000000000FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
          0000000000000000000000000000FFFFFF00FFFFFF0000000000FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
          0000000000000000000000000000FFFFFF00FFFFFF0000000000FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
          0000000000000000000000000000FFFFFF00FFFFFF0000000000FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
          0000000000000000000000000000FFFFFF00FFFFFF0000000000FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
          0000000000000000000000000000FFFFFF00FFFFFF0000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
          0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          7E000000424D7E000000000000003E0000002800000010000000100000000100
          010000000000400000000000000000000000020000000000000000000000FFFF
          FF00FFFF0000C3FF0000E3FF0000C20000008A000000BE000000B0000000F000
          0000F0000000F0000000F0000000F0000000F0000000F0070000F0070000F007
          0000}
      end
      item
        ImageClass = 'TBitmap'
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF000000000000000000000000000000000000000000000000000000
          00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF000000000000000000000000000000000000000000000000000000
          00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00000000000000000000000000000000000000000000000000FFFF
          FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00000000000000000000000000000000000000000000000000FFFF
          FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00000000000000000000000000000000000000000000000000FFFF
          FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00000000000000000000000000000000000000000000000000FFFF
          FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00000000000000000000000000000000000000000000000000FFFF
          FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00000000000000000000000000000000000000000000000000FFFF
          FF00FFFFFF000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
          000000000000000000000000000000000000000000000000000000000000FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          7E000000424D7E000000000000003E0000002800000010000000100000000100
          010000000000400000000000000000000000020000000000000000000000FFFF
          FF00FFFF0000E00F0000E00F0000E00F0000000F0000000F0000000F0000000F
          0000000F0000000F0000000D0000007D00000051000000430000FFC70000FFC3
          0000}
      end
      item
        ImageClass = 'TBitmap'
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
          000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          7E000000424D7E000000000000003E0000002800000010000000100000000100
          010000000000400000000000000000000000020000000000000000000000FFFF
          FF00FFFF000080010000BFFD0000B8050000B8050000B8050000B8050000BFFD
          0000A07D0000A07D0000A07D0000A07D0000A07D0000BFFD000080010000FFFF
          0000}
      end
      item
        ImageClass = 'TBitmap'
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000FFFFFF00FFFFFF0000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000FFFFFF00FFFFFF0000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000FFFFFF00FFFFFF00FFFFFF0000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          7E000000424D7E000000000000003E0000002800000010000000100000000100
          010000000000400000000000000000000000020000000000000000000000FFFF
          FF00FFF00000FFF00000FFF00000FFF00000FFFF0000003E00007FBE000060A8
          000060A1000060A300007FA1000041BF000041BF000041BF00007FBF0000003F
          0000}
      end
      item
        ImageClass = 'TBitmap'
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000FFFFFF00FFFFFF00FFFFFF0000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          7E000000424D7E000000000000003E0000002800000010000000100000000100
          010000000000400000000000000000000000020000000000000000000000FFFF
          FF00FFFF0000003F00007FBF000060BF000060BF000060BF00007FBF000041AF
          000041A7000041B200007FB0000000380000FFF00000FFFF0000FFFF0000FFFF
          0000}
      end
      item
        ImageClass = 'TBitmap'
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
          FF00FFFFFF000000000000000000000000000000000000000000000000000000
          0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
          FF00FFFFFF000000000000000000000000000000000000000000000000000000
          0000FFFFFF00FFFFFF000000000000000000000000000000000000000000FFFF
          FF00FFFFFF000000000000000000000000000000000000000000000000000000
          0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
          FF00FFFFFF000000000000000000000000000000000000000000000000000000
          0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
          FF00FFFFFF000000000000000000000000000000000000000000000000000000
          000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          7E000000424D7E000000000000003E0000002800000010000000100000000100
          010000000000400000000000000000000000020000000000000000000000FFFF
          FF00FFFF0000FFFB0000FFF10000FFE30000F0470000C00F0000C01F0000800F
          0000800F0000800F0000800F0000800F0000C01F0000C01F0000F07F0000FFFF
          0000}
      end
      item
        ImageClass = 'TBitmap'
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF000000000000000000000000000000000000000000000000000000
          0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF000000000000000000000000000000000000000000000000000000
          0000FFFFFF00FFFFFF000000000000000000000000000000000000000000FFFF
          FF00FFFFFF000000000000000000000000000000000000000000000000000000
          0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF000000000000000000000000000000000000000000000000000000
          0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF000000000000000000000000000000000000000000000000000000
          000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          7E000000424D7E000000000000003E0000002800000010000000100000000100
          010000000000400000000000000000000000020000000000000000000000FFFF
          FF00FFFF0000FFFB0000FFF10000FFE30000F0470000C00F0000C01F0000800F
          0000800F0000800F0000800F0000800F0000C01F0000C01F0000F07F0000FFFF
          0000}
      end
      item
        ImageClass = 'TBitmap'
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          7E000000424D7E000000000000003E0000002800000010000000100000000100
          010000000000400000000000000000000000020000000000000000000000FFFF
          FF00FFFF0000FFFF0000FEFF0000FC7F0000F83F0000FEFF0000EFEF0000CFE7
          000087C30000CFE70000EFEF0000FEFF0000F83F0000FC7F0000FEFF0000FFFF
          0000}
      end>
  end
  object ilSmallImages: TcxImageList
    SourceDPI = 96
    Height = 11
    Width = 11
    FormatVersion = 1
    DesignInfo = 3670120
    ImageInfo = <
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707070007070700000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707
          070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700070707000707070007070700000000FF000000FF0000
          00FF070707000707070007070700070707000707070007070700000000FF0000
          00FFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FF07070700070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF07070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFF000000FF07070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000707
          0700000000FF000000FFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FF0707
          07000707070007070700070707000707070007070700000000FF000000FF0000
          00FF070707000707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700000000FF000000FF000000FF0000
          00FF000000FF0707070007070700070707000707070007070700000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707070007070700000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707
          070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707
          070007070700070707000707070007070700000000FF000000FF000000FF0000
          00FF000000FF0707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700000000FF0707070007070700070707000707
          07000707070007070700070707000707070007070700000000FFFFFFFFFF0000
          00FF070707000707070007070700070707000707070007070700070707000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707070007070700070707000707
          070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF070707000707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF07070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF070707000707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707070007070700070707000707
          070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFF000000FF070707000707
          07000707070007070700070707000707070007070700000000FFFFFFFFFF0000
          00FF070707000707070007070700070707000707070007070700070707000707
          070007070700000000FF0707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF070707000707
          070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF07070700070707000707070007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFF000000FF070707000707070007070700070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000707
          0700070707000707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF070707000707070007070700070707000707070007070700070707000000
          00FFFFFFFFFF000000FF07070700070707000707070007070700070707000707
          07000707070007070700000000FFFFFFFFFF000000FF07070700070707000707
          0700070707000707070007070700070707000707070007070700000000FF0707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700000000FF0707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700000000FF0707070007070700070707000707
          0700070707000707070007070700070707000707070007070700000000FF0707
          0700070707000707070007070700070707000707070007070700070707000707
          0700000000FFFFFFFFFF000000FF070707000707070007070700070707000707
          0700070707000707070007070700000000FFFFFFFFFF000000FF070707000707
          07000707070007070700070707000707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF07070700070707000707070007070700070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000707
          07000707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF07070700070707000707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707070007070700070707000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF0707070007070700070707000707
          07000707070007070700070707000707070007070700000000FFFFFFFFFF0000
          00FF000000FF0707070007070700070707000707070007070700070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FF070707000707
          0700070707000707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF000000FF070707000707070007070700000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0000
          00FF07070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF000000FF070707000707070007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF000000FF07070700070707000707070007070700070707000000
          00FFFFFFFFFF000000FF000000FF070707000707070007070700070707000707
          07000707070007070700000000FF000000FF0707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700000000FF000000FF070707000707070007070700070707000707
          07000707070007070700000000FF000000FFFFFFFFFF000000FF070707000707
          0700070707000707070007070700000000FF000000FFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF070707000707070007070700000000FF000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700000000FF000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707
          07000707070007070700000000FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF07070700070707000707070007070700070707000000
          00FF000000FFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000707
          070007070700070707000707070007070700000000FF000000FFFFFFFFFF0000
          00FF070707000707070007070700070707000707070007070700070707000707
          070007070700000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          070007070700000000FF000000FF000000FF000000FF000000FF070707000707
          0700070707000707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF07070700070707000707070007070700000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF07070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FFFF00FF0007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFF00FF00070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFF00
          FF00FF00FF000707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FFFF00FF00FF00FF000707070007070700070707000000
          00FF000000FF000000FF000000FF000000FFFF00FF00FF00FF00FF00FF00}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0707
          070007070700000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF07070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF0707070007070700000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0707
          070007070700000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF07070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF0707070007070700000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0707
          070007070700000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF0707070007070700000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF07070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF0707070007070700000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF0707070007070700000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0707
          070007070700000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF0707070007070700000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF07070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF0707070007070700000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF0707070007070700000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0707
          070007070700000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF0707070007070700000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF07070700070707000000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700070707000707070007070700000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF070707000707
          0700000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF0707070007070700000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF0707070007070700000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0707
          070007070700000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF0707070007070700000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF07070700070707000000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700070707000707070007070700000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF070707000707
          0700000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF0707070007070700000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF0707070007070700000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0707
          070007070700000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF0707070007070700000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF07070700070707000000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF070707000707
          0700000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF0707070007070700000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF0707070007070700000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0707
          070007070700000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF0707070007070700000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF07070700070707000000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF070707000707
          0700000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF0707070007070700000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF0707070007070700000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0707
          070007070700000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF0707070007070700000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF07070700070707000000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF07070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700000000FF0707070007070700070707000707
          07000707070007070700070707000707070007070700000000FF070707000000
          00FF070707000707070007070700070707000707070007070700070707000707
          0700000000FF0707070007070700000000FF0707070007070700070707000707
          07000707070007070700000000FF070707000707070007070700070707000000
          00FF0707070007070700070707000707070007070700000000FF070707000707
          0700070707000707070007070700000000FF0707070007070700070707000000
          00FF070707000707070007070700070707000707070007070700070707000000
          00FF0707070007070700000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700000000FF07070700070707000707
          0700070707000707070007070700070707000707070007070700070707000000
          00FF070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700000000FF000000FF000000FF000000FF07070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700000000FF000000FF07070700070707000707070007070700070707000707
          07000707070007070700070707000707070007070700000000FF000000FF0707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF070707000707070007070700070707000707
          07000707070007070700070707000707070007070700000000FF070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700000000FF07070700070707000707070007070700070707000707
          070007070700070707000707070007070700000000FF07070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700000000FF0707070007070700070707000707070007070700070707000707
          0700070707000707070007070700000000FF0707070007070700070707000707
          0700070707000707070007070700070707000707070007070700000000FF}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700000000FF000000FF000000FF0000
          00FF000000FF000000FF07070700070707000707070007070700070707000707
          070007070700070707000707070007070700000000FF07070700070707000707
          0700070707000707070007070700070707000707070007070700070707000000
          00FF070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF070707000707070007070700070707000707
          07000707070007070700070707000707070007070700000000FF070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700000000FF0707070007070700070707000707070007070700070707000707
          0700070707000707070007070700000000FF000000FF000000FF000000FF0000
          00FF000000FF0707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000000
          00FF070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF070707000707070007070700070707000707
          07000707070007070700070707000707070007070700000000FF070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700000000FF0707070007070700070707000707070007070700070707000707
          0700070707000707070007070700000000FF0707070007070700070707000707
          0700070707000707070007070700070707000707070007070700000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700000000FF0707070007070700070707000707070007070700070707000707
          0700070707000707070007070700000000FF0707070007070700070707000707
          0700070707000707070007070700070707000707070007070700000000FF0707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700000000FF07070700070707000707070007070700070707000707
          070007070700070707000707070007070700000000FF0707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000000
          00FF07070700000000FF0707070007070700000000FF07070700070707000000
          00FF07070700000000FF000000FF07070700000000FF07070700000000FF0707
          0700000000FF07070700000000FF07070700000000FF000000FF070707000000
          00FF07070700000000FF07070700000000FF07070700000000FF070707000000
          00FF000000FF07070700000000FF07070700000000FF07070700000000FF0707
          0700000000FF07070700000000FF000000FF07070700000000FF070707000000
          00FF07070700000000FF07070700000000FF07070700000000FF000000FF0000
          00FF070707000707070007070700000000FF0707070007070700000000FF0000
          00FF070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700000000FF0707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700000000FF07070700070707000707
          0700070707000707070007070700070707000707070007070700000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0707
          07000707070007070700000000FF070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700000000FF0707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000000
          00FF000000FF000000FF07070700070707000707070007070700070707000707
          070007070700000000FF070707000707070007070700000000FF070707000707
          0700070707000707070007070700000000FF0707070007070700070707000707
          070007070700000000FF07070700070707000707070007070700000000FF0707
          070007070700070707000707070007070700000000FF000000FF000000FF0707
          070007070700000000FF07070700070707000707070007070700070707000000
          00FF0707070007070700070707000707070007070700000000FF070707000707
          070007070700000000FF07070700070707000707070007070700070707000707
          070007070700000000FF000000FF000000FF0707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700070707000707070007070700000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF0707070007070700070707000707
          0700000000FF0707070007070700070707000707070007070700000000FF0707
          0700070707000707070007070700000000FF0707070007070700070707000707
          070007070700000000FF07070700070707000707070007070700000000FF0707
          070007070700070707000707070007070700000000FF000000FF000000FF0707
          070007070700000000FF07070700070707000707070007070700070707000000
          00FF07070700070707000707070007070700000000FF07070700070707000707
          07000707070007070700000000FF070707000707070007070700070707000000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000000
          00FF07070700000000FF0707070007070700000000FF07070700070707000000
          00FF07070700000000FF000000FF07070700000000FF07070700000000FF0707
          0700000000FF07070700000000FF07070700000000FF000000FF070707000000
          00FF07070700000000FF07070700000000FF07070700000000FF070707000000
          00FF000000FF07070700000000FF07070700000000FF07070700000000FF0707
          0700000000FF07070700000000FF000000FF07070700000000FF070707000000
          00FF07070700000000FF07070700000000FF07070700000000FF000000FF0000
          00FF070707000707070007070700000000FF0707070007070700000000FF0000
          00FF070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700000000FF0707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF070707000707070007070700000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0707
          0700070707000707070007070700070707000707070007070700070707000707
          0700000000FF0707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700000000FF07070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700000000FF000000FF000000FF0707070007070700070707000707
          0700070707000707070007070700000000FF0707070007070700070707000000
          00FF0707070007070700070707000707070007070700000000FF070707000707
          0700070707000707070007070700000000FF0707070007070700000000FF0000
          00FF000000FF0707070007070700070707000707070007070700000000FF0707
          0700070707000707070007070700000000FF0707070007070700070707000707
          070007070700000000FF07070700070707000707070007070700070707000000
          00FF070707000707070007070700000000FF0707070007070700070707000707
          0700070707000707070007070700000000FF000000FF000000FF070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF070707000707
          07000707070007070700000000FF070707000707070007070700070707000707
          0700000000FF07070700070707000707070007070700000000FF070707000707
          0700070707000707070007070700000000FF0707070007070700000000FF0000
          00FF000000FF0707070007070700070707000707070007070700000000FF0707
          0700070707000707070007070700000000FF0707070007070700070707000707
          070007070700000000FF07070700070707000707070007070700000000FF0707
          070007070700070707000707070007070700000000FF07070700070707000707
          070007070700000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000000
          00FF070707000707070007070700070707000707070007070700070707000707
          070007070700000000FF000000FF000000FF000000FF07070700070707000707
          07000707070007070700070707000707070007070700000000FF070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700000000FF07070700070707000707
          0700070707000707070007070700070707000707070007070700000000FF0707
          0700070707000707070007070700070707000707070007070700070707000707
          0700000000FF000000FF000000FF000000FF000000FF07070700070707000707
          070007070700070707000707070007070700000000FF07070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700000000FF0707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700000000FF07070700070707000707070007070700070707000707
          0700070707000707070007070700000000FF0707070007070700070707000707
          07000707070007070700070707000707070007070700000000FF070707000707
          0700070707000707070007070700070707000707070007070700070707000000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF070707000707
          0700070707000707070007070700000000FF0707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000000
          00FF070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700000000FF0707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700000000FF0707
          0700070707000707070007070700070707000707070007070700070707000707
          0700000000FF0707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700000000FF07070700070707000707
          0700070707000707070007070700070707000707070007070700000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0707
          07000707070007070700000000FF070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700000000FF0707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700000000FF0707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700000000FF0707070007070700070707000707
          07000707070007070700070707000707070007070700000000FF070707000707
          0700070707000707070007070700070707000707070007070700070707000000
          00FF070707000707070007070700070707000707070007070700070707000707
          070007070700000000FF07070700070707000707070007070700070707000707
          0700070707000707070007070700000000FF0707070007070700070707000707
          07000707070007070700070707000707070007070700000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF07070700000000FF07070700070707000707070007070700070707000707
          07000707070007070700070707000707070007070700000000FF070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700000000FF07070700070707000707070007070700070707000707
          07000707070007070700070707000707070007070700000000FF070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700000000FF0707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700000000FF0707070007070700070707000707070007070700070707000707
          070007070700000000FF000000FF000000FF000000FF07070700070707000707
          0700070707000707070007070700070707000707070007070700000000FF0707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700000000FF07070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700000000FF0707070007070700070707000707070007070700070707000707
          0700000000FF000000FF000000FF000000FF000000FF07070700070707000707
          0700070707000707070007070700070707000707070007070700000000FF0707
          0700070707000707070007070700070707000707070007070700070707000707
          0700000000FF0707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700000000FF07070700070707000707070007070700070707000707
          07000707070007070700070707000707070007070700000000FF070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700000000FF07070700070707000707070007070700070707000000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF070707000707
          0700070707000707070007070700070707000707070007070700070707000000
          00FF070707000707070007070700070707000707070007070700070707000707
          070007070700000000FF07070700070707000707070007070700070707000707
          0700070707000707070007070700000000FF0707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700000000FF0707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700000000FF0707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF070707000707070007070700000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0707
          0700070707000707070007070700070707000707070007070700070707000707
          0700000000FF0707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700000000FF07070700070707000707
          0700070707000707070007070700070707000707070007070700000000FF0707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700000000FF0707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000000
          00FF070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700000000FF0707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000000
          00FF070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700000000FF07070700000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF070707000707070007070700070707000707070007070700070707000707
          070007070700000000FF07070700070707000707070007070700070707000707
          0700070707000707070007070700000000FF0707070007070700070707000707
          07000707070007070700070707000707070007070700000000FF070707000707
          0700070707000707070007070700070707000707070007070700070707000000
          00FF070707000707070007070700070707000707070007070700070707000707
          070007070700000000FF0707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707070007070700000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707
          070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000000
          00FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF07070700000000FF07070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707070007070700000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707
          070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000000
          00FFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000000
          00FF07070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707070007070700000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707
          070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700000000FF0707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707070007070700000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707
          070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          0700070707000707070007070700000000FF070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707070007070700000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707
          070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700000000FF}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707070007070700000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707
          070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FF000000FF070707000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707070007070700000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FF0000
          00FF07070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF000000FF000000FF07070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707070007070700000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707
          070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700000000FF07070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FF070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707070007070700000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707
          070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          07000707070007070700070707000707070007070700000000FF070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFF000000FF070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707070007070700000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707
          070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700000000FF0707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFF000000FF070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707070007070700000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707
          070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          070007070700000000FF07070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFF0000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707070007070700000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707
          070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700000000FF07070700070707000707070007070700070707000707
          07000707070007070700070707000707070007070700000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF070707000707
          0700000000FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707070007070700000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707
          070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFF000000FF0707070007070700000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF07070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF070707000000
          00FF000000FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707070007070700000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707
          070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700000000FF000000FF0000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707
          070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707070007070700000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707
          070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF07070700000000FF000000FF000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707070007070700000000FF0000
          00FFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707
          070007070700000000FF000000FF000000FF000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF0707070007070700000000FF000000FFFFFFFFFF0000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000000
          00FFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707070007070700000000FFFFFF
          FFFFFFFFFFFF000000FFFFFFFFFF000000FFFFFFFFFFFFFFFFFF000000FF0707
          070007070700000000FFFFFFFFFFFFFFFFFF000000FF000000FF000000FFFFFF
          FFFFFFFFFFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFF0000
          00FFFFFFFFFF000000FFFFFFFFFFFFFFFFFF000000FF07070700070707000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707070007070700000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFF000000FF000000FF0707
          070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0000
          00FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FFFFFFFFFF000000FF000000FF07070700070707000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FF000000FFFFFFFFFF000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707070007070700000000FF0000
          00FF000000FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707
          070007070700000000FF000000FFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF0707070007070700000000FFFFFFFFFF000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFF000000FFFFFF
          FFFF000000FFFFFFFFFFFFFFFFFF000000FF0707070007070700000000FFFFFF
          FFFFFFFFFFFF000000FF000000FF000000FFFFFFFFFFFFFFFFFF000000FF0707
          070007070700000000FFFFFFFFFFFFFFFFFF000000FFFFFFFFFF000000FFFFFF
          FFFFFFFFFFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FFFFFFFFFF000000FF000000FF0707070007070700000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FF000000FF000000FF0707
          070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFF
          FFFF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFF000000FF07070700070707000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FF000000FFFFFF
          FFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF070707000707
          0700000000FF000000FF000000FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FF000000FFFFFFFFFF000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707070007070700000000FFFFFF
          FFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707
          070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFF
          FFFF000000FFFFFFFFFF000000FFFFFFFFFFFFFFFFFF000000FF070707000707
          0700000000FFFFFFFFFFFFFFFFFF000000FF000000FF000000FFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFF000000FFFFFF
          FFFF000000FFFFFFFFFFFFFFFFFF000000FF0707070007070700000000FFFFFF
          FFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707
          070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFF000000FF000000FF070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FF0000
          00FF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FFFFFFFFFF000000FF000000FF0707070007070700000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFF000000FF0707
          070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707070007070700000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707
          070007070700000000FF000000FF000000FF000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF0707070007070700000000FF000000FF000000FF0000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000000
          00FF000000FF000000FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707070007070700000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707
          070007070700000000FFFFFFFFFFFFFFFFFF000000FF000000FF000000FFFFFF
          FFFFFFFFFFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFF0000
          00FF000000FF000000FFFFFFFFFFFFFFFFFF000000FF07070700070707000000
          00FFFFFFFFFFFFFFFFFF000000FF000000FF000000FFFFFFFFFFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707070007070700000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707
          070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0000
          00FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF000000FF000000FF000000FF07070700070707000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FF000000FF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FF000000FF000000FF000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707070007070700000000FF0000
          00FF000000FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707
          070007070700000000FF000000FF000000FF000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFF000000FF0000
          00FF000000FFFFFFFFFFFFFFFFFF000000FF0707070007070700000000FFFFFF
          FFFFFFFFFFFF000000FF000000FF000000FFFFFFFFFFFFFFFFFF000000FF0707
          070007070700000000FFFFFFFFFFFFFFFFFF000000FF000000FF000000FFFFFF
          FFFFFFFFFFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF000000FF000000FF000000FF0707070007070700000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FF000000FF000000FF0707
          070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0000
          00FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FF000000FF0000
          00FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF070707000707
          0700000000FF000000FF000000FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FF000000FF000000FF000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707070007070700000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707
          070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFF
          FFFF000000FF000000FF000000FFFFFFFFFFFFFFFFFF000000FF070707000707
          0700000000FFFFFFFFFFFFFFFFFF000000FF000000FF000000FFFFFFFFFFFFFF
          FFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFF000000FF0000
          00FF000000FFFFFFFFFFFFFFFFFF000000FF0707070007070700000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707
          070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          07000707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF0707070007070700000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFF000000FF000000FF000000FF000000FF070707000707
          0700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FF0000
          00FF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000FF000000FF000000FF000000FF0707070007070700000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF0707
          070007070700000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000FF0707070007070700000000FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF07070700070707000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FF0707070007070700000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000000
          00FF07070700000000FF0707070007070700000000FF07070700070707000000
          00FF07070700000000FF000000FF07070700000000FF07070700000000FF0707
          0700000000FF07070700000000FF07070700000000FF000000FF070707000000
          00FF07070700000000FF07070700000000FF07070700000000FF070707000000
          00FF000000FF07070700000000FF07070700000000FF07070700000000FF0707
          0700000000FF07070700000000FF000000FF07070700000000FF070707000000
          00FF07070700000000FF07070700000000FF07070700000000FF000000FF0000
          00FF070707000707070007070700000000FF0707070007070700000000FF0000
          00FF070707000707070007070700070707000707070007070700070707000707
          0700070707000707070007070700070707000707070007070700070707000707
          070007070700070707000707070007070700070707000707070007070700}
      end
      item
        ImageClass = 'TBitmap'
        Image.Data = {
          1A020000424D1A0200000000000036000000280000000B0000000B0000000100
          200000000000E401000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000FF000000FF000000FF0000
          00FF000000FF0000000000000000000000000000000000000000000000000000
          00000000000000000000000000FF000000FF0000000000000000000000000000
          000000000000000000000000000000000000000000FF00000000000000FF0000
          0000000000000000000000000000000000000000000000000000000000FF0000
          000000000000000000FF00000000000000000000000000000000000000000000
          0000000000FF000000000000000000000000000000FF00000000000000000000
          00000000000000000000000000FF000000000000000000000000000000000000
          0000000000FF000000000000000000000000000000FF00000000000000000000
          0000000000000000000000000000000000FF0000000000000000000000FF0000
          0000000000000000000000000000000000000000000000000000000000FF0000
          0000000000FF0000000000000000000000000000000000000000000000000000
          000000000000000000FF2B0012FF000000000000000000000000000000000000
          000000000000000000000000000000000000000000FF000000FF000000FF0000
          00FF000000FF000000000000000000000000000000000000000000000000}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D494844520000000B0000000B0806000000A9AC77
          260000003E4944415478DA63644005FFA134230316C088A610AB2274C5FF7118
          4094C9FF49518C95CF88C753187EC0E7219862B826428A510CA5AAC944B9196B
          B803006DBE110A63B165110000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D494844520000000B0000000B0806000000A9AC77
          26000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
          00097048597300000EC300000EC301C76FA86400000048494441542853A58B51
          0E00200842BDFFA52D4A3E50DB5AC1B494A779780A4D8A19BD1B8246F9A080ED
          FC05E3CDC57D81A9767E82E3222215F6055E9F084E25F0C9727463806E6E03AE
          5B0FFF34E6E9450000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D494844520000000B0000000B0806000000A9AC77
          26000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
          00097048597300000EC300000EC301C76FA864000000494944415428538D8D41
          0A00200804FDFFA72D93B53918D6608CAEA079C37ECE5E40935854711E4067A0
          33F9352789F2236A5E68A71A051DCA6BF00374069AC44915E701F4D22DBAB92D
          03E5E11F5E8A1CBB0000000049454E44AE426082}
      end>
  end
  object ColorDialog: TdxColorDialog
    Left = 110
    Top = 208
  end
end
