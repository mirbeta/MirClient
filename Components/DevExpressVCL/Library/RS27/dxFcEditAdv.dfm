object FChartAdvEditor: TFChartAdvEditor
  Left = 354
  Top = 105
  Caption = 'dxFlowChart Editor'
  ClientHeight = 574
  ClientWidth = 708
  Color = clBtnFace
  Constraints.MinHeight = 350
  Constraints.MinWidth = 350
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object bvlSpace1: TBevel
    Left = 0
    Top = 31
    Width = 10
    Height = 493
    Align = alLeft
    Shape = bsSpacer
  end
  object bvlSpace2: TBevel
    Left = 698
    Top = 31
    Width = 10
    Height = 493
    Align = alRight
    Shape = bsSpacer
  end
  object bvlSpace3: TBevel
    Left = 0
    Top = 21
    Width = 708
    Height = 10
    Align = alTop
    Shape = bsSpacer
  end
  object bvlSpace4: TBevel
    Left = 55
    Top = 31
    Width = 8
    Height = 493
    Align = alLeft
    Shape = bsSpacer
  end
  object Panel1: TPanel
    Left = 10
    Top = 31
    Width = 45
    Height = 493
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
      Top = 491
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
    Top = 524
    Width = 708
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
      Left = 542
      Top = 16
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TcxButton
      Left = 623
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
    Top = 31
    Width = 635
    Height = 493
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
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 142
    Top = 208
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
    DesignInfo = 3670256
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
    DesignInfo = 3670160
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
  object dxBarManager1: TdxBarManager
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    CanCustomize = False
    Categories.Strings = (
      'Default'
      'MainMenu'
      'File'
      'Edit'
      'View'
      'Unions'
      'Options'
      'Help'
      'ShapePopupMenu'
      'ChartPopupMenu'
      'DArrowSizePopupMenu'
      'DestArrowPopupMenu'
      'DPointPopupMenu'
      'ImagePositionPopupMenu'
      'LinePopupMenu'
      'SArrowSizePopupMenu'
      'SourceArrowPopupMenu'
      'SPointPopupMenu'
      'StylePopupMenu'
      'TextPositionPopupMenu')
    Categories.ItemsVisibles = (
      2
      2
      2
      2
      2
      2
      2
      2
      2
      2
      2
      2
      2
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
      True
      True
      True
      True
      True
      True
      True
      True
      True
      True
      True
      True
      True
      True)
    ImageOptions.Images = ilLargeImages
    ImageOptions.StretchGlyphs = False
    PopupMenuLinks = <>
    Style = bmsUseLookAndFeel
    UseSystemFont = True
    Left = 320
    Top = 192
    PixelsPerInch = 96
    DockControlHeights = (
      0
      0
      21
      0)
    object dxBarManager1Bar1: TdxBar
      AllowClose = False
      AllowCustomizing = False
      AllowQuickCustomizing = False
      AllowReset = False
      Caption = 'Main Menu'
      CaptionButtons = <>
      DockedDockingStyle = dsTop
      DockedLeft = 0
      DockedTop = 0
      DockingStyle = dsTop
      FloatLeft = 0
      FloatTop = 0
      FloatClientWidth = 0
      FloatClientHeight = 0
      IsMainMenu = True
      ItemLinks = <
        item
          Visible = True
          ItemName = 'miFile'
        end
        item
          Visible = True
          ItemName = 'miEdit'
        end
        item
          Visible = True
          ItemName = 'View1'
        end
        item
          Visible = True
          ItemName = 'miUnions'
        end
        item
          Visible = True
          ItemName = 'miOptions'
        end
        item
          Visible = True
          ItemName = 'miHelp'
        end>
      MultiLine = True
      NotDocking = [dsNone, dsLeft, dsTop, dsRight, dsBottom]
      OneOnRow = True
      Row = 0
      ShowMark = False
      SizeGrip = False
      UseOwnFont = False
      UseRecentItems = False
      Visible = True
      WholeRow = True
    end
    object miFile: TdxBarSubItem
      Caption = '&File'
      Category = 1
      Visible = ivAlways
      ItemLinks = <
        item
          Visible = True
          ItemName = 'miOpen'
        end
        item
          Visible = True
          ItemName = 'miSaveAs'
        end>
    end
    object miEdit: TdxBarSubItem
      Caption = '&Edit'
      Category = 1
      Visible = ivAlways
      ItemLinks = <
        item
          Visible = True
          ItemName = 'miUndo'
        end
        item
          BeginGroup = True
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
        end>
    end
    object View1: TdxBarSubItem
      Caption = '&View'
      Category = 1
      Visible = ivAlways
      ItemLinks = <
        item
          Visible = True
          ItemName = 'miAntialiasing'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'miZoomIn'
        end
        item
          Visible = True
          ItemName = 'miZoomOut'
        end
        item
          Visible = True
          ItemName = 'miFit'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'miActualSize'
        end>
    end
    object miUnions: TdxBarSubItem
      Caption = '&Unions'
      Category = 1
      Visible = ivAlways
      ItemLinks = <
        item
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
    end
    object miOptions: TdxBarSubItem
      Caption = '&Options'
      Category = 1
      Visible = ivAlways
      ItemLinks = <
        item
          Visible = True
          ItemName = 'miDynamicMoving'
        end
        item
          Visible = True
          ItemName = 'miDynamicSizing'
        end>
    end
    object miHelp: TdxBarSubItem
      Caption = '&Help'
      Category = 1
      Visible = ivAlways
      ItemLinks = <
        item
          Visible = True
          ItemName = 'Contents1'
        end>
    end
    object miOpen: TdxBarButton
      Caption = '&Open'
      Category = 2
      Visible = ivAlways
      OnClick = miOpenClick
    end
    object miSaveAs: TdxBarButton
      Caption = 'Save &As ...'
      Category = 2
      Visible = ivAlways
      OnClick = miSaveAsClick
    end
    object miUndo: TdxBarButton
      Caption = '&Undo'
      Category = 3
      Enabled = False
      Visible = ivAlways
      ShortCut = 16474
      OnClick = miUndoClick
    end
    object miCut: TdxBarButton
      Caption = 'Cu&t'
      Category = 3
      Enabled = False
      Visible = ivAlways
      ShortCut = 16472
      OnClick = iCutClick
    end
    object miCopy: TdxBarButton
      Caption = '&Copy'
      Category = 3
      Enabled = False
      Visible = ivAlways
      ShortCut = 16451
      OnClick = iCopyClick
    end
    object miPaste: TdxBarButton
      Caption = '&Paste'
      Category = 3
      Enabled = False
      Visible = ivAlways
      ShortCut = 16470
      OnClick = iPasteClick
    end
    object miDelete: TdxBarButton
      Caption = '&Delete'
      Category = 3
      Enabled = False
      Visible = ivAlways
      ShortCut = 46
      OnClick = iDeleteClick
    end
    object miSelectAll: TdxBarButton
      Caption = 'Se&lect All'
      Category = 3
      Visible = ivAlways
      ShortCut = 16449
      OnClick = iSelectAllClick
    end
    object miClearSelection: TdxBarButton
      Caption = 'Cl&ear Selection'
      Category = 3
      Enabled = False
      Visible = ivAlways
      OnClick = iClearSelectionClick
    end
    object miBringToFront: TdxBarButton
      Caption = 'Bring To &Front'
      Category = 3
      Enabled = False
      Visible = ivAlways
      OnClick = iBringToFrontClick
    end
    object miSendToBack: TdxBarButton
      Caption = 'Send To &Back'
      Category = 3
      Enabled = False
      Visible = ivAlways
      OnClick = iSendToBackClick
    end
    object miAntialiasing: TdxBarButton
      Caption = '&Antialiasing'
      Category = 4
      Visible = ivAlways
      ButtonStyle = bsChecked
      OnClick = miAntialiasingClick
    end
    object miZoomIn: TdxBarButton
      Caption = 'Zoom &In'
      Category = 4
      Visible = ivAlways
      ShortCut = 16457
      OnClick = miZoomInClick
    end
    object miZoomOut: TdxBarButton
      Caption = 'Zoom &Out'
      Category = 4
      Visible = ivAlways
      ShortCut = 16469
      OnClick = miZoomOutClick
    end
    object miFit: TdxBarButton
      Caption = '&Fit'
      Category = 4
      Visible = ivAlways
      ButtonStyle = bsChecked
      ShortCut = 16454
      OnClick = miFitClick
    end
    object miActualSize: TdxBarButton
      Caption = '&Actual Size'
      Category = 4
      Visible = ivAlways
      ShortCut = 16465
      OnClick = miActualSizeClick
    end
    object miNewUnion: TdxBarButton
      Caption = 'New Union'
      Category = 5
      Enabled = False
      Visible = ivAlways
      OnClick = iNewUnionClick
    end
    object miAddToUnion: TdxBarButton
      Caption = 'Add To Union'
      Category = 5
      Enabled = False
      Visible = ivAlways
      OnClick = iAddToUnionClick
    end
    object miRemoveFromUnion: TdxBarButton
      Caption = 'Remove From Union'
      Category = 5
      Enabled = False
      Visible = ivAlways
      OnClick = iRemoveFromUnionClick
    end
    object miClearUnion: TdxBarButton
      Caption = 'Clear Union'
      Category = 5
      Enabled = False
      Visible = ivAlways
      OnClick = iClearUnionClick
    end
    object miClearAllUnions: TdxBarButton
      Caption = 'Clear All Unions'
      Category = 5
      Visible = ivAlways
      OnClick = iClearAllUnionsClick
    end
    object miDynamicMoving: TdxBarButton
      Caption = 'Dynamic &Moving'
      Category = 6
      Visible = ivAlways
      ButtonStyle = bsChecked
      OnClick = miDynamicMovingClick
    end
    object miDynamicSizing: TdxBarButton
      Caption = 'Dynamic &Sizing'
      Category = 6
      Visible = ivAlways
      ButtonStyle = bsChecked
      OnClick = miDynamicSizingClick
    end
    object Contents1: TdxBarButton
      Caption = '&Contents'
      Category = 7
      Visible = ivAlways
      ShortCut = 112
      OnClick = Contents1Click
    end
    object iNone: TdxBarButton
      Caption = 'None'
      Category = 8
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iRectangle: TdxBarButton
      Tag = 1
      Caption = 'Rectangle'
      Category = 8
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iEllipse: TdxBarButton
      Tag = 2
      Caption = 'Ellipse'
      Category = 8
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iRoundRect: TdxBarButton
      Tag = 3
      Caption = 'Round Rect'
      Category = 8
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iDiamond: TdxBarButton
      Tag = 4
      Caption = 'Diamond'
      Category = 8
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iNorthTriangle: TdxBarButton
      Tag = 5
      Caption = 'North Triangle'
      Category = 8
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object itSouthTriangle: TdxBarButton
      Tag = 6
      Caption = 'South Triangle'
      Category = 8
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object itEastTriangle: TdxBarButton
      Tag = 7
      Caption = 'East Triangle'
      Category = 8
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object itWestTriangle: TdxBarButton
      Tag = 8
      Caption = 'West Triangle'
      Category = 8
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object itHexagon: TdxBarButton
      Tag = 9
      Caption = 'Hexagon'
      Category = 8
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iEdit: TdxBarButton
      Caption = '&Properties'
      Category = 9
      Visible = ivAlways
      OnClick = iEditClick
    end
    object iCut: TdxBarButton
      Caption = 'Cu&t'
      Category = 9
      Enabled = False
      Visible = ivAlways
      ShortCut = 16472
      OnClick = iCutClick
    end
    object iCopy: TdxBarButton
      Caption = '&Copy'
      Category = 9
      Enabled = False
      Visible = ivAlways
      ShortCut = 16451
      OnClick = iCopyClick
    end
    object iPaste: TdxBarButton
      Caption = '&Paste'
      Category = 9
      Enabled = False
      Visible = ivAlways
      ShortCut = 16470
      OnClick = iPasteClick
    end
    object iDelete: TdxBarButton
      Caption = '&Delete'
      Category = 9
      Enabled = False
      Visible = ivAlways
      ShortCut = 46
      OnClick = iDeleteClick
    end
    object iRemovePoint: TdxBarButton
      Caption = '&Remove Point'
      Category = 9
      Enabled = False
      Visible = ivAlways
      OnClick = iRemovePointClick
    end
    object iSelectAll: TdxBarButton
      Caption = 'Se&lect All'
      Category = 9
      Visible = ivAlways
      ShortCut = 16449
      OnClick = iSelectAllClick
    end
    object iClearSelection: TdxBarButton
      Caption = 'Cl&ear Selection'
      Category = 9
      Enabled = False
      Visible = ivAlways
      OnClick = iClearSelectionClick
    end
    object iBringToFront: TdxBarButton
      Caption = 'Bring To &Front'
      Category = 9
      Enabled = False
      Visible = ivAlways
      OnClick = iBringToFrontClick
    end
    object iSendToBack: TdxBarButton
      Caption = 'Send To &Back'
      Category = 9
      Enabled = False
      Visible = ivAlways
      OnClick = iSendToBackClick
    end
    object iNewUnion: TdxBarButton
      Caption = 'New Union'
      Category = 9
      Enabled = False
      Visible = ivAlways
      OnClick = iNewUnionClick
    end
    object iAddToUnion: TdxBarButton
      Caption = 'Add To Union'
      Category = 9
      Enabled = False
      Visible = ivAlways
      OnClick = iAddToUnionClick
    end
    object iRemoveFromUnion: TdxBarButton
      Caption = 'Remove From Union'
      Category = 9
      Enabled = False
      Visible = ivAlways
      OnClick = iRemoveFromUnionClick
    end
    object iClearUnion: TdxBarButton
      Caption = 'Clear Union'
      Category = 9
      Enabled = False
      Visible = ivAlways
      OnClick = iClearUnionClick
    end
    object iClearAllUnions: TdxBarButton
      Caption = 'Clear All Unions'
      Category = 9
      Enabled = False
      Visible = ivAlways
      OnClick = iClearAllUnionsClick
    end
    object iD10: TdxBarButton
      Tag = 1
      Caption = '10 x 10'
      Category = 10
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iD20: TdxBarButton
      Tag = 2
      Caption = '15 x 15'
      Category = 10
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iD30: TdxBarButton
      Tag = 3
      Caption = '20 x 20'
      Category = 10
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iD40: TdxBarButton
      Tag = 4
      Caption = '25 x 25'
      Category = 10
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iD50: TdxBarButton
      Tag = 5
      Caption = '30 x 30'
      Category = 10
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iDNone: TdxBarButton
      Caption = 'None'
      Category = 11
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iDArrow: TdxBarButton
      Tag = 1
      Caption = 'Arrow'
      Category = 11
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iDOvalArrow: TdxBarButton
      Tag = 2
      Caption = 'Ellipse Arrow'
      Category = 11
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iDRectArrow: TdxBarButton
      Tag = 3
      Caption = 'Rect Arrow'
      Category = 11
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iDP1: TdxBarButton
      Caption = 'Point 1'
      Category = 12
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iDP2: TdxBarButton
      Tag = 1
      Caption = 'Point 2'
      Category = 12
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iDP3: TdxBarButton
      Tag = 2
      Caption = 'Point 3'
      Category = 12
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iDP4: TdxBarButton
      Tag = 3
      Caption = 'Point 4'
      Category = 12
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iDP5: TdxBarButton
      Tag = 4
      Caption = 'Point 5'
      Category = 12
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iDP6: TdxBarButton
      Tag = 5
      Caption = 'Point 6'
      Category = 12
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iDP7: TdxBarButton
      Tag = 6
      Caption = 'Point 7'
      Category = 12
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iDP8: TdxBarButton
      Tag = 7
      Caption = 'Point 8'
      Category = 12
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iDP9: TdxBarButton
      Tag = 8
      Caption = 'Point 9'
      Category = 12
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iDP10: TdxBarButton
      Tag = 9
      Caption = 'Point 10'
      Category = 12
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iDP11: TdxBarButton
      Tag = 10
      Caption = 'Point 11'
      Category = 12
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iDP12: TdxBarButton
      Tag = 11
      Caption = 'Point 12'
      Category = 12
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iDP13: TdxBarButton
      Tag = 12
      Caption = 'Point 13'
      Category = 12
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iDP14: TdxBarButton
      Tag = 13
      Caption = 'Point 14'
      Category = 12
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iDP15: TdxBarButton
      Tag = 14
      Caption = 'Point 15'
      Category = 12
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iDP16: TdxBarButton
      Tag = 15
      Caption = 'Point 16'
      Category = 12
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iImageTopLeft: TdxBarButton
      Caption = 'Top-Left'
      Category = 13
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iImageTop: TdxBarButton
      Tag = 1
      Caption = 'Top'
      Category = 13
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iImageTopRight: TdxBarButton
      Tag = 2
      Caption = 'Top-Right'
      Category = 13
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iImageLeft: TdxBarButton
      Tag = 3
      Caption = 'Left'
      Category = 13
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iImageCenter: TdxBarButton
      Tag = 4
      Caption = 'Center'
      Category = 13
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iImageRight: TdxBarButton
      Tag = 5
      Caption = 'Right'
      Category = 13
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iImageBottomLeft: TdxBarButton
      Tag = 6
      Caption = 'Bottom-Left'
      Category = 13
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iImageBottom: TdxBarButton
      Tag = 7
      Caption = 'Bottom'
      Category = 13
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iImageBottomRight: TdxBarButton
      Tag = 8
      Caption = 'Bottom-Right'
      Category = 13
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object i1p: TdxBarButton
      Tag = 1
      Caption = '1 pixel'
      Category = 14
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object i2p: TdxBarButton
      Tag = 2
      Caption = '2 pixels'
      Category = 14
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object i3p: TdxBarButton
      Tag = 3
      Caption = '3 pixels'
      Category = 14
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object i4p: TdxBarButton
      Tag = 4
      Caption = '4 pixels'
      Category = 14
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object i5p: TdxBarButton
      Tag = 5
      Caption = '5 pixels'
      Category = 14
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object i6p: TdxBarButton
      Tag = 6
      Caption = '6 pixels'
      Category = 14
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object i7p: TdxBarButton
      Tag = 7
      Caption = '7 pixels'
      Category = 14
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object i8p: TdxBarButton
      Tag = 8
      Caption = '8 pixels'
      Category = 14
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object i9p: TdxBarButton
      Tag = 9
      Caption = '9 pixels'
      Category = 14
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object i10p: TdxBarButton
      Tag = 10
      Caption = '10 pixels'
      Category = 14
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iS10: TdxBarButton
      Tag = 1
      Caption = '10 x 10'
      Category = 15
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iS20: TdxBarButton
      Tag = 2
      Caption = '15 x 15'
      Category = 15
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iS30: TdxBarButton
      Tag = 3
      Caption = '20 x 20'
      Category = 15
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iS40: TdxBarButton
      Tag = 4
      Caption = '25 x 25'
      Category = 15
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iS50: TdxBarButton
      Tag = 5
      Caption = '30 x 30'
      Category = 15
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iSNone: TdxBarButton
      Caption = 'None'
      Category = 16
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iSArrow: TdxBarButton
      Tag = 1
      Caption = 'Arrow'
      Category = 16
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iSOvalArrow: TdxBarButton
      Tag = 2
      Caption = 'Ellipse Arrow'
      Category = 16
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iSRectArrow: TdxBarButton
      Tag = 3
      Caption = 'Rect Arrow'
      Category = 16
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iSP1: TdxBarButton
      Caption = 'Point 1'
      Category = 17
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iSP2: TdxBarButton
      Tag = 1
      Caption = 'Point 2'
      Category = 17
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iSP3: TdxBarButton
      Tag = 2
      Caption = 'Point 3'
      Category = 17
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iSP4: TdxBarButton
      Tag = 3
      Caption = 'Point 4'
      Category = 17
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iSP5: TdxBarButton
      Tag = 4
      Caption = 'Point 5'
      Category = 17
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iSP6: TdxBarButton
      Tag = 5
      Caption = 'Point 6'
      Category = 17
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iSP7: TdxBarButton
      Tag = 6
      Caption = 'Point 7'
      Category = 17
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iSP8: TdxBarButton
      Tag = 7
      Caption = 'Point 8'
      Category = 17
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iSP9: TdxBarButton
      Tag = 8
      Caption = 'Point 9'
      Category = 17
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iSP10: TdxBarButton
      Tag = 9
      Caption = 'Point 10'
      Category = 17
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iSP11: TdxBarButton
      Tag = 10
      Caption = 'Point 11'
      Category = 17
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iSP12: TdxBarButton
      Tag = 11
      Caption = 'Point 12'
      Category = 17
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iSP13: TdxBarButton
      Tag = 12
      Caption = 'Point 13'
      Category = 17
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iSP14: TdxBarButton
      Tag = 13
      Caption = 'Point 14'
      Category = 17
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iSP15: TdxBarButton
      Tag = 14
      Caption = 'Point 15'
      Category = 17
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iSP16: TdxBarButton
      Tag = 15
      Caption = 'Point 16'
      Category = 17
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iStraight: TdxBarButton
      Tag = 1
      Caption = 'Straight'
      Category = 18
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iCurved: TdxBarButton
      Tag = 2
      Caption = 'Curved'
      Category = 18
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iRectHorizontal: TdxBarButton
      Tag = 3
      Caption = 'Rect Horizontal'
      Category = 18
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iRectVertical: TdxBarButton
      Tag = 4
      Caption = 'Rect Vertical'
      Category = 18
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iTextTopLeft: TdxBarButton
      Caption = 'Top-Left'
      Category = 19
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iTextTop: TdxBarButton
      Tag = 1
      Caption = 'Top'
      Category = 19
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iTextTopRight: TdxBarButton
      Tag = 2
      Caption = 'Top-Right'
      Category = 19
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iTextLeft: TdxBarButton
      Tag = 3
      Caption = 'Left'
      Category = 19
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iTextCenter: TdxBarButton
      Tag = 4
      Caption = 'Center'
      Category = 19
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iTextRight: TdxBarButton
      Tag = 5
      Caption = 'Right'
      Category = 19
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iTextBottomLeft: TdxBarButton
      Tag = 6
      Caption = 'Bottom-Left'
      Category = 19
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iTextBottom: TdxBarButton
      Tag = 7
      Caption = 'Bottom'
      Category = 19
      Visible = ivAlways
      OnClick = iRectangleClick
    end
    object iTextBottomRight: TdxBarButton
      Tag = 8
      Caption = 'Bottom-Right'
      Category = 19
      Visible = ivAlways
      OnClick = iRectangleClick
    end
  end
  object bpmShape: TdxBarPopupMenu
    BarManager = dxBarManager1
    Images = ilSmallImages
    ItemLinks = <
      item
        Visible = True
        ItemName = 'iNone'
      end
      item
        Visible = True
        ItemName = 'iRectangle'
      end
      item
        Visible = True
        ItemName = 'iEllipse'
      end
      item
        Visible = True
        ItemName = 'iRoundRect'
      end
      item
        Visible = True
        ItemName = 'iDiamond'
      end
      item
        Visible = True
        ItemName = 'iNorthTriangle'
      end
      item
        Visible = True
        ItemName = 'itSouthTriangle'
      end
      item
        Visible = True
        ItemName = 'itEastTriangle'
      end
      item
        Visible = True
        ItemName = 'itWestTriangle'
      end
      item
        Visible = True
        ItemName = 'itHexagon'
      end>
    UseOwnFont = False
    Left = 96
    Top = 112
    PixelsPerInch = 96
  end
  object bpmChart: TdxBarPopupMenu
    BarManager = dxBarManager1
    Images = ilLargeImages
    ItemLinks = <
      item
        Visible = True
        ItemName = 'iEdit'
      end
      item
        BeginGroup = True
        Visible = True
        ItemName = 'iCut'
      end
      item
        Visible = True
        ItemName = 'iCopy'
      end
      item
        Visible = True
        ItemName = 'iPaste'
      end
      item
        Visible = True
        ItemName = 'iDelete'
      end
      item
        Visible = True
        ItemName = 'iRemovePoint'
      end
      item
        BeginGroup = True
        Visible = True
        ItemName = 'iSelectAll'
      end
      item
        Visible = True
        ItemName = 'iClearSelection'
      end
      item
        BeginGroup = True
        Visible = True
        ItemName = 'iBringToFront'
      end
      item
        Visible = True
        ItemName = 'iSendToBack'
      end
      item
        BeginGroup = True
        Visible = True
        ItemName = 'iNewUnion'
      end
      item
        Visible = True
        ItemName = 'iAddToUnion'
      end
      item
        Visible = True
        ItemName = 'iRemoveFromUnion'
      end
      item
        Visible = True
        ItemName = 'iClearUnion'
      end
      item
        Visible = True
        ItemName = 'iClearAllUnions'
      end>
    UseOwnFont = False
    OnPopup = ChartPopupMenuPopup
    Left = 136
    Top = 112
    PixelsPerInch = 96
  end
  object bpmDArrowSize: TdxBarPopupMenu
    BarManager = dxBarManager1
    Images = ilSmallImages
    ItemLinks = <
      item
        Visible = True
        ItemName = 'iD10'
      end
      item
        Visible = True
        ItemName = 'iD20'
      end
      item
        Visible = True
        ItemName = 'iD30'
      end
      item
        Visible = True
        ItemName = 'iD40'
      end
      item
        Visible = True
        ItemName = 'iD50'
      end>
    UseOwnFont = False
    Left = 176
    Top = 112
    PixelsPerInch = 96
  end
  object bpmDestArrow: TdxBarPopupMenu
    BarManager = dxBarManager1
    Images = ilSmallImages
    ItemLinks = <
      item
        Visible = True
        ItemName = 'iDNone'
      end
      item
        Visible = True
        ItemName = 'iDArrow'
      end
      item
        Visible = True
        ItemName = 'iDOvalArrow'
      end
      item
        Visible = True
        ItemName = 'iDRectArrow'
      end>
    UseOwnFont = False
    Left = 216
    Top = 112
    PixelsPerInch = 96
  end
  object bpmDPoint: TdxBarPopupMenu
    BarManager = dxBarManager1
    Images = ilSmallImages
    ItemLinks = <
      item
        Visible = True
        ItemName = 'iDP1'
      end
      item
        Visible = True
        ItemName = 'iDP2'
      end
      item
        Visible = True
        ItemName = 'iDP3'
      end
      item
        Visible = True
        ItemName = 'iDP4'
      end
      item
        Visible = True
        ItemName = 'iDP5'
      end
      item
        Visible = True
        ItemName = 'iDP6'
      end
      item
        Visible = True
        ItemName = 'iDP7'
      end
      item
        Visible = True
        ItemName = 'iDP8'
      end
      item
        Visible = True
        ItemName = 'iDP9'
      end
      item
        Visible = True
        ItemName = 'iDP10'
      end
      item
        Visible = True
        ItemName = 'iDP11'
      end
      item
        Visible = True
        ItemName = 'iDP12'
      end
      item
        Visible = True
        ItemName = 'iDP13'
      end
      item
        Visible = True
        ItemName = 'iDP14'
      end
      item
        Visible = True
        ItemName = 'iDP15'
      end
      item
        Visible = True
        ItemName = 'iDP16'
      end>
    UseOwnFont = False
    Left = 256
    Top = 112
    PixelsPerInch = 96
  end
  object bpmImagePosition: TdxBarPopupMenu
    BarManager = dxBarManager1
    Images = ilSmallImages
    ItemLinks = <
      item
        Visible = True
        ItemName = 'iImageTopLeft'
      end
      item
        Visible = True
        ItemName = 'iImageTop'
      end
      item
        Visible = True
        ItemName = 'iImageTopRight'
      end
      item
        Visible = True
        ItemName = 'iImageLeft'
      end
      item
        Visible = True
        ItemName = 'iImageCenter'
      end
      item
        Visible = True
        ItemName = 'iImageRight'
      end
      item
        Visible = True
        ItemName = 'iImageBottomLeft'
      end
      item
        Visible = True
        ItemName = 'iImageBottom'
      end
      item
        Visible = True
        ItemName = 'iImageBottomRight'
      end>
    UseOwnFont = False
    Left = 296
    Top = 112
    PixelsPerInch = 96
  end
  object bpmLine: TdxBarPopupMenu
    BarManager = dxBarManager1
    Images = ilSmallImages
    ItemLinks = <
      item
        Visible = True
        ItemName = 'i1p'
      end
      item
        Visible = True
        ItemName = 'i2p'
      end
      item
        Visible = True
        ItemName = 'i3p'
      end
      item
        Visible = True
        ItemName = 'i4p'
      end
      item
        Visible = True
        ItemName = 'i5p'
      end
      item
        Visible = True
        ItemName = 'i6p'
      end
      item
        Visible = True
        ItemName = 'i7p'
      end
      item
        Visible = True
        ItemName = 'i8p'
      end
      item
        Visible = True
        ItemName = 'i9p'
      end
      item
        Visible = True
        ItemName = 'i10p'
      end>
    UseOwnFont = False
    Left = 336
    Top = 112
    PixelsPerInch = 96
  end
  object bpmSArrowSize: TdxBarPopupMenu
    BarManager = dxBarManager1
    Images = ilSmallImages
    ItemLinks = <
      item
        Visible = True
        ItemName = 'iS10'
      end
      item
        Visible = True
        ItemName = 'iS20'
      end
      item
        Visible = True
        ItemName = 'iS30'
      end
      item
        Visible = True
        ItemName = 'iS40'
      end
      item
        Visible = True
        ItemName = 'iS50'
      end>
    UseOwnFont = False
    Left = 376
    Top = 112
    PixelsPerInch = 96
  end
  object bpmSourceArrow: TdxBarPopupMenu
    BarManager = dxBarManager1
    Images = ilSmallImages
    ItemLinks = <
      item
        Visible = True
        ItemName = 'iSNone'
      end
      item
        Visible = True
        ItemName = 'iSArrow'
      end
      item
        Visible = True
        ItemName = 'iSOvalArrow'
      end
      item
        Visible = True
        ItemName = 'iSRectArrow'
      end>
    UseOwnFont = False
    Left = 416
    Top = 112
    PixelsPerInch = 96
  end
  object bpmSPoint: TdxBarPopupMenu
    BarManager = dxBarManager1
    Images = ilSmallImages
    ItemLinks = <
      item
        Visible = True
        ItemName = 'iSP1'
      end
      item
        Visible = True
        ItemName = 'iSP2'
      end
      item
        Visible = True
        ItemName = 'iSP3'
      end
      item
        Visible = True
        ItemName = 'iSP4'
      end
      item
        Visible = True
        ItemName = 'iSP5'
      end
      item
        Visible = True
        ItemName = 'iSP6'
      end
      item
        Visible = True
        ItemName = 'iSP7'
      end
      item
        Visible = True
        ItemName = 'iSP8'
      end
      item
        Visible = True
        ItemName = 'iSP9'
      end
      item
        Visible = True
        ItemName = 'iSP10'
      end
      item
        Visible = True
        ItemName = 'iSP11'
      end
      item
        Visible = True
        ItemName = 'iSP12'
      end
      item
        Visible = True
        ItemName = 'iSP13'
      end
      item
        Visible = True
        ItemName = 'iSP14'
      end
      item
        Visible = True
        ItemName = 'iSP15'
      end
      item
        Visible = True
        ItemName = 'iSP16'
      end>
    UseOwnFont = False
    Left = 456
    Top = 112
    PixelsPerInch = 96
  end
  object bpmStyle: TdxBarPopupMenu
    BarManager = dxBarManager1
    Images = ilSmallImages
    ItemLinks = <
      item
        Visible = True
        ItemName = 'iStraight'
      end
      item
        Visible = True
        ItemName = 'iCurved'
      end
      item
        Visible = True
        ItemName = 'iRectHorizontal'
      end
      item
        Visible = True
        ItemName = 'iRectVertical'
      end>
    UseOwnFont = False
    Left = 496
    Top = 112
    PixelsPerInch = 96
  end
  object bpmTextPosition: TdxBarPopupMenu
    BarManager = dxBarManager1
    Images = ilSmallImages
    ItemLinks = <
      item
        Visible = True
        ItemName = 'iTextTopLeft'
      end
      item
        Visible = True
        ItemName = 'iTextTop'
      end
      item
        Visible = True
        ItemName = 'iTextTopRight'
      end
      item
        Visible = True
        ItemName = 'iTextLeft'
      end
      item
        Visible = True
        ItemName = 'iTextCenter'
      end
      item
        Visible = True
        ItemName = 'iTextRight'
      end
      item
        Visible = True
        ItemName = 'iTextBottomLeft'
      end
      item
        Visible = True
        ItemName = 'iTextBottom'
      end
      item
        Visible = True
        ItemName = 'iTextBottomRight'
      end>
    UseOwnFont = False
    Left = 536
    Top = 112
    PixelsPerInch = 96
  end
end
