object frmHSIEditor: TfrmHSIEditor
  Left = 360
  Top = 129
  Caption = 'HotspotImage editor'
  ClientHeight = 478
  ClientWidth = 778
  Color = clBtnFace
  Constraints.MinHeight = 458
  Constraints.MinWidth = 540
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Shape1: TShape
    Left = 352
    Top = 224
    Width = 65
    Height = 65
  end
  object pnImage: TPanel
    Left = 0
    Top = 31
    Width = 778
    Height = 408
    Align = alClient
    BevelOuter = bvNone
    BorderStyle = bsSingle
    TabOrder = 1
    object Splitter: TSplitter
      Left = 290
      Top = 0
      Width = 4
      Height = 404
      AutoSnap = False
      Color = 13160664
      MinSize = 233
      ParentColor = False
      ResizeStyle = rsLine
    end
    object pnContainer: TPanel
      Left = 294
      Top = 0
      Width = 480
      Height = 404
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object pnProperties: TPanel
        Left = 0
        Top = 0
        Width = 480
        Height = 404
        Align = alClient
        BevelInner = bvLowered
        Enabled = False
        TabOrder = 0
        DesignSize = (
          480
          404)
        object lblHint: TLabel
          Left = 9
          Top = 107
          Width = 22
          Height = 13
          Caption = 'Hint:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lblName: TLabel
          Left = 9
          Top = 30
          Width = 31
          Height = 13
          Caption = '&Name:'
          FocusControl = edtName
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lblID: TLabel
          Left = 9
          Top = 57
          Width = 14
          Height = 13
          Caption = '&ID:'
          FocusControl = edtID
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbAngle: TLabel
          Left = 157
          Top = 56
          Width = 30
          Height = 13
          Caption = '&Angle:'
          FocusControl = spAngle
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label8: TLabel
          Left = 9
          Top = 83
          Width = 41
          Height = 13
          Caption = 'Offset X:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label9: TLabel
          Left = 157
          Top = 83
          Width = 41
          Height = 13
          Caption = 'Offset Y:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object pnTitle: TPanel
          Left = 2
          Top = 2
          Width = 476
          Height = 18
          Align = alTop
          BevelOuter = bvNone
          Caption = 'Hotspot Properties'
          Color = clInactiveCaption
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
        end
        object txtHint: TMemo
          Left = 57
          Top = 108
          Width = 384
          Height = 53
          Anchors = [akLeft, akTop, akRight]
          ScrollBars = ssVertical
          TabOrder = 5
        end
        object edtName: TEdit
          Left = 57
          Top = 24
          Width = 121
          Height = 21
          Hint = 'Enter the Name of the HotSpot'
          MaxLength = 50
          TabOrder = 1
          OnMouseUp = edtNameMouseUp
        end
        object edtID: TEdit
          Left = 57
          Top = 52
          Width = 30
          Height = 21
          Hint = 'Enter the ID of the HotSpot'
          MaxLength = 10
          TabOrder = 2
          OnKeyPress = edtIDKeyPress
          OnMouseUp = edtIDMouseUp
        end
        object spAngle: TSpinEdit
          Left = 206
          Top = 52
          Width = 47
          Height = 22
          Hint = 'Enter the Angle of the HotSpot'
          MaxLength = 3
          MaxValue = 180
          MinValue = -180
          TabOrder = 3
          Value = 0
          OnChange = spAngleChange
        end
        object ckClip: TCheckBox
          Left = 262
          Top = 24
          Width = 57
          Height = 17
          Hint = 
            'Check if you want the shape of the hotspot to clip the hover&cli' +
            'ck images'
          Caption = '&Clipped'
          Checked = True
          State = cbChecked
          TabOrder = 4
          OnClick = ckClipClick
        end
        object PageControl1: TPageControl
          Left = 7
          Top = 165
          Width = 441
          Height = 245
          ActivePage = ts_Click
          Anchors = [akLeft, akTop, akRight, akBottom]
          TabOrder = 6
          object ts_Click: TTabSheet
            Caption = 'Click'
            DesignSize = (
              433
              217)
            object lblClick: TLabel
              Left = 8
              Top = 10
              Width = 86
              Height = 13
              Caption = 'Clicked picture'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object lbl_ClickImgIndex: TLabel
              Left = 9
              Top = 183
              Width = 58
              Height = 13
              Anchors = [akBottom]
              Caption = 'ImageIndex:'
            end
            object Label1: TLabel
              Left = 254
              Top = 183
              Width = 52
              Height = 13
              Anchors = [akLeft, akBottom]
              Caption = 'Click color:'
            end
            object Shape_ClickColor: TShape
              Left = 343
              Top = 174
              Width = 42
              Height = 21
              Anchors = [akLeft, akBottom]
            end
            object btLoadClick: TButton
              Left = 328
              Top = 6
              Width = 57
              Height = 21
              Anchors = [akTop, akRight]
              Caption = 'Load...'
              TabOrder = 0
              OnClick = btLoadClickClick
            end
            object pnClickImage: TPanel
              Left = 1
              Top = 32
              Width = 430
              Height = 132
              Anchors = [akLeft, akTop, akRight, akBottom]
              BevelOuter = bvLowered
              TabOrder = 1
              object imgClick: TImage
                Left = 1
                Top = 1
                Width = 428
                Height = 130
                Align = alClient
                Center = True
                PopupMenu = pmClickImage
              end
            end
            object Pn_ClickColor: TPanel
              Left = 321
              Top = 175
              Width = 20
              Height = 20
              Anchors = [akLeft, akBottom]
              Ctl3D = True
              ParentCtl3D = False
              TabOrder = 2
              OnClick = Pn_ClickColorClick
            end
            object cmb_ClickImgIndex: TComboBox
              Left = 95
              Top = 179
              Width = 121
              Height = 19
              Style = csOwnerDrawFixed
              Anchors = [akLeft, akBottom]
              ItemHeight = 13
              TabOrder = 3
              OnChange = cmb_ClickImgIndexChange
              OnDrawItem = cmb_BlinkImgIndexDrawItem
            end
            object btClearClick: TButton
              Left = 392
              Top = 6
              Width = 41
              Height = 21
              Anchors = [akTop, akRight]
              Caption = 'Clear'
              TabOrder = 4
              OnClick = btClearClickClick
            end
            object bt_ClearClickColor: TButton
              Left = 321
              Top = 197
              Width = 65
              Height = 19
              Anchors = [akLeft, akBottom]
              Caption = 'Clear'
              TabOrder = 5
              OnClick = bt_ClearClickColorClick
            end
          end
          object ts_Hover: TTabSheet
            Caption = 'Hover'
            ImageIndex = 1
            DesignSize = (
              433
              217)
            object lbHover: TLabel
              Left = 8
              Top = 10
              Width = 78
              Height = 13
              Caption = 'Hover picture'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object Label2: TLabel
              Left = 9
              Top = 183
              Width = 58
              Height = 13
              Anchors = [akBottom]
              Caption = 'ImageIndex:'
            end
            object Label3: TLabel
              Left = 251
              Top = 183
              Width = 58
              Height = 13
              Anchors = [akLeft, akBottom]
              Caption = 'Hover color:'
            end
            object Shape_HoverColor: TShape
              Left = 343
              Top = 174
              Width = 42
              Height = 21
              Anchors = [akLeft, akBottom]
            end
            object pnHoverImage: TPanel
              Left = 1
              Top = 32
              Width = 429
              Height = 132
              Anchors = [akLeft, akTop, akRight, akBottom]
              BevelOuter = bvLowered
              TabOrder = 0
              object imgHover: TImage
                Left = 1
                Top = 1
                Width = 427
                Height = 130
                Align = alClient
                Center = True
                PopupMenu = pmHoverImage
              end
            end
            object btLoadHover: TButton
              Left = 328
              Top = 6
              Width = 57
              Height = 21
              Anchors = [akTop, akRight]
              Caption = 'Load...'
              TabOrder = 1
              OnClick = btLoadHoverClick
            end
            object cmb_HoverImgIndex: TComboBox
              Left = 95
              Top = 179
              Width = 121
              Height = 22
              Style = csOwnerDrawFixed
              Anchors = [akLeft, akBottom]
              TabOrder = 2
              OnChange = cmb_HoverImgIndexChange
              OnDrawItem = cmb_BlinkImgIndexDrawItem
            end
            object Pn_HoverColor: TPanel
              Left = 321
              Top = 175
              Width = 20
              Height = 20
              Anchors = [akLeft, akBottom]
              TabOrder = 3
              OnClick = Pn_HoverColorClick
            end
            object btClearHover: TButton
              Left = 392
              Top = 6
              Width = 41
              Height = 21
              Anchors = [akTop, akRight]
              Caption = 'Clear'
              TabOrder = 4
              OnClick = btClearHoverClick
            end
            object bt_ClearHoverColor: TButton
              Left = 321
              Top = 197
              Width = 65
              Height = 19
              Anchors = [akLeft, akBottom]
              Caption = 'Clear'
              TabOrder = 5
              OnClick = bt_ClearHoverColorClick
            end
          end
          object ts_Seletced: TTabSheet
            Caption = 'Selected'
            ImageIndex = 2
            DesignSize = (
              433
              217)
            object lbl_Selected: TLabel
              Left = 8
              Top = 10
              Width = 94
              Height = 13
              Caption = 'Selected picture'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object Label4: TLabel
              Left = 9
              Top = 183
              Width = 58
              Height = 13
              Anchors = [akBottom]
              Caption = 'ImageIndex:'
            end
            object Label5: TLabel
              Left = 244
              Top = 183
              Width = 71
              Height = 13
              Anchors = [akLeft, akBottom]
              Caption = 'Selected color:'
            end
            object Shape_SelectedColor: TShape
              Left = 343
              Top = 174
              Width = 42
              Height = 21
              Anchors = [akLeft, akBottom]
            end
            object Btn_Selected: TButton
              Left = 328
              Top = 6
              Width = 57
              Height = 21
              Anchors = [akTop, akRight]
              Caption = 'Load...'
              TabOrder = 0
              OnClick = Btn_SelectedClick
            end
            object Pn_Selected: TPanel
              Left = 1
              Top = 32
              Width = 430
              Height = 132
              Anchors = [akLeft, akTop, akRight, akBottom]
              BevelOuter = bvLowered
              TabOrder = 1
              object Img_Selected: TImage
                Left = 1
                Top = 1
                Width = 428
                Height = 130
                Align = alClient
                Center = True
                PopupMenu = pm_SelectedImage
              end
            end
            object cmb_SelectImgIndex: TComboBox
              Left = 95
              Top = 179
              Width = 121
              Height = 22
              Style = csOwnerDrawFixed
              Anchors = [akLeft, akBottom]
              TabOrder = 2
              OnChange = cmb_SelectImgIndexChange
              OnDrawItem = cmb_BlinkImgIndexDrawItem
            end
            object Pn_SelectedColor: TPanel
              Left = 321
              Top = 175
              Width = 20
              Height = 20
              Anchors = [akLeft, akBottom]
              TabOrder = 3
              OnClick = Pn_SelectedColorClick
            end
            object btClearSelected: TButton
              Left = 392
              Top = 6
              Width = 41
              Height = 21
              Anchors = [akTop, akRight]
              Caption = 'Clear'
              TabOrder = 4
              OnClick = btClearSelectedClick
            end
            object bt_ClearSelectColor: TButton
              Left = 321
              Top = 197
              Width = 65
              Height = 19
              Anchors = [akLeft, akBottom]
              Caption = 'Clear'
              TabOrder = 5
              OnClick = bt_ClearSelectColorClick
            end
          end
          object ts_Blink: TTabSheet
            Caption = 'Blink'
            ImageIndex = 3
            DesignSize = (
              433
              217)
            object lbl_Blink: TLabel
              Left = 8
              Top = 10
              Width = 72
              Height = 13
              Caption = 'Blink picture'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object Label6: TLabel
              Left = 9
              Top = 183
              Width = 58
              Height = 13
              Anchors = [akBottom]
              Caption = 'ImageIndex:'
            end
            object Label7: TLabel
              Left = 254
              Top = 183
              Width = 52
              Height = 13
              Anchors = [akLeft, akBottom]
              Caption = 'Blink color:'
            end
            object Shape_BlinkColor: TShape
              Left = 343
              Top = 174
              Width = 42
              Height = 21
              Anchors = [akLeft, akBottom]
            end
            object Btn_Blink: TButton
              Left = 328
              Top = 6
              Width = 57
              Height = 21
              Anchors = [akTop, akRight]
              Caption = 'Load...'
              TabOrder = 0
              OnClick = Btn_BlinkClick
            end
            object Pn_Blink: TPanel
              Left = 1
              Top = 32
              Width = 430
              Height = 132
              Anchors = [akLeft, akTop, akRight, akBottom]
              BevelOuter = bvLowered
              TabOrder = 1
              object Img_Blink: TImage
                Left = 1
                Top = 1
                Width = 428
                Height = 130
                Align = alClient
                Center = True
                PopupMenu = pm_BlinkImage
              end
            end
            object cmb_BlinkImgIndex: TComboBox
              Left = 95
              Top = 179
              Width = 121
              Height = 19
              Style = csOwnerDrawFixed
              Anchors = [akLeft, akBottom]
              ItemHeight = 13
              TabOrder = 2
              OnChange = cmb_BlinkImgIndexChange
              OnDrawItem = cmb_BlinkImgIndexDrawItem
            end
            object Pn_BlinkColor: TPanel
              Left = 321
              Top = 175
              Width = 20
              Height = 20
              Anchors = [akLeft, akBottom]
              TabOrder = 3
              OnClick = Pn_BlinkColorClick
            end
            object btClearBlink: TButton
              Left = 392
              Top = 6
              Width = 41
              Height = 21
              Anchors = [akTop, akRight]
              Caption = 'Clear'
              TabOrder = 4
              OnClick = btClearBlinkClick
            end
            object bt_ClearBlinkColor: TButton
              Left = 321
              Top = 197
              Width = 65
              Height = 19
              Anchors = [akLeft, akBottom]
              Caption = 'Clear'
              TabOrder = 5
              OnClick = bt_ClearBlinkColorClick
            end
          end
        end
        object chk_Blink: TCheckBox
          Left = 206
          Top = 24
          Width = 49
          Height = 17
          Caption = 'Blink'
          TabOrder = 7
          OnClick = chk_BlinkClick
        end
        object se_OffsetX: TSpinEdit
          Left = 57
          Top = 80
          Width = 47
          Height = 22
          Hint = 'Enter the Angle of the HotSpot'
          MaxLength = 3
          MaxValue = 180
          MinValue = -180
          TabOrder = 8
          Value = 4
          OnChange = se_OffsetXChange
        end
        object se_OffsetY: TSpinEdit
          Left = 206
          Top = 79
          Width = 47
          Height = 22
          Hint = 'Enter the Angle of the HotSpot'
          MaxLength = 3
          MaxValue = 180
          MinValue = -180
          TabOrder = 9
          Value = 4
          OnChange = se_OffsetYChange
        end
        object ckSelectable: TCheckBox
          Left = 328
          Top = 24
          Width = 73
          Height = 17
          Caption = 'Selectable'
          Checked = True
          State = cbChecked
          TabOrder = 10
          OnClick = ckSelectableClick
        end
      end
    end
    object pnBackground: TPanel
      Left = 0
      Top = 0
      Width = 290
      Height = 404
      Align = alLeft
      BevelOuter = bvNone
      Constraints.MinWidth = 290
      TabOrder = 1
      DesignSize = (
        290
        404)
      object pnLoadBack: TPanel
        Left = 0
        Top = 363
        Width = 290
        Height = 41
        Align = alBottom
        BevelInner = bvRaised
        BevelOuter = bvNone
        TabOrder = 1
        object lbPozx: TLabel
          Left = 19
          Top = 5
          Width = 6
          Height = 13
          Caption = '0'
        end
        object lbPozy: TLabel
          Left = 19
          Top = 23
          Width = 6
          Height = 13
          Caption = '0'
        end
        object lbnX: TLabel
          Left = 7
          Top = 5
          Width = 10
          Height = 13
          Caption = 'X:'
        end
        object lbnY: TLabel
          Left = 7
          Top = 23
          Width = 10
          Height = 13
          Caption = 'Y:'
        end
        object lbNPoints: TLabel
          Left = 46
          Top = 5
          Width = 3
          Height = 13
          Visible = False
        end
      end
      object btBackImage: TButton
        Left = 139
        Top = 381
        Width = 145
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Load background picture'
        TabOrder = 0
        OnClick = btBackImageClick
      end
      object sbPicture: TScrollBox
        Left = 0
        Top = 0
        Width = 290
        Height = 363
        Align = alClient
        BorderStyle = bsNone
        TabOrder = 2
        object PB: TPaintBox
          Left = 0
          Top = 0
          Width = 290
          Height = 350
          OnMouseDown = PBMouseDown
          OnMouseMove = PBMouseMove
          OnMouseUp = PBMouseUp
          OnPaint = PBPaint
        end
      end
    end
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 778
    Height = 31
    AutoSize = True
    ButtonHeight = 27
    ButtonWidth = 26
    EdgeBorders = [ebTop, ebBottom]
    TabOrder = 0
    object btNormal: TSpeedButton
      Left = 0
      Top = 0
      Width = 26
      Height = 27
      Hint = 'Edit hot spots'
      GroupIndex = 1
      Down = True
      Flat = True
      Glyph.Data = {
        C6050000424DC605000000000000360400002800000014000000140000000100
        08000000000090010000C40E0000C40E00000001000000000000000000004000
        000080000000FF000000002000004020000080200000FF200000004000004040
        000080400000FF400000006000004060000080600000FF600000008000004080
        000080800000FF80000000A0000040A0000080A00000FFA0000000C0000040C0
        000080C00000FFC0000000FF000040FF000080FF0000FFFF0000000020004000
        200080002000FF002000002020004020200080202000FF202000004020004040
        200080402000FF402000006020004060200080602000FF602000008020004080
        200080802000FF80200000A0200040A0200080A02000FFA0200000C0200040C0
        200080C02000FFC0200000FF200040FF200080FF2000FFFF2000000040004000
        400080004000FF004000002040004020400080204000FF204000004040004040
        400080404000FF404000006040004060400080604000FF604000008040004080
        400080804000FF80400000A0400040A0400080A04000FFA0400000C0400040C0
        400080C04000FFC0400000FF400040FF400080FF4000FFFF4000000060004000
        600080006000FF006000002060004020600080206000FF206000004060004040
        600080406000FF406000006060004060600080606000FF606000008060004080
        600080806000FF80600000A0600040A0600080A06000FFA0600000C0600040C0
        600080C06000FFC0600000FF600040FF600080FF6000FFFF6000000080004000
        800080008000FF008000002080004020800080208000FF208000004080004040
        800080408000FF408000006080004060800080608000FF608000008080004080
        800080808000FF80800000A0800040A0800080A08000FFA0800000C0800040C0
        800080C08000FFC0800000FF800040FF800080FF8000FFFF80000000A0004000
        A0008000A000FF00A0000020A0004020A0008020A000FF20A0000040A0004040
        A0008040A000FF40A0000060A0004060A0008060A000FF60A0000080A0004080
        A0008080A000FF80A00000A0A00040A0A00080A0A000FFA0A00000C0A00040C0
        A00080C0A000FFC0A00000FFA00040FFA00080FFA000FFFFA0000000C0004000
        C0008000C000FF00C0000020C0004020C0008020C000FF20C0000040C0004040
        C0008040C000FF40C0000060C0004060C0008060C000FF60C0000080C0004080
        C0008080C000FF80C00000A0C00040A0C00080A0C000FFA0C00000C0C00040C0
        C00080C0C000FFC0C00000FFC00040FFC00080FFC000FFFFC0000000FF004000
        FF008000FF00FF00FF000020FF004020FF008020FF00FF20FF000040FF004040
        FF008040FF00FF40FF000060FF004060FF008060FF00FF60FF000080FF004080
        FF008080FF00FF80FF0000A0FF0040A0FF0080A0FF00FFA0FF0000C0FF0040C0
        FF0080C0FF00FFC0FF0000FFFF0040FFFF0080FFFF00FFFFFF00E3E3E3E3E3E3
        E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3
        E3E3E3E3E3E3E3E3E3E3E3E30000E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E300
        FFFF00E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E300FFFF00E3E3E3E3E3E3E3E3E3
        E3E300E3E3E300FFFF00E3E3E3E3E3E3E3E3E3E3E3E30000E3E300FFFF00E3E3
        E3E3E3E3E3E3E3E3E3E300FF0000FFFF00E3E3E3E3E3E3E3E3E3E3E3E3E300FF
        FFFFFFFF00E3E3E3E3E3E3E3E3E3E3E3E3E300FFFFFFFFFF00000000E3E3E3E3
        E3E3E3E3E3E300FFFFFFFFFFFFFF00E3E3E3E3E3E3E3E3E3E3E300FFFFFFFFFF
        FF00E3E3E3E3E3E3E3E3E3E3E3E300FFFFFFFFFF00E3E3E3E3E3E3E3E3E3E3E3
        E3E300FFFFFFFF00E3E3E3E3E3E3E3E3E3E3E3E3E3E300FFFFFF00E3E3E3E3E3
        E3E3E3E3E3E3E3E3E3E300FFFF00E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E300FF
        00E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E30000E3E3E3E3E3E3E3E3E3E3E3E3
        E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3
        E3E3E3E3E3E3E3E3E3E3}
      OnClick = btNormalClick
    end
    object btRect: TSpeedButton
      Left = 26
      Top = 0
      Width = 26
      Height = 27
      Hint = 'Create new rectangular hot spot'
      GroupIndex = 1
      Flat = True
      Glyph.Data = {
        66010000424D6601000000000000760000002800000014000000140000000100
        040000000000F0000000C40E0000C40E00001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DD0000000000
        000000000000DD0DDDDDDDDDDDDDDDD00000DD0DDDDDDDDDDDDDDDD00000DD0D
        DDDDDDDDDDDDDDD00000DD0DDDDDDDDDDDDDDDD00000DD0DD000000000000DD0
        0000DD0DD0DDDDDDDDDD0DD00000DD0DD0DDDDDDDDDD0DD00000DD0DD0DDDDDD
        DDDD0DD00000DD0000000000000000000000DDDDD0DDDDDDDDDD0DDD0000DDDD
        D0DDDDDDDDDD0DDD00000000000000000DDD0DDD00000DDDD0DDDDDD0DDD0DDD
        00000DDDD0DDDDDD0DDD0DDD00000DDDD0DDDDDD0DDD0DDD00000DDDD0000000
        00000DDD00000DDDDDDDDDDD0DDDDDDD00000DDDDDDDDDDD0DDDDDDD00000000
        000000000DDDDDDD0000}
      OnClick = btRectClick
    end
    object btEllipse: TSpeedButton
      Left = 52
      Top = 0
      Width = 26
      Height = 27
      Hint = 'Create new elliptical hot spot'
      GroupIndex = 1
      Flat = True
      Glyph.Data = {
        66010000424D6601000000000000760000002800000014000000140000000100
        040000000000F0000000C40E0000C40E00001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
        DDDDDDDD0000DDDDDDDDDDDDDDDDDDDD0000DDDDDDD000000DDDDDDD0000DDDD
        D00DDDDDD00DDDDD0000DDD00DDDDDDDDDD00DDD0000DD0DDDDDDDDDDDDDD0DD
        0000DD0DDDDDDDDDDDDDD0DD0000D0DDDDDDDDDDDDDDDD0D0000D0DDDDDDDDDD
        DDDDDD0D0000D0DDDDDDDDDDDDDDDD0D0000D0DDDDDDDDDDDDDDDD0D0000D0DD
        DDDDDDDDDDDDDD0D0000D0DDDDDDDDDDDDDDDD0D0000DD0DDDDDDDDDDDDDD0DD
        0000DD0DDDDDDDDDDDDDD0DD0000DDD00DDDDDDDDDD00DDD0000DDDDD00DDDDD
        D00DDDDD0000DDDDDDD000000DDDDDDD0000DDDDDDDDDDDDDDDDDDDD0000DDDD
        DDDDDDDDDDDDDDDD0000}
      OnClick = btEllipseClick
    end
    object btPoly: TSpeedButton
      Left = 78
      Top = 0
      Width = 28
      Height = 27
      Hint = 'Create new polygonal hot spot'
      GroupIndex = 1
      Flat = True
      Glyph.Data = {
        62050000424D62050000000000003604000028000000130000000F0000000100
        0800000000002C01000000000000000000000001000000000000000000000101
        0100020202000303030004040400050505000606060007070700080808000909
        09000A0A0A000B0B0B000C0C0C000D0D0D000E0E0E000F0F0F00101010001111
        1100121212001313130014141400151515001616160017171700181818001919
        19001A1A1A001B1B1B001C1C1C001D1D1D001E1E1E001F1F1F00202020002121
        2100222222002323230024242400252525002626260027272700282828002929
        29002A2A2A002B2B2B002C2C2C002D2D2D002E2E2E002F2F2F00303030003131
        3100323232003333330034343400353535003636360037373700383838003939
        39003A3A3A003B3B3B003C3C3C003D3D3D003E3E3E003F3F3F00404040004141
        4100424242004343430044444400454545004646460047474700484848004949
        49004A4A4A004B4B4B004C4C4C004D4D4D004E4E4E004F4F4F00505050005151
        5100525252005353530054545400555555005656560057575700585858005959
        59005A5A5A005B5B5B005C5C5C005D5D5D005E5E5E005F5F5F00606060006161
        6100626262006363630064646400656565006666660067676700686868006969
        69006A6A6A006B6B6B006C6C6C006D6D6D006E6E6E006F6F6F00707070007171
        7100727272007373730074747400757575007676760077777700787878007979
        79007A7A7A007B7B7B007C7C7C007D7D7D007E7E7E007F7F7F00808080008181
        8100828282008383830084848400858585008686860087878700888888008989
        89008A8A8A008B8B8B008C8C8C008D8D8D008E8E8E008F8F8F00909090009191
        9100929292009393930094949400959595009696960097979700989898009999
        99009A9A9A009B9B9B009C9C9C009D9D9D009E9E9E009F9F9F00A0A0A000A1A1
        A100A2A2A200A3A3A300A4A4A400A5A5A500A6A6A600A7A7A700A8A8A800A9A9
        A900AAAAAA00ABABAB00ACACAC00ADADAD00AEAEAE00AFAFAF00B0B0B000B1B1
        B100B2B2B200B3B3B300B4B4B400B5B5B500B6B6B600B7B7B700B8B8B800B9B9
        B900BABABA00BBBBBB00BCBCBC00BDBDBD00BEBEBE00BFBFBF00C0C0C000C1C1
        C100C2C2C200C3C3C300C4C4C400C5C5C500C6C6C600C7C7C700C8C8C800C9C9
        C900CACACA00CBCBCB00CCCCCC00CDCDCD00CECECE00CFCFCF00D0D0D000D1D1
        D100D2D2D200D3D3D300D4D4D400D5D5D500D6D6D600D7D7D700D8D8D800D9D9
        D900DADADA00DBDBDB00DCDCDC00DDDDDD00DEDEDE00DFDFDF00E0E0E000E1E1
        E100E2E2E200E3E3E300E4E4E400E5E5E500E6E6E600E7E7E700E8E8E800E9E9
        E900EAEAEA00EBEBEB00ECECEC00EDEDED00EEEEEE00EFEFEF00F0F0F000F1F1
        F100F2F2F200F3F3F300F4F4F400F5F5F500F6F6F600F7F7F700F8F8F800F9F9
        F900FAFAFA00FBFBFB00FCFCFC00FDFDFD00FEFEFE00FFFFFF00FFFFFFFFFFFF
        00000000000000FFFFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFFFF
        FF00FFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFFFFFF00FFFFFFFF00FFFFFFFFFF
        FFFFFFFF00FFFFFFFF00FFFFFFFF00FFFFFFFFFFFFFFFFFF00FFFFFFFF00FFFF
        FF00FFFFFFFFFFFFFFFFFFFFFF00FFFFFF00FFFFFF00FFFFFFFFFFFFFFFFFFFF
        FF00FFFFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFFFF00FFFF
        FFFFFFFFFFFFFFFFFF00FFFFFF00FFFFFFFF00FFFFFFFFFFFFFFFFFF00FFFFFF
        FF00FFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFFFF00FFFFFF
        FFFF00FFFFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFF00FFFFFFFFFFFFFF00FFFF
        FFFFFFFFFFFF00FF00FFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFFFF00FFFFFFFF
        FFFFFFFFFF00}
      OnClick = btPolyClick
    end
    object btWand: TSpeedButton
      Left = 106
      Top = 0
      Width = 28
      Height = 27
      Hint = 'Create new polygonal hot spot by Magic Wand selection'
      GroupIndex = 1
      Flat = True
      Glyph.Data = {
        16050000424D16050000000000003604000028000000100000000E0000000100
        080000000000E000000000000000000000000001000000000000000000000101
        0100020202000303030004040400050505000606060007070700080808000909
        09000A0A0A000B0B0B000C0C0C000D0D0D000E0E0E000F0F0F00101010001111
        1100121212001313130014141400151515001616160017171700181818001919
        19001A1A1A001B1B1B001C1C1C001D1D1D001E1E1E001F1F1F00202020002121
        2100222222002323230024242400252525002626260027272700282828002929
        29002A2A2A002B2B2B002C2C2C002D2D2D002E2E2E002F2F2F00303030003131
        3100323232003333330034343400353535003636360037373700383838003939
        39003A3A3A003B3B3B003C3C3C003D3D3D003E3E3E003F3F3F00404040004141
        4100424242004343430044444400454545004646460047474700484848004949
        49004A4A4A004B4B4B004C4C4C004D4D4D004E4E4E004F4F4F00505050005151
        5100525252005353530054545400555555005656560057575700585858005959
        59005A5A5A005B5B5B005C5C5C005D5D5D005E5E5E005F5F5F00606060006161
        6100626262006363630064646400656565006666660067676700686868006969
        69006A6A6A006B6B6B006C6C6C006D6D6D006E6E6E006F6F6F00707070007171
        7100727272007373730074747400757575007676760077777700787878007979
        79007A7A7A007B7B7B007C7C7C007D7D7D007E7E7E007F7F7F00808080008181
        8100828282008383830084848400858585008686860087878700888888008989
        89008A8A8A008B8B8B008C8C8C008D8D8D008E8E8E008F8F8F00909090009191
        9100929292009393930094949400959595009696960097979700989898009999
        99009A9A9A009B9B9B009C9C9C009D9D9D009E9E9E009F9F9F00A0A0A000A1A1
        A100A2A2A200A3A3A300A4A4A400A5A5A500A6A6A600A7A7A700A8A8A800A9A9
        A900AAAAAA00ABABAB00ACACAC00ADADAD00AEAEAE00AFAFAF00B0B0B000B1B1
        B100B2B2B200B3B3B300B4B4B400B5B5B500B6B6B600B7B7B700B8B8B800B9B9
        B900BABABA00BBBBBB00BCBCBC00BDBDBD00BEBEBE00BFBFBF00C0C0C000C1C1
        C100C2C2C200C3C3C300C4C4C400C5C5C500C6C6C600C7C7C700C8C8C800C9C9
        C900CACACA00CBCBCB00CCCCCC00CDCDCD00CECECE00CFCFCF00D0D0D000D1D1
        D100D2D2D200D3D3D300D4D4D400D5D5D500D6D6D600D7D7D700D8D8D800D9D9
        D900DADADA00DBDBDB00DCDCDC00DDDDDD00DEDEDE00DFDFDF00E0E0E000E1E1
        E100E2E2E200E3E3E300E4E4E400E5E5E500E6E6E600E7E7E700E8E8E800E9E9
        E900EAEAEA00EBEBEB00ECECEC00EDEDED00EEEEEE00EFEFEF00F0F0F000F1F1
        F100F2F2F200F3F3F300F4F4F400F5F5F500F6F6F600F7F7F700F8F8F800F9F9
        F900FAFAFA00FBFBFB00FCFCFC00FDFDFD00FEFEFE00FFFFFF00FFFFFFFFFFFF
        FFFFFFFFFFFFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFF
        FFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFF
        FFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFF00FF00FFFFFFFFFFFFFFFFFFFFFF
        FF00FF00FFFFFFFFFFFFFFFFFFFF00FFFFFF00FFFFFFFFFFFFFFFFFF00FFFFFF
        00FFFFFFFFFFFFFFFFFFFFFFFF00FF00FFFFFFFFFFFFFFFFFFFFFF00FFFF00FF
        FF00FFFFFFFFFFFFFFFFFFFFFF00FF00FFFFFFFFFFFFFFFFFFFFFFFF00FFFFFF
        00FFFFFFFFFFFFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFFFFFFFF}
      OnClick = btWandClick
    end
    object btDelete: TSpeedButton
      Left = 134
      Top = 0
      Width = 83
      Height = 27
      Hint = 'Delete Selected HotSpot (DEL)'
      Caption = 'Delete Hotspot'
      Enabled = False
      Flat = True
      OnClick = btDeleteClick
    end
    object btCopyToClipBoard: TSpeedButton
      Left = 217
      Top = 0
      Width = 80
      Height = 27
      Hint = 
        'Copies the image behind the current hotspot to clipboard (Ctrl+C' +
        ')'
      Caption = 'To Clipboard'
      Enabled = False
      Flat = True
      OnClick = btCopyToClipBoardClick
    end
    object BevelSep: TBevel
      Left = 297
      Top = 0
      Width = 4
      Height = 27
      Shape = bsLeftLine
    end
    object btAddPoint: TSpeedButton
      Left = 301
      Top = 0
      Width = 28
      Height = 27
      Hint = 'Click to Add a Point to the Polygon'
      GroupIndex = 1
      Enabled = False
      Flat = True
      Glyph.Data = {
        26050000424D260500000000000036040000280000000F0000000F0000000100
        080000000000F000000000000000000000000001000000000000000000000101
        0100020202000303030004040400050505000606060007070700080808000909
        09000A0A0A000B0B0B000C0C0C000D0D0D000E0E0E000F0F0F00101010001111
        1100121212001313130014141400151515001616160017171700181818001919
        19001A1A1A001B1B1B001C1C1C001D1D1D001E1E1E001F1F1F00202020002121
        2100222222002323230024242400252525002626260027272700282828002929
        29002A2A2A002B2B2B002C2C2C002D2D2D002E2E2E002F2F2F00303030003131
        3100323232003333330034343400353535003636360037373700383838003939
        39003A3A3A003B3B3B003C3C3C003D3D3D003E3E3E003F3F3F00404040004141
        4100424242004343430044444400454545004646460047474700484848004949
        49004A4A4A004B4B4B004C4C4C004D4D4D004E4E4E004F4F4F00505050005151
        5100525252005353530054545400555555005656560057575700585858005959
        59005A5A5A005B5B5B005C5C5C005D5D5D005E5E5E005F5F5F00606060006161
        6100626262006363630064646400656565006666660067676700686868006969
        69006A6A6A006B6B6B006C6C6C006D6D6D006E6E6E006F6F6F00707070007171
        7100727272007373730074747400757575007676760077777700787878007979
        79007A7A7A007B7B7B007C7C7C007D7D7D007E7E7E007F7F7F00808080008181
        8100828282008383830084848400858585008686860087878700888888008989
        89008A8A8A008B8B8B008C8C8C008D8D8D008E8E8E008F8F8F00909090009191
        9100929292009393930094949400959595009696960097979700989898009999
        99009A9A9A009B9B9B009C9C9C009D9D9D009E9E9E009F9F9F00A0A0A000A1A1
        A100A2A2A200A3A3A300A4A4A400A5A5A500A6A6A600A7A7A700A8A8A800A9A9
        A900AAAAAA00ABABAB00ACACAC00ADADAD00AEAEAE00AFAFAF00B0B0B000B1B1
        B100B2B2B200B3B3B300B4B4B400B5B5B500B6B6B600B7B7B700B8B8B800B9B9
        B900BABABA00BBBBBB00BCBCBC00BDBDBD00BEBEBE00BFBFBF00C0C0C000C1C1
        C100C2C2C200C3C3C300C4C4C400C5C5C500C6C6C600C7C7C700C8C8C800C9C9
        C900CACACA00CBCBCB00CCCCCC00CDCDCD00CECECE00CFCFCF00D0D0D000D1D1
        D100D2D2D200D3D3D300D4D4D400D5D5D500D6D6D600D7D7D700D8D8D800D9D9
        D900DADADA00DBDBDB00DCDCDC00DDDDDD00DEDEDE00DFDFDF00E0E0E000E1E1
        E100E2E2E200E3E3E300E4E4E400E5E5E500E6E6E600E7E7E700E8E8E800E9E9
        E900EAEAEA00EBEBEB00ECECEC00EDEDED00EEEEEE00EFEFEF00F0F0F000F1F1
        F100F2F2F200F3F3F300F4F4F400F5F5F500F6F6F600F7F7F700F8F8F800F9F9
        F900FAFAFA00FBFBFB00FCFCFC00FDFDFD00FEFEFE00FFFFFF00FFFFFF000000
        0000FFFFFFFFFFFFFF00FFFFFF0000000000FFFFFFFFFFFFFF00FFFFFF000000
        0000FFFFFFFFFFFFFF00FFFF00000000000000FFFFFFFFFFFF00FFFFFF00FFFF
        FF00FFFFFFFFFFFFFF00FFFFFF00FFFFFF00FFFFFFFFFFFFFF00FFFF00FFFF00
        FFFF00FFFFFFFFFFFF00FFFF00FFFFFFFFFF00FFFFFFFFFFFF00FFFF00FFFF00
        FFFF00FFFFFFFFFFFF00FFFFFF00FF00FF00FFFFFFFF00FFFF00FFFFFF00FF00
        FF00FFFFFFFF00FFFF00FFFFFFFF00FF00FFFFFF000000000000FFFFFFFF00FF
        00FFFFFFFFFF00FFFF00FFFFFFFFFF00FFFFFFFFFFFF00FFFF00FFFFFFFFFF00
        FFFFFFFFFFFFFFFFFF00}
    end
    object btDelPoint: TSpeedButton
      Left = 329
      Top = 0
      Width = 28
      Height = 27
      Hint = 'Click to Delete a Point from the Polygon'
      GroupIndex = 1
      Enabled = False
      Flat = True
      Glyph.Data = {
        26050000424D260500000000000036040000280000000F0000000F0000000100
        080000000000F000000000000000000000000001000000000000000000000101
        0100020202000303030004040400050505000606060007070700080808000909
        09000A0A0A000B0B0B000C0C0C000D0D0D000E0E0E000F0F0F00101010001111
        1100121212001313130014141400151515001616160017171700181818001919
        19001A1A1A001B1B1B001C1C1C001D1D1D001E1E1E001F1F1F00202020002121
        2100222222002323230024242400252525002626260027272700282828002929
        29002A2A2A002B2B2B002C2C2C002D2D2D002E2E2E002F2F2F00303030003131
        3100323232003333330034343400353535003636360037373700383838003939
        39003A3A3A003B3B3B003C3C3C003D3D3D003E3E3E003F3F3F00404040004141
        4100424242004343430044444400454545004646460047474700484848004949
        49004A4A4A004B4B4B004C4C4C004D4D4D004E4E4E004F4F4F00505050005151
        5100525252005353530054545400555555005656560057575700585858005959
        59005A5A5A005B5B5B005C5C5C005D5D5D005E5E5E005F5F5F00606060006161
        6100626262006363630064646400656565006666660067676700686868006969
        69006A6A6A006B6B6B006C6C6C006D6D6D006E6E6E006F6F6F00707070007171
        7100727272007373730074747400757575007676760077777700787878007979
        79007A7A7A007B7B7B007C7C7C007D7D7D007E7E7E007F7F7F00808080008181
        8100828282008383830084848400858585008686860087878700888888008989
        89008A8A8A008B8B8B008C8C8C008D8D8D008E8E8E008F8F8F00909090009191
        9100929292009393930094949400959595009696960097979700989898009999
        99009A9A9A009B9B9B009C9C9C009D9D9D009E9E9E009F9F9F00A0A0A000A1A1
        A100A2A2A200A3A3A300A4A4A400A5A5A500A6A6A600A7A7A700A8A8A800A9A9
        A900AAAAAA00ABABAB00ACACAC00ADADAD00AEAEAE00AFAFAF00B0B0B000B1B1
        B100B2B2B200B3B3B300B4B4B400B5B5B500B6B6B600B7B7B700B8B8B800B9B9
        B900BABABA00BBBBBB00BCBCBC00BDBDBD00BEBEBE00BFBFBF00C0C0C000C1C1
        C100C2C2C200C3C3C300C4C4C400C5C5C500C6C6C600C7C7C700C8C8C800C9C9
        C900CACACA00CBCBCB00CCCCCC00CDCDCD00CECECE00CFCFCF00D0D0D000D1D1
        D100D2D2D200D3D3D300D4D4D400D5D5D500D6D6D600D7D7D700D8D8D800D9D9
        D900DADADA00DBDBDB00DCDCDC00DDDDDD00DEDEDE00DFDFDF00E0E0E000E1E1
        E100E2E2E200E3E3E300E4E4E400E5E5E500E6E6E600E7E7E700E8E8E800E9E9
        E900EAEAEA00EBEBEB00ECECEC00EDEDED00EEEEEE00EFEFEF00F0F0F000F1F1
        F100F2F2F200F3F3F300F4F4F400F5F5F500F6F6F600F7F7F700F8F8F800F9F9
        F900FAFAFA00FBFBFB00FCFCFC00FDFDFD00FEFEFE00FFFFFF00FFFFFF000000
        0000FFFFFFFFFFFFFF00FFFFFF0000000000FFFFFFFFFFFFFF00FFFFFF000000
        0000FFFFFFFFFFFFFF00FFFF00000000000000FFFFFFFFFFFF00FFFFFF00FFFF
        FF00FFFFFFFFFFFFFF00FFFFFF00FFFFFF00FFFFFFFFFFFFFF00FFFF00FFFF00
        FFFF00FFFFFFFFFFFF00FFFF00FFFFFFFFFF00FFFFFFFFFFFF00FFFF00FFFF00
        FFFF00FFFFFFFFFFFF00FFFFFF00FF00FF00FFFFFFFFFFFFFF00FFFFFF00FF00
        FF00FFFFFFFFFFFFFF00FFFFFFFF00FF00FFFFFF000000000000FFFFFFFF00FF
        00FFFFFFFFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFFFFFFFFFF00FFFFFFFFFF00
        FFFFFFFFFFFFFFFFFF00}
    end
    object btDelLine: TSpeedButton
      Left = 357
      Top = 0
      Width = 28
      Height = 27
      Hint = 'Click to Delete a Line from the Polygon'
      GroupIndex = 1
      Enabled = False
      Flat = True
      Glyph.Data = {
        62050000424D62050000000000003604000028000000120000000F0000000100
        0800000000002C01000000000000000000000001000000000000000000000101
        0100020202000303030004040400050505000606060007070700080808000909
        09000A0A0A000B0B0B000C0C0C000D0D0D000E0E0E000F0F0F00101010001111
        1100121212001313130014141400151515001616160017171700181818001919
        19001A1A1A001B1B1B001C1C1C001D1D1D001E1E1E001F1F1F00202020002121
        2100222222002323230024242400252525002626260027272700282828002929
        29002A2A2A002B2B2B002C2C2C002D2D2D002E2E2E002F2F2F00303030003131
        3100323232003333330034343400353535003636360037373700383838003939
        39003A3A3A003B3B3B003C3C3C003D3D3D003E3E3E003F3F3F00404040004141
        4100424242004343430044444400454545004646460047474700484848004949
        49004A4A4A004B4B4B004C4C4C004D4D4D004E4E4E004F4F4F00505050005151
        5100525252005353530054545400555555005656560057575700585858005959
        59005A5A5A005B5B5B005C5C5C005D5D5D005E5E5E005F5F5F00606060006161
        6100626262006363630064646400656565006666660067676700686868006969
        69006A6A6A006B6B6B006C6C6C006D6D6D006E6E6E006F6F6F00707070007171
        7100727272007373730074747400757575007676760077777700787878007979
        79007A7A7A007B7B7B007C7C7C007D7D7D007E7E7E007F7F7F00808080008181
        8100828282008383830084848400858585008686860087878700888888008989
        89008A8A8A008B8B8B008C8C8C008D8D8D008E8E8E008F8F8F00909090009191
        9100929292009393930094949400959595009696960097979700989898009999
        99009A9A9A009B9B9B009C9C9C009D9D9D009E9E9E009F9F9F00A0A0A000A1A1
        A100A2A2A200A3A3A300A4A4A400A5A5A500A6A6A600A7A7A700A8A8A800A9A9
        A900AAAAAA00ABABAB00ACACAC00ADADAD00AEAEAE00AFAFAF00B0B0B000B1B1
        B100B2B2B200B3B3B300B4B4B400B5B5B500B6B6B600B7B7B700B8B8B800B9B9
        B900BABABA00BBBBBB00BCBCBC00BDBDBD00BEBEBE00BFBFBF00C0C0C000C1C1
        C100C2C2C200C3C3C300C4C4C400C5C5C500C6C6C600C7C7C700C8C8C800C9C9
        C900CACACA00CBCBCB00CCCCCC00CDCDCD00CECECE00CFCFCF00D0D0D000D1D1
        D100D2D2D200D3D3D300D4D4D400D5D5D500D6D6D600D7D7D700D8D8D800D9D9
        D900DADADA00DBDBDB00DCDCDC00DDDDDD00DEDEDE00DFDFDF00E0E0E000E1E1
        E100E2E2E200E3E3E300E4E4E400E5E5E500E6E6E600E7E7E700E8E8E800E9E9
        E900EAEAEA00EBEBEB00ECECEC00EDEDED00EEEEEE00EFEFEF00F0F0F000F1F1
        F100F2F2F200F3F3F300F4F4F400F5F5F500F6F6F600F7F7F700F8F8F800F9F9
        F900FAFAFA00FBFBFB00FCFCFC00FDFDFD00FEFEFE00FFFFFF00FFFF00000000
        00000000FFFFFFFFFFFFFFFF0000FFFF00FFFFFFFFFFFF0000FFFFFFFFFFFFFF
        0000FFFF00FFFFFFFFFFFF00FF00FFFFFFFFFFFF0000FFFF0000000000000000
        FFFF00FFFFFFFFFF0000FFFFFF00FFFFFFFFFFFF00FFFF00FFFFFFFF0000FFFF
        FFFF00FFFFFFFFFFFF00FFFF00FFFFFF0000FFFFFFFFFF00FFFFFFFFFFFF00FF
        FF00FFFF0000FFFFFFFFFFFF00FFFFFFFFFFFF00FFFF00FF0000FFFFFFFFFFFF
        FFFF0000FFFFFFFF00FF00FF0000FF000000FFFFFF00FFFF00FFFFFFFF0000FF
        0000FFFFFF000000FF000000FFFF0000000000FF0000FFFFFFFFFF000000FFFF
        FFFFFFFFFFFFFFFF0000FFFFFF000000FF000000FFFFFFFFFFFFFFFF0000FF00
        0000FFFFFF00FFFF00FFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFF0000FFFFFFFF
        FFFFFFFF0000}
      OnClick = btDelLineClick
    end
    object SpeedButton1: TSpeedButton
      Left = 385
      Top = 0
      Width = 28
      Height = 27
      Hint = 'Delete all hotspots'
      Flat = True
      Glyph.Data = {
        36050000424D3605000000000000360400002800000010000000100000000100
        08000000000000010000220B0000220B000000010000000100000031DE000031
        E7000031EF000031F700FF00FF000031FF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00040404040404
        0404040404040404000004000004040404040404040404000004040000000404
        0404040404040000040404000000000404040404040000040404040402000000
        0404040400000404040404040404000000040000000404040404040404040400
        0101010004040404040404040404040401010204040404040404040404040400
        0201020304040404040404040404030201040403030404040404040404050203
        0404040405030404040404040303050404040404040303040404040303030404
        0404040404040403040403030304040404040404040404040404030304040404
        0404040404040404040404040404040404040404040404040404}
      OnClick = SpeedButton1Click
    end
    object ToolButton1: TToolButton
      Left = 413
      Top = 0
      Width = 8
      Caption = 'ToolButton1'
      Style = tbsSeparator
    end
    object btZoomIn: TSpeedButton
      Left = 421
      Top = 0
      Width = 48
      Height = 27
      Hint = 'Zoom the Image and the HotSpots (Ctrl+I)'
      Caption = 'Zoom In'
      Flat = True
      OnClick = btZoomInClick
    end
    object btZoomOut: TSpeedButton
      Left = 469
      Top = 0
      Width = 56
      Height = 27
      Hint = 'Zoom the Image and the HotSpots (Ctrl-Shift+I)'
      Caption = 'Zoom Out'
      Flat = True
      OnClick = btZoomOutClick
    end
    object btZoomRST: TSpeedButton
      Left = 525
      Top = 0
      Width = 72
      Height = 27
      Hint = 'Reset the Zooming to 1:1 (Ctrl+R)'
      Caption = 'Zoom Reset'
      Flat = True
      OnClick = btZoomRSTClick
    end
  end
  object pnButtons: TPanel
    Left = 0
    Top = 439
    Width = 778
    Height = 39
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      778
      39)
    object lbWTolerance: TLabel
      Left = 155
      Top = 13
      Width = 51
      Height = 13
      Caption = 'Tolerance:'
      Visible = False
    end
    object lbWDensity: TLabel
      Left = 7
      Top = 13
      Width = 80
      Height = 13
      Caption = 'Wand Accuracy:'
      Visible = False
    end
    object lbZoomRatio: TLabel
      Left = 296
      Top = 13
      Width = 58
      Height = 13
      Caption = 'Zoom Ratio:'
    end
    object btOk: TButton
      Left = 592
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Ok'
      ModalResult = 1
      TabOrder = 2
      OnClick = btOkClick
    end
    object btCancel: TButton
      Left = 672
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 3
    end
    object trTol: TTrackBar
      Left = 206
      Top = 6
      Width = 67
      Height = 27
      Hint = 'Select Magic Wand tolerance'
      Max = 100
      PageSize = 5
      Frequency = 5
      Position = 10
      TabOrder = 1
      Visible = False
      OnChange = trTolChange
    end
    object trDensity: TTrackBar
      Left = 87
      Top = 6
      Width = 66
      Height = 27
      Hint = 
        'Select Magic Wand point density (accuracy of the selection vs nu' +
        'mber of polygon points)'
      Max = 50
      Min = 1
      PageSize = 5
      Frequency = 5
      Position = 50
      TabOrder = 0
      Visible = False
      OnChange = trTolChange
    end
    object seZoomRatio: TSpinEdit
      Left = 360
      Top = 8
      Width = 49
      Height = 22
      MaxValue = 200
      MinValue = 1
      TabOrder = 4
      Value = 10
    end
  end
  object pmHotSpot: TPopupMenu
    Left = 8
    Top = 40
    object miDelete: TMenuItem
      Caption = 'Delete'
      OnClick = miDeleteClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object miSaveHSImage: TMenuItem
      Caption = 'Save hotspot image...'
      OnClick = miSaveHSImageClick
    end
  end
  object OPD: TOpenPictureDialog
    Options = [ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select picture'
    Left = 40
    Top = 40
  end
  object pmClickImage: TPopupMenu
    Left = 564
    Top = 176
    object miClearClickImage: TMenuItem
      Caption = 'Clear'
      OnClick = miClearClickImageClick
    end
  end
  object pmHoverImage: TPopupMenu
    Left = 524
    Top = 176
    object miClearHoverImage: TMenuItem
      Caption = 'Clear'
      OnClick = miClearHoverImageClick
    end
  end
  object SPD: TSavePictureDialog
    Title = 'Save hotspot image'
    Left = 72
    Top = 40
  end
  object ColorDialog1: TColorDialog
    Left = 610
    Top = 177
  end
  object pm_SelectedImage: TPopupMenu
    Left = 530
    Top = 209
    object mi_CLearSelectImage: TMenuItem
      Caption = 'Clear'
      OnClick = mi_CLearSelectImageClick
    end
  end
  object pm_BlinkImage: TPopupMenu
    Left = 568
    Top = 208
    object mi_ClearBlinkImage: TMenuItem
      Caption = 'Clear'
      OnClick = mi_ClearBlinkImageClick
    end
  end
end
