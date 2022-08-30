inherited frmScreenTipRepositoryEditor: TfrmScreenTipRepositoryEditor
  BorderStyle = bsSizeable
  Caption = 'frmScreenTipRepositoryEditor'
  ClientHeight = 383
  ClientWidth = 612
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter [0]
    Left = 172
    Top = 0
    Height = 336
  end
  inherited pnlItems: TPanel
    AlignWithMargins = True
    Left = 8
    Top = 8
    Width = 161
    Height = 328
    Margins.Left = 8
    Margins.Top = 8
    Margins.Bottom = 0
    Align = alLeft
    BevelOuter = bvNone
    inherited ToolBar1: TToolBar
      AlignWithMargins = True
      Left = 0
      Top = 0
      Width = 161
      Height = 24
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      EdgeBorders = []
    end
    inherited Panel: TPanel
      Left = 0
      Top = 27
      Width = 161
      Height = 301
      inherited ListView1: TListView
        Width = 161
        Height = 301
      end
    end
  end
  object pnlButtons: TPanel [2]
    AlignWithMargins = True
    Left = 8
    Top = 344
    Width = 596
    Height = 31
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnClose: TButton
      AlignWithMargins = True
      Left = 521
      Top = 3
      Width = 75
      Height = 25
      Margins.Right = 0
      Align = alRight
      Cancel = True
      Caption = 'Close'
      Default = True
      TabOrder = 0
      OnClick = btnCloseClick
    end
    object btnShowOptions: TButton
      AlignWithMargins = True
      Left = 440
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Caption = 'Hide options'
      TabOrder = 1
      OnClick = btnShowOptionsClick
    end
  end
  object pnlPreviewAndOptions: TPanel [3]
    AlignWithMargins = True
    Left = 178
    Top = 8
    Width = 426
    Height = 328
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 0
    Align = alClient
    BevelOuter = bvNone
    ParentShowHint = False
    ShowHint = False
    TabOrder = 2
    object pnlPreview: TPanel
      Left = 0
      Top = 0
      Width = 426
      Height = 75
      Align = alClient
      BevelOuter = bvNone
      Constraints.MinHeight = 50
      TabOrder = 0
      OnResize = pnlPreviewResize
      object gbPreview: TGroupBox
        AlignWithMargins = True
        Left = 0
        Top = 0
        Width = 426
        Height = 72
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alClient
        Caption = 'Preview'
        TabOrder = 0
        object PaintBox1: TPaintBox
          Left = 163
          Top = 13
          Width = 74
          Height = 62
          OnPaint = PaintBox1Paint
        end
      end
    end
    object pnlOptions: TPanel
      Left = 0
      Top = 75
      Width = 426
      Height = 253
      Align = alBottom
      BevelOuter = bvNone
      Constraints.MinHeight = 205
      Constraints.MinWidth = 414
      TabOrder = 1
      object pcOptions: TPageControl
        AlignWithMargins = True
        Left = 0
        Top = 3
        Width = 426
        Height = 250
        Margins.Left = 0
        Margins.Right = 0
        Margins.Bottom = 0
        ActivePage = tsScreenTipOptions
        Align = alBottom
        TabOrder = 0
        object tsScreenTipOptions: TTabSheet
          Caption = 'ScreenTip'
          object gbScreenTipInfo: TGroupBox
            Left = 4
            Top = 3
            Width = 408
            Height = 212
            Anchors = [akLeft, akTop, akRight, akBottom]
            Ctl3D = True
            ParentCtl3D = False
            TabOrder = 0
            object Label1: TLabel
              Left = 279
              Top = 187
              Width = 32
              Height = 13
              Anchors = [akLeft]
              Caption = 'Width:'
            end
            object btnHeaderRichTextEdit: TSpeedButton
              Left = 378
              Top = 47
              Width = 22
              Height = 22
              Anchors = [akRight, akBottom]
              Caption = '...'
              OnClick = btnDescriptionRichTextEditClick
            end
            object btnDescriptionRichTextEdit: TSpeedButton
              Tag = 1
              Left = 378
              Top = 95
              Width = 22
              Height = 22
              Anchors = [akRight, akBottom]
              Caption = '...'
              OnClick = btnDescriptionRichTextEditClick
            end
            object btnFooterRichTextEdit: TSpeedButton
              Tag = 2
              Left = 378
              Top = 145
              Width = 22
              Height = 22
              Anchors = [akRight, akBottom]
              Caption = '...'
              OnClick = btnDescriptionRichTextEditClick
            end
            object edtHeader: TEdit
              Left = 30
              Top = 48
              Width = 259
              Height = 21
              Anchors = [akLeft, akRight, akBottom]
              TabOrder = 0
              OnChange = edtHeaderChange
            end
            object edtDescription: TMemo
              Tag = 1
              Left = 48
              Top = 83
              Width = 241
              Height = 49
              Anchors = [akLeft, akRight, akBottom]
              TabOrder = 1
              OnChange = edtHeaderChange
            end
            object edtFooter: TEdit
              Tag = 2
              Left = 30
              Top = 146
              Width = 259
              Height = 21
              Anchors = [akLeft, akRight, akBottom]
              TabOrder = 2
              OnChange = edtHeaderChange
            end
            object chbUseHintAsHeader: TCheckBox
              Left = 8
              Top = 186
              Width = 114
              Height = 17
              Anchors = [akLeft]
              Caption = 'UseHintAsHeader'
              TabOrder = 3
              OnClick = chbUseHintAsHeaderClick
            end
            object chbUseStandardFooter: TCheckBox
              Left = 133
              Top = 186
              Width = 114
              Height = 17
              Anchors = [akLeft]
              Caption = 'UseStandardFooter'
              TabOrder = 4
              OnClick = chbUseStandardFooterClick
            end
            object UpDown1: TUpDown
              Left = 376
              Top = 184
              Width = 16
              Height = 21
              Anchors = [akLeft]
              Associate = edtWidth
              Max = 30000
              TabOrder = 5
            end
            object edtWidth: TEdit
              Left = 319
              Top = 184
              Width = 57
              Height = 21
              Anchors = [akLeft]
              TabOrder = 6
              Text = '0'
              OnChange = edtWidthChange
              OnExit = edtWidthExit
            end
            object Panel1: TPanel
              Left = 8
              Top = 91
              Width = 34
              Height = 34
              Anchors = [akLeft, akBottom]
              BevelOuter = bvLowered
              TabOrder = 7
              object pbDescription: TPaintBox
                Tag = 1
                Left = 1
                Top = 1
                Width = 32
                Height = 32
                Align = alClient
                PopupMenu = PopupMenu3
                OnContextPopup = pbHeaderContextPopup
                OnDblClick = pbHeaderClick
                OnPaint = pbHeaderPaint
              end
            end
            object Panel2: TPanel
              Left = 7
              Top = 148
              Width = 17
              Height = 17
              Anchors = [akLeft, akBottom]
              BevelOuter = bvLowered
              TabOrder = 8
              object pbFooter: TPaintBox
                Tag = 2
                Left = 1
                Top = 1
                Width = 15
                Height = 15
                Align = alClient
                PopupMenu = PopupMenu3
                OnContextPopup = pbHeaderContextPopup
                OnDblClick = pbHeaderClick
                OnPaint = pbHeaderPaint
              end
            end
            object Panel3: TPanel
              Left = 7
              Top = 50
              Width = 17
              Height = 17
              Anchors = [akLeft, akBottom]
              BevelOuter = bvLowered
              TabOrder = 9
              object pbHeader: TPaintBox
                Left = 1
                Top = 1
                Width = 15
                Height = 15
                Align = alClient
                PopupMenu = PopupMenu3
                OnContextPopup = pbHeaderContextPopup
                OnDblClick = pbHeaderClick
                OnPaint = pbHeaderPaint
              end
            end
            object chbDescriptionPlain: TCheckBox
              Tag = 1
              Left = 301
              Top = 98
              Width = 67
              Height = 17
              Anchors = [akRight, akBottom]
              Caption = 'Plain text'
              TabOrder = 10
              OnClick = chbPlainTextClick
            end
            object chbFooterPlain: TCheckBox
              Tag = 2
              Left = 301
              Top = 149
              Width = 67
              Height = 17
              Anchors = [akRight, akBottom]
              Caption = 'Plain text'
              TabOrder = 11
              OnClick = chbPlainTextClick
            end
            object chbHeaderPlain: TCheckBox
              Left = 301
              Top = 50
              Width = 67
              Height = 17
              Anchors = [akRight, akBottom]
              Caption = 'Plain text'
              TabOrder = 12
              OnClick = chbPlainTextClick
            end
          end
        end
        object tsRepositoryOptions: TTabSheet
          Caption = 'Repository'
          ImageIndex = 1
          object gbScreenTipsInfo: TGroupBox
            Left = 4
            Top = 3
            Width = 408
            Height = 212
            Anchors = [akLeft, akTop, akRight, akBottom]
            TabOrder = 0
            object Label2: TLabel
              Left = 8
              Top = 16
              Width = 81
              Height = 13
              Caption = 'Standard footer:'
            end
            object btnHeaderFont: TSpeedButton
              Left = 378
              Top = 131
              Width = 22
              Height = 22
              Anchors = [akRight, akBottom]
              Caption = '...'
              OnClick = btnHeaderFontClick
            end
            object btnDescriptionFont: TSpeedButton
              Tag = 1
              Left = 378
              Top = 157
              Width = 22
              Height = 22
              Anchors = [akRight, akBottom]
              Caption = '...'
              OnClick = btnDescriptionFontClick
            end
            object btnFooterFont: TSpeedButton
              Tag = 2
              Left = 378
              Top = 183
              Width = 22
              Height = 22
              Anchors = [akRight, akBottom]
              Caption = '...'
              OnClick = btnFooterFontClick
            end
            object lblHeaderFontInfo: TLabel
              Left = 72
              Top = 139
              Width = 39
              Height = 13
              Anchors = [akLeft, akBottom]
              Caption = 'Header:'
            end
            object lblDescriptionFontInfo: TLabel
              Tag = 1
              Left = 89
              Top = 165
              Width = 57
              Height = 13
              Anchors = [akLeft, akBottom]
              Caption = 'Description:'
            end
            object lblFooterFontInfo: TLabel
              Tag = 2
              Left = 67
              Top = 191
              Width = 36
              Height = 13
              Anchors = [akLeft, akBottom]
              Caption = 'Footer:'
            end
            object btnStdFooterRichTextEdit: TSpeedButton
              Tag = 3
              Left = 378
              Top = 39
              Width = 22
              Height = 22
              Anchors = [akRight, akBottom]
              Caption = '...'
              OnClick = btnDescriptionRichTextEditClick
            end
            object chbShowDescription: TCheckBox
              Left = 8
              Top = 111
              Width = 114
              Height = 17
              Anchors = [akLeft, akBottom]
              Caption = 'Show description'
              TabOrder = 0
              OnClick = chbShowDescriptionClick
            end
            object edtStandardFooter: TEdit
              Tag = 3
              Left = 29
              Top = 40
              Width = 259
              Height = 21
              Anchors = [akLeft, akRight, akBottom]
              TabOrder = 1
              OnChange = edtStandardFooterChange
            end
            object Panel4: TPanel
              Left = 7
              Top = 42
              Width = 17
              Height = 17
              Anchors = [akLeft, akBottom]
              BevelOuter = bvLowered
              TabOrder = 2
              object pbStandardFooter: TPaintBox
                Tag = 3
                Left = 1
                Top = 1
                Width = 15
                Height = 15
                Align = alClient
                PopupMenu = PopupMenu3
                OnContextPopup = pbHeaderContextPopup
                OnDblClick = pbStandardFooterClick
                OnPaint = pbStandardFooterPaint
              end
            end
            object chbHeaderFont: TCheckBox
              Left = 8
              Top = 139
              Width = 58
              Height = 13
              Anchors = [akLeft, akBottom]
              Caption = 'Header:'
              TabOrder = 3
              OnClick = chbHeaderFontClick
            end
            object chbDescriptionFont: TCheckBox
              Tag = 1
              Left = 8
              Top = 165
              Width = 81
              Height = 13
              Anchors = [akLeft, akBottom]
              Caption = 'Description:'
              TabOrder = 4
              OnClick = chbHeaderFontClick
            end
            object chbFooterFont: TCheckBox
              Tag = 2
              Left = 8
              Top = 191
              Width = 58
              Height = 13
              Anchors = [akLeft, akBottom]
              Caption = 'Footer:'
              TabOrder = 5
              OnClick = chbHeaderFontClick
            end
            object chbPlainText: TCheckBox
              Tag = 3
              Left = 301
              Top = 42
              Width = 67
              Height = 17
              Anchors = [akRight, akBottom]
              Caption = 'Plain text'
              TabOrder = 6
              OnClick = chbPlainTextClick
            end
          end
        end
      end
    end
  end
  inherited ActionList: TActionList
    object actLeftDescriptionAlign: TAction
      AutoCheck = True
      Caption = 'actLeftDescriptionAlign'
      Checked = True
    end
  end
  inherited ilActions: TcxImageList
    FormatVersion = 1
  end
  inherited ilToolBar: TcxImageList
    FormatVersion = 1
  end
  inherited ilToolBarDisabled: TcxImageList
    FormatVersion = 1
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Left = 72
    Top = 72
  end
  object PopupMenu3: TPopupMenu
    OnPopup = PopupMenu3Popup
    Left = 40
    Top = 72
    object Open1: TMenuItem
      Caption = 'Open...'
      OnClick = Open1Click
    end
    object Clear1: TMenuItem
      Caption = 'Clear'
      OnClick = Clear1Click
    end
    object FixedWidth1: TMenuItem
      Caption = 'Fixed Width'
      OnClick = FixedWidth1Click
    end
    object Align1: TMenuItem
      Caption = 'Align'
      object Left1: TMenuItem
        Tag = 1
        Caption = 'Left'
        GroupIndex = 1
        RadioItem = True
        OnClick = Right1Click
      end
      object Right1: TMenuItem
        Caption = 'Right'
        GroupIndex = 1
        RadioItem = True
        OnClick = Right1Click
      end
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 8
    Top = 72
  end
end
