object ImageListEditor: TImageListEditor
  Left = 0
  Top = 0
  HelpContext = 26140
  Caption = #22270#29255#21015#34920#32534#36753#22120
  ClientHeight = 299
  ClientWidth = 592
  Color = clBtnFace
  Constraints.MinHeight = 333
  Constraints.MinWidth = 600
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  DesignSize = (
    592
    299)
  PixelsPerInch = 96
  TextHeight = 13
  object OK: TButton
    Left = 509
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = #30830#23450
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Cancel: TButton
    Left = 509
    Top = 37
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = #21462#28040
    ModalResult = 2
    TabOrder = 1
  end
  object Apply: TButton
    Left = 509
    Top = 68
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = #24212#29992
    TabOrder = 2
    OnClick = ApplyClick
  end
  object ImageListGroup: TGroupBox
    Left = 10
    Top = 159
    Width = 574
    Height = 125
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = #22270#29255
    TabOrder = 3
    DesignSize = (
      574
      125)
    object ImageView: TListView
      Left = 10
      Top = 18
      Width = 553
      Height = 70
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Caption = 'asdf'
          Width = 100
        end>
      DragCursor = crArrow
      DragMode = dmAutomatic
      HideSelection = False
      IconOptions.Arrangement = iaLeft
      IconOptions.AutoArrange = True
      IconOptions.WrapText = False
      MultiSelect = True
      ReadOnly = True
      TabOrder = 0
      OnCompare = ImageViewCompare
      OnEndDrag = ImageViewEndDrag
      OnDragDrop = ImageViewDragDrop
      OnDragOver = ImageViewDragOver
      OnSelectItem = ImageViewSelectItem
    end
    object Add: TButton
      Left = 39
      Top = 94
      Width = 100
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = #22686#21152'...'
      TabOrder = 1
      OnClick = AddClick
    end
    object Delete: TButton
      Left = 251
      Top = 94
      Width = 100
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = #21024#38500
      Enabled = False
      TabOrder = 2
      OnClick = DeleteClick
    end
    object Clear: TButton
      Left = 357
      Top = 94
      Width = 100
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = #28165#31354
      Enabled = False
      TabOrder = 3
      OnClick = ClearClick
    end
    object ExportBtn: TButton
      Left = 463
      Top = 94
      Width = 100
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = #23548#20986
      Enabled = False
      TabOrder = 4
      OnClick = ExportBtnClick
    end
    object ReplaceBtn: TButton
      Left = 145
      Top = 94
      Width = 100
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = #26367#25442'...'
      Enabled = False
      TabOrder = 5
      OnClick = AddClick
    end
  end
  object ImageGroup: TGroupBox
    Left = 10
    Top = 8
    Width = 493
    Height = 145
    Anchors = [akLeft, akTop, akRight]
    Caption = #24403#21069#22270#29255
    TabOrder = 4
    DesignSize = (
      493
      145)
    object OptionsPanel: TPanel
      Left = 93
      Top = 15
      Width = 397
      Height = 131
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        397
        131)
      object FillLabel: TLabel
        Left = 10
        Top = 42
        Width = 372
        Height = 13
        AutoSize = False
        Caption = '&Fill Color:'
        Transparent = False
      end
      object TransparentLabel: TLabel
        Left = 10
        Top = 0
        Width = 372
        Height = 13
        AutoSize = False
        Caption = #36879#26126#33394
        Transparent = False
      end
      object OptionsGroup: TRadioGroup
        Left = 10
        Top = 82
        Width = 380
        Height = 37
        Anchors = [akLeft, akTop, akRight]
        Caption = #36873#39033
        Columns = 3
        Enabled = False
        ItemIndex = 0
        Items.Strings = (
          #24179#38138
          #25289#20280
          #23621#20013)
        TabOrder = 0
        OnClick = OptionsGroupClick
      end
      object FillColor: TColorBox
        Left = 10
        Top = 56
        Width = 380
        Height = 22
        DefaultColorColor = clWindow
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        OnChange = FillColorChange
      end
      object TransparentColor: TColorBox
        Left = 10
        Top = 14
        Width = 380
        Height = 22
        DefaultColorColor = clWindow
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        OnChange = TransparentColorChange
      end
    end
    object MainPanel: TPanel
      Left = 10
      Top = 18
      Width = 78
      Height = 78
      BevelOuter = bvNone
      BorderWidth = 5
      BorderStyle = bsSingle
      ParentColor = True
      TabOrder = 0
      object MainImage: TImage
        Left = 5
        Top = 5
        Width = 64
        Height = 64
        Align = alClient
        Stretch = True
        OnMouseDown = MainImageMouseDown
        OnMouseMove = MainImageMouseMove
        OnMouseUp = MainImageMouseUp
        ExplicitWidth = 60
        ExplicitHeight = 60
      end
    end
  end
  object OpenDialog: TOpenPictureDialog
    HelpContext = 27000
    DefaultExt = 'bmp'
    Filter = 
      'All (*.bmp, *.ico)|*.bmp;*.ico|Bitmaps (*.bmp)|*.bmp|Icons (*.ic' +
      'o)|*.ico'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Title = 'Add Images'
    Left = 32
    Top = 232
  end
  object DragTimer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = DragTimerTimer
    Left = 32
    Top = 280
  end
  object SaveDialog: TSavePictureDialog
    HelpContext = 27010
    DefaultExt = 'bmp'
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Options = [ofOverwritePrompt, ofEnableSizing]
    Title = 'Export Images'
    Left = 32
    Top = 184
  end
end
