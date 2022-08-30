object GDIPPictureContainerEditor: TGDIPPictureContainerEditor
  Left = 0
  Top = 0
  Caption = 'Picture Container Editor'
  ClientHeight = 600
  ClientWidth = 869
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 201
    Top = 41
    Height = 559
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 869
    Height = 41
    Align = alTop
    Color = clWhite
    TabOrder = 0
    object Label1: TLabel
      Left = 399
      Top = 14
      Width = 30
      Height = 13
      Caption = 'Rows:'
    end
    object Button2: TButton
      Left = 8
      Top = 9
      Width = 75
      Height = 25
      Hint = 'Remove selected item in picture list'
      Caption = 'Remove'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 204
      Top = 9
      Width = 173
      Height = 25
      Hint = 'Add selected images to picture list'
      Caption = 'Add Selected Images To List'
      Enabled = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = Button3Click
    end
    object CheckBox1: TCheckBox
      Left = 583
      Top = 13
      Width = 97
      Height = 17
      Hint = 'Automatically calculate row count'
      Caption = 'AutoRows'
      Checked = True
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 2
      OnClick = CheckBox1Click
    end
    object TrackBar1: TTrackBar
      Left = 431
      Top = 5
      Width = 150
      Height = 32
      Hint = 'Change rows  to display less or more images'
      Enabled = False
      Max = 50
      Min = 1
      ParentShowHint = False
      Frequency = 5
      Position = 3
      ShowHint = True
      TabOrder = 3
      OnChange = TrackBar1Change
    end
    object Button1: TButton
      Left = 679
      Top = 9
      Width = 91
      Height = 25
      Hint = 'Load directory in image browser'
      Caption = 'Load Directory'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = Button1Click
    end
    object ComboBox2: TComboBox
      Left = 776
      Top = 11
      Width = 82
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      Text = 'All Images'
      OnChange = ComboBox2Change
      Items.Strings = (
        'All Images'
        '*.bmp'
        '*.jpg'
        '*.jpeg'
        '*.png'
        '*.gif'
        '*.tiff'
        '*.ico')
    end
    object Button4: TButton
      Left = 89
      Top = 9
      Width = 75
      Height = 25
      Hint = 'Remove all items in picture list'
      Caption = 'Remove All'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      OnClick = Button4Click
    end
  end
  object FileList: TAdvSmoothImageListBox
    Left = 0
    Top = 41
    Width = 201
    Height = 559
    Hint = 'Double click on selected item to change name'
    SelectedItemIndex = 0
    Items = <>
    TopLayerItems = <>
    ItemAppearance.AutoSize = True
    ItemAppearance.ImageAlign = alTop
    ItemAppearance.TextAlign = alTop
    ItemAppearance.TextWidth = 20
    ItemAppearance.TextHeight = 20
    ItemAppearance.ItemWidth = 250
    ItemAppearance.ItemHeight = 100
    ItemAppearance.Fill.Color = 16645629
    ItemAppearance.Fill.ColorTo = 15395562
    ItemAppearance.Fill.ColorMirror = clNone
    ItemAppearance.Fill.ColorMirrorTo = clNone
    ItemAppearance.Fill.GradientMirrorType = gtVertical
    ItemAppearance.Fill.BorderColor = clBlack
    ItemAppearance.Fill.Rounding = 4
    ItemAppearance.Fill.ShadowColor = clBlack
    ItemAppearance.Fill.ShadowOffset = 5
    ItemAppearance.SelectedFill.Color = 16645629
    ItemAppearance.SelectedFill.ColorTo = 10349823
    ItemAppearance.SelectedFill.ColorMirror = clNone
    ItemAppearance.SelectedFill.ColorMirrorTo = clNone
    ItemAppearance.SelectedFill.GradientMirrorType = gtVertical
    ItemAppearance.SelectedFill.BorderColor = clBlack
    ItemAppearance.SelectedFill.Rounding = 4
    ItemAppearance.SelectedFill.ShadowColor = clBlack
    ItemAppearance.SelectedFill.ShadowOffset = 5
    ItemAppearance.DisabledFill.Color = 14022655
    ItemAppearance.DisabledFill.ColorTo = 9693951
    ItemAppearance.DisabledFill.ColorMirror = clNone
    ItemAppearance.DisabledFill.ColorMirrorTo = clNone
    ItemAppearance.DisabledFill.GradientMirrorType = gtVertical
    ItemAppearance.DisabledFill.BorderColor = clBlack
    ItemAppearance.DisabledFill.Rounding = 4
    ItemAppearance.DisabledFill.ShadowColor = clBlack
    ItemAppearance.DisabledFill.ShadowOffset = 5
    ItemAppearance.HoverFill.Color = 16645629
    ItemAppearance.HoverFill.ColorTo = 16770764
    ItemAppearance.HoverFill.ColorMirror = clNone
    ItemAppearance.HoverFill.ColorMirrorTo = clNone
    ItemAppearance.HoverFill.GradientMirrorType = gtVertical
    ItemAppearance.HoverFill.BorderColor = clBlack
    ItemAppearance.HoverFill.Rounding = 4
    ItemAppearance.HoverFill.ShadowColor = clBlack
    ItemAppearance.HoverFill.ShadowOffset = 5
    ItemAppearance.Splitter.Fill.Color = 11196927
    ItemAppearance.Splitter.Fill.ColorTo = 7257087
    ItemAppearance.Splitter.Fill.ColorMirror = clNone
    ItemAppearance.Splitter.Fill.ColorMirrorTo = clNone
    ItemAppearance.Splitter.Fill.GradientType = gtHorizontal
    ItemAppearance.Splitter.Fill.BorderColor = clBlack
    ItemAppearance.Splitter.Fill.Rounding = 0
    ItemAppearance.Splitter.Fill.ShadowOffset = 0
    ItemAppearance.Splitter.TextFont.Charset = DEFAULT_CHARSET
    ItemAppearance.Splitter.TextFont.Color = clWindowText
    ItemAppearance.Splitter.TextFont.Height = -11
    ItemAppearance.Splitter.TextFont.Name = 'Tahoma'
    ItemAppearance.Splitter.TextFont.Style = []
    ItemAppearance.Splitter.ExpanderColor = 16445163
    ItemAppearance.Splitter.ExpanderDownColor = 7257087
    ItemAppearance.Splitter.ExpanderHoverColor = 11196927
    ItemAppearance.Stretch = isShrinkOnly
    Header.Caption = 'Picture List'
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = 9126421
    Header.Font.Height = -16
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Height = 30
    Header.Fill.Color = 16316406
    Header.Fill.ColorTo = clNone
    Header.Fill.ColorMirror = clNone
    Header.Fill.ColorMirrorTo = clNone
    Header.Fill.Opacity = 71
    Header.Fill.BorderColor = 7630959
    Header.Fill.Rounding = 0
    Header.Fill.ShadowOffset = 0
    Header.Navigator.Visible = False
    Header.Navigator.Color = 16445163
    Header.Navigator.HintNext = 'Next Item'
    Header.Navigator.HintPrevious = 'Previous Item'
    Header.Navigator.HintNextPage = 'Next Page'
    Header.Navigator.HintPreviousPage = 'Previous Page'
    Header.Navigator.DisabledColor = clGray
    Header.Navigator.HoverColor = 11196927
    Header.Navigator.DownColor = 7257087
    Header.Navigator.BorderColor = clBlack
    Footer.Font.Charset = DEFAULT_CHARSET
    Footer.Font.Color = 9126421
    Footer.Font.Height = -13
    Footer.Font.Name = 'Tahoma'
    Footer.Font.Style = []
    Footer.Height = 30
    Footer.Fill.Color = 16316406
    Footer.Fill.ColorTo = clNone
    Footer.Fill.ColorMirror = clNone
    Footer.Fill.ColorMirrorTo = clNone
    Footer.Fill.Opacity = 71
    Footer.Fill.BorderColor = 7630959
    Footer.Fill.Rounding = 0
    Footer.Fill.ShadowOffset = 0
    Footer.Navigator.Visible = True
    Footer.Navigator.Color = 16445163
    Footer.Navigator.HintNext = 'Next Item'
    Footer.Navigator.HintPrevious = 'Previous Item'
    Footer.Navigator.HintNextPage = 'Next Page'
    Footer.Navigator.HintPreviousPage = 'Previous Page'
    Footer.Navigator.DisabledColor = clGray
    Footer.Navigator.HoverColor = 11196927
    Footer.Navigator.DownColor = 7257087
    Footer.Navigator.BorderColor = clBlack
    Fill.Color = clWhite
    Fill.ColorTo = clNone
    Fill.ColorMirror = clNone
    Fill.ColorMirrorTo = clNone
    Fill.BorderColor = clGray
    Fill.Rounding = 0
    Fill.ShadowOffset = 0
    DefaultHTMLText.Location = cpCenterCenter
    DefaultHTMLText.Font.Charset = DEFAULT_CHARSET
    DefaultHTMLText.Font.Color = clWindowText
    DefaultHTMLText.Font.Height = -11
    DefaultHTMLText.Font.Name = 'Tahoma'
    DefaultHTMLText.Font.Style = []
    Columns = 1
    Rows = 0
    ZoomOnDblClick = False
    OnItemDblClick = FileListItemDblClick
    OnItemDraw = FileListItemDraw
    OnItemStartDrag = FileListItemStartDrag
    Align = alLeft
    TabOrder = 1
    ParentShowHint = False
    ShowHint = True
    OnResize = FileListResize
    OnDragDrop = FileListDragDrop
    OnDragOver = FileListDragOver
  end
  object ImageList: TAdvSmoothImageListBox
    Left = 204
    Top = 41
    Width = 665
    Height = 559
    Hint = 
      'Select image and drag drop image in picture list to add to the c' +
      'ollection'
    AnimationFactor = 2
    SelectedItemIndex = 0
    Items = <>
    TopLayerItems = <
      item
        Visible = False
        HTMLText.Location = cpTopLeft
        HTMLText.Font.Charset = DEFAULT_CHARSET
        HTMLText.Font.Color = clWindowText
        HTMLText.Font.Height = -13
        HTMLText.Font.Name = 'Tahoma'
        HTMLText.Font.Style = []
        Fill.Color = 16645629
        Fill.ColorTo = 15395562
        Fill.ColorMirror = clNone
        Fill.ColorMirrorTo = clNone
        Fill.GradientMirrorType = gtVertical
        Fill.Opacity = 37
        Fill.OpacityTo = 37
        Fill.BorderColor = clBlack
        Fill.Rounding = 4
        Fill.ShadowColor = clWhite
        Fill.ShadowOffset = 5
        Tag = 0
      end>
    ItemAppearance.AutoSize = True
    ItemAppearance.ImageAlign = alTop
    ItemAppearance.ImageHeight = 75
    ItemAppearance.TextAlign = alTop
    ItemAppearance.TextWidth = 100
    ItemAppearance.TextHeight = 50
    ItemAppearance.ItemWidth = 275
    ItemAppearance.ItemHeight = 275
    ItemAppearance.Fill.Color = 16645629
    ItemAppearance.Fill.ColorTo = 15395562
    ItemAppearance.Fill.ColorMirror = clNone
    ItemAppearance.Fill.ColorMirrorTo = clNone
    ItemAppearance.Fill.GradientMirrorType = gtVertical
    ItemAppearance.Fill.BorderColor = clBlack
    ItemAppearance.Fill.Rounding = 4
    ItemAppearance.Fill.ShadowColor = clBlack
    ItemAppearance.Fill.ShadowOffset = 5
    ItemAppearance.SelectedFill.Color = 16645629
    ItemAppearance.SelectedFill.ColorTo = 10349823
    ItemAppearance.SelectedFill.ColorMirror = clNone
    ItemAppearance.SelectedFill.ColorMirrorTo = clNone
    ItemAppearance.SelectedFill.GradientMirrorType = gtVertical
    ItemAppearance.SelectedFill.BorderColor = clBlack
    ItemAppearance.SelectedFill.Rounding = 4
    ItemAppearance.SelectedFill.ShadowColor = clBlack
    ItemAppearance.SelectedFill.ShadowOffset = 5
    ItemAppearance.DisabledFill.Color = 16645629
    ItemAppearance.DisabledFill.ColorTo = 15395562
    ItemAppearance.DisabledFill.ColorMirror = clNone
    ItemAppearance.DisabledFill.ColorMirrorTo = clNone
    ItemAppearance.DisabledFill.GradientMirrorType = gtVertical
    ItemAppearance.DisabledFill.BorderColor = clBlack
    ItemAppearance.DisabledFill.Rounding = 4
    ItemAppearance.DisabledFill.ShadowColor = clBlack
    ItemAppearance.DisabledFill.ShadowOffset = 5
    ItemAppearance.HoverFill.Color = 16645629
    ItemAppearance.HoverFill.ColorTo = 16770764
    ItemAppearance.HoverFill.ColorMirror = clNone
    ItemAppearance.HoverFill.ColorMirrorTo = clNone
    ItemAppearance.HoverFill.GradientMirrorType = gtVertical
    ItemAppearance.HoverFill.BorderColor = clBlack
    ItemAppearance.HoverFill.Rounding = 4
    ItemAppearance.HoverFill.ShadowColor = clBlack
    ItemAppearance.HoverFill.ShadowOffset = 5
    ItemAppearance.Splitter.Fill.Color = 11196927
    ItemAppearance.Splitter.Fill.ColorTo = 7257087
    ItemAppearance.Splitter.Fill.ColorMirror = clNone
    ItemAppearance.Splitter.Fill.ColorMirrorTo = clNone
    ItemAppearance.Splitter.Fill.GradientType = gtHorizontal
    ItemAppearance.Splitter.Fill.BorderColor = clBlack
    ItemAppearance.Splitter.Fill.Rounding = 0
    ItemAppearance.Splitter.Fill.ShadowOffset = 0
    ItemAppearance.Splitter.TextFont.Charset = DEFAULT_CHARSET
    ItemAppearance.Splitter.TextFont.Color = clWindowText
    ItemAppearance.Splitter.TextFont.Height = -11
    ItemAppearance.Splitter.TextFont.Name = 'Tahoma'
    ItemAppearance.Splitter.TextFont.Style = []
    ItemAppearance.Splitter.ExpanderColor = 16445163
    ItemAppearance.Splitter.ExpanderDownColor = 7257087
    ItemAppearance.Splitter.ExpanderHoverColor = 11196927
    ItemAppearance.Stretch = isShrinkOnly
    Header.Caption = 'Image browser'
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = 9126421
    Header.Font.Height = -16
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Height = 30
    Header.Fill.Color = 16316406
    Header.Fill.ColorTo = clNone
    Header.Fill.ColorMirror = clNone
    Header.Fill.ColorMirrorTo = clNone
    Header.Fill.PicturePosition = ppCustom
    Header.Fill.PictureLeft = 10
    Header.Fill.PictureTop = 1
    Header.Fill.PictureSize = psCustom
    Header.Fill.PictureWidth = 100
    Header.Fill.PictureHeight = 100
    Header.Fill.Opacity = 71
    Header.Fill.BorderColor = 7630959
    Header.Fill.Rounding = 0
    Header.Fill.ShadowOffset = 0
    Header.Navigator.Visible = False
    Header.Navigator.Color = 16445163
    Header.Navigator.HintNext = 'Next Item'
    Header.Navigator.HintPrevious = 'Previous Item'
    Header.Navigator.HintNextPage = 'Next Page'
    Header.Navigator.HintPreviousPage = 'Previous Page'
    Header.Navigator.DisabledColor = clGray
    Header.Navigator.HoverColor = 11196927
    Header.Navigator.DownColor = 7257087
    Header.Navigator.BorderColor = clBlack
    Footer.CaptionLocation = cpCenterLeft
    Footer.CaptionLeft = 200
    Footer.CaptionTop = 12
    Footer.Font.Charset = DEFAULT_CHARSET
    Footer.Font.Color = 9126421
    Footer.Font.Height = -13
    Footer.Font.Name = 'Tahoma'
    Footer.Font.Style = []
    Footer.Height = 30
    Footer.Fill.Color = 16316406
    Footer.Fill.ColorTo = clNone
    Footer.Fill.ColorMirror = clNone
    Footer.Fill.ColorMirrorTo = clNone
    Footer.Fill.Opacity = 71
    Footer.Fill.BorderColor = 7630959
    Footer.Fill.Rounding = 0
    Footer.Fill.ShadowOffset = 0
    Footer.Navigator.Visible = True
    Footer.Navigator.Color = 16445163
    Footer.Navigator.HintNext = 'Next Item'
    Footer.Navigator.HintPrevious = 'Previous Item'
    Footer.Navigator.HintNextPage = 'Next Page'
    Footer.Navigator.HintPreviousPage = 'Previous Page'
    Footer.Navigator.DisabledColor = clGray
    Footer.Navigator.HoverColor = 11196927
    Footer.Navigator.DownColor = 7257087
    Footer.Navigator.BorderColor = clBlack
    Fill.Color = clWhite
    Fill.ColorTo = clNone
    Fill.ColorMirror = clNone
    Fill.ColorMirrorTo = clNone
    Fill.PicturePosition = ppCenterCenter
    Fill.BorderColor = clGray
    Fill.Rounding = 0
    Fill.ShadowOffset = 0
    DefaultHTMLText.Location = cpCenterCenter
    DefaultHTMLText.Font.Charset = DEFAULT_CHARSET
    DefaultHTMLText.Font.Color = clWindowText
    DefaultHTMLText.Font.Height = -11
    DefaultHTMLText.Font.Name = 'Tahoma'
    DefaultHTMLText.Font.Style = []
    ThreadLoading = tlAll
    ZoomMode = zmAspectRatio
    ZoomOnTop = True
    KeyBoardLookUp = True
    MultiSelect = True
    OnItemSelect = ImageListItemSelect
    OnItemClick = ImageListItemClick
    OnItemZoomIn = ImageListItemZoomIn
    OnItemDraw = ImageListItemDraw
    OnItemStartDrag = ImageListItemStartDrag
    Align = alClient
    TabOrder = 2
    ParentShowHint = False
    ShowHint = True
    OnResize = ImageListResize
  end
end
