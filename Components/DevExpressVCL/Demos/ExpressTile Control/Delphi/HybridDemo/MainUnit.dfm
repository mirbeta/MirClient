object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'DevAV'
  ClientHeight = 962
  ClientWidth = 1784
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object tbMain: TdxTileBar
    Left = 0
    Top = 32
    Width = 1784
    Height = 185
    FocusedItem = tbiTasks
    Images = DM.cxImageList1
    OptionsDetailAnimate.AnimationMode = damScrollFade
    OptionsView.IndentVert = 10
    OptionsView.ItemWidth = 70
    Style.GradientBeginColor = 15263976
    TabOrder = 0
    OnPopupActivate = tbMainPopupActivate
    object tmMainMyWorldGroup: TdxTileControlGroup
      Caption.Font.Charset = DEFAULT_CHARSET
      Caption.Font.Color = clMedGray
      Caption.Font.Height = -16
      Caption.Font.Name = 'Segoe UI'
      Caption.Font.Style = []
      Caption.Text = 'MY WORLD'
      Index = 0
    end
    object tbMainOperationsGroup: TdxTileControlGroup
      Caption.Font.Charset = DEFAULT_CHARSET
      Caption.Font.Color = clMedGray
      Caption.Font.Height = -16
      Caption.Font.Name = 'Segoe UI'
      Caption.Font.Style = []
      Caption.Text = 'OPERATIONS'
      Index = 1
    end
    object tbHiddenItemsGroup: TdxTileControlGroup
      Visible = False
      Index = 2
    end
    object tbiTasks: TdxTileBarItem
      Tag = 1
      Glyph.Align = oaTopLeft
      Glyph.ImageIndex = 1
      GroupIndex = 0
      IndexInGroup = 0
      Size = tbisLarge
      Text1.AssignedValues = []
      Text2.AssignedValues = []
      Text3.AssignedValues = [avFont]
      Text3.Font.Charset = DEFAULT_CHARSET
      Text3.Font.Color = 536870912
      Text3.Font.Height = -16
      Text3.Font.Name = 'Segoe UI'
      Text3.Font.Style = []
      Text3.Value = 'Tasks'
      Text4.AssignedValues = []
      OnActivateDetail = tbiEmployeesActivateDetail
      OnClick = tbiEmployeesClick
    end
    object tbiTasksPrint: TdxTileBarItem
      Tag = 2
      GroupIndex = 0
      IndexInGroup = 1
      Size = tbisRegular
      Text1.AssignedValues = []
      Text2.AssignedValues = []
      Text3.AssignedValues = []
      Text4.AssignedValues = []
      Visible = False
      OnActivateDetail = tbiEmployeesActivateDetail
      OnDeactivatingDetail = tbiEmployeeEditDeactivatingDetail
    end
    object tbiEmployees: TdxTileBarItem
      Tag = 3
      Glyph.Align = oaTopLeft
      Glyph.ImageIndex = 2
      GroupIndex = 0
      IndexInGroup = 2
      Size = tbisLarge
      Style.GradientBeginColor = 2064338
      Text1.AssignedValues = []
      Text2.AssignedValues = []
      Text3.AssignedValues = [avFont]
      Text3.Font.Charset = DEFAULT_CHARSET
      Text3.Font.Color = 536870912
      Text3.Font.Height = -16
      Text3.Font.Name = 'Segoe UI'
      Text3.Font.Style = []
      Text3.Value = 'Employees'
      Text4.AssignedValues = []
      OnActivateDetail = tbiEmployeesActivateDetail
      OnClick = tbiEmployeesClick
      PopupOptions.BorderColor = clRed
    end
    object tbiEmployeeEdit: TdxTileBarItem
      Tag = 4
      GroupIndex = 0
      IndexInGroup = 3
      Size = tbisRegular
      Text1.AssignedValues = []
      Text2.AssignedValues = []
      Text3.AssignedValues = []
      Text4.AssignedValues = []
      Visible = False
      OnActivateDetail = tbiEmployeesActivateDetail
      OnDeactivatingDetail = tbiEmployeeEditDeactivatingDetail
    end
    object tbiProducts: TdxTileBarItem
      Tag = 5
      Glyph.Align = oaTopLeft
      Glyph.ImageIndex = 3
      GroupIndex = 1
      IndexInGroup = 0
      Size = tbisLarge
      Style.GradientBeginColor = 12420127
      Text1.AssignedValues = []
      Text2.AssignedValues = []
      Text3.AssignedValues = [avFont]
      Text3.Font.Charset = DEFAULT_CHARSET
      Text3.Font.Color = 536870912
      Text3.Font.Height = -16
      Text3.Font.Name = 'Segoe UI'
      Text3.Font.Style = []
      Text3.Value = 'Products'
      Text4.AssignedValues = []
      OnActivateDetail = tbiEmployeesActivateDetail
      OnClick = tbiEmployeesClick
      PopupOptions.PopupControl = ptcProducts
    end
    object tbiProductEdit: TdxTileBarItem
      Tag = 6
      GroupIndex = 1
      IndexInGroup = 1
      Size = tbisLarge
      Text1.AssignedValues = []
      Text2.AssignedValues = []
      Text3.AssignedValues = []
      Text4.AssignedValues = []
      Visible = False
      OnActivateDetail = tbiEmployeesActivateDetail
      OnDeactivatingDetail = tbiEmployeeEditDeactivatingDetail
    end
    object tbiCustomers: TdxTileBarItem
      Tag = 7
      Glyph.Align = oaTopLeft
      Glyph.ImageIndex = 4
      GroupIndex = 1
      IndexInGroup = 2
      Size = tbisLarge
      Style.GradientBeginColor = 5723991
      Text1.AssignedValues = []
      Text2.AssignedValues = []
      Text3.AssignedValues = [avFont]
      Text3.Font.Charset = DEFAULT_CHARSET
      Text3.Font.Color = 536870912
      Text3.Font.Height = -16
      Text3.Font.Name = 'Segoe UI'
      Text3.Font.Style = []
      Text3.Value = 'Customers'
      Text4.AssignedValues = []
      OnActivateDetail = tbiEmployeesActivateDetail
      OnClick = tbiEmployeesClick
      PopupOptions.PopupControl = ptcCustomers
    end
    object tbiCustomerEdit: TdxTileBarItem
      Tag = 8
      GroupIndex = 1
      IndexInGroup = 3
      Size = tbisLarge
      Text1.AssignedValues = []
      Text2.AssignedValues = []
      Text3.AssignedValues = []
      Text4.AssignedValues = []
      Visible = False
      OnActivateDetail = tbiEmployeesActivateDetail
      OnDeactivatingDetail = tbiEmployeeEditDeactivatingDetail
    end
    object tbiSales: TdxTileBarItem
      Tag = 9
      Glyph.Align = oaTopLeft
      Glyph.ImageIndex = 5
      GroupIndex = 1
      IndexInGroup = 4
      Size = tbisLarge
      Style.GradientBeginColor = 5275989
      Text1.AssignedValues = []
      Text2.AssignedValues = []
      Text3.AssignedValues = [avFont]
      Text3.Font.Charset = DEFAULT_CHARSET
      Text3.Font.Color = 536870912
      Text3.Font.Height = -16
      Text3.Font.Name = 'Segoe UI'
      Text3.Font.Style = []
      Text3.Value = 'Sales'
      Text4.AssignedValues = []
      OnActivateDetail = tbiEmployeesActivateDetail
      OnClick = tbiEmployeesClick
    end
    object tbiSaleView: TdxTileBarItem
      Tag = 10
      GroupIndex = 1
      IndexInGroup = 5
      Size = tbisRegular
      Text1.AssignedValues = []
      Text2.AssignedValues = []
      Text3.AssignedValues = []
      Text4.AssignedValues = []
      Visible = False
      OnActivateDetail = tbiEmployeesActivateDetail
      OnDeactivatingDetail = tbiEmployeeEditDeactivatingDetail
    end
    object tbiSalesPrint: TdxTileBarItem
      Tag = 11
      GroupIndex = 1
      IndexInGroup = 6
      Size = tbisLarge
      Text1.AssignedValues = []
      Text2.AssignedValues = []
      Text3.AssignedValues = []
      Text4.AssignedValues = []
      Visible = False
      OnActivateDetail = tbiEmployeesActivateDetail
      OnDeactivatingDetail = tbiEmployeeEditDeactivatingDetail
    end
    object tbiLanguages: TdxTileBarItem
      Tag = 8
      Glyph.Align = oaTopLeft
      Glyph.ImageIndex = 7
      GroupIndex = 0
      IndexInGroup = 4
      Size = tbisLarge
      Style.GradientBeginColor = 12615808
      Text1.AssignedValues = []
      Text2.AssignedValues = []
      Text3.AssignedValues = [avFont]
      Text3.Font.Charset = DEFAULT_CHARSET
      Text3.Font.Color = 536870912
      Text3.Font.Height = -16
      Text3.Font.Name = 'Segoe UI'
      Text3.Font.Style = []
      Text3.Value = 'Language'
      Text4.AssignedValues = []
      OnClick = tbiLanguagesClick
    end
  end
  object ptcProducts: TdxTileControl
    Left = 160
    Top = 176
    Width = 721
    Height = 113
    Align = alNone
    OptionsBehavior.ItemCheckMode = tcicmNone
    OptionsBehavior.ItemFocusMode = tcifmOuterFrame
    OptionsBehavior.ItemHotTrackMode = tcihtmNone
    OptionsBehavior.ItemMoving = False
    OptionsBehavior.ItemPressAnimation = False
    OptionsBehavior.ScrollMode = smScrollButtons
    OptionsView.FixedIndentHorz = True
    OptionsView.GroupLayout = glVertical
    OptionsView.GroupMaxRowCount = 1
    OptionsView.IndentHorz = 24
    OptionsView.IndentVert = 7
    OptionsView.ItemHeight = 40
    OptionsView.ItemIndent = 13
    OptionsView.ItemWidth = 72
    Style.GradientBeginColor = clFuchsia
    TabOrder = 1
    Transparent = True
    object ptcProductsFilterGroup: TdxTileControlGroup
      Caption.Font.Charset = DEFAULT_CHARSET
      Caption.Font.Color = clWhite
      Caption.Font.Height = -16
      Caption.Font.Name = 'Segoe UI'
      Caption.Font.Style = []
      Caption.Text = 'CUSTOM FILTER'
      Index = 0
    end
    object ptiHDVideoPlayer: TdxTileControlItem
      GroupIndex = 0
      IndexInGroup = 0
      Size = tcisLarge
      Style.GradientBeginColor = clWhite
      Text1.Align = oaMiddleCenter
      Text1.AssignedValues = [avFont]
      Text1.Font.Charset = DEFAULT_CHARSET
      Text1.Font.Color = clBlack
      Text1.Font.Height = -16
      Text1.Font.Name = 'Segoe UI'
      Text1.Font.Style = []
      Text1.Value = 'HD Video Player'
      Text2.AssignedValues = []
      Text3.AssignedValues = []
      Text4.AssignedValues = []
      OnClick = ptiHDVideoPlayerClick
    end
    object pti50inchPlasma: TdxTileControlItem
      Tag = 1
      GroupIndex = 0
      IndexInGroup = 1
      Size = tcisLarge
      Style.GradientBeginColor = clWhite
      Text1.Align = oaMiddleCenter
      Text1.AssignedValues = [avFont]
      Text1.Font.Charset = DEFAULT_CHARSET
      Text1.Font.Color = clBlack
      Text1.Font.Height = -16
      Text1.Font.Name = 'Segoe UI'
      Text1.Font.Style = []
      Text1.Value = '50inch Plasma'
      Text2.AssignedValues = []
      Text3.AssignedValues = []
      Text4.AssignedValues = []
      OnClick = ptiHDVideoPlayerClick
    end
    object pti21inchMonitor: TdxTileControlItem
      Tag = 2
      GroupIndex = 0
      IndexInGroup = 2
      Size = tcisLarge
      Style.GradientBeginColor = clWhite
      Text1.Align = oaMiddleCenter
      Text1.AssignedValues = [avFont]
      Text1.Font.Charset = DEFAULT_CHARSET
      Text1.Font.Color = clBlack
      Text1.Font.Height = -16
      Text1.Font.Name = 'Segoe UI'
      Text1.Font.Style = []
      Text1.Value = '21inch Monitor'
      Text2.AssignedValues = []
      Text3.AssignedValues = []
      Text4.AssignedValues = []
      OnClick = ptiHDVideoPlayerClick
    end
    object ptiRemoteControl: TdxTileControlItem
      Tag = 3
      GroupIndex = 0
      IndexInGroup = 3
      Size = tcisLarge
      Style.GradientBeginColor = clWhite
      Text1.Align = oaMiddleCenter
      Text1.AssignedValues = [avFont]
      Text1.Font.Charset = DEFAULT_CHARSET
      Text1.Font.Color = clBlack
      Text1.Font.Height = -16
      Text1.Font.Name = 'Segoe UI'
      Text1.Font.Style = []
      Text1.Value = 'Remote Control'
      Text2.AssignedValues = []
      Text3.AssignedValues = []
      Text4.AssignedValues = []
      OnClick = ptiHDVideoPlayerClick
    end
  end
  object ptcCustomers: TdxTileControl
    Left = 152
    Top = 523
    Width = 721
    Height = 110
    Align = alNone
    OptionsBehavior.ItemCheckMode = tcicmNone
    OptionsBehavior.ItemFocusMode = tcifmOuterFrame
    OptionsBehavior.ItemHotTrackMode = tcihtmNone
    OptionsBehavior.ItemMoving = False
    OptionsBehavior.ItemPressAnimation = False
    OptionsBehavior.ScrollMode = smScrollButtons
    OptionsView.FixedIndentHorz = True
    OptionsView.FixedIndentVert = True
    OptionsView.IndentHorz = 24
    OptionsView.IndentVert = 7
    OptionsView.ItemHeight = 40
    OptionsView.ItemIndent = 13
    OptionsView.ItemWidth = 72
    TabOrder = 2
    Transparent = True
    object ptcCustomersFilterGroup: TdxTileControlGroup
      Caption.Font.Charset = DEFAULT_CHARSET
      Caption.Font.Color = clWhite
      Caption.Font.Height = -16
      Caption.Font.Name = 'Segoe UI'
      Caption.Font.Style = []
      Caption.Text = 'CUSTOM FILTER'
      Index = 0
    end
    object ptiAllCustomers: TdxTileControlItem
      GroupIndex = 0
      IndexInGroup = 0
      Size = tcisLarge
      Style.GradientBeginColor = clWhite
      Text1.Align = oaMiddleCenter
      Text1.AssignedValues = [avFont]
      Text1.Font.Charset = DEFAULT_CHARSET
      Text1.Font.Color = clBlack
      Text1.Font.Height = -16
      Text1.Font.Name = 'Segoe UI'
      Text1.Font.Style = []
      Text1.Value = 'All Customers'
      Text2.AssignedValues = []
      Text3.AssignedValues = []
      Text4.AssignedValues = []
      OnClick = ptiAllCustomersClick
    end
    object ptiMyAccount: TdxTileControlItem
      Tag = 1
      GroupIndex = 0
      IndexInGroup = 1
      Size = tcisLarge
      Style.GradientBeginColor = clWhite
      Text1.Align = oaMiddleCenter
      Text1.AssignedValues = [avFont]
      Text1.Font.Charset = DEFAULT_CHARSET
      Text1.Font.Color = clBlack
      Text1.Font.Height = -16
      Text1.Font.Name = 'Segoe UI'
      Text1.Font.Style = []
      Text1.Value = 'My Account'
      Text2.AssignedValues = []
      Text3.AssignedValues = []
      Text4.AssignedValues = []
      OnClick = ptiAllCustomersClick
    end
    object ptiJohnAccount: TdxTileControlItem
      Tag = 2
      GroupIndex = 0
      IndexInGroup = 2
      Size = tcisLarge
      Style.GradientBeginColor = clWhite
      Text1.Align = oaMiddleCenter
      Text1.AssignedValues = [avFont]
      Text1.Font.Charset = DEFAULT_CHARSET
      Text1.Font.Color = clBlack
      Text1.Font.Height = -16
      Text1.Font.Name = 'Segoe UI'
      Text1.Font.Style = []
      Text1.Value = 'John'#39's Account'
      Text2.AssignedValues = []
      Text3.AssignedValues = []
      Text4.AssignedValues = []
      OnClick = ptiAllCustomersClick
    end
    object ptiTopStores: TdxTileControlItem
      Tag = 3
      GroupIndex = 0
      IndexInGroup = 3
      Size = tcisLarge
      Style.GradientBeginColor = clWhite
      Text1.Align = oaMiddleCenter
      Text1.AssignedValues = [avFont]
      Text1.Font.Charset = DEFAULT_CHARSET
      Text1.Font.Color = clBlack
      Text1.Font.Height = -16
      Text1.Font.Name = 'Segoe UI'
      Text1.Font.Style = []
      Text1.Value = 'Top Stores'
      Text2.AssignedValues = []
      Text3.AssignedValues = []
      Text4.AssignedValues = []
      OnClick = ptiAllCustomersClick
    end
  end
  object cxGroupBox1: TcxGroupBox
    Left = 0
    Top = 0
    Align = alTop
    PanelStyle.Active = True
    ParentBackground = False
    ParentColor = False
    ParentFont = False
    Style.BorderStyle = ebsNone
    Style.Color = 4144959
    Style.LookAndFeel.NativeStyle = True
    StyleDisabled.LookAndFeel.NativeStyle = True
    TabOrder = 3
    Height = 32
    Width = 1784
  end
  object ptcLanguages: TdxTileControl
    Left = 8
    Top = 314
    Width = 425
    Height = 95
    Align = alCustom
    LookAndFeel.SkinName = ''
    OptionsBehavior.ItemCheckMode = tcicmNone
    OptionsBehavior.ItemFocusMode = tcifmOuterFrame
    OptionsBehavior.ItemHotTrackMode = tcihtmNone
    OptionsBehavior.ItemMoving = False
    OptionsBehavior.ItemPressAnimation = False
    OptionsBehavior.ScrollMode = smScrollButtons
    OptionsView.FixedIndentHorz = True
    OptionsView.FixedIndentVert = True
    OptionsView.GroupMaxRowCount = 1
    OptionsView.IndentHorz = 24
    OptionsView.IndentVert = 7
    OptionsView.ItemHeight = 40
    OptionsView.ItemIndent = 13
    OptionsView.ItemWidth = 72
    Style.GradientBeginColor = 12615808
    TabOrder = 4
    Visible = False
    object ptcLanguageLeftToRightGroup: TdxTileControlGroup
      Caption.Font.Charset = DEFAULT_CHARSET
      Caption.Font.Color = clWhite
      Caption.Font.Height = -16
      Caption.Font.Name = 'Segoe UI'
      Caption.Font.Style = []
      Caption.Text = 'LEFT TO RIGHT'
      Index = 0
    end
    object ptcLanguageRightToLeftGroup: TdxTileControlGroup
      Caption.Font.Charset = DEFAULT_CHARSET
      Caption.Font.Color = clWhite
      Caption.Font.Height = -16
      Caption.Font.Name = 'Segoe UI'
      Caption.Font.Style = []
      Caption.Text = 'RIGHT TO LEFT'
      Index = 1
    end
    object ptcLanguageEnglish: TdxTileControlItem
      GroupIndex = 0
      IndexInGroup = 0
      Size = tcisLarge
      Style.GradientBeginColor = clWhite
      Text1.Align = oaMiddleCenter
      Text1.AssignedValues = [avFont]
      Text1.Font.Charset = DEFAULT_CHARSET
      Text1.Font.Color = clBlack
      Text1.Font.Height = -16
      Text1.Font.Name = 'Segoe UI'
      Text1.Font.Style = []
      Text1.Value = 'English'
      Text2.AssignedValues = []
      Text3.AssignedValues = []
      Text4.AssignedValues = []
      OnClick = ptcLanguageFarsiClick
    end
    object ptcLanguageArabic: TdxTileControlItem
      Tag = 1025
      GroupIndex = 1
      IndexInGroup = 0
      Size = tcisLarge
      Style.GradientBeginColor = clWhite
      Text1.Align = oaMiddleCenter
      Text1.AssignedValues = [avFont]
      Text1.Font.Charset = DEFAULT_CHARSET
      Text1.Font.Color = clBlack
      Text1.Font.Height = -16
      Text1.Font.Name = 'Segoe UI'
      Text1.Font.Style = []
      Text1.Value = 'Arabic'
      Text2.AssignedValues = []
      Text3.AssignedValues = []
      Text4.AssignedValues = []
      OnClick = ptcLanguageArabicClick
    end
  end
  object dxSkinController1: TdxSkinController
    SkinName = 'HybridApp'
    TouchMode = True
    Left = 80
    Top = 264
  end
  object dxCalloutPopup1: TdxCalloutPopup
    PopupControl = ptcLanguages
    Left = 992
    Top = 288
  end
end
