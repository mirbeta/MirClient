object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'ExpressNavBar Photo Studio Demo'
  ClientHeight = 821
  ClientWidth = 1164
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object dxGalleryControl2: TdxGalleryControl
    Left = 0
    Top = 720
    Width = 1164
    Height = 101
    Align = alBottom
    AutoSizeMode = asAutoHeight
    BorderStyle = cxcbsNone
    OptionsBehavior.ItemCheckMode = icmSingleCheck
    OptionsView.ColumnCount = 21
    OptionsView.ContentOffsetItems.All = -1
    OptionsView.ContentOffsetItems.Left = 6
    OptionsView.ContentOffsetItems.Top = 1
    OptionsView.ContentOffsetItems.Right = 6
    OptionsView.ContentOffsetItems.Bottom = 1
    OptionsView.Item.Image.ShowFrame = False
    OptionsView.Item.Image.Size.Height = 80
    OptionsView.Item.Image.Size.Width = 125
    OptionsView.Item.Text.AlignVert = vaCenter
    TabOrder = 0
    OnItemClick = dxGalleryControl2ItemClick
    object dxGalleryControl2Group1: TdxGalleryControlGroup
      Caption = 'New Group'
      ShowCaption = False
    end
  end
  object dxNavBar1: TdxNavBar
    Left = 855
    Top = 0
    Width = 309
    Height = 720
    Align = alRight
    ActiveGroupIndex = 0
    TabOrder = 1
    View = 20
    OptionsBehavior.Common.AllowChildGroups = True
    OptionsBehavior.Common.AllowExpandAnimation = True
    object dxNavBar1Group1: TdxNavBarGroup
      Caption = 'Properties'
      SelectedLinkIndex = -1
      TopVisibleLinkIndex = 0
      OptionsExpansion.AllowMultipleGroupExpansion = False
      Links = <>
      ParentGroupIndex = -1
      Position = 0
    end
    object dxNavBar1Group2: TdxNavBarGroup
      Caption = 'Image'
      SelectedLinkIndex = -1
      TopVisibleLinkIndex = 0
      OptionsGroupControl.ShowControl = True
      OptionsGroupControl.UseControl = True
      Links = <>
      ParentGroupIndex = -1
      Position = 1
    end
    object ngFilters: TdxNavBarGroup
      Caption = 'Filters'
      SelectedLinkIndex = -1
      TopVisibleLinkIndex = 0
      OptionsGroupControl.ShowControl = True
      OptionsGroupControl.UseControl = True
      Links = <>
      ParentGroupIndex = 0
      Position = 0
    end
    object ngColors: TdxNavBarGroup
      Caption = 'Color'
      SelectedLinkIndex = -1
      TopVisibleLinkIndex = 0
      OptionsGroupControl.ShowControl = True
      OptionsGroupControl.UseControl = True
      OptionsExpansion.Expanded = False
      Links = <>
      ParentGroupIndex = 0
      Position = 1
    end
    object ngBrightnessContrast: TdxNavBarGroup
      Caption = 'Brightness/Contrast'
      SelectedLinkIndex = -1
      TopVisibleLinkIndex = 0
      OptionsGroupControl.ShowControl = True
      OptionsGroupControl.UseControl = True
      OptionsExpansion.Expanded = False
      Links = <>
      ParentGroupIndex = 0
      Position = 2
    end
    object dxNavBar1Group2Control: TdxNavBarGroupControl
      Left = 2
      Top = 352
      Width = 305
      Height = 280
      Caption = 'dxNavBar1Group2Control'
      TabOrder = 3
      UseStyle = True
      GroupIndex = 1
      OriginalHeight = 280
      object cxGroupBox2: TcxGroupBox
        Left = 0
        Top = 221
        Align = alClient
        Style.BorderStyle = ebsNone
        TabOrder = 0
        Transparent = True
        Height = 59
        Width = 305
        object dxRatingControl1: TdxRatingControl
          Left = 216
          Top = 3
          Anchors = [akTop, akRight]
          TabOrder = 0
          Transparent = True
        end
        object lblImageFileName: TcxLabel
          Left = 5
          Top = 3
          Caption = 'lblImageFileName'
          Transparent = True
        end
        object lblImageFileInfo: TcxLabel
          Left = 5
          Top = 19
          Caption = 'lblImageFileInfo'
          Style.TextColor = 10000536
          Transparent = True
        end
      end
      object cxImage2: TcxImage
        Left = 0
        Top = 0
        Align = alTop
        Properties.FitMode = ifmProportionalStretch
        Properties.ReadOnly = True
        Properties.ShowFocusRect = False
        Style.BorderStyle = ebsNone
        Style.Edges = [bLeft, bTop, bRight, bBottom]
        Style.HotTrack = False
        TabOrder = 1
        Transparent = True
        Height = 221
        Width = 305
      end
    end
    object gcFilters: TdxNavBarGroupControl
      Left = 2
      Top = 64
      Width = 305
      Height = 198
      Caption = 'gcFilters'
      TabOrder = 0
      GroupIndex = 2
      OriginalHeight = 198
      object dxGalleryControl1: TdxGalleryControl
        Left = 0
        Top = 0
        Width = 305
        Height = 198
        Align = alClient
        BorderStyle = cxcbsNone
        OptionsBehavior.ItemCheckMode = icmSingleCheck
        OptionsView.ColumnCount = 3
        OptionsView.ContentOffset.All = -1
        OptionsView.ContentOffset.Left = 0
        OptionsView.ContentOffset.Top = 10
        OptionsView.ContentOffset.Right = 0
        OptionsView.ContentOffset.Bottom = 0
        OptionsView.ContentOffsetItems.All = 4
        OptionsView.Item.Image.ShowFrame = False
        OptionsView.Item.Image.Size.Height = 60
        OptionsView.Item.Image.Size.Width = 80
        OptionsView.Item.Text.AlignVert = vaCenter
        OptionsView.Item.Text.Position = posBottom
        TabOrder = 0
        OnItemClick = dxGalleryControl1ItemClick
        object dxGalleryControl1Group1: TdxGalleryControlGroup
          Caption = 'New Group'
          ShowCaption = False
        end
      end
    end
    object gcColor: TdxNavBarGroupControl
      Left = 2
      Top = 89
      Width = 305
      Height = 88
      Caption = 'gcColor'
      TabOrder = 1
      UseStyle = True
      GroupIndex = 3
      OriginalHeight = 88
      object tbR: TcxTrackBar
        Left = 43
        Top = 7
        Properties.AutoSize = False
        Properties.Max = 255
        Properties.ShowTicks = False
        Properties.ThumbStep = cxtsJump
        Properties.OnChange = tbRGBPropertiesChange
        TabOrder = 0
        Transparent = True
        Height = 25
        Width = 252
      end
      object tbG: TcxTrackBar
        Left = 43
        Top = 32
        Properties.AutoSize = False
        Properties.Max = 255
        Properties.ShowTicks = False
        Properties.ThumbStep = cxtsJump
        Properties.OnChange = tbRGBPropertiesChange
        TabOrder = 1
        Transparent = True
        Height = 25
        Width = 252
      end
      object tbB: TcxTrackBar
        Left = 43
        Top = 56
        Properties.AutoSize = False
        Properties.Max = 255
        Properties.ShowTicks = False
        Properties.ThumbStep = cxtsJump
        Properties.OnChange = tbRGBPropertiesChange
        TabOrder = 2
        Transparent = True
        Height = 25
        Width = 252
      end
      object cxLabel1: TcxLabel
        Left = 23
        Top = 8
        Caption = 'R:'
        Transparent = True
      end
      object cxLabel2: TcxLabel
        Left = 23
        Top = 33
        Caption = 'G:'
        Transparent = True
      end
      object cxLabel3: TcxLabel
        Left = 23
        Top = 58
        Caption = 'B:'
        Transparent = True
      end
    end
    object ngBrightnessContrastControl: TdxNavBarGroupControl
      Left = 2
      Top = 114
      Width = 305
      Height = 63
      TabOrder = 2
      UseStyle = True
      GroupIndex = 4
      OriginalHeight = 63
      object tbBrightness: TcxTrackBar
        Left = 80
        Top = 7
        Properties.AutoSize = False
        Properties.Max = 255
        Properties.ShowTicks = False
        Properties.ThumbStep = cxtsJump
        Properties.OnChange = tbContrastPropertiesChange
        TabOrder = 0
        Transparent = True
        Height = 25
        Width = 215
      end
      object tbContrast: TcxTrackBar
        Left = 80
        Top = 32
        Properties.AutoSize = False
        Properties.Max = 255
        Properties.ShowTicks = False
        Properties.ThumbStep = cxtsJump
        Properties.OnChange = tbContrastPropertiesChange
        TabOrder = 1
        Transparent = True
        Height = 25
        Width = 215
      end
      object cxLabel4: TcxLabel
        Left = 22
        Top = 31
        Caption = 'Contrast:'
        Transparent = True
      end
      object cxLabel5: TcxLabel
        Left = 23
        Top = 8
        Caption = 'Brightness:'
        Transparent = True
      end
    end
  end
  object cxImage1: TcxImage
    Left = 0
    Top = 0
    Align = alClient
    Properties.FitMode = ifmProportionalStretch
    Properties.ReadOnly = True
    Properties.ShowFocusRect = False
    Style.BorderStyle = ebsNone
    Style.Edges = [bLeft, bTop, bRight, bBottom]
    Style.Font.Charset = DEFAULT_CHARSET
    Style.Font.Color = clWindowText
    Style.Font.Height = -21
    Style.Font.Name = 'Tahoma'
    Style.Font.Style = []
    Style.HotTrack = False
    Style.IsFontAssigned = True
    TabOrder = 2
    Transparent = True
    Height = 720
    Width = 855
  end
end
