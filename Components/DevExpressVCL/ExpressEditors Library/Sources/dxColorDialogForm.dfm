object dxColorDialogForm: TdxColorDialogForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  BorderWidth = 10
  Caption = 'Color Editor'
  ClientHeight = 402
  ClientWidth = 730
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 200
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottom: TcxGroupBox
    Left = 0
    Top = 362
    Align = alBottom
    PanelStyle.Active = True
    Style.BorderStyle = ebsNone
    Style.LookAndFeel.NativeStyle = True
    StyleDisabled.LookAndFeel.NativeStyle = True
    TabOrder = 4
    Transparent = True
    Height = 40
    Width = 730
    object bvlSeparator: TdxBevel
      Left = 0
      Top = 2
      Width = 730
      Height = 4
      Anchors = [akLeft, akTop, akRight]
      Shape = dxbsLineTop
    end
    object btnOK: TcxButton
      AlignWithMargins = True
      Left = 562
      Top = 14
      Width = 80
      Height = 24
      Margins.Top = 12
      Margins.Bottom = 0
      Align = alRight
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TcxButton
      AlignWithMargins = True
      Left = 648
      Top = 14
      Width = 80
      Height = 24
      Margins.Top = 12
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alRight
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object cpColorPicker: TdxColorPicker
    Left = 286
    Top = 0
    Width = 444
    Height = 321
    Visible = False
    Color = 0
    OnColorChanged = cpColorPickerColorChanged
  end
  object btnDefineCustomColors: TcxButton
    Left = 0
    Top = 327
    Width = 280
    Height = 25
    Caption = '&Define Custom Colors >>'
    TabOrder = 1
    OnClick = btnDefineCustomColorsClick
  end
  object gcPalette: TdxGalleryControl
    Left = 0
    Top = 0
    Width = 280
    Height = 321
    AutoSizeMode = asAutoSize
    OptionsBehavior.ItemCheckMode = icmSingleRadio
    OptionsView.ColumnCount = 8
    OptionsView.ContentOffsetGroups.All = -1
    OptionsView.ContentOffsetGroups.Left = 0
    OptionsView.ContentOffsetGroups.Top = 2
    OptionsView.ContentOffsetGroups.Right = 0
    OptionsView.ContentOffsetGroups.Bottom = 2
    OptionsView.ContentOffsetItems.All = 3
    OptionsView.Item.Image.Size.Height = 17
    OptionsView.Item.Image.Size.Width = 19
    TabOrder = 0
    OnItemClick = gcPaletteItemClick
    object gcPaletteGroup1: TdxGalleryControlGroup
      Caption = 'New Group'
    end
    object gcPaletteGroup2: TdxGalleryControlGroup
      Caption = 'New Group'
    end
  end
  object btnAddtoCustomColors: TcxButton
    Left = 286
    Top = 327
    Width = 444
    Height = 25
    Caption = '&Add to Custom Colors'
    TabOrder = 2
    OnClick = btnAddtoCustomColorsClick
  end
end
