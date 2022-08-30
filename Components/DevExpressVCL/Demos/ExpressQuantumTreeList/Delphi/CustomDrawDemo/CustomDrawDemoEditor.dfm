object CustomDrawDemoEditorForm: TCustomDrawDemoEditorForm
  Left = 344
  Top = 235
  ActiveControl = tlCustomDrawItems
  BorderStyle = bsDialog
  Caption = 'Custom Draw Settings'
  ClientHeight = 246
  ClientWidth = 425
  Color = 15451300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnClose: TcxButton
    Left = 342
    Top = 211
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    Default = True
    ModalResult = 2
    TabOrder = 2
    OnClick = btnCloseClick
    LookAndFeel.NativeStyle = True
  end
  object tlCustomDrawItems: TcxTreeList
    Left = 8
    Top = 8
    Width = 129
    Height = 228
    Bands = <
      item
        Caption.Text = 'Band + 1'
      end>
    OptionsBehavior.ImmediateEditor = False
    OptionsBehavior.DragExpand = False
    OptionsBehavior.MultiSort = False
    OptionsBehavior.ShowHourGlass = False
    OptionsBehavior.Sorting = False
    OptionsCustomizing.BandCustomizing = False
    OptionsCustomizing.BandHorzSizing = False
    OptionsCustomizing.BandMoving = False
    OptionsCustomizing.BandVertSizing = False
    OptionsCustomizing.ColumnCustomizing = False
    OptionsCustomizing.ColumnHorzSizing = False
    OptionsCustomizing.ColumnMoving = False
    OptionsCustomizing.ColumnVertSizing = False
    OptionsData.Editing = False
    OptionsData.Deleting = False
    OptionsSelection.InvertSelect = False
    OptionsView.CellAutoHeight = True
    OptionsView.Buttons = False
    OptionsView.ColumnAutoWidth = True
    OptionsView.ShowRoot = False
    Styles.Background = CustomDrawDemoDataDM.cxStyle7
    Styles.Content = CustomDrawDemoDataDM.cxStyle7
    Styles.Inactive = CustomDrawDemoDataDM.cxStyle6
    Styles.Selection = CustomDrawDemoDataDM.cxStyle6
    Styles.ColumnHeader = CustomDrawDemoDataDM.cxStyle4
    TabOrder = 0
    OnSelectionChanged = tlCustomDrawItemsSelectionChanged
    object tlCustomDrawItemscxTreeListColumn1: TcxTreeListColumn
      Caption.Text = 'Draw Item'
      DataBinding.ValueType = 'String'
      Options.Sorting = False
      Width = 127
      Position.ColIndex = 0
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
  end
  object gbEventHandlerSettings: TGroupBox
    Left = 144
    Top = 8
    Width = 273
    Height = 193
    Caption = 'Event Handler Settings'
    TabOrder = 1
    object lbFont: TLabel
      Left = 142
      Top = 161
      Width = 60
      Height = 13
      Caption = 'Choose Font'
    end
    object sbFont: TSpeedButton
      Left = 216
      Top = 156
      Width = 23
      Height = 22
      Glyph.Data = {
        66010000424D6601000000000000760000002800000016000000140000000100
        040000000000F000000000000000000000001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00800000000000
        0000000000008FFFFFFFFFFFFFFFFFFFF0008FF44FFFFFFFFFFFFFFFF0008FF4
        4FFFFFFFFFFFFFFFF0008FF44FFFFFFFFF818FFFF0008FF444FFFFFFFFF1FFFF
        F0008FF44FFFFFFFFFF1F8FFF0008FF44FFFFFFFFFF111FFF0008FF4444FF55F
        FFF1F8FFF0008FFFFFFFFF85FFF1FF8FF0008FFFFFFFFFF5FF81111FF0008FFF
        FFFFFFF5FFFFFFFFF0008FFFFFFFFF555FFFFFFFF0008FFFFFFFFFF5FFFFFFFF
        F0008FFFFFFFFFF58FFFFFFFF0008FFFFFFFFFFF55FFFFFFF0008FFFFFFFFFFF
        FFFFFFFFF0008000000000000000000000008F0CCCCCCCCCCCCCC0F0F0008888
        88888888888888888800}
      OnClick = sbFontClick
    end
    object bvSeparator: TBevel
      Left = 16
      Top = 145
      Width = 249
      Height = 4
      Shape = bsBottomLine
    end
    object cbGradient: TcxComboBox
      Left = 138
      Top = 48
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cbGradientPropertiesChange
      Style.Color = 16247513
      TabOrder = 3
      Width = 120
    end
    object mruBkImage: TcxMRUEdit
      Left = 138
      Top = 24
      Properties.DropDownListStyle = lsFixedList
      Properties.ReadOnly = False
      Properties.OnButtonClick = mruBkImagePropertiesButtonClick
      Properties.OnEditValueChanged = mruBkImagePropertiesEditValueChanged
      Style.BorderStyle = ebsUltraFlat
      Style.Color = 16247513
      TabOrder = 1
      Width = 120
    end
    object rbBackGroundImage: TcxRadioButton
      Left = 8
      Top = 24
      Width = 130
      Height = 17
      Caption = '&Background Image'
      Checked = True
      Color = 15451300
      ParentColor = False
      TabOrder = 0
      TabStop = True
      OnClick = rbRadioButtonClick
    end
    object rbGradient: TcxRadioButton
      Tag = 1
      Left = 8
      Top = 48
      Width = 130
      Height = 17
      Caption = '&Gradient'
      TabOrder = 2
      OnClick = rbRadioButtonClick
    end
    object rpendsOnTheData: TcxRadioButton
      Tag = 3
      Left = 8
      Top = 96
      Width = 130
      Height = 17
      Caption = 'D&epends On the Data'
      TabOrder = 4
      OnClick = rbRadioButtonClick
    end
    object rfaultDrawing: TcxRadioButton
      Tag = 2
      Left = 8
      Top = 72
      Width = 130
      Height = 17
      Caption = '&Default Drawing'
      TabOrder = 5
      OnClick = rbRadioButtonClick
    end
    object chbOwnerDrawText: TcxCheckBox
      Left = 11
      Top = 157
      Caption = 'Owner &draw text'
      Properties.OnChange = chbOwnerDrawTextPropertiesChange
      TabOrder = 6
      Width = 121
    end
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 352
  end
  object OpenDialog: TOpenDialog
    Filter = 'BMP Windows Bitmap|*.bmp'
    Left = 384
  end
end
