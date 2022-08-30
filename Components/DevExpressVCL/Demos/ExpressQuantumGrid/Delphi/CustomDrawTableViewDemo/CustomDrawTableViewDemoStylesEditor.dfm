object CustomDrawTableViewDemoStylesEditorForm: TCustomDrawTableViewDemoStylesEditorForm
  Left = 297
  Top = 173
  BorderStyle = bsDialog
  Caption = 'Custom Draw Settings'
  ClientHeight = 267
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
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object btnClose: TcxButton
    Left = 342
    Top = 232
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    Default = True
    ModalResult = 2
    TabOrder = 0
    OnClick = btnCloseClick
  end
  object tvCustomDrawItems: TTreeView
    Left = 8
    Top = 8
    Width = 129
    Height = 249
    Color = 16247513
    HideSelection = False
    HotTrack = True
    Indent = 19
    ReadOnly = True
    ShowButtons = False
    TabOrder = 1
    OnClick = tvCustomDrawItemsClick
    Items.Data = {
      02000000240000000000000000000000FFFFFFFFFFFFFFFF0000000006000000
      0B4D617374657220566965771D0000000000000000000000FFFFFFFFFFFFFFFF
      00000000000000000443656C6C250000000000000000000000FFFFFFFFFFFFFF
      FF00000000000000000C436F6C756D6E4865616465721F000000000000000000
      0000FFFFFFFFFFFFFFFF000000000000000006466F6F74657222000000000000
      0000000000FFFFFFFFFFFFFFFF00000000000000000947726F757043656C6C26
      0000000000000000000000FFFFFFFFFFFFFFFF00000000000000000D496E6469
      6361746F7243656C6C270000000000000000000000FFFFFFFFFFFFFFFF000000
      00000000000E506172744261636B67726F756E64240000000000000000000000
      FFFFFFFFFFFFFFFF00000000060000000B44657461696C20566965771D000000
      0000000000000000FFFFFFFFFFFFFFFF00000000000000000443656C6C250000
      000000000000000000FFFFFFFFFFFFFFFF00000000000000000C436F6C756D6E
      4865616465721F0000000000000000000000FFFFFFFFFFFFFFFF000000000000
      000006466F6F746572220000000000000000000000FFFFFFFFFFFFFFFF000000
      00000000000947726F757043656C6C260000000000000000000000FFFFFFFFFF
      FFFFFF00000000000000000D496E64696361746F7243656C6C27000000000000
      0000000000FFFFFFFFFFFFFFFF00000000000000000E506172744261636B6772
      6F756E64}
  end
  object gbEventHandlerSettings: TGroupBox
    Left = 144
    Top = 8
    Width = 273
    Height = 193
    Caption = 'Event Handler Settings'
    TabOrder = 2
    object lbFont: TLabel
      Left = 22
      Top = 145
      Width = 60
      Height = 13
      Caption = 'Choose Font'
    end
    object sbFont: TSpeedButton
      Left = 96
      Top = 140
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
      Top = 129
      Width = 249
      Height = 4
      Shape = bsBottomLine
    end
    object lbIndicatorGlyph: TLabel
      Left = 27
      Top = 96
      Width = 71
      Height = 13
      Caption = 'Indicator Glyph'
    end
    object cbGradient: TcxComboBox
      Left = 138
      Top = 48
      Width = 120
      Height = 21
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'Grey'
        'Blue'
        'Gold'
        'Green')
      Properties.OnChange = cbGradientPropertiesChange
      Style.Color = 16247513
      TabOrder = 3
      Text = 'Grey'
    end
    object mruBkImage: TcxMRUEdit
      Left = 138
      Top = 24
      Width = 120
      Height = 21
      Properties.LookupItems.Strings = (
        'Tile'
        'Sky'
        'Egypt'
        'My Face')
      Properties.ReadOnly = False
      Properties.OnButtonClick = mruBkImagePropertiesButtonClick
      Properties.OnChange = mruBkImagePropertiesChange
      Style.BorderStyle = ebsUltraFlat
      Style.Color = 16247513
      TabOrder = 1
      Text = 'Tile'
      OnKeyPress = mruBkImageKeyPress
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
      OnClick = rbBackGroundImageClick
    end
    object rbGradient: TcxRadioButton
      Left = 8
      Top = 48
      Width = 130
      Height = 17
      Caption = '&Gradient'
      TabOrder = 2
      OnClick = rbGradientClick
    end
    object rbDependsOnTheData: TcxRadioButton
      Left = 8
      Top = 96
      Width = 130
      Height = 17
      Caption = 'D&epends On the Data'
      TabOrder = 4
      OnClick = rbDependsOnTheDataClick
    end
    object rbDafaultDrawing: TcxRadioButton
      Left = 8
      Top = 72
      Width = 130
      Height = 17
      Caption = '&Default Drawing'
      TabOrder = 5
      OnClick = rbDafaultDrawingClick
    end
    object pnSampleText: TPanel
      Left = 144
      Top = 140
      Width = 121
      Height = 41
      BevelInner = bvLowered
      BevelOuter = bvNone
      BevelWidth = 2
      Caption = 'Sample Text'
      Color = 16247513
      TabOrder = 6
    end
    object icbIndicatorImages: TcxImageComboBox
      Left = 138
      Top = 93
      Width = 120
      Height = 21
      Properties.DefaultDescription = 'Clubs'
      Properties.DefaultImageIndex = 0
      Properties.Images = CustomDrawTableViewDemoMainForm.imIndicatorImages
      Properties.Items = <
        item
          Description = 'Clubs'
          ImageIndex = 0
          Value = '0'
        end
        item
          Description = 'Hearts'
          ImageIndex = 1
          Value = '1'
        end
        item
          Description = 'Spades'
          ImageIndex = 2
          Value = '2'
        end
        item
          Description = 'Diamonds'
          ImageIndex = 3
          Value = '3'
        end>
      Properties.OnEditValueChanged = icbIndicatorImagesPropertiesEditValueChanged
      Style.Color = 16247513
      TabOrder = 7
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
