object StylesSimpleDemoEditForm: TStylesSimpleDemoEditForm
  Left = 387
  Top = 216
  BorderStyle = bsDialog
  Caption = 'Edit Style...'
  ClientHeight = 223
  ClientWidth = 328
  Color = 15451300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 168
    Top = 192
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
  end
  object nbtCancel: TButton
    Left = 248
    Top = 192
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    OnClick = nbtCancelClick
  end
  object DesignGroupBox: TGroupBox
    Left = 8
    Top = 8
    Width = 313
    Height = 177
    Caption = 'Style properties'
    TabOrder = 0
    object lbColor: TLabel
      Left = 16
      Top = 28
      Width = 24
      Height = 13
      Caption = 'Color'
    end
    object lbTextColor: TLabel
      Left = 16
      Top = 52
      Width = 48
      Height = 13
      Caption = 'Text Color'
    end
    object lbFont: TLabel
      Left = 16
      Top = 76
      Width = 21
      Height = 13
      Caption = 'Font'
    end
    object lbColorValue: TLabel
      Left = 168
      Top = 28
      Width = 30
      Height = 13
      Caption = 'Color'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lbTextColorValue: TLabel
      Left = 168
      Top = 52
      Width = 59
      Height = 13
      Caption = 'Text Color'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lbBitmap: TLabel
      Left = 16
      Top = 100
      Width = 32
      Height = 13
      Caption = 'Bitmap'
    end
    object imgExample: TImage
      Left = 16
      Top = 128
      Width = 273
      Height = 41
    end
    object btnedFont: TcxButtonEdit
      Left = 72
      Top = 72
      Width = 217
      Height = 21
      Properties.Buttons = <
        item
          Default = True
          Kind = bkEllipsis
        end>
      Properties.OnButtonClick = btnedFontPropertiesButtonClick
      Style.Color = 16247513
      TabOrder = 2
    end
    object btnedTextColor: TcxButtonEdit
      Tag = 1
      Left = 72
      Top = 48
      Width = 81
      Height = 21
      Properties.Buttons = <
        item
          Default = True
          Kind = bkEllipsis
        end>
      Properties.ViewStyle = vsHideCursor
      Properties.OnButtonClick = btnedColorPropertiesButtonClick
      Style.Color = 16247513
      TabOrder = 1
      OnKeyPress = btnedTextColorKeyPress
    end
    object btnedColor: TcxButtonEdit
      Left = 72
      Top = 24
      Width = 81
      Height = 21
      Properties.Buttons = <
        item
          Default = True
          Kind = bkEllipsis
        end>
      Properties.ViewStyle = vsHideCursor
      Properties.OnButtonClick = btnedColorPropertiesButtonClick
      Style.Color = 16247513
      TabOrder = 0
      OnKeyPress = btnedTextColorKeyPress
    end
    object btnedBitmap: TcxButtonEdit
      Left = 72
      Top = 96
      Width = 217
      Height = 21
      Properties.Buttons = <
        item
          Default = True
          Kind = bkEllipsis
        end
        item
          Glyph.Data = {
            9E020000424D9E0200000000000036000000280000000E0000000E0000000100
            1800000000006802000000000000000000000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFF
            FFFFFFFFFFFFFF000000808080FFFFFFFFFFFFFFFFFFFFFFFF808080000000FF
            FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFC0C0C0000000808080FFFFFFFF
            FFFF808080000000C0C0C0FFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFF
            FFFFC0C0C0000000808080808080000000C0C0C0FFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000000000C0C0C0FFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080
            000000000000808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFF
            FFFFFFFFFFFF808080000000C0C0C0C0C0C0000000808080FFFFFFFFFFFFFFFF
            FFFFFFFF0000FFFFFFFFFFFFFFFFFF808080000000C0C0C0FFFFFFFFFFFFC0C0
            C0000000808080FFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFF000000C0C0
            C0FFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FFFFFFFFFFFFFFFFFF0000FFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000}
          Kind = bkGlyph
        end>
      Properties.ReadOnly = True
      Properties.OnButtonClick = btnedBitmapPropertiesButtonClick
      Style.Color = 16247513
      TabOrder = 3
    end
  end
  object ColorDialog: TColorDialog
    Left = 96
    Top = 192
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 128
    Top = 192
  end
  object OpenPictureDialog: TOpenPictureDialog
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 60
    Top = 191
  end
end
