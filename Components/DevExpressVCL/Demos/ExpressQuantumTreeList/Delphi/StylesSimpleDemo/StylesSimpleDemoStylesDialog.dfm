object StylesSimpleDemoStylesDialogForm: TStylesSimpleDemoStylesDialogForm
  Left = 416
  Top = 184
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Styles Dialog'
  ClientHeight = 385
  ClientWidth = 258
  Color = 15451300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lscrip: TLabel
    Left = 0
    Top = 0
    Width = 258
    Height = 39
    Align = alTop
    Caption = 
      'Select one of the predefined styles using the dropdown and/or ed' +
      'it styles by pressing the ellipsis button.'
    Color = 4707838
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    WordWrap = True
  end
  object cxTreeList: TcxTreeList
    Left = 0
    Top = 39
    Width = 258
    Height = 305
    Styles.StyleSheet = TreeListStyleSheetDevExpress
    Align = alTop
    Bands = <
      item
        Caption.Text = 'Band + 1'
        Width = 254
      end>
    OptionsView.ShowEditButtons = ecsbFocused
    OptionsView.GridLines = tlglBoth
    OptionsView.ShowRoot = False
    TabOrder = 0
    Data = {
      00000400790200000F00000044617461436F6E74726F6C6C6572310200000012
      000000546378537472696E6756616C7565547970651200000054637853747269
      6E6756616C7565547970650F00000000000A0000004261636B67726F756E6401
      00000E00000042616E646261636B67726F756E640100000B00000042616E6443
      6F6E74656E740100000A00000042616E644865616465720100000C000000436F
      6C756D6E466F6F7465720100000C000000436F6C756D6E486561646572010000
      07000000436F6E74656E740100000B000000436F6E74656E744576656E010000
      0A000000436F6E74656E744F646401000006000000466F6F7465720100000800
      0000496E61637469766501000009000000496E63536561726368010000090000
      00496E64696361746F7201000007000000507265766965770100000900000053
      656C656374696F6E010F00000000000000100000000000000000000000FFFFFF
      FF01000000100000000000000000000000FFFFFFFF0200000010000000000000
      0000000000FFFFFFFF03000000100000000000000000000000FFFFFFFF040000
      00100000000000000000000000FFFFFFFF050000001000000000000000000000
      00FFFFFFFF06000000100000000000000000000000FFFFFFFF07000000100000
      000000000000000000FFFFFFFF08000000100000000000000000000000FFFFFF
      FF09000000100000000000000000000000FFFFFFFF0A00000010000000000000
      0000000000FFFFFFFF0B000000100000000000000000000000FFFFFFFF0C0000
      00100000000000000000000000FFFFFFFF0D0000001000000000000000000000
      00FFFFFFFF0E000000100000000000000000000000FFFFFFFF}
    object tlcStyle: TcxTreeListColumn
      Caption.Text = 'Styles'
      DataBinding.ValueType = 'String'
      Options.Editing = False
      Position.BandIndex = 0
      Position.ColIndex = 0
      Position.RowIndex = 0
      Width = 116
    end
    object tlcStyleNames: TcxTreeListColumn
      PropertiesClassName = 'TcxMRUEditProperties'
      Properties.DropDownListStyle = lsFixedList
      Properties.ImmediatePost = True
      Properties.ReadOnly = False
      Properties.OnButtonClick = tlcStyleNamesPropertiesButtonClick
      Properties.OnEditValueChanged = tlcStyleNamesPropertiesEditValueChanged
      Caption.Text = 'Style Names'
      DataBinding.ValueType = 'String'
      Position.BandIndex = 0
      Position.ColIndex = 1
      Position.RowIndex = 0
      Width = 138
    end
  end
  object btnRestore: TcxButton
    Left = 16
    Top = 352
    Width = 225
    Height = 25
    Caption = 'Restore Default'
    TabOrder = 1
    OnClick = btnRestoreClick
  end
  object cxStyleRepository1: TcxStyleRepository
    Left = 224
    Top = 88
    object cxStyle1: TcxStyle
      AssignedValues = [svColor]
      Color = 15451300
    end
    object cxStyle2: TcxStyle
      AssignedValues = [svColor]
      Color = 15451300
    end
    object cxStyle3: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 12937777
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object cxStyle4: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 12937777
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object cxStyle5: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 16247513
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle6: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 16247513
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle7: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 16247513
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle8: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 14811135
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clNavy
    end
    object cxStyle9: TcxStyle
      AssignedValues = [svColor]
      Color = 15451300
    end
    object cxStyle10: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 4707838
      TextColor = clBlack
    end
    object cxStyle11: TcxStyle
      AssignedValues = [svColor]
      Color = 15451300
    end
    object cxStyle12: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 14811135
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clNavy
    end
    object cxStyle13: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 12937777
      TextColor = clWhite
    end
    object TreeListStyleSheetDevExpress: TcxTreeListStyleSheet
      Caption = 'DevExpress'
      Styles.Background = cxStyle1
      Styles.Content = cxStyle5
      Styles.Inactive = cxStyle9
      Styles.Selection = cxStyle13
      Styles.BandBackground = cxStyle2
      Styles.BandHeader = cxStyle3
      Styles.ColumnHeader = cxStyle4
      Styles.ContentEven = cxStyle6
      Styles.ContentOdd = cxStyle7
      Styles.Footer = cxStyle8
      Styles.IncSearch = cxStyle10
      Styles.Indicator = cxStyle11
      Styles.Preview = cxStyle12
      BuiltIn = True
    end
  end
end
