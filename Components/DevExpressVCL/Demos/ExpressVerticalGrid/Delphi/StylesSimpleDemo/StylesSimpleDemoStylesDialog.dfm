object StylesSimpleDemoStylesDialogForm: TStylesSimpleDemoStylesDialogForm
  Left = 388
  Top = 156
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Styles Dialog'
  ClientHeight = 229
  ClientWidth = 190
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
  object btnRestore: TcxButton
    Left = 14
    Top = 192
    Width = 161
    Height = 25
    Caption = 'Restore Defaults'
    TabOrder = 0
    OnClick = btnRestoreClick
  end
  object cxVerticalGrid: TcxVerticalGrid
    Left = 0
    Top = 0
    Width = 190
    Height = 177
    Styles.Background = StylesSimpleDemoDataDM.Autumn
    Styles.OnGetContentStyle = cxVerticalGridStylesGetContentStyle
    Styles.StyleSheet = cxVerticalGridStyleSheetDevExpress
    Styles.OnGetHeaderStyle = cxVerticalGridStylesGetHeaderStyle
    Align = alTop
    OptionsView.RowHeaderWidth = 93
    OptionsBehavior.BandSizing = False
    OptionsBehavior.HeaderSizing = False
    TabOrder = 1
    object cxVerticalGridCaption: TcxMultiEditorRow
      Properties.Editors = <
        item
          Caption = 'Styles'
          Options.Editing = False
          Options.Focusing = False
          DataBinding.ValueType = 'String'
          Value = 'Style Names'
        end>
    end
    object cxVerticalGridBackground: TcxEditorRow
      Properties.Caption = 'Background'
      Properties.RepositoryItem = cxEditRepositoryMRUItem
      Properties.DataBinding.ValueType = 'String'
      Properties.Value = Null
    end
    object cxVerticalGridCategory: TcxEditorRow
      Properties.Caption = 'Category'
      Properties.RepositoryItem = cxEditRepositoryMRUItem
      Properties.DataBinding.ValueType = 'String'
      Properties.Value = Null
    end
    object cxVerticalGridContent: TcxEditorRow
      Properties.Caption = 'Content'
      Properties.RepositoryItem = cxEditRepositoryMRUItem
      Properties.DataBinding.ValueType = 'String'
      Properties.Value = Null
    end
    object cxVerticalGridHeader: TcxEditorRow
      Properties.Caption = 'Header'
      Properties.RepositoryItem = cxEditRepositoryMRUItem
      Properties.DataBinding.ValueType = 'String'
      Properties.Value = Null
    end
    object cxVerticalGridInactive: TcxEditorRow
      Properties.Caption = 'Inactive'
      Properties.RepositoryItem = cxEditRepositoryMRUItem
      Properties.DataBinding.ValueType = 'String'
      Properties.Value = Null
    end
    object cxVerticalGridIncSearch: TcxEditorRow
      Properties.Caption = 'IncSearch'
      Properties.RepositoryItem = cxEditRepositoryMRUItem
      Properties.DataBinding.ValueType = 'String'
      Properties.Value = Null
    end
    object cxVerticalGridSelection: TcxEditorRow
      Properties.Caption = 'Selection'
      Properties.RepositoryItem = cxEditRepositoryMRUItem
      Properties.DataBinding.ValueType = 'String'
      Properties.Value = Null
    end
  end
  object cxEditRepository: TcxEditRepository
    Left = 128
    Top = 24
    object cxEditRepositoryMRUItem: TcxEditRepositoryMRUItem
      Properties.DropDownListStyle = lsFixedList
      Properties.ImmediatePost = True
      Properties.OnButtonClick = OnButtonClick
      Properties.OnEditValueChanged = OnEditValueChanged
    end
  end
  object ActionList1: TActionList
    Left = 160
    Top = 24
  end
  object cxStyleRepository1: TcxStyleRepository
    Left = 88
    Top = 120
    object cxStyle1: TcxStyle
      AssignedValues = [svColor]
      Color = 14590588
    end
    object cxStyle2: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 13795663
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clYellow
    end
    object cxStyle3: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 16247513
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = clNavy
    end
    object cxStyle4: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 14590588
      TextColor = clWhite
    end
    object cxStyle5: TcxStyle
      AssignedValues = [svColor]
      Color = 15185807
    end
    object cxStyle6: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 4707838
      TextColor = clBlack
    end
    object cxStyle7: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 15120279
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxVerticalGridStyleSheetDevExpress: TcxVerticalGridStyleSheet
      Caption = 'DevExpress'
      Styles.Background = cxStyle1
      Styles.Content = cxStyle3
      Styles.Inactive = cxStyle5
      Styles.Selection = cxStyle7
      Styles.Category = cxStyle2
      Styles.Header = cxStyle4
      Styles.IncSearch = cxStyle6
      BuiltIn = True
    end
  end
end
