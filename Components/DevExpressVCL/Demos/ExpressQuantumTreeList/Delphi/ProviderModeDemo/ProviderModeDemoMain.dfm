inherited ProviderModeDemoMainForm: TProviderModeDemoMainForm
  Left = 232
  Top = 114
  Caption = 'ExpressQuantumTreeList ProviderModeDemo '
  OldCreateOrder = True
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  inherited lscrip: TLabel
    Caption = 
      'Use the TreeList to work with a custom data structure (about 100,'+
      '000 nodes) and experiment with the various options and operations ' + 
      'above. See Help/About for other things to try.'
  end
  inherited sbMain: TStatusBar
    Panels = <
      item
        Width = 120
      end
      item
        Width = 120
      end
      item
        Width = 120
      end>
    SimplePanel = False
  end
  object TreeList: TcxVirtualTreeList [2]
    Left = 0
    Top = 32
    Width = 700
    Height = 361
    Align = alClient
    Bands = <
      item
        Caption.AlignHorz = taCenter
        Caption.Text = 'Provider mode'
        Width = 515
      end>
    DragMode = dmAutomatic
    OptionsBehavior.ImmediateEditor = False
    OptionsData.Inserting = True
    OptionsView.GridLineColor = 11316396
    OptionsView.GridLines = tlglBoth
    OptionsView.Indicator = True
    Styles.StyleSheet = TreeListStyleSheetDevExpress
    TabOrder = 1
    OnDragOver = TreeListDragOver
    OnExpanding = TreeListExpanding
    object clnId: TcxTreeListColumn
      PropertiesClassName = 'TcxSpinEditProperties'
      Caption.Text = 'Id'
      DataBinding.ValueType = 'Integer'
      Width = 182
      Position.ColIndex = 0
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object clnName: TcxTreeListColumn
      Caption.Text = 'Text'
      DataBinding.ValueType = 'String'
      Width = 162
      Position.ColIndex = 1
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object clnDate: TcxTreeListColumn
      Caption.Text = 'Date'
      DataBinding.ValueType = 'DateTime'
      Width = 171
      Position.ColIndex = 2
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
  end
  inherited mmMain: TMainMenu
    inherited miOptions: TMenuItem
      object miSmartLoadMode: TMenuItem [0]
        AutoCheck = True
        Caption = '&Smart Load mode'
        Checked = True
        Hint = 'Enables SmartLoad mode '
        OnClick = miSmartLoadModeClick
      end
      object N2: TMenuItem [1]
        AutoCheck = True
        Caption = '-'
      end
      object miShowButtons: TMenuItem [2]
        AutoCheck = True
        Caption = 'Show &Buttons'
        Checked = True
        Hint = 'Displays buttons to the left of each node with children'
        OnClick = miShowButtonsClick
      end
      object miShowRoot: TMenuItem [3]
        AutoCheck = True
        Caption = 'Show &Root'
        Checked = True
        Hint = 
          'Displays the button to the left of the first node as a root of a' +
          'll nodes'
        OnClick = miShowRootClick
      end
      object miShowIndicator: TMenuItem [4]
        AutoCheck = True
        Caption = 'Show &Indicator'
        Checked = True
        Hint = 'Shows indicators on the left side of tree list nodes'
        OnClick = miShowIndicatorClick
      end
      object miShowTreeLines: TMenuItem [5]
        AutoCheck = True
        Caption = 'Show Tree &Lines'
        Checked = True
        Hint = 
          'Shows the dotted lines between parent and child nodes within a t' +
          'ree list control'
        OnClick = miShowTreeLinesClick
      end
      object miCellAutoHeight: TMenuItem [6]
        AutoCheck = True
        Caption = 'Cell Auto &Height'
        Hint = 
          'If checked, a cell'#39's contents are displayed in multiple lines wh' +
          'ere necessary'
        OnClick = miCellAutoHeightClick
      end
      object miCellEndEllipsis: TMenuItem [7]
        AutoCheck = True
        Caption = 'Cell End &Ellipsis'
        Hint = 
          'Displays an ellipsis when the entire text cannot be displayed wi' +
          'thin a cell'
        OnClick = miCellEndEllipsisClick
      end
      object miColumnAutoWidth: TMenuItem [8]
        AutoCheck = True
        Caption = 'Column Auto &Width'
        Hint = 
          'If checked, column widths are changed in order to display all co' +
          'lumns without using the horizontal scrollbar'
        OnClick = miColumnAutoWidthClick
      end
      object N1: TMenuItem [9]
        Caption = '-'
      end
    end
    object Operations1: TMenuItem [2]
      Caption = 'O&perations'
      object FullExpand1: TMenuItem
        Caption = '&Full Expand'
        OnClick = FullExpand1Click
      end
    end
  end
  object StyleRepository: TcxStyleRepository
    Left = 568
    Top = 8
    PixelsPerInch = 96
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
      Color = 15252642
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = 11032875
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
      Color = 15784893
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
      AssignedValues = [svColor, svTextColor]
      Color = 15451300
      TextColor = clBlack
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
      Color = 16048336
      TextColor = clBlack
    end
    object stlGroupNode: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 15253902
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object stlFixedBand: TcxStyle
      AssignedValues = [svColor]
      Color = 15322014
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
      Styles.ContentEven = cxStyle7
      Styles.ContentOdd = cxStyle6
      Styles.Footer = cxStyle8
      Styles.IncSearch = cxStyle10
      Styles.Indicator = cxStyle11
      Styles.Preview = cxStyle12
      BuiltIn = True
    end
  end
end
