inherited ProviderModeDemoMainForm: TProviderModeDemoMainForm
  Left = 350
  Top = 208
  Caption = 'ExpressVerticalGrid ProviderModeDemo'
  ClientHeight = 253
  ClientWidth = 634
  OnCloseQuery = FormCloseQuery
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescrip: TLabel
    Width = 634
    Caption = 
      'Uses the ExpressVerticalGrid in provider mode with a text file a' +
      's its data. Experiment by selecting the Options above and also s' +
      'ee Help/About for other things to try.'
  end
  inherited sbMain: TStatusBar
    Top = 234
    Width = 634
  end
  inherited memAboutText: TMemo
    Left = 656
    Top = 256
    Lines.Strings = (
      'In this demo you can:'
      ''
      
        '- edit the customer list by pressing the Insert and Delete butto' +
        'ns (note the effect on the ID value)'
      ''
      
        '- use the cxNavigator buttons. In particular try setting/using t' +
        'he bookmark and filtering (the rightmost three buttons)'
      ''
      
        '- examine the code in the LoadData method to see how easy it is ' +
        'to handle text data'
      ''
      '')
    TabOrder = 2
  end
  object cxVirtualVerticalGrid: TcxVirtualVerticalGrid [3]
    Left = 0
    Top = 32
    Width = 634
    Height = 160
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    LayoutStyle = lsMultiRecordView
    OptionsView.CellAutoHeight = True
    OptionsView.CellEndEllipsis = True
    OptionsView.RowHeaderWidth = 143
    OptionsView.ValueWidth = 55
    Styles.StyleSheet = cxVerticalGridStyleSheetDevExpress
    TabOrder = 1
    Version = 1
  end
  object cxNavigator1: TcxNavigator [4]
    Left = 197
    Top = 200
    Width = 255
    Height = 25
    Control = cxVirtualVerticalGrid
    Anchors = [akLeft, akBottom]
    TabOrder = 3
  end
  inherited mmMain: TMainMenu
    Top = 40
    inherited miOptions: TMenuItem
      object CellEndEllipsis1: TMenuItem [0]
        Action = actCellEndEllipsis
      end
      object CellAutoHeight1: TMenuItem [1]
        Action = actCellAutoHeight
      end
      object GridLines1: TMenuItem [2]
        Caption = 'Grid Lines'
        object Node1: TMenuItem
          Caption = 'None'
          RadioItem = True
          OnClick = miLinesClick
        end
        object Horizontal1: TMenuItem
          Tag = 1
          Caption = 'Horizontal'
          RadioItem = True
          OnClick = miLinesClick
        end
        object Vertical1: TMenuItem
          Tag = 2
          Caption = 'Vertical'
          RadioItem = True
          OnClick = miLinesClick
        end
        object Both1: TMenuItem
          Tag = 3
          Caption = 'Both'
          Checked = True
          RadioItem = True
          OnClick = miLinesClick
        end
      end
      object miSeparator1: TMenuItem [3]
        Caption = '-'
      end
    end
  end
  inherited ilMain: TImageList
    Left = 464
    Top = 40
  end
  inherited alMain: TActionList
    Left = 408
    Top = 40
    object actCellAutoHeight: TAction
      Category = 'Options'
      Caption = '&CellAutoHeight'
      Checked = True
      OnExecute = actCellAutoHeightExecute
    end
    object actCellEndEllipsis: TAction
      Category = 'Options'
      Caption = 'CellEnd&Ellipsis'
      Checked = True
      OnExecute = actCellEndEllipsisExecute
    end
  end
  inherited cxLookAndFeelController: TcxLookAndFeelController
    Left = 376
    Top = 32
  end
  object StyleRepository: TcxStyleRepository
    Left = 336
    Top = 32
    PixelsPerInch = 96
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
