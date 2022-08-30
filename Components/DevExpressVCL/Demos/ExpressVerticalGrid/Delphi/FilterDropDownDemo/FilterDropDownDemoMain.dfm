inherited frmMain: TfrmMain
  Left = 401
  Top = 185
  Caption = 'ExpressVerticalGrid Filter Dropdown Demo'
  ClientHeight = 551
  ClientWidth = 763
  Constraints.MinHeight = 600
  Constraints.MinWidth = 770
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescrip: TLabel
    Width = 763
    Height = 64
    Caption = 
      'This demo illustrates the filter dropdown'#39's capabilities. Hover ' +
      'the mouse pointer over a row header and click its filter button ' +
      'to invoke the row'#39's filter dropdown. You can use its UI to creat' +
      'e, customize, or apply filter conditions to the grid. Select Fil' +
      'ter Options in the main menu to choose between Excel-inspired an' +
      'd Classic modes and access their options. Click Help | About thi' +
      's demo for more information.'
  end
  inherited sbMain: TStatusBar
    Top = 532
    Width = 763
  end
  inherited memAboutText: TMemo
    Lines.Strings = (
      'In this demo, you can:'
      ''
      
        '- Hover the mouse pointer over a row header and click its filter' +
        ' button to display the row'#39's filter dropdown.'
      ''
      
        '- Use the filter dropdown'#39's UI to create/modify filter condition' +
        's and apply them to the grid.'
      ''
      
        '- Choose between Excel-inspired and Classic filter dropdowns (th' +
        'e Filter Options | Mode menu item).'
      ''
      
        '- Specify how the grid updates its filter in response to changes' +
        ' made in a filter dropdown (the Filter Options | Apply Changes m' +
        'enu item).'
      ''
      
        '- Specify how the Values page in Excel-inspired filter dropdowns' +
        ' display:'
      
        '  - Numeric values (the Filter Options | Numeric Values Page Sty' +
        'le menu item).'
      
        '  - Date-time values (the Filter Options | Date Time Values Page' +
        ' Style menu item).')
  end
  object VerticalGrid: TcxDBVerticalGrid [3]
    Left = 0
    Top = 64
    Width = 763
    Height = 468
    Align = alClient
    FilterBox.Visible = fvNonEmpty
    Filtering.RowPopupMode = fpmExcel
    FindPanel.DisplayMode = fpdmManual
    FindPanel.UseExtendedSyntax = True
    LayoutStyle = lsMultiRecordView
    OptionsView.GridLineColor = clBtnFace
    OptionsView.RowHeaderWidth = 183
    OptionsView.ValueWidth = 170
    OptionsBehavior.RowFiltering = bTrue
    Navigator.Buttons.CustomButtons = <>
    Navigator.Visible = True
    TabOrder = 2
    DataController.DataSource = dsCarOrders
    Version = 1
    object VerticalGridRecId: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'RecId'
      Visible = False
      ID = 0
      ParentID = -1
      Index = 0
      Version = 1
    end
    object VerticalGridID: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'ID'
      Visible = False
      ID = 1
      ParentID = -1
      Index = 1
      Version = 1
    end
    object VerticalGridTrademark: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'Trademark'
      ID = 2
      ParentID = -1
      Index = 2
      Version = 1
    end
    object VerticalGridName: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'Name'
      ID = 3
      ParentID = -1
      Index = 3
      Version = 1
    end
    object VerticalGridModification: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'Modification'
      ID = 4
      ParentID = -1
      Index = 4
      Version = 1
    end
    object VerticalGridPrice: TcxDBEditorRow
      Properties.EditPropertiesClassName = 'TcxCurrencyEditProperties'
      Properties.DataBinding.FieldName = 'Price'
      ID = 5
      ParentID = -1
      Index = 5
      Version = 1
    end
    object VerticalGridMPGCity: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'MPG City'
      ID = 6
      ParentID = -1
      Index = 6
      Version = 1
    end
    object VerticalGridMPGHighway: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'MPG Highway'
      ID = 7
      ParentID = -1
      Index = 7
      Version = 1
    end
    object VerticalGridBodyStyleID: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'BodyStyleID'
      Visible = False
      ID = 8
      ParentID = -1
      Index = 8
      Version = 1
    end
    object VerticalGridCilinders: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'Cilinders'
      ID = 9
      ParentID = -1
      Index = 9
      Version = 1
    end
    object VerticalGridSalesDate: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'SalesDate'
      ID = 10
      ParentID = -1
      Index = 10
      Version = 1
    end
    object VerticalGridBodyStyle: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'BodyStyle'
      ID = 11
      ParentID = -1
      Index = 11
      Version = 1
    end
  end
  inherited mmMain: TMainMenu
    Left = 300
    Top = 256
    object miFilterPopup: TMenuItem [1]
      Caption = 'Filter &Options'
      object miFilterPopupMode: TMenuItem
        Action = acFilterPopupMode
        object miFilterPopupModeClassic: TMenuItem
          Action = acFilterPopupModeClassic
          AutoCheck = True
          GroupIndex = 1
          RadioItem = True
        end
        object miFilterPopupModeExcel: TMenuItem
          Action = acFilterPopupModeExcel
          AutoCheck = True
          GroupIndex = 1
          RadioItem = True
        end
      end
      object miSeparatorFP: TMenuItem
        Caption = '-'
      end
      object miClassicModeMultiSelect: TMenuItem
        Tag = 1
        Action = acClassicModeMultiSelect
        AutoCheck = True
        Caption = 'Multi Select'
      end
      object miClassicModeApplyChanges: TMenuItem
        Tag = 1
        Action = acClassicModeApplyChanges
        object miClassicModeApplyChangesImmediatly: TMenuItem
          Action = acClassicModeApplyChangesImmediately
          AutoCheck = True
        end
        object miClassicModeApplyChangesButtonClick: TMenuItem
          Action = acClassicModeApplyChangesOnButtonClick
          AutoCheck = True
        end
      end
      object miExcelModeApplyChanges: TMenuItem
        Tag = 2
        Action = acExcelModeApplyChanges
        object miExcelModeApplyChangesImmediatly: TMenuItem
          Action = acExcelModeApplyChangesImmediately
          AutoCheck = True
        end
        object miExcelModeTabOrOKButtonClick: TMenuItem
          Action = acExcelModeApplyChangesOnTabOrOKButtonClick
          AutoCheck = True
        end
      end
      object miExcelModeDateTimePage: TMenuItem
        Tag = 2
        Action = acExcelModeDateTimePageType
        object miDateTimePageTree: TMenuItem
          Action = acExcelModeDateTimePageTypeTree
          AutoCheck = True
        end
        object miDateTimePageList: TMenuItem
          Action = acExcelModeDateTimePageTypeList
          AutoCheck = True
        end
      end
      object miExcelModeNumericPage: TMenuItem
        Tag = 2
        Action = acExcelModeNumericPageType
        object miNumericPageTree: TMenuItem
          Action = acExcelModeNumericPageTypeRange
          AutoCheck = True
        end
        object miNumericPageList: TMenuItem
          Action = acExcelModeNumericPageTypeList
          AutoCheck = True
        end
      end
    end
  end
  inherited ilMain: TImageList
    Left = 456
    Top = 256
    Bitmap = {
      494C010104000900180010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000808080008080800080808000808080008080
      8000808080008080800080808000808080000000000000000000000000008080
      8000808080008080800080808000808080008080800080808000808080008080
      8000808080008080800080808000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000080808000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C00080808000000000000000000000000000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000000000000000000000000000000000000000008080
      800080808000FF000000C0C0C000FFFFFF0000FFFF00FFFFFF00FFFFFF00FFFF
      FF0000FFFF00FFFFFF00C0C0C00080808000000000000000000000000000C0C0
      C000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C0C0C000808080000000000000000000000000000000
      0000000000000000000000000000800080008000800080808000000000000000
      00000000000000000000000000000000000000000000FFFFFF000000000000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000000000FFFFFF0000000000000000000000000080808000FF00
      0000FF000000FF000000C0C0C000FFFFFF00FFFFFF00FFFFFF0000FFFF00FFFF
      FF00FFFFFF00FFFFFF00C0C0C000808080000000000000000000FF000000C0C0
      C000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C0C0C000808080000000000000000000000000000000
      0000000000008000800080008000FFFFFF00FFFFFF00C0C0C000808080000000
      0000000000000000000000000000000000000000000000FFFF00FFFFFF000000
      000000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFF
      FF0000000000FFFFFF0000FFFF00000000000000000080808000FF000000FF00
      0000FF000000FF000000C0C0C000FFFFFF0000FFFF0080808000808080008080
      8000C0C0C000FFFFFF00C0C0C0008080800000000000FF000000808080008080
      8000808080008080800080808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C0C0C000808080000000000000000000000000008000
      800080008000FFFFFF00FFFFFF000000000000000000C0C0C000C0C0C0008080
      80000000000000000000000000000000000000000000FFFFFF0000FFFF00FFFF
      FF000000000000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000
      0000FFFFFF0000FFFF00FFFFFF00000000000000000080808000FF000000FF00
      0000FF000000FF000000C0C0C000FFFFFF0080808000FF000000FF000000FF00
      000080808000FFFFFF00C0C0C0008080800080808000FF000000808080008080
      8000FF000000FF000000FF000000FF00000080808000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C0C0C00080808000808080008000800080008000FFFF
      FF00FFFFFF000000000000000000800080008000800000000000C0C0C000C0C0
      C000808080000000000000000000000000000000000000FFFF00FFFFFF000000
      0000FFFFFF000000000000FFFF00FFFFFF0000FFFF00FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000FFFF000000000080808000FF000000FF000000FF00
      0000FF000000FF000000C0C0C000FFFFFF0080808000C0C0C00080808000FF00
      000080808000FFFFFF00C0C0C00080808000808080008080800080808000FF00
      0000FF000000C0C0C000C0C0C00080808000FF00000080808000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C0C0C000808080008080800080008000FFFFFF000000
      000000000000800080008000800080008000800080008000800000000000C0C0
      C000C0C0C00080808000000000000000000000000000FFFFFF0000000000FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF000000000080808000FF000000FF000000FF00
      00008080800080808000C0C0C000FFFFFF0080808000FFFFFF00C0C0C0008080
      800080808000FFFFFF00C0C0C0008080800080808000C0C0C00080808000FF00
      000080808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C0C0C000808080008080800000000000000000008000
      800080008000800080000080800000FFFF008000800080008000800080000000
      0000C0C0C000C0C0C00080808000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000000000000080808000FF000000FF0000008080
      80008080800080808000C0C0C000FFFFFF0000FFFF0080808000808080008080
      8000C0C0C000FFFFFF00C0C0C00080808000000000008080800000FFFF00FF00
      0000FF000000FF000000FF000000FF000000FF00000080808000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C0C0C000808080008080800080008000800080008000
      8000800080008000800080008000008080008000800080008000800080008000
      800000000000C0C0C000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000080808000FF000000FF0000008080
      80008080800080808000C0C0C000FFFFFF00FFFFFF00FFFFFF0000FFFF00FFFF
      FF008080800080808000808080008080800000000000000000008080800000FF
      FF00FF000000FFFFFF00FFFFFF0080808000FF00000080808000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C0C0C000808080000000000080008000FFFFFF008000
      80008000800080008000800080008000800000FFFF0000FFFF00800080008000
      80008000800000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF000000FF000000FF00
      0000FFFFFF00FFFFFF00FFFFFF000000000080808000FF000000FF000000C0C0
      C0008080800080808000C0C0C000FFFFFF0000FFFF00FFFFFF00FFFFFF00FFFF
      FF00C0C0C000FFFFFF00C0C0C000000000000000000000000000000000008080
      800000FFFF00FF000000FF000000FF00000080808000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C0C0C00080808000000000000000000080008000FFFF
      FF0080008000800080008000800080008000800080000080800000FFFF0000FF
      FF00800080008000800000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF000000FF000000FFFFFF00FF00
      0000FF000000FFFFFF00000000000000000000000000C0C0C000FF000000FF00
      0000FFFFFF00C0C0C000C0C0C000FFFFFF00FFFFFF00FFFFFF0000FFFF00FFFF
      FF00C0C0C000C0C0C0000000000000000000000000000000000000000000C0C0
      C000FF000000FF000000FF00000080808000FFFFFF00FF000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C0C0C000808080000000000000000000000000008000
      8000FFFFFF00800080008000800080008000008080008000800000FFFF0000FF
      FF0080008000800080008000800000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF000000FF000000FFFFFF00FFFFFF00FFFF
      FF00FF000000FF000000000000000000000000000000C0C0C000FF000000FFFF
      FF00C0C0C000FFFFFF00C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000808080000000000000000000000000000000000000000000C0C0
      C000FFFFFF00FFFFFF00FFFFFF00FF000000FF000000FFFFFF00FFFFFF00FFFF
      FF00808080008080800080808000808080000000000000000000000000000000
      000080008000FFFFFF00800080008000800000FFFF0000FFFF0000FFFF008000
      8000800080008000800000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FF000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FF000000FF000000000000000000000000000000C0C0C000FF00
      0000FF000000C0C0C000FFFFFF00C0C0C0008080800080808000808080008080
      800080808000000000000000000000000000000000000000000000000000C0C0
      C000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00C0C0C000FFFFFF00C0C0C000000000000000000000000000000000000000
      00000000000080008000FFFFFF00800080008000800080008000800080008000
      8000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      00000000000000000000FF000000FF000000000000000000000000000000C0C0
      C000C0C0C000FF000000FF000000FFFFFF00C0C0C00080808000808080008080
      800000000000000000000000000000000000000000000000000000000000C0C0
      C000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00C0C0C000C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000080008000FFFFFF008000800080008000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000FF0000000000000000000000000000000000
      000000000000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000000000000000
      000000000000000000000000000000000000000000000000000000000000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000800080008000800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FC00E000FFFF0000F800E000FE3F0000
      E000E000F81F0000C000C000E00F000080008000800700008000000000030000
      0000000000010000000000000000000000008000000100000000C00080010000
      0001E000C00180018003E000E000C0038003E000F000E001C007E001F803F00C
      E00FE003FC0FF81EF83FE007FE3FFC3F00000000000000000000000000000000
      000000000000}
  end
  inherited alMain: TActionList
    Left = 384
    Top = 256
    object acFilterPopupModeClassic: TAction
      Category = 'FilterPopup'
      AutoCheck = True
      Caption = 'Classic'
      GroupIndex = 1
      OnExecute = acFilterPopupModeExecute
    end
    object acFilterPopupModeExcel: TAction
      Category = 'FilterPopup'
      AutoCheck = True
      Caption = 'Excel'
      Checked = True
      GroupIndex = 1
      OnExecute = acFilterPopupModeExecute
    end
    object acExcelModeApplyChangesImmediately: TAction
      Category = 'FilterPopup'
      AutoCheck = True
      Caption = 'Immediately'
      Checked = True
      GroupIndex = 2
      OnExecute = acExcelModeApplyChangesExecute
    end
    object acExcelModeApplyChangesOnTabOrOKButtonClick: TAction
      Category = 'FilterPopup'
      AutoCheck = True
      Caption = 'On Tab Or OK Button Click'
      GroupIndex = 2
      OnExecute = acExcelModeApplyChangesExecute
    end
    object acExcelModeDateTimePageTypeTree: TAction
      Category = 'FilterPopup'
      AutoCheck = True
      Caption = 'Tree'
      Checked = True
      GroupIndex = 3
      OnExecute = acDateTimePageTypeExecute
    end
    object acExcelModeDateTimePageTypeList: TAction
      Category = 'FilterPopup'
      AutoCheck = True
      Caption = 'List'
      GroupIndex = 3
      OnExecute = acDateTimePageTypeExecute
    end
    object acExcelModeNumericPageTypeRange: TAction
      Category = 'FilterPopup'
      AutoCheck = True
      Caption = 'Range'
      Checked = True
      GroupIndex = 4
      OnExecute = acNumericPageTypeExecute
    end
    object acExcelModeNumericPageTypeList: TAction
      Category = 'FilterPopup'
      AutoCheck = True
      Caption = 'List'
      GroupIndex = 4
      OnExecute = acNumericPageTypeExecute
    end
    object acClassicModeApplyChangesImmediately: TAction
      Category = 'FilterPopup'
      AutoCheck = True
      Caption = 'Immediately'
      Checked = True
      GroupIndex = 5
      OnExecute = acClassicModeApplyChangesExecute
    end
    object acClassicModeApplyChangesOnButtonClick: TAction
      Category = 'FilterPopup'
      AutoCheck = True
      Caption = 'On Button Click'
      GroupIndex = 5
      OnExecute = acClassicModeApplyChangesExecute
    end
    object acClassicModeMultiSelect: TAction
      Category = 'FilterPopup'
      AutoCheck = True
      Caption = 'acClassicModeMultiSelect'
      Checked = True
      Visible = False
      OnExecute = acClassicModeMultiSelectExecute
    end
    object acExcelModeDateTimePageType: TAction
      Category = 'FilterPopup'
      Caption = 'Date Time Values Page Style'
      OnExecute = acDoNothingExecute
    end
    object acExcelModeApplyChanges: TAction
      Category = 'FilterPopup'
      Caption = 'Apply Changes'
      OnExecute = acDoNothingExecute
    end
    object acExcelModeNumericPageType: TAction
      Category = 'FilterPopup'
      Caption = 'Numeric Values Page Style'
      OnExecute = acDoNothingExecute
    end
    object acClassicModeApplyChanges: TAction
      Category = 'FilterPopup'
      Caption = 'Apply Changes'
      Visible = False
      OnExecute = acDoNothingExecute
    end
    object acFilterPopupMode: TAction
      Category = 'FilterPopup'
      Caption = 'Mode (Excel)'
      OnExecute = acDoNothingExecute
    end
  end
  inherited cxLookAndFeelController: TcxLookAndFeelController
    Left = 200
    Top = 256
  end
  object dsBodyStyle: TDataSource
    DataSet = mdBodyStyle
    Left = 88
    Top = 256
  end
  object mdBodyStyle: TdxMemData
    Indexes = <>
    Persistent.Data = {
      5665728FC2F5285C8FFE3F020000000400000003000300494400FF0000001400
      05004E616D6500}
    SortOptions = []
    Left = 48
    Top = 256
    object mdBodyStyleID: TIntegerField
      FieldName = 'ID'
    end
    object mdBodyStyleName: TWideStringField
      FieldName = 'Name'
      Size = 255
    end
  end
  object mdCarOrders: TdxMemData
    Indexes = <>
    SortOptions = []
    Left = 46
    Top = 304
    object mdCarOrdersID: TIntegerField
      FieldName = 'ID'
      Visible = False
    end
    object mdCarOrdersTrademark: TStringField
      FieldName = 'Trademark'
    end
    object mdCarOrdersName: TWideStringField
      DisplayLabel = 'Model'
      FieldName = 'Name'
      Size = 255
    end
    object mdCarOrdersModification: TWideStringField
      FieldName = 'Modification'
      Size = 255
    end
    object mdCarOrdersPrice: TBCDField
      FieldName = 'Price'
    end
    object mdCarOrdersMPG_City: TIntegerField
      DisplayLabel = 'City (mpg)'
      FieldName = 'MPG City'
    end
    object mdCarOrdersMPG_Highway: TIntegerField
      DisplayLabel = 'Highway (mpg)'
      FieldName = 'MPG Highway'
    end
    object mdCarOrdersBodyStyleID: TIntegerField
      FieldName = 'BodyStyleID'
      Visible = False
    end
    object mdCarOrdersCilinders: TIntegerField
      DisplayLabel = 'Cylinders'
      FieldName = 'Cilinders'
    end
    object mdCarOrdersSalesDate: TDateField
      DisplayLabel = 'Sales Date'
      FieldName = 'SalesDate'
    end
    object mdCarOrdersBodyStyle: TStringField
      DisplayLabel = 'Body Style'
      FieldKind = fkLookup
      FieldName = 'BodyStyle'
      LookupDataSet = mdBodyStyle
      LookupKeyFields = 'ID'
      LookupResultField = 'Name'
      KeyFields = 'BodyStyleID'
      Size = 40
      Lookup = True
    end
  end
  object dsCarOrders: TDataSource
    DataSet = mdCarOrders
    Left = 88
    Top = 304
  end
end