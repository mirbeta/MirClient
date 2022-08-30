inherited frmMain: TfrmMain
  Left = 401
  Top = 185
  Caption = 'ExpressVerticalGrid Find Panel Demo'
  ClientHeight = 551
  ClientWidth = 763
  Constraints.MinHeight = 600
  Constraints.MinWidth = 770
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescrip: TLabel
    Width = 763
    Height = 48
    Caption = 
      'This demo illustrates the search capabilities provided by the bu' +
      'ilt-in Find Panel. To execute a search, simply enter text within' +
      ' the Find Panel'#39's box and the vertical grid will display those r' +
      'ecords that have matching values. Click Help | About this demo f' +
      'or more information.'
  end
  inherited sbMain: TStatusBar
    Top = 532
    Width = 763
  end
  object cxGroupBox1: TcxGroupBox [2]
    Left = 0
    Top = 48
    Align = alTop
    TabOrder = 2
    Height = 113
    Width = 763
    object cbClearFindOnClose: TcxCheckBox
      Left = 467
      Top = 10
      Action = actClearFindOnClose
      TabOrder = 0
      Transparent = True
    end
    object cbShowClearButton: TcxCheckBox
      Left = 641
      Top = 58
      Action = actShowClearButton
      TabOrder = 1
      Transparent = True
    end
    object cbShowCloseButton: TcxCheckBox
      Left = 641
      Top = 10
      Action = actShowCloseButton
      TabOrder = 2
      Transparent = True
    end
    object cbShowFindButton: TcxCheckBox
      Left = 641
      Top = 34
      Action = actShowFindButton
      TabOrder = 3
      Transparent = True
    end
    object cbHighlightSearchResults: TcxCheckBox
      Left = 467
      Top = 34
      Action = actHighlightSearchResults
      TabOrder = 4
      Transparent = True
    end
    object seFindDelay: TcxSpinEdit
      Left = 120
      Top = 58
      Properties.MaxValue = 5000.000000000000000000
      Properties.MinValue = 100.000000000000000000
      Properties.OnChange = seFindDelayPropertiesChange
      TabOrder = 5
      Value = 1000
      Width = 334
    end
    object lbSearchDelay: TcxLabel
      Left = 12
      Top = 60
      Caption = 'Search Delay, ms:'
      Transparent = True
    end
    object icbFindFilterColumns: TcxImageComboBox
      Left = 120
      Top = 82
      Properties.Items = <>
      Properties.OnChange = icbFindFilterColumnsPropertiesChange
      TabOrder = 7
      Width = 334
    end
    object lbSearchableColumns: TcxLabel
      Left = 12
      Top = 84
      Caption = 'Searchable Rows:'
      Transparent = True
    end
    object cbeFindPanelPosition: TcxComboBox
      Left = 120
      Top = 34
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'Top'
        'Bottom')
      Properties.OnChange = cbFindPanelPositionPropertiesChange
      TabOrder = 9
      Text = 'Top'
      Width = 334
    end
    object lbFindPanelPosition: TcxLabel
      Left = 12
      Top = 36
      Caption = 'Find Panel Position:'
      Transparent = True
    end
    object cbeDisplayMode: TcxComboBox
      Left = 120
      Top = 10
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'Never'
        'Manual (Focus the grid and press Ctrl+F)'
        'Always')
      Properties.OnChange = cbDisplayModePropertiesChange
      TabOrder = 11
      Text = 'Manual (Focus the grid and press Ctrl+F)'
      Width = 334
    end
    object lbDisplayMode: TcxLabel
      Left = 12
      Top = 12
      Caption = 'Display Mode:'
      Transparent = True
    end
    object cbUseDelayedSearch: TcxCheckBox
      Left = 467
      Top = 58
      Action = actUseDelayedSearch
      TabOrder = 13
      Transparent = True
    end
    object cbUseExtendedSyntax: TcxCheckBox
      Left = 467
      Top = 82
      Action = actUseExtendedSyntax
      TabOrder = 14
      Transparent = True
    end
  end
  inherited memAboutText: TMemo
    Lines.Strings = (
      'In this demo you can:'
      ''
      '.....'
      ''
      
        'Enter text within the Find Panel'#39's box and the grid will display' +
        ' those records that have one or more fields that include the tex' +
        't.'
      ''
      
        'Enable the extended syntax for search strings with the correspon' +
        'ding option to apply multiple conditions. According to the exten' +
        'ded syntax, words separated by the space character are treated a' +
        's individual conditions combined by the OR logical operator. The' +
        ' grid shows records that match at least one of these conditions.' +
        ' To search for a string containing a space character, enclose th' +
        'e string in quotation marks.'
      ''
      
        'The syntax allows you to use the following specifiers and wildca' +
        'rds to narrow search results:'
      ''
      
        ' - The "+" specifier. Preceding a condition with this specifier ' +
        'causes the grid to display only records that match this conditio' +
        'n. The "+" specifier implements the logical AND operator. There ' +
        'should be no space character between the "+" sign and the condit' +
        'ion.'
      ''
      
        ' - The "'#8211'" specifier. Preceding a condition with "'#8211'" excludes re' +
        'cords that match this condition from search results. There shoul' +
        'd be no space between the "'#8211'" sign and the condition.'
      ''
      
        ' - The percent ("%") wildcard. This wildcard substitutes any num' +
        'ber of characters in a condition.'
      ''
      
        ' - The underscore ("_") wildcard. This wildcard represents any s' +
        'ingle character in a condition.'
      ''
      'Try various options to adjust the Find Panel'#39's look & feel.'
      ''
      
        'All the editing capabilities are enabled in this demo for you to' +
        ' try them out in combination with the Find Panel.')
  end
  object VerticalGrid: TcxDBVerticalGrid [4]
    Left = 0
    Top = 161
    Width = 763
    Height = 371
    Align = alClient
    FindPanel.DisplayMode = fpdmManual
    FindPanel.UseExtendedSyntax = True
    LayoutStyle = lsMultiRecordView
    OptionsView.GridLineColor = clBtnFace
    OptionsView.ValueWidth = 170
    Navigator.Buttons.CustomButtons = <>
    TabOrder = 3
    DataController.DataSource = dsEmployeesGroups
    Version = 1
    object VerticalGridRecId: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'RecId'
      Visible = False
      ID = 0
      ParentID = -1
      Index = 0
      Version = 1
    end
    object VerticalGridId: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'Id'
      Visible = False
      ID = 1
      ParentID = -1
      Index = 1
      Version = 1
    end
    object VerticalGridParentId: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'ParentId'
      Visible = False
      ID = 2
      ParentID = -1
      Index = 2
      Version = 1
    end
    object VerticalGridJobTitle: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'JobTitle'
      ID = 3
      ParentID = -1
      Index = 3
      Version = 1
    end
    object VerticalGridFirstName: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'FirstName'
      ID = 4
      ParentID = -1
      Index = 4
      Version = 1
    end
    object VerticalGridLastName: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'LastName'
      ID = 5
      ParentID = -1
      Index = 5
      Version = 1
    end
    object VerticalGridCity: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'City'
      ID = 6
      ParentID = -1
      Index = 6
      Version = 1
    end
    object VerticalGridStateProvinceName: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'StateProvinceName'
      ID = 7
      ParentID = -1
      Index = 7
      Version = 1
    end
    object VerticalGridPhone: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'Phone'
      ID = 8
      ParentID = -1
      Index = 8
      Version = 1
    end
    object VerticalGridEmailAddress: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'EmailAddress'
      ID = 9
      ParentID = -1
      Index = 9
      Version = 1
    end
    object VerticalGridAddressLine1: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'AddressLine1'
      ID = 10
      ParentID = -1
      Index = 10
      Version = 1
    end
    object VerticalGridPostalCode: TcxDBEditorRow
      Properties.DataBinding.FieldName = 'PostalCode'
      ID = 11
      ParentID = -1
      Index = 11
      Version = 1
    end
  end
  inherited mmMain: TMainMenu
    Left = 220
    Top = 192
    object miFindPanelOptions: TMenuItem [1]
      Caption = 'Find Panel Options'
      object ClearFindOnClose1: TMenuItem
        Action = actClearFindOnClose
        AutoCheck = True
      end
      object HighlightFindResult1: TMenuItem
        Action = actHighlightSearchResults
        AutoCheck = True
      end
      object miVisibleButtons: TMenuItem
        Caption = 'Button Visibility'
        object ShowClearButton2: TMenuItem
          Action = actShowClearButton
          AutoCheck = True
        end
        object ShowCloseButton2: TMenuItem
          Action = actShowCloseButton
          AutoCheck = True
        end
        object ShowFindButton2: TMenuItem
          Action = actShowFindButton
          AutoCheck = True
        end
      end
      object UseDelayedFind1: TMenuItem
        Action = actUseDelayedSearch
        AutoCheck = True
      end
      object UseExtendedSyntax1: TMenuItem
        Action = actUseExtendedSyntax
        AutoCheck = True
      end
    end
  end
  inherited ilMain: TImageList
    Bitmap = {
      494C0101040009000C0010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
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
  object erMain: TcxEditRepository
    Left = 380
    Top = 196
    PixelsPerInch = 96
    object erMainFlag: TcxEditRepositoryImageItem
      Properties.FitMode = ifmProportionalStretch
      Properties.GraphicClassName = 'TdxSmartImage'
    end
  end
  object alAction: TActionList
    Left = 304
    Top = 192
    object actClearFindOnClose: TAction
      AutoCheck = True
      Caption = 'Clear Find Filter Text On Close'
      Checked = True
      OnExecute = actClearFindOnCloseChange
    end
    object actShowClearButton: TAction
      AutoCheck = True
      Caption = 'Show Clear Button'
      Checked = True
      OnExecute = actShowClearButtonChange
    end
    object actShowCloseButton: TAction
      AutoCheck = True
      Caption = 'Show Close Button'
      Checked = True
      OnExecute = actShowCloseButtonChange
    end
    object actShowFindButton: TAction
      AutoCheck = True
      Caption = 'Show Find Button'
      Checked = True
      OnExecute = actShowFindButtonEChange
    end
    object actHighlightSearchResults: TAction
      AutoCheck = True
      Caption = 'Highlight Search Results'
      Checked = True
      OnExecute = actHighlightFindResultChange
    end
    object actUseDelayedSearch: TAction
      AutoCheck = True
      Caption = 'Use Delayed Search'
      Checked = True
      OnExecute = actUseDelayedSearchExecute
    end
    object actUseExtendedSyntax: TAction
      AutoCheck = True
      Caption = 'Use Extended Syntax'
      OnExecute = actUseExtendedSyntaxExecute
    end
  end
  object mdEmployeesGroups: TdxMemData
    Indexes = <>
    SortOptions = []
    Left = 216
    Top = 272
    object mdEmployeesGroupsId: TStringField
      FieldName = 'Id'
      Visible = False
      Size = 3
    end
    object mdEmployeesGroupsParentId: TStringField
      FieldName = 'ParentId'
      Visible = False
      Size = 3
    end
    object mdEmployeesGroupsJobTitle: TStringField
      DisplayLabel = 'Job Title'
      FieldName = 'JobTitle'
      Size = 40
    end
    object mdEmployeesGroupsFirstName: TStringField
      DisplayLabel = 'First Name'
      FieldName = 'FirstName'
      Size = 10
    end
    object mdEmployeesGroupsLastName: TStringField
      DisplayLabel = 'Last Name'
      FieldName = 'LastName'
      Size = 9
    end
    object mdEmployeesGroupsCity: TStringField
      DisplayLabel = 'Origin City'
      FieldName = 'City'
      Size = 12
    end
    object mdEmployeesGroupsStateProvinceName: TStringField
      DisplayLabel = 'Origin State'
      FieldName = 'StateProvinceName'
      Size = 13
    end
    object mdEmployeesGroupsPhone: TStringField
      FieldName = 'Phone'
      Size = 14
    end
    object mdEmployeesGroupsEmailAddress: TStringField
      DisplayLabel = 'E-mail'
      FieldName = 'EmailAddress'
      Size = 30
    end
    object mdEmployeesGroupsAddressLine1: TStringField
      DisplayLabel = 'Address Line'
      FieldName = 'AddressLine1'
      Size = 30
    end
    object mdEmployeesGroupsPostalCode: TStringField
      DisplayLabel = 'Postal Code'
      FieldName = 'PostalCode'
      Size = 5
    end
  end
  object dsEmployeesGroups: TDataSource
    DataSet = mdEmployeesGroups
    Left = 264
    Top = 272
  end
end
