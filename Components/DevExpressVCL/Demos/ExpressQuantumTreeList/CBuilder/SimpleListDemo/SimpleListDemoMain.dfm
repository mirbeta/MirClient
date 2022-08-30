inherited SimpleListDemoMainForm: TSimpleListDemoMainForm
  Left = 198
  Top = 139
  Caption = 'ExpressQuantumTreeList SimpleList Demo'
  ClientWidth = 681
  OldCreateOrder = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited lscrip: TLabel
    Width = 681
    Caption = 
      'Use Express QuantumTreeList as a ListView replacement (in report' +
      ' mode). Experiment by changing the Options above and also see He' +
      'lp/About for other things to try.'
  end
  inherited sbMain: TStatusBar
    Width = 681
  end
  object cxDBTreeList: TcxDBTreeList [2]
    Left = 0
    Top = 32
    Width = 681
    Height = 361
    Align = alClient
    Bands = <
      item
        Caption.AlignHorz = taCenter
        Caption.Text = 'Primary Info'
        Width = 401
      end
      item
        Caption.Text = 'Secondary Info'
        Width = 1003
      end>
    DataController.DataSource = dmCars.dsModels
    DataController.ParentField = 'ID'
    DataController.KeyField = 'ID'
    Navigator.Buttons.CustomButtons = <>
    OptionsBehavior.IncSearch = True
    OptionsBehavior.IncSearchItem = cxDBTreeListTrademark
    OptionsSelection.MultiSelect = True
    OptionsView.Bands = True
    OptionsView.GridLineColor = 15451300
    OptionsView.Indicator = True
    OptionsView.ShowRoot = False
    PopupMenus.ColumnHeaderMenu.UseBuiltInMenu = True
    RootValue = -1
    Styles.StyleSheet = SimpleListDemoDataDM.TreeListStyleSheetDevExpress
    TabOrder = 1
    object cxDBTreeListID: TcxDBTreeListColumn
      Visible = False
      DataBinding.FieldName = 'ID'
      Position.ColIndex = 4
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListTrademark: TcxDBTreeListColumn
      DataBinding.FieldName = 'Trademark'
      Position.ColIndex = 0
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListModel: TcxDBTreeListColumn
      Caption.Text = 'Model'
      DataBinding.FieldName = 'Name'
      Position.ColIndex = 1
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListPicture: TcxDBTreeListColumn
      RepositoryItem = eriPicture
      DataBinding.FieldName = 'Photo'
      Position.ColIndex = 2
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListPrice: TcxDBTreeListColumn
      DataBinding.FieldName = 'Price'
      Width = 101
      Position.ColIndex = 3
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListHP: TcxDBTreeListColumn
      RepositoryItem = eriHP
      DataBinding.FieldName = 'Horsepower'
      Position.ColIndex = 0
      Position.RowIndex = 0
      Position.BandIndex = 1
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListTorque: TcxDBTreeListColumn
      DataBinding.FieldName = 'Torque'
      Position.ColIndex = 1
      Position.RowIndex = 0
      Position.BandIndex = 1
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListCyl: TcxDBTreeListColumn
      RepositoryItem = cxEditRepositorySpinItem
      Caption.Text = 'Cyl'
      DataBinding.FieldName = 'Cilinders'
      Width = 101
      Position.ColIndex = 2
      Position.RowIndex = 0
      Position.BandIndex = 1
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListTransmissSpeedCount: TcxDBTreeListColumn
      RepositoryItem = cxEditRepositorySpinItem
      DataBinding.FieldName = 'Transmission Speeds'
      Position.ColIndex = 3
      Position.RowIndex = 0
      Position.BandIndex = 1
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListTransmissAutomatic: TcxDBTreeListColumn
      RepositoryItem = dmCars.EditRepositoryTransmissionTypeCheckBox
      DataBinding.FieldName = 'Transmission Type'
      Position.ColIndex = 4
      Position.RowIndex = 0
      Position.BandIndex = 1
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListMPG_City: TcxDBTreeListColumn
      RepositoryItem = cxEditRepositorySpinItem
      DataBinding.FieldName = 'MPG City'
      Position.ColIndex = 5
      Position.RowIndex = 0
      Position.BandIndex = 1
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListMPG_Highway: TcxDBTreeListColumn
      RepositoryItem = cxEditRepositorySpinItem
      DataBinding.FieldName = 'MPG Highway'
      Width = 101
      Position.ColIndex = 6
      Position.RowIndex = 0
      Position.BandIndex = 1
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListCategory: TcxDBTreeListColumn
      RepositoryItem = dmCars.EditRepositoryCategoryLookup
      Caption.Text = 'Category'
      DataBinding.FieldName = 'CategoryID'
      Width = 101
      Position.ColIndex = 7
      Position.RowIndex = 0
      Position.BandIndex = 1
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListHyperlink: TcxDBTreeListColumn
      RepositoryItem = eriURL
      DataBinding.FieldName = 'Hyperlink'
      Position.ColIndex = 8
      Position.RowIndex = 0
      Position.BandIndex = 1
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListDescription: TcxDBTreeListColumn
      RepositoryItem = eriDescription
      DataBinding.FieldName = 'Description'
      Position.ColIndex = 9
      Position.RowIndex = 0
      Position.BandIndex = 1
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
  end
  inherited mmMain: TMainMenu
    inherited miOptions: TMenuItem
      object miView: TMenuItem [0]
        Caption = '&View'
        object miBands: TMenuItem
          AutoCheck = True
          Caption = '&Bands'
          Checked = True
          Hint = 'Shows/hides bands within a tree list control'
          OnClick = miBandsClick
        end
        object miHeaders: TMenuItem
          AutoCheck = True
          Caption = '&Headers'
          Checked = True
          Hint = 'Shows/hides column headers within a tree list control'
          OnClick = miHeadersClick
        end
        object miGridLines: TMenuItem
          AutoCheck = True
          Caption = 'Grid &Lines'
          Hint = 'Displays lines that separate TreeList items'
          OnClick = miGridLinesClick
        end
      end
      object miBehavior: TMenuItem [1]
        Caption = '&Behavior'
        object miIncSearch: TMenuItem
          AutoCheck = True
          Caption = '&Incremental Search'
          Checked = True
          Hint = 
            'Enables a user to start typing and make the TreeList automatical' +
            'ly navigate to the closest match'
          OnClick = miIncSearchClick
        end
        object miFocusCellOnCycle: TMenuItem
          AutoCheck = True
          Caption = 'Focus Cell On &Cycle'
          Hint = 
            'Enables moving focus to the next row after it reaches the last c' +
            'ell within the current row'
          OnClick = miFocusCellOnCycleClick
        end
        object miImmediateEditor: TMenuItem
          AutoCheck = True
          Caption = 'Imme&diate Editor'
          Checked = True
          Hint = 
            'If checked, a specific column editor is activated when a user cl' +
            'icks an appropriate cell'
          OnClick = miImmediateEditorClick
        end
      end
      object miMultiSelect: TMenuItem [2]
        AutoCheck = True
        Caption = '&Multi Select'
        Checked = True
        Hint = 'Enables multiple rows  selection'
        OnClick = miMultiSelectClick
      end
      object miSeparator1: TMenuItem [3]
        Caption = '-'
      end
    end
  end
  object cxEditRepository: TcxEditRepository
    Left = 352
    Top = 8
    object cxEditRepositorySpinItem: TcxEditRepositorySpinItem
      Properties.SpinButtons.ShowFastButtons = True
    end
    object eriPicture: TcxEditRepositoryBlobItem
      Properties.BlobEditKind = bekPict
      Properties.PictureGraphicClassName = 'TJPEGImage'
    end
    object eriHP: TcxEditRepositorySpinItem
      Properties.Increment = 10.000000000000000000
      Properties.LargeIncrement = 100.000000000000000000
      Properties.SpinButtons.ShowFastButtons = True
    end
    object eriDescription: TcxEditRepositoryBlobItem
      Properties.BlobEditKind = bekMemo
      Properties.MemoScrollBars = ssVertical
      Properties.PictureGraphicClassName = 'TJPEGImage'
    end
    object eriURL: TcxEditRepositoryHyperLinkItem
    end
  end
end
