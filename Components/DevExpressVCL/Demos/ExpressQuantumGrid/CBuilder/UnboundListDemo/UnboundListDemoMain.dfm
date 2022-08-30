inherited UnboundListDemoMainForm: TUnboundListDemoMainForm
  Left = 270
  Top = 120
  Caption = 'ExpressQuantumGrid UnboundList Demo'
  ClientHeight = 407
  ClientWidth = 692
  OldCreateOrder = True
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Caption = 
      'In this example the grid is working in Provider Mode and using a' +
      ' text file as its data source. Click '#39'About this demo'#39' for more ' +
      'information.'
  end
  inherited sbMain: TStatusBar
    Top = 388
    Width = 692
  end
  object cxGrid: TcxGrid [2]
    Left = 0
    Top = 32
    Width = 692
    Height = 356
    Align = alClient
    TabOrder = 1
    object tvCustomers: TcxGridTableView
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsView.ColumnAutoWidth = True
      OptionsView.Indicator = True
      Styles.StyleSheet = GridTableViewStyleSheetDevExpress
    end
    object lvCustomers: TcxGridLevel
      GridView = tvCustomers
    end
  end
  inherited StyleRepository: TcxStyleRepository
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
    inherited GridCardViewStyleSheetDevExpress: TcxGridCardViewStyleSheet
      BuiltIn = True
    end
  end
  object cxGridPopupMenu: TcxGridPopupMenu
    Grid = cxGrid
    PopupMenus = <>
    Left = 332
    Top = 20
  end
end
