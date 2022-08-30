inherited UnboundExternalDataDemoMainForm: TUnboundExternalDataDemoMainForm
  Left = 270
  Top = 120
  Caption = 'ExpressQuantumGrid UnboundExternalData Demo'
  ClientHeight = 346
  ClientWidth = 636
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 636
    Height = 32
    Caption = 
      'Work with Ini files using the grid'#39's unbound mode. Experiment us' +
      'ing the menu items above. Click '#39'About this demo'#39' for more infor' +
      'mation.'
  end
  inherited sbMain: TStatusBar
    Top = 327
    Width = 636
    Panels = <
      item
        Width = 100
      end
      item
        Width = 150
      end
      item
        Text = 'Changes Count: 0'
        Width = 150
      end>
  end
  object cxGrid: TcxGrid [2]
    Left = 0
    Top = 32
    Width = 636
    Height = 295
    Align = alClient
    TabOrder = 1
    object tvSections: TcxGridTableView
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <
        item
        end>
      DataController.Summary.SummaryGroups = <>
      OptionsSelection.InvertSelect = False
      OptionsView.ColumnAutoWidth = True
      OptionsView.GroupByBox = False
      Styles.StyleSheet = GridTableViewStyleSheetDevExpress
    end
    object tvValues: TcxGridTableView
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsSelection.InvertSelect = False
      OptionsView.ColumnAutoWidth = True
      OptionsView.GroupByBox = False
      Styles.StyleSheet = GridTableViewStyleSheetDevExpress
    end
    object SectionLevel: TcxGridLevel
      Caption = 'Sections'
      GridView = tvSections
      MaxDetailHeight = 200
      object DetailLevel: TcxGridLevel
        Caption = 'Details'
        GridView = tvValues
      end
    end
  end
  inherited mmMain: TMainMenu
    Left = 136
    Top = 56
    inherited miFile: TMenuItem
      object miOpen: TMenuItem [0]
        Caption = '&Open...'
        Hint = 'Open the existing ini-file for editing'
        OnClick = miOpenClick
      end
      object miSave: TMenuItem [1]
        Caption = '&Save'
        Hint = 'Saves the modified ini-file'
        ShortCut = 16467
        OnClick = miSaveClick
      end
      object miSaveAs: TMenuItem [2]
        Caption = 'Save&As..'
        Hint = 
          'Saves the modified ini-file allowing you to specify a new name a' +
          'nd a different location'
        OnClick = miSaveAsClick
      end
      object N1: TMenuItem [3]
        Caption = '-'
      end
    end
    object Edit1: TMenuItem [1]
      Caption = '&Edit'
      object miInsertSection: TMenuItem
        Caption = '&Insert'
        Hint = 'Inserts a new record to the current grid view'
        OnClick = miInsertSectionClick
      end
      object miDeleteSection: TMenuItem
        Caption = '&Delete'
        Hint = 'Deletes the selected record from the current grid view'
        OnClick = miDeleteSectionClick
      end
    end
  end
  inherited StyleRepository: TcxStyleRepository
    Left = 16
    Top = 24
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
    inherited GridCardViewStyleSheetDevExpress: TcxGridCardViewStyleSheet
      BuiltIn = True
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '*.ini'
    Filter = '*.ini|*.ini'
    Left = 16
    Top = 56
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '*.ini'
    Filter = '*.ini|*.ini'
    Left = 48
    Top = 56
  end
end
