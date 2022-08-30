inherited UnboundSimpleDemoMainForm: TUnboundSimpleDemoMainForm
  Left = 270
  Top = 120
  Caption = 'ExpressQuantumGrid UnboundSimple Demo'
  ClientHeight = 484
  ClientWidth = 781
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 781
    Height = 32
    Caption = 
      'This example demonstrates the grid in unbound mode loading recor' +
      'ds from a CSV file. Click '#39'About this demo'#39' for more information' +
      '.'
  end
  object cxGrid: TcxGrid [1]
    Left = 0
    Top = 32
    Width = 781
    Height = 452
    Align = alClient
    TabOrder = 0
    object tvPlanets: TcxGridTableView
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsBehavior.IncSearch = True
      OptionsData.Editing = False
      OptionsData.Inserting = False
      OptionsView.ColumnAutoWidth = True
      OptionsView.HeaderAutoHeight = True
      object tvPlanetsNAME: TcxGridColumn
        Caption = 'Name'
        HeaderAlignmentHorz = taCenter
        Width = 142
      end
      object tvPlanetsNO: TcxGridColumn
        Caption = '#'
        RepositoryItem = edrepCenterText
        HeaderAlignmentHorz = taCenter
        Width = 56
      end
      object tvPlanetsORBITS: TcxGridColumn
        Caption = 'Orbits'
        RepositoryItem = edrepCenterText
        HeaderAlignmentHorz = taCenter
        Width = 91
      end
      object tvPlanetsDISTANCE: TcxGridColumn
        Caption = 'Distance (1000km)'
        RepositoryItem = edrepRightText
        HeaderAlignmentHorz = taCenter
        Width = 117
      end
      object tvPlanetsPERIOD: TcxGridColumn
        Caption = 'Period (days)'
        RepositoryItem = edrepRightText
        HeaderAlignmentHorz = taCenter
        Width = 112
      end
      object tvPlanetsDISCOVERER: TcxGridColumn
        Caption = 'Discoverer'
        RepositoryItem = edrepCenterText
        HeaderAlignmentHorz = taCenter
        Width = 89
      end
      object tvPlanetsDATE: TcxGridColumn
        Caption = 'Date'
        RepositoryItem = edrepCenterText
        HeaderAlignmentHorz = taCenter
        Width = 91
      end
      object tvPlanetsRADIUS: TcxGridColumn
        Caption = 'Radius (km)'
        RepositoryItem = edrepRightText
        HeaderAlignmentHorz = taCenter
        Width = 89
      end
    end
    object lvPlanets: TcxGridLevel
      GridView = tvPlanets
    end
  end
  inherited mmMain: TMainMenu
    Left = 272
    Top = 35
  end
  inherited StyleRepository: TcxStyleRepository
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
  end
  object edrepMain: TcxEditRepository
    Left = 304
    Top = 35
    object edrepCenterText: TcxEditRepositoryTextItem
      Properties.Alignment.Horz = taCenter
    end
    object edrepRightText: TcxEditRepositoryTextItem
      Properties.Alignment.Horz = taRightJustify
    end
  end
end
