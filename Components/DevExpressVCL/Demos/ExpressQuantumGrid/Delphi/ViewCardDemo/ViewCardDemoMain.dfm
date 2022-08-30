inherited ViewCardDemoMainForm: TViewCardDemoMainForm
  Left = 107
  Top = 75
  Caption = 'ExpressQuantumGrid ViewCard Demo'
  ClientHeight = 465
  ClientWidth = 781
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 781
    Height = 32
    Caption = 
      'Use the new Card View'#39's functionality. Experiment by changing th' +
      'e Options. Click '#39'About this demo'#39' for more information.'
  end
  object cxGrid: TcxGrid [1]
    Left = 0
    Top = 32
    Width = 781
    Height = 433
    Align = alClient
    TabOrder = 0
    object cvPersons: TcxGridDBCardView
      FilterBox.Visible = fvAlways
      DataController.DataSource = ViewCardDemoDataDM.dsPersons
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsCustomize.CardExpanding = True
      OptionsCustomize.RowMoving = True
      OptionsView.CardIndent = 7
      OptionsView.CardWidth = 370
      OptionsView.CellAutoHeight = True
      object cvPersonsFullname: TcxGridDBCardViewRow
        DataBinding.FieldName = 'FullName'
        Kind = rkCaption
        Position.BeginsLayer = True
      end
      object cvPersonsID: TcxGridDBCardViewRow
        DataBinding.FieldName = 'ID'
        Visible = False
        Position.BeginsLayer = True
      end
      object cvPersonsFIRSTNAME: TcxGridDBCardViewRow
        DataBinding.FieldName = 'FIRSTNAME'
        Position.BeginsLayer = True
      end
      object cvPersonsSECONDNAME: TcxGridDBCardViewRow
        DataBinding.FieldName = 'SECONDNAME'
        Position.BeginsLayer = True
      end
      object cvPersonsGENDER: TcxGridDBCardViewRow
        DataBinding.FieldName = 'GENDER'
        PropertiesClassName = 'TcxImageComboBoxProperties'
        Properties.Images = ViewCardDemoDataDM.ilPics
        Properties.Items = <
          item
            Description = 'Female'
            ImageIndex = 0
            Value = False
          end
          item
            Description = 'Male'
            ImageIndex = 1
            Value = True
          end>
        Position.BeginsLayer = True
      end
      object cvPersonsBIRTHNAME: TcxGridDBCardViewRow
        DataBinding.FieldName = 'BIRTHNAME'
        Position.BeginsLayer = True
      end
      object cvPersonsDATEOFBIRTH: TcxGridDBCardViewRow
        DataBinding.FieldName = 'DATEOFBIRTH'
        Position.BeginsLayer = True
      end
      object cvPersonsBIRTHCOUNTRY: TcxGridDBCardViewRow
        DataBinding.FieldName = 'BIRTHCOUNTRY'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'ACRONYM'
          end
          item
            FieldName = 'NAME'
          end>
        Properties.ListFieldIndex = 1
        Properties.ListSource = ViewCardDemoDataDM.dsCountries
        Visible = False
        Position.BeginsLayer = True
      end
      object cvPersonsLOCATIONOFBIRTH: TcxGridDBCardViewRow
        DataBinding.FieldName = 'LOCATIONOFBIRTH'
        Visible = False
        Position.BeginsLayer = True
      end
      object cvPersonsNICKNAME: TcxGridDBCardViewRow
        DataBinding.FieldName = 'NICKNAME'
        Position.BeginsLayer = True
      end
      object cvPersonsHOMEPAGE: TcxGridDBCardViewRow
        DataBinding.FieldName = 'HOMEPAGE'
        PropertiesClassName = 'TcxHyperLinkEditProperties'
        Position.BeginsLayer = True
      end
      object cvPersonsBIOGRAPHY: TcxGridDBCardViewRow
        DataBinding.FieldName = 'BIOGRAPHY'
        PropertiesClassName = 'TcxMemoProperties'
        Properties.ScrollBars = ssVertical
        Options.Filtering = False
        Position.BeginsLayer = True
      end
    end
    object lvPersons: TcxGridLevel
      Caption = 'DevExpress'
      GridView = cvPersons
    end
  end
  inherited mmMain: TMainMenu
    object miOptions: TMenuItem [1]
      Caption = 'Options'
      object miShowEmptyRows: TMenuItem
        AutoCheck = True
        Caption = 'Show &empty rows'
        Checked = True
        Hint = 'Show/hide empty rows '
        OnClick = miShowEmptyRowsClick
      end
      object miFiltering: TMenuItem
        AutoCheck = True
        Caption = '&Filtering'
        Checked = True
        Hint = 'Enables data to be filtered visually'
        OnClick = miFilteringClick
      end
      object miExpandingCollapsing: TMenuItem
        AutoCheck = True
        Caption = 'Card E&xpanding/Collapsing'
        Checked = True
        Hint = 'Enables cards to be expanded/collapsed'
        OnClick = miExpandingCollapsingClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object miRowsCustomization: TMenuItem
        Caption = 'Rows C&ustomization...'
        Hint = 'Shows the customization form for rows'
        OnClick = miRowsCustomizationClick
      end
    end
  end
  inherited StyleRepository: TcxStyleRepository
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
  end
end
