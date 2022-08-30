inherited StylesCardViewDemoMainForm: TStylesCardViewDemoMainForm
  Left = 270
  Top = 120
  Caption = 'ExpressQuantumGrid StylesCardView Demo'
  ClientHeight = 572
  ClientWidth = 808
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 808
    Height = 32
    Caption = 
      'Practice using style sheets in a card view. Experiment by changi' +
      'ng the Options View and pressing the Edit User-Defined Style She' +
      'et button below. Click '#39'About this demo'#39' for more information.'
  end
  inherited sbMain: TStatusBar
    Top = 553
    Width = 808
  end
  object cxGrid: TcxGrid [2]
    Left = 177
    Top = 32
    Width = 631
    Height = 521
    Align = alClient
    TabOrder = 1
    RootLevelOptions.DetailTabsPosition = dtpTop
    OnActiveTabChanged = cxGridActiveTabChanged
    OnLayoutChanged = cxGridLayoutChanged
    object cvDevExpress: TcxGridDBCardView
      DataController.DataSource = StylesCardViewDemoMainDM.dsPersons
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsView.CardIndent = 7
      OptionsView.CardWidth = 300
      OptionsView.CellAutoHeight = True
      Styles.StyleSheet = StylesCardViewDemoMainDM.cvssDevExpress
      object cvDevExpressFullname: TcxGridDBCardViewRow
        DataBinding.FieldName = 'FullName'
        Kind = rkCaption
        Position.BeginsLayer = True
      end
      object cvDevExpressID: TcxGridDBCardViewRow
        DataBinding.FieldName = 'ID'
        Position.BeginsLayer = True
      end
      object cvDevExpressFIRSTNAME: TcxGridDBCardViewRow
        DataBinding.FieldName = 'FIRSTNAME'
        Position.BeginsLayer = True
      end
      object cvDevExpressSECONDNAME: TcxGridDBCardViewRow
        DataBinding.FieldName = 'SECONDNAME'
        Position.BeginsLayer = True
      end
      object cvDevExpressGENDER: TcxGridDBCardViewRow
        DataBinding.FieldName = 'GENDER'
        RepositoryItem = StylesCardViewDemoMainDM.edrepGender
        Position.BeginsLayer = True
      end
      object cvDevExpressBIRTHNAME: TcxGridDBCardViewRow
        DataBinding.FieldName = 'BIRTHNAME'
        Position.BeginsLayer = True
      end
      object cvDevExpressDATEOFBIRTH: TcxGridDBCardViewRow
        DataBinding.FieldName = 'DATEOFBIRTH'
        Position.BeginsLayer = True
      end
      object cvDevExpressBIRTHCOUNTRY: TcxGridDBCardViewRow
        DataBinding.FieldName = 'BIRTHCOUNTRY'
        RepositoryItem = StylesCardViewDemoMainDM.edrepCountry
        Position.BeginsLayer = True
      end
      object cvDevExpressLOCATIONOFBIRTH: TcxGridDBCardViewRow
        DataBinding.FieldName = 'LOCATIONOFBIRTH'
        Position.BeginsLayer = True
      end
      object cvDevExpressNICKNAME: TcxGridDBCardViewRow
        DataBinding.FieldName = 'NICKNAME'
        Position.BeginsLayer = True
      end
      object cvDevExpressBIOGRAPHY: TcxGridDBCardViewRow
        DataBinding.FieldName = 'BIOGRAPHY'
        Options.Filtering = False
        Position.BeginsLayer = True
      end
    end
    object cvSlate: TcxGridDBCardView
      DataController.DataSource = StylesCardViewDemoMainDM.dsPersons
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsView.CardIndent = 7
      OptionsView.CardWidth = 300
      OptionsView.CellAutoHeight = True
      Styles.StyleSheet = StylesCardViewDemoMainDM.cvssSlate
      object cvSlateFullName: TcxGridDBCardViewRow
        DataBinding.FieldName = 'FullName'
        Kind = rkCaption
        Position.BeginsLayer = True
      end
      object cvSlateID: TcxGridDBCardViewRow
        DataBinding.FieldName = 'ID'
        Position.BeginsLayer = True
      end
      object cvSlateFIRSTNAME: TcxGridDBCardViewRow
        DataBinding.FieldName = 'FIRSTNAME'
        Position.BeginsLayer = True
      end
      object cvSlateSECONDNAME: TcxGridDBCardViewRow
        DataBinding.FieldName = 'SECONDNAME'
        Position.BeginsLayer = True
      end
      object cvSlateGENDER: TcxGridDBCardViewRow
        DataBinding.FieldName = 'GENDER'
        RepositoryItem = StylesCardViewDemoMainDM.edrepGender
        Position.BeginsLayer = True
      end
      object cvSlateBIRTHNAME: TcxGridDBCardViewRow
        DataBinding.FieldName = 'BIRTHNAME'
        Position.BeginsLayer = True
      end
      object cvSlateDATEOFBIRTH: TcxGridDBCardViewRow
        DataBinding.FieldName = 'DATEOFBIRTH'
        Position.BeginsLayer = True
      end
      object cvSlateBIRTHCOUNTRY: TcxGridDBCardViewRow
        DataBinding.FieldName = 'BIRTHCOUNTRY'
        RepositoryItem = StylesCardViewDemoMainDM.edrepCountry
        Position.BeginsLayer = True
      end
      object cvSlateLOCATIONOFBIRTH: TcxGridDBCardViewRow
        DataBinding.FieldName = 'LOCATIONOFBIRTH'
        Position.BeginsLayer = True
      end
      object cvSlateBIOGRAPHY: TcxGridDBCardViewRow
        DataBinding.FieldName = 'BIOGRAPHY'
        Options.Filtering = False
        Position.BeginsLayer = True
      end
      object cvSlateNICKNAME: TcxGridDBCardViewRow
        DataBinding.FieldName = 'NICKNAME'
        Position.BeginsLayer = True
      end
    end
    object cvHighContrast: TcxGridDBCardView
      DataController.DataSource = StylesCardViewDemoMainDM.dsPersons
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsView.CardIndent = 7
      OptionsView.CardWidth = 300
      OptionsView.CellAutoHeight = True
      Styles.StyleSheet = StylesCardViewDemoMainDM.cvssHighContrast
      object cvHighContrastFullName: TcxGridDBCardViewRow
        DataBinding.FieldName = 'FullName'
        Kind = rkCaption
        Position.BeginsLayer = True
      end
      object cvHighContrastID: TcxGridDBCardViewRow
        DataBinding.FieldName = 'ID'
        Position.BeginsLayer = True
      end
      object cvHighContrastFIRSTNAME: TcxGridDBCardViewRow
        DataBinding.FieldName = 'FIRSTNAME'
        Position.BeginsLayer = True
      end
      object cvHighContrastSECONDNAME: TcxGridDBCardViewRow
        DataBinding.FieldName = 'SECONDNAME'
        Position.BeginsLayer = True
      end
      object cvHighContrastGENDER: TcxGridDBCardViewRow
        DataBinding.FieldName = 'GENDER'
        RepositoryItem = StylesCardViewDemoMainDM.edrepGender
        Position.BeginsLayer = True
      end
      object cvHighContrastBIRTHNAME: TcxGridDBCardViewRow
        DataBinding.FieldName = 'BIRTHNAME'
        Position.BeginsLayer = True
      end
      object cvHighContrastDATEOFBIRTH: TcxGridDBCardViewRow
        DataBinding.FieldName = 'DATEOFBIRTH'
        Position.BeginsLayer = True
      end
      object cvHighContrastBIRTHCOUNTRY: TcxGridDBCardViewRow
        DataBinding.FieldName = 'BIRTHCOUNTRY'
        RepositoryItem = StylesCardViewDemoMainDM.edrepCountry
        Position.BeginsLayer = True
      end
      object cvHighContrastLOCATIONOFBIRTH: TcxGridDBCardViewRow
        DataBinding.FieldName = 'LOCATIONOFBIRTH'
        Position.BeginsLayer = True
      end
      object cvHighContrastBIOGRAPHY: TcxGridDBCardViewRow
        DataBinding.FieldName = 'BIOGRAPHY'
        Options.Filtering = False
        Position.BeginsLayer = True
      end
      object cvHighContrastNICKNAME: TcxGridDBCardViewRow
        DataBinding.FieldName = 'NICKNAME'
        Position.BeginsLayer = True
      end
    end
    object cvUserDefined: TcxGridDBCardView
      DataController.DataSource = StylesCardViewDemoMainDM.dsPersons
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsView.CardIndent = 7
      OptionsView.CardWidth = 300
      OptionsView.CellAutoHeight = True
      Styles.StyleSheet = StylesCardViewDemoMainDM.cvssUserDefined
      object cvUserDefinedFullName: TcxGridDBCardViewRow
        DataBinding.FieldName = 'FullName'
        Kind = rkCaption
        Position.BeginsLayer = True
      end
      object cvUserDefinedID: TcxGridDBCardViewRow
        DataBinding.FieldName = 'ID'
        Position.BeginsLayer = True
      end
      object cvUserDefinedFIRSTNAME: TcxGridDBCardViewRow
        DataBinding.FieldName = 'FIRSTNAME'
        Position.BeginsLayer = True
      end
      object cvUserDefinedSECONDNAME: TcxGridDBCardViewRow
        DataBinding.FieldName = 'SECONDNAME'
        Position.BeginsLayer = True
      end
      object cvUserDefinedGENDER: TcxGridDBCardViewRow
        DataBinding.FieldName = 'GENDER'
        RepositoryItem = StylesCardViewDemoMainDM.edrepGender
        Position.BeginsLayer = True
      end
      object cvUserDefinedBIRTHNAME: TcxGridDBCardViewRow
        DataBinding.FieldName = 'BIRTHNAME'
        Position.BeginsLayer = True
      end
      object cvUserDefinedDATEOFBIRTH: TcxGridDBCardViewRow
        DataBinding.FieldName = 'DATEOFBIRTH'
        Position.BeginsLayer = True
      end
      object cvUserDefinedBIRTHCOUNTRY: TcxGridDBCardViewRow
        DataBinding.FieldName = 'BIRTHCOUNTRY'
        RepositoryItem = StylesCardViewDemoMainDM.edrepCountry
        Position.BeginsLayer = True
      end
      object cvUserDefinedLOCATIONOFBIRTH: TcxGridDBCardViewRow
        DataBinding.FieldName = 'LOCATIONOFBIRTH'
        Position.BeginsLayer = True
      end
      object cvUserDefinedBIOGRAPHY: TcxGridDBCardViewRow
        DataBinding.FieldName = 'BIOGRAPHY'
        Options.Filtering = False
        Position.BeginsLayer = True
      end
      object cvUserDefinedNICKNAME: TcxGridDBCardViewRow
        DataBinding.FieldName = 'NICKNAME'
        Position.BeginsLayer = True
      end
    end
    object lvDevExpress: TcxGridLevel
      Caption = 'DevExpress'
      GridView = cvDevExpress
    end
    object lvSlate: TcxGridLevel
      Caption = 'Slate'
      GridView = cvSlate
    end
    object lvHighContrast: TcxGridLevel
      Caption = 'High Contrast'
      GridView = cvHighContrast
    end
    object lvUserDefined: TcxGridLevel
      Caption = 'User Defined'
      GridView = cvUserDefined
    end
  end
  object pnlLeft: TPanel [3]
    Left = 0
    Top = 32
    Width = 177
    Height = 521
    Align = alLeft
    BevelOuter = bvNone
    Color = 15451300
    TabOrder = 2
    object GroupBox1: TGroupBox
      Left = 0
      Top = 0
      Width = 177
      Height = 105
      Align = alTop
      Caption = 'Options View'
      TabOrder = 0
      object Label1: TLabel
        Left = 8
        Top = 27
        Width = 53
        Height = 13
        Caption = 'Card Width'
      end
      object Label2: TLabel
        Left = 8
        Top = 51
        Width = 87
        Height = 13
        Caption = 'Card Border Width'
      end
      object spedCardWidth: TcxSpinEdit
        Left = 112
        Top = 23
        Properties.MaxValue = 1000.000000000000000000
        Properties.OnChange = spedCardWidthPropertiesChange
        Style.Color = 16247513
        TabOrder = 0
        Value = 300
        OnKeyPress = spedCardWidthKeyPress
        Width = 48
      end
      object spedCardBorderWidth: TcxSpinEdit
        Left = 112
        Top = 47
        Properties.MaxValue = 100.000000000000000000
        Properties.OnChange = cxSpinEdit2PropertiesChange
        Style.Color = 16247513
        Style.Shadow = False
        TabOrder = 1
        Value = 2
        OnKeyPress = spedCardWidthKeyPress
        Width = 48
      end
      object cbCellAutoHeight: TcxCheckBox
        Left = 10
        Top = 74
        AutoSize = False
        Caption = 'Cell Auto Height'
        ParentColor = False
        Properties.OnChange = cbSellAutoHeightPropertiesChange
        State = cbsChecked
        Style.Color = 15451300
        TabOrder = 2
        Height = 21
        Width = 143
      end
    end
    object Panel2: TPanel
      Left = 0
      Top = 477
      Width = 177
      Height = 44
      Align = alBottom
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 1
      object btnEdit: TcxButton
        Left = 8
        Top = 8
        Width = 161
        Height = 25
        Caption = 'Edit User Defined StyleSheet...'
        Enabled = False
        TabOrder = 0
        OnClick = btnEditClick
      end
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
end
