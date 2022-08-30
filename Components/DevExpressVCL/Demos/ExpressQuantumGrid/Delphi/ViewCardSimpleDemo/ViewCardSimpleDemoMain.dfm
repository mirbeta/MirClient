inherited ViewCardSimpleDemoMainForm: TViewCardSimpleDemoMainForm
  Left = 200
  Top = 132
  Caption = 'ExpressQuantumGrid ViewCardSimple Demo'
  ClientHeight = 487
  ClientWidth = 806
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 806
    Caption = 
      'Use the new Card View. Experiment by changing the Options above.' +
      ' Click '#39'About this demo'#39' for more information.'
  end
  object Splitter: TSplitter [1]
    Left = 185
    Top = 16
    Width = 2
    Height = 471
  end
  object pnDepartments: TPanel [2]
    Left = 0
    Top = 16
    Width = 185
    Height = 471
    Align = alLeft
    Caption = 'pnDepartments'
    TabOrder = 1
    object pnDepartmentsCaption: TPanel
      Left = 1
      Top = 1
      Width = 183
      Height = 32
      Align = alTop
      BevelInner = bvLowered
      BevelOuter = bvLowered
      Caption = 'Departments'
      Color = 4707838
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
    end
    object cxGridDepartments: TcxGrid
      Left = 1
      Top = 33
      Width = 183
      Height = 437
      Align = alClient
      TabOrder = 1
      object cxGridDepartmentsDBTableView: TcxGridDBTableView
        DataController.DataSource = ViewCardSimpleDemoMainDM.dsDEPARTMENTS
        DataController.Summary.DefaultGroupSummaryItems = <>
        DataController.Summary.FooterSummaryItems = <>
        DataController.Summary.SummaryGroups = <>
        OptionsCustomize.ColumnFiltering = False
        OptionsCustomize.ColumnGrouping = False
        OptionsData.Deleting = False
        OptionsData.Editing = False
        OptionsData.Inserting = False
        OptionsSelection.InvertSelect = False
        OptionsView.ColumnAutoWidth = True
        OptionsView.GroupByBox = False
        object cxGridDepartmentsDBTableViewNAME: TcxGridDBColumn
          Caption = 'Name'
          DataBinding.FieldName = 'NAME'
          Options.Editing = False
          Options.Filtering = False
        end
      end
      object cxGridDepartmentsLevel: TcxGridLevel
        GridView = cxGridDepartmentsDBTableView
      end
    end
  end
  object pnUsers: TPanel [3]
    Left = 187
    Top = 16
    Width = 619
    Height = 471
    Align = alClient
    Caption = 'pnUsers'
    TabOrder = 0
    object cxGridUsers: TcxGrid
      Left = 1
      Top = 33
      Width = 617
      Height = 437
      Align = alClient
      TabOrder = 0
      object cxGridUsersDBCardView: TcxGridDBCardView
        Navigator.Visible = True
        DataController.DataSource = ViewCardSimpleDemoMainDM.dsUSERS
        DataController.Summary.DefaultGroupSummaryItems = <>
        DataController.Summary.FooterSummaryItems = <>
        DataController.Summary.SummaryGroups = <>
        OptionsView.CardIndent = 7
        OptionsView.SeparatorColor = 12937777
        object cxGridUsersDBCardViewFNAME: TcxGridDBCardViewRow
          Caption = 'First Name'
          DataBinding.FieldName = 'FNAME'
          Kind = rkCaption
          Position.BeginsLayer = True
        end
        object cxGridUsersDBCardViewMNAME: TcxGridDBCardViewRow
          Caption = 'Middle Name'
          DataBinding.FieldName = 'MNAME'
          Position.BeginsLayer = True
        end
        object cxGridUsersDBCardViewLNAME: TcxGridDBCardViewRow
          Caption = 'Last Name'
          DataBinding.FieldName = 'LNAME'
          Position.BeginsLayer = True
        end
        object cxGridUsersDBCardViewCOUNTRY: TcxGridDBCardViewRow
          Caption = 'Country'
          DataBinding.FieldName = 'COUNTRY'
          Position.BeginsLayer = True
        end
        object cxGridUsersDBCardViewPOSTALCODE: TcxGridDBCardViewRow
          Caption = 'Postalcode'
          DataBinding.FieldName = 'POSTALCODE'
          PropertiesClassName = 'TcxMaskEditProperties'
          Properties.MaskKind = emkRegExpr
          Properties.EditMask = '\d\d\d\d\d? | \w\w\w'#39' '#39'\w\w\w'
          Position.BeginsLayer = True
        end
        object cxGridUsersDBCardViewCITY: TcxGridDBCardViewRow
          Caption = 'City'
          DataBinding.FieldName = 'CITY'
          Position.BeginsLayer = True
        end
        object cxGridUsersDBCardViewADDRESS: TcxGridDBCardViewRow
          Caption = 'Address'
          DataBinding.FieldName = 'ADDRESS'
          Position.BeginsLayer = True
        end
        object cxGridUsersDBCardViewPHONE: TcxGridDBCardViewRow
          Caption = 'Phone'
          DataBinding.FieldName = 'PHONE'
          PropertiesClassName = 'TcxMaskEditProperties'
          Properties.EditMask = '!\(999\) 000-0000;1;_'
          Position.BeginsLayer = True
        end
        object cxGridUsersDBCardViewFAX: TcxGridDBCardViewRow
          Caption = 'Fax'
          DataBinding.FieldName = 'FAX'
          PropertiesClassName = 'TcxMaskEditProperties'
          Properties.MaskKind = emkRegExprEx
          Properties.EditMask = '(\(\d\d\d\)'#39' '#39')?\d\d\d-\d\d\d\d'
          Position.BeginsLayer = True
        end
        object cxGridUsersDBCardViewEMAIL: TcxGridDBCardViewRow
          Caption = 'Email'
          DataBinding.FieldName = 'EMAIL'
          PropertiesClassName = 'TcxMaskEditProperties'
          Properties.MaskKind = emkRegExprEx
          Properties.EditMask = '\w+@\w+\.\w+(\.\w+)*'
          Position.BeginsLayer = True
        end
        object cxGridUsersDBCardViewHOMEPAGE: TcxGridDBCardViewRow
          Caption = 'Homepage'
          DataBinding.FieldName = 'HOMEPAGE'
          PropertiesClassName = 'TcxHyperLinkEditProperties'
          Position.BeginsLayer = True
        end
      end
      object cxGridUsersLevel: TcxGridLevel
        GridView = cxGridUsersDBCardView
      end
    end
    object pnUsersCaption: TPanel
      Left = 1
      Top = 1
      Width = 617
      Height = 32
      Align = alTop
      BevelInner = bvLowered
      BevelOuter = bvLowered
      Caption = 'Users'
      Color = 4707838
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
    end
  end
  inherited mmMain: TMainMenu
    object miOptions: TMenuItem [1]
      Caption = '&Options'
      object miInvertSelect: TMenuItem
        AutoCheck = True
        Caption = 'Invert Select'
        Checked = True
        Hint = 
          'Determines whether the selection mark highlights a single cell w' +
          'ithin a focused row or the whole focused row'
        OnClick = miInvertSelectClick
      end
      object miCellSelect: TMenuItem
        AutoCheck = True
        Caption = 'CellSelect'
        Checked = True
        Hint = 
          'Determines whether the individual cells are selected within a vi' +
          'ew instead of the whole rows'
        OnClick = miCellSelectClick
      end
      object miMultiSelect: TMenuItem
        AutoCheck = True
        Caption = 'MultiSelect'
        Hint = 'Allows you to select several rows'
        OnClick = miMultiSelectClick
      end
      object miHideFocusRect: TMenuItem
        AutoCheck = True
        Caption = 'Hide Focus Rect'
        Checked = True
        Hint = 
          'Determines whether the focus rectangle is displayed around the f' +
          'ocused record within a view'
        OnClick = miHideFocusRectClick
      end
      object miShowNavigator: TMenuItem
        AutoCheck = True
        Caption = 'Show &Navigator'
        Checked = True
        Hint = 'Displays the inplace navigator'
        OnClick = miShowNavigatorClick
      end
    end
  end
  inherited StyleRepository: TcxStyleRepository
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
  end
end
