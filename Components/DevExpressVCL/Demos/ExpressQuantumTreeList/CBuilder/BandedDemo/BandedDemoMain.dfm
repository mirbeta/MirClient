inherited BandedDemoMainForm: TBandedDemoMainForm
  Left = 88
  Top = 38
  Caption = 'ExpressQuantumTreeList BandedDemo'
  ClientHeight = 567
  ClientWidth = 883
  OldCreateOrder = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited lscrip: TLabel
    Width = 883
    Height = 25
    AutoSize = False
    Caption = 
      'Create and remove bands at runtime. Experiment using the Options' +
      ' available and see Help/About for other things to try.'
  end
  inherited sbMain: TStatusBar
    Top = 548
    Width = 883
  end
  object cxDBTreeList: TcxDBTreeList [2]
    Left = 0
    Top = 25
    Width = 883
    Height = 523
    Align = alClient
    Bands = <
      item
        Caption.AlignHorz = taCenter
        Caption.Text = 'Common department info'
        Width = 331
      end
      item
        Caption.AlignHorz = taCenter
        Caption.Text = 'Detailed department info'
        Width = 310
      end
      item
        Caption.AlignHorz = taCenter
        Caption.Text = 'Manager info'
        Width = 445
      end>
    DataController.DataSource = BandedDemoDataDM.dsDepartments
    DataController.ParentField = 'PARENTID'
    DataController.KeyField = 'ID'
    DragMode = dmAutomatic
    OptionsCustomizing.BandsQuickCustomization = True
    OptionsCustomizing.ColumnsQuickCustomization = True
    OptionsData.Inserting = True
    OptionsView.Bands = True
    OptionsView.GridLineColor = 14916958
    OptionsView.GridLines = tlglBoth
    OptionsView.Indicator = True
    OptionsView.UseNodeColorForIndent = False
    PopupMenu = mnuNodeOptions
    PopupMenus.ColumnHeaderMenu.UseBuiltInMenu = True
    Preview.Place = tlppTop
    Preview.Visible = True
    RootValue = -1
    Styles.StyleSheet = BandedDemoDataDM.TreeListStyleSheetDevExpress
    TabOrder = 1
    OnInitInsertingRecord = cxDBTreeListInitInsertingRecord
    object cxDBTreeListID: TcxDBTreeListColumn
      Visible = False
      DataBinding.FieldName = 'ID'
      Width = 63
      Position.ColIndex = 2
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListPARENTID: TcxDBTreeListColumn
      Visible = False
      DataBinding.FieldName = 'PARENTID'
      Width = 62
      Position.ColIndex = 3
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListNAME: TcxDBTreeListColumn
      DataBinding.FieldName = 'NAME'
      Width = 274
      Position.ColIndex = 0
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListPHONE: TcxDBTreeListColumn
      RepositoryItem = eriTelephoneMaskEdit
      DataBinding.FieldName = 'PHONE'
      Width = 160
      Position.ColIndex = 1
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListFAX: TcxDBTreeListColumn
      RepositoryItem = eriTelephoneMaskEdit
      DataBinding.FieldName = 'FAX'
      Width = 90
      Position.ColIndex = 0
      Position.RowIndex = 0
      Position.BandIndex = 1
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListBUDGET: TcxDBTreeListColumn
      DataBinding.FieldName = 'BUDGET'
      Position.ColIndex = 2
      Position.RowIndex = 0
      Position.BandIndex = 1
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListVACANCY: TcxDBTreeListColumn
      DataBinding.FieldName = 'VACANCY'
      Width = 120
      Position.ColIndex = 1
      Position.RowIndex = 0
      Position.BandIndex = 1
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListManager: TcxDBTreeListColumn
      PropertiesClassName = 'TcxLookupComboBoxProperties'
      Properties.ImmediatePost = True
      Properties.KeyFieldNames = 'ID'
      Properties.ListColumns = <
        item
          FieldName = 'Name'
        end>
      Properties.ListSource = BandedDemoDataDM.dsPersons
      Caption.Text = 'Manager'
      DataBinding.FieldName = 'MANAGERID'
      Width = 117
      Position.ColIndex = 0
      Position.RowIndex = 0
      Position.BandIndex = 2
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListManagerPhone: TcxDBTreeListColumn
      PropertiesClassName = 'TcxLookupComboBoxProperties'
      Properties.KeyFieldNames = 'ID'
      Properties.ListColumns = <
        item
          FieldName = 'Phone'
        end>
      Properties.ListSource = BandedDemoDataDM.dsPersons
      Properties.ReadOnly = False
      Caption.Text = 'Manager Phone'
      DataBinding.FieldName = 'MANAGERID'
      Width = 121
      Position.ColIndex = 1
      Position.RowIndex = 0
      Position.BandIndex = 2
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListManagerEmail: TcxDBTreeListColumn
      PropertiesClassName = 'TcxLookupComboBoxProperties'
      Properties.KeyFieldNames = 'ID'
      Properties.ListColumns = <
        item
          FieldName = 'EMAIL'
        end>
      Properties.ListSource = BandedDemoDataDM.dsPersons
      Properties.ReadOnly = False
      Caption.Text = 'Manager Email'
      DataBinding.FieldName = 'MANAGERID'
      Width = 207
      Position.ColIndex = 2
      Position.RowIndex = 0
      Position.BandIndex = 2
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
  end
  inherited alMain: TActionList
    object actCustomizationForm: TAction
      Category = 'Options'
      Caption = '&Customization Form ...'
      Hint = 'Shows the column customization form'
      OnExecute = actCustomizationFormExecute
    end
  end
  inherited mmMain: TMainMenu
    inherited miOptions: TMenuItem
      object miBandHorzSizing: TMenuItem [0]
        Caption = 'Band Horz &Sizing'
        Checked = True
        Hint = 'Enables horizontal band sizing'
        OnClick = miBandHorzSizingClick
      end
      object miBandVertSizing: TMenuItem [1]
        Caption = 'Band &Vert Sizing'
        Checked = True
        Hint = 'Enables vertical band sizing'
        OnClick = miBandVertSizingClick
      end
      object miBandMoving: TMenuItem [2]
        Caption = 'Band &Moving'
        Checked = True
        Hint = 'Enables band moving'
        OnClick = miBandMovingClick
      end
      object N2: TMenuItem [3]
        Caption = '-'
      end
      object miAddBand: TMenuItem [4]
        Caption = '&Create Band ...'
        Hint = 'Click to create a new band'
        OnClick = miAddBandClick
      end
      object miRemoveBands: TMenuItem [5]
        Caption = '&Delete Band ...'
        Hint = 'Click to remove an existing band'
        OnClick = miRemoveBandsClick
      end
      object N1: TMenuItem [6]
        Caption = '-'
      end
      object miColumnCustomization: TMenuItem [7]
        Action = actCustomizationForm
      end
      object miSeparator1: TMenuItem [8]
        Caption = '-'
      end
    end
  end
  object mnuNodeOptions: TPopupMenu
    OnPopup = mnuNodeOptionsPopup
    Left = 88
    Top = 8
    object miBandDelete: TMenuItem
      Caption = '&Delete Band'
      Hint = 'Deletes selected Band'
      OnClick = miBandDeleteClick
    end
    object miBandHide: TMenuItem
      Caption = '&Hide Band'
      Hint = 'Hides selected Band'
      OnClick = miBandHideClick
    end
    object miCustomisationForm: TMenuItem
      Action = actCustomizationForm
    end
  end
  object cxEditRepository1: TcxEditRepository
    Left = 432
    Top = 48
    object eriTelephoneMaskEdit: TcxEditRepositoryMaskItem
      Properties.MaskKind = emkRegExprEx
      Properties.EditMask = '(\((\d\d\d)?\))? \d(\d\d?)? - \d\d(\d\d)?( - \d\d)?'
    end
  end
end
