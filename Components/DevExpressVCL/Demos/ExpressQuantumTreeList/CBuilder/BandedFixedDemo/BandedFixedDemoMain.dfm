inherited BandedFixedDemoMainForm: TBandedFixedDemoMainForm
  Left = 195
  Top = 130
  Caption = 'ExpressQuantumTreeList BandedFixedDemo'
  ClientHeight = 473
  ClientWidth = 681
  OldCreateOrder = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited lscrip: TLabel
    Width = 681
    Height = 33
    AutoSize = False
    Caption = 
      'Fix columns on the left and right using bands. Experiment using ' +
      'the Options available and see Help/About for other things to try' +
      '.'
  end
  inherited sbMain: TStatusBar
    Top = 454
    Width = 681
  end
  object cxDBTreeList: TcxDBTreeList [2]
    Left = 0
    Top = 33
    Width = 681
    Height = 421
    Align = alClient
    Bands = <
      item
        Caption.AlignHorz = taCenter
        Caption.Text = 'Employee'
        FixedKind = tlbfLeft
        Width = 261
      end
      item
        Caption.Text = 'Days'
        Width = 676
      end
      item
        Caption.AlignHorz = taCenter
        Caption.Text = 'Row Total'
        FixedKind = tlbfRight
        Width = 102
      end>
    DataController.DataSource = BandedFixedDemoDataDM.dsSheduler
    DataController.ParentField = 'ProjectManagerID'
    DataController.KeyField = 'USERID'
    OptionsData.Deleting = False
    OptionsView.Bands = True
    OptionsView.Footer = True
    OptionsView.GridLineColor = 14916958
    OptionsView.GridLines = tlglBoth
    OptionsView.Indicator = True
    OptionsView.PaintStyle = tlpsCategorized
    OptionsView.UseNodeColorForIndent = False
    PopupMenu = mnuNodeOptions
    PopupMenus.ColumnHeaderMenu.UseBuiltInMenu = True
    PopupMenus.FooterMenu.UseBuiltInMenu = True
    Preview.Place = tlppTop
    Preview.Visible = True
    RootValue = -1
    Styles.OnGetContentStyle = cxDBTreeListStylesGetContentStyle
    Styles.StyleSheet = BandedFixedDemoDataDM.TreeListStyleSheetDevExpress
    TabOrder = 1
    object cxDBTreeListID: TcxDBTreeListColumn
      Visible = False
      DataBinding.FieldName = 'ID'
      Options.Editing = False
      Position.ColIndex = -1
      Position.RowIndex = -1
      Position.BandIndex = -1
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListPROJECTID: TcxDBTreeListColumn
      Visible = False
      DataBinding.FieldName = 'PROJECTID'
      Options.Editing = False
      Position.ColIndex = -1
      Position.RowIndex = -1
      Position.BandIndex = -1
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListProjectManagerID: TcxDBTreeListColumn
      Visible = False
      DataBinding.FieldName = 'ProjectManagerID'
      Options.Editing = False
      Position.ColIndex = -1
      Position.RowIndex = -1
      Position.BandIndex = -1
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListEmployee: TcxDBTreeListColumn
      PropertiesClassName = 'TcxLookupComboBoxProperties'
      Properties.KeyFieldNames = 'ID'
      Properties.ListColumns = <
        item
          FieldName = 'Name'
        end>
      Properties.ListSource = BandedFixedDemoDataDM.dsPersons
      Caption.Text = 'EMPLOYEE'
      DataBinding.FieldName = 'USERID'
      Options.Editing = False
      Width = 156
      Position.ColIndex = 0
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
      OnGetDisplayText = cxDBTreeListEmployeeGetDisplayText
      OnGetEditProperties = cxDBTreeListEmployeeGetEditProperties
    end
    object cxDBTreeListEmployeePhone: TcxDBTreeListColumn
      PropertiesClassName = 'TcxLookupComboBoxProperties'
      Properties.KeyFieldNames = 'ID'
      Properties.ListColumns = <
        item
          FieldName = 'Phone'
        end>
      Properties.ListSource = BandedFixedDemoDataDM.dsPersons
      Caption.Text = 'PHONE'
      DataBinding.FieldName = 'USERID'
      Width = 105
      Position.ColIndex = 1
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListEmployeeEmail: TcxDBTreeListColumn
      PropertiesClassName = 'TcxLookupComboBoxProperties'
      Properties.KeyFieldNames = 'ID'
      Properties.ListColumns = <
        item
          FieldName = 'EMAIL'
        end>
      Properties.ListSource = BandedFixedDemoDataDM.dsPersons
      Visible = False
      Caption.Text = 'EMAIL'
      DataBinding.FieldName = 'USERID'
      Width = 267
      Position.ColIndex = 2
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListSUNDAY: TcxDBTreeListColumn
      RepositoryItem = cxEditRepositoryCalcItem
      DataBinding.FieldName = 'SUNDAY'
      Width = 88
      Position.ColIndex = 0
      Position.RowIndex = 0
      Position.BandIndex = 1
      Summary.FooterSummaryItems = <
        item
          Kind = skSum
        end>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListMONDAY: TcxDBTreeListColumn
      RepositoryItem = cxEditRepositoryCalcItem
      DataBinding.FieldName = 'MONDAY'
      Width = 88
      Position.ColIndex = 1
      Position.RowIndex = 0
      Position.BandIndex = 1
      Summary.FooterSummaryItems = <
        item
          Kind = skSum
        end>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListTUESDAY: TcxDBTreeListColumn
      RepositoryItem = cxEditRepositoryCalcItem
      DataBinding.FieldName = 'TUESDAY'
      Width = 88
      Position.ColIndex = 2
      Position.RowIndex = 0
      Position.BandIndex = 1
      Summary.FooterSummaryItems = <
        item
          Kind = skSum
        end>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListWEDNESDAY: TcxDBTreeListColumn
      RepositoryItem = cxEditRepositoryCalcItem
      DataBinding.FieldName = 'WEDNESDAY'
      Width = 85
      Position.ColIndex = 3
      Position.RowIndex = 0
      Position.BandIndex = 1
      Summary.FooterSummaryItems = <
        item
          Kind = skSum
        end>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListTHURSDAY: TcxDBTreeListColumn
      RepositoryItem = cxEditRepositoryCalcItem
      DataBinding.FieldName = 'THURSDAY'
      Width = 119
      Position.ColIndex = 4
      Position.RowIndex = 0
      Position.BandIndex = 1
      Summary.FooterSummaryItems = <
        item
          Kind = skSum
        end>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListFRIDAY: TcxDBTreeListColumn
      RepositoryItem = cxEditRepositoryCalcItem
      DataBinding.FieldName = 'FRIDAY'
      Width = 95
      Position.ColIndex = 5
      Position.RowIndex = 0
      Position.BandIndex = 1
      Summary.FooterSummaryItems = <
        item
          Kind = skSum
        end>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListSATURDAY: TcxDBTreeListColumn
      RepositoryItem = cxEditRepositoryCalcItem
      DataBinding.FieldName = 'SATURDAY'
      Width = 113
      Position.ColIndex = 6
      Position.RowIndex = 0
      Position.BandIndex = 1
      Summary.FooterSummaryItems = <
        item
          Kind = skSum
        end>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListWeekSum: TcxDBTreeListColumn
      Caption.Text = 'SUM'
      DataBinding.FieldName = 'WeekSum'
      Options.Editing = False
      Width = 52
      Position.ColIndex = 0
      Position.RowIndex = 0
      Position.BandIndex = 2
      Summary.FooterSummaryItems = <
        item
          Kind = skSum
        end>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListWeekAVG: TcxDBTreeListColumn
      Caption.Text = 'AVG'
      DataBinding.FieldName = 'WeekAVG'
      Options.Editing = False
      Width = 50
      Position.ColIndex = 1
      Position.RowIndex = 0
      Position.BandIndex = 2
      Summary.FooterSummaryItems = <
        item
          Kind = skSum
        end>
      Summary.GroupFooterSummaryItems = <>
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
      object miShowBands: TMenuItem [4]
        Caption = 'Show &Bands'
        Checked = True
        Hint = 'Shows bands within a tree list control'
        OnClick = miShowBandsClick
      end
      object miShowHeaders: TMenuItem [5]
        Caption = 'Show &Headers'
        Checked = True
        Hint = 'Shows column headers within a tree list control'
        OnClick = miShowHeadersClick
      end
      object miColumnCustomization: TMenuItem [6]
        Action = actCustomizationForm
      end
      object miSeparator1: TMenuItem [7]
        Caption = '-'
      end
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
  object mnuNodeOptions: TPopupMenu
    OnPopup = mnuNodeOptionsPopup
    Left = 88
    Top = 8
    object miFixBand: TMenuItem
      Caption = '&Fix Band'
      object miFixBandNone: TMenuItem
        Caption = '&None'
        Hint = 'Makes the band unfixed'
        RadioItem = True
        OnClick = miFixBandClick
      end
      object miFixBandLeft: TMenuItem
        Tag = 1
        Caption = '&Left'
        Hint = 'Fixes the band to the left of the TreeList '
        RadioItem = True
        OnClick = miFixBandClick
      end
      object miFixBandRight: TMenuItem
        Tag = 2
        Caption = '&Right'
        Hint = 'Fixes the band to the right of the TreeList '
        RadioItem = True
        OnClick = miFixBandClick
      end
    end
    object miBandHide: TMenuItem
      Caption = '&Hide Band'
      Hint = 'Hides a selected band'
      OnClick = miBandHideClick
    end
    object CustomisationForm1: TMenuItem
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
    object cxEditRepository1TextItem1: TcxEditRepositoryTextItem
      Properties.Alignment.Horz = taLeftJustify
      Properties.Alignment.Vert = taVCenter
    end
    object cxEditRepositoryCalcItem: TcxEditRepositoryCalcItem
    end
  end
end
