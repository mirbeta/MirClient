inherited EditorsInPlaceDemoMainForm: TEditorsInPlaceDemoMainForm
  Left = 99
  Top = 55
  Caption = 'ExpressQuantumTreeList EditorsInPlaceDemo'
  ClientHeight = 534
  ClientWidth = 867
  OldCreateOrder = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited lscrip: TLabel
    Width = 867
    AutoSize = False
    Caption = 
      'This demo shows various column editors. See Help/About this demo' +
      ' for more information'
  end
  inherited sbMain: TStatusBar
    Top = 515
    Width = 867
  end
  object tlOrders: TcxDBTreeList [2]
    Left = 0
    Top = 32
    Width = 867
    Height = 483
    Align = alClient
    Bands = <
      item
        Caption.Text = 'Company'
        Width = 300
      end
      item
        Caption.Text = 'Car'
        Width = 289
      end
      item
        Caption.Text = 'Purchase Info'
        Width = 456
      end>
    DataController.DataSource = EditorsInPlaceDemoDataDM.dsOrders
    DataController.ParentField = 'ID'
    DataController.KeyField = 'ID'
    Navigator.Buttons.CustomButtons = <>
    OptionsData.SmartRefresh = True
    OptionsView.Bands = True
    OptionsView.GridLines = tlglBoth
    OptionsView.Indicator = True
    OptionsView.ShowRoot = False
    PopupMenus.ColumnHeaderMenu.UseBuiltInMenu = True
    RootValue = 255
    Styles.StyleSheet = EditorsInPlaceDemoDataDM.TreeListStyleSheetDevExpress
    TabOrder = 1
    object tlOrdersCustomerID: TcxDBTreeListColumn
      PropertiesClassName = 'TcxLookupComboBoxProperties'
      Properties.ImmediatePost = True
      Properties.KeyFieldNames = 'ID'
      Properties.ListColumns = <
        item
          FieldName = 'Company'
        end>
      Properties.ListOptions.GridLines = glNone
      Properties.ListSource = EditorsInPlaceDemoDataDM.dsCustomers
      Caption.Text = 'Company'
      DataBinding.FieldName = 'CustomerID'
      Width = 126
      Position.ColIndex = 0
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object tlOrdersCompanyEmail: TcxDBTreeListColumn
      PropertiesClassName = 'TcxButtonEditProperties'
      Properties.Buttons = <
        item
          Default = True
          Kind = bkEllipsis
        end>
      Properties.OnButtonClick = tlOrdersCompanyEmailPropertiesButtonClick
      Caption.Text = 'Company Email'
      DataBinding.FieldName = 'CustomerEmail'
      Width = 174
      Position.ColIndex = 1
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object tlOrdersProductID: TcxDBTreeListColumn
      PropertiesClassName = 'TcxLookupComboBoxProperties'
      Properties.KeyFieldNames = 'ID'
      Properties.ListColumns = <
        item
          FieldName = 'FullName'
        end>
      Properties.ListSource = dmCars.dsModels
      Caption.Text = 'Car'
      DataBinding.FieldName = 'ProductID'
      Width = 227
      Position.ColIndex = 0
      Position.RowIndex = 0
      Position.BandIndex = 1
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object tlOrdersCarInfo: TcxDBTreeListColumn
      PropertiesClassName = 'TcxPopupEditProperties'
      Properties.PopupControl = EditorsInPlaceDemoCarInfoForm.pnlCarInfo
      Properties.PopupSysPanelStyle = True
      Properties.OnCloseUp = tlOrdersCarInfoPropertiesCloseUp
      Properties.OnInitPopup = tlOrdersCarInfoPropertiesInitPopup
      Caption.Text = 'CarInfo'
      Width = 62
      Position.ColIndex = 1
      Position.RowIndex = 0
      Position.BandIndex = 1
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
      OnGetDisplayText = tlOrdersCarInfoGetDisplayText
    end
    object tlOrdersPurchaseDate: TcxDBTreeListColumn
      DataBinding.FieldName = 'PurchaseDate'
      Width = 93
      Position.ColIndex = 0
      Position.RowIndex = 0
      Position.BandIndex = 2
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object tlOrdersTime: TcxDBTreeListColumn
      PropertiesClassName = 'TcxTimeEditProperties'
      DataBinding.FieldName = 'Time'
      Width = 82
      Position.ColIndex = 1
      Position.RowIndex = 0
      Position.BandIndex = 2
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object tlOrdersPaymentType: TcxDBTreeListColumn
      PropertiesClassName = 'TcxImageComboBoxProperties'
      Properties.Images = EditorsInPlaceDemoDataDM.PaymentTypeImages
      Properties.Items = <
        item
          Description = 'American Express'
          ImageIndex = 3
          Value = 'AmEx'
        end
        item
          Description = 'Master Card'
          ImageIndex = 2
          Value = 'Master'
        end
        item
          Description = 'Visa Card'
          ImageIndex = 1
          Value = 'Visa'
        end
        item
          Description = 'Cash'
          ImageIndex = 0
          Value = 'Cash'
        end>
      DataBinding.FieldName = 'PaymentType'
      Width = 119
      Position.ColIndex = 2
      Position.RowIndex = 0
      Position.BandIndex = 2
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object tlOrdersPaymentAmount: TcxDBTreeListColumn
      PropertiesClassName = 'TcxCalcEditProperties'
      DataBinding.FieldName = 'PaymentAmount'
      Width = 102
      Position.ColIndex = 3
      Position.RowIndex = 0
      Position.BandIndex = 2
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object tlOrdersQuantity: TcxDBTreeListColumn
      PropertiesClassName = 'TcxSpinEditProperties'
      DataBinding.FieldName = 'Quantity'
      Width = 60
      Position.ColIndex = 4
      Position.RowIndex = 0
      Position.BandIndex = 2
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
  end
  inherited mmMain: TMainMenu
    inherited miOptions: TMenuItem
      object miShowEditButtons: TMenuItem [0]
        Caption = 'Show &Edit Buttons'
        object miEditBtnsAlways: TMenuItem
          Caption = '&Always'
          Hint = 
            'If checked, the editor buttons are always visible within the Tre' +
            'eList '
          RadioItem = True
          OnClick = miShowEditBtnsClick
        end
        object miEditBtnsFocused: TMenuItem
          Tag = 1
          Caption = '&Focused node only'
          Hint = 'If checked, the editor buttons are visible for the focused node'
          RadioItem = True
          OnClick = miShowEditBtnsClick
        end
        object miEditBtnsNever: TMenuItem
          Tag = 2
          Caption = '&Never'
          Checked = True
          Hint = 
            'If checked, the editor buttons are displayed only for the focuse' +
            'd cell'
          RadioItem = True
          OnClick = miShowEditBtnsClick
        end
      end
      object N1: TMenuItem [1]
        Caption = '-'
      end
    end
  end
end
