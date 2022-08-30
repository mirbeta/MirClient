inherited EditorsInPlaceDemoMainForm: TEditorsInPlaceDemoMainForm
  Left = 185
  Top = 126
  Caption = 'ExpressVerticalGrid EditorsInPlace Demo'
  ClientHeight = 495
  ClientWidth = 800
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescrip: TLabel
    Width = 800
    Height = 25
    AutoSize = False
    Caption = 
      'This demo shows various column editors. See Help/About for more ' +
      'information'
  end
  inherited sbMain: TStatusBar
    Top = 476
    Width = 800
  end
  inherited memAboutText: TMemo
    Left = 624
    Top = 232
    Lines.Strings = (
      'In this demo you can:'
      ''
      
        '- keep this window open while you experiment with the various ed' +
        'itors'
      ''
      
        '- focus on the Company entry and type the first character of its' +
        ' name to see a filtered dropdown list.'
      ''
      
        '- focus on the Company Email entry and press the ellipsis button' +
        ' to send an email'
      ''
      
        '- focus on the Car entry and type the first character of its nam' +
        'e to see a filtered dropdown list.'
      ''
      '- display a calendar dropdown by editing a PurchaseDate'
      ''
      '- edit a Time via the time spin edit'
      ''
      '- display a calculator by editing a PaymentAmount'
      ''
      '- change the PaymentType via a dropdown image list'
      ''
      '- edit the Quantity via a spin edit'
      ''
      
        '- focus on the CarInfo column and click on its button to display' +
        ' details and:'
      ' (1) edit the Trademark via the MRU editor'
      ' (2) edit the Category via the Radiogroup editor'
      
        ' (3) display a resizable image by focusing & clicking on the ico' +
        'n below the fixed image'
      
        ' (4) visit the manufacturer'#39's website by double clicking on the ' +
        'URL')
  end
  object vgOrders: TcxDBVerticalGrid [3]
    Left = 0
    Top = 25
    Width = 800
    Height = 451
    Align = alClient
    LayoutStyle = lsMultiRecordView
    OptionsView.RowHeaderWidth = 144
    Navigator.Buttons.CustomButtons = <>
    Styles.StyleSheet = EditorsInPlaceDemoDataDM.cxVerticalGridStyleSheetDevExpress
    TabOrder = 2
    DataController.DataSource = EditorsInPlaceDemoDataDM.dsOrders
    Version = 1
    object vgOrdersCompany: TcxCategoryRow
      Properties.Caption = 'Company'
      ID = 0
      ParentID = -1
      Index = 0
      Version = 1
    end
    object vgOrdersCustomerID: TcxDBEditorRow
      Properties.Caption = 'Company'
      Properties.EditPropertiesClassName = 'TcxLookupComboBoxProperties'
      Properties.EditProperties.ImmediatePost = True
      Properties.EditProperties.KeyFieldNames = 'ID'
      Properties.EditProperties.ListColumns = <
        item
          FieldName = 'Company'
        end>
      Properties.EditProperties.ListSource = EditorsInPlaceDemoDataDM.dsCustomers
      Properties.EditProperties.MaxLength = 1
      Properties.DataBinding.FieldName = 'CustomerID'
      ID = 1
      ParentID = 0
      Index = 0
      Version = 1
    end
    object vgOrdersCustomerEmail: TcxDBEditorRow
      Properties.Caption = 'Company Email'
      Properties.EditPropertiesClassName = 'TcxButtonEditProperties'
      Properties.EditProperties.Buttons = <
        item
          Default = True
          Kind = bkEllipsis
        end>
      Properties.EditProperties.MaxLength = 30
      Properties.EditProperties.OnButtonClick = vgOrdersCompanyEmailPropertiesButtonClick
      Properties.DataBinding.FieldName = 'CustomerEmail'
      ID = 2
      ParentID = 0
      Index = 1
      Version = 1
    end
    object vgOrdersPurchaseInfo: TcxCategoryRow
      Properties.Caption = 'Purchase Info'
      ID = 3
      ParentID = -1
      Index = 1
      Version = 1
    end
    object vgOrdersPaymentType: TcxDBEditorRow
      Properties.EditPropertiesClassName = 'TcxImageComboBoxProperties'
      Properties.EditProperties.Images = EditorsInPlaceDemoDataDM.PaymentTypeImages
      Properties.EditProperties.Items = <
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
      Properties.DataBinding.FieldName = 'PaymentType'
      ID = 4
      ParentID = 3
      Index = 0
      Version = 1
    end
    object vgOrdersPaymentAmount: TcxDBEditorRow
      Properties.EditPropertiesClassName = 'TcxCalcEditProperties'
      Properties.DataBinding.FieldName = 'PaymentAmount'
      ID = 5
      ParentID = 3
      Index = 1
      Version = 1
    end
    object vgOrdersTime: TcxDBEditorRow
      Properties.EditPropertiesClassName = 'TcxTimeEditProperties'
      Properties.DataBinding.FieldName = 'Time'
      ID = 6
      ParentID = 3
      Index = 2
      Version = 1
    end
    object vgOrdersPurchaseDate: TcxDBEditorRow
      Properties.EditPropertiesClassName = 'TcxDateEditProperties'
      Properties.DataBinding.FieldName = 'PurchaseDate'
      ID = 7
      ParentID = 3
      Index = 3
      Version = 1
    end
    object vgOrdersQuantity: TcxDBEditorRow
      Properties.EditPropertiesClassName = 'TcxSpinEditProperties'
      Properties.DataBinding.FieldName = 'Quantity'
      ID = 8
      ParentID = 3
      Index = 4
      Version = 1
    end
    object vgOrdersCar: TcxCategoryRow
      Properties.Caption = 'Car'
      ID = 9
      ParentID = -1
      Index = 2
      Version = 1
    end
    object vgOrdersProductID: TcxDBEditorRow
      Properties.Caption = 'Car'
      Properties.EditPropertiesClassName = 'TcxLookupComboBoxProperties'
      Properties.EditProperties.KeyFieldNames = 'ID'
      Properties.EditProperties.ListColumns = <
        item
          FieldName = 'FullName'
        end>
      Properties.EditProperties.ListSource = dmCars.dsModels
      Properties.EditProperties.MaxLength = 100
      Properties.DataBinding.FieldName = 'ProductID'
      ID = 10
      ParentID = 9
      Index = 0
      Version = 1
    end
    object vgOrdersCarInfo: TcxDBEditorRow
      Properties.Caption = 'CarInfo'
      Properties.EditPropertiesClassName = 'TcxPopupEditProperties'
      Properties.EditProperties.MaxLength = 0
      Properties.EditProperties.PopupControl = EditorsInPlaceDemoCarInfoForm.pnlCarInfo
      Properties.EditProperties.PopupSysPanelStyle = True
      Properties.EditProperties.OnCloseUp = vgOrdersCarInfoEditPropertiesCloseUp
      Properties.EditProperties.OnInitPopup = vgOrdersCarInfoEditPropertiesInitPopup
      Properties.OnGetDisplayText = vgOrdersCarInfoPropertiesGetDisplayText
      ID = 11
      ParentID = 9
      Index = 1
      Version = 1
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
          Caption = '&Focused record only'
          Hint = 
            'If checked, the editor buttons are visible for the focused recor' +
            'd'
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
