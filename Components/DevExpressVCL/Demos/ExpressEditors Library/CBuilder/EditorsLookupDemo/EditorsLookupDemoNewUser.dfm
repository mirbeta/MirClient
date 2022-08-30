object EditorsLookupDemoNewUserForm: TEditorsLookupDemoNewUserForm
  Left = 327
  Top = 224
  ActiveControl = btnOK
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'New User'
  ClientHeight = 234
  ClientWidth = 434
  Color = 15451300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 53
    Height = 13
    Caption = 'First Name:'
  end
  object Label2: TLabel
    Left = 104
    Top = 8
    Width = 59
    Height = 13
    Caption = 'Midle Name:'
  end
  object Label3: TLabel
    Left = 200
    Top = 8
    Width = 54
    Height = 13
    Caption = 'Last Name:'
  end
  object Label4: TLabel
    Left = 8
    Top = 44
    Width = 39
    Height = 13
    Caption = 'Country:'
  end
  object Label5: TLabel
    Left = 168
    Top = 44
    Width = 20
    Height = 13
    Caption = 'City:'
  end
  object Label6: TLabel
    Left = 304
    Top = 44
    Width = 57
    Height = 13
    Caption = 'PostalCode:'
  end
  object Label7: TLabel
    Left = 8
    Top = 80
    Width = 41
    Height = 13
    Caption = 'Address:'
  end
  object Label8: TLabel
    Left = 8
    Top = 116
    Width = 34
    Height = 13
    Caption = 'Phone:'
  end
  object Label9: TLabel
    Left = 208
    Top = 116
    Width = 20
    Height = 13
    Caption = 'Fax:'
  end
  object Label10: TLabel
    Left = 8
    Top = 152
    Width = 28
    Height = 13
    Caption = 'Email:'
  end
  object Label11: TLabel
    Left = 208
    Top = 152
    Width = 59
    Height = 13
    Caption = 'Home Page:'
  end
  object Label12: TLabel
    Left = 296
    Top = 8
    Width = 58
    Height = 13
    Caption = 'Department:'
  end
  object edFirstName: TcxDBTextEdit
    Left = 8
    Top = 21
    Width = 90
    Height = 21
    DataBinding.DataField = 'FNAME'
    DataBinding.DataSource = EditorsLookupDemoDataDM.dsUsers
    Style.StyleController = EditorsLookupDemoDataDM.StyleController
    TabOrder = 0
  end
  object edMidleName: TcxDBTextEdit
    Left = 104
    Top = 21
    Width = 90
    Height = 21
    DataBinding.DataField = 'MNAME'
    DataBinding.DataSource = EditorsLookupDemoDataDM.dsUsers
    Style.StyleController = EditorsLookupDemoDataDM.StyleController
    TabOrder = 1
  end
  object edLastName: TcxDBTextEdit
    Left = 200
    Top = 21
    Width = 90
    Height = 21
    DataBinding.DataField = 'LNAME'
    DataBinding.DataSource = EditorsLookupDemoDataDM.dsUsers
    Style.StyleController = EditorsLookupDemoDataDM.StyleController
    TabOrder = 2
  end
  object edCountry: TcxDBTextEdit
    Left = 8
    Top = 57
    Width = 153
    Height = 21
    DataBinding.DataField = 'COUNTRY'
    DataBinding.DataSource = EditorsLookupDemoDataDM.dsUsers
    Style.StyleController = EditorsLookupDemoDataDM.StyleController
    TabOrder = 4
  end
  object edCity: TcxDBTextEdit
    Left = 168
    Top = 57
    Width = 129
    Height = 21
    DataBinding.DataField = 'CITY'
    DataBinding.DataSource = EditorsLookupDemoDataDM.dsUsers
    Style.StyleController = EditorsLookupDemoDataDM.StyleController
    TabOrder = 5
  end
  object mePostalCode: TcxDBMaskEdit
    Left = 304
    Top = 57
    Width = 121
    Height = 21
    DataBinding.DataField = 'POSTALCODE'
    DataBinding.DataSource = EditorsLookupDemoDataDM.dsUsers
    Properties.MaskKind = emkRegExprEx
    Properties.MaxLength = 0
    Style.StyleController = EditorsLookupDemoDataDM.StyleController
    TabOrder = 6
  end
  object edAddress: TcxDBTextEdit
    Left = 8
    Top = 93
    Width = 417
    Height = 21
    DataBinding.DataField = 'ADDRESS'
    DataBinding.DataSource = EditorsLookupDemoDataDM.dsUsers
    Style.StyleController = EditorsLookupDemoDataDM.StyleController
    TabOrder = 7
  end
  object mePhone: TcxDBMaskEdit
    Left = 8
    Top = 129
    Width = 193
    Height = 21
    DataBinding.DataField = 'PHONE'
    DataBinding.DataSource = EditorsLookupDemoDataDM.dsUsers
    Properties.MaskKind = emkRegExprEx
    Properties.EditMask = '(\(\d\d\d\))? \d(\d\d?)? - (\d\d\d? - \d\d|\d\d\d\d)'
    Properties.MaxLength = 0
    Style.StyleController = EditorsLookupDemoDataDM.StyleController
    TabOrder = 8
  end
  object meFax: TcxDBMaskEdit
    Left = 208
    Top = 129
    Width = 217
    Height = 21
    DataBinding.DataField = 'FAX'
    DataBinding.DataSource = EditorsLookupDemoDataDM.dsUsers
    Properties.MaskKind = emkRegExpr
    Properties.EditMask = '(\(\d\d\d\))? \d(\d\d?)? - (\d\d - \d\d | \d\d\d\d)'
    Properties.MaxLength = 0
    Style.StyleController = EditorsLookupDemoDataDM.StyleController
    TabOrder = 9
  end
  object heEMail: TcxDBHyperLinkEdit
    Left = 8
    Top = 168
    Width = 193
    Height = 21
    DataBinding.DataField = 'EMAIL'
    DataBinding.DataSource = EditorsLookupDemoDataDM.dsUsers
    ParentFont = False
    Style.StyleController = EditorsLookupDemoDataDM.StyleController
    TabOrder = 10
  end
  object heHomePAge: TcxDBHyperLinkEdit
    Left = 208
    Top = 168
    Width = 217
    Height = 21
    DataBinding.DataField = 'HOMEPAGE'
    DataBinding.DataSource = EditorsLookupDemoDataDM.dsUsers
    ParentFont = False
    Style.StyleController = EditorsLookupDemoDataDM.StyleController
    TabOrder = 11
  end
  object lcbDepartment: TcxDBLookupComboBox
    Left = 296
    Top = 21
    Width = 129
    Height = 21
    DataBinding.DataField = 'DEPARTMENTID'
    DataBinding.DataSource = EditorsLookupDemoDataDM.dsUsers
    Properties.KeyFieldNames = 'ID'
    Properties.ListColumns = <
      item
        FieldName = 'NAME'
      end>
    Properties.ListSource = EditorsLookupDemoDataDM.dsDepartments
    Style.Color = 16247513
    Style.StyleController = EditorsLookupDemoDataDM.StyleController
    TabOrder = 3
  end
  object btnOK: TcxButton
    Left = 264
    Top = 200
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 12
  end
  object btnCancel: TcxButton
    Left = 352
    Top = 200
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 13
  end
end
