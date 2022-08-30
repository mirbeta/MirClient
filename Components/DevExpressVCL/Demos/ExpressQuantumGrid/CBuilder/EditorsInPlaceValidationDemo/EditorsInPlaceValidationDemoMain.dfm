inherited EditorsInPlaceValidationDemoMainForm: TEditorsInPlaceValidationDemoMainForm
  Left = 0
  Top = 0
  Caption = 'ExpressQuantumGrid EditorsInPlaceValidation Demo'
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Height = 32
    Caption = 
      'This demo shows how to validate data input in-place using the bu' +
      'ilt-in validation capabilities. Click '#39'About this demo'#39' for more' +
      ' information.'
  end
  object cxGrid: TcxGrid [1]
    Left = 0
    Top = 32
    Width = 623
    Height = 325
    Align = alClient
    TabOrder = 0
    object cxGridTableView: TcxGridTableView
      Navigator.Buttons.CustomButtons = <>
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      DataController.Data = {
        290200000F00000044617461436F6E74726F6C6C657231050000001200000054
        6378537472696E6756616C75655479706512000000546378537472696E675661
        6C75655479706512000000546378537472696E6756616C756554797065120000
        00546378537472696E6756616C75655479706512000000546378537472696E67
        56616C75655479706506000000445841464D540000040000004A6F686E010019
        00000031323320486F6D65204C616E652C20486F6D657376696C6C6501014458
        41464D5400000500000048656E72790100160000003433362031737420417665
        2C20436C6576656C616E64000E0000002838303029203234342D31303639000F
        000000696E666F40686F74626F782E636F6D445841464D540000050000004672
        616E6B0006000000486F6C6D6573001C00000033343920477261706869632044
        657369676E204C2C204E65776D616E0101445841464D540000070000004C6574
        696369610004000000466F726401000E0000002835353529203737362D313536
        36000B000000666F726440686F74626F78445841464D540000050000004B6172
        656E0006000000486F6C6D657301000E0000002835353529203334322D323537
        3401445841464D54000005000000526F67657200090000004D696368656C736F
        6E001E00000033393230204D696368656C736F6E2044722E2C20427269646765
        666F7264000E0000002835353529203935342D353138380011000000726F6765
        726D406D796D61696C2E626F78}
      OptionsBehavior.CellHints = True
      OptionsCustomize.ColumnsQuickCustomization = True
      object cxGridTableViewColumnFirstName: TcxGridColumn
        Caption = 'First Name'
        PropertiesClassName = 'TcxTextEditProperties'
        Properties.ValidateOnEnter = True
        Properties.OnValidate = cxGridTableViewColumnFirstNamePropertiesValidate
        OnValidateDrawValue = cxGridTableViewColumnFirstNameValidateDrawValue
      end
      object cxGridTableViewColumnLastName: TcxGridColumn
        Caption = 'Last Name'
        PropertiesClassName = 'TcxTextEditProperties'
        Properties.ValidateOnEnter = True
        Properties.OnValidate = cxGridTableViewColumnLastNamePropertiesValidate
        OnValidateDrawValue = cxGridTableViewColumnLastNameValidateDrawValue
        Width = 99
      end
      object cxGridTableViewColumnAddress: TcxGridColumn
        Caption = 'Address'
        PropertiesClassName = 'TcxComboBoxProperties'
        Properties.DropDownListStyle = lsFixedList
        Properties.Items.Strings = (
          '123 Home Lane, Homesville'
          '436 1st Ave, Cleveland'
          '349 Graphic Design L, Newman'
          '3920 Michelson Dr., Bridgeford')
        Properties.OnValidate = cxGridTableViewColumnAddressPropertiesValidate
        OnValidateDrawValue = cxGridTableViewColumnAddressValidateDrawValue
        Width = 175
      end
      object cxGridTableViewColumnPhoneNumber: TcxGridColumn
        Caption = 'Phone Number'
        PropertiesClassName = 'TcxTextEditProperties'
        Properties.ValidateOnEnter = True
        Properties.OnValidate = cxGridTableViewColumnPhoneNumberPropertiesValidate
        OnValidateDrawValue = cxGridTableViewColumnPhoneNumberValidateDrawValue
        Width = 102
      end
      object cxGridTableViewColumnEmail: TcxGridColumn
        Caption = 'Email'
        PropertiesClassName = 'TcxTextEditProperties'
        Properties.ValidateOnEnter = True
        Properties.OnValidate = cxGridTableViewColumnEmailPropertiesValidate
        OnValidateDrawValue = cxGridTableViewColumnEmailValidateDrawValue
        Width = 120
      end
    end
    object cxGridLevel: TcxGridLevel
      GridView = cxGridTableView
    end
  end
  inherited mmMain: TMainMenu
    object miOptions: TMenuItem [1]
      Caption = 'Options'
      object ValidationOptions1: TMenuItem
        Caption = 'Validation Options'
        object miValidationRaiseException: TMenuItem
          AutoCheck = True
          Caption = 'Raise Exception'
          OnClick = InitializeEditors
        end
        object miValidationShowErrorIcon: TMenuItem
          AutoCheck = True
          Caption = 'Show Error Icons'
          Checked = True
          OnClick = InitializeEditors
        end
        object miValidationAllowLoseFocus: TMenuItem
          AutoCheck = True
          Caption = 'Allow Lose Focus'
          Checked = True
          OnClick = InitializeEditors
        end
      end
    end
  end
  inherited StyleRepository: TcxStyleRepository
    PixelsPerInch = 96
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
    inherited GridCardViewStyleSheetDevExpress: TcxGridCardViewStyleSheet
      BuiltIn = True
    end
  end
  object cxGridPopupMenu1: TcxGridPopupMenu
    Grid = cxGrid
    PopupMenus = <>
    Left = 304
    Top = 200
  end
  object cxHintStyleController: TcxHintStyleController
    HintStyleClassName = 'TdxScreenTipStyle'
    HintStyle.ScreenTipLinks = <
      item
        ScreenTip = stGrid
        Control = cxGrid
      end>
    HintStyle.ScreenTipActionLinks = <>
    HintHidePause = 10000
    OnShowHintEx = cxHintStyleControllerShowHintEx
    Left = 168
    Top = 216
  end
  object dxScreenTipRepository: TdxScreenTipRepository
    AssignedFonts = [stbHeader]
    HeaderFont.Charset = DEFAULT_CHARSET
    HeaderFont.Color = 5000268
    HeaderFont.Height = -12
    HeaderFont.Name = 'Segoe UI Semibold'
    HeaderFont.Style = [fsBold]
    Left = 168
    Top = 272
    object stGrid: TdxScreenTip
      Description.GlyphFixedWidth = False
      UseHintAsHeader = True
      Width = 1
    end
  end
  object icCustomIconList: TcxImageCollection
    Left = 88
    Top = 248
    object icCustomIcon1: TcxImageCollectionItem
      Picture.Data = {
        07544269746D617076020000424D760200000000000036000000280000000C00
        00000C0000000100200000000000400200000000000000000000000000000000
        00002DABC7FE1195B3FF1095B4FF0F94B4FF0F94B4FF0F94B4FF0F94B4FF0F94
        B4FF1095B4FF1095B4FF1195B3FF36B3CEFF37BDD9FE1DC1E5FF18C5F0FF14C2
        F0FF14C2F0FF0A6F8BFF0B6F8AFF16C3F0FF18C5F0FF1AC6F1FF1BBFE4FF44BD
        D8FF2771819A29BCDAFF24CDEFFF1ECAF1FF1CC8F1FF042128FF052127FF21CC
        F2FF24CFF2FF26CFF1FF29BCDAFF1539414D234249533DADC4E92ECFEAFF31DA
        F5FF2ED8F4FF29C6E0FF2BC7E1FF31DAF5FF32DBF5FF2DCFE9FF3DACC3EA0F1C
        1E22000000002C616C7E29B9D5FF43E8F6FF43E9F8FF165158FF155057FF40E6
        F8FF3EE4F5FF29BAD6FF2D626D7F00000000000000000A1112142999B3DB32CE
        E3FF4DF1F9FF0C2829FF0B2728FF49EEF9FF32D0E5FF2A9AB3DC0A1112140000
        0000000000000000000060969FB020B3D1FF4AEAF4FF0D282AFF0C2729FF49EA
        F5FF20B3D2FF6097A2B1000000000000000000000000000000000000000059C6
        DBFF36D0E3FF2F8E92FF2F8E92FF37D3E5FF5AC6DCFF00000000000000000000
        00000000000000000000000000001C3B414C2BB9D4FF4BECF5FF4CEEF6FF2FBF
        D8FF1C3B424C000000000000000000000000000000000000000000000000161F
        202251B4C8E937D4E6FF3DDBEAFF51B4C9E91720212300000000000000000000
        000000000000000000000000000000000000335F68762DB5D1FC31BAD3FC3360
        6977000000000000000000000000000000000000000000000000000000000000
        0000242F313312424D5D1E454D5A151C1D1F0000000000000000000000000000
        0000}
    end
  end
end
