object ColumnsShareDemoLookupCustomizeForm: TColumnsShareDemoLookupCustomizeForm
  Left = 270
  Top = 201
  BorderStyle = bsDialog
  Caption = 'Customize the Lookups of all Person columns'
  ClientHeight = 340
  ClientWidth = 362
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbDescr: TLabel
    Left = 0
    Top = 0
    Width = 362
    Height = 33
    Align = alTop
    AutoSize = False
    Caption = 
      'Change the fields displayed and/or lookup properties of all '#39'per' +
      'son'#39' columns'
    Color = 12937777
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    WordWrap = True
  end
  object btnClose: TcxButton
    Left = 279
    Top = 307
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    ModalResult = 1
    TabOrder = 0
    OnClick = btnCloseClick
  end
  object PageControl1: TcxPageControl
    Left = 8
    Top = 40
    Width = 345
    Height = 257
    TabOrder = 1
    Properties.ActivePage = tsLookupListFields
    Properties.ShowFrame = True
    ClientRectBottom = 256
    ClientRectLeft = 1
    ClientRectRight = 344
    ClientRectTop = 24
    object tsLookupListFields: TcxTabSheet
      BorderWidth = 4
      Caption = 'Lookup List Fields'
      object lbDescription: TcxLabel
        Left = 198
        Top = 6
        Caption = 'Lookup list fields'
        Transparent = True
      end
      object Label3: TcxLabel
        Left = 1
        Top = 6
        Caption = 'Unlinked fields'
        Transparent = True
      end
      object Label1: TcxLabel
        Left = 0
        Top = 181
        Align = alBottom
        Caption = 
          'Move an unlinked field (e.g. COUNTRY or CITY) to the linked list' +
          ' and then see the affect on a column in the main form  (e.g. Cre' +
          'ator, Owner etc).'
        ParentColor = False
        ParentFont = False
        Properties.WordWrap = True
        Transparent = True
        Width = 335
      end
      object btnAdd: TcxButton
        Left = 152
        Top = 56
        Width = 33
        Height = 25
        Caption = '>'
        TabOrder = 0
        OnClick = btnAddClick
      end
      object btnDelete: TcxButton
        Left = 152
        Top = 89
        Width = 33
        Height = 25
        Caption = '<'
        TabOrder = 1
        OnClick = btnDeleteClick
      end
      object lbUnlinkedColumns: TcxListBox
        Left = 3
        Top = 24
        Width = 130
        Height = 145
        ItemHeight = 13
        Style.Color = 16247513
        TabOrder = 2
        OnDblClick = btnAddClick
        OnKeyPress = lbUnlinkedColumnsKeyPress
      end
      object lbListColumns: TcxListBox
        Left = 200
        Top = 24
        Width = 130
        Height = 145
        ItemHeight = 13
        Style.Color = 16247513
        TabOrder = 3
        OnDblClick = btnDeleteClick
        OnKeyPress = lbListColumnsKeyPress
      end
    end
    object tsLookupProperties: TcxTabSheet
      Caption = 'Lookup Properties'
      Enabled = False
      ImageIndex = 1
      object Label5: TcxLabel
        Left = 191
        Top = 91
        Caption = 'List field index'
        Transparent = True
      end
      object Label4: TcxLabel
        Left = 191
        Top = 48
        Caption = 'DropDown rows:'
        Transparent = True
      end
      object Label2: TcxLabel
        Left = 191
        Top = 6
        Caption = 'DropDown list style:'
        Transparent = True
      end
      object chbHeaders: TcxCheckBox
        Left = 8
        Top = 86
        Caption = 'Headers'
        TabOrder = 0
        OnClick = chbHeadersClick
        Width = 121
        Transparent = True
      end
      object chbIncrementalFilltering: TcxCheckBox
        Left = 8
        Top = 60
        Caption = 'Incremental Filltering'
        TabOrder = 1
        OnClick = chbIncrementalFillteringClick
        Width = 121
        Transparent = True
      end
      object chbImmediateDropDown: TcxCheckBox
        Left = 8
        Top = 34
        Caption = 'Immediate Drop Down'
        TabOrder = 2
        OnClick = chbImmediateDropDownClick
        Width = 129
        Transparent = True
      end
      object chbDropDownAutoSize: TcxCheckBox
        Left = 8
        Top = 8
        Caption = 'Drop Down Auto Size'
        TabOrder = 3
        OnClick = chbDropDownAutoSizeClick
        Width = 129
        Transparent = True
      end
      object seListFieldIndex: TcxSpinEdit
        Left = 192
        Top = 108
        Properties.OnChange = seListFieldIndexPropertiesChange
        Style.Color = 16247513
        TabOrder = 4
        Width = 129
      end
      object seDropDownRows: TcxSpinEdit
        Left = 192
        Top = 65
        Properties.OnChange = seDropDownRowsPropertiesChange
        Style.Color = 16247513
        TabOrder = 5
        Width = 129
      end
      object cbDropDownListStyle: TcxComboBox
        Left = 192
        Top = 23
        Properties.DropDownListStyle = lsFixedList
        Properties.Items.Strings = (
          'lsEditFixedList'
          'lsEditList'
          'lsFixedList')
        Properties.OnChange = cbDropDownListStylePropertiesChange
        Style.Color = 16247513
        TabOrder = 6
        Width = 129
      end
    end
  end
end
