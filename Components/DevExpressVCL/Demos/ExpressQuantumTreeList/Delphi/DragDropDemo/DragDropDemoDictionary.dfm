object DragDropDemoDictionaryForm: TDragDropDemoDictionaryForm
  Left = 158
  Top = 119
  Width = 504
  Height = 397
  Caption = 'Dictionaries'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pcDictionary: TPageControl
    Left = 0
    Top = 0
    Width = 496
    Height = 363
    ActivePage = tsPersons
    Align = alClient
    MultiLine = True
    TabOrder = 0
    object tsDepartments: TTabSheet
      Caption = 'Departments'
      object lsc: TLabel
        Left = 0
        Top = 0
        Width = 488
        Height = 48
        Align = alTop
        Caption = 
          'Multiselect and drag rows from the list below, then drop them on' +
          'to the necessary place of the Departments hierarchy displayed by' +
          ' the main form '
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
      object tlDeptDict: TcxDBTreeList
        Left = 0
        Top = 48
        Width = 488
        Height = 287
        Styles.StyleSheet = DragDropDemoDataDM.TreeListStyleSheetDevExpress
        Align = alClient
        Bands = <
          item
            Caption.Text = 'Band + 1'
          end>
        BufferedPaint = False
        DataController.DataSource = DragDropDemoDataDM.dsDeptDict
        DataController.ParentField = 'ID'
        DataController.KeyField = 'ID'
        DragMode = dmAutomatic
        OptionsBehavior.ImmediateEditor = False
        OptionsBehavior.AutomateLeftMostIndent = False
        OptionsBehavior.IncSearchItem = tlDeptDictNAME
        OptionsBehavior.DragDropText = True
        OptionsBehavior.IncSearch = True
        OptionsData.Inserting = True
        OptionsSelection.MultiSelect = True
        OptionsView.Indicator = True
        OptionsView.ShowRoot = False
        RootValue = 255
        TabOrder = 0
        OnDragOver = tlDictDragOver
        OnMoveTo = tlDictMoveTo
        object tlDeptDictPARENTID: TcxDBTreeListColumn
          Visible = False
          Caption.Text = 'PARENTID'
          DataBinding.FieldName = 'PARENTID'
          Position.ColIndex = 6
          Position.RowIndex = 0
          Position.BandIndex = 0
        end
        object tlDeptDictNAME: TcxDBTreeListColumn
          Caption.Text = 'NAME'
          DataBinding.FieldName = 'NAME'
          Width = 177
          Position.ColIndex = 0
          Position.RowIndex = 0
          Position.BandIndex = 0
          SortOrder = soAscending
        end
        object tlDeptDictBUDGET: TcxDBTreeListColumn
          Caption.Text = 'BUDGET'
          DataBinding.FieldName = 'BUDGET'
          Width = 173
          Position.ColIndex = 1
          Position.RowIndex = 0
          Position.BandIndex = 0
        end
        object tlDeptDictPHONE: TcxDBTreeListColumn
          Caption.Text = 'PHONE'
          DataBinding.FieldName = 'PHONE'
          Width = 174
          Position.ColIndex = 2
          Position.RowIndex = 0
          Position.BandIndex = 0
        end
        object tlDeptDictFAX: TcxDBTreeListColumn
          Caption.Text = 'FAX'
          DataBinding.FieldName = 'FAX'
          Width = 177
          Position.ColIndex = 3
          Position.RowIndex = 0
          Position.BandIndex = 0
        end
        object tlDeptDictEMAIL: TcxDBTreeListColumn
          Caption.Text = 'EMAIL'
          DataBinding.FieldName = 'EMAIL'
          Width = 173
          Position.ColIndex = 4
          Position.RowIndex = 0
          Position.BandIndex = 0
        end
        object tlDeptDictVACANCY: TcxDBTreeListColumn
          Caption.Text = 'VACANCY'
          DataBinding.FieldName = 'VACANCY'
          Width = 177
          Position.ColIndex = 5
          Position.RowIndex = 0
          Position.BandIndex = 0
        end
      end
    end
    object tsPersons: TTabSheet
      Caption = 'Persons'
      ImageIndex = 1
      object Label1: TLabel
        Left = 0
        Top = 0
        Width = 488
        Height = 48
        Align = alTop
        Caption = 
          'Multiselect and drag rows from the list below, then drop them on' +
          'to the Departments hierarchy or the current department'#39's employe' +
          'e list displayed by the main form'
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
      object tlEmplDict: TcxDBTreeList
        Left = 0
        Top = 48
        Width = 488
        Height = 287
        Styles.StyleSheet = DragDropDemoDataDM.TreeListStyleSheetDevExpress
        Align = alClient
        Bands = <
          item
            Caption.Text = 'Band + 1'
          end>
        BufferedPaint = False
        DataController.DataSource = DragDropDemoDataDM.dsPersDict
        DataController.ParentField = 'ID'
        DataController.KeyField = 'ID'
        DragMode = dmAutomatic
        OptionsBehavior.ImmediateEditor = False
        OptionsBehavior.AutomateLeftMostIndent = False
        OptionsBehavior.IncSearchItem = tlDeptDictNAME
        OptionsBehavior.DragDropText = True
        OptionsBehavior.IncSearch = True
        OptionsData.Inserting = True
        OptionsSelection.MultiSelect = True
        OptionsView.Indicator = True
        OptionsView.ShowRoot = False
        RootValue = 255
        TabOrder = 0
        OnDragOver = tlDictDragOver
        OnMoveTo = tlDictMoveTo
        object tlEmplDictName: TcxDBTreeListColumn
          Caption.Text = 'Name'
          DataBinding.FieldName = 'Name'
          Width = 187
          Position.ColIndex = 0
          Position.RowIndex = 0
          Position.BandIndex = 0
          SortOrder = soAscending
        end
        object tlEmplDictCountry: TcxDBTreeListColumn
          Caption.Text = 'Country'
          DataBinding.FieldName = 'Country'
          Width = 187
          Position.ColIndex = 1
          Position.RowIndex = 0
          Position.BandIndex = 0
        end
        object tlEmplDictPostalCode: TcxDBTreeListColumn
          Caption.Text = 'PostalCode'
          DataBinding.FieldName = 'PostalCode'
          Width = 187
          Position.ColIndex = 2
          Position.RowIndex = 0
          Position.BandIndex = 0
        end
        object tlEmplDictCity: TcxDBTreeListColumn
          Caption.Text = 'City'
          DataBinding.FieldName = 'City'
          Width = 187
          Position.ColIndex = 3
          Position.RowIndex = 0
          Position.BandIndex = 0
        end
        object tlEmplDictAddress: TcxDBTreeListColumn
          Caption.Text = 'Address'
          DataBinding.FieldName = 'Address'
          Width = 194
          Position.ColIndex = 4
          Position.RowIndex = 0
          Position.BandIndex = 0
        end
        object tlEmplDictPhone: TcxDBTreeListColumn
          Caption.Text = 'Phone'
          DataBinding.FieldName = 'Phone'
          Width = 187
          Position.ColIndex = 5
          Position.RowIndex = 0
          Position.BandIndex = 0
        end
        object tlEmplDictFax: TcxDBTreeListColumn
          Caption.Text = 'Fax'
          DataBinding.FieldName = 'Fax'
          Width = 187
          Position.ColIndex = 6
          Position.RowIndex = 0
          Position.BandIndex = 0
        end
        object tlEmplDictEMAIL: TcxDBTreeListColumn
          Caption.Text = 'EMAIL'
          DataBinding.FieldName = 'EMAIL'
          Width = 187
          Position.ColIndex = 7
          Position.RowIndex = 0
          Position.BandIndex = 0
        end
        object tlEmplDictHOMEPAGE: TcxDBTreeListColumn
          Caption.Text = 'HOMEPAGE'
          DataBinding.FieldName = 'HOMEPAGE'
          Width = 187
          Position.ColIndex = 8
          Position.RowIndex = 0
          Position.BandIndex = 0
        end
        object tlEmplDictDepartmentID: TcxDBTreeListColumn
          Visible = False
          Caption.Text = 'DepartmentID'
          DataBinding.FieldName = 'DepartmentID'
          Position.ColIndex = 9
          Position.RowIndex = 0
          Position.BandIndex = 0
        end
      end
    end
  end
end
