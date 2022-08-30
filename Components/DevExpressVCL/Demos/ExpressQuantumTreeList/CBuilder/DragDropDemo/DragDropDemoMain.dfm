inherited DragDropDemoMainForm: TDragDropDemoMainForm
  Left = 188
  Top = 42
  Caption = 'ExpressQuantumTreeList DragDropDemo'
  ClientHeight = 494
  ClientWidth = 750
  OldCreateOrder = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited lscrip: TLabel
    Width = 750
    Caption =
      'This demo demonstrates how to populate the TreeList control from' +
      ' another one and change its hierarchy using drag/drop. See Help/' +
      'About for more information'
  end
  object Splitter1: TSplitter [1]
    Left = 361
    Top = 32
    Width = 8
    Height = 443
    Color = 15981511
    ParentColor = False
  end
  inherited sbMain: TStatusBar
    Top = 475
    Width = 750
  end
  object pnlDepartments: TPanel [3]
    Left = 0
    Top = 32
    Width = 361
    Height = 443
    Align = alLeft
    BevelOuter = bvNone
    Caption = 'pnlDepartments'
    TabOrder = 2
    object tlDepartments: TcxDBTreeList
      Left = 0
      Top = 28
      Width = 361
      Height = 415
      Align = alClient
      Bands = <
        item
          Caption.Text = 'Band + 1'
        end>
      DataController.DataSource = DragDropDemoDataDM.dsDepartments
      DataController.ParentField = 'PARENTID'
      DataController.KeyField = 'ID'
      DragMode = dmAutomatic
      OptionsBehavior.ImmediateEditor = False
      OptionsBehavior.DragDropText = True
      OptionsBehavior.DragFocusing = True
      OptionsView.GridLineColor = 14916958
      OptionsView.GridLines = tlglBoth
      OptionsView.Indicator = True
      Preview.Column = tlDepartmentsNAME
      Preview.Place = tlppTop
      Preview.Visible = True
      RootValue = 255
      Styles.StyleSheet = DragDropDemoDataDM.TreeListStyleSheetDevExpress
      TabOrder = 0
      OnDragDrop = tlDepartmentsDragDrop
      OnDragOver = tlDepartmentsDragOver
      OnInitInsertingRecord = tlDepartmentsInitInsertingRecord
      object tlDepartmentsID: TcxDBTreeListColumn
        Visible = False
        DataBinding.FieldName = 'ID'
        Position.ColIndex = 6
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlDepartmentsPARENTID: TcxDBTreeListColumn
        Visible = False
        DataBinding.FieldName = 'PARENTID'
        Position.ColIndex = 7
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlDepartmentsNAME: TcxDBTreeListColumn
        DataBinding.FieldName = 'NAME'
        Position.ColIndex = 5
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlDepartmentsBUDGET: TcxDBTreeListColumn
        DataBinding.FieldName = 'BUDGET'
        Position.ColIndex = 0
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlDepartmentsPHONE: TcxDBTreeListColumn
        DataBinding.FieldName = 'PHONE'
        Position.ColIndex = 1
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlDepartmentsFAX: TcxDBTreeListColumn
        DataBinding.FieldName = 'FAX'
        Position.ColIndex = 2
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlDepartmentsEMAIL: TcxDBTreeListColumn
        DataBinding.FieldName = 'EMAIL'
        Position.ColIndex = 3
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlDepartmentsVACANCY: TcxDBTreeListColumn
        DataBinding.FieldName = 'VACANCY'
        Position.ColIndex = 4
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
    end
    object pnlDeptCaption: TPanel
      Left = 0
      Top = 0
      Width = 361
      Height = 28
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Departments'
      Color = 12937777
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -24
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
  end
  object pnlEmployees: TPanel [4]
    Left = 369
    Top = 32
    Width = 381
    Height = 443
    Align = alClient
    BevelOuter = bvNone
    Caption = 'pnlEmployees'
    TabOrder = 1
    object pnlEmplCaption: TPanel
      Left = 0
      Top = 0
      Width = 381
      Height = 28
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Employees'
      Color = 12937777
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -24
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object tlEmployees: TcxDBTreeList
      Left = 0
      Top = 28
      Width = 381
      Height = 415
      Align = alClient
      Bands = <
        item
          Caption.Text = 'Band + 1'
        end>
      DataController.DataSource = DragDropDemoDataDM.dsPersons
      DataController.ParentField = 'ID'
      DataController.KeyField = 'ID'
      DragMode = dmAutomatic
      OptionsBehavior.ImmediateEditor = False
      OptionsBehavior.DragDropText = True
      OptionsBehavior.DragFocusing = True
      OptionsSelection.MultiSelect = True
      OptionsView.Buttons = False
      OptionsView.Indicator = True
      OptionsView.ShowRoot = False
      RootValue = 255
      Styles.StyleSheet = DragDropDemoDataDM.TreeListStyleSheetDevExpress
      TabOrder = 1
      OnDragDrop = tlEmployeesDragDrop
      OnDragOver = tlEmployeesDragOver
      OnMoveTo = tlEmployeesMoveTo
      object tlEmployeesName: TcxDBTreeListColumn
        DataBinding.FieldName = 'Name'
        Position.ColIndex = 0
        Position.RowIndex = 0
        Position.BandIndex = 0
        SortOrder = soAscending
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlEmployeesCountry: TcxDBTreeListColumn
        DataBinding.FieldName = 'Country'
        Position.ColIndex = 1
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlEmployeesPostalCode: TcxDBTreeListColumn
        DataBinding.FieldName = 'PostalCode'
        Position.ColIndex = 2
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlEmployeesCity: TcxDBTreeListColumn
        DataBinding.FieldName = 'City'
        Position.ColIndex = 3
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlEmployeesAddress: TcxDBTreeListColumn
        DataBinding.FieldName = 'Address'
        Position.ColIndex = 4
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlEmployeesPhone: TcxDBTreeListColumn
        DataBinding.FieldName = 'Phone'
        Position.ColIndex = 5
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlEmployeesFax: TcxDBTreeListColumn
        DataBinding.FieldName = 'Fax'
        Position.ColIndex = 6
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlEmployeesEMAIL: TcxDBTreeListColumn
        DataBinding.FieldName = 'EMAIL'
        Position.ColIndex = 7
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlEmployeesHOMEPAGE: TcxDBTreeListColumn
        DataBinding.FieldName = 'HOMEPAGE'
        Position.ColIndex = 8
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlEmployeesDepartmentID: TcxDBTreeListColumn
        Visible = False
        DataBinding.FieldName = 'DepartmentID'
        Position.ColIndex = 9
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
    end
  end
  inherited mmMain: TMainMenu
    inherited miOptions: TMenuItem
      object ShowDictionaries1: TMenuItem [0]
        Caption = 'Show &Dictionaries'
        Hint = 'Shows the Dictionaries form'
        OnClick = ShowDictionaries1Click
      end
      object N2: TMenuItem [1]
        Caption = '-'
      end
      object miDragExpande: TMenuItem [2]
        Caption = 'Drag &Expand'
        Checked = True
        Hint = 
          'If checked, a collapsed node is expanded when a user drags a nod' +
          'e to it'
        OnClick = miDragExpandeClick
      end
      object miDragCollapse: TMenuItem [3]
        Caption = 'Drag &Collapse'
        Checked = True
        Hint = 
          'If checked, an expanded node is collapsed when a user drags a no' +
          'de to it '
        OnClick = miDragCollapseClick
      end
      object N1: TMenuItem [4]
        Caption = '-'
      end
      object miColumnCustomization: TMenuItem [5]
        Caption = 'Column &Customization ...'
        Hint = 'Shows the column customization form'
        OnClick = miColumnCustomizationClick
      end
      object miSeparator1: TMenuItem [6]
        Caption = '-'
      end
    end
  end
end
