inherited StylesMultiDemoMainForm: TStylesMultiDemoMainForm
  Left = 113
  Top = 108
  Caption = 'ExpressQuantumTreeList StylesMultiDemo'
  ClientHeight = 512
  ClientWidth = 799
  OldCreateOrder = True
  OnActivate = FormActivate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited lscrip: TLabel
    Width = 799
    Height = 25
    AutoSize = False
    Caption = 
      'Experiment using StyleSheets. See Help/About for other things to' +
      ' try.'
  end
  object Splitter: TSplitter [1]
    Left = 217
    Top = 25
    Width = 2
    Height = 468
    MinSize = 4
  end
  inherited sbMain: TStatusBar
    Top = 493
    Width = 799
  end
  object pnlLeft: TPanel [3]
    Left = 0
    Top = 25
    Width = 217
    Height = 468
    Align = alLeft
    Anchors = [akLeft]
    BevelInner = bvLowered
    BevelOuter = bvNone
    Color = 15451300
    Constraints.MinWidth = 215
    TabOrder = 0
    object gbUserDefined: TGroupBox
      Left = 1
      Top = 352
      Width = 215
      Height = 115
      Align = alBottom
      Caption = 'User Defined Style Sheets'
      TabOrder = 2
      TabStop = True
      object btnLoad: TcxButton
        Left = 11
        Top = 49
        Width = 193
        Height = 25
        Action = actLoadFromFile
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
      end
      object btnSave: TcxButton
        Left = 11
        Top = 76
        Width = 193
        Height = 25
        Action = actSaveToFile
        Anchors = [akLeft, akTop, akRight]
        Enabled = False
        TabOrder = 2
      end
      object btnEdit: TcxButton
        Left = 11
        Top = 22
        Width = 193
        Height = 25
        Action = actEditStyleSheet
        Anchors = [akLeft, akTop, akRight]
        Enabled = False
        TabOrder = 0
      end
    end
    object gbPredefined: TGroupBox
      Left = 1
      Top = 36
      Width = 215
      Height = 316
      Align = alClient
      Caption = 'Predefined Style Sheets'
      TabOrder = 1
      TabStop = True
      object tlStyleSheets: TcxTreeList
        Left = 2
        Top = 15
        Width = 211
        Height = 299
        Align = alClient
        Bands = <
          item
            Caption.Text = 'Band + 1'
            Width = 209
          end>
        OptionsCustomizing.BandCustomizing = False
        OptionsCustomizing.BandHorzSizing = False
        OptionsCustomizing.BandMoving = False
        OptionsCustomizing.BandVertSizing = False
        OptionsCustomizing.ColumnCustomizing = False
        OptionsCustomizing.ColumnHorzSizing = False
        OptionsCustomizing.ColumnMoving = False
        OptionsCustomizing.ColumnVertSizing = False
        OptionsData.Editing = False
        OptionsData.Deleting = False
        OptionsSelection.CellSelect = False
        OptionsSelection.HideSelection = True
        OptionsSelection.InvertSelect = False
        OptionsView.CategorizedColumn = clnGroupName
        OptionsView.ColumnAutoWidth = True
        OptionsView.Headers = False
        OptionsView.PaintStyle = tlpsCategorized
        Styles.Background = StylesMultiDemoDataDM.styGroupNode
        Styles.OnGetContentStyle = tlStyleSheetsStylesGetContentStyle
        Styles.OnGetNodeIndentStyle = tlStyleSheetsStylesGetNodeIndentStyle
        TabOrder = 0
        OnIsGroupNode = tlStyleSheetsIsGroupNode
        OnSelectionChanged = tlStyleSheetsSelectionChanged
        Data = {
          00000500F00000000F00000044617461436F6E74726F6C6C6572310200000012
          000000546378537472696E6756616C7565547970651200000054637853747269
          6E6756616C75655479706503000000000100040000004E6F6E65000100170000
          00507265646566696E6564207374796C65207368656574730001001900000055
          73657220646566696E6564207374796C65207368656574730300000000000000
          08000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF01000000080000000000
          00000000FFFFFFFFFFFFFFFFFFFFFFFF0200000008000000000000000000FFFF
          FFFFFFFFFFFFFFFFFFFF0A0003000000}
        object clnRadio: TcxTreeListColumn
          PropertiesClassName = 'TcxRadioGroupProperties'
          Properties.DefaultValue = False
          Properties.Items = <
            item
              Value = True
            end>
          Caption.AlignHorz = taRightJustify
          DataBinding.ValueType = 'String'
          Options.Customizing = False
          Options.Editing = False
          Options.IncSearch = False
          Options.Sorting = False
          Width = 54
          Position.ColIndex = 0
          Position.RowIndex = 0
          Position.BandIndex = 0
          Summary.FooterSummaryItems = <>
          Summary.GroupFooterSummaryItems = <>
        end
        object clnGroupName: TcxTreeListColumn
          DataBinding.ValueType = 'String'
          Width = 155
          Position.ColIndex = 1
          Position.RowIndex = 0
          Position.BandIndex = 0
          Summary.FooterSummaryItems = <>
          Summary.GroupFooterSummaryItems = <>
        end
      end
    end
    object pnlCurrentStyleSheet: TPanel
      Left = 1
      Top = 1
      Width = 215
      Height = 35
      Align = alTop
      BevelOuter = bvLowered
      Color = 12937777
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
    end
  end
  object cxDBTreeList: TcxDBTreeList [4]
    Left = 219
    Top = 25
    Width = 580
    Height = 468
    Align = alClient
    Bands = <
      item
        Caption.AlignHorz = taCenter
        Caption.Text = 'Common department info'
        Width = 201
      end
      item
        Caption.AlignHorz = taCenter
        Caption.Text = 'Detailed department info'
        Width = 220
      end
      item
        Caption.AlignHorz = taCenter
        Caption.Text = 'Manager info'
        Width = 339
      end>
    DataController.DataSource = StylesMultiDemoDataDM.dsDepartments
    DataController.ParentField = 'PARENTID'
    DataController.KeyField = 'ID'
    DragMode = dmAutomatic
    OptionsBehavior.CellHints = True
    OptionsBehavior.FooterHints = True
    OptionsBehavior.HeaderHints = True
    OptionsData.Inserting = True
    OptionsView.CellAutoHeight = True
    OptionsView.Bands = True
    OptionsView.Footer = True
    OptionsView.GridLineColor = clGray
    OptionsView.GridLines = tlglBoth
    OptionsView.GroupFooters = tlgfAlwaysVisible
    OptionsView.Indicator = True
    OptionsView.UseNodeColorForIndent = False
    Preview.Column = cxDBTreeListManagerAdress
    Preview.Visible = True
    RootValue = 255
    TabOrder = 2
    OnDragOver = cxDBTreeListDragOver
    OnInitInsertingRecord = cxDBTreeListInitInsertingRecord
    object cxDBTreeListID: TcxDBTreeListColumn
      Visible = False
      DataBinding.FieldName = 'ID'
      Width = 63
      Position.ColIndex = 4
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
      PropertiesClassName = 'TcxMemoProperties'
      DataBinding.FieldName = 'NAME'
      Width = 259
      Position.ColIndex = 0
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <
        item
          Kind = skCount
        end>
      Summary.GroupFooterSummaryItems = <
        item
          Kind = skCount
        end>
    end
    object cxDBTreeListPHONE: TcxDBTreeListColumn
      DataBinding.FieldName = 'PHONE'
      Width = 167
      Position.ColIndex = 1
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListFAX: TcxDBTreeListColumn
      DataBinding.FieldName = 'FAX'
      Width = 90
      Position.ColIndex = 0
      Position.RowIndex = 0
      Position.BandIndex = 1
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListBUDGET: TcxDBTreeListColumn
      PropertiesClassName = 'TcxCalcEditProperties'
      DataBinding.FieldName = 'BUDGET'
      Width = 86
      Position.ColIndex = 2
      Position.RowIndex = 0
      Position.BandIndex = 1
      Summary.FooterSummaryItems = <
        item
          Kind = skSum
        end>
      Summary.GroupFooterSummaryItems = <
        item
          Kind = skMin
        end
        item
          Kind = skMax
        end>
    end
    object cxDBTreeListVACANCY: TcxDBTreeListColumn
      DataBinding.FieldName = 'VACANCY'
      Width = 77
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
      Properties.ListSource = StylesMultiDemoDataDM.dsPersons
      Caption.Text = 'Manager'
      DataBinding.FieldName = 'MANAGERID'
      Width = 84
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
      Properties.ListSource = StylesMultiDemoDataDM.dsPersons
      Properties.ReadOnly = False
      Caption.Text = 'Manager Phone'
      DataBinding.FieldName = 'MANAGERID'
      Width = 103
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
      Properties.ListSource = StylesMultiDemoDataDM.dsPersons
      Properties.ReadOnly = False
      Caption.Text = 'Manager Email'
      DataBinding.FieldName = 'MANAGERID'
      Width = 152
      Position.ColIndex = 2
      Position.RowIndex = 0
      Position.BandIndex = 2
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListManagerAdress: TcxDBTreeListColumn
      PropertiesClassName = 'TcxLookupComboBoxProperties'
      Properties.KeyFieldNames = 'ID'
      Properties.ListColumns = <
        item
          FieldName = 'Address'
        end>
      Properties.ListSource = StylesMultiDemoDataDM.dsPersons
      DataBinding.FieldName = 'MANAGERID'
      Width = 67
      Position.ColIndex = 2
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
  end
  inherited alMain: TActionList
    Left = 432
    object actSaveToFile: TAction
      Category = 'Options'
      Caption = '&SaveToFile...'
      OnExecute = actSaveToFileExecute
    end
    object actLoadFromFile: TAction
      Category = 'Options'
      Caption = '&LoadFromFile...'
      OnExecute = actLoadFromFileExecute
    end
    object actEditStyleSheet: TAction
      Category = 'Options'
      Caption = '&Edit Style Sheet'
      OnExecute = actEditStyleSheetExecute
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '*.ini'
    Filter = '*.ini|*.ini'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofExtensionDifferent, ofEnableSizing]
    Left = 536
    Top = 8
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '*.ini'
    Filter = '*.ini|*.ini'
    Left = 568
    Top = 8
  end
end
