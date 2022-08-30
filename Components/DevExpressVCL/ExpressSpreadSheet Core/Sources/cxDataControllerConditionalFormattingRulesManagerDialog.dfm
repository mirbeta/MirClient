inherited frmDataControllerConditionalFormattingRulesManagerDialog: TfrmDataControllerConditionalFormattingRulesManagerDialog
  PixelsPerInch = 96
  TextHeight = 13
  inherited lcMain: TdxLayoutControl
    inherited tlRules: TcxTreeList
      inherited tlcRuleName: TcxTreeListColumn
        Width = 264
      end
      inherited tlcRuleFormat: TcxTreeListColumn
        Width = 108
      end
      inherited tlcRuleArea: TcxTreeListColumn
        Width = 107
      end
      inherited tlcRuleStopIfTrue: TcxTreeListColumn
        Width = 89
        Position.ColIndex = 4
      end
      object tlcRuleApplyToTheRecord: TcxTreeListColumn
        Caption.Text = 'Apply to the record'
        DataBinding.ValueType = 'String'
        Width = 115
        Position.ColIndex = 3
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
        OnGetEditProperties = tlcRuleApplyToTheRecordGetEditProperties
      end
    end
    inherited btnMoveUp: TcxButton
      PaintStyle = bpsGlyph
    end
    inherited btnMoveDown: TcxButton
      PaintStyle = bpsGlyph
    end
  end
  inherited dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList
    inherited dxLayoutCxLookAndFeel: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
  inherited ilImages: TcxImageList
    FormatVersion = 1
  end
  inherited cxEditRepository: TcxEditRepository
    PixelsPerInch = 96
  end
  object cxEditRepositoryDataController: TcxEditRepository [5]
    Left = 304
    PixelsPerInch = 96
    object cxEditRepositoryCheckBoxApplyToTheRow: TcxEditRepositoryCheckBoxItem
      Properties.NullStyle = nssInactive
      Properties.OnEditValueChanged = cxEditRepositoryCheckBoxApplyToTheRowPropertiesEditValueChanged
    end
  end
end
