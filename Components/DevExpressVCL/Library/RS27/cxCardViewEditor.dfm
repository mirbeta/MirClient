inherited cxCardViewEditor: TcxCardViewEditor
  Caption = 'cxCardViewEditor'
  PixelsPerInch = 96
  TextHeight = 13
  inherited PViewEditor: TPanel
    inherited lcMain: TdxLayoutControl
      inherited lgItems: TdxLayoutGroup
        CaptionOptions.Text = '   Rows   '
      end
      inherited lgSummary: TdxLayoutGroup
        Visible = False
      end
    end
  end
  inherited dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList
    inherited dxLayoutCxLookAndFeel: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
