inherited cxLayoutViewEditor: TcxLayoutViewEditor
  Caption = 'cxLayoutViewEditor'
  PixelsPerInch = 96
  TextHeight = 13
  inherited PViewEditor: TPanel
    inherited lcMain: TdxLayoutControl
      inherited lgItems: TdxLayoutGroup
        CaptionOptions.Text = '   Items   '
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
