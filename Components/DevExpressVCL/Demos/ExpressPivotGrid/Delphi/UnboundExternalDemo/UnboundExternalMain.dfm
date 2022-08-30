inherited frmUnboundExternal: TfrmUnboundExternal
  Left = 228
  Top = 187
  Caption = 'PivotGrid - UnboundExternal Demo'
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescrip: TLabel
    Height = 16
    Caption = 'This demo shows you how to mine custom structured data.'
  end
  object UnboundPivotGrid: TcxPivotGrid [1]
    Left = 0
    Top = 16
    Width = 807
    Height = 527
    Align = alClient
    Groups = <>
    TabOrder = 0
    TabStop = True
  end
end
