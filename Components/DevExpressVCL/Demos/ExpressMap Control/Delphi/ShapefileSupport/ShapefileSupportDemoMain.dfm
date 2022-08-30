inherited ShapefileSupportDemoMainForm: TShapefileSupportDemoMainForm
  Caption = 'Shapefile Support'
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited cxGroupBox2: TcxGroupBox
    inherited dxMapControl1: TdxMapControl
      object dxMapControl1ItemFileLayer1: TdxMapItemFileLayer
        ProjectionClassName = 'TdxMapControlSphericalMercatorProjection'
        Active = True
        FileName = '.\Countries.shp'
        FileType = miftShape
        ItemHint = '{NAME}'
        ItemStyleHot.AssignedValues = [mcsvBorderColor]
        ItemStyleHot.BorderColor = -16777216
        ItemStyleSelected.AssignedValues = [mcsvBorderWidth, mcsvBorderColor]
        ItemStyleSelected.BorderColor = -16777216
        ItemStyleSelected.BorderWidth = 2
        ItemTitleOptions.Text = '{NAME}'
      end
    end
    object cxLabel1: TcxLabel
      Left = 3
      Top = 3
      Caption = 'GDP map'
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -27
      Style.Font.Name = 'Tahoma'
      Style.Font.Style = []
      Style.Shadow = False
      Style.IsFontAssigned = True
      Properties.Alignment.Horz = taLeftJustify
      Properties.Depth = 1
      Properties.LabelEffect = cxleExtrude
      Properties.ShadowedColor = clWhite
    end
  end
  inherited mmMain: TMainMenu
    inherited miOptions: TMenuItem
      object Political1: TMenuItem
        Action = actPolitical
        AutoCheck = True
        GroupIndex = 1
        RadioItem = True
      end
      object GDP1: TMenuItem
        Action = actGdp
        AutoCheck = True
        Checked = True
        GroupIndex = 1
        RadioItem = True
      end
      object Population1: TMenuItem
        Action = actPopulation
        AutoCheck = True
        GroupIndex = 1
        RadioItem = True
      end
    end
  end
  object ActionList1: TActionList
    Left = 488
    Top = 240
    object actPopulation: TAction
      Tag = 2
      AutoCheck = True
      Caption = 'Population'
      OnExecute = actPopulationExecute
    end
    object actGdp: TAction
      Tag = 1
      AutoCheck = True
      Caption = 'GDP'
      OnExecute = actPopulationExecute
    end
    object actPolitical: TAction
      AutoCheck = True
      Caption = 'Political'
      OnExecute = actPopulationExecute
    end
  end
end
