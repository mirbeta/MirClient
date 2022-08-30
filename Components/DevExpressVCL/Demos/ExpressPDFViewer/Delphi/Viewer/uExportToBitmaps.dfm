inherited frmExportToBitmaps: TfrmExportToBitmaps
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited dxLayoutControl1: TdxLayoutControl
    inherited cbOpenAfterExport: TcxCheckBox
      Top = 64
      TabOrder = 4
    end
    inherited sePageZoom: TcxSpinEdit
      Left = 95
    end
    object teFilePrefix: TcxTextEdit [3]
      Left = 95
      Top = 37
      Style.BorderColor = clWindowFrame
      Style.BorderStyle = ebs3D
      Style.HotTrack = False
      TabOrder = 3
      Text = 'Image_'
      Width = 121
    end
    inherited dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      Index = 1
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      CaptionOptions.Text = 'File name prefix:'
      Control = teFilePrefix
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahLeft
      AlignVert = avClient
      Index = 2
      AutoCreated = True
    end
  end
end
