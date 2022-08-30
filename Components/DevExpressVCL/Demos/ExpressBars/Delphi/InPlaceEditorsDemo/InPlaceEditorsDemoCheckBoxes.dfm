inherited frmCheckBoxes: TfrmCheckBoxes
  Tag = 5
  Left = 406
  Top = 186
  BorderIcons = []
  Caption = 'frmCheckBoxes'
  ClientHeight = 404
  ClientWidth = 618
  PixelsPerInch = 96
  TextHeight = 13
  inherited lblFrameDescription: TLabel
    Width = 618
  end
  object Panel1: TPanel
    Left = 0
    Top = 16
    Width = 618
    Height = 388
    Align = alClient
    BevelOuter = bvNone
    Color = clWindow
    TabOrder = 0
    OnResize = Panel1Resize
    object Panel2: TPanel
      Left = 200
      Top = 88
      Width = 195
      Height = 211
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 0
      object GroupBox1: TGroupBox
        Left = 1
        Top = 1
        Width = 81
        Height = 209
        Caption = 'CheckGroup'
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 0
        object Shape1: TShape
          Left = 25
          Top = 27
          Width = 32
          Height = 32
          Brush.Color = clRed
        end
        object Shape2: TShape
          Left = 25
          Top = 91
          Width = 32
          Height = 32
          Brush.Color = clYellow
        end
        object Shape3: TShape
          Left = 25
          Top = 156
          Width = 32
          Height = 32
          Brush.Color = clGreen
        end
      end
      object GroupBox2: TGroupBox
        Left = 113
        Top = 1
        Width = 81
        Height = 209
        Caption = 'RadioGroup'
        TabOrder = 1
        object Shape4: TShape
          Left = 25
          Top = 27
          Width = 32
          Height = 32
          Brush.Color = clRed
          Shape = stCircle
        end
        object Shape5: TShape
          Left = 25
          Top = 91
          Width = 32
          Height = 32
          Brush.Color = clYellow
          Shape = stCircle
        end
        object Shape6: TShape
          Left = 25
          Top = 156
          Width = 32
          Height = 32
          Brush.Color = clGreen
          Shape = stCircle
        end
      end
    end
  end
end
