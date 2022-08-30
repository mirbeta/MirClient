object LayoutEditForm: TLayoutEditForm
  Left = 233
  Top = 209
  ActiveControl = cbDescriptions
  BorderStyle = bsDialog
  Caption = 'LayoutEditForm'
  ClientHeight = 88
  ClientWidth = 282
  Color = clBtnFace
  OldCreateOrder = False
  Position = poScreenCenter
  Font.Height = -11
  PixelsPerInch = 96
  TextHeight = 13
  AutoScroll = False
  object LayoutControl: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 282
    Height = 88
    Align = alClient
    TabOrder = 0
    TabStop = False
    object btnOK: TButton
      Left = 116
      Top = 55
      Width = 75
      Height = 23
      Caption = 'btnOK'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
    object btnCancel: TButton
      Left = 197
      Top = 55
      Width = 75
      Height = 23
      Cancel = True
      Caption = 'btnCancel'
      ModalResult = 2
      TabOrder = 2
    end
    object cbDescriptions: TComboBox
      Left = 10
      Top = 28
      Width = 262
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
    end
    object TdxLayoutGroup
      AlignHorz = ahParentManaged
      AlignVert = avTop
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      object LayoutControlItemEdit: TdxLayoutItem
        CaptionOptions.Text = 'Edit1'
        CaptionOptions.Layout = clTop
        Control = cbDescriptions
        ControlOptions.ShowBorder = False
      end
      object dxLayoutControl1Group1: TdxLayoutGroup
        AlignHorz = ahRight
        CaptionOptions.Visible = False
        ButtonOptions.Buttons = <>
        LayoutDirection = ldHorizontal
        ShowBorder = False
        object dxLayoutControl1Item2: TdxLayoutItem
          CaptionOptions.Text = 'Button1'
          CaptionOptions.Visible = False
          Control = btnOK
          ControlOptions.ShowBorder = False
        end
        object dxLayoutControl1Item3: TdxLayoutItem
          CaptionOptions.Text = 'Button2'
          CaptionOptions.Visible = False
          Control = btnCancel
          ControlOptions.ShowBorder = False
        end
      end
    end
  end
end
