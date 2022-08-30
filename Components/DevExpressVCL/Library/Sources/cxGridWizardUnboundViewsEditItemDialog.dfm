object cxGridWizardUnboundViewsEditItemDialogForm: TcxGridWizardUnboundViewsEditItemDialogForm
  Left = 198
  Top = 144
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'Edit Item Dialog'
  ClientHeight = 135
  ClientWidth = 335
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 335
    Height = 135
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object edItemCaption: TcxTextEdit
      Left = 10
      Top = 28
      Style.HotTrack = False
      TabOrder = 0
      Width = 121
    end
    object cbItemProperties: TcxComboBox
      Left = 10
      Top = 73
      Properties.DropDownListStyle = lsFixedList
      Style.HotTrack = False
      TabOrder = 1
      Width = 121
    end
    object btnOK: TcxButton
      Left = 89
      Top = 100
      Width = 75
      Height = 25
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 2
    end
    object btnCancel: TcxButton
      Left = 170
      Top = 100
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 3
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object lciItemCaption: TdxLayoutItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Caption:'
      CaptionOptions.Layout = clTop
      Control = edItemCaption
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciItemProperties: TdxLayoutItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Properties:'
      CaptionOptions.Layout = clTop
      Control = cbItemProperties
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciOK: TdxLayoutItem
      Parent = lcgButtons
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnOK
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciCancel: TdxLayoutItem
      Parent = lcgButtons
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcgButtons: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahCenter
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      LookAndFeel.NativeStyle = True
    end
  end
end
