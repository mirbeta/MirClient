object Form1: TForm1
  Left = 445
  Top = 186
  BorderStyle = bsDialog
  Caption = 'Send Debug'
  ClientHeight = 508
  ClientWidth = 226
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '����'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 12
  object lblMsg: TLabel
    Left = 8
    Top = 152
    Width = 60
    Height = 12
    Caption = '�������ݣ�'
  end
  object lblCount: TLabel
    Left = 8
    Top = 176
    Width = 60
    Height = 12
    Caption = '���ʹ�����'
  end
  object Bevel1: TBevel
    Left = 8
    Top = 200
    Width = 209
    Height = 9
    Shape = bsTopLine
  end
  object Bevel2: TBevel
    Left = 8
    Top = 240
    Width = 209
    Height = 9
    Shape = bsTopLine
  end
  object Button1: TButton
    Left = 168
    Top = 176
    Width = 49
    Height = 21
    Caption = '����'
    TabOrder = 10
    OnClick = Button1Click
  end
  object cbbLevel: TComboBox
    Left = 120
    Top = 80
    Width = 97
    Height = 20
    Style = csDropDownList
    ItemHeight = 12
    TabOrder = 2
    Items.Strings = (
      '0'
      '1'
      '2'
      '3')
  end
  object chkLevel: TCheckBox
    Left = 8
    Top = 80
    Width = 97
    Height = 17
    Caption = 'ʹ��Level����'
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
  object chkType: TCheckBox
    Left = 8
    Top = 104
    Width = 97
    Height = 17
    Caption = 'ʹ�� Type����'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object cbbType: TComboBox
    Left = 120
    Top = 104
    Width = 97
    Height = 20
    Style = csDropDownList
    ItemHeight = 12
    TabOrder = 4
    Items.Strings = (
      '0'
      '1'
      '2'
      '3'
      '4'
      '5'
      '6'
      '7'
      '8'
      '9'
      '10'
      '11'
      '12'
      '13'
      '14')
  end
  object chkTag: TCheckBox
    Left = 8
    Top = 128
    Width = 97
    Height = 17
    Caption = 'ʹ�� Tag ����'
    TabOrder = 5
  end
  object cbbTag: TComboBox
    Left = 120
    Top = 128
    Width = 97
    Height = 20
    ItemHeight = 12
    TabOrder = 6
  end
  object edtMsg: TEdit
    Left = 72
    Top = 152
    Width = 145
    Height = 20
    TabOrder = 7
    Text = 'Test Message'
    OnKeyPress = edtMsgKeyPress
  end
  object edtCount: TEdit
    Left = 72
    Top = 176
    Width = 73
    Height = 20
    TabOrder = 8
    Text = '1'
  end
  object udCount: TUpDown
    Left = 145
    Top = 176
    Width = 15
    Height = 20
    Associate = edtCount
    Min = 1
    Max = 1000
    Position = 1
    TabOrder = 9
    Wrap = False
  end
  object Button2: TButton
    Left = 8
    Top = 208
    Width = 65
    Height = 21
    Caption = '��ʱ��ʼ'
    TabOrder = 11
    OnClick = Button2Click
  end
  object rgMethod: TRadioGroup
    Left = 8
    Top = 8
    Width = 209
    Height = 65
    Caption = 'ʹ�÷�ʽ'
    ItemIndex = 1
    Items.Strings = (
      'Log   ��ֻ��DEBUG ����ʱ��Ч��'
      'Trace ��ֻ��NDEBUG����ʱ��Ч��')
    TabOrder = 0
  end
  object btnEnter: TButton
    Left = 80
    Top = 208
    Width = 41
    Height = 21
    Caption = '����'
    TabOrder = 12
    OnClick = btnEnterClick
  end
  object btnLeave: TButton
    Left = 128
    Top = 208
    Width = 41
    Height = 21
    Caption = '�˳�'
    TabOrder = 13
    OnClick = btnLeaveClick
  end
  object Button3: TButton
    Left = 176
    Top = 208
    Width = 41
    Height = 21
    Caption = '�ָ�'
    TabOrder = 14
    OnClick = Button3Click
  end
  object edtInt: TEdit
    Left = 8
    Top = 248
    Width = 73
    Height = 20
    TabOrder = 15
    Text = '1'
  end
  object udInt: TUpDown
    Left = 81
    Top = 248
    Width = 15
    Height = 20
    Associate = edtInt
    Min = 1
    Max = 1000
    Position = 1
    TabOrder = 16
    Wrap = False
  end
  object btnSendInt: TButton
    Left = 104
    Top = 248
    Width = 113
    Height = 21
    Caption = '��������'
    TabOrder = 17
    OnClick = btnSendIntClick
  end
  object edtFloat: TEdit
    Left = 8
    Top = 272
    Width = 73
    Height = 20
    TabOrder = 18
    Text = '3.14159'
  end
  object btnSendFloat: TButton
    Left = 104
    Top = 272
    Width = 113
    Height = 21
    Caption = '���͸���'
    TabOrder = 19
    OnClick = btnSendFloatClick
  end
  object btnSendColor: TButton
    Left = 40
    Top = 296
    Width = 65
    Height = 21
    Caption = '������ɫ'
    TabOrder = 21
    OnClick = btnSendColorClick
  end
  object btnSendBool: TButton
    Left = 120
    Top = 296
    Width = 97
    Height = 21
    Caption = '���Ͳ���'
    TabOrder = 22
    OnClick = btnSendBoolClick
  end
  object btnSendPoint: TButton
    Left = 8
    Top = 320
    Width = 97
    Height = 21
    Caption = '���͵��'
    TabOrder = 23
    OnClick = btnSendPointClick
  end
  object btnSendRect: TButton
    Left = 120
    Top = 320
    Width = 97
    Height = 21
    Caption = '���Ϳ��'
    TabOrder = 24
    OnClick = btnSendRectClick
  end
  object pnlColor: TPanel
    Left = 8
    Top = 296
    Width = 17
    Height = 17
    BevelOuter = bvLowered
    TabOrder = 20
    OnClick = pnlColorClick
  end
  object btnDump: TButton
    Left = 8
    Top = 344
    Width = 97
    Height = 21
    Caption = '���ڴ�'
    TabOrder = 25
    OnClick = btnDumpClick
  end
  object btnExcept: TButton
    Left = 120
    Top = 344
    Width = 97
    Height = 21
    Caption = '���쳣'
    TabOrder = 26
    OnClick = btnExceptClick
  end
  object btnWriteComp: TButton
    Left = 8
    Top = 368
    Width = 97
    Height = 21
    Caption = 'д���'
    TabOrder = 27
    OnClick = btnWriteCompClick
  end
  object btnWriteObj: TButton
    Left = 120
    Top = 368
    Width = 97
    Height = 21
    Caption = 'д����'
    TabOrder = 28
    OnClick = btnWriteObjClick
  end
  object btnWriteCol: TButton
    Left = 8
    Top = 392
    Width = 97
    Height = 21
    Caption = 'дCollection'
    TabOrder = 29
    OnClick = btnWriteColClick
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 489
    Width = 226
    Height = 19
    Panels = <
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 50
      end>
    SimplePanel = True
  end
  object btnThread: TButton
    Left = 120
    Top = 392
    Width = 97
    Height = 21
    Caption = '�߳��ڷ���'
    TabOrder = 30
    OnClick = btnThreadClick
  end
  object Button4: TButton
    Left = 8
    Top = 416
    Width = 97
    Height = 21
    Hint = 'OutputDebugString'
    Caption = 'Debug API'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 31
    OnClick = Button4Click
  end
  object btnEvaluate: TButton
    Left = 8
    Top = 440
    Width = 97
    Height = 21
    Caption = '�鿴����Object'
    TabOrder = 33
    OnClick = btnEvaluateClick
  end
  object btnEvaColl: TButton
    Left = 120
    Top = 440
    Width = 97
    Height = 21
    Caption = '�鿴 Collection'
    TabOrder = 34
    OnClick = btnEvaCollClick
  end
  object btnDatetime: TButton
    Left = 120
    Top = 416
    Width = 97
    Height = 21
    Caption = '����ʱ��'
    TabOrder = 35
    OnClick = btnDatetimeClick
  end
  object btnFmtError: TButton
    Left = 8
    Top = 464
    Width = 209
    Height = 21
    Hint = 'OutputDebugString'
    Caption = '��������� Format'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 36
    OnClick = btnFmtErrorClick
  end
  object dlgColor: TColorDialog
    Ctl3D = True
    Left = 16
    Top = 296
  end
  object tmr1: TTimer
    OnTimer = tmr1Timer
    Left = 96
    Top = 24
  end
end
