object fmAlphaBlendingDailog: TfmAlphaBlendingDailog
  Left = 359
  Top = 210
  ActiveControl = btnClose
  BorderStyle = bsDialog
  Caption = 'GDI+ is not installed'
  ClientHeight = 355
  ClientWidth = 395
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object btnClose: TButton
    Left = 318
    Top = 326
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    TabOrder = 0
    OnClick = btnCloseClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 395
    Height = 319
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 1
    object RichEdit1: TRichEdit
      Left = 0
      Top = 0
      Width = 395
      Height = 319
      Align = alClient
      Lines.Strings = (
        '============================================='
        'IMPORTANT NOTE ABOUT THE EXPRESSNAVBAR DEMO'
        '============================================='
        ''
        
          'The ExpressNavBar Suite includes advanced alphablending support ' +
          'via '
        'Microsoft'#39's new GDI+.'
        ''
        
          'Microsoft'#39's GDI+ library was developed for .NET and it allows a ' +
          'developer to '
        
          'determine the opacity or transparency of an image.  To see alpha' +
          'blending '
        
          'within the ExpressNavBar Suite, you must have a copy of GDIPlus.' +
          'DLL on '
        'your '
        
          'system either installed as part of the .NET framework or in the ' +
          'directory of your '
        'application;'
        ''
        'You can get the redistributable for GDI+ here'
        'http://www.microsoft.com/downloads/release.asp?releaseid=32738'
        ''
        
          'Download and run the executable, unzip the files to a local dire' +
          'ctory and'
        
          'copy GDIPlus.DLL to the directory in which the ExpressNavBar dem' +
          'os are '
        
          'located. Note this will not affect any other application in your' +
          ' system unless it is '
        'run from the same directory and uses GDI+.'
        ''
        
          'It is important to note that this dll is ONLY required if you in' +
          'tend on enabling the '
        
          'alphablending capabilities of the ExpressNavBar Suite. If you ha' +
          've no need for '
        'alphablending, then you need not concern yourself with this dll.'
        ''
        'Best Wishes'
        'The Developer Express Team'
        'www.devexpress.com ')
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
    end
  end
end
