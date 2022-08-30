{===============================================================================
  RzToolbarPrefixForm Unit

  Raize Components - Design Editor Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Description
  ------------------------------------------------------------------------------
  This form is used to specify a prefix or suffix to be used when naming new
  TRzToolButton and TRzSpacer components.


  Modification History
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Added support for specifying a suffix as well as a prefix.
===============================================================================}

{$I RzComps.inc}

unit RzToolbarPrefixForm;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  RzButton,
  StdCtrls,
  Mask,
  RzEdit,
  RzRadChk,
  RzLabel;

type
  TRzFrmPrefixSuffix = class(TForm)
    LblPrefix: TRzLabel;
    OptPrefix: TRzRadioButton;
    OptSuffix: TRzRadioButton;
    EdtPrefix: TRzEdit;
    BtnOK: TRzButton;
    BtnCancel: TRzButton;
    procedure OptPrefixClick(Sender: TObject);
    procedure OptSuffixClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}

procedure TRzFrmPrefixSuffix.FormCreate(Sender: TObject);
begin
  PopupMode := pmAuto;
end;


procedure TRzFrmPrefixSuffix.OptPrefixClick(Sender: TObject);
begin
  LblPrefix.Caption := 'Prefix';
end;


procedure TRzFrmPrefixSuffix.OptSuffixClick(Sender: TObject);
begin
  LblPrefix.Caption := 'Suffix';
end;


end.
