{===============================================================================
  RzLookFm Unit

  Raize Components - Form Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Forms
  ------------------------------------------------------------------------------
  TRzLookupForm
    Form file used by TRzLookupDialog component.


  Modification History
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * No changes.
===============================================================================}

{$I RzComps.inc}

unit RzLookupForm;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  SysUtils,
  Windows,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Buttons,
  ExtCtrls,
  Mask,
  RzEdit,
  RzLstBox,
  RzDlgBtn, RzPanel;

type
  TRzLookupForm = class(TForm)
    PnlSelections: TPanel;
    LstSelections: TRzListBox;
    PnlPrompt: TPanel;
    PnlSearch: TPanel;
    EdtSearch: TRzEdit;
    PnlButtons: TRzDialogButtons;
    procedure EdtSearchChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PnlButtonsClickHelp(Sender: TObject);
    procedure EdtSearchKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  private
    function FindClosest( S: string ): Integer;
  end;


implementation

{$R *.dfm}

procedure TRzLookupForm.FormCreate(Sender: TObject);
begin
  PopupMode := pmAuto;
end;


function TRzLookupForm.FindClosest( S: string ): Integer;
begin
  for Result := 0 to LstSelections.Items.Count - 1 do
    if AnsiCompareText( Copy( LstSelections.Items[ Result ], 1, Length( S ) ), S ) = 0 then
      Exit;
  Result := -1;
end;

procedure TRzLookupForm.EdtSearchChange(Sender: TObject);
var
  I: Integer;
begin
  I := FindClosest( EdtSearch.Text );
  if I <> -1 then
    LstSelections.ItemIndex := I;
end;

procedure TRzLookupForm.FormResize(Sender: TObject);
begin
  EdtSearch.Width := LstSelections.Width;
end;

procedure TRzLookupForm.FormShow(Sender: TObject);
begin
  EdtSearch.SelStart := Length( EdtSearch.Text );
end;

procedure TRzLookupForm.PnlButtonsClickHelp(Sender: TObject);
begin
  Application.HelpContext( PnlButtons.HelpContext );
end;

procedure TRzLookupForm.EdtSearchKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ( Key = vk_Down ) or ( Key = vk_Up ) then
  begin
    if Key = vk_Down then
    begin
      if LstSelections.ItemIndex < LstSelections.Count - 1 then
        LstSelections.ItemIndex := LstSelections.ItemIndex + 1;
    end
    else
    begin
      if LstSelections.ItemIndex > 0 then
        LstSelections.ItemIndex := LstSelections.ItemIndex - 1;
    end;
    Key := 0;
  end;
end;

end.
