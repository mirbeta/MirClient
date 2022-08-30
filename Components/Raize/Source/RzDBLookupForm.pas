{===============================================================================
  RzDBLookupForm Unit

  Raize Components - Form Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Forms
  ------------------------------------------------------------------------------
  TRzDBLookupForm
    Form file used by TRzDBLookupDialog component


  Modification History
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Replaced TDBNavigator with TRzDBNavigator.
===============================================================================}

{$I RzComps.inc}

unit RzDBLookupForm;

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
  Grids,
  DBGrids,
  DB,
  Buttons,
  ExtCtrls,
  DBCtrls,
  RzPanel,
  Mask,
  RzEdit,
  RzDlgBtn, 
  RzDBNav, 
  RzDBGrid;

type
  TRzDBLookupForm = class(TForm)
    SrcLookup: TDataSource;
    PnlPrompt: TPanel;
    LblPrompt: TLabel;
    PnlNavigator: TPanel;
    EdtSearch: TRzEdit;
    PnlLookup: TRzPanel;
    GrdLookup: TRzDBGrid;
    PnlButtons: TRzDialogButtons;
    NavLookup: TRzDBNavigator;
    procedure EdtSearchChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure EdtSearchKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure PnlButtonsClickHelp(Sender: TObject);
    procedure EdtSearchKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure GrdLookupDblClick(Sender: TObject);
  private
  public
    SearchField: string;
    KeyField: string;
    NumbersOnly: Boolean;
    UseFilter: Boolean;
    FilterOnStringField: Boolean;
    procedure FindKeyField( const KeyValue: string );
  end;


implementation

{$R *.dfm}
         
uses
  RzCommon;

{==========================}
{== TRzFrmLookup Methods ==}
{==========================}

{===============================================================================
  TRzFrmLookup.EdtSearchChange

  Description
    This method is called whenever the contents of the EdtSearch edit field
    changes.  As the user enters characters, this method performs a simple
    linear search looking for a match.

  Comments
    The FindNearest method is not used because it is limited to working only
    with TTables. Plus, FindNearest is fine for searching for string values,
    but it does not work well for searching for numbers.

    The dataset must be sorted on the column being searched.
===============================================================================}

procedure TRzDBLookupForm.EdtSearchChange( Sender: TObject );
var
  S: string;
begin
  SrcLookup.Dataset.DisableControls;               // Disable controls so grid does not flicker
  try
    if EdtSearch.Text = '' then
    begin
      if UseFilter then
        SrcLookup.Dataset.Filtered := False;
      SrcLookup.Dataset.First
    end
    else
    begin
      S := QuotedStr( EdtSearch.Text );
      S := Copy( S, 2, Length( S ) - 2 );
      if UseFilter then
      begin
        if FilterOnStringField then
          SrcLookup.Dataset.Filter := SearchField + ' = ''' + S + '*'''
        else
          SrcLookup.Dataset.Filter := SearchField + '=' + S;
        SrcLookup.Dataset.FilterOptions := [ foCaseInsensitive ];
        SrcLookup.Dataset.Filtered := True;
      end
      else
      begin
        SrcLookup.Dataset.Locate( SearchField, S, [ loPartialKey, loCaseInsensitive ] );
      end;
    end;
  finally
    SrcLookup.Dataset.EnableControls;                         // Be sure to enable the controls
  end;
end;


procedure TRzDBLookupForm.FindKeyField( const KeyValue: string );
begin
  SrcLookup.Dataset.DisableControls;               { Disable controls so grid does not flicker }
  try
    if KeyValue = '' then
      SrcLookup.Dataset.First
    else
      SrcLookup.Dataset.Locate( KeyField, KeyValue, [ loPartialKey, loCaseInsensitive ] );
    EdtSearch.Text := SrcLookup.Dataset.FieldByName( SearchField ).AsString;
  finally
    SrcLookup.Dataset.EnableControls;                         { Be sure to enable the controls }
  end;
end;


procedure TRzDBLookupForm.FormShow( Sender: TObject );
begin
  EdtSearch.Width := GrdLookup.Width;                  { Resize the edit field }
  EdtSearch.SetFocus;                  { Reset the focus if dialog redisplayed }
end;


procedure TRzDBLookupForm.FormResize( Sender: TObject );
begin
  EdtSearch.Width := GrdLookup.Width;                  { Resize the edit field }
end;


procedure TRzDBLookupForm.EdtSearchKeyDown( Sender: TObject; var Key: Word;
                                            Shift: TShiftState );
begin
  if ( Key = vk_Down ) or ( Key = vk_Up ) then
  begin
    if Key = vk_Down then
      SrcLookup.Dataset.Next
    else
      SrcLookup.Dataset.Prior;
    Key := 0;
  end;
end;


procedure TRzDBLookupForm.EdtSearchKeyPress( Sender: TObject; var Key: Char );
begin
  if NumbersOnly and ( Key >= #32 ) and
     not CharInSet( Key, [ '-', '+', '0'..'9' ] ) then
  begin
    MessageBeep( 0 );
    Key := #0;
  end;
end;


procedure TRzDBLookupForm.FormCreate( Sender: TObject );
begin
  PopupMode := pmAuto;

  NavLookup.Flat := True;
  PnlNavigator.FullRepaint := False;
  PnlPrompt.FullRepaint := False;
end;

procedure TRzDBLookupForm.PnlButtonsClickHelp( Sender: TObject );
begin
  Application.HelpContext( PnlButtons.HelpContext );
end;


procedure TRzDBLookupForm.GrdLookupDblClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

end.


