{===============================================================================
  RzLookup Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzLookupDialog
    When launched, displays a dialog contains an edit field and a list box.
    Allows a user to select an item from a list using more space than just a 
    combo box drop-down list.


  Modification History
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Surfaced CharCase property for the search edit field in the lookup dialog
      box.  The new FlatColor property can be used to change the color of the
      line that is drawn for the border.
    * Add component editor to TRzLookupDialog so that the dialog can be tested
      at design-time.
===============================================================================}

{$I RzComps.inc}

unit RzLookup;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Classes,
  SysUtils,
  StdCtrls,
  Graphics,
  Forms,
  RzCommon;

type
  TRzLookupDialog = class( TRzDialogComponent )
  private
    FAutoSelect: Boolean;
    FButtonGlyphs: Boolean;
    FCharCase: TEditCharCase;
    FPrompt: string;
    FList: TStrings;
    FSelectedIndex: Integer;
    FSelectedItem: string;
    FSearchEdit: TCustomEdit;
    FSearchString: string;
    FUpdateSearchEdit: Boolean;
  protected
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    procedure DefineProperties( Filer: TFiler ); override;

    { Property Access Methods }
    procedure SetList( Value: TStrings ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    function Execute: Boolean;

    property SearchString: string
      read FSearchString
      write FSearchString;

    property SelectedIndex: Integer
      read FSelectedIndex
      write FSelectedIndex;

    property SelectedItem: string
      read FSelectedItem;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property AutoSelect: Boolean
      read FAutoSelect
      write FAutoSelect
      default True;

    property ButtonGlyphs: Boolean
      read FButtonGlyphs
      write FButtonGlyphs
      default False;

    property CharCase: TEditCharCase
      read FCharCase
      write FCharCase
      default ecNormal;

    property Prompt: string
      read FPrompt
      write FPrompt;

    property List: TStrings
      read FList
      write SetList;

    property SearchEdit: TCustomEdit
      read FSearchEdit
      write FSearchEdit;

    property UpdateSearchEdit: Boolean
      read FUpdateSearchEdit
      write FUpdateSearchEdit
      default False;

    { Inherited Properties & Events }
    property BorderStyle;
    property Caption;
    property CaptionOK;
    property CaptionCancel;
    property CaptionHelp;
    property Font;
    property FrameColor;
    property FrameStyle;
    property FrameVisible;
    property FramingPreference;
    property Height default 275;
    property HelpContext;
    property Width default 275;
  end;


implementation

uses
  {&RAS}
  Windows,
  Controls,
  RzLookupForm;

{&RT}
{=============================}
{== TRzLookupDialog Methods ==}
{=============================}

constructor TRzLookupDialog.Create( AOwner: TComponent );
begin
  inherited;
  FList := TStringList.Create;
  Height := 275;
  Width := 275;
  FAutoSelect := True;
  FButtonGlyphs := False;
  FCharCase := ecNormal;
  FUpdateSearchEdit := False;  { Set to False for backward compatibility }
  {&RCI}
end;


destructor TRzLookupDialog.Destroy;
begin
  FList.Free;
  inherited;
end;


procedure TRzLookupDialog.DefineProperties( Filer: TFiler );
begin
  inherited;
  { Handle the fact that the SearchBtnEdit property was published in v1.6 }
  Filer.DefineProperty( 'SearchBtnEdit', TRzOldPropReader.ReadOldIdentProp, nil, False );
end;


procedure TRzLookupDialog.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;

  if ( Operation = opRemove ) and ( AComponent = FSearchEdit ) then
    FSearchEdit := nil;
end;


procedure TRzLookupDialog.SetList( Value: TStrings );
begin
  FList.Assign( Value );
end;


function TRzLookupDialog.Execute: Boolean;
var
  Dlg: TRzLookupForm;
begin
  {&RV}
  Dlg := TRzLookupForm.Create( Application );
  with Dlg do
  begin
    try
      Dlg.BorderStyle := Self.BorderStyle;
      Dlg.Left := Self.OriginLeft;
      Dlg.Top := Self.OriginTop;
      Dlg.Width := Self.Width;
      Dlg.Height := Self.Height;

      CenterForm( Dlg );

      Dlg.Font := Self.Font;
      Dlg.Caption := Self.Caption;
      PnlPrompt.Caption := FPrompt;

      PnlButtons.ShowHelpButton := Self.HelpContext <> 0;
      PnlButtons.HelpContext := Self.HelpContext;
      PnlButtons.ShowGlyphs := FButtonGlyphs;
      PnlButtons.CaptionOK := CaptionOK;
      PnlButtons.CaptionCancel := CaptionCancel;
      PnlButtons.CaptionHelp := CaptionHelp;

      if FrameVisible then
      begin
        EdtSearch.FrameVisible := True;
        EdtSearch.FrameColor := FrameColor;
        EdtSearch.FrameStyle := FrameStyle;
        LstSelections.FrameVisible := True;
        LstSelections.FrameColor := FrameColor;
        LstSelections.FrameStyle := FrameStyle;
      end;

      if FPrompt = '' then
        PnlPrompt.Height := 4
      else
        PnlPrompt.Height := 21;
      LstSelections.Items := FList;

      if FSearchEdit <> nil then
        EdtSearch.Text := FSearchEdit.Text
      else
        EdtSearch.Text := FSearchString;

      EdtSearch.AutoSelect := FAutoSelect;
      if not FAutoSelect then
        EdtSearch.SelStart := Length( EdtSearch.Text );
      EdtSearch.CharCase := FCharCase;

      Result := ShowModal = idOK;
      if Result then
      begin
        FSelectedIndex := LstSelections.ItemIndex;
        FSelectedItem := LstSelections.SelectedItem;

        if FUpdateSearchEdit and ( FSearchEdit <> nil ) then
          FSearchEdit.Text := FSelectedItem;
      end;
    finally
      Free;
    end;
  end; { with }
end;

{&RUIF}
end.
