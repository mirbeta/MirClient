{===============================================================================
  RzCaptionEditor Unit

  Raize Components - Design Editor Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Design Editors
  ------------------------------------------------------------------------------
  TRzCaptionProperty
    This unit implements the TRzCaptionProperty property editor, which when
    registered allows a user to enter multi-line Caption.  This unit also 
    implements the TRzHintProperty property editor, which also allows a user to 
    enter multi-line Hints.


  Modification History
  ------------------------------------------------------------------------------
  5.0    (30 Sep 2008)
    * Registered with the TRzCaptionProperty property editor with all controls
      in Raize Components that support a multi-line Caption. For example,
      TRzButton.Caption.
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * Added TRzGroupItemCaptionProperty and TRzGroupTemplateItemCaptionProperty.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Initial release.
===============================================================================}

{$I RzComps.inc}

unit RzCaptionEditor;

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
  DesignIntf,
  DesignEditors,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  RzDlgBtn,
  RzEdit,
  RzPanel,
  RzButton,
  RzRadChk;

type
  TRzCaptionProperty = class( TStringProperty )
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;


  TRzHintProperty = class( TStringProperty )
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;


  TRzGroupItemCaptionProperty = class( TStringProperty )
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;


  TRzGroupTemplateItemCaptionProperty = class( TStringProperty )
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;


  TRzCaptionEditDlg = class(TForm)
    EdtCaption: TRzMemo;
    PnlEditor: TRzPanel;
    RzDialogButtons1: TRzDialogButtons;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure EdtCaptionKeyPress(Sender: TObject; var Key: Char);
  private
  public
  end;


procedure CaptionToStringList( const Caption: string; Lines: TStrings );
procedure StringListToCaption( Lines: TStrings; var Caption: string );


implementation

{$R *.dfm}

uses
  Registry,
  RzCommon,
  RzGroupBar,
  RzDesignEditors;

const
  CaptionEditorSection = 'CaptionEditor';

{========================}
{== Support Procedures ==}
{========================}

procedure CaptionToStringList( const Caption: string; Lines: TStrings );
var
  I: Integer;
  S: string;
begin
  if Caption <> '' then
  begin
    S := '';
    for I := 1 to Length( Caption ) do
    begin
      if Caption[ I ] = #13 then
      begin
        Lines.Add( S );
        S := '';
      end
      else
        S := S + Caption[ I ];
    end;
    if S <> '' then
      Lines.Add( S );
  end;
end;


procedure StringListToCaption( Lines: TStrings; var Caption: string );
var
  I, L: Integer;
  S: string;
begin
  Caption := '';
  S := Lines.Text;
  L := Length( S );
  while ( L > 0 ) and CharInSet( S[ L ], [ #10, #13 ] ) do
    Dec( L );
  for I := 1 to L do
  begin
    // Ignore line feed characters.  #13 characters will cause line breaks.
    if S[ I ] <> #10 then
      Caption := Caption + S[ I ];
  end;
end;



{================================}
{== TRzCaptionProperty Methods ==}
{================================}

function TRzCaptionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [ paDialog, paMultiSelect, paAutoUpdate ];
end;


procedure TRzCaptionProperty.Edit;
var
  Component: TComponent;
  Dlg: TRzCaptionEditDlg;
  OwnerName, S: string;
  W: Integer;
begin
  Component := TComponent( GetComponent( 0 ) );

  Dlg := TRzCaptionEditDlg.Create( Application );
  try
    if Component.Owner <> nil then
      OwnerName := Component.Owner.Name + '.'
    else
      OwnerName := '';
    Dlg.Caption := OwnerName + Component.Name + ' - Caption Editor';

    if Component is TLabel then
    begin
      Dlg.EdtCaption.Font := TLabel( Component ).Font;
      Dlg.EdtCaption.Font.Color := clWindowText;
      Dlg.EdtCaption.Alignment := TLabel( Component ).Alignment;
      W := GetSystemMetrics( sm_CxVScroll );
      if TLabel( Component ).Width + W + Dlg.EdtCaption.Font.Size + 24 > Dlg.Width then
        Dlg.Width := TLabel( Component ).Width + W + Dlg.EdtCaption.Font.Size + 24;
    end;

    CaptionToStringList( GetStrValue, Dlg.EdtCaption.Lines );

    if Dlg.ShowModal = mrOK then
    begin
      StringListToCaption( Dlg.EdtCaption.Lines, S );
      SetStrValue( S );
    end;

  finally
    Dlg.Free;
  end;
end; {= TRzCaptionProperty.Edit =}



{=============================}
{== TRzHintProperty Methods ==}
{=============================}

function TRzHintProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [ paDialog, paMultiSelect ];
end;


procedure TRzHintProperty.Edit;
var
  Component: TComponent;
  Dlg: TRzCaptionEditDlg;
  OwnerName, S: string;
begin
  Component := TComponent( GetComponent( 0 ) );

  Dlg := TRzCaptionEditDlg.Create( Application );
  try
    if Component.Owner <> nil then
      OwnerName := Component.Owner.Name + '.'
    else
      OwnerName := '';
    Dlg.Caption := OwnerName + Component.Name + ' - Hint Editor';

    CaptionToStringList( GetStrValue, Dlg.EdtCaption.Lines );

    if Dlg.ShowModal = mrOK then
    begin
      StringListToCaption( Dlg.EdtCaption.Lines, S );
      SetStrValue( S );
    end;

  finally
    Dlg.Free;
  end;
end; {= TRzHintProperty.Edit =}



{=========================================}
{== TRzGroupItemCaptionProperty Methods ==}
{=========================================}

function TRzGroupItemCaptionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [ paDialog, paMultiSelect, paAutoUpdate ];
end;


procedure TRzGroupItemCaptionProperty.Edit;
var
  Item: TRzGroupItem;
  Dlg: TRzCaptionEditDlg;
  S: string;
begin
  Item := TRzGroupItem( GetComponent( 0 ) );

  Dlg := TRzCaptionEditDlg.Create( Application );
  try
    Dlg.Caption := Item.Group.Name + ' Item #' + IntToStr( Item.Index ) + ' - Caption Editor';

    CaptionToStringList( GetStrValue, Dlg.EdtCaption.Lines );

    if Dlg.ShowModal = mrOK then
    begin
      StringListToCaption( Dlg.EdtCaption.Lines, S );
      SetStrValue( S );
    end;

  finally
    Dlg.Free;
  end;
end; {= TRzGroupItemCaptionProperty.Edit =}



{=================================================}
{== TRzGroupTemplateItemCaptionProperty Methods ==}
{=================================================}

function TRzGroupTemplateItemCaptionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [ paDialog, paMultiSelect, paAutoUpdate ];
end;


procedure TRzGroupTemplateItemCaptionProperty.Edit;
var
  Item: TRzGroupTemplateItem;
  Dlg: TRzCaptionEditDlg;
  S: string;
begin
  Item := TRzGroupTemplateItem( GetComponent( 0 ) );

  Dlg := TRzCaptionEditDlg.Create( Application );
  try
    Dlg.Caption := Item.Template.Name + ' Item #' + IntToStr( Item.Index ) + ' - Caption Editor';

    CaptionToStringList( GetStrValue, Dlg.EdtCaption.Lines );

    if Dlg.ShowModal = mrOK then
    begin
      StringListToCaption( Dlg.EdtCaption.Lines, S );
      SetStrValue( S );
    end;

  finally
    Dlg.Free;
  end;
end; {= TRzGroupTemplateItemCaptionProperty.Edit =}


{===============================}
{== TRzCaptionEditDlg Methods ==}
{===============================}

procedure TRzCaptionEditDlg.FormCreate(Sender: TObject);
var
  R: TRegIniFile;
begin
  PopupMode := pmAuto;
  Position := poDesigned;

  Icon.Handle := LoadIcon( HInstance, 'RZDESIGNEDITORS_EDIT_ITEMS_ICON' );

  // Load size and position of form
  R := TRegIniFile.Create( RC_SettingsKey );
  try
    Width := R.ReadInteger( CaptionEditorSection, 'Width', Constraints.MinWidth );
    Height := R.ReadInteger( CaptionEditorSection, 'Height', Constraints.MinHeight );
    Left := R.ReadInteger( CaptionEditorSection, 'Left', ( Screen.Width - Width ) div 2 );
    Top := R.ReadInteger( CaptionEditorSection, 'Top', ( Screen.Height - Height ) div 2 );
  finally
    R.Free;
  end;
end;

procedure TRzCaptionEditDlg.FormClose( Sender: TObject; var Action: TCloseAction );
var
  R: TRegIniFile;
begin
  // Store size and position of form
  R := TRegIniFile.Create( RC_SettingsKey );
  try
    R.WriteInteger( CaptionEditorSection, 'Width', Width );
    R.WriteInteger( CaptionEditorSection, 'Height', Height );
    R.WriteInteger( CaptionEditorSection, 'Left', Left );
    R.WriteInteger( CaptionEditorSection, 'Top', Top );
  finally
    R.Free;
  end;
end;



procedure TRzCaptionEditDlg.EdtCaptionKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
    ModalResult := mrCancel;
end;

end.
