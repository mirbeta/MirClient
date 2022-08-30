{===============================================================================
  RzColorNamesEditor Unit

  Raize Components - Design Editor Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Design Editors
  ------------------------------------------------------------------------------
  TRzColorNamesProperty
    Allows user to customize the names of colors used in a TRzColorComboBox.

  TRzColorNamesEditor
    Allows user to customize the names of colors used in a TRzColorComboBox.


  Modification History
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Updated form to use custom framing editing controls and HotTrack style
      buttons, radio buttons, and check boxes.
===============================================================================}

{$I RzComps.inc}

unit RzColorNamesEditor;

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
  RzCmboBx,
  RzDesignEditors,
  DesignIntf,
  DesignEditors,
  DesignMenus,
  Menus,
  Grids,
  RzButton,
  ExtCtrls,
  RzPanel;

type
  TRzColorNamesProperty = class( TClassProperty )
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

  TRzColorNamesEditor = class( TRzDefaultEditor )
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ): string; override;
    procedure ExecuteVerb( Index: Integer ); override;
  end;

  TRzColorNamesEditDlg = class(TForm)
    BtnOK: TRzButton;
    BtnCancel: TRzButton;
    BtnLoad: TRzButton;
    BtnSave: TRzButton;
    DlgOpen: TOpenDialog;
    DlgSave: TSaveDialog;
    RzPanel1: TRzPanel;
    GrdColors: TStringGrid;
    procedure GrdColorsDrawCell(Sender: TObject; Col, Row: Longint;
      Rect: TRect; State: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GrdColorsSelectCell(Sender: TObject; Col, Row: Longint;
      var CanSelect: Boolean);
    procedure BtnLoadClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure GrdColorsSetEditText(Sender: TObject; ACol, ARow: Longint;
      const Value: String);
  private
    FColorNames: TRzColorNames;
    procedure SetColorNames( Value: TRzColorNames );
  public
    property ColorNames: TRzColorNames
      read FColorNames
      write SetColorNames;
  end;


implementation

{$R *.dfm}

uses
  RzCommon;

{===========================}
{== TRzColorNamesProperty ==}
{===========================}

function TRzColorNamesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [ paDialog ];
end;


function TRzColorNamesProperty.GetValue: string;
begin
  { Display Type Name in Object Inspector }
  Result := Format( '(%s)', [ GetPropType^.Name ] );
end;


procedure TRzColorNamesProperty.Edit;
var
  Dialog: TRzColorNamesEditDlg;
begin
  Dialog := TRzColorNamesEditDlg.Create( Application );
  try
    Dialog.Caption := TComponent( GetComponent(0) ).Owner.Name + '.' +
                      TComponent( GetComponent(0) ).Name +
                      '.' + GetName + ' - ' + Dialog.Caption;

    Dialog.ColorNames := TRzColorNames( GetOrdValue );

    if Dialog.ShowModal = mrOK then                       { Display Dialog Box }
    begin              { If user presses OK, move TabList from Dlg to Property }
      SetOrdValue( Longint( Dialog.ColorNames ) );
    end;
  finally
    Dialog.Free;                             { Don't forget to free dialog box }
  end;
end; {= TRzColorNamesProperty.Edit =}


{=================================}
{== TRzColorNamesEditor Methods ==}
{=================================}

function TRzColorNamesEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;


function TRzColorNamesEditor.GetVerb( Index: Integer ): string;
begin
  Result := 'Edit Color Names...';
end;


procedure TRzColorNamesEditor.ExecuteVerb( Index: Integer );
var
  Dialog: TRzColorNamesEditDlg;
begin
  Dialog := TRzColorNamesEditDlg.Create( Application );

  try
    Dialog.ColorNames := ( Component as TRzColorComboBox ).ColorNames;

    Dialog.Caption := Component.Owner.Name + '.' + Component.Name +
                      Dialog.Caption;

    if Dialog.ShowModal = mrOK then                   { Display Dialog Box }
    begin
      ( Component as TRzColorComboBox ).ColorNames := Dialog.ColorNames;
      DesignerModified;
    end;
  finally
    Dialog.Free;                         { Don't forget to free dialog box }
  end;
end; {= TRzColorNamesEditor.ExecuteVerb =}


{==================================}
{== TRzColorNamesEditDlg Methods ==}
{==================================}

procedure TRzColorNamesEditDlg.FormCreate(Sender: TObject);
var
  I: Integer;
  InitSelection: TGridRect;
begin
  PopupMode := pmAuto;

  FColorNames := TRzColorNames.Create;
  GrdColors.RowCount := 1 + MaxStdColors + MaxSysColors + 2;

  with GrdColors do
  begin
    Cells[ 0, 0 ] := 'Color';
    Cells[ 1, 0 ] := 'Sample';
    Cells[ 2, 0 ] := 'Name';

    Cells[ 0, 1 ] := DefaultColorItem.Name;
    for I := 0 to MaxStdColors - 1 do
      Cells[ 0, I + 2 ] := StdColorItems[ I ].Name;
    for I := 0 to MaxSysColors - 1 do
      Cells[ 0, I + 18 ] := SysColorItems[ I ].Name;
    Cells[ 0, 43 ] := CustomColorItem.Name;

    InitSelection.Left := 2;
    InitSelection.Top := 1;
    InitSelection.Right := 2;
    InitSelection.Bottom := 1;
    GrdColors.Selection := InitSelection;
  end;
end;

procedure TRzColorNamesEditDlg.FormDestroy(Sender: TObject);
begin
  FColorNames.Free;
end;

procedure TRzColorNamesEditDlg.SetColorNames( Value: TRzColorNames );
var
  I: Integer;
begin
  FColorNames.Assign( Value );

  GrdColors.Cells[ 2, 1 ] := FColorNames.Default;
  for I := 0 to MaxStdColors - 1 do
    GrdColors.Cells[ 2, I + 2 ] := FColorNames.StdColors[ I ];
  for I := 0 to MaxSysColors - 1 do
    GrdColors.Cells[ 2, I + 18 ] := FColorNames.SysColors[ I ];
  GrdColors.Cells[ 2, 43 ] := FColorNames.Custom;
end;


procedure TRzColorNamesEditDlg.GrdColorsDrawCell(Sender: TObject; Col,
  Row: Longint; Rect: TRect; State: TGridDrawState);
begin
  with GrdColors.Canvas do
  begin
    (*
    if Row = 0 then
    begin
      GrdColors.Canvas.Font.Style := [ fsBold ];
      TextRect( Rect, Rect.Left + 2, Rect.Top + 1, GrdColors.Cells[ Col, 0 ] );
      GrdColors.Canvas.Font.Style := [];
    end;
    *)

    if ( Row > 0 ) and ( Col < 2 ) then
    begin
      Brush.Color := clBtnFace;
      Pen.Color := clGray;
      FillRect( Rect );
      (*
      MoveTo( Rect.Left - 1, Rect.Top );
      LineTo( Rect.Left - 1, Rect.Bottom );
      LineTo( Rect.Right, Rect.Bottom );
      LineTo( Rect.Right, Rect.Top - 1 );
      *)

      if Col = 0 then
        TextRect( Rect, Rect.Left + 2, Rect.Top + 1, GrdColors.Cells[ Col, Row ] )
      else
      begin
        Pen.Color := clBtnFace;
        if Row = 1 then
          Brush.Color := DefaultColorItem.Color
        else if Row < 18 then
          Brush.Color := StdColorItems[ Row - 2 ].Color
        else if Row < 43 then
          Brush.Color := SysColorItems[ Row - 18 ].Color
        else
          Brush.Color := CustomColorItem.Color;
        Rectangle( Rect.Left + 2, Rect.Top + 2, Rect.Right - 2, Rect.Bottom - 2 );
      end;
    end;
  end;
end;


procedure TRzColorNamesEditDlg.GrdColorsSelectCell(Sender: TObject; Col,
  Row: Longint; var CanSelect: Boolean);
begin
  CanSelect := ( Col = 2 ) and ( Row > 0 );
end;


procedure TRzColorNamesEditDlg.GrdColorsSetEditText(Sender: TObject; ACol,
  ARow: Longint; const Value: String);
begin
  if ACol = 2 then
  begin
    if ARow = 1 then
      ColorNames.Default := Value
    else if ARow < 18 then
      ColorNames.StdColors[ ARow - 2 ] := Value
    else if ARow < 43 then
      ColorNames.SysColors[ ARow - 18 ] := Value
    else
      ColorNames.Custom := Value;
  end;
end;


procedure TRzColorNamesEditDlg.BtnLoadClick(Sender: TObject);
var
  TempList: TStringList;
  I: Integer;
begin
  if DlgOpen.Execute then
  begin
    TempList := TStringList.Create;
    try
      TempList.LoadFromFile( DlgOpen.FileName );
      FColorNames.Default := TempList[ 0 ];
      for I := 1 to TempList.Count - 2 do
      begin
        if I < 17 then
          FColorNames.StdColors[ I - 1 ] := TempList[ I ]
        else
          FColorNames.SysColors[ I - 17 ] := TempList[ I ]
      end;
      FColorNames.Custom := TempList[ 1 + MaxStdColors + MaxSysColors ];

      SetColorNames( FColorNames );
    finally
      TempList.Free;
    end;
  end;
end;

procedure TRzColorNamesEditDlg.BtnSaveClick(Sender: TObject);
var
  TempList: TStringList;
  I: Integer;
begin
  if DlgSave.Execute then
  begin
    TempList := TStringList.Create;
    try
      TempList.Add( FColorNames.Default );
      for I := 0 to MaxStdColors - 1 do
        TempList.Add( FColorNames.StdColors[ I ] );
      for I := 0 to MaxSysColors - 1 do
        TempList.Add( FColorNames.SysColors[ I ] );
      TempList.Add( FColorNames.Custom );
      TempList.SaveToFile( DlgSave.FileName );
    finally
      TempList.Free;
    end;
  end;
end;


end.
