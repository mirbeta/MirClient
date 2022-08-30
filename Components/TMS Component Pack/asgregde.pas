{*********************************************************************}
{ TADVSTRINGGRID component                                            }
{ for Delphi & C++Builder                                             }
{                                                                     }
{ written by TMS Software                                             }
{            copyright © 1996-2012                                    }
{            Email : info@tmssoftware.com                             }
{            Web : http://www.tmssoftware.com                         }
{*********************************************************************}

unit AsgRegDE;

interface

{$I TMSDEFS.INC}

uses
  Advgrid, Classes, AsgDE , BaseGrid, AsgPrint, AsgPrev, AsgHTML,
{$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors
{$ELSE}
  DsgnIntf
{$ENDIF}
  ;

type
  TAdvGridPrintSettingsEditor = class(TComponentEditor)
  public
    function GetVerb(index:integer):string; override;
    function GetVerbCount:integer; override;
    procedure ExecuteVerb(Index:integer); override;
  end;

  TPrintSettingsProperty =class(TClassProperty)
  public
    function GetAttributes:TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TAdvPreviewEditor = class(TComponentEditor)
  public
    function GetVerb(index:integer):string; override;
    function GetVerbCount:integer; override;
    procedure ExecuteVerb(Index:integer); override;
  end;

  TAdvGridHTMLSettingsEditor = class(TComponentEditor)
  public
    function GetVerb(index:integer):string; override;
    function GetVerbCount:integer; override;
    procedure ExecuteVerb(Index:integer); override;
  end;

  THTMLSettingsProperty =class(TClassProperty)
  public
    function GetAttributes:TPropertyAttributes; override;
    procedure Edit; override;
  end;
  


procedure Register;  


implementation

uses
  Forms;

procedure Register;
begin
  {$IFDEF DELPHI9_LVL}
  ForceDemandLoadState(dlDisable);
  EnableDemandLoadReport(false);
  {$ENDIF}
  RegisterComponentEditor(TAdvStringGrid,TAdvStringGridEditor);
  RegisterComponentEditor(TAdvGridPrintSettingsDialog,TAdvGridPrintSettingsEditor);
  RegisterPropertyEditor(TypeInfo(TPrintSettings),TAdvStringGrid,'PrintSettings',TPrintSettingsProperty);
  RegisterComponentEditor(TAdvPreviewDialog,TAdvPreviewEditor);
  RegisterComponentEditor(TAdvGridHTMLSettingsDialog,TAdvGridHTMLSettingsEditor);
  RegisterPropertyEditor(TypeInfo(THTMLSettings),TAdvStringGrid,'HTMLSettings',THTMLSettingsProperty);  
end;

procedure TAdvGridPrintSettingsEditor.ExecuteVerb(Index: integer);
begin
  (component as TAdvGridPrintSettingsDialog).Execute;
end;

function TAdvGridPrintSettingsEditor.GetVerb(index: integer): string;
begin
  Result := '&Execute';
end;

function TAdvGridPrintSettingsEditor.GetVerbCount: integer;
begin
  Result := 1;
end;

{ TPrintSettingsProperty }

procedure TPrintSettingsProperty.Edit;
var
  Grid: TAdvStringGrid;
  Settings: TAdvGridPrintSettingsDialog;
begin
  Grid := TAdvStringGrid(getcomponent(0));
  Settings := TAdvGridPrintSettingsDialog.Create(Application);
  Settings.Grid := grid;
  Settings.PrintPreview := true;
  Settings.Options := [psBorders,psGeneral,psFonts,psDateTime,psTitle,psPages,psMargins,psSpacing,psOrientation];
  if Settings.Execute then
    Modified;
  Settings.free;
end;

function TPrintSettingsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TAdvPreviewEditor.ExecuteVerb(Index: integer);
begin
  (Component as TAdvPreviewDialog).Execute;
end;

function TAdvPreviewEditor.GetVerb(index: integer): string;
begin
  Result := '&Execute';
end;

function TAdvPreviewEditor.GetVerbCount: integer;
begin
  Result := 1;
end;

procedure TAdvGridHTMLSettingsEditor.ExecuteVerb(Index: integer);
begin
  (Component as TAdvGridHTMLSettingsDialog).Execute;
end;

function TAdvGridHTMLSettingsEditor.GetVerb(index: integer): string;
begin
  Result := '&Execute';
end;

function TAdvGridHTMLSettingsEditor.GetVerbCount: integer;
begin
  Result := 1;
end;

procedure THTMLSettingsProperty.Edit;
var
  Grid: TAdvStringGrid;
  Settings: TAdvGridHTMLSettingsDialog;
begin
  Grid := TAdvStringGrid(Getcomponent(0));
  Settings := TAdvGridHTMLSettingsDialog.Create(Application);
  Settings.Grid := Grid;
  if Settings.Execute then
    Modified;
  Settings.Free;
end;

function THTMLSettingsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;


end.
