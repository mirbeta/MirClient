{********************************************************************}
{ TCABFile component                                                 }
{ for Delphi & C++Builder                                            }
{ version 1.4                                                        }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 1999-2004                                   }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{********************************************************************}

unit cabregde;

interface
{$I TMSDEFS.INC}
uses
  CabFiles, Classes,
{$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors
{$ELSE}
  DsgnIntf
{$ENDIF}
  ;

type 
  TCABFileEditor = class(TDefaultEditor)
  protected
    {$IFNDEF DELPHI6_LVL}
    procedure EditProperty(PropertyEditor: TPropertyEditor;
      var Continue, FreeEditor: Boolean); override;
    {$ELSE}
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
    {$ENDIF}
  public
    function GetVerb(index:integer):string; override;
    function GetVerbCount:integer; override;
    procedure ExecuteVerb(Index:integer); override;
  end;

procedure Register;

implementation

uses
  SysUtils, Dialogs;

procedure Register;
begin
  RegisterComponentEditor(TCABFile, TCABFileEditor);
end;

procedure TCABFileEditor.ExecuteVerb(Index: integer);
begin
  case Index of
  0: Edit;
  end;
end;

{$IFDEF DELPHI6_LVL}
procedure TCABFileEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
{$ELSE}
procedure TCABFileEditor.EditProperty(PropertyEditor: TPropertyEditor;
  var Continue, FreeEditor: Boolean);
{$ENDIF}
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;

  if (CompareText(PropName, 'CABFILECONTENTS') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;


function TCABFileEditor.GetVerb(Index: Integer): string;
begin
  Result := '';
  case Index of
  0:Result := 'Items';
  end;
end;

function TCABFileEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;




end.

