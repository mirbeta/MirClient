unit AdvTreeComboBoxDE;

{$i tmsdefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,ComCtrls,buttons,imglist,menus,extctrls,
  {$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors
  {$ELSE}
  DsgnIntf
 {$ENDIF}
 ;

type
  TAdvTreeComboBoxEditor = class(TDefaultEditor)
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

implementation

//--------- EDITOR
{$IFDEF DELPHI6_LVL}
procedure TAdvTreeComboBoxEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
{$ELSE}
procedure TAdvTreeComboBoxEditor.EditProperty(PropertyEditor: TPropertyEditor;
  var Continue, FreeEditor: Boolean);
{$ENDIF}
begin
 if (PropertyEditor.GetName = 'Items') then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;


procedure TAdvTreeComboBoxEditor.ExecuteVerb(Index: integer);
begin
  case index of
  0:begin
     edit;
    end;
  end;
  Designer.Modified;
end;

function TAdvTreeComboBoxEditor.GetVerb(index: integer): string;
begin
  case index of
  0:Result := '&Items';
  end;
end;

function TAdvTreeComboBoxEditor.GetVerbCount: integer;
begin
  Result := 1;
end;


end.
