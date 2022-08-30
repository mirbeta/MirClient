{*************************************************************************}
{ TTodoList component                                                     }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ Copyright © 2001-2012                                                   }
{ Email : info@tmssoftware.com                                            }
{ Web : http://www.tmssoftware.com                                        }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

{$I TMSDEFS.INC}

unit ToDoListde;

interface

uses
  Classes, Windows, SysUtils, ToDoList, Forms, Controls, Dialogs
  {$IFDEF DELPHI6_LVL}
  , DesignIntf, DesignEditors
 
  {$ELSE}
  , DsgnIntf
  {$ENDIF}
  ;

type

  TToDoListEditor = class(TDefaultEditor)
  protected
  {$IFNDEF DELPHI6_LVL}
    procedure EditProperty(PropertyEditor: TPropertyEditor;
                           var Continue, FreeEditor: Boolean); override;
  {$ELSE}
    procedure EditProperty(const PropertyEditor:IProperty; var Continue:Boolean); override;
  {$ENDIF}
  public
    function GetVerb(index:integer):string; override;
    function GetVerbCount:integer; override;
    procedure ExecuteVerb(Index:integer); override;
  end;

implementation

procedure TToDoListEditor.ExecuteVerb(Index: integer);
var
  compiler: string;
begin
  case Index of
  0: begin
      {$IFDEF VER90}
      compiler := 'Delphi 2';
      {$ENDIF}
      {$IFDEF VER93}
      compiler:='C++Builder 1';
      {$ENDIF}
      {$IFDEF VER100}
      compiler := 'Delphi 3';
      {$ENDIF}
      {$IFDEF VER110}
      compiler := 'C++Builder 3';
      {$ENDIF}
      {$IFDEF VER120}
      compiler := 'Delphi 4';
      {$ENDIF}
      {$IFDEF VER125}
      compiler := 'C++Builder 4';
      {$ENDIF}
      {$IFDEF VER130}
      {$IFDEF BCB}
      compiler := 'C++Builder 5';
      {$ELSE}
      compiler := 'Delphi 5';
      {$ENDIF}
      {$ENDIF}
      {$IFDEF VER140}
      {$IFDEF BCB}
      compiler := 'C++Builder 6';
      {$ELSE}
      compiler := 'Delphi 6';
      {$ENDIF}
      {$ENDIF}

    {$IFDEF VER150}
      {$IFDEF BCB}
      compiler := '';
      {$ELSE}
      compiler := 'Delphi 7';
      {$ENDIF}
    {$ENDIF}
    {$IFDEF VER160}
      compiler := 'Delphi 8';
    {$ENDIF}
    {$IFDEF VER170}
      compiler := 'Delphi 2005';
    {$ENDIF}
    {$IFDEF VER180}
      {$IFDEF BCB}
      compiler := 'C++Builder 2006';
      {$ELSE}
      compiler := 'Delphi 2006';
      {$ENDIF}
    {$ENDIF}
    {$IFDEF VER185}
      {$IFDEF BCB}
      compiler := 'C++Builder 2007';
      {$ELSE}
      compiler := 'Delphi 2007';
      {$ENDIF}
    {$ENDIF}
    {$IFDEF VER200}
      {$IFDEF BCB}
      compiler := 'C++Builder 2009';
      {$ELSE}
      compiler := 'Delphi 2009';
      {$ENDIF}
    {$ENDIF}

    {$IFDEF VER200}
      {$IFDEF BCB}
      compiler := 'C++Builder 2010';
      {$ELSE}
      compiler := 'Delphi 2010';
      {$ENDIF}
    {$ENDIF}


      MessageDlg(Component.ClassName+' version '+ (Component as TCustomTodoList).VersionString + ' for ' + compiler + #13#10#13#10'© 2002-2010 by TMS software'#13#10'http://www.tmssoftware.com',
                 mtInformation,[mbok],0);
    end;

  1: Edit;
  end;


  if Index > 1 then
    (Component as TCustomTodoList).Look := TTodoListStyle(Index - 2);
end;

{$IFNDEF DELPHI6_LVL}
procedure TToDoListEditor.EditProperty(PropertyEditor: TPropertyEditor;
                                      var Continue, FreeEditor: Boolean);
{$ELSE}
procedure TToDoListEditor.EditProperty(const PropertyEditor:IProperty;
                                      var Continue:Boolean);
{$ENDIF}
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'COLUMNS') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;


function TToDoListEditor.GetVerb(Index: Integer): string;
begin
  Result := '';
  case Index of
  0:Result := 'About';
  1:Result := 'Columns Editor';
  2: Result := 'Office 2003 Blue';
  3: Result := 'Office 2003 Silver';
  4: Result := 'Office 2003 Olive';
  5: Result := 'Office 2003 Classic';
  6: Result := 'Office 2007 Luna';
  7: Result := 'Office 2007 Obsidian';
  8: Result := 'Windows XP';
  9: Result := 'Whidbey';
  10: Result := 'Custom';
  11: Result := 'Office 2007 Silver';
  12: Result := 'Windows Vista';
  13: Result := 'Windows 7';
  14: Result := 'Terminal';
  15: Result := 'Office 2010 Blue';
  16: Result := 'Office 2010 Silver';
  17: Result := 'Office 2010 Black';
  end;
end;

function TToDoListEditor.GetVerbCount: Integer;
begin
  Result := 18;
end;



end.

