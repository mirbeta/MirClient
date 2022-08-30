{**************************************************************************}
{ TADVCOLUMNGRID DESIGN TIME EDITOR                                        }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 1996-2012                                         }
{            Email : info@tmssoftware.com                                  }
{            Web : http://www.tmssoftware.com                              }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit acgde;

interface

{$I TMSDEFS.INC}

uses
  Classes, AdvCGrid, DesignIntf, DesignEditors;

type
  TAdvColumnGridEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
  public
    function GetVerb(index:integer):string; override;
    function GetVerbCount:integer; override;
    procedure ExecuteVerb(Index:integer); override;
  end;

implementation

uses
  SysUtils, Dialogs;

procedure TAdvColumnGridEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
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


procedure TAdvColumnGridEditor.ExecuteVerb(Index: integer);
var
  compiler: string;
  od: TOpendialog;
begin
  case index of
  0:begin
      {$I COMPILERTEXT.INC}

      MessageDlg(Component.ClassName+' version '+(Component as TAdvColumnGrid).VersionString+' for '+compiler+#13#10'© 1997-2015 by TMS software',
                 mtinformation,[mbok],0);
    end;
  1:begin
    od := TOpenDialog.Create(nil);
    od.DefaultExt := '*.CSV';
    od.Filter := 'CSV files (*.csv)|*.csv|All files (*.*)|*.*';
    if od.Execute then
    begin
      (Component as TAdvColumnGrid).SaveFixedCells := False;
      (Component as TAdvColumnGrid).LoadFromCSV(od.FileName);
    end;
    od.Free;
   end;
  2:begin
     (Component as TAdvColumnGrid).Clear;
    end;
  3:begin
      Edit;
    end;
  end;
end;

function TAdvColumnGridEditor.GetVerb(index: integer): string;
begin
  case index of
  0:Result := '&Version';
  1:Result := '&Load CSV file';
  2:Result := '&Clear';
  3:Result := 'C&olumns';
  end;
end;

function TAdvColumnGridEditor.GetVerbCount: integer;
begin
  Result := 4;
end;


end.

