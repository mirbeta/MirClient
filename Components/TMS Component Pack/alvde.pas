{*************************************************************************}
{ TADVLISTVIEW DESIGN TIME EDITOR                                         }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{            copyright © 1998-2014                                        }
{            Email : info@tmssoftware.com                                 }
{            Web : http://www.tmssoftware.com                             }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AlvDE;

interface

{$I TMSDEFS.INC}

uses
  Classes, AdvListV, DesignIntf, DesignEditors;


type
  TAdvListViewEditor = class(TComponentEditor)
  public
    function GetVerb(index:integer):string; override;
    function GetVerbCount:integer; override;
    procedure ExecuteVerb(Index:integer); override;
  end;

implementation

uses
 Dialogs;

procedure TAdvListViewEditor.ExecuteVerb(Index: integer);
var
  compiler: string;
  od: TOpenDialog;
begin
  case Index of
  0:
  begin
    {$I COMPILERTEXT.INC}

    MessageDlg(Component.ClassName+' version '+(Component as TAdvListView).VersionString+' for '+Compiler+#13#10'© 1998-2015 by TMS software',
               mtInformation,[mbOK],0);
  end;
  1:
  begin
    od := TOpenDialog.Create(nil);
    od.DefaultExt := '*.CSV';
    od.Filter := 'CSV files (*.csv)|*.csv|All files (*.*)|*.*';
    if od.Execute then
    begin
      (Component as TAdvListView).LoadHeader := False;
      (Component as TAdvListView).LoadFromCSV(od.FileName);
    end;
    od.Free;
 end;
 2:
 begin
   (Component as TAdvListView).Clear;
 end;
 end;
end;

function TAdvListViewEditor.GetVerb(index: integer): string;
begin
  case index of
  0:result := '&Version';
  1:result := '&Load CSV file';
  2:result := '&Clear';
  end;
end;

function TAdvListViewEditor.GetVerbCount: integer;
begin
  Result := 3;
end;


end.

