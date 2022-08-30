{**************************************************************************}
{ TADVSTRINGGRID DESIGN TIME EDITOR                                        }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 1996-2015                                         }
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

unit asgde;

interface

{$I TMSDEFS.INC}

uses
  Classes, AdvGrid, Windows, Forms, TypInfo, Controls
  , DesignIntf, DesignEditors, ContNrs, AsgPropPref, AsgGallery
  ;

type
  TAdvStringGridEditor = class(TDefaultEditor)
  protected
    procedure PrintSetProc(const Prop: IProperty);
  public
    function GetVerb(index:integer):string; override;
    function GetVerbCount:integer; override;
    procedure ExecuteVerb(Index:integer); override;
  end;

implementation

uses
  Dialogs, SysUtils, ShlObj, ActiveX, ShellAPI;

{ get My Documents folder }

procedure FreePidl( pidl: PItemIDList );
var
  allocator: IMalloc;
begin
  if Succeeded(SHGetMalloc(allocator)) then
    allocator.Free(pidl);
end;

function GetMyDocuments: string;
var
  pidl: PItemIDList;
  Path: array [0..MAX_PATH-1] of char;
begin
  Result := '';

  if Succeeded(
       SHGetSpecialFolderLocation(0, CSIDL_PERSONAL, pidl)
     ) then
  begin
    if SHGetPathFromIDList(pidl, Path) then
      Result := StrPas(Path);
    FreePidl(pidl);
  end;
end;

procedure TAdvStringGridEditor.PrintSetProc(const Prop: IProperty);
begin
  if Prop.GetName = 'PrintSettings' then
    Prop.Edit
end;

procedure TAdvStringGridEditor.ExecuteVerb(Index: integer);
var
  compiler: string;
  od: TOpendialog;
  sd: TSaveDialog;
  CList: IDesignerSelections;
  gg: TGridGallery;

begin
  case index of
  0:begin
      {$I COMPILERTEXT.INC}
      MessageDlg(Component.ClassName+' version '+(Component as TAdvStringGrid).VersionString+' for '+compiler+#13#10'© 1997-2015 by TMS software', mtinformation,[mbok],0);
    end;
  1:begin
      ShellExecute(0,'open','http://www.tmssoftware.com',nil,nil,SW_NORMAL);
    end;
  2:begin
    od := TOpenDialog.Create(nil);
    od.DefaultExt := '*.CSV';
    od.Filter := 'CSV files (*.csv)|*.csv|All files (*.*)|*.*';
    if od.Execute then
    begin
      (Component as TAdvStringGrid).SaveFixedCells := False;
      (Component as TAdvStringGrid).LoadFromCSV(od.FileName);
    end;
    od.Free;
   end;
  3:begin
     (Component as TAdvStringGrid).Clear;
    end;
  4:begin
      CList := TDesignerSelections.Create;
      CList.Add(Component);
      GetComponentProperties(CList, tkProperties, Designer, PrintSetProc,nil);
    end;
  5:begin
      if FileExists(GetMyDocuments + '\ASGPREF.CFG') then
        RestorePropertiesToFile(Component,GetMyDocuments + '\ASGPREF.CFG');
    end;
  6:begin
      StorePropertiesToFile(Component,GetMyDocuments + '\ASGPREF.CFG');
    end;
  7:begin
      od := TOpenDialog.Create(Application);
      if od.Execute then
        RestorePropertiesToFile(Component,od.FileName);
      od.Free;
    end;
  8:begin
      sd := TSaveDialog.Create(Application);
      if sd.Execute then
        StorePropertiesToFile(Component,sd.FileName);
      sd.Free;
    end;
  9:begin
      gg := TGridGallery.Create(Application);
      if gg.ShowModal = mrOK then
        (Component as TAdvStringGrid).LoadVisualProps(gg.GalleryFile);

      gg.Free;
    end;
  end;
end;

function TAdvStringGridEditor.GetVerb(index: integer): string;
begin
  case index of
  0:result := '&Version';
  1:result := 'www.tmssoftware.com';
  2:result := '&Load CSV file';
  3:result := '&Clear';
  4:result := '&Print settings';
  5:Result := 'Get preference';
  6:Result := 'Set preference';
  7:Result := 'Load Config';
  8:Result := 'Save Config';
  9:Result := 'Gallery';
  end;
end;

function TAdvStringGridEditor.GetVerbCount: integer;
begin
  Result := 10;
end;


end.

