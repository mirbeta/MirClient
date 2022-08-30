{$I TMSDEFS.INC}

{***********************************************************************}
{ TPlanner component design time code                                   }
{ for Delphi & C++Builder                                               }
{                                                                       }
{ written by TMS Software                                               }
{            copyright © 1999-2013                                      }
{            Email: info@tmssoftware.com                                }
{            Web: http://www.tmssoftware.com                            }
{                                                                       }
{ The source code is given as is. The author is not responsible         }
{ for any possible damage done due to the use of this code.             }
{ The component can be freely used in any application. The complete     }
{ source code remains property of the author and may not be distributed,}
{ published, given or sold in any form as such. No parts of the source  }
{ code can be included in any other component or application without    }
{ written authorization of the author.                                  }
{***********************************************************************}

unit plande;

interface

uses
  Classes, SysUtils
  , Windows, Dialogs, Planner, Forms, PlanStyles, Controls
  , PlanPropPref
  {$IFDEF DELPHI6_LVL}
  , DesignIntf, DesignEditors
  {$ELSE}
  , DsgnIntf
  {$ENDIF}
  ;

type

  TPlannerEditor = class(TDefaultEditor)
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

  TPlannerWaitListEditor = class(TDefaultEditor)
  protected
  {$IFNDEF DELPHI6_LVL}
    procedure EditProperty(PropertyEditor: TPropertyEditor;
                           var Continue, FreeEditor: Boolean); override;
  {$ELSE}
    procedure EditProperty(const PropertyEditor:IProperty; var Continue:Boolean); override;
  {$ENDIF}
  public
  end;

  TSkinProperty = class(TClassProperty)
  public
    function GetAttributes:TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TPlannerHeaderEditor = class(TDefaultEditor)
  protected
  {$IFNDEF DELPHI6_LVL}
    procedure EditProperty(PropertyEditor: TPropertyEditor;
      var Continue, FreeEditor: Boolean); override;
  {$ELSE}
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
  {$ENDIF}
  public
  end;


implementation

uses
  ShlObj, ActiveX, AdvStyleIF;

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


procedure TPlannerEditor.ExecuteVerb(Index: integer);
var
  compiler:string;
  od,sd: topendialog;
  psf: TPlanStyleForm;
  style: TTMSStyle;
begin
  case Index of
  0:begin
      {$I COMPILERTEXT.INC}
      MessageDlg(Component.ClassName+' version '+ (Component as TCustomPlanner).VersionString + ' for ' + compiler + #13#10#13#10'© 1999-2015 by TMS software'#13#10'http://www.tmssoftware.com',
                 mtInformation,[mbok],0);
    end;

  1: Edit;

  2: begin
       if FileExists(GetMyDocuments + '\PLANPREF.CFG') then
         RestorePropertiesToFile(Component,GetMyDocuments + '\PLANPREF.CFG');
     end;
  3: begin
       StorePropertiesToFile(Component,GetMyDocuments + '\PLANPREF.CFG');
     end;
  4: begin
       od := TOpenDialog.Create(Application);
       if od.Execute then
         RestorePropertiesToFile(Component,od.FileName);
       od.Free;
     end;
  5: begin
      sd := TSaveDialog.Create(Application);
      if sd.Execute then
        StorePropertiesToFile(Component,sd.FileName);
      sd.Free;
     end;
  6: begin
      style := (Component as TCustomPlanner).GetComponentStyle;

      psf := TPlanStyleForm.Create(Application);
      case style of
        tsOffice2003Blue: psf.RadioGroup1.ItemIndex := 0;
        tsOffice2003Olive: psf.RadioGroup1.ItemIndex := 1;
        tsOffice2003Silver: psf.RadioGroup1.ItemIndex := 2;
        tsOffice2003Classic: psf.RadioGroup1.ItemIndex := 3;
        tsOffice2007Luna: psf.RadioGroup1.ItemIndex := 4;
        tsOffice2007Obsidian: psf.RadioGroup1.ItemIndex := 5;
        tsOffice2007Silver: psf.RadioGroup1.ItemIndex := 6;
        tsOffice2010Blue: psf.RadioGroup1.ItemIndex := 7;
        tsOffice2010Silver: psf.RadioGroup1.ItemIndex := 8;
        tsOffice2010Black: psf.RadioGroup1.ItemIndex := 9;
        tsWindowsXP: psf.RadioGroup1.ItemIndex := 10;
        tsWindowsVista: psf.RadioGroup1.ItemIndex := 11;
        tsWindows7: psf.RadioGroup1.ItemIndex := 12;
        tsTerminal: psf.RadioGroup1.ItemIndex := 13;
        tsWindows8: psf.RadioGroup1.ItemIndex := 14;
        tsOffice2013White: psf.RadioGroup1.ItemIndex := 15;
        tsOffice2013LightGray: psf.RadioGroup1.ItemIndex := 16;
        tsOffice2013Gray: psf.RadioGroup1.ItemIndex := 17;
        tsWindows10: psf.RadioGroup1.ItemIndex := 18;
        tsOffice2016White: psf.RadioGroup1.ItemIndex := 19;
        tsOffice2016Gray: psf.RadioGroup1.ItemIndex := 20;
        tsOffice2016Black: psf.RadioGroup1.ItemIndex := 21;
      end;

      if psf.ShowModal = mrOK then
      begin
        case psf.RadioGroup1.ItemIndex of
          0: style := tsOffice2003Blue;
          1: style := tsOffice2003Olive;
          2: style := tsOffice2003Silver;
          3: style := tsOffice2003Classic;
          4: style := tsOffice2007Luna;
          5: style := tsOffice2007Obsidian;
          6: style := tsOffice2007Silver;
          7: style := tsOffice2010Blue;
          8: style := tsOffice2010Silver;
          9: style := tsOffice2010Black;
          10: style := tsWindowsXP;
          11: style := tsWindowsVista;
          12: style := tsWindows7;
          13: style := tsTerminal;
          14: style := tsWindows8;
          15: style := tsOffice2013White;
          16: style := tsOffice2013LightGray;
          17: style := tsOffice2013Gray;
          18: style := tsWindows10;
          19: style := tsOffice2016White;
          20: style := tsOffice2016Gray;
          21: style := tsOffice2016Black;
        end;
        if (Component is TCustomPlanner) then
           (Component as TCustomPlanner).SetComponentStyle(style);
           Designer.Modified;
      end;
      psf.Free;
     end;
  7: begin
       (Component as TCustomPlanner).Skin.ClearSkin;
       Designer.Modified;
     end;
  end;
end;

{$IFNDEF DELPHI6_LVL}
procedure TPlannerEditor.EditProperty(PropertyEditor: TPropertyEditor;
                                      var Continue, FreeEditor: Boolean);
{$ELSE}
procedure TPlannerEditor.EditProperty(const PropertyEditor:IProperty;
                                      var Continue:Boolean);
{$ENDIF}
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'ITEMS') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;


function TPlannerEditor.GetVerb(Index: Integer): string;
begin
  Result := '';
  case Index of
  0:Result := 'About';
  1:Result := 'Items Editor';
  2:Result := 'Get preference';
  3:Result := 'Set preference';
  4:Result := 'Load Config';
  5:Result := 'Save Config';
  6:Result := 'Styles';
  7:Result := 'Clear skin';
  end;
end;

function TPlannerEditor.GetVerbCount: Integer;
begin

  Result := 8;
end;

{ TSkinProperty }

procedure TSkinProperty.Edit;
var
  od: TOpenDialog;
  Planner: TCustomPlanner;
begin
  Planner := TCustomPlanner(Getcomponent(0));

  od := TOpenDialog.Create(Application);
  od.Filter := 'Planner Skin (*.plskin)|*.plskin|All files (*.*)|*.*';

  if od.Execute then
  begin
    Planner.Skin.LoadFromFile(od.FileName);
    Modified;
  end;

  od.Free;
end;

function TSkinProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

{$IFNDEF DELPHI6_LVL}
procedure TPlannerWaitListEditor.EditProperty(PropertyEditor: TPropertyEditor;
                                      var Continue, FreeEditor: Boolean);
{$ELSE}
procedure TPlannerWaitListEditor.EditProperty(const PropertyEditor:IProperty;
                                      var Continue:Boolean);
{$ENDIF}
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'ITEMS') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;

{$IFDEF DELPHI6_LVL}
procedure TPlannerHeaderEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
{$ELSE}
procedure TPlannerHeaderEditor.EditProperty(PropertyEditor: TPropertyEditor;
  var Continue, FreeEditor: Boolean);
{$ENDIF}
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;

  if (CompareText(PropName, 'CustomGroups') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;


end.

