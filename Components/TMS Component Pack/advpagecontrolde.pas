{***************************************************************************}
{ TAdvPageControl design time editor                                        }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2003 - 2012                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit AdvPageControlDE;

interface

{$I TMSDEFS.INC}

uses
  Classes, AdvPageControl, Comctrls, Windows, Forms, TypInfo, ExtCtrls, Controls
{$IFDEF DELPHI6_LVL}
  , DesignIntf, DesignEditors, ContNrs
{$ELSE}
  , DsgnIntf
{$ENDIF}
  ;

type
  TAdvPageControlEditor = class(TDefaultEditor)
  protected
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TAdvTabSheetEditor = class(TDefaultEditor)
  protected
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TAdvTabSetEditor = class(TDefaultEditor)
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
  SysUtils;

{ PageControlEditor }

procedure TAdvPageControlEditor.ExecuteVerb(Index: integer);
var
    page : tAdvTabsheet;
begin
  inherited;
  case Index of
  0:
    begin
      TCustomTabControl(Component).ControlStyle := TCustomTabControl(Component).ControlStyle + [csAcceptsControls];
      page := TAdvTabSheet(Designer.CreateComponent(TAdvTabSheet,Component,23,0,100,100));

      page.parent := TAdvpagecontrol(component);
      page.AdvPageControl := TAdvpagecontrol(component);
      page.Caption := page.name;
      TAdvpagecontrol(component).ActivePage:= page;
      with TAdvPageControl(Component) do
          Update;

      (Component as TCustomTabControl).Invalidate;
      TCustomTabControl(Component).ControlStyle := TCustomTabControl(Component).ControlStyle - [csAcceptsControls];
    end;
  1: TAdvPageControl(Component).SelectNextPage(false);
  2: TAdvPageControl(Component).SelectNextPage(True);
  end;
end;

function TAdvPageControlEditor.GetVerb(Index: Integer): string;
begin
  case Index of
  0: Result := 'New Page';
  1: Result := 'Previous Page';
  2: Result := 'Next Page';
  end;
end;

function TAdvPageControlEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

{ TAdvTabSheetEditor }

procedure TAdvTabSheetEditor.ExecuteVerb(Index: integer);
var
  page: TAdvTabsheet;
begin
  inherited;
  case Index of
  0:
    begin
      TWinControl(Component).Parent.ControlStyle := TWinControl(Component).Parent.ControlStyle + [csAcceptsControls];
      page := TAdvTabSheet(Designer.CreateComponent(TAdvTabSheet,TWinControl(Component).Parent,23,0,100,100));
      page.parent := TWinControl(Component).Parent;
      page.AdvPageControl := TAdvpagecontrol(TWinControl(Component).Parent);
      page.Caption := page.name;
      TAdvpagecontrol(TWinControl(Component).Parent).ActivePage:= page;

      with TAdvPageControl(TWinControl(Component).Parent) do
        Update;

      (TWinControl(Component).Parent as TWinControl).Invalidate;
      TWinControl(Component).Parent.ControlStyle := TWinControl(Component).Parent.ControlStyle - [csAcceptsControls];
    end;
  1: TAdvPageControl(TCustomPanel(Component).Parent).SelectNextPage(false);
  2: TAdvPageControl(TCustomPanel(Component).Parent).SelectNextPage(true);
  3:
    begin
      TAdvTabSheet(Component).AdvPageControl := nil;
      Component.Free;
      Designer.Modified;
    end;
  end;
end;

function TAdvTabSheetEditor.GetVerb(Index: Integer): string;
begin
  case Index of
  0: Result := 'New Page';
  1: Result := 'Previous Page';
  2: Result := 'Next Page';
  3: Result := 'Delete Page';
  end;
end;

function TAdvTabSheetEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;

{ TAdvTabSetEditor }
{$IFDEF DELPHI6_LVL}
procedure TAdvTabSetEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
{$ELSE}
procedure TAdvTabSetEditor.EditProperty(PropertyEditor: TPropertyEditor;
  var Continue, FreeEditor: Boolean);
{$ENDIF}
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'ADVTABS') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;



end.
